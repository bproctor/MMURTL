;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0

.DATA
.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC
.INCLUDE RQB.INC
.INCLUDE JOB.INC

dJunk   DD 0		;Used as temp in Service Abort function
dmsg    DD 2 DUP(0) ;tmp for abort

EXTRN TimerTick  DD
EXTRN SwitchTick DD
EXTRN dfHalted   DD
EXTRN _nSwitches DD
EXTRN _nHalts    DD
EXTRN _nReady    DD

.CODE

EXTRN LinToPhy NEAR

;This file contains all the internal kernel functions plus
;the PUBLIC kernel functions SendMsg, ISendMsg, WaitMsg, CheckMsg,
;Request, Respond, MoveMsg, NewTask and SpawnTask.
;Exchange management functions such as AllocExch and DeAllocExch are
;also here.
;
; Note on interrupts and the kernel primitives:
; Because certain kernel functions may be called from ISRs,
; and because portions of other kernel functions may be
; interrupted by a task change that happens because of an action
; that an ISR takes, we must ensure that interrupts are
; DISABLED prior to the allocation or deallocation
; of ALL kernel data segment resources. This especially applies when
; a message is "in transit."  For example: taken from an exchange
; but not yet linked to a TSS and placed on the ready queue.
; This is important!
;
; NOTE on Exchanges, Messages, and Tasks (TSSs)
; In MMURTL, an exchange is a place where either Messages or
; Tasks wait.  There can never be tasks AND messages at an
; exchange at the same time (unless the kernel is BROKEN!).
; When a message is sent to an exchange, if a task is waiting
; there, is it immediately associated with the message
; and placed on the readyQ in priority order.
; For this reason we share the HEAD and TAIL link pointers
; for tasks and messages on an exchange.
;
;=============================================================================

enQueueMsg:
;
; INPUT : ESI,EAX
; OUTPUT : NONE
; REGISTERS : EAX,EDX,ESI,FLAGS
; MODIFIES : EDX
;
; This routine will place the link block pointed to by EAX onto the exchange
; pointed to by the ESI register. If EAX is NIL then the routine returns.
;
		OR  EAX,EAX				; if pLBin = NIL THEN Return;
		JZ eqMsgDone            ;
		MOV DWORD PTR [EAX+NextLB], 0   ; pLBin^.Next <= NIL;
		XCHG ESI,EAX            ; pExch => EAX, pLBin => ESI
		CMP DWORD PTR [EAX+EHead], 0    ; if ..MsgHead = NIL
		JNE eqMNotNIL           ; then
		MOV [EAX+EHead],ESI     ;  ..MsgHead <= pLBin;
		MOV [EAX+ETail],ESI     ;  ..MsgTail <= pLBin;
		MOV DWORD PTR [EAX+fEMsg], 1		; Flag it as a Msg (vice a task)
		XCHG EAX,ESI            ; Put pExch Back in ESI
		RETN                    ; else
eqMNotNIL:
	    MOV EDX,[EAX+ETail]     ;  ..MsgTail^.NextLB <= pLBin;
		MOV [EDX+NextLB],ESI    ;
		MOV [EAX+ETail],ESI     ;  ..MsgTail <= pLBin;
		MOV DWORD PTR [EAX+fEMsg], 1		; Flag it as a Msg (vice a task)
		XCHG EAX,ESI            ; Put pExch Back in ESI
eqMsgDone:
	    RETN                    ;

;=============================================================================

deQueueMsg:
;
; INPUT : ESI
; OUTPUT : EAX
; REGISTERS : EAX,EBX,ESI,FLAGS
; MODIFIES : *prgExch[ESI].msg.head and EBX
;
; This routine will dequeue a link block on the exchange pointed to by the
; ESI register and place the pointer to the link block dequeued into EAX.
;
		MOV EAX,[ESI+fEMsg]     ; Get Msg Flag
		OR EAX, EAX				; Is it a Msg?
		JZ deMsgDone			; No! (return 0)
		MOV EAX,[ESI+EHead]     ; pLBout <= ..MsgHead;
		OR EAX, EAX             ; if pLBout = NIL then Return;
		JZ deMsgDone            ;
		MOV EBX,[EAX+NextLB]    ; ..MsgHead <= ..MsgHead^.Next;
		MOV [ESI+EHead],EBX     ;
deMsgDone:
	    RETN                    ;

;=============================================================================

deQueueTSS:
;
; INPUT : ESI
; OUTPUT : EAX
; REGISTERS : EAX,EBX,ESI,FLAGS
; MODIFIES : EAX,EBX
;
; This routine will dequeue a TSS on the exchange pointed to by the ESI
; register and place the pointer to the TSS dequeued into EAX.
; EAX return NIL if no TSS is waiting at Exch ESI
;
		XOR EAX,EAX				; Set up to return nothing
		MOV EBX,[ESI+fEMsg]		; Msg flag (is it a Msg)
		OR EBX, EBX
		JNZ deTSSDone			; It's a Msg (return leaving EAX 0)
		MOV EAX,[ESI+EHead] 	; pTSSout <= ..TSSHead;
		OR EAX, EAX           	; if pTSSout = NIL then Return;
		JZ deTSSDone           	;
		MOV EBX,[EAX+NextTSS]  	; ..TSSHead <= ..TSSHead^.Next;
		MOV [ESI+EHead],EBX  	;
deTSSDone:
	    RETN                    	;

;=============================================================================

PUBLIC enQueueRdy:
;
; INPUT : EAX
; OUTPUT : NONE
; REGISTERS : EAX,EBX,EDX,FLAGS
; MODIFIES : EAX,EBX,EDX
;
; This routine will place a TSS pointed to by EAX onto the ReadyQueue. This
; algorithm chooses the proper priority queue based on the TSS priority.
; The Rdy Queue is an array of QUEUES (2 pointers, head & tail per QUEUE).
; This links the TSS to rgQueue[nPRI].
;
		OR  EAX,EAX             ; if pTSS = NIL then return;
		JZ eqRdyDone            ;
		INC _nReady				;
		MOV DWORD PTR [EAX+NextTSS], 0  ; pTSSin^.Next <= NIL;
		XOR EBX,EBX             ; get the priority
		MOV BL,[EAX+Priority]   ; in EBX
		XCHG EAX,EBX            ; Priority => EAX, pTSSin => EBX
		SHL EAX, 3              ; Times 8 (size of QUEUE)
		LEA EDX,RdyQ            ; Add offset of RdyQ => EAX
		ADD EAX,EDX             ; EAX pts to proper Rdy Queue
		CMP DWORD PTR [EAX+Head], 0      ; if Head = NIL
		JNE eqRNotNIL           ; then
		MOV [EAX+Head],EBX      ;  ..Head <= pTSSin;
		MOV [EAX+Tail],EBX      ;  ..Tail <= pTSSin;
		RETN                    ; else
eqRNotNIL:
	    MOV EDX,[EAX+Tail]      ;  ..Tail^.NextTSS <= pTSSin;
		MOV [EDX+NextTSS],EBX   ;
		MOV [EAX+Tail],EBX      ;  ..Tail <= pTSSin;
eqRdyDone:
	    RETN                    ;

;=============================================================================

PUBLIC deQueueRdy:
;
; INPUT : NONE
; OUTPUT : EAX
; REGISTERS : EAX,EBX,ECX,FLAGS
; MODIFIES : RdyQ
;
; This routine will return a pointer in EAX to the highest priority task
; queued on the RdyQ. Then the routine will "pop" the TSS from the RdyQ.
; If there was no task queued, EAX is returned as NIL.
;
		MOV ECX,nPRI            ; Set up the number of times to loop
		LEA EBX,RdyQ            ; Get base address of RdyQ in EBX
deRdyLoop:
	    MOV EAX,[EBX]           ; Get pTSSout in EAX
		OR  EAX, EAX            ; IF pTSSout is NIL Then go and
		JNZ deRdyFound          ; check the next priority.
		ADD EBX,sQUEUE          ; Point to the next Priority Queue
		LOOP deRdyLoop          ; DEC ECX and LOOP IF NOT ZERO
deRdyFound:
		OR  EAX, EAX            ; IF pTSSout is NIL Then there are
		JZ deRdyDone            ; No TSSs on the RdyQ; RETURN
		DEC _nReady				;
		MOV ECX,[EAX+NextTSS]   ; Otherwise, deQueue the process
		MOV [EBX],ECX           ; And return with the pointer in EAX
deRdyDone:
	    RETN                    ;

;=============================================================================

PUBLIC ChkRdyQ:
;
; INPUT : NONE
; OUTPUT : EAX
; REGISTERS : EAX,EBX,ECX,FLAGS
; MODIFIES : RdyQ
;
; This routine will return a pointer to the highest priority TSS that
; is queued to run. It WILL NOT remove it from the Queue.
; If there was no task queued, EAX is returned as NIL.
;
		MOV ECX,nPRI            ; Set up the number of times to loop
		LEA EBX,RdyQ            ; Get base address of RdyQ in EBX
ChkRdyLoop:
	    MOV EAX,[EBX]           ; Get pTSSout in EAX
		OR  EAX, EAX            ; IF pTSSout is NIL Then go and
		JNZ ChkRdyDone          ; check the next priority.
		ADD EBX,sQUEUE          ; Point to the next Priority Queue
		LOOP ChkRdyLoop       ; DEC ECX and LOOP IF NOT ZERO
ChkRdyDone:
	    RETN                    ;

;=============================================================================
;================= BEGIN NEAR KERNEL HELPER ROUTINES =========================
;=============================================================================

; RemoveRdyJob  (NEAR)
;
; This routine searchs all ready queue priorities for tasks belonging
; to pJCB. When one is found it is removed from the queue
; and the TSS is freed up.  This is called when we are killing
; a job.
;
; Procedureal Interface :
;
;		RemoveRdyJob(char *pJCB):ercType
;
;	pJCB is a pointer to the JCB that the tasks to kill belong to.
;
; pJCB		 	EQU DWORD PTR [EBP+8]
;
; INPUT :  (pJCB on stack)
; OUTPUT : NONE
; REGISTERS : All general registers are trashed
; MODIFIES : RdyQ
;
;
PUBLIC _RemoveRdyJob:
;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV ECX,nPRI            ; Set up the number of times to loop
		LEA EBX,RdyQ            ; Get base address of RdyQ in EBX
	    MOV EDX, [EBP+8]      	; EDX holds pJCB for comparison (RAB)

		;EBX points to begining of next Priority Queue
RemRdyLoop:
	    MOV EAX,[EBX+Head]      ; Get pTSS in EAX
		MOV EDI, EAX			; EDI points to last TSS by default (or NIL)
		OR  EAX,EAX             ; Is pTSS 0 (none left queued here)
		JNZ RemRdy0		        ; Valid pTSS!
RemRdyLoop1:
		MOV [EBX+Tail], EDI		; EDI always points to last TSS or NIL
		ADD EBX,sQUEUE          ; Point to the next Priority Queue
		LOOP RemRdyLoop         ; DEC ECX and LOOP IF NOT ZERO

		XOR EAX, EAX			; No error
		POP EBP
		RETN 4					; All done (clean stack)

		;Go here to dequeue a TSS at head of list
RemRdy0:
		CMP EDX, [EAX+TSS_pJCB]	; Is this from the JCB we want?
		JNE RemRdy2				; No

		MOV EDI, [EAX+NextTSS]  ; Yes, deQueue the TSS
		MOV [EBX+Head], EDI     ; Fix link in Queue list

		PUSH EBX				; Save ptr to RdyQue (crnt priority)

		;Free up the TSS (add it to the free list)
		MOV EBX,pFreeTSS        ; pTSSin^.Next <= pFreeTSS;
		MOV [EAX+NextTSS],EBX   ;
		MOV DWORD PTR [EAX+TSS_pJCB], 0	; Make TSS invalid
		MOV pFreeTSS,EAX        ; pFreeTSS <= pTSSin;
		INC _nTSSLeft			;

		POP EBX
		MOV EAX, EDI 		    ; Make EAX point to new head TSS
		OR EAX, EAX				; Is it Zero?
		JZ RemRdyLoop1			; Next Queue please
		JMP RemRdy0				; back to check next at head of list

		;Go here to dequeue a TSS in middle or end of list
RemRdy2:
		MOV EAX, [EDI+NextTSS]	; Get next link in list
		OR EAX, EAX				; Valid pTSS?
		JZ RemRdyLoop1			; No. Next Queue please
		CMP EDX, [EAX+TSS_pJCB]	; Is this from JCB we want?
		JE RemRdy3				; Yes. Trash it.
		MOV	EDI, EAX			; No. Next TSS
		JMP RemRdy2
RemRdy3:
		;EDI points to prev TSS
		;EAX points to crnt TSS
		;Make ESI point to NextTSS

		MOV ESI, [EAX+NextTSS]  ; Yes, deQueue the TSS

		;Now we fix the list (Make Prev point to Next)
		;This extracts EAX from the list

		MOV [EDI+NextTSS], ESI	;Jump the removed link
		PUSH EBX				;Save ptr to RdyQue (crnt priority)

		;Free up the TSS (add it to the free list)
		MOV EBX,pFreeTSS        ; pTSSin^.Next <= pFreeTSS;
		MOV [EAX+NextTSS],EBX   		;
		MOV DWORD PTR [EAX+TSS_pJCB], 0	; Make TSS invalid
		MOV pFreeTSS,EAX    	    	; pFreeTSS <= pTSSin;
		INC _nTSSLeft					;

		POP EBX
		;
		OR  ESI, ESI			;Is EDI the new Tail? (ESI = 0)
		JZ  RemRdyLoop1			;Yes. Next Queue please
		JMP RemRdy2				;back to check next TSS


;=============================================================================
; GetExchOwner  (NEAR)
;
; This routine returns the owner of the exchange specified.
; A pointer to the JCB of the owner is returned.
; ErcNotAlloc is returned if the exchange isn't allocated.
; ErcOutofRange is returned is the exchange number is invalid (too high)
;
; Procedureal Interface :
;
;		GetExchOwner(long Exch, char *pJCBRet): dErrror
;
;	Exch is the exchange number.
;	pJCBRet is a pointer to the JCB that the tasks to kill belong to.
;
; Exch	 	EQU DWORD PTR [EBP+12]
; pJCBRet 	EQU DWORD PTR [EBP+8]

PUBLIC _GetExchOwner:	        ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV EAX, [EBP+12]		; Get Resp Exchange in EDX
		CMP EAX,nExch           ; Is the exchange out of range?
		JB GEO01	            ; No, continue
		MOV EAX,ErcOutOfRange   ; Yes, Error in EAX register
		JMP GEOEnd				;
GEO01:
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ; sExch * Exch number
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EDX,EAX             ; EDX -> Exch
		MOV EAX, [EDX+Owner]
		OR EAX, EAX				; Valid Exch (Allocated)
		JNZ GEO02
		MOV EAX, ErcNotAlloc	; No, not allocated
		JMP SHORT GEOEnd
GEO02:
		MOV ESI, [EBP+8]		;Where to return pJCB of Exchange
		MOV [ESI], EAX			;
		XOR EAX, EAX
GEOEnd:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN 8                  ;

;=============================================================================
; SetExchOwner  (NEAR)
;
; This routine sets the owner of the exchange specified to the
; pJCB specified. This is used by the Job code to set the owner of
; a TSS exchange to a new JCB (even though the exchange was allocated
; by the OS).  No error checking is done as the job code does it upfront!
;
; Procedureal Interface :
;
;		SetExchOwner(long Exch, char *pNewJCB): dErrror
;
;	Exch is the exchange number.
;	pNewJCB is a pointer to the JCB of the new owner.
;
; Exch	 	EQU DWORD PTR [EBP+12]
; pNewJCB 	EQU DWORD PTR [EBP+8]

PUBLIC _SetExchOwner:			;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX, [EBP+12]		; Exchange Number
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ; sExch * Exch number
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ; EAX -> oExch + prgExch
		MOV EBX, [EBP+8]
		MOV [EAX+Owner], EBX
		XOR EAX, EAX
		POP EBP                 ;
		RETN 8                  ;

;=============================================================================
; SendAbort  (NEAR)
;
; This routine sends one abort message to each valid service
; with the jobnum of the aborting job. If we receive a
; kernel error on Request it may be becuase it is a service
; that is aborting itself. We ignore the kernel errors.
;
; Procedureal Interface :
;
;		SendAbort(long JobNum, ValidExch): dErrror
;
;	JobNum is the job that is aborting
;	ValidExch is any valid exchange so the request will go through
;
; JobNum 	EQU DWORD PTR [EBP+12]
; ValidExch	EQU DWORD PTR [EBP+8]

PUBLIC _SendAbort:              ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV ESI,OFFSET rgSVC	; Get the address of rgSVC
		MOV ECX,nSVC			; Get the number of Service Descriptors
SAB01:
		CMP DWORD PTR [ESI], 0	; Valid name?
		JE SAB05				; NO, next service

		PUSH ESI				;Save count and pointer to SVC name
		PUSH ECX

		;Push all the params to make the request
		PUSH ESI				;pName
		PUSH 0					;Abort Service Code
		MOV EAX, [EBP+8]		;Exchange
		PUSH EAX
		PUSH OFFSET dJunk		;pHandleRet
		PUSH 0					;npSend
		PUSH 0					;pData0
		PUSH 0					;cbData0
		PUSH 0					;pData1
		PUSH 0					;cbData1
		MOV EAX, [EBP+12]		;JobNum
		PUSH EAX				;dData0
		PUSH 0					;dData1
		PUSH 0					;dData2
		CALL FWORD PTR _Request

;RAB
SAB02:
		PUSH DWORD PTR [EBP+8]
		PUSH OFFSET	dmsg
		CALL FWORD PTR _WaitMsg
		MOV EAX, dmsg
		CMP EAX, dJunk		;see if we got got response back!
		JNE SAB02			;NO - wait again
;RAB

		POP ECX
		POP ESI
SAB05:
		ADD ESI, sSVC			;Next Service name
		LOOP SAB01
		XOR EAX, EAX
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN 8                  ;



;=======================================================================
;============== BEGIN PUBLIC KERNEL PRIMITIVES =========================
;=======================================================================
;
; Request - The kernel request primitive sends a message like the Send
; primitive except this function requires several more parameters.
; A system structure called a request block is allocated and some of these
; parameters are placed in it.  A request block is the basic
; structure used for Client-Server communications.  The exchange where a
; request should be queued is determined by searching the system service
; array for a matching request service name specified in the request block.
; The procedural interface to Request looks like this:
;
;    Request(  pSvcName     [EBP+56]
;              wSvcCode     [EBP+52]
;              dRespExch    [EBP+48]
;              pRqHndlRet   [EBP+44]
;              dnpSend      [EBP+40]
;              pData1       [EBP+36]
;              dcbData1     [EBP+32]
;              pData2       [EBP+28]
;              dcbData2     [EBP+24]
;              dData0       [EBP+20]
;              dData1       [EBP+16]
;              dData2       [EBP+12]  ) : dError


PUBLIC __Request:				;
		PUSH EBP				; Save the Previous FramePtr
		MOV EBP,ESP				; Set up New FramePtr

		;Validate service name from registry and get exchange
		MOV EAX, [EBP+56]		;pServiceName
		CALL GetExchange		;Leaves Service Exch in ESI if no errors
		OR  EAX,EAX				;Any errors?
		JZ SHORT Req02			;No
		JMP ReqEnd				;Yes, return error
Req02:
		;Validate exchange
		MOV EDX, [EBP+48]		; Get Resp Exchange in EDX
		CMP EDX,nExch           ; Is the exchange out of range?
		JB Req03	            ; No, continue
		MOV EAX,ercOutOfRange   ; Yes, Error in EAX register
		JMP ReqEnd				;
Req03:
		;Get them a request block
		CLI
		CALL NewRQB				;EAX has ptr to new RqBlk (or 0 if none)
		STI
		OR  EAX, EAX			;Did we get one? (NIL (0) means we didn't)
		JNZ Req04				;Yes. EAX ptr to new RqBlk
		MOV EAX, ErcNoMoreRqBlks ;No, Sorry...
		JMP ReqEnd				;
Req04:
		;ESI still has the exchange for the service
		;EDX still has the response exchange
		;EAX has pRqBlk (Handle)

		MOV EBX, EAX				;EBX now pts to RqBlk
		MOV [EBX+ServiceExch], ESI	;Put Svc Exch into RqBlk
		MOV EAX, [EBP+52]			;Get Svc Code
		MOV [EBX+ServiceCode], AX	;Put Svc Code into RqBlk
		MOV [EBX+RespExch], EDX		;Put Resp Exch into RqBlk
		CALL GetCrntJobNum			;Get crnt JCB (Job Num of owner)
		MOV [EBX+RqOwnerJob], EAX	;put in RqBlk
		MOV EAX, [EBP+20]			;Get dData0
		MOV [EBX+dData0], EAX		;put in RqBlk
		MOV EAX, [EBP+16]			;Get dData1
		MOV [EBX+dData1], EAX		;put in RqBlk
		MOV EAX, [EBP+20]			;Get dData2
		MOV [EBX+dData2], EAX		;put in RqBlk
		MOV EAX, [EBP+36]			;Get pData1
		MOV [EBX+pData1], EAX		;put in RqBlk
		MOV EAX, [EBP+32]			;Get cbData1
		MOV [EBX+cbData1], EAX		;put in RqBlk
		MOV EAX, [EBP+28]			;Get pData2
		MOV [EBX+pData2], EAX		;put in RqBlk
		MOV EAX, [EBP+24]			;Get cbData2
		MOV [EBX+cbData2], EAX		;put in RqBlk
		MOV EAX, [EBP+40]			;Number of Send PbCbs
		CMP EAX, 3					;Must be 2 or less
		JB Req06					;
		MOV EAX, 2
Req06:
		MOV [EBX+npSend], AL		;Put nSend PbCbs into RqBlk
		MOV CL, 2					;Caculate nRecv (2-nSend)
		SUB CL, AL					;Leave in CL
		MOV [EBX+npRecv], CL		;Put npRecv in RqBlk

		;At this point the RqBlk is all filled in.
		;Now we will return the RqBlkHandle to the user.
		;The handle is actually a ptr to the RqBlk but they can't use
		;it as one anyway (so no problem)

        MOV EDI, [EBP+44]			;Ptr to return handle to
        MOV [EDI], EBX				;Give it to them
        MOV EDX, EBX				;Save RqBlk in EDX

		CLI							; No interruptions from here on
		;Now we allocate a Link block to use

		MOV EAX,pFreeLB         	; EAX <= pFreeLB;
		OR EAX,EAX	             	; Is pFreeLB NIL? (out of LBs)
		JNZ Req08   	         	;
		CALL DisposeRQB				; NO... free up RqBlk
		MOV EAX,ercNoMoreLBs    	; Move error in the EAX register
		JMP ReqEnd					; Go home with bad news
Req08:
		MOV EBX,[EAX+NextLB]    	; pFreeLB <= pFreeLB^.Next
		MOV pFreeLB,EBX         	;
		DEC _nLBLeft				;

        MOV DWORD PTR [EAX+LBType],REQLB	; This is a Request Link Block
		MOV DWORD PTR [EAX+NextLB], 0	  	; pLB^.Next <= NIL;
		MOV [EAX+DataLo],EDX    			; RqHandle into Lower 1/2 of Msg
		MOV DWORD PTR [EAX+DataHi], 0 	   	; Store zero in upper half of pLB^.Data
		PUSH EAX                		; Save pLB on the stack

		;ESI still has the exchange Number for the service.
		;The ptr to the exch is required for deQueueTSS so we get it.

        MOV EAX,ESI             ; Exch => EAX
		MOV EDX, sEXCH          ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             ; MAKE ESI <= pExch

		;ESI now points to the exchange

		CALL deQueueTSS         	; DeQueue a TSS on that Exch
		OR  EAX,EAX             	; Did we get one?
		JNZ Req10	              	; Yes, give up the message
		POP EAX                 	; No, Get the pLB just saved
		CALL enQueueMsg         	; EnQueue the Message on Exch
		XOR EAX,EAX					; No Error
		JMP SHORT ReqEnd           	; And get out!
Req10:
        POP EBX         	        ; Get the pLB just saved into EBX
		MOV [EAX+pLBRet],EBX	    ; and put it in the TSS
		CALL enQueueRdy         	; EnQueue the TSS on the RdyQ
		MOV EAX,pRunTSS    			; Get the Ptr To the Running TSS
		CALL enQueueRdy         	; and put him on the RdyQ
		CALL deQueueRdy         	; Get high priority TSS off the RdyQ
		CMP EAX,pRunTSS         	; If the high priority TSS is the
		JNE Req12	            	; same as the Running TSS then return
        XOR EAX,EAX             	; Return to Caller with erc ok.
		JMP SHORT ReqEnd
Req12:
        MOV pRunTSS,EAX         ; Make the TSS in EAX the Running TSS
		MOV BX,[EAX+Tid]        ; Get the task Id (TR)
		MOV TSS_Sel,BX          ; Put it in the JumpAddr for Task Swtich
		INC _nSwitches			; Keep track of how many swtiches for stats
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]     ; JMP TSS (This is the task swtich)
        XOR EAX,EAX             ; Return to Caller with erc ok.
ReqEnd:
		STI                     ;
		MOV ESP,EBP				;
		POP EBP					;
		RETF 48					; Rtn to Caller & Remove Params from stack

;=============================================================================
; The response primitive is used by system services to respond to a
; Request received at their service exchange.  The RqBlk handle must be
; supplied along with the error/status code to be returned to the
; caller.  This is very similar to Send except is dealiases addresses
; in the RqBlk and then deallocates it.  The exchange to respond to
; is located inside the RqBlk.
; If dStatRet is ErcOwnerAbort, simply return the Reqest Block
; to the free pool and return Erc 0 to caller.
;     Respond(dRqHndl, dStatRet): dError
;
;
dRqHndl	 EQU DWORD PTR [EBP+16]
dStatRet EQU DWORD PTR [EBP+12]

PUBLIC __Respond: 				;
		PUSH EBP				; Save Callers Frame
		MOV EBP,ESP				; Setup Local Frame
;RAB
		MOV EAX, dRqHndl		; pRqBlk into EAX
		MOV EBX, dStatRet
		CMP EBX, ErcOwnerAbort	;
		JNE Resp01
		CLI						; No interruptions
		CALL DisposeRQB			; Return Aborted RQB to pool.
		XOR EAX, EAX            ; No Error
		JMP RespEnd				; Get out
Resp01:
;RAB
		MOV ESI, [EAX+RespExch] ; Response Exchange into ESI
		CMP ESI,nExch           ; Is the exchange out of range?
		JNAE Resp02             ; No, continue
		MOV EAX,ercOutOfRange   ; Error into the EAX register.
		JMP RespEnd				; Get out
Resp02:
        MOV EAX,ESI             ; Exch => EAX
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             ; MAKE ESI <= pExch
		CMP DWORD PTR [EAX+Owner], 0    ; If the exchange is not allocated
		JNE Resp04              ; return to the caller with error
		MOV EAX,ercNotAlloc     ; in the EAX register.
		JMP RespEnd				;
Resp04:
		MOV EAX, dRqHndl        ; Get Request handle into EBX (pRqBlk)
		MOV EBX, [EAX+RqOwnerJob]
		CALL GetCrntJobNum
		CMP EAX, EBX
		JE Resp06				;Same job - no DeAlias needed

		MOV EAX, dRqHndl        ; Get Request handle into EBX (pRqBlk)
		MOV EBX, [EAX+cbData1]	;
		OR EBX, EBX
		JZ Resp05				;No need to dealias (zero bytes)
		MOV EDX, [EAX+pData1]
		OR EDX, EDX
		JZ Resp05				;Null pointer!

		PUSH ESI				;Save pExch across call

		PUSH EDX				;pMem
		PUSH EBX				;cbMem
		CALL GetCrntJobNum
		PUSH EAX
		CALL FWORD PTR _DeAliasMem	;DO it and ignore errors
		POP ESI					;get pExch back
Resp05:
		MOV EAX, dRqHndl        ; Get Request handle into EBX (pRqBlk)
		MOV EBX, [EAX+cbData2]	;
		OR EBX, EBX
		JZ Resp06				;No need to dealias (zero bytes)
		MOV EDX, [EAX+pData2]
		OR EDX, EDX
		JZ Resp06				;Null pointer!
		PUSH ESI				;Save pExch across call

		PUSH EDX				;pMem
		PUSH EBX				;cbMem
		CALL GetCrntJobNum		;
		PUSH EAX
		CALL FWORD PTR _DeAliasMem	;DO it and ignore errors
		POP ESI					;get pExch back
Resp06:
		MOV EAX, dRqHndl        ; Get Request handle into EBX (pRqBlk)
		CLI						; No interruptions
		CALL DisposeRQB			; Return Rqb to pool. Not needed anymore

		; Allocate a link block
		MOV EAX,pFreeLB         ; NewLB <= pFreeLB;
		OR EAX,EAX              ; IF pFreeLB=NIL THEN No LBs;
		JNZ Resp07              ;
		MOV EAX,ercNoMoreLBs    ; caller with error in the EAX register
		JMP RespEnd
Resp07:
		MOV EBX,[EAX+NextLB]    ; pFreeLB <= pFreeLB^.Next
		MOV pFreeLB,EBX         ;
		DEC _nLBLeft			;

        MOV DWORD PTR [EAX+LBType], RESPLB ; This is a Response Link Block
		MOV DWORD PTR [EAX+NextLB], 0      ; pLB^.Next <= NIL;
		MOV EBX, dRqHndl        ; Get Request handle into EBX
		MOV [EAX+DataLo],EBX    ; Store in lower half of pLB^.Data
		MOV EBX, dStatRet       ; Get Status/Error into EBX
		MOV [EAX+DataHi],EBX    ; Store in upper half of pLB^.Data
		PUSH EAX                ; Save pLB on the stack
		CALL deQueueTSS         ; DeQueue a TSS on that Exch
		OR  EAX,EAX             ; Did we get one?
		JNZ Resp08              ; Yes, give up the message
		POP EAX                 ; Get the pLB just saved
		CALL enQueueMsg         ; EnQueue the Message on Exch
		XOR EAX, EAX			; No Error
		JMP SHORT RespEnd       ; And get out!
Resp08:
        POP EBX                 ; Get the pLB just saved into EBX
		MOV [EAX+pLBRet],EBX    ; and put it in the TSS
		CALL enQueueRdy         ; EnQueue the TSS on the RdyQ
		MOV EAX,pRunTSS         ; Get the Ptr To the Running TSS
		CALL enQueueRdy         ; and put him on the RdyQ
		CALL deQueueRdy         ; Get high priority TSS off the RdyQ

		CMP EAX,pRunTSS         ; If the high priority TSS is the
		JNE Resp10              ; same as the Running TSS then return
		XOR EAX,EAX             ; Return to Caller with erc ok.
		JMP SHORT RespEnd		;
Resp10:
        MOV pRunTSS,EAX         ; Make the TSS in EAX the Running TSS
		MOV BX,[EAX+Tid]        ; Get the task Id (TR)
		MOV TSS_Sel,BX          ; Put it in the JumpAddr
		INC _nSwitches
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]     ; JMP TSS
        XOR EAX,EAX             ; Return to Caller with erc ok.
RespEnd:
		STI
		MOV ESP,EBP				;
		POP EBP					;
		RETF 8					; Rtn to Caller & Remove Params

;=============================================================================
;
; MoveRequest - The kernel Move Request primitive.
; This allows a service to move a request to another exchange it owns.
; This can not be used to forward a request to another service or Job.
; It is very similar to send except it checks to ensure the destination
; Exchange is owned by the sender.
;
; Procedural Interface :
;
;      MoveRequest(dRqBlkHndl, DestExch):ercType
;
;           dqMsg is the handle of the RqBlk to forward.
;           DestExch the exchange to where the Request should be sent.
;
;
;dRqBlkHndl     EQU [EBP+16]
;DestExch		EQU [EBP+12]

PUBLIC __MoveRequest:            ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV ESI, [EBP+12]       ; Get Exchange Parameter in ESI
		CMP ESI,nExch           ; Is the exchange is out of range
		JNAE MReq02             ; No, continue
		MOV EAX,ercOutOfRange   ; in the EAX register.
		JMP MReqEnd				; Get out
MReq02:
        MOV EAX,ESI             ; Exch => EAX
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             ; MAKE ESI <= pExch
		MOV EDX, [EAX+Owner]	; Put exch owner into EDX (pJCB)
		CALL GetpCrntJCB		; Leaves it in EAX (uses only EAX)
		CMP EDX, EAX		    ; If the exchange is not owned by sender
		JE  MReq04              ; return to the caller with error
		MOV EAX, ErcNotOwner    ; in the EAX register.
		JMP MReqEnd				; Get out
MReq04:
		CLI						; No interruptions from here on
		; Allocate a link block
		MOV EAX,pFreeLB         ; NewLB <= pFreeLB;
		OR EAX,EAX              ; IF pFreeLB=NIL THEN No LBs;
		JNZ MReq08              ;
		MOV EAX,ercNoMoreLBs    ; caller with error in the EAX register
		JMP MReqEnd				; Go home with bad news
MReq08:
		MOV EBX,[EAX+NextLB]    ; pFreeLB <= pFreeLB^.Next
		MOV pFreeLB,EBX         ;
		DEC _nLBLeft			;

        MOV DWORD PTR [EAX+LBType], REQLBA	; Request Link Block (ALIASED! RAB)
		MOV DWORD PTR [EAX+NextLB], 0    	; pLB^.Next <= NIL;
		MOV EBX, [EBP+16]					; RqHandle
		MOV [EAX+DataLo],EBX    			; RqHandle into Lower 1/2 of Msg
		MOV DWORD PTR [EAX+DataHi], 0 	   	; Store zero in upper half of pLB^.Data
		PUSH EAX                	; Save pLB on the stack
		CALL deQueueTSS         	; DeQueue a TSS on that Exch
		OR  EAX,EAX             	; Did we get one?
		JNZ MReq10	              	; Yes, give up the message
		POP EAX                 	; Get the pLB just saved
		CALL enQueueMsg         	; EnQueue the Message on Exch ESI
		XOR EAX, EAX
		JMP SHORT MReqEnd          	; And get out!
MReq10:
        POP EBX         	        ; Get the pLB just saved into EBX
		MOV [EAX+pLBRet],EBX	    ; and put it in the TSS
		CALL enQueueRdy         	; EnQueue the TSS on the RdyQ
		MOV EAX,pRunTSS    			; Get the Ptr To the Running TSS
		CALL enQueueRdy         	; and put him on the RdyQ
		CALL deQueueRdy         	; Get high priority TSS off the RdyQ
		CMP EAX,pRunTSS         	; If the high priority TSS is the
		JNE MReq12	            	; same as the Running TSS then return
		XOR EAX,EAX		           	; Return to Caller with erc ok.
		JMP SHORT MReqEnd
MReq12:
        MOV pRunTSS,EAX         ; Make the TSS in EAX the Running TSS
		MOV BX,[EAX+Tid]        ; Get the task Id (TR)
		MOV TSS_Sel,BX          ; Put it in the JumpAddr for Task Swtich
		INC _nSwitches			; Keep track of how many swtiches for stats
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]     ; JMP TSS (This is the task switch)
        XOR EAX,EAX             ; Return to Caller with erc ok.
MReqEnd:
		STI                     ;
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ;

;=============================================================================
;
; SendMsg - The kernel send primitive. This	sends a non-specific message
; from a running task to an exchange. This may cause a task swtich if
; a task is waiting at the exchange and it is of equal or higher priority
; that the task that sent the message.
;
; Procedural Interface :
;
;       SendMsg(exch, dMsg1, dMsg2):ercType
;
;           exch is a DWORD (4 BYTES) containing the exchange to where the
;           message should be sent.
;
;           dMsg1 & dMsg2 are DWord values defined and understood
;			only by the sending and receiving tasks.
;
SendExchange   EQU [EBP+14h]
MessageHi      EQU DWORD PTR [EBP+10h]
MessageLo      EQU DWORD PTR [EBP+0Ch]

PUBLIC __SendMsg:                       ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV ESI,SendExchange    ; Get Exchange Parameter in ESI
		CMP ESI,nExch           ; If the exchange is out of range
		JNAE Send00             ; the return to caller with error
		MOV EAX,ercOutOfRange   ; in the EAX register.
		JMP SendEnd
Send00:
        MOV EAX,ESI             ; Exch => EAX
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             ; MAKE ESI <= pExch
		CMP DWORD PTR [EAX+Owner], 0    ; If the exchange is not allocated
		JNE Send01              ; return to the caller with error
		MOV EAX,ercNotAlloc     ; in the EAX register.
		JMP SendEnd				;
Send01:
		CLI						; No interrupts
		; Allocate a link block
		MOV EAX,pFreeLB         ; NewLB <= pFreeLB;
		OR EAX,EAX              ; IF pFreeLB=NIL THEN No LBs;
		JNZ SHORT Send02        ;
		MOV EAX,ercNoMoreLBs    ; caller with error in the EAX register
		JMP SHORT MReqEnd		; Go home with bad news
Send02:
		MOV EBX,[EAX+NextLB]    ; pFreeLB <= pFreeLB^.Next
		MOV pFreeLB,EBX         ;
		DEC _nLBLeft			;

        MOV DWORD PTR [EAX+LBType], DATALB ; This is a Data Link Block
		MOV DWORD PTR [EAX+NextLB], 0      ; pLB^.Next <= NIL;
		MOV EBX,MessageLo       ; Get lower half of Msg in EBX
		MOV [EAX+DataLo],EBX    ; Store in lower half of pLB^.Data
		MOV EBX,MessageHi       ; Get upper half of Msg in EBX
		MOV [EAX+DataHi],EBX    ; Store in upper half of pLB^.Data

		PUSH EAX                ; Save pLB on the stack

		CLI						; No interrupts
		CALL deQueueTSS         ; DeQueue a TSS on that Exch
		STI
		OR  EAX,EAX             ; Did we get one?
		JNZ Send25              ; Yes, give up the message
		POP EAX                 ; Get the pLB just saved
		CLI						; No interrupts
		CALL enQueueMsg         ; EnQueue the Message on Exch
		JMP Send04              ; And get out (Erc 0)!
Send25:
        POP EBX                 ; Get the pLB just saved into EBX
		CLI						; No interrupts
		MOV [EAX+pLBRet],EBX    ; and put it in the TSS
		CALL enQueueRdy         ; EnQueue the TSS on the RdyQ
		MOV EAX,pRunTSS         ; Get the Ptr To the Running TSS
		CALL enQueueRdy         ; and put him on the RdyQ
		CALL deQueueRdy         ; Get high priority TSS off the RdyQ
		CMP EAX,pRunTSS         ; If the high priority TSS is the
		JNE Send03              ; same as the Running TSS then return
		JMP SHORT Send04		; Return with ErcOk

Send03:
        MOV pRunTSS,EAX         ; Make the TSS in EAX the Running TSS
		MOV BX,[EAX+Tid]        ; Get the task Id (TR)
		MOV TSS_Sel,BX          ; Put it in the JumpAddr
		INC _nSwitches
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]     ; JMP TSS
Send04:
        XOR EAX,EAX             ; Return to Caller with erc ok.
SendEnd:
		STI                     ;
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12                 ;

;=============================================================================
;
; ISendMsg - The OS Interrupt Send primitive.
;  This procedure allows an ISR to send a message to an exchange.
;  This is the same as SendMsg except NO task switch is
;  performed. If a task is waiting at the exchange, the message is
;  associated (linked) with it and it is moved to the RdyQ.
;  It will get a chance to run the next time the RdyQ is evaluated
;  by the Kernel which will probably be by the timer interrupt slicer.
;  Interrupt tasks can use ISendMsg to send single or multiple messages
;  to exchanges during their execution.
;  Interrupts are CLEARED on entry and WILL NOT BE SET on exit!!!
;  It is the responsibility of the caller to set them if desired.
;  ISendMsg is intended only to be used by ISRs in device drivers
;
;
; Procedural Interface :
;
;       ISendMsg(exch, dMsg1, dMsg2):ercType
;
;           exch is a DWORD (4 BYTES) containing the exchange to where the
;           message should be sent.
;
;           dMsg1 and dMsg2 are DWORD messages.
;
; Parameters on stack are the same as _SendMsg.

PUBLIC __ISendMsg: 			            ;
		CLI                     ;INTS ALWAYS CLEARED AND LEFT THAT WAY!
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV ESI,SendExchange    ; Get Exchange Parameter in ESI
		CMP ESI,nExch           ; If the exchange is out of range
		JNAE ISend00            ; then return to caller with error
		MOV EAX,ercOutOfRange   ; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12                 ;
ISend00:
        MOV EAX,ESI             ; Exch => EAX
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             		; MAKE ESI <= pExch
		CMP DWORD PTR [EAX+Owner], 0	; If the exchange is not allocated
		JNE ISend01             		; return to the caller with error
		MOV EAX,ercNotAlloc     ; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12                 ;
ISend01:
		; Allocate a link block
		MOV EAX,pFreeLB         ; NewLB <= pFreeLB;
		OR EAX,EAX              ; IF pFreeLB=NIL THEN No LBs;
		JNZ SHORT ISend02        ;
		MOV EAX,ercNoMoreLBs    ; caller with error in the EAX register
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12                 ;
ISend02:
		MOV EBX,[EAX+NextLB]    ; pFreeLB <= pFreeLB^.Next
		MOV pFreeLB,EBX         ;
		DEC _nLBLeft			;

        MOV DWORD PTR [EAX+LBType], DATALB ; This is a Data Link Block
		MOV DWORD PTR [EAX+NextLB],0      ; pLB^.Next <= NIL;
		MOV EBX,MessageLo       ; Get lower half of Msg in EBX
		MOV [EAX+DataLo],EBX    ; Store in lower half of pLB^.Data
		MOV EBX,MessageHi       ; Get upper half of Msg in EBX
		MOV [EAX+DataHi],EBX    ; Store in upper half of pLB^.Data
		PUSH EAX                ; Save pLB on the stack
		CALL deQueueTSS         ; DeQueue a TSS on that Exch
		OR  EAX,EAX             ; Did we get one?
		JNZ ISend03             ; Yes, give up the message
		POP EAX                 ; No, Get the pLB just saved
		CALL enQueueMsg         ; EnQueue the Message on Exch
		JMP ISend04             ; And get out!
ISend03:
        POP EBX                 ; Get the pLB just saved into EBX
		MOV [EAX+pLBRet],EBX    ; and put it in the TSS
		CALL enQueueRdy         ; EnQueue the TSS on the RdyQ
ISend04:
        XOR EAX,EAX             ; Return to Caller with erc ok.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12                 ;

;=============================================================================
;
; Wait - The kernel wait primitive. This procedure allows a task to
;  receive information from another task from an exchange.  If no
;  message is at the exchange, the task is placed on the exchange
;  and the ReadyQ is reevaluated to make the next tast run.
;
; A result code is returned in the EAX register.
;
; Procedural Interface :
;
;       Wait(exch,pdqMsgRet):ercType
;
;           exch is a DWORD (4 BYTES) containing the exchange to where the
;           message should be sent.
;
;           pMessage is a pointer to an 8 byte area where the
;           message is stored.
;
WaitExchange	EQU [EBP+10h]
pMessage		EQU [EBP+0Ch]
;
;
PUBLIC __WaitMsg:		             	;
		PUSH EBP               	;
		MOV EBP,ESP            	;
		MOV ESI,WaitExchange   	; Get Exchange Parameter in ESI
		CMP ESI,nExch          	; If the exchange is out of range
		JNAE Wait00             	; the return to caller with error
		MOV EAX,ercOutOfRange  	; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8					;
Wait00:
		MOV EAX,ESI             ; ExchId => EAX
		MOV EBX,sEXCH           ; Compute offset of ExchId in rgExch
		MUL EBX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             ; Put Exch in to ESI
		CMP DWORD PTR [EAX+Owner], 0   ; If the exchange is not allocated
		JNE Wait01              ; return to the caller with error
		MOV EAX,ercNotAlloc     ; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ;
Wait01:
		CLI                    	;
		CALL deQueueMsg         ; EAX <= pLB from pExch (ESI)
		OR  EAX,EAX             ; If no message (pLB = NIL) Then
		JZ Wait02               ; Wait for Message Else
		JMP Wait05              ; Get Message and Return

Wait02:
		MOV EAX,pRunTSS         ; Get pRunTSS in EAX to Wait

		;This next section of code Queues up the TSS pointed to
		;by EAX on the exchange pointed to by ESI
		; (i.e., we make the current task "wait")

		MOV DWORD PTR [EAX+NextTSS], 0  ; pTSSin^.Next <= NIL;
		XCHG ESI,EAX            		; pExch => EAX, pTSSin => ESI
		CMP DWORD PTR [EAX+EHead], 0    ; if ..TSSHead = NIL
		JNE Wait025						; then
		MOV [EAX+EHead],ESI     ;  ..TSSHead <= pTSSin;
		MOV [EAX+ETail],ESI     ;  ..TSSTail <= pTSSin;
		MOV DWORD PTR [EAX+fEMsg], 0		; Flag it as a TSS (vice a Msg)
		XCHG ESI,EAX            ; Make ESI <= pExch Again
		JMP SHORT Wait03                  ; else
Wait025:
	    MOV EDX,[EAX+ETail]     ;  ..TSSTail^.NextTSS <= pTSSin;
		MOV [EDX+NextTSS],ESI   ;
		MOV [EAX+ETail],ESI     ;  ..TSSTail <= pTSSin;
		MOV DWORD PTR [EAX+fEMsg], 0		; Flag it as a TSS (vice a Msg)
		XCHG ESI,EAX            ; Make ESI <= pExch Again

		;We just placed the current TSS on an exchange,
		;now we get the next TSS to run (if there is one)

Wait03:
		CALL deQueueRdy         ; Get highest priority TSS off the RdyQ
		OR EAX, EAX				; Anyone ready to run?
		JNZ Wait035             ; Yes (jump to check pTSS)

		MOV EDI, 1
		MOV dfHalted, EDI
		INC _nHalts
		STI                     ; No, then HLT CPU until ready
		HLT                     ; Halt CPU and wait for interrupt
		CLI                     ; An interrupt has occured. Clear Interrupts
		XOR EDI,EDI
		MOV dfHalted, EDI
		JMP Wait03              ; Check for a task to switch to

Wait035:
		CMP EAX,pRunTSS         ; Same one as before???
		JE Wait04               ; You bet! NO SWITCH!!!

		;Now we switch tasks by placing the address of the
		;new TSS in pRunTSS and jumping to it.  This forces
		;a 386 task switch.

		MOV pRunTSS,EAX 		; Make high priority TSS Run.
		MOV BX,[EAX+Tid]		;
		MOV TSS_Sel,BX			;
		INC _nSwitches
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]		; JUMP TSS (Switch Tasks)

		; A task has just finished "Waiting"
		; We are now in the new task with its memory space
		; (or the same task if he was high pri & had a msg)
		; If this is a system service it may need RqBlk address aliases
		; If it is an OS service we alias in OS memory!

Wait04:
		MOV EDX,pRunTSS         ; Put the TSS in EAX into EDX
		MOV EAX,[EDX+pLBRet]    ; Get the pLB in EAX
Wait05:
		; if we got here, we have either switched tasks
		; and we are delivering a message (or Req) to the new task,
		; or the there was a message waiting at the exch of
		; the first caller and we are delivering it.
		; Either way, the message is already deQueued from
		; the exch and the critical part of WaitMsg is over.
		; We can start interrupts again except when we have
		; to return the Link Block to the pool (free it up)

		STI									; WE CAN RESTART INTERRUPTS HERE
		CMP DWORD PTR [EAX+LBType],REQLB	; Is the link block a Req Link Block?
		JNE Wait06							; No, Treat it as a data link block

		;pLB.DataLo is RqHandle (pRqBlk)

		PUSH EAX				; Save ptr to Link Block
		MOV EBX,[EAX+DataLo]    ; Get pRqBlk into EBX

		;Now we set up to alias the memory for the service
		; (Alias the 2 Pointers in the RqBlk)
		;_AliasMem(pMem, dcbMem, dJobNum, ppAliasRet): dError

		MOV ECX, [EBX+cbData1]		;
		OR ECX, ECX					;is cbData1 0?
		JZ Wait051					;Yes

		MOV EAX, [EBX+pData1]		;
		OR EAX, EAX					;is pData1 NULL?
		JZ Wait051					;Yes

									;Set up params for AliasMem
		PUSH EAX					;pMem
		PUSH ECX					;cbMem
		MOV EAX, [EBX+RqOwnerJob]
		PUSH EAX					;dJobNum
		ADD EBX, pData1				;Offset to pData1 in RqBlk
		PUSH EBX					;Linear Address of pData1
		CALL FWORD PTR _AliasMem
		OR EAX, EAX					;Error??
		JZ Wait051					;No, continue
		POP EBX						;Make stack right
		MOV ESP,EBP             	;Return Error...
		POP EBP                 	;
		RETF 8                  	;

Wait051:							;Second Pointer (pData2)
		POP EAX						;Restore ptr to Link Block
		PUSH EAX					;Save again
		MOV EBX,[EAX+DataLo]    	; Get pRqBlk into EBX

		MOV ECX, [EBX+cbData2]		;
		OR ECX, ECX					;is cbData2 0?
		JZ Wait052					;Yes

		MOV EAX, [EBX+pData2]		;
		OR EAX, EAX					;is pData2 NULL?
		JZ Wait052					;Yes
									;Set up params for AliasMem
		PUSH EAX					;pMem
		PUSH ECX					;cbMem
		MOV EAX, [EBX+RqOwnerJob]
		PUSH EAX					;dJobNum
		ADD EBX, pData2				;Offset to pData2 in RqBlk
		PUSH EBX					;Linear Address of PData1
		CALL FWORD PTR _AliasMem
		OR EAX, EAX					;Error??
		JZ Wait052					;No, continue
		POP EBX						;Make stack right
		MOV ESP,EBP             	;Return Error...
		POP EBP                 	;
		RETF 8                  	;

Wait052:
		POP EAX					;Restore ptr to Link Block
Wait06:
		MOV EBX,[EAX+DataLo]    ; Get pLB^.Data into ECX:EBX
		MOV ECX,[EAX+DataHi]    ;
		MOV EDX,pMessage		; Get Storage Addr in EDX
		MOV [EDX],EBX			; Put pLB^.Data in specified
		MOV [EDX+4],ECX 	    ; memory space (EDX)

		;Return the LB to the pool
		CLI
		MOV EBX,pFreeLB         ; pLBin^.Next <= pFreeLB;
		MOV [EAX+NextLB],EBX    ;
		MOV pFreeLB,EAX         ; pFreeLB <= pLBin;
		INC _nLBLeft			;
		STI                     ;
		XOR EAX,EAX             ; ErcOK! (0)
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ;

;
;=============================================================================
;
; CheckMsg - The kernel Check primitive. This procedure provides access
; to the operating system by allowing a task to receive information
; from another process WITHOUT BLOCKING. In other words, if no message is
; available Check returns to the caller. If a message IS available it
; is returned to the caller immediately. The caller is never placed on
; an exchange and the RdyQ is not evaluated.
;
; A result code is returned in the EAX register.
;
; Procedureal Interface :
;
;       CheckMsg(exch,pdqMsg):ercType
;
;           exch is a DWORD (4 BYTES) containing the exchange to where the
;           message should be sent.
;
;           pdqMsg is a pointer to an 8 byte area where the message is stored.
;
ChkExchange   EQU [EBP+10h]
pCkMessage    EQU [EBP+0Ch]

PUBLIC __CheckMsg:               ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV ESI,ChkExchange     ; Get Exchange Parameter in ESI
		CMP ESI,nExch           ; If the exchange is out of range
		JNAE Chk01              ; the return to caller with error
		MOV EAX,ercOutOfRange   ; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ;
Chk01:
		MOV EAX,ESI             ; Exch => EAX
		MOV EBX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EBX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ESI,EAX             		; Put pExch in to ESI
		CMP DWORD PTR [EAX+Owner], 0	; If the exchange is not allocated
		JNE Chk02               		; return to the caller with error

		MOV EAX,ercNotAlloc     ; in the EAX register.
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8					;
Chk02:
		CLI                     ; Can't be interrupted
		CALL deQueueMsg         ; EAX <= pLB from pExch (ESI)
		OR  EAX,EAX             ; If pLB = NIL Then
		JNZ Chk03               ; Go to get msg and return

		STI                     ;
		MOV EAX,ercNoMsg        ; return with erc no msg
		MOV ESP,EBP             ;
		POP EBP					;
		RETF 8					;
Chk03:
		STI						;We can be interrupted again

		CMP DWORD PTR [EAX+LBType],REQLB	; Is the link block a Req Link Block?
		JNE Chk04				; No, Treat it as a data link block

		;pLB.DataLo is RqHandle (pRqBlk)

		PUSH EAX				; Save ptr to Link Block
		MOV EBX,[EAX+DataLo]    ; Get pRqBlk into EBX

		;Now we set up to alias the memory for the service
		; (Alias the 2 Pointers in the RqBlk)
		;_AliasMem(pMem, dcbMem, dJobNum, ppAliasRet): dError

		MOV ECX, [EBX+cbData1]		;
		OR ECX, ECX					;is cbData1 0?
		JZ Chk031					;Yes

		MOV EAX, [EBX+pData1]		;
		OR EAX, EAX					;is pData1 NULL?
		JZ Chk031					;Yes
									;Set up params for AliasMem
		PUSH EAX					;pMem
		PUSH ECX					;cbMem
		MOV EAX, [EBX+RqOwnerJob]
		PUSH EAX					;dJobNum
		ADD EBX, pData1				;Offset to pData1 in RqBlk
		PUSH EBX					;Linear Address of pData1
		CALL FWORD PTR _AliasMem
		OR EAX, EAX					;Error??
		JZ Chk031					;No, continue
		POP EBX						;Make stack right
		MOV ESP,EBP             	;Return Error...
		POP EBP                 	;
		RETF 8                  	;

Chk031:								;Second Pointer (pData2)
		POP EAX						;Restore ptr to Link Block
		PUSH EAX					;Save again
		MOV EBX,[EAX+DataLo]    	; Get pRqBlk into EBX

		MOV ECX, [EBX+cbData2]		;
		OR ECX, ECX					;is cbData2 0?
		JZ Chk032					;Yes

		MOV EAX, [EBX+pData2]		;
		OR EAX, EAX					;is pData2 NULL?
		JZ Chk032					;Yes
									;Set up params for AliasMem
		PUSH EAX					;pMem
		PUSH ECX					;cbMem
		MOV EAX, [EBX+RqOwnerJob]
		PUSH EAX					;dJobNum
		ADD EBX, pData2				;Offset to pData2 in RqBlk
		PUSH EBX					;Linear Address of PData1
		CALL FWORD PTR _AliasMem
		OR EAX, EAX					;Error??
		JZ Chk032					;No, continue
		POP EBX						;Make stack right
		MOV ESP,EBP             	;Return Error...
		POP EBP                 	;
		RETF 8                  	;

Chk032:
		POP EAX						;Restore Ptr to Link Block

Chk04:
        MOV EBX,[EAX+DataLo]    ; Get pLB^.Data into ECX:EBX
		MOV ECX,[EAX+DataHi]    ;
		MOV EDX,pCkMessage		; Get Storage Addr in EDX
		MOV [EDX],EBX        	; Put pLB^.Data in specified
		MOV [EDX+4],ECX			; memory space (EDX)

		;Return the LB to the pool
		CLI
		MOV EBX,pFreeLB         ; pLBin^.Next <= pFreeLB;
		MOV [EAX+NextLB],EBX    ;
		MOV pFreeLB,EAX         ; pFreeLB <= pLBin;
		INC _nLBLeft			;
		STI                     ;

		XOR EAX,EAX             ;ErcOK! (0)
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8					;

;====================================================================
; NewTask --- OS PUBLIC - Creates a new task and schedules it for execution.
; used primarily to create a task for a job other than the one you are in.
; The OS uses this to create the initial task for a newly loaded job.
;
; The OS stacks are preallocated as part of the TSS.  If this is an OS
; task (CodeSeg = 8) then we load the TSS Stack into TSS_ESP and
; TSS_ESP0, otherwise we take the ESP param and place it into TSS_ESP.
;
;  Procedural interface:
;
;	NewTask(JobNum, CodeSeg, Priority, fDebug, Exch, ESP, EIP): dErcRet
;
;
NTS_Job		EQU [EBP+36]		;Job Num for this task
NTS_CS		EQU [EBP+32]		;8 for OS, 18h for user task
NTS_Pri		EQU [EBP+28]		;Priority of this task
NTS_fDbg	EQU [EBP+24]		;TRUE for DEBUGing
NTS_Exch	EQU [EBP+20]		;Exchange for TSS
NTS_ESP		EQU [EBP+16]		;Initial stack pointer
NTS_EIP		EQU [EBP+12]		;Task start address

PUBLIC __NewTask:                    ;
		PUSH EBP                    ;
		MOV EBP,ESP                 ;

        MOV EDX, NTS_Pri			;
		CMP EDX, nPRI-1   			;Priority OK?
		JBE NT0000
		MOV EAX,ercBadPriority
		JMP NTEnd
NT0000:
		MOV ECX, NTS_Exch
        CMP ECX, nExch 				;Exch in range?
		JBE NT0001
		MOV EAX,ercOutOfRange
		JMP NTEnd
NT0001:
		CLI                     ;we can't be interrupted
		MOV EAX,pFreeTSS        ; NewTSS <= pFreeTSS;
		OR EAX,EAX              ; IF pFreeTSS=NIL THEN Return;
		JNZ NT0002              ;
		MOV EAX,ercNoMoreTSSs       ;No...
		JMP NTEnd
NT0002:
		MOV EBX,[EAX+NextTSS]   ; pFreeTSS <= pFreeTSS^.Next
		MOV pFreeTSS,EBX        ;
		DEC _nTSSLeft			;
		STI

		;EAX now has pNewTSS

		MOV [EAX+Priority],DL       ;put Priority into TSS
		MOV DWORD PTR [EAX+TSS_EFlags],0202h  ;Load the Flags Register
		MOV [EAX+TSS_Exch], ECX		;Put new Exch in TSS (ECX is free)
		MOV EBX, NTS_EIP			;mov EIP into TSS (Start Address)
		MOV [EAX+TSS_EIP],EBX
		MOV EBX, NTS_ESP			;mov ESP into TSS
		MOV [EAX+TSS_ESP],EBX
		MOV [EAX+TSS_ESP0],EBX		;
		MOV ECX, NTS_CS				;mov CS into TSS
		MOV [EAX+TSS_CS],CX

		PUSH EAX					;Save pNewTSS

		;Now we get pJCB from JobNum they passed in so we can
		;get the PD from the JCB

		MOV EAX, NTS_Job			;Set up to call GetpJCB
		CALL GetpJCB				;EAX now has pJCB
		MOV ECX, EAX				;ECX now has pJCB

		POP EAX						;Restore pNewTSS to EAX

		MOV [EAX+TSS_pJCB],ECX		;Put pJCB into TSS
		MOV EBX, [ECX+JcbPD]        ;Set up to call LinToPhy

		PUSH EAX					;Save pNewTSS again

		MOV EAX, NTS_Job			;
		CALL LinToPhy				;Get Physical Address for PD into EAX
		MOV EBX, EAX
		POP EAX						;pNewTSS into EAX
		MOV [EAX+TSS_CR3],EBX		;Put Physical Add for PD into TSS_CR3
		CMP DWORD PTR NTS_fDbg, 0	;Debug on entry?
		JE NT0004                   ;No
		MOV WORD PTR [EAX+TSS_TrapBit], 1    ;Yes
NT0004:

		MOV EBX, NTS_Pri			;Get priority of new task

		CLI                         ;We can't be interrupted
		MOV EDX,pRunTSS        		    ;Get who's running
		CMP BYTE PTR [EDX+Priority],BL  ;Who got the highest Pri?
		JA NT0005                   	;New guy does (lowest num)
		CALL enQueueRdy             ;Just put new guy on the ReadyQue (EAX)
		XOR EAX,EAX                 ;ercOk
		JMP NTEnd                   ;Return to caller
NT0005:
        XCHG EAX,EDX                ;CrntTSS -> EAX, New TSS -> EDX
        PUSH EDX					;Save New TSS
		CALL enQueueRdy             ;
		POP EAX						;New TSS -> EAX
		MOV pRunTSS,EAX             ;Move new TSS into pRunTSS
		MOV BX,[EAX+Tid]            ;Put Selector/Offset in "TSS"
		MOV TSS_Sel,BX              ;
		INC _nSwitches
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]         ;Jump to new TSS
		XOR EAX,EAX                 ;ErcOk
NTEnd:
		STI                         ;
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 28						;

;====================================================================
; SpawnTask --- OS PUBLIC - Creates a new task in the current job
; and schedules it for execution
;
; Procedural Interface:
; SpawnTask(pEntry,	dPriority, fDebug, pStack, fOSCode);
;
;
pEntryST	EQU DWORD PTR [EBP+28]
dPriST		EQU DWORD PTR [EBP+24]
fDebugST	EQU DWORD PTR [EBP+20]
pStackST	EQU DWORD PTR [EBP+16]
fOSCodeST	EQU DWORD PTR [EBP+12]

NewExchST	EQU DWORD PTR [EBP-4]
NewTSSST	EQU DWORD PTR [EBP-8]

PUBLIC __SpawnTask:                  ;
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		SUB ESP, 8					;two local DWORD vars
		CMP dPriST, nPRI-1			;Priority OK?
		JBE ST0001
		MOV EAX,ercBadPriority
		JMP STEnd
ST0001:
		LEA EAX, NewExchST			;Allocate exchange
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;see if we got an error
		JNZ STEnd					;Yup, bad news

		;Allocate a new TSS
		CLI                     ;we can't be interrupted
		MOV EAX,pFreeTSS        ; NewTSS <= pFreeTSS;
		OR EAX,EAX              ; IF pFreeTSS=NIL THEN Return;
		JNZ ST0002              ;
		STI

		;Dealloc Exch if we didn't get a TSS
		PUSH NewExchST
		CALL FWORD PTR _DeAllocExch
		MOV EAX,ercNoMoreTSSs       ;No...
		JMP NTEnd
ST0002:
		MOV EBX,[EAX+NextTSS]   ; pFreeTSS <= pFreeTSS^.Next
		MOV pFreeTSS,EBX        ;
		DEC _nTSSLeft			;
		STI

		MOV NewTSSST, EAX			;Save new TSS
        MOV EBX, NewExchST			;mov exch into TSS
		MOV [EAX+TSS_Exch],EBX
		MOV WORD PTR [EAX+TSS_CS], OSCodeSel	;Defaults to OS code selector
		CMP fOSCodeST, 0
		JNE ST0003
		MOV WORD PTR [EAX+TSS_CS], JobCodeSel	;Make OS code selector
ST0003:
		MOV EBX,pEntryST			;mov EIP into TSS
		MOV [EAX+TSS_EIP],EBX
		MOV EBX, pStackST			;mov ESP into TSS
		MOV [EAX+TSS_ESP],EBX
		MOV [EAX+TSS_ESP0],EBX
		MOV EBX, pRunTSS
		MOV EDX, [EBX+TSS_pJCB]		;Get pJCB from Crnt Task
		MOV [EAX+TSS_pJCB],EDX
		MOV EDX, [EBX+TSS_CR3]		;Get CR3 from crnt task
		MOV [EAX+TSS_CR3],EDX		; move into new TSS
		MOV DWORD PTR [EAX+TSS_EFlags],0202h  ;Load the Flags Register
		CMP fDebugST, 0						;Debug on entry?
		JE ST0004                   		;No
		MOV WORD PTR [EAX+TSS_TrapBit], 1   ;Yes
ST0004:
        MOV EBX, dPriST				;mov priority into BL
		MOV [EAX+Priority],BL       ;put in TSS

		CLI                         ;we can't be interrupted
		MOV EDX,pRunTSS             ;Get who's running
		CMP [EDX+Priority],BL       ;Who got the highest Pri?
		JA ST0005                   ;If crnt >, New guy does (lowest num)
		CALL enQueueRdy             ;Old guy does, just put new guy on Q.
		XOR EAX,EAX                 ;ercOk
		JMP STEnd                   ;Return to caller
ST0005:
        XCHG EAX,EDX                ;CrntTSS -> EAX, New TSS -> EDX
        PUSH EDX					;New TSS -> Stack
		CALL enQueueRdy             ;Place crnt TSS on Q
		POP EAX						;New TSS -> EAX
		MOV pRunTSS,EAX             ;Move new TSS into pRunTSS
		MOV BX,[EAX+Tid]            ;Put Selector/Offset in "TSS"
		MOV TSS_Sel,BX              ;
		INC _nSwitches
		MOV EAX, TimerTick		;Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		JMP FWORD PTR [TSS]         ;Jump to new TSS
		XOR EAX,EAX                 ;ErcOk
STEnd:
		STI                         ;
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 20						;

;=============================================================================
;
; AllocExch - The kernel Allocate Exchange primitive. This procedure
; provides access to the operating system by allowing a TASK to
; allocate a message port for the transmission and reception of messages from
; another process.
;
; Procedural Interface :
;
;       AllocExch(pExchRet):dError
;
;          pExchRet is a pointer to where you want the Exchange Handle
;          returned.  The Exchange Handle is a DWORD (4 BYTES).
;
;=============================================================================

PUBLIC __AllocExch:              ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		XOR ESI,ESI             ; Zero the Exch Index
		MOV EBX,prgExch         ; EBX <= ADR rgExch
		MOV ECX,nExch           ; Get number of exchanges in ECX
AE000:
		CLI                     		;
		CMP DWORD PTR [EBX+Owner], 0    ; Is this exchange free to use
		JE AE001      			        ; If we found a Free Exch, JUMP
		ADD EBX,sEXCH           ; Point to the next Exchange
		INC ESI                 ; Increment the Exchange Index
		LOOP AE000              ; Keep looping until we are done
		STI                     ;
		MOV EAX,ercNoMoreExch   ; There are no instances of the Exch
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4                  ;

AE001:
		MOV EDX,[EBP+0CH]       ; Get the pExchRet in EDX
		MOV [EDX],ESI			; Put Index of Exch at pExchRet
		MOV EDX,pRunTSS         ; Get pRunTSS in EDX
		MOV EAX,[EDX+TSS_pJCB]  ; Get the pJCB in EAX
		MOV [EBX+Owner],EAX     ; Make the Exch owner the Job
		STI                     ;
		MOV DWORD PTR [EBX+EHead],0   	; Make the msg/TSS queue NIL
		MOV DWORD PTR [EBX+ETail],0   	;
		DEC _nEXCHLeft					; Stats
		XOR EAX,EAX             ;ercOK (0)
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4                  ;

;=============================================================================
;
; DeAllocExch - The kernel DeAllocate Exchange primitive. It allows a TASK
; to deallocate a "message port."  It also deQueues any messages, and frees
; up any Link Blocks, TSSs, and RQBs, attached to the exchange
;
; Procedural Interface :
;
;       DeAllocExch(Exch):ercType
;
;           Exch is the Exchange Handle the process is asking to be released.

PUBLIC __DeAllocExch:                 ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV ESI,[EBP+0CH]       ; Load the Exchange Index in ESI
		MOV EAX,ESI             ; Get the Exchange Index in EAX
		MOV EDX,sEXCH           ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ;
		MOV ECX,EAX             ; Make a copy in ECX (ECX = pExch)

		MOV EDX,pRunTSS         ; Get the pRunTSS in EDX
		MOV EBX,[EDX+TSS_pJCB]  ; Get pJCB in EBX
		MOV EDX,[EAX+Owner]     ; Get the Exchange Owner in EDX
		CMP EBX,EDX             ; If the CurrProc owns the Exchange,
		JE DE000                ; yes
		CMP EBX, OFFSET MonJCB  ; if not owner, is this the OS???
		JE DE000                ; yes
		MOV EAX,ercNotOwner     ;
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4                  ;

DE000:
		CLI                     ;
		CMP DWORD PTR [ECX+fEMsg],0 ; See if a message may be queued
		JE DE001                	; No. Go check for Task (TSS)
		MOV ESI, ECX				; ESI must point to Exch for deQueue
		CALL deQueueMsg         ; Yes, Get the message off of the Exchange
		OR EAX, EAX
		JZ DE002				; Nothing there. Go free the Exch.

		;Return the LB to the pool
		MOV EBX,pFreeLB         ; pLBin^.Next <= pFreeLB;
		MOV [EAX+NextLB],EBX    ;
		MOV pFreeLB,EAX         ; pFreeLB <= pLBin;
		INC _nLBLeft			;
		JMP DE000               ; Go And Check for more.

		; If we find an RqBlk on the exchange we must respond
		;with ErcInvalidExch before we continue! This will
		;only happen if a system service writer doesn't follow
		;instructions or a service crashes!
		;
DE001:
		CMP DWORD PTR [ECX+EHead], 0 	; Check to See if TSS is queued
		JE DE002                ; NIL = Empty, JUMP
		MOV ESI, ECX			; ESI must point to Exch for deQueue
		CALL deQueueTSS         ; Get the TSS off of the Exchange

		;Free up the TSS (add it to the free list)
		MOV EBX,pFreeTSS        ; pTSSin^.Next <= pFreeTSS;
		MOV [EAX+NextTSS],EBX   		;
		MOV DWORD PTR [EAX+TSS_pJCB], 0	; Make TSS invalid
		MOV pFreeTSS,EAX        		; pFreeTSS <= pTSSin;
		INC _nTSSLeft			;

		JMP DE001               ; Go And Check for more.
DE002:
		MOV DWORD PTR [ECX+Owner], 0     ; Free up the exchange.
		MOV DWORD PTR [ECX+fEMsg], 0     ; Reset msg Flag.
		INC _nEXCHLeft			; Stats
		STI                     ;
		XOR EAX,EAX             ;ercOK (0)
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4                  ;

;=============================================================================
;
; GetTSSExch - This returns the Exchange of the current TSS to the
; caller.  This is primarily provided for System Services that provide
; direct access blocking calls for customers.
;
; Procedural Interface :
;
;       GetTSSExch(pExchRet):dError
;
;          pExchRet is a pointer to where you want the Exchange Handle
;          returned.  The Exchange is a DWORD (4 BYTES).
;
PUBLIC __GetTSSExch:            ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX,pRunTSS    		; Get the Ptr To the Running TSS
		MOV ESI,[EBP+0CH]       ; Get the pExchRet in EDX
		MOV EBX, [EAX+TSS_Exch] ; Get Exch in EBX
		MOV [ESI],EBX			; Put Index of Exch at pExchRet
		XOR EAX, EAX            ; ErcOK
		POP EBP                 ;
		RETF 4                  ;

;=============================================================================
;
; SetPriority - This sets the priority of the task that called it
; to the priority specified in the single parameter.
;
; Procedural Interface :
;
;       SetPriority(bPriority):dError
;
;          bPriority is a byte with the new priority.
;
PUBLIC __SetPriority            ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX,pRunTSS    		; Get the Ptr To the Running TSS
		MOV EBX,[EBP+0CH]       ; Get the new pri into EBX
		AND EBX, 01Fh			; Nothing higher than 31!
		MOV BYTE PTR [EAX+Priority], BL ;Put it in the TSS
		XOR EAX, EAX            ; ErcOK - No error.
		POP EBP                 ;
		RETF 4                  ;

;=============================================================================
;
; GetPriority - This gets the priority of the task that called it
; and passes it to bPriorityRet.
;
; Procedural Interface :
;
;       SetPriority(bPriorityRet):dError
;
;          bPriorityret is a pointer to a byte where you want the
;             priority returned.
;
PUBLIC __GetPriority            ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX,pRunTSS    		; Get the Ptr To the Running TSS
		MOV EBX,[EBP+0CH]       ; Get the return pointer into EBX
		MOV DL, BYTE PTR [EAX+Priority]
		MOV BYTE PTR [EBX], DL  ;
		XOR EAX, EAX            ; ErcOK - No error.
		POP EBP                 ;
		RETF 4                  ;

;======================== End of Module ======================
