;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0

.DATA
.INCLUDE MOSEDF.INC
.INCLUDE JOB.INC
.INCLUDE TSS.INC

PUBLIC pFreeJCB		DD 0				; Ptr to free Job Control Blocks
PUBLIC pJCBs		DD 0				; JCBs are in allocated memory
PUBLIC _nJCBLeft	DD nJCBs			; For Monitor stats

EXTRN MonJCB  DB 	; Monitor JCB reference
EXTRN _BootDrive DD ; From Main.asm

;======= End data,Begin Code ==================

.CODE
;================================================================
;InitNewJCB is used initially by the OS to fill in the first two
;jobs (Monitor & Debugger)
;
PUBLIC InitNewJCB:
; INPUT :	EAX -- Ptr to JCB that is to be filled in
;			EBX -- Linear Ptr to Page Directory for Job
;			ESI -- pbJobName
;			ECX -- cbJobName
;			EDX -- Pointer to Job Virtual Video Buffer (all jobs have one!)
;
; OUTPUT :	JOB Number in EAX
; USED : 	EAX, EBX, ECX, EDX, EDI, ESI, EFlags
; MODIFIES : JCB pointed to in EBX
;
; This fills in a JCB with new information.  This is used to initilaize
; a new JCB during OS init and when a new Job is loaded and run.
;
		MOV [EAX+JcbPD],EBX	    		;Put Ptr to PD into JCB
		MOV EDI, EAX					;EDI points to JCB
		ADD EDI, sbJobName				;Now to JobName
		MOV BYTE PTR [EDI], CL			;size is filled in
		INC EDI							;first byte of name
		REP MOVSB						;Move it in
		MOV [EAX+pVirtVid], EDX			;Video number is in JCB
		MOV DWORD PTR [EAX+nCols], 80	;
		MOV DWORD PTR [EAX+nLines], 25	;
		MOV DWORD PTR [EAX+NormAttr], 7	;
		MOV EAX, [EAX+JobNum]
		RETN


;=============================================================================
; InitFreeJCB
; INPUT : 	EAX - Address of JCBs to be initialized
;			ECX - Count of JCBs
;			EDX - Size of JCBs
; OUTPUT :	NONE
; USED:		EAX,EBX,ECX,EDX,ESI EFLAGS
; MODIFIES: pFreeJCB, pJCBs
;
; This routine will initialize the free pool of Job Control Blocks (JCBs).
; EAX points to the first JCB,
; ECX is count of JCBs,
; EDX is size of each JCB.
;
; The pFreeJCB pointer is set to address the first element in rgJCBs.
; Each element of rgJCBs is set to point to the next element of rgJCBs.
; The last element of rgJCBs is set to point to nothing (NIL).
; The JCBs are also sequentially numbered. We can't use it's position
; in the array because some JCBs are static (Mon and Debugger), while
; others (the ones we are initializing now) are dynamicly allocated.
;
PUBLIC InitFreeJCB:
		MOV pFreeJCB,EAX        ;Set up OS pointer to list
		MOV pJCBs, EAX			;Set up global ptr to first JCB
		MOV EBX, 3				;1st number for Dynamic JCBs
JCB_Loop:
	    MOV ESI,EAX             ;EBX has pointer to current one
		ADD EAX,EDX             ;EAX points to next one
		MOV [ESI+NextJCB],EAX   ;Make current point to next
		MOV [ESI+JobNum], EBX	;Number it
		INC EBX
		LOOP JCB_Loop           ;Go back till done
		MOV DWORD PTR [ESI+NextJCB], 0   ;Make last one NIL
		RETN                    ;

;================================================================
; Allocate 4 pages (16384 bytes) for 32 Job Control Blocks (structures).
; Then call InitFreeJCB

PUBLIC InitDynamicJCBs:
		PUSH 4						; 4 pages for 32 JCBs (16384 bytes)
		MOV EAX, OFFSET pJCBs		; Returns ptr to allocated mem in pJCBs
		PUSH EAX					;
		CALL FWORD PTR _AllocOSPage	; Get 'em!

		XOR EAX, EAX				; Clear allocated memory for JCBs
		MOV ECX, 4096				; (4*4096=16384 - DWORDS!)
		MOV EDI, pJCBs				; where to store 0s
		REP STOSD					; Do it

		MOV EAX, pJCBs				; Ptr to JCBs
		MOV ECX, nJCBs				; Count of Job Control Blocks
		MOV EDX, sJCB				; EDX is size of a JCB
		CALL InitFreeJCB    		; Init the array of JCBs
		RETN

;=============================================================================

PUBLIC NewJCB:
; INPUT : NONE
; OUTPUT : EAX
; REGISTERS : EAX,EBX,FLAGS
; MODIFIES : pFreeJCB
;
; This routine will return to the caller a pointer to the next free jcb.
; The data used in this algorithm is the free jcb pointer (pFreeJCB).
; This routine will return in EAX register the address of the next free jcb.
; If none exists, then EAX will contain NIL (0). This routine will also
; update the value of pFreeJCB to point to the next "unused" JCB in
; the free pool.
;
		MOV EAX,pFreeJCB        ;Get OS pointer to JCBs
		CMP EAX,0	            ;IF pFreeJCB=NIL THEN Return;
		JE NewJCBDone           ;
		MOV EBX,[EAX+NextJCB]   ;Get pointer to next free one
		MOV pFreeJCB,EBX        ;Put it in OS pointer
		DEC DWORD PTR _nJCBLeft			;
NewJCBDone:
	    RETN                    ;

;=============================================================================

PUBLIC DisposeJCB:
; INPUT : EAX
; OUTPUT : NONE
; REGISTERS : EBX,FLAGS
; MODIFIES : pFreeJCB
;
; This routine will place the jcb pointed to by EAX back into the free
; pool of JCBs pointed to by (pFreeJCB) if EAX is not NIL.
; This invalidates the JCB by placing 0 in JcbPD.
;
		CMP EAX, 0	             		; If pJCBin = NIL THEN Return;
		JE DispJCBDone          		;
		MOV DWORD PTR [EAX+JcbPD], 0	;Invalidate JCB
		MOV EBX,pFreeJCB        		;EBX has OS ptr to free list
		MOV [EAX+NextJCB],EBX   		;Move it into newly freed JCB
		MOV pFreeJCB,EAX        		;Move ptr to newly frred JCB to OS
		INC DWORD PTR _nJCBLeft					;
DispJCBDone:
	    RETN                    ;

;============================================================
;
; GetpCrntJCB
; Returns a pointer to the current Job Control Block in EAX.
; This is based on which Task is executing.  All TSSs are
; assigned to a Job.  A Job may have more than one Task.
;
; INPUT:	Nothing
; OUTPUT:	EAX -- Linear Address of current JCB
; USED:		EAX, EFlags
;
PUBLIC GetpCrntJCB:
		MOV EAX, pRunTSS		;Current Task State Segment
		MOV EAX, [EAX+TSS_pJCB]	;Pointer to JCB
		RETN

;============================================================
;
; GetCrntJobNum
; Many OS functions deal with the Job number. The Job number
; is a field in the JCB structure.
; Returns the Job number for the currently executing task.
; This is based on which Task is executing.  All TSSs are
; assigned to a Job!  A Job may have more than one Task.
;
; INPUT:	Nothing
; OUTPUT:	EAX -- Current Job Number
; USED:		EAX, EFlags
;
PUBLIC GetCrntJobNum:
		CALL GetpCrntJCB
		MOV EAX, [EAX+JobNum]			;Current JCB
		RETN

;============================================================
;
; GetpJCB
; Returns a pointer to a Job Control Block identified by number
; in EAX.  All TSSs are assigned to a Job.
;
; INPUT:	EAX -- Job Number of desired pJCB
; OUTPUT:	EAX -- Linear Address of the JCB or 0 for invalid number
; USED:		EAX, EFlags
;
PUBLIC GetpJCB:
		PUSH EDX
		CMP EAX, 1
		JNE GetpJCB1
		MOV EAX, OFFSET	MonJCB
		POP EDX
		RETN
GetpJCB1:
		CMP EAX, 2
		JNE GetpJCB2
		MOV EAX, OFFSET	DbgJCB
		POP EDX
		RETN
GetpJCB2:
		CMP	EAX, nJCBs+2		;Add in two static JCBs
		JLE GetpJCB3			;Within range of JCBs
		XOR EAX, EAX
		POP EDX
		RETN
GetpJCB3:
		SUB EAX, 3				;Take off static JCBs+1 (make it an offset)
		MOV EDX, sJCB
		MUL EDX					;Times size of JCB
		ADD EAX, pJCBs			;Now points to desired JCB
		POP EDX
		RETN					;

;============================================================
;
; GetJobNum
; Many OS functions deal with the Job number. The Job number
; is a field in the JCB structure.
; Returns the Job number for the pJCB in EAX in EAX.
;
; INPUT:	EAX pJCB we want job number from.
; OUTPUT:	EAX -- Current Job Number
; USED:		EAX, EFlags
;
PUBLIC GetJobNum:
		MOV EAX, [EAX+JobNum]			;Current JCB
		RETN

;============================================================
;
; AllocJCB  (NEAR)
; This allocates a new JCB (from the pool).  This is a NEAR
; call to support the public job management calls in high level
; languages.
;
; Procedureal Interface :
;
;		AllocJCB(pdJobNumRet, ppJCBRet):ercType
;
;   pdJobNumRet is the number of the new JCB.
;	pJCBRet is a pointer where you want the pointer to the new JCB is returned.
;
;   ErcNoMoreJCBs will be returned if no more JCBs are avaialble.
;
; pdJobNum		 	EQU [EBP+12]
; pJCBRet		 	EQU [EBP+8]

PUBLIC _AllocJCB:            	;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		CALL NewJCB				; Get a new JCB
		OR EAX, EAX				;
		JNZ SHORT AJCB01		; We got one!
		MOV EAX, ErcNoMoreJCBs	; Sorry, out of them!
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN 8	                ;

AJCB01:
		MOV ESI, [EBP+8]		;pJCBRet
		MOV [ESI], EAX
		MOV ESI, [EBP+12]		;Job Num
		CALL GetJobNum			;
		MOV [ESI], EAX			;
		XOR EAX, EAX			;No error
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN 8	                ;

;============================================================
;
; DeAllocJCB  (NEAR)
; This Deallocates a JCB (returns it to the pool).  This is a NEAR
; call to support the public job management calls in high level
; languages in the OS code.
;
; Procedureal Interface :
;
;		DeAllocJCB(pJCB):ercType
;
;	pJCB is a pointer the JCB to be deallocated.
;
;   ErcNoMoreJCBs will be returned if no more JCBs are avaialble.
;
; pJCB		 	EQU [EBP+8]

PUBLIC _DeAllocJCB:		       	;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV EAX, [EBP+8]		; pJCB
		CALL DisposeJCB			; Get a new JCB
		XOR EAX, EAX			;
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN 4	                ;

;============================================================
;===== BEGIN PUBLIC FAR JOB CALLS ===========================
;============================================================
;
; GetpJCB
; This PUBLIC returns a pointer to the JCB for the JobNum
; you specifiy.
;
; Procedureal Interface :
;
;		GetpJCB(dJobNum, pJCBRet):ercType
;
;   dJobNum is the number of the JCB you want.
;	pJCBRet is a pointer where you want the JCB returned.
;
;   ErcBadJobNum will be returned if dJobNum is out of range
;
;   ErcBadJobNum will be returned if dJobNum is invalid
;   or 0 will be returned with the data.
;
; dJobNum		 	EQU [EBP+16]
; pJCBRet		 	EQU [EBP+12]

PUBLIC __GetpJCB:            	;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV EAX, [EBP+16]		;Job Num
		OR EAX, EAX
		JZ GetpJcbBad			;0 is invalid
		CMP EAX, nJCBs + 2;		;Dynamic + 2 static
		JBE GetpJcbOK
GetpJcbBad:
		MOV EAX, ErcBadJobNum	;
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8	                ;
GetpJcbOk:
		CALL GetpJCB			;puts address of JCB in EAX
		MOV ESI, [EBP+12]		;pJCBRet
		MOV [ESI], EAX
		CMP DWORD PTR [EAX+JcbPD], 0		;Is this a valid JCB
		JNE GetpJCBOk1
        MOV EAX, ErcInvalidJCB	;JCB we are pointing to is unused
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8	                ;
GetpJcbOk1:
		XOR EAX, EAX
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8	                ;

;============================================================
;
; GetJobNum
; This PUBLIC returns the number for the current Job. This is
; the job that the task that called this belongs to.
;
; Procedureal Interface :
;
;		GetJobNum(pJobNumRet):ercType
;
;	pJCBRet is a pointer where you want the JCB returned.
;
; pJobNumRet	EQU [EBP+12]

PUBLIC __GetJobNum:            	;
		PUSH EBP                ;
		MOV EBP,ESP             ;
        CALL GetCrntJobNum		;Leave jobnum in EAX
		MOV ESI, [EBP+12]		;pJobNumRet
		MOV [ESI], EAX			;
		XOR EAX, EAX			;No Error
		POP EBP                 ;
		RETF 4	                ;


;============================================================
;
; GetSystemDisk
; This PUBLIC returns a single byte which represents the
; disk that the system booted from. This is from the public
; variable _BootDrive defined in Main.asm.
; It return 0-n (which corresponds to A-x)
; This code is here for lack of a better place.
; It's really not a filesystem function either. And will
; still be needed if a loadable filesystem is installed.
;
; Procedureal Interface :
;
;		GetSystemDisk(pSysDiskRet):ercType
;
;           pSysDiskRet is a pointer to a byte where
;           the number representing the system disk is returned.
;
;  pSysDiskRet  EQU [EBP+12]

PUBLIC __GetSystemDisk         	;
		PUSH EBP                ;
		MOV EBP,ESP             ;
        MOV EAX, _BootDrive
        AND EAX, 7Fh			;get rid of high bits
		MOV ESI, [EBP+12]		;pJobNumRet
		MOV [ESI], AL			;
		XOR EAX, EAX			;No Error
		POP EBP                 ;
		RETF 4	                ;


;================= MODULE END =================================
