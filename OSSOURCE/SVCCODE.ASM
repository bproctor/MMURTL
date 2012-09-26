;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993, Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version x0.8

.DATA
.INCLUDE MOSEDF.INC

;=============================================================================
.CODE
;=============================================================================
;
; GetExchange
;
; This returns the exchange associated with a service name pointed to in EAX.
; It scans the array of servce names and finds a match. ESI pts to the
; SvcDesc if found and EAX has ercOk, else EAX has error code and ESI
; is undefined.
;
; INPUT  - EAX = Pointer to the Service Name (8 bytes long)
; OUTPUT - EAX = result code
;  		   ESI = exchange
; USED   - EAX, EBX, ESI, EDX, EFlags
;
;=============================================================================

PUBLIC GetExchange:
		MOV EDX,[EAX]			; First DWORD of the SvcName in EDX
		MOV EBX,[EAX+4]			; Second DWORD of the SvcName in EBX
		MOV ESI,OFFSET rgSVC	; Get the address of rgSVC
		MOV ECX,nSVC			; Get the number of Service Descriptors
GE0000:
		CMP EDX, [ESI]			; Compare first 4 bytes
		JNE SHORT GE0001		; Not equal.. JUMP
		CMP EBX, [ESI+4]		; Compare first 4 bytes
		JNE SHORT GE0001		; Not equal.. JUMP
		MOV ESI,[ESI+8]			; Get the Exchange in ESI
		MOV EAX,ercOk			; Set EAX to ercOk
		RETN					; return
GE0001:
		ADD ESI,sSVC			; Move Dest Ptr to Next rgSvc Element
		LOOP GE0000				; Loop until ECX = 0
		MOV EAX,ErcNoSuchSvc	; set result code
		RETN					; Get out

;=============================================================================

FindEmptySVC:
;
; INPUT : NONE
; OUTPUT : EAX,ESI
; REGISTERS : ECX
; MODIFIES : NOTHING
;
; Searches the list of Service Descriptors and returns a ptr to an empty
; Service Descriptor in ESI with ercOk in EAX.
; If an empty Svc Desc in not found, an Error is returned in EAX
; and ESI is undefined. Used by RegisterSvc (below)
;
		MOV ESI, OFFSET rgSVC   ; Get address of the Service Descriptor Array
		MOV ECX,nSVC            ; Get number of Service Descriptors
		CLI
FELOOP:
        CMP DWORD PTR [ESI],0   ; Is the lower dword empty?
		JNE SHORT FENEXT        ; NO
		CMP DWORD PTR [ESI+4],0 ; Is the upper dword empty?
		JNE SHORT FENEXT        ; NO
		XOR EAX,EAX             ; No Error
		STI
		RETN                    ;
FENEXT:
        ADD ESI,sSVC            ;
		LOOP FELOOP             ;
		MOV EAX,ErcOutOfSvcDesc ;
FEEXIT:
        STI
		RETN

;==============================

FindMatchSVC:
;
; INPUT : EDX:EBX  contains name of service to search for
; OUTPUT : EAX,ESI
; REGISTERS : ECX
; MODIFIES : NOTHING
;
; Searches the list of Service Descriptors and returns a ptr to the match
; in ESI with ercOk in EAX.
; If not found an Error is returned in EAX and ESI is undefined.
; Used by RegisterSvc (below)
;
		MOV ESI, OFFSET rgSVC       ; Get address of the Service Desc Array
		MOV ECX,nSVC                ; Get number of Service Descriptors
		CLI
FMLOOP:
        CMP [ESI],EDX    			; lower dword match?
		JNE SHORT FMNEXT            ; NO
		CMP [ESI+4],EBX       		; upper dword match?
		JNE SHORT FMNEXT            ; NO
		XOR EAX,EAX                 ; No Error
		STI
		RETN                        ;
FMNEXT:
        ADD ESI,sSVC                ;
		LOOP FMLOOP                 ;
		MOV EAX, ErcNoSuchSvc       ;
FMEXIT:
        STI
		RETN

;=============================================================================
;
; RegisterSvc - The kernel system service name registery. This procedure will
; identify a particular system service name with a particular exchange.
; The name is an 8 byte (null padded) CASE SENSITIVE string. The name is
; registered with the operating system along with a service exchange. Together
; the information allows the OS to direct requests for system services without
; the originator having to know where the actual exchange is located or on
; what machine the request is being serviced.
;
; A result code in returned in the EAX register.
;
; Procedural Interface :
;
;   RegisterName(pName,exch):ercType
;
;           pName is a POINTER to the new Service Name

;           exch is a DWORD (4 BYTES) containing the exchange where the
;           service will accept messages.
;
pSVCName       EQU [EBP+16]
SVCExchange    EQU [EBP+12]

PUBLIC __RegisterSvc:
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX,pSVCName        ; in EAX
		MOV EDX,[EAX]        	; Get the Name into EDX:EBX for Find call
		MOV EBX,[EAX+4]      	;
		CALL FindMatchSVC       ; Is it already there? (if so, overwrite)
		CMP EAX,ercOk           ; ercOk if it is
		JNE SHORT RSNew         ; No, go get an empty one
		JMP SHORT RSFill        ; Yes, Overwrite it
RSNew:
        CALL FindEmptySVC       ;
		CMP EAX,ercOk           ;
		JNE SHORT RSDone        ;
RSFill:
		MOV EAX,pSVCName        ; pSVCName in EAX
		MOV EDX, [EAX]			; Low DWORD of name into EDX
		MOV [ESI], EDX        	; Now into Svc Descriptor
		MOV EBX,[EAX+4]      	; Low DWORD of name into EBX
		MOV [ESI+4], EBX       	; Now into Svc Descriptor
        MOV ECX,SVCExchange     ;
		MOV [ESI+SvcExch],ECX   ;
		XOR EAX,EAX             ;
RSDone:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8 					;

;=============================================================================
;
; UnRegisterSvc -  This removes the service name from the name
; name registry. The name must be an exact match for the one
; supplied to RegisterSvc.
; The name is an 8 byte (null padded) CASE SENSITIVE string.
;
; A result code in returned in the EAX register.
;
; Procedural Interface :
;
;   UnRegisterName(pName):ercType
;
;           pName is a POINTER to the existing Service Name

;           exch is a DWORD (4 BYTES) containing the exchange where the
;           service will accept messages.
;
pOldSVCName       EQU [EBP+12]

PUBLIC __UnRegisterSvc:
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX,pOldSVCName     ; in EAX
		MOV EDX,[EAX]        	; Get the Name into EDX:EBX for Find call
		MOV EBX,[EAX+4]      	;
		CALL FindMatchSVC       ; Is it already there? (if so, overwrite)
		OR EAX, EAX				; ErcOK?
		JZ  SHORT URSKill       ; Found it, go kill it
		JMP SHORT URSDone       ; ErcNoSuchSvc already in EAX
URSKill:
		XOR EAX, EAX			; Low DWORD of name into EDX
		MOV [ESI], EAX        	; Now into Svc Descriptor
		MOV [ESI+4], EAX       	; (EAX already zero for ErcOK)
URSDone:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4 					;

;=========== End of Module ===================
