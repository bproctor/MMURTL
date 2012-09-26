;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0
;=============================================================================
;Data and equates for Request Block management
;
.DATA
;
.INCLUDE RQB.INC

;Request Block control variables

pFreeRQB    	DD 0					; Ptr to free Request Blocks
PUBLIC pRQBs	DD 0					; RQBs are in allocated memory

PUBLIC 	_nRQBLeft	DD nRQBs		; For Monitor stats


;=============== End Data, Begin Code =====================

.CODE

;================================================================

InitFreeRQB:
; INPUT : 	EAX - Address of RQBs to be initialized
;			ECX - Count of JCBs
;			EDX - Size of JCBs
; OUTPUT :	NONE
; USED:		EAX,ECX,EDX,ESI EFLAGS
; MODIFIES: pFreeRQB, pRQBs
;
; This routine will initialize a free pool of Request Blocks (RQBs).
; ECX is count of RQBs to initialize,
; EDX is the size (in bytes), and EAX points to a list of free JCBs (pFreeJCB).
;
; The pFreeRQB pointer is set to address the first element in rgRQBs.
; Each element of rgRQBs is set to point to the next element of rgRQBs.
; The last element of rgRQBs is set to point to nothing (NIL).
; The RqBlk handle used in many calls is its position in the array.
; This can be done because the array of RQBs is allocated dynamically.
;
		MOV pFreeRQB,EAX        ;Set up OS pointer to list
		MOV pRQBs, EAX			;Set up global ptr to first RQB
RQB_Loop:
	    MOV ESI,EAX             ;EBX has pointer to current one
		ADD EAX,EDX             ;EAX points to next one
		MOV [ESI+pNextRQB],EAX  ;Make current point to next
		LOOP RQB_Loop 			        ;Go back till done
		MOV DWORD PTR [ESI+pNextRQB],0  ;Make last one nil
		RETN                   			;

;---------------------------------------------------------------
; Allocate 2 pages (8192 bytes) for 128 Request Blocks (structures).
; Then call InitFreeRQB

PUBLIC InitDynamicRQBs:

		PUSH 2						; 2 pages for 128 RQBs
		MOV EAX, OFFSET pRQBs		; Returns ptr to allocated mem in pRQBs
		PUSH EAX					;
		CALL FWORD PTR _AllocOSPage	; Get 'em!

		XOR EAX, EAX				; Clear allocated memory for RQBs
		MOV ECX, 2048				; (8192)
		MOV EDI, pRQBs				; where to store 0s
		REP STOSD					; Do it

		MOV EAX, pRQBs				; Ptr to RQBs
		MOV ECX, nRQBs				; Count of Request Blocks
		MOV EDX, sRQB				; EDX is size of a RQB
		CALL InitFreeRQB    		; Init the array of RQBs
		RETN

;---------------------------------
PUBLIC NewRQB:
; INPUT : NONE
; OUTPUT : EAX
; REGISTERS : EAX,EBX,FLAGS
; MODIFIES : pFreeRQB
;
; This routine returns to the caller a pointer to the next free RQB.
; The data used in this algorithm is the free jcb pointer (pFreeRQB).
; This routine will return in EAX register the address of the next free jcb.
; If none exists, then EAX will contain NIL (0). This routine will also
; update the value of pFreeRQB to point to the next "unused" RQB in
; the free pool.
;
		MOV EAX,pFreeRQB        ;Get OS pointer to RQBs
		CMP EAX, 0              ;IF pFreeRQB=NIL THEN Return;
		JE NewRQBDone           ;
		MOV EBX,[EAX+pNextRQB]  ;Get pointer to next free one
		MOV pFreeRQB,EBX        ;Put it in OS pointer
		DEC _nRQBLeft			;
NewRQBDone:
	    RETN                    ;

;=============================================================================

PUBLIC DisposeRQB:
; INPUT : EAX
; OUTPUT : NONE
; REGISTERS : EAX, EBX, FLAGS
; MODIFIES : pFreeRQB
;
; This routine will place the RQB pointed to by EAX back into the free
; pool of RQBs pointed to by (pFreeRQB) if EAX is not NIL.
;
		CMP EAX, 0              ; If pJQBin = NIL THEN Return;
		JE DispRQBDone          ;
		MOV EBX,pFreeRQB        ;EBX has OS ptr to free list
		MOV [EAX+pNextRQB],EBX  ;Move it into newly freed RQB
		MOV pFreeRQB,EAX        ;Move ptr to newly frred RQB to OS
		INC DWORD PTR _nRQBLeft			;
DispRQBDone:
	    RETN                    ;

;========================= End of Module =================================
