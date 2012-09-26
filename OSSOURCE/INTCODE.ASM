;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0
.DATA
.INCLUDE MOSEDF.INC

.CODE
;=============================================================================
; Set8259
; This sets IRQ00-0F vectors in the 8259s
; to be Int20 thru 2F.
;
; When the PICUs are initialized, all the hardware interrupts are MASKED.
; Each driver that uses a hardware interrupt(s) is responsible
; for unmasking that particular IRQ.
;
PICU1          EQU 0020h
PICU2          EQU 00A0h

PUBLIC Set8259:
		MOV AL,00010001b
		OUT PICU1+0,AL          	;ICW1 - MASTER
		PUSH EAX
		POP EAX
		OUT PICU2+0,AL          	;ICW1 - SLAVE
		PUSH EAX
		POP EAX
		MOV AL,20h
		OUT PICU1+1,AL          	;ICW2 - MASTER
		PUSH EAX
		POP EAX
		MOV AL,28h
		OUT PICU2+1,AL          	;ICW2 - SLAVE
		PUSH EAX
		POP EAX
		MOV AL,00000100b
		OUT PICU1+1,AL          	;ICW3 - MASTER
		PUSH EAX
		POP EAX
		MOV AL,00000010b
		OUT PICU2+1,AL          	;ICW3 - SLAVE
		PUSH EAX
		POP EAX
		MOV AL,00000001b
		OUT PICU1+1,AL          	;ICW4 - MASTER
		PUSH EAX
		POP EAX
		OUT PICU2+1,AL          	;ICW4 - SLAVE
		PUSH EAX
		POP EAX
		MOV AL,11111010b			;Masked all but cascade/timer
;		MOV AL,01000000b			;Floppy masked
		OUT PICU1+1,AL          	;MASK - MASTER (0= Ints ON)
		PUSH EAX
		POP EAX
		MOV AL,11111111b
;		MOV AL,00000000b
		OUT PICU2+1,AL          	;MASK - SLAVE
		PUSH EAX
		POP EAX
		RETN

;=====================================================
;The following PUBLIC FAR calls support ISR operations:
;
; SetIRQVector(IRQnum, pVector)
; GetIRQVector(IRQnum, pVectorRet)
; EndOfIRQ(IRQnum)
; MaskIRQ(IRQnum)
; UnMaskIQR(IRQnum)
;
; In each case, IRQnum is the hardware interrupt request number for the
; IRQ served. This will be 0-7 for interrupts on 8259 #1 and 8-15 for
; interrupts on 8259 #2.  The predetermined IRQ uses are:
;
;	IRQ 0	8254 Timer
;	IRQ 1 	Keyboard (8042)
;	IRQ 2	Cascade from PICU2 (handled internally)
;	IRQ 3 	COMM 2 Serial port
;	IRQ 4	COMM 1 Serial port
;	IRQ 5	Line Printer 2
;	IRQ 6	Floppy disk controller
;	IRQ 7	Line Printer 1
;
;	IRQ 8	CMOS Clock
;	IRQ 9	?
;	IRQ 10	?
;	IRQ 11	?
;	IRQ 12	?
;	IRQ 13	Math coprocessor
;	IRQ 14	Hard disk controller
;	IRQ 15	?

;=============================================================================
;
; SetIntVector(IRQNum, pISR)
; This sets a 32 bit offset for an interrupt handler
; that services one of the hardware interrupts. (0-15)
; The ISR MUST reside in the OS Code segment. This means that only
; device drivers or the OS can set and service interrupts!!
; The DPL is set to 3 (all code can be hardware interrupted).
;
PUBLIC __SetIRQVector:
		PUSH EBP
		MOV EBP, ESP
		MOV ECX, [EBP+16]		;Get IRQ number (0-15)
		AND ECX, 0Fh			;0 to 15 max!
		ADD ECX, 20h			;INT number is set for IRQ
		MOV EAX, 08E00h			;Int gate description
		MOV EBX, OSCodeSel		;
		MOV ESI, [EBP+12]		;
		CALL FWORD PTR _AddIDTGate ;
		MOV ESP,EBP
		POP EBP
		RETF 8                   ;

;=============================================================================
;
; GetIntVector(IRQNum, pVectorRet)
; This returns the vector (offset) of the ISR that is currently
; serving the IRQ.
;
PUBLIC __GetIRQVector:
		PUSH EBP
		MOV EBP, ESP
		MOV ESP,EBP
		POP EBP
		RETF 8                   ;

;=============================================================================
;
; Sends End Of Interrupt to PICU (or Both) based on IRQ number (0-15)
; If IRQnum is 0-7 then we send to 1, else we send to 1 then 2.
;
PUBLIC __EndOfIRQ:
		PUSH EBP
		MOV EBP, ESP
		PUSH EAX				;
		MOV EAX, [EBP+0Ch]		;Get IRQ number (0-15)
		MOV AH, AL
		MOV AL,20h              ;
		OUT PICU1,AL            ;
		CMP AH, 7				;PICU1 only?
		JBE	EOI00				;Yes
		OUT PICU2,AL            ;Send to 2 also
EOI00:
		POP EAX                 ;
		MOV ESP,EBP
		POP EBP
		RETF 4                  ;

;===============================================
; MaskIRQ(IRQnum)  masks the IRQ number specified (0-15).
; The proper PICU is selected based on the IRQ number.
;
PUBLIC __MaskIRQ:
		PUSH EBP
		MOV EBP, ESP
		PUSHFD
		CLI
		PUSH EAX				;
		PUSH ECX
		MOV EAX, 1
		MOV ECX, [EBP+0Ch]		;Get IRQ number (0-15)
		AND ECX, 0Fh			;(0-15)
		SHL EAX, CL				;Set the bit for the IRQ (0-7) or (8-15)
		AND AL,AL
		JZ MIRQ2
		MOV AH,AL
		IN AL, PICU1+1
		PUSH EAX
		POP EAX
		OR AL, AH
		OUT PICU1+1, AL
		JMP SHORT MIRQEnd
MIRQ2:
		IN AL, PICU2+1			;AH already has correct value
		PUSH EAX
		POP EAX
		OR AL, AH
		OUT PICU2+1, AL
MIRQEnd:
		POP ECX                 ;
		POP EAX                 ;
		POPFD					;Give em their flags back
		MOV ESP,EBP
		POP EBP
		RETF 4                  ;
;===============================================
; UnMaskIRQ(IRQnum)  UNmasks the IRQ number specified (0-15).
; The proper PICU is selected based on the IRQ number.
;
PUBLIC __UnMaskIRQ:
		PUSH EBP
		MOV EBP, ESP
		PUSHFD
		CLI						;Previous state is reset on POPFD
		PUSH EAX				;
		PUSH ECX
		MOV EAX,1
		MOV ECX,[EBP+0Ch]		;Get IRQ number (0-15)
		AND ECX, 0Fh			; (0-15 only)
		SHL EAX, CL				;Set the bit for the IRQ (0-7)
		AND AL,AL
		JZ UMIRQ2
		MOV AH, AL
		IN AL, PICU1+1
		PUSH EAX
		POP EAX
		NOT AH
		AND AL, AH
		OUT PICU1+1, AL
		JMP SHORT UMIRQEnd
UMIRQ2:
		IN AL, PICU2+1
		PUSH EAX
		POP EAX
		NOT AH
		AND AL, AH
		OUT PICU2+1, AL
UMIRQEnd:
		POP ECX                 ;
		POP EAX                 ;
		POPFD
		MOV ESP,EBP
		POP EBP
		RETF 4                  ;
;
;===== Module End ================
