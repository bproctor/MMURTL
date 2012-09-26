;=============================================================================
;
; MMURTL IMAGE LOADER code:
;	Clears interrupts
;	Turn ON A20 adress line gate
;   Moves the OS data to 00000000 physical.
;	Moves the OS Code to 00010000 physical (64K Boundary)
;   Jumps to the OS Code entry point.
; (This basically turns MS-DOS into a $79.00 loader)
;
;
; We define the GDT and IDT entries in MOS.gdt and MOS.idt.
;
; NOTE: This the last the processor sees of 16 bit real mode code
;   (no great loss at all)
;
;=============================================================================

LoadSeg        SEGMENT USE16
		ASSUME CS:LoadSeg, DS:Nothing, ES:Nothing, SS:Nothing

IDTptr      DW 7FFh                 ;LIMIT 256 IDT Slots
			DD 0000h                ;BASE (Linear)

GDTptr      DW (nGDTSlots*8)-1      ;LIMIT 768 GDT Slots
			DD 0800h                ;BASE (Linear)


OSCodeSize	EQU OSCodeEnd-OSCodeBegin
OSCodeMore  EQU OSCodeSize - 10000h
OSDataSize	EQU OSDataEnd-OSDataBegin



START:  CLI                     ; first we clear interrupts

; Note: this next few lines of code turns on the address line 20 Gate!
; This must be done before we can physically address above 1 Meg!

		XOR CX,CX		        ;check 64K times
IBEmm0:
		IN AL,64h		        ;Read Status Byte into AL
		TEST AL,02h				;Test The Input Buffer Full Bit
		LOOPNZ IBEmm0
		MOV AL,0D1h
		OUT 64h,AL
		XOR CX,CX				;check 64K times
IBEmm1:
		IN AL,64h				;Read Status Byte into AL
		TEST AL,02h				;Test The Input Buffer Full Bit
		LOOPNZ IBEmm1
		MOV AL,0DFh
		OUT 60h,AL
		XOR CX,CX				;check 64K times
IBEmm2:
		IN AL,64h				;Read Status Byte into AL
		TEST AL,02h				;Test The Input Buffer Full Bit
		LOOPNZ IBEmm2

; Move OS data to Physical Address 0000h

		MOV AX,SEG OSDSeg		; Move Data segment to 0000h
		MOV DS,AX               ;
		XOR SI,SI               ; Source DS:SI
		MOV AX,0000h            ;
		MOV ES,AX               ;
		XOR DI,DI               ; Destination ES:DI
		MOV ECX, OSDataSize+3	;
		CLD                     ;
		REP MOVSB               ;

; Move OS Code to Physical Address 20000h (in 32K chunks)

		MOV AX,SEG OSCSeg		; Move OS Code to 10000h
		ADD AX, 1000h			; 10000h bytes into DOS segment
		MOV DS,AX               ;
		XOR SI, SI 				; Source DS:SI
		MOV AX,1000h            ; Segment 1000h (address 10000h)
		MOV ES,AX               ;
		XOR DI,DI              	; Destination ES:DI
		MOV CX,8000h			; 64K Max (CODE is > 64K)
		CLD                    	;
		REP MOVSB				;

		MOV AX,SEG OSCSeg		; Move OS Code to 10000h
		ADD AX, 1800h			; 10000h bytes into DOS segment
		MOV DS,AX               ;
		XOR SI, SI 				; Source DS:SI
		MOV AX,1800h            ; Segment 1800h (address 18000h)
		MOV ES,AX               ;
		XOR DI,DI              	; Destination ES:DI
		MOV CX,8000h			; 64K Max (CODE is > 64K)
		CLD                    	;
		REP MOVSB				;

		MOV AX,SEG OSCSeg		; Move OS Code to 10000h
		ADD AX, 2000h			;
		MOV DS,AX               ;
		XOR SI, SI 				; Source DS:SI
		MOV AX,2000h            ; Segment 1000h (address 10000h)
		MOV ES,AX               ;
		XOR DI,DI              	; Destination ES:DI
		MOV CX, OSCodeMore+4;
		CLD                    	;
		REP MOVSB				;

		LIDT CS:FWORD PTR IDTptr ; Load IDTR
		LGDT CS:FWORD PTR GDTptr ; Load GDTR

		MOV EAX,CR0             ; Set Protected Mode BIT
		OR AL,1                 ;
		MOV CR0,EAX             ;

		JMP Next                ; Clear Instruction Prefetch Queue
Next:
        MOV BX,DataSel 			; Setup Data Selectors
		MOV DS,BX               ; D Selector
		MOV ES,BX               ; E Selector
		MOV FS,BX               ; F Selector
		MOV GS,BX               ; G Selector
		MOV SS,BX               ; S Selector
;
; PROTECTED MODE FAR JUMP TO 8:Offset OS code
;
		DB 066h                 ; 32 bit Data prefix
		DB 067h                 ; 32 bit Address instruction prefix
		DB 0EAh                 ; far jump opcode
		DD OFFSET OSInitBegin   ; 32 bit Offset   ( Code Offset )
		DW OSCodeSel            ; 16 bit Selector ( Code Selector )

LoadSeg        ENDS

		END START
