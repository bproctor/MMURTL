;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED   Version 1.0
;=============================================================================
;
;
;-------------------------------------------------------
; Each Task State Segment (Structure) is 512 bytes long
; The Task State Segment is a structure required by the
; CPU for harware task switching. It is used to save
; the state of the processor for a task that is suspended.
; The rest of the TSS is used to save software state information
; for the OS.
;
; Space is also reserved for Math Coprocessor registers
; which will be used when we add support for management of tasks
; using the coprocessor.  The top potions for registers
; are aligned as required for the hardware (processor).
; The rest is WORD or DWORD aligned for best efficiency.
;
sTSS		EQU 512

TSS_BackLink	EQU 0	;DW 0000h,0000h		; Task Selector of Interrupted TASK
TSS_ESP0		EQU 4	;DD 00000000h		; Top of Stack for Prot Level 0
TSS_SS0			EQU 8	;DW 0000h,0000h		; Selector for Prot Level 0 Stack
TSS_ESP1		EQU 12	;DD 00000000h		; Top of Stack for Prot Level 1
TSS_SS1			EQU 16	;DW 0000h,0000h		; Selector for Prot Level 1 Stack
TSS_ESP2		EQU 20	;DD 00000000h		; Top of Stack for Prot Level 2
TSS_SS2			EQU 24	;DW 0000h,0000h		; Selector for Prot Level 2 Stack
TSS_CR3			EQU 28	;DD 00000000h		; Physical Address of Page Directory
TSS_EIP			EQU 32	;DD 00000000h		; Extended Instruction Pointer
TSS_EFlags		EQU 36	;DD 00000000h		; Extended Flags
TSS_EAX			EQU 40	;DD 00000000h		; General Purpose Registers
TSS_ECX			EQU 44	;DD 00000000h		;
TSS_EDX			EQU 48	;DD 00000000h		;
TSS_EBX			EQU 52	;DD 00000000h		;
TSS_ESP			EQU 56	;DD 00000000h		; Top of Stack for Current Prot Level
TSS_EBP			EQU 60	;DD 00000000h		; Base Frame Pointer (64 bytes to here)
TSS_ESI			EQU 64	;DD 00000000h		; Source Index Register
TSS_EDI			EQU 68	;DD 00000000h		; Destination Index Register
TSS_ES			EQU 72	;DW 0000h,0000h		; E Selector (Extra)
TSS_CS			EQU 76	;DW 0000h,0000h		; C Selector (Code)
TSS_SS			EQU 80	;DW 0000h,0000h		; S Selector (Stack Curr Prot Level)
TSS_DS			EQU 84	;DW 0000h,0000h		; D Selector (Data)
TSS_FS			EQU 88	;DW 0000h,0000h		; F Selector (Extra)
TSS_GS			EQU 92	;DW 0000h,0000h		; G Selector (Extra)
TSS_LDT			EQU 96	;DW 0000h,0000h		; LDT Selector ( ALWAYS 0)  (100)
TSS_TrapBit		EQU 100	;DW 0000h			; 1 = DEBUG; 0 = NODEBUG
TSS_IOBitBase	EQU 102	;DW 1111111111111111b	; NULL I/O Permission Bit Map

; This begins MMURTL's software state that we save in the TSS

TSS_Exch		EQU 104	;DD 00000000h		; Task Exchange used by OS ONLY
TSS_pJCB 		EQU 108	;DD 00000000h			; pointer to Job Control Blk (Owner)
Tid				EQU 112	;DW 0000h				; 1 word Task Id (Selector)
Priority		EQU 114	;DB 00h					; 1 byte Priority (0..31)
TSS_Rsvd1		EQU 115	;DB 00h					; For alignment
TSS_MSG			EQU 116	;DD 00000000h,00000000h	; 8 Byte OS Msg Block
pLBRet			EQU 124 ;DD 00000000h			; pLB containing MessageRet (128)
NextTSS 		EQU 128 ;DD 00000000h			; Near Pointer to next TSS (132)
TSSNum	 		EQU 132 ;DW 0000h				; Sequential Number of this TSS

;TSS_Rsvd2		EQU 134	;DB 122 dup (00h)		; Math Coprocessor Registers (future)
;TSS_Rsvd3       ;DD 64 dup (00000000h)   ; Future Expansion

;================== End of module ===========================
