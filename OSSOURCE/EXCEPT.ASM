;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0
.DATA
.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC
.INCLUDE JOB.INC

EXTRN DbgpTSSSave DD
EXTRN dbgFAULT	  DD
EXTRN dbgFltErc	  DD
EXTRN dbgOldEIP	  DD
EXTRN dbgOldCS	  DD
EXTRN dbgOldEFlgs DD

EXTRN DbgTSS	  DB
EXTRN pRunTSS	  DD

.CODE

;This file contains Exception Handlers and Debugger Entry
;and Exit code.

;=============================================================================
; Procedure jumped to by interrupt procedures to enter debugger
;=============================================================================
;EnterDebug
;
; This piece of code sets up to enter the debugger.  If we get here,
; one of the exceptions has activated and has done a little work based
; on which exception is was, then it jumped here to enter the debugger.
; This code effectively replaces the interrupted task with the debugger
; task (without going through the kernel).  First we copy the Page Dir
; entry from the current job into the debuggers job, then copy the CR3
; register from the current TSS into the debugger's TSS.  This makes the
; debugger operate in the current tasks memory space.  All of the debugger's
; code and data are in OS protected pages (which are shared with all tasks),
; so this is OK to do even if the offending task referenced a bogus address.
; Next, we save the current pRunTSS and place the debugger's TSS in
; pRunTSS, then jump to the debugger's selector.  This switches tasks.
;
EnterDebug:
		PUSH EAX					;we MUST save caller's registers
		PUSH EBX					; and restore them before the
		PUSH EDX					; task switch into the debugger

		MOV EAX, pRunTSS			;pRunTSS -> EAX
		MOV EBX, [EAX+TSS_CR3]		;current CR3 -> EBX
		MOV EDX, OFFSET DbgTSS		;pDebuggerTSS -> EDX
		MOV [EDX+TSS_CR3], EBX		;CR3 -> DebuggerTSS

		MOV EAX, [EAX+TSS_pJCB]		;pCrntJCB -> EAX
		MOV EDX, [EDX+TSS_pJCB]		;pDebuggerJCB -> EDX
		MOV EBX, [EAX+JcbPD]		;CrntJob Page Dir -> EBX
		MOV [EDX+JcbPD], EBX		;Page Dir -> Debugger JCB

		MOV EAX, pRunTSS			;Save the current pRunTSS
		MOV DbgpTSSSave, EAX
		MOV EAX, OFFSET DbgTSS		;Install Debugger's as current
		MOV pRunTSS, EAX			;Set Dbgr as running task

		MOV BX, [EAX+Tid]
        MOV TSS_Sel, BX				;Set up debugger selector

		POP EDX						;make his registers right!
		POP EBX
		POP EAX

        JMP FWORD PTR [TSS]			;Switch tasks to debugger

		;When the debugger exits, we come here

		PUSH dbgOldEFlgs			;Put the stack back the way it was
		PUSH dbgOldCS				;
		PUSH dbgOldEIP				;
		IRETD						;Go back to the caller

;=============================================================================
; INTERRUPT PROCEDURES FOR FAULTS
;=============================================================================
; This is the general purpose "We didn't expect this interrupt" interrupt
; This will place "99" in the upper left corner of the screen so we
; can see there are SPURIOUS interrupts!!!!!

PUBLIC INTQ:
		PUSH EAX
		MOV EAX,07390739h       ;99 - All unassigned ints come here
		MOV DS:VGATextBase+00h,EAX
		POP EAX
		IRETD

;===================== Divide By ZERO (Int 0) ============================

PUBLIC IntDivBy0:
;		MOV EAX,07300730h       ;00
;		MOV DS:VGATextBase+00h,EAX
;		CLI
;		HLT

		MOV dbgFAULT,00h        ; Divide By Zero
		POP dbgOldEIP			; Get EIP of offender for debugger
		POP dbgOldCS			;
		POP dbgOldEFlgs			;
		JMP EnterDebug			;Enter debugger

;===================== Debugger Single Step (Int 1) ======================

PUBLIC IntDbgSS:

		POP dbgOldEIP			; Get EIP of offender for debugger
		POP dbgOldCS			;
		POP dbgOldEFlgs			;
		JMP EnterDebug			;Enter debugger

;===================== Debugger Entry (Int 3) ===========================

PUBLIC IntDebug:

		POP dbgOldEIP			; Get EIP of offender for debugger
		POP dbgOldCS			; Get CS
		POP dbgOldEFlgs			; Get Flags
		JMP EnterDebug			;Enter debugger

;===================== OverFlow (Int 4) ==================================

PUBLIC IntOverFlow:
		PUSH EAX
		MOV EAX,07340730h       ;04
		MOV DS:VGATextBase+00h,EAX
		CLI
		HLT

;========================== Bad Opcode (Int 6) ================================
PUBLIC INTOpCode:
;		MOV EAX,07360730h       ;06
;		MOV DS:VGATextBase+00h,EAX
;		HLT

		MOV dbgFAULT,06h        ; Invalid Opcode
		POP dbgOldEIP			; Get EIP of offender for debugger
		POP dbgOldCS			;
		POP dbgOldEFlgs			;
		JMP EnterDebug			;Enter debugger

;========================== Dbl Exception (Int 08)============================
PUBLIC IntDblExc:
;		MOV EAX,07380730h       ;08
;		MOV DS:VGATextBase+00h,EAX
;		CLI
;		HLT

		MOV dbgFAULT,08h        ; Double Exception
		POP dbgFltErc			; Error Code pushed last by processor
		POP dbgOldEIP
		POP dbgOldCS
		POP dbgOldEFlgs
		POP dbgOldEFlgs			;
		JMP EnterDebug			;Enter debugger

;========================= Invalid TSS 10 ====================================
PUBLIC INTInvTss:
		MOV EAX,07300731h       ;10
		MOV DS:VGATextBase+00h,EAX
		CLI
		HLT

		MOV dbgFAULT,0Ah        ; Invalid TSS
		POP dbgFltErc			; Error code pushed last by processor
		POP dbgOldEIP
		POP dbgOldCS
		POP dbgOldEFlgs
		JMP EnterDebug			;Enter debugger

;========================== Seg Not Present 11 ===============================

PUBLIC INTNoSeg:
		MOV EAX,07310731h       ;11
		MOV DS:VGATextBase+00h,EAX
		CLI
		HLT

;========================== Stack Overflow 12 ================================
PUBLIC INTStkOvr:
;		MOV EAX,07320731h       ;12
;		MOV DS:VGATextBase+00h,EAX
;		CLI
;		HLT

		MOV dbgFAULT,0Ch        ; Stack overflow
		POP dbgFltErc
		POP dbgOldEIP
		POP dbgOldCS
		POP dbgOldEFlgs
		JMP EnterDebug			;Enter debugger

;========================== GP 13  ===========================================
PUBLIC IntGP:
;		MOV EAX,07330731h       ;13
;		MOV DS:VGATextBase+00h,EAX
;		CLI
;		HLT

		MOV dbgFAULT,0Dh        ; GP Fault
		POP dbgFltErc
		POP dbgOldEIP
		POP dbgOldCS
		POP dbgOldEFlgs
		JMP EnterDebug			;Enter debugger

;========================== Page Fault 14 ====================================
PUBLIC INTPgFlt:
;		MOV EAX,07340731h       ;14
;		MOV DS:VGATextBase+00h,EAX
;		CLI
;		HLT

		MOV dbgFAULT,0Eh        ; Page Fault
		POP dbgFltErc
		POP dbgOldEIP
		POP dbgOldCS
		POP dbgOldEFlgs
		JMP EnterDebug			;Enter debugger

;=============================================================================
; ISR for interrupt on PICU1 from Slave
;=============================================================================

PUBLIC IntPICU2:                           ; IRQ Line 2 from Slave 8259 (Int22)
		PUSH EAX
		MOV EAX,07320750h       	; From PICU#2 (P2)
		MOV DS:VGATextBase+100h,EAX
		PUSH 2
		CALL FWORD PTR _EndOfIRQ
		POP EAX
		IRETD

;=============== end of module ==================
