;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED   Version 1.0
;=============================================================================
.DATA
.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC
.INCLUDE RQB.INC
.INCLUDE JOB.INC

.ALIGN DWORD

;External near Variables

EXTRN rgTmrBlks DD
EXTRN pJCBs 	DD
EXTRN RdyQ  	DD
EXTRN rgPAM  	DB

;debugger variables and buffers

PUBLIC DbgpTSSSave	DD 0		;TO save the TSS we interrupted
PUBLIC dbgFAULT	    DD 0FFh		;0FFh is NO FAULT
PUBLIC dbgFltErc	DD 0
PUBLIC dbgOldEIP	DD 0
PUBLIC dbgOldCS		DD 0
PUBLIC dbgOldEflgs	DD 0

DbgVidSave	DD 0			;To save interrupted video user
dbgBuf		DB '00000000'	;General use buffers
dbgBuf2 	DB '0000000000'

.ALIGN DWORD
cbBufLen2	DD 0			;Active bytes in buf2
dbgKeyCode	DD 0			;For ReadDbgKbd
NextEIP		DD 0			;For Disassem display (next ins to be executed)
dbgGPdd1	DD 0		    ;General purpose DDs (used all over)
dbgGPdd2	DD 0		    ;
dbgGPdd3	DD 0		    ;
dbgGPdd4	DD 0		    ;
dbgGPdd5	DD 0		    ;
dbgGPdd6	DD 0		    ;flag 1 = Msg, 0 = Exch

dbgNextAdd	DD 0		    ;Address we are setting as next
dbgCrntAdd	DD 0		    ;Address of instructions we are displaying
dbgDumpAdd	DD 0		    ;Address we are dumping

dbgX		DD 0			;For line and column display coordination
dbgY		DD 0

dbgBPAdd	DD 0			;Crnt Breakpoint Linear addresses

dbgfDumpD	DB 0		    ;Boolean- are we dumping DWORDS?
fDbgInit	DB 0			;Has Dbg Video been initialized?

dbgCRLF 	DB 0Dh, 0Ah	    ;CR LF for Dump etc...
dbgChar 	DB 0

dbgMenu 	DB 'SStep',0B3h,'SetBP',0B3h,'ClrBP',0B3h,'CS:EIP  '
			DB 'Exchs',0B3h,'Tasks',0B3h,'     ',0B3h,'CrntAddr'
			DB 'DumpB',0B3h,'DumpD',0B3h,'     ',0B3h,'AddInfo '
dbgSpace	DB '      '
dbgCont 	DB 'ENTER to continue, ESC to quit.'
dbgClear 	DB '                                        '  ;40 bytes
dbgAsterisk	DB 42

;               0123456789012345678901234567890123456789012345678901234
dbgExchMsg	DB 'Exch      Owner     dMsgLo    dMsgHi              Task'

;               0123456789012345678901234567890123456789012345678901234
dbgTaskMsg	DB 'Task#     Job       pJCB      pTSS      Priority      '

;For Debugger entry conditions
dbgFltMsg	DB 'FATAL Processor Exception/Fault: '
sdbgFltMsg	DD $-dbgFltMsg

	;Register display text

dbgTxt00	DB 0B3h,'TSS: '
dbgTxt01	DB 0B3h,'EAX: '
dbgTxt02	DB 0B3h,'EBX: '
dbgTxt03	DB 0B3h,'ECX: '
dbgTxt04	DB 0B3h,'EDX: '
dbgTxt05	DB 0B3h,'ESI: '
dbgTxt06	DB 0B3h,'EDI: '
dbgTxt07	DB 0B3h,'EBP: '
dbgTxt08	DB 0B3h,' SS: '
dbgTxt09	DB 0B3h,'ESP: '
dbgTxt10	DB 0B3h,' CS: '
dbgTxt11	DB 0B3h,'EIP: '
dbgTxt12	DB 0B3h,' DS: '
dbgTxt13	DB 0B3h,' ES: '
dbgTxt14	DB 0B3h,' FS: '
dbgTxt15	DB 0B3h,' GS: '
dbgTxt16	DB 0B3h,'EFL: '
dbgTxt17	DB 0B3h,'CR0: '
dbgTxt18	DB 0B3h,'CR2: '
dbgTxt19	DB 0B3h,'CR3: '
dbgTxt20	DB 0B3h,'Erc: '

dbgTxtAddr	DB 'Linear address: '

	;For Important Address Info Display

dbgM0	     DB 'IDT:  '
dbgM1	     DB 'GDT:  '
dbgM2	     DB 'RQBs: '
dbgM3	     DB 'TSS1: '
dbgM4	     DB 'TSS3: '
dbgM5	     DB 'LBs:  '
dbgM6	     DB 'RdyQ: '
dbgM7	     DB 'JCBs: '
dbgM8	     DB 'SVCs: '
dbgM9	     DB 'Exch: '
dbgPA	     DB 'PAM:  '
dbgMB	     DB 'aTmr: '

;==================== End Data, Begin Code ==========================

.CODE

EXTRN DDtoHex NEAR
EXTRN HexToDD NEAR
EXTRN _disassemble NEAR
EXTRN ReadDbgKbd NEAR

PUBLIC DbgTask:

		MOV EAX, OFFSET DbgVidSave	;Save number of vid we interrupted
		PUSH EAX
		CALL FWORD PTR _GetVidOwner

		STI

		PUSH 2
		CALL FWORD PTR _SetVidOwner		;Dbgr is Job 2

		CMP fDbgInit, 0
		JNE DbgInitDone
		CALL FWORD PTR _ClrScr
		MOV fDbgInit, 1

DbgInitDone:

		MOV EAX, DbgpTSSSave

		;When a fault or debug exception occurs, the values of
		;the Instruction Pointer, Code Seg, and flags are not the
		;way they were when the exception fired off becuase of the
		;interrupt procedure they enterred to get to the debugger.
		;We make them the same by putting the values we got from
		;the stack (entering the debugger) into the caller's TSS.
		;
		MOV EBX,dbgOldEflgs			;Store correct flags
		MOV [EAX+TSS_EFlags],EBX	;EAX still has DbgpTSSSave
		MOV EBX,dbgOldCS			;Store correct CS
		MOV [EAX+TSS_CS],BX
		MOV EBX,dbgOldEIP			;Store correct EIP
		MOV [EAX+TSS_EIP],EBX
		;
		;NOTE: The "book" says the TF flag is reset by the processor
		; when the handler is entered. This only applies if
		; the handler is a procedure (NOT a task).  The debugger
		; is always entered as a procedure, (we chanage the tasks)
		; so we shouldn't have to reset it.  But we do...
		; I guess I'm not reading it right or ROD SERLING LIVES!
		;
		MOV EBX,[EAX+TSS_EFlags]	;Reset TF in case single steping
		AND EBX,0FFFFFEFFh
		MOV [EAX+TSS_EFlags],EBX

		;We set the FAULT variable based on which interrupt
		;procedure was entered.

		CMP DWORD PTR dbgFAULT,0FFh	;Was the dbgr entered on a FAULT?
		JE dbg000					;NO
		;
		;NOTE: Must eventually add SS/ESP for a change in CPL on faults!!!
		;REF - See page 3-4 System Software Writer's Guide

		XOR EAX, EAX
		PUSH EAX					;Display fault message and
		PUSH EAX					;  and number at 0 ,0
		CALL FWORD PTR _SetXY

		LEA EAX,dbgFltMsg
		PUSH EAX
		PUSH sdbgFltMsg
		PUSH 40h					;Color Black on RED
		CALL FWORD PTR _TTYOut

		MOV EAX,dbgFAULT
		PUSH EAX
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		LEA EAX,dbgBuf
		PUSH EAX
		PUSH 8
		PUSH 70h
		CALL FWORD PTR _TTYOut
		MOV DWORD PTR dbgFAULT, 0FFh				;reset fault indicator

		LEA EAX,dbgCRLF
		PUSH EAX
		PUSH 2
		PUSH 07h					;Color White on black
		CALL FWORD PTR _TTYOut
		XOR EAX, EAX
		MOV dbgX, EAX				;Reset X & Y to 0,1
		INC EAX
		MOV dbgY, EAX

;===================================================================
dbg000:
 		CALL DbgRegVid				;Display BackLink's Register values
		CALL dbgDispMenu			;Display menu
		PUSH dbgX					;Back to where we were
		PUSH dbgY
		CALL FWORD PTR _SetXY


		;Display Instruction at CS:EIP
		MOV EBX,DbgpTSSSave			;Get USER pUserTSS
		MOV EAX, [EBX+TSS_EIP]
		MOV dbgCrntAdd, EAX
		CALL dbgShowBP
		PUSH EAX
		CALL _disassemble			;This puts the instruction on the line
		MOV NextEIP, EAX
		CALL dbgCheckScroll			;Fall through to keyboard loop

;===========================================================
		;Now we read the keyboard
dbg00:
		MOV EAX, OFFSET dbgKeyCode
		PUSH EAX
		CALL ReadDbgKbd				;
		MOV EAX, dbgKeyCode
		AND EAX, 0FFh				;Lop off key status bytes

		CMP EAX, 1Bh				;ESCAPE (Exit)
		JE dbgExit

		CMP EAX, 0Fh				;Single Step (F1)
		JNE dbg02
		MOV EBX,DbgpTSSSave			;Get USER pUserTSS
		MOV ECX,[EBX+TSS_EFlags]	;
		OR ECX,00000100h			;Set TF in flags for single step
		MOV [EBX+TSS_EFlags],ECX
		JMP dbgExit
dbg02:
		CMP EAX, 10h				;Set BP (Current Address - F2)
		JNE dbg03
		MOV EBX, dbgCrntAdd			;Address displayed
		MOV dbgBPAdd, EBX			;Save it so we know where BP is
		MOV DR0, EBX				;Move into BreakPoint Reg 0
		XOR EAX, EAX
		MOV DR6, EAX
		MOV EAX, 00000002h			;BP0 Set global
		MOV DR7, EAX
		JMP dbg00					;
dbg03:
		CMP EAX, 11h				;Clear BP (Current Address - F3)
		JNE dbg04
		XOR EAX, EAX
		MOV dbgBPAdd, EAX			;Clear BP saved address
		MOV DR7, EAX
		JMP dbg00					;
dbg04:
		CMP EAX, 12h				;Return to CS:EIP (F4)
		JNE dbg05
		MOV EBX,DbgpTSSSave			;Get USER pUserTSS
		MOV EAX, [EBX+TSS_EIP]
		MOV dbgCrntAdd, EAX
		CALL dbgShowBP
		PUSH EAX
		CALL _disassemble			;This puts the instruction on the line
		MOV NextEIP, EAX
		CALL dbgCheckScroll			;See if we need to scroll up
		JMP dbg00					;
dbg05:
		CMP EAX, 13h				;Display Exchanges (F5)
		JNE dbg06
		CALL dbgDispExchs
		JMP dbg000					;Full display
dbg06:
		CMP EAX, 14h				;Task array display (F6)
		JNE dbg07
		CALL dbgDispTasks
		JMP dbg000					;
dbg07:
		CMP EAX, 15h				;Not used yet
		JNE dbg08
		JMP dbg00					;
dbg08:
		CMP AL, 16h				 	;Set Disassembly Address (F8)
		JNE dbg09
		CALL dbgSetAddr				;Sets NextEIP
		PUSH dbgX					;Back to where we were
		PUSH dbgY
		CALL FWORD PTR _SetXY
		MOV EAX, NextEIP
		MOV dbgCrntAdd, EAX
		CALL dbgShowBP
		PUSH EAX
		CALL _disassemble			;This puts the instruction on the line
		MOV NextEIP, EAX
		CALL dbgCheckScroll			;See if we need to scroll up
		JMP dbg00					;
dbg09:
		CMP AL, 17h				 	;Memory Dump Bytes (F9)
		JNE dbg10
		MOV BL, 00
		MOV dbgfDumpD, BL
		CALL dbgDump				;
		JMP dbg000
dbg10:
		CMP AL, 18h					;Memory Dump DWORDS (F10)
		JNE dbg12
		MOV BL, 0FFh
		MOV dbgfDumpD, BL
		CALL dbgDump				;
		JMP dbg000
dbg12:
		CMP AL, 01Ah				;Info Address dump (F12)
		JNE dbg13
		CALL DbgInfo				;
		JMP dbg00

dbg13:	CMP AL, 02h					;Display next Instruction (Down Arrow)
		JNE dbg14
		MOV EAX, NextEIP
		MOV dbgCrntAdd, EAX
		CALL dbgShowBP
		PUSH EAX
		CALL _disassemble			;This puts the instruction on the line
		MOV NextEIP, EAX
		CALL dbgCheckScroll			;See if we need to scroll up
		JMP dbg00

dbg14:
		JMP dbg00				;GO back for another key

DbgExit:

		LEA EAX,dbgX			;Query XY
		PUSH EAX
		LEA EAX,dbgY
		PUSH EAX
		CALL FWORD PTR _GetXY

		PUSH DbgVidSave
		CALL FWORD PTR _SetVidOwner	;Change screens back

		MOV EAX, DbgpTSSSave		;Return saved pRunTSS
		MOV pRunTSS, EAX
		MOV BX, [EAX+Tid]
        MOV TSS_Sel, BX				;Set up caller's TSS selector

		JMP FWORD PTR [TSS]

		;Next time we enter the debugger task it will be here!
		JMP	DbgTask					;Back to begining

;==============================================================

dbgShowBP:
		;This compares the current breakpoint the address
		;we are about to display. If they are the same,
		;we put up an asterisk to indicate it's the breakpoint
		;EAX must be preserved

		MOV EBX, dbgCrntAdd			;Address displayed
		MOV ECX, dbgBPAdd			;BP Address
		CMP EBX, ECX
		JZ dbgShowBP1
		RETN
dbgShowBP1:
		PUSH EAX					;Save EAX across call
		PUSH OFFSET dbgAsterisk		;3 params to TTYOut
		PUSH 01h
		PUSH 47h					;Reverse Vid WHITE on RED
		CALL FWORD PTR _TTYOut
		POP EAX
		RETN

;==============================================================
;This sets up and calls to display all of the regsiter
;information on the right side of the debugger video display

DbgRegVid:
		MOV EBX,DbgpTSSSave			;EBX MUST be DbgpTSSSave
		MOV ECX,00					;TSS Display
		MOV ESI,OFFSET DbgTxt00
		XOR EAX,EAX
		MOV AX,[EBX+TSSNum]			;Number of this TSS
		CALL DispRegs

		MOV ECX,01					;EAX Display
		MOV ESI,OFFSET DbgTxt01
		MOV EAX,[EBX+TSS_EAX]
		CALL DispRegs

		MOV ECX,02					;EBX Display
		MOV ESI,OFFSET DbgTxt02
		MOV EAX,[EBX+TSS_EBX]
		CALL DispRegs

		MOV ECX,03					;ECX Display
		MOV ESI,OFFSET DbgTxt03
		MOV EAX,[EBX+TSS_ECX]
		CALL DispRegs

		MOV ECX,04					;EDX Display
		MOV ESI,OFFSET DbgTxt04
		MOV EAX,[EBX+TSS_EDX]
		CALL DispRegs

		MOV ECX,05					;ESI Display
		MOV ESI,OFFSET DbgTxt05
		MOV EAX,[EBX+TSS_ESI]
		CALL DispRegs

		MOV ECX,06					;EDI Display
		MOV ESI,OFFSET DbgTxt06
		MOV EAX,[EBX+TSS_EDI]
		CALL DispRegs

		MOV ECX,07					;EBP Display
		MOV ESI,OFFSET DbgTxt07
		MOV EAX,[EBX+TSS_EBP]
		CALL DispRegs

		MOV ECX,08					;SS Display
		MOV ESI,OFFSET DbgTxt08
		XOR EAX,EAX
		MOV AX,[EBX+TSS_SS]
		CALL DispRegs

		MOV ECX,09					;ESP Display
		MOV ESI,OFFSET DbgTxt09
		MOV EAX,[EBX+TSS_ESP]
		CALL DispRegs

		MOV ECX,10					;CS Display
		MOV ESI,OFFSET DbgTxt10
		XOR EAX,EAX
		MOV AX,[EBX+TSS_CS]
		CALL DispRegs

		MOV ECX,11					;EIP Display
		MOV ESI,OFFSET DbgTxt11
		MOV EAX,[EBX+TSS_EIP]
		CALL DispRegs
		MOV ECX,12					;DS Display
		MOV ESI,OFFSET DbgTxt12
		XOR EAX,EAX
		MOV AX,[EBX+TSS_DS]
		CALL DispRegs
		MOV ECX,13					;ES Display
		MOV ESI,OFFSET DbgTxt13
		XOR EAX,EAX
		MOV AX,[EBX+TSS_ES]
		CALL DispRegs
		MOV ECX,14					;FS Display
		MOV ESI,OFFSET DbgTxt14
		XOR EAX,EAX
		MOV AX,[EBX+TSS_FS]
		CALL DispRegs
		MOV ECX,15					;GS Display
		MOV ESI,OFFSET DbgTxt15
		XOR EAX,EAX
		MOV AX,[EBX+TSS_GS]
		CALL DispRegs
		MOV ECX,16					;EFlags Display
		MOV ESI,OFFSET DbgTxt16
		MOV EAX,[EBX+TSS_EFlags]
		CALL DispRegs
		MOV ECX,17					;CR0 Display
		MOV ESI,OFFSET DbgTxt17
		MOV EAX,CR0
		CALL DispRegs
		MOV ECX,18					;CR2 Display
		MOV ESI,OFFSET DbgTxt18
		MOV EAX,CR2
		CALL DispRegs
		MOV ECX,19					;CR3 Display
		MOV ESI,OFFSET DbgTxt19
		MOV EAX,CR3
		CALL DispRegs
		MOV ECX,20					;Fault Error Code Display
		MOV ESI,OFFSET DbgTxt20
		MOV EAX,dbgFltErc
		CALL DispRegs
		RETN
;=============================================================================
;
; This is for Debugger Register display
; Call with: EAX loaded with value to display (from TSS reg)
;		 ECX loaded with number of text line to display on
;		 ESI loaded with EA of text line to display
;   We save all registers cause the vid calls don't

DispRegs:
		PUSHAD

		PUSH EAX		   ;Save number to display

		PUSH 66
		PUSH ECX
		CALL FWORD PTR _SetXY

		PUSH ESI
		PUSH 05h
		PUSH 07h
		CALL FWORD PTR _TTYOut

		POP EAX 		   ;Get number back for display

		PUSH EAX
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH OFFSET dbgBuf
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _TTYOut
		POPAD
		RETN
;==============================================
; This displays the debugger function key menu

dbgDispMenu:
		PUSH 0				;Display Debugger FKey Menu
		PUSH 24
		CALL FWORD PTR _SetXY

		LEA EAX,dbgMenu
		PUSH EAX
		PUSH 78
		PUSH 70h
		CALL FWORD PTR _TTYOut

		PUSH 25
		PUSH 24
		CALL FWORD PTR _SetXY

		LEA EAX,dbgSpace
		PUSH EAX
		PUSH 1
		PUSH 07h
		CALL FWORD PTR _TTYOut

		PUSH 51
		PUSH 24
		CALL FWORD PTR _SetXY

		LEA EAX,dbgSpace
		PUSH EAX
		PUSH 1
		PUSH 07h
		CALL FWORD PTR _TTYOut

		RETN
;===========================
;Allows the user to pick the currently displayed address

dbgSetAddr:
		PUSH 0						;Goto Query Line
		PUSH 23						;
		CALL FWORD PTR _SetXY

		LEA EAX, dbgTxtAddr
		PUSH EAX
		PUSH 16
		PUSH 07h
		CALL FWORD PTR _TTYOut
		CMP EAX, 0
		JNE DumpDone

		LEA EAX, DbgBuf2		;
		PUSH EAX				;pEdString
		PUSH cbBufLen2			;Crnt size
		PUSH 8					;Max size
		LEA EAX, cbBufLen2		;
		PUSH EAX				;ptr to size returned
		LEA EAX, dbgChar		;
		PUSH EAX				;ptr to char returned
		PUSH 70h				;Black On White
		CALL FWORD PTR _EditLine	;Ignore error if any

		MOV AL, dbgChar			;did they exit with CR?
		CMP AL, 0Dh
		JNE dbgSetAddrDone

		LEA EAX, dbgBuf2		;Convert String to DD
		PUSH EAX				;ptr to string
		LEA EAX, dbgNextAdd
		PUSH EAX				;ptr to destination DD
		PUSH cbBufLen2			;length of string
		CALL HexToDD			;dbgDumpAdd has address to dump!
		CMP EAX, 0
		JNE dbgSetAddrDone

		MOV EAX, dbgNextAdd
		MOV NextEIP, EAX
dbgSetAddrDone:
		CALL dbgClearQuery
		RETN					;Go home...
;===========================
;Queries user for address then dumps data to screen

dbgDump:
		PUSH 0						;Goto Query Line
		PUSH 23						;
		CALL FWORD PTR _SetXY

		LEA EAX, dbgTxtAddr
		PUSH EAX
		PUSH 16
		PUSH 07h
		CALL FWORD PTR _TTYOut
		CMP EAX, 0
		JNE DumpDone

		LEA EAX, DbgBuf2		;
		PUSH EAX				;pEdString
		PUSH cbBufLen2			;Crnt size
		PUSH 8					;Max size
		LEA EAX, cbBufLen2		;
		PUSH EAX				;ptr to size returned
		LEA EAX, dbgChar		;
		PUSH EAX				;ptr to char returned
		PUSH 70h				;Black On White
		CALL FWORD PTR _EditLine	;Ignore error if any

		MOV AL, dbgChar			;did they exit with CR?
		CMP AL, 0Dh
		JE dbgDoDump
		CALL dbgClearQuery
		RETN					;Go home...

dbgDoDump:
		LEA EAX, dbgBuf2		;Convert String to DD
		PUSH EAX				;ptr to string
		LEA EAX, dbgDumpAdd
		PUSH EAX				;ptr to destination DD
		PUSH cbBufLen2			;length of string
		CALL HexToDD			;dbgDumpAdd has address to dump!
		CMP EAX, 0
		JNE DumpDone

		CALL FWORD PTR _ClrScr

dbgDump00:
		MOV DWORD PTR dbgGPdd1, 24	;line counter begins at 24
dbgDump01:
		MOV DWORD PTR dbgGPdd3, 4 	;number of quads per line
		PUSH dbgDumpAdd 	;convert address to text
		LEA EAX, dbgBuf
		PUSH EAX
		CALL DDtoHex

		LEA EAX, dbgBuf
		PUSH EAX
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _TTYOut
		CMP EAX, 0
		JNE DumpDone
dbgDump02:
		MOV DWORD PTR dbgGPdd2, 6 	;byte offset begins at 6
		LEA EAX, dbgSpace
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut
		CMP EAX, 0
		JNE DumpDone
		MOV EBX, dbgDumpAdd ;get dump address
		MOV EAX, [EBX]	    ;Get what it's pointing to
		PUSH EAX			;make it a DD Text
		LEA EAX, dbgBuf
		PUSH EAX
		CALL DDtoHex
		MOV AL, dbgfDumpD	;Dumping DWORDS
		CMP AL, 0
		JE DumpB			;NO - go to display bytes

		LEA EAX, dbgBuf		;Yes display Quad
		PUSH EAX
		PUSH 8
		PUSH 07
		CALL FWORD PTR _TTYOut
		JMP DumpDin

 dumpB:
		LEA EAX, dbgBuf			;Display First byte
		ADD EAX, dbgGPdd2
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut	;ignore error

		LEA EAX, dbgSpace       ;Display 1 spaces
		PUSH EAX
		PUSH 1
		PUSH 07h
		CALL FWORD PTR _TTYOut
		DEC dbgGPdd2			;point to second 2 bytes
		DEC dbgGPdd2

		LEA EAX, dbgBuf 	    ;display 2st byte
		ADD EAX, dbgGPdd2
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut	;ignore error

		LEA EAX, dbgSpace		; display 1 space
		PUSH EAX
		PUSH 1
		PUSH 07h
		CALL FWORD PTR _TTYOut

		DEC dbgGPdd2
		DEC dbgGPdd2

		LEA EAX, dbgBuf    		 ;display 3rd byte
		ADD EAX, dbgGPdd2
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut	;ignore error

		LEA EAX, dbgSpace		;a space
		PUSH EAX
		PUSH 1
		PUSH 07h
		CALL FWORD PTR _TTYOut
		DEC dbgGPdd2
		DEC dbgGPdd2

		LEA EAX, dbgBuf         ;display 4th byte
		ADD EAX, dbgGPdd2
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut	;ignore error
DumpDin:
		INC dbgDumpAdd
		INC dbgDumpAdd
		INC dbgDumpAdd
		INC dbgDumpAdd
		DEC dbgGPdd3			;done with 4 quads??
		JNZ dbgDump02			;NO - go back for next 4 bytes

		LEA EAX, dbgSpace		;Yes - Display 2 spaces
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut

		LEA EAX,dbgX			;Query XY
		PUSH EAX
		LEA EAX,dbgY
		PUSH EAX
		CALL FWORD PTR _GetXY

		PUSH dbgX		  		;Put 16 TEXT chars on right
		PUSH dbgY
		MOV EAX, dbgDumpAdd
		SUB EAX, 16
		PUSH EAX
		PUSH 16
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;ignore error

		LEA EAX, dbgCRLF        ;Do CR/LF
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut
		;
		DEC dbgGPdd1		;23lines yet??
		JNZ dbgDump01		;NO
		;
		LEA EAX, dbgCont    ;"Continue" Text
		PUSH EAX
		PUSH 31 		    ;size of "cont text"
		PUSH 07h
		CALL FWORD PTR _TTYOut
dbgDump03:
		MOV EAX, OFFSET dbgKeyCode
		PUSH EAX
		CALL ReadDbgKbd
		MOV EAX, dbgKeyCode
		AND EAX, 0FFh				;Lop off key status bytes
		CMP EAX, 0
		JE dbgDump03
		CMP EAX, 1Bh		    ;Escape (Quit??)
		JE DumpDone

		LEA EAX, dbgCRLF	    ;Do CR/LF
		PUSH EAX
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut
		JMP dbgDump00

		;
 DumpDone:
 		CALL FWORD PTR _ClrScr
 		MOV DWORD PTR dbgX, 0
 		MOV DWORD PTR dbgY, 0
		RETN
;===========================

;Displays exchanges that are allocated along with
;messages or tasks that may be wiating at them

dbgDispExchs:

		MOV DWORD PTR dbgGPdd2, 0		;Exch# we are on
dbgDE00:
		CALL FWORD PTR _ClrScr
		PUSH 0					;Col
		PUSH 0					;Line for labels
		PUSH OFFSET dbgExchMsg	;
		PUSH 54					;Length of message
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;
		MOV DWORD PTR dbgGPdd1, 1			;line we are one

		;First we do the exchange on the current line
dbgDE01:
		PUSH dbgGPdd2 			;Convert Exch number for display
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH 0					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;ExchNum
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		;Then we do the Exch Owner (Job Number) next to it

		MOV EAX, dbgGPdd2		; Exch number
		MOV EDX, sEXCH          ; Compute offset of Exch in rgExch
		MUL EDX                 ;
		MOV EDX,prgExch         ; Add offset of rgExch => EAX
		ADD EAX,EDX             ; EAX now pts to Exch
		MOV dbgGPdd3, EAX		; pExch into save variable
		MOV EBX, [EAX+Owner]	; pJCB of Owner into EBX
		XOR EAX, EAX			; Clear for use as JobNum
		OR EBX, EBX				; pNIL? (No owner if so)
		JZ dbgDE03
		MOV EAX, [EBX+JobNum]	;
dbgDE03:
		PUSH EAX	 			;Convert Job Number
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH 10					;Col
		PUSH dbgGPdd1			;Line
		PUSH OFFSET dbgBuf		;
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		MOV dbgGPdd5, 0			;Set pNextMsg to 0

		;See if there is a first message

		MOV EAX, dbgGPdd3		;pExch -> EAX
		MOV EBX, [EAX+EHead]	;pMsg -> EBX
		OR EBX, EBX				;Is is NIL (no msg or task)?
		JZ dbgDE13				;Yes. Go to next Exch

		MOV EBX, [EAX+fEMsg]	;MsgFlag -> EBX
		OR EBX, EBX				;Is is 1 (a message)?
		JZ dbgDE05				;No, Go check for tasks
		MOV EBX, [EAX+EHead]	;pMsg -> EBX

		;Display Messages
dbgDE04:
		MOV DWORD PTR dbgGPdd6, 0	;Flag to indicate we are doing messages
		MOV EAX, [EBX+NextLB]	;For next msg in chain (if it exists)
		MOV dbgGPdd5, EAX		;Save for loop
		MOV EAX, [EBX+DataHi]	;Get dMsg1
		MOV EDX, [EBX+DataLo]	;Get dMsg2
		PUSH EDX				;Save dMsg2

		PUSH EAX	 			;Convert dMsg1
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH 20					;Col
		PUSH dbgGPdd1			;Line
		PUSH OFFSET dbgBuf		;
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		POP EDX					;Get dMsg2 back

		;Could have left it on stack, but would be confusing later...
		;"simplicity of maintenance is as important as simplicity of design"

		PUSH EDX	 			;Convert dMsg2
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH 30						;Col
		PUSH dbgGPdd1				;Line
		PUSH OFFSET dbgBuf			;
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;
		JMP  dbgDE07				;Next line please

		;See if there are tasks waiting
dbgDE05:
		MOV DWORD PTR dbgGPdd6, 1	;Flag to indicate we are doing tasks
		MOV DWORD PTR dbgGPdd5, 0	;Clear pNextTask
		MOV EAX, dbgGPdd3		;pExch -> EAX
		MOV EBX, [EAX+EHead]	;pTSS -> EBX
		OR EBX, EBX				;Is is 0 (no TSS)?
		JZ dbgDE07				;
dbgDE06:
		MOV EAX, [EBX+NextTSS]
		MOV dbgGPdd5, EAX		;Save ptr to next task if it exists
		XOR EAX, EAX
		MOV AX, [EBX+TSSNum]	;Get Number of Task at exch

		PUSH EAX	 			;Convert Task Number
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH 50					;Col
		PUSH dbgGPdd1			;Line
		PUSH OFFSET dbgBuf		;
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

dbgDE07:
		INC dbgGPdd1			;Next line
		CMP DWORD PTR dbgGPdd1, 23		;23 lines yet?
		JB dbgDE09				;No
		;
dbgDE08:
		PUSH 0					;Col
		PUSH 24					;Line
		PUSH OFFSET dbgCont		;
		PUSH 31					;length of Cont string
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		MOV EAX, OFFSET dbgKeyCode
		PUSH EAX
		CALL ReadDbgKbd
		MOV EAX, dbgKeyCode
		AND EAX, 0FFh				;Lop off key status bytes
		CMP EAX, 1Bh			    ;Escape (Quit??)
		JE dbgDEDone

		CMP DWORD PTR dbgGPdd2, nDynEXCH	; Number of dynamic exchanges
		JAE dbgDEDone				; All Exchs displayed

		CALL FWORD PTR _ClrScr		;
		PUSH 0						;Col
		PUSH 0						;Line for labels
		PUSH OFFSET dbgExchMsg		;
		PUSH 54						;Length of message
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;
		MOV DWORD PTR dbgGPdd1, 1	;line. We are on line 1 again

dbgDE09:
		MOV EBX, dbgGPdd5			;Set up to loop for next msg/task
		XOR EBX, EBX				;Another pointer in the link?
		JZ dbgDE13					;No
		MOV EAX, dbgGPdd6			;
		OR EAX, EAX					;NonZero if we are doing tasks
		JNZ dbgDE06					;Tasks
		JMP dbgDE04					;Messages
dbgDE13:
		INC dbgGPdd2				; Exch number
		CMP DWORD PTR dbgGPdd2, nDynEXCH	; Number of dynamic exchanges
		JAE dbgDE08					; Go back for prompt (to pause)
		JMP dbgDE01					; Back to display new exch num
dbgDEDone:
 		CALL FWORD PTR _ClrScr
 		MOV DWORD PTR dbgX, 0
 		MOV DWORD PTR dbgY, 0
		RETN

;===========================
;Displays Tasks that are active along with 
;pertinent address info about them

dbgDispTasks:
		MOV DWORD PTR dbgGPdd2, 1		;Task# we are on
dbgDT00:
		CALL FWORD PTR _ClrScr
		PUSH 0					;Col
		PUSH 0					;Line for labels
		PUSH OFFSET dbgTaskMsg	;
		PUSH 54					;Length of message
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;
		MOV DWORD PTR dbgGPdd1, 1			;line we are one

		;First we get pTSS and see if it is valid
		;If so, we get all the data BEFORE we display it
		;If not, we increment TSS number and go back
		;for the next one
dbgDT01:
		;We get pJCB out of TSS and get	JobNum from JCB

		MOV EAX, dbgGPdd2		; Task number
		CMP EAX, 1				; Is this TSS 1 (Static memory)
		JNE dbgDT02
		MOV EBX, OFFSET MonTSS	;
		JMP SHORT dbgDT04
dbgDT02:
		CMP EAX, 2				; Is this TSS 2 (Static memory)
		JNE dbgDT03
		MOV EBX, OFFSET DbgTSS	;
		JMP SHORT dbgDT04
dbgDT03:
		MOV EBX, pDynTSSs
		DEC EAX					;Make TSS Num offset in dynamic array
		DEC EAX					;of TSSs
		DEC EAX
		MOV ECX, 512
		MUL ECX
		ADD EBX, EAX			;EBX points to TSS!
dbgDT04:
		;EBX has pTSS of interest
		MOV dbgGPdd5, EBX		;Save pTSS for display
		XOR ECX, ECX			;
		MOV CL, [EBX+Priority]	;Priotity of this task
		MOV	dbgGPdd6, ECX
		MOV EAX, [EBX+TSS_pJCB] ;EAX has pJCB
		OR EAX, EAX				;NON zero means it's valid
		JNZ dbgDT05
		MOV EAX, dbgGPdd2		;Not used, go for next Task number
		INC EAX
		CMP EAX, nTSS			;
		JE dbgDT06
		MOV dbgGPdd2, EAX
		JMP SHORT dbgDT01		;back for next one
dbgDT05:
		;EAX now pJCB
		MOV dbgGPdd4, EAX		;Save pJCB for display
		MOV EBX, [EAX]			;Job number is first DD in JCB
		MOV	dbgGPdd3, EBX

		PUSH dbgGPdd2 			;Convert TSS number for display
		PUSH OFFSET dbgBuf
		CALL DDtoHex
		PUSH 0					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;TaskNum
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		PUSH dbgGPdd3 			;Convert and display Job number
		PUSH OFFSET dbgBuf		;
		CALL DDtoHex
		PUSH 10					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;
		PUSH 8					;Size
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		PUSH dbgGPdd4 			;Convert and display pJCB
		PUSH OFFSET dbgBuf		;
		CALL DDtoHex
		PUSH 20					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;
		PUSH 8					;Size
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		PUSH dbgGPdd5 			;Convert and display pTSS
		PUSH OFFSET dbgBuf		;
		CALL DDtoHex
		PUSH 30					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;
		PUSH 8					;Size
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		PUSH dbgGPdd6 			;Convert and display Priority
		PUSH OFFSET dbgBuf		;
		CALL DDtoHex
		PUSH 40					;Col
		PUSH dbgGPdd1			;Line we are one
		PUSH OFFSET dbgBuf		;
		PUSH 8					;Size
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		MOV EAX, dbgGPdd2		;go for next Task number
		INC EAX
		CMP EAX, nTSS			;
		JE dbgDT06
		MOV dbgGPdd2, EAX		;Save it for the top

		INC dbgGPdd1			;Next line
		CMP DWORD PTR dbgGPdd1, 23		;23 lines yet?
		JAE dbgDT06				;Yes, continue prompt
		JMP dbgDT01				;No, go back for next
dbgDT06:
		PUSH 0					;Col
		PUSH 24					;Line
		PUSH OFFSET dbgCont		;
		PUSH 31					;length of Cont string
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;

		MOV EAX, OFFSET dbgKeyCode
		PUSH EAX
		CALL ReadDbgKbd
		MOV EAX, dbgKeyCode
		AND EAX, 0FFh				;Lop off key status bytes
		CMP EAX, 1Bh			    ;Escape (Quit??)
		JE dbgDTDone
		JMP dbgDT00					;Back for next screen
dbgDTDone:
 		CALL FWORD PTR _ClrScr
 		MOV DWORD PTR dbgX, 0
 		MOV DWORD PTR dbgY, 0
		RETN

;=============================================================================
; This is for Debugger Address Info display
; Call with:
;		EAX loaded with address to display (Linear Address)
;		ESI loaded with EA of text line to display
;   We save all registers cause the vid calls don't
;=============================================================================
DispAddr:
		PUSHAD
		PUSH EAX		   ;Save number to display

		PUSH ESI				;ptr to line
		PUSH 06h				;Length of line
		PUSH 07h				;Vid Attribute
		CALL FWORD PTR _TTYOut	;Do it

		POP EAX 			   ;Get number back for display

		PUSH EAX
		PUSH OFFSET dbgBuf
		CALL DDtoHex

		PUSH OFFSET dbgBuf
		PUSH 8
		PUSH 07h
		CALL FWORD PTR _TTYOut

		PUSH OFFSET dbgCRLF
		PUSH 2
		PUSH 07h
		CALL FWORD PTR _TTYOut

		CALL dbgCheckScroll

		POPAD
		RETN
;===============================================
;DbgInfo - Displays important linear address for the OS

DbgInfo:
		MOV ESI,OFFSET DbgM0	;IDT
		LEA EAX, IDT
		CALL DispAddr
		MOV ESI,OFFSET DbgM1	;GDT
		LEA EAX, GDT
		CALL DispAddr
		MOV ESI,OFFSET DbgM2	;RQBs
		MOV EAX, pRQBs
		CALL DispAddr
		MOV ESI,OFFSET DbgM3	;MonTSS
		MOV EAX, OFFSET MonTSS
		CALL DispAddr
		MOV ESI,OFFSET DbgM4	;pTSS3
		MOV EAX, pDynTSSs
		CALL DispAddr
		MOV ESI,OFFSET DbgM5	;LBs
		LEA EAX, rgLBs
		CALL DispAddr
		MOV ESI,OFFSET DbgM6	;RdyQ
		LEA EAX, RdyQ
		CALL DispAddr
		MOV ESI,OFFSET DbgM7	;JCBs
		MOV EAX, pJCBs
		CALL DispAddr
		MOV ESI,OFFSET DbgM8	;SVCs
		LEA EAX, rgSVC
		CALL DispAddr
		MOV ESI,OFFSET DbgM9	;Exchs
		MOV EAX, prgExch
		CALL DispAddr
		MOV ESI,OFFSET DbgPA	;PAM (Page Allocation map)
		LEA EAX, rgPAM
		CALL DispAddr
		MOV ESI,OFFSET DbgMB	;Timer Blocks
		LEA EAX, rgTmrBlks
		CALL DispAddr
		RETN

;=======================================================
;All of the debugger text is displayed in a window
;between colums 0 and 66, and line 0 to 24. The other
;areas are resrved for the menu, query line,
;and the register display.
;This checks to see if the cursor is on line 23.
;If so, we scroll up the text area by one line.

dbgCheckScroll:
		LEA EAX,dbgX			;Query XY (See what line and Col)
		PUSH EAX
		LEA EAX,dbgY
		PUSH EAX
		CALL FWORD PTR _GetXY
		CMP	DWORD PTR dbgY, 23	;Are we at bottom (just above menu)??
		JB	dbgNoScroll			;No, go back for next key

		PUSH 0					;Yes, Scroll test area (Col 0-64, Line 0-24)
		PUSH 0
		PUSH 66					;Columns 0-65
		PUSH 24					;Lines 0-23
		PUSH 1					;fUp (1)
		CALL FWORD PTR _ScrollVid

		PUSH 0					;Got to Column 0, Line 22
		PUSH 22
		CALL FWORD PTR _SetXY
dbgNoScroll:
		RETN
;
;=======================================================
;Clear the query line (Line 23, 40 chars)

dbgClearQuery:
		PUSH 0			  		;Col 0, Line 23
		PUSH 23
		PUSH OFFSET dbgClear
		PUSH 40
		PUSH 07h
		CALL FWORD PTR _PutVidChars	;ignore error
		RETN

;===================== module end ======================
