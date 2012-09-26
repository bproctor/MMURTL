;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0

.DATA
.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC


.CODE


EXTRN IntQ NEAR
EXTRN IntDivBy0 NEAR
EXTRN IntDbgSS  NEAR
EXTRN IntDebug  NEAR
EXTRN IntOverFlow NEAR
EXTRN INTOpCode   NEAR
EXTRN IntDblExc   NEAR
EXTRN INTInvTss   NEAR
EXTRN INTNoSeg    NEAR
EXTRN INTStkOvr   NEAR
EXTRN IntGP       NEAR
EXTRN INTPgFlt    NEAR
EXTRN IntPICU2    NEAR
EXTRN IntTimer    NEAR
EXTRN IntKeyBrd   NEAR

;=============================================================================
;The following code	initializes structures just after bootup
;=============================================================================
;
; INPUT : ECX,EDX
; OUTPUT : NONE
; REGISTERS : EAX,EBX,ECX,FLAGS
; MODIFIES : pFreeLB,rgLBs
;
; This routine will initialize a free pool of link blocks.
; The data used in this algorithm are an array of ECX link blocks (rgLBs),
; each EDX bytes long and pointer to a list of free link blocks (pFreeLB).
;
; The pFreeLB pointer is set to address the first element in rgLBs. Each
; element of rgLBs is set to point to the next element of rgLBs. The
; last element of rgLBs is set to point to nothing (NIL).
;
PUBLIC InitFreeLB:
		LEA EAX,rgLBs           ; pFreeLB <= ^rgLBs;
		MOV pFreeLB,EAX         ;
LB_Loop:
		MOV EBX,EAX           			; for I = 0 TO ECX
		ADD EAX,EDX             		;  rgLBs[I].Next <=
		MOV [EBX+NextLB],EAX    		;    ^rgLBs[I+1];
		LOOP LB_Loop            		;
		MOV DWORD PTR [EBX+NextLB], 0   ; rgFree[1023].Next <= NIL;
		RETN                    		;
;=============================================================================
; AddTSSDesc
; Builds a descriptor for a task and places it in the GDT.  If you
; check the intel documentation the bits of data that hold this
; information are scattered through the descriptor entry, so
; we have to do some shifting, moving, anding and oring to get
; the descriptor the way the processor expects it.  See the Intel
; docs for a complete description of the placement of the bits.
;
; Note: The granularity bit represents the TSS itself, not the code
; that will run under it!
;
;
; IN:
;	EAX - Size of TSS
;	EBX - Decriptor type (default for OS TSS is 0089h)
;		(0089h - G(0),AV(0),LIM(0000),P(1),DPL(00),(010),B(0),(1))
;	EDX - Address of TSS
;	EDI - Address of Desc in GDT
; OUT:
;	GDT is updated with descriptor
; USED:
;	EFlags  (all other registers are saved)

PUBLIC AddTSSDesc:
		;The following code section builds a descriptor entry for
		;the TSS and places it into the GDT
		PUSH EAX
		PUSH EBX
		PUSH EDX
		PUSH EDI
		DEC EAX                 ; (Limit is size of TSS-1)
	    SHL EBX,16              ; Chinese puzzle rotate
		ROL EDX,16              ; Exchange hi & lo words of Base Addr
		MOV BL,DH               ; Base 31 .. 24
		MOV BH,DL               ; Base 23 .. 16
		ROR EBX,8               ; Rotate to Final Alignment
		MOV DX,AX               ; Limit 15 .. 0 with Base 15 .. 0
		AND EAX,000F0000h       ; Mask Limit 19 .. 16
		OR EBX,EAX              ; OR into high order word
		MOV [EDI],EDX           ; Store lo double word
		MOV [EDI+4],EBX         ; Store hi double word
		POP EDI
		POP EDX
		POP EBX
		POP EAX
		RETN

;=============================================================================
; InitFreeTSS
; INPUT :  EAX, ECX
; OUTPUT : NONE
; USED :   ALL General registers, FLAGS
; MODIFIES : pFreeTSS (and the dynamic array of TSSs)
;
; This routine initializes the free pool of Task State Segments.
; On entry:
;	EAX points to the TSSs to initialize (allocated memory).
;   ECX has the count of TSSs to initialize.
;   The size of the TSS is taken from the constant sTSS.
;
; The pFreeTSS pointer is set to address the first TSS. The NextTSS
; field in each TSS is set to point to the next free TSS. The
; last TSS is set to point to nothing (NIL). The IOBitBase field is
; also set to FFFFh for NULL I/O permissions in each TSS.
; NOTE: The allocated memory area for the TSS MUST BE ZEROED before
; calling this routine.  By deafult, we add the TSS descriptors at OS
; protection level. If we spawn or add a User level TSS we must
; OR the DPL bits with 3!

PUBLIC InitFreeTSS:
		MOV pFreeTSS,EAX        	; First one free to use
		MOV EDI, OFFSET rgTSSDesc	; ptr to TSS descriptors
		ADD EDI, 16					; First two TSSs are Static (Mon & Dbgr)
		MOV EDX, sTSS				; Size of TSS (in bytes) into EDX
		MOV EBX, 3					; Number of first dynamic TSS
TSS_Loop:
		MOV ESI,EAX           		  	; for I = 0 TO ECX
		ADD EAX,EDX             		;   EAX <= rgTSSs[I].Next
		MOV [ESI+NextTSS],EAX   		;     ^rgTSSs[I+1];
		MOV WORD PTR [ESI+TSS_IOBitBase], 0FFFFh	; IOBitBase
		MOV [ESI+TSSNum], BX			; TSS Number
		MOV WORD PTR [ESI+TSS_DS], DataSel		;Set up for Data Selectors
		MOV WORD PTR [ESI+TSS_ES], DataSel
		MOV WORD PTR [ESI+TSS_FS], DataSel
		MOV WORD PTR [ESI+TSS_GS], DataSel
		MOV WORD PTR [ESI+TSS_SS], DataSel
		MOV WORD PTR [ESI+TSS_SS0], DataSel
		PUSH EAX					;Save pTSS
		MOV EAX,EDI            		; Get offset of Curr TssDesc in EAX
		SUB EAX, OFFSET GDT    		; Sub offset of GDT Base to get Sel of TSS
		MOV WORD PTR [ESI+Tid],AX   ; Store TSS Selector in TSS (later use)
		PUSH EBX
		PUSH EDX

		MOV EAX,EDX             ; Size of TSS (TSS + SOFTSTATE)
		MOV EDX,ESI             ; Address of TSS
		MOV EBX,0089h           ; G(0),AV(0),LIM(0),P(1),DPL(0),B(0)

		CALL AddTSSDesc

		ADD EDI,8               ; Point to Next GDT Slot (for next one)
		POP EDX
		POP EBX
		POP EAX
		INC EBX							; TSS Number
		LOOP TSS_Loop          			;
		MOV DWORD PTR [ESI+NextTSS], 0	; rgFree[LastOne].Next <= NIL;
		RETN                    		;

;=============================================================================
; DUMMY CALL for uninitialized GDT call gate slots
;=============================================================================

DummyCall:
		MOV EAX, ercBadCallGate
		RETF


;=============================================================================
; InitCallGates inits the array of call gates with an entry to a generic
; handler that returns ErcNotInstalled when called. This prevents new code
; running on old MMURTLs or systems where special call gates don't exist
; without crashing too horribly.
;
; IN: Nothing
; Out : Nothing
; Used : ALL registers and flags
;
PUBLIC InitCallGates:

		;First we set up all call gates to point to a
		;dummy procedure

		MOV ECX, nCallGates		;Number of callgates to init
InitCG01:
		PUSH ECX				;Save nCallGates
		DEC ECX					;make it an index, not the count
		SHL ECX, 3				;
		ADD ECX, 40h			;Now ecx is selector number
		MOV EAX, 0EC00h			;DPL 3, 0 Params
		MOV DX, OSCodeSel
		MOV ESI, OFFSET DummyCall

		;Same code as in PUBLIC AddCallGate
		MOVZX EBX, CX
		SUB EBX, 40			;sub call gate base selector
		SHR EBX, 3			;make index vice selector
		MOVZX EBX, CX		;Extend selector into EBX
		ADD EBX, GDTBase	;NOW a true offset in GDT
		MOV WORD PTR [EBX+02], 8	;Put Code Seg selector into Call gate
		MOV [EBX], SI		;0:15 of call offset
		SHR ESI, 16			;move upper 16 of offset into SI
		MOV [EBX+06], SI	;16:31 of call offset
		MOV [EBX+04], AX	;call DPL & ndParams

		POP ECX					;ignore error...
		LOOP InitCG01			;This decrements ECX till 0

		;Another chicken and egg here.. In order to be able
		;to call the FAR PUBLIC "AddCallGate" though a callgate,
		;we have to add it as a callgate... ok....

		MOV EAX, 08C00h		;AddCallGate -- 0 DWord Params  DPL 0
		MOV ECX, 0C8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AddCallGate
		;Same code as in PUBLIC AddCallGate
		MOVZX EBX, CX
		SUB EBX, 40			;sub call gate base selector
		SHR EBX, 3			;make index vice selector
		MOVZX EBX, CX		;Extend selector into EBX
		ADD EBX, GDTBase	;NOW a true offset in GDT
		MOV WORD PTR [EBX+02], 8	;Put Code Seg selector into Call gate
		MOV [EBX], SI		;0:15 of call offset
		SHR ESI, 16			;move upper 16 of offset into SI
		MOV [EBX+06], SI	;16:31 of call offset
		MOV [EBX+04], AX	;call DPL & ndParams
		RETN

;=============================================================================
; InitIDT
; First, inits the IDT with 256 entries to a generic
; handler that does nothing (except IRETD) when called.
; Second, adds each of the basic IDT entries for included
; software and hardware interrupt handlers.
; ISRs loaded with device drivers must use SetIRQVector.
;
; IN	: Nothing
; Out	: Nothing
; Used	: ALL registers and flags
;
PUBLIC InitIDT:
		MOV ECX, 255			;Last IDT Entry
InitID01:
		PUSH ECX
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate
		POP ECX
		LOOP InitID01

		;Now we add each of the known interrupts

		MOV ECX, 0				;Divide By Zero
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntDivBy0
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 1				;Single Step
		MOV EAX, 08F00h			;DPL 3, Trap gate
		MOV EBX, OSCodeSel		;
		MOV ESI, Offset IntDbgSS
		CALL FWORD PTR _AddIDTGate

		;Trying 8E00 (Int gate vice trap gate which leaves Ints disabled)

		MOV ECX, 3				;Breakpoint
		MOV EAX, 08F00h			;DPL 3, Trap Gate (for Debugger) WAS 8F00
		MOV EBX, OSCodeSel		;This will be filled in with TSS of Dbgr later
		MOV ESI, OFFSET IntDebug
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 4				;Overflow
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntOverFlow
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 6				;Invalid OPcode
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntOpCode
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 8				;Double Exception
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntDblExc
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 0Ah			;Invalid TSS
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntInvTSS
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 0Bh			;Seg Not Present
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntNoSeg
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 0Ch			;Int Stack Overflow
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntStkOvr
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 0Dh			;GP fault
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntGP
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 0Eh			;Int Page Fault
		MOV EAX, 08F00h			;DPL 3, TRAP GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntPgFlt
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 20h			;Int TIMER				IRQ0
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntTimer
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 21h			;Int KEYBOARD			IRQ1
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntKeyBrd
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 22h			;Int PICU 2 (from PICU) IRQ2
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntPICU2
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 23h			;Int COM2				IRQ3
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 24h			;Int COM1				IRQ4
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 25h			;Int LPT2 				IRQ5
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 26h			;Int Floppy				IRQ6
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ	;FDD will set this himself
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 27h			;Int ..........			IRQ7
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 28h			;Int ..........			IRQ8
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 29h			;Int ..........			IRQ9
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Ah			;Int ..........			IRQ10
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Bh			;Int ..........			IRQ11
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Ch			;Int ..........			IRQ12
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Dh			;Int ..........			IRQ13
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Eh			;Int ..........			IRQ14
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		MOV ECX, 2Fh			;Int ..........			IRQ15
		MOV EAX, 08E00h			;DPL 3, INTERRUPT GATE
		MOV EBX, OSCodeSel		;
		MOV ESI, OFFSET IntQ
		CALL FWORD PTR _AddIDTGate

		RETN

;=============================================================================
; InitOSPublics adds all OS primitives to the array of call gates. This can't
; before initcallgates, but MUST be called before the first far call to any
; OS primitive thorugh a call gate!!!
; IF YOU ADD AN OS PUBLIC MAKE SURE IT GETS PUT HERE!!!!!
;
; IN   : Nothing
; Out  : Nothing
; Used : ALL registers and flags
;
PUBLIC InitOSPublics:

		MOV EAX, 0EC02h		;WaitMsg -- 2 DWord Params, DPL 3
		MOV ECX, 40h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __WaitMsg
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;SendMsg -- 3 DWord Params, DPL 3
		MOV ECX, 48h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SendMsg
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C03h		;ISendMsg -- 3 DWord params, DPL 0
		MOV ECX, 50h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ISendMsg
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;Set Priority - 1Dword param, DPL 3
		MOV ECX, 58h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetPriority
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC0Ch		;Request --  12 nDWord params
		MOV ECX, 60h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Request
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;Respond -- 2 nDWord params
		MOV ECX, 68h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Respond
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;Check -- 2 DWord Params, DPL 3
		MOV ECX, 70h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CheckMsg
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC07h		;NewTask -- 7 DWord param, DPL 3
		MOV ECX, 78h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __NewTask
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;AllocExch -- 1 DWord param, DPL 3
		MOV ECX, 80h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AllocExch
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;DeAllocExch -- 1 DWord param, DPL 3
		MOV ECX, 88h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeAllocExch
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;Sleep -- 1 DWord param, DPL 3
		MOV ECX, 90h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Sleep
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;Alarm -- 2 DWord params, DPL 3
		MOV ECX, 98h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Alarm
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;AllocOSPage -- 2 DWord params, DPL 3
		MOV ECX, 0A0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AllocOSPage
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;AllocPage -- 2 DWord params, DPL 3
		MOV ECX, 0A8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AllocPage
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;RegisterSvc -- 2 DWord params, DPL 3
		MOV ECX, 0B0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __RegisterSvc
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C05h		;DMASetUp -- 5 DWord Params  DPL 0
		MOV ECX, 0B8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DMASetUp
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;ReadKBD -- 2 DWord Param  DPL 3
		MOV ECX, 0C0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ReadKBD
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C00h		;AddCallGate -- 0 DWord Params  DPL 0
		MOV ECX, 0C8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AddCallGate
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C00h		;AddIDTGate -- 0 DWord Params  DPL 0
		MOV ECX, 0D0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AddIDTGate
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;EndOfIRQ -- 1 DWord Params  DPL 0
		MOV ECX, 0D8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __EndOfIRQ
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;MaskIRQ -- 1 DWord Params  DPL 0
		MOV ECX, 0E0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __MaskIRQ
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;UnMaskIRQ -- 1 DWord Params  DPL 0
		MOV ECX, 0E8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __UnMaskIRQ
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;SetIRQVector -- 2 DWord Params  DPL 0
		MOV ECX, 0F0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetIRQVector
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;GetIRQVector -- 2 DWord Params  DPL 0
		MOV ECX, 0F8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetIRQVector
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C04h		;InitDevDr -- 4 DWord Params  DPL 0
		MOV ECX, 100h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __InitDevDr
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;DeviceInit -- 3 DWord Params  DPL 3
		MOV ECX, 108h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeviceInit
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;DeviceOp -- 5 DWord Params  DPL 3
		MOV ECX, 110h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeviceOp
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;DeviceStat -- 4 DWord Params  DPL 3
		MOV ECX, 118h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeviceStat
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC00h		;Beep -- 0 DWord Params  DPL 3
		MOV ECX, 120h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Beep
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;Tone -- 2 DWord Params  DPL 3
		MOV ECX, 128h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Tone
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;KillAlarm -- 1 DWord Params  DPL 3
		MOV ECX, 130h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __KillAlarm
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;MicroDelay -- 1 DWord Params  DPL 3
		MOV ECX, 138h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __MicroDelay
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;SpawnTask -- 5 DWord param, DPL 3
		MOV ECX, 140h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SpawnTask
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetCMOSTime -- 1 DWord param, DPL 3
		MOV ECX, 148h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetCMOSTime
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetTimerTick -- 1 DWord param, DPL 3
		MOV ECX, 150h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetTimerTick
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;OutByte -- 2 DWord param, DPL 0
		MOV ECX, 158h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __OutByte
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;OutWord -- 2 DWord param, DPL 0
		MOV ECX, 160h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __OutWord
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;OutDWord -- 2 DWord param, DPL 0
		MOV ECX, 168h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __OutDWord
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;InByte -- 1 DWord param, DPL 0
		MOV ECX, 170h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __InByte
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;InWord -- 1 DWord param, DPL 0
		MOV ECX, 178h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __InWord
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C01h		;InDWord -- 1 DWord param, DPL 0
		MOV ECX, 180h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __InDWord
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;ReadCMOS -- 1 DWord param, DPL 3
		MOV ECX, 188h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ReadCMOS
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;CopyData -- 3 DWord param, DPL 3
		MOV ECX, 190h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CopyData
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;CopyDataR -- 3 DWord param, DPL 3
		MOV ECX, 198h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CopyDataR
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;FillData -- 3 DWord param, DPL 3
		MOV ECX, 1A0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __FillData
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;CompareNCS -- 3 DWord param, DPL 3
		MOV ECX, 1A8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CompareNCS
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;Compare -- 3 DWord param, DPL 3
		MOV ECX, 1B0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Compare
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C03h		;InWords -- 3 DWord param, DPL 0
		MOV ECX, 1B8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __InWords
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C03h		;OutWords -- 3 DWord param, DPL 0
		MOV ECX, 1C0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __OutWords
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;MoveRequest --  2 nDWord params DPL 3
		MOV ECX, 1C8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __MoveRequest
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;DeAllocPage --  2 nDWord params DPL 3
		MOV ECX, 1D0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeAllocPage
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;LoadNewJob --  3 nDWord params DPL 3
		MOV ECX, 1D8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __LoadNewJob
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;SetVidOwner --  1 nDWord params DPL 3
		MOV ECX, 1E0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetVidOwner
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetVidOwner --  1 nDWord params DPL 3
		MOV ECX, 1E8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetVidOwner
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC00h		;ClrScr --  0 nDWord params DPL 3
		MOV ECX, 1F0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ClrScr
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;TTYOut --  3 nDWord params DPL 3
		MOV ECX, 1F8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __TTYOut
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;PutVidChars --  5 nDWord params DPL 3
		MOV ECX, 200h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __PutVidChars
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetXY --  2 nDWord params DPL 3
		MOV ECX, 208h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetXY
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetXY --  2 nDWord params DPL 3
		MOV ECX, 210h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetXY
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC06h		;EditLine --  6 nDWord params DPL 3
		MOV ECX, 218h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __EditLine
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetTSSExch --  1 nDWord params DPL 3
		MOV ECX, 220h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetTSSExch
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;OpenFile --  5 nDWord params DPL 3
		MOV ECX, 228h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __OpenFile
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;CloseFile --  1 nDWord params DPL 3
		MOV ECX, 230h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CloseFile
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;ReadBlock --  5 nDWord params DPL 3
		MOV ECX, 238h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ReadBlock
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;WriteBlock --  5 nDWord params DPL 3
		MOV ECX, 240h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __WriteBlock
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;ReadBytes --  4 nDWord params DPL 3
		MOV ECX, 248h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ReadBytes
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;WriteBytes --  4 nDWord params DPL 3
		MOV ECX, 250h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __WriteBytes
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetFileLFA --  2 nDWord params DPL 3
		MOV ECX, 258h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetFileLFA
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetFileLFA --  2 nDWord params DPL 3
		MOV ECX, 260h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetFileLFA
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetFileSize --  2 nDWord params DPL 3
		MOV ECX, 268h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetFileSize
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;CreateFile --  3 nDWord params DPL 3
		MOV ECX, 270h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CreateFile
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;RenameFile --  4 nDWord params DPL 3
		MOV ECX, 278h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __RenameFile
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;DeleteFile --  1 nDWord params DPL 3
		MOV ECX, 280h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeleteFile
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetpJCB --  2 nDWord params DPL 3
		MOV ECX, 288h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetpJCB
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;QueryPages --  1 nDWord params DPL 3
		MOV ECX, 290h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __QueryPages
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetPhyAdd --  2 nDWord params DPL 3
		MOV ECX, 298h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetPhyAdd
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;ScrollVid --  5 nDWord params DPL 3
		MOV ECX, 2A0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ScrollVid
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC05h		;GetDirSector --  5 nDWord params DPL 3
		MOV ECX, 2A8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetDirSector
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetJobNum --  1 nDWord params DPL 3
		MOV ECX, 2B0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetJobNum
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;ExitJob --  1 nDWord params DPL 3
		MOV ECX, 2B8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __ExitJob
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetUserName --  2 nDWord params DPL 3
		MOV ECX, 2C0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetUserName
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetUserName --  2 nDWord params DPL 3
		MOV ECX, 2C8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetUserName
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetCmdLine --  2 nDWord params DPL 3
		MOV ECX, 2D0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetCmdLine
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetCmdLine --  2 nDWord params DPL 3
		MOV ECX, 2D8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetCmdLine
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetPath --  2 nDWord params DPL 3
		MOV ECX, 2E0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetPath
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;GetPath --  3 nDWord params DPL 3
		MOV ECX, 2E8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetPath
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetExitJob --  2 nDWord params DPL 3
		MOV ECX, 2F0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetExitJob
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetExitJob --  2 nDWord params DPL 3
		MOV ECX, 2F8h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetExitJob
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetSysIn --  2 nDWord params DPL 3
		MOV ECX, 300h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetSysIn
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetSysOut --  2 nDWord params DPL 3
		MOV ECX, 308h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetSysOut
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetSysIn --  2 nDWord params DPL 3
		MOV ECX, 310h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetSysIn
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;GetSysOut --  2 nDWord params DPL 3
		MOV ECX, 318h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetSysOut
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;PutVidAttrs --  4 nDWord params DPL 3
		MOV ECX, 320h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __PutVidAttrs
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;GetVidChar --  4 nDWord params DPL 3
		MOV ECX, 328h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetVidChar
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;SetNormVid --  1 nDWord params DPL 3
		MOV ECX, 330h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetNormVid
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetNormVid --  1 nDWord params DPL 3
		MOV ECX, 338h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetNormVid
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;Chain --  3 nDWord params DPL 3
		MOV ECX, 340h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __Chain
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetFileSize --  2 nDWord params DPL 3
		MOV ECX, 348h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetFileSize
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetCMOSDate --  1 nDWord params DPL 3
		MOV ECX, 350h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetCMOSDate
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;CreateDir --  2 nDWord params DPL 3
		MOV ECX, 358h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __CreateDir
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;DeleteDir --  3 nDWord params DPL 3
		MOV ECX, 360h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeleteDir
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC03h		;DeAliasMem --  3 DWord params DPL 3
		MOV ECX, 368h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __DeAliasMem
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC04h		;AliasMem --  4 DWord params DPL 3
		MOV ECX, 370h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AliasMem
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C02h		;GetDMACount --  2 DWord params DPL 0
		MOV ECX, 378h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetDMACount
		CALL FWORD PTR _AddCallGate

		MOV EAX, 08C03h		;AllocDMAPage --  3 DWord params DPL 0
		MOV ECX, 380h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __AllocDMAPage
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC02h		;SetJobName --  2 DWord params DPL 3
		MOV ECX, 388h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __SetJobName
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;KillJob --  1 DWord params DPL 3
		MOV ECX, 390h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __KillJob
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;GetSystemDisk --  1 DWord params DPL 3
		MOV ECX, 398h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __GetSystemDisk
		CALL FWORD PTR _AddCallGate

		MOV EAX, 0EC01h		;UnRegisterSvc --  1 DWord params DPL 3
		MOV ECX, 3A0h
		MOV DX, OSCodeSel
		MOV ESI, OFFSET __UnRegisterSvc
		CALL FWORD PTR _AddCallGate

		RETN

;================== END of Module =================
