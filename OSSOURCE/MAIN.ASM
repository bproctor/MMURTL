;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994  Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0
;
; This is the main Data/Code file for the MMURTL OS.
; It contains important structures and initialization code
; including the first instructions executed after a boot!
; This should follow the IDT, GDT, PDR & Public call table in memory.

.DATA
.INCLUDE MOSEDF.INC
.INCLUDE JOB.INC
.INCLUDE TSS.INC

.ALIGN DWORD
;=============================================================================
; Kernel Structures - See MOSEDF.INC for structure details.
;=============================================================================

PUBLIC MonTSS		DB sTSS dup (0)	; Initial TSS for OS/Monitor
PUBLIC DbgTSS		DB sTSS dup (0)	; Initial TSS for Debugger
PUBLIC pFreeTSS		DD NIL			; Pointer to Free List of Task State Segs
PUBLIC pDynTSSs		DD 0			; ptr to alloced mem for dynamic TSSs
PUBLIC _nTSSLeft	DD nTSS-2		; For stats (less static TSSs)

;------------------

PUBLIC rgLBs    DB (nLB*sLINKBLOCK) dup (0)	; pool of LBs
PUBLIC pFreeLB  DD NIL						; Ptr to Free List of Link Block
PUBLIC _nLBLeft	DD nLB						; For Monitor stats

;------------------
; The RUN Queue for "Ready to Run" tasks

PUBLIC RdyQ  DB (sQUEUE*nPRI) dup (0)	; Priority Based Ready Queue

;------------------
; Two static Job Control Blocks (JCBs) to get us kick-started.
; The rest of them are in allocated memory

PUBLIC MonJCB      DB sJCB dup (0)		; Monitor JCB
PUBLIC DbgJCB      DB sJCB dup (0)		; Debugger JCB

;------------------

PUBLIC GDTLimit		DW 0000h		;Global Descriptor Table Limit
PUBLIC GDTBase		DD 00000000h	;base
PUBLIC IDTLimit		DW 0000h        ;Interrupt Descriptor Table Limit
PUBLIC IDTBase		DD 00000000h	;base

;------------------
PUBLIC rgSVC    DB (sSVC*nSVC) dup (0)		; Setup an array of Service Descriptors

;------------------
;Exchanges take up a fair amount of memory. In order to use certain kernel
;primitives, we need exchanges before they are allocated dynamically.
;We have an array of 3 exchanges set aside for this purpose.  After
;the dynamic array is allocated an initialized, we copy these into
;the first three dynamic exchanges.  These static exchanges are used
;by the Monitor TSS, Debugger TSS, and memory Management.

PUBLIC nExch  DD 3					; This will be changed after allocation
									; of dynamic exchanges.
rgExchTmp  DB (sEXCH * 3) dup (0)	; Setup three static temporary Exchanges
									; for Monitor and Debugger TSSs
									; These are moved to a dynamic Exch Array
									; as soon as it's allocated.
PUBLIC 	 prgExch    DD OFFSET rgExchTmp		; Pointer to current array of Exchanges.
PUBLIC   pExchTmp   DD 0				; Pointer to dynamic array Exchanges.
PUBLIC   _nEXCHLeft DD nDynEXCH			; For Monitor stats

;-------------------
;Scheduling management variables

PUBLIC TSS			DD 00000000h	; Used for jumping to next task
PUBLIC TSS_Sel		DW 0000h		; "    "

.ALIGN DWORD

PUBLIC pRunTSS		DD NIL			; Pointer to the Running TSS
PUBLIC SwitchTick	DD 0			; Tick of last task switch
PUBLIC dfHalted     DD 0			; nonzero if processor was halted

PUBLIC _nSwitches	DD 0			; # of switches for statistics
PUBLIC _nSlices		DD 0			; # of sliced switches for stats
PUBLIC _nReady		DD 0			; # of task Ready to Run for stats
PUBLIC _nHalts		DD 0			; # of times CPU halted for stats

;THESE ARE THE INITIAL STACKS FOR THE OS Monitor AND Debugger

OSStack		DD 0FFh DUP (00000000h) ;1K OS Monitor Stack
OSStackTop	DD 00000000h
OSStackSize	EQU OSStackTop-OSStack

Stack1		DD 0FFh DUP (00000000h) ;1K Debugger Stack
Stack1Top	DD 00000000h
Stack1Size	EQU Stack1Top-Stack1

;-----------------

rgNewJob	DB 'New Job'			;Placed in New JCBs
cbNewJob	EQU 7

rgOSJob		DB 'MMOS Monitor'		;Placed in first JCB
cbOSJob		EQU 12

rgDbgJob	DB 'Debugger    '		;Name for Job
cbDbgJob	DD 12					;Size of Job Name

PUBLIC _BootDrive  DD 00			;Source drive of the boot
;=============================================================================
;=============================================================================
; This begins the OS Code Segment
.CODE
;
.VIRTUAL 10000h				;64K boundry. This lets the assembler know
							;that this is the address where we execute
;
; BEGIN OS INITIALIZATION CODE
;
; This code is used to initialize the permanent OS structures
; and calls procedures that initialize dynamic structures too.
;
; "Will Robinson, WARNING, WARNING!! Dr. Smith is approaching!!!"
;  BEWARE ON INITIALIZATION.  The kernel structures and their
;  initialization routines are so interdependent, you must pay
;  close attention before you change the order of ANYTHING.
;  (Anything before we jump to the monitor code that is)

EXTRN InitCallGates NEAR
EXTRN InitOSPublics NEAR
EXTRN _Monitor NEAR
EXTRN InitIDT NEAR
EXTRN InitFreeLB NEAR
EXTRN InitDMA NEAR
EXTRN Set8259 NEAR
EXTRN InitKBD NEAR
EXTRN AddTSSDesc NEAR
EXTRN InitMemMgmt NEAR
EXTRN InitNewJCB NEAR
EXTRN InitVideo NEAR
EXTRN InitFreeTSS NEAR
EXTRN InitDynamicJCBs NEAR
EXTRN InitDynamicRQBs NEAR
EXTRN DbgTask NEAR
;=============================================================================
; Set up the initial Stack Pointer (SS = DS already).
; This is the first code we execute after the loader
; code throws us into protected mode. It's a FAR jump
; from that code...
;=============================================================================

.START
PUBLIC OSInitBegin:
		LEA EAX,OSStackTop      	; Setup initial OS Stack
		MOV ESP,EAX					; FIRST THING IN OS CODE.
		MOV _BootDrive, EDX			; Left there from BootSector Code

;=============================================================================
; Set up OS Common Public for IDT and GDT Base and Limits - SECOND THING IN OS
;=============================================================================

		SGDT FWORD PTR GDTLimit 	;A formality - we know where they are!
		SIDT FWORD PTR IDTLimit 	;

;=============================================================================
; Setup Operating System Structures and motherboard hardware
; THIS IS RIGHT AFTER WE GET A STACK.
; YOU CAN'T ALLOCATE ANY OS RESOURCES UNTIL THIS CODE EXECUTES!!!
;=============================================================================

		CALL InitCallGates			; Sets up all call gates as DUMMYs
									;  except AddCallGate which
									   must be made valid first!

		CALL InitOSPublics			; Sets up OS PUBLIC call gates

		CALL InitIDT				;Sets up default Interrupt table
									;NOTE: This uses CallGates!

		MOV ECX,nLB					; count of Link Blocks
		MOV EDX,sLinkBlock			; EDX is size of a Link Block
		CALL InitFreeLB				; Init the array of Link Blocks

		CALL InitDMA				; Sets up DMA with defaults

		CALL Set8259            	; Set up 8259s for ints (before KBD)

		CALL InitKBD            	; Initialize the Kbd hardware

		PUSH 0						; Highest IRQ number (all IRQs)
		CALL FWORD PTR _EndOfIRQ	; Tell em to work

		;Set time counter divisor to 11938 - 10ms ticks
		;Freq in is 1.193182 Mhz/11932 = 100 per second
		;or 1 every 10 ms.  (2E9Ch = 11932 decimal)

		MOV AL,9Ch					; Makes the timer Tick (lo)
		OUT 40h,AL              	; 10 ms apart by setting
		MOV AL,02Eh					; clock divisior to (hi byte)
		OUT 40h,AL             		; 11,932

		STI							; We are ready to GO (for now)

;=============================================================================
; The following code finishes the initialization procedures BEFORE the
; OS goes into paged memory mode.
; We set up an initial Task by filling a static TSS, creating and loading
; a descriptor entry for it in the GDT and we do the same for the debugger.
;=============================================================================

		; Make the default TSS for the CPU to switch from a valid one.
		; This TSS is a valid TSS after the first task switch.
		; IMPORTANT - Allocate Exch and InitMemMgmt calls depend on
		; pRunTSS being valid.  They can not be called before
		; this next block of code!!!  Note that this TSS does NOT
		; get placed in the linked list with the rest of the TSSs.
		; It will never be free. We also have to manaully make it's
		; entry in the GDT.

		;The following code section builds a descriptor entry for
		;the initial TSS (for Montitor program) and places it into the GDT

		MOV EAX, sTSS   			; Limit of TSS (TSS + SOFTSTATE)
		MOV EBX, 0089h             	; G(0),AV(0),LIM(0),P(1),DPL(0),B(0)
		MOV EDX, OFFSET MonTSS		; Address of TSS
		MOV EDI, OFFSET rgTSSDesc	; Address of GDT entry to fill
		CALL AddTSSDesc

		;Now that we have valid Descriptor, we set up the TSS itself
		;and Load Task Register with the descriptor (selector)
		;Note that none of the TSS register values need to be filled in
		;because they will be filled by the processor on the first
		;task switch.

		MOV EBX, OFFSET MonTSS		; Get ptr to initial TSS in EBX
		MOV pRunTSS,EBX				; this IS our task now!!!
		MOV EAX, OFFSET rgTSSDesc	; ptr to initial TSS descriptor
		SUB EAX, OFFSET GDT 	    ; Sub offset of GDT Base to get Sel of TSS
		MOV WORD PTR [EBX+TSS_IOBitBase], 0FFh; I/O Permission
		MOV [EBX+Tid],AX  	    	; Store TSS Selector in TSS (Task ID)
		LTR WORD PTR [EBX+Tid]				; Setup the Task Register
		MOV BYTE PTR [EBX+Priority], 25 	; Priority 25 (monitor is another APP)
		MOV DWORD PTR [EBX+TSS_CR3], OFFSET PDir1	;Physical address of PDir1
		MOV WORD PTR [EBX+TSSNum], 1		;Number of first TSS (Duh)

		;Set up Job Control Block for Monitor (always Job 1)
		;JOB 0 is not allowed. First JCB IS job 1!

		MOV EAX, OFFSET MonJCB			;
		MOV DWORD PTR [EAX+JobNum], 1	;Number the JCB
		MOV EBX, OFFSET MonTSS			;Must put ptr to JCB in TSS
		MOV [EBX+TSS_pJCB], EAX			;pJCB into MonTSS
		MOV EBX, OFFSET PDir1			;Page Directory
		MOV ESI, OFFSET rgOSJob			;Job Name
		MOV ECX, cbOSJob				;Size of Name
		XOR EDX, EDX					;NO pVirtVid yet (set up later in init)
		CALL InitNewJCB


; IMPORTANT - You can't call AllocExch before pRunTSS is VALID!

; THIS IS THE FIRST POINT AllocExh is valid

 		MOV EAX, pRunTSS
		ADD EAX, TSS_Exch			;Alloc exch for initial (first) TSS
		PUSH EAX
		CALL FWORD PTR _AllocExch



;=============================================================================
;
; Set up DEBUGGER Task and Job
; The debugger is set up as another job with its one task.
; The debugger must not be called (Int03) until this code executes,
; AND the video is initialized.
; We can't use NewTask because the debugger operates independent of
; the kernel until it is called (INT 03 or Exception)
;
		;The following code section builds a descriptor entry for
		;the initial TSS (for Debugger) and places it into the GDT

		MOV EAX, sTSS   			; Limit of TSS (TSS + SOFTSTATE)
		MOV EBX, 0089h             	; G(0),AV(0),LIM(0),P(1),DPL(0),B(0)
		MOV EDX, OFFSET DbgTSS		; Address of Debugger TSS
		MOV EDI, OFFSET rgTSSDesc+8	; Address of GDT entry to fill in
		CALL AddTSSDesc

		;Now that we have valid Descriptor, we set up the TSS itself

		MOV EAX, OFFSET rgTSSDesc+8	; ptr to second TSS descriptor
		SUB EAX, OFFSET GDT 	    ; Sub offset of GDT Base to get Sel of TSS
		MOV EBX, OFFSET DbgTSS		; Get ptr to initial TSS in EBX
		MOV WORD PTR [EBX+TSS_IOBitBase], 0FFh; I/O Permission
		MOV [EBX+Tid],AX  	    	; Store TSS Selector in TSS (Task ID)
		MOV BYTE PTR [EBX+Priority], 1 		; Debugger is HIGH Priority
		MOV DWORD PTR [EBX+TSS_CR3], OFFSET PDir1	;Physical address of PDir1
		MOV EDX, OFFSET DbgTask
		MOV [EBX+TSS_EIP],EDX
		MOV WORD PTR [EBX+TSS_CS],OSCodeSel		; Put OSCodeSel in the TSS
		MOV WORD PTR [EBX+TSS_DS],DataSel		; Put DataSel in the TSS
		MOV WORD PTR [EBX+TSS_ES],DataSel		;
		MOV WORD PTR [EBX+TSS_FS],DataSel		;
		MOV WORD PTR [EBX+TSS_GS],DataSel		;
		MOV WORD PTR [EBX+TSS_SS],DataSel		;
		MOV WORD PTR [EBX+TSS_SS0],DataSel		;
		MOV EAX, OFFSET Stack1Top
		MOV DWORD PTR [EBX+TSS_ESP],EAX			; A 1K Stack in the Dbg TSS
		MOV DWORD PTR [EBX+TSS_ESP0],EAX			;
		MOV DWORD PTR [EBX+TSS_EFlags],00000202h	; Load the Flags Register
		MOV WORD PTR [EBX+TSSNum], 2				; Number of Dubegger TSS

		;Set up Job Control Block for Debugger
		;JOB 0 is not allowed. First JCB IS job 1, debugger is always 2

		MOV EAX, OFFSET DbgJCB
		MOV DWORD PTR [EAX+JobNum], 2			;Number the JCB
		MOV [EBX+TSS_pJCB], EAX		;EBX still points to DbgTSS
		MOV EBX, OFFSET PDir1		;Page Directory (OS PD to start)
		MOV ESI, OFFSET rgDbgJob	;Name
		MOV ECX, cbDbgJob			;size of name
		MOV EDX, 1					;Debugger gets video 1
		CALL InitNewJCB

		;Now allocate the default exchange for Debugger

 		MOV EAX, OFFSET DbgTSS
		ADD EAX, TSS_Exch			;Alloc exch for Debugger TSS
		PUSH EAX
		CALL FWORD PTR _AllocExch

;================================================================
; ALSO NOTE: The InitMemMgmt call enables PAGING!  Physical addresses
; will not necessarily match Linear addresses beyond this point.
; Pay attention!
;

		CALL InitMemMgmt			;InitMemMgmt allocates an exch so
									;this the first point it can be called.
									;It also calls SEND!


;================================================================
; FIRST POINT memory management calls are valid
;================================================================

		;Now we will allocate two virtual video screens (1 Page each)
		;for the Monitor and Debugger and place them in the JCBs
		;Also we set pVidMem for Monitor to VGATextBase address
		;cause it has the active video by default

		PUSH 1						; 1 page
		MOV EAX, OFFSET MonJCB		; Ptr to Monitor JCB
		ADD EAX, pVirtVid			; Offset in JCB to pVirtVid
		PUSH EAX
		CALL FWORD PTR _AllocOSPage	; Get 'em!
		MOV EAX, OFFSET MonJCB		; Make VirtVid Active for Monitor
		MOV DWORD PTR [EAX+pVidMem], VGATextBase


		PUSH 1						; 1 page
		MOV EAX, OFFSET DbgJCB		;
		ADD EAX, pVirtVid
		PUSH EAX
		CALL FWORD PTR _AllocOSPage	; Get 'em!
		MOV EAX, OFFSET DbgJCB		;
		MOV EBX, [EAX+pVirtVid]
		MOV [EAX+pVidMem], EBX		; Video NOT active for Debugger

		CALL InitVideo				;Set Text Screen 0


;		MOV EAX,30423042h       ;BB on CYAN - So we know we are here!
;		MOV DS:VGATextBase+00h,EAX

;================================================================
; FIRST POINT video calls are valid
; FIRST POINT Debugger is working  (only because it needs the video)

; At this point we can allocate pages for dynamic structures and
; initialize them.
;================================================================

; Allocate 8 pages (32768 bytes) for 64 Task State Segments (structures).
; Then call InitFreeTSS (which fills them all in with default values)

		PUSH 8						; 8 pages for 64 TSSs (32768 bytes)
		MOV EAX, OFFSET pDynTSSs	;
		PUSH EAX
		CALL FWORD PTR _AllocOSPage	; Get 'em!

		XOR EAX, EAX				; Clear allocated memory for TSSs
		MOV ECX, 8192				; (512 * 64 = 32768 = 8192 * 4)
		MOV EDI, pDynTSSs			; where to store 0s
		REP STOSD					; Do it

		MOV EAX, pDynTSSs
		MOV ECX, nTSS-2				; count of dynamic TSSs (- OS & Dbgr TSS)
		CALL InitFreeTSS			; Init the array of Process Control Blocks

		CALL InitDynamicJCBs
		CALL InitDynamicRQBs

;================================================================
; Allocate 1 page (4096 bytes) for 256 Exchanges (16*256=4096).
; Exchanges are 16 bytes each.  Then zero the memory which has
; the effect of initializing them because all fields in an Exch
; are zero if not allocated.

		PUSH 1						; 1 pages for 256 Exchs (4096 bytes)
		MOV EAX, OFFSET pExchTmp	; Returns ptr to allocated mem in pJCBs
		PUSH EAX					;
		CALL FWORD PTR _AllocOSPage	; Get it!

		XOR EAX, EAX				; Clear allocated memory
		MOV ECX, 1024				; (4*1024=4096)
		MOV EDI, pExchTmp			; where to store 0s
		REP STOSD					; Store EAX in 1024 locations

		;Now we move the contents of the 3 static exchanges
		;into the dynamic array.  This is 60 bytes for 3
		;exchanges.

		MOV ESI, prgExch			; Source (static ones)
		MOV EDI, pExchTmp			; Destination (dynamic ones)
		MOV ECX, 12					; 12 DWords (3 Exchanges)
		REP MOVSD					; Move 'em!
		MOV EAX, pExchTmp			; The new ones
		MOV prgExch, EAX			; prgExch now points to new ones
		MOV nExch, nDynEXCH			; 256 to use (-3 already in use)

;================================================================

		PUSH 7						; Look at LPT IRQ
		CALL FWORD PTR _UnMAskIRQ	;

		CALL _Monitor					;Head for the Monitor!!

		;The Monitor call never comes back (it better not...)

		HLT			;Well, you never know (I AM human)


;

;================== End of Main.asm ==============================
