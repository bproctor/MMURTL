;=============================================================================
;  MMURTL Operating System Source Code
;  Copyright 1991,1992,1993,1994 Richard A. Burgess
;  ALL RIGHTS RESERVED     Version 1.0
;
.DATA

.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC
.INCLUDE JOB.INC
.ALIGN DWORD
;
;=============================================================================
; Memory Management Data
;=============================================================================

PUBLIC _nPagesFree		DD 0			;Number of free physical pages left
PUBLIC _oMemMax			DD 000FFFFFh	;Default to 1 MB
									;Page Allocation Map
PUBLIC rgPAM	DB 2048 DUP (0)		;1 bit/4Kbytes - 2048 bytes = 64Mb
PUBLIC sPAM		DD 32				;Deafult is 32 (1Mb)
sPAMmax			EQU 2048			;Max is 2048 (64Mb)

MemExch			DD 00000000h		;Semaphore exchange for Mem Mgmt
pNextPT			DD 00000000h		;We alloc the "next" page table
									;in advance (MANDATORY)
;Some equates (MASKS) for PT Management

;             20 Bit Address      AVL00DA00UWP
MEMALIAS EQU  00000000000000000000100000000111b	;User Writable
MEMUSERD EQU  00000000000000000000000000000111b	;User Writable (Data)
MEMUSERC EQU  00000000000000000000000000000101b	;User Readable (Code)
MEMSYS   EQU  00000000000000000000000000000101b	;System Read/Write
ALIASBIT EQU  00000000000000000000100000000000b
PRSNTBIT EQU  00000000000000000000000000000001b
;
;=============================================================================
; Memory Management Code
;=============================================================================

.CODE

;
; MEMORY Management Code
; OS Internal support calls (near) are at top of this file.
; OS PUBLIC calls are at the bottom
;
; The following internals deal with PHYSICAL memory and the Page
; Allocation Map (PAM).
;
;   InitMemMgmt  - Called to set up all OS memory management functions
;
;   FindHiPage   - Finds first available physical page from top of memory
;				   down and sets it	allocated then returns the
;				   Physical address of the page.
;	FindLoPage   - Finds first available physical page from BOTTOM of memory
;				   and sets it allocated then returns the
;				   Physical address of the page. This is for DMA.
;	MarkPage	 - Given a physical page address, this SETS the PAM bit
;				   to allocate it.
;	UnMarkPage   - Given the physical page address, this RESETS the PAM bit
;				   to deallocate it.
;
; The following internals deal with LINEAR memory and PTs.
;
;	LinToPhy    - Converts a linear to a physical address (OS or APP).
;                 Also provides lin address to PTE in question.
;				  IN:   EBX is Linear address
;				        EAX is Job Number (for PD)
;				  OUT:  EAX is Physical address
;						ESI is Linear address of PTE for EBX Lin Add
;	FindRun     - Finds a run of FREE contiguous PTEs
;				  IN:   EAX = 0 for OS run, or 256 for User run
;				  		EBX = nPages
;				  OUT:  EAX = Linear Address of run (0 if can't find)
;						EBX = nPages (same as in)
;	AddRun      - Creates one or more PTEs (first owner)
;				  IN:	EAX = Linear address of page(s) to add.
;						EBX = count of pages
;				  OUT:	EAX = 0 if OK, else Error
;						EBX = count of pages (same as in)
;	AddDMARun   - Creates one or more PTEs (first owner)
;					(Same as AddRUn except adds LO pages)
;				  IN:	EAX = Linear address of page(s) to add.
;						EBX = count of pages
;				  OUT:	EAX = 0 if OK, else Error
;						EBX = count of pages (same as in)
;
;	AliasRun    - Creates one or more ALIAS PTEs
;
;	AddUserPT   - Allocates physical memory and adds new PT to user PD
;
;	AddOSPT     - Allocates physical memory and adds new PT to ALL PDs
;
;========================================================================

;IN:   Nothing
;OUT:  Nothing (except that you can use memory management routines now!)
;USED: ALL REGISTERS ARE USED.
;
; This section finds out how much memory we have (in MBs) by writing
; to the highest DWord in each meg until it fails a readback test.
; It sets nPages Free after finding out just how much we have.
; We assume 1MB to start (which means we start at 2Mb (1FFFFC).
; It places the highest addressable offset in GLOBAL oMemMax.
; We also calculate the number of pages of physical memory this
; is and store it in the GLOBAL nPagesFree.

PUBLIC InitMemMgmt:
		MOV _nPagesFree, 256	;1 Mb of pages = 256
		MOV EAX,1FFFFCh         ;top of 2 megs (for DWORD)
		XOR EBX,EBX				;
		MOV ECX,06D72746CH      ;'mrtl' test string value for memory
MEMLoop:
		MOV DWORD PTR [EAX],0h	;Set it to zero intially
		MOV DWORD PTR [EAX],ECX	;Move in test string
		MOV EBX,DWORD PTR [EAX]	;Read test string into EBX
		CMP EBX,ECX				;See if we got it back OK
		JNE MemLoopEnd			;NO!
		ADD EAX,3				;Yes, oMemMax must be last byte
		MOV _oMemMax,EAX		;Set oMemMax
		SUB EAX,3				;Make it the last DWord again
		ADD EAX,100000h			;Next Meg
		ADD _nPagesFree, 256	;Another megs worth of pages
		ADD sPAM, 32			;Increase PAM by another meg
		CMP EAX,3FFFFFCh        ;Are we above 64 megs
		JAE MemLoopEnd			;Yes!
		XOR EBX,EBX				;Zero out for next meg test
		JMP MemLoop
MemLoopEnd:

; Page Allocation Map is now sized and ZEROed
; Now we must fill in bits used by OS which was just loaded and
; the Video RAM and Boot ROM (neither of which we consider free).
; This also fills out each of the Page Table Entries (PTEs) for the
; initial OS code and data.  Note that linear address match physical
; address for the initial OS data and code (its the law!)

; This first part MARKS the OS code and data pages as used
; and makes PTEs.
;
		MOV EDX, OFFSET pTbl1		;EDX points to OS Page Table 1
		XOR EAX, EAX				;Point to 1st physical/linear page (0)
IMM001:
		MOV [EDX], EAX				;Make Page Table Entry
		AND DWORD PTR [EDX], 0FFFFF000h 	;Leave upper 20 Bits
		OR	DWORD PTR [EDX], 0001h 			;Supervisor, Present
		MOV EBX, EAX
		CALL MarkPage				;Marks page in PAM
		ADD EDX, 4					;Next table entry
		ADD EAX, 4096
		CMP EAX, 30000h				;Reserve 192K for OS (for now)
		JAE SHORT IMM002
		JMP SHORT IMM001			;Go for more

; Now we fill in PAM and PTEs for Video and ROM slots.
; This covers A0000 thru 0FFFFFh (upper 384K of first Meg).
; Right now we just mark everything from A0000 to FFFFF as used.
; This routine could be expanded to search through the ROM pages of
; ISA memory (C0000 -FFFFF) finding the unaccessable ones and marking
; them as allocated in the PAM. Several chip sets on the market allow
; you to set ROM areas as useable RAM (such as the 82C30 C&T).  But we
; can't be sure everyone can do it, nor can we provide instructions
; to everyone.

IMM002:
		MOV EAX, 0A0000h			;Points to 128K Video & 256K ROM area
		MOV EBX, EAX				;
		SHR EBX, 10					;Make it index (SHR 12, SHL 2)
		MOV EDX, OFFSET pTbl1		;EDX pts to Page Table
		ADD EDX, EBX
IMM003:
		MOV [EDX], EAX					;Make Page Table Entry
		AND DWORD PTR [EDX], 0FFFFF000h ;Leave upper 20 Bits
		OR	DWORD PTR [EDX], 0101b 		;Mark it "User" "ReadOnly" & "Present"
		MOV EBX, EAX				;Setup for MarkPage call
		CALL MarkPage				;Mark it used in the PAM
		ADD EDX, 4					;Next PTE entry
		ADD EAX, 4096				;Next page please
		CMP EAX, 100000h			;1Mb yet?
		JAE IMM004					;Yes
		JMP SHORT IMM003			;No, go back for more

; Initial Page Directory and the Page Table are static.
; Now we can go into PAGED Memory mode.  This is done by loading
; CR3 with the physcial address of the Page Directory, then reading
; CR0, ANDing it with 8000000h and then writing it again.
; After the MOV CR0 we must JMP to clear the prefetch queue of
; any bogus physical addresses.

IMM004:
		MOV EAX, OFFSET PDir1  ;Physical address of OS page directory
		MOV CR3, EAX		;Store in Control Reg 3
		MOV EAX, CR0		;Get Control Reg 0
		OR  EAX, 80000000h	;Set paging bit ON
		MOV CR0, EAX		;Store Control Reg 0
		JMP IM0005			;Clear prefetch queue
IM0005:
;
; Now we allocate an Exchange that the OS uses for a semaphore
; use to prevent reentrant use of the any of the critical
; memory managment functions.
;
		LEA EAX, MemExch		;Alloc Semaphore Exch for Memory calls
		PUSH EAX
		CALL FWORD PTR _AllocExch

		PUSH MemExch				;Send a dummy message to pick up
		PUSH 0FFFFFFF1h
		PUSH 0FFFFFFF1h
		CALL FWORD PTR _SendMsg


		;We must allocate a Page Table to be used when one
		;must be added to a PD (User or OS).  This must be
		;done in advance of finding out we need one because
		;we may not have a linear address to access it in
		;if the current PTs are all used up!  A tad complicated
		;I'm afraid...

		PUSH 1						; 1 page for Next Page Table
		MOV EAX, OFFSET pNextPT		;
		PUSH EAX
		CALL FWORD PTR _AllocOSPage	; Get 'em!

		RETN				;Done initializing memory managment
;

;========================================================================
FindHiPage:
; This finds the first unused physical page in memory from the TOP down
; and returns the physical address of it to the caller.
; It also MARKS the page as used (assuming that we will allocate it).
; Of course this means if we call FindHiPage and don't use it we
; must call UnMarkPage to release it.
; This reduces nPagesFree by one.
;
;IN  :  Nothing
;OUT :  EBX is the physical address of the new page, or 0 if error
;USED:	EBX, Flags

		PUSH EAX
		PUSH ECX
		PUSH EDX
		MOV ECX, OFFSET rgPAM		;Page Allocation Map
		MOV EAX, sPAM				;Where we are in PAM
		DEC EAX						;EAX+ECX will be offset into PAM
FHP1:
		CMP BYTE PTR [ECX+EAX],0FFh		;All 8 pages used?
		JNE FHP2					;No
		CMP EAX, 0					;Are we at Bottom of PAM?
		JE  FHPn					;no memory left...
		DEC EAX						;Another Byte lower
		JMP SHORT FHP1				;Back for next byte
FHP2:
		MOV EBX, 7					;
		XOR EDX, EDX
		MOV DL, BYTE PTR [ECX+EAX]	;Get the byte with a whole in it...
FHP3:
		BT  EDX, EBX				;Test bits
		JNC FHPf					;FOUND ONE! (goto found)
		CMP EBX, 0					;At the bottom of the Byte?
		JE  FHPn					;Error (BAD CPU???)
		DEC EBX						;Next bit
		JMP FHP3
FHPf:
		BTS EDX, EBX				;Set the bit indexed by EBX
		MOV BYTE PTR [ECX+EAX], DL	;Set page in use
		SHL EAX, 3					;Multiply time 8 (page number base in byte)
		ADD EBX, EAX				;Add page number in byte
		SHL EBX, 12					;Now EBX = Physical Page Addr (EBX*4096)
		DEC _nPagesFree				;One less available
		POP EDX
		POP ECX
		POP EAX
		RETN
FHPn:
		XOR EBX, EBX				;Set to zero for error
		POP EDX
		POP ECX
		POP EAX
		RETN
;========================================================================
FindLoPage:
; This finds the first unused physical page in memory from the BOTTOM up.
; It also MARKS the page as used (assuming that we will allocate it).
; Of course this means if we call FindLoPage and don't use it we
; must call UnMarkPage to release it.
; This reduces nPagesFree by one.
;
;IN  :  Nothing
;OUT :  EBX is the physical address of the new page, or 0 if error
;USED:	EBX, Flags

		PUSH EAX
		PUSH ECX
		PUSH EDX
		MOV ECX, OFFSET rgPAM		;Page Allocation Map
		XOR EAX, EAX				;Start at first byte in PAM
FLP1:
		CMP BYTE PTR [ECX+EAX],0FFh		;All 8 pages used?
		JNE FLP2					;No
		INC EAX						;Another Byte higher
		CMP EAX, sPAM				;Are we past at TOP of PAM?
		JAE FLPn					;no memory left...
		JMP SHORT FLP1				;Back for next byte
FLP2:
		XOR EBX, EBX				;
		XOR EDX, EDX
		MOV DL, BYTE PTR [ECX+EAX]	;Get the byte with a whole in it...
FLP3:
		BT  EDX, EBX				;Test bits
		JNC FLPf					;FOUND ONE! (goto found)
		INC EBX						;Next bit
		CMP EBX, 8					;End of the Byte?
		JAE FLPn					;Error (BAD CPU???)
		JMP FLP3
FLPf:
		BTS EDX, EBX				;Set the bit indexed by EBX
		MOV BYTE PTR [ECX+EAX], DL	;Set page in use

		SHL EAX, 3					;Multiply time 8 (page number base in byte)
		ADD EBX, EAX				;Add page number in byte
		SHL EBX, 12					;Now EBX = Physical Page Addr (EBX*4096)
		DEC _nPagesFree				;One less available
		POP EDX
		POP ECX
		POP EAX
		RETN
FLPn:
		XOR EBX, EBX				;Set to zero for error
		POP EDX
		POP ECX
		POP EAX
		RETN

;========================================================================
MarkPage:
; Given a physical memory address, this finds the bit in the PAM associated
; with it and SETS it to show the physical page in use.  This is used
; with the routines that initialize all memory mgmt function.
; This reduces nPagesFree by one.
;
;IN  :  EBX is the physical address of the page to mark
;OUT :	Nothing
;USED:	EBX, Flags

		PUSH EAX
		PUSH ECX
		PUSH EDX
		MOV EAX, OFFSET rgPAM		;Page Allocation Map
		AND EBX, 0FFFFF000h			;Round down to page modulo 4096
		MOV ECX, EBX
		SHR ECX, 15					;ECX is now byte offset into PAM
		SHR EBX, 12					;Get Bit offset into PAM
		AND EBX, 07h				;EBX is now bit offset into byte of PAM
		MOV DL, [EAX+ECX]			;Get the byte into DL
		BTS EDX, EBX				;BitSet nstruction with Bit Offset
		MOV [EAX+ECX], DL			;Save the new PAM byte
		DEC _nPagesFree				;One less available
		POP EDX
		POP ECX
		POP EAX
		RETN
;========================================================================
UnMarkPage:
; Given a physical memory address, this finds the bit in the PAM associated
; with it and RESETS it to show the physical page available again.
; This increases nPagesFree by one.
;
;IN  :  EBX is the physical address of the page to UNmark
;OUT :	Nothing
;USED:	EBX, Flags

		PUSH EAX
		PUSH ECX
		PUSH EDX
		MOV EAX, OFFSET rgPAM		;Page Allocation Map
		AND EBX, 0FFFFF000h			;Round down to page modulo
		MOV ECX, EBX
		SHR ECX, 15					;ECX is now byte offset into PAM
		SHR EBX, 12					;
		AND EBX, 07h				;EBX is now bit offset into byte of PAM
		ADD EAX, ECX
		MOV DL, [EAX]
		BTR EDX, EBX				;BitReset instruction
		MOV [EAX], DL
		INC _nPagesFree				;One more available
		POP EDX
		POP ECX
		POP EAX
		RETN

;============================================================
;
; LinToPhy
; Looks Up the Physical address of a 32 bit linear address passed in.
; The JCB is used to identify who's page tables we are translating.
; The linear address is used to look up the Page Table entry which is
; used to get the physical address.  This call is used for things like
; aliasing for messages, DMA operations, etc.
; This also leave the Linear Address of the PTE itself in ESI
; for callers that need it.
;
; INPUT:	EAX -- Job Number that owns memory we are aliasing
;			EBX -- Linear address
;
; OUTPUT:	EAX -- Physical Address
;           ESI -- Linear Address of PTE for this linear address
;
; USED:		EAX, EBX, ESI, EFlags
;
PUBLIC LinToPhy:
		PUSH EBX				;Save Linear
		CALL GetpJCB			;Leaves pJCB in EAX
		MOV EAX, [EAX+JcbPD]	;EAX now has ptr to PD!
		ADD EAX, 2048			;Move to shadow addresses in PD
		SHR EBX, 22				;Shift out lower 22 bits leaving 10 bit offset
		SHL EBX, 2				;*4 to make it a byte offset into PD shadow
		ADD EBX, EAX			;EBX/EAX now points to shadow
		MOV EAX, [EBX]			;EAX now has Linear of Page Table
		POP EBX					;Get original linear back in EBX
		PUSH EBX				;Save it again
		AND EBX, 003FFFFFh		;Get rid of upper 10 bits
		SHR EBX, 12				;get rid of lower 12 to make it an index
		SHL EBX, 2				;*4 makes it byte offset in PT
		ADD	EBX, EAX			;EBX now points to Page Table entry!
		MOV ESI, EBX			;Save this address for caller
		MOV EAX, [EBX]			;Physical base of page is in EAX
		AND EAX, 0FFFFF000h		;mask off lower 12
		POP EBX					;Get original linear
		AND EBX, 00000FFFh		;Cut off upper 22 bits of linear
		OR EAX, EBX				;EAX now has REAL PHYSICAL ADDRESS!
		RETN

;=============================================================================
; FindRun
; This finds a linear run of FREE LINEAR memory in one of the USER or OS PTs.
; This is either at address base 0 (for OS) or 1Gb (for user).
; EAX = 0 if we are looking for OS memory, else
; EAX = 256 if we are looking for USER memory.
; The linear address of the run is returned in EAX unless no
; run that large exists, in which case we return 0.
; The linear run may span page tables (if they already exist).
; This is an interesting routine because it uses two nested loops
; to walk thru the page directory and page tables while using the
; SIB (Scale Index Base) addressing of the 386 for indexing.
;
; IN :  EAX  PD Shadow Base Offset for memory (0 for OS, 256 for user)
;		EBX  Number of Pages for run
;
; OUT:  EAX  Linear address or 0 if no run is large enough
;		EBX  still has count of pages
; USED: EAX, EBX, EFlags  (all other registers saved & restored)
;
;
FindRun:
		PUSH EBX				;Holds count of pages (saved for caller)
		PUSH ECX				;Keeps count of how many free found so far
		PUSH EDX				;Index into PD for PT we are working on
		PUSH ESI				;Address of PD saved here
		PUSH EDI				;

		MOV ECX, EBX			;Copy number of pages to ECX. Save in EBX
		MOV EDX, EAX			;Index into shadow addresses from EAX

		CALL GetpCrntJCB		;Leaves pCrntJCB in EAX
		MOV ESI, [EAX+JcbPD] 	;ESI now has ptr to PD
		ADD ESI, 2048			;Move to shadow addresses

FR0:
		MOV EDI, [ESI+EDX*4]	;Linear address of next page table into EDI
		OR EDI, EDI				;Is the address NON-ZERO (valid)?
		JNZ FR1					;Yes, go for it
		XOR EAX, EAX			;Return 0 cause we didn't find it!
		JMP SHORT FREXIT		;
FR1:
		XOR EAX, EAX			;EAX indexes into PT (to compare PTEs)
FR2:
		CMP EAX, 1024			;Are we past last PTE of this PT?
		JB  FR3					;No, keep testing
		INC EDX					;Next PT!
		JMP SHORT FR0			;
FR3:
		CMP DWORD PTR [EDI+EAX*4], 0	;Zero means it's empty (available)
		JNE	FR4							;In use
		DEC ECX					;One more empty one!
		JZ  FROK				;We found enough entries goto OK
		INC EAX					;Not done yet, Next PTE Please.
		JMP SHORT FR2			;
FR4:
		;If we got here we must reset ECX for full count and
		;go back and start looking again
		INC EAX					;Not empty, next PTE please
		MOV ECX, EBX			;We kept original count in EBX
		JMP FR2
FROK:
		;If we got here it means that ECX has made it to zero and
		;we have a linear run large enough to satisy the request.
		;The starting linear address is equal to number of the last
		;PTE we found minus ((npages -1) * 4096)
		;EDX was index into PD, while EAX was index into PT.
		;EBX still has count of pages.

		SHL EDX, 22			;EDX is 10 MSBs of Linear Address
		SHL EAX, 12			;EAX is next 10 bits of LA
		OR EAX, EDX			;This is the linear address we ended at
		DEC EBX				;One less page (0 offset)
		SHL EBX, 12			;Times size of page (* 4096)
		SUB EAX, EBX		;From current linear address in tables

FREXIT:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;
		RETN
;=============================================================================
; AddRun
; This adds one or more PTEs to a page table (or tables if the run
; spans two or more tables).
; The address determines the protection level of the PTE's we add.
; If it is less than 1GB it means OS memory which we will set to SYSTEM.
; Above 1Gb is user which we will set to user level protection.
; The linear address of the run should be in EAX, and the count of
; pages should be in EBX (this is the way FindRun left them).
;
; IN :  EAX  Linear address of first page
;		EBX  Number of Pages to add
; OUT:  Nothing
; USED: EAX, EFlags
;
AddRun:
		PUSH EBX				;(save for caller)
		PUSH ECX				;
		PUSH EDX				;
		PUSH ESI				;
		PUSH EDI				;

		MOV ECX, EBX			;Copy number of pages to ECX (EBX free to use).
		MOV EDX, EAX			;LinAdd to EDX
		SHR EDX, 22				;Get index into PD for first PT
		SHL EDX, 2				;Make it index to DWORDS

		PUSH EAX				;Save EAX thru GetpCrntJCB call
		CALL GetpCrntJCB		;Leaves pCrntJCB in EAX
		MOV ESI, [EAX+JcbPD] 	;ESI now has ptr to PD!
		POP EAX					;Restore linear address

		ADD ESI, 2048			;Offset to shadow address of PD
		ADD ESI, EDX			;ESI now points to initial PT (EDX now free)

		MOV EDX, EAX			;LinAdd into EDX again
		AND EDX, 003FF000h		;get rid of upper 10 bits & lower 12
		SHR EDX, 10 			;Index into PD for PT (10 vice 12 -> DWORDS)
AR0:
		MOV EDI, [ESI]			;Linear address of next page table into EDI

		;At this point, EDI is pointing the next PT.
		;SO EDI+EDX will point to the next PTE to do.
		;Now we must call FindPage to get a physical address into EBX,
		;then check the original linear address to see if SYSTEM or USER
		;and OR in the appropriate control bits, THEN store it in PT.

AR1:
		CALL FindHiPage			;EBX has Phys Pg (only EBX affected)
		OR EBX, MEMSYS			;Set PTE to present, User ReadOnly
		CMP EAX, 40000000h		;See if it's a user page
		JB  AR2
		OR EBX, MEMUSERD		;Sets User/Writable bits of PTE

AR2:
		MOV DWORD PTR [EDI+EDX], EBX	;EDX is index to exact entry
		DEC ECX							;Are we done??
		JZ ARDone
		ADD EDX, 4				;Next PTE please.
		CMP EDX, 4096			;Are we past last PTE of this PT?
		JB AR1					;No, go do next PTE
		ADD ESI, 4				;Yes, next PDE (to get next PT)
		XOR EDX,EDX				;Start at the entry 0 of next PT
		JMP SHORT AR0			;
ARDone:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;
		RETN

;=============================================================================
; AddDMARun
; This adds one or more PTEs to a page table (or tables if the run
; spans two or more tables).
; The address determines the protection level of the PTE's we add.
; If it is less than 1GB it means OS memory which we will set to SYSTEM.
; Above 1Gb is user which we will set to user level protection.
; The linear address of the run should be in EAX, and the count of
; pages should be in EBX (this is the way FindRun left them).
;
; IN :  EAX  Linear address of first page
;		EBX  Number of Pages to add
; OUT:  Nothing
; USED: EAX, EFlags
;
AddDMARun:
		PUSH EBX				;(save for caller)
		PUSH ECX				;
		PUSH EDX				;
		PUSH ESI				;
		PUSH EDI				;

		MOV ECX, EBX			;Copy number of pages to ECX (EBX free to use).
		MOV EDX, EAX			;LinAdd to EDX
		SHR EDX, 22				;Get index into PD for first PT
		SHL EDX, 2				;Make it index to DWORDS

		PUSH EAX				;Save EAX thru GetpCrntJCB call
		CALL GetpCrntJCB		;Leaves pCrntJCB in EAX
		MOV ESI, [EAX+JcbPD] 	;ESI now has ptr to PD!
		POP EAX					;Restore linear address

		ADD ESI, 2048			;Offset to shadow address of PD
		ADD ESI, EDX			;ESI now points to initial PT (EDX now free)

		MOV EDX, EAX			;LinAdd into EDX again
		AND EDX, 003FF000h		;get rid of upper 10 bits & lower 12
		SHR EDX, 10 			;Index into PD for PT (10 vice 12 -> DWORDS)
ARD0:
		MOV EDI, [ESI]			;Linear address of next page table into EDI

		;At this point, EDI is pointing the next PT.
		;SO EDI+EDX will point to the next PTE to do.
		;Now we must call FindPage to get a physical address into EBX,
		;then check the original linear address to see if SYSTEM or USER
		;and OR in the appropriate control bits, THEN store it in PT.

ARD1:
		CALL FindLoPage			;EBX has Phys Pg (only EBX affected)
		OR EBX, MEMSYS			;Set PTE to present, User ReadOnly
		MOV DWORD PTR [EDI+EDX], EBX	;EDX is index to exact entry
		DEC ECX							;Are we done??
		JZ ARDDone
		ADD EDX, 4				;Next PTE please.
		CMP EDX, 4096			;Are we past last PTE of this PT?
		JB ARD1					;No, go do next PTE
		ADD ESI, 4				;Yes, next PDE (to get next PT)
		XOR EDX,EDX				;Start at the entry 0 of next PT
		JMP SHORT ARD0			;
ARDDone:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;
		RETN

;=============================================================================
; AddAliasRun
; This adds one or more PTEs to a page table (or tables if the run
; spans two or more tables) adding PTEs from another job's PTs marking
; them as ALIAS entries.
; Aliased runs are ALWAYS at USER protection levels even if in the
; OS address span!
;
; The NEW linear address of the run should be in EAX, and the count of
; pages should be in EBX (this is the way FindRun left them).
; ESI has the linear address we are aliasing and EDX has the Job#
;
; IN :  EAX  Linear address of first page of new alias entries
;            (from find run)
;		EBX  Number of Pages to alias
;       ESI  Linear Address of pages to Alias (from other job)
;       EDX  Job Number of Job we are aliasing
;
; OUT:  Nothing
; USED: EAX, EFlags
;
AliasLin    EQU DWORD PTR [EBP-4]
AliasJob    EQU DWORD PTR [EBP-8]

AddAliasRun:
		PUSH EBP                ;
		MOV EBP,ESP             ;
		SUB ESP, 8

		MOV AliasLin, ESI
		MOV AliasJob, EDX

		PUSH EBX				;(save for caller)
		PUSH ECX				;
		PUSH EDX				;
		PUSH ESI				;
		PUSH EDI				;

		;This first section sets to make [ESI] point to first PT that
		;we have to move the other guy's physical pages into

		MOV ECX, EBX			;Copy number of pages to ECX (EBX free to use).
		MOV EDX, EAX			;LinAdd to EDX
		SHR EDX, 22				;Get index into PD for first PT
		SHL EDX, 2				;Make it index to DWORDS

		PUSH EAX				;Save EAX thru GetpCrntJCB call
		CALL GetpCrntJCB		;Leaves pCrntJCB in EAX
		MOV ESI, [EAX+JcbPD] 	;ESI now has linear address of PD
		POP EAX					;Restore linear address

		ADD ESI, 2048			;Offset to shadow addresses in PD
		ADD ESI, EDX			;ESI now points to first PT of interest

		MOV EDX, EAX			;LinAdd into EDX again
		AND EDX, 003FF000h		;get rid of upper 10 bits & lower 12
		SHR EDX, 10 			;Index into PD for PT (10 vice 12 -> DWORDS)
ALR0:
		MOV EDI, [ESI]			;Linear address of crnt page table into EDI

		;At this point, EDI is pointing to the PT we are in.
		;SO then EDI+EDX will point to the next PTE to do.
		;Now we must call LinToPhy with Linear Add & JobNum
		; to get a physical address into EAX.
		;This is the Physical address to store in the new PTE.  We must
		;mark it MEMALIAS before adding it to PT.
ALR1:
		PUSH ESI				;Save for next loop (used by LinToPhy)

		MOV EAX, AliasJob		;Job we are aliasing
		MOV EBX, AliasLin		;Address we are aliasing
		ADD AliasLin, 4096		;Set up for next loop (post increment)
		CALL LinToPhy			;

		;EAX now has physical address for this page
		;

		AND EAX, 0FFFFF000h		;cut off system bits of PTE
		OR EAX, MEMALIAS		;Set system bits as ALIAS

		POP	ESI					;Restore ESI (LinToPhy used it)

		;Now store it in new PTE

		MOV DWORD PTR [EDI+EDX], EAX	;EDX is index to exact entry

		DEC ECX					;Are we done??
		JZ ALRDone
		ADD EDX, 4				;Next PTE please.
		CMP EDX, 4096			;Are we past last PTE of this PT?
		JB ALR1					;No, go do next PTE
		ADD ESI, 4				;Yes, next PDE (to get next PT)
		XOR EDX,EDX				;Start at the entry 0 of next PT
		JMP SHORT ALR0			;
ALRDone:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;

		MOV ESP,EBP             ;
		POP EBP                 ;
		RETN

;=============================================================================
; AddUserPT
; This creates a new User Page Table, initializes it and sticks it
; in the Users's PD (in User Address space above 1GB).
; This is easier than AddOSPT because there is no need to update
; anyone else's PDs!  This sets the protection on the PT to user
; Read & Write.  Individual PTEs will be set read only for code.
;
; IN :  Nothing
; OUT:  0 if OK or Error (ErcNoMem - no free phy pages!)
; USED: EAX, EFlags
;
AddUserPT:
		PUSH EBX				;(save for caller)
		PUSH ECX				;
		PUSH EDX				;
		PUSH ESI				;
		PUSH EDI				;

		MOV EAX, _nPagesFree	;See if have enuf physical memory
		OR EAX, EAX
		JNZ AUPT01
		MOV EAX, ErcNoMem		;Sorry, out of physical mem
		JMP AUPTDone
AUPT01:
		CALL GetCrntJobNum		;Leaves job num in EAX (for LinToPhy)
		MOV EBX, pNextPT		;Pre allocated Page (Linear Address)
		CALL LinToPhy			;EAX will have Physical address

		; Put it in the User PD (and linear in shadow).
		; Find first empty slot

		CALL GetpCrntJCB		;pJCB in EAX
		MOV EDI, JcbPD			;Offset to PcbPD in JCB
		ADD EDI, EAX			;EDI points to UserPD Address
		MOV ESI, [EDI]			;ESI now points to PD
		ADD ESI, 2048			;ESI now points to upper 1 GB in PD
		MOV ECX, 511			;Number of entries (at least 1 is already gone)
AUPT02:
		ADD ESI, 4				; Next possible empty entry
		MOV	EBX, [ESI]
		OR EBX, EBX 			; Is it empty?
		LOOPNZ AUPT02			; No! (Try again)

		; ESI now points to empty Slot
		; Physical Address of new table is still in EAX
		; Get Linear address back into EBX
		; and put them into PD

		OR  EAX, MEMUSERD		;Set user bits (Read/Write)
		MOV [ESI], EAX			;Physical address in lower half
		ADD ESI, 2048			;Move to shadow
		MOV EBX, pNextPT		;Linear add back into EBX
		MOV [ESI], EBX			;Put in Linear address of PT (upper half)

		;Now we now need another PreAllocated Page Table for
		;next time. Get a run of 1 for next new page table

		MOV EBX, 1				;size of request
		XOR EAX, EAX			;PD shadow offset needed by FindRun (0)
		CALL FindRun
		OR EAX, EAX				;was there an error (0 means no mem)
		JNZ AUPT05
		MOV EAX, ErcNoMem		;
		JMP SHORT AUPTDone
AUPT05:
		MOV pNextPT, EAX		;save pNextPT (the linear address)
		CALL AddRun				;AddRun will return NON-ZERO on error
AUPTDone:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;
		RETN

;=============================================================================
; AddOSPT
; This creates a new OS Page Table, initializes it and sticks it
; in the OS's PD (in OS address space below 1GB).
; This also updates ALL PDs for ALL jobs.  We must do this
; to ensure the OS code can reach its memory no matter what JOB/TASK
; it is running in.
;
; IN :  Nothing
; OUT:  0 if OK or Error (ErcNoMem - no free phy pages!)
; USED: EAX, EFlags
;
AddOSPT:
		PUSH EBX				;(save for caller)
		PUSH ECX				;
		PUSH EDX				;
		PUSH ESI				;
		PUSH EDI				;

		MOV EAX, _nPagesFree	;See if have enuf physical memory
		OR EAX, EAX
		JNZ AOPT01
		MOV EAX, ErcNoMem		;Sorry, out of physical mem
		JMP AOPTDone
AOPT01:
		MOV EAX, 1				;OS Job Number (Monitor)
		MOV EBX, pNextPT		;Pre allocated Page (Linear Address)
		CALL LinToPhy			;EAX will have Physical address

		; Put it in the OS PD (and linear in shadow).
		; Find first empty slot

		MOV ESI, OFFSET PDir1	; ESI points to OS Pdir
		MOV ECX, 511			; Count of PDEs to check
AOPT02:
		ADD ESI, 4				; Next possible empty entry
		MOV EBX, [ESI]
		OR EBX, EBX				; Is it empty?
		LOOPNZ AOPT02			; No! (Try again)

		; ESI now points to empty PDir Slot
		; EAX still has Physical Address of new table
		; Get Physical Address back into EBX
		; and put them into PDir

		OR EAX, PRSNTBIT		;Set present bit
		MOV [ESI], EAX			;Physical address in lower half
		ADD ESI, 2048			;Move to shadow
		MOV EBX, pNextPT		;Linear add back into EBX
		MOV [ESI], EBX			;Put in Linear address of PT (upper half)

		; Update ALL PDs from PDir1 !!
		; This doesn't happen often if it happens at all.
		; The OS will usually not take 4 MBs even with ALL
		; of its dynamic structures (except on
		; a 32 Mb system or larger and when fully loaded)

		MOV EDX, nJCBs			; # of dynamic JCBs
AOPT03:
		MOV EAX, EDX			;Next JCB
		CALL GetpJCB			;EAX now has pointer to a job's PD
		MOV ECX, [EAX+JcbPD]	;See if PD id zero (Inactive JCB)
		OR ECX, ECX				;Is it a valid Job? (0 if not)
		JZ AOPT04				;No, Not a valid JCB (unused)

		ADD EAX, JcbPD			;EAX NOW points to PD of JCB
		MOV EBX, OFFSET PDir1	;Source of Copy

		PUSH EDX				;Save nJCB we are on

		PUSH EAX				;Save values on stack
		PUSH EBX

		PUSH EBX				;Source
		PUSH EAX				;Destination
		PUSH 1024				;Lower half of PD (Physical Adds)
		CALL FWORD PTR _CopyData

		POP  EBX				;Get values from stack
		POP  EAX

		ADD EBX, 2048			;Move to shadow
		PUSH EBX
		ADD EAX, 2048			;Move to shadow
		PUSH EAX
		PUSH 1024				;Upper half of PD (Linear Adds)
		CALL FWORD PTR _CopyData

		POP EDX					; Get back JCB number

AOPT04:
		DEC EDX
		CMP EDX, 2
		JA AOPT03				;Jobs 1 & 2 use PDir1 (Mon & Debugger)

		;At this point the new table is valid to ALL jobs!
		;We now need another PreAllocated Page Table for
		;next time. Get a run of 1 for next new page table

		MOV EBX, 1				;size of request
		XOR EAX, EAX			;PD shadow offset needed by FindRun (0)
		CALL FindRun
		OR EAX, EAX				;was there an error (0 means no mem)
		JNZ AOPT05
		MOV EAX, ErcNoMem		;
		JMP SHORT AOPTDone
AOPT05:
		MOV pNextPT, EAX		;save pNextPT (the linear address)
		CALL AddRun				;AddRun
		XOR EAX, EAX			;Set ErcOK (0)
AOPTDone:
		POP EDI				;
		POP ESI				;
		POP EDX				;
		POP ECX				;
		POP EBX				;
		RETN

;=============================================================================
; GetGDTDesc
; You supply the GDT selector in BX and this puts entry in EAX,EDX.
; EAX, EBX and EDX are used.
; This assumes that the PUBLIC Variable GDTbase has been filled in by
; the OS using SGDT after it went PMode.
; Used by the Debugger among other things.
;
GetGDTDesc:
		AND EBX,0000FFF8h       ;Mask any left overs (hi word)
		ADD EBX,GDTBase         ;Add to GDT base
		MOV EAX,[EBX]
		MOV EDX,[EBX+4]
		RETN

;=============================================================================
;=============================================================================
;  BEGIN PUBLIC CALL DEFINITION FOR MEMORY MANAGEMENT
;=============================================================================
;=============================================================================
; PUBLIC calls (far through call gates)
; AddCallGate   - Adds a public CallGate to the GDT
; AddIDTGate    - Adds an Interrupt Vector to the IDT
; AllocPage     - Returns a ptr to allocated linear memory pages (Hi Phys)
; AllocOSPage   - Returns a ptr to allocated linear memory pages (Hi Phys)
; AllocDMAPage  - Returns a ptr with physical and linear memory pages
; AliasMem		- Aliases a memory address from one Job to another
; DeAllocPage   - Provided with ptr, deallocates memory pages
; QueryMemPages - Tells you how many pages are left free
; GetPhyAdd		- Returns the Physical Address for a Linear Address
;=============================================================================
; AddGDTCallGate will build and add a GDT entry for a call gate allowing
; access to OS procedures. This call doesn't check to see if the GDT
; descriptor for the call is already defined. It assumes you know what you
; are doing and overwrites one if already defined.  The Selector number is
; checked to make sure you're in range (40h thru max call gate num).
;
; IN: AX - Word with Call Gate ID type as follows:
;
;			DPL entry of 3 EC0x   (most likely)
;			DPL entry of 2 CC0x   (Not used in MMURTL)
;			DPL entry of 1 AC0x   (Not used in MMURTL)
;			DPL entry of 0 8C0x   (OS call ONLY)
;			(x = count of DWord params 0-F)
;
;     CX    Selector number for call gate in GDT (constants!)
;	  ESI	Offset of entry point in segment of code to execute
;
; OUT:  EAX	Returns Errors, else 0 if all's well
;
; USES: EAX, EBX, ECX, ESI, EFLAGS

PUBLIC __AddCallGate:
		CMP CX, 40h 		;Is number within range of callgates?
		JAE AddCG01			;not too low.
		MOV EAX, ercBadGateNum
		RETF
AddCG01:
		MOVZX EBX, CX
		SUB EBX, 40			;sub call gate base selector
		SHR EBX, 3			;make index vice selector
		CMP EBX, nCallGates	;see if too high!
		JBE AddCG02			;No.
		MOV EAX, ercBadGateNum	;Yes.
		RETF
AddCG02:
		MOVZX EBX, CX		;Extend selector into EBX
		ADD EBX, GDTBase	;NOW a true offset in GDT
		MOV WORD PTR [EBX+02], 8	;Put Code Seg selector into Call gate
		MOV [EBX], SI		;0:15 of call offset
		SHR ESI, 16			;move upper 16 of offset into SI
		MOV [EBX+06], SI	;16:31 of call offset
		MOV [EBX+04], AX	;call DPL & ndParams
		XOR EAX, EAX		;0 = No Error
		RETF

;=============================================================================
; AddIDTGate will build and add an IDT Trap, Interrupt, or Task Gate.
; The Selector of the call is Always 8 for Int or Trap, and is the
; TSS of the task for a Task gate.
;
; IN:	AX	- Word with Gate ID type as follows:
;				Trap Gate with DPL of 3       8F00
;				Interrupt Gate with DPL of 3  8E00
;				Task Gate with DPL of 3       8500
;
;		BX	- Selector of gate (08 or TSS selector for task gates)
;
;		CX	- Word with Interrupt Number (00-FF)
;
;		ESI - Offset of entry point in OS code to execute
;			  (THIS MUST BE 0 FOR TASK GATES)
;
; USES: EAX, EBX, ECX, EDX, ESI, EFLAGS

PUBLIC __AddIDTGate:
		MOVZX EDX, CX				;Extend INT Num into EDX
		SHL EDX, 3					;Gates are 8 bytes each (times 8)
		ADD EDX, OFFSET IDT			;EDX now points to gate
		MOV WORD PTR [EDX+4], AX	;Put Gate ID into gate
		MOV EAX, ESI
		MOV WORD PTR [EDX], AX		;Put Offset 15:00 into gate
		SHR EAX, 16
		MOV WORD PTR [EDX+6], AX	;Put Offset 31:16 into gate
		MOV WORD PTR [EDX+2], BX	;Put in the selector
		RETF
;
;
;=============================================================================
; AllocOSPage --
; This allocates one or more pages of physical memory and returns a
; linear pointer to one or more pages of contiguous memory in the OS space.
; A result code is returned in the EAX register.
; STEPS:
; 1) See if we have enough physical memory (check nPagesFree)
; 2) Find a contiguous run of linear pages to allocate (PTEs)
; 3) Allocate each physical page placing it in the run of PTEs
;
; We search thru the page tables for the current job and find enough
; contiguous PTEs to satisfy the request.  If the current PT doesn't have
; enough contiguous entries, we add another page table to the OS PD
; and get them from the new one and the old one (i.e., the run may
; span page tables).
;
; Procedureal Interface :
;
;   AllocOSPage(dn4KPages,ppMemRet): dError
;
;   dn4KPages is a DWORD (4 BYTES). This is the number of contigous pages
;	to be allocated.
;
;   ppMemRet points to the pointer where the address of the
;			new linear memory is returned.
;
n4KPages	EQU [EBP+10h]		;These equates are also used by AllocPage
ppMemRet 	EQU [EBP+0Ch]		;

PUBLIC __AllocOSPage:           ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		PUSH MemExch			;Wait at the MemExch for Msg
		MOV EAX, pRunTSS		;Put Msg in callers TSS Message Area
		ADD EAX, TSS_Msg
		PUSH EAX
		CALL FWORD PTR _WaitMsg
		CMP EAX,0h				;Kernel Error??
		JNE SHORT ALOSPExit     ;Yes! Serious problem.

		MOV EAX,n4KPages		;size of request
		OR  EAX,EAX				;More than 0?
		JNZ ALOSP00				;Yes
		MOV EAX,ercBadMemReq    ;Can't be zero!
		JMP ALOSPExit           ;
ALOSP00:
		CMP EAX, _nPagesFree	;See if have enuf physical memory
		JBE ALOSP01				;Yes
		MOV EAX, ErcNoMem		;Sorry boss, we're maxed out
		JMP SHORT ALOSPExit
ALOSP01:
		MOV EBX,n4KPages		;size of request
		XOR EAX, EAX			;PD shadow offset needed by FindRun (0)
		CALL FindRun
		OR EAX, EAX				;(0 = No Runs big enuf)
		JNZ SHORT ALOSP02		;No Error!

		;If we didn't find a run big enuf we add a page table

		CALL AddOSPT			;Add a new page table (we need it!)
		OR EAX, EAX				;See if it's 0 (0 = NO Error)
		JZ SHORT ALOSP01		;Go back & try again
		JMP SHORT ALOSPExit		;ERROR!!
ALOSP02:
								;EAX now has linear address
								;EBX still has count of pages
		CALL AddRun				;Does not return error
								;EAX still has new linear address
		MOV EBX, ppMemRet		;Get address of caller's pointer
		MOV [EBX], EAX			;Give em new LinAdd
		XOR EAX, EAX			;No error
ALOSPExit:			            ;
		PUSH EAX				;Save last error
		PUSH MemExch			;Send a Semaphore msg (so next guy can get in)
		PUSH 0FFFFFFF1h			;
		PUSH 0FFFFFFF1h			;
		CALL FWORD PTR _SendMsg	;
		POP EAX					;Get original error back (ignore kernel erc)
		MOV ESP,EBP				;
		POP EBP                 ;
		RETF 8                  ;

;=============================================================================
; AllocPage --
; This is identical to AllocOSPage except it's call gate is set to USER
; level and the PD base value is set to 3072.  See the call description
; for AllocOSPage

PUBLIC __AllocPage:             ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		PUSH MemExch			;Wait at the MemExch for Msg
		MOV EAX, pRunTSS		;Put Msg in callers TSS Message Area
		ADD EAX, TSS_Msg
		PUSH EAX
		CALL FWORD PTR _WaitMsg
		CMP EAX,0h				;Kernel Error??
		JNE SHORT ALPExit       ;Yes! Serious problem.
		MOV EAX,n4KPages		;size of request
		OR  EAX,EAX				;More than 0?
		JNZ ALP00				;Yes
		MOV EAX,ercBadMemReq    ;Can't be zero!
		JMP ALPExit				;
ALP00:
		MOV EAX,n4KPages		;size of request
		CMP EAX, _nPagesFree	;See if have enuf physical memory
		JBE ALP01				;Yes
		MOV EAX, ErcNoMem		;Sorry boss, we're maxed out
		JMP SHORT ALPExit
ALP01:
		MOV EBX, EAX			;nPages
		MOV EAX, 256			;PD base for USER mem (needed by FindRun)
		CALL FindRun
		OR EAX, EAX				;(0 = No Runs big enuf)
		JNZ SHORT ALP02			;No Error!

		CALL AddUserPT			;Add a new page table (we need it!)
		OR EAX, EAX				; 0 = NO Error
		JZ SHORT ALP01			; Go back & try again
		JMP SHORT ALPExit		; ERROR!!

ALP02:
		CALL AddRun				;Does not return error
		MOV EBX, ppMemRet		;Get address of caller's pointer
		MOV [EBX], EAX			;Give em new LinAdd
		XOR EAX, EAX			;No error
ALPExit:			            ;
		PUSH EAX				;Save last error
		PUSH MemExch			;Send a Semaphore msg (so next guy can get in)
		PUSH 0FFFFFFF1h			;
		PUSH 0FFFFFFF1h			;
		CALL FWORD PTR _SendMsg	;
		POP EAX					;Get original error back (ignore kernel erc)
		MOV ESP,EBP				;
		POP EBP                 ;
		RETF 8                  ;

;=============================================================================
; AllocDMAPage --
; This allocates one or more pages of physical memory and returns a
; linear pointer to one or more pages of contiguous memory in the OS space.
; This is for DMA, and thusly allocates the physical memory
; from the low end!!!!
; The caller is responsible to ensure the physical addresses do
; not cross 64K boundaries for 8 bit channel use,
; or 128K boundaries for 16 bit use. The caller should also check
; to ensure the physical pages are contiguous. He can allocate
; until they are then deallocate what he doesn't use.
;
; A result code is returned in the EAX register.
; STEPS:
; 1) See if we have enough physical memory (check nPagesFree)
; 2) Find a contiguous run of linear pages to allocate (PTEs)
; 3) Allocate each physical page placing it in the run of PTEs
;    from the bottom up!!!
;
; We search thru the page tables for the OS and find enough
; contiguous PTEs to satisfy the request.  If the current PT doesn't have
; enough contiguous entries, we add another page table to the OS PD
; and get them from the new one and the old one (i.e., the run may
; span page tables).
;
; Procedureal Interface :
;
;   AllocDMAPage(dn4KPages,ppMemRet,pdPhyMemRet): dError
;
;   dn4KPages is a DWORD (4 BYTES). This is the number of contigous pages
;	to be allocated.
;
;   ppMemRet points to the pointer where the address of the
;			new linear memory is returned.

;   pdPhyMemRet points to DWord where the physical address of the memory
;   		is returned.
;
n4KDMAPages 	EQU [EBP+20]		;
ppDMAMemRet 	EQU [EBP+16]		;
pdDMAPhyMemRet 	EQU [EBP+12]		;

EXTRN GetCrntJobNum NEAR

PUBLIC __AllocDMAPage:           ;
		PUSH EBP                 ;
		MOV EBP,ESP             ;
		PUSH MemExch			;Wait at the MemExch for Msg
		MOV EAX, pRunTSS		;Put Msg in callers TSS Message Area
		ADD EAX, TSS_Msg
		PUSH EAX
		CALL FWORD PTR _WaitMsg
		CMP EAX,0h				;Kernel Error??
		JNE SHORT ALDMAPExit    ;Yes! Serious problem.

		MOV EAX,n4KDMAPages		;size of request
		OR  EAX,EAX				;More than 0?
		JNZ ALDMAP00			;Yes
		MOV EAX,ercBadMemReq    ;Can't be zero!
		JMP ALDMAPExit          ;
ALDMAP00:
		CMP EAX, _nPagesFree	;See if have enuf physical memory
		JBE ALDMAP01			;Yes
		MOV EAX, ErcNoMem		;Sorry boss, we're maxed out
		JMP SHORT ALDMAPExit
ALDMAP01:
		MOV EBX, EAX			;size of request
		XOR EAX, EAX			;PD shadow offset needed by FindRun (0)
		CALL FindRun
		OR EAX, EAX				;(0 = No Runs big enuf)
		JNZ SHORT ALDMAP02		;No Error!

		;If we didn't find a run big enuf we add a page table

		CALL AddOSPT			;Add a new page table (we need it!)
		OR EAX, EAX				;See if it's 0 (0 = NO Error)
		JZ SHORT ALDMAP01		;Go back & try again
		JMP SHORT ALDMAPExit		;ERROR!!
ALDMAP02:
								;EAX now has linear address
								;EBX still has count of pages
		CALL AddDMARun			;Does not return error
								;EAX still has new linear address
		MOV EBX, ppDMAMemRet		;Get address of caller's pointer
		MOV [EBX], EAX			;Give em new LinAdd

		;Set up to get the physical address of the linear we just gave 'em

		MOV EBX, EAX			;Linear to EBX
		CALL GetCrntJobNum		;Leaves job num in EAX
		CALL LinToPhy			;Leave Phy in EAX
		MOV EBX,pdDMAPhyMemRet	;Give them the physical address
		MOV [EBX], EAX

		XOR EAX, EAX			;No error

ALDMAPExit:			            ;
		PUSH EAX				;Save last error
		PUSH MemExch			;Send a Semaphore msg (so next guy can get in)
		PUSH 0FFFFFFF1h			;
		PUSH 0FFFFFFF1h			;
		CALL FWORD PTR _SendMsg	;
		POP EAX					;Get original error back (ignore kernel erc)
		MOV ESP,EBP				;
		POP EBP                 ;
		RETF 12                 ;

;=============================================================================
; AliasMem --
; This creates alias pages in the current job's PD/PTs if the current
; PD is different than the PD for the job specified.  This allows
; system services to access a caller memory for messaging WITHOUT having
; to move data around.  The pages are create at USER protection level
; even if they are in OS memory space (a service installed in OS memory).
; Even if the address is only two bytes, if it crosses page boundries,
; we need two pages.
;
; STEPS:
; 1) See if the current PD = Specified Job PD. If so, Exit. (No alias needed).
; 2) Calculate how many entries (pages) will be needed.
; 3) See if they are available.
; 3) Make PTE entries and return alias address to caller.
;
; Procedureal Interface :
;
;   AliasMem(pMem, dcbMem, dJobNum, ppAliasRet): dError
;
;   pMem   is the address to alias.
;	dcbMem is the number of bytes needed for alias access.
;	JobNum is the job number that pMem belongs to.
;	ppAliasRet is the address to return the alias address to.
;
;
;pMem		EQU [EBP+24]		;
;dcbMem		EQU [EBP+20]		;
;JobNum		EQU [EBP+16]		;
;ppAliasRet EQU [EBP+12]		;

PUBLIC __AliasMem               ;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		CALL GetCrntJobNum		;Puts Current Job Num in EAX
		MOV EBX, [EBP+16]		;Get Job number for pMem
		CMP EAX, EBX			;Are they the same Page Directory??
		JNE ALSPBegin			;No, alias it
		XOR EAX, EAX			;Yes, No Error
		JMP ALSPDone			;Exit, we're done

ALSPBegin:
		;Now wait our turn with memory management

;		PUSH MemExch			;Wait at the MemExch for Msg
;		MOV EAX, pRunTSS		;Put Msg in callers TSS Message Area
;		ADD EAX, TSS_Msg
;		PUSH EAX
;		CALL FWORD PTR _WaitMsg
;		CMP EAX,0h				;Kernel Error??
;		JNE ALSPDone      		;Yes! Serious problem.

		; We're IN!

ALSP00:
		MOV EBX, [EBP+24]			;pMem into EAX
		AND EBX, 0FFFh				;MODULO 4096  (remainder)
		MOV EAX, [EBP+20]			;dcbMem
		ADD EAX, EBX				;Add the remainder of address
		SHR EAX, 12				    ;EAX is nPages-1
		INC EAX						;EAX is now nPages we need!
		MOV ECX, EAX				;Save nPages in ECX

		;Now we find out whos memory we are in to make alias
		;EAX is 256 for user space, 0 for OS
		;EBX is number of pages for run

		CALL GetCrntJobNum			;See if it is OS based service
		CMP EAX, 1					;OS Job?
		JE SHORT ALSP011			;Yes
		MOV EAX, 256				;No, User memory
		JMP SHORT ALSP01
ALSP011:
		XOR EAX, EAX				;Set up for OS memory space
ALSP01:
		MOV EBX, ECX				;Number of pages we need into EBX
		CALL FindRun				;EAX has 0 or 256

		;EAX is now linear address or 0 if no run is large enough
		;EBX  still has count of pages

		OR EAX, EAX					;Was there enough PTEs?
		JNZ ALSP04					;Yes

		CALL GetCrntJobNum			;See if it is OS based service
		CMP EAX, 1					;OS Job?
		JE SHORT ALSP02				;Yes (RAB)

		CALL AddUserPT				;No!  Add a new USER page table
		JMP SHORT ALSP03
ALSP02:
		CALL AddOSPT				;No!  Add a new OS page table
ALSP03:
		OR EAX, EAX					;0 = NO Error
		JZ SHORT ALSP00				;Go back & try again
		JMP SHORT ALSPExit			;ERROR!!

ALSP04:
		;EAX has linear address (from find run) Sve in EDI
		;EBX still has number of pages to alias
		;Set ESI to linear address of pages to alias (from other job)
		;Set EDX job number of job we are aliasing

		MOV EDI, EAX				;Save alias page address base
		MOV ESI, [EBP+24]			;Address to alias
		MOV EDX, [EBP+16]			;Job number
		CALL AddAliasRun

		;Now, take new alias mem and add trailing bits to address
		;and return to caller so he knows address (EDI is lin add)

		MOV EAX, [EBP+24]			;original pMem
		AND EAX, 0FFFh				;Get remaining bits
		ADD EDI, EAX
		MOV ESI, [EBP+12]			;pAliasRet
		MOV [ESI], EDI				;Returned address to caller!

		XOR EAX, EAX				;Set to 0 (no error)

		;We are done
ALSPExit:			            ;
;		PUSH EAX				;Save last error
;		PUSH MemExch			;Send a Semaphore msg (so next guy can get in)
;		PUSH 0FFFFFFF1h				;
;		PUSH 0FFFFFFF1h				;
;		CALL FWORD PTR _SendMsg	;
;		POP EAX					;Get original error back (ignore kernel erc)
ALSPDone:
		MOV ESP,EBP				;
		POP EBP                 ;
		RETF 16                 ;

;=============================================================================
; DeAliasMem --
;
; Procedureal Interface :
;
;		DeAliasMem(pAliasMem, dcbAliasBytes, JobNum):ercType
;
;   pAliasMem is the address which was given to you from the AliasMem call.
;   This zeros out the page entries that were made during the AliasMem
;   call. We do not need to go through the OS MEM semaphore exchange
;	because we are only zeroing out PTE's one at a time. This
;	WOULD NOT interfere with any memory allocation routines.
;
;	pAliasMem is the address to DeAlias
;	dcbAliasBytes is the size of the original memory aliased
;
;pAliasMem 		EQU [EBP+20]
;dcbAliasBytes	EQU [EBP+16]
;AliasJobNum	EQU [EBP+12]

PUBLIC __DeAliasMem           	;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV EBX, [EBP+20]			;pMem into EBX
		AND EBX, 0FFFh				;MODULO 4096  (Get remainder)
		MOV EAX, [EBP+16]			;dcbMem
		ADD EAX, EBX				;Add the remainder of address
		SHR EAX, 12				    ;EAX is nPages-1
		INC EAX						;EAX is now nPages we to dealias!
		MOV ECX, EAX				;Number of pages into EDI & ECX
		MOV EDI,ECX					;Save also in EDI (for compare)
		MOV EDX, [EBP+20]			;Linear Mem to DeAlias

DALM01:
		MOV EBX, EDX			;Address of next page to deallocate
		MOV EAX, [EBP+12]		;Job num into EAX for LinToPhy
		CALL LinToPhy			;Call this to get address of PTE into ESI

		;Now we have Physical Address in EAX (we don't really need it)
		;and pointer to PTE in ESI (We NEEDED THIS).
		;See if PTE is an alias, if so just ZERO PTE.
		;DO NOT deallocate the physical page

		MOV EBX, [ESI]			;Get PTE into EBX
		TEST EBX, PRSNTBIT		;Is page present (valid)???
		JNZ DALM02				;Yes, it's page is present

		CMP ECX, EDI			;NO! (did we do any at all)
		JNE DALM011				;We did some.
		MOV EAX, ErcBadLinAdd	;None at all!
		JMP SHORT DALMExit

DALM011:
		MOV EAX, ErcBadAlias	;We dealiased what we could,
		JMP SHORT DALMExit		;but less than you asked for!

DALM02:
		TEST EBX, ALIASBIT		;Is page an ALIAS?
		JZ DALM03				;NO - DO not zero it!

		;If we got here the page is presnt and IS an alias
		;so we zero out the page.

		XOR EAX, EAX			;
		MOV [ESI], EAX			;ZERO PTE entry
DALM03:

		ADD EDX, 4096			;Next linear page
		LOOP DALM01
								;If we fall out EAX = ErcOK already
DALMExit:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12	                ;


;=============================================================================
; DeAllocPage --
;
; Procedureal Interface :
;
;		DeAllocPage(pOrigMem, n4KPages):ercType
;
;   pOrigMem is a POINTER which should be point to memory page(s) to be
;   deallocate.	 The lower 12 bits of the pointer is actually ignored
;   because we deallocate 4K pages.  This will free physical pages unless
;   the page is marked as an alias. It will always free linear memory
;   providing it is valid.  Even if you specify more pages than are valid
;   this will deallocate or dealias as much as it can before reaching
;   an invalid page.
;
;	n4KPages is the number of 4K pages to deallocate
;
pOrigMem 	EQU [EBP+10h]
n4KPagesD	EQU [EBP+0Ch]		;

PUBLIC __DeAllocPage:           ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		PUSH MemExch			;Wait at the MemExch for Msg
		MOV EAX, pRunTSS		;Put Msg in callers TSS Message Area
		ADD EAX, TSS_Msg
		PUSH EAX
		CALL FWORD PTR _WaitMsg
		CMP EAX,0h				;Error??
		JNE DAMExit             ;Yes!

		MOV EDX, pOrigMem		;Linear Mem to deallocate
		AND EDX, 0FFFFF000h		;Drop odd bits from address (MOD 4096)
		MOV ECX, n4KPagesD		;Number of pages to deallocate

DAP01:
		MOV EBX, EDX			;Address of next page to deallocate
		CALL GetCrntJobNum		;Leave Job# in EAX for LinToPhy
		CALL LinToPhy			;

		;Now we have Physical Address in EAX
		;and pointer to PTE in ESI.
		;See if PTE is an alias, if so just ZERO PTE,
		;else deallocate physical page THEN zero PTE

		MOV EBX, [ESI]			;Get PTE into EBX
		TEST EBX, PRSNTBIT		;Is page present (valid)???
		JNZ DAP02				;Yes, it's page is present

		CMP ECX, n4KPagesD		;NO! (did we do any at all)
		JNE DAP011				;We did some..
		MOV EAX, ErcBadLinAdd	;None at all!
		JMP SHORT DAMExit

DAP011:
		MOV EAX, ErcShortMem	;We deallocated what we could,
		JMP SHORT DAMExit		;but less than you asked for!

DAP02:
		TEST EBX, ALIASBIT		;Is page an ALIAS?
		JNZ DAP03				;Yes, it's an Alias

		;If we got here the page is presnt and NOT an alias
		;so we must unmark (release) the physical page.

		AND EBX, 0FFFFF000h		;get rid of OS bits
		CALL UnMarkPage			;

DAP03:
		XOR EAX, EAX			;
		MOV [ESI], EAX			;ZERO PTE entry

		ADD EDX, 4096			;Next linear page
		LOOP DAP01
								;If we fall out EAX = ErcOK already
DAMExit:
		PUSH EAX				;save Memory error

		PUSH MemExch			;Send a dummy message to pick up
		PUSH 0FFFFFFF1h			; so next guy can get in
		PUSH 0FFFFFFF1h
		CALL FWORD PTR _SendMsg	;
		CMP EAX, 0				;Kernel error has priority
		JNE DAMExit1			; over memory error

		POP EAX					;get Memory error back

DAMExit1:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8 	                ;

;=============================================================================
; QueryMemPages --
;
; Procedureal Interface :
;
;		QueryMemPages(pdnPagesRet):ercType
;
;	pdnPagesRet is a pointer where you want the count of pages
;   left available returned
;
pMemleft 	EQU [EBP+0Ch]

PUBLIC __QueryPages:            ;
		PUSH EBP                ;
		MOV EBP,ESP             ;

		MOV ESI, pMemLeft
		MOV EAX,  _nPagesFree
		MOV [ESI], EAX
		XOR EAX, EAX			;No Error

		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 4 	                ;

;==============================================================================
;
; GetPhyAdd -- This returns the phyical address for a linear address
;
;
; Procedureal Interface :
;
;		GetPhyAdd(JobNum, LinAdd, pPhyRet):ercType
;
;	LinAdd is the Linear address you want the physical address for
;	pPhyRet points to the unsigned long where Phyadd is returned
;
;
;JobNum  EQU [EBP+20]
;LinAdd  EQU [EBP+16]
;pPhyRet EQU [EBP+12]
;

PUBLIC __GetPhyAdd:      		;
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EBX, [EBP+16]		; Linear Address
		MOV EAX, [EBP+20]		; Job Number
		CALL LinToPhy
		MOV ESI, [EBP+12]		; pPhyRet
		MOV [ESI], EAX			;
		XOR EAX, EAX			; No Error
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 12	                ;


;====== End Of Module =====================
