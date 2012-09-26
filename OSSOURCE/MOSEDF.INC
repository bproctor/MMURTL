;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED   Version 1,0
;=============================================================================
; General EQUates for the OS. Included in many modules.
;=============================================================================

TRUE			EQU 1
FALSE			EQU 0

; Error and Status Codes.  All MMURTL functions (with the exception
; of a very few) return a Status or Error code. It is a 32 bit
; value. Unlike some systems which may indicate any number
; of errors or function status by returning a single value
; such as -1, MMURTL tells you what happened exactly upon
; return from each call.
; You can assume (unless specified in the API for that call),
; that ZERO (0) means everything went as expected.
; The values assigned are broken into groups based on
; functionality.  The numbers were selected arbitrarily as the
; OS code was developed, so there is no real reasoning behind
; the numbers themselves.
; If you add services, or device drivers, please use numbers beyond
; the range of these to indicate errors that would not
; otherwise be standardized.  If a standardized error code
; fits the description of an error you would return, then use it.
;---------------------------
;
;1-199	OS Kernel and Resource Management (general, used by all)

ErcOk			EQU 0	;Normal Status, No errors
ErcEOF          EQU 1	;End of file
ErcOpCancel		EQU 4	;Operator cancel
ErcNullPtr		EQU 6	;Null ptr passed into call or service

;10 - 19 Exchange Errors for Allocation and Use

ErcOutOfRange	EQU 10	;Exchange number beyond allocated count
ErcNotAlloc		EQU 11	;Bad exchange (not allocated)
ErcNotOwner		EQU 12	;Attempt to deallocate an Exch that's not yours

;20 - 29 Message Management

ErcNoMsg		EQU 20	;Returned by Check primitive when no msg waiting

;30 - 39 System Service Management

ErcNoSuchSvc	EQU 30	;No service by that name
ErcBadSvcCode	EQU 32	;Service doesn't handle that code
ErcNotRqBlk		EQU 34	;Service received a NON RqBlock at it's exchange
ErcOutOfSvcDesc EQU 36	;No more empty Service Descriptors
ErcOwnerAbort	EQU 37	;Service received SvcCode 0 on owner of this RqBlk

;OS Resource Management Errors

ErcNoMoreExch	EQU 40	;
ErcNoMoreLBs	EQU 41	;Out of Link Blocks
;ErcMsgMissing	EQU 42
ErcNoSvcBlks	EQU 43	;No more servcice can install
ErcNoMoreTBs	EQU 44	;Out of timer blocks
ErcNoMoreTSSs	EQU 45	;Out of TSSs
ErcNoMoreRqBlks EQU 46	;Out of Request Blocks
ErcNoMoreJCBs   EQU 47	;Out of Job Control Blocks

;OS Call Gate Management
ErcBadGateNum  	EQU 48	;tried to add an invalid GDT call gate
ErcBadCallGate	EQU 49	;Called an UNinitialized Call Gate!!

;OS Task Management

ErcBadPriority	EQU 50

;OS Job Management

ErcBadJobNum	EQU 70	;A Bad job number was specified in an OS call
ErcInvalidJCB	EQU 71	;The Job number specifies an unassigned JCB
ErcBadRunFile	EQU 74	;The run file you specified is NOT a run file!
ErcNoExitJob	EQU 76	;No exit job was specified on ExitJob(n)
ErcBadParams	EQU 80	;Invalid or bad params were passed to a command

;Memory Management

ErcNoGdtSlots	EQU 100	;No more free rgMemDesc GDT slots!
ErcNoMem		EQU 101	;Not enough memory (no more pages!!!)
ErcBadMemPage	EQU 102	;Bad physical page specified in Mem call
ErcBadMemReq	EQU 104	;Invalid size for memory request
ErcInternalMem  EQU 106 ;An error has occurred that shouldn't, this indicates
						;an internal inconsistancy. AddPage can't!
ErcNoRuns		EQU 107 ;No free runs large enough in PTs (temporary)
ErcBadLinAdd    EQU 108	;Bad linear address was passed to DeallocMem
ErcShortMem     EQU 109 ;Passed in too many pages to Dealloc, but
                        ;as many as possible were deallocated
ErcBadAlias     EQU 111 ;Address passed in WASN't an alias (it should be)

;File System    200 - 299  See FSYS.C for complete description

;Character Video Management 300 -399

ErcVidNum		EQU 300 ;Bad video number passed as param to vid call
ErcVidparam		EQU 301 ;A param was out of range to a Vid call
ErcEditParam	EQU 300	;Bad param to EditLine
ErcBadString	EQU 302	;Invalid sting passed to Math Cnvrt Func

;OS hardware Resource Management

ErcDMAChannel	EQU 400  ;Invalid DMA channel specified as param
ErcDMAMode		EQU 401  ;Bad mode specified

;500-599 Device Management (General)

ErcBadDevNum	EQU 500		;DCB number too large ( > max DCBs)
ErcDevInUse		EQU 501		;DCB already in use
ErcBadDevName	EQU 502		;Size of device name incorrect
ErcBadOpNum		EQU 503		;OpNum to DD that it doesn't handle!
ErcNoDevice		EQU 504		;Driver's installed but NO device is
ErcNoDriver		EQU 505		;No driver installed for that Device Num
ErcDCBInUse		EQU 506		;Attempt to install driver over valid FDC

;600-649 Floppy Device Driver (See FDD.c)

;650-699 Hard Disk Device Driver (See HDD.c)

;700-749 Keyboard Service

ErcNoKeyAvail	EQU 700

;800-899 Async Comms Device Driver (COMx)

;900-949 Parallel Comms Device Driver (LPTx)


;----------------------------------------------------------------
;OS Constants

OSCodeSel		EQU 0008h	;OS code is 0 based on this selector
DataSel			EQU 0010h	;All data is 0 based on this selector
JobCodeSel		EQU 0018h	;All User Code is 0 based on this selector

NIL				EQU 0		;Duh...
nTSS			EQU 66		;2 static and 64 dynamic
nLB				EQU 512		;Number of link blocks
nDynEXCH		EQU 256		;number of dynamic exchanges (fills one page)
nPRI			EQU 32		;Task priorities (0 to 31)
nSVC			EQU 64		;max services that can be installed
nTmrBlks		EQU 32		;max outstanding timer blocks
nCallGates		EQU 694		;Number of call gates (Max!)

;6144 (6Kb) of GDT slots - 768 total

nGDTSlots		EQU (8 + nCallGates + nTSS)

;=============================================================================
;
; The Link Blocks are small structures used to attach
; messages to exchanges.  They each form a piece
; of a linked list. This is required because more
; than one message may be waiting at an exchange.
; Each link block is 16 bytes long
;
DATALB		EQU 00h				;Used in LBType to indicate LB usage
REQLB		EQU 01h
RESPLB		EQU 02h
REQLBA		EQU 03h				;RqBlk with pointers already aliased

sLINKBLOCK	EQU 16
DataLo		EQU 0	;DD 00000000h		; Data Low DWord   or  pRqBlk
DataHi		EQU 4	;DD 00000000h		; Data High DWord
NextLB		EQU 8	;DD 00000000h		; Pointer to next LB (Free, or in Link)
LBType		EQU 12	;DB 00h				; SEND                 REQUEST
;LBResvd		;DB 0,0,0			; Pad to 16 bytes
;
;------------------------------------
; The queue type is used to form an array of Queues
; where tasks will be lined up to run.  They
; form the ReadyQ.
; Each Queue Type is 8 bytes long
;
; NOTE: If this size sQUEUE changes you must change the enQueueRdy
;		code because it uses a shift instruction
;		(SHL 3) for speed instead of multiplying by the size.
;		It is unlikely that this size will change, but you never know.
;
sQUEUE		EQU 8
Head		EQU 0	;DD 00000000h		; head Queue Pointer
Tail		EQU 4	;DD 00000000h		; tail Queue Pointer
;
;
;------------------------------------
; Each Exchange is 16 bytes long
; NOTE: We use shift instructions for multiplying to calculate
; a pointer to an exchange. The size of the Exch structure
; should always be a power of 2. It's a speed issue...
;
sEXCH		EQU 16
EHead		EQU 0	;DD 00000000h		; Struct for the msg/Task queue
ETail		EQU 4	;DD 00000000h		;
fEMsg 		EQU 8	;DD 00000000h		; True if this holds messages (vice task)
Owner		EQU 12	;DD 00000000h		; pointer to JCB

;
;
;-------------------------------------------------
; The System Service Descriptor	type is a structure
; that keeps the names and exchanges of all active
; system services (it forms an array of names).
; This is used to locate the exchange where a service
; is waiting for requests.
; Each System Service Descriptor is 12 bytes long
;
sSVC		EQU 12
SvcName		EQU 0	;DD 00000000h		; System Service Name (8 bytes)
					;DD 00000000h
SvcExch		EQU 8	;DD 00000000h		; System Service Exchange
;
;
;Video stuff used in more than one module

VGATextBase EQU 0B8000h ;Offset from OS DS to VGA Text screen 0

;================= END of Module ==================
