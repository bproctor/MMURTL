;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994,1995 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0
;
.DATA					;Begin Keyboard Data
.INCLUDE MOSEDF.INC
.INCLUDE RQB.INC
.INCLUDE TSS.INC
.ALIGN DWORD

EXTRN ddVidOwner	DD

;The keyboard service is designed to return the complete status
;of the keyboard shift, control, alt, and lock keys as well as
;the key in the buffer that the state applies to.
;
;This is done by using two buffers. The first is a "RAW" key buffer
;that holds keys from the interrupt service routine which contains
;all raw keystrokes including shift keys going up and down.
;This information is needed to build coherent keystroke information
; for an application.  This buffer is 32 bytes long.
;
;The second buffer is 256 bytes and contains the translated information
;in the form of DWords (a 4 byte quantity times 64).  This DWord contains the
;following information:
;
;Low byte  (bits 0-7)   Partially translated application keystroke
;Next byte (bits 8-15)  Shift State (Ctrl, Shift, Alt)
;Next byte (bits 16-23) Lock State (CAPS, Num, Scroll)
;Hi Byte   (bits 24-31) Key Source (Bit 1 Set = Key From Numeric Pad)
;
KBDSvcName	DB 'KEYBOARD'

KbdMainExch	DD 0			;Used by the Kbd Process
KbdHoldExch DD 0			;Used for requestors that don't own the keyboard
KbdGlobExch DD 0			;Used to hold Global CTRL-ALT requests
KbdWaitExch DD 0			;Used for Kbd Wait-For-Key requests
KbdTempExch DD 0			;Used to rifle thru requests

dGlobalKey 	DD 0			;Global Key Found, else 0
dfPrefix	DD 0

KbdMsgBuf1L	DD 0			;Message buffers for Kbd Service
KbdMsgBuf1H	DD 0

KbdMsgBuf2L DD 0
KbdMsgBuf2H DD 0

KbdOwner		DD 1		;Current owner of Kbd (Mon is default)
KbdAbortJob		DD 0		;Job that is aborting
KbdCancelJob	DD 0		;Job that is cancelling global request

rgbKbdBuf   DB 20h DUP (0) ;32 byte RAW buffer
dKbdCnt     DD 0
pKbdIn	    DD OFFSET rgbKbdBuf		;ptr to next char going in
pKbdOut     DD OFFSET rgbKbdBuf		;ptr to next char going out
;
bKbdMode    DB 0		;ZERO for Cooked, 1 for RAW (testing)
fKBDInitDone DB 0			;Set true when KBD Service is up and running
;
KbdState    DB 0		;(See State Masks below)
KbdLock     DB 0		;(See Lock Masks below)
;
rgdKBBuf	DD 40h DUP (0)	;64 Dwords for translated key buffer
dKBCnt		DD 0
pKBIn		DD OFFSET rgdKBBuf	;ptr to codes comming in
pKBOut	    DD OFFSET rgdKBBuf	;ptr to codes going out
;
; These "masks" are for keyboard states that change with special keys:
; They are BIT OFFSETS and NOT MASKS for logical operations!!!!!

CtrlLeftBit    EQU 0
CtrlRiteBit    EQU 1
ShftLeftBit    EQU 2
ShftRiteBit    EQU 3
AltLeftBit     EQU 4
AltRiteBit     EQU 5
;
; Mask to tell if one of the 3 states exist (Ctrl, Shift, Alt)
CtrlDownMask	EQU 00000011b
ShftDownMask	EQU 00001100b
AltDownMask		EQU 00110000b
;
; BIT OFFSETS
CpLockBit      EQU 2
NmLockBit      EQU 1
ScLockBit      EQU 0
; MASKS
CpLockMask     DB 00000100b
NmLockMask     DB 00000010b
ScLockMask     DB 00000001b

;
; The following special keys are processed by the Keyboard Task and handled
; as follows:
; NUMLOCK  - Lights NumLock LED and processes keys accordingly
; SHIFT    - Sets shift flag and processes keys accordingly
; CTRL	   - Sets Ctrl flag
; CAPSLOCK - Lights CapsLock LED and processes keys accordingly
; ALT	   - Sets Alt flag.
; SCRLLOCK - Lights ScrollLock LED and flag
;
; This table is used to translate all active editing keys from
; the raw value provided by the hardware.
;
;								SHIFT
;								Value
KbdTable DB 0;   		00
DB	01Bh	 ;  Esc	 	01
DB	031h	 ;  1	 	02		21h  !
DB	032h	 ;  2	 	03		40h  @
DB	033h	 ;  3	 	04		23h  #
DB	034h	 ;  4	 	05		24h  $
DB	035h	 ;  5	 	06		25h  %
DB	036h	 ;  6	 	07		5Eh  ^
DB	037h	 ;  7	 	08		26h  &
DB	038h	 ;  8	 	09		2Ah  *
DB	039h	 ;  9	 	0A		28h  (
DB	030h	 ;  0	 	0B		29h  )
DB	02Dh	 ;  -	 	0C		5Fh  _
DB	03Dh	 ;  =	 	0D		2Bh  +
DB	008h	 ;  BkSpc	0E
DB	009h	 ;  TAB	 	0F
DB	071h	 ;  q	 	10		51h
DB	077h	 ;  w	 	11		57h
DB	065h	 ;  e	 	12		45h
DB	072h	 ;  r	 	13		52h
DB	074h	 ;  t	 	14		54h
DB	079h	 ;  y	 	15		59h
DB	075h	 ;  u	 	16		55h
DB	069h	 ;  i	 	17		49h
DB	06Fh	 ;  o	 	18		4Fh
DB	070h	 ;  p	 	19		50h
DB	05Bh	 ;  [	 	1A		7Bh
DB	05Dh	 ;  ]	 	1B		7Dh
DB	00Dh	 ;  CR	 	1C
DB	0h		 ;  LCtrl 	1D    Special handling
DB	061h	 ;  a	 	1E		41h
DB	073h	 ;  s	 	1F		53h
DB	064h	 ;  d	 	20		44h
DB	066h	 ;  f	 	21		46h
DB	067h	 ;  g	 	22		47h
DB	068h	 ;  h	 	23		48h
DB	06Ah	 ;  j	 	24		4Ah
DB	06Bh	 ;  k	 	25		4Bh
DB	06Ch	 ;  l (L) 	26		4Ch
DB	03Bh	 ;  ;	 	27		3Ah
DB	027h	 ;  '    	28		22h
DB	060h	 ;  `	 	29		7Eh
DB	0h		 ;  LfShf 	2A    Special handling
DB	05Ch	 ;  \ 	 	2B		7Ch
DB	07Ah	 ;  z	 	2C		5Ah
DB	078h	 ;  x	 	2D		58h
DB	063h	 ;  c	 	2E		43h
DB	076h	 ;  v	 	2F		56h
DB	062h	 ;  b	 	30		42h
DB	06Eh	 ;  n	 	31		4Eh
DB	06Dh	 ;  m	 	32		4Dh
DB	02Ch	 ;  ,	 	33		3Ch
DB	02Eh	 ;  .	 	34		3Eh
DB	02Fh	 ;  /	 	35		3Fh
DB	0h		 ;  RtShf 	36    Special handling
DB	02Ah	 ;  Num * 	37		Num pad
DB	0h		 ;  LAlt  	38    Special handling
DB	020h	 ;  Space 	39
DB	0h		 ;  CpsLk 	3A    Special handling
DB	00Fh	 ;  F1	 	3B
DB	010h	 ;  F2	 	3C
DB	011h	 ;  F3	 	3D
DB	012h	 ;  F4	 	3E
DB	013h	 ;  F5	 	3F
DB	014h	 ;  F6	 	40
DB	015h	 ;  F7	 	41
DB	016h	 ;  F8	 	42
DB	017h	 ;  F9	 	43
DB	018h	 ;  F10	 	44
DB	0h		 ;  NumLk 	45   Special handling
DB	0h		 ;  ScrLk 	46   Special handling
DB	086h	 ;  Num 7 	47		37h		Num Home
DB	081h	 ;  Num 8 	48		38h		Num Up
DB	085h	 ;  Num 9 	49		39h		Num Pg Up
DB	0ADh	 ;  Num - 	4A				Num Pad
DB	083h	 ;  Num 4 	4B		34h		Num Left
DB	09Fh	 ;  Num 5 	4C		35h		Num (Extra code)
DB	084h	 ;  Num 6 	4D		36h		Num Right
DB	0ABh	 ;  Num + 	4E				Num Pad
DB	08Bh	 ;  Num 1 	4F		31h		Num End
DB	082h	 ;  Num 2 	50		32h		Num Down
DB	08Ch	 ;  Num 3 	51		33h		Num Pg Dn
DB	08Eh	 ;  Num 0 	52		30h		Num Insert
DB	0FFh	 ;  Num . 	53		2Eh		Num Del
DB	01Ch	 ;  Pr Scr 	54  			SYS REQUEST
DB	000h	 ; 	     	55
DB	000h	 ;	     	56
DB	019h	 ;  F11	 	57
DB	01Ah	 ;  F12	 	58
DB	000h	 ;	 	 	59
DB	000h	 ;	 		5A
DB	000h	 ;	 	 	5B
DB	000h	 ;	 	 	5C
DB	000h	 ;	 	 	5D
DB	000h	 ;	 	 	5E
DB	000h	 ;	 	 	5F   ;The following chars are subs from table2
DB	00Eh	 ;  Ins	 	60	Cursor pad
DB	00Bh	 ;  End	 	61	Cursor pad
DB	002h	 ;  Down	62	Cursor pad
DB	00Ch	 ;  PgDn	63	Cursor pad
DB	003h	 ;  Left	64	Cursor pad
DB	000h	 ;	     	65
DB	004h	 ;  Right	66	Cursor pad
DB	006h	 ;  Home	67	Cursor pad
DB	001h	 ;  Up		68	Cursor pad
DB	005h	 ;  PgUp	69	Cursor pad
DB	07Fh	 ;  Delete  6A	Cursor pad
DB	0AFh	 ;  /	 	6B  Num Pad
DB	08Dh	 ;  ENTER	6C  Num Pad
DB	0h		 ;			6D
DB  0h	 	 ;	     	6E
DB  0h		 ;	 		6F
DB  0h	 	 ;	 		70
DB  0h		 ;	 		71
DB  0h		 ;	 		72
DB  0h		 ;	 		73
DB  0h		 ;	 		74
DB  0h		 ;	 		75
DB  0h		 ;	 		76
DB  0h		 ;	 		77
DB  0h		 ;	 		78
DB  0h		 ;	 		79
DB  0h		 ;	 		7A
DB  0h		 ;	 		7B
DB  0h		 ;	 		7C
DB  0h		 ;	 		7D
DB  0h		 ;	 		7E
DB  0h		 ;	 		7F
;
;This table does an initial character translation from the characters
;provided by the keyboard.  The Kbd translates incoming keystrokes
;from the original scan set 2 for the IBM PC.  All PCs are are set to this
;by default.  Keys on the 101 keyboard that were common to the numeric
;keypad use a two character escape sequence begining with E0 hex.
;If we see an E0 hex we scan this table and provide the translation
;to another unique character which is looked up in the primary
;table above.  This gives us unique single characters for every key.
;
nKbdTable2 EQU 16
;
KbdTable2 DB 052h,  060h    ;Insert
		  DB 04Fh,  061h    ;End
		  DB 050h,  062h    ;Down
		  DB 051h,  063h    ;Pg Down
		  DB 04Bh,  064h    ;Left
		  DB 04Dh,  066h    ;Rite
		  DB 047h,  067h    ;Home
		  DB 048h,  068h    ;Up
		  DB 049h,  069h    ;Pg Up
		  DB 053h,  06Ah    ;Delete

		  DB 037h,  06Bh    ;Num /
		  DB 01Ch,  06Ch    ;Num ENTER

		  DB 038h,  070h    ;Right ALT DOWN	    These are special cause we
		  DB 01Dh,  071h    ;Right Ctrl DOWN	track UP & DOWN!!!
		  DB 0B8h,  0F0h    ;Right ALT UP
		  DB 09Dh,  0F1h    ;Right Ctrl UP


;This table provides shift level values for codes from the primary KbdTable.
;In Shift-ON state, keycodes 21 - 7E hex are translated through this table.
;In CAPS LOCK state, codes 61h to 7Ah are translated through this table
;In NUM LOCK state, codes with High Bit set are translated
;

KbdTableS DB 0;	00
DB	38h		; 	01  Up   8  Numeric pad
DB	32h		; 	02  Dn   2  Numeric pad
DB	34h		; 	03  Left 4  Numeric pad
DB	36h		; 	04  Rite 6  Numeric pad
DB	39h		; 	05  PgUp 9  Numeric pad
DB	37h		; 	06  Home 7  Numeric pad
DB	07h		; 	07
DB	08h		; 	08
DB	09h		; 	09
DB	0Ah		; 	0A
DB	31h		; 	0B  End  1  Numeric Pad
DB	33h		; 	0C  PgDn 3  Numeric pad
DB	0Dh		; 	0D
DB	30h		; 	0E  Ins  0  Numeric pad
DB	0Fh		; 	0F
DB	10h		; 	10
DB	11h		; 	11
DB	12h		; 	12
DB	13h		; 	13
DB	14h		; 	14
DB	15h		; 	15
DB	16h		; 	16
DB	17h		; 	17
DB	18h		; 	18
DB	18h		; 	19
DB	1Ah		; 	1A
DB	1Bh		; 	1B
DB	1Ch		; 	1C
DB	1Dh		; 	1D
DB	1Eh		; 	1E
DB	35h		; 	1F	Blnk 5  Numeric pad
DB	20h		; 	20
DB	21h		; 	21
DB	22h		; 	22
DB	23h		; 	23
DB	24h		; 	24
DB	25h		; 	25
DB	26h		; 	26
DB	22h		; 	27  '  "
DB	28h		; 	28
DB	29h		; 	29
DB	2Ah		; 	2A
DB	2Bh		; 	2B
DB	3Ch		; 	2C  ,  <
DB	5Fh		; 	2D  -  _
DB	3Eh		; 	2E  .  >
DB	3Fh		; 	2F  /  ?
DB	29h		; 	30  0  )
DB	21h		; 	31  1  !
DB	40h	 	; 	32  2  @
DB	23h	 	; 	33  3  #
DB	24h		; 	34  4  $
DB	25h	 	; 	35  5  %
DB	5Eh	 	; 	36  6  ^
DB	26h	 	; 	37  7  &
DB	2Ah	 	; 	38  8  *
DB	28h	 	; 	39  9  (
DB	3Ah	 	; 	3A
DB	3Ah	 	; 	3B  ;  :
DB	3Ch	 	; 	3C
DB	2Bh	 	; 	3D  =  +
DB	3Eh	 	; 	3E
DB	3Fh	 	; 	3F
DB	40h	 	; 	40
DB	41h	 	; 	41
DB	42h	 	; 	42
DB	43h	 	; 	43
DB	44h	 	; 	44
DB	45h	 	; 	45
DB	46h	 	; 	46
DB	47h	 	; 	47
DB	48h	 	; 	48
DB	49h	 	; 	49
DB	4Ah	 	; 	4A
DB	4Bh	 	; 	4B
DB	4Ch	 	; 	4C
DB	4Dh	 	; 	4D
DB	4Eh	 	; 	4E
DB	4Fh	 	; 	4F
DB	50h	 	; 	50
DB	51h	 	; 	51
DB	52h	 	; 	52
DB	53h	 	; 	53
DB	54h	 	; 	54
DB	55h	 	; 	55
DB	56h	 	; 	56
DB	57h	 	; 	57
DB	58h	 	; 	58
DB	59h	 	; 	59
DB	5Ah	 	; 	5A
DB	7Bh	 	; 	5B  [  {
DB	7Ch	 	; 	5C  \  |
DB	7Dh	 	; 	5D  ]  }
DB	5Eh	 	; 	5E
DB	5Fh	 	; 	5F
DB	7Eh	 	; 	60  `  ~
DB	41h	 	; 	61  a  A
DB	42h	 	; 	62	b  B
DB	43h	 	; 	63	c  C
DB	44h	 	; 	64	d  D
DB	45h	 	; 	65	e  E
DB	46h	 	; 	66	f  F
DB	47h	 	; 	67	g  G
DB	48h	 	; 	68	h  H
DB	49h	 	; 	69	i  I
DB	4Ah	 	; 	6A	j  J
DB	4Bh	 	; 	6B	k  K
DB	4Ch	 	; 	6C	l  L
DB	4Dh	 	; 	6D	m  M
DB	4Eh	 	; 	6E	n  N
DB	4Fh	 	; 	6F	o  O
DB	50h	 	; 	70	p  P
DB	51h	 	; 	71	q  Q
DB	52h	 	; 	72	r  R
DB	53h	 	; 	73	s  S
DB	54h	 	; 	74	t  T
DB	55h	 	; 	75	u  U
DB	56h	 	; 	76	v  V
DB	57h	 	; 	77  w  W
DB	58h	 	; 	78  x  X
DB	59h	 	; 	79  y  Y
DB	5Ah	 	; 	7A  z  Z
DB	7Bh	 	; 	7B
DB	7Ch	 	; 	7C
DB	7Dh	 	; 	7D
DB	7Eh	 	; 	7E
DB	2Eh	 	; 	7F	Del  .  Numeric Pad


KbdSvcStack 	DD 255 DUP(0)	;1024 byte stack for KbsSvc
KbdSvcStackTop	DD 0

;=============================================================================
;The following equates are for the hardware handling code.
;8042 Status Byte, Port Hex 0064 Read
;=============================================================================

STATUSPORT		EQU 64h
COMMANDPORT		EQU 64h
DATAPORT		EQU 60h

PARITYERROR		EQU 10000000b
GENERALTIMEOUT	EQU 01000000b
AUXOUTBUFFFULL	EQU 00100000b
INHIBITSWITCH	EQU 00010000b
COMMANDDATA		EQU 00001000b
SYSTEMFLAG		EQU 00000100b
INPUTBUFFFULL	EQU 00000010b
OUTPUTBUFFFULL	EQU 00000001b


;==================Begin Keyboard Code ===================

.CODE
;
;
;ISR for the keyboard.  This is vectored to by the processor whenever
;INT 21 fires off.  This puts the single byte from the 8042
;KBD processor into the buffer.  Short and sweet the way all ISRs
;should be... (most are not this easy though).  This also sends
;a message to the KBD Task (using ISend) when the buffer is almost
;full so it will be forced to process some of the raw keys even
;if no keyboard requests are waiting.

PUBLIC IntKeyBrd:				 	;Key Board (KB) INT 21
	    PUSHAD						;Save all registers
	    MOV ESI, pKbdIn				;Set up pointer
	    XOR EAX,EAX
	    IN  AL, 60h	  				;Read byte
	    MOV EBX, dKbdCnt			;See if buffer full
	    CMP EBX, 20h				;Buffer size
	    JE KbdEnd 	     			;Buffer is full - Don't save it
	    MOV BYTE PTR [ESI], AL 			;Move into buf
	    INC dKbdCnt						;One more in the buf
	    INC ESI							;Next byte in
	    CMP ESI, OFFSET rgbKbdBuf+20h	;past end yet?
	    JB KbdEnd
	    MOV ESI, OFFSET rgbKbdBuf	;Back to beginning of buffer
KbdEnd:
	    MOV pKbdIn, ESI				;Set up pointer for next time

		CMP BYTE PTR fKBDInitDone, 0  ;Service isn't ready for messages yet
		JE KbdExit

		;ISend a msg to Keyboard task
		MOV EBX, KbdMainExch		;Yes - ISend Message to KbdTask
		PUSH EBX                    ;exchange to send to
		PUSH 0FFFFFFFFh             ;bogus msg
		PUSH 0FFFFFFFFh             ;bogus msg
		CALL FWORD PTR _ISendMsg		;tell him to come and get it...
KbdExit:
		PUSH 1
		CALL FWORD PTR _EndOfIRQ
	    POPAD
	    IRETD

;===============================================
;This gets one byte from the Kbd Buffer and returns it in AL
;Zero is returned if no key exists.
;
ReadKBDBuf:
		CLI
	    MOV ESI, pKbdOut		;Get ptr to next char to come out
	    MOV EAX, dKbdCnt		;See if there are any bytes
	    CMP EAX, 0
	    JE RdKBDone 			;No - Leave 0 in EAX
	    DEC dKbdCnt				;Yes - make cnt right
	    XOR EAX,EAX
	    MOV AL, BYTE PTR [ESI]			;Put byte in AL
	    INC ESI
	    CMP ESI, OFFSET rgbKbdBuf+20h	;past end yet?
	    JB RdKBDone
	    MOV ESI, OFFSET rgbKbdBuf	;Back to beginning of buffer
RdKBDone:
	    MOV pKbdOut, ESI			;Save ptr to next char to come out
	    STI
		RETN
;
;========================================================
;
; Reads and processes all bytes from the RAW keyboard buffer
; and places them and their proper state bytes and into the next DWord
; in the translated buffer if it is an edit key.

XLateRawKBD:
	    CALL ReadKbdBuf
	    CMP EAX, 0
	    JE XLateDone			;No

	    MOV BL, bKbdMode		;See if we are RAW... (for testing ONLY)
	    CMP BL, 1
	    JNE KB001				;NO - keep going
	    JMP KB029A				;Yes, leave the key in AL for buffer

		;Now we check to see if the byte is 0Eh which tells us
		;this is a two key code that needs to be translated from
		;our special table before processing.  This turns the
		;two key code into a single code and sets a state
		;bit to indicate this.

KB001:
		CMP dfPrefix, 1
		JE KB003
	    CMP AL, 0E0h			;Key PREFIX???
		JNE KB006				;No
        MOV dfPrefix, 1			;Yes, We got an E0
	    JMP XLateDone			;

KB003:
        MOV dfPrefix, 0			;No prefix
	    MOV ESI, OFFSET KbdTable2
	    MOV ECX, nKbdTable2
KB004:
	    CMP BYTE PTR [ESI], AL
	    JE	KB005
	    INC ESI					;Two byte further into table 2
	    INC ESI
	    DEC ECX
	    JNZ KB004				;Go to next table entry
	    JMP XLateDone			;No translation - ignore it

KB005:
	    INC ESI					;One byte further over to get Xlate byte
	    XOR EAX,EAX
	    MOV AL, [ESI]			;Fall thru to check on char...

      ;This next section checks for special keys (shift, alt, etc.)
      ;BL has SHIFT state, CL has LOCK State, AL has byte from buffer.

KB006:
        MOV dfPrefix, 0			;No prefix
	    XOR EBX, EBX
	    XOR ECX, ECX
	    MOV BL, KbdState		;BL has Shift, Alt, Ctrl states
	    MOV CL, KbdLock			;BH has Num, Caps, & Scroll Lock

	    CMP AL, 45h 			;Key = NumLock ?
	    JNE KB007				;NO...
	    BTC ECX, NmLockBit		;Compliment bit
	    JMP KB022
KB007:
	    CMP AL, 3Ah 			;Caps Lock?
	    JNE KB008
	    BTC ECX, CpLockBit		;Compliment bit in BH
	    JMP KB022
KB008:
	    CMP AL, 46h 			;Scroll Lock?
	    JNE KB009
	    BTC ECX, ScLockBit		;Compliment bit in BH
	    JMP KB022
KB009:
	    CMP AL, 2Ah 			;Char Left Shift On?
	    JNE KB010
	    BTS EBX, ShftLeftBit
	    JMP KB021
KB010:
	    CMP AL, 36h 			;Right Shift On?
	    JNE KB011
	    BTS EBX, ShftRiteBit
	    JMP KB021
KB011:
	    CMP AL, 0AAh			;Left Shift Off?
	    JNE KB012
	    BTR EBX, ShftLeftBit
	    JMP KB021

KB012:
	    CMP AL, 0B6h			;Right Shift Off?
	    JNE KB013
	    BTR EBX, ShftRiteBit
	    JMP KB021

KB013:
	    CMP AL, 1Dh 			;Left Ctrl On?
	    JNE KB014
	    BTS EBX, CtrlLeftBit
	    JMP KB021

KB014:
	    CMP AL, 71h 			;Right Ctrl On?
	    JNE KB015
	    BTS EBX, CtrlRiteBit
	    JMP KB021

KB015:
	    CMP AL, 09Dh			;Left Ctrl Off?
	    JNE KB016
	    BTR EBX, CtrlLeftBit
	    JMP KB021

KB016:
	    CMP AL, 0F1h			;Right Ctrl Off?
	    JNE KB017
	    BTR EBX, CtrlRiteBit
	    JMP KB021
KB017:
	    CMP AL, 38h 			;Left Alt On?
	    JNE KB018
	    BTS EBX, AltLeftBit
	    JMP KB021
KB018:
	    CMP AL, 70h 			;Right Alt On?
	    JNE KB019
	    BTS EBX, AltRiteBit
	    JMP KB021
KB019:
	    CMP AL, 0B8h			;Left Alt Off?
	    JNE KB020
	    BTR EBX, AltLeftBit
	    JMP KB021
KB020:
	    CMP AL, 0F0h			;Right Alt Off?
	    JNE KB023
	    BTR EBX, AltRiteBit
KB021:
	    MOV KbdState, BL		;Put Kbd Shift State back
	    JMP XLateDone 			;
KB022:
	    MOV KbdLock, CL			;Put Kbd Lock State back
	    CALL SetKbdLEDS			;Set LEDs on keyboard
	    JMP XLateRawKBD			;

		;We jumped here if it wasn't a key that is specially handled

KB023:
	    TEST AL,  80h			;Check for high bit (key-up code)
	    JNZ	 XLateDone 			;Go back, else fall through

	    OR AL,AL				;Zero not a valid code
	    JZ XLateDone 			;Go back, else fall through

       ;If we got here, IT'S AN EDIT KEY DOWN!
       ;Now we lookup the code and do a single translation.

	    AND EAX, 07Fh				;Chop off any upper bit junk
	    MOV ESI, OFFSET KbdTable	;Set up to index table
	    MOV DL, BYTE PTR [ESI+EAX]	;Save in DL
	    OR AL,AL					;Zero not a valid code
	    JZ XLateDone 				;Go back, else fall through

	    MOV CL, KbdState			;Get Shift state
	    MOV CH, KbdLock				;Get lock state

		;TO let the user know if the key came from the Numeric
		;keypad we set the high bits in the first translation
		;table for these keys.  This next piece of code tests for it
		;and sets the low bit in DH if it its. DH is later moved
		;into the high byte of the returned key code.

		MOV DH, 0
		TEST DL, 80h				;High bit set?
		JZ SHORT KB024
		MOV DH, 1					;Indicates key came numeric pad

		;See if shift key is down and shift all keys it is is
KB024:
		TEST CL, ShftDownMask		;Either shift key down?
		JZ KB025					;No, go look for locks
		CMP DL, 21h					;Is key < ' '
		JB KB025					;Yes, look for locks
		CMP DL, 7Eh					;Is key > '~'
		JA KB025					;Yes, look for locks
		JMP SHORT KB027				;In-range, go do the translation

KB025:  ;See if key is from Numerc Keypad (high bit will be set)

		TEST DL, 80h				;High bit set?
		JZ KB026					;No
		AND CH, NmLockMask			;Yes, is NumLock ON
		JZ KB026					;No
		CMP DL, 0ADh				;Yes, but don't shift DASH (-) Special Case
		JE KB026
		JMP SHORT KB027				;Do the shift Xlation

KB026:	;See if Caps Lock is on and if key is between 61h and 7Ah
		;do the translation

		TEST CH, CpLockMask			;Is CpLock ON
		JZ KB029					;No
		CMP DL, 61h					;Is key >= 'a'
		JB KB029					;No
		CMP DL, 7Ah					;Is key <= 'z'
		JA KB029					;No
		;Fall through to do the translation

KB027:	;Do the shift translation and leave in DL
		MOV	AL, DL					;Put in AL
		AND EAX, 07Fh				;Chop all above 7 bits
	    MOV ESI, OFFSET KbdTableS	;Set up to index table
	    MOV DL, BYTE PTR [ESI+EAX]	;Save in DL
		;Fall though to put key in final buffer

       ;Place DL in the LOW byte of the DWord to go into the
       ;final buffer (the data the user will get)
       ;If the high bit is set coming from the primary
       ;translation table, this means the key was from the
       ;numeric keypad so we set the numpad bit in status
       ;which should already be in DH
KB029:
		MOV AH, DH
		SHL EAX, 8				;Num Pad indicator
	    MOV AL, KbdState		;Get Shift state
	    MOV AH, KbdLock			;Get lock state
	    SHL EAX, 8
		AND DL, 7Fh				;Lop of high bit (if there)
	    MOV AL, DL

		;EAX now has the buffered info for the user (Key, Shifts & Locks)
		;Now we put it in the DWord buffer if it is NOT a GLOBAL.
		;If global, we put it in dGlobalKey.

		TEST AH, CtrlDownMask		;Either Ctrl Down?
		JZ KB029A					;No
		TEST AH, AltDownMask		;Either Alt Down?
		JZ KB029A					;No

		;It IS a global key request!
		MOV dGlobalKey, EAX			;Save it
	    JMP  XLateRawKBD			;Back for more (if there is any)

KB029A:
	    MOV  EBX, dKBCnt			;See if buffer full
	    CMP  EBX, 64				;number of DWords in final buffer
	    JE   XLateDone 				;Buffer is FULL..
	    MOV  ESI, pKBIn				;Get ptr to next IN to final buffer
	    MOV  [ESI], EAX				;Move into buf
	    INC  dKBCnt					;One more DWord in the buf
		ADD  ESI, 4
	    CMP  ESI, OFFSET rgdKBBuf+100h ;40h * 4
	    JB   KB030
	    MOV  ESI, OFFSET rgdKBBuf	;Reset to buf beginning
KB030:
	    MOV  pKBIn, ESI			;Save ptr to next in
	    JMP  XLateRawKBD
XlateDone:
		XOR EAX, EAX
	    RETN

;========================================================
;
; Returns a keyboard code from FINAL keyboard buffer.
; Returns zero in EAX if buffer is empty.
;
; IN :  Nothing
; OUT:  EAX has Key or 0 if none
; USED: EAX, ESI
; MODIFIES: dKBCnt, pKBOut
;
ReadKBFinal:
		MOV EAX, dKBCnt
		CMP EAX, 0
		JE KBFDone					;Nothing final buffer
	    DEC dKBCnt					;One more DWord in the buf
		MOV ESI, pKBOut				;ptr to next code out
		MOV EAX, [ESI]				;Put it in EAX
		ADD ESI, 4					;Next code please...
	    CMP ESI, OFFSET rgdKBBuf+100h	;Past end of buff?
	    JB  KBF02					;No
	    MOV	ESI, OFFSET rgdKBBuf	;Yes, Reset to beginning
KBF02:
	    MOV pKBOut, ESI				;Update pKBOut
KBFDone:
	    RETN

;========================================================
;
; This is the keyboard Service task. It is an infinite loop
; that services requests from users of the keyboard.  It
; calls XLateRawKBD to process raw keyboard buffer data,
; and waits at the KeyBoard Service Main Exchange for users.
; When it gets a request it checks the service code and handles
; it accordingly.

KBDServiceTask:
		CALL XLateRawKBD			;Processes RAW buffer if not empty

		CMP DWORD PTR dGlobalKey, 0
		JE  KST01					;No global key came in
KBDGlobal1:
		PUSH KbdGlobExch
		PUSH OFFSET KbdMsgBuf1L		;Where to return pRqBlk
		CALL FWORD PTR _CheckMsg	;Check to see if RqWaiting
		OR EAX, EAX					;Yes if ZERO
		JZ KBDGlobal2				;Rq waiting for global
		MOV DWORD PTR dGlobalKey, 0	;Wipe out global key (no one wants it)
		JMP KBDServiceTask			;Start over again

KBDGlobal2:
		MOV EBX, KbdMsgBuf1L		;pRqBlk into EBX
		MOV ESI, [EBX+pData1]		;Ptr where to return key
		OR ESI, ESI					;Is it null?? (Bad news if so)
		JNZ KBDGlobal3				;No, probably good ptr
		PUSH EBX					;Yes, BAD PTR. Push pRqBlk
		PUSH ErcNullPtr				;Push error
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top
KBDGlobal3:
		MOV EDX, dGlobalKey
		MOV [ESI], EDX				;Give em the key!
		PUSH EBX					;Push pRqBlk
		PUSH 0						;Push NO ERROR
		CALL FWORD PTR _Respond
		JMP KBDGlobal1				;Go back to see if others want it

KST01:
		CMP DWORD PTR ddVidOwner, 2	;Debugger has video
		JNE	KST01ND					;NOT in Debugger
		PUSH 20
		CALL FWORD PTR _Sleep		;
		CALL XLateRawKBD			;Processes RAW buffer
		JMP KBDServiceTask			;Go back to the top

KST01ND:
		PUSH KbdMainExch			;See if someones "Requesting"
		PUSH OFFSET KbdMsgBuf1L		;
		CALL FWORD PTR _WaitMsg		;Wait for the message

		;If we got here, we have a Request or a Msg the from ISR

		CMP DWORD PTR KbdMsgBuf1L, 0FFFFFFFFh	;Is it a msg from the KBD ISR?
		JNE KST02					;No, jump to handle Request

		;If we got here, ISR sent msg to us (something in the buffer)

		CALL XLateRawKBD			;Processes RAW buffer

		CMP DWORD PTR dGlobalKey, 0
		JNE KBDGlobal1				;A global key came in

		PUSH KbdWaitExch			;See if owner is waiting for a key
		PUSH OFFSET KbdMsgBuf1L		;Where to return Request or msg
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX
		JNZ KBDServiceTask			;No Rq/Msg waiting, Go back to the top,
									;or fall thru to check the request

KST02:
		;If we got here we've got a Request from Main or Wait Exch
		MOV EBX, KbdMsgBuf1L		;pRqBlk into EBX
		MOV CX, [EBX+ServiceCode]	;Save in CX

		CMP CX, 0					;Job Abort Notify
		JE KSTAbort					;
		CMP CX, 1					;ReadKbd
		JE KSTRead					;
		CMP CX, 2					;ReadKbdGlobal
		JE KSTReadGlobal			;
		CMP CX, 3					;CancelGlobal
		JE KSTCancelGlobal			;
		CMP CX, 4					;AssignKBD
		JE KSTAssignKbd				;
		PUSH EBX					;HOMEY DON'T SERVICE THAT!
		PUSH ErcBadSvcCode			;Bad service code
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top

;-------------------------------------------------------
KSTRead:
		MOV EAX, [EBX+RqOwnerJob]	;Whose Request is it?
		CMP EAX, KbdOwner
		JE KSTRead00				;This guy owns it!
		PUSH EBX					;Not the owner, so send to Hold Exch
		PUSH KbdHoldExch			;
		CALL FWORD PTR _MoveRequest
		JMP KBDServiceTask			;Go back to the top
KSTRead00:
		CALL ReadKBFinal			;Get Code from Buf (Uses EAX, ESI)
		CMP EAX, 0					;No Key in Final Buffer
		JE KSTRead02				;Go see if they asked to wait
		MOV ESI, [EBX+pData1]		;Ptr where to return key
		CMP ESI, 0					;Is it null?? (Bad news if so)
		JNE KSTRead01				;No, probably good ptr
		PUSH EBX					;Yes, BAD PTR. Push pRqBlk
		PUSH ErcNullPtr				;Push error
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top
KSTRead01:
		MOV [ESI], EAX				;Give them the key code
		PUSH EBX					;RqHandle
		PUSH 0						;NO Error
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top

KSTRead02:
		CMP DWORD PTR [EBX+dData0], 0	;Wait for key?  0 in dData0 = Don't wait
		JNE KSTRead04				;Yes
		PUSH EBX
		PUSH ErcNoKeyAvail			;Error Code (No key to give you)
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top

KSTRead04:
		PUSH EBX					;They opted to wait for a key
		PUSH KbdWaitExch			;Send em to the wait exch
		CALL FWORD PTR _MoveRequest
		JMP KBDServiceTask			;Go back to the top
;--------------------------------------------------------

KSTAbort:
		;Respond to all requests we are holding for Job in dData0
		;with Erc with ErcOwnerAbort. Then respond to Abort
		;request last. Requests can be at the HoldExch, the WaitExch,
		;or the GlobalKeyExch. We must chack all 3!
		;Save abort job for comparison

		MOV EAX, [EBX+dData0]		;Get aborting job number
		MOV KbdAbortJob, EAX        ;this is aborting job

KSTAbort10:							;Check the WaitExch
		PUSH KbdWaitExch			;See if he was "waiting" for a key
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX					;Yes (someone's waiting) if ZERO
		JNZ	KSTAbort20				;No more waiters
		MOV EDX, KbdMsgBuf2L		;pRq of holding job into EDX
		MOV	EBX, [EDX+RqOwnerJob]
		CMP	EBX, KbdAbortJob
		JE KSTAbort11				;Go to respond with Erc
		PUSH EDX					;Else move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated
		CALL FWORD PTR _MoveRequest
		JMP KSTAbort10				;Go back to look for more waiters
KSTAbort11:							;
		PUSH EDX					;Respond to this request
		PUSH ErcOwnerAbort			;cause he's dead
		CALL FWORD PTR _Respond
		JMP SHORT KSTAbort10

KSTAbort20:							;Check HoldExch for dead job
		PUSH KbdHoldExch			;See if anyone is on hold
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX					;Yes (someones holding) if ZERO
		JNZ	KSTAbort30				;No more holders
		MOV EDX, KbdMsgBuf2L		;pRq of holding job into EDX
		MOV	EBX, [EDX+RqOwnerJob]
		CMP	EBX, KbdAbortJob
		JE KSTAbort21				;Go to respond with Erc
		PUSH EDX					;Else move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated. It's not him.
		CALL FWORD PTR _MoveRequest
		JMP KSTAbort20				;Go back to look for more holders
KSTAbort21:							;
		PUSH EDX					;Respond to this request
		PUSH ErcOwnerAbort			;cause he's dead
		CALL FWORD PTR _Respond
		JMP SHORT KSTAbort20		;Go back to look for more holders

KSTAbort30:							;Check GlobalExch for dead job
		PUSH KbdGlobExch			;See if anyone is at global
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX					;Yes (someones holding) if ZERO
		JNZ	KSTAbort40				;No more holders
		MOV EDX, KbdMsgBuf2L		;pRq of holding job into EDX
		MOV	EBX, [EDX+RqOwnerJob]
		CMP	EBX, KbdAbortJob
		JE KSTAbort31				;Go to respond with Erc
		PUSH EDX					;Else move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated
		CALL FWORD PTR _MoveRequest
		JMP KSTAbort30				;Go back to look for more globals
KSTAbort31:							;
		PUSH EDX					;Respond to this request
		PUSH ErcOwnerAbort			;cause he's dead
		CALL FWORD PTR _Respond
		JMP SHORT KSTAbort30

KSTAbort40:				 			;Respond to original abort Req
		MOV EBX, KbdMsgBuf1L		;pRqBlk of original Abort Request
		PUSH EBX					;
		PUSH 0						;Error Code (OK)
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top
;----------------------------------------------------------

KSTReadGlobal:
		PUSH EBX					;They want a global key
		PUSH KbdGlobExch			;Send em to the Global exch
		CALL FWORD PTR _MoveRequest
		JMP KBDServiceTask			;Go back to the top
;-----------------------------------------------------------
;Assign a new owner for the keyboard. We must check to see
;if the new owner had any keyboard requests on hold.
;We do this by sending all the requests that were on hold
;back to the main exchange to be reevaluated.
;Then we respond to the original request.

KSTAssignKBD:
		;Change owner of Kbd
		MOV EAX, [EBX+dData0]		;Get new owner
		CMP EAX, KbdOwner
		JNE KSTAssign01				;New Owner!

		PUSH EBX					;Same owner
		PUSH 0						;Error Code (OK)
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top
KSTAssign01:
		MOV KbdOwner, EAX			;Set new owner

KSTAssign02:						;Move all waiters to main exch
		PUSH KbdWaitExch			;See if anyone is "waiting" for a key
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX					;Yes (someones waiting) if ZERO
		JNZ	KSTAssign03				;No more waiters
		MOV EDX, KbdMsgBuf2L		;pRq into EDX
		PUSH EDX					;Move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated
		CALL FWORD PTR _MoveRequest
		JMP KSTAssign02				;Go back to look for more waiters
KSTAssign03:						;Waiter have been moved, Respond to Req
		PUSH KbdHoldExch			;See if anyone is on hold
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg		;
		OR EAX, EAX					;Yes if ZERO
		JNZ	KSTAssign04				;No more holders
		MOV EDX, KbdMsgBuf2L		;pRq into EDX
		PUSH EDX					;Move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated
		CALL FWORD PTR _MoveRequest
		JMP KSTAssign03				;Go back to look for more holders
KSTAssign04:						;Holders have been moved, Respond to Req
		MOV EBX, KbdMsgBuf1L		;pRqBlk of original Assign Request
		PUSH EBX					;
		PUSH 0						;Error Code (OK)
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top
;-------------------------------------------------------

KSTCancelGlobal:
		;Rifle thru Global Exch and respond with ErcNoKeyAvail
		;to those with the same JobNum as dData0
		MOV EAX, [EBX+dData0]	;Save Job that is cancelling global request
		MOV KbdCancelJob, EAX	;

KSTCancel10:						;Check GlobalExch for canceled job
		PUSH KbdGlobExch			;See if anyone is at global
		PUSH OFFSET KbdMsgBuf2L		;Where to return Request
		CALL FWORD PTR _CheckMsg	;
		OR EAX, EAX					;Yes (someones holding) if ZERO
		JNZ	KSTCancel20				;No more globals
		MOV EDX, KbdMsgBuf2L		;pRq of holding job into EDX
		MOV	EBX, [EDX+RqOwnerJob]
		CMP	EBX, KbdCancelJob
		JE KSTCancel11				;Go to respond with Erc
		PUSH EDX					;Else move Request to MainKbd Exch
		PUSH KbdMainExch			; to be reevaluated
		CALL FWORD PTR _MoveRequest
		JMP KSTCancel10				;Go back to look for more globals
KSTCancel11:							;
		PUSH EDX					;Respond to this request
		PUSH ErcNoKeyAvail			;cause he cancelled it!
		CALL FWORD PTR _Respond
		JMP SHORT KSTCancel10		;Back to check for more

KSTCancel20:				 		;Respond to original cancel Req
		MOV EBX, KbdMsgBuf1L		;pRqBlk of original Request
		PUSH EBX					;
		PUSH 0						;Error Code (OK)
		CALL FWORD PTR _Respond
		JMP KBDServiceTask			;Go back to the top

;=============================================================
;PUBLIC blocking call to read the keyboard. This uses the
;Default TSS exchange and the stack to make the request to
;the keyboard service for the caller.  The request is a standard
;service code one (Wait On Key) request.
;If fWait is NON-ZERO, this will not return without a key unless
;a kernel/fatal error occurs.
;
;The call is fully reentrant (it has to be...).
;
; Procedural interface:
;
;	ReadKbd(pKeyCodeRet, fWait): dError
;
;   pKeyCodeRet is a pointer to a DWORD where the keycode is returned.
;     [EBP+16]
;	fWait is NON-ZERO to wait for a key.
;	  [EBP+12]
;
; Stack Variables:
;   Hndl    [EBP-4]
;
PUBLIC __ReadKBD:
		PUSH EBP					; Save the Previous FramePtr
		MOV EBP,ESP					; Set up New FramePtr
		SUB ESP, 4					; One DWORD local var

		MOV EAX, OFFSET KBDSvcName	;'KEYBOARD '
		PUSH EAX

		PUSH 1						;Service Code (Read Keyboard)

		MOV ECX,pRunTSS             ;Get TSS_Exch for our use
		MOV EBX,[ECX+TSS_Exch]      ;Exchange (TSS Exch)
		PUSH EBX                    ;

		LEA EAX, [EBP-4]			;Rq Handle (Local Var)
		PUSH EAX

		PUSH 0						;npSend
		MOV EAX, [EBP+16]			;Key Code return (Their Ptr)
		PUSH EAX					;pData1
		PUSH 4						;Size of key code
		PUSH 0						;pData2
		PUSH 0						;cbData2

		XOR EAX, EAX
		CMP DWORD PTR [EBP+12], 0	;Don't wait for Key?
		JE ReadKbd1					;No wait
		MOV EAX, 1					;Set up to wait!
ReadKbd1:
		PUSH EAX					;Wait value (dData0)

		PUSH 0
		PUSH 0

		CALL FWORD PTR _Request		;make the Request

		;The request is made. Now we call Wait!

		MOV ECX,pRunTSS             ;Get TSS_Exch for our use
		MOV EBX,[ECX+TSS_Exch]      ;
		PUSH EBX                    ;Pass exchange (for WaitMsg)
		ADD ECX,TSS_Msg             ;Offset of TSS msg area
		PUSH ECX
		CALL FWORD PTR _WaitMsg		;Wait on it

		;When we get here the caller should have the key code
		;HOWEVER, we want to pass any errors back via EAX

		OR EAX, EAX					;Was there a kernel error?
		JNZ ReadKbdEnd				;YES.... bummer
		MOV ECX,pRunTSS             ;Get TSS_Msg area so we can get error
		ADD ECX,TSS_Msg             ;Offset of TSS msg area
		MOV EBX, [ECX]				;pRqBlk
		MOV EAX, [ECX+4]			;Service error in second DWord
ReadKbdEnd:
		MOV ESP,EBP				;
		POP EBP					;
		RETF 8					; Rtn to Caller & Remove Params from stack

;=============================================================
;Special Call for Debugger so it doesn't have to pass thru
;the kernel Request mechanism for a keystroke.
;It acts like ReadKbd with fWait set to true.
;It sucks keys directly from the Final Keyboard buffer.
;
; Procedural interface:
;
;	ReadDbgKbd(pKeyCodeRet)
;
;   pKeyCodeRet is a pointer to a DWORD where the keycode is returned.
;     [EBP+8]
;
PUBLIC ReadDbgKBD:
		PUSH EBP					; Save the Previous FramePtr
		MOV EBP,ESP					; Set up New FramePtr
RDKB0:
		CALL ReadKBFinal			;Get Code from Buf (Uses EAX, ESI)
		OR EAX, EAX					;Got a key?? (non zero)
		JNZ RDKB1					;No. Loop back again
		PUSH 2						;Sleep for 20 ms
		CALL FWORD PTR _Sleep
		JMP RDKB0					;Check again
RDKB1:
		MOV ESI, [EBP+8]			;Ptr where to return key
		MOV [ESI], EAX
		XOR EAX, EAX
		MOV ESP,EBP				;
		POP EBP					;
		RETN 4					; Rtn to Caller & Remove Params from stack

;=================================================
;This sets the Keyboard Scan Set to #2 with 8042 interpretation ON
;
PUBLIC InitKBD:

		PUSH 1					;KBD IRQ
		CALL FWORD PTR _MaskIRQ

		CALL InBuffEmpty    ;Wait for Input Buffer to Empty
		MOV AL,0FAh	     	;Set ALL keys typematic/make/break
		OUT DataPort,AL     ;Send Command to KBD (not 8042)

		CALL OutBuffFull	;Eat response
		IN AL, DATAPORT

		CALL InBuffEmpty    ;Wait for Input Buffer to Empty
		MOV AL,0F0h	     	;Set Scan code set
		OUT DataPort,AL     ;Send Command to KBD (not 8042)

		CALL OutBuffFull	;Eat response
		IN AL, DATAPORT


		CALL InBuffEmpty    ;Wait for Input Buffer to Empty
		MOV AL,02h	     	;Scan set 2
		OUT DataPort,AL     ;Send Command

		CALL OutBuffFull	;Eat response
		IN AL, DATAPORT

		CALL InBuffEmpty    ;Wait for Input Buffer to Empty
		MOV AL,060h	     	;Set up to write 8042 command byte
		OUT COMMANDPORT,AL  ;Send Command
		CALL InBuffEmpty    ;Wait for Input Buffer to Empty
		MOV AL,45h	     	;Enable IBM Xlate
		OUT DataPort,AL     ;Send Command

		PUSH 1				;KBD IRQ
		CALL FWORD PTR _UnMaskIRQ

		CALL SetKbdLEDs

		RETN

;=============================================================================
;This creates the Keyboard Task and Service.
;
PUBLIC _InitKBDService:

		;All initial requests and messages from the ISR come to
		;this exchange

		MOV EAX, OFFSET KbdMainExch	;Alloc Main Kbd exch for service
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		;TempExch for testing

		MOV EAX, OFFSET KbdTempExch	;Alloc Hold Kbd exch for Kbd service
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		;Requests for ReadkeyBoard (ScvCode #1) that are from job
		;that currently owns the keyboard waits here if it wants
		;to wait for a key.

		MOV EAX, OFFSET KbdWaitExch	;Alloc Hold Kbd exch for Kbd service
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		;Requests for ReadkeyBoard (ScvCode #1) that are from jobs
		;that do NOT currently own the keyboard get sent here using
		;MoveRequest.

		MOV EAX, OFFSET KbdHoldExch	;Alloc Hold Kbd exch for Kbd service
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		;Requests for ReadkeyGlobal (SvcCode #3) wait here until we
		;get a global key from the keyboard.
		;
		MOV EAX, OFFSET KbdGlobExch	;Alloc Global Wait exch for Kbd service
		PUSH EAX
		CALL FWORD PTR _AllocExch
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		;Spawn the Keyboard Service task

		MOV EAX, OFFSET KBDServiceTask
		PUSH EAX
		PUSH 7						;Priority
		PUSH 0						;fDebug
		MOV EAX, OFFSET KbdSvcStackTop
		PUSH EAX
		PUSH 1						;OS Job task
		CALL FWORD PTR _SpawnTask
		OR EAX, EAX					;Check for error on AllocExch
		JNZ InitKBDSvcEnd			;YUP, we got bad problems

		MOV EAX, OFFSET KBDSvcName
		PUSH EAX
		PUSH KbdMainExch
		CALL FWORD PTR _RegisterSvc
InitKBDSvcEnd:
		MOV BYTE PTR fKBDInitDone, 1	;We're UP!
		RETN
;
;=============================================================================
;This tells the 8042 Controller to Disable the Keyboard device.
;=============================================================================

KbdDisable:
		PUSH EAX
		CALL InBuffEmpty	;Wait for Input Buffer to Empty
		MOV AL,0ADh		;Set Command to "Write the 8042 Command Byte"
		OUT COMMANDPORT,AL	;Send Command
		CALL InBuffEmpty	;Wait for Input Buffer to Empty
		POP EAX
		RETN

;=============================================================================
; This tells the 8042 Controller to Enable the Keyboard Device.
;=============================================================================

KbdEnable:
		CALL InBuffEmpty	;Wait for Input Buffer to Empty
		MOV AL,0AEh			;Set Command to "Write the 8042 Command Byte"
		OUT COMMANDPORT,AL	;Send Command
		CALL InBuffEmpty	;Wait for Input Buffer to Empty
		RETN
;
;=============================================================================
; Waits until the 8042 Input Buffer is EMPTY
;=============================================================================

InBuffEmpty:
		PUSH EAX
		PUSH ECX
		MOV ECX,2FFFFh 			;check 128k times
IBE:
		JMP IBE1
IBE1:
		JMP IBE2
IBE2:
		IN AL,STATUSPORT		;Read Status Byte into AL
		TEST AL,INPUTBUFFFULL	;Test The Input Buffer Full Bit
		LOOPNZ IBE
		POP ECX
		POP EAX
		RETN

;=============================================================================
; Waits until the 8042 Output Buffer is FULL so we can read it
;=============================================================================
;
; Before calling this makes sure that the Keyboard interrupts have been
; masked so the keyboard interrupt doesn't eat the byte you're
; looking for!!
;
OutBuffFull:
		PUSH EAX
		PUSH ECX
		MOV ECX,2FFFFh
OBF:
		JMP OBF1:
OBF1:
		JMP OBF2:
OBF2:
		IN AL,STATUSPORT	;Read Status Byte into AL
		TEST AL,OUTPUTBUFFFULL	;Test The Output Buffer Full Bit
		LOOPZ OBF
		POP ECX
		POP EAX
		RETN

;=============================================================================
; This sets the indicators on the keyboard based on data in KbdState
;=============================================================================

SetKbdLEDs:
		PUSH EAX

		PUSH 1					;KBD IRQ
		CALL FWORD PTR _MaskIRQ

		CALL InBuffEmpty		;Wait for Input Buffer to Empty
		MOV AL,0EDh				;Set/Reset Status Indicators
		OUT DATAPORT,AL 		;Send KBD Command

		CALL OutBuffFull		;Eat response
		IN AL, DATAPORT

		CALL InBuffEmpty		;Wait for Input Buffer to Empty
		MOV AL,KbdLock			;Get Current Lock Status Byte
		AND AL,00000111b		;Mask all but low order 3 bits
		OUT DATAPORT,AL			;Send KBD Command

		CALL OutBuffFull		;Eat response
		IN AL, DATAPORT

		PUSH 1					;KBD IRQ
		CALL FWORD PTR _UnMaskIRQ
		POP EAX
		RETN

;================= END OF MODULE ==================
