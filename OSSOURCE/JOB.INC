;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0

;DO NOT INCLUDE DATA OR CODE IN THIS MODULE. ONLY EQUATES/MACROS

; Job Control Block (all tasks are associated with a JOB)
; The JOB NUMBER is how the job is identifed by many OS functions.
; Each JCB is assigned a number when the it is initialized.
; Job 1 is always reserved for the OS Monitor and 2 for the debugger.
;  (even though the debugger isn't really a job...)
;
; The JCB keeps job related information such as the current path
; of the job, Linear address of it's PD, virtual video info, Cmd line
; when executed, Job path, etc...
;
nJCBs		EQU 32			;max Job Contol Blocks (excluding 2 static JCBs)
sJCB		EQU 512     	;
;
;			Offsets into the structure
JobNum		EQU 0			;DD 0				;
sbJobName	EQU 4			;DB 14 DUP (0h)		; 13 bytes w/1 length
JcbPD		EQU 18			;DD 0				; Linear add of Job's PD
pJcbCode	EQU 22			;DD 0 				;User Address of code segment
sJcbCode	EQU 26			;DD 0				;Size of user code segment
pJcbData	EQU 30			;DD 0				;User Adresss of data segment
sJcbData	EQU 34			;DD 0				;Size of user data segment
pJcbStack	EQU 38			;DD 0				;User Addrees of first stack
sJcbStack	EQU 42			;DD 0				;Size of user first stack
JcbUserName EQU 46			;DB 30 DUP (0h)		; User Name for Job - LString
JcbPath 	EQU 76			;DB 70 DUP (0h)		; path name (prefix) - LString
JcbExitRF	EQU 146			;DB 80 DUP (0h)		; Exit Run file (if any) - LString
JcbCmdLine	EQU 226			;DB 80 DUP (0h)		; Command Line string - LString
JcbSysIn	EQU 306			;DB 50 DUP (0h)		; Standard input (KBD)
JcbSysOut	EQU 356			;DB 50 DUP (0h)		; Standard output (VID)
ExitError   EQU 406			;DD 0		;Error passed by ExitJob
pVidMem     EQU 410			;DD 0		;pointer to crnt video (real or virtual)
pVirtVid	EQU 414			;DD 0		;Virtual Video Buffer Address
CrntX	    EQU 418			;DD 0		;Current cursor position
CrntY	    EQU 422			;DD 0
nCols	    EQU 426			;DD 80	    ;Screen size
nLines	    EQU 430			;DD 25
VidMode     EQU 434			;DD 0	    ;0 = 80x25 VGA color text
NormAttr    EQU 438 		;DD 07	    ;Normal Video Attr
fCursOn	    EQU 442			;DB 0	    ;Is cursor visible on this screen
fCursType   EQU 443			;DB 0	    ;Cursor type (0=UL, 1 = Block)
ScrlCnt     EQU 444			;DB 0	    ;Count since last pause (or Kbd)
fVidPause   EQU 445			;DB 0	    ;Full screen pause (Text mode)
NextJCB 	EQU 446			;DD 0		;

;============ ENd of Module ======================
