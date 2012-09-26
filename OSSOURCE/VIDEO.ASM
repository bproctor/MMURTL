;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0

;This module contains ALL Video code & Data
;    (along with EdLine for lack of a better place)
;
;This module supports MMURTL's Virtual Text Video.
;All video calls are based on the video information in the job's
;Job Control Block.
;
;Virtual video buffers allow a job to output data even when it is
;not assigned to the real video display buffer.
;
;The call SetVidOwner is the key. It swaps virtual buffers for the real one
;and changes the current pointer in that job's JCB.  It also updates
;the cursor in the correct place on entry.
;
; The following calls are implemented here:
;
;------------------------------
;GetVidOwner(pdJobNumRet)
; Desc: This returns the Job Number that currently has active video
; screen.
;------------------------------
;SetVidOwner(ddJobNum)
; Desc: This selects the screen that you see.  This call is used
; by the monitor in conjunction with the SetKbdOwner call to change
; who gets the keystrokes and video (should be the same app).
; The internal debugger is the only code that will use this call
; and not move the keyboard also.  It has it's own keyboard code
; (else how could it debug the real keyboard code??)
; Params:
;   ddJobNum is the new Job to get the active screen
;   (the one to be displayed)
;------------------------------
;SetNormVid(dAttr)
; Desc: This selects the normal background attribute and fill char
; used by ClrScr and ScrollVid on the screen.
; Params:
;   dCharAttr is the Character and aAttr values used in
;   standard video operation on the current screen
;
;------------------------------
;GetNormVid(pVidRet)
;
; Desc: This returns the value the normal screen attribute
; used by ClrScr and ScrollVid on the screen.
;
; Params:
;   pVidRet points to the character where you want the NormVid
;   attribute returned.
;
;------------------------------
;ClrScr ();
;
; Desc:
;   This clears the screen for the executing job. It may or may not
;   be the one you are viewing...
;
;------------------------------
;TTYOut (pTextOut, ddTextOut, ddAttrib)
;
;   Desc: This places characters on the screen in a TTY fashion at the
;   X & Y coordinates that are in effect for that screen.
;   The following characters in the stream are interpreted as follows:
;   Hex   Action
;   0A    Line Feed
;     The cursor (next active character placement) will be on the
;     following line at column 0.  If this line is below the bottom
;     of the screen, the entire screen will be scrolled up on line,
;     the bottom line will be blanked, and the cursor will be placed
;     on the last line in the first column.
;   0D    Carriage Return
;     The Cursor will be moved to column zero on the current line.
;   08    BackSpace - The cursor will be moved one column to the left.
;     If already at column 0, Backspace will have no effect.
;     The backspace is non-destructive (no chars are changed)
;
;   pTextOut is a NEAR Ptr to the text.
;   ddTextOut is the number of chars of text.
;   ddAttrib is the attribute/Color you want.
;
;------------------------------
;PutVidChars(ddCol,ddLine,pChars,sChars,ddAttrib)
;
; Desc: This places characters on the screen without affecting
;   the current TTY coordinates or the TTY data.  It is independent
;   of the current video "Stream."
;
; Params:
;   ddCol is the column to start on (0-79)
;   ddLine is the line (0-24)
;   pChars is a pointer the text to be displayed
;   sChars is the number of chars
;   ddAtrib is the color/attribute to use during display
;    which applies to all of the characters on this
;    call to PutVidChars.
;------------------------------
;GetVidChar(ddCol,ddLine,pCharRet,pAttrRet)
;
; Desc: This returns the current character and attribute
;   from the screen coordinates you specify.
;
; Params:
;   ddCol is the column to start on (0-79)
;   ddLine is the line (0-24)
;   pCharRet is a pointer where you want the character returned
;   pAttrRet is a pointer where you want the attribute returned
;------------------------------
;PutVidAttrs(ddCol,ddLine,sChars,dAttr)
;
; Desc: This sets screen colors (attrs) for the without affecting
;   the current TTY coordinates or the character data. It is independent
;   of the current video "Stream."
;
; Params:
;   ddCol is the column to start on (0-79)
;   ddLine is the line (0-24)
;   sChars is the number of char spaces to place dAttr
;   dAttr is the color/attribute to fill the character spaces with
;
;------------------------------
;ScrollVid(ddULCol,ddULline,nddCols,nddLines, ddfUp)
;
; Desc: This scrolls the described square area on the screen either
;   UP or DOWN one line.  If ddfUp is NON-ZERO the scroll will be UP.
;   The line left blank is filled with NormAttr from JCB.
; Parms:
;   ddULCol is the UPERR LEFT column to start on (0-79)
;   ddULLine is the UPPER LEFT line (0-24)
;   nddCols is the number of columns to be scrolled.
;   nddLines is the count of lines to be scrolled.
;   ddfUp is NON-ZERO to cause the scroll to be up (vise down).
;
;   If you want to scroll the entire screen UP one line, the
;   params would be  ScrollVid(VidNum, 0,0,80,25,1).
;   In this case the top line is lost (not really scrolled),
;   and the bottom line would be blanked. Infact, if you specified
;   (Vidnum, 0,1,80,24,1) you would get the same results.
;
;------------------------------
;SetXY(NewX,NewY)
; Desc: Position VGA cursor (Text mode) to the X & Y position.
;
; Params:
;   NewX is the new horizontal cursor postion
;   NewY is the new vertical cursor position
;
;------------------------------
;GetXY(pXRet,pYRet)
; Desc: Returns the current X & Y position for your job.
;
; Params:
;   pXRet is a ptr where you want the current horizontal cursor postion
;   pYRet is a ptr where you want the current vertical cursor position
;
.DATA
.INCLUDE MOSEDF.INC
.INCLUDE JOB.INC

;This is the data for the virtual character video service.
;The video code is fully reentrant because each job has it's own
;video screen.  Only one of these screens will be the active video
;display.
;
;Video Equates and Types
;
CRTCPort1   EQU 03D4h   ;Index port for CRTC
CRTCPort2   EQU 03D5h   ;Data port for CRTC
CRTCAddHi   EQU 0Ch     ;Register for lo byte of Video address
CRTCAddLo   EQU 0Dh     ;Register for lo byte of Video address
CRTCCurHi   EQU 0Eh     ;Register for lo byte of Cursor address
CRTCCurLo   EQU 0Fh     ;Register for lo byte of Cursor address
CRTC0C      DB 0        ;CRT Reg 0C HiByte address value
CRTC0D      DB 0        ;CRT Reg 0D LoByte address value

PUBLIC ddVidOwner   DD 1        ;JCB that currently owns video
                                ;Default to monitor (Job 1)

;End of Data & Equates
;
;============================================================================
; BEGIN INTERNAL CODE FOR VIDEO
;============================================================================
;
.CODE
;
EXTRN GetpJCB NEAR
EXTRN GetpCrntJCB NEAR
EXTRN ReadDbgKbd NEAR
EXTRN GetCrntJobNum NEAR

;InitVideo makes Video Screen 0 the default screen. That
;makes the VGATextBase address 0B8000h
;
PUBLIC InitVideo:
        MOV AL, CRTCAddHi       ;Index of hi byte
        MOV DX, CRTCPort1       ;Index Port
        OUT DX, AL
        MOV AL, CRTC0C          ;hi byte value to send
        MOV DX, CRTCPort2       ;Data Port
        OUT DX, AL
        ;
        MOV AL, CRTCAddLo       ;Index of lo byte
        MOV DX, CRTCPort1       ;Index Port
        OUT DX, AL
        MOV AL, CRTC0D          ;lo byte value to send
        MOV DX, CRTCPort2       ;Data Port
        OUT DX, AL
        RETN
;
;============================================================================
; BEGIN PUBLIC CODE FOR VIDEO
;============================================================================
;
;============================================================================
; SetVidOwner - Make a new a screen actively displayed.
; EAX Returns NON-Zero if JOB number is invalid

ddJobVidCV  EQU DWORD PTR [EBP+12]

PUBLIC __SetVidOwner:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        MOV EAX, ddJobVidCV     ;
        CMP EAX, ddVidOwner     ;Already own it?
        JNE ChgVid01            ;No
        XOR EAX, EAX            ;Yes
        JMP ChgVidDone
ChgVid01:
        CALL GetpJCB                    ;Leaves ptr to new vid JCB in EAX
        CMP DWORD PTR [EAX+pVidMem] ,0  ;Got valid video memory???
        JNE ChgVid02                    ;Yes
        MOV EAX, ErcVidNum      ;NO! Give em an error!
        JMP ChgVidDone
ChgVid02:
        ;Save data on screen to CURRENT job's pVirtVid
        MOV EAX, ddVidOwner
        CALL GetpJCB
        PUSH VGATextBase        ;Source
        MOV EBX, [EAX+pVirtVid] ;Destination
        PUSH EBX
        PUSH 4000               ;Size of video
        CALL FWORD PTR _CopyData    ;Do it!

        ;Make pVidMem same as pVirtVid for CURRENT OWNER
        MOV EAX, ddVidOwner
        CALL GetpJCB            ;Leaves ptr to new vid JCB in EAX
        MOV EBX, [EAX+pVirtVid]
        MOV [EAX+pVidMem], EBX

        ;Update current video owner to NEW owner

        MOV EAX, ddJobVidCV     ;
        MOV ddVidOwner, EAX

        ;Copy in Data from new pVirtVid

        MOV EAX, ddVidOwner
        CALL GetpJCB
        MOV EBX, [EAX+pVirtVid] ;Source
        PUSH EBX
        PUSH VGATextBase        ;Destination
        PUSH 4000               ;Size of video
        CALL FWORD PTR _CopyData    ;Do it!

        ;Make new pVidMem real video screen for new owner

        MOV EAX, ddVidOwner
        CALL GetpJCB
        MOV EBX, VGATextBase
        MOV [EAX+pVidMem], EBX

        ;Set Cursor position

        MOV EAX, ddVidOwner
        CALL GetpJCB
        MOV ECX, EAX
        MOV EBX, [ECX+CrntX]    ;Get current X for new screen
        MOV EAX, [EBX+CrntY]    ;Current Y
        CALL HardXY             ;Set it up
        XOR EAX, EAX            ;No Error
ChgVidDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 4

;============================================================================
; SetNormVid - Sets the normal video attribute (color) used in
; ClrScr, EditLine, and ScrollVid.
; EAX Returns Zero (No Error)

ddNormVid   EQU DWORD PTR [EBP+12]

PUBLIC __SetNormVid:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;pJCB -> EAX
        MOV EBX, ddNormVid      ;
        MOV [EAX+NormAttr], EBX ;
        XOR EAX, EAX
        POP EBP                 ;
        RETF 4

;============================================================================
; GetNormVid - Returns the normal video attribute (color) used in
; ClrScr, EditLine, and ScrollVid.
; EAX Returns Zero (No Error)

pdNormVidRet EQU DWORD PTR [EBP+12]

PUBLIC __GetNormVid:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;pJCB -> EAX
        MOV EBX, [EAX+NormAttr] ;
        MOV ESI, pdNormVidRet   ;
        MOV [ESI], BL           ;
        XOR EAX, EAX
        POP EBP                 ;
        RETF 4

;============================================================================
; GetVidOwner - Returns the Job number of current active video
;  number to the caller.
;=============================================================================

pVidNumRet  EQU DWORD PTR [EBP+12]

PUBLIC __GetVidOwner:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        MOV ESI, pVidNumRet     ;
        MOV EAX, ddVidOwner     ;
        MOV [ESI], EAX          ;
        XOR EAX, EAX            ; no error obviously
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 4

;=============================================================================
; Clear caller's video screen (Use Space and Attr from JCB NormAttr)
;=============================================================================
;
PUBLIC __ClrScr:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX
        MOV EDI,[EBX+pVidMem]   ;EDI points to his video memory
        MOV EAX, [EBX+NormAttr] ;Attr
        SHL EAX, 8              ;
        MOV AL, 20h             ;
        MOV DX, AX
        SHL EAX, 16
        MOV AX, DX              ;Fill Char & Attr
        MOV ECX,0400h
        CLD
        REP STOSD
        PUSH 0
        PUSH 0
        CALL FWORD PTR _SetXY   ;Erc in EAX on Return
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF

;=============================================================================
; TTYOut:
;=============================================================================

pTextOut    EQU DWORD PTR [EBP+20]
sTextOut    EQU DWORD PTR [EBP+16]
dAttrText   EQU DWORD PTR [EBP+12]

DataByte    EQU BYTE PTR [ECX]

PUBLIC __TTYOut:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX
        MOV EAX, sTextOut       ;make sure count isn't null
        OR EAX, EAX
        JZ TTYDone

TTY00:
        MOV EAX,[EBX+CrntX]     ; EAX has CrntX (col)
        MOV EDX,[EBX+CrntY]     ; EDX has CrntY (line)
        MOV ECX,pTextOut

        CMP DataByte,0Ah        ; LF?
        JNE TTY02
        INC EDX
        CMP EDX,[EBX+nLines]    ; Equal to or past the bottom?
        JB  TTY06               ; No
        JMP TTYScr              ; Yes, goto scroll
TTY02:
        CMP DataByte,0Dh        ; CR?
        JNE TTY03
        MOV EAX,0
        JMP TTY06
TTY03:
        CMP DataByte,08h        ; BackSpace?
        JNE TTY04
        CMP EAX,0
        JE  TTY04
        DEC EAX
        JMP TTY06
TTY04:
        PUSH EBX                ;Save pointer to VCB

        PUSH EAX                ;X (Param 1)
        PUSH EDX                ;Y (Param 2)
        PUSH ECX                ;pointer to text char (Param3)
        PUSH 1                  ;Param 4 (nchars)
        MOV ECX,dAttrText       ;
        PUSH ECX                ;Param 5
        CALL FWORD PTR _PutVidChars ;
        POP EBX                 ;restore ptr to VCB
        CMP EAX, 0
        JNE TTYDone
        MOV EAX, [EBX+CrntX]
        MOV EDX, [EBX+CrntY]
        INC EAX                 ;Next column
        CMP EAX,[EBX+nCols]
        JNE TTY06               ;Make cursor follow
        MOV EAX,0
        INC EDX
        CMP EDX,[EBX+nLines]    ; past the bottom?
        JNE TTY06               ; No - goto 06 else fall thru

TTYScr:
        DEC EDX                 ; back up one line
        PUSH EAX                ;Save registers (scroll eats em)
        PUSH EBX
        PUSH ECX
        PUSH EDX

        PUSH 0
        PUSH 0
        PUSH 80
        PUSH 25
        PUSH 1                  ;fUP (non zero)
        CALL FWORD PTR _ScrollVid   ;Ignore error
        POP EDX                 ;restore registers
        POP ECX
        POP EBX
        POP EAX                 ;Fall thru to

TTY06:
        PUSH EBX                ;save ptr to pJCB

        PUSH EAX
        PUSH EDX
        CALL FWORD PTR _SetXY

        POP EBX                 ;Restore ptr to VCB
        CMP EAX, 0
        JNE TTYDone
        DEC sTextOut
        JZ TTYDone
        INC pTextOut
        JMP TTY00               ; Go back for next char
TTYDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 12



;=============================================================================
; PutVidAttrs:
; Desc: This sets screen colors (attrs) for the without affecting
;   the current TTY coordinates or the character data. It is independent
;   of the current video "Stream."
;
; Params:
;   ddCol is the column to start on (0-79)
;   ddLine is the line (0-24)
;   sChars is the number of char spaces to place dAttr
;   dAttr is the color/attribute to fill the character spaces with
;
; Start Position in screen memory is (Line * 80 + (Column*2))
; pass Char, then Color, pass char, then color etc... DO NOT EXCEED 2000!
; Needs to be fixed to tell if ECX + sDDChars will go off screen...
;
;=============================================================================

oADDX       EQU DWORD PTR [EBP+24] ;Param 1 COLUMN
oADDY       EQU DWORD PTR [EBP+20] ;Param 2 LINE
sADDChars   EQU DWORD PTR [EBP+16] ;Param 3 sChars
sADDColor   EQU DWORD PTR [EBP+12] ;Param 4 Attr

PUBLIC __PutVidAttrs:
        PUSH EBP                ;
        MOV EBP,ESP             ;

        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX

        MOV EDI, [EBX+pVidMem]  ;point to this VCBs video memory
        MOV EBX,oADDx           ;x Position
        SHL EBX,1               ;Times 2
        MOV EAX,oADDy           ;y Position
        MOV ECX,0A0h            ;Times 160 (char/attrs per line)
        MUL ECX                 ;Times nColumns
        ADD EAX,EBX
        CMP EAX,0F9Eh           ;Last legal posn on screen
        JBE PutAttrs00
        MOV EAX, ErcVidParam
        JMP PcADone
PutAttrs00:
        MOV ECX,sADDChars
        OR ECX, ECX
        JZ PcADone
        ADD EDI,EAX
        MOV EAX,sADDColor
        CLD
pcAMore:
        INC EDI                 ;Pass the char value
        STOSB                   ;Move Color in
        LOOP pcAMore
        XOR EAX, EAX            ;No Error!
pcADone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 16

;=============================================================================
; PutVidChars:
; This Places characters on the VGA Character Screen in the XY Coords
;  Params
;  1) DD X (Column 0-79)
;  2) DD Y (Line 0-24)
;  3) DD Near Ptr (relative to DS) of string
;  4) DD Size of String
;  5) DD (of which the low order byte is the Color)
;
; Start Position in screen memory is (Line * 80 + (Column*2))
; Put Char, then Color, then char, then color etc... DO NOT EXCEED 2000!
; Needs to be fixed to tell if ECX + sDDChars will go off screen...
;
;=============================================================================

oDDX        EQU DWORD PTR [EBP+28] ;Param 1 COLUMN
oDDY        EQU DWORD PTR [EBP+24] ;Param 2 LINE
pDDChars    EQU DWORD PTR [EBP+20] ;Param 3 pChars
sDDChars    EQU DWORD PTR [EBP+16] ;Param 4 sChars
sDDColor    EQU DWORD PTR [EBP+12] ;Param 5 Attr

PUBLIC __PutVidChars:
        PUSH EBP                ;
        MOV EBP,ESP             ;
		CLI
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX
        MOV EDI, [EBX+pVidMem]  ;point to this VCBs video memory
		STI
        MOV EBX,oDDx
        SHL EBX,1               ;Times 2
        MOV EAX,oDDy
        MOV ECX,0A0h            ;Times 160
        MUL ECX                 ;Times nColumns
        ADD EAX,EBX
        CMP EAX,0F9Eh           ;Last legal posn on screen
        JBE PutChars00
        MOV EAX, ErcVidParam
        JMP PcDone
PutChars00:
        MOV ECX,sDDChars
        OR ECX, ECX
        JZ PcDone
        MOV ESI,pDDChars
        ADD EDI,EAX
        MOV EAX,sDDColor
        CLD
pcMore:
        MOVSB                   ;Move Char in
        STOSB                   ;Move Color in
        LOOP pcMore
        XOR EAX, EAX            ;No Error!
pcDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 20

;=============================================================================
;GetVidChar(ddCol,ddLine,pCharRet,pAttrRet)
;
; Desc: This returns the current character and attribute
;   from the screen coordinates you specify.
;
; Params:
;   ddCol is the column to start on (0-79)
;   ddLine is the line (0-24)
;   pCharRet is a pointer where you want the character returned
;   pAttrRet is a pointer where you want the attribute returned
;

oGDDX       EQU DWORD PTR [EBP+24] ;Param 1 COLUMN
oGDDY       EQU DWORD PTR [EBP+20] ;Param 2 LINE
pGDDCRet    EQU DWORD PTR [EBP+16] ;Param 3 pCharRet
pGDDARet    EQU DWORD PTR [EBP+12] ;Param 4 pAttrRet

PUBLIC __GetVidChar:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX

        MOV EDI, [EBX+pVidMem]  ;point to this VCBs video memory
        MOV EBX,oGDDx
        SHL EBX,1               ;Times 2
        MOV EAX,oGDDy
        MOV ECX,0A0h            ;Times 160
        MUL ECX                 ;Times nColumns
        ADD EAX,EBX
        CMP EAX,0F9Eh           ;Last legal posn on screen
        JBE GetChar00
        MOV EAX, ErcVidParam
        JMP PcGDone
GetChar00:
        ADD EDI,EAX             ;EDI now points to char
        MOV ESI,pGDDCRet
        MOV AL, [EDI]
        MOV [ESI], AL           ;Give them the char
        INC EDI                 ;Move to Attr
        MOV ESI,pGDDARet
        MOV AL, [EDI]
        MOV [ESI], AL           ;Give them the Attr
        XOR EAX, EAX            ;No Error!
pcGDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 20
;=============================================================================
; ScrollVid:
; This scrolls the defined area up or down one line.
;  Params
;  1) Upper Left column X (Column 0-79)
;  2) Upper Left line Y (Line 0-24)
;  3) nCols to scroll
;  4) nLines to scroll
;  5) TRUE for up (any NON zero QUAD)
;
;  We check all params for validity. ErcVidParam is returned if one is
;  invalid.
;=============================================================================

oULX        EQU DWORD PTR [EBP+28] ;Param 1 COLUMN
oULY        EQU DWORD PTR [EBP+24] ;Param 2 LINE
nddCols     EQU DWORD PTR [EBP+20] ;Param 3 Number of columns
nddLines    EQU DWORD PTR [EBP+16] ;Param 4 Number of Lines
ddfUP       EQU DWORD PTR [EBP+12] ;Param 5 Attr

PUBLIC __ScrollVid:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX            ;Save pJCB & use in EBX
        MOV EAX, oULX
        CMP EAX, 79
        JA svErcExit
        ADD EAX, nddCols
        CMP EAX, 80
        JA svErcExit
        MOV EAX, oULY
        CMP EAX, 24
        JA svErcExit
        ADD EAX, nddLines
        CMP EAX, 25
        JA svErcExit

        CMP ddfUP, 0        ;Scroll UP?
        JNE svUP0           ;Yes... Scroll UP!

;Scroll DOWN begins

        MOV EAX, oULY       ;First line
        ADD EAX, nddLines   ;Last line
        MOV ECX, 160
        MUL ECX                 ;times nBytes per line
        MOV EDI, [EBX+pVidMem]  ;EDI points to video memory 0,0
        MOV EDX, EBX            ;Save pJCB
        ADD EDI, EAX            ;EDI is ptr to 1st dest line
        ADD EDI, oULX       ;offset into line
        ADD EDI, oULX       ;add again for attributes
        MOV ESI, EDI        ;
        SUB ESI, 160        ;ESI is 1st source line
        MOV EBX, ESI        ;Save in EBX for reload
        MOV EAX, nDDLines   ;How many lines to move
        DEC EAX             ;one less than window height
svDOWN1:
        MOV ECX, nddCols    ;How many WORDS per line to move
        REP MOVSW           ;Move a line (of WORDS!)
        MOV EDI, EBX        ;Reload Dest to next line
        MOV ESI, EDI
        SUB ESI, 160
        MOV EBX, ESI        ;Save again
        DEC EAX
        JNZ svDOWN1
        MOV EAX, [EDX+NormAttr] ;Normal video attributes!!!
        SHL EAX, 8
        MOV AL, 20h             ;Space
        MOV EDI, EBX            ;Put the last line into EDI
        MOV ECX, nddCols
        CLD
        REP STOSW
        XOR EAX, EAX        ;No error
        JMP svDone
                            ;No... scroll down begins
svUP0:
        MOV EAX, oULY       ;First line
        MOV ECX, 160
        MUL ECX                 ;times nBytes per line
        MOV EDI, [EBX+pVidMem]  ;EDI points to video memory 0,0
        MOV EDX, EBX            ;Save pJCB
        ADD EDI, EAX            ;EDI is ptr to 1st dest line
        ADD EDI, oULX       ;offset into line
        ADD EDI, oULX       ;add again for attributes
        MOV ESI, EDI        ;
        ADD ESI, 160        ;ESI is 1st source line
        MOV EBX, ESI        ;Save in EBX for reload
        MOV EAX, nDDLines   ;How many lines to move
        DEC EAX             ;two less than window height
svUP1:
        MOV ECX, nddCols    ;How many WORDS per line to move
        REP MOVSW           ;Move a line (of WORDS!)
        MOV EDI, EBX        ;Reload Dest to next line
        MOV ESI, EDI
        ADD ESI, 160
        MOV EBX, ESI        ;Save again
        DEC EAX
        JNZ svUP1
        MOV EAX, [EDX+NormAttr] ;Normal video attributes!!!
        SHL EAX, 8
        MOV AL, 20h             ;Space
        MOV EDI, EBX        ;Put the last line into EDI
        SUB EDI, 160
        MOV ECX, nddCols
        CLD
        REP STOSW
        XOR EAX, EAX        ;No error
        JMP svDone
svErcExit:                  ;Error exits will jump here
        MOV EAX, ErcVidParam
svDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 20
;
;=============================================================
; HardXY - Intenal  Internal to support SetXY and SetVidOwner
;           This sets the hardware cursor position
; Input:
;   EAX : New Y position
;   EBX : New X position
; Used:
;   EAX, EBX, EDX, Flags
; Output:
;   None

HardXY:
        MOV ECX,80
        MUL ECX                 ; Line * 80
        ADD EAX,EBX             ; Line plus column
        MOV DX,CRTCPort1        ; Index register
        PUSH EAX
        MOV AL,CRTCCurLo
        OUT DX,AL               ; Index 0Fh for low byte
        POP EAX
        MOV DX,CRTCPort2        ; Data register
        OUT DX,AL               ; Send Low byte out
        SHR EAX,08              ; shift hi byte into AL
        PUSH EAX
        MOV DX,CRTCPort1
        MOV AL,CRTCCurHi
        OUT DX,AL               ; Index for High byte
        POP EAX
        MOV DX,CRTCPort2
        OUT DX,AL               ; Send High byte out
        RETN
;
;=============================================================================
; SetXY:
; Position VGA cursor (Text mode) to the X & Y position.
; Also sets hardware CrntX and CrntY cursor position if
; crnt job is assigned the real screen
;=============================================================================

NewX        EQU DWORD PTR [EBP+16]
NewY        EQU DWORD PTR [EBP+12]

PUBLIC __SetXY:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX

        MOV ECX,NewX            ; Column
        MOV EDX,NewY            ; Line
        MOV [EBX+CrntX],ECX     ; This saves it in the VCB
        MOV [EBX+CrntY],EDX     ;

        CALL GetCrntJobNum      ;Leaves ptr to current JCB in EAX
        CMP EAX, ddVidOwner
        JNE GotoXYDone          ;If not on Active screen, skip it

        MOV EAX,NewY            ;Setup to call HardXY
        MOV EBX,NewX
        CALL HardXY
GotoXYDone:
        XOR EAX,EAX             ;No Error
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 8

;=============================================================================
; GetXY:
; Returns position of VGA cursor (Text mode X & Y position).
; This appliies to the values for the caller's VCB
;=============================================================================

pXret       EQU DWORD PTR [EBP+16]
pYret       EQU DWORD PTR [EBP+12]

PUBLIC __GetXY:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, EAX

        MOV EAX,[EBX+CrntX]     ; Column
        MOV ESI,pXret
        MOV [ESI], EAX
        MOV EAX,[EBX+CrntY]     ; Line
        MOV ESI,pYret
        MOV [ESI], EAX
        XOR EAX,EAX
QXYDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 8


;=============================================================================
; EditLine
; Reads a line of text from the keyboard and puts it into the
; specified string with full line editing features.
;
; EditLine(pStr, dCrntLen, dMaxLen, pdLenRet, pbExitChar): dError
;
; Param 1 is a NEAR Ptr to string to be edited
; Param 2 is current length of string to edit (80 Max)
; Param 3 is the max length the string can be (80 Max)
; Param 4 is a NEAR Ptr to a DD where the length of the string is returned
; Param 5 is a pointer to a Byte where the exit key from the edit
;        operation is returned.
; Param 6 is the editing attribute to use.
;
; Display and keyboard are handled entirely inside EditLine.
; The following keys are recognized and handled inside, any other key
; causes Editline to exit returning the string in it's current condition
; and returning the key that caused the exit to pKeyRet:
;
; 08 (Backspace) move cursor to left replacing char with 20h
;     which is a destructive backspace
; 20-7E Hex places character in current position and advances position

; Any other keystroke causes the edit line routine to be exited
; with that keystroke returned to pExitKeyRet
;
;
;=============================================================================

pEdString       EQU DWORD PTR [EBP+32]
ddSzCrnt        EQU DWORD PTR [EBP+28]
ddSzMax         EQU DWORD PTR [EBP+24]
pddSzRet        EQU DWORD PTR [EBP+20]
pExitKeyRet     EQU DWORD PTR [EBP+16]
dEditAttr       EQU DWORD PTR [EBP+12]

;Local vars EditX and EditY hold position of first char of text
;CrntX is the cursor postion
;
PosnX           EQU DWORD PTR [EBP-04]
EditX           EQU DWORD PTR [EBP-08]
EditY           EQU DWORD PTR [EBP-12]
KeyCode         EQU DWORD PTR [EBP-16]

PUBLIC __EditLine:
        PUSH EBP                ;
        MOV EBP,ESP             ;
        SUB ESP, 16

        CMP ddSzCrnt, 80     ;Is it currently too long?
        JA BadEdit
        CMP ddSzMax, 80      ;Is Max len to long?
        JA BadEdit
        MOV EAX, ddSzCrnt
        CMP EAX, ddSzMax     ;Is Crnt len > Max???
        JA BadEdit

        LEA EAX, EditX          ;Get current cursor posns in local vars
        PUSH EAX
        LEA EAX, EditY
        PUSH EAX
        CALL FWORD PTR _GetXY
        CMP EAX, 0
        JNE EditDone         ;Bad Erc from call

        MOV EAX, EditX
        ADD EAX, ddSzCrnt
        MOV PosnX, EAX       ;make PosnX end of string

        MOV ECX, ddSzMax
        SUB ECX, ddSzCrnt    ;ECX  how many bytes to zero
        JZ  EdLn01           ;None to zero out
        MOV ESI, pEdString   ;Initialize currrent string
        ADD ESI, ddSzCrnt    ;ESI ptr to 1st empty byte
        MOV AL, 20h          ;fill with spaces
Edln00:
        MOV [ESI], AL
        INC ESI
        LOOP Edln00

EdLn01:
        PUSH PosnX
        PUSH EditY
        CALL FWORD PTR _SetXY
        CMP EAX, 0
        JNE EditDone

EdLn02:
        PUSH EditX              ;Display current string
        PUSH EditY
        PUSH pEdString
        PUSH ddSzMax
        PUSH dEditAttr          ;Attribute they selected
        CALL FWORD PTR _PutVidChars
        CMP EAX, 0
        JNE EditDone
EdLn03:
        LEA EAX, KeyCode
        PUSH EAX
        CMP ddVidOwner, 2       ;Debugger???
        JE EdLn035
        PUSH 1                  ;Wait for a key
        CALL FWORD PTR _ReadKbd ;Get a key
        JMP SHORT EdLn036
EdLn035:
        CALL ReadDbgKbd
EdLn036:
        MOV EAX, KeyCode
        AND EAX, 07Fh
        OR  EAX, EAX
        JZ  EdLn03
        CMP EAX, 08h            ;BackSpace?
        JNE EdLn04              ;No - Next test
EdLn037:
        CMP ddSzCrnt, 0
        JE EdLn01
        DEC PosnX
        DEC ddSzCrnt
        MOV ESI, pEdString
        MOV ECX, ddSzCrnt
        MOV BYTE PTR [ESI+ECX], 20h
        JMP Edln01
EdLn04: CMP EAX, 03h            ;Left?
        JNE EdLn045             ;No - Next test
        JMP EdLn037
EdLn045:
        CMP EAX, 83h            ;Num-Left?
        JNE EdLn046             ;No - Next test
        JMP EdLn037
EdLn046:
        CMP EAX, 0Dh            ;CR?
        JNE EdLn05              ;No - Next test
        JMP EdLn07
EdLn05: CMP EAX, 1Bh            ;Escape?
        JNE EdLn06              ;No - Next test
        JMP EdLn07
EdLn06:
        CMP EAX, 7Eh            ;Is it above text?
        JA  EdLn07              ;Yes, Exit!
        CMP EAX, 20h            ;Is it below text??
        JB  EdLn07              ;Yes, Exit
        MOV ESI, pEdString      ;It's really a char!
        MOV ECX, ddSzCrnt
        MOV BYTE PTR [ESI+ECX], AL
        MOV ECX, ddSzMax
        CMP ddSzCrnt, ECX
        JAE EdLn01
        INC PosnX
        INC ddSzCrnt
        JMP EdLn01
EdLn07:
        MOV ESI,pExitKeyRet
        MOV [ESI], AL
        MOV ESI,pddSzRet
        MOV EAX, ddSzCrnt
        MOV [ESI], EAX

        PUSH EditX              ;Display current string w/Norm Attrs
        PUSH EditY
        PUSH pEdString
        PUSH ddSzMax
        CALL GetpCrntJCB        ;Leaves ptr to current JCB in EAX
        MOV EBX, [EAX+NormAttr]
        PUSH EBX                ;Normal Attribute from JCB
        CALL FWORD PTR _PutVidChars ;Ignore error (we are leaving anyway)
        XOR EAX, EAX
        JMP EditDone
BadEdit:
        MOV EAX, ErcEditParam
EditDone:
        MOV ESP,EBP             ;
        POP EBP                 ;
        RETF 24

;===================== END OF MODULE ================
