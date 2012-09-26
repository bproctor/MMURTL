;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED   Version 1.0

.DATA
.INCLUDE MOSEDF.INC
;=============================================================================
.CODE
;=============================================================================
;
; HextoDD - Converts a text string of Hex chars to a DD
; This converts an ASCII string of hex digits to a DD  (3 params)
;  1) 32 bit near ptr to the ASCII Hex String
;  2) The 32 Bit Near Ptr (relative to DS) of returned DD
;  3) DD with count of bytes in string
;  This can easily be converted to accept a DF (48 bit Far Ptr)
;  EAX returns ercBadString if string supplied was not convertable!
;  This only saves segment registers!!!
;
pHexStr		EQU DWORD PTR [EBP+10h]
pDDOut		EQU DWORD PTR [EBP+0Ch]
cbHexStr	EQU DWORD PTR [EBP+08h]
HexTotal	EQU DWORD PTR [EBP-04h]   ;Local variable

PUBLIC HexToDD:
		ENTER 4,0
		MOV ECX,cbHexStr        ;how long is the string
		MOV ESI,pHexStr         ;EBX points to str[1]
		ADD ESI, ECX            ;EBX points to str[cbStr] (LSD)
		DEC ESI                 ;Offset of last is p - 1
		MOV EDI,pDDOut          ;prepare to return answer
		MOV HexTotal, 0         ;start with nothing
		MOV EBX, 1              ;First mutliplier
NextHex:
		XOR EAX, EAX
		MOV AL, [ESI]
		CMP AL, 47h             ;
		JB Hex000               ;It's not lower case...
		AND AL, 01001111b       ;make it Upper case
Hex000:
		CMP AL, 30h             ;Less than "0"?
		JB BadHex               ;Yes...
		SUB AL, 30h             ;No, make it a binary number
		CMP AL, 0Ah             ;Is it A-F??
		JB Hex001
		SUB AL, 07h             ;Yes, make it a number
Hex001:
		MUL EBX                 ;TIMES multiplier
		ADD EAX, HexTotal       ;add to total
		MOV HexTotal, EAX
		SHL EBX, 4              ;increase mutiplier TIMES 16
		DEC ESI
		LOOP NextHex
		MOV EAX, HexTotal
		MOV [EDI], EAX
		MOV EAX, ercOk
		JMP HexDone
BadHex:
		MOV EAX, ercBadString
HexDone:
		LEAVE
		RETN 12

;=============================================================================
;
; DecToDD:
; This converts an ASCII string of Decimal digits to a DD  (3 params)
;  1) 32 bit near ptr to the ASCII Decimal String
;  2) The 32 Bit Near Ptr (relative to DS) of returned DD
;  3) DD with count of bytes in string
;  EAX returns ercBadString if string supplied was not convertable!
;
;=============================================================================

pDecStr		EQU DWORD PTR [EBP+16]
pDDOut1		EQU DWORD PTR [EBP+12]
cbDecStr	EQU DWORD PTR [EBP+08]
DecTotal	EQU DWORD PTR [EBP-04]

PUBLIC DecToDD:
		ENTER 4,0
		MOV ECX,cbDecStr        ;how long is the string
		MOV ESI,pDecStr         ;EBX points to str[1]
		ADD ESI, ECX            ;EBX points to str[cbStr] (LSD)
		DEC ESI                 ;Offset of last is p - 1
		MOV EDI,pDDOut1         ;prepare to return answer
		MOV DecTotal, 0         ;start with nothing
		MOV EBX, 1              ;First mutliplier
NextDec:
		XOR EAX, EAX
		MOV AL, [ESI]
		CMP AL, 39h             ;> "9"?
		JA BadDec               ;It's not lower case...
Dec000:
		CMP AL, 30h             ;Less than "0"?
		JB BadDec               ;Yes...
		SUB AL, 30h             ;No, make it a binary number
		MUL EBX                 ;TIMES multiplier
		ADD EAX, DecTotal       ;add to total
		MOV DecTotal, EAX
		MOV EAX, 10
		MUL EBX                 ;increase mutiplier TIMES 10
		MOV EBX, EAX
		DEC ESI
		LOOP NextDec
		MOV EAX, DecTotal
		MOV [EDI], EAX
		MOV EAX, ercOk
		JMP DecDone
BadDec:
		MOV EAX, ercBadString
DecDone:
		LEAVE
		RETN 12

;=============================================================================
;
; DDtoHex:
; This converts a DD to an 8 Byte ASCII String in Hex (2 params)
;  1) the DD to convert
;  2) The 32 Bit Near Ptr (relative to DS) of returned string
;
;=============================================================================

DDin	EQU DWORD PTR [EBP+0Ch]
pStrOut	EQU DWORD PTR [EBP+08h]

PUBLIC DDtoHex:
		ENTER 0,0
		PUSHAD
		MOV EAX,DDin
		MOV EDI,pStrOut
		MOV ECX,8h
DDAgain:
		MOV BL,AL
		AND BL,0Fh
		MOV BH,30h              ;0 in ASCII
		ADD BH, BL
		CMP BH,39h
		JLE SHORT PutOut
		ADD BH,7
PutOut:
		MOV [EDI+ECX-1],BH
		DEC ECX
		JZ SHORT DDDone
		SHR EAX,4
		JMP SHORT DDAgain
ddDone:	POPAD
		LEAVE
		RETN 08h

;=============================================================================
;
; DDtoDec:
; This converts a DD to a 10 Byte ASCII DECIMAL String  (2 params)
;  1) the DD to convert
;  2) The 32 Bit Near Ptr (relative to DS) of returned string
;  This can easily be converted to accept a DF (48 bit Far Ptr)
;
;=============================================================================

DDinD           EQU DWORD PTR [EBP+0Ch]
pStrOutD        EQU DWORD PTR [EBP+08h]

PUBLIC DDtoDec:
		ENTER 0,0
		PUSHAD
		MOV ESI,DDin            ;ESI has what's left of DD
		MOV EDI,pStrOut
		MOV ECX,10
		MOV EBX, 1000000000     ;A billion (exactly...DUH)
		MOV AL, 30h             ;A zero (ASCII)
DDtoDec0:
		CMP ESI, EBX
		JB  DDtoDec1            ;Go for the next digit
		SUB ESI, EBX
		INC AL
		JMP DDtoDec0
DDtoDec1:
		MOV [EDI], AL           ;Give them this byte
		INC EDI                 ;setup for next byte in string
		XOR EDX,EDX             ;Div EBX by 10 put back in EBX
		MOV EAX,EBX
		MOV EBX,10
		DIV EBX
		MOV EBX, EAX            ;
		MOV AL, 30h             ;start with zero again
		LOOP DDtoDec0           ;Go back for more
		POPAD
		LEAVE
		RETN 08h

;============ End of Module ==================
