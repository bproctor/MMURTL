;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED   Version 1.0

;===============================================
.CODE

;The following calls are miscellaneous support functions that
;support I/O and high speed string functions on the 80386/486
;processors. They are made accessible via call gates to all
;users (except for the I/O ops which are limited to supervisor
;level callers).

; OutByte(Byte, dPort)
; OutWord(Word,dPort)
; OutWords(dPort,pDataOut,dBytes)
; OutDWord(DWord, dPort)
; InByte(dPort):Byte
; InWord(dPort):Word
; InWords(dPort, pDataIn,dBytes)
; InDWord(dPort):DWord
; ReadCMOS(bAddress):Byte
; CopyData(pSource, pDestination, dBytes)
; CopyDataR(pSource, pDestination, dBytes)
; FillData(pDest, cBytes, bFill)
; CompareNCS(pS1, pS2, dSize) : returned offset or -1
; Compare(pS1, pS2, dSize) : returned offset or -1

;===============================================
; OutByte(Byte, wPort)
; The Byte is sent out the I/O Port specified.
; Byte  = [EBP+16]
; wPort = [EBP+12]
;
PUBLIC __OutByte:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		MOV AL, BYTE PTR [EBP+16]
		OUT DX, AL
		POP EBP
		RETF 8                  ;
;
;===============================================
; OutWord(Word, wPort)
; The Word is sent out the I/O Port specified.
; Word  = [EBP+16]
; wPort = [EBP+12]
;
PUBLIC __OutWord:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		MOV AX, WORD PTR [EBP+16]
		OUT DX, AX
		POP EBP
		RETF 8                  ;

;===============================================
; OutWords(wPort, pDataOut, dBytes)
; The dBytes/2 are sent out to wPort from the pDataOut address
; wPort    = [EBP+20]
; pDataOut = [EBP+16]
; dBytes   = [EBP+12]
;
PUBLIC __OutWords:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+20]
		MOV ESI, DWORD PTR [EBP+16]
		MOV ECX, DWORD PTR [EBP+12]
		SHR ECX, 1					;Make WORDS vice bytes
		CLD
		REP OUTSW
		POP EBP
		RETF 12                  ;
;
;===============================================
; OutDWord(DWord, wPort)
; The Word is sent out the I/O Port specified.
; DWord  = [EBP+16]
; wPort  = [EBP+12]
;
PUBLIC __OutDWord:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		MOV EAX, DWORD PTR [EBP+16]
		OUT DX, EAX
		POP EBP
		RETF 8                  ;
;
;===============================================
; InByte(wPort)
; The Byte is read from the I/O Port specified and returned in AL
; wPort = [EBP+12]
;
PUBLIC __InByte:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		IN AL, DX
		AND EAX, 0FFh			;Only get the byte
		POP EBP
		RETF 4                  ;
;
;===============================================
; InWord(wPort)
; The Byte is read from the I/O Port specified and returned in AX
; wPort = [EBP+12]
;
PUBLIC __InWord:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		IN AX, DX
		AND EAX, 0FFFFh			;Only get the Word
		POP EBP
		RETF 4                  ;
;
;===============================================
; InWords(wPort, pDataIn, dBytes)
; The dBytes/2 are read in from wPort to pDataIn
; wPort    = [EBP+20]
; pDataIn  = [EBP+16]
; dBytes   = [EBP+12]
;
; ASSUMES ES == DS !!!! (In MMURTL it always does...)
;
PUBLIC __InWords:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+20]
		MOV EDI, DWORD PTR [EBP+16]
		MOV ECX, DWORD PTR [EBP+12]
		SHR ECX, 1					;Make WORDS vice bytes
		CLD
		REP INSW
		POP EBP
		RETF 12                  ;
;
;===============================================
; InDWord(wPort)
; The Byte is read from the I/O Port specified and returned in EAX
; wPort = [EBP+12]
;
PUBLIC __InDWord:
		PUSH EBP
		MOV EBP, ESP
		MOV DX, WORD PTR [EBP+12]
		IN EAX, DX
		POP EBP
		RETF 4                  ;

;===============================================
; ReadCMOS(wAddress)
; The Byte is read from the CMOS address specified and returned in AL
; wAddress = [EBP+12]
;
PUBLIC __ReadCMOS:
		PUSH EBP
		MOV EBP, ESP
		MOV AL, BYTE PTR [EBP+12]
		OR AL, 80h
		CLI
		OUT 70h,AL
		NOP
		NOP
		NOP
		NOP
		IN AL, 71h
		STI
		AND EAX, 0FFh
		POP EBP
		RETF 4                  ;

;===============================================
;
; CopyData(pSource, pDestination, dBytes)
;
; pSource is address of place to copy from,
; pDestination is address of place to copy to,
; and cBytes is number of bytes to copy
;
; A usefull routine that ALWAYS moves DWORDS
; when possible. We test to see if there is an
; odd byte, if so we move it. Then check for an
; odd word, if there, we move it. Then we move
; the rest with MOVSD (whole DWORDS!).
; WARNING: ASSUMES ES = DS  !!!!
; Do NOT use to shift data in an array to the right,
; use CopyDataR instead.  CopyData can be used
; to shift data to the left in an array safely.
;
;pSource	[EBP + 20]
;pDest		[EBP + 16]
;cBytes		[EBP + 12]
;
PUBLIC __CopyData	PROC	FAR
		PUSH EBP
		MOV EBP,ESP
		MOV EDI,[EBP+16]		;Load destination address
		MOV ESI,[EBP+20]		;Load source address
		MOV ECX,[EBP+12]		;Load count of bytes to move
		CLD						;Auto incrementing move
		SHR ECX,1				;Check odd byte
		JNC NoByteC
		MOVSB					;Handle the odd byte
NoByteC:
		SHR ECX,1				;Check odd word
		JNC NoWordC
		MOVSW					;Handle the odd word
NoWordC:
		REP MOVSD				;Move all DWORDS that are left
		POP EBP
		RETF 12					;Pop args off the stack

;===============================================
;
; CopyDataR(pSource, pDestination, dBytes)
;
; pSource is begining address of place to copy from,
; pDestination is begining address of place to copy to,
; and cBytes is number of bytes to copy
;
; Same as CopyData except data is copied from
; the higest addresses of pSource and pDest first.
; (e,g, pSource+dBytes => pDest+dBytes)
; WARNING: ASSUMES ES = DS  !!!!
; Do NOT use to shift data in an array to the left,
; use CopyData instead.  CopyDataR can be used
; to shift data to the left in an array safely.
;
;pSource	[EBP + 20]
;pDest		[EBP + 16]
;cBytes		[EBP + 12]
;
PUBLIC __CopyDataR:
		PUSH EBP
		MOV EBP,ESP
		MOV EDI,[EBP+16]		;Load destination address
		MOV ESI,[EBP+20]		;Load source address
		MOV ECX,[EBP+12]		;Load count of bytes to move
		STD						;Auto Decrement
		ADD ESI, ECX			;Point to end of strings
		ADD EDI, ECX
		DEC ESI					;correct addresses after addition
		DEC EDI
		SHR ECX,1				;Check odd byte
		JNC NoByteR
		MOVSB					;Handle the odd byte
NoByteR:
		SHR ECX,1				;Check odd word
		JNC NoWordC
		MOVSW					;Handle the odd word
NoWordR:
		REP MOVSD				;Move all DWORDS that are left
		POP EBP
		RETF 12					;Pop args off the stack

;===============================================
;
; FillData(pDest, cBytes, bFill)
;
; pDestination is begining address to fill
; cBytes is the size of the fill area
; bFill is the byte value to fill with
;
; Trys to use DWORDS if it can.
;
;pDest		[EBP + 20]
;cBytes		[EBP + 16]
;bFill		[EBP + 12]
;
PUBLIC __FillData:
		PUSH EBP
		MOV	EBP,ESP
		MOV	AL, BYTE PTR [EBP+12]	;Byte to fill with
		MOV	AH,AL					;Set up to store DWords
		SHL EAX,8
		MOV AL,AH
		SHL EAX,8
		MOV AL,AH					;Byte is now in all four of EAX
		MOV EDI, DWORD PTR [EBP+20]	;Load destination address
		MOV	ECX, DWORD PTR [EBP+16]	;Load count of bytes to fill
		CLD							;Auto-increment
		SHR	ECX,1					;Check even/odd
		JNC NoByteF
		STOSB						;Handle the odd byte
NoByteF:
		SHR	ECX,1					;Check even/odd
		JNC NoWordF
		STOSW						;Handle the odd word
NoWordF:
		REP	STOSD					;Store FillCh in each DWORD
		POP	EBP
		RETF 12						;Pop args off the stack

;===============================================
; This routine does a NON case sensitive comparison of two strings up to
; the length specified.
; It returns -1 (0FFFFFFFFh) if all of the bytes are the same,
; otherwise it returns the offset of the first error.
;
; CompareNCS(pS1, pS2, dSize) :	returned offset or -1
;
; pS1 & pS2 pointer to strings to compare
; dSize is length to compare out to
;
; Case is only ignored in the letters of the ASCII alphabet (A-Z).
;
; pS1   [EBP+20]
; pS2   [EBP+16]
; dSize [EBP+12]
;
PUBLIC __CompareNCS:
		PUSH EBP					;Save calling frame
		MOV	EBP,ESP					;Set up my own frame pointer
		MOV ESI, [EBP+20]			;Load address of String1
		MOV EDI, [EBP+16]			;Load address of String2
		MOV	ECX, [EBP+12]			;Load count of bytes
		CLD							;Set auto-increment
CompNCS1:
		REP	CMPSB					;Compare strings...
		JZ NCSMatch					;If all bytes ok, .. go for it
NCSCase:
		MOV	AL,BYTE PTR [ESI-1]		;Get the p1 byte that failed
		OR	AL,20h					;Force it lower case
		CMP	AL,7Ah					;Greater than Little z ?
		JG NCSNoGo						;If yes, Not character
		CMP	AL,61h					;Less than little a ?
		JL NCSNoGo					;If yes, Not character
		MOV BL,BYTE PTR [EDI-1]		;get the p2 byte that failed
		OR BL,20h					;Force it lower case
		CMP AL,BL
		JNE NCSNoGo	            	;Still no match
		JECXZ SHORT NCSMatch		;ECX=0 no chars left to check
		JMP SHORT CompNCS1			;Back to the top
NCSNoGo:
		MOV EAX, [EBP+12]			;Calc offset of bad byte
		INC ECX						;Fix CX (DEC after LOOP)
		SUB EAX,ECX					;Leave in AX for return value
		JMP SHORT NCSDone
NCSMatch:
		MOV	EAX, -1					;Strings match
NCSDone:
		POP	EBP						;Restore callers frame ptr
		RETF 12						;Pop args off the stack

;===============================================
; This routine does a CASE SENSITIVE comparison of two strings up to
; the length specified.
; It returns -1 (0FFFFFFFFh) if all of the bytes are the same,
; otherwise it returns the offset of the first error.
;
; Compare(pS1, pS2, dSize) : returned offset or -1
;
; pDestination is begining address to fill
; cBytes is the size of the fill area
; bFill is the byte value to fill with
;
; pS1   [EBP+20]
; pS2   [EBP+16]
; dSize [EBP+12]
;
PUBLIC __Compare:
		PUSH EBP					;Save calling frame
		MOV	EBP,ESP					;Set up my own frame pointer
		MOV ESI, [EBP+20]			;Load address of String1
		MOV EDI, [EBP+16]			;Load address of String2
		MOV	ECX, [EBP+12]			;Load count of bytes
		CLD							;Set auto-increment
		REP	CMPSB					;Compare strings...
		JZ CompMatch				;If all bytes ok, .. go for it
		MOV EAX, [EBP+12]			;Calc offset of bad byte
		INC ECX						;Fix CX (DEC after LOOP)
		SUB EAX,ECX					;Leave in AX for return value
		JMP SHORT CompDone
CompMatch:
		MOV	EAX, -1					;Strings match
CompDone:
		POP	EBP						;Restore callers frame ptr
		RETF 12						;Pop args off the stack

;===================  Module End  ===============================
