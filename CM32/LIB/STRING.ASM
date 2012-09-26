
.DATA


.CODE
PUBLIC _strcmp:
	PUSH EBP
	MOV EBP,ESP
	MOV ESI,[EBP+12]
	MOV EDI,[EBP+8]
strcmp0:
	MOV AL,[ESI]
	CMP AL,BYTE PTR [EDI]
	JG strcmp1
	JL strcmp2
	CMP AL, 0
	JE strcmp3
	INC DI
	INC SI
	JMP SHORT strcmp0
strcmp1:
	MOV EAX, 1
	JMP SHORT strcmp4
strcmp2:
	MOV EAX, -1
	JMP SHORT strcmp4
strcmp3:
	XOR EAX,EAX
strcmp4:

	POP EBP
	RETN 8
PUBLIC _strncmp:
	PUSH EBP
	MOV EBP,ESP
	MOV ESI,[EBP+16]	;str1
	MOV EDI,[EBP+12]	;str2
	MOV ECX,[EBP+8]		;n
strncmp0:
	CMP ECX, 0			;Equal so far?
	JE strncmp3			;Yes, we're done
	DEC ECX				;One less
	MOV AL,[ESI]		;
	CMP AL,BYTE PTR [EDI]
	JG strncmp1
	JL strncmp2
	CMP AL, 0			;End of String?
	JE strncmp3			;Equal up to here!
	INC EDI				;Next chars
	INC ESI
	JMP SHORT strncmp0	;Back again
strncmp1:
	MOV EAX, 1
	JMP SHORT strncmp4
strncmp2:
	MOV EAX, -1
	JMP SHORT strncmp4
strncmp3:
	XOR EAX,EAX
strncmp4:

	POP EBP
	RETN 12
PUBLIC _strcpy:
	PUSH EBP
	MOV EBP,ESP
	MOV EDI,[EBP+12]		;destination s
	MOV ESI,[EBP+8]			;source ct
	CLD
strcpy0:
	MOVSB
	CMP BYTE PTR [ESI], 0
	JNZ strcpy0
	MOVSB					;Move the null too!
	MOV EAX,[EBP+12]		;return s
	POP EBP
	RETN 8
PUBLIC _strncpy:
	PUSH EBP
	MOV EBP,ESP
	MOV EDI,[EBP+16]		;destination *s
	MOV ESI,[EBP+12]		;source *ct
	MOV ECX,[EBP+8]			;max mov  n
	CLD
strncpy0:
	CMP ECX, 0				;End yet??
	JE strncpy2				;Yes
	DEC ECX					;One less
	MOVSB					;Move it
	CMP BYTE PTR [ESI], 0	;End of Source?
	JNZ strncpy0			;No, go back
strncpy1:
	CMP ECX,0
	JE strncpy2
	INC EDI
	MOV BYTE PTR [EDI], 0
	DEC ECX
	JMP SHORT strncpy1
strncpy2:
	MOV EAX,[EBP+16]

	POP EBP
	RETN 12
PUBLIC _strlen:
	PUSH EBP
	MOV EBP,ESP
	XOR EAX, EAX
	MOV ESI,[EBP+8]
_strlen0:
	CMP BYTE PTR [ESI],0
	JE _strlen1
	INC ESI
	INC EAX
	JMP SHORT _strlen0
_strlen1:
	POP EBP
	RETN 4
PUBLIC _strncat:
	PUSH EBP
	MOV EBP,ESP
	MOV EDI,[EBP+16]		;destination *s
	MOV ESI,[EBP+12]		;source *ct
	MOV ECX,[EBP+8]			;max mov  n
	CLD
strncat00:					;get to end of s
	CMP BYTE PTR [EDI], 0
	JE strncat0
	INC EDI
	JMP SHORT strncat00
strncat0:
	CMP ECX, 0				;End yet??
	JE strncat2				;Yes
	DEC ECX					;One less
	MOVSB					;Move it
	CMP BYTE PTR [ESI], 0	;End of Source?
	JNZ strncat0			;No, go back
strncat1:
	CMP ECX,0
	JE strncat2
	INC EDI
	MOV BYTE PTR [EDI], 0
	DEC ECX
	JMP SHORT strncat1
strncat2:
	MOV EAX,[EBP+16]		;Return s

	POP EBP
	RETN 12
PUBLIC _strcat:
	PUSH EBP
	MOV EBP,ESP
	MOV EDI,[EBP+12]	;destination *s
	MOV ESI,[EBP+8]		;source *ct
	CLD
strcat0:					;get to end of s
	CMP BYTE PTR [EDI], 0
	JE strcat1
	INC EDI
	JMP SHORT strcat0
strcat1:
	MOVSB					;Move it
	CMP BYTE PTR [ESI], 0	;End of Source?
	JNZ strcat1				;No, go back for more
	MOVSB					;Yes, Null terminte
	MOV EAX,[EBP+12]		;Return s
	POP EBP
	RETN 8
PUBLIC _strchr:
	PUSH EBP
	MOV EBP,ESP
L_1:
	MOV ESI,DWORD PTR [EBP+12]
	MOVSX EAX,BYTE PTR [ESI]
	CMP EAX,[EBP+8]
	SETE AL
	AND AL,AL
	JZ L_4
	MOV EAX,[EBP+12]
	JMP L_5
L_4:
L_2:
	MOV ESI,DWORD PTR [EBP+12]
	INC DWORD PTR [EBP+12]
	MOVSX EAX,BYTE PTR [ESI]
	AND EAX,EAX
	JNZ L_1
L_3:
	XOR EAX,EAX
L_5:
	POP EBP
	RETN 8
