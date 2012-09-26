;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0
.DATA

.INCLUDE MOSEDF.INC

.CODE
;========== DMA Equates for DMA amd PAGE registers =========
;NOTES:  With 8 bit DMA, the lower WORD (bits 15-0) are placed into
; the address registers of the DMA, and the Page register is the next
; most significant byte (bits 23-16). With WORD DMA moves (channels 5-7),
; address bits 16-1 are placed in the address registers, while 23-17 are
; put in the page register.  Bit 16 is ignored by the page register.
;
;
; DMA 1 Port addresses and page registers

DMA10Add	EQU  00h	;Ch 0 Address
DMA10Cnt	EQU  01h	;Ch 0 Word Count
DMA11Add	EQU  02h	;Ch 1 Address
DMA11Cnt	EQU  03h	;Ch 1 Word Count
DMA12Add	EQU  04h	;Ch 2 Address
DMA12Cnt	EQU  05h	;Ch 2 Word Count
DMA13Add	EQU  06h	;Ch 3 Address
DMA13Cnt	EQU  07h	;Ch 3 Word Count
DMA1StatCmd	EQU  08h	;Read Status/Write Command
DMA1RqReg	EQU  09h	;Read/Write DMA Rq Register
DMA1RCmdWbm	EQU  0Ah	;Read Command/Write Single bit mask
DMA1Mode	EQU  0Bh	;Read/Write Mode register
DMA1FF		EQU  0Ch	;Writing this address clears byte ptr flip flop
DMA1Clear	EQU  0Dh	;Write causes MASTER Clear (Read from Temp Reg)
DMA1ClrMode	EQU  0Eh	;Rd clears mode reg count/Wr Clr ALL mask bits
DMA1MskBts	EQU  0Fh	;Read/Write DMA Rq Mask Register

; DMA 2 Port addresses

DMA20Add	EQU  0C0h  	;Ch 0 Address
DMA20Cnt	EQU  0C2h  	;Ch 0 Word Count
DMA21Add	EQU  0C4h   ;Ch 1 Address
DMA21Cnt	EQU  0C6h   ;Ch 1 Word Count
DMA22Add	EQU  0C8h   ;Ch 2 Address
DMA22Cnt	EQU  0CAh   ;Ch 2 Word Count
DMA23Add	EQU  0CCh   ;Ch 3 Address
DMA23Cnt	EQU  0CEh   ;Ch 3 Word Count
DMA2StatCmd	EQU  0D0h   ;Read Status/Write Command
DMA2RqReg	EQU  0D2h   ;Read/Write DMA Rq Register
DMA2RCmdWbm	EQU  0D4h   ;Read Command/Write Single bit mask
DMA2Mode	EQU  0D6h   ;Read/Write Mode register
DMA2FF		EQU  0D8h   ;Writing this address clears byte ptr flip flop
DMA2Clear	EQU  0DAh   ;Write causes MASTER Clear (Read from Temp Reg)
DMA2ClrMode	EQU  0DCh   ;Rd clears mode reg count/Wr Clr ALL mask bits
DMA2MskBts	EQU  0DEh   ;Read/Write DMA Rq Mask Register

;DMA Page register by DRQ/DACK number

DMAPage0	EQU 87h		;DMA DACK0 Page register
DMAPage1	EQU 83h		;    DACK1 (etc. etc. etc.)
DMAPage2	EQU 81h
DMAPage3	EQU 82h
DMAPage5	EQU 8Bh
DMAPage6	EQU 89h
DMAPage7	EQU 8Ah
;
; The following code sets up the initial DMA channel values for
; both chips to most probable use. Includes cascade mode for CH4
; which is DMA channel 0 on chip 2
;
PUBLIC InitDMA:
	MOV AL, 04				;Master disable
	OUT	DMA1StatCmd, AL
	OUT	DMA2StatCmd, AL

	XOR AL, AL				;MASTER CLEAR (same as hardware reset)
	OUT DMA1Clear, AL
	OUT DMA2Clear, AL

	XOR AL, AL				;All commands set to default (0)
	OUT	DMA1StatCmd, AL
	OUT	DMA2StatCmd, AL

	MOV AL, 40h				;CH 0 DMA 1
	OUT	DMA1Mode, AL		;
	MOV AL, 0C0h			;CH 0 DMA 2  (Cascade Mode)
	OUT	DMA2Mode, AL		;

	MOV AL, 41h				;CH 1 DMA 1 & 2
	OUT	DMA1Mode, AL		;
	OUT	DMA2Mode, AL		;

	MOV AL, 42h				;CH 2 DMA 1 & 2
	OUT	DMA1Mode, AL		;
	OUT	DMA2Mode, AL		;

	MOV AL, 43h				;CH 3 DMA 1 & 2
	OUT	DMA1Mode, AL		;
	OUT	DMA2Mode, AL		;

	XOR AL,AL
	OUT DMA1ClrMode, AL  	;Enable ALL DMA 1 Channels
	OUT DMA2ClrMode, AL  	;Enable ALL DMA 2 Channels
	RETN

;
;-----------------------------------------------------------
; The following code sets up a single DMA channel for the caller.
; DMA is crippled because it can't move across 64K physical
; boundries. It's the caller's responsibility to know if he is
; crossing a boundry! If he does, this will wrap around over the segment!!!
; This code is completely reentrant.
; There is only one call for DMA set up.

; The caller sets the type of DMA operation (In, Out, Verify).
; For channels 5,6 & 7, the address & count must be divided by
; two for the DMA hardware. This routine does this for you!
; Algorythm - Put mode byte bits (except channel bits) in BL, then jump to
; correct routine for that channel. The call will allow set up of
; all the differnet types of DMA modes for read and write with this
; single call.	The call:
;
;	DmaSetUp(dPhyMem, sdMem, dChannel, dType, dMode)
; 	   EBP+  28       24     20        16     12
;
;	dPhyMem is physical memory address
;	sdMem is nBytes to move (STILL bytes for word xfers, we do math)
;	dChannel (0,1,2,3,5,6,7)
;	dType - 0 = Verify, 1 = In (Write Mem), 2 = Out (Read memory)
;	dMode - 0 Demand Mode, 1 Single Cycle,     (Disk I/O uses 1)
;			2 Block, 3 Cascade
;
;
PUBLIC __DMASetUp:
		PUSH EBP                ;
		MOV EBP,ESP             ;
		MOV EAX, [EBP+12]		; dMode
		CMP EAX, 04				; Mode must be < 4
		JB DMAModeOK
		MOV EAX, ErcDMAMode
		JMP DMAEnd
DMAModeOK:
		SHL EAX, 6				; Move Mode bits to 6 and 7
		MOV BL, AL				; Put it in BL
		MOV EAX, [EBP+16]		; get dType
		AND BL, 0C0h			; Set to Verify by default (low bits 0)
		CMP EAX, 0				; Check fType (Verify?)
		JE DMASelect			; Yes
		CMP EAX, 1				; In? (Write)
		JE DMAWrite				; Yes (if no then fall thru to Read)
DMARead:
		OR BL,	00001000b		; OR read command to BL (OUT)
		JMP DMASelect
DMAWrite:
		OR BL, 00000100b		; OR write command to BL (IN)

DMASelect:
		MOV EAX, [EBP+20]		;Jump table for channel selection
		CMP EAX, 0				;
		JE DMA0
		CMP EAX, 1
		JE DMA1
		CMP EAX, 2
		JE DMA2
		CMP EAX, 3
		JE DMA3
		CMP EAX, 5
		JE DMA5
		CMP EAX, 6
		JE DMA6
		CMP EAX, 7
		JE DMA7
		MOV EAX, ErcDMAChannel
		JMP DMAEnd

DMA0:
		CLI
		MOV AL,	00000100b		; channel 0 Set Mask for DRQ
		OUT DMA1RCmdWbm, AL		;
		MOV AL,	00000000b		; CH0 (CH is last 2 bits)
		OR  AL, BL				; OR with MODE
		OUT DMA1Mode, AL
		OUT DMA1FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		OUT DMA10Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA10Add, AL		; Hi Byte
		SHR EAX, 8
		OUT DMAPage0, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]
		DEC EAX
		OUT DMA10Cnt, AL
		SHR EAX, 8
		OUT DMA10Cnt, AL
		MOV AL,	00000000b		; channel 0 Clear Mask for DRQ
		OUT DMA1RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd

DMA1:
		CLI
		MOV AL,	00000101b		; channel 1 Set Mask for DRQ
		OUT DMA1RCmdWbm, AL
		MOV AL, 00000001b		; CH1
		OR  AL, BL				; OR with MODE/TYPE
		OUT DMA1Mode, AL
		OUT DMA1FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		OUT DMA11Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA11Add, AL		; Hi Byte
		SHR EAX, 8
		OUT DMAPage1, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		DEC EAX
		OUT DMA11Cnt, AL
		SHR EAX, 8
		OUT DMA11Cnt, AL
		MOV AL,	00000001b		; channel 1 Clear Mask for DRQ
		OUT DMA1RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd

DMA2:
		CLI
		MOV AL,	00000110b		; channel 2 Set Mask for DRQ
		OUT DMA1RCmdWbm, AL
		MOV AL, 00000010b		; CH2
		OR  AL, BL				; OR with MODE
		OUT DMA1Mode, AL
		OUT DMA1FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		OUT DMA12Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA12Add, AL		; Hi Byte
		SHR EAX, 8
		OUT DMAPage2, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		DEC EAX
		OUT DMA12Cnt, AL
		SHR EAX, 8
		OUT DMA12Cnt, AL
		MOV AL,	00000010b		; channel 2 Clear Mask for DRQ
		OUT DMA1RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd

DMA3:
		CLI
		MOV AL,	00000111b		; channel 3 Set Mask for DRQ
		OUT DMA1RCmdWbm, AL
		MOV AL, 00000011b		; CH3
		OR  AL, BL				; OR with MODE
		OUT DMA1Mode, AL
		OUT DMA1FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		OUT DMA13Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA13Add, AL		; Hi Byte
		SHR EAX, 8
		OUT DMAPage3, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		DEC EAX
		OUT DMA13Cnt, AL
		SHR EAX, 8
		OUT DMA13Cnt, AL
		MOV AL,	00000011b		; channel 3 Clear Mask for DRQ
		OUT DMA1RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd

;NOTE: DMA channels 5-7 are on DMA2 and numbered 1-3 for chip select purposes

DMA5:
		CLI
		MOV AL,	00000101b		; channel 1 DMA2 Set Mask for DRQ
		OUT DMA2RCmdWbm, AL
		MOV AL, 00000001b		; CH1 on DMA 2
		OR  AL, BL				; OR with MODE
		OUT DMA2Mode, AL
		OUT DMA2FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		MOV EBX, EAX			; Save EBX for page
		AND EAX, 0FFFFh			; Rid of all but lower 16
		SHR EAX, 1				; DIV by 2 for WORD Xfer (bits 16-1)
		OUT DMA21Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA21Add, AL		; Hi Byte
		MOV EAX, EBX
		SHR EAX, 15				; We only need 23-17 for the page
		OUT DMAPage5, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		SHR EAX, 1				; DIV by 2 for WORD Xfer
		DEC EAX					; One less word
		OUT DMA21Cnt, AL
		SHR EAX, 8
		OUT DMA21Cnt, AL
		MOV AL, 00000001b		; channel 1 Clear Mask for DRQ
		OUT DMA2RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd
;
DMA6:
		CLI
		MOV AL, 00000110b		; channel 2 Set Mask for DRQ
		OUT DMA2RCmdWbm, AL
		MOV AL, 00000010b		; CH2 on DMA 2
		OR  AL, BL				; OR with MODE
		OUT DMA2Mode, AL
		OUT DMA2FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		MOV EBX, EAX
		AND EAX, 0FFFFh			; Rid of all but lower 16
		SHR EAX, 1				; DIV by 2 for WORD Xfer
		OUT DMA22Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA22Add, AL		; Hi Byte
		MOV EAX, EBX
		SHR EAX, 15
		OUT DMAPage6, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		SHR EAX, 1				; DIV by 2 for WORD Xfer
		DEC EAX
		OUT DMA22Cnt, AL
		SHR EAX, 8
		OUT DMA22Cnt, AL
		MOV AL,	00000010b		; channel 2 Clear Mask for DRQ
		OUT DMA2RcmdWbm, AL
		STI
		XOR EAX, EAX
		JMP DMAEnd
;
DMA7:
        CLI
		MOV AL,	00000111b		; channel 3 Set Mask for DRQ
		OUT DMA2RCmdWbm, AL
		MOV AL, 00000011b		; CH3 on DMA 2
		OR  AL, BL				; OR with MODE
		OUT DMA2Mode, AL
        OUT DMA2FF, AL			; Clear FlipFLop (Val in AL irrelevent)
		MOV EAX, [EBP+28]		; dPhyMem
		MOV EBX, EAX
		AND EAX, 0FFFFh			; Rid of all but lower 16
		SHR EAX, 1				; DIV by 2 for WORD Xfer
		OUT DMA23Add, AL		; Lo byte address
		SHR EAX, 8
		OUT DMA23Add, AL		; Hi Byte
		MOV EAX, EBX
		SHR EAX, 15
		OUT DMAPage6, AL		; Highest byte (to page register)
		MOV EAX, [EBP+24]		; sdMem
		SHR EAX, 1				; DIV by 2 for WORD Xfer
		DEC EAX
		OUT DMA23Cnt, AL
		SHR EAX, 8
		OUT DMA23Cnt, AL
		MOV AL,	00000011b		; channel 3 Clear Mask for DRQ
		OUT DMA2RcmdWbm, AL
		STI
		XOR EAX, EAX
DMAEnd:
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 20                 ; 20 bytes of crap to dump

;------------------------------------------------------------
; GetDMACount returns the number of bytes or words left in the
; DMA count register for the channel specified.  For channels
; 5-7 this will be the number of WORDS. For 0-3 this will be BYTES!
; Programmers should note (I should tell them) this value will read
; one less byte/word than is really in the channel. This is because
; 0 = 1 for set purposes. to move 64K, you actually set the
; channel 65,535.
;
;	GetDMACount(dChannel, pwCountRet)
; 	   EBP+     16        12
;
;	dChannel (0,1,2,3,5,6,7)
;	pwCountRet is a pointer to a Word (2 byte unsigned value) where
;       the count will be returned. The count is number of WORDS-1
;       for channels 5-7 and BYTES-1 for channels 0-3.

PUBLIC __GetDMACount:
		PUSH EBP           		;
		MOV EBP,ESP             ;
		MOV EAX, [EBP+16]		;Channel
		MOV ESI, [EBP+12]		;Return address for count
DMACSelect:
		CMP EAX, 0				;
		JNE SHORT DMAC1
		MOV DX, DMA10Cnt   		;
		JMP SHORT DMACDoIt
DMAC1:
		CMP EAX, 1
		JNE SHORT DMAC2
		MOV DX, DMA11Cnt   		;
		JMP SHORT DMACDoIt
DMAC2:
		CMP EAX, 2
		JNE SHORT DMAC3
		MOV DX, DMA12Cnt   		;
		JMP SHORT DMACDoIt
DMAC3:
		CMP EAX, 3
		JNE SHORT DMAC5
		MOV DX, DMA13Cnt   		;
		JMP SHORT DMACDoIt
DMAC5:
		CMP EAX, 5
		JNE SHORT DMAC6
		MOV DX, DMA21Cnt   		;
		JMP SHORT DMACDoIt
DMAC6:
		CMP EAX, 6
		JNE SHORT DMAC7
		MOV DX, DMA22Cnt   		;
		JMP SHORT DMACDoIt
DMAC7:
		MOV DX, DMA23Cnt   		;
		CMP EAX, 7
		JE SHORT DMACDoIt
		MOV EAX, ErcDMAChannel	;No such channel!
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ; 20 bytes of junk to dump

DMACDoIt:
		CLI
		IN  AL,DX           	;
		MOV CL,AL           	;
		IN  AL,DX           	;
		MOV CH,AL           	;CX has words/bytes left in DMA
		STI						;
		MOV WORD PTR [ESI], CX			;
		XOR EAX, EAX			; No Error
		MOV ESP,EBP             ;
		POP EBP                 ;
		RETF 8                  ; 8 bytes to dump from the stack

;=========== module end ==============
