.386P
;This boot sector is STUFFed to the gills to do a single
;stage boot of the MMURTL OS which is about 160K stored as
;a loadable image beginning at cluster 2 on the disk. The OS must
;be stored contiguously in each of the following logical sectors.
;The actual number of sectors is stored in the data param nOSSectors.

CSEG    SEGMENT WORD 'Code' USE16
        ASSUME CS:CSEG, DS:CSEG, ES:Nothing

ORG     0h

	JMP SHORT Bootup
	NOP

;This 59 byte structure follows the 3 jump bytes above
;and is found on all MS-DOS FAT compatible
;disks. This contains additional information about the file system
;on this disk that the operating needs to know. The boot sector code also
;needs some of this information if this is a bootable disk.

Herald          DB   'MURTL1.0'
nBytesPerSect   DW   0200h		;nBytes/Sector
nSectPerClstr   DB   01h		;Sect/Cluster
nRsvdSect       DW   0001h		;Resvd sectors
nFATS           DB   02			;nFATs
nRootDirEnts    DW   00E0h		;Root Dir entries max
nTotalSectors   DW   0B40h		;nTotal Sectors (0 = <32Mb)
bMedia          DB   0F0h		;media desc. (worthless)
nSectPerFAT     DW   0009h		;nSect in FAT
nSectPerTrack   DW   0012h		;nSectors/track
nHeads          DW   0002h		;nHeads
nHidden         DD   00000000h	;nHidden Sectors (first whole track on HD)
nTotalSect32    DD   00000000h	;nTotalSectors if > 32Mb
bBootDrive		DB   00h		;Drive boot sector came from
ResvdByte    	DB   00h		;Used for temp storage of Sector to Read
ExtBootSig   	DB   29h		; Ext Boot Signature (always 29h)
nOSSectors		DW   0140h      ; (140 sectors max) Was Volume ID number
ResvdWord		DW   0000h
Volname    		DB   'RICH       '	;11 bytes for volume name
FatType    		DB   'FAT12   '		;8 bytes for FAT name (Type)


;The following are pointers to my IDT and GDT after my OS loads
; and are not part of the above boot sector structure.

IDTptr          DW 7FFh                 ;LIMIT 256 IDT Slots
	            DD 0000h                ;BASE (Linear)

GDTptr          DW 17FFh                ;LIMIT 768 slots
                DD 0800h                ;BASE (Linear)

;This is where we jump from those first 3 bytes

BootUp:

;Boot blocks first instruction starts here after initial jump from beginning
    CLI					;Clear interrupts

	;Stick the stack at 98000h (an arbitrary location)

    MOV  AX,9000h
    MOV  SS, AX
	MOV  SP, 8000h

;Move this boot sector UP to 90000h Linear.

	MOV  AX, 09000h
	MOV  ES, AX
	XOR  DI, DI
	MOV	 AX, 7C0h
	MOV  DS, AX
	XOR  SI, SI
	MOV  CX, 512
    REP  MOVSB

	; Now we jump UP to where we moved it.

	MOV AX, 09000h		;Segment
	PUSH AX
	MOV AX, 6Fh			;Offset to new location
	PUSH AX
	RETF

; Now set DS equal to ES which is 9000h
	PUSH ES
	POP  DS
	MOV CX, nSectPerTrack

	XOR AX, AX
	MOV DS, AX
    MOV  BX, 0078h	   ;Int 1E FDC Params!
    LDS  SI, DS:[BX]

    MOV  BYTE PTR [SI+4], CL
	MOV  BYTE PTR [SI+9], 0Fh

	PUSH ES
	POP DS
	PUSH DS

    STI

	MOV DL, bBootDrive		;Required for Disk System Reset
	XOR AX, AX

    INT  13h				;Reset Disk Controller (DL has drive num)
    JC   SHORT BadBoot		;Reset failed...

	POP DS

    ;The controller is reset, now let's read some stuff!!
	;We are gonna skip checking to see if the first file
	;really IS the OS. We need the space for other code.

    MOV  SI, OFFSET MsgLoad
    CALL PutChars

	;What we do now is calcualte our way to the third cluster
	;on the disk and read in the total number of OS sectors in
	;logical sector order. (3rd cluster is really the first allocated
	; cluster because first 2 are unused).
	;The layout of the Disk is:
	;	Boot Sector (at logical sector 0)
	;   Hidden Sectors (optional)
	;	FATS (1 or more)
	;   Additional Reserved sectors (optional)
	;	Root Directory (n Sectors long)

	XOR AX, AX
    MOV  AL, nFATS
    MUL  WORD PTR nSectPerFAT
    ADD  AX, WORD PTR nHidden	;
    ADC  DX, WORD PTR nHidden+2
    ADD  AX, nRsvdSect
	MOV  CX, AX				;Save in CX

	;CX now has a Word that contains the sector of the Root

	;Calculate the size of the root directory and skip past it
	;to the first allocated sectors (this is where the OS or
	;stage one of the a two stage loader should be).

    MOV  AX,0020h 			;Size of Dir Entry
    MUL  WORD PTR nRootDirEnts
    MOV  BX, nBytesPerSect
    DIV  BX
	ADD  AX, CX

	;AX is at sector for cluster 0, but cluster 0 and 1 don't exist
	;so we are really at cluster 2 like we want to be.

    MOV  CX, nOSSectors	;Number of OS sectors to read
	JMP SHORT ContinueBoot

;Bad boot goes here and displays a message then
;waits for a key to reboot (or tries to) via int 19h

BadBoot:
    MOV  SI, OFFSET MsgBadDisk
    CALL PutChars

    XOR  AX,AX
    INT  16h		;Wait for keystroke
    INT  19h        ;Sys Reboot

PutChars:
    LODSB
    OR   AL,AL
    JZ   SHORT Done
    MOV  AH, 0Eh
    MOV  BX,0007
    INT  10h
    JMP  SHORT PutChars
Done:
	RETN

ContinueBoot:
    MOV  BX, 06000h    ;This is segment where we load the OS.
    MOV  ES, BX

NextSector:
    PUSH AX
    PUSH CX
    PUSH DX
	PUSH ES

	XOR BX, BX

; Read a logical sector to ES:BX
; AX has Logical Sector Number
;
	MOV  SI, nSectPerTrack
    DIV  SI					;Divide LogicalSect by nSectPerTrack
    INC  DL					;Sector numbering begins at 1 (not 0)
    MOV  ResvdByte, DL			;Sector to read
    XOR  DX, DX				;Logical Track left in AX
    DIV  WORD PTR nHeads	;Leaves Head in DL, Cyl in AX
	MOV  DH, bBootDrive
	XCHG DL, DH				;Head to DH, Drive to DL
	MOV  CX, AX				;Cyl into CX
	XCHG CL, CH				;Low 8 bits of Cyl to CH, Hi 2 bits to CL
	SHL  CL, 6				;  shifted to bits 6 and 7
	OR   CL, BYTE PTR ResvdByte	;OR with Sector number
	MOV  AL, 1				;Number of sectors
    MOV  AH, 2				;Read
    INT  13h				;Read that sucker!
    JC   SHORT BadBoot

    MOV  SI, OFFSET MsgDot
    CALL PutChars

    POP  ES
    POP  DX
    POP  CX
    POP  AX

	MOV  BX, ES
	ADD  BX, 20h	;512 bytes for segment
	MOV  ES, BX
	INC  AX			;Next Sector
    LOOP NextSector

	;At this point we have the OS loaded in a contigous section
	;from 60000 linear up to about 80000 linear.
	;Now we disable interrupts, turn on the A20 line, move
	;it down to address 0, set protected mode and JUMP!

	CLI
	XOR CX,CX
IBEmm0:
	IN AL,64h
	TEST AL,02h
	LOOPNZ IBEmm0
	MOV AL,0D1h
	OUT 64h,AL
	XOR CX,CX
IBEmm1:
	IN AL,64h
	TEST AL,02h
	LOOPNZ IBEmm1
	MOV AL,0DFh
	OUT 60h,AL
	XOR CX,CX
IBEmm2:
	IN AL,64h
	TEST AL,02h
	LOOPNZ IBEmm2

	;A20 line should be ON Now
	;So move the OS

	; Set up our new DS to where we moved the data
	; We must do this before each 32K load cause we use DS */

	MOV DX, 8000h

	; Move 64K data chunk from linear 60000h to linear 0

	MOV BX, 06000h
	MOV DS, BX
	XOR SI, SI
	XOR AX, AX
	MOV ES,AX
	XOR DI,DI
	MOV CX, DX
	CLD                    	;
	REP MOVSW				;WORD move

	; Move first 64K code chunk from linear 70000h to 10000h

	MOV BX, 07000h
	MOV DS, BX
	XOR SI, SI
	MOV AX,1000h
	MOV ES,AX
	XOR DI,DI
	MOV CX, DX
	REP MOVSW				;WORD move

	; Move last code (32K) from linear 80000h to 18000h

	MOV DS, DX				;DX is 8000h anyway
	XOR SI, SI
	MOV AX,2000h
	MOV ES,AX
	XOR DI,DI
	MOV CX, DX
	REP MOVSB				;BYTE move

	MOV BX, 9000h
	MOV DS, BX

	XOR EDX, EDX
	MOV DL, bBootDrive		;OS can find bootdrive in DL on entry

	LIDT FWORD PTR IDTptr
	LGDT FWORD PTR GDTptr

	MOV EAX,CR0
	OR AL,1
	MOV CR0,EAX
	JMP $+2
	NOP
	NOP

    MOV BX, 10h
	MOV DS,BX
	MOV ES,BX
	MOV FS,BX
	MOV GS,BX
	MOV SS,BX

	;We define a far jump with 48 bit pointer manually

	DB 66h
	DB 67h
	DB 0EAh
	DD 10000h
	DW 8h

MsgNone     DB '                                '
MsgBadDisk  DB 0Dh, 0Ah, 'Bad Boot Disk!', 00h
MsgLoad     DB 0Dh, 0Ah, 'Loading MMURTL', 00h
MsgDot      DB '.', 00h

BootSig   DW 0AA5Fh

CSEG    ENDS
        END

;Character Message stuff to save for troubleshooting
;	MOV BX, 0B800h
;	MOV ES, BX
;	XOR BX,BX
;	MOV WORD PTR ES:[BX], 4730h
