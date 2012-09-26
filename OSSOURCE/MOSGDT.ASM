;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1,0
.DATA
.INCLUDE MOSEDF.INC

;=============================================================================
;
; All the static GDT descriptors below are defined as shown on page
; 2-5 of the 80386 System Software Writer's Guide.
; Comments next	to each of the entries mean:
;
; Limit - 00:15 DW A WORD (16 bits) with LSW of the Linear Segment Limit
; Base  - 00:15 DW A WORD (16 bits) with LSW of the Linear Segment Base
; Base  - 16:23 DB A BYTE (8 bits) with MSB of the Linear Segment Base
; ProtType      DB P/DPL/1/CRA or EWA/ Protection, Type, etc...
; GranSizeLim   DB Granularity/Size for code/
; Base  - 24:31 DB A BYTE (8 bits) with MSB of the Linear Segment base
;
;=============================================================================
;Sel 00h
PUBLIC GDT	DD 2 DUP (0h)		;The first desc is always null

;Sel 08
OSCodeDesc	DW 0FFFFh			; Limit 15-0 (4Gb)
			DW 0000h			; Base  15-0
			DB 00h				; Base  23-16	;10000h
			DB 10011010b		; P(1) DPL(00) 1 1 C(0) R(1) A(0)
			DB 11001111b		; G(1) D(1) 0 0 Limit[19-16]
			DB 00h				; Base  31-24

;Sel 10h  - This the OS and User Data descriptor.

DataDesc	DW 0FFFFh			;Limit (bits 0:15) at linear 00K
			DW 0000h			;base (bits 15:0)
			DB 00h				;base (bits 23:16)
			DB 10010010b		;P(1) DPL(00) 1 0 E(0) W(1) A(0)
			DB 11001111b		;G(1), B(1) 0 0 limit[19-16)
			DB 00h				;Base at 0 (bits 31:24)

;Sel 18h - This is the user's code descriptor.

CodeDesc	DW 0FFFFh			; Limit 15-0 (0FFFFh)
			DW 0000h			; Base  15-0
			DB 00h				; Base  23-16
			DB 10011010b		; P(1)-DPL(00)-1-1-C(0)-R(1)-A(0)
			DB 11001111b		; G(1) D(1) 0 0 Limit[19-16]
			DB 00h				; Base  31-24

;Save these for the future...

OSXtra0Desc	DD 0, 0		;Sel 20h
OSXtra1Desc	DD 0, 0		;Sel 28h
OSXtra2Desc	DD 0, 0		;Sel 30h
OSXtra3Desc	DD 0, 0		;Sel 38h

;WARNING: The rgCall Descriptors must never be moved from this location
;	  in the GDT. They describe system entry points that will become
;	  PUBLIC for external programs.	The first PUBLIC call gate is
;	  selector 40h.

;Sel 40

PUBLIC rgCallDesc	DD (nCallGates*2) DUP (0)	;Call Gates

PUBLIC rgTSSDesc	DD (nTSS*2) DUP (0) 		;Task descriptors


;--------- END OF MODULE ------------------------------
