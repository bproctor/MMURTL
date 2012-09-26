;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0
;
;This defines the Interrupt Desecriptor Table
;
PUBLIC IDT	DD	512 DUP (0)	;2K for IDT (0.5 pages, 1K) @ 00000000h Physcial
;
; ========== Module End ===========
