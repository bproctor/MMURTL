;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0

;This is MMURTL's page directory and initial page table.
;The page directory has one entry in it which is the linear address
;of it's only page table.  The page table has just enough entries
;in it to cover the OS Data, Stack and Code.
;Note that the page directory actual becomes assigned to the Monitor program
;because the OS code isn't actually a task.  Remember, OS code runs in OTHER
;job's tasks.  The monitor is the first true job launched, which can
;install services, launch programs, report errors on termination, etc.

;The page directory is 4K and is ALWAYS PAGE ALIGNED
;We fill in the first page directory entry statically, then the
;InitMem routine fills in the page table for us.
;
;A page directory or table entry with all zeros means "not used yet."
;
;The AVL bits are for OS use and are defined as follows:
;	A - 1 = Alias of someone elses physical memory
;	V - 1 = Undefined (use later for virtual memory mgmt)
;	L - 1 = Undefined (use later for virtual memory mgmt)
;The other bits are Hardware defined and set as follows:
;	D - 1 = Dirty (written to) since created.  CPU sets this.
;	A - 1 = Accessed since created. CPU sets this.
;	U - 1 = User,  0 = Supervisor. OS sets this.
;	W - 1 = Writable for user (super pages are always writable to the OS)
;	P - 1 = Present. CPU will not read or mod entry if this is Zero
;
;
;					20 Bit Address		AVL00DA00UWP
;					|					|  | ||  |||
PUBLIC PDir1	DD	00000000000000000011000000000101b	;PT physical address
				DD  511 DUP (0)							;511 BLANK entries

				DD	00000000000000000011000000000000b	;Shadow for PT Linear address
				DD  511 DUP (0)							;511 BLANK entries

PUBLIC PTbl1	DD  1024 DUP (0)						;1024 blank entries


;-------------------- END OF MODULE ----------------------
