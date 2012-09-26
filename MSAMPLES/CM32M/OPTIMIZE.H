/*
 * Peephole Optimizer for the C Minus32 386 compiler.
 *
 * The strategy behind the peephole optimizer is to keep the peephole
 * buffer completly filled.  The list of optimizations is in priority order
 * in the table below. Each buffer full is checked against each entry
 * in the table.  Even after a substitution is made, the new substituted
 * code is left in the buffer to check for further optimization.
 * See the rules for table entries
 * just prior to the table below.  All entries are searched even after
 * new code is inserted to ensure all optimization sequences used.
 *
 * Copyright 1990 Dave Dunfield
 * Copyright 1992 R.A. Burgess
 */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define	OBUF_SIZE		10		/* number of entries in peephole buffer */
#define	OLINE_SIZE		100		/* maximum size of input line */
#define OSYMBOLS		8		/* maximum # symbols per peep */
#define OSYMBOL_SIZE	25		/* maximum size of symbol */

/*
 * Peephole optimization table:
 *
 * Each two entries represent the instruction sequences:
 * The first is the string of instructions to SEARCH for,
 * the second is the optimized REPLACEMENT string.
 *
 * 1) The REPLACEMENT instructions are in reverse order!
 * 2) The highest priority entries should be entered first in the table.
 * 3) Tabs (\t) should begin each entry.
 * 4) Linefeeds (\n) should end each except for the last entry of a line.
 *
 */

char *peep_table[] = {

/* 80386/80486 Optimizations */

/* data movement & register usage optimizations */

"\tMOV \200,\201\n\tMOV \201,\200",
"\tMOV \200,\201",

"\tMOV \200,\201\n\tMOV \200,\202",
"\tMOV \200,\202",

"\tMOV \200,\201\n\tMOV \202,\200\n\tMOV \201,\200",
"\tMOV \202,\200\n\tMOV \200,\201",

"\tMOV EAX,\200\n\tMOV ECX,EAX",
"\tMOV ECX,\200",

"\tMOV ESI,\200\n\tMOV EAX,ESI",
"\tMOV EAX,\200",

"\tLEA ESI,\200\n\tMOV EAX,ESI",
"\tLEA EAX,\200",

"\tPUSH EAX\n\tLEA ESI,\200\n\tMOV EAX,ESI\n\tPOP EBX\n\tADD EAX,EBX",
"\tADD EAX,ESI\n\tLEA ESI,\200",

"\tPOP EBX\n\tMOV EAX,EBX",
"\tPOP EAX",

/* indexing operations */

"\tMOV ECX,32\n\tMUL ECX",
"\tSHL EAX,5",

"\tMOV ECX,32\n\tIMUL ECX",
"\tSHL EAX,5",

"\tMOV ECX,16\n\tMUL ECX",
"\tSHL EAX,4",

"\tMOV ECX,16\n\tIMUL ECX",
"\tSHL EAX,4",

"\tMOV ECX,8\n\tMUL ECX",
"\tSHL EAX,3",

"\tMOV ECX,8\n\tIMUL ECX",
"\tSHL EAX,3",

"\tMOV ECX,4\n\tMUL ECX",
"\tSHL EAX,2",

"\tMOV ECX,4\n\tIMUL ECX",
"\tSHL EAX,2",

"\tMOV ECX,2\n\tMUL ECX",
"\tSHL EAX,1",

"\tMOV ECX,2\n\tIMUL ECX",
"\tSHL EAX,1",

/* jump optimizations */

"\tJMP \200\n\200:",
"\200:",

"\tJMP \200\n\201:\n\200:",
"\200:\n\201:",

"\tJMP \200\n\tJMP \201",
"\tJMP \200",

"\tJNZ \200\n\tJMP \201\n\200:\n\tJMP \202\n\201:",
"\201:\n\tJMP \202\n\tJZ \201",

"\tJZ \200\n\tJMP \201\n\200:\n\tJMP \202\n\201:",
"\201:\n\tJMP \202\n\tJNZ \201",

"\tJNZ \200\n\tJMP \201\n\200:\n\tJMP SHORT \202",
"\tJMP \201\n\tJNZ \202",

"\tJZ \200\n\tJMP \201\n\200:\n\tJMP SHORT \202",
"\tJMP \201\n\tJZ \202",

/* conversion optimizations */

"\tMOV AL,\200\n\tMOVSX EAX,AL",
"\tMOVSX EAX,BYTE PTR \200",

"\tMOV AL,\200\n\tMOVZX EAX,AL",
"\tMOVZX EAX,BYTE PTR \200",

"\tMOV AX,\200\n\tMOVSX EAX,AX",
"\tMOVSX EAX,WORD PTR \200",

"\tMOV AX,\200\n\tMOVZX EAX,AX",
"\tMOVZX EAX,WORD PTR \200",

/* comparisons to ECX, or MOV into EAX for compare (when not needed) */

"\tMOV ECX,\200\n\tCMP EAX,ECX",
"\tCMP EAX,\200",

"\tMOVSX ECX,BYTE PTR\200\n\tCMP EAX,ECX",
"\tCMP AL,BYTE PTR\200",

"\tMOVZX ECX,BYTE PTR\200\n\tCMP EAX,ECX",
"\tCMP AL,BYTE PTR\200",

/* stack/parameters */

"\tMOV EAX,[\200\n\tPUSH EAX",
"\tPUSH DWORD PTR [\200",

"\tMOV EAX,\200\n\tPUSH EAX",
"\tPUSH \200",

/* more simple optimizations */

"\tMOV EAX,0",
"\tXOR EAX,EAX",

"\tMOV AX,0",
"\tXOR AX,AX",

"\tMOV AL,0",
"\tXOR AL,AL",

"\tMOV AL,0",
"\tXOR AL,AL",

0 };

/* circular peep hole buffer & read/write pointers */

	char peep_buffer[OBUF_SIZE][OLINE_SIZE];

	unsigned peep_top = 0,		/* first entry in circular buffer */
			 peep_next = 0;		/* Next place to put new entry    */

	/* If ((next+1) mod n entries) == top then buffer is full */
	/* If next == top then buffer is empty */

/* Symbol table */

	char symbols[OSYMBOLS][OSYMBOL_SIZE];

/* End of Optimize.h */

/* candidates
OLD:
	SHL EAX,2
	MOV ESI,OFFSET xxxx
	ADD ESI,EAX
	MOV EAX,[ESI]
NEW:
	MOV ESI,OFFSET xxxx
	MOV EAX, [ESI+EAX*2]

OLD:
	ADD ESI,xx
	MOV rr,nnn
	MOV [ESI],rr
NEW
	MOV rr,nnn
	MOV [ESI+xx],rr

OLD
	ADD ESI,xx
	MOV rr, [ESI]
NEW
	MOV rr, [ESI+xx]


OLD (candidates):
	ADD ESI,EAX
	ADD ESI,4
	MOVZX EAX,WORD PTR _CrntStrucDef
	MOV [ESI],AX

	ADD ESI,EAX
	MOV EAX,[EBP-16]
	MOV [ESI],EAX

	ADD ESI,EAX
	ADD ESI,10
	MOVZX EAX,WORD PTR [ESI]

	ADD ESI,EAX
	ADD ESI,8
	MOV EAX,DWORD PTR [ESI]

	ADD ESI,EAX
	MOV EAX,DWORD PTR [ESI]

*/
