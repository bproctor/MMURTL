/* TOKENS32.h
   Tokens for the C Minus compiler:
   The tokens are arranged in alphebetical order by the first
   character.  From there they are grouped grouped from longest to
   shortest (i.e. = comes after ==).
   Derived from code Copyright (c) 1989, Dave Dunfield
   Copyright 1991,1992,1993,1994 R.A. Burgess
*/

/*
    This is an index table for the first character of the token.
	It's ASCII numeric value provides an index to the token table.
	This cuts searching and comparison down to less than 4 compares
	worst case, 2 on the average!
*/
	static unsigned char itoken[128] = {
		0,   0,   0,   0,   0,   0,   0,   0,    /* 00 - 07  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 08 - 15  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 16 - 23  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 24 - 31  */
		0,   1,   0,   0,   0,   3,   5,   0,	 /* 32 - 39  */
		8,   9,  10,  12,  15,  16,  20,  22,	 /* 40 - 47  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 48 - 55  */
		0,   0,  24,  25,  26,  30,  32,  36,	 /* 56 - 63  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 64 - 71  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 72 - 79  */
		0,   0,   0,   0,   0,   0,   0,   0,	 /* 80 - 87  */
		0,   0,   0,  37,   0,  38,  39,   0, 	 /* 88 - 95  */
		0,   0,  41,  42,  46,  48,  50,  52,	 /* 96 - 103 */
		0,  53,   0,   0,  56,   0,   0,   0,	 /* 104- 111 */
		0,   0,  57,  59,  65,  66,  68,  69,	 /* 112- 119 */
		0,   0,   0,  70,  71,  74,  75,   0};	 /* 120- 127 */

	static char *tokens[] = {

	/* token         number  priority  optype  ASCII */
		"",          /* 00       0       0        0  */
		"!=",        /* 01       9       2        33 */
		"!",         /* 02       0       1           */
		"%=",        /* 03       2       3        37 */
		"%",         /* 04       13      2           */
		"&&",        /* 05       5       4        38 */
		"&=",        /* 06       2       3           */
		"&",         /* 07       8       2           */
		"(",         /* 08       0       0        40 */
		")",         /* 09       0       0        41 */
		"*=",        /* 10       2       3        42 */
		"*",         /* 11       13      2           */
		"++",        /* 12       15      0        43 */
		"+=",        /* 13       2       3           */
		"+",         /* 14       12      2           */
		",",         /* 15       0       0        44 */
		"--",        /* 16       15      0        45 */
		"-=",        /* 17       2       3           */
		"->",        /* 18       15      2           */
		"-",         /* 19       12      2           */
		"...",       /* 20       0       0        46 */
		".",         /* 21       15      2        46 */
		"/=",        /* 22       2       3        47 */
		"/",         /* 23       13      2           */
		":",         /* 24       0       0        58 */
		";",         /* 25       0       0        59 */
		"<<=",       /* 26       2       3        60 */
		"<<",        /* 27       11      2           */
		"<=",        /* 28       10      2           */
		"<",         /* 29       10      2           */
		"==",        /* 30       9       2        61 */
		"=",         /* 31       2       3           */
		">>=",       /* 32       2       3        62 */
		">>",        /* 33       11      2           */
		">=",        /* 34       10      2           */
		">",         /* 35       10      2           */
		"?",         /* 36       3       4        63 */
		"[",         /* 37       0       0        91 */
		"]",         /* 38       0       0        93 */
		"^=",        /* 39       2       3        94 */
		"^",         /* 40       7       2           */
		"break",     /* 41       0       0        98 */
		"case",      /* 42       0       0        99 */
		"char",      /* 43       0       0           */
		"const",     /* 44       0       0           */
		"continue",  /* 45       0       0           */
		"default",   /* 46       0       0        100*/
		"do",        /* 47       0       0           */
		"else",      /* 48       0       0        101*/
		"extern",    /* 49       0       0           */
		"far",       /* 50       0       0        102*/
		"for",       /* 51       0       0           */
		"goto",      /* 52       0       0        103*/
		"interrupt", /* 53       0       0        105*/
		"int",       /* 54       0       0           */
		"if",        /* 55       0       0           */
		"long",      /* 56       0       0        108*/
		"register",  /* 57       0       0        114*/
		"return",    /* 58       0       0           */
		"signed",    /* 59       0       0        115*/
		"sizeof",    /* 60       0       1           */
		"static",    /* 61       0       0           */
		"struct",    /* 62       0       0           */
		"switch",    /* 63       0       0           */
		"short",     /* 64       0       0           */
		"typedef",   /* 65       0       0        116*/
		"unsigned",  /* 66       0       0        117*/
		"union",     /* 67       0       0           */
		"void",      /* 68       0       0        118*/
		"while",     /* 69       0       0        119*/
		"{",         /* 70       0       0        123*/
		"||",        /* 71       4       4        124*/
		"|=",        /* 72       2       3           */
		"|",         /* 73       6       2           */
		"}",         /* 74       0       0        125*/
		"~",         /* 75       ?       1        126*/
		0};           /* End of table */

#define NE		 1		/* '!='  */
#define NOT		 2		/* '!'   */
#define MODE	 3		/* '%='  */
#define MOD		 4		/* '%'   */
#define DAND	 5		/* '&&' (marks beginning of binaries) */
#define ANDE	 6		/* '&='  */
#define AND		 7		/* '&' - And & address of */
#define ORB		 8		/* (     */
#define CRB		 9		/* )     */
#define STARE	10		/* '*='  */
#define STAR	11		/* '*' - multiply & pointer */
#define INC		12		/* '++'  */
#define ADDE	13		/* '+='  */
#define ADD		14		/* '+'   */
#define COMMA	15		/* ,     */
#define DEC		16		/* '--'  */
#define SUBE	17		/* '-='  */
#define DEREF	18		/* '->'  */
#define SUB		19		/* '-' - subtract & negate */
#define ELIPSE	20		/* '...' Elipse for args */
#define DOT		21		/* '.'   */
#define DIVE	22		/* '/='  */
#define DIV		23		/* '/'   */
#define COLON	24		/* :     */
#define SEMI	25		/* ;     */
#define SHLE	26		/* '<<=' */
#define SHL		27		/* '<<'  */
#define LE		28		/* '<='  */
#define LT		29		/* '<'   */
#define EQ		30		/* '=='  */
#define ASSIGN	31		/* '='   */
#define SHRE	32		/* '>>=' */
#define SHR		33		/* '>>'  */
#define GE		34		/* '>='  */
#define GT		35		/* '>'   */
#define QUEST	36		/* '?' (marks ending of binaries) */
#define OSB		37		/* [     */
#define CSB		38		/* ]     */
#define XORE	39		/* '^='  */
#define XOR		40		/* '^'   */
#define BREAK	41		/* 'break' statement */
#define CASE	42		/* 'case' statement */
#define CHAR	43		/* 'char' */
#define CONST	44		/* 'const' */
#define CONTIN	45		/* 'continue' statement */
#define DEFAULT	46		/* 'default' statement */
#define DO		47		/* 'do' statement */
#define ELSE	48		/* 'else' modifier */
#define EXTERN	49		/* 'extern' */
#define FARR	50		/* 'far'  */
#define FOR		51		/* 'for' statement */
#define GOTO	52		/* 'goto' statement */
#define INTR	53		/* 'interrupt' type for functions */
#define INT		54		/* 'int'   */
#define IF		55		/* 'if' statement */
#define LONG	56		/* 'long'   */
#define REGIS	57		/* 'register' */
#define RETURN	58		/* 'return' statement */
#define SIGNED	59		/* 'signed' */
#define SIZEOF  60		/* 'sizeof' */
#define STAT    61		/* 'static' */
#define STRUC   62		/* 'struct' */
#define SWITCH	63		/* 'switch' statement */
#define SHORT	64		/* 'short' */
#define TYPEDEF 65		/* 'typedef' */
#define UNSIGN	66		/* 'unsigned' */
#define UNION	67		/* 'union' */
#define VOIDD	68		/* 'void' */
#define WHILE	69		/* 'while' statement */
#define OCB		70		/* {    */
#define DOR		71		/* '||' */
#define ORE		72		/* '|=' */
#define OR		73		/* '|'  */
#define CCB		74		/* }    */
#define COM		75		/* '~'  */
#define ULT 	76		/* pseudo operators for unsigned compares */
#define ULE 	77		/* pseudo operators for unsigned compares */
#define UGT 	78		/* pseudo operators for unsigned compares */
#define UGE 	79		/* pseudo operators for unsigned compares */

/* Table defining expression operator precedence.
   Position in array is token number, while value
   in that position is priority. (15 is highest)
*/
	static char priority[] = {
		 0,	  9,  0,  2, 13,  5,  2,  8,	/* 00 - 07 */
		 0,   0,  2, 13, 15,  2, 12,  0,	/* 08 - 15 */
		 15,  2, 15, 12,  0, 15,  2, 13,	/* 16 - 23 */
		 0,   0,  2, 11, 10, 10,  9,  2,	/* 24 - 31 */
		 2,  11, 10, 10,  3,  0,  0,  2,	/* 32 - 39 */
		 7,	  0,  0,  0,  0,  0,  0,  0,	/* 40 - 47 */
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 48 - 55 */
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 56 - 63 */
		 0,   0,  0,  0,  0,  0,  0,  4,	/* 64 - 71 */
		 2,   6,  0,  0,  0,  0,  0,  0,	/* 72 - 79 */
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 80 - 87 */
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 88 - 95 */
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 96 - 103*/
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 104- 111*/
		 0,   0,  0,  0,  0,  0,  0,  0,	/* 112- 119*/
		 0,   0,  0,  0,  0,  0,  0,  0		/* 120- 127*/
	 };

/* This table determines operator type.
   Two operators grouped Right to Left = 3.
   Two operators grouped Left to Right = 2.
   One operator (ALWAYS grouped Right to Left) = 1.
   Logical operators are not included. They are handled
   separately in the code (numbered as 4).
*/
	static char optype[] = {
		 0,	 2,  1,  3,  2,  4,  3,  2,		/* 00 - 07 */
		 0,  0,  3,  2,  0,  3,  2,  0,		/* 08 - 15 */
		 0,  3,  2,  2,  0,  2,  3,  2,		/* 16 - 23 */
		 0,  0,  3,  2,  2,  2,  2,  3,		/* 24 - 31 */
		 3,  2,  2,  2,  4,  0,  0,  3,		/* 32 - 39 */
		 2,	 0,  0,  0,  0,  0,  0,  0,		/* 40 - 47 */
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 48 - 55 */
		 0,  0,  0,  0,  1,  0,  0,  0,		/* 56 - 63 */
		 0,  0,  0,  0,  0,  0,  0,  4,		/* 64 - 71 */
		 3,  2,  0,  1,  0,  0,  0,  0,		/* 72 - 79 */
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 80 - 87 */
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 88 - 95 */
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 96 - 103*/
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 104- 111*/
		 0,  0,  0,  0,  0,  0,  0,  0,		/* 112- 119*/
		 0,  0,  0,  0,  0,  0,  0,  0		/* 120- 127*/
	 };

/* End of tokens32.h */
