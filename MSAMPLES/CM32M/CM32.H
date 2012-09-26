/*
 * Global definitions for the C MINUS 32 compiler.
 *
 * Copyright 1990, Dave Dunfield
 * Copyright 1992-1995 R.A. Burgess
*/

#define FALSE		0
#define TRUE		1

/*
 * Misc. fixed compiler parameters
 */
#define	LINE_MAX	200		/* maximum size of input line */
#define MACROS		500		/* number of macros allowed */
#define MAC_BUFF	15000	/* string space alloted to macro pool */
#define PARAMETERS	10		/* maximum # parameters to a macro */
#define PARM_BUFF	200		/* parameter names & definitions */
#define INCL_DEPTH	5		/* maximum depth of include files */

#define GBUFFSIZE   15000	/* n bytes in symbol name pool */
#define LBUFFSIZE   300		/* n bytes in Local symbol name pool */
#define SYMBOL_SIZE	31		/* Max chars in symbol name */
#define EXPR_DEPTH	20		/* maximum depth of expression stack */
#define MAX_SYMBOLS	300		/* maximum # active symbols */
#define MAX_ARGS	20		/* maximum # arguments to a function */
#define MAX_PROTOS	700		/* maximum # params for all functions */
#define LOOP_DEPTH	10		/* maximum # nested loops */
#define MAX_SWITCH	80		/* maximum # active switch-case statements */
#define DIM_SIZE	100		/* maximum # active array dimensions */
#define DEFINE_SIZE	260		/* maximum # defines - was 150 */
#define DEFINE_BUFF 2800	/* size of define string space - was 1500*/
#define LITER_BUFF	10000	/* size of literal string space */
#define MAX_ERRORS	15		/* # error before termination forced */

/******************************************************************
 Bits found in the "type" entry of symbol table, also
 used on the expression stack to keep track of element types,
 and passed to the code generation routines to identify types.

 The POINTER definition indicates the level of pointer indirection,
 and should contain all otherwise unused bits, in the least significant
 (rightmost) bits of the word.

 All structure definitions are marked as STRUCDEF.  The TAG is optionally
 the name in the symbol table.  The tag may be reused to dine other
 structure variables. The members of a STRUCDEF follow the it directly
 in the sysbol table and are marked as STRUCMEM.  When a structure
 variable is actually defined it is simply marked as a STRUCT and
*/

#define STRUCMEM	0x04000000L		/* structure member (follows strucdef) */
#define STRUCDEF	0x02000000L		/* struct TAG define, members follow */
#define TYPDEF		0x01000000L		/* for type definition & struct tags */
#define ISR			0x00800000L		/* function is interrupt type */
#define REFERENCE	0x00400000L		/* symbol has been referenced */
#define GLOBAL		0x00200000L		/* symbol is not local - access by name */
#define INITED		0x00100000L		/* symbol is initialized */
#define GLABEL		0x00080000L		/* symbol is a goto label - local only */
#define FUNCTION	0x00040000L		/* symbol is a function */
#define PROTO   	0x00020000L		/* symbol is a prototype function */
#define VARARGS		0x00010000L		/* function has var nParams */
#define ARGUMENT	0x00008000L		/* symbol is a function argument */
#define EXTERNAL	0x00004000L		/* external reference */
#define FAR			0x00002000L		/* far - pointers and functions ONLY */
#define STATIC		0x00001000L		/* symbol is not public or External*/
#define CONSTANT	0x00000800L		/* warning if assigned a value */
#define REGISTER	0x00000400L		/* register symbol */
#define STRUCT		0x00000200L		/* a structure */
#define ARRAY		0x00000100L		/* symbol is an array */
#define UNSIGNED	0x00000080L		/* Use unsigned value */
#define DWORD		0x00000040L		/* 32 bit  */
#define WORD		0x00000020L		/* 16 bit */
#define BYTE		0x00000010L		/* 8  bit */
#define VOID		0x00000008L		/* functions and params only */
#define POINTER		0x00000007L		/* 7 levels of pointer indirection max */

#define SIZEMASK	0x00000070L		/* mask to test size only */

/*************************************************
  Tokens identifing value types.
**************************************************/

#define NUMBER		100	/* numeric constant */
#define STRING		101	/* literal constant */
#define LABEL		102	/* label address */
#define SYMBOL		103	/* symbol value */
#define INEAX		104	/* value in EAX (accumulator) */
#define INECX		105	/* value is in secondary register */
#define INEDX		106	/* pointer in EDX reg (indirect access) */
#define PECX		107	/* pointer in ECX reg (indirect access) */
#define PEDX		108	/* pointer in EDX register (indirect access) */
#define PESI		109	/* pointer in index register (indirect access) */

/* EBX is used as the top of the processor stack. It's simply
   for rapid access to stacked items. If another item is placed
   on the stack and a token is the "active EBX item" then it
   must be pushed onto the real stack.
*/

#define STACK_TOP	110 /* Value in EBX for accessibility */
#define ISTACK_TOP	111 /* Index to value in EBX for accessibility */

#define ON_STACK	112 /* Value actually on Processor stack */
#define ION_STACK	113 /* Index to value actually on Processor stack */
