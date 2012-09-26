/*
 C Minus 32 (C-32) Compiler  (DOS version 2.2)

 Derived from code Copyright (c) 1989, Dave Dunfield
 Copyright (c) 1991, 1992, 1993, 1994 R.A. Burgess
 Permission is required from the authors for
 any commercial use of this source code.

 32 bit assembly language generation specificly for the Intel 386/486
 for the DASM assembler.

 CM-32 integer support:
 short = int = short int = 16 bits,
 long = long int = 32 bits.

 16 bit Intel addressing modes are NOT supported for code generation.

*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "CM32.h"
#include "Tokens32.h"

/* Input buffer & input pointer for preprocessed lines */
char line_in[LINE_MAX], *input_ptr;

/* Global value storage locations (results from lexical scanner) */

char gst[SYMBOL_SIZE+1];	/* Name if SYMBOL */
U8  namesize;				/* size of SYMBOL name */
U32 gvalue;

/*
   if token = SYMBOL then gvalue is length (number of characters).
   if token = NUMBER then gvalue is value of NUMBER.
   if token = STRING then gvalue is index into literal pool to the string.
*/

/* Symbol table and associated variables */

char GPool[GBUFFSIZE];  /* Pool for symbol names (packed) */
char LPool[LBUFFSIZE];  /* Pool for local symbol names (packed) */

struct sym {
	U32 type;		/* 32 symbol type bits  */
	U16 itypedef;	/* index in symtab for type definition, or zero */
					/* If strucmem, struct it belongs to */
	U16 oname;		/* offset in pool to sym name */
	U16 argoffset;	/* arg offset from EBP for locals */
	U16 strucoff;   /* size of strucdef (also offset for next new member), */
					/*	 if strucmem, it's member's offset in structure */
	U16	dindex;		/* rg for index to dim */
	};				/* or index to proto list for arg types */
					/* or holds labels for for local Goto */

struct sym symtab[MAX_SYMBOLS];  /* SYM TABLE... */

U16 oNextGName = 0;		/* offset to next new name in global pool */
U16 oNextLName = 0;		/* offset to next new name in local pool */

U32 proto_list[MAX_PROTOS],	/* list of arg types for functions */
 	global_top = 0,
 	global_count = 0,
 	iproto_next = 1,		/* index to next available proto args (0 illegal)*/
 	icrntpro = 0,			/* index to arg being tested */
 	iarg = 0,
 	argtype = 0,
 	local_top = MAX_SYMBOLS,
 	arg_count,				/* number of args so far  */
 	local_stack,
 	sptr,					/* symbol ptr */
 	fptr;					/* ptr to function symbol we are in */

	/* structure definition use and control variables */

	S8 fInStruct = 0;	/* true if currently defining struct members */
    U16 CrntStrucDef;	/* iSym for Structure def we are using or working on */
	char structname[12] = "0StructDef";
	U8 NxtStrucNum = 0;
	U32 memoffset;		/* used to calculate members offset */
	U32 strucsize;		/* used for size of struct in INC and DEC */

/* Literal + Dimension pools and associated variables */

U32 dim_top = 0,
    literal_top = 0,
    dim_pool[DIM_SIZE];		/* each entry has nDims for an array */


char literal_pool[LITER_BUFF];

/* Expression evaluation stack. The value field contents vary
   depending on what the token is. If it's a Symbol, this is
   the symbol table entry. If token is a number then value is
   the actual value of the number.  If it's a string, then
   value is the length.
   The offset is used to hold constant values for addresses
   that are in the index register or on the stack.
   When accessing the data pointed to, if this is NON-zero,
   we use the [ESI+NUM] addressing mode (Base+Offset). */

struct expr {
	U32 token;		/* Number, Symbol, String, In-Accumulator, etc. */
	U32 value;		/* varies depending on token */
	U32 type;		/* type from symtab */
	U32 offset;		/* Offset of pointer in structure for access */
	};

struct expr expstk[EXPR_DEPTH];  /* Expression Stack */

U32 expr_ptr = 0;

/* Misc. global variables */

char if_flag = 0,
     asm_flag = 0,
     not_flag = 0,
	 prefix = 'L';				/* override with /P:x switch */

U32 break_stack[LOOP_DEPTH],
	continue_stack[LOOP_DEPTH],
	switch_stack[MAX_SWITCH*2],
	exit_label = 0,
	exit_used = 0,
	in_function = 0,
	loop_ptr = 0,
	switch_ptr = 0,
	define_top = 0,
	sdefault = 0,
	exit_flag = 0,
	next_lab = 0,
	line_number = 0,
	begin_comment = 0,  	/* line that a comment starts on for errors */
	ungot_token = 0,
	error_count = 0,
	warn_count = 0;


	/* input/output pointers and buffer for for preprocessor */

char buffer[LINE_MAX],
	 *buffin_ptr,
	 *buffout_ptr;

	/* macro definition: index, pool and top pointers */

U32  macro = 0;
char *define_index[MACROS],
      define_pool[MAC_BUFF],
     *define_ptr;

	/* macro parameter: index, pool and top pointers */
unsigned parm;
char *parm_index[PARAMETERS],
      parm_pool[PARM_BUFF],
     *parm_ptr;


	/* include: line stack & count, file pointers, filename */
long include = 0,
	 incl_line[INCL_DEPTH];

FILE *incl_fh[INCL_DEPTH],
	 *source_fh = 0,
	 *asm_fh = 0,
     *temp_fh = 0,
     *code_fh = 0,
	 *list_fh = 0;

char *tmpname = "Code.tmp";
char codename[40];
char srcname[40];
char asmname[40];
char lstname[40];

/* file open flags */

char fLISTOpen = 0,
     fTEMPOpen = 0,
     fCODEOpen = 0,
     fASMOpen = 0;

	/* misc. variables and flags */
char comment_flag = -1,
	 fQuiet = 0,
     fSource = 0,
     fNoOpt = 0,
     fOptS = 0,
     fList = 0,
     fGen = 0,
     fWarnings = 0;

	/* inlcude path */

char *incdir = "\\CM32\\INCLUDE";

U32	global_width = 0,
	asmlab = 0;

char zero_flag,
     stack_flag = 0;


#include "Optimize.h"
#include "Proto32.h"

/******************************************
* Determine if a character is alphabetic
* or is underscore.
*******************************************/

static char is_alpha(char chr)
{
	return ((chr >= 'a') && (chr <= 'z'))
		|| ((chr >= 'A') && (chr <= 'Z'))
		|| (chr == '_');
}

/*********************************************
* Determine if a character is a numeric digit
**********************************************/

static char is_digit(char chr)
{
	return (chr >= '0') && (chr <= '9');
}


/******************************************
* Test for valid character in a name.
*******************************************/

static char is_alnum(char c)
{
	return is_alpha(c) || is_digit(c);
}

/****************
* Copy a string
*****************/

static void copystring(char *dest, char *source)
{
	while(*dest++ = *source++);
}


/***************************
* Test for string equality
****************************/

static char equal_string(char *str1, char *str2)
{
	do {
		if(*str1 != *str2++)
		return 0; }
	while(*str1++);
	return -1;
}

/*********************************************************
 Skip to next non-blank (space, tab, CR or LF) in buffer
 returning the character we find.
***********************************************************/

static char skip_blanks(void)
{
	while((*buffin_ptr == ' ') || (*buffin_ptr == 9)
	   || (*buffin_ptr == '\n') || (*buffin_ptr == '\r'))
		++buffin_ptr;
	return *buffin_ptr;
}

/***************************************
* Test for more macro parameters
****************************************/

static long more_parms(void)
{
	register char c;

	if(((c = skip_blanks()) == ',') || (c == ')'))
		++buffin_ptr;
	else
		line_error("Invalid macro parameter");
	return c == ',';
}

/**************************************
* Skip ahead through a comment
***************************************/

static void skip_comment(void)
{
	register U16 x;
	register char c;

	x = 0;
	for(;;) {
		if(!(c = *buffin_ptr++)) {					/* end of line_in */
			if(!fgets(buffin_ptr = buffer, LINE_MAX, source_fh)) {
				*buffin_ptr = 0;
				UTC_error();
				return; }
			++line_number; }
		else {
			if((x = (x << 8) + c) == (('*' << 8) + '/'))	/* close */
				return;
			if(x == (('/' << 8) + '*'))				/* imbedded comment */
				skip_comment();
		}
	}
}

/*****************************************************************
* Copy a named symbol from the input buffer to the output buffer
******************************************************************/

static void copy_name(void)
{
	do
		*buffout_ptr++ = *buffin_ptr++;
	while(is_alnum(*buffin_ptr));
	*buffout_ptr = 0;
}

/******************************************************************
* Copy a quoted string from the input buffer to the output buffer
* with regard for "protected" characters.
*******************************************************************/

static void copy_string(void)
{
	register char delim;

	if(((delim = *buffin_ptr) == '"') || (delim == 0x27)) {
		do {
			if(!(*buffout_ptr++ = *buffin_ptr)) {		/* premature end */
				line_error("Unterminated string");
				return; }
			if(*buffin_ptr++ == '\\')				/* protected char */
				*buffout_ptr++ = *buffin_ptr++; }
		while(*buffin_ptr != delim);
		*buffout_ptr++ = *buffin_ptr++; }
}

/**************************************************************
* Lookup a word from the input stream to see if it is a macro
***************************************************************/

static U32 lookup_macro(char eflag)
{
	register long i;
	register char *name;

	name = buffout_ptr;
	copy_name();
	for(i = macro - 1; i >= 0; --i)			/* look it up */
		if(!strcmp(name, define_index[i]))
			return i;
	if(eflag)								/* not found */
		line_error("Undefined macro");
	return -1;
}

/**************************************************************
* Resolve a word into a macro definition (if it is defined)
***************************************************************/

static void resolve_macro(void)
{
	char *mptr, *ptr, *old_ptr;
	long i;
	register char c;

	old_ptr = buffout_ptr;
	if((i = lookup_macro(0)) != -1) {	/* Substitution required */
		mptr = define_index[i];
		while(*mptr++);
		parm = 0;
		parm_ptr = parm_pool;
		if(*mptr++) {					/* parameterized macro */
			if(skip_blanks() == '(') {
				++buffin_ptr;
				do {
					parm_index[parm++] = parm_ptr;
					while(*buffin_ptr && (*buffin_ptr != ',') && (*buffin_ptr != ')'))
						*parm_ptr++ = *buffin_ptr++;
					*parm_ptr++ = 0; }
				while(more_parms()); } }
		while(c = *mptr) {				/* copy over definition */
			if(c & 0x80) {				/* parameter substitution */
				if((i = c & 0x7f) < parm) {
					for(ptr = parm_index[i]; *ptr; ++ptr)
						*old_ptr++ = *ptr; } }
			else
				*old_ptr++ = *mptr;
			++mptr;
		}
		*(buffout_ptr = old_ptr) = 0;
	}
}

/****************************************
* Test for a string in input stream
*****************************************/

static unsigned long match(char *ptr)
{
	register char *ptr1;

	ptr1 = buffin_ptr;
	while(*ptr)
		if(*ptr++ != *ptr1++)		/* symbols do not match */
			return 0;
	if(is_alnum(*ptr1))			/* symbol continues */
		return 0;
	buffin_ptr = ptr1;
	skip_blanks();
	return(1);
}

/*******************************************************
* Compare all optimization table entries with the
* instructions in the peephole buffer.
* Return:	0	= No match
*           n	= Full match begining at peep_first
*				  and ending at entry 'n'
********************************************************/

static int compareT(char *ptable, int peep)
{
	int i;
	char *ptr1, *ptr2, c;

	for(i=0; i < OSYMBOLS; ++i)
		symbols[i][0] = 0;

	ptr1 = peep_buffer[peep];
	while(c = *ptable) {			/* any chars left in entry? */
		if(c == '\n') {				/* end of line in table entry */
			if(*ptr1)				/* and no match... */
				return 0;
            peep = (peep+1) % OBUF_SIZE;
			if(peep == peep_next)
				return 0;
			ptr1 = peep_buffer[peep];	/* next buffer entry */
		}
		else if (c == ' ')  {		/* space */
			if(!isspace(*ptr1))
				return 0;
			while(isspace(*ptr1))
				++ptr1; }
		else if(c & 0x80) {			/* symbol name */

			c = *(ptable + 1);

            ptr2 = symbols[*ptable & 0x7f];
			if(*ptr2) {
				while(*ptr1 && (*ptr1 != c))
					if(*ptr1++ != *ptr2++)
						return 0;
				if(*ptr2)
					return 0; }
			else {							/* new symbol */
				while(*ptr1 && (*ptr1 != c))
					*ptr2++ = *ptr1++;
				*ptr2 = 0; } }

		else if(c != *ptr1++) return 0;		/* normal character */

		++ptable;
	}
	return (*ptr1) ? 0 : peep + 1;
}


/********************************************************
* Exchange new code for old code in the peephole buffer.
* pnew points to the first char of the new code.
* old points to last entry of buffer code to be replaced.
*********************************************************/

static void exchange(unsigned old, char *pnew)
{
	char *ptr1, *ptr2;

    peep_first = (old+(OBUF_SIZE-1)) % OBUF_SIZE;  /* last entry of replacement */
	ptr2 = peep_buffer[peep_first];
	while(*pnew) {						/* while still some new stuff left */
		if(*pnew & 0x80) {				/* output a symbol */
			ptr1 = symbols[*pnew & 0x7f]; 	/* ptr1 points to a symbol */
			while(*ptr1)
				*ptr2++ = *ptr1++; }
		else if(*pnew == '\n') {		/* end of a new entry */
			*ptr2 = 0;					/* put null at end of line */
            peep_first = (peep_first+(OBUF_SIZE-1)) % OBUF_SIZE;
			ptr2 = peep_buffer[peep_first]; }
		else
			*ptr2++ = *pnew;
		++pnew; }
	*ptr2 = 0;
}

/****************************************
* Read a line into the peephole buffer
* from the input file
*****************************************/

static long read_line(void)
{
char c, *sptr;

	if(fgets(peep_buffer[peep_next], OLINE_SIZE, temp_fh)) {

		/*strip CR/LF from line */
		sptr = &(peep_buffer[peep_next]);

		while (c = *sptr) {
			if (c == '\r') *sptr = '\0';
			if (c == '\n') *sptr = '\0';
			sptr++;
		}

		/* next peep_buf entry please... */
		peep_next = (peep_next+1) % OBUF_SIZE;
		return 1;
	}
	return 0;
}

/*********************************************************
* Write a line from the peephole buffer to the output file
**********************************************************/
static void write_line(void)
{
	if (fGen)
		fputs(peep_buffer[peep_first], code_fh);
	else
		fputs(peep_buffer[peep_first], asm_fh);
	peep_first = (peep_first + 1) % OBUF_SIZE;
	if (fGen)
		fputs("\n", code_fh);
	else
		fputs("\n", asm_fh);
}


/*********************************************************
* Peephole Optimizer function for code segment
**********************************************************/
static void optimize(void)
{
	int i, j;
	char *ptr;

	if (!fQuiet)
		fputs("CM32 V2.2 optimizer phase\r\n", stdout);

	/* initially fill every line in peephole buffer */

	j = 0;

	for(;;) {
			/* keep buffer full! */
		while ( ((peep_next+1) % OBUF_SIZE) != peep_first ) {
  	    	if (!read_line()) {
  	      		break;
			}
		}
		/* walk thru the whole peep_table and see if we have
			matches in the peep_buffer  */

		for(i=0; ptr = peep_table[i]; i += 2) {

            j = compareT(ptr, peep_first);

			if(j) {							/* we have a match, exchange it */
				exchange(j, peep_table[i+1]);
				break;						/* break to fill buffer again */
			}
		}
		if(!j) write_line();		/* no matches, flush this line */

		if (peep_first == peep_next)
			return;					/* We are done! */
	}
}

/********************************************
 Read a line & perform pre-processing. if_flag
 is used to indicate the nesting of ifs.  It
 is incremented for each if found.  The high
 bit is set if we are actually IN an active
 macro.
*********************************************/

static long readline(void)
{
U32 i;
char c, ch, fgotone;

for(;;) {
	if(!fgets(buffin_ptr = buffer, LINE_MAX, source_fh))
	{
		if(include)
		{
			fclose(source_fh);
			line_number = incl_line[--include];
			source_fh = incl_fh[include];
			continue;
		}
		return 0;
	}		/* no more lines... */

	++line_number;
	buffout_ptr = line_in;

	fgotone = 0;

	if ((ch = skip_blanks()) == '#') 
	{

		if(match("#asm"))
		{								/* if inline assembly */
			asm_flag = -1;
			fgotone = -1;
		}

		else if(match("#endasm")) 
		{								/* end of assembly */
			asm_flag = 0;
			fgotone = -1;
		}

		else if(match("#ifdef"))
		{								/* if macro defined */
			fgotone = -1;
			if(if_flag)
				++if_flag;					/* one deeper */
			else if(lookup_macro(0) == -1)
			{
				if_flag = 0x80;
			}
		}

		else if(match("#ifndef"))
		{			/* if macro not defined */
			fgotone = -1;
			if(if_flag)
				++if_flag;
			else if(lookup_macro(0) != -1)
			{
					if_flag = 0x80;
			}
		}

		else if(match("#else"))
		{			/* reverse condition */
			fgotone = -1;
			if(!(if_flag & 0x7f))			/* not nested? */
				if_flag ^= 0x80;
		}									/* XOR high bit */

		else if(match("#endif"))
		{									/* end conditional */
			fgotone = -1;
			if(if_flag & 0x7f)				/* if nested, reduce one */
				--if_flag;
			else
				if_flag = 0;
		}				/* else it's over */

		else if(match("#pragma"))
		{		/* later... */
				fgotone = -1;
		}
	}

		/* If we got here, we have found # and it's not
		   a conditional, so it must be a new macro.
		*/

	if ((!fgotone) && (!if_flag))
	{
		if (ch == '#')
		{
			if(match("#define")) {			/* define a new macro */
				if(macro >= MACROS) {
					fatal_error("Too many macro definitions");
					exit(-1); }
				buffout_ptr = define_index[macro++] = define_ptr;
				if(!is_alpha(*buffin_ptr)) {
					line_error("Invalid macro name");
					continue; }
				copy_name();				/* get macro name */
				define_ptr = buffout_ptr;
				*define_ptr++ = 0;
				parm = 0;
				parm_ptr = parm_pool;
				if(*buffin_ptr == '(') {	/* parameterized macro */
					*define_ptr++ = 1;
					++buffin_ptr;
					do {
						if(parm >= PARAMETERS) {
							line_error("Too many macro parameters");
							break; }
						parm_index[parm++] = buffout_ptr = parm_ptr;
						skip_blanks();
						copy_name();
						parm_ptr = buffout_ptr+1; }
					while(more_parms()); }
				else
					*define_ptr++ = 0;
				skip_blanks();
				while(c = *buffin_ptr) {
					buffout_ptr = define_ptr;
					if(is_alpha(c)) {
						resolve_macro();
						for(i=0; i < parm; ++i) {
							if(!strcmp(define_ptr, parm_index[i])) {
								*define_ptr++ =  i + 0x80;
								buffout_ptr = define_ptr;
								break; } }
						define_ptr = buffout_ptr; }
					else if((c == '"') || (c == 0x27)) {
						copy_string();
						define_ptr = buffout_ptr; }
					else {
						if((*++buffin_ptr == '*') && (c == '/')) {
							++buffin_ptr;
							begin_comment = line_number;
							skip_comment(); }
						else
							*define_ptr++ = c; }
					/* skip_blanks(); */
				}
				*define_ptr++ = 0; }

			else if(match("#undef"))
			{			/* undefine a macro */
				if((i = lookup_macro(-1)) != -1)
				{
					if(i == (macro - 1))		/* last one, simple delete */
						define_ptr = define_index[i];
					else
					{						/* not last, reclaim space */
						define_ptr -= (parm = (buffin_ptr = define_index[i+1]) -
							(parm_ptr = define_index[i]));
						while(parm_ptr < define_ptr)
							*parm_ptr++ = *buffin_ptr++;
						while(i < macro)
						{		/* adjust index list */
							define_index[i] = define_index[i+1] - parm;
							++i;
						}
					}
					--macro;
				}
			}
			else if(match("#include"))
			{		/* include a file */
				if(include >= INCL_DEPTH)
					fatal_error("Too many include files");
				if((c = skip_blanks()) == '<')
				{	/* incdir definition */
					for(parm_ptr = incdir; *parm_ptr; ++parm_ptr)
						*buffout_ptr++ = *parm_ptr;
					*buffout_ptr++ = '\\';			/*** OS dependent!!! ***/
					c = '>';
				}
				else if(c != '"')
				{					/* current directory */
					line_error("Invalid include file name");
					continue;
				}
				while(*++buffin_ptr && (*buffin_ptr != c))
					*buffout_ptr++ = *buffin_ptr;
				*buffout_ptr = 0;
				incl_fh[include] = source_fh;
				incl_line[include] = line_number;
				if(source_fh = fopen(line_in, "r"))
				{
					line_number = 0;
					++include;
				}
				else
				{
					line_error("Cannot open include file");
					source_fh = incl_fh[include];
				}
			}
			else
				line_error("Unknown preprocessor directive");

		}
		else
		{				/* default, perform pre-processing */

			if(asm_flag)		/* inline assembly */
				do_asm(buffer);
			else
			while(c = *buffin_ptr)
			{
				if(is_alpha(c))				/* symbol, could be macro */
					resolve_macro();
				else if((c == '"') || (c == 0x27))		/* quoted string */
					copy_string();
				else
				{
					if((*++buffin_ptr == '*') && (c == '/') && comment_flag)
					{ /* comment */
						++buffin_ptr;
						begin_comment = line_number; /* save begin line*/
						skip_comment();
					}
					else				/* nothing special, copy it */
						*buffout_ptr++ = c;
				}
			}
			if (fSource)
			{
				code_chr(';');
				code_str(buffer);
			}
			input_ptr=line_in;	/* point to start of new line */
			*buffout_ptr = 0;		/* null at end of line */
			return -1;
		}
	}  /* !fgotone && !if_flag */
} /*for*/
}



/***********************************************************
* Test to see if the last statement compiled was an "exit"
* statement, and if so, generate a jump to the appriopriate
* label. This prevents generation of a spurious jump when a
* "return" statement is used at the end of a function.
************************************************************/

static U32 test_exit(void)
{
	if(exit_flag) {
		jump(exit_flag, -1);
		exit_used = -1;
		return(exit_flag = 0);
		}
	return -1;
}

/*********************************************
* Output an warning message with quoted text
**********************************************/

static void t_warn(char *msg, char *txt)
{
int i;
	++warn_count;
	if (!fWarnings)	return;

	incl_line[include] = line_number;
	for(i=0; i <= include; ++i) {
		put_num(incl_line[i], list_fh);
		fputc(':', list_fh);
		}
	fputc(' ', list_fh);
	put_str("Warning: ", list_fh);
	put_str(msg, list_fh);
	fputc(' ', list_fh);
	fputc(0x27, list_fh);		/* single quote */
	put_str(txt, list_fh);
	fputc(0x27, list_fh);		/* single quote */
	fputc('\r', list_fh);
	fputc('\n', list_fh);
}

/*********************************************
* Output an error message with quoted text
**********************************************/

static void t_error(char *msg, char *txt)
{
	char emsg[50], *ptr;

	ptr = emsg;
	while(*msg)
		*ptr++ = *msg++;
	*ptr++ = ':';
	*ptr++ = ' ';
	*ptr++ = 0x27;		/* single quote */
	while(*txt)
		*ptr++ = *txt++;
	*ptr++ = 0x27;		/* single quote */
	*ptr = 0;
	line_error(emsg);
}


/***********************************************************
* Report an error involving a freshly parsed symbol name
************************************************************/

static void symbol_error(char *msg)
{
	t_error(msg, gst);
}


/***********************************************************
* Report a syntax error
************************************************************/

static void syntax_error(void)
{
	line_error("Syntax error");
}

/***********************************************************
* Report incompatable types
************************************************************/

static void type_error(void)
{
	line_error("Types mismatch");
}

/***********************************************************
* Report an error in indirection
************************************************************/

static void index_error(void)
{
	line_error("Illegal indirection");
}


/****************************************************
* Test the next token in the input stream, put it
* back if its not the one we are looking for.
****************************************************/

static char test_token(U32 token)
{
	U32 token1;

	if((token1 = get_token()) == token)
		return -1;
	unget_token(token1);
	return 0;
}


/**************************************************************
* Check for a certain token occuring next in the input stream.
* If it is not found, report an error.
***************************************************************/

static void expect(U32 token)
{
	if(!test_token(token))
		t_error("Expected", tokens[token]);
}

/************************************************
* Write a string to device indicated by "fh"
*************************************************/

static void put_str(char *ptr, FILE *fh)
{
	while(*ptr)
		fputc(*ptr++, fh);
}


/***************************************
 Report a Unterminated Comment error
****************************************/

static void UTC_error(void)
{
	U32 i;

	incl_line[include] = line_number;
	for(i=0; i <= include; ++i) {
		put_num(incl_line[i], list_fh);
		fputc(':', list_fh);
		}
	fputc(' ', list_fh);
	put_str("Unterminated comment from line: ", list_fh);
	put_num(begin_comment, list_fh);
	fputc('\r', list_fh);
	fputc('\n', list_fh);

	if(++error_count == MAX_ERRORS)
		fatal_error("Too many errors");
}

/***************************************
* Report a compile error
****************************************/

static void line_error(char *message)
{
	U32 i;

	incl_line[include] = line_number;
	for(i=0; i <= include; ++i) {
		put_num(incl_line[i], list_fh);
		fputc(':', list_fh);
		}
	fputc(' ', list_fh);
	put_str(message, list_fh);
	fputc('\r', list_fh);
	fputc('\n', list_fh);

	if(++error_count == MAX_ERRORS)
		fatal_error("Too many errors");
}

/*******************************************
* Report a non-recoverable compile error
********************************************/

static void fatal_error(char *string)
{
	line_error(string);

	/* put this out even if fQuiet */
	put_str("Fatal error, compilation aborted\r\n", stdout);

	if (fList) {
		put_str("Fatal error, compilation aborted\r\n", list_fh);
		if (fLISTOpen)
			fclose(list_fh);
		}

	/* close files */
	if (fTEMPOpen) { fclose(temp_fh);
	remove(tmpname);  /* leave out while troubleshooting */
	}
	if (fASMOpen) fclose(asm_fh);
	exit(-1);  /* We are done ! */

}

/*****************************************************
* Check that a loop is active & setup exit condition
******************************************************/

static void check_loop(U32 stack[])
{
	expect(SEMI);
	if(loop_ptr)
		exit_flag = stack[loop_ptr-1];
	else
		line_error("No active loop");
}

/****************************************************
* Check that a switch is active & allocate label
*****************************************************/

static U32 check_switch(void)
{
	if(!sdefault)
		line_error("No active switch");
	gen_label(++next_lab);
	return next_lab;
}

/*******************************************************************
* Compile a jump only if last statement compiled was not an "exit"
* statement ("return", "break", or "continue"). This prevents the
* generation of an unaccessable jump following these statements.
********************************************************************/

static void test_jump(U32 label)
{
	if(test_exit())
		jump(label, -1);
}


/********************************************
* Compile a conditional jump and
* set up 'Z' flag if necessary.
*********************************************/

static void cond_jump(char cond, U32 label, char ljmp)
{
	if(zero_flag)
	{
		out_inst("AND EAX,EAX");
		zero_flag = 0;
	}

	jump_if(cond ^ not_flag, label, ljmp);
	not_flag = 0;
}


/**********************************************************
* Get a number in a number base for a maximum # of digits
* (digits = 0 means no limit)
***********************************************************/

static U32 get_number(U32 base, U32 digits)
{
	U32 value;
	char c;

	value = 0;
	do {
		if(is_digit(c = *input_ptr))		/* convert numeric digits */
			c -= '0';
		else if(c >= 'a')				/* convert lower case alphas */
			c -= ('a' - 10);
		else if(c >= 'A')				/* convert upper case alphas */
			c -= ('A' - 10);
		else
			break;
		if(c >= base)					/* outside of base */
			break;
		value = (value * base) + c;		/* include in total */
		++input_ptr; }
	while(--digits);					/* enforce maximum digits */
	return value;
}




/*****************************************************************
* End of file has been encountered, dump the literal pool, and
* generate definitions for any external or uninitialized global
* variables.
******************************************************************/

static void clean_up(void)
{
	U32 type, size, i, j;

	if(in_function)
		fatal_error("Unterminated function");

	/* generate references for DASM */

	for(sptr = 0; sptr < global_top; ++sptr)
	{
		if((type = symtab[sptr].type) & (EXTERNAL))
			if (type & REFERENCE)
				gen_ext_data_DASM(sptr);
	}

/* dump literal pool */

	gen_literal(literal_pool, literal_top);

/* Generate all global variables that are not initialized */

	for(sptr = 0; sptr < global_top; ++sptr) {
		type = symtab[sptr].type;
		if(!(type & (FUNCTION | INITED | EXTERNAL | TYPDEF))) {

			if (type & (POINTER | DWORD)) size = 4;
			else if (type & WORD)  size = 2;
			else if (type & BYTE)  size = 1;
			else if (type & STRUCT) {
			  size = symtab[sptr].strucoff;		/* contains size */
			  }
			else size = 1;	/* should be an error maybe?? */

			if(type & ARRAY) {				/* calculate size of array */
				i = symtab[sptr].dindex;    /* i = index to dimpool */
				j = dim_pool[i++];			/* j = nDims */
				while(j--) size *= dim_pool[i++];
				}
			gen_global(sptr, size);
		}
	}

	fclose(temp_fh);

	if (!error_count) {  /* No optimize if errors */

		if (fNoOpt) {
			if(!(temp_fh = fopen(tmpname, "r")))
				fatal_error("Cannot re-open temp file");
			else
				while (fgets(buffer, LINE_MAX, temp_fh)) {
					if (fGen)
						fputs(buffer, code_fh);
					else
						fputs(buffer, asm_fh);
				}
		}
		else {
			if(!(temp_fh = fopen(tmpname, "r")))
				fatal_error("Cannot re-open temp file");
 			optimize();
		}
	}
	fclose(temp_fh);
	fclose(asm_fh);
	if (fLISTOpen)
		fclose(list_fh);
	if (fCODEOpen)
		fclose(code_fh);

	remove(tmpname); /*   leave out while troubleshooting  */

	if(!fQuiet) {
		put_num(error_count, stdout);
		put_str(" errors\r\n", stdout);
		put_num(warn_count, stdout);
		put_str(" warnings\r\n", stdout);
		}

	exit(0);  /* We are done ! */
}


/*****************************************
* Read a character from the input file
******************************************/

static char read_char(void)
{
	char c;

	while(!(c = *input_ptr++)) {		/* end of this line */
		if (!readline()) {
			clean_up();
			exit(error_count);
		}
	}
	return c;
}

/***********************************************************
* Allow a single token to be returned to the input stream
************************************************************/

static void unget_token(U32 token)
{
	ungot_token = token;
}


/************************************************
* Read special character (with translations)
*************************************************/

static U32 read_special(char delim)
{
	S32 c;

	if((c = read_char()) == delim)
		return 0xff00;
	if(c == '\\')
	  switch(c = read_char()) {
		case 'n':				/* newline */
			c = 0x0a;
			break;
		case 'r':				/* return */
			c = 0x0d;
			break;
		case 't':				/* tab */
			c = 0x09;
			break;
		case 'f' :				/* formfeed */
			c = 0x0c;
			break;
		case 'b':				/* backspace */
			c = 0x08;
			break;
		case 'v':				/* vertical tab */
			c = 0x0b;
			break;
		case 'x' :				/* hex value */
			c = get_number(16, 2);
			break;
		default:
			if(is_digit(c)) {		/* octal value */
				--input_ptr;
				c = get_number(8, 3);
	    }
	}
	return c & 0xff;
}

/*************************************************************************
* Get a token from the input stream, and return it as a simple value
* (indicating type). If it a special token type (NUMBER, STRING, SYMBOL),
* global variables "gvalue" and "gst" are set to appriopriate values.
**************************************************************************/

static U32 get_token(void)
{
	U32 i;
	char *ptr, *last_pos, chr;

/* if a token has been ungot, re-get it */
/* (gvalue & gst) will not have changed */
	if(ungot_token) {
		i = ungot_token;
		ungot_token = 0;
		return i;
	}

/* skip any leading whilespace  */
	do {
        chr = read_char();
		}
	while((chr == ' ') || (chr == '\t') ||
		(chr == '\n') || (chr == '\r'));

	--input_ptr;


/* lookup token in token table */
	last_pos = input_ptr;				/* remember where we were */

	if (itoken[*input_ptr] != 0) {
        	/* start at first match char */
	 	for(i=itoken[*input_ptr]; ptr = tokens[i]; ++i) {
			if (*input_ptr != *ptr) break;
			while((chr = *input_ptr) && (*ptr == chr)) {
				++ptr;
				++input_ptr; }
			if(!*ptr) {						/* we found a token */
				if(is_alpha(*(ptr-1)) && is_alpha(*input_ptr))
					continue;					/* not this token */
				return i; }
			input_ptr = last_pos; 			/* reset pointer */
		}
	}
/* we didn't find a token, check out special cases */
	input_ptr = last_pos;

	if((chr = *input_ptr) == '"')	{	/* string value */
		++input_ptr;
		gvalue = literal_top;		/* set to index to string */
		do {
			if(literal_top >= LITER_BUFF)
				fatal_error("String space exausted");
			literal_pool[literal_top++] = i = read_special('"');
		}
		while(!(i & 0xff00));	/* 0xff00 returned from read_special */
		return STRING; }

	/* Quoted values (e.g., '\n') are read into gvalue.
	   ANSI says that multibyte character constants are
	   implementation dependent.  SOOO, In our case we
	   shift multiple bytes to the left.  '\n\r' will
	   make gvalue equal 0x0D0A even though the \n (0x0A) is
	   first.
	 */

	if(chr == 0x27)
	{					/* quoted value */
		++input_ptr;
		while( !((i = read_special(0x27)) & 0xff00) )
			gvalue =  i & 0xff;	/* strip notify bits */
		return NUMBER;
	}


	if(is_digit(chr)) {					/* numeric constant */
		if(chr == '0') {
			++input_ptr;
			if((*input_ptr == 'x') || (*input_ptr == 'X')) {
				++input_ptr;
				gvalue = get_number(16, 0);			/* hex */
			}
			else
				gvalue = get_number(8, 0);      	/* octal */
		}
		else gvalue = get_number(10, 0);			/* decimal */

		/* Look for Unsigned and Long terminal characters */

		if((*input_ptr == 'U') || (*input_ptr == 'u')) {
			++input_ptr;
			if((*input_ptr == 'L') || (*input_ptr == 'l')) ++input_ptr;
		}
		else if((*input_ptr == 'L') || (*input_ptr == 'l')) ++input_ptr;

		return NUMBER;
	}

	if(is_alpha(chr)) {					/* symbol name */
		gvalue = 0;
		while(is_alnum(chr = *input_ptr)) {
			if(gvalue < SYMBOL_SIZE)
				gst[gvalue++] = chr;
			++input_ptr; }
		gst[gvalue] = 0;
		namesize = gvalue;
		return SYMBOL; }

/* not a token or special value */
	++input_ptr;			/* skip offending character */
	return -1;				/* report "unknown" token type */
}


/*****************************************************
* Locate a symbol in the local symbol table
******************************************************/

static U32 lookup_local(void)
{
	U16 i,j;

	i = MAX_SYMBOLS-1;
	while(i > local_top-1) {
		j = symtab[i].oname;
		if(equal_string(gst, &LPool[j]))
			return symtab[sptr=i].type |= REFERENCE;
	    i--;
    }
	return 0;
}


/*************************************************
  Locate a symbol in the global symbol table.
**************************************************/

static U32 lookup_global(void)
{
	U16 i, j;

	for(i=0; i < global_top; i++) {
		j = symtab[i].oname;
		if(equal_string(gst, &GPool[j]))
			return symtab[sptr=i].type |= REFERENCE;
	}
	return 0;
}

/**************************************************
  Locate a structure member in either symbol table
***************************************************/

static U32 lookup_member(U16 CrntStruc)
{
	U16 i, j, CrntStrucDef;

	/* structure member names were hidden in the tables
	   by inserting a '0' as the first char.  Now we
	   must make the current symbol name match
	   so we can find it in the tables.  It will follow
	   the structure definition entry for CrntStruc.
	*/

	for (i=SYMBOL_SIZE; i > 0; i--)		/* fix member name  */
           gst[i] = gst[i-1];
     gst[0] = '0';					/* make it start with digit */
     ++namesize;

	CrntStrucDef = symtab[CrntStruc].itypedef;

	if (symtab[CrntStrucDef].type & GLOBAL) {

		for(i=CrntStrucDef+1; i < global_top; i++) {
			if (!(symtab[i].type & STRUCMEM))
				break;
			j = symtab[i].oname;
			if(equal_string(gst, &GPool[j]))
				return symtab[sptr=i].type |= REFERENCE;
		}
	}
	else {	/* must be local */

		for(i=CrntStrucDef-1; i > local_top-1; i--) {
			if (!(symtab[i].type & STRUCMEM))
				break;
			j = symtab[i].oname;
			if(equal_string(gst, &LPool[j]))
				return symtab[sptr=i].type |= REFERENCE;
		}
	}
	return 0;		/* didn't find it! */
}

/***************************************************************
  Enter a symbol in the symbol table with specified name & type.
  Leaves global sptr with the index of the symbol just added.
****************************************************************/

static void define_symbol(U32 type, U32 dim_index)
{
	U32 index;
	U16 i, j;

	if(in_function) {		/* within a function, use local */
		if (type&PROTO) {	/* give it a false name to satisfy symbol table */
			gst[0] = '_';				/* 2 underscores */
			gst[1] = '-';
			gst[2] = arg_count + 65;	/* A-Z */
			gst[3] = '\0';					/* null terminate */
			namesize = 3;
			}
		else {
			if(lookup_local()) {
				symbol_error("Duplicate local or arg");
				return;
				}
			}

		sptr = --local_top;
		if(type & ARGUMENT) {

			type &= ~PROTO;  /* turn off proto warning */

			/* if a prototype arg already exists, make sure it's the same */
			if (icrntpro) {
				if (proto_list[icrntpro++] != type) {
				  j=symtab[fptr].oname;
			      t_warn("Arg not same type as prototype in ", &GPool[j]);
				}
			}
			else proto_list[iproto_next++] = type;
			if(iproto_next>MAX_PROTOS)
			  fatal_error("Prototype table full");
		}
		else
			index = local_stack;

		if(global_top > local_top)
			fatal_error("Symbol table full");
		if((oNextLName+SYMBOL_SIZE) > LBUFFSIZE)
			fatal_error("Local symbol name pool full");
		symtab[sptr].oname=oNextLName;
		copystring(&LPool[oNextLName], gst);
		oNextLName += namesize;
		oNextLName++;				/* for null */
		symtab[sptr].type = type;
		symtab[sptr].argoffset = index;
		symtab[sptr].dindex = dim_index;

	}
	else {					/* outside of function, use global */
		type |= GLOBAL;
		if(index = lookup_global()) {		/* symbol already exists */
			if(index & (PROTO|FUNCTION)) {  /* re-definition */
				if((index | (INITED|REFERENCE|EXTERNAL|PROTO)) !=
					(type | (INITED|REFERENCE|EXTERNAL|PROTO)))
					symbol_error("Inconsistant re-declaration");
				symtab[sptr].type = type;
				return;
			}
			else if (type & STRUCMEM)
				 {}		/* no error */
			else if ((type & STRUCDEF) &&
					 (equal_string(&structname[0], &GPool[symtab[sptr].oname])))
				 {}		/* no error */
			else {
				symbol_error("Duplicate global");
				return; }
		}
		sptr = global_top++;
		index = global_count++;
		if(global_top > local_top)
			fatal_error("Symbol table full");
		if((oNextGName+SYMBOL_SIZE) > GBUFFSIZE)
			fatal_error("Global symbol name pool full");
		symtab[sptr].oname = oNextGName;
		copystring(&GPool[oNextGName], gst);
		oNextGName += namesize;
		oNextGName++;			 /* for null */
		symtab[sptr].type = type;
		symtab[sptr].argoffset = index;
		symtab[sptr].dindex = dim_index;
	}
}


/******************************************
* Push a value on the expression stack
*******************************************/

static void push(U32 token, U32 value, U32 type, U32 offset)
{
	if(expr_ptr >= EXPR_DEPTH)
		fatal_error("Expression stack overflow");

	expstk[expr_ptr].token = token;
	expstk[expr_ptr].value = value;
	expstk[expr_ptr].type  = type;
	expstk[expr_ptr].offset = offset;
	expr_ptr++;
}

/*********************************************
* Pop a value from the expression stack
**********************************************/

static void pop(U32 *token, U32 *value, U32 *type, U32 *offset)
{
	if(!expr_ptr)
		fatal_error("Expression stack underflow");

    expr_ptr--;
	*token  = expstk[expr_ptr].token;
	*value  = expstk[expr_ptr].value;
	*type   = expstk[expr_ptr].type;
	*offset = expstk[expr_ptr].offset;
}


/********************************************
* Get a constant value (NUMBER or STRING)
* which can be evaluated at compile time.
*********************************************/

static void get_constant(U32 *token, U32 *value)
{
	U32 type, offset;

	expr_ptr = 0;
	unget_token(do_oper(SEMI));		/* do_oper gets the token... */

	pop(token, value, &type, &offset);	/* then we pop it into token */

	if((*token != NUMBER) && (*token != STRING))
		line_error("Constant expression required");
}

/********************************
* Define a variable
*********************************/

static void define_var(U32 type)
{
	U32 token, stype, lasttoken, value, index, size, i, j, ocbcnt;
	U32 sDim1, nDims, iDim, eTotal;
	char eflag, nflag;

	if(in_function > 1)
		line_error("Declaration must preceed code");

/* calculate base variable size - store in j for later use if array */

	if (type&BYTE) size = 1;
	else if (type&WORD)  size = 2;
	else if (type&DWORD) size = 4;
	else if (type&STRUCT)
		size = symtab[CrntStrucDef].strucoff;
	else
		line_error("Type specifier missing");

	if (type&(ARGUMENT | POINTER))  size = 4; /* pointers are 32 bit offsets */
	j = size;

/* If fInStruct then this is a structure member definition.
   NOT a structure member! */

	if (fInStruct) {
		type |= (STRUCMEM|TYPDEF);			/* make it a member definition */
		for (i=SYMBOL_SIZE; i > 0; i--)		/* fix member name  */
            gst[i] = gst[i-1];
        gst[0] = '0';					/* make it start with digit */
        ++namesize;
        }

/* evaluate any array indexes */

	iDim = dim_top;
	nflag = 0;
	nDims = 0;
	while(test_token(OSB)) {			/* array definition */
		++nDims;
		++dim_top;
		if(test_token(CSB)) {		/* null definition */
			if ((nflag) || (nDims > 1))
				line_error("Null only allowed in first index");
			--nflag;
			size *= dim_pool[dim_top] = 1;  /* dummy up unbounded array */
			continue; }
		get_constant(&token, &value);
		if(token != NUMBER)
			line_error("Numeric constant required");
		size *= dim_pool[dim_top] = value;
		expect(CSB);
	}

	if(nDims) {			/* defining an array */
		type |= ARRAY;
		dim_pool[iDim] = nDims;
		if(++dim_top > DIM_SIZE)
			fatal_error("Dimension table full");
	}

	if(test_token(ASSIGN)) 		/* initialized variable */
		type |= INITED;

	local_stack += size;		/* Keep track of offset of local vars */

	define_symbol(type, iDim);	/* Create the symbol table entry */

	if (type&(STRUCMEM|TYPDEF)) {
		symtab[sptr].itypedef = CrntStrucDef;
		symtab[sptr].strucoff = symtab[CrntStrucDef].strucoff;
	    symtab[CrntStrucDef].strucoff += size;	/* add to the total */
		}

	if (type&(STRUCT)) {
		symtab[sptr].itypedef = CrntStrucDef;
		symtab[sptr].strucoff = symtab[CrntStrucDef].strucoff;
		}

/*
   Initialization of arrays has changed with ANSI. Nested braces and
   trailing commas are now allowed. We make sure the braces balance.
*/
	eflag = -1;		/* Expexting initializer */
	sDim1 = 0;		/* tracks nDimensions for unbounded arrays [] */
	eTotal = 0;		/* Total elements initialized so far */

	if(type & INITED) {		/* force immediate allocation */
		if ((in_function) || (fInStruct))
			line_error("Illegal initialization");
		data_global(sptr);	/* generate label in DSeg */
		ocbcnt = 0;  	/* number of open brackets */
		index = 0;		/* tracks index for current dimensions */
		do {
			switch (token=get_token()) {
			case OCB:
				if ((nflag) && (ocbcnt == 1))	/* count for unbounded */
					++sDim1;
				ocbcnt++;
				if (ocbcnt > nDims)
					line_error("Too many open braces");
				lasttoken = OCB;
				eflag = -1;
				break;
			case CCB:
				if (ocbcnt) --ocbcnt;
				else
					line_error("Unbalanced braces");
				if ((nDims > 1) && (ocbcnt))  {
					while (index < dim_pool[dim_top-1]) {
						init_static(NUMBER, 0, j);
						++index;
						}
					}
				eTotal+=index;		/* total inited so far */
				index = 0;			/* tracks index for current dimensions */
				eflag = 0;			/* no constant expected now! */
				lasttoken = CCB;
				break;				/* we need a comma first/ocb first */
			case COMMA:
				/* secial case of char[x][y] = {"abc","def","ghi"}; */
				if ((nDims > 1) && (ocbcnt==1) &&
				    (lasttoken==STRING) && !(type & POINTER)) {
					while (index < dim_pool[dim_top-1]) {
						init_static(NUMBER, 0, j);
						++index;
						}
					eTotal+=index;		/* total inited so far */
					index = 0;
					}
				if (lasttoken==CCB) eflag = 0;
				else eflag = -1;
				lasttoken = COMMA;
				break;
			case STRING:
				/* special case of char[x] = "xx"; or char[] = "xx";  */
				if ((!ocbcnt) && (nDims==1))
					eflag = -1;
			case NUMBER:
			case SUB:
			case COM:
			case NOT:
				unget_token(token);		/* put it back */
				if (eflag) {			/* we are expecting a constant */
					if ((nflag) && (ocbcnt == 1))
						++sDim1;
					get_constant(&token, &value);
					if((token == STRING) && !(type & POINTER)) {
						do {
							init_static(NUMBER, literal_pool[value], j);
							++index;}
						while(++value < literal_top);
						literal_top = gvalue;

						/* special case of char[]="xx" or char[x]="xx";  */
						if ((!ocbcnt) && (nDims==1)) {
							eTotal = index;
							sDim1 = index;
							}
						}
					else {
						init_static(token, value, j);
						if (!nDims)
							++eTotal;	/* tracked by index if array */
						++index;
						}
					lasttoken=token;
					}
				else line_error("Improper bracketed initialization");
				break;
			case SYMBOL:
				lasttoken=token;
				if (eflag) {	/* we are expecting an initializer */
					if (type&POINTER) {
						if(stype=lookup_global()) {
							if (stype&(EXTERNAL|FUNCTION))
								symbol_error("Invalid init type");
							else {
								init_static(SYMBOL, sptr, j);
								index += j;
								}
							}
						else symbol_error("Undefined");
						}
					else line_error("Must be pointer array");
					}
				else line_error("Improper bracketed initialization");
				break;
			default:
				line_error("Improper symbol in initialization");
				break;
			}
		}
		while(((ocbcnt) || (lasttoken==COMMA)) && (token!=SEMI));
		if(ocbcnt) expect(CCB);		/* make sure brackets balance!! */

		if(nflag) 				/* fixup null definition */
			dim_pool[iDim+1] = sDim1;

		/* new we make sure all the array elements were initialized
		   to ensure we have allocated Dseg for them.
		*/

		i=1;
		while (nDims) i*= dim_pool[iDim+nDims--];	/* i has total elements */

		while (eTotal < i) {		/* eTotal is total inited */
			init_static(NUMBER, 0, j);
			++eTotal;
		}

		if (eTotal > i)
			line_error("Too many initial values");

		end_static();

	}  /* if (INITED) */

}

/*****************************************************
* Check that we are within a function definition
******************************************************/

static void check_func(void)
{
	if(in_function) {
		if(in_function < 2) {		/* first invocation */
			in_function = 2;
			enter_func(fptr, local_stack); } }
	else
		line_error("Incorrect declaration");
}

/**********************************************************
* Declare a symbol (function or variable) with modifiers.
  If we get here we have found a legal type specifier.
  Definitions other than structures are quite simple. We
  loop through eating the variables behind it adding them
  to the symbol table.
  Structures are handled differently because you can define
  a structure with a TAG without actually allocating space.
  This is actually a type definition.
  In fact, you can define the structure with an optional
  tag, then place the variable names right behind it.
  This checks to see if we have a tag name first. If the tag
  name is present, we then check to see if it's a defined
  structure already, in which case we expect a new symbol
  name will follow. If the tag is not already defined than
  we expect {} with a structure definition in between!
***********************************************************/

static void declare(U32 token, U32 type)
{
    fInStruct = 0;
	for(;;) {
		switch(token) {
			case CHAR:
				type &= ~WORD;		/* cancel WORD */
				type |= BYTE;
				break;
			case INT:
				if (!(type&DWORD)) type |= WORD;
				break;
			case SHORT:
				type |= WORD;
				break;
			case LONG:
				type &= ~WORD;		/* cancel WORD */
				type |= DWORD;
				break;
			case UNSIGN:
				type |= WORD;		/* cancelled for char or long */
				type |= UNSIGNED;
				break;
			case SIGNED:
				type |= WORD;		/* this will be cancelled if it's long */
				type &= ~UNSIGNED;
				break;
			case STAT:
				type |= STATIC;
				break;
			case STRUC:
				type |= STRUCDEF;
				break;
			case CONST:
				type |= CONSTANT;
				break;
			case EXTERN:
				type |= EXTERNAL;
				break;
			case REGIS:
				type |= REGISTER;
				break;
			case INTR:
				type |= ISR;	/* ISR modified for functions */
				break;
			case VOIDD:
				type |= VOID;
				break;
			case FARR:
				type |= FAR;
				break;
			case STAR:		/* pointer reference */
				do
					++type;
				while(test_token(STAR));

				/* allow for prototype arg pointers with no symbol name */

				if((type&ARGUMENT) && (test_token(COMMA))) {
					define_var(type|=PROTO); /* tell 'em it may be symboless */
					return;
					}
				else
				if(!test_token(SYMBOL)) syntax_error();
			case SYMBOL:		/* we have a symbol name */
				/* If STRUCDEF, MUST be a Struct Tag Name (new or existing) */
				if(type & STRUCDEF) {      /* This symbol follows "struct" */
					if (lookup_global()) {   /* Existing tag ?? */
						if (symtab[sptr].type &	STRUCDEF) {
		                    CrntStrucDef = sptr;  /* tag we just found */
							/* might be a pointer to a structure */
							if (test_token(STAR))
								do
									++type;
								while(test_token(STAR));
							/* now expect a new structure variable name */
							if(!test_token(SYMBOL)) {
							  line_error("struct variable expected");
							  return;
							}
							else {
							  type &= ~STRUCDEF; /* struct variable w/tag */
							  type |= STRUCT;
							}
 						}
						else
						  line_error("struct tag name expected");
					}

					/* we didn't find the tag so it must be a new one! */

					else {	   /* So add the strucdef */
						define_symbol(type|TYPDEF, 0);
	                    CrntStrucDef = sptr;  	/* symbol we just found */
						if(!(test_token(OCB)))   /* expecting {          */
						  line_error("Structure { expected");
						else {
					    	fInStruct = 1;		  /* we are in it now...  */
					    	type = 0;
					    	break;
						}
					}
				}
				if(type&ARGUMENT) {
					define_var(type);
					return;
					}
				if(test_token(ORB))
					define_func(type);	/* function definition */
				else
					define_var(type);	/* variable definition */
				if(test_token(COMMA))
					break;

				test_token(SEMI);  /* eat the semicolon if there */

				if (fInStruct) {	/* still defining struct members */
					type = 0;
					break;
					}
				else
					return;
			case OCB:		/* '{' only allowed for immediate struct defs */
				if(type & STRUCDEF) {    /* Immediate struct definition */
					copystring(gst, structname);
					gvalue = 11;
					define_symbol(type|TYPDEF, 0);
                    CrntStrucDef = sptr;  /* symbol we just added */
			    	fInStruct = 1;		  /* we are in it now...  */
			    	type = 0;
					break;
					}
				else
					syntax_error();
				break;
			case CCB:
				if (!fInStruct) {
					syntax_error();
					return;
					}
				else {
				    type &= ~STRUCDEF; /* struct variable */
				    type |= STRUCT;
					fInStruct = 0;
					}
				if (test_token(SEMI))  /* if semicolon, eat it and return */
					return;
				break;
			case CRB:
				unget_token(token);		/* fall thru to COMMA */
			case COMMA:
				if(type&ARGUMENT) {
					define_var(type|=PROTO);
					return;
					}
				break;
			default:
				syntax_error();

		 }
		type &= ~(POINTER | ARRAY);  /* clear indirection and rg of last var */
		token = get_token();
	}
}

/**************************************************************
Define a function

Notes on prototype handling:

We have an array called proto_list and an index to the list called
iproto_next. For any function (external or global), we keep track
of the argument types in the proto list.  We do this by assuming that the
first time a function is identified, we define it as if it was a real
function.  If it turns out to be a proto, we just throw away the
local arguments and mark it as proto.  If we find a function that is
being defined and it's args don't match the defined types, emit a warning
or error (if type is not convertable).
In any case, this means we now know what type of argument
to expect for any function that is called. We can do the proper
type manipulations and flag errors when found.
This also means you must prototype all external function, and
that all of them MUST have all arguments defined!!!
As per ANSI - the proto MUST contain the type of each arg, and
may or may not provide a name for each arg.

For functions that are defined with variable parameter lists (unknown
numbers of args and type), we add a single identifier in the Proto list
that indicates this.  The arg definition for variable parameters is
an elipse (...).
All args in a function with variable args are accessed differently than
those with fixed parameters.  Normally the EBP register accesses the
args with a fixed offset.  When you have variable args (and becuase we
push args from left to right) we won't know where the fixed args are or
where the variable args begin.  To find out where they are and access them
we push the number of variable args as the last arg on the stack.
From this value we can determine where the variable args begin and the fixed
args leave off.  We access the fixed args with [EBP+EDI+offset]
to access the args.  This is because the fucntion "va_arg" will set
EDI up with the proper context to access the variable required. See
stdarg.c for more information.

****************************************************************/

static void define_func(U32 type)
{
	U32 token, dim_save, t, flastarg, i, stackpop;

	icrntpro = 0;
	flastarg = 0;
	arg_count = 0;
	oNextLName = 0;

	/* do not allow functions or protos within functions */

	if(in_function) {
		line_error("Illegally nested function or prototype");
		return;
	}

	if(t = lookup_global()) { 		/* symbol already exists */
		if (t & PROTO) {
			icrntpro = symtab[sptr].dindex;	/* icrntpro points to arg types */
		}
	}
	else {
		define_symbol(type | FUNCTION | GLOBAL, 0);
        symtab[sptr].dindex = iproto_next;
	}

	fptr = sptr;

/* accept variable declarations for local arguments */
/* These must be inside the parens -- ANSI style    */

	local_top = MAX_SYMBOLS;
	dim_save = dim_top;
	in_function = 1;					/* indicate inside a function */

	do {
		switch(token = get_token()) {	/* define local arguments */
			case SHORT:
			case LONG:
			case INT:
			case SIGNED:
			case UNSIGN:
			case CHAR:
			case FAR:
			case CONST:
			case STRUC:
				declare(token, ARGUMENT);
				arg_count += 1;
				break;
			case COMMA: break;
			case ELIPSE:
				proto_list[iproto_next++] = VOID;
				flastarg = -1;
				symtab[fptr].type |= VARARGS;
				break;
			case VOIDD:
				if (arg_count) syntax_error(); 	/* fall through to default */
			default:
				if (!flastarg) {
					proto_list[iproto_next++] = VOID;
					if(iproto_next>MAX_PROTOS)
					  fatal_error("Prototype table full");
					flastarg = -1;
				if (token==CRB) unget_token(token);
				}
				break;
		}
	} while (!flastarg);

	token = get_token();
	if(token != CRB)
		syntax_error();

	if(test_token(SEMI)) {		/* a prototype function !! */
		symtab[fptr].type |= PROTO;
		in_function = 0;
		exit_label = 0;
		exit_flag = 0;
		exit_used = 0;
		dim_top = dim_save;
		if (symtab[fptr].type & EXTERNAL)
			gen_extern_DASM(fptr);
		}
	else {
		symtab[fptr].type &= ~PROTO;
		symtab[fptr].type |= FUNCTION;
		local_stack = 0;
		exit_label = 0;
		exit_flag = 0;
		exit_used = 0;
		stackpop = 0;

		/* Loop through the args putting in correct stack offsets
		NOTE: For running 32 bit code in a 16 bit environment, the
		value to be added to stackpop is 6 for each item on the stack.
		For a 32 bit environment its 8 for each.

         +-----+-----+ xx
         |Last param |     The last parameter pushed is here
         +-----+-----+ 06/8/0C
         | n Bytes   |     Return Address (2, 4 or 8)
         +-----+-----+ 04
         | 4 Bytes   |     Previous Frame Pointer
         +-----+-----+ 00

		The Return Address is either 2, 4 or 8 bytes depending on
		the environment and whether the call was Near or Far.
		16 Bit segments -  n = 2 for Near, 4 for Far
		32 Bit segments -  n = 4 for Near, 8 for Far

		*/

		for(i=arg_count; i>0; i--) {
			if (symtab[fptr].type & FAR)
				symtab[MAX_SYMBOLS-i].argoffset = stackpop+12; /* 12 for 32 far */
			else
				symtab[MAX_SYMBOLS-i].argoffset = stackpop+8; /* 8 for MMURTL */
			stackpop += 4;
		}
		statement(token=get_token());
		check_func();	/* ensure enter gets written in null func */
		if((exit_label) && (exit_used))
			gen_label(exit_label);
		if (symtab[fptr].type & VARARGS)
			end_func(0);
		else
			end_func(stackpop);
		in_function = 0;
		exit_label = 0;
		exit_flag = 0;
		exit_used = 0;
		dim_top = dim_save;

		/* Error reporting when end of func is reached and certain
		   conditions exist */

		while(local_top < MAX_SYMBOLS) {
			if((token = symtab[local_top].type) & EXTERNAL)
				t_error("Unresolved", &LPool[symtab[local_top].oname]);
  			if(!(token & REFERENCE))
				t_warn("Unreferenced", &LPool[symtab[local_top].oname]);
			++local_top; }
		return;
	}
}


/***********************************************************************
  Write an assembler string to access an operand value.
  Certain types of operands may require type inducers suxh as "BYTE PTR."
************************************************************************/
static void write_oper(U32 token, U32 value, U32 type, U32 offset)
{

	switch(token) {
		case INEAX:
			code_str("EAX");
			break;
		case NUMBER:
			code_num(value);
			break;
		case STRING:
			code_str("OFFSET ");
			code_chr(prefix);
			code_str("_lit+");
			code_num(value);
			break;
		case SYMBOL:
			if(type & GLOBAL) {
				code_chr('_');		/* prefix with _ */
				code_str(&GPool[symtab[value].oname]);
				break; }
			if(type & ARGUMENT)
			{
				if (symtab[fptr].type & VARARGS)
				{
					if (type&(DWORD|POINTER))
						code_str("DWORD PTR [EBP+EDI+");
					else if (type & WORD)
						code_str("WORD PTR [EBP+EDI+");
					else
						code_str("BYTE PTR [EBP+EDI+");
					code_num(symtab[value].argoffset);
					code_chr(']');
				}
				else
				{
					if (type&(DWORD|POINTER))
						code_str("DWORD PTR [EBP+");
					else if (type & WORD)
						code_str("WORD PTR [EBP+");
					else
						code_str("BYTE PTR [EBP+");
					code_num(symtab[value].argoffset);
					code_chr(']');
				}
			}
			else
			{
				if (type&(DWORD|POINTER))
					code_str("DWORD PTR [EBP-");
				else if (type & WORD)
					code_str("WORD PTR [EBP-");
				else
					code_str("BYTE PTR [EBP-");
				code_num(symtab[value].argoffset);
				code_chr(']');
			}
			break;
		case INECX:
			code_str("ECX");
			break;
		case INEDX:
			code_str("EDX");
			break;
		case PESI:
		case PEDX:
		case PECX:			/* pointer in other reg - indirect access */
		case ISTACK_TOP:
			if (type&(DWORD|POINTER))
				code_str("DWORD PTR ");
			else if (type & WORD)
				code_str("WORD PTR ");
			else
				code_str("BYTE PTR ");

			if (offset)
			{
				if (token==PESI)
					code_str("[ESI+");
				else if (token==PECX)
					code_str("[ECX+");
				else if (token==PEDX)
					code_str("[EDX+");
				else
					code_str("[EBX+");
				code_num(offset);
				code_chr(']');
			}
			else
			{
				if (token==PESI)
					code_str("[ESI]");
				else if (token==PEDX)
					code_str("[EDX]");
				else if (token==PECX)
					code_str("[ECX]");
				else
					code_str("[EBX]");
			}
			break;
		case STACK_TOP:
			code_str("EBX");
			break;

		case ION_STACK:		/* shouldn't happen */
		case ON_STACK:
		default:		/* Unknown (error) */
			code_num(token);
			code_str(" ERROR in write_oper\n");
	}
}


/**************************************************
  Places operand in code string and sends instruction
  out to code segment.
**************************************************/

static void GenCodeOper(char *ptr, U32 token, U32 value, U32 type, U32 offset)
{
	/* interpret the output string & insert the operand */

	code_chr('\t');
	while(*ptr) {
		if(*ptr == '|')
			write_oper(token, value, type, offset);
		else
			code_chr(*ptr);
		++ptr;
	}
	code_chr('\n');
}

/*************************************************************
  Examine the expression stack and see if any active token
  is in EAX. If so, place it on the stack.
  The stack top is really the EBX register. If we find
  an active item in the EBX register, we must place
  it on the real processor stack first and change it's
  token to show where it is.
**************************************************************/

static void StackEAX(void)
{
	S32 i, j;

	for(i=0; i < expr_ptr; ++i) {
		if (expstk[i].token == INEAX) 		/* Found it */
		{
			for(j=0; j < expr_ptr; ++j)
			{
				if ((expstk[j].token == STACK_TOP) ||
					(expstk[j].token == ISTACK_TOP))
				{
					out_inst("PUSH EBX");
					if (expstk[j].token == STACK_TOP)
                        expstk[j].token = ON_STACK;
					else
                        expstk[j].token = ION_STACK;
					break;
				}
			}
			test_not();
			out_inst("MOV EBX,EAX");
			expstk[i].token = STACK_TOP;
		}
	}
}

/**********************************************************
 If the token we need is actually on the processor stack,
 we must must pop it out so we can use it. To do this,
 we pop it into EDX. EDX is only used for multiply and
 divide operations otherwise.
***********************************************************/
static U32 CheckStack(U32 token)
{
	if (token==ION_STACK)
	{
		out_inst("POP EDX");
		token = PEDX;
	}
	else if	(token==ON_STACK)
	{
		out_inst("POP EDX");
		token = INEDX;
	}
	return(token);
}

/**************************************************************
 Get a parameter into the EAX register.
 The value/register is always sign or zero extended to 32 bits!
***************************************************************/

static void LoadEAX(U32 token, U32 value, U32 type, U32 offset)
{
	if(type & FUNCTION)
		type_error();

	if(token == INEAX) {		/* Already there */
		test_not();
		return;
	}

	token = CheckStack(token);   /* If it's on the processor stack, get it into EDX */

	StackEAX();				/* stack EAX if needed */
	not_flag = 0;

	if((token == NUMBER) && (!value)) {		/* 0 Value */
		test_not();
		code_str("\tXOR EAX,EAX\n");
		return;
	}

	if ((type&(DWORD|POINTER)) ||
		(token == NUMBER) ||
		(token == INEDX) ||
		(token == STACK_TOP) ||
		(token == INECX))

		GenCodeOper("MOV EAX,|", token, value, type, offset);
	else if (type & WORD) 
	{
		if (type & UNSIGNED)
			GenCodeOper("MOVZX EAX,|", token, value, type, offset);
		else
			GenCodeOper("MOVSX EAX,|", token, value, type, offset);
	}
	else
	{
		if (type & UNSIGNED)
		{
			code_str("\tXOR EAX,EAX\n");
			GenCodeOper("MOV AL,|", token, value, type, offset);
		}
		else
			GenCodeOper("MOVSX EAX,|", token, value, type, offset);
	}

	zero_flag = -1;
}

/**************************************************************
 Get a parameter into the ECX register.
 The value/register is always sign or zero extended to 32 bits!
***************************************************************/

static void LoadECX(U32 token, U32 value, U32 type, U32 offset)
{
	if(type & FUNCTION)
		type_error();

	if(token == INECX)
	{		/* Already there */
		return;
	}

	token = CheckStack(token);   /* If it's on the processor stack, get it into EDX */

	if ((token == NUMBER) && (!value))
	{		/* 0 Value */
		code_str("\tXOR ECX,ECX\n");
		return;
	}

	if ((type&(DWORD|POINTER)) ||
		(token == NUMBER) ||
		(token == INEDX) ||
		(token == STACK_TOP) ||
		(token == INEAX))

		GenCodeOper("MOV ECX,|", token, value, type, offset);
	else if (type & WORD) {
		if (type & UNSIGNED)
			GenCodeOper("MOVZX ECX,|", token, value, type, offset);
		else
			GenCodeOper("MOVSX ECX,|", token, value, type, offset);
	}
	else
	{
		if (type & UNSIGNED)
		{
			code_str("\tXOR ECX,ECX\n");
			GenCodeOper("MOV CL,|", token, value, type, offset);
		}
		else
			GenCodeOper("MOVSX ECX,|", token, value, type, offset);
	}
}


/****************************************************************
* Evaluate a sub expression & handle COMMA operator. This must
* be done as a special case, because the function performed by
* COMMA differs with the context of the expression, and can not
* therefore be handled as a general operator.
*****************************************************************/

static void sub_eval(U32 term)
{
U32 token;

	for(;;) {
		if((token = do_oper(SEMI)) != COMMA) {
			unget_token(token);
			expect(term);
			return;
		}
		pop(&token, &token, &token, &token);	/* throw it away */
	}
}


/********************************************************
* Evaluate a full expression at the highest level, and
* load the result into the accumulator if necessary.
*********************************************************/

static void eval(U32 term, char flag)
{
U32 token, value, type, offset;

	expr_ptr = 0;
	not_flag = 0;
	sub_eval(term);

	pop(&token, &value, &type, &offset);
	if((token != INEAX) || flag)
		LoadEAX(token, value, type, offset);
}

/************************************************
* Write an instruction with text formatting
  to the code tmp file
*************************************************/

static void out_inst(char *ptr)
{
	code_chr('\t');
	code_str(ptr);
	code_chr('\n');
}


/*************************************************************
  Examine the expression stack and see if any active token
  is in the EBX register which acts as the stack top for
  fast access. If so, put it on the real stack because
  we must preserve it through a call.
**************************************************************/

static void StackTop(void)
{
	S32 i, j;

	for(j=0; j < expr_ptr; ++j)
	{
		if ((expstk[j].token == STACK_TOP) ||
			(expstk[j].token == ISTACK_TOP))
		{
			out_inst("PUSH EBX");
			if (expstk[j].token == STACK_TOP)
    			expstk[j].token = ON_STACK;
			else
	            expstk[j].token = ION_STACK;
			break;
		}
	}
}


/*************************************************************
  Examine the expression stack and see if any active token
  is in the Index Reg. If so, place it on the stack.
  The stack top is really the EBX register. If we find
  an active item in the EBX register, we must place
  it on the real processor stack first and change it's
  token to show where it is.
**************************************************************/

static void StackESI(void)
{
	S32 i, j;

	for(i=0; i < expr_ptr; ++i) {
		if (expstk[i].token == PESI) 		/* Found it */
		{
			for(j=0; j < expr_ptr; ++j)
			{
				if ((expstk[j].token == STACK_TOP) ||
					(expstk[j].token == ISTACK_TOP))
				{
					out_inst("PUSH EBX");
					if (expstk[j].token == STACK_TOP)
                        expstk[j].token = ON_STACK;
					else
                        expstk[j].token = ION_STACK;
					break;
				}
			}
			out_inst("MOV EBX,ESI");
			expstk[i].token = ISTACK_TOP;
		}
	}
}


/*****************************************
* Load index address for an array
******************************************/

static void load_index(U32 t, U32 v, U32 tt, U32 o)
{
		StackESI();
		if((tt & ARGUMENT) || !(tt & ARRAY)) 	/* pointer or argument */
			index_ptr(t, v, tt, o);
		else									/* standard array */
			index_adr(t, v, tt, o);
}


/*************************************************************************
* Evaluate a unary operation, if possible, evaluate constant expressions
* into another constant. Produce code to perform operation if necessary.
**************************************************************************/

static void do_unary(U32 oper)
{
	U32 token, value, type, offset;
	char flag;

	pop(&token, &value, &type, &offset);
	flag = 0;
	/* Evaluate any operations that can be performed at compile time */
	if(token == NUMBER)
	{
		flag = -1;
		switch(oper)
		{
			case SUB :		/* unary minus */
				value = -value;
				break;
			case COM:		/* ones complement */
				value = ~value;
				break;
			case NOT:		/* logical complement */
				value = !value;
				break;
			default:
				flag = 0;
		}
	}

	/* Generate code to perform operation */
	if(!flag)
	{
		switch(oper)
		{
			case SUB:				/* unary minus */
				GenCodeOper("NEG |", token, value, type, offset);
				break;
			case COM:				/* ones complement */
				GenCodeOper("NOT |", token, value, type, offset);
				break;
			case NOT:				/* logical complement */
				LoadEAX(token, value, type, offset);
				token = INEAX;
				not_flag = TRUE;
				break;
			case INC:				/* '++' increment token & load */
				if (ispStruct(type, value))
					GenCodeOper("ADD |,strucsize", token, value, type, offset);
				else if (isp32(type))
					GenCodeOper("ADD |,4", token, value, type, offset);
				else if (isp16(type))
					GenCodeOper("ADD |,2", token, value, type, offset);
				else
					GenCodeOper("INC |", token, value, type, offset);
				break;
			case DEC:				/* '--' decrement & store */
				if (ispStruct(type, value))
					GenCodeOper("SUB |,strucsize", token, value, type, offset);
				else if (isp32(type))
					GenCodeOper("SUB |,4", token, value, type, offset);
				else if (isp16(type))
					GenCodeOper("SUB |,2", token, value, type, offset);
				else
					GenCodeOper("DEC |", token, value, type, offset);
				break;
			default:
				syntax_error();
		}
	}
	push(token, value, type, offset);
}


/******************************************************
 This evaluates array indeces for get_value.
*******************************************************/

static S8 eval_index(U32 t, U32 v, U32 tp, U32 ofs, U32 *tpRet)
{
	S32 ndim, vcnt;
	U32 token, tp1, dptr, iSym, vsize, ofs1;
	char fMultiDim;

	fMultiDim = FALSE;	/* true when processing second or subsequent dims */

	if(tp & ARRAY) {			/* array, get # dimensions */
		dptr = symtab[v].dindex;
		ndim = dim_pool[dptr++];
		}
	else					/* pointer, fake # dims as 1 */
		dptr = ndim = 1;

	iSym = v;			/* save index into symtab */

	push(t, v, tp, ofs);	/* save symbol we are indexing on exp stack */

	do {				/* calculate index */

		/* this gets the size of variable into vsize */

		v = tp & (POINTER | ARRAY);
		if ((v==ARRAY) || (v < 2)) {
			 if (tp & BYTE) vsize = 1;
			 else if (tp & WORD) vsize = 2;
			 else if (tp & DWORD) vsize = 4;
			 else if (tp & STRUCT)
			 	vsize = symtab[iSym].strucoff; /* strucoff = size of struc*/
		 }
		else vsize = 4;
		--ndim;
		if(tp & ARRAY)  		/* array reference */
		{
			t = ++dptr;
			if(!(v = ndim))		/* all indices given, load pointer */
				tp &= ~ARRAY;	/* Not an array ref anymore, ptr instead */
			while(v--)
				vsize *= dim_pool[t++];
		}
		else
		{
			if(tp & POINTER)
			{		/* pointer reference */
				--tp;				/* drop defer level by one */
				if(fMultiDim)
				{					/* array of pointers */
					pop(&t, &v, &tp1, &ofs1);
					LoadEAX(t, v, tp1, ofs1);
					pop(&t, &v, &tp1, &ofs1);
					load_index(t, v, tp1, ofs1);
					out_inst("ADD ESI,EAX");
					push(t = PESI, v, tp, ofs1);
					fMultiDim = 0;
				}
			}
			else					/* invalid indexing */
				index_error();
		}
		sub_eval(CSB);		/* GET the value inside the [] */

			/* Multiply [] by size of index */

		if(vsize != 1) {	/* optimize away size of 1 */
			push(NUMBER, vsize, DWORD, 0);
			do_lr2op(STAR);				/* multiply by size */
		}
		if(fMultiDim) do_lr2op(ADD);	/* add to last index if there */
		fMultiDim = TRUE;				/* if there is another... */
	}
	while(test_token(OSB));

	*tpRet = tp;
	return fMultiDim;
}

/******************************************************
 Gets the next token and perform any processing
 required to evaluate it.  This includes structure
 members.
*******************************************************/

static void get_value(void)
{
	S32 ndim, vcnt;
	U32 i, j, size, token, t, v, tp, tp1, ofs, ofs1;
	char fMultiDim, fvarargs;

	switch(token = get_token()) {
		case NUMBER:			/* a constant number */
			t = NUMBER;			/* constant */
			v = gvalue;			/* value of number */
			tp = DWORD;			/* all constants are DWORDS */
			ofs = 0;			/* no constant offset */
			break;
		case STRING:			/* a literal string */
			t = STRING;			/* will be found in lit pool */
			v = gvalue;			/* length of string  */
			tp = BYTE | 1;      /* 1 is level of indirection  */
			ofs = 0;			/* offset of zero */
			break;
		case SYMBOL:			/* symbol value */
			if(!(lookup_local() || lookup_global())) {		/* not defined */
				if(test_token(ORB)) {			/* function, but no proto! */
					symbol_error("Function not prototyped");
				}
				else							/* variable, report error */
					symbol_error("Undefined symbol");
			}
			t = SYMBOL;
			v = sptr;				/* symtab entry */
			tp = symtab[v].type;
			ofs = 0;			/* offset of zero */
			break;
		case STAR:				/* pointer dereference */
			get_value();
			pop(&t, &v, &tp, &ofs);
			StackESI();
			index_ptr(t, v, tp, ofs);
			t = PESI;
			if(tp & POINTER)
				--tp;
			else
				index_error();
			break;
		case AND:				/* address of */
			get_value();
			pop(&t, &v, &tp, &ofs);
			if(t == SYMBOL)
			{
				StackEAX();
				code_str((tp & GLOBAL) ? "\tMOV EAX,OFFSET " : "\tLEA EAX,");
				write_oper(t, v, tp, ofs);
				code_chr('\n');
				not_flag=0;
				tp = (tp + 1) & ~FUNCTION;  /* tp + 1 ups the ptr ref count */
				t = INEAX;
			}
			else if (t == PESI)
			{
				StackEAX();
				not_flag=0;
				out_inst("MOV EAX,ESI");
				tp = (tp + 1) & ~FUNCTION;  /* tp + 1 ups the ptr ref count */
				t = INEAX;
			}
			else if ((t == INEAX) && (tp & POINTER))
			{ /* do nothing... it's there. */ }
			else
				line_error("Invalid '&' operation");
			break;
		case ORB:				/* sub-expression */
			sub_eval(CRB);
			pop(&t, &v, &tp, &ofs);
			break;
		case SIZEOF:				/* sizeof */
			if(test_token(ORB)) {
				get_value();			/* look for a symbol */
				pop(&t, &v, &tp, &ofs);
				if (t == SYMBOL) {
					if (tp & POINTER) size = 4;
					else if (tp & BYTE) size = 1;
					else if (tp & WORD) size = 2;
					else if (tp & DWORD) size = 4;
					else if (tp & STRUCT)
						size = symtab[v].strucoff; /* strucoff = size */
					else size = 4;
					if ((tp & ARRAY) && (!(tp & POINTER))) { /* array ref */
						i = symtab[v].dindex;    /* i = index to dimpool */
						j = dim_pool[i++];			/* j = nDims */
						while(j--) size *= dim_pool[i++];
					}
 					t = NUMBER;
					tp = DWORD;			/* all constants are DWORDS */
					v = size;
					push(t, v, tp, 0);	/* PUSH THE "Value" ON THE EXP STACK */
					expect(CRB);
					return;
				}
				else
					line_error("Symbol expected");
			}
			else
				line_error("'(' expected");
		    break;
		default:				/* anything else (operators) */
			get_value();		/* look for a value */
			do_unary(token);
			return;
	}

/* Function calls  - Open Round Brackett (ORB) */

	if(test_token(ORB))
	{
		iarg = symtab[v].dindex;	/* index to prototyped args! */
		push(t, v, tp, ofs);
		StackEAX();
		StackESI();
		if (tp & VARARGS) fvarargs = -1;
		else fvarargs = 0;
		vcnt = ndim = 0;
		StackTop();
		if(!test_token(CRB))
		{		/* evaluate function operands */
			do {
				argtype = proto_list[iarg]; /* current arg type */
				if(!(argtype&VOID)) iarg++; /* if not end of proto args, get next*/
				token = do_oper(SEMI);		/* handle operation on arg */
				pop(&t, &v, &tp, &ofs);		/* get token arg from expstack */
				LoadEAX(t, v, tp, ofs);
				if ((t != PESI) && (ofs) && (tp&POINTER))
				{	/* offset must be added */
					code_str("\tADD EAX,");
					code_num(ofs);
					code_chr('\n');
				}
				out_inst("PUSH EAX");

				++ndim;
				if ((!fvarargs) || (fvarargs && (argtype==VOID))) {
					++vcnt;
				}
			}
			while(token == COMMA);
			if(token != CRB)
				syntax_error();
		}
		iarg = 0;
		pop(&t, &v, &tp, &ofs);	/* get function back off the expr stack */

		/*  if the function we are about to call had variable parameters
			then put the count of bytes that were pushed into EDI as the
			code in a vararg function uses it to access the fixed args.
			Also, we leave ndim (total count of bytes push) with it's
			value so that the "call" function knows he must remove the
			data from the stack and the "end_func" function knows not
			to use the RET XX instruction.
		*/

		if (fvarargs) {
			code_str("\tMOV EDI, ");
			code_num(vcnt*4);
			code_chr('\n');
		}
		else ndim = 0;

		call(t, v, tp, ofs, ndim);

		t = INEAX;			/* set up to leave the return value in ACC */
		tp &= ~FUNCTION;
	}



/* Indexing operations - Open Square Brackett (OSB) */

	fMultiDim = 0;
	if(test_token(OSB)) {
		fMultiDim = eval_index(t, v, tp, ofs, &tp);

		/* get index value token pushed by eval_index */

		pop(&token, &v, &t, &ofs);
		if ((token==NUMBER) & (!v))
		{  /* index is 0 */
			pop(&t, &v, &tp1, &ofs);		/* Get var base */
			if ((tp1 & (POINTER|ARRAY)) && (t!=PESI))
				load_index(t, v, tp1, ofs);	/* Load base into Index reg */
		}
		else
		{
			LoadEAX(token, v, t, ofs);	/* load it into ACC */
			pop(&t, &v, &tp1, &ofs);		/* Get var base */
			if (tp1 & (POINTER|ARRAY) && (t!=PESI))
				load_index(t, v, tp1, ofs);	/* Load base into Index reg */
			out_inst("ADD ESI,EAX");
		}
		t = PESI;			/* Let em know ESI pts to item */
	}

/* Convert any [UNINDEXED] array references to address values.
   This is done later to structures and struct members.
*/

	if((tp & ARRAY) && (!(tp & (STRUCT|STRUCMEM)))) {
		tp = (tp + 1) & ~ARRAY;			/* tp+1 ups the ptr ref count */
		if(!(tp & ARGUMENT))
		{
			if(!fMultiDim)
			{
				StackEAX();				/* save what's in EAX and ESI */
				StackESI();
				index_adr(t, v, tp, ofs);	/* load address of rg into ESI */
			}
			StackEAX();				/* save what's in EAX if needed */
			not_flag = 0;
			out_inst("MOV EAX,ESI");
			t = INEAX;
		}
	}

/* handle the dereference operators . and -> for
   structures and pointers to structures if present */


	if(test_token(DEREF)) {
		if ((tp & STRUCT) && ((tp & POINTER) || (t==PESI)) ) {
			if (t == SYMBOL) {				/* may already be PESI */
				StackEAX();					/* save what's in EAX */
				StackESI();				/* stack ESI */
			    index_ptr(t, v, tp, ofs);		/* pointer base into ESI */
   				t = PESI;				/* tell em it's indirect now */
			}
		    if (test_token(SYMBOL)) {
				if (lookup_member(v)) {
				    memoffset = symtab[sptr].strucoff;
				    tp = symtab[sptr].type;
			    	v = sptr;
					ofs = memoffset;	/* NEW */

					if (tp & ARRAY)
						tp = (tp + 1) & ~ARRAY;	 /* tp+1 ups the ptr ref count */
	   				t = PESI;		/* tell em it's indirect now */
				}
				else
					line_error("Not a structure member");
			}
			else
				line_error("Structure member expected");
		}
		else
			line_error("Pointer to Struct expected");
	}

	if (test_token(DOT)) {
		if ((tp & STRUCT) && (!(tp & POINTER))) {
			if (t == SYMBOL) {				/* may already be PESI */
				StackEAX();					/* save what's in EAX */
				StackESI();				/* stack ESI */
			    index_adr(t, v, tp, ofs);	/* ptr to base of struct into ESI */
			    t = PESI;
			}
	    	if (test_token(SYMBOL)) {
				if (lookup_member(v)) {
		  		  	memoffset = symtab[sptr].strucoff;
					tp = symtab[sptr].type;		/* member type */
					v = sptr;					/* symtab entry of member */
					ofs = memoffset;	/* NEW */

	   				t = PESI;		/* tell em it's indirect now */

				}
			  	else
					line_error("Structure member expected");
			}
		  	else
				line_error("Structure member expected");
		}
	  	else
			line_error("Invalid structure operation");
	}

/* Indexing operations for STRUCTMEMS- Open Square Brackett (OSB) */

	fMultiDim = 0;
	if(test_token(OSB)) {
		fMultiDim = eval_index(t, v, tp, ofs, &tp);
		pop(&token, &v, &t, &ofs);		/* get index value token just pushed */
		if ((token==NUMBER) & (!v))
		{	/* index is 0 */
			pop(&t, &v, &tp1, &ofs);	/* Get var base */
			if (tp1 & (POINTER|ARRAY) && (t!=PESI))
			{
				load_index(t, v, tp1, ofs);	/* Load base into Index reg */
				if ((t==ISTACK_TOP) &&
					(symtab[v].type & ARRAY))
					t=STACK_TOP;
			}
		}
		else {
			LoadEAX(token, v, t, ofs);	/* load it into ACC */
			pop(&t, &v, &tp1, &ofs);			/* Get var base */
			if (tp1 & (POINTER|ARRAY) && (t!=PESI)) {
				if ((t==ISTACK_TOP) &&
					(symtab[v].type & ARRAY))
					t=STACK_TOP;
				load_index(t, v, tp1, ofs);	/* Load base into Index reg */
			}
			out_inst("ADD ESI,EAX");
		}
		t = PESI;			/* Let em know ESI pts to item */
	}


/* Convert any [UNINDEXED] array references to address values
   for struct members that are arrays.
*/

	if((tp & ARRAY) && (tp & (STRUCMEM))) {
		tp = (tp + 1) & ~ARRAY;			/* tp+1 ups the ptr ref count */
		if(!(tp & ARGUMENT))
		{
			if(!fMultiDim)
			{
				StackEAX();					/* stack EAX */
				StackESI();				/* stack ESI */
				index_adr(t, v, tp, ofs);	/* load address of rg into ESI */
			}
			StackEAX();					/* stack EAX if needed */
			not_flag = 0;
			out_inst("MOV EAX,ESI");
			t = INEAX;
		}
	}

/* handle any post operators (++ and --) if present */

	if(test_token(INC)) {			/* post '++' */
		if (tp & POINTER)
		{
			load_index(t, v, tp, ofs);
		}
		else
			LoadEAX(t, v, tp, ofs);
		if (ispStruct(tp, v))
			GenCodeOper("ADD |,strucsize", t, v, tp, ofs);
		else if (isp32(tp))
			GenCodeOper("ADD |,4", t, v, tp, ofs);
		else if (isp16(tp))
			GenCodeOper("ADD |,2", t, v, tp, ofs);
		else
			GenCodeOper("INC |", t, v, tp, ofs);
		if (tp & POINTER)
		{
			t = PESI;
		}
		else
			t = INEAX;
	}

	else if(test_token(DEC))
	{									/* post '--' */
		if (tp & POINTER)
		{
			load_index(t, v, tp, ofs);
		}
		else
			LoadEAX(t, v, tp, ofs);
		if (ispStruct(tp, v))
			GenCodeOper("SUB |,strucsize", t, v, tp, ofs);
		else if (isp32(tp))
			GenCodeOper("SUB |,4", t, v, tp, ofs);
		else if (isp16(tp))
			GenCodeOper("SUB |,2", t, v, tp, ofs);
		else
			GenCodeOper("DEC |", t, v, tp, ofs);
		if (tp & POINTER)
		{
			t = PESI;
		}
		else
			t = INEAX;
	}
	push(t, v, tp, ofs);	/* FINALLY PUSH THE "Value" ON THE EXP STACK */
}

/****************************************************
 Combine two types (For binary operations)
 This raises the type to the lowest common type, or
 in the case of a strucr-member operation, the type
 is the type become the member's type.
*****************************************************/

static U32 combine(U32 type1, U32 type2)
{
	U32 new_type;

/* preserve width and indirection if pointer operation is involved */
/* if neither is pointer no harm is done with this code */

	if ((type1 & POINTER) >= (type2 & POINTER))
		new_type = type1 & POINTER;
	else
		new_type = type2 & POINTER;

/* raise to lowest common type */

	if ((type1 & DWORD) || (type2 & DWORD)) new_type |= DWORD;
	else if ((type1 & WORD) || (type2 & WORD)) new_type |= WORD;
	else new_type |= BYTE;

/* ANSI says: If combining unsigned and signed types and signed var
   is large enough to hold all values of unsigned then combined type
   remains SIGNED, else the type becomes UNSIGNED!
*/

	if (!((type1 & type2) & UNSIGNED))  /* both signed */
		return new_type;

	if ((type1 & UNSIGNED) && (type2 & UNSIGNED)) {  /* both unsigned */
		new_type |=UNSIGNED;
		return new_type;
	}

	/* ONLY one is signed... */

	if (((type1 & SIZEMASK) >= (type2 & SIZEMASK)) && (type2 & UNSIGNED))
		new_type |= UNSIGNED;

	else
	if (((type1 & SIZEMASK) <= (type2 & SIZEMASK)) && (type1 & UNSIGNED))
		new_type |= UNSIGNED;

	return new_type;

}

/**********************************************************
* Perform an operation & all higher priority operations.
***********************************************************/

static U32 do_oper(U32 token)
{
	U32 token1, t, v, tp, tp1, ofs, ofs1, exit_lab;

	/* Handle "special" binary operators which involve jumps */
	if((token == DAND) || (token == DOR) || (token == QUEST))
	{
		pop(&t, &v, &tp, &ofs);
		StackEAX();					/* stack EAX */
		StackESI();				/* stack ESI */
		if(t != INEAX)
			LoadEAX(t, v, tp, ofs);
		exit_lab = ++next_lab;
		if(token == QUEST)
		{		/* conditional expression */
			cond_jump(FALSE, token1 = ++next_lab, 0);
			sub_eval(COLON);
			pop(&t, &v, &tp, &ofs);
			LoadEAX(t, v, tp, ofs);
			jump(exit_lab, 0);
			gen_label(token1);
		}
		else
		{					/* && and || */
			test_not();
			if(token == DAND)
				cond_jump(FALSE, exit_lab, -1);
			else
				cond_jump(TRUE, exit_lab, -1);
		}
		token1 = do_oper(SEMI);
		pop(&t, &v, &tp1, &ofs1);
		LoadEAX(t, v, tp1, ofs1);
		gen_label(exit_lab);
		push(INEAX, v, combine(tp, tp1), ofs1);
		return token1;
	}

	get_value();			/* stack the value (number or whatever) */

/* Handle operator precedence and grouping. optype is 2 if operator
   separates two operands and grouping is L-R. It's 3 if R-L */

	token1 = get_token();	/* Look at next operator */
		while((optype[token1] > 1) &&
		 	  (priority[token1] >= priority[token]))
		{
			/* if they are the same priority AND grouping == L-R */

			if((priority[token1] == priority[token]) &&
			   (optype[token] == 2)) 
			{
				do_lr2op(token);
				return do_oper(token1);
			}

			token1 = do_oper(token1);
		}

/* Perform the operation */
	if(token!=SEMI)
		do_lr2op(token);

	return token1;
}


/*************************************************************************
 Evaluates two operands for operators with L to R grouping.
 if possible, evaluate constant expressions into another constant.
 Produce code to perform operation if necessary.
 The two operands are on the top of the expression stack and are
 loaded into token and token1. The item at the top of the stack is the
 second operand.  This means that if it were "A minus B", A would go into 
 token and B would go into token1.
**************************************************************************/

static void do_lr2op(U32 oper)
{
	U32 token, value, type, token1, value1, type1, offset, offset1;
	U32 atoken, avalue, atype, temp, ctype;
	U32 uflag, swap, order, wheretok;

	pop(&token1, &value1, &type1, &offset1);
	pop(&token, &value,  &type, &offset);

	uflag = swap = order = atoken = 0;

	/* Constant numbers assume the type of the other operand */
	if(token == NUMBER)
	{
		swap = 1;
		type = type1;
	}
	if(token1 == NUMBER)
	{
		swap = 0;
		type1 = type;
	}

	ctype = combine(type, type1);

	/* Do any operations that can be performed at compile time */

	if((token == NUMBER) && (token1 == NUMBER) &&
		(oper != DAND) && (oper != DOR)) {
		switch(oper) {
			case ADD :
				value += value1;
				break;
			case SUB :
				value -= value1;
				break;
			case STAR:
				value *= value1;
				break;
			case DIV:
				value /= value1;
				break;
			case MOD:
				value %= value1;
				break;
			case AND:
				value &= value1;
				break;
			case OR:
				value |= value1;
				break;
			case XOR:
				value ^= value1;
				break;
			case SHL:
				value <<= value1;
				break;
			case SHR:
				value >>= value1;
				break;
			case EQ:
				value = value == value1;
				break;
			case NE:
				value = value != value1;
				break;
			case LT:
				value = value < value1;
			case LE:
				value = value <= value1;
				break;
			case GT:
				value = value > value1;
				break;
			case GE:
				value = value >= value1;
				break;
			default:
				syntax_error();
		}
	}
	else
	{
		/* Generate code to perform the operation */
		avalue = value;
		atype = type;
		wheretok = INEAX;		/* default place for result */

		/* Use unsigned operations if needed */
		if((type | type1) & (POINTER | UNSIGNED))
			uflag = 1;

		/* Try and re-arrange for partial results already in ACC */
		if(token1 == INEAX)
			swap = 1;

		/* This first switch sets up self assignments and
		   changes the operand to the real operation to be
		   performed
		*/

		switch(oper)
		{
			case DAND:	/* logical AND and OR are done elsewhere */
			case DOR:
				push(token1, value1, type1, offset1);
				return;
			case ADDE:
				atoken = token;	/* nonzero atoken indicates self assignment*/
				oper = ADD;
				break;
			case SUBE:
				atoken = token;
				oper = SUB;
				break;
			case STARE:
				atoken = token;
				oper = STAR;
				break;
			case DIVE:
				oper = DIV;
				atoken = token;
				break;
			case MODE:
				oper = MOD;
				atoken = token;
				break;
			case SHLE:
				oper = SHL;
				atoken = token;
				break;
			case SHRE:
				oper = SHR;
				atoken = token;
				break;
			case ANDE:
				oper = AND;
				atoken = token;
				break;
			case ORE:
				oper = OR;
				atoken = token;
				break;
			case XORE:
				oper = XOR;
				atoken = token;
				break;
		}

		/* this next switch looks for operations that MUST
		   use special registers (such as EAX or EDX).
		   Or must be in a special order for a particular
		   operation.
		*/
		switch(oper)
		{
			case SUB:
			case DIV:
			case MOD:
			case SHL:
			case SHR:
				order = 1;
				break;
			case LT:
				if (swap) {
					if (uflag)
						oper = UGT;
					else
						oper = GT;
				}
				else if (uflag)
					oper = ULT;
				break;
			case LE:
				if (swap) {
					if (uflag)
						oper = UGE;
					else
						oper = GE;
				}
				else if (uflag)
					oper = ULE;
				break;
			case GT:
				if (swap) {
					if (uflag)
						oper = ULT;
					else
						oper = LT;
				}
				else if (uflag)
					oper = UGT;
				break;
			case GE:
				if (swap) {
					if (uflag)
						oper = ULE;
					else
						oper = LE;
				}
				else if (uflag)
					oper = UGE;
				break;
		}

		/* Now we can use the flags to set up a swap of operands if
		   indicated so long as ordering was not important (order flag).
		   Some operations require the use of EAX.
		*/

		if((token1 == INEAX) && order)
		{	/* out of order - use ECX */
			test_not();
			out_inst("MOV ECX,EAX");
			token1 = INECX;
		}

		if(swap && !order)
		{
			temp = token;  token = token1;   token1 = temp;
			temp = value;  value = value1;   value1 = temp;
			temp = type;   type  = type1;    type1  = temp;
			temp = offset; offset = offset1; offset1 = temp;
		}

		if(type1 & FUNCTION)
			type_error();

		if (oper != ASSIGN)  /* simple assignment is optimized case */
		{
			LoadEAX(token, value, type, offset);	/* insure its loaded */
		}

		/* If it's on the processor stack, get it into EDX */
		token1 = CheckStack(token1);

		/* Now we perform the operation */

		switch(oper)
		{
			case ASSIGN:
				/* optimize away an assigment */
				if ((token1 == token) && (value == value1))
				{
                    wheretok = token;
					break;
				}
				if (swap)	/* token to store is already in EAX */
				{
					store(token1, value1, type1, offset1);
				}
				else  /* token1 may not be loaded already */
				{
					LoadEAX(token1, value1, type1, offset1);
					store(token, value, type, offset);
				}
				break;
			case ADD:
				if (type1&(DWORD|POINTER))
					GenCodeOper("ADD EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("ADD EAX,ECX");
				}
				zero_flag = 0;
				break;
			case SUB:
				if (type1&(DWORD|POINTER))
					GenCodeOper("SUB EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("SUB EAX,ECX");
				}
				zero_flag = 0;
				break;
			case STAR:
				LoadECX(token1, value1, type1, offset1);
				if (ctype & UNSIGNED)
					out_inst("MUL ECX");
				else
					out_inst("IMUL ECX");
				zero_flag = 1;
				break;
			case DIV:
			case MOD:
				LoadECX(token1, value1, type1, offset1);
				if (ctype & UNSIGNED)
				{
					out_inst("XOR EDX,EDX");
					out_inst("DIV ECX");
				}
				else
				{
					out_inst("CDQ");
					out_inst("IDIV ECX");
				}
				zero_flag = -1;
				if (oper == MOD)
					out_inst("MOV EAX,EDX");
				break;
			case AND:
	 			if (type1 & (DWORD|POINTER))
					GenCodeOper("AND EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("AND EAX,ECX");
				}
				zero_flag = 0;
				break;
			case OR:
	 			if (type1 & (DWORD|POINTER))
					GenCodeOper("OR EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("OR EAX,ECX");
				}
				zero_flag = 0;
				break;
			case XOR:
	 			if (type1 & (DWORD|POINTER))
					GenCodeOper("XOR EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("XOR EAX,ECX");
				}
				zero_flag = 0;
				break;
			case SHL:
				LoadECX(token1, value1, type1, offset1);
				out_inst("SHL EAX,CL");
				if(ctype & WORD)
					out_inst("AND EAX,0FFFFh");
				else if (ctype & BYTE)
					out_inst("AND EAX,0FFh");
				zero_flag = 0;
				break;
			case SHR:
				LoadECX(token1, value1, type1, offset1);
				if (ctype & (DWORD | POINTER))
					out_inst("SHR EAX,CL");
				else if (ctype & WORD)
					out_inst("SHR AX,CL");
				else
					out_inst("SHR AL,CL");
				zero_flag = 0;
				break;
			case EQ:  		/* compare & test */
			case NE:
			case LT:
			case LE:
			case GT:
			case GE:
			case ULT:
			case ULE:
			case UGT:
			case UGE:
	 			if (type1&(DWORD|POINTER))
					GenCodeOper("CMP EAX,|", token1, value1, type1, offset1);
				else
				{
					LoadECX(token1, value1, type1, offset1);
					out_inst("CMP EAX,ECX");
				}
				switch(oper)
				{
					case EQ:  out_inst("SETE AL");  break;
					case NE:  out_inst("SETNE AL");  break;
					case LT:  out_inst("SETL AL");  break;
					case LE:  out_inst("SETLE AL");  break;
					case GT:  out_inst("SETG AL");  break;
					case GE:  out_inst("SETGE AL");  break;
					case ULT: out_inst("SETB AL");  break;
					case ULE: out_inst("SETBE AL");  break;
					case UGT: out_inst("SETA AL");  break;
					case UGE: out_inst("SETAE AL");  break;
				}
				out_inst("AND AL,AL");
				zero_flag = 0;
				break;
			default:
				syntax_error();
		}

		/* for self assigns, replace value */
		if(atoken)
			store(atoken, avalue, atype, offset);

		token = wheretok;
	}

	push(token, value, ctype, offset);
}



/***************************************************
* Test for a logically negated value, and update
* the accumulator if one is being used.
****************************************************/

static void test_not(void)
{
	if(not_flag)
	{
		out_inst("AND EAX,EAX");
		out_inst("SETZ AL");
		out_inst("AND AL,AL");
		not_flag = 0;
		zero_flag = 0;
	}
}


/********************************************
* Store accumulator value
*********************************************/

static void store(U32 token, U32 value, U32 type, U32 offset)
{
char *ptr;

	token = CheckStack(token);	/* get stack operand to EDX if needed */

	switch(token) {
		case SYMBOL:
		case PESI:
		case PECX:
		case PEDX:
		case ISTACK_TOP:
			token = CheckStack(token);	/* get stack operand to EDX if needed */
			if(type & (DWORD | POINTER))
				GenCodeOper("MOV |, EAX", token, value, type, offset);
			else if (type & WORD)
				GenCodeOper("MOV |, AX", token, value, type, offset);
			else
				GenCodeOper("MOV |, AL", token, value, type, offset);
			zero_flag = -1;
			break;
		default:
			line_error("Non-assignable");
	}
}


/*********************************************
 Determine if a type is a pointer to struct
**********************************************/

static U32 ispStruct(U32 type, U32 value)
{
	if((type & POINTER) > 1)	/* pointer to pointer */
		return 0;
	if ((type & STRUCT) && (type & POINTER)) {
		strucsize = symtab[value].strucoff;
		return 1;
	}
	return 0;					/* not our pointer */
}

/*********************************************
 Determine if a type is a pointer to 32 bits
**********************************************/

static U32 isp32(U32 type)
{
	if(type & (POINTER-1))		/* pointer to pointer */
		return 1;
	if(type & POINTER)			/* first level pointer */
		return (type & DWORD);
	return 0;					/* not a pointer */
}

/***********************************************
 Determine if a type is a pointer to 16 bits
************************************************/
static U32 isp16(U32 type)
{
	if(type & POINTER)			/* first level pointer */
		return (type & WORD);
	return 0;					/* not a pointer */
}

/*
 * Output a string to the assembler followed by newline.
 */
static void do_asm(char *ptr)
{
	code_str(ptr);
}

/***************************
 Initialize static storage
****************************/

static void init_static(U32 token, U32 value, char word)
{
	char *ptr, *ptr1, *nptr;
	U32  i;

	ptr = "";

	if(global_width)		/* continuing definition */
		ptr = ",";
	else {					/* new definition */
		if      (word==1) ptr = " DB ";
		else if (word==2) ptr = " DW ";
		else if (word==4) ptr = " DD ";
	}

	if(token == SYMBOL) {		/* Symbol - MUST BE GLOBAL - NOT EXTERNAL */
		ptr1 = "OFFSET ";
		global_width += 7; }
	else if(token == STRING) {		/* literal pool entry */
		ptr1 = "OFFSET L_lit+";
		*(ptr1+7) = prefix;
		global_width += 23; }
	else if(token == LABEL) {	/* instruction label */
		ptr1 = "L_";
		*ptr1 = prefix;
		global_width += 4; }
	else {						/* constant value */
		ptr1 = "";
		global_width += 6; }

	if (*ptr) data_str(ptr);
	if (*ptr1) data_str(ptr1);
	if (token!=SYMBOL)
		data_num(value);
	else {
		nptr=&GPool[symtab[value].oname];
		data_str(nptr);
		i=0; while(*nptr++) i+=1;
		global_width += i;
	 }

	if(global_width > 60) {
		global_width = 0;
		data_chr('\n');
		}
}

/********************************
* End static storage definition
*********************************/
static void end_static(void)
{
	if(global_width) {
		data_chr('\n');
		global_width = 0;
	}
}

/*******************************************************************
* Define a global non-static variable
********************************************************************/

static void gen_global(U32 symbol, U32 size)
{
	data_global(symbol);
	if(symtab[symbol].type & (POINTER | DWORD)) {
		if (size==4)
			data_str(" DD 0h\n");
		else {
			data_str(" DD ");
			data_num(size/4);
			data_str(" DUP(0)\n");
			}
		}
	else
	if(symtab[symbol].type & WORD) {
		if (size==2)
			data_str(" DW 0h\n");
		else {
			data_str(" DW ");
			data_num(size/2);
			data_str(" DUP(0)\n");
			}
		}
	else
	if(symtab[symbol].type & (BYTE|STRUCT)) {
		if (size==1)
			data_str(" DB 0h\n");
		else {
			data_str(" DB ");
			data_num(size);
			data_str(" DUP(0)\n");
			}
		}
}

/****************************************************
  Define an external label for DASM near or FAR call
  Print EXTRN _Symname: NEAR in CSeg if NEAR function.
  Print EXTRN _Symname  FWORD in DSeg if FAR function.
*****************************************************/

static void gen_extern_DASM (U32 symbol)
{
	U32 type;

	type = symtab[symbol].type;

	if (type & FAR) {
		data_str("EXTRN ");
		data_chr('_');
		data_str(&GPool[symtab[symbol].oname]);
		data_str(" FWORD");
		data_chr('\n');
	}
	else if (type & FUNCTION)
	{
		code_str("EXTRN ");
		code_chr('_');
		code_str(&GPool[symtab[symbol].oname]);
		code_chr(':');
		code_str(" NEAR");
		code_chr('\n');
	}

}

/****************************************************
  Define an external label for DASM data references.
  Print EXTRN _Symname DB (DW or DD) if NOT function.
*****************************************************/

static void gen_ext_data_DASM (U32 symbol)
{
	U32 type;

	type = symtab[symbol].type;

	if (!(type & FUNCTION))
	{
		data_str("EXTRN ");
		data_chr('_');
		data_str(&GPool[symtab[symbol].oname]);
		if (type & (POINTER|DWORD))
			data_str(" DD");
		else if (type & WORD)
			data_str(" DW");
		else
			data_str(" DB");
		data_chr('\n');
	}
}

/*******************************************************
* Enter function & allocate local variable stack space
********************************************************/

static void enter_func(U32 symbol, U32 size)
{
	code_global(symbol);
	code_str(":\n");
	if (symtab[fptr].type & ISR) 
	{
		out_inst("PUSHAD");
	}
	else {
		out_inst("PUSH EBP");
		out_inst("MOV EBP,ESP");
		if(size) {
			code_str("\tSUB ESP,");
			code_num(size);
			code_chr('\n'); }
	}
}

/************************************************
* Clean up the stack & end function definition
*************************************************/

static void end_func(U32 stackpop)
{
	if (symtab[fptr].type & ISR)
	{
			out_inst("POPAD");
			out_inst("IRETD");
	}
	else {
		if(local_stack)
			out_inst("MOV ESP,EBP");
		out_inst("POP EBP");
		if (stackpop) {
	 		if (symtab[fptr].type & FAR) code_str("\tRETF ");
	 		else code_str("\tRETN ");
	 		code_num(stackpop);
    		code_chr('\n');
		}
		else if (symtab[fptr].type & FAR) out_inst("\tRETF");
	 	else out_inst("RETN");
	}
}

/**********************************
 Define a compiler generated label
***********************************/

static void gen_label(U32 label)
{
	code_chr(prefix);
	code_chr('_');
	code_num(label);
	code_str(":\n");
}

/***********************
 Define literal pool
************************/
static void gen_literal(unsigned char *ptr, U32 size)
{
	U32 i;

	if(size) {
		i = 0;
		data_chr(prefix);
		data_str("_lit");
		while(i < size) {
			data_str((i % 16) ? "," : " DB ");
			data_num(*ptr++);
			if(!(++i % 16))
			data_chr('\n');	}
		if(i % 16)
			data_chr('\n'); }
}

/***********************************
* Call a function by name
************************************/
static void call(U32 token, U32 value, U32 type, U32 offset, U32 clean)
{

	token = CheckStack(token);
	if (type & FAR)
	{
			code_str("\tCALL FWORD PTR ");
	}
	else code_str("\tCALL ");
	if(token == NUMBER)
		code_num(value);
	else
		write_oper(token, value, type, offset);
	code_chr('\n');

	if(clean)
	{	/* clean up stack following function call */
		clean *=4;				/* each stack operand is 4 bytes */
		code_str("\tADD ESP,");
		code_num(clean);
		code_chr('\n');
	}

	zero_flag = -1;

}

/**********************************
* Unconditional jump to label
***********************************/

static void jump(U32 label, char ljmp)
{
	code_str(ljmp ? "\tJMP " : "\tJMP SHORT ");
	code_chr(prefix);
	code_chr('_');
	code_num(label);
	code_chr('\n');
}

/****************************************
* Conditional jump to label
*****************************************/
static void jump_if(char cond, U32 label, char ljmp)
            /* condition TRUE or FALSE, destination, long jump required */
{
	if (ljmp) code_str(cond ? "\tJNZ " : "\tJZ ");
	else code_str(cond ? "\tJNZ SHORT " : "\tJZ SHORT ");
	code_chr(prefix);
	code_chr('_');
	code_num(label);
	code_chr('\n');
}

/***********************************************
 JUMP to the begining of the switch jump table
************************************************/
static void do_switch(U32 label)
{
	code_str("\tJMP ");
	code_chr(prefix);
	code_chr('_');
	code_num(label);
	code_chr('\n');
}

/*************************************************
* Build switch compare/jump table.
* d is the count of switch jump table entries.
**************************************************/

static void build_switch(U32 d)
{
	while(switch_ptr > d) {
		code_str("\tCMP EAX,");
		code_num(switch_stack[--switch_ptr]);
		code_chr('\n');
		code_str("\tJE ");
		code_chr(prefix);
		code_chr('_');
		code_num(switch_stack[--switch_ptr]);
		code_chr('\n');
	}
}


/********************************************
 Load index register with a pointer value
*********************************************/

static void index_ptr(U32 token, U32 value, U32 type, U32 offset)
{
	if(token == INEAX)
	{
		out_inst("MOV ESI,EAX");
	}
	else if (token==PESI)
		return;
	else
	{
		token = CheckStack(token);
		code_str("\tMOV ESI,");
		write_oper(token, value, type, offset);
		code_str("\n");
	}
}

/************************************************
 Load index register with the address of a symbol
*************************************************/

static void index_adr(U32 token, U32 value, U32 type, U32 offset)
{
	if (token != PESI)
	{
		code_str((type & GLOBAL) ? "\tMOV ESI,OFFSET " : "\tLEA ESI,");
		write_oper(token, value, type, offset);
		code_str("\n");
	}
}

/**********************************
* Output a global code symbol name
***********************************/

static void code_global(U32 symbol)
{
char *ptr;
	ptr = &GPool[symtab[symbol].oname];
	if(!(symtab[symbol].type & STATIC)) 
	{
		code_str("PUBLIC ");
	}
	code_chr('_');
	code_str(ptr);
}

/**********************************
* Output a global data symbol name
***********************************/

static void data_global(U32 symbol)
{
char *ptr;
	ptr = &GPool[symtab[symbol].oname];
	if(!(symtab[symbol].type & STATIC)) 
	{
		data_str("PUBLIC ");
	}
	data_chr('_');
	data_str(ptr);
}


/**********************************************
 CODE FILE OUTPUT ROUTINES
***********************************************/

/* Write a string to the code tmp file */

static void code_str(char *ptr)
{
	while(*ptr)
		fputc(*ptr++, temp_fh);
}

/***************************************
 Write a char to the code tmp file
***************************************/

static void code_chr(char chr)
{
	fputc(chr, temp_fh);
}

/***************************************
 Write a number to the code tmp file
***************************************/

static void code_num(U32 value)
{
	put_num(value, temp_fh);
}

/***************************************
 Write a HEX number to the code tmp file
***************************************/

static void code_hex(U32 value)
{
	put_num(value, temp_fh);
}

/**********************************************
 ASM FILE OUTPUT ROUTINES (INITIAL DATA SEG)
***********************************************/

/* Write a string to the asm file (all data) */

static void data_str(char *ptr)
{
	while(*ptr)
		fputc(*ptr++, asm_fh);
}
/* Write a char to the asm file (all data) */

static void data_chr(char chr)
{
	fputc(chr, asm_fh);
}

/* Write a number to the asm file (data seg) */

static void data_num(U32 value)
{
		put_num(value, asm_fh);
}

/* Write a HEX BYTE to the asm file (data seg) */

static void data_byte(U8 value)
{
		put_hex(value, asm_fh);
}


/********************************
* Write a number to file
*********************************/

static void put_num(U32 value, FILE *outfile)
{
	char stack[10];
	register U16 i;

	if(value & 0x80000000) {
		fputc('-', outfile);
		value = -value; }

	i = 0;
	do
		stack[i++] = (value % 10) + '0';
	while(value /= 10);

	while(i)
		fputc(stack[--i], outfile);
}

/********************************
* Write a hex number to file.
*********************************/

static void put_hex(U8 num, FILE *outfile)
{
	U8 c;

	c= ((num & 0xf0) >> 4) + '0';
	if (c > 0x39) {
		c += 7;
		fputc('0', outfile);		/* leading zero needed */
		}
	fputc(c, outfile);
	c= ((num & 0xf0) >> 4) + '0';
	if (c > 0x39) c += 7;
	fputc(c, outfile);
	fputc('h', outfile);
}


/***************************************
* Process a language statement
****************************************/

static void statement(U32 token)
{
	U32 a, b, c, d;

	test_exit();		/* generate any preceeding exit */

	switch(token) 
	{		/* act upon the token */
		case SEMI:		/* ';' - null statement */
			check_func();
			return;
		case OCB:		/* '{' - begin a block */
			while((token = get_token()) != CCB)
				statement(token);
			break;
		case INT:		/* 16/32 integer variable declaration */
		case CHAR:		/* 8 bit variable declaration */
		case SHORT:		/* 16 bit variable declaration */
		case LONG:		/* 32 bit variable declaration */
		case SIGNED:	/* signed variable declaration (the default anyway)*/
		case UNSIGN:	/* unsigned variable declaration */
		case STAT:		/* static modifier */
		case STRUC:		/* structure declaration */
		case EXTERN:	/* external modifier */
		case REGIS:		/* register modifier */
		case INTR:		/* interrupt modifier for functions */
		case VOIDD:		/* void for function, and empty args */
			declare(token, 0);
			break;
		case IF:
			check_func();
			expect(ORB);
			eval(CRB, 0);
			cond_jump(FALSE, a = ++next_lab, -1);
			statement(get_token());
			if(test_token(ELSE)) {
				test_jump(b = ++next_lab);
				gen_label(a);
				a = b;
				statement(get_token()); }
			test_exit();
			gen_label(a);
			break;
		case WHILE:
			check_func();
			gen_label(continue_stack[loop_ptr] = a = ++next_lab);
			break_stack[loop_ptr++] = b = ++next_lab;
			expect(ORB);
			eval(CRB, 0);
			cond_jump(FALSE, b, -1);
			statement(get_token());
			test_jump(a);
			gen_label(b);
			--loop_ptr;
			break;
		case DO:
			check_func();
			gen_label(a = ++next_lab);
			continue_stack[loop_ptr] = b = ++next_lab;
			break_stack[loop_ptr++] = c = ++next_lab;
			statement(get_token());
			gen_label(b);
			expect(WHILE);
			eval(SEMI, 0);
			cond_jump(TRUE, a, -1);
			gen_label(c);
			--loop_ptr;
			break;
		case FOR:
			check_func();
			expect(ORB);
			if(!test_token(SEMI))			/* initial */
				eval(SEMI, -1);
			gen_label(d = a = ++next_lab);
			break_stack[loop_ptr] = b = ++next_lab;
			if(!test_token(SEMI)) {			/* test */
				eval(SEMI, 0);
				cond_jump(FALSE, b, -1); }
			if(!test_token(CRB)) 
			{			/* end */
				jump(c = ++next_lab, 0);
				gen_label(d = ++next_lab);
				eval(CRB, -1);
				jump(a, 0);
				gen_label(c); 
			}
			continue_stack[loop_ptr++] = d;
			statement(get_token());
			test_jump(d);
			gen_label(b);					/* exit */
			--loop_ptr;
			break;
		case SWITCH:
			check_func();
			a = sdefault;
			break_stack[loop_ptr++] = sdefault = b = ++next_lab;
			expect(ORB);
			eval(CRB, -1);
			do_switch(c = ++next_lab);
			d = switch_ptr;
			statement(get_token());
			test_jump(b);
			gen_label(c);
			build_switch(d);
			if (sdefault!=a) jump(sdefault, -1);
			gen_label(b);
			--loop_ptr;
			sdefault = a;
			break;
		case CASE:
			a = check_switch();
			get_constant(&b, &c);
			switch_stack[switch_ptr++] = a;
			switch_stack[switch_ptr++] = c;
			if(switch_ptr >= (MAX_SWITCH*2))
				fatal_error("Too many active cases");
			expect(COLON);
			break;
		case DEFAULT:
			sdefault = check_switch();
			expect(COLON);
			break;
		case RETURN:
			check_func();
			if(!test_token(SEMI))
			{
				eval(SEMI, -1);
			}
			exit_flag = exit_label ? exit_label : (exit_label = ++next_lab);
			break;
		case BREAK:
			check_loop(break_stack);
			break;
		case CONTIN:
			check_loop(continue_stack);
			break;
		case GOTO:
			check_func();
			if(get_token() != SYMBOL) {
				syntax_error();
				break; }
			if(a = lookup_local()) {
				if(!(a & GLABEL))
					type_error(); }
			else
				define_symbol(REFERENCE|GLABEL, ++next_lab);
			jump(symtab[sptr].dindex, -1);
			break;
		case SYMBOL:	/* a symbol table entry */
			if(!in_function) {		/* global definition */
				declare(token, 0);
				break; }
			if(*input_ptr == ':') {		/* label definition */
				check_func();
				++input_ptr;
				if(lookup_local())
					symtab[sptr].type &= ~EXTERNAL;
				else
					define_symbol(GLABEL, ++next_lab); /* sets sptr */
				gen_label(symtab[sptr].dindex);
				break;
			 }
		default:		/* expression evaluation */
			check_func();
			unget_token(token);
			eval(SEMI, -1);
	}
}

/*********************************************************
* Main compile loop, process statements until end of file
**********************************************************/

static void compile(void)
{
	define_ptr = define_pool;
	*(input_ptr = line_in) = 0;
	if (!fGen) 
	{
	    code_str("\n\n.CODE\n");
		data_str("\n.DATA\n");
	}
	for(;;)
		statement(get_token());
}

/****************************************************
* Initialize I/O & execute compiler  MAIN MAIN MAIN
*****************************************************/

void main(S16 argc, char *argv[])
{
	S16 i;
	char *ptr, *pname;

/* first process any filenames and command line options */

	list_fh = stdout;	/* default the list file */

	for(i=1; i < argc; ++i) {	/* start at arg 1 */
		ptr = argv[i];
		if (*ptr == '/') {
		  ptr++;
		  switch(*ptr) {
			case 'S' :			/* quiet mode */
			case 's' :
				fQuiet = 1;
				break;
			case 'L' :			/* List file ON */
			case 'l' :
				fList = 1;
				break;
			case 'E' :			/* Embedded source mode */
			case 'e' :
				fSource = 1;
				break;
			case 'G' :			/* Generate separate modules */
			case 'g' :
				fGen = 1;
				break;
			case 'N' :			/* NO optimize */
			case 'n' :
				fNoOpt = 1;
				break;
			case 'O' :			/* Optimize for speed */
			case 'o' :
				fOptS = 1;
				break;
			case 'W' :			/* Warnings ON */
			case 'w' :
				fWarnings = 1;
				break;
			case 'P' :			/* Prefix label character */
			case 'p' :
				ptr++;
				if (!is_alpha(*ptr))
					fatal_error("Invalid label prefix character");
				prefix = *ptr;
				break;
			default:
				fatal_error("Invalid switch");
				break;
		  }
		}
		else {
			if(!source_fh) {
				copystring(srcname, argv[i]);
				source_fh = fopen(argv[i], "r");
				}
			else if(!asm_fh) {
				copystring(asmname, argv[i]);
				if(!(asm_fh = fopen(argv[i], "w"))) {
				  put_str("Error: Can't open ASM file\n", stdout);
				  exit(-1);
				  }
				}
			else
				fatal_error("Too many parameters");
	   }
	}

/* Input file not explicitly named errors out */
	if(!source_fh) {				/* default to standard input */
		put_str("C Minus 32 Compiler, Version 2.2\r\n", stdout);
		put_str("Usage: SourceFile [AsmFile] /S /E /G /L /W /Px\r\n", stdout);
		put_str("/S  Suppress screen output (e.g., herald)\r\n", stdout);
		put_str("/E  Embed source in ASM output\r\n", stdout);
		put_str("/G  Generate separate Code & Data files\r\n", stdout);
		put_str("/L  List file generated for errors\r\n", stdout);
		put_str("/N  No optimization\r\n", stdout);
		put_str("/O  Optimize for speed.\r\n", stdout);
		put_str("/W  Warnings ON\r\n", stdout);
		put_str("/Px Label prefix character (x=Label char)\r\n\n", stdout);
		put_str("Error: Source filename required\r\n", stdout);
		exit(-1);
		}

/* Output file not explicitly named defaults to Source.asm */

	if(!asm_fh)	{			/* default asm name to SourceName.asm */
		copystring(asmname, srcname);
		pname=asmname;
		while ((*pname != '.') && (*pname!= '\0')) pname++;

		if (fGen) {
			*pname++ = '.';
			*pname++ = 'D';
			*pname++ = 'A';
			*pname++ = 'S';
			}
		else {
			*pname++ = '.';
			*pname++ = 'A';
			*pname++ = 'S';
			*pname++ = 'M';
			}
		*pname   = '\0';
		if(!(asm_fh = fopen(asmname, "w"))) {
			put_str("Error: Can't open ASM file\r\n", stdout);
			exit(-1);
			}
		}
	fASMOpen = TRUE;

/* If -L then List file named Source.LST is generated. */

	if (fList) {
		copystring(lstname, srcname);
		pname=lstname;
		while ((*pname != '.') && (*pname!= '\0')) pname++;
		*pname++ = '.';
		*pname++ = 'L';
		*pname++ = 'S';
		*pname++ = 'T';
		*pname   = '\0';
		if(!(list_fh = fopen(lstname, "w")))
					fatal_error("Cannot open LIST file");
		else fLISTOpen = TRUE;
		}

	/* open code segment tmp file  or Gen file */

	if (fGen) {
		copystring(codename, srcname);
		pname=codename;
		while ((*pname != '.') && (*pname!= '\0')) pname++;
		*pname++ = '.';
		*pname++ = 'C';
		*pname++ = 'A';
		*pname++ = 'S';
		*pname   = '\0';
		if(!(code_fh = fopen(codename, "w")))
					fatal_error("Cannot open Code file");
		else fCODEOpen = TRUE;
		}

	if(!(temp_fh = fopen(tmpname, "w")))
			fatal_error("Cannot open temporary file\r\n");

	fTEMPOpen = TRUE;

	if(!fQuiet)
		put_str("C Minus 32 Compiler, Version 2.2\r\n", stdout);

	compile();
}
