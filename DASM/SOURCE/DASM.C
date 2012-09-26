/*
 * D-Group Assembler for MMURTL (DOS Version).
 *
 * Copyright 1991,1992,1993,1994 R.A. Burgess

  Version 1.2 11/20/93 - major rewrite on instruction table
  Version 1.3 8/28/94 - Clear macro buf on level 1 to 0 transition
  Version 1.4 9/29/94 - Smashed bug in 66| prefix for string commands
  Version 1.5 10/5/94 - Optimized a couple of commands and fixed
				bug in ENTER X,X instruction.  This version seems
				really stable ("yeaaaaa Right," he says...)
				Actually it is....
               10/29/94 Fix problem with sign extending bytes on ADD
  Version 1.6  12/31/94 - Removed temp files on succesful assemble
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
#include "dasm.h"
#include "runfile.h"

/* variables */

#define LEVELS 5
U8 level = 0;				/* current include level */
U32 lineno[LEVELS];			/* line number being parsed */
U8 fContinue = 0;			/* True if just returned from include */

char srcname[LEVELS][40];	/* also active include filenames */
char runname[40];
char lstname[40];

/* File handles for all files */

FILE *src_fh[5] = {0,0,0,0,0};	/* Current source file */
FILE *run_fh   = 0;		/* Current .RUN, .DLL, or .DDR (output) */
FILE *ds_fh    = 0;		/* temp file for ALL DS strorage */
FILE *cs_fh    = 0;		/* temp file for ALL CS code */
FILE *lst_fh   = 0;		/* List file */
FILE *sym_fh   = 0;		/* Symbol file for debugger */

char *csname = "CS.TMP";
char *dsname = "DS.TMP";

U8 filetype = 1;		/* 1 = RUN, 2 = DLL, 3 = DDR */

U8 fListA = 0;			/* True if detailed list file */
U8 fListE = 0;			/* True if Error only list file */
U8 fSymDump = 0;		/* True if we add symbols to the list file */
U8 Column = 0;			/* where are we on the LIST line */
U16 error_count = 0;
U16 warn_count = 0;


 /* Max input line is 132, but 200 allows plenty for macro subs */
S8 line_buf0[200];	/* Two buffers are swapped during macro substitution */
S8 line_buf1[200];
S8 *line_ptr;		/* pointer to next char on line */

S8 list_buf[200];	/* Used to hold line string for list file */
S8 fLineIn =0;		/* TRUE is list_buf has something to list */

S8 TString[133];	/* all parsed tokens are placed here in text form */
S16 CBString;		/* size of parsed token */
S16  TSymnum;		/* Symbol table entry number, else 0 */
U32  TNumber;		/* value of numeric token */
S16  TInst;			/* Instruction number, else 0 */
S16  TReg;			/* Register number, else 0 */
S16  Token;			/* Token type (also returned from parse) */
S8   fPutBack = 0;  /* TRUE (non=zero) if last token was not used */

S8   LTString[133];	/* Duplicates of Token storage for ReturnToken(); */
S16  LCBString;
S16  LTSymnum;
U32  LTNumber;
S16  LTInst;
S16  LTReg;
S16  LToken;

S8 UString[31];	/* Place to save unknown labels for Forward Reference */
S16 UCBString;		/* Size of Unknown label */

/* The two Symbol tables have 5 entries;  Type, Size, Pointer to Name,
   Segment Offset, and Line.
   As each symbol is found, it's identifed by type, and the name
   is moved to the packed symbol buffer. A pointer is added to the
   table to point to the name in the buffer, while it's size, type,
   segment offset, and line number are added to the table.
   Macro names are also stored in the symbol table, but offset entry
   for a macro indicates it's offset in the Macro buffer.
*/

/* Variables for symbol tables */

/* Global Symbol Table */

#define SYMSMAX 500
#define SYMBUFMAX 10140 	/* Avg of 10 byte per name, 700 Max */

struct symtab {
	U16  Type;		/* Token (e.g, CLabel) */
	U16  Line;		/* Line symbol was declared on */
	U8   Size;		/* Size of symbol name */
	S8   *Ptr;		/* Pointer to name in packed buffer */
	U32  Offs;		/* Offset in segment */
	};

struct symtab gst[SYMSMAX];	/* storage for the GST */

/* S8  SymBuf[SYMBUFMAX];	*/	/* Where names are stored. Will be Alloced */
S8 *pSymBuf;				/* ptr to allocated buffer */

S8   *pSymNext;		/* ptr to next new entry in symbol buffer */
S16 iSymNext = 1;	/* index to next new symbol table entry.  */
					/* Starts at 1 - zero it reserved because */
					/* Parse returns 0 for EOL */

S8 fPublic = 0;
S8 fExtern = 0;
S8 fFarLabel = 0;

/*********** Local Symbol Table *************/

/* This is cleared after each include file from level 1 is closed */
/* Clearing means we reset *pLSymNext to begining of buffer */
/* and reset iLSymNext to 1. This "hides" local symbols. */

#define LSYMSMAX 1800
#define LSYMBUFMAX 16384	  		/* Avg of 7 bytes per name */

struct symtab lst[LSYMSMAX];	/* storage for the LST */

/* S8  LSymBuf[LSYMBUFMAX]; */		/* Where names are stored.*/
S8  *pLSymBuf;					/* for allocated buffer */

S8   *pLSymNext;	/* ptr to next new entry in symbol buffer */
S16 iLSymNext = 1;	/* index to next new symbol table entry.  */
					/* Starts at 1 - zero it reserved because */
					/* Parse returns 0 for EOL */

/************* Forward Ref Table *********************************/
/*
   Labels that are encountered as forward references are
   stored in this table.  When we run across a forward, we do
   not know what segment they are refering to unless it is from
   a jump, call, or loop instruction.  We will NOT know for many
   items until they are actually declared.  After all the code
   and data has been processed, we go through this table
   and fix all the unknown references using the relative
	The type of forward references are:

		1) DSEG DWORD item refers to UNK item (?Seg MemRef)
		2) CSEG DWORD item refers to UNK item (?Seg MemRef)
		3) CSEG to CSEG relative 8  (Jump, Jc, or Loop)
		4) CSEG to CSEG relative 32 (Jump or Call)

	MOTE: If a calculation is made with the unknown reference, 0
	is used in the calculation. We ALWAYS ADD the value we stored
	with what is found when the reference is resolved.

*/

#define FREFSMAX   32768/12
#define FREFTABMAX 32768
#define FREFBUFMAX 16384

/* Types of forward references. */

#define DSEGREF   1		/* 32 bit Abs Ref in DSeg, to what is unknown! */
#define CSEGREF   2		/* 32 bit Abs Ref in CSeg, to what is unknown! */
#define CCR32REF  3		/* 32 bit Relative in CSeg */
#define CCR8REF   4		/* 8 bit Relative in CSeg */

struct forreftab {
	U8   Type;  	/* 1, 2, 3, or 4 */
	U8   NameSz;	/* Size of ref name */
	U16  Line;		/* Line reference was made on  (for error if not found) */
	S8   *Ptr;		/* Pointer to name in packed buffer */
	S32  Offs;		/* Offset in segment were it should go */
	};

struct forreftab *pfrt;	/* pointer to allocated table */

S8   *pRefBuf;
S8   *pRefNext;		/* ptr to next new entry in symbol buffer */
S16  iRefNext = 0;	/* index to next new forward ref table entry.  */

/************ External Reference table ************/

/* External definitions that are encountered in a module are
   first entered in the Global Symbol Table (GST) as externals
   if they haven't already been defined (public) by the module
   that owns them.
   When a reference is made to an external, we see if the public has
   already been defined. If so, we resolve it immediately. If not,
   we make an entry in the External Reference Table (ERT).  The
   entry has the following 4 pieces of information:
     - Line where reference was made.
     - Index to place holder entry in GST.
	 - Segment where reference was made (boolean - TRUE for code seg).
	 - Offset in that segment where the resolution will be made.
   With this information we can reslove all the external references
   before we write the code segment to the run file.
*/

#define EREFSMAX 400

struct extreftab {
	U8   Type;   /* Type of reference so we know how to apply it */
	U16  iSym;	 /* Index of gst entry. Hi bit seg if CSeg ref */
	U32  Offs;	 /* Offset in segment were ref needs 32 bit fixing */
	};

struct extreftab ert[FREFSMAX];

S16  iERefNext = 0; 	/* index to next external ref table entry.  */

U16 nExtRef;

/*********** Fix Up Table *********************/

/* This table holds 6 types of fixups along with the offset
   in the segment to be fixed up, and if applicable, an index
   to the DLL public name in the GST (which should be defined
   as EXTRNDLL):
     - Fixup type
         - Ref in CSEG to address in DSEG
         - Ref in CSEG to address in CSEG
         - Ref in DSEG to address in DSEG
         - Ref in DSEG to address in CSEG
         - CSEG DLL near 32 bit call used
         - CSEG DLL near 32 bit call defined
     - Offset of 32 bit reference to be resolved
     - Index to DLL public name in GST (if applicable)

   With this information we can supply the loader with the TAG entries
   in the RUN file needed to resolve them.  Most of these entries will
   be made as we parse the code, but many will also be added after
   we fix the forward references because we won't know what segment
   the reference is made to until then.
*/

#define FIXUPSMAX    32768/7
#define FIXUPBUFMAX  32768

struct futab {
	U8   type;   /* Type of fixup C0, C1, C2, C3, C5, or C8 */
	U32  Offs;	 /* Offset in segment for 32 bit fixup */
	U16  iSym;	 /* index of DLL entry in gst, else 0 */
	};

struct futab *pfut;	/* pointer to allocated Fix Up Table */

S16  iFUNext = 0; 	/* index to next fix up table entry.  */
					/* Starts at 0 */

U16	nCDFix = 0;
U16 nDDFix = 0;
U16 nDCFix = 0;
U16 nCCFix = 0;

/********* Macro variables *******************/

#define MACSMAX 200
#define MACBUFMAX  3000

S8   *rgMacPtr[MACSMAX];	/* pointer to simple macros */
S8   *pMacBuf;
S8   *pMacNext;				/* ptr to next new entry in macro buffer */
S16  iMacNext = 0;			/* index to next new symbol entry */

/* Variables for current segment address offset tracking */

S8  fStart = 0;			/* True if they gave a start address */
S32 StartAddr = 0;		/* Filled in when .START is encountered */
S32 oNextData = 0;		/* Tracks current DSEG offset */
S32 oNextCode = 0;      /* Tracks current CSEG offset */
S32 CodeOffset = 0;		/* From Virtual Command */
S32 DataOffset = 0;		/* From Virtual Command */
S32 *pNextAddr = 0;		/* Points to current segment offset counter */
S8  fDataSeg = 0;		/* True if parsing DSEG, else parsing CSEG */
S32 StackTotal = 0;		/* Total Stack Specified in Code segs */

/******************************************************************
   Variables for RAW operands in an instruction.
   These are used when we read in and separate each part of the
   operands so it can be evaluated.
*/

S16  rgToken[3][20]; /* Raw tokens in the operand (as many as 20) */
S32  rgVal[3][20];   /* value if token is number/displacement */
S16  rgTID[3][20];	 /* Register ID if token is a register */
S16  rgTCnt[3];		 /* total count of tokens in raw operand */

/******************************************************************
   These variables are filled in while parsing and evaluating
   instructions that have been read in.
   They have all the info needed to
   encode the instruction into the object module and
   produce FixUp Records if needed.
*/

/* These are used in the description of all OPERANDS for the current
   instruction we are working on.
*/
S16  rgOpType[3];	/* Operand type for compare to instruction */
S16  rgOpReg[3];    /* If reg only, this which one */
S8   OpSize[3];     /* Operand size for first two (fByte, fWord, fFar etc.) */
S8   OpSizeA;       /* Overall operand size for instruction */
S8   OpPrefix;      /* Bit set for a segment prefix (fESp etc.) */
S16  iInstEntry;	/* Which entry is it in rgINS */
S16  CrntInst;		/* instruction we are working on 	*/
S16  InstPfx;		/* Special Insturction prefix for crnt inst */
S16  nOperands;		/* number of operands we found */
S8   fForRef;       /* Crnt inst makes a Forward Reference */

/* The following vars are used if we have a memory reference that is
   encoded as part of the instruction.
*/
S8   OpMType;		/* Mem type (compared to rgM32). 0 if not mem type */
S16  OpBase;        /* Base register if fBase is true */
S16  OpIndx;        /* Index register if fIndx is true */
S32  OpDisp;        /* Displacement if fDisp8 or fDisp32 is TRUE */
S16  iMemEntry;		/* If mem operand, which entry in rgM32 */

/* The following are used if we have immediate values to be encoded as
part of the instruction.  Immediates can also be addresses such as
those in Direct and Relative Jumps and Calls!
*/
S32  OpImm;         /* Immediate value if fOpImm is true */
S8   fOpImm;		/* If OpImm has a value */
S32  OpImm2;        /* Second Imm value for iSAD and other multi-Imm types */
S8   fOpImm2;		/* If OpImm2 has a value */

/* This is set for each type of fixup required after an instruction
line.  Data fixups are done directly in the code that generates
storage in DSeg. */

U8   nFixUp;		/* Fixup number (Cx) */

/*************************************************************************/

/* In the final stages of building an instruction, these contain the
   fragment bytes of encoded instructions (along with flags for
   optional parts used).
*/

U8  bOpc1;		/* Zero if not used */
U8  bOpc2;		/* Always used */
U8  bModRM;		/* ModRM byte value if used */
U8  bSIB;		/* SIB value if used */
S8  fModRM; 	/* True if bModRM is used */
S8  fSIB;   	/* True if bSIB is used */

/*** These are used in the expression parser ******/

U8  ExpType;		/* 0 = Forward, 1 = Local, 2 = Global, 3 = Current Addr */
U8  ExpType0;
S16 nExpSyms;		/* Used by the numeric expression evaluator */
S16 iExpSym, iExpSym0;	/* if a symbol value translates to a SYMOFF,
					   this holds the symbol table index. */
S8  fOffset;		/* True if derived from an Offset */

/* Variables for Storage allocation */


S8  fMoreStorage = 0;   /* True when storage continues to next line */
S16 StoreSize = 0;

/****  Variables for building the RUN File (DLL or Device Driver) ********/

/* Once we have the CS and DS tmp files run through to correct any external
   variables or forward labels, we build the run file.  This is done
   by building the tag records, sending them followed by the proper data
   for each one.
*/

struct tagtype tag;


/******************* END VARIABLES - BEGIN CODE ********************/

/* ANSI prototypes for all functions are in DProtos.h */

#include "DProtos.h"


/***************************************************************/
/* Write a DWord to the current segment and update the counter */
/***************************************************************/

void OutDWord(unsigned long Data)
{
unsigned char *pOut, b;
int i;
	pOut = &Data;
	if (fDataSeg) {
		for (i=0; i<4; i++) {
			b = *pOut++;
			fputc(b , ds_fh);
		}
	    oNextData+=4;
	}
	else {
		for (i=0; i<4; i++) {
			b = *pOut++;
			fputc(b , cs_fh);
		}
	    oNextCode+=4;
    }
}

/******************************************************************/
/* Write a DWord to the current segment and DO NOT update counter */
/******************************************************************/

void OutDWordCS(unsigned long Data)
{
unsigned char *pOut, b;
int i;
	pOut = &Data;
	for (i=0; i<4; i++) {
		b = *pOut++;
		fputc(b , cs_fh);
	}
}

/******************************************************************/
/* Write a DWord to the current segment and DO NOT update counter */
/******************************************************************/

void OutDWordDS(unsigned long Data)
{
unsigned char *pOut, b;
int i;
	pOut = &Data;
	for (i=0; i<4; i++) {
		b = *pOut++;
		fputc(b , ds_fh);
	}
}

/**************************************************************/
/* Write a Word to the current segment and update the counter */
/**************************************************************/

void OutWord(unsigned int Data)
{
unsigned char *pOut, b;
int i;
	pOut = &Data;
	b = *pOut++;
	if (fDataSeg) {
		fputc(b , ds_fh);
		b = *pOut;
		fputc(b , ds_fh);
	    oNextData+=2;
	}
	else {
		fputc(b , cs_fh);
		b = *pOut;
		fputc(b , cs_fh);
	    oNextCode+=2;
	}
}

/**************************************************************/
/* Write a BYTE to the current segment and update the counter */
/**************************************************************/

void OutByte(unsigned char Data)
{
	if (fDataSeg) {
		fputc(Data , ds_fh);
	    oNextData++;
	}
	else {
		fputc(Data , cs_fh);
	    oNextCode++;
	}
}

/*****************************************************************/
/* Write a BYTE to the current segment and DO NOT update counter */
/*****************************************************************/

void OutByteCS(unsigned long Data)
{
char b;
	b = Data;
	fputc(b , cs_fh);
}

/* All the "grunt work" functions are in dasmq.c */

#include "DASMq.c"

/*********************************************
This searches the Reference table for the name
described by pb, cb. It only compares items that
are the same length (cb == TRefSize[x]).
It returns the number or 0 if not found.
**********************************************/

S16 findref(S8 *pb, S16 cb)  /* pointer to, and size of string */
{
S16 i;
S8   name[132];

strncpy(name, pb, cb);		/* move name local */
name[cb] = 0;  				/* null terminate */

i = iRefNext;
while (i>0) {				/* backwards through forward ref table */
	i--;
                    		/* Only compare if same size  */
	if (pfrt[i].NameSz == cb) {
		if (strncmp(name, pfrt[i].Ptr, cb) == 0) return(i);
	}
}
return(0);
}


/*****************************************************
   Evaluates only single token operands and sets up
   globals so EvalOper can finish the job.
   The actual value or register is left in
   rgToken[op][0], and the type is placed
   in rgTempl[op][0] for a memory operand.
*****************************************************/
S16 EvalOper1(S16 op)
{
S16 i;
U16 symtype;

	/* Set up symtype up front in case it's needed */

	if (ExpType == 1)				/* Local */
		symtype = lst[iExpSym].Type;
	else if (ExpType == 2) {		/* global */
		if (gst[iExpSym].Type & tEXTRN)
			nExtRef = iExpSym;
		symtype = gst[iExpSym].Type;
	}
	else symtype = 0;

	switch (rgToken[op][0]) {
		case REGIST:
			if (is_r32(rgTID[op][0]))  rgOpType[op] = r32;
			else if (is_r16(rgTID[op][0])) rgOpType[op] = r16;
			else if (is_r8(rgTID[op][0]))   rgOpType[op] = r8;
			else if (is_rCRG(rgTID[op][0])) rgOpType[op] = rCRG;
			else if (is_rDRG(rgTID[op][0])) rgOpType[op] = rDRG;
			else if (is_rTRG(rgTID[op][0])) rgOpType[op] = rTRG;
			else if (is_rSEG(rgTID[op][0])) rgOpType[op] = rSEG;
			rgOpReg[op] = rgTID[op][0];
			break;
		case NUMBER:
			if ((rgVal[op][0] >= -128) && (rgVal[op][0] <= 127))
                 rgOpType[op] = val8;
			else if ((rgVal[op][0] >= -32768) && (rgVal[op][0] <= 32767))
                 rgOpType[op] = val16;
			else rgOpType[op] = val32;
			if (!fOpImm) {
	            OpImm = rgVal[op][0];
	            fOpImm = 1;
	            }
			else {
	            OpImm2 = rgVal[op][0];
	            fOpImm2 = 1;
	            }
			break;
		case NUMOFF:		/* OFFSET var name. IMMEDIATE VALUE. */
			OpSize[op] = fDWord;
			rgOpType[op] = val32;
            OpImm = rgVal[op][0];
            fOpImm = 1;
			if (symtype & CLABEL)
				nFixUp = CCFIXTAG;
			else if (symtype & DLABEL)
				nFixUp = CDFIXTAG;
			if (!symtype)
				fForRef = 1;
			break;
		case SYMOFF:   /* Check for ALL jump, call & Loop instructions */

			if (ExpType == 1)				/* Local */
				symtype = lst[iExpSym].Type;
			else if (ExpType == 2) {		/* global */
				if (gst[iExpSym].Type & tEXTRN)
					nExtRef = iExpSym;
				symtype = gst[iExpSym].Type;
			}
			else							/* Unknown - Forward */
				symtype = 0;

			if (((CrntInst >= xJA) &&
			     (CrntInst <= xJZ)) ||
                 (CrntInst == xCALL)) {

				if (OpSize[op] & fShort)
					rgOpType[op]  = rel8;
				else
					rgOpType[op] = relW;
	   	        OpImm = rgVal[op][0];
	   	        fOpImm = 1;
			}

			else if ((CrntInst >= xLOOP) && (CrntInst <= xLOOPZ)) {
					rgOpType[op] = rel8;
		   	        OpImm = rgVal[op][0];
		   	        fOpImm = 1;
			}
			else {
				rgOpType[op]  = mem;
	            OpMType |= fDisp32;
	            OpDisp = rgVal[op][0];
				if (symtype & CLABEL)
					nFixUp = CCFIXTAG;
				else if (symtype & DLABEL)
					nFixUp = CDFIXTAG;
			}
			if (!symtype)
				fForRef = 1;
			break;
		default:
 	 	   line_error(15);
		   return(0);
	} /* switch */

/*  print_Oper(op); Testing only... */

return(1);
}

/***********  EvalOper *******************************
   Evaluates the array of reserved words, registers,
   numbers, etc. that should make up an operand.
   For single token operands it's easy! BUT for memory
   operands it gets a little more complicated.
   For memory operands we look for key sequences of
   tokens that make up the "effective address" as
   described in the Intel documentation, then see if
   we have all the pieces to make one (EA).
   This returns 1 if we can classify the operand and
   there were no errors.  The type of operand we find
   (e.g., r8, r16, r32, val8, val16, val32, mem,
   relW, etc.) is placed in rgOpType[op].
   This calls EvalOper1() to handle single token
   oprerands and evaluates multiple token operands
   internally. The reason we break single token operands
   out in a separate call is for speed and clarity
   of the code.
   Special handling is required for for JMP, Jcond,
   and CALL instructions because they may be using
   a forward reference not yet defined.  This is the
   only case where such a reference is allowed.
*****************************************************/

S16 EvalOper(S16 op)
{
S16 i;
S8   fDone, fOpenSQ, fError;
U16 symtype;
/*
These are the Raw operands:
S16  rgToken[3][20];  Raw tokens in the operand
S32  rgVal[3][20];    value if token is number/displacement
S16  rgTID[3][20];	  Register ID if token is a register
S16  rgTCnt[3];		  total count of tokens in raw operand

 This is what is produced:
*/

rgOpType[op] = 0;	/* int - Operand type for compare to instruction */
rgOpReg[op] = 0;    /* If reg only, this which one */

i = 0;		 /* index into raw tokens */
fError = 0;  /* set true for invalid operand error */
fDone = 0;   /* Set when no more tokens are expected in operand */
fOpenSQ = 0; /* keep track of [] */

/* flags for segment instruction prefix - NONE unless otherwise set */
/* If it's a single token operand, take the fast, easy way out! */

if (rgTCnt[op] == 1) {
	return (EvalOper1(op));
}

else {		/* multiple tokens in operand */

	/* with more than 1 token it is usually a memory reference
	   but not always.  We will default to mem and change those we
	   find that aren't.  */

	rgOpType[op] = mem;

	/* Segment prefix?  If so, set flag and eat 2 tokens */

	if ((rgToken[op][0]==REGIST) && (is_rSEG(rgTID[op][0]))) {
		if (rgToken[op][1] == COLON) {
			switch (rgToken[op][0]) {
				case rDS: OpPrefix |= fDSp; break;
				case rES: OpPrefix |= fESp; break;
				case rSS: OpPrefix |= fSSp; break;
				case rFS: OpPrefix |= fFSp; break;
				case rGS: OpPrefix |= fGSp; break;
				case rCS: OpPrefix |= fCSp; break;
				default:;
			}
			i += 2;		/* skip rSEG and colon */
		}
		else {
 	 	   line_error(16);
		   return(0);
	   }
	}

/* Loop through the raw tokens looking for Base Reg, Index Reg,
   Scale value, or displacement and setting varaibles as needed.
*/

	while ((i < rgTCnt[op]) &&
		  (!fError) &&
		  (!fDone)) {
		switch (rgToken[op][i]) {

		case REGIST:
			if (is_r32(rgTID[op][i])) {  /* check for Indx & Base Reg */
                if (rgToken[op][i+1] == STAR) {  		/* Index w/Scale */
					if (!(OpMType & fIndx))  { 	/* still OK */
                        OpMType |= fIndx;
                        OpIndx = rgTID[op][i];
						if (rgToken[op][i+2] == NUMBER) {
							switch (rgVal[op][i+2]) {
							case 2: OpMType |= fScale2; break;
							case 4: OpMType |= fScale4; break;
							case 8: OpMType |= fScale8; break;
							default:
								line_error(17);
								fError = 1;
							}
						}
						else {
							line_error(18);
							fError = 1;
						}
					}
					else {
						line_error(19);
						fError = 1;
					}
					i+=3;	/* get past *NUMBER */
                }
                /* Must be base, unless fBase true, then try Index */
                else {
					if (!(OpMType & fBase)) { /* Base for sure */
                        OpMType |= fBase;
                        OpBase = rgTID[op][i];
                        i++;
					}
					else {	/* try Index */
						if (!(OpMType & fIndx)) { /* It's free, use it */
	                        OpMType |= fIndx;
    	                    OpIndx = rgTID[op][i];
	                        i++;
						}
					}
				}
			}
			else { /* must be 32 bit general purpose register */
 		 		line_error(20);
				fError = 1;
			}
			break;


		case SYMOFF:  /* One symbol was used by itself or in an expression */
			if (ExpType == 1)				/* Local */
				symtype = lst[iExpSym].Type;
			else if (ExpType == 2) {		/* global */
				if (gst[iExpSym].Type & tEXTRN)
					nExtRef = iExpSym;
				symtype = gst[iExpSym].Type;
			}
			else							/* Unknown - Forward */
				symtype = 0;

			if (OpMType & (fDisp32|fDisp8)) {   /* Already a disp! */
	 	 		line_error(21);
	 	 		fError = 1;
	 	 		}

			/* Check for conditional jumps. */

			else if ((CrntInst >= xJA) &&
				     (CrntInst <= xJZ) &&
				     (CrntInst != xJMP) &&
				     (!(fOpenSQ)))
			{
					if (OpSize[op] & fShort)
						rgOpType[op] = rel8;
					else rgOpType[op] = relW;
					if (fOpImm) {
			   	        OpImm2 = rgVal[op][i];
			   	        fOpImm2 = 1;
			        }
					else {
			   	        OpImm = rgVal[op][i];
			   	        fOpImm = 1;
					}
					if (!symtype)
						fForRef = 1;
			}

				/* Check for JMP */

			else if (CrntInst == xJMP)  /* &&  (!(fOpenSQ))) */
			{
				if ((OpSize[op] & fFar) || (symtype & tFAR)) {
					rgOpType[op] = iSAD;
					if (fOpImm) {
			   	        OpImm2 = rgVal[op][i];
			   	        fOpImm2 = 1;
			        }
					else {
			   	        OpImm = rgVal[op][i];
			   	        fOpImm = 1;
					}
				}
				else if (OpSize[op] & fShort) {
					rgOpType[op] = rel8;
			        OpImm = rgVal[op][i];
			        fOpImm = 1;
					if (!symtype)
						fForRef = 1;
				}
				else if (OpSize[op] & fFWord) {
					rgOpType[op]  = memF;  /* memF = 32 disp which has 16:32 */
		            OpMType |= fDisp32;
		            OpDisp = rgVal[op][i];
					if (symtype & CLABEL)
						nFixUp = CCFIXTAG;
					else if (symtype & DLABEL)
						nFixUp = CDFIXTAG;
					if (!symtype)
						fForRef = 1;
				}
				else {
					rgOpType[op] = relW;
			   	    OpImm = rgVal[op][i];
			   	    fOpImm = 1;
					if (!symtype)
						fForRef = 1;
				}
			}

				/* Check for CALL */

			else if ((CrntInst == xCALL) && (!(fOpenSQ)))
			{
				if ((OpSize[op] & fFar) || (symtype & tFAR)) {
					rgOpType[op] = iSAD;
					if (fOpImm) {
			   	        OpImm2 = rgVal[op][i];
			   	        fOpImm2 = 1;
				    }
					else {
			   	        OpImm = rgVal[op][i];
			   	        fOpImm = 1;
					}
				}
				else if (OpSize[op] & fFWord) {
					rgOpType[op]  = memF;  /* memF = 32 disp which has 16:32 */
		            OpMType |= fDisp32;
		            OpDisp = rgVal[op][i];
					if (symtype & CLABEL)
						nFixUp = CCFIXTAG;
					else if (symtype & DLABEL)
						nFixUp = CDFIXTAG;
					if (!symtype)
						fForRef = 1;
				}
				else {		/* Relative call */
					rgOpType[op] = relW;
					if (!symtype)
						fForRef = 1;
				}
			}

				/* Check for SGDT SIDT */

			else if ((CrntInst == xSGDT) || (CrntInst == xSIDT))
			{
				if (OpSize[op] & fFWord) {
					rgOpType[op]  = memF;  /* memF = 32 disp which has 16:32 */
		            OpMType |= fDisp32;
		            OpDisp = rgVal[op][i];
					if (symtype & CLABEL)
						nFixUp = CCFIXTAG;
					else if (symtype & DLABEL)
						nFixUp = CDFIXTAG;
					if (!symtype)
						fForRef = 1;
				}
			}

			  /* Check for Loop */

			else if ((CrntInst >= xLOOP) && (CrntInst <= xLOOPZ))
			{
					rgOpType[op] = rel8;
			   	    OpImm = rgVal[op][0];
			   	    fOpImm = 1;
					if (!symtype)
						fForRef = 1;
			}
			else {
				    OpDisp = rgVal[op][i];
					OpMType |= fDisp32;
					if (symtype & CLABEL)
						nFixUp = CCFIXTAG;
					else if (symtype & DLABEL)
						nFixUp = CDFIXTAG;
					if (!symtype)
						fForRef = 1;
			}
			i++;
			break;

		case NUMBER:  /* can be 8 or 32 bit disp, or hard address */
	        OpDisp = rgVal[op][i];
			if ((rgVal[op][i] >= -128) && (rgVal[op][i] <= 127))
				OpMType |= fDisp8;
			else
				OpMType |= fDisp32;
			i++;
			break;

		case OPENSQR:
			if (fOpenSQ) {
				line_error(23);
				fError = 1;
				}
			else fOpenSQ = 1;
			i++;
			break;
		case CLOSSQR:
			if (!fOpenSQ) {
				line_error(24);
				fError = 1;
				}
			else fOpenSQ = 0;
			i++;
			break;
		case COLON:		/*  for far addresses */
			i++;
			break;
		case PLUS:
			i++;
			break;
	    case rBYTE:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fByte;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
	    case rWORD:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fWord;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
	    case rDWORD:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fDWord;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
	    case rFWORD:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fFWord;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
		case rNEAR:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fNear;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
		case rFAR:
	    	if ((!OpSize[op]) && (rgToken[op][i+1] == rPTR)) {
				OpSize[op] |= fFar;
				i+=2;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
		case rSHORT:
	    	if (!OpSize[op]) {
				OpSize[op] |= fShort;
				i++;
				}
			else {
				line_error(25);
				fError = 1;
				}
			break;
		default: {
			line_error(32);
			fprintf(lst_fh, "  %d  ", rgToken[op][i]);  /* TESTING */
			i++;
			}

		} /* switch */
	} /* while */
}
if (fError) return(0);
if ((fDone) && (i< rgTCnt[op])) {
	line_error(33);
	return(0);
	}

return(1);

}

/********************************************
  Adds ALL forward reference to the Ref table.
  This holds references to all foward addresses
  (unknown) in the current module.
  This stores all Refs UPPER case.
*********************************************/
void ForRef(U8 type, S32 Offset)
{
S16 i;

if (pRefNext >= (pRefBuf + FREFBUFMAX))
	fatal_error("Forward Reference buffer overflow...");
if (iRefNext >= FREFSMAX)
	fatal_error("Forward Reference table overflow...");

/* Make a forward reference table entry */

strncpy(pRefNext, UString, UCBString);
pfrt[iRefNext].NameSz = UCBString;	/* size of name */
pfrt[iRefNext].Ptr = pRefNext;
pfrt[iRefNext].Line = lineno[level];	/* for error reporting */
pfrt[iRefNext].Type = type;			/* type of ref */


pfrt[iRefNext].Offs = Offset;		/* Offset to correction in CS or DS */

/* update for next symbol */

pRefNext += UCBString;
iRefNext++;

}

/********************************************
  Adds fixup table entries to be placed in
  the run file.	 This includes DLL entries.
*********************************************/

void FixUp(U8 typef, S32 Offset, U16 iSymbol)
{

if (typef == CDFIXTAG)
	nCDFix++;
else if (typef == DDFIXTAG)
	nDDFix++;
else if (typef == DCFIXTAG)
	nDCFix++;
else if (typef == CCFIXTAG)
	nCCFix++;


if (iFUNext >= FIXUPSMAX)
	fatal_error("Fixup Table overflow...");
pfut[iFUNext].type = typef;
pfut[iFUNext].Offs = Offset;
pfut[iFUNext].iSym = iSymbol;  /* global symbol entry for DLL */

iFUNext++;

}

/********************************************
  Adds External Reference to table.
  This DOES NOT includes DLL Refs.
  ETypes are the same as ForRef types
  except there and no 8 bit externals.
*********************************************/

void ExtRef(U8 EType, U16 iSymbol)
{

if (iERefNext >= EREFSMAX)
	fatal_error("External Reference Table overflow...");

if (fDataSeg)
	ert[iERefNext].Offs = oNextData;
else {
	ert[iERefNext].Offs = oNextCode;
	}
ert[iERefNext].iSym = iSymbol;  /* global symbol entry for external */
ert[iERefNext].Type = EType;  /* global symbol entry for external */

iERefNext++;
}

/*****************************************************
   Reads in all of the reserved words, numbers, math
   operators, etc. that make up one operand.  They are
   read into one of 3 2D arrays as indicated by "op".
   Integer rgTCnt[op] is number if items.  A special
   case for jump and call instructions is tested if
   we end up with an Unknown Symbol as and operand.
   We Error out if we have an UNKSYM without a
   jump or call.
*****************************************************/

S16 GetOper(S16 op)
{
S16 i, T;

i = 0;
while (1) {
	T = Parse();
	switch (T) {
	  case ZERO:
		    rgTCnt[op] = i;
 			if (i) return(1);
 			else return(0);
	  case REGIST:
		rgTID[op][i] = TReg;
		rgToken[op][i] = T;
		break;
	  case NUMBER:  /* check for special case of REG*2,*4,*8  */
		if ((i > 1) &&
			(rgToken[op][i-1] == STAR) &&
			(rgToken[op][i-2] == REGIST)) {
				rgVal[op][i] = TNumber;
				rgToken[op][i] = T;
				break;	/* only break here if we find special case */
			}			/* Else fall thru to Expression */
	  case LSYMBOL:
	  case SYMBOL:
	  case DOLLAR:   	/* These should all evaluate to a number!! */
	  case OPENRND:
	  case MINUS:
	  case UNKSYM:
	  case rOFFSET:
		if (Expression()) {
			rgVal[op][i] = TNumber;
			rgToken[op][i] = Token;		/* Token instead of T (From Expr)*/
			}
		else {
			line_error(35);
			return(0);
			}

		break;
	  case COMMA:
			if (!i) {
				line_error(36);
				return(0);
			}
		    rgTCnt[op] = i;
			ReturnToken();
			return(1);
	  case OPENSQR:
	  case CLOSSQR:
	  case STAR:
	  case PLUS:
	  case SLASH:
	  case SEMI:
	  case COLON:
	  case rFAR:
	  case rBYTE:
	  case rWORD:
	  case rDWORD:
	  case rFWORD:
	  case rPTR:
	  case rSHORT:
			rgToken[op][i] = T;
			break;
	  default:
			line_error(38);
			return(0);
	}
 i++;
} /* while */
}

/********************************************
 This finds the first matching entry in rgINS
 by first matching the instruction number,
 then matching operands.  The table is indexed
 with an array providing the first entry for
 the instruction. This speed things up a bit.
*********************************************/

S16 INSEntry(S16 InsNum, S16 nOpers)
{
S16 i;

	i = rgInsLookUp[InsNum];
	while (i <= nrgINS) {

		/* instruction entries of the same type
		are all kept together. This is an early out.
		*/

		if (rgINS[i][0] != InsNum)
		  return(0);	/* couldn't find a match at all... */

		/* See if all the operators match by calling is_Comp for each.
		*/

	  	if ((is_Comp(i,0)) &&
	 	    (is_Comp(i,1)) &&
	 	    (is_Comp(i,2)))

	 		return(i);		/* yes, they all match! */

	 	i++;	/* No, goto next entry */
	}
}


/********************************************
  EmitInst spits out the instruction bytes that
  were encoded into variables by EncodeInst.
  This handles output to the code segment
  file as well as the text to the list file.
  It updates the code segment address and
  also adds fixup records as they are needed.

The instruction  (up to 16 bytes) is ordered
as follows:
  Instruction Prefix   0 or 1 byte
  Address Size Prefix  0 or 1 byte
  Operand Prefix       0 or 1 byte
  Segment Override     0 or 1 bytes
  Opcode               1 or 2 bytes
  MODR/M               0 or 1 bytes
  SIB                  0 or 1 bytes
  Displacement         0,1,2 or 4 bytes
  Immediate            0,1,2, or 4 bytes
**********************************************/

void EmitInst(void)
{
U8   oppfx;
S8   sbyte;
S16  sword;
U16  i;		/* used local to each section if needed */



	/*
    Instruction prefix: 0 or 1 byte (Lock, Rep, etc.)
	*/

	if (InstPfx) {
		if (InstPfx==xLOCK) {
			if (fListA) {
				Column += 2;
				put_hexb(0x0F, lst_fh);
				}
			OutByte(0xF0);
			}
		else if ((InstPfx==xREPNE) || (InstPfx==xREPNZ)) {
			if (fListA) {
				Column += 2;
				put_hexb(0xF2, lst_fh);
				}
			OutByte(0xf2);
			}
		else {	/* Must be REP... */
			if (fListA) {
				Column += 2;
				put_hexb(0xF3, lst_fh);
				}
			OutByte(0xf3);
			}
		if (fListA)
			Column += fprintf(lst_fh, "| ");
	}

/*
   Skip Address Size prefix: (67h)  cause we aren't
   doing USE16, nor do we support 16 bit addressing modes!
*/

/* Operand Size prefix: 0 or 1 (66h)  */

	if (OpSizeA & fWord) {
		if (fListA) {
			Column += 2;
			put_hexb(0x66, lst_fh);
			Column += fprintf(lst_fh, "| ");
			}
		OutByte(0x66);
		}

/* Segment Override prefix */

	switch (OpPrefix) {
		case fDSp: oppfx = 0x3e; break;
		case fESp: oppfx = 0x26; break;
		case fSSp: oppfx = 0x36; break;
		case fFSp: oppfx = 0x64; break;
		case fGSp: oppfx = 0x65; break;
		case fCSp: oppfx = 0x2e; break;
		default: oppfx = 0;
	}
	if (oppfx) {
		if (fListA) {
			Column += 2;
			put_hexb(oppfx, lst_fh);
			Column += fprintf(lst_fh, "| ");
			}
			OutByte(oppfx);
		}

/*   OpCode byte 1 (optional)
     bOpc1 was setup in	EncodeInst (either 0 or 0Fh)
*/

	if (bOpc1) {
		if (fListA) {
			Column += 2;
			put_hexb(bOpc1, lst_fh);
			Column += fprintf(lst_fh, " ");
			}
			OutByte(bOpc1);
		}

/*   OpCode byte 2 (always sent) */

	if (fListA) {
			Column += 2;
			put_hexb(bOpc2, lst_fh);
			Column += fprintf(lst_fh, " ");
		}

	OutByte(bOpc2);

/*  ModR/M  (optional)  */

	if (fModRM) {
		if (fListA) {
			Column += 2;
			put_hexb(bModRM, lst_fh);
			Column += fprintf(lst_fh, " ");
		}
		OutByte(bModRM);
	}

/*  SIB  (optional) */

	if (fSIB) {
		if (fListA) {
			Column += 2;
			put_hexb(bSIB, lst_fh);
			Column += fprintf(lst_fh, " ");
		}
		OutByte(bSIB);
	}

/*
   Disp: 0, 1, 2 or 4
   A Displacement is a memory reference (an offset in a segment)
   that is encoded as part of the instruction. The value to encode
   is placed in OpDisp when the instruction is parsed and
   evaluated.
*/

	if (OpMType & fDisp32) {

		if (fListA) {
			Column += 8;
			put_hexd(OpDisp, lst_fh);
			if ((nExtRef) || (fForRef))
				Column += fprintf(lst_fh, "r ");
			else
				Column += fprintf(lst_fh, "  ");
		}
		if (nExtRef)
			ExtRef(CSEGREF, nExtRef);
		else if (fForRef)
			ForRef(CSEGREF, oNextCode);
		else if (nFixUp)
			FixUp(nFixUp, oNextCode, 0); /* Code ref to data */
		OutDWord(OpDisp);
	}

	if (OpMType & fDisp8) {

		if (fListA) {
			sbyte = OpDisp;
			Column += 2;
			put_hexb(sbyte, lst_fh);
			Column += fprintf(lst_fh, "  ");
		}
		OutByte(OpDisp);
	}

/*
   Immediate: 0, 1, 2 or 4
   The type of instruction operands tell us if we must encode
   immediate values as part of the instruction.  All instructions
   have only one immediate value except ENTER. We make a check here
   to determine if it is this instruction.

     imm8  = Immediate Byte in OpImm
     imm16 = immediate 16 bit value in OpImm
     immX  = immediate value matching OpSize in OpImm
     rel8  = immediate Byte calculated from a label
     relW  = immediate Word (16 or 32) calculated from a label
     iSAD  = immediate DWORD:WORD from Far Pointer (OpImm & OpImm2)

     immv1 is not encoded (implied by instruction)
     immv3 is not encoded (also implied - INT 03 - Debug)
   */
	if (fOpImm) 
	{
		if ((rgINS[iInstEntry][1] == immX) ||
		    (rgINS[iInstEntry][2] == immX) ||
            (rgINS[iInstEntry][3] == immX))
        {
			if (OpSizeA & fByte)
			{
				if (fListA)
				{
					sbyte = OpImm;
					Column += 2;
					put_hexb(sbyte, lst_fh);
					Column += fprintf(lst_fh, "  ");
				}
				OutByte(OpImm);
			}
			else if (OpSizeA & fWord)
			{
				if (fListA) 
				{
					sword = OpImm;
					Column += 4;
					put_hexw(sword, lst_fh);
					Column += fprintf(lst_fh, "  ");
				}
				OutWord(OpImm);
			}
			else {					/* MUST be a DWord */
				if (fListA) {
					Column += 8;
					put_hexd(OpImm, lst_fh);
					if (nFixUp)
						Column += fprintf(lst_fh, "r ");
					else
						Column += fprintf(lst_fh, "  ");
				}
				if (nExtRef)
					ExtRef(CSEGREF, nExtRef);
				else if (fForRef)
					ForRef(CSEGREF, oNextCode);
				else if (nFixUp)
					FixUp(nFixUp, oNextCode, 0); /* Code ref to data */
				OutDWord(OpImm);
			}
		}

		if ((rgINS[iInstEntry][1] == imm16) ||
		    (rgINS[iInstEntry][2] == imm16) ||
            (rgINS[iInstEntry][3] == imm16))   {
			if (fListA) {
				Column += 4;
				sword = OpImm;
				put_hexw(sword, lst_fh);
				Column += fprintf(lst_fh, "  ");
				}
			OutWord(OpImm);
		}
		else if ((rgINS[iInstEntry][1] == imm8) ||
		         (rgINS[iInstEntry][2] == imm8) ||
		         (rgINS[iInstEntry][3] == imm8) ||
		         (rgINS[iInstEntry][1] == ims8) ||
		         (rgINS[iInstEntry][2] == ims8) ||
                 (rgINS[iInstEntry][3] == ims8))
        {
			if (fListA) {
				Column += 2;
				sbyte = OpImm;
				put_hexb(sbyte, lst_fh);
				Column += fprintf(lst_fh, "  ");
				}
			OutByte(OpImm);
		}

		/* Special check for Enter. It is only instructon with 2 Imms!*/

		if (rgINS[iInstEntry][0] == xENTER)
        {
			if (fListA) {
				Column += 2;
				sbyte = OpImm2;
				put_hexb(sbyte, lst_fh);
				Column += fprintf(lst_fh, "  ");
				}
			OutByte(OpImm2);
		}


		/* With relative values for immediates, OpImm comes in as
	 	  the address of the target jump or call. We must take
		   the current code offset (of the next instruction) and
		   subtract it from the target (OpImm) to get the relative
		   jump value. If it is an UNKSYM we will put the value
		   of the target into the FRT to hold it and
		   do the math on the second pass of the machine code.
		   Math Example:

			oCode  = 100  (next instruction)
			Target =  50
			Jump = -50  (Target-Current)

			oCode  = 100  (next instruction)
			Target = 150
			Jump = 50  (Target-Current)

			Relative immediate values found in the GST as EXTERN
			require an ERT entry so they can be fixed up when
			the final code module is written to the run file.

			Relative immediate values found in the GST as PUBLICS
			can be fixed up NOW.

			Relative immediate values NOT found are assumed local
			and an entry is made in the FRT so they can be fixed
			up when this code module is written to the main
			temp code module.
 		   */

		if (rgINS[iInstEntry][1] == relW) {
			if (nExtRef) {						/* list external */
				OpImm = 0;
				ExtRef(CCR32REF, nExtRef);
			}
			else if (fForRef) {					/* list Forward Relative */
				OpImm = 0;
				ForRef(CCR32REF, oNextCode);
			}
			else {								/* Fix known ref */
				OpImm = OpImm - (oNextCode + 4);
			}

			if (fListA) {
				Column += 8;
				put_hexd(OpImm, lst_fh);
				if ((!fForRef) && (!nExtRef))
					Column += fprintf(lst_fh, "  ");
				else
					Column += fprintf(lst_fh, "R ");
			}
			OutDWord(OpImm);
		}

		if (rgINS[iInstEntry][1] == rel8) {
			if (!fForRef) {					/* Fix KNOWN Relative */
				OpImm = OpImm - (oNextCode + 1);
				if ((OpImm > 127) || (OpImm < -127))
					line_error(39);
			}
			else {							/* Else List Unknown */
				ForRef(CCR8REF, oNextCode);
				OpImm = 0;
				}

			if (fListA) {
				Column += 2;
				sbyte = OpImm;
				put_hexb(sbyte, lst_fh);
				if ((!fForRef) && (!nExtRef))
					Column += fprintf(lst_fh, "  ");
				else
					Column += fprintf(lst_fh, "R ");
			}
			OutByte(OpImm);
		}

		if (rgINS[iInstEntry][1] == iSAD) {
			if (fListA) {
				Column += 4;
				sword = OpImm;
				put_hexw(sword, lst_fh);
				Column += fprintf(lst_fh, ":");
				Column += 4;
				put_hexd(OpImm2, lst_fh);
				Column += fprintf(lst_fh, "  ");
			}
			OutWord(OpImm);
			OutDWord(OpImm2);
		}
	}
}


/********************************************
  This encodes the instructions into bytes
  and calls EmitInst() to put them into the
  current code segment.	The instruction can
  be as long as 16 bytes if all options are used.
  The order of the bytes and their possible
  sizes are as follows:
   Inst Prefix: 0 or 1 (Lock, Rep, etc.)
   Address Size prefix: 0 or 1 (67h) - We don't use this
   Operand Size prefix: 0 or 1 (66h)
   Segment Override: 0 or 1
   OpCode: 1 or 2
   ModR/M: 0 or 1
   SIB: 0 or 1
   Disp: 0, 1, 2 or 4
   Immediate: 0, 1, 2 or 4
   This routine follows the guidance to build
   instructions given in the rgINS table
   (as bit flags). It calls	EmitInst() to
   emit the code, list-file information and
   fixups to the FUT.
   **********************************************/
void EncodeInst(void)
{
S8   fError;
S16 i;
U8 bTmp;

/*
U8 bOpc1;		Zero if not used   THESE ARE GLOBAL...
U8 bOpc2;		Always used
U8 bModRM;
U8 bSIB;
S8 fModRM;	    Is bModRM set/used?
S8 fSIB;		Is bSIB set/used?
*/

fModRM = 0;		/* not used by default */
fSIB   = 0;


/* SKIP the Address Size Prefix for now because we don't do 16 Bit
   Effective addresses
*/


/*
   Now we will build the instruction in (up to) 4 temporary bytes.
   We do it in temporary byte vars so we can see if an Operand Prefix
   and a possible address fixup record is required before we
   actually put it into the code segment.
*/

/*
  The WORD forms of string instructions require us
  to force a WORD size. We set this in the instruction table
  as qUW in byte [4],
*/

if (rgINS[iInstEntry][4] & qUW)
  OpSizeA |= fWord;

/* Put in the first byte of Opcode from table (if required). qP0F is
   set in rgINS[iInstEntry][4] if a 0Fh prefix is required for this inst.
   A 0 in bObc1 indicates that this byte is NOT to be used.
*/

if (rgINS[iInstEntry][4] & qP0F)
    bOpc1 = 0x0F;
else bOpc1 = 0;

/* Get the Opcode from table into bOpc2 */

bOpc2 = rgINS[iInstEntry][5];

/* The flags zMR1, zMR2, zMOP, zAR1, and zAR2 all indicate that the
   ModRM is needed. The following set of if-else statements checks these
   flags and sets REG/OP field of MOD/RM byte if required.
   OpSize should already be set. We also complete
   the instruction encoding (with the exception of any immediate values)
   if the other operand is a register.
*/

if (rgINS[iInstEntry][7] & zMR1) {
	bModRM = rgINS[iInstEntry][6];
	fModRM = 1;
	if (is_Reg(rgOpType[0]))
	    EncodeRegBits(rgOpReg[0], &bModRM, 3);
	if (is_Reg(rgOpType[1])) {
	    EncodeRegBits(rgOpReg[1], &bModRM, 0);
	    bModRM |= 0xC0;	/* indictes REG in RM field */
		}
	}
else if (rgINS[iInstEntry][7] & zMR2) {
	bModRM = rgINS[iInstEntry][6];
	fModRM = 1;
	if (is_Reg(rgOpType[1]))
	    EncodeRegBits(rgOpReg[1], &bModRM, 3);
	if (is_Reg(rgOpType[0])) {
	    EncodeRegBits(rgOpReg[0], &bModRM, 0);
	    bModRM |= 0xC0;	/* indictes REG in RM field */
		}
	}
else if (rgINS[iInstEntry][7] & zMOP) {
	bModRM = rgINS[iInstEntry][6];
	fModRM = 1;
	if (is_Reg(rgOpType[0])) {
	    EncodeRegBits(rgOpReg[0], &bModRM, 0);
	    bModRM |= 0xC0;	/* indictes REG in RM field */
		}
	}
else if (rgINS[iInstEntry][7] & zAR1) {
	bTmp = 0;
    EncodeRegBits(rgOpReg[0], &bTmp, 0);
	bOpc2 += bTmp;
	}
else if (rgINS[iInstEntry][7] & zAR2) {
	bTmp = 0;
    EncodeRegBits(rgOpReg[1], &bTmp, 0);
	bOpc2 += bTmp;
	}
else if ((rgINS[iInstEntry][7] & zRG1) && (rgOpType[0] != mem)) {
	bModRM = rgINS[iInstEntry][6];
	fModRM = 1;
    EncodeRegBits(rgOpReg[0], &bModRM, 3);
    bModRM |= 0xC0;	/* indictes REG in RM field */
	}


	/* OpSize may not be required for many instructions, but we know
	   for a fact it MUST be known for memory operands!
	*/

	if ((rgOpType[0] == mem) ||
		(rgOpType[1] == mem) ||
		(rgOpType[0] == memF)) {
			if  (!(OpSizeA & (fByte|fWord|fDWord|fFWord))) {
				line_error(40);
				return;
				}
	}

	/*
	   If zORD, see if we have word or dword register and OR the Opcode
	   (Opc2) with 01 if so.
	*/

	if (rgINS[iInstEntry][7] & zORD) {
		if (OpSizeA & (fWord | fDWord))
		bOpc2 |= 0x01;
		}

	/* Perform the following additonal steps if we have a memory reference
	   as determined by iMemEntry.
	*/

	fSIB = 0;
	if (iMemEntry) {

	/* If SIB is needed (as determined from OpMType), get it and also
  	 OR ModRM by required value from rgM32 table
  	*/

		bModRM |= rgM32[iMemEntry][2];	/* Use ModRM 'OR' value from table */
	 	if (rgM32[iMemEntry][1]) { 		/* is there an SIB byte? */
	 		bModRM &= 0x38;					/* 00111000 leaves Reg bit on */
			bModRM |= rgM32[iMemEntry][2];	/* 'OR' ModRM with table value */
	 		bSIB = rgM32[iMemEntry][3];
	 		fSIB = 1;
		}


		/* At this point we have our Opcode, ModRM, and SIB (if required).
		   The MOD and SS bit are already filled in from the table.
		   This means we just stick the register values or bits for Disp etc
		   in the correct positions to complete it.
		   If we didn't have an SIB, we either have a single displacement,
		   a base or both.
		*/

		if (!fSIB) {		/* only a base, disp, or both */
			if (OpMType & fBase)
			    EncodeRegBits(OpBase, &bModRM, 0);
			else
			    bModRM |= 0x05;		/* only a disp32 */
			}
		else {
			if (OpMType & fBase)
			    EncodeRegBits(OpBase, &bSIB, 0);
			else bModRM |= 0x20;				/* else set [--][--] bits */
			if (OpMType & fIndx)
			    EncodeRegBits(OpIndx, &bSIB, 3);
			}
	}

	EmitInst();

}


/********************************************
  When the dispatcher detects an Instruction
  it calls this code to read in (via parse)
  the parametrs for the instruction and to
  put together the opcodes.  First we loop thru
  and get up to 3 operands, then we evaluate
  them (classify by type). Next we look up
  the instruction and find a match for the
  operand types they have specified, and
  finally, we encode the instruction into the
  code segment updating the code segment offset
  pointer.
*********************************************/
void Instruction(void)
{
S16 i;
S8   fError;
S8   OpSizeTmp;

fError = 0;
if (fDataSeg) {
	line_error(41);
	return;
	}

/* If the instruction is a prefix instruction, save it
   and Parse again to get the real instruction */

if ((TInst==xREP) || (TInst==xREPE) || (TInst==xREPZ) ||
    (TInst==xREPNE) || (TInst==xREPNZ) || (TInst==xLOCK)) {
    InstPfx = TInst;
	i = Parse();
	if (i != INSTRU) {
		line_error(42);
		return;
	}
} else InstPfx = 0;

/* RESET all global instruction variables */

CrntInst = TInst;	/* Save the instruction */
nOperands = 0;		/* none yet... */
rgOpType[0] = 0;
rgOpType[1] = 0;
rgOpType[2] = 0;
OpMType = 0;	    /* char - memory type (a byte to compare to rgM32) */
OpSize[0] = 0;		/* char - Mem op size (fByte, fWord, fDword, fFword) */
OpSize[1] = 0;		/* char - Mem op size (fByte, fWord, fDword, fFword) */
OpSizeA = 0;		/* char - Mem op size (fByte, fWord, fDword, fFword) */
OpSizeTmp = 0;		/* char - Mem op size (fByte, fWord, fDword, fFword) */
OpPrefix = 0;		/* For Segment register prefix flags */
OpDisp = 0;         /* long - Displacement if fDisp8 or fDisp32 is TRUE */
OpBase = 0;         /* int - Base register if fBase is true */
OpIndx = 0;         /* int - Index register if fIndx is true */
fOpImm = 0;			/* No Immediate value yet */
OpImm = 0;			/* Default to 0 */
fOpImm2 = 0;		/* No second Immediate value yet */
OpImm2 = 0;			/* Default to 0 */
nFixUp = 0;			/* Fixup type if needed, else 0 */
nExtRef = 0;		/* GST entry if external ref was made */
fForRef = 0;		/* No Forward Ref in crnt inst yet */
ExpType = 0;		/* Type of symbol used in instruction */
ExpType0 = 0;		/* Type of symbol used in instruction */
iExpSym = 0;		/* Index into symtab for symbol if used */
iExpSym0 = 0;		/* Index into symtab for symbol if used */

if (GetOper(0)) {
	nOperands++;
	ExpType0 = ExpType;
	iExpSym0 = iExpSym;
	if ((Parse()== COMMA)) {
		if (GetOper(1)) {
			nOperands++;
			if ((Parse()== COMMA)) {
				if (GetOper(2)) {
					nOperands++;
					if (Parse()) {
						line_error(33);
						return;
					}
				}
		    }
		}
	}
}

/*
At this point we have the instruction operands stored in
tokenized form in arrays.  We now call EvalOper() which evaluates
the operands and fills in several variables that will assist in
building the instruction. EvalOper() returns 1 if it was a valid
operand, else 0.
*/

if (nOperands) {								/* at least one */
	if (EvalOper(0)) {
		if (nOperands > 1) {					/* a second operand */
			if (EvalOper(1)) {
				if (nOperands > 2) {			/* a third??? - could be... */
					if (!(EvalOper(2)))
						fError = 1;
				}
			}
			else fError = 1;
	    }
	}
	else fError = 1;
}

if (fError) return;

/*
   We must check to see what the word size of the instruction is
   by looking at register sizes if any are included in the instruction.
   If there are memory references or immediates involved, OpSizeA
   MUST be set properly else it's an error!	 When there is a symbol involved
   such as a move to a memory variable, we check the variable size and set
   the OpSize to the variable size. If it's still not set, the line will
   error out when we try to build the instruction because the caller
   should have explicitly set the size (e.g., DWORD PTR ...).

   If a register is one of the operands in a memory transfer
   (e.g., MOV [mem], EAX) then the move obviously defaults to the size
   of the register (no ifs, ands, or buts).

   If there is no reg involved, we then look at 'OpSize[i]'
   which may have been set by the size operators (e.g., DWORD PTR).
   If it's zero we look at iExpSym which was set if there was a single
   symbol involved in the memory reference. If there was, then we use it.
   If it is blank, we error out cause we don't know what size memory
   access we are doing!
   This is done for each operand then the two are compared. If one is
   zero we use the other, if both have something but are different
   we error out!

*/
	/* Find the first operand size */

	/* First, let's look to see if they forced it! */

	if  (OpSize[0] & (fByte|fWord|fDWord|fFWord))

	{ /* do nothing */ }

	/* If they didn't force it, we'll look for the register size! */

    else if (rgOpType[0] == r32) OpSize[0] |= fDWord;
    else if (rgOpType[0] == r16) OpSize[0] |= fWord;
    else if (rgOpType[0] == rSEG) OpSize[0] |= fWord;
    else if (rgOpType[0] == r8) OpSize[0] |= fByte;

		/* Still nothing, so let's at symbols.  */

	else if (ExpType0 == 1)
	{ 	/* Local symbol */
		if (lst[iExpSym0].Type & sBYTE) OpSize[0] |= fByte;
		else if (lst[iExpSym0].Type & sWORD) OpSize[0]  |= fWord;
		else if (lst[iExpSym0].Type & sDWORD) OpSize[0] |= fDWord;
		else if (lst[iExpSym0].Type & sFWORD) OpSize[0] |= fFWord;
	}
    else if (ExpType0 == 2)
	{   /* Global Symbol */
		if (gst[iExpSym0].Type & sBYTE) OpSize[0] |= fByte;
		else if (gst[iExpSym0].Type & sWORD) OpSize[0] |= fWord;
		else if (gst[iExpSym0].Type & sDWORD) OpSize[0] |= fDWord;
		else if (gst[iExpSym0].Type & sFWORD) OpSize[0] |= fFWord;
	}
	else if (ExpType0 == 3)
		OpSize[0] |= fDWord;		/* $ defaults to DWord */


	/* Find the second operand size. Check "forced fit" first */

	if  (OpSize[1] & (fByte|fWord|fDWord|fFWord))
	{ /* do nothing */ }

    else if (rgOpType[1] == r32) OpSize[1] |= fDWord;
    else if (rgOpType[1] == r16) OpSize[1] |= fWord;
    else if (rgOpType[1] == rSEG) OpSize[1] |= fWord;
    else if (rgOpType[1] == r8) OpSize[1] |= fByte;

		/* No registers, so let's look to see if they forced it! */

		/* Still nothing, so let's at symbols.  */

	else if (ExpType == 1)
	{ 	/* Local symbol */
		if (lst[iExpSym].Type & sBYTE) OpSize[1] |= fByte;
		else if (lst[iExpSym].Type & sWORD) OpSize[1]  |= fWord;
		else if (lst[iExpSym].Type & sDWORD) OpSize[1] |= fDWord;
		else if (lst[iExpSym].Type & sFWORD) OpSize[1] |= fFWord;
	}
    else if (ExpType == 2)
	{   /* Global Symbol */
		if (gst[iExpSym].Type & sBYTE) OpSize[1] |= fByte;
		else if (gst[iExpSym].Type & sWORD) OpSize[1] |= fWord;
		else if (gst[iExpSym].Type & sDWORD) OpSize[1] |= fDWord;
		else if (gst[iExpSym].Type & sFWORD) OpSize[1] |= fFWord;
	}
	else if (ExpType == 3)
		OpSize[1] |= fDWord;		/* $ defaults to DWord */

    /* Special cases for operand size matching. */

	if (CrntInst == xOUT)
		OpSize[0] = OpSize[1];


/*
   Now that have the operands, and we know the TYPE of data (fByte etc.)
   we call INSEntry to find the matching
   entry in the array that describes each instruction. If we don't
   find one that matches we error out telling them they have bad
   operands.
*/

/* find first matching entry in rgINS */

    iInstEntry = INSEntry(CrntInst, nOperands);

    if (!iInstEntry) {
    	line_error(44);
    	return;
    }

    /* Now we make sure that we have set all operand sizes
       and check for special cases as defined in rgINS.
	*/

	if (rgINS[iInstEntry][7] & zSIZ)
        OpSize[1] = OpSize[0];

    if (!OpSize[1])
        OpSize[1] = OpSize[0];

    if (!OpSize[0])
        OpSize[0] = OpSize[1];

    /* Give them an error if operand sizes don't match */

    if (nOperands > 1)
        if (OpSize[0] != OpSize[1]) {
           line_error(43);
	       return;
	   }

	OpSizeA = OpSize[0];   /* Set primary operand size */

/* If either the first or second operand was memory, we have to find
   which entry in rgM32 it matches before we can encode it. If none
   matches it is an invalid memory operand.  If neither is a mem operand
   we set iMemEntry to zero.  There is the special case of moffs type
   which is a short form of displacmement when moving data to and from thew
   accumulator (e.g., MOV EAX, VAR1).  We check for this and skip the
   mem check if this is the instruction type we are on.
*/

	if ((rgINS[iInstEntry][1] == moff) || (rgINS[iInstEntry][2] == moff))
		iMemEntry = 0;
	else if ((rgOpType[0]==mem) ||
			 (rgOpType[1]==mem) ||
			 (rgOpType[0]==memF))
	{

		for (i=1; i<nrgM32; i++) {
			if (OpMType == rgM32[i][0]) break;
		}

		/* if i is off the end of the array we
		   didn't find an entry that matched
		*/

		if (i==nrgM32) {
			line_error(45);
			return;
		}
			else iMemEntry = i;
	}
	else iMemEntry = 0;

/*
 At this point we should have all information we need to actually
 encode the instruction (unless it's a forward ref). So we call EncodeInst.
*/

EncodeInst();


}


/********************************************
   Create Storage in current segment. At this
   point we should see a storage statement as
   the current token followed by a number (or
   string if DB). If string (DB) we insert
   single byte values (one per char) into the
   segment.
*********************************************/
void Storage(void)
{
S32 TSave, NSave, nDUP;
S16 i, j, sword;
S8   fColon, fExpectColon, fComma, fDUP;
U16 symtype;

fDUP = 0;
nDUP = 1;		/* default duplicate value */
fComma = 0;
fColon = 0;
fExpectColon = 0;

if (!fMoreStorage) {		/* is it a continuation of previous line? */
	switch(Token) { 		/* parse returns Token */
   		case rDB:
   			StoreSize = 1;
	    	break;
	    case rDW:
	    	StoreSize = 2;
	    	break;
	    case rDD:
	    	StoreSize = 4;
	    	break;
		case rDF:
	    	StoreSize = 6;
	    	break;
		default:;
	}
}

/* Now we loop thru looking for strings and numbers taking into account
   the special case of DF which should be DWORD:WORD, and OFFSET
   which will be a DWORD.
*/

while (1) {
	if (fMoreStorage) {
		i = Token;
		fMoreStorage = 0;
	}
	else i = Parse();

	switch(i) {
		case STRING:
			fComma=0;
			if (StoreSize==1) {
				for(j=0; j<CBString; j++) {
					if (fListA) {
						put_hexb(TString[j], lst_fh);
						Column += 2;
						Column += fprintf(lst_fh, "  ");
						if (Column > 51) {
							fprintf(lst_fh, "\r\n                ");
							Column=16;
						}
					}
					OutByte(TString[j]);
				}
			}
			else {
				line_error(46);
				return;
			}
			break;
		case rOFFSET:
			if (StoreSize != 4) {
				line_error(50);
				return;
			}
			/* fall thru to evaluate & store value */
		case NUMBER:
		case DOLLAR:   	/* These should all evaluate to a number. */
	  	case OPENRND:	/* If it's SYMOFF, we do a fixup or Forward Ref */
	  	case MINUS:
	  	case SYMBOL:
	  	case LSYMBOL:
	  	case UNKSYM:
			fComma=0;
			if (!(Expression()))  	/* 0 means error was emmited. */
				return;

			symtype = 0;		/* default to no symbol type */
			nExtRef = 0;		/* default to no external ref */

			if ((Token==SYMOFF) || (Token==NUMOFF))
			{
				if (ExpType == 1)				/* Local */
					symtype = lst[iExpSym].Type;
				else if (ExpType == 2) {		/* global */
					symtype = gst[iExpSym].Type;
					if (gst[iExpSym].Type & tEXTRN)
						nExtRef = iExpSym;
				}
			}

			if (!fDUP) {
				NSave = TNumber;
				TSave = Token;
		  		if (Parse() == rDUP)
		  		{
		  			nDUP = NSave;
		  			fDUP = 1;
                    Parse();
                    if (Token == OPENRND) {
						if (!(Expression()))  	/* 0 means error was emitted.*/
							return;
					}
					else
						line_error(47);
					/* TNumber now has (VALUE) from DUP */
		  		}
				else {
					ReturnToken();
					TNumber = NSave;
					Token = TSave;
				}
			}

			if (StoreSize==6)
			{
				if (fColon) {
					fColon=0;		/* reset it */
					if (fListA) {
						sword = TNumber;
						Column += 4;
						put_hexw(sword, lst_fh);
						Column += fprintf(lst_fh, " ");
						}
					OutWord(TNumber);
				}
				else {
					fExpectColon = 1;
					if (fListA) {
						Column += 8;
						put_hexd(TNumber, lst_fh);
						Column += fprintf(lst_fh, " ");
						}
					OutDWord(TNumber);
				}
			}
			else if (StoreSize==4) {
				if (fDUP)
				{
					if (fListA) {
					  if ((Token == SYMOFF) || (Token == NUMOFF))
					    Column += fprintf(lst_fh, "%08lX * (%08lXr)",nDUP,TNumber);
					  else
					    Column += fprintf(lst_fh, "%08lX * (%08lX)",nDUP,TNumber);
					}
					while (nDUP--)
					{
						if ((Token==SYMOFF) || (Token==NUMOFF))
						{
							if (fDataSeg) {		/* Data Seg */
								if (nExtRef)
									ExtRef(DSEGREF, iExpSym);
								else if (!symtype)
									ForRef(DSEGREF, oNextData);
								else if (symtype & CLABEL)
									FixUp(DCFIXTAG, oNextData, 0);
								else if (symtype & DLABEL)
									FixUp(DDFIXTAG, oNextData, 0);
							}
							else {				/* Code Seg */
								if (nExtRef)
									ExtRef(CSEGREF, iExpSym);
								if (!symtype)
									ForRef(CSEGREF, oNextCode);
								else if (symtype & CLABEL)
									FixUp(CCFIXTAG, oNextCode, 0);
								else if (symtype & DLABEL)
									FixUp(CDFIXTAG, oNextCode, 0);
							}
						}
						OutDWord(TNumber);
				    }
				}
				else {
					if (fListA)
					{
					  if ((Token == SYMOFF) || (Token == NUMOFF))
					    Column += fprintf(lst_fh, " %08lXr", TNumber);
					  else
					    Column += fprintf(lst_fh, " %08lX", TNumber);
					}

					/* Fixup & Forref here! */
					if ((Token==SYMOFF) || (Token==NUMOFF))
					{
					  if (fDataSeg)
					  {					/* Data Seg */
						if (nExtRef)
							ExtRef(DSEGREF, iExpSym);
						else if (!symtype)
							ForRef(DSEGREF, oNextData);
						else if (symtype & CLABEL)
							FixUp(DCFIXTAG, oNextData, 0);
						else if (symtype & DLABEL)
							FixUp(DDFIXTAG, oNextData, 0);
					  }
					  else
					  {					/* Code Seg */
						if (nExtRef)
							ExtRef(CSEGREF, iExpSym);
						else if (!symtype)
							ForRef(CSEGREF, oNextCode);
						else if (symtype & CLABEL)
							FixUp(CCFIXTAG, oNextCode, 0);
						else if (symtype & DLABEL)
							FixUp(CDFIXTAG, oNextCode, 0);
					  }
					}
					OutDWord(TNumber);
				}
			}
			else if (StoreSize==2) {
				if (fDUP) {
					if (fListA)
					  Column += fprintf(lst_fh, "%08lX * (%04lX) ",nDUP,TNumber);
					while (nDUP--)
						OutWord(TNumber);
					}
				else {
					if (fListA)
						Column += fprintf(lst_fh, "%04lX ", TNumber);
					OutWord(TNumber);
					}
			}
			else {
				if (fDUP) {
					if (fListA)
					  Column += fprintf(lst_fh, "%08lX * (%02lX) ",nDUP,TNumber);
					while (nDUP--)
						OutByte(TNumber);
					}
				else {
					if (fListA)
					  Column += fprintf(lst_fh, "%02lX ", TNumber);
					OutByte(TNumber);
					}
			}
			break;
		case COMMA:	/* Just eat the comma */
			if (fComma) {
				line_error(48);
				return;
			}
			fComma = 1;
			break;
		case COLON:
			fComma=0;
			if ((StoreSize == 6) && (fExpectColon)) {
				fColon = 1;
				fExpectColon = 0;
			}
			else {
				line_error(49);
				return;
			}
			break;
		case ZERO:
			if (fComma)	fMoreStorage = 1;
			return;
		default: {
			line_error(51);
			fMoreStorage = 0;
			return;
			}
	}
}
}

/********************************************
  Add new GLOBAL symbol to table (TString).
  Then parse again to hand off to
  proper function. This stores all
  Symbols UPPER case.
*********************************************/
void NewSymbol()
{
S16 i;
S8   fColon, fStorage;

	if ((pSymNext + CBString) >= (pSymBuf + SYMBUFMAX))
		fatal_error("Symbol buffer overflow...");
	if (iSymNext >= SYMSMAX)
		fatal_error("Symbol table overflow...");

	strncpy(pSymNext, TString, CBString);
	gst[iSymNext].Size = CBString;
	gst[iSymNext].Type = 0;				/* initialize type */
	gst[iSymNext].Ptr = pSymNext;
	gst[iSymNext].Line = lineno[level];
	gst[iSymNext].Offs = 0;

	/* All declarations that reach NewSymbol are either at
	level 0 or defined as PUBLIC or EXTERN. */

	if (fExtern)
		gst[iSymNext].Type |= tEXTRN;
	else
		gst[iSymNext].Type |= tPUBLIC;

	if (fFarLabel) gst[iSymNext].Type |= tFAR;

	if (fDataSeg) {
		if (!fExtern)
			gst[iSymNext].Offs = oNextData;
	    gst[iSymNext].Type |= DLABEL;
	}
	else {
		if (!fExtern)
			gst[iSymNext].Offs = oNextCode;
		gst[iSymNext].Type |= CLABEL;
	}

	/* update for next symbol */
	pSymNext += CBString;
	iSymNext++;


	/* Now, parse again and hand off. We have just added a symbol
	so expect to see an EQU, COLON or Storage statement.
	If we don't see one of these it's an error unless it's an extern.
	If we see a COLON we parse again and expect an instruction or EOL!
	*/

	fStorage = 0;
	fColon=0;
	i = Parse();

	if (i == COLON) {
		fColon = 1;
		if (fDataSeg) {
	     	line_error(49);
		    return;
		}
		i = Parse();
	}

	switch(i) { 		/* parse returns Token */
		case INSTRU:
		    if (fDataSeg) {
		     	line_error(41);
			    return;
			}
			if (!fColon) {
		     	line_error(54);
			    return;
			}
		    Instruction();
		    break;
	    case rDB:
			gst[iSymNext-1].Type |= sBYTE;		/*  type */
			fStorage = 1;
			break;
	    case rDW:
			gst[iSymNext-1].Type |= sWORD;	/*  type */
			fStorage = 1;
			break;
	    case rDD:
			gst[iSymNext-1].Type |= sDWORD;	/*  type */
			fStorage = 1;
			break;
		case rDF:
			if ((!fDataSeg) && (!fColon)) {
			     	line_error(54);
				    return;
				}
			gst[iSymNext-1].Type |= sFWORD;	/*  type */
			fStorage = 1;
			break;
		case rEQU:
	     	line_error(55);
	/*		AddMacro(); */
			break;
		case COLON:
			if (fExtern)
			return;
		case ZERO:
			if ((fDataSeg) && (!fExtern))
				line_error(56);
			break;
		default:
			if (fDataSeg)
				line_error(56);
	  }

	if (gst[iSymNext-1].Type & tEXTRN)
		fStorage = 0;

	if (fStorage)
		Storage();
}

/********************************************
  This changes an EXTRN entry in the gst to
  a PUBLIC once we find it.  This checks to
  make sure the extern declaration was the
  same type as this public.
*********************************************/

void MakePublic(void)
{
S16 i;
S8   fColon, fStorage;

	if (gst[TSymnum].Type & tPUBLIC) {
		line_error(64);
		return;
	}

	if ((fDataSeg) && (!(gst[TSymnum].Type & DLABEL))) {
		line_error(69);
		return;
	}

	if ((!fDataSeg) && (!(gst[TSymnum].Type & CLABEL))) {
		line_error(69);
		return;
	}

	gst[TSymnum].Type |= tPUBLIC;	/* Turn ON Public */
	gst[TSymnum].Type &= ~tEXTRN;	/* Turn off EXTERN */

	if (fDataSeg)
		gst[TSymnum].Offs = oNextData;
	else
		gst[TSymnum].Offs = oNextCode;

	/* Now, parse again and hand off. We have just added a symbol
	so expect to see an EQU, COLON or Storage statement.
	If we don't see one of these it's an error unless it's an extern.
	If we see a COLON we parse again and expect an instruction or EOL!
	*/

fStorage = 0;
fColon=0;
i = Parse();

if (i == COLON) {
	fColon = 1;
	if (fDataSeg) {
     	line_error(49);
	    return;
	}
	i = Parse();
}

switch(i) { 		/* parse returns Token */
	case INSTRU:
	    if (fDataSeg) {
	     	line_error(41);
		    return;
		}
		if (!fColon) {
	     	line_error(54);
		    return;
		}
	    Instruction();
	    break;
    case rDB:
		gst[TSymnum].Type |= sBYTE;		/*  type */
		fStorage = 1;
		break;
    case rDW:
		gst[TSymnum].Type |= sWORD;	/*  type */
		fStorage = 1;
		break;
    case rDD:
		gst[TSymnum].Type |= sDWORD;	/*  type */
		fStorage = 1;
		break;
	case rDF:
		if ((!fDataSeg) && (!fColon)) {
		     	line_error(54);
			    return;
			}
		gst[TSymnum].Type |= sFWORD;	/*  type */
		fStorage = 1;
		break;
	case rEQU:
     	line_error(55);
		break;
	case COLON:
		return;
	default:
		if (fDataSeg)
			line_error(56);
  }

if (fStorage)
	Storage();

}


/********************************************
  This checks to ensure multiple externs are
  the same type when we run across them, and
  also that an extern is the same type if
  the public is already declared.
*********************************************/
void CheckExtern(void)
{
	/* Check to make sure new extern and symbol are same type */

	if ((fDataSeg) && (!(gst[TSymnum].Type & DLABEL))) {
		line_error(69);
		return;
	}

	if ((!fDataSeg) && (!(gst[TSymnum].Type & CLABEL))) {
		line_error(69);
		return;
	}
}


/********************************************
  Add new LOCAL symbol to table (TString).
  Then parse again to hand off to
  proper function. This stores all
  Symbols UPPER case.
*********************************************/
void NewLSymbol(void)
{
S16 i;
S8   fColon, fStorage;

if ((pLSymNext + CBString) >= (pLSymBuf + LSYMBUFMAX))
	fatal_error("Local symbol buffer overflow...");
if (iLSymNext >= LSYMSMAX)
	fatal_error("Local symbol table overflow...");

strncpy(pLSymNext, TString, CBString);
lst[iLSymNext].Size = CBString;
lst[iLSymNext].Type = 0;				/* initialize type */
lst[iLSymNext].Ptr = pLSymNext;
lst[iLSymNext].Line = lineno[level];

if (fDataSeg) {
	lst[iLSymNext].Offs = oNextData;
	lst[iLSymNext].Type |= DLABEL;
	}
else {
	lst[iLSymNext].Offs = oNextCode;
	lst[iLSymNext].Type |= CLABEL;
	}

/* update for next symbol */
pLSymNext += CBString;
iLSymNext++;

/* Now, parse again and hand off. We have just added a symbol
so expect to see an EQU, COLON or Storage statement.
If we don't see one of these it's an error!
If we see a COLON we parse again and expect an instruction or EOL!
 */

fStorage = 0;
fColon=0;
i = Parse();

if (i == COLON) {
	fColon = 1;
	if (fDataSeg) {
     	line_error(49);
	    return;
	}
	i = Parse();
}

switch(i) { 		/* parse returns Token */
	case INSTRU:
	    if (fDataSeg) {
	     	line_error(41);
		    return;
		}
		if (!fColon) {
	     	line_error(54);
		    return;
		}
	    Instruction();
	    break;
    case rDB:
		lst[iLSymNext-1].Type |= sBYTE;		/*  type */
		fStorage = 1;
		break;
    case rDW:
		lst[iLSymNext-1].Type |= sWORD;	/*  type */
		fStorage = 1;
		break;
    case rDD:
		lst[iLSymNext-1].Type |= sDWORD;	/*  type */
		fStorage = 1;
		break;
	case rDF:
		if ((!fDataSeg) && (!fColon)) {
		     	line_error(54);
			    return;
			}
		lst[iLSymNext-1].Type |= sFWORD;	/*  type */
		fStorage = 1;
		break;
	case rEQU:
		AddMacro();
		break;
	case COLON:
		return;
	default:
		if (fDataSeg)
			line_error(56);
  }

if (lst[iLSymNext-1].Type & tEXTRN)
	fStorage = 0;

if (fStorage)
	Storage();

}

/*****************************************
 After we transition from level 1 back to
 level 0 we go back to the point in the
 code or data file where we began writing this
 section and resolve all forward code
 and local data references. These will be
 referenced in the module we just finished.
 ******************************************/

void Resolve(void)
{
int i, isym;
S32 Relative;
S32 Partial;
S32 AddFix;

	i = 0;

	while (i < iRefNext)  /* While there are forward references */
	{
		if (pfrt[i].Type == DSEGREF) 		/* Ref is in DSEG */
		{
			fseek(ds_fh, (pfrt[i].Offs - DataOffset) , SEEK_SET);
			fread(&Partial, 4, 1, ds_fh);
			fseek(ds_fh, (pfrt[i].Offs - DataOffset) , SEEK_SET);
			AddFix = pfrt[i].Offs - DataOffset;
		}
		else 	/* Ref is in CSEG */
		{
			fseek(cs_fh, (pfrt[i].Offs - CodeOffset) , SEEK_SET);

			if (pfrt[i].Type == CSEGREF) 	 /* IT'S ABSOLUTE! */
			{
				fread(&Partial, 4, 1, cs_fh);
				fseek(cs_fh, (pfrt[i].Offs - CodeOffset) , SEEK_SET);
				AddFix = pfrt[i].Offs - CodeOffset;
			}
		}
		/* We are where we should write the reference
		   now we need to find it in the local symbol table
		   and calculate the proper offset.  The calculation
		   is different for Relative and absolute references.
		   For Relative, we subtract the address where the correction
		   is going to be stored from the offset of reference.
		   For Absolute, we read what was already at the address
		   and add it to the offset of the referenced item.
		   Both of these are also adjusted for the Virtual segment offset.
		*/

		/* Look in the Local Symbol table first! */

		isym = findLsymbol(pfrt[i].Ptr, pfrt[i].NameSz);

		if (isym)		/* we found it! */
		{
			if (pfrt[i].Type == CCR8REF)		/* 8 bit relative */
			{
				Relative = lst[isym].Offs - (pfrt[i].Offs - CodeOffset + 1);
				OutByteCS(Relative);
			}
			else if (pfrt[i].Type == CCR32REF) /* 32 bit relative */
			{
				/* Fixed to make relatives ok in Virtual Segs */

				Relative = (lst[isym].Offs - CodeOffset) -
				           (pfrt[i].Offs - CodeOffset + 4);
				OutDWordCS(Relative);
			}
			else if (pfrt[i].Type == CSEGREF)  /* 32 bit absolute */
			{
				Partial += lst[isym].Offs;

				if (lst[isym].Type & CLABEL)
					FixUp(CCFIXTAG, AddFix, 0);
				else if (lst[isym].Type & DLABEL)
					FixUp(CDFIXTAG, AddFix, 0);

				OutDWordCS(Partial);
			}
			else if (pfrt[i].Type == DSEGREF)
			{
				Partial += lst[isym].Offs;			/* RAB was + DataOffset */

				if (lst[isym].Type & CLABEL)
					FixUp(DCFIXTAG, AddFix, 0);
				else if (lst[isym].Type & DLABEL)
					FixUp(DDFIXTAG, AddFix, 0);

				OutDWordDS(Partial);
			}
		}
		else 	/* Look in Global table */
		{

			isym = findGsymbol(pfrt[i].Ptr, pfrt[i].NameSz);

			if (isym)

			{		/* we found it! */


				if (pfrt[i].Type == CCR8REF) {		/* 8 bit relative */
					Relative = gst[isym].Offs - (pfrt[i].Offs - CodeOffset + 1);
					OutByteCS(Relative);
				}
				else if (pfrt[i].Type == CCR32REF) {  /*  32 bit relative */
					Relative = gst[isym].Offs - (pfrt[i].Offs - CodeOffset + 4);
					OutDWordCS(Relative);
				}
				else if (pfrt[i].Type == CSEGREF) {
					Partial += gst[isym].Offs;

					if (gst[isym].Type & CLABEL)
						FixUp(CCFIXTAG, AddFix, 0);
					else if (gst[isym].Type & DLABEL)
						FixUp(CDFIXTAG, AddFix, 0);

					OutDWordCS(Partial);
				}
				else if (pfrt[i].Type == DSEGREF)
				{
					Partial += gst[isym].Offs;

					if (gst[isym].Type & CLABEL)
						FixUp(DCFIXTAG, AddFix, 0);
					else if (gst[isym].Type & DLABEL)
						FixUp(DDFIXTAG, AddFix, 0);

					OutDWordDS(Partial);
				}
			}
			else
				prev_error("Unresolved symbol in current module", pfrt[i].Line);
		}
		i++;
	}

	fseek(cs_fh, 0L, SEEK_END);	/* Get back to the end! */
	fseek(ds_fh, 0L, SEEK_END);

}

/*****************************************
 When we have processed all the source
 we call this to resolve ALL externals.
 ******************************************/

void ResolveExt(void)
{
int i, isym;
S32 Relative;
S32 Partial;
S32 AddFix;
char name[31];

i = 0;
Partial = 0;
Relative = 0;
AddFix = 0;

while (i < iERefNext) {	/* While there are unresolved externals */

	/* See if ref is in GST as PUBLIC */

	isym = ert[i].iSym;

	if (gst[isym].Type & tPUBLIC) {

		if (ert[i].Type == DSEGREF) {					/* Ref is in DSEG */

			fseek(ds_fh, (ert[i].Offs - DataOffset) , SEEK_SET);
			fread(&Partial, 4, 1, ds_fh);
			fseek(ds_fh, (ert[i].Offs - DataOffset) , SEEK_SET);
			AddFix = ert[i].Offs - DataOffset;
		}
		else  {											/* Ref is in CSEG */

			fseek(cs_fh, (ert[i].Offs - CodeOffset) , SEEK_SET);

			if (ert[i].Type == CSEGREF) { 			 /* and IT'S ABSOLUTE! */
				fread(&Partial, 4, 1, cs_fh);
				fseek(cs_fh, (ert[i].Offs - CodeOffset) , SEEK_SET);
				AddFix = ert[i].Offs - CodeOffset;
			}
		}
			/* We are where we should write the reference
			   now we need to use the index in the external ref
			   table to see if it has been defined PUBLIC.
			   If so, we resolve it, else we error out.
			   To resolve it, the calculation is different for Relative
			   and absolute references.
			   For Relative, we subtract the address where the correction
			   is going to be stored from the offset of reference.
			   For Absolute, we read what was already at the address
			   and add it to the offset of the referenced item.
			   Both of these are also adjusted for the Virtual segment offset.
			*/

		if (ert[i].Type == CCR32REF) {  /*  32 bit relative */

			/* Fixed to make relatives ok in Virtual Segs */

			Relative = (gst[isym].Offs - CodeOffset) -
			           (ert[i].Offs - CodeOffset + 4);
			OutDWordCS(Relative);
		}
		else if (ert[i].Type == CSEGREF) {
			Partial += gst[isym].Offs;

			if (gst[isym].Type & DLABEL)
				FixUp(CDFIXTAG, AddFix, 0);
			else if (gst[isym].Type & CLABEL)
				FixUp(CCFIXTAG, AddFix, 0);

			OutDWordCS(Partial);
		}
		else if (ert[i].Type == DSEGREF) {
			Partial += gst[isym].Offs;

			if (gst[isym].Type & CLABEL)
				FixUp(DCFIXTAG, AddFix, 0);
			else if (gst[isym].Type & DLABEL)
					FixUp(DDFIXTAG, AddFix, 0);

			OutDWordDS(Partial);
		}
	}
	else {
		strncpy(name, gst[isym].Ptr, gst[isym].Size);
		name[gst[isym].Size] = '\0';
		fprintf(lst_fh, "Unresolved external: %s\n", name);
		Column = 0;
		++error_count;
	}
i++;
}	/* while more Erefs */

fseek(cs_fh, 0L, SEEK_END);	/* Get back to the end! */
fseek(ds_fh, 0L, SEEK_END);

}

/*****************************************
  If there were no errors, and all looks
  well, we build the run file. This consists
  of building the required tags in order
  and writing them to the file with the
  data.
******************************************/

void BuildRunFile(void)
{
unsigned char b;
int i;
long sdata;

	tag.id = IDTAG;						/* File type */
	tag.len = 1;
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
	fwrite(&filetype, 1, 1, run_fh);	/* Write the filetype */

	/* Version tag goes here */
	/* DatTime tag goes here */
	/* Comment tag(s) goes here */

	tag.id = SEGTAG;					/* Initial Segment Sizes */
	tag.len = 12;
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
	fwrite(&StackTotal, 4, 1, run_fh);	/* Write the Stacktotal */
	sdata = oNextCode - CodeOffset;		/* Size of CS.TMP */
	fwrite(&sdata, 4, 1, run_fh);		/* Write the Code Size */
	sdata = oNextData - DataOffset;		/* Size of DS.TMP */
	fwrite(&sdata, 4, 1, run_fh);		/* Write the Data Size */

	printf("Stack Size: %ld\r\n", StackTotal);
	printf("Code  Size: %ld\r\n", oNextCode - CodeOffset);
	printf("Data  Size: %ld\r\n", oNextData - DataOffset);

	tag.id = DOFFTAG;					/* Assumed Data Offset */
	tag.len = 4;
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
	fwrite(&DataOffset, 4, 1, run_fh);	/* Write the data */

	tag.id = COFFTAG;					/* Assumed Code Offset */
	tag.len = 4;
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
	fwrite(&CodeOffset, 4, 1, run_fh);	/* Write the data */

	tag.id = STRTTAG;					/* Starting Address */
	tag.len = 4;
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
	fwrite(&StartAddr, 4, 1, run_fh);	/* Write the data */

	tag.id = CODETAG;					/* Code Segment */
	tag.len = oNextCode - CodeOffset;	/* Size of CS.TMP */
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */

	fseek(cs_fh, 0L , SEEK_SET);
	sdata = tag.len;
	while (sdata > 0) {
		i = fread(line_buf0, 1, 200, cs_fh);
		sdata -= i;
		if (!i) break;
		fwrite(line_buf0, 1, i, run_fh);
	}

	tag.id = DATATAG;					/* Data Segment */
	tag.len = oNextData - DataOffset;	/* Size of DS.TMP */
	fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */

	fseek(ds_fh, 0L , SEEK_SET);
	sdata = tag.len;
	while (sdata > 0) {
		i = fread(line_buf0, 1, 200, ds_fh);
		sdata -= i;
		if (!i) break;
		fwrite(line_buf0, 1, i, run_fh);
	}

	if (nCDFix) {
		tag.id = CDFIXTAG;					/* CSEG Data Fixups */
		tag.len = nCDFix * 4;				/* Count * 4 of Fixups */
		fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
		i = iFUNext;
		while (i--) {						/* Write CD fixups */
			if (pfut[i].type == CDFIXTAG)
				fwrite(&pfut[i].Offs, 1, 4, run_fh);
		}
	}

	if (nCCFix) {
		tag.id = CCFIXTAG;					/* CSEG Code Fixups */
		tag.len = nCCFix * 4;				/* Count * 4 of Fixups */
		fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
		i = iFUNext;
		while (i--)	{						/* Write CC fixups */
			if (pfut[i].type == CCFIXTAG)
				fwrite(&pfut[i].Offs, 1, 4, run_fh);
		}
	}

	if (nDDFix) {
		tag.id = DDFIXTAG;					/* DESG Data Fixups */
		tag.len = nDDFix * 4;				/* Count * 4 of Fixups */
		fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
		i = iFUNext;
		while (i--)	{						/* Write DD fixups */
			if (pfut[i].type == DDFIXTAG)
				fwrite(&pfut[i].Offs, 1, 4, run_fh);
		}
	}

	if (nDCFix) {
		tag.id = DCFIXTAG;					/* DESG Code Fixups */
		tag.len = nDCFix * 4;				/* Count * 4 of Fixups */
		fwrite(&tag, 5, 1, run_fh);			/* Write the tag record */
		i = iFUNext;
		while (i--)	{						/* Write DC fixups */
			if (pfut[i].type == DCFIXTAG)
				fwrite(&pfut[i].Offs, 1, 4, run_fh);
		}
	}

	/* DLL Address fixup tag goes here */
	/* DLL Public tag goes here */

	tag.id = ENDTAG;						/* End Tag */
	tag.len = 4;	/* Size of CS.TMP */
	fwrite(&tag, 5, 1, run_fh);				/* Write the tag record */
	sdata = 0;
	fwrite(&sdata, 4, 1, run_fh);			/* Write the tag record */

}


/*****************************************
    Reads a whole line into line buffer.
******************************************/

S16 readline(void)
{
U32 i;
U8 *pLine;

 if (!(fgets(line_ptr = line_buf0, 100, src_fh[level]))) {

	if (level) {	/* we are in a nested include */
		fclose(src_fh[level]);
		--level;
		if (level==0) {		/* back up one level */

			/* Processes forward references from this module */
			Resolve();

			if (fSymDump) {
				DumpLSymbols();
				DumpFRT();
			}

			/* Clear local symbol, forward refs, & macro tables */

			iLSymNext = 1;	/* 1st entry.  */
			pLSymNext = pLSymBuf; /* beginning of buffer */

			iRefNext = 0;	/* 1st entry.  */
			pRefNext = pRefBuf;		/* Allocate memory - Was RefBuf */

			iMacNext = 0;			/* 1st entry */
			pMacNext = pMacBuf;		/* Begining of buffer */


		}
		if (fListA)
			fprintf(lst_fh, "\r\n                ");
		fprintf(lst_fh, "CONTINUING-> %s, Level:%d\r\n", srcname[level],level);
	    fprintf(lst_fh, "\r\n");

		if (lineno[level]) --lineno[level];
		fContinue = 1;
		}
	else {		/* We are at the end of the ATF file */

		/* Processes forward references from Level 0 */
		Resolve();

		if (fSymDump) {
			DumpGSymbols();
			DumpFRT();
		}

		if (!fStart)
			line_error(12);	/* Starting address not found */

		if (!error_count)
			ResolveExt();

		printf("%d Errors\r\n%d Warnings\r\n", error_count, warn_count);

		if (fListA | fListE)
			fprintf(lst_fh,"%d Errors\r\n%d Warnings\r\n",
		  			error_count,warn_count);

		if (!error_count) {
			printf("Building Run file...\n");
			BuildRunFile();
			printf("Done.\n");
		}

		fclose(lst_fh);
		fclose(cs_fh);
		fclose(ds_fh);
		fclose(lst_fh);
		fclose(src_fh[level]);
		remove("CS.TMP");
		remove("DS.TMP");
	    exit(1);
	    }
    }

 list_buf[0] = 0;
 if (fListA) {
	 pLine = line_ptr;
	 i=0;
	 while (isspace(*pLine)) pLine++;
	 while ((*pLine) && (*pLine != ';') && (*pLine != 0x0A))
		list_buf[i++] = *pLine++;
	 while ((i) && (list_buf[i-1] == ' ')) --i; /* eat trailing spaces */
	 list_buf[i] = 0;							/* null terminate */
	 if (i) fLineIn = 1;
	 else fLineIn = 0;
	 }
 ++lineno[level];
 return(1);
}


/********************************************
   Dispatch calls Parse from the beginning
   of a line and calls the proper routine
   based on what it finds (from Parse()).
   Dispatch  calls readline to get a new line
   after each of the line-specific modules
   gets done with it's line or lines(s).
*********************************************/

void Dispatch(void)
{
S16 i, j;
S8   st[80];

while (1) {
  readline();

  if (fContinue) {		/* We have just returned from an include */
  	fContinue = 0;		/* and must reread the line without processing */
  	continue;
  	}

  if (lineno[level] == 1) {
	if (fListA)
		fprintf(lst_fh, "\r\n                ");
	fprintf(lst_fh, "PROCESSING-> %s, Level:%d\r\n", srcname[level], level);
	}

  if (fListA) {
      Column = fprintf(lst_fh, "%06d ", lineno[level]);
      }

  fPublic = 0;
  fExtern = 0;
  fFarLabel = 0;

  i = Parse();
  if (fMoreStorage)
	Storage();
  else switch (i) {
    case INSTRU:
			if (fListA)
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			Instruction();
			break;
	case REGIST:
			line_error(63);
			break;
	case SYMBOL:
			line_error(64);
			break;
	case LSYMBOL:
			line_error(65);
			break;
	case STRING:
	case NUMBER:
			line_error(66);
			break;
	case rPUBLIC:
			fPublic = 1;
			if (fListA)
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			j = Parse();
			if (j==SYMBOL)
					MakePublic();
			else if (j==UNKSYM)
				NewSymbol();
			else
				line_error(67);
			break;
	case rEXTRN:
			fExtern = 1;
			if (fListA)
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			j = Parse();
			if (j==SYMBOL)
					CheckExtern();
			else if (j==UNKSYM)
				NewSymbol();
			else
				line_error(67);
			break;
	case UNKSYM:
			if (fListA)
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			if (level)
				NewLSymbol();
			else
				NewSymbol();
			break;
	case DOT:
		  	Command();
			break;
	case rDB:
	case rDD:
	case rDW:
	case rDF:
		if (fListA)
			Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
		Storage();
		break;
	case ZERO:
		break;
	default:
		line_error(68);
		break;
	}

  if ((fLineIn) && (fListA)) {
	  while (Column < 53) Column += fprintf(lst_fh, " ");
	  fprintf(lst_fh, "%s", list_buf);
      }
  if (Column)
	  fprintf(lst_fh, "\r\n");

 } /* while */
}

/*********************
* Main program
**********************/

void main(S16 argc, S8  *argv[])
{
S8   *ptr, *pname;
S16 i, j, fdone;

	lst_fh = stdout;	/* default the list file */

	for(i=1; i < argc; ++i) {
		ptr = argv[i];
		if (*ptr == '/') {
		  ptr++;
		  switch(*ptr) {
			case 'L' :			/* List file ON */
			case 'l' :
				fListA = 1;
				break;
			case 'S' :			/* Dump Symbols */
			case 's' :
				fSymDump = 1;
				break;
			case 'E' :			/* List file ON for errors/warns only */
			case 'e' :
				fListE = 1;
				break;
			case 'D' :			/* Process as DLL */
			case 'd' :
				filetype = 2;
				break;
			case 'V' :			/* Process as Devive Driver */
			case 'v' :
				filetype = 3;
				break;
			default:
				fatal_error("Invalid option\n");
				break;
		  }
		}
		else {
			if(!src_fh[0]) {
				strncpy(srcname, argv[i], 39);
				src_fh[0] = fopen(argv[i], "r");
				}
			else if(!run_fh) {
				strncpy(runname, argv[i], 39);
				if(!(run_fh = fopen(argv[i], "wb"))) {
				  fatal_error("Can't open RUN file\n");
				  }
				}
			else
				fatal_error("Too many options\n"); /* Too many parameters */
	   }
	}

/* Input file not explicitly named errors out */
	if(!src_fh[0]) {
		printf("Usage: ATFfile [RunFile] /L /E /D /V\r\n");
		printf("/L = Complete List file generated\r\n");
		printf("/S = Include SYMBOLS (only in complete list file)\r\n");
		printf("/E = List file for Errors/warnings only\r\n");
		printf("/D = Process as Dynamic link library\r\n");
		printf("/V = Process as deVice driver\r\n");
		fatal_error("Can't open Template file\n"); /* Can't open ATF file */
		}

/* Output file not explicitly named defaults to Source.RUN */

	if(!run_fh)	{			/* default asm name to SourceName.run */
		strncpy(runname, srcname, 40);
		pname=runname;
		while ((*pname != '.') && (*pname!= '\0')) pname++;
		*pname++ = '.';
        if (filetype == 2) {
		 *pname++ = 'D';
		 *pname++ = 'L';
		 *pname++ = 'L';
        }
        else if (filetype == 3) {
		 *pname++ = 'D';
		 *pname++ = 'D';
		 *pname++ = 'R';
        }
        else {
         filetype = 1;
		 *pname++ = 'R';
		 *pname++ = 'U';
		 *pname++ = 'N';
		}
		*pname   = '\0';
		if(!(run_fh = fopen(runname, "wb"))) {
			fatal_error("Can't open OUTPUT file\n");  /* Can't open RUN file */
			}
		}

/* List file named Source.LIS for fListA and fListE only. */

	if (fListA | fListE) {
		strncpy(lstname, srcname, 40);
		pname=lstname;
		while ((*pname != '.') && (*pname!= '\0')) pname++;
		*pname++ = '.';
		*pname++ = 'L';
		*pname++ = 'I';
		*pname++ = 'S';
		*pname   = '\0';
		if(!(lst_fh = fopen(lstname, "w")))
			fatal_error("Can't open list file (source.LIS)\n"); /* Can't open List file */
		}
		else
            lst_fh = stdout;

printf("DASM-DOS Ver 1.6 (c) R.A. Burgess 1992,1993,1994\r\n\r\n");

if (fListA | fListE)
	fprintf(lst_fh, "DASM-DOS Ver 1.6 (c) R.A. Burgess 1992, 1993, 1994\r\n\r\n");

if (fListA)
	fprintf(lst_fh,
    "LINE   OFFSET   ACTION/DATA/CODE                     SOURCE\r\n\r\n");

	pLSymBuf = malloc(LSYMBUFMAX);
	if(!pLSymBuf)
		fatal_error("Can't Allocate buffer 1\n");

	pSymBuf = malloc(SYMBUFMAX);
	if(!pSymBuf)
		fatal_error("Can't Allocate buffer 2\n");

	pMacBuf = malloc(MACBUFMAX);
	if(!pMacBuf)
		fatal_error("Can't Allocate buffer 3\n");

	pRefBuf = malloc(FREFBUFMAX);
	if(!pRefBuf)
		fatal_error("Can't Allocate buffer 4\n");

	pfrt = malloc(FREFTABMAX);
	if(!pfrt)
		fatal_error("Can't Allocate buffer 5\n");

	pfut = malloc(FIXUPBUFMAX);
	if(!pfut)
		fatal_error("Can't Allocate buffer 6\n");

pSymNext = pSymBuf;		/* Allocate memory - Was SymBuf */
pLSymNext = pLSymBuf;	/* Allocate memory - Was LSymBuf */
pMacNext = pMacBuf;		/* Allocate memory - Was MacBuf */
pRefNext = pRefBuf;		/* Allocate memory - Was RefBuf */


pNextAddr = &oNextData;	/* default to DataSeg */

if(!(cs_fh = fopen(csname, "wb+")))
	fatal_error("Can't open CS temp file\n");

if(!(ds_fh = fopen(dsname, "wb+")))
	fatal_error("Can't open DS temp file\n");

/* We now build a dynamic instruction lookup table which indexes
the first entry of an instruction in the opcode array rgINS.
This is done dynamically because we are still optimizing.
*/

/* i is the instruction we are indexing */
/* j is the position in rgINS */

for (i=1; i<ninst+1; i++) {
	for (j=1; j<nrgINS; j++) {
		if (rgINS[j][0] == i) {
	        rgInsLookUp[i] = j;
	        break;
		}
	}
}

Dispatch();

}
