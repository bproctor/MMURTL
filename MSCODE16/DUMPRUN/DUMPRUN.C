/* Dumprun.c  Dumps the contents of a MMURTL Run file (DLL or Device Driver*/


#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

#define TRUE 1
#define FALSE 1

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "runfile.h"

/* disassembler data */

#include "uproto.h"

int seg_size = 32;		/* set to 32 for /3 option */
int fRaw = 0;			/* dump raw byte too? */
int nRaw;
U8 RawBuf[10];

S32  addrIn;
U32 fEOF = 0;

/*  The Intel 386 Software Writers Guide provides a table that uses the
following codes to assist in manual disassembly of 386 code. The
letters below are the same as the codes in the manual.  The % (vertical
bar is an escape character - like in c - to signify expansion of the codes is
required when the string is being outut.
*/

char *opmap1[] = {
/* 0 */
  "ADD %Eb,%Gb", "ADD %Ev,%Gv",  "ADD %Gb,%Eb", "ADD %Gv,%Ev",
  "ADD AL,%Ib",  "ADD %eAX,%Iv", "PUSH ES",     "POP ES",
  "OR %Eb,%Gb",  "OR %Ev,%Gv",   "OR %Gb,%Eb",  "OR %Gv,%Ev",
  "OR AL,%Ib",   "OR %eAX,%Iv",  "PUSH CS",     "%2 ",
/* 1 */
  "ADC %Eb,%Gb", "ADC %Ev,%Gv",  "ADC %Gb,%Eb", "ADC %Gv,%Ev",
  "ADC AL,%Ib",  "ADC %eAX,%Iv", "PUSH SS",     "POP SS",
  "SBB %Eb,%Gb", "SBB %Ev,%Gv",  "SBB %Gb,%Eb", "SBB %Gv,%Ev",
  "SBB AL,%Ib",  "SBB %eAX,%Iv", "PUSH DS",     "POP DS",
/* 2 */
  "AND %Eb,%Gb", "AND %Ev,%Gv",  "AND %Gb,%Eb", "AND %Gv,%Ev",
  "AND AL,%Ib",  "AND %eAX,%Iv", "%pE",         "DAA",
  "SUB %Eb,%Gb", "SUB %Ev,%Gv",  "SUB %Gb,%Eb", "SUB %Gv,%Ev",
  "SUB AL,%Ib",  "SUB %eAX,%Iv", "%pC",         "DAS",
/* 3 */
  "XOR %Eb,%Gb", "XOR %Ev,%Gv",  "XOR %Gb,%Eb", "XOR %Gv,%Ev",
  "XOR AL,%Ib",  "XOR %eAX,%Iv", "%pS",         "AAA",
  "CMP %Eb,%Gb", "CMP %Ev,%Gv",  "CMP %Gb,%Eb", "CMP %Gv,%Ev",
  "CMP AL,%Ib",  "CMP %eAX,%Iv", "%pD",         "AAS",
/* 4 */
  "INC %eAX",    "INC %eCX",     "INC %eDX",    "INC %eBX",
  "INC %eSP",    "INC %eBP",     "INC %eSI",    "INC %eDI",
  "DEC %eAX",    "DEC %eCX",     "DEC %eDX",    "DEC %eBX",
  "DEC %eSP",    "DEC %eBP",     "DEC %eSI",    "DEC %eDI",
/* 5 */
  "PUSH %eAX",   "PUSH %eCX",    "PUSH %eDX",   "PUSH %eBX",
  "PUSH %eSP",   "PUSH %eBP",    "PUSH %eSI",   "PUSH %eDI",
  "POP %eAX",    "POP %eCX",     "POP %eDX",    "POP %eBX",
  "POP %eSP",    "POP %eBP",     "POP %eSI",    "POP %eDI",
/* 6 */
  "PUSHA",       "POPA",         "BOUND %Gv,%Ma", "ARPL %Ew,%Rw",
  "%pF",         "%pG",          "%so",           "%sa",
  "PUSH %Iv",    "IMUL %Gv=%Ev*%Iv", "PUSH %Ib",  "IMUL %Gv=%Ev*%Ib",
  "INSB %Yb,DX", "INS%ew %Yv,DX", "OUTSB DX,%Xb", "OUTS%ew DX,%Xv",
/* 7 */
  "JO %Jb",      "JNO %Jb",       "JC %Jb",       "JNC %Jb",
  "JZ %Jb",      "JNZ %Jb",       "JBE %Jb",      "JNBE %Jb",
  "JS %Jb",      "JNS %Jb",       "JPE %Jb",      "JPO %Jb",
  "JL %Jb",      "JGE %Jb",       "JLE %Jb",      "JG %Jb",
/* 8 */
  "%g1 %Eb,%Ib",  "%g1 %Ev,%Iv",  "MOV AL,%Ib",   "%g1 %Ev,%Ib",
  "TEST %Eb,%Gb", "TEST %Ev,%Gv", "XCHG %Eb,%Gb", "XCHG %Ev,%Gv",
  "MOV %Eb,%Gb",  "MOV %Ev,%Gv",  "MOV %Gb,%Eb",  "MOV %Gv,%Ev",
  "MOV %Ew,%Sw",  "LEA %Gv,%M ",  "MOV %Sw,%Ew",  "POP %Ev",
/* 9 */
  "NOP",            "XCHG %eAX,%eCX", "XCHG %eAX,%eDX", "XCHG %eAX,%eBX",
  "XCHG %eAX,%eSP", "XCHG %eAX,%eBP", "XCHG %eAX,%eSI", "XCHG %eAX,%eDI",
  "CBW",            "CDW",            "CALL %Ap",       "FWAIT",
  "PUSH %eflags",   "POP %eflags",    "SAHF",           "LAHF",
/* a */
  "MOV AL,%Ov",     "MOV %eAX,%Ov",     "MOV %Ov,al",    "MOV %Ov,%eAX",
  "MOVSB %Xb,%Yb",  "MOVS%ew %Xv,%Yv",  "CMPSB %Xb,%Yb", "CMPS%ew %Xv,%Yv",
  "TEST AL,%Ib",    "TEST %eAX,%Iv",    "STOSB %Yb,AL",  "STOS%ew %Yv,%eAX",
  "LODSB AL,%Xb",   "LODS%ew %eAX,%Xv", "SCASB AL,%Xb",  "SCAS%ew %eAX,%Xv",
/* b */
  "MOV AL,%Ib",   "MOV CL,%Ib",   "MOV DL,%Ib",   "MOV BL,%Ib",
  "MOV AH,%Ib",   "MOV CH,%Ib",   "MOV DH,%Ib",   "MOV BH,%Ib",
  "MOV %eAX,%Iv", "MOV %eCX,%Iv", "MOV %eDX,%Iv", "MOV %eBX,%Iv",
  "MOV %eSP,%Iv", "MOV %eBP,%Iv", "MOV %eSI,%Iv", "MOV %eDI,%Iv",
/* c */
  "%g2 %Eb,%Ib",   "%g2 %Ev,%Ib",  "RET %Iw",      "RET",
  "LES %Gv,%Mp",   "LDS %Gv,%Mp",  "MOV %Eb,%Ib",  "MOV %Ev,%Iv",
  "ENTER %Iw,%Ib", "LEAVE",        "RETF %Iw",     "RETF",
  "INT 3",         "INT %Ib",      "INTO",         "IRET",
/* d */
  "%g2 %Eb,1", "%g2 %Ev,1", "%g2 %Eb,cl", "%g2 %Ev,cl",
  "AAM", "AAD", 0, "XLAT",

/*
  "ESC 0,%Ib", "ESC 1,%Ib", "ESC 2,%Ib", "ESC 3,%Ib",
  "ESC 4,%Ib", "ESC 5,%Ib", "ESC 6,%Ib", "ESC 7,%Ib",
*/

  "%f0", "%f1", "%f2", "%f3",
  "%f4", "%f5", "%f6", "%f7",


/* e */
  "LOOPNE %Jb", "LOOPE %Jb", "LOOP %Jb", "JCXZ %Jb",
  "IN AL,%Ib", "IN %eAX,%Ib", "OUT %Ib,AL", "OUT %Ib,%eAX",
  "CALL %Jv", "JMP %Jv", "JMP %Ap", "JMP %Jb",
  "IN AL,DX", "IN %eAX,DX", "OUT DX,AL", "OUT DX,%eAX",
/* f */
  "LOCK%p ", 0, "REPNE%p ", "REP(e)%p ",
  "HLT", "CMC", "%g3", "%g0",
  "CLC", "STC", "CLI", "STI",
  "CLD", "STD", "%g4", "%g5"
  };

char *SecOp00[] = {
/* 0 */
  "%g6", "%g7", "LAR %Gv,%Ew", "LSL %Gv,%Ew", 0, 0, "CLTS", 0,
  0, 0, 0, 0, 0, 0, 0, 0 };

char *SecOp20[] = {
/* 2 */
  "MOV %Rd,%Cd", "MOV %Rd,%Dd", "MOV %Cd,%Rd", "MOV %Dd,%Rd",
  "MOV %Rd,%Td", 0, "MOV %Td,%Rd", 0,
  0, 0, 0, 0, 0, 0, 0, 0};

char *SecOp80[] = {
  "JO %Jv", "JNO %Jv", "JC %Jv",  "JNC %Jv",		/*RAB  Rev JNC /JC */
  "JZ %Jv", "JNZ %Jv", "JBE %Jv", "JNBE %Jv",
  "JS %Jv", "JNS %Jv", "JPE %Jv", "JPO %Jv",
  "JL %Jv", "JGE %Jv", "JLE %Jv", "JG %Jv",
/* 9 */
  "SETO %Eb", "SETNO %Eb", "SETNC %Eb", "SETC %Eb",
  "SETZ %Eb", "SETNZ %Eb", "SETBE %Eb", "SETNBE %Eb",
  "SETS %Eb", "SETNS %Eb", "SETP %Eb", "SETNP %Eb",
  "SETL %Eb", "SETGE %Eb", "SETLE %Eb", "SETG %Eb",
/* a */
  "PUSH FS",          "POP FS",          0,          "BT %Ev,%Gv",
  "SHLD %Ev,%Gv,%Ib", "SHLD %Ev,%Gv,cl", 0,           0,
  "PUSH GS",          "POP GS",          0,          "BTS %Ev,%Gv",
  "SHRD %Ev,%Gv,%Ib", "SHRD %Ev,%Gv,cl", 0,          "IMUL %Gv,%Ev",

/* b */
  0,             0,            "LSS %Mp",       "BTR %Ev,%Gv",
  "LFS %Mp",    "LGS %Mp",     "MOVZX %Gv,%Eb", "MOVZX %Gv,%Ew",
  0,             0,            "%g8 %Ev,%Ib",   "BTC %Ev,%Gv",
  "BSF %Gv,%Ev", "BSR%Gv,%Ev", "MOVSX %Gv,%Eb", "MOVSX %Gv,%Ew",
  };

/* NOTE: Second byte of 2 byte OpCodes are Invalid if Over 0xBF */


char *groups[9][8] = {   /* group 0 is group 3 for %Ev set */
  { "TEST %Ev,%Iv", "TEST %Ev,%Iv,", "NOT %Ev", "NEG %Ev",
    "MUL %eAX,%Ev", "IMUL %eAX,%Ev", "DIV %eAX,%Ev", "IDIV %eAX,%Ev" },
  { "ADD", "OR", "ADC", "SBB", "AND", "SUB", "XOR", "CMP" },
  { "ROL", "ROR", "RCL", "RCR", "SHL", "SHR", "SHL", "SAR" },
  { "TEST %Eb,%Ib", "TEST %Eb,%Ib,", "NOT %Eb", "NEG %Eb",
    "MUL AL,%Eb", "IMUL AL,%Eb", "DIV AL,%Eb", "IDIV AL,%Eb" },
  { "INC %Eb", "DEC %Eb", 0, 0, 0, 0, 0, 0 },
  { "INC %Ev", "DEC %Ev", "CALL %Ev", "CALL %Ep",
    "JMP %Ev", "JMP %Ep", "PUSH %Ev", 0 },
  { "SLDT %Ew", "STR %Ew", "LLDT %Ew", "LTR %Ew",
    "VERR %Ew", "VERW %Ew", 0, 0 },
  { "SGDT %Ms", "SIDT %Ms", "LGDT %Ms", "LIDT %Ms",
    "SMSW %Ew", 0, "LMSW %Ew", 0 },
  { 0, 0, 0, 0, "BT", "BTS", "BTR", "BTC" }
  };

	/* for display */
char *seg_names[]= {"ES","CS","SS","DS","FS","GS"};
char *breg_names[]={"AL","CL","DL","BL","AH","CH","DH","BH" };
char *wreg_names[]={"AX","CX","DX","BX","SP","BP","SI","DI" };
char *dreg_names[]={"EAX","ECX","EDX","EBX","ESP","EBP","ESI","EDI" };

S16 prefix;
U8 modrmv;
S8 fmodrmv;
U8 sibv;
S8 fsibv;
S16 opsize;
S16 addrsize;


/* Run file data */

char *pCode, *pData, *pStack;	/* Ptrs in User mem to load to */
long sCode,   sData,  sStack;	/* Size of segments */
unsigned long oCode, oData;		/* Offset in file to Code & Data */
unsigned long offCode, offData;	/* Virtual Offset for Code & Data Segs */
unsigned long nCDFIX, oCDFIX,
			  nCCFIX, oCCFIX,
			  nDDFIX, oDDFIX,
			  nDCFIX, oDCFIX;

char *pStart;

struct tagtype tag;

FILE *run_fh   = 0;		/* Current .RUN, .DLL, or .DDR (output) */
FILE *out_fh   = 0;		/* Output file */

char runname[40];
char outname[40];

char fUASM = 0;
char fDumpData = 0;
char fOutFile = 0;

/*********************************************
  Dumps all bytes pointed to to stream with
  offset printed in front of each line.
*/

void Dump(long cb)
{
U32 i, val, addr;
U16 j;
unsigned char buff[17];
unsigned char line[78]; /* Addr xx xx xx xx... ASCII */

	addr = 0;

	while (cb) {
		if (cb > 15)
			j=16;
		else
			j = cb;

		sprintf(line, "%08lX   ", addr);	/* print current offset */

		fread (buff, 1, j, run_fh);		/* Read j bytes from stream */

		for (i=0; i<j; i++) {
			val = buff[i];
			sprintf(&line[11 + (i*3)], "%02lX ", val);
			if (buff[i] < 0x20)
				buff[i] = 0x2E;
			if (buff[i] > 0x7F)
				buff[i] = 0x2E;
			line[i+61] = buff[i];	/* printed char */
		}
		line[59] = 0x20;
		line[60] = 0x20;
		line[i+61] = 0;

		fprintf(out_fh, "%s\r\n", line);	/* put in chars */

		addr += j;
		cb -=j;
	}
	return;
}



/*****************************************************
Gets a byte to disassemble saving the raw bytes that
make it up in RawBuf[nRaw].
******************************************************/

U8 getbyte(void)
{
S32 i;
U8 b;

i=fgetc(run_fh);
if (i==EOF) fEOF = 1;
b = i & 0xff;
if (fRaw)
	RawBuf[nRaw++] = b;
addrIn++;
return b;
}


/*************************************************/
/* Get Mod/RM field byte for current instruction */

U8 modrm(void)
{
  if (!fmodrmv) {
    modrmv = getbyte();
    fmodrmv = 1;
    }
  return modrmv;
}


/*************************************************/
/* Get 'scale-index-base' byte for current instruction */

U8 sib(void)
{
  if (!fsibv) {
    sibv = getbyte();
    fsibv = 1;
    }
  return sibv;
}

/**********************************************************/
/* Macros to shift or mask a byte so bit type data such as
   reg or mod can be pulled out of certain instruction
   bytes.
*/

#define mod(a)	(((a)>>6)&7)
#define reg(a)	(((a)>>3)&7)
#define rm(a)	((a)&7)
#define ss(a)	(((a)>>6)&7)
#define indx(a)	(((a)>>3)&7)
#define base(a)	((a)&7)

/*------------------------------------------------------------------------*/

/**************************************
* Write a decimal number to a file fout
***************************************/

void put_num(U32 value, FILE *fout)
{
S8   stack[10];
register U16 i;

	if(value & 0x80000000) {
		fputc('-', fout);
		value = -value; }
	i = 0;
	do
		stack[i++] = (value % 10) + '0';
	while(value /= 10);
	while(i) {
		fputc(stack[--i], fout);
		}
}

/**************************************
  Write a Hex byte to a file fout.
***************************************/

void put_hexb(U8 value, FILE *fout)
{
S8  stack[10];
U16 i, j;

	i = 0; j = 2;
	do {
		stack[i] = (value % 16) + '0';
		if (stack[i] > 0x39)
            stack[i] += 7;
		i++;
		}
	while(value /= 16);
	while (i < j--) {			/* zero pad front end to width */
		fputc('0', fout);
		}
	while(i) {
		fputc(stack[--i], fout);
		}
}

/**************************************
  Write a Hex number to a file fout.
***************************************/

void put_hexw(U16 value, FILE *fout)
{
S8  stack[10];
U16 i, j;

	i = 0;
	j = 4;
	do {
		stack[i] = (value % 16) + '0';
		if (stack[i] > 0x39)
            stack[i] += 7;
		i++;
		}
	while(value /= 16);
	while (i < j--) {			/* zero pad front end to width */
		fputc('0', fout);
		}
	while(i) {
		fputc(stack[--i], fout);
		}
}

/**************************************
  Write a hex dword to a file fout.
***************************************/

void put_hexd(U32 value, FILE *fout)
{
S8  stack[10];
U16 i, j;

	i = 0;
	j = 8;
	do {
		stack[i] = (value % 16) + '0';
		if (stack[i] > 0x39)
            stack[i] += 7;
		i++;
		}
	while(value /= 16);
	while (i < j--) {			/* zero pad front end to width */
		fputc('0', fout);
		}
	while(i) {
		fputc(stack[--i], fout);
		}
}

/*------------------------------------------------------------------------*/
/* determines how many bytes left in the instruction from the
  letter in the table (which is passed in here).  "v" is a variable
  number based on current segment size of code descriptor.
*/

int bytes(char c)
{
  switch (c)
  {
    case 'b':
      return 1;
    case 'w':
      return 2;
    case 'd':
      return 4;
    case 'v':
      if (opsize == 32)
        return 4;
      else
        return 2;
  }
  return 0;
}

/**************************************************************
Get the correct number of bytes for immediate data from the
code stream and output it as hex.
***************************************************************/

void ohex(char c, int extend, int optional, int defsize)
{
int n, s, i, j;
S32 delta;
unsigned char buf[6];
char *name;

  n=0;
  s=0;

  switch (c)
  {
    case 'a':
      break;
    case 'b':	/* byte */
      n = 1;
      break;
    case 'w':	/* word */
      n = 2;
      break;
    case 'd':	/* dword */
      n = 4;
      break;
    case 's':	/* fword */
      n = 6;
      break;
    case 'c':
    case 'v':
      if (defsize == 32)
        n = 4;
      else
        n = 2;
      break;
    case 'p':	/* 32 or 48 bit pointer */
      if (defsize == 32)
        n = 6;
      else
        n = 4;
      s = 1;
      break;
  }

  for (i=0; i<n; i++)
    buf[i] = getbyte();

  /* sign extend the value into a U32 */

  for (; i<extend; i++)
    buf[i] = (buf[i-1] & 0x80) ? 0xff : 0;

  if (s)		/* outputs the segment value of FAR pointer */
  {

	fprintf(out_fh,"%02X%02X",buf[n-1],buf[n-2]);
/*
    put_hexb(buf[n-1], out_fh);
    put_hexb(buf[n-2], out_fh);
*/
    n -= 2;
  }

  if (extend > n)
  {
    if (!optional)
	fprintf(out_fh,"+");
	n = 4;
  }
  switch (n)
  {
    case 1: {
			fprintf(out_fh,"%02X",buf[0]);
/*    		put_hexb(buf[0], out_fh); */
    		break;
    		}
    case 2:	{
			fprintf(out_fh,"%02X%02X",buf[1],buf[0]);
/*    		put_hexb(buf[1], out_fh);
    		put_hexb(buf[0], out_fh);
*/
    		break;
    		}
    case 4:	{
			fprintf(out_fh,"%02X%02X%02X%02X",buf[3],buf[2],buf[1],buf[0]);
/*
    		put_hexb(buf[3], out_fh);
    		put_hexb(buf[2], out_fh);
    		put_hexb(buf[1], out_fh);
    		put_hexb(buf[0], out_fh);
*/
    		break;
    		}
  }
}

/*------------------------------------------------------------------------*/

void reg_name(U8 which, char size)
{
  if (size == 'F')
  {
    fprintf(out_fh, "st(%d)",which);

    return;
  }
  if (((size == 'v') && (opsize == 32)) || (size == 'd'))
  {
    fprintf(out_fh,"E");
  }
  if (size == 'b')
  {
    fprintf(out_fh, "%s", breg_names[which]);
  }
  else
  {
    fprintf(out_fh, "%s", wreg_names[which]);
  }
}

/******************************************************************
   This takes in a two chars that represent part of the op code and
   puts out the proper text to match what the letter represents.
   c is the first char after the dollar sign and t is next one. See
   opcode1[] strings for what the chars mean.
*******************************************************************/

void escape(char c, char t)
{
  U32 v;
  S32  delta, vofs, tmp;
  S8 vofsb;
  S16 vofsw;
  char *name;
  int extend;
  U8 b2, w;

  extend = (addrsize == 32) ? 4 : 2;

  switch (c)
  {
    case 'A':                             /* Direct Address */
	    ohex(t, extend, 0, addrsize);
	    break;
    case 'C':                             /* Reg of R/M picks control reg */
		fprintf(out_fh, "CR%d",reg(modrm()));
	    break;
    case 'D':                             /* Reg of R/M pick debug reg */
		fprintf(out_fh, "DR%d",modrm());
		break;
    case 'E':                             /* R/M picks operand */
		do_modrm(t);
		break;
    case 'G':                             /* Reg of R/M picks general reg */
		if (t == 'F')
        	reg_name(rm(modrm()), t);
		else
		reg_name(reg(modrm()), t);
		break;
    case 'I':                             /* Immediate data */
		ohex(t, 0, 0, opsize);
		break;
    case 'J':                             /* Relative IP offset */
		switch (bytes(t))
		{
        case 1:
          vofsb = getbyte();		/* must remian signed! */
          vofs = vofsb;
          break;
        case 2:
          vofsb = getbyte();        /*Must be Signed bytes/Words */
          vofsw = getbyte()<<8;
          vofs = vofsw + vofsb;
          break;
        case 4:
          vofs = getbyte();
          tmp = getbyte();
          vofs |= tmp << 8;
          tmp = getbyte();
          vofs |= tmp << 16;
          tmp = getbyte();
          vofs |= tmp << 24;
          break;
		}
		delta = addrIn + vofs;
	    fprintf(out_fh, "%08lX", delta);
		break;
    case 'M':                             /* R/M picks memory */
		do_modrm(t);
		break;
	case 'O':                             /* NO R/M, Offset only */
		expand_out("%p:[");
		ohex(t, extend, 0, addrsize);
		fprintf(out_fh,"]");
		break;
    case 'R':                             /* Mod of R/M pick REG only */
		do_modrm(t);
		break;
    case 'S':                             /* Reg of R/M picks seg reg */
	    fprintf(out_fh, "%s", seg_names[reg(modrm())]);
		break;
    case 'T':                             /* Reg of R/M picks test reg */
		fprintf(out_fh, "TR%d",modrm());
		break;
    case 'X':                             /* DS:ESI */
		fprintf(out_fh,"DS:[");
		if (addrsize == 32)
      		fprintf(out_fh,"E");
		fprintf(out_fh,"SI]");
		break;
	case 'Y':                             /* ES:EDI */
		fprintf(out_fh,"ES:[");
		if (addrsize == 32)
 			fprintf(out_fh,"E");
		fprintf(out_fh,"DI]");
		break;
    case '2':                             /* Prefix of 2 byte opcode */
		b2 = getbyte();
		if (b2 < 0x10)
			expand_out(SecOp00[b2]);
		else if ((b2 > 0x1F) && (b2 < 0x30))
			expand_out(SecOp20[b2-0x20]);
		else if ((b2 > 0x7F) && (b2 < 0xC0))
			expand_out(SecOp80[b2-0x80]);
		else
		    fprintf(out_fh, "<invalid>");
		break;
	case 'e':                 /* If "USE32" t is part of reg name */
		if (opsize == 32)
		{
			if (t == 'w')     /* put out "d" if t is "w" on USE32 segs*/
		  		fprintf(out_fh,"D");
			else
			{
			  fprintf(out_fh,"E");  /* put out "E" if not t <> "w" then put t */
			  fputc(t, out_fh);
			}
		}
		else {
			fputc(t, out_fh);    /* when USE16 just put out esc char */
		}
		break;
    case 'f':                /* floating point */
	    fprintf(out_fh,"<Float Op>");

/*		floating_point(t-'0');  */

		break;
    case 'g':                             /* do R/M group 'n' */
		expand_out(groups[t-'0'][reg(modrm())]);
		break;
    case 'p':                             /* Segment prefix */
		switch (t)
		{
		case 'C':                         /* CS */
		case 'D':                         /* DS */
		case 'E':                         /* ES */
		case 'F':                         /* FS */
		case 'G':                         /* GS */
		case 'S':                         /* SS */
		  prefix = t;
          expand_out(opmap1[getbyte()]);
          break;
 		case ':':
          if (prefix) {
          	fputc(prefix, out_fh);
          	fprintf(out_fh,"S:");
          }
          break;
    	case ' ':
          expand_out(opmap1[getbyte()]);
          break;
		}
		break;
    case 's':								/* Size override */
		switch (t)
  	    {
    	case 'a':
			addrsize = 48 - addrsize;		/* a is address */
			expand_out(opmap1[getbyte()]);
	  		break;
		case 'o':							/* o is operand */
		  opsize = 48 - opsize;
		  expand_out(opmap1[getbyte()]);
		  break;
  	    }
	    break;
  }
}


/******************************************
This expands and outputs the instruction
string passed in if it find the escape
character (vertical bar).
******************************************/

void expand_out(char *s)
{
  int i;
  char c;

  if (s == 0)                   /* if NULL pointer, then it's BAD */
  {
    fprintf(out_fh,"<invalid>");
  }
  while ((c = *s++) != 0)       /* put next char in c */
  {
    if (c == '%')               /* if c is % then ESCAPE */
    {
      c = *s++;                 /* get letter representing value */
      escape(c, *s++);
    }
    else
      if (c == ' ')             /* if space, put TAB in string */
		fprintf(out_fh," ");
      else {
		fputc(c, out_fh);            /* else put out the char found! */
		}
  }
}


/* outputs 'scale-index-base' instructions */

void do_sib(int m)
{
  int s, i, b;
  s = ss(sib());
  i = indx(sib());
  b = base(sib());
  switch (b)
  {
    case 0: expand_out("%p:[EAX"); break;
    case 1: expand_out("%p:[ECX"); break;
    case 2: expand_out("%p:[EDX"); break;
    case 3: expand_out("%p:[EBX"); break;
    case 4: expand_out("%p:[ESP"); break;
    case 5:
      if (m == 0)
      {
        expand_out("%p:[");
        ohex('d', 4, 0, addrsize);
      }
      else
        expand_out("%p:[EBP");
      break;
    case 6: expand_out("%p:[ESI"); break;
    case 7: expand_out("%p:[EDI"); break;
  }
  switch (i)
  {
    case 0: fprintf(out_fh,"+EAX"); break;
    case 1: fprintf(out_fh,"+ECX"); break;
    case 2: fprintf(out_fh,"+EDX"); break;
    case 3: fprintf(out_fh,"+EBX"); break;
    case 4: break;
    case 5: fprintf(out_fh,"+EBP"); break;
    case 6: fprintf(out_fh,"+ESI"); break;
    case 7: fprintf(out_fh,"+EDI"); break;
  }
  if (i != 4)
    switch (s)
    {
      case 0: break;
      case 1: fprintf(out_fh,"*2"); break;
      case 2: fprintf(out_fh,"*4"); break;
      case 3: fprintf(out_fh,"*8"); break;

    }
}
/*------------------------------------------------------------------------*/
void do_modrm(char t)
{
  int m;
  int r;
  int extend;

  m =  mod(modrm());
  r = rm(modrm());
  extend = (addrsize == 32) ? 4 : 2;

  if (m == 3)
  {
    reg_name(r, t);
    return;
  }
  if ((m == 0) && (r == 5) && (addrsize == 32))
  {
    expand_out("%p:[");
    ohex('d', extend, 0, addrsize);
    fprintf(out_fh,"]");
    return;
  }
  if ((m == 0) && (r == 6) && (addrsize == 16))
  {
    expand_out("%p:[");
    ohex('w', extend, 0, addrsize);
    fprintf(out_fh,"]");
    return;
  }
  if ((addrsize != 32) || (r != 4))
    expand_out("%p:[");
  if (addrsize == 16)
  {
    switch (r)
    {
      case 0: fprintf(out_fh,"BX+SI"); break;
      case 1: fprintf(out_fh,"BX+DI"); break;
      case 2: fprintf(out_fh,"BP+SI"); break;
      case 3: fprintf(out_fh,"BP+DI"); break;
      case 4: fprintf(out_fh,"SI"); break;
      case 5: fprintf(out_fh,"DI"); break;
      case 6: fprintf(out_fh,"BP"); break;
      case 7: fprintf(out_fh,"BX"); break;
    }
  }
  else
  {
    switch (r)
    {
      case 0: fprintf(out_fh,"EAX"); break;
      case 1: fprintf(out_fh,"ECX"); break;
      case 2: fprintf(out_fh,"EDX"); break;
      case 3: fprintf(out_fh,"EBX"); break;
      case 4: do_sib(m); break;
      case 5: fprintf(out_fh,"EBP"); break;
      case 6: fprintf(out_fh,"ESI"); break;
      case 7: fprintf(out_fh,"EDI"); break;
    }
  }
  switch (m)
  {
    case 1:
      ohex('b', extend, 0, addrsize);  /* was 1 */
      break;
    case 2:
      fprintf(out_fh,"+");
      ohex('v', extend, 0, addrsize);  /* was 1 */
      break;
  }
  fprintf(out_fh,"]");
}

/***********************************************
  This disassembles one instruction each time it
  is called.
************************************************/

U32 disassemble(void)
{
  int    i;
  char   *cmp, *brp;
  U8     *wp;
  U32    delta;
  char   *name, *lname;

  prefix = 0;
  fmodrmv = 0;
  fsibv = 0;
  nRaw = 0;
  opsize = addrsize = seg_size;

  fprintf(out_fh, "%08lX   ", addrIn);

  expand_out(opmap1[getbyte()]);	/* decode instruction and output */

  if (fRaw) {
     i = 0;
	 fprintf(out_fh, "            ");
	 while (nRaw--) {
	  	put_hexb(RawBuf[i++], out_fh);
		fprintf(out_fh, " ");
	 }
  }

  fprintf(out_fh, "\r\n");
  return addrIn;
}


/************************************************
* Write part of an instruction to the file "fi"
*************************************************/

void put_str(char *ptr, FILE *fi)
{
	while(*ptr)
		fputc(*ptr++, fi);
}

/*******************************************
* Report a non-recoverable error
********************************************/

void fatal_error(char *string)
{
	put_str(string, stderr);
	put_str("Disassembly aborted\r\n", stderr);

	exit(-1);
}

/*********************************************************
  This reads and dumps a RUN file with complete tag field
  descriptions and data.
*********************************************************/

void DumpRunFile(FILE *fh)
{
int ii;
long i, dret, nPages, nobj;
char fDone, filetype, junk;

	addrIn = 0; 	/* Default Address */
	offCode = 0;    /* Default virtual code */
	fDone = 0;
	while (!fDone) {
		tag.id = 0;
		fprintf(out_fh, "\r\nFile LFA: %ld\r\n", ftell(fh));
		nobj = fread (&tag, 1, 5, fh);
		if (nobj) {
			ii = tag.id;
			fprintf(out_fh, "Tage Value  (hex): %02X\r\n", ii);
			fprintf(out_fh, "Tage Length (dec): %ld\r\n", tag.len);
		}
		else
			fDone = TRUE;
		switch (tag.id) {
			case IDTAG:
				nobj = fread (&filetype, 1, 1, fh);
				if (filetype == 1)
					fprintf(out_fh, "File type: STANDARD RUN FILE\r\n");
				else if (filetype == 2)
					fprintf(out_fh, "File type: DYNAMIC LINK LIBRAY\r\n");
				else if (filetype == 3)
					fprintf(out_fh, "File type: DEVICE DRIVER\r\n");
				else {
					fprintf(out_fh, "File type: UNKNOWN (ID=%d)\r\n",filetype);
					fDone = 1;
				}
				break;
			case SEGTAG:
				fprintf(out_fh, "Segment Sizes:\r\n");
				nobj = fread (&sStack, 1, 4, fh);
				if (nobj) nobj = fread (&sCode, 1, 4, fh);
				if (nobj) nobj = fread (&sData, 1, 4, fh);
				fprintf(out_fh, "  Stack   %ld\r\n", sStack);
				fprintf(out_fh, "  Code    %ld\r\n", sCode);
				fprintf(out_fh, "  Data    %ld\r\n", sData);
				break;
			case DOFFTAG:
				nobj = fread (&offData, 1, 4, fh);
				fprintf(out_fh, "Virtual Data Offset: %08lXh\r\n", offData);
				break;
			case COFFTAG:
				nobj = fread (&offCode, 1, 4, fh);
				fprintf(out_fh, "Virtual Code Offset: %08lXh\r\n", offCode);
				addrIn = offCode; 	/* Virtual Address */
				break;
			case STRTTAG:
				nobj = fread (&pStart, 1, 4, fh);
				fprintf(out_fh, "Start Address: %08lXh\r\n", pStart);
				break;
			case CODETAG:
				fprintf(out_fh, "Code Segment Contents...\r\n");
				if (fUASM) {
					while ((disassemble() - offCode)  < tag.len);
				 }
				 else
					while (tag.len--)
						nobj = fread (&junk, 1, 1, fh);
				break;
			case DATATAG:
				fprintf(out_fh, "Data Segment Contents...\r\n");
				if (fDumpData) {
					Dump(tag.len);
				}
				else {
					while (tag.len--)
						nobj = fread (&junk, 1, 1, fh);
				}
				break;
			case CDFIXTAG:
				fprintf(out_fh, "CD Fixups - Refs to Data at CSeg offsets:\r\n");
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, fh);
					tag.len-=4;
					fprintf(out_fh, "  Offset- %08lXh\r\n", i);
				}
				break;
			case CCFIXTAG:
				fprintf(out_fh, "CC Fixups - Refs to Code at CSeg offsets:\r\n");
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, fh);
					tag.len-=4;
					fprintf(out_fh, "  Offset- %08lXh\r\n", i);
				}
				break;
			case DDFIXTAG:
				fprintf(out_fh, "DD Fixups - Refs to Data at DSeg offsets:\r\n");
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, fh);
					tag.len-=4;
					fprintf(out_fh, "  Offset- %08lXh\r\n", i);
				}
				break;
			case DCFIXTAG:
				fprintf(out_fh, "DD Fixups - Refs to Code at DSeg offsets:\r\n");
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, fh);
					tag.len-=4;
					fprintf(out_fh, "  Offset- %08lXh\r\n", i);
				}
				break;
			case ENDTAG:
				nobj = fread (&i, 1, 4, fh);
				fprintf(out_fh, "END Tag Value: %ld\r\n", i);
				fDone = TRUE;
				break;
			default:
				while ((tag.len--) && (nobj))
					nobj = fread (&junk, 1, 1, fh);
				break;
		}
	}
}

/***************************
* Main program DUMPRUNFILE
****************************/

void main(S16 argc, S8   *argv[])
{
S8   *ptr, *pname;
S16 i, j, fdone;

	out_fh = 0;		/* default the list file */

	for(i=1; i < argc; ++i) {
		ptr = argv[i];
		if (*ptr == '/') {
		  ptr++;
		  switch(*ptr) {
			case 'U' :			/* UnAssemble Code */
			case 'u' :
				fUASM = 1;
				break;
			case 'D' :			/* Dump Symbols */
			case 'd' :
				fDumpData = 1;
				break;
			case 'O' :			/* Output Hex code with Unassembly */
			case 'o' :
				fRaw = 1;
				break;
			default:
				printf("Invalid option/swtich \n");
				if (run_fh)
					fclose(run_fh);
				exit(1);
				break;
		  }
		}
		else {
			if(!run_fh) {
				strncpy(runname, argv[i], 39);
				run_fh = fopen(argv[i], "rb");
			}
			else if (!out_fh) {
				strncpy(outname, argv[i], 39);
				if(!(out_fh = fopen(argv[i], "w"))) {
				  printf("Can't open OUTPUT file\n");
				  exit(1);
				}
				fOutFile = 1;
			}
			else {
				printf("Too many options\n"); /* Too many parameters */
				exit(1);
			}
	    }
	}

/* Input file not explicitly named errors out */

	if (!run_fh) {
		printf("Usage: DumpRun  runfile  outputfile /U /D \r\n");
		printf("/U = Unassemble Code segment\r\n");
		printf("/O = Output code in hex with Unassembly (must use /U)\r\n");
		printf("/D = Dump data segment contents in hex format\r\n");
		printf("If outputfile isn't specifiled output is sent to screenr\r\n");
		printf("Can't open RUNFILE.\n"); /* Can't open file */
		exit(1);
	}

/* Output file not explicitly named is set to stdout */

	if (!out_fh)
		out_fh = stdout;


if (fOutFile) {
	printf("DumpRunFile Ver x1.0 (c) R.A. Burgess 1993, 1994\r\n");
	printf("Dump being sent to file: %s \r\n\r\n", outname);
}

fprintf(out_fh, "DumpRunFile Ver x1.0 (c) R.A. Burgess 1993, 1994\r\n\r\n");

DumpRunFile(run_fh);
if (run_fh)
	fclose(run_fh);
if (fOutFile)
	fclose(out_fh);
exit(0);

}

/*********************** End of Module *****************/
