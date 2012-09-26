/* This is the MMURTL Debugger disassembler.

  MMURTL Operating System Source Code
  Copyright 1991,1992,1993,1994 Richard A. Burgess
  ALL RIGHTS RESERVED
  Version 1.0
*/

#define U32 unsigned long
#define U16 unsigned short
#define U8 unsigned char
#define S32 signed long
#define S16 signed short
#define S8 signed char

static U8 getbyte(void);
static U8 modrm(void);
static U8 sib(void);
static int bytes(char c);
static void ohex(char c, int extend, int optional, int defsize);
static void reg_name(U8 which, char size);
static void escape(char c, char t);
static void decode(char *s);
static void do_sib(int m);
static void do_modrm(char t);
U32 disassemble(U32 Addr);

extern long xprintf(char *fmt, ...);		/* From Monitor.c */

#define SEGSIZE  32
static U8 *addrIn;

/*  The Intel 386 DX Programmer's Reference Manual provides a table that
uses the following codes to assist in disassembly of 386 code (page A-3).
The letters below are the same as the codes in the manual.  The ~ (tilde)
is an escape character to signify expansion of the codes is
required when the string is being outut to the screen.

Tilde tokens in strings:
   First char after '~':
 A - Direct address
 C - Reg of R/M picks control register
 D - Reg of R/M picks debug register
 E - R/M picks operand
 F - Flag register
 G - Reg of R/M selects a general register
 I - Immediate data
 J - Relative IP offset
 M - R/M picks memory
 O - No R/M, offset only
 R - Mod of R/M picks register only
 S - Reg of R/M picks segment register
 T - reg of R/M picks test register
 X - DS:ESI
 Y - ES:EDI
 2 - prefix of two-byte opcode
 e - put in 'e' if use32 (second char is part of reg name)
     put in 'w' for use16 or 'd' for use32 (second char is 'w')
 f - Floating point (second char is esc value)
 g - do R/M group 'n'
 p - prefix
 s - Size override (second char is a,o)

   Second char after '~':
 a - Two words in memory (BOUND)
 b - Byte
 c - Byte or word
 d - DWord
 p - 32 or 48 bit pointer
 s - Six byte pseudo-descriptor
 v - Word or DWord
 w - Word
 1-8 - group number, esc value, etc
*/

static char *opmap1[] = {
/* 0 */
  "ADD ~Eb,~Gb", "ADD ~Ev,~Gv",  "ADD ~Gb,~Eb", "ADD ~Gv,~Ev",
  "ADD AL,~Ib",  "ADD ~eAX,~Iv", "PUSH ES",     "POP ES",
  "OR ~Eb,~Gb",  "OR ~Ev,~Gv",   "OR ~Gb,~Eb",  "OR ~Gv,~Ev",
  "OR AL,~Ib",   "OR ~eAX,~Iv",  "PUSH CS",     "~2 ",
/* 1 */
  "ADC ~Eb,~Gb", "ADC ~Ev,~Gv",  "ADC ~Gb,~Eb", "ADC ~Gv,~Ev",
  "ADC AL,~Ib",  "ADC ~eAX,~Iv", "PUSH SS",     "POP SS",
  "SBB ~Eb,~Gb", "SBB ~Ev,~Gv",  "SBB ~Gb,~Eb", "SBB ~Gv,~Ev",
  "SBB AL,~Ib",  "SBB ~eAX,~Iv", "PUSH DS",     "POP DS",
/* 2 */
  "AND ~Eb,~Gb", "AND ~Ev,~Gv",  "AND ~Gb,~Eb", "AND ~Gv,~Ev",
  "AND AL,~Ib",  "AND ~eAX,~Iv", "~pE",         "DAA",
  "SUB ~Eb,~Gb", "SUB ~Ev,~Gv",  "SUB ~Gb,~Eb", "SUB ~Gv,~Ev",
  "SUB AL,~Ib",  "SUB ~eAX,~Iv", "~pC",         "DAS",
/* 3 */
  "XOR ~Eb,~Gb", "XOR ~Ev,~Gv",  "XOR ~Gb,~Eb", "XOR ~Gv,~Ev",
  "XOR AL,~Ib",  "XOR ~eAX,~Iv", "~pS",         "AAA",
  "CMP ~Eb,~Gb", "CMP ~Ev,~Gv",  "CMP ~Gb,~Eb", "CMP ~Gv,~Ev",
  "CMP AL,~Ib",  "CMP ~eAX,~Iv", "~pD",         "AAS",
/* 4 */
  "INC ~eAX",    "INC ~eCX",     "INC ~eDX",    "INC ~eBX",
  "INC ~eSP",    "INC ~eBP",     "INC ~eSI",    "INC ~eDI",
  "DEC ~eAX",    "DEC ~eCX",     "DEC ~eDX",    "DEC ~eBX",
  "DEC ~eSP",    "DEC ~eBP",     "DEC ~eSI",    "DEC ~eDI",
/* 5 */
  "PUSH ~eAX",   "PUSH ~eCX",    "PUSH ~eDX",   "PUSH ~eBX",
  "PUSH ~eSP",   "PUSH ~eBP",    "PUSH ~eSI",   "PUSH ~eDI",
  "POP ~eAX",    "POP ~eCX",     "POP ~eDX",    "POP ~eBX",
  "POP ~eSP",    "POP ~eBP",     "POP ~eSI",    "POP ~eDI",
/* 6 */
  "PUSHA",       "POPA",         "BOUND ~Gv,~Ma", "ARPL ~Ew,~Rw",
  "~pF",         "~pG",          "~so",           "~sa",
  "PUSH ~Iv",    "IMUL ~Gv=~Ev*~Iv", "PUSH ~Ib",  "IMUL ~Gv=~Ev*~Ib",
  "INSB ~Yb,DX", "INS~ew ~Yv,DX", "OUTSB DX,~Xb", "OUTS~ew DX,~Xv",
/* 7 */
  "JO ~Jb",      "JNO ~Jb",       "JNC ~Jb",      "JC ~Jb",
  "JZ ~Jb",      "JNZ ~Jb",       "JBE ~Jb",      "JNBE ~Jb",
  "JS ~Jb",      "JNS ~Jb",       "JPE ~Jb",      "JPO ~Jb",
  "JL ~Jb",      "JGE ~Jb",       "JLE ~Jb",      "JG ~Jb",
/* 8 */
  "~g1 ~Eb,~Ib",  "~g1 ~Ev,~Iv",  "MOV AL,~Ib",   "~g1 ~Ev,~Ib",
  "TEST ~Eb,~Gb", "TEST ~Ev,~Gv", "XCHG ~Eb,~Gb", "XCHG ~Ev,~Gv",
  "MOV ~Eb,~Gb",  "MOV ~Ev,~Gv",  "MOV ~Gb,~Eb",  "MOV ~Gv,~Ev",
  "MOV ~Ew,~Sw",  "LEA ~Gv,~M ",  "MOV ~Sw,~Ew",  "POP ~Ev",
/* 9 */
  "NOP",            "XCHG ~eAX,~eCX", "XCHG ~eAX,~eDX", "XCHG ~eAX,~eBX",
  "XCHG ~eAX,~eSP", "XCHG ~eAX,~eBP", "XCHG ~eAX,~eSI", "XCHG ~eAX,~eDI",
  "CBW",            "CDW",            "CALL ~Ap",       "FWAIT",
  "PUSH ~eflags",   "POP ~eflags",    "SAHF",           "LAHF",
/* a */
  "MOV AL,~Ov",     "MOV ~eAX,~Ov",     "MOV ~Ov,al",    "MOV ~Ov,~eAX",
  "MOVSB ~Xb,~Yb",  "MOVS~ew ~Xv,~Yv",  "CMPSB ~Xb,~Yb", "CMPS~ew ~Xv,~Yv",
  "TEST AL,~Ib",    "TEST ~eAX,~Iv",    "STOSB ~Yb,AL",  "STOS~ew ~Yv,~eAX",
  "LODSB AL,~Xb",   "LODS~ew ~eAX,~Xv", "SCASB AL,~Xb",  "SCAS~ew ~eAX,~Xv",
/* b */
  "MOV AL,~Ib",   "MOV CL,~Ib",   "MOV DL,~Ib",   "MOV BL,~Ib",
  "MOV AH,~Ib",   "MOV CH,~Ib",   "MOV DH,~Ib",   "MOV BH,~Ib",
  "MOV ~eAX,~Iv", "MOV ~eCX,~Iv", "MOV ~eDX,~Iv", "MOV ~eBX,~Iv",
  "MOV ~eSP,~Iv", "MOV ~eBP,~Iv", "MOV ~eSI,~Iv", "MOV ~eDI,~Iv",
/* c */
  "~g2 ~Eb,~Ib",   "~g2 ~Ev,~Ib",  "RET ~Iw",      "RET",
  "LES ~Gv,~Mp",   "LDS ~Gv,~Mp",  "MOV ~Eb,~Ib",  "MOV ~Ev,~Iv",
  "ENTER ~Iw,~Ib", "LEAVE",        "RETF ~Iw",     "retf",
  "INT 3",         "INT ~Ib",      "INTO",         "IRET",
/* d */
  "~g2 ~Eb,1", "~g2 ~Ev,1", "~g2 ~Eb,cl", "~g2 ~Ev,cl",
  "AAM", "AAD", 0, "XLAT",

/*
  "ESC 0,~Ib", "ESC 1,~Ib", "ESC 2,~Ib", "ESC 3,~Ib",
  "ESC 4,~Ib", "ESC 5,~Ib", "ESC 6,~Ib", "ESC 7,~Ib",
*/

  "~f0", "~f1", "~f2", "~f3",
  "~f4", "~f5", "~f6", "~f7",


/* e */
  "LOOPNE ~Jb", "LOOPE ~Jb", "LOOP ~Jb", "JCXZ ~Jb",
  "IN AL,~Ib", "IN ~eAX,~Ib", "OUT ~Ib,AL", "OUT ~Ib,~eAX",
  "CALL ~Jv", "JMP ~Jv", "JMP ~Ap", "JMP ~Jb",
  "IN AL,DX", "IN ~eAX,DX", "OUT DX,AL", "OUT DX,~eAX",
/* f */
  "LOCK~p ", 0, "REPNE~p ", "REP(e)~p ",
  "HLT", "CMC", "~g3", "~g0",
  "CLC", "STC", "CLI", "STI",
  "CLD", "STD", "~g4", "~g5"
  };

char *SecOp00[] = {
/* 0 */
  "~g6", "~g7", "LAR ~Gv,~Ew", "LSL ~Gv,~Ew", 0, 0, "CLTS", 0,
  0, 0, 0, 0, 0, 0, 0, 0 };

static char *SecOp20[] = {
/* 2 */
  "MOV ~Rd,~Cd", "MOV ~Rd,~Dd", "MOV ~Cd,~Rd", "MOV ~Dd,~Rd",
  "MOV ~Rd,~Td", 0, "MOV ~Td,~Rd", 0,
  0, 0, 0, 0, 0, 0, 0, 0}

static char *SecOp80[] = {
  "JO ~Jv", "JNO ~Jv", "JC ~Jv", "JNC ~Jv",
  "JZ ~Jv", "JNZ ~Jv", "JBE ~Jv", "JNBE ~Jv",
  "JS ~Jv", "JNS ~Jv", "JPE ~Jv", "JPO ~Jv",
  "JL ~Jv", "JGE ~Jv", "JLE ~Jv", "JG ~Jv",
/* 9 */
  "SETO ~Eb", "SETNO ~Eb", "SETNC ~Eb", "SETC ~Eb",
  "SETZ ~Eb", "SETNZ ~Eb", "SETBE ~Eb", "SETNBE ~Eb",
  "SETS ~Eb", "SETNS ~Eb", "SETP ~Eb", "SETNP ~Eb",
  "SETL ~Eb", "SETGE ~Eb", "SETLE ~Eb", "SETG ~Eb",
/* a */
  "PUSH FS",          "POP FS",          0,          "BT ~Ev,~Gv",
  "SHLD ~Ev,~Gv,~Ib", "SHLD ~Ev,~Gv,cl", 0,           0,
  "PUSH GS",          "POP GS",          0,          "BTS ~Ev,~Gv",
  "SHRD ~Ev,~Gv,~Ib", "SHRD ~Ev,~Gv,cl", 0,          "IMUL ~Gv,~Ev",
/* b */
  0, 0, "LSS ~Mp", "BTR ~Ev,~Gv",
  "LFS ~Mp", "LGS ~Mp", "MOVZX ~Gv,~Eb", "MOVZX ~Gv,~Ew",
  0, 0, "~g8 ~Ev,~Ib", "BTC ~Ev,~Gv",
  "BSF ~Gv,~Ev", "BSR~Gv,~Ev", "MOVSX ~Gv,~Eb", "MOVSX ~Gv,~Ew",
  };
/* NOTE: Second byte of 2 byte OpCodes are Invalid if over 0xBF */


static char *groups[9][8] = {   /* group 0 is group 3 for ~Ev set */
  { "TEST ~Ev,~Iv", "TEST ~Ev,~Iv,", "NOT ~Ev", "NEG ~Ev",
    "MUL ~eAX,~Ev", "IMUL ~eAX,~Ev", "DIV ~eAX,~Ev", "IDIV ~eAX,~Ev" },
  { "ADD", "OR", "ADC", "SBB", "AND", "SUB", "XOR", "CMP" },
  { "ROL", "ROR", "RCL", "RCR", "SHL", "SHR", "SHL", "SAR" },
  { "TEST ~Eb,~Ib", "TEST ~Eb,~Ib,", "NOT ~Eb", "NEG ~Eb",
    "MUL AL,~Eb", "IMUL AL,~Eb", "DIV AL,~Eb", "IDIV AL,~Eb" },
  { "INC ~Eb", "DEC ~Eb", 0, 0, 0, 0, 0, 0 },
  { "INC ~Ev", "DEC ~Ev", "CALL ~Ev", "CALL ~Ep",
    "JMP ~Ev", "JMP ~Ep", "PUSH ~Ev", 0 },
  { "SLDT ~Ew", "STR ~Ew", "LLDT ~Ew", "LTR ~Ew",
    "VERR ~Ew", "VERW ~Ew", 0, 0 },
  { "SGDT ~Ms", "SIDT ~Ms", "LGDT ~Ms", "LIDT ~Ms",
    "SMSW ~Ew", 0, "LMSW ~Ew", 0 },
  { 0, 0, 0, 0, "BT", "BTS", "BTR", "BTC" }
  };

	/* for display */
static char *seg_names[]= {"ES","CS","SS","DS","FS","GS"};
static char *breg_names[]={"AL","CL","DL","BL","AH","CH","DH","BH" };
static char *wreg_names[]={"AX","CX","DX","BX","SP","BP","SI","DI" };
static char *dreg_names[]={"EAX","ECX","EDX","EBX","ESP","EBP","ESI","EDI" };

static S16 prefix;
static U8 modrmv;
static S8 fmodrmv;
static U8 sibv;
static S8 fsibv;
static S16 opsize;

/*****************************************************
Gets a byte to disassemble and update addrIn.
******************************************************/

static U8 getbyte(void)
{
U8 b;
;
#asm
	MOV EAX, _addrIn
	MOV AL, CS:[EAX]
	MOV [EBP-1], AL
#endasm

 ++addrIn;
 return b;
}


/*************************************************/
/* Get Mod/RM field byte for current instruction */

static U8 modrm(void)
{
  if (!fmodrmv) {
    modrmv = getbyte();
    fmodrmv = 1;
    }
  return modrmv;
}


/*************************************************/
/* Get 'scale-index-base' byte for current instruction */

static U8 sib(void)
{
  if (!fsibv) {
    sibv = getbyte();
    fsibv = 1;
    }
  return sibv;
}

/**********************************************************/
/* The register is encode as bit 3,4,5 in the byte.
   xxRRRxxx
   This macro extracts it.  Used in several places.
*/

#define reg(a)	(((a)>>3)&7)

/*------------------------------------------------------------------------*/

/*------------------------------------------------------------------------*/
/* Determines how many bytes left in the instruction from the
  letter in the table (which is passed in here).
*/

static int bytes(char c)
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

static void ohex(char c, int extend, int optional, int defsize)
{
int n, s, i;
unsigned char buf[6];

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
	xprintf("%02x%02x",buf[n-1],buf[n-2]);
    n -= 2;
  }

  if (extend > n)
  {
    if (!optional)
	xprintf("+");
	n = 4;
  }
  switch (n)
  {
    case 1: {
			xprintf("%02x",buf[0]);
    		break;
    		}
    case 2:	{
			xprintf("%02x%02x",buf[1],buf[0]);
    		break;
    		}
    case 4:	{
			xprintf("%02x%02x%02x%02x",buf[3],buf[2],buf[1],buf[0]);
    		break;
    		}
  }
}

/*------------------------------------------------------------------------*/

static void reg_name(U8 which, char size)
{
  if (size == 'F')
  {
    xprintf( "st(%d)",which);

    return;
  }
  if (((size == 'v') && (opsize == 32)) || (size == 'd'))
  {
    xprintf("E");
  }
  if (size == 'b')
  {
    xprintf( "%s", breg_names[which]);
  }
  else
  {
    xprintf( "%s", wreg_names[which]);
  }
}

/******************************************************************
   This takes in two chars that represent part of the op code and
   puts out the proper text to match what the letter represents.
   c is the first char after the tilde and t is next one. See
   opcode1[] strings for what the chars mean.
*******************************************************************/

static void escape(char c, char t)
{
  S32  delta, vals;
  U8 b2;
  S8 valsb;
  S16 valsw;

  switch (c)
  {
    case 'A':                             /* Direct Address */
	    ohex(t, 4, 0, 32);
	    break;
    case 'C':                             /* Reg of R/M picks control reg */
		xprintf("CR%d",reg(modrm()));
	    break;
    case 'D':                             /* Reg of R/M pick debug reg */
		xprintf("DR%d",modrm());
		break;
    case 'E':                             /* R/M picks operand */
		do_modrm(t);
		break;
    case 'G':                             /* Reg of R/M picks general reg */
		if (t == 'F')
        	reg_name((modrm()&7), t);
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
          valsb = getbyte();		/* must remian signed! */
          vals = valsb;
          break;
        case 2:
          valsb = getbyte();        /*RAB  Made SIGNEd bytes/Words */
          valsw = getbyte()<<8;
          vals = valsw + valsb;
          break;
        case 4:
          vals = getbyte();
          vals |= getbyte() << 8;
          vals |= getbyte() << 16;
          vals |= getbyte() << 24;
          break;
		}
		delta = addrIn + vals;
	    xprintf( "%x",delta);
		break;
    case 'M':                             /* R/M picks memory */
		do_modrm(t);
		break;
	case 'O':                             /* NO R/M, Offset only */
		decode("~p:[");
		ohex(t, 4, 0, 32);
		xprintf("]");
		break;
    case 'R':                             /* Mod of R/M pick REG only */
		do_modrm(t);
		break;
    case 'S':                             /* Reg of R/M picks seg reg */
	    xprintf( "%s", seg_names[reg(modrm())]);
		break;
    case 'T':                             /* Reg of R/M picks test reg */
		xprintf( "TR%d",modrm());
		break;
    case 'X':                             /* DS:ESI */
		xprintf("DS:[ESI]");
		break;
	case 'Y':                             /* ES:EDI */
		xprintf("ES:[EDI]");
		break;
    case '2':                             /* Prefix of 2 byte opcode */
		b2 = getbyte();
		if (b2 < 0x10)
			decode(SecOp00[b2]);
		else if ((b2 > 0x1F) && (b2 < 0x30))
			decode(SecOp20[b2-0x20]);
		else if ((b2 > 0x7F) && (b2 < 0xC0))
			decode(SecOp80[b2-0x80]);
		else
		    xprintf("<bogus>");
		break;
	case 'e':                 /*  t is part of reg name */
		if (opsize == 32)
		{
			if (t == 'w')     /* put out "d" if t is "w" on 32 bit opsize */
		  		xprintf("D");
			else
			{
			  xprintf("E");  /* put out "E" if not t <> "w" then put t */
			  xprintf("%c",t);
			}
		}
		else {
			  xprintf("%c",t);
		}
		break;
    case 'f':                /* floating point */
	    xprintf("<Float Op>");

/*		floating_point(t-'0');  */

		break;
    case 'g':                             /* do R/M group 'n' */
		decode(groups[t-'0'][reg(modrm())]);
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
          decode(opmap1[getbyte()]);
          break;
 		case ':':
          if (prefix) {
          	xprintf("%cS:",prefix);
          }
          break;
    	case ' ':
          decode(opmap1[getbyte()]);
          break;
		}
		break;
    case 's':								/* Size override */
		if (t=='o') {						/* o is operand */
		  opsize = 48 - opsize;
		  decode(opmap1[getbyte()]);
  	    }
	    break;
  }
}


/******************************************
This expands and outputs the instruction
string passed in if it finds the escape
character (tilde).
******************************************/

static void decode(char *s)
{
  char c;

  if (s == 0)                   /* if NULL pointer, then it's BAD */
  {
    xprintf("<invalid>");
  }
  while ((c = *s++) != 0)       /* put next char in c */
  {
    if (c == '~')               /* if c is ~ then ESCAPE */
    {
      c = *s++;                 /* get letter representing value */
      escape(c, *s++);
    }
    else
      if (c == ' ') 	         /* space */
		xprintf(" ");
	  else {
		xprintf("%c",c);         /* else put out the char found! */
		}
  }
}


/* outputs 'scale-index-base' instructions */

static void do_sib(int m)
{
  int s, i, b;
  s = ((sib()) >> 6) & 7;		/* SSxxxxxx Scale */
  i = ((sib()) >> 3) & 7;		/* xxIIIxxx Index */
  b = sib() & 7;				/* xxxxxBBB Base  */
  switch (b)
  {
    case 0: decode("~p:[EAX"); break;
    case 1: decode("~p:[ECX"); break;
    case 2: decode("~p:[EDX"); break;
    case 3: decode("~p:[EBX"); break;
    case 4: decode("~p:[ESP"); break;
    case 5:
      if (m == 0)
      {
        decode("~p:[");
        ohex('d', 4, 0, 32);
      }
      else
        decode("~p:[EBP");
      break;
    case 6: decode("~p:[ESI"); break;
    case 7: decode("~p:[EDI"); break;
  }
  switch (i)
  {
    case 0: xprintf("+EAX"); break;
    case 1: xprintf("+ECX"); break;
    case 2: xprintf("+EDX"); break;
    case 3: xprintf("+EBX"); break;
    case 4: break;
    case 5: xprintf("+EBP"); break;
    case 6: xprintf("+ESI"); break;
    case 7: xprintf("+EDI"); break;
  }
  if (i != 4)
    switch (s)
    {
      case 0: break;
      case 1: xprintf("*2"); break;
      case 2: xprintf("*4"); break;
      case 3: xprintf("*8"); break;

    }
}
/*------------------------------------------------------------------------*/
static void do_modrm(char t)
{
  int m;
  int r;

  m = ((modrm()) >> 6) & 7;
  r = modrm() & 7;

  if (m == 3)
  {
    reg_name(r, t);
    return;
  }
  if ((m == 0) && (r == 5))
  {
    decode("~p:[");
    ohex('d', 4, 0, 32);
    xprintf("]");
    return;
  }

  if (r != 4)
    decode("~p:[");

  switch (r)
    {
      case 0: xprintf("EAX"); break;
      case 1: xprintf("ECX"); break;
      case 2: xprintf("EDX"); break;
      case 3: xprintf("EBX"); break;
      case 4: do_sib(m); break;
      case 5: xprintf("EBP"); break;
      case 6: xprintf("ESI"); break;
      case 7: xprintf("EDI"); break;
  }
  switch (m)
  {
    case 1:
      ohex('b', 4, 0, 32);
      break;
    case 2:
      xprintf("+");
      ohex('v', 4, 0, 32);
      break;
  }
  xprintf("]");
}

/***********************************************
  This disassembles one instruction each time it
  is called.
************************************************/

U32 disassemble(U32 Addr)
{
  prefix = 0;
  fmodrmv = 0;
  fsibv = 0;
  opsize = SEGSIZE;	 /* default operand size is DWORD */
  addrIn = Addr;

  xprintf( "%08x   ", addrIn);

  decode(opmap1[getbyte()]);	/* decode instruction and output */

  xprintf( "\r\n");
  return addrIn;
}
