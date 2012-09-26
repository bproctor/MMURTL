/* DASMQ.c contains procedures that are stable and are not
  subject to change during debugging and development. They
  have been moved out of the main module to reduce SCROLL Mania...

*/

/******************************************
* Determine if a character is alphabetic
* or underscore. These can be used for first
* or subsequent chars in an identifier.
*******************************************/

char is_ident(char chr)
{
	return (isalpha(chr)) || (chr == '_');
}


/********************************************
  Determine if character is "skip" character.
  All values from 0x20 down to CTRL-A are
  skipped.
*********************************************/

char isskip(char chr)
{
	if ((chr > 0) && (chr <= ' ')) return(1);
	else return(0);
}
/**************************************
  Write a Hex byte to a file fout.
***************************************/

void put_hexb(U8 value, FILE *fout)
{
S8  stack[10];
U32 i, j;

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

void put_hexw(U32 value, FILE *fout)
{
S8  stack[10];
U32 i, j;

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
U32 i, j;

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


/*************************************
  Acquires VALUE for Expression()
*************************************/
long expr4(long *val)
{
long k;

if (Token == rOFFSET) {
	fOffset = 1;
	Parse();
	}

if (Token == OPENRND) {			/* Open paren */
	Parse();
	k=expr1(val);
	if (Token == CLOSRND) {
		Parse();
		}
	else{
		line_error(1);
		return(0);
		}
	}
else if (Token == NUMBER) {
	*val = TNumber;
	Parse();				/* set up for next token check */
	}
else if (Token == LSYMBOL) {
	nExpSyms++;
	*val = lst[TSymnum].Offs;
	iExpSym = TSymnum;		/* so we can trace symbol number came from */
	ExpType = 1;
	Parse();				/* set up for next token check */
	}
else if (Token == SYMBOL) {
	nExpSyms++;
	*val = gst[TSymnum].Offs;
	iExpSym = TSymnum;		/* so we can trace symbol number came from */
	ExpType = 2;
	Parse();				/* set up for next token check */
	}
else if (Token == UNKSYM) {
	nExpSyms++;
	*val = 0;				/* No offset for UNKSYM */
	iExpSym =0;
	ExpType = 0;			/* 0 means forward ref. No Assumption. */

	/* Save name for forward reference */
	strncpy(UString, TString, 30);
	if (CBString > 30)
		UCBString = 30;
	else
		UCBString = CBString;			/* Size of Unknown label */
	UString[UCBString] = '\0';			/* Null terminate */

	Parse();				/* set up for next token check */
	}
else if (Token == DOLLAR) {
	if (fDataSeg)
		*val = oNextData;
	else
		*val = oNextCode;
	ExpType = 3;
	Parse();				/* set up for next token check */
	}
else{
	line_error(2);
	return(0);
	}
return(1);
}

/*****************************************
 Evaluates a UNARY MINUS for Expression()
*****************************************/
long expr3(long *val)
{
long k;

if (Token == MINUS) {		/* Unary MINUS */
	Parse();				/* set up for next token check */
	k=expr4(val);
	if (k) {
		*val = -(*val);
		return(1);
		}
	else {
		line_error(3);
		return(0);
		}
	}
else {
	k = expr4(val);
	return(k);
	}
}


/*************************************
 Evaluates a * and / Expression()
*************************************/
long expr2(long *val)
{
long k;
long val2;

k=expr3(val);
if ((Token != STAR) && (Token != SLASH))
	return(k);
while(1) {
	if (Token == STAR) {		/* MULTIPLY */
		Parse();				/* set up for next token check */
		if(expr2(&val2))
			*val *= val2;
		}
	else if (Token == SLASH) {	/* DIVIDE */
		Parse();				/* set up for next token check */
		if(expr2(&val2))
			*val /= val2;
		}
	else return(1);  /* Expr doesn't continue but it's OK to here */
}
}

/*************************************
  Evaluates a + and - for Expression()
*************************************/
long expr1(long *val)
{
long k;
long val2;

k=expr2(val);
if ((Token != PLUS) && (Token != MINUS)) return(k);
while(1) {
	if (Token == PLUS) {
		Parse();
		if (Token==REGIST) return(1);	/* allow for N+REG */
		if(expr2(&val2))
			*val += val2;
		else return(0);
		}
	else if (Token == MINUS) {
		Parse();
		if (Token==REGIST) return(1);	/* allow for N+REG */
		if(expr2(&val2))
			*val -= val2;
		else return(0);
		}
	else return(1);  /* Expr doesn't continue but it's OK to here */
}
}

/*******************************************
  Expression is called to evaluate a math
  expression made of constants, and/or
  address labels (offsets).
  Uses functions expr1 - expr4 recursively
  to handle parenthetical expressions
  while keeping standard math operator
  precedence correct. Exits with (0) if error
  in expression is encountered. Exits with
  1 if value is left in TNumber.  If a NON
  math Token is encountered but expression
  was OK to that point, it calls ReturnToken
  and leaves the currenly evaluated number
  as the current token.

  If a single symbol or a single "current address"
  opeator ($) is used in the expression, it will
  return with SYMOFF as the Token type with the
  offset value in TNumber, and the symbol
  in TSymnum. If the OFFSET operator is used,
  it will return with the offset in TNumber
  and it will be a NUMOFF.
  If more than 1 symbol or the
  current address operator ($) and a symbol
  is used it will assume a simple numeric
  value from the computation and return
  NUMBER as it's token.
********************************************/

long Expression(void)
{
long val;

/* iExpSym = 0; */
nExpSyms = 0;
fOffset = 0;

if (expr1(&val)) {

    if (Token)			/* if not EOL (EOL returns 0) */
		ReturnToken();	/*   give back non-expression token */
	if (nExpSyms == 1) {
		if (fOffset)
			Token = NUMOFF; /* Derived from OFFSET cmd */
		else
			Token = SYMOFF;	/* single address operator used (Mem Type) */
	}
	else
		Token = NUMBER;	/* Make current token a NUMBER */

	TSymnum = iExpSym;	/* We can tell where number came from */
	TNumber = val;		/* Tell em what it is */
	return(1);			/* Tell em they got one */
}
else return(0);			/* Tell em they got an error */
}

/*****************************************************
   Determines if token is 32 bit general register.
*****************************************************/
long is_r32(long id)
{
switch (id) {
  	case rEAX:
  	case rEBX:
  	case rECX:
  	case rEDX:
  	case rESI:
  	case rEDI:
  	case rEBP:
  	case rESP: return(1);
	default:   return(0);
  }
}

/*****************************************************
   Determines if token is 16 bit general.
*****************************************************/
long is_r16(long id)
{
switch (id) {
  	case rAX:
  	case rBX:
  	case rCX:
  	case rDX:
  	case rSI:
  	case rDI:
  	case rBP:
  	case rSP: return(1);
	default:  return(0);
  }
}

/*****************************************************
   Determines if token is 8 bit general register.
*****************************************************/
long is_r8(long id)
{
switch (id) {
  	case rAL:
  	case rBL:
  	case rCL:
  	case rDL:
  	case rAH:
  	case rBH:
  	case rCH:
  	case rDH: return(1);
	default:  return(0);
  }
}

/*****************************************************
   Determines if token is Segment Register.
*****************************************************/
long is_rSEG(long id)
{
switch (id) {
  	case rDS:
  	case rES:
  	case rCS:
  	case rFS:
  	case rGS:
  	case rSS: return(1);
	default:   return(0);
  }
}

/*****************************************************
   Determines if token is Debug register.
*****************************************************/
long is_rDRG(long id)
{
switch (id) {
  	case rDR0:
  	case rDR1:
  	case rDR2:
  	case rDR3:
  	case rDR6:
  	case rDR7: return(1);
	default:   return(0);
  }
}

/*****************************************************
   Determines if token is Control register.
*****************************************************/
long is_rCRG(long id)
{
return ((id == rCR0) || (id == rCR2) || (id == rCR3));
}

/*****************************************************
   Determines if token is Test register.
*****************************************************/
long is_rTRG(long id)
{
return ((id == rTR6) || (id == rTR7));
}

/*****************************************************
   Determines if operand entry in instruction (ins) is
   compatible with the operand type the user gave (op).
   An entry of 0 may match which means there is no
   operand.
*****************************************************/


long is_Comp(long ins, long op)
{
switch (rgINS[ins][op+1]) { 	/* check against op type in instruction */
    case 0:
    	if (!rgOpType[op]) return(1);	/* no operand in either matches */
		break;
    case rel8:
    	if (rgOpType[op] == rel8) return(1);
    	break;
    case relW:
    	if (rgOpType[op] == relW) return(1);
    	break;
    case iSAD:
    	if (rgOpType[op] == r8) return(1);
    	break;
    case r8:
    	if (rgOpType[op] == r8) return(1);
    	break;
    case r16:
    	if (rgOpType[op] == r16) return(1);
    	break;
    case r32:
    	if (rgOpType[op] == r32) return(1);
    	break;
    case rREG:
    	if ((rgOpType[op] == r8) ||
            (rgOpType[op] == r16) ||
            (rgOpType[op] == r32)) return(1);
    	break;
    case rRGW:
    	if ((rgOpType[op] == r16) ||
            (rgOpType[op] == r32)) return(1);
    	break;
    case rACC:
    	if (((rgOpType[op] == r8) && (rgOpReg[op] == rAL)) ||
            ((rgOpType[op] == r16) && (rgOpReg[op] == rAX)) ||
            ((rgOpType[op] == r32) && (rgOpReg[op] == rEAX)))
        return(1);
    	break;
    case rSEG:
    	if (rgOpType[op] == rSEG) return(1);
    	break;
    case rCRG:
    	if (rgOpType[op] == rCRG) return(1);
    	break;
    case rDRG:
    	if (rgOpType[op] == rDRG) return(1);
    	break;
    case rTRG:
    	if (rgOpType[op] == rTRG) return(1);
    	break;
    case imm8:
    	if (rgOpType[op] == val8)
    	    return(1);

    	if ((rgOpType[op] == val16) &&
    	    (OpImm < 256) &&
    	    (OpImm >= 0))
    	    return(1);

    	break;
    case ims8:	/* This is where a byte in an instruction
                will be sign extended. We will only allow byte values
			    between -128 and 127 to be compatible here. This is because
			    they can put AND EAX, A0h and we have to assume they DON'T
			    want it extended into a negative 32 bit integer such as 0FFFFFFA0h.
			*/
    	if (rgOpType[op] == val8)
    	    return(1);

    	if ((rgOpType[op] == val16) &&
    	    (OpImm < 128) &&
    	    (OpImm >= 0))
    	    return(1);

    	break;
    case imm16:
    	if ((rgOpType[op] == val8) ||
    	    (rgOpType[op] == val16)) return(1);
    	break;
    case immX:
    	if ((rgOpType[op] == val8) ||
    	    (rgOpType[op] == val16) ||
    	    (rgOpType[op] == val32)) return(1);
    	break;
    case rm8:
    	if ((rgOpType[op] == r8) ||
    	    ((rgOpType[op] == mem) && (OpSize[op] & fByte))) return(1);
    	break;
    case rm16:
    	if ((rgOpType[op] == r16) ||
    	    ((rgOpType[op] == mem) && (OpSize[op] & fWord)) ) return(1);
    	break;
    case rRM:
    	if ((rgOpType[op] == r8) ||
    	    (rgOpType[op] == r16) ||
    	    (rgOpType[op] == r32) ||
    	    (rgOpType[op] == mem)) return(1);
    	break;
    case rRMW:
		if (rgOpType[op] == mem)
		{
			if (OpSize[op] & fFWord)
				return(0);
			if (OpSize[op] & fByte)
				return(0);
			return(1);
		}
    	if ((rgOpType[op] == r16) ||
    	     (rgOpType[op] == r32))
    	     return(1);
		break;
	case mem:
		if ((rgOpType[op] == mem) && (!(OpSize[op] & fFWord))) return(1);
		break;
	case memF:
		if ((rgOpType[op] == memF) && (OpSize[op] & fFWord)) return(1);
		break;
    case moff:
    	if ((rgOpType[op] == mem) &&
    		(OpMType & fDisp32)  &&
    		((OpMType & fIndx)==0) &&
    		((OpMType & fBase)==0))
    	return(1);
    	break;
    case immv3:
    	if ((rgOpType[op] == val8) &&
    	    (OpImm == 3)) return(1);
    	break;
    case immv1:
    	if ((rgOpType[op] == val8) &&
    	    (OpImm == 1)) return(1);
    	break;
    case rDX:
    	if ((rgOpType[op] == r16) &&
    	    (rgOpReg[op] == rDX)) return(1);
    	break;
    case rCL:
    	if ((rgOpType[op] == r8) &&
    	    (rgOpReg[op] == rCL)) return(1);
    	break;
    case rAL:
    	if ((rgOpType[op] == r8) &&
    	    (rgOpReg[op] == rAL)) return(1);
    	break;
    case rAX:
    	if ((rgOpType[op] == r16) &&
    	    (rgOpReg[op] == rAX)) return(1);
    	break;
    case rEAX:
    	if ((rgOpType[op] == r32) &&
    	    (rgOpReg[op] == rEAX)) return(1);
    	break;
    case rCS:
    case rSS:
    case rDS:
    case rES:
    case rFS:
    case rGS:
    	if ((rgOpType[op] == rSEG) &&
    	    (rgOpReg[op] == rgINS[ins][op+1])) return(1);
    	break;
	default:;
	}
return(0);
}

/*****************************************************
   Determines if entry in rgOpType is a register.
*****************************************************/

long is_Reg(long op)
{
	switch (op) { 	/* This should be the value from rgOpType[x] */
		case r8:
	    case r16:
	    case r32:
	    case rCRG:
	    case rDRG:
	    case rTRG:
	    	return(1);
		default:;
	}
	return(0);
}


/*********************************************
This displays the string and exits for a FATAL
assembler error.
**********************************************/

void fatal_error(S8   *pst)
{

++error_count;

if (fListA | fListE) {
	fprintf(lst_fh, "\r\nFATAL ERROR, line %ld - %s\r\n", lineno[level], pst);

/*
	DumpGSymbols();
	DumpLSymbols();
*/
	fclose(lst_fh);
	}

printf("\r\nFATAL ERROR, line %ld - %s\r\n", lineno[level], pst);
printf("%d Errors\r\n%d Warnings\r\n", error_count, warn_count);

exit(1);
}

/*********************************************
This displays the string and line number for
non-fatal errors.
**********************************************/

void line_error(long num)
{
S8 *p;

	switch (num) {
	case  1: p="Invalid expression, ')' expected";		break;
	case  2: p="Invalid expression, value expected"; 	break;
	case  3: p="Value expected after unary '-'";		break;
	case  4: p="Too many digits for numeric radix";		break;
	case  5: p="Invalid character in a number";	 		break;
	case  6: p="Unterminated string";					break;
	case  7: p="Unrecognized character";				break;
	case  8: p="Invalid Alignment specified";			break;
	case  9: p="Start command only allowed in CSEG";		break;
	case 10: p="Virtual command must be first in segment";	break;
	case 11: p="Invalid Virtual value";			 		break;
	case 12: p="Starting address not found";			break;
	case 13: p="Stack command not allowed in DSEG";		break;
	case 14: p="Invalid DOT command";					break;
	case 15: p="Invalid Operand";						break;
	case 16: p="Invalid segment register use";			break;
	case 17: p="Invalid scale value 'Reg*?'";			break;
	case 18: p="Scale value expected (*2,*4,*8)";		break;
	case 19: p="Too many address scale values";			break;
	case 20: p="Invalid register for memory operand";	break;
	case 21: p="Invalid memory operand";				break;
	case 22: p="Offset must be from data segment";		break;
	case 23: p="Nested brackets";						break;
	case 24: p="Unbalanced brackets";			   		break;
	case 25: p="Invalid operand size attribute";   		break;
	case 26:
	case 27:
	case 28:
	case 29:
	case 30:
	case 31: p="";								   			break;
	case 32: p="Unknown token in operand array";			break;
	case 33: p="Too many operands or extra character";		break;
	case 34: p="";											break;
	case 35: p="Invalid expression or numeric value";   	break;
	case 36: p="Operand expected before comma";			   	break;
	case 37: p="";											break;
	case 38: p="Invalid character or reserved word in operand";	break;
	case 39: p="Relative jump out of range";					break;
	case 40: p="Operand size NOT specified or implied";			break;
	case 41: p="Instructions not allowed in data segment";		break;
	case 42: p="Instruction expected after prefix";		 		break;
	case 43: p="Operand sizes don't match";				 		break;
	case 44: p="Wrong operand type for instruction";			break;
	case 45: p="Incorrect format for memory operand";	 		break;
	case 46: p="Strings only valid for DB storage";		 		break;
	case 47: p="Expected '(' after 'DUP'";				 		break;
	case 48: p="Storage expected between commas";		 		break;
	case 49: p="':' not expected";						 		break;
	case 50: p="DWord storage required for OFFSET";		 		break;
	case 51: p="Invalid storage value";					 		break;
	case 52:
	case 53: p="";										 		break;
	case 54: p="':' expected after last label";			 		break;
	case 55: p="Macro not allowed in lexical level 0";	 		break;
	case 56: p="EQU or Storage expected";				 		break;
	case 57:
	case 58:
	case 59:
	case 60:
	case 61:
	case 62: p="";										 		break;
	case 63: p="Instruction expected before register name";		break;
	case 64: p="Public Symbol already defined";			 		break;
	case 65: p="Local symbol already defined";			 		break;
	case 66: p="Number not expected";					 		break;
	case 67: p="New symbol must follow PUBLIC keyword";			break;
	case 68: p="Label, Command, Instruction, or Storage expected";	break;
	case 69: p="Inconsistant redeclaration";					 	break;
	case 70: p="";													break;
	default:
		break;
    }

  fprintf(lst_fh, "\r\nERROR: %d, line: %ld, %s\r\n", num, lineno[level], p);
  *line_ptr = 0;				/* this KILLS the rest of the line */
  Column = 0;
  ++error_count;
}

/*********************************************
This displays the string and line number for
errors dicovered AFTER we past the line.
This is for non-fatal errors.
**********************************************/

void prev_error(S8  *pst, S32 line)
{
   fprintf(lst_fh, "\r\nERROR, line %d - %s\r\n", line, pst);
  Column = 0;
  ++error_count;
}

/**********************************************************
  The parser calls this when it detects a digit leaving
  the line_ptr* pointing to digit. It accepts base 2, 10
  & 16. Base 2 numbers are suffixed with a B or b, base 16
  with an h or H and base 10 with no suffix.
***********************************************************/

U32 get_number(void)
{
U32  value, base;
S8   c, i;
S32  len;
S8   st[33];
  value = 0;
  len = 0;
  base = 10;						/* default base is 10 */

  while(isxdigit(c = *line_ptr)) {	/* get all the digits */
	st[len++] = c;
	line_ptr++;
  }

  if ((*line_ptr== 'h') || (*line_ptr== 'H')) {   /* looks like hex */
	line_ptr++;
	base = 16;
  }
  else if ((st[len-1] == 'b') || (st[len-1] == 'B')) {
	base = 2;
	len--;
  }
  if (((base == 2) && (len > 33)) ||
	  ((base == 10) && (len > 10)) ||
	  ((base == 16) && (len > 9))) {
  	line_error(4);
	return(0);
  }
  i = 0;
  do {
	c = st[i];
	if(isdigit(c))					/* convert numeric digits */
		c -= '0';
	else if(c >= 'a')				/* convert lower case alphas */
		c -= ('a' - 10);
	else if(c >= 'A')				/* convert upper case alphas */
		c -= ('A' - 10);
	else
		break;
	if(c >= base) {					/* outside of base */
		line_error(5);
		return(0);
	}
	value = (value * base) + c;		/* include in total */
	i++; }
  while(i < len);						/* to length of string */
  return value;
}

/*********************************************
This is a fast binary search for a reserved
word that is not an instructions or a register.
**********************************************/

S32 findrsvd(S8 *pb, S32 cb)  /* pointer to and size of string */
{
S32 f, m, n, k;
S8   id[8];

 if (cb>srsvd) return(0);		/* can't be a reserved word */
 strncpy(id, pb, cb);			/* move string local */
 id[cb] = 0;					/* null terminate */

 m = 0;
 n = nreserved-1;
 while (m<=n) {
    k = m + (n-m) / 2;
	f = strncmp(id, rgReserved[k], srsvd-1);
    if (!f) return(k + nrsvd1);			/* found it!   */
    else if (f > 0)						/* it was less */
	   m = k+1;
	else								/* it was more  */
	   n = k-1;
 }
 return(0);
}

/*********************************************
This is a fast binary search for an instuction.
It returns the number or 0 if not found.
**********************************************/

S32 findinst(S8 *pb, S32 cb)  /* pointer to and size of parsed string */
{
S32 f, m, n, k;
S8   id[6];

 if (cb>sinst) return(0);		/* can't be an instruction */
 strncpy(id, pb, cb);		/* move string local */
 id[cb] = 0;  				/* null terminate */

 m = 0;
 n = ninst-1;
 while (m<=n) {
    k = m + (n-m) / 2;
	f = strncmp(id, rginst[k], sinst-1);
    if (!f) return(k+1);				/* found it!   */
    else if (f > 0)						/* id was less */
	   m = k+1;
	else								/* idwas more  */
	   n = k-1;
 }
 return(0);
}

/*********************************************
This is a fast binary search for a register.
It returns the number or 0 if not found.
**********************************************/

S32 findreg(S8 *pb, S32 cb)  /* pointer to, and size of parsed string */
{
S32 f, m, n, k;
S8   id[3];

 if ((cb>sregs-1) || (cb<2)) return(0);	/* can't be a register */
 strncpy(id, pb, cb);				/* move string local */
 id[cb] = 0;  						/* null pad */

 m = 0;
 n = nregs-1;
 while (m<=n) {
    k = m + (n-m) / 2;
	f = strncmp(id, rgreg[k], sregs-1);
    if (!f) return(k+nreg1);			/* found it!   */
    else if (f > 0)						/* it was less */
	   m = k+1;
	else								/* it was more  */
	   n = k-1;
 }
 return(0);
}

/*********************************************
This searches the LOCAL symbol table for the name
described by pb, cb. It only compares items that
are the same length (cb == TSymsize[x]).
It returns the number or 0 if not found.
**********************************************/

S32 findLsymbol(S8 *pb, S32 cb)  /* pointer to, and size of string */
{
S32 i;
S8   name[132];

strncpy(name, pb, cb);		/* move name local */
name[cb] = 0;  				/* null terminate */

i = iLSymNext;
while (i>1) {				/* backwards through symbol table */
	i--;
                    		/* Only compare if same size  */
	if (lst[i].Size == cb) {
		if (strncmp(name, lst[i].Ptr, cb) == 0) return(i);
		}
	}
return(0);		/* not found... */
}


/*********************************************
This searches the symbol table for the name
described by pb, cb. It only compares items that
are the same length (cb == TSymsize[x]).
If the include level is greater than 0 it
searches the local first, then the global.
If at level 0, it only searches the global.
It returns the number or 0 if not found.
**********************************************/

S32 findGsymbol(S8 *pb, S32 cb)  /* pointer to, and size of string */
{
S32 i;
S8   name[132];

strncpy(name, pb, cb);		/* move name local */
name[cb] = 0;  				/* null terminate */

i = iSymNext;
while (i>1) {				/* backwards through symbol table */
	i--;
                    		/* Only compare if same size  */
	if (gst[i].Size == cb) {
		if (strncmp(name, gst[i].Ptr, cb) == 0) return(i);
		}
	}
return(0);		/* not found... */
}

/*********************************************
	DUMP SYMBOL TABLE FOR TESTING
**********************************************/

void DumpGSymbols(void)
{
S32 i;
S8   name[132];

fprintf(lst_fh, "PUBLIC SYMBOLS: \r\n");

i = 1;
while (i<iSymNext) {						/* forward through symbol table */
	strncpy(name, gst[i].Ptr, gst[i].Size);	/* move to name */
	name[gst[i].Size] = 0; 					/* null terminate */

	fprintf(lst_fh, "Name: %s      Offset %08lX  ", name, gst[i].Offs);
	if (gst[i].Type & CLABEL)
		fprintf(lst_fh, "CSEG  ");
	if (gst[i].Type & DLABEL)
		fprintf(lst_fh, "DSEG  ");
	if (gst[i].Type & sBYTE)
		fprintf(lst_fh, "BYTE  ");
	if (gst[i].Type & sWORD)
		fprintf(lst_fh, "WORD  ");
	if (gst[i].Type & sDWORD)
		fprintf(lst_fh, "DWORD  ");
	if (gst[i].Type & sFWORD)
		fprintf(lst_fh, "FWORD  ");
	if (gst[i].Type & tEXTRN)
		fprintf(lst_fh, "EXTRN  ");
	if (gst[i].Type & tFAR)
		fprintf(lst_fh, "FAR    ");
	if (gst[i].Type & tPUBLIC)
		fprintf(lst_fh, "PUBLIC ");
	if (gst[i].Type & MACRO)
		fprintf(lst_fh, "MACRO  ");
	fprintf(lst_fh, "\r\n");
	i++;
	}

}

/*********************************************
	DUMP SYMBOL TABLE FOR TESTING
**********************************************/

void DumpLSymbols(void)
{
S32 i;
S8   name[132];

fprintf(lst_fh, "LOCAL SYMBOLS: \r\n");

i = 1;
while (i<iLSymNext) {						/* forward through symbol table */
	strncpy(name, lst[i].Ptr, lst[i].Size);	/* move to name */
	name[lst[i].Size] = 0; 					/* null terminate */

	fprintf(lst_fh, "Name: %s      Offset %lX   ", name, lst[i].Offs);
	if (lst[i].Type & CLABEL)
		fprintf(lst_fh, "CSEG  ");
	if (lst[i].Type & DLABEL)
		fprintf(lst_fh, "DSEG  ");
	if (lst[i].Type & sBYTE)
		fprintf(lst_fh, "BYTE  ");
	if (lst[i].Type & sWORD)
		fprintf(lst_fh, "WORD  ");
	if (lst[i].Type & sDWORD)
		fprintf(lst_fh, "DWORD  ");
	if (lst[i].Type & sFWORD)
		fprintf(lst_fh, "FWORD  ");
	if (lst[i].Type & tEXTRN)
		fprintf(lst_fh, "EXTRN  ");
	if (lst[i].Type & tFAR)
		fprintf(lst_fh, "FAR    ");
	if (lst[i].Type & tPUBLIC)
		fprintf(lst_fh, "PUBLIC ");
	if (lst[i].Type & MACRO)
		fprintf(lst_fh, "MACRO");
	fprintf(lst_fh, "\r\n");
	i++;
	}
}

/*********************************************
	DUMP Forward Reference Table for testing
**********************************************/

void DumpFRT(void)
{
S32 i;
S8   name[132];

fprintf(lst_fh, "FORWARD REFERENCES:\n");

i = 0;
while (i<iRefNext) {						/* forward through symbol table */
	strncpy(name, pfrt[i].Ptr, pfrt[i].NameSz);	/* move to name */
	name[pfrt[i].NameSz] = 0; 					/* null terminate */

	fprintf(lst_fh, "Name: %s      Offset: %lX   Line: %d\n",
			name,
			pfrt[i].Offs,
			pfrt[i].Line);
	i++;
	}

}

/*********************************************
This sets the global variable fPutBack to TRUE
and copies the current token info so save it
so it can be used again. (returning a token
to the stream...)
**********************************************/
void ReturnToken(void)
{
strncpy(LTString, TString, 132);
LCBString = CBString;
LTSymnum = TSymnum;
LTNumber = TNumber;
LTInst = TInst;
LTReg = TReg;
LToken = Token;
fPutBack = 1;
}


/********************************************
 Parse reads and identifies the next token
 for the caller. See SASM.h for info on each
 of the tokens it identifies.
 Token type is placed in Token.
 All parsed chars are placed in TString.
 Size of TString is placed in CBString.
 Converted numeric values are put in TNumber.
 Instruction numbers are placed in TInst.
 Declared symbol ID numbers are placed in TSymnum.

 Unidentified strings are first checked for
 macro substitution and replaced with the
 stored string if found. If no macro is
 found, the symbol table is searched to see if
 it's a symbol. If so, it's returned. If not
 a symbol, Parse assumes it may be a new one
 and leaves it in TString with the TokenType
 of UNKSYM.
*********************************************/

S32 Parse(void)
{
S32  i;

if (fPutBack) {		/* if a symbol was put back this makes it current */
	strncpy(TString, LTString, 132);
	CBString = LCBString;
	TSymnum = LTSymnum;
	TNumber = LTNumber;
	TInst = LTInst;
	TReg = LTReg;
	Token = LToken;
	fPutBack = 0;
	return(Token);
}

begin:				/* return here after MACRO substitution */
 TSymnum = 0;
 TInst = 0;
 TReg = 0;
 Token = 0;
 TString[0] = 0;
 CBString = 0;
 TNumber = 0;

 while(isskip(*line_ptr)) line_ptr++;		/* skip while space */
 if (!(*line_ptr)) return(0);				/* end of line */

 if	(*line_ptr == SEMI)  					/* if comment */
 {
 	*line_ptr = 0;					/* kill rest of line */
 	return(0);						/* return 0 for EOL */
 }

  /* See if it's a legal 1st char for identifier or reserved word. */
  /* If so, pick it up and put it in TString ALL UPPER CASE */

 i=0;
 if (is_ident(*line_ptr))
 {
	 while( (isalnum(*line_ptr)) || (*line_ptr == '_') )
	 {
		 TString[i++] = toupper(*line_ptr++);
	 }
	CBString = i;
	TString[i] = 0;

    if (TReg = findreg(TString, i))    /* Register? */
 		return (Token = REGIST);

    if (TInst = findinst(TString, i))  /* Instruction? */
 		return (Token = INSTRU);

    if (Token = findrsvd(TString, i))  /* Reserved Word? */
 		return (Token);

	if (level) {
		if (TSymnum = findLsymbol(TString, i)) {

			if (lst[TSymnum].Type & MACRO) {		/* MACRO !! */
			  i = lst[TSymnum].Offs;				/* get number */
			  strcpy(line_buf1, rgMacPtr[i]);		/* move it in */
			  strncat(line_buf1, line_ptr, 132);	/* cat rest of line */
			  line_ptr = line_buf1;
			  goto begin;
			}
			else return (Token = LSYMBOL);
		}
	}

	if (TSymnum = findGsymbol(TString, i))
	{
		 return (Token = SYMBOL);
	}
	else return (Token = UNKSYM);
 }

 /* If char was not legal for an identifier the only things left
    are digits, special characters, and 'strings' */

 if (isdigit(*line_ptr))
 {
  	TNumber = get_number();
 	return (Token =	NUMBER);
 }
 switch (*line_ptr) {
	case DOLLAR:
	case OPENRND:
	case CLOSRND:
	case STAR:
	case PLUS:
	case COMMA:
	case MINUS:
	case DOT:
	case SLASH:
	case COLON:
	case OPENSQR:
	case CLOSSQR:
		{
		Token = *line_ptr;
		TString[0] = *line_ptr++;
		TString[1] = 0;
		CBString = 1;
 		return (Token);
 		}
    case SQUOTE:
    	{
		line_ptr++;
		while ((*line_ptr) && (*line_ptr != SQUOTE) && (i < 132))
			TString[i++] = *line_ptr++;
		CBString = i;
		if (*line_ptr != SQUOTE)
			line_error(6);
		else
			line_ptr++;		/* next char if this is SQUOTE */
		TString[i] = 0;		/* null terminate string */
		CBString = i;		/* Set size */
		return(Token = STRING);
		}
    default: {
		line_error(7);
		return(Token = ERROR);
	}
 } /* end switch */
}

/**************************************
  Handles include files for INCLUDE and
  SEARCH commands.
***************************************/
void DoInclude(char *pName)
{
		if (level < LEVELS-1) {
			 ++level;

			strcpy(srcname[level], pName);

			if(!(src_fh[level] = fopen(srcname[level], "r"))) {
				--level;
				fatal_error("Can't open INCLUDE file\n");
				}
			else
				lineno[level] = 0;

			if (fListA) {
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
				Column += fprintf(lst_fh, "INCLUDE: %s", srcname[level]);
				}
			}
		else
			fatal_error("EXCEEDED MAX INCLUDE DEPTH (5)");

}

/*******************************************
  Handles SEARCH command for library search.
  We open the PUB file given and find the
  first filename. We save this name, then
  run through the publics seeing if we
  need any of them.
*******************************************/
void DoSearch(char *pName)
{

}


/********************************************
   Handles DOT Commands
*********************************************/
void Command(void)
{
U32 num;
S32 i, ii;
char tmpname[50];

i = Parse();

switch(i)
{
	case rDATA:
		fDataSeg = 1;
		if (fListA) {
			pNextAddr = &oNextData;
			Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			Column += fprintf(lst_fh, "<- DSEG Begin", lst_fh);
			}
		break;
	case rCODE:
		fDataSeg = 0;
		if (fListA) {
			pNextAddr = &oNextCode;
			Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			Column += fprintf(lst_fh, "<- CSEG Begin", lst_fh);
			}
		break;
	case rALIGN:
		ii = Parse();
		if (fListA)
			Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
		if (ii == rWORD)
		{
			if (fListA)
				Column += fprintf(lst_fh, "<- WORD ALIGN, PAD: ", lst_fh);
			if (fDataSeg)
			{
				if (oNextData & 1)
				{
					OutByteX(0);	/* EMIT byte to Data seg, value 0 */
					if (fListA)
						Column += fprintf(lst_fh, "00 ", lst_fh);
				}
			}
			else
			{
				if (oNextCode & 1)
				{
					OutByteX(0x90); /* EMIT byte to Code seg, 0x90 (NOP) */
					if (fListA)
						Column += fprintf(lst_fh, "90 ", lst_fh);
				}
			}
		}
		else if (ii == rDWORD)
		{
			if (fListA)
				Column += fprintf(lst_fh, "<- DWORD ALIGN, PAD: ", lst_fh);
			if (fDataSeg)
			{
				while (oNextData & 3) {
					OutByteX(0);	/* EMIT byte to Data seg, value 0 */
					if (fListA)
						Column += fprintf(lst_fh, "00 ", lst_fh);
				}
			}
			else
			{
				while (oNextCode & 3)
				{
					OutByteX(0x90); /* EMIT byte to Code seg, 0x90 (NOP) */
					if (fListA)
						Column += fprintf(lst_fh, "90 ", lst_fh);
				}
			}
		}
		else if (ii == rPARA)
		{
			if (fListA)
				Column += fprintf(lst_fh, "<- PARA(16) ALIGN, PAD: ", lst_fh);
			if (fDataSeg)
			{
				while (oNextData & 0x0f) {
					OutByteX(0);	/* EMIT byte to Data seg, value 0 */
					if (fListA)
						Column += fprintf(lst_fh, "00 ", lst_fh);
				}
			}
			else
			{
				while (oNextCode & 0x0f)
				{
					OutByteX(0x90); /* EMIT byte to Code seg, 0x90 (NOP) */
					if (fListA)
						Column += fprintf(lst_fh, "90 ", lst_fh);
				}
			}
		}
		else
			line_error(8);
		break;
	case rEND:
		if (fListA) {
			Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
			Column += fprintf(lst_fh, "<- END of Source ");
			}
		break;
	case rSTART:
		if (!fDataSeg) {
			if (fListA) {
				Column += fprintf(lst_fh, "%08lX ", *pNextAddr);
				Column += fprintf(lst_fh, "<- START Address ");
				}
            fStart = 1;
            StartAddr = oNextCode;
			}
		else
			line_error(9);
		break;
	case rVIRTUAL:
		if (fDataSeg) {
			if (fListA)
				Column += fprintf(lst_fh, "VIRTUAL Segment address: ");
			if (Parse() == NUMBER) {
				if (oNextData > 0)
					line_error(10);
				else {
					if (fListA)
						Column += fprintf(lst_fh, "%08lX", TNumber);
					oNextData = TNumber;
					DataOffset = TNumber;
				}
			}
			else
			  line_error(11);
		}
		else {
			if (fListA)
				Column += fprintf(lst_fh, "VIRTUAL Segment address: ");
			if (Parse() == NUMBER) {
				if (oNextCode > 0)
					line_error(10);
				else {
					if (fListA)
						Column += fprintf(lst_fh, "%08lX", TNumber);
					oNextCode = TNumber;
					CodeOffset = TNumber;
				}
		    }
			else
			  line_error(11);
		}
		break;
	case rINCLUDE:
		tmpname[0] = 0;		/* default to null name */
		while (isskip(*line_ptr)) line_ptr++;
		i=0;
		while ((*line_ptr) && (*line_ptr != ';') && (!isskip(*line_ptr)))
			tmpname[i++] = *line_ptr++;
		tmpname[i] = 0;					/* null terminate */

		DoInclude(tmpname);
		break;
	case rSEARCH:
		tmpname[0] = 0;		/* default to null name */
		while (isskip(*line_ptr)) line_ptr++;
		i=0;
		while ((*line_ptr) && (*line_ptr != ';') && (!isskip(*line_ptr)))
			tmpname[i++] = *line_ptr++;
		tmpname[i] = 0;					/* null terminate */

		DoSearch(tmpname);
		break;
	case rSTACK:
		if (!fDataSeg) {
			if (fListA)
				Column += fprintf(lst_fh, "Stack Total: ");
			if (Parse() == NUMBER) {
				StackTotal += TNumber;
				if (fListA)
					Column += fprintf(lst_fh, "%08lX", StackTotal);
			}
			else
				line_error(35);	/* Invalid number or expression */
		}
		else
		  line_error(13);
	  	break;
	default: line_error(14);
		return;
}
}

/********************************************
 This adds the 3 bit code for the specified
 register to the ModRM or SIB bytes which
 is pointed to by *modbyte. nshift is the
 number of bits to shift the register code
 before ORing it with *modbyte (ModRM or SIB).
*********************************************/

void EncodeRegBits(S32 regnum, S8   *modbyte, S32 nshift)
{

switch (regnum) {
	case rAL:
	case rAX:		/* value is 1 don't do anything */
	case rEAX:
	case rES:
	case rCR0:
	case rDR0:
		break;
	case rCL:
	case rCX:
	case rECX:
	case rCS:
	case rCR1:
	case rDR1:
		*modbyte |= (1 << nshift);
		break;
	case rDL:
	case rDX:
	case rEDX:
	case rSS:
	case rCR2:
	case rDR2:
		*modbyte |= (2 << nshift);
		break;
	case rBL:
	case rBX:
	case rEBX:
	case rDS:
	case rCR3:
	case rDR3:
		*modbyte |= (3 << nshift);
		break;
	case rAH:
	case rSP:
	case rESP:
		*modbyte |= (4 << nshift);
		break;
	case rCH:
	case rBP:
	case rEBP:
	case rFS:
		*modbyte |= (5 << nshift);
		break;
	case rDH:
	case rSI:
	case rESI:
	case rGS:
	case rTR6:
	case rDR6:
		*modbyte |= (6 << nshift);
		break;
	case rBH:
	case rDI:
	case rEDI:
	case rTR7:
	case rDR7:
		*modbyte |= (7 << nshift);
		break;
	default:;		/* if it's not one of these we do nothing */
}
}

/********************************************
  Add Macro to LOCAL symbol table.
  The last symbol added to the symbol
  table is the label for this macro.

*********************************************/
void AddMacro(void)
{
S32 i, j;
S8   mac[100];

mac[0] = 0;

if (iMacNext >= MACBUFMAX-50)
	fatal_error("Macro buffer overflow...");
if (iMacNext >= MACSMAX)
	fatal_error("Macro table overflow...");

i = 0; j = 0;
while(isskip(*line_ptr)) line_ptr++;		/* skip while space */

/* Read the macro including white space upto EOL or comment.
   j keeps track of the last NON-space character so we don't
   store unneeded spaces at the end of the macro.
*/

while ((*line_ptr) && (*line_ptr != SEMI)) {
	mac[i++] = *line_ptr++;
	if (mac[i-1] > 0x20) j = i;			/* j will be length */
}

strncpy(pMacNext, mac, j);

/* These are all "iLSymNext-1" because the Macro label has
   already been added to the symbol table when AddMacro
   is called.
*/

lst[iLSymNext-1].Type = 0;			/* cancel previous type */
lst[iLSymNext-1].Type |= MACRO;		/* initialize type */

rgMacPtr[iMacNext] = pMacNext;		/* store mac ptr in pmac array */
lst[iLSymNext-1].Offs = iMacNext;	/* store mac # in offset */
iMacNext++;
pMacNext += j;
*pMacNext = 0;						/* null terminate the macro */
pMacNext++;

}
