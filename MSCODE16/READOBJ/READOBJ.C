/*
 * Object Module Reader for Intel object records
 * Copyright 1992 R.A. Burgess

To build this, use Boralnd or Turbo C as follows:

   BCC -ml ReadObj.c

This will give you ReadObj.exe. The -ml is optional and
makes this a large model program. I have had so many less problems
with pointers and data conversions using it on everything,
I just do it out of habit now.


 */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* misc variables */

FILE *input_fp = 0, *output_fp = 0;
int RType, erc;
unsigned long  lfa;
unsigned int reclen;

/*********** BEGIN FUNCTIONS **************/

void Encode (char *Dest, int x)
{
int i;

Dest[0] = ' ';
Dest[1] = ' ';
Dest[2] = ' ';
Dest[3] = ' ';
Dest[4] = ' ';
i = 4;
do {
  Dest[i] = (x % 10) + 0x30;
  x /= 10;
  i--;
}
while (x);
}

/*********************************************/

void EncodeHex (char *pDest, unsigned char x)
{
int  i;
unsigned char nibble;

for (i=1; i>=0; i--) {
  nibble = x & 0x0f;
  if (nibble < 0x0A)
	pDest[i] = nibble + 0x30;
  else
	pDest[i] = nibble + 0x37;
  x /= 0x10;
 }
}

/*********************************************/

void Copyn(char *dest, char *source, int n)
{
while (n--)	*dest++ = *source++;
}

/*********************************************/

void ReadRecordLen (unsigned int *pCbRet)
{
int  cblo, cbhi, cb;

cblo = fgetc(input_fp);
cbhi = fgetc(input_fp);
cb = (cbhi << 8) + cblo;
lfa = lfa + cb + 3;
*pCbRet = cb;
}


/*********************************************/

void WriteRecInfo(void)
{
char num[9];
int sData;
unsigned int cb;

EncodeHex (&num[0], (lfa >> 24) & 0xff);
EncodeHex (&num[2], (lfa >> 16) & 0xff);
EncodeHex (&num[4], (lfa >> 8) & 0xff);
EncodeHex (&num[6], lfa & 0xff);
num[8] = 0;
fputs(num,output_fp);
ReadRecordLen (&reclen);
fputs(", size: ", output_fp);
cb = reclen -1;					/* exclude checksum in size display */
EncodeHex (&num[0], (cb >> 8) & 0xff);
EncodeHex (&num[2], cb & 0xff);
num[4] = 0;
fputs(num,output_fp);
}

/*********************************************/

void Dump (char *pb, int cb)
{
char   line[70];
int    sDataRet;
int    i;
char   b;
int    limit;

for (i=0; i<69; i++) line[i] = ' ';		/* clear line */
while (cb) {
  if (cb >= 16) {
    limit = 15;
    cb -= 16;
	}
  else {
    limit = cb - 1;
    cb = 0;
	}
  Copyn(&line[50], pb, limit + 1);
  pb += 16;
  for (i=0; i<=limit; i++) {
    b = line[50 + i];
    EncodeHex (&line[(3*i)], b);
    if ((b < 0x20) || (b > 0x7E)) line[50+i] = '.';
  }
  line [67] = '\r';
  line [68] = '\n';
  line [69] = 0;
  fputs(line, output_fp);
 }
}


/*********************************************/

void ReadBsRecord(unsigned char *pDest, int nbytes)
{
int i;
unsigned char b;

while (nbytes--) {
  i = fgetc(input_fp);
  if (i==EOF) exit(1);
  b = i & 0xff;
  *pDest++ = b;
  }
}

/*********************************************/

void DumpRecord (void)
{
int  cb;
char line[16];
int  len;
int  sData;

 cb = reclen;
 cb--;				/* Get rid of Check Sum */
 while (cb) {
  if (cb > 16) {
    len = 16;
    cb -= 16;
  }
  else {
    len = cb;
    cb = 0;
  }

  ReadBsRecord (line, len);
  Dump (line, len);
 }
 fputs("\r\n", output_fp);
 if (fseek(input_fp, lfa, 0)) exit(1);
}


/*********************************************/

void UnknownRType (unsigned char RType)
{
char  num[3];
int   sData;

EncodeHex (num, RType);
num[2] = 0;
fputs("Unknown Record Type ", output_fp);
fputs(num, output_fp);
fputs(" at lfa ", output_fp);
WriteRecInfo ();
fputs("\r\n", output_fp);
}

/*********************************************/

void ReadIndex (unsigned int *pIndex, int *pCb)
{
unsigned char  b;
int i;

i = fgetc(input_fp);
if (i==EOF) exit(1);
b = i & 0xff;
--*pCb;
if (!(b & 0x80))
  *pIndex = b;
else {
  i = b & 0x7F;
  *pIndex = i * 0x100;
  i = fgetc(input_fp);
  if (i==EOF) exit(1);
  b = i & 0xff;
  --*pCb;
  *pIndex += b;
 }
}

/*********************************************/

void ThreadRecord (unsigned char FType, int *pCb)
{
unsigned char Thread[8];   /* ARRAY [WRD (0)..7] OF BYTE */
unsigned int  TIndex[8];   /* ARRAY [WRD (0)..7] OF WORD */
unsigned int  method;
unsigned char i;

 i = FType & 3;
 if (FType & 0x40) i += 4;
 method = (FType / 4) & 7;
 Thread[i] = method;
 if ((i <= 4) || (method < 4))
   ReadIndex (&TIndex[i], pCb);
 else
   TIndex[i] = 0;
}


/*********************************************/

void FixupRecord (unsigned char FType, int *pCb)
{
unsigned char  Thread[8];   /* ARRAY [WRD (0)..7] OF BYTE */
unsigned int   TIndex[8];   /* ARRAY [WRD (0)..7] OF WORD */
unsigned char  DataOffset;
char           Number[3];
unsigned int   sData;
unsigned char  FixData;
unsigned char  frame;
unsigned int   fDatum;
unsigned char  target;
unsigned int   tDatum;
long  displacement;

if (!(FType & 0x40))
  fputs("Self relative ", output_fp);
else
  fputs("Segment relative ", output_fp);
switch ((FType / 4) & 7) {
  case 0: fputs("lobyte  ", output_fp); break;
  case 1: fputs("offset  ", output_fp); break;
  case 2: fputs("base    ", output_fp); break;
  case 3: fputs("pointer ", output_fp); break;
  case 4: fputs("hibyte  ", output_fp); break;
  default: ;
  }

DataOffset = fgetc(input_fp);
--*pCb;
EncodeHex (Number, FType & 3);
Number[2] = 0;
fputs(Number,output_fp);
EncodeHex (Number, DataOffset);
fputs(Number,output_fp);
FixData = fgetc(input_fp);
--*pCb;
if (!(FixData & 0x80)) {
  frame = (FixData / 0x10) & 7;
  if (frame < 4)
	ReadIndex (&fDatum, pCb);
  else
	fDatum = 0;
}
else {
/*  WRITE (' frame thread ');  */
  frame = Thread [4 + ((FixData / 16) & 3)];
  fDatum = TIndex [4 + ((FixData / 16) & 3)];
  }
if (!(FixData & 8)) {
  target = FixData & 7;
  ReadIndex (&tDatum, pCb);
}
else {
  /* WRITE (' target thread');  */
  target = Thread[FixData & 3] | (FixData & 4);
  tDatum = TIndex[FixData & 3];
}
fputs(" , F", output_fp);
fputc(frame + 0x30, output_fp);
if (fDatum) {
  fputc(' ', output_fp);
  EncodeHex (Number, (fDatum >> 8));
  Number[2] = 0;
  fputs(Number,output_fp);

  EncodeHex (Number, (fDatum & 0xff));
  Number[2] = 0;
  fputs(Number,output_fp);
}
fputs(" , T", output_fp);
fputc(target + 0x30, output_fp);
fputc(' ', output_fp);

EncodeHex (Number, (tDatum >> 8));
Number[2] = 0;
fputs(Number,output_fp);

EncodeHex (Number, (tDatum & 0xff));
Number[2] = 0;
fputs(Number,output_fp);

if (!(FixData & 4)) {
  displacement = 0;
  fputc(' ', output_fp);
  if (!(FType & 2)) {
    ReadBsRecord (&displacement, 2);
	*pCb -= 2;
  }
  else {
    ReadBsRecord (&displacement, 3);
	*pCb -= 3;
	Number[2] = 0;
	EncodeHex (Number, (displacement >> 16) & 0xff);
    fputs(Number,output_fp);
  }
  EncodeHex (Number, (displacement >> 8) & 0xff);
  fputs(Number,output_fp);
  EncodeHex (Number, displacement& 0xff);
  fputs(Number,output_fp);
}
fputs("\r\n",output_fp);
}


/*********************************************/

void FIXUPP (void)
{
int  cb;
unsigned char  FType;
unsigned int   sData;

fputs("\r\n",output_fp);
cb = reclen;
cb--;
while (cb) {
  FType = fgetc(input_fp);
  cb--;
  if (!(FType & 0x80))  ThreadRecord (FType, &cb);
  else FixupRecord (FType, &cb);
  }
if (fseek(input_fp, lfa, 0)) exit(1);
}


/****************************************
* Write a line to the output file
*****************************************/

void write_line(char *pstr)
{
	fputs(pstr, output_fp);
	fputs("\r\n", output_fp);
}

/*******************************************
* Report a non-recoverable error
********************************************/

void fatal_error(char *string)
{
	fputs("Fatal error, object dump aborted\r\n", stderr);
	fputs(string,stderr);
	exit(-1);
}


/*********************
* Main program
**********************/

void main(int argc, char *argv[])
{
	int i, j, fdone;
	char *ptr;

/* first process any filenames and command line options */
	for(i=1; i < argc; ++i) {
		ptr = argv[i];
		switch((*ptr++ << 8) | *ptr++) {
			default:
				if(!input_fp) {
					if(!(input_fp = fopen(argv[i], "rb")))
						fatal_error("Cannot open input file\n"); }
				else if(!output_fp) {
					if(!(output_fp = fopen(argv[i], "w")))
						fatal_error("Cannot open output file\n"); }
				else
					fatal_error("Too many parameters\n");
				}
		}

/* any files not explicitly named default to standard I/O */
	if(!input_fp)				/* default to standard input */
	{
		printf("No input file specified...");
		exit(1);
	}
	if(!output_fp)				/* default to standard output */
		output_fp = stdout;

	fputs("Object Module Reader V1.0\r\n", stderr);
	fputs("\r\n", output_fp);


RType = fgetc(input_fp);
if (RType == EOF) erc = 1;

while (!erc) {
  switch (RType) {
    case 0x6E :
    			fputs("RHEADR   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x70 :
    			fputs("REGINT   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x72 :
    			fputs("REDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x74 :
    			fputs("RIDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x76 :
    			fputs("OVLDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x78 :
    			fputs("ENDREC   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x7A :
    			fputs("BLKDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x7C :
    			fputs("BLKEND   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x7E :
    			fputs("DEBSYM   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x80 :
    			fputs("THEADR   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x82 :
    			fputs("LHEADR   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x84 :
    			fputs("PEDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x86 :
    			fputs("PIDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x88 :
    			fputs("COMENT   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x8A :
    			fputs("MODEND   ",output_fp);
                exit(1);
				break;
    case 0x8C :
    			fputs("EXTDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x8E :
    			fputs("TYPDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x90 :
    			fputs("PUBDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x91 :
    			fputs("PUBDEF32 ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x92 :
    			fputs("LOCSYM   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x94 :
    			fputs("LINNUM   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0x96 :
    			fputs("LNAMES   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0x98 :
    			fputs("SEGDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0x99 :
    			fputs("SEGDEF32 ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0x9A :
    			fputs("GRPDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0x9C :
    			fputs("FIXUPP   ",output_fp);
    			WriteRecInfo();
                FIXUPP();
    			fputs("\r\n", output_fp);
				break;
	case 0x9D :
				fputs("FIXUPP32 ",output_fp);
				WriteRecInfo();
/*				FIXUPP(); */
    			fputs("\r\n", output_fp);
				DumpRecord();
    			fputs("\r\n", output_fp);
				break;
	case 0xA0 :
    			fputs("LEDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0xA1 :
    			fputs("LEDATA32 ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0xA2 :
    			fputs("LIDATA   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0xA4 :
    			fputs("LIBHED   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0xA6 :
    			fputs("LIBNAM   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
    case 0xA8 :
    			fputs("LIBLOC   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
 	case 0xAA :
    			fputs("LIBDIC   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0xB0 :
    			fputs("COMDEF   ",output_fp);
    			WriteRecInfo();
    			fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0xB2 :
				fputs("FORREF   ",output_fp);
				WriteRecInfo();
				fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0xB4 :
				fputs("MODEXT   ",output_fp);
				WriteRecInfo();
				fputs("\r\n", output_fp);
				DumpRecord();
				break;
	case 0xB6 :
				fputs("MODPUB   ",output_fp);
				WriteRecInfo();
				fputs("\r\n", output_fp);
				DumpRecord();
				break;
	default:
    			UnknownRType(RType);
    			fputs("\r\n", output_fp);
                DumpRecord();
				break;
	}
  RType = fgetc(input_fp);
  if (RType == EOF) erc = 1;

  }

}
