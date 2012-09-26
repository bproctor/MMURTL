/* MMLoader.c
   This is the DOS loader for the MMURTL OS RUN file.

   MMURTL is loaded into memory in two basic pieces.
   The first piece is the DATA segment. It is loaded at linear
   addrsss 0.  The default OS stacks are contained in this segment.
   The second piece is the CODE. It is loaded at linear
   address 10000h (The 64K boundry).  If you know anything about
   MS-DOS, you that is doesn't consume this kind of memory.
   This loader program will load somwhere between the 25K
   and 64K linear boundry depending on whether DOS is loaded
   "HIGH" or not, and also what TSRs you have installed.
   We must allocate some dummy space to ensure that when we
   load our two segments from disk and copy them down, we don't
   overwrite this loader program (in which case
   your system will lock up tighter than a drum!).

   NOTE: This is the only program requiring a DOS programming
   tool because "We don't do 16 bit real mode code..."
   This must be compiled with Borland C	due to the inline assembler.

	You MUST use the -B and -ml options. Example

		BCC -B -ml MMLoader.c

   The -B forces assebly language compilation and linkage.
       -ml sets it to large model
   You probably won't need to rebuild it all because it reads the
   size of the segments for the MMURTL run file and adjusts the
   buffers for them.  So modifying MMURTL (adding or taking away code or
   data) shouldn't affect how the loader works (unless you exceed
   128K for code or data, in which case you'll have to modify this
   to handle it).

   What actually happens here:
	(This basically turns MS-DOS into a $79.00 loader)

	1) Load the code and data into DOS memory (we don't own)
	   Gets the boot drive from OSName or MS-DOS id not
	   specified in OSName.
	3) Clear interrupts
	2) Turn on the A20 Address Line Gate
	4) Move the data from the buffers into proper place for execution
	5) Set the Interrupt and Global Descriptor table registers
	   in preparation for protect mode transition
	6) Set Protected mode bit in CR0 (Control Register 0)
	7) Jump to OS Code entry point at linear address 10000h
	in the code segment/selector (which is 8h).
	All OS segments are ZERO based.

	NOTE: This the last the processor sees of 16 bit real mode code
    (no great loss at all in my humble opinion)

*/



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
#include <dos.h>
#include <dir.h>
#include "\OSSOURCE\runfile.h"

U32 fEOF = 0;

long CSegSize = 0;
long DSegSize = 0;

char *ptest;

long	sStack = 0;
long	sCode = 0;
long	sData = 0;

long	offCode = 0;
long	offData = 0;
long	pStart = 0;

struct tagtype tag;

char osname[50] = {"MMURTL.RUN"};	/* default OS name */

FILE *run_fh = 0;
FILE *out_fh = 0;

unsigned char bootdrive;

/**** Execute ************************************************
   At this point:
     Interrupts are disabled,
     The code and data are waiting to be moved down to 0 linear
	 So we move it, then we tell the processor the size and
	 location of the IDT and GDT (with the SIDT and SGDT
	 insturctions), then set the protected mode bit and
	 jump to the OS code!
     This code can't access any data segment variables.
     It must all be self contained because we relocate it.
**************************************************************/

void Execute(void)
{

/* We're pulling some REAL cheating here in real mode
to get this done.  We set DS up to where we moved the
variables we need to use. We did this so we wouldn't
overwrite them. Then we move the OS code & data DOWN
and DO IT!  This code ends up at 9000:0
*/

asm {	.386P
		CLI
		NOP

		MOV BX, 9800h
		MOV DS, BX
		MOV BX, 0
		MOV WORD PTR DS:[BX], 07FFh
		MOV BX, 2
		MOV WORD PTR DS:[BX], 0h
		MOV BX, 4
		MOV WORD PTR DS:[BX], 0h

		MOV BX, 6
		MOV WORD PTR DS:[BX], 017FFh
		MOV BX, 8
		MOV WORD PTR DS:[BX], 0800h
		MOV BX, 10
		MOV WORD PTR DS:[BX], 0h


		MOV BX, 0B800h
		MOV ES, BX
		MOV BX, 0
		MOV WORD PTR ES:[BX], 4731h

		/* Set up our new DS to where we moved the data */
		/* We must do this before each 32K load cause we use DS */


		/* Move first 32K data chunk */

		MOV BX, 06000h
		MOV DS, BX
		XOR SI, SI
		MOV AX,0000h
		MOV ES,AX
		XOR DI,DI
		MOV CX,8000h
		CLD                    	;
		REP MOVSB				;

		/* Move second 32K data chunk */

		MOV BX, 06800h
		MOV DS, BX
		XOR SI, SI
		MOV AX,0800h
		MOV ES,AX
		XOR DI,DI
		MOV CX,8000h
		CLD                    	;
		REP MOVSB				;

		/* Move first 32K code chunk */

		MOV BX, 07000h
		MOV DS, BX
		XOR SI, SI
		MOV AX,1000h
		MOV ES,AX
		XOR DI,DI
		MOV CX,8000h
		CLD                    	;
		REP MOVSB				;

		/* Move second 32K code chunk */

		MOV BX, 07800h
		MOV DS, BX
		XOR SI, SI
		MOV AX,1800h
		MOV ES,AX
		XOR DI,DI
		MOV CX,8000h
		CLD                    	;
		REP MOVSB				;

		/* Move third 32K code chunk */

		MOV BX, 08000h
		MOV DS, BX
		XOR SI, SI
		MOV AX,2000h
		MOV ES,AX
		XOR DI,DI
		MOV CX,8000h
		CLD                    	;
		REP MOVSB				;


		MOV BX, 0B800h
		MOV ES, BX
		MOV BX, 2
		MOV WORD PTR ES:[BX], 4732h

		MOV BX, 9800h
		MOV DS, BX
		MOV BX, 0

		LIDT FWORD PTR DS:[BX+0]
		LGDT FWORD PTR DS:[BX+6]
		MOV EAX,CR0
		OR AL,1
		MOV CR0,EAX
		JMP $+4
		NOP
		NOP
		NOP
		NOP

        MOV BX, 10h
		MOV DS,BX
		MOV ES,BX
		MOV FS,BX
		MOV GS,BX
		MOV SS,BX
		DB 66h
		DB 67h
		DB 0EAh
		DD 10000h
		DW 8h
		RETN
} /* end of assembler */

}


/**** TurnOnA20 ****************************************************
    This turns on the address line 20 Gate. This must be done before
    we can physically address above 1 Meg.  If your machine only
    shows one Meg of RAM and you know there's more, this isn't
    working on your machine... (R&D time..)
********************************************************************/

void TurnOnA20(void)
{

asm {
		CLI
		XOR CX,CX
	}

IBEmm0:

asm {
		IN AL,64h
		TEST AL,02h
		LOOPNZ IBEmm0
		MOV AL,0D1h
		OUT 64h,AL
		XOR CX,CX
	}

IBEmm1:

asm {
		IN AL,64h
		TEST AL,02h
		LOOPNZ IBEmm1
		MOV AL,0DFh
		OUT 60h,AL
		XOR CX,CX
	}

IBEmm2:

asm {
		IN AL,64h
		TEST AL,02h
		LOOPNZ IBEmm2
	}
}

/*** Relocate *********************************************
This is a tricky little piece of code. We are going to
manipulate DOS memory we don't even own.
We are going to set up a temporary little environment
up at A0000h linear (A000:0 segmented) for our
data environment. And then at 90000h (9000:0) for
the loader code itself. This will become CS.
**********************************************************/

void Relocate(void)
{
char *pdest, *psource, **pnewp;
long *pnewl;
unsigned int *pneww;
int  i;

/* First we move the code up */

	psource = (void*) &Execute;

	printf("SEG: %u OFF: %u\r\n", FP_SEG(psource), FP_OFF(psource));

	pdest = MK_FP(0x9000,0);
	i = 2000;	/* more than enough for the code */
	while(i--)
		*pdest++ = *psource++;

}


void CallExecute(void)

{
/* We push the new code address on the stack then RETF to it.
   We PUSH CS then IP */

asm {
	MOV AX,	09000h
	PUSH AX
	MOV AX, 0
	PUSH AX
	RETF
}

}

/* Borland calls procedures as near indirect
to far data definitions. This means the
bootdrive param will be at EBP+06
*/

void SetBootDrive(unsigned int bootdrive)

{
/* get boot drive off stack and place in DL */

asm {
	XOR EDX, EDX
	MOV DX,	WORD PTR [BP+06h]
}

}


/*********************************************************
  This reads and loads the two MMURTL segments into
  six 32K buffers. Two for the data segment, and 4 for
  the code segment. (192K Maximum).
  The MMURTL OS is built as a standard MMURRL run file.
  This is a tag/length/value file as described in the
  DASM documentation (in great detail).
*********************************************************/

void LoadRunFile(void)
{
int ii;
long i, dret, nobj, nread;
char filetype, junk, *pin;

	while (1) {
		tag.id = 0;
/*		fprintf(out_fh, "\r\nFile LFA: %ld\r\n", ftell(run_fh)); */
		nobj = fread (&tag, 1, 5, run_fh);
		if (nobj) {
/*
			ii = tag.id;
			fprintf(out_fh, "Tag Value  (hex): %02X\r\n", ii);
			fprintf(out_fh, "Tag Length (dec): %ld\r\n", tag.len);
*/
		}
		else {
			fprintf(out_fh, "Invalid OS Run file. No End Tag Found.\r\n");
			fclose(run_fh);
			exit(1);
		}

		switch (tag.id) {
			case IDTAG:
				nobj = fread (&filetype, 1, 1, run_fh);
				if (filetype == 1)
					break;
				else {
					fprintf(out_fh, "File MUST be a run file\r\n");
					exit(1);
				}
				break;
			case SEGTAG:
				fprintf(out_fh, "Segment Sizes:\r\n");
				nobj = fread (&sStack, 1, 4, run_fh);
				if (nobj) nobj = fread (&sCode, 1, 4, run_fh);
				if (nobj) nobj = fread (&sData, 1, 4, run_fh);
				fprintf(out_fh, "  Stack   %ld\r\n", sStack);
				fprintf(out_fh, "  Code    %ld\r\n", sCode);
				fprintf(out_fh, "  Data    %ld\r\n", sData);
				break;
			case DOFFTAG:
				nobj = fread (&offData, 1, 4, run_fh);
				fprintf(out_fh, "Virtual Data Offset: %08lXh\r\n", offData);
				break;
			case COFFTAG:
				nobj = fread (&offCode, 1, 4, run_fh);
				fprintf(out_fh, "Virtual Code Offset: %08lXh\r\n", offCode);
				break;
			case STRTTAG:
				nobj = fread (&pStart, 1, 4, run_fh);
				fprintf(out_fh, "Start Address: %08lXh\r\n", pStart);
				break;
			case CODETAG:
				fprintf(out_fh, "Reading code:  %ld bytes\r\n", tag.len);
                CSegSize = tag.len;

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;
				pin = MK_FP(0x7000,0);
				nobj = fread (pin, 1, nread, run_fh);

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;
				pin = MK_FP(0x7800,0);
				nobj = fread (pin, 1, nread, run_fh);

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;
				pin = MK_FP(0x8000,0);
				nobj = fread (pin, 1, nread, run_fh);
				break;
			case DATATAG:
				fprintf(out_fh, "Reading data:  %ld bytes\r\n", tag.len);
                DSegSize = tag.len;
				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;
				pin = MK_FP(0x6000, 0);
				nobj = fread (pin, 1, nread, run_fh);

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;
				pin = MK_FP(0x6800,0);
				nobj = fread (pin, 1, nread, run_fh);
				break;
			case CDFIXTAG:
			case CCFIXTAG:
			case DDFIXTAG:
			case DCFIXTAG:
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, run_fh);
					tag.len-=4;
				}
				break;
			case ENDTAG:
				nobj = fread (&i, 1, 4, run_fh); 	/* Eat the end tag */
				fclose(run_fh);
                TurnOnA20();
				Relocate();			/* Move the loader code & data */
				SetBootDrive((unsigned int) bootdrive);
				CallExecute();
				break;
			default:
				while ((tag.len--) && (nobj))
					nobj = fread (&junk, 1, 1, run_fh);
				break;

		} /* end of switch */
	}
}

/***************************
* Main program -- MMLoader
****************************/

void main(S16 argc, S8   *argv[])
{
S8   *ptr;
S16  i;
char fName;

	out_fh = stdout;
	fName = 0;

	for(i=1; i < argc; ++i) {
		ptr = argv[i];
		if (*ptr == '/') {
		  ptr++;
		  switch(*ptr) {
			case 'U' :
			case 'u' :
				break;
			case 'D' :
			case 'd' :
				break;
			case 'O' :
			case 'o' :
				fName = 1;
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
			if (fName)
				strncpy(osname, argv[i], 39);
		}
	}

	printf("MMURTL DOS Loader Ver 1.0 \r\n");

	printf("OS Filename specified: %s \r\n",  osname);

	run_fh = fopen(osname, "rb");
	if (!run_fh) {
		printf("Can't open: %s \r\n",  osname);
		exit(1);
	}

	if (osname[1] == ':')
	{
		bootdrive = osname[0] - 0x41;
		if (bootdrive >= 0x20)
			bootdrive -= 0x20;	/* may be lower case */
	}
	else
	   bootdrive = getdisk();

	ptest = malloc(100);	/*  32K */
	printf("First MEM = SEG: %u OFF: %u\r\n", FP_SEG(ptest), FP_OFF(ptest));

	LoadRunFile();

}

/*********************** End of Module *****************/
