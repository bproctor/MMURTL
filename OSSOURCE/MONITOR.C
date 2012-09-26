/*
  MMURTL Operating System Source Code
  Copyright 1991,1992,1993,1994 Richard A. Burgess
  ALL RIGHTS RESERVED
  Version 1.0
*/


/* This is the MMURTL Monitor program.
   It provides the user with statistics and control of the
   environment, and a few basic functions such as starting a CLI.

   The monitor has three main functions:
   1) Sets up internal device drivers and services.
   2) Checks config files and loads any system services specified
      in the config file,
   3) Loads a CLI and assigns the video and keyboard to it.

   Because the Monitor is part of the OS we do not declare a main
   function.  The entry point to the monitor is called Monitor and
   it has no parameters.  After the OS gets done with its low level
   initialization, the monitor procedure is "Jumped" to.

*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

#include "MKernel.h"
#include "MMemory.h"
#include "MData.h"
#include "MTimer.h"
#include "MVid.h"
#include "MKbd.h"
#include "MJob.h"
#include "MFiles.h"
#include "MDevDrv.h"


#define ok 0
#define ErcNoDevice 504

static char rgStatLine[] =
"mm/dd/yy  00:00:00              MMURTL Monitor                   Tick:0         ";

static char rgMonMenu1[] = "LdCLI\xb3Jobs  \xb3Stats \xb3      ";
static char rgMonMenu2[] = "     \xb3      \xb3      \xb3Reboot";
static char rgMonMenu3[] = "     \xb3Debug \xb3      \xb3     ";

static char rgCPR1[] ="MMURTL (tm) - Message based, MUltitasking, Real-Time kerneL";
static char rgCPR2[] ="Copyright (c) R.A. Burgess, 1990-1993, All Rights Reserved";

static char *CRLF = "\r\n\r\n";

static unsigned long Color = WHITE|BGBLACK;		/* Color test for xprintf */

static long time, date, tick;

unsigned long KillExch;		/* Messaging for stat task KILL proc */

static unsigned long KillMsg[2];	/* First DWORD = TSSExch, second is ERROR */
static unsigned long KillError;
static unsigned long KillJobNum;
static unsigned char fKilled;

static unsigned long MngrExch;		/* Messaging for stat task */
static unsigned long MngrMsg[2];
static unsigned long MngrHndl;
static unsigned long gcode;

static unsigned long GPExch;		/* Messaging for main */
static unsigned long GPMsg[2];
static unsigned long GPHndl;

static unsigned long GP1Exch;		/* Extra Messaging for main */
static unsigned long GP1Msg[2];
static unsigned long GP1Hndl;

/* Structure for disk device driver status and setup */

static struct diskstattype {
  U32 erc;
  U32 blocks_done;
  U32 BlocksMax;
  U8 fNewMedia;
  U8 type_now;		/* current disk type for drive selected */
  U8 resvd1[2];		/* padding for DWord align  */
  U32 nCyl;			/* total physical cylinders */
  U32 nHead;		/* total heads on device    */
  U32 nSectors;		/* Sectors per track        */
  U32 nBPS;			/* Number of bytes per sect */

  U32 LastRecalErc0;
  U32 LastSeekErc0;
  U8  LastStatByte0;
  U8  LastErcByte0;
  U8  fIntOnReset;	/* Interrupt was received on HDC_RESET */
  U8  filler0;
  U32 LastRecalErc1;
  U32 LastSeekErc1;
  U8  LastStatByte1;
  U8  LastErcByte1;
  U8  ResetStatByte;	/* Status Byte immediately after RESET */
  U8  filler1;
  U32 resvd1[2];	/* out to 64 bytes */
  };

static struct diskstattype DiskStatus;

#define nMaxJCBs 34		/* 32 dynamic plus 2 static */
static struct JCBRec *pJCB;

static long StatStack[256];	/* 1024 byte stack for Stat task */

static long MngrStack[256];	/* 1024 byte stack for Mngr task */

static unsigned char Buffer[512];
static unsigned long nMemPages;

extern unsigned long oMemMax;
extern unsigned long nSwitches;
extern unsigned long nSlices;
extern unsigned long nHalts;
extern unsigned long nReady;
extern unsigned long nRQBLeft;
extern unsigned long nJCBLeft;
extern unsigned long nTSSLeft;
extern unsigned long nLBLeft;
extern unsigned long nEXCHLeft;
extern unsigned long BootDrive;

/*============ protos (NEAR MMURTL support calls) =================*/

extern long InitKBDService(void);	/* From Keyboard.asm */
extern long fdisk_setup(void);		/* From Floppy.c */
extern long hdisk_setup(void);		/* From HardIDE.c */
extern long coms_setup(void);		/* From RS232.c */
extern long lpt_setup(void);		/* From Parallel.c */
extern long InitFS(void);			/* From Fsys.c */

extern long GetExchOwner(long Exch, char *pJCBRet);
extern long DeAllocJCB(long *pdJobNumRet, char *ppJCBRet);

/*=================== START OF CODE ====================*/

/**************************************************************
 Formatted output routines for montitor program
 xprintf, xsprintf.
***************************************************************/

#include <stdarg.h>

#define	S_SIZE	100

/*********************************************
* Determine if a character is a numeric digit
**********************************************/

static long isdigit(long chr)
{
;
#asm
	MOV EAX,[EBP+8]
	CMP AL, 30h		;0
	JL isdigit0		;No
	CMP AL, 39h		;
	JLE isdigit1	;Yes
isdigit0:
	XOR EAX,EAX		;No
	JMP SHORT isdigit2
isdigit1:
	MOV EAX, -1
isdigit2:

#endasm
}

static long strlen(char *cs)
{
;
#asm
	XOR EAX, EAX
	MOV ESI,[EBP+8]
_strlen0:
	CMP BYTE PTR [ESI],0
	JE _strlen1
	INC ESI
	INC EAX
	JMP SHORT _strlen0
_strlen1:
#endasm
}

/*************************************************************
 This does the actual parsing of the format and also moves to
 the next arg(s) in the list from the passed in arg pointer.
 The number of chars written is returned (not incl \0).
**************************************************************/
static long _ffmt(char *outptr, char *fmt, long *argptr)
{
char numstk[33], *ptr, justify, zero, minus, chr;
unsigned long width, value, i, total;

	total = 0;
	while(chr = *fmt++) 
	{
		if(chr == '%') 
		{					/* format code */
			chr = *fmt++;
            ptr = &numstk[32];
			*ptr = justify = minus = 0;
			width = value = i = 0;
			zero = ' ';
			if(chr == '-')
			{				/* left justify */
				--justify;
				chr = *fmt++;
			}
			if(chr == '0')					/* leading zeros */
				zero = '0';
			while(isdigit(chr))
			{			/* field width specifier */
				width = (width * 10) + (chr - '0');
				chr = *fmt++;
			}

			value = *--argptr;				/* get parameter value */

			switch(chr)
			{
				case 'd' :					/* decimal number */
					if(value & 0x80000000)
					{
						value = -value;
						++minus;
					}
				case 'u' :					/* unsigned number */
					i = 10;
					break;
				case 'x' :					/* hexidecimal number */
				case 'X' :
					i = 16;
					break;
				case 'o' :					/* octal number */
					i = 8;
					break;
				case 'b' :					/* binary number */
					i = 2;
					break;
				case 'c' :					/* character data */
					*--ptr = value;
					break;
				case 's' :					/* string */
					ptr = value;			/* value is ptr to string */
					break;
				default:					/* all others */
					*--ptr = chr;
					++argptr;				/* backup to last arg */
			}

			if(i)		/* for all numbers, generate the ASCII string */
				do 
				{
					if((chr = (value % i) + '0') > '9')
						chr += 7;
					*--ptr = chr; 
				}
				while(value /= i);

			/* output sign if any */

			if(minus) 
			{
				*outptr++ = '-';
				++total;
				if(width)
					--width;
			}

			/* pad with 'zero' value if right justify enabled  */

			if(width && !justify) 
			{
				for(i = strlen(ptr); i < width; ++i)
					*outptr++ = zero;
					++total;
			}

			/* move in data */

			i = 0;
			value = width - 1;

			while((*ptr) && (i <= value)) 
			{
				*outptr++ = *ptr++;
				++total;
				++i;
			}

			/* pad with 'zero' value if left justify enabled */

			if(width && justify) 
			{
				while(i < width) 
				{
					*outptr++ = zero;
					++total;
					++i;
				}
			}
		}
		else 
		{
			/* not format char, just move into string  */
			*outptr++ = chr;
			++total;
		}
	}

	*outptr = 0;
	return total;
}

/************************************
    Formatted print to screen
*************************************/

long xprintf(char *fmt, ...)
{
	va_list ap;
	long total;
	char buffer[S_SIZE];

	va_start(ap, fmt);		/* set up ap pointer */
	total = _ffmt(buffer, fmt, ap);
	TTYOut(buffer, strlen(buffer), Color);
	va_end(ap, fmt);
	return total;
}

/************************************
    Formatted print to string s
*************************************/

long xsprintf(char *s, char *fmt, ...)
{
	va_list ap;
	long total;

	va_start(ap, fmt);			/* set up ap pointer */
	total = _ffmt(s, fmt, ap);
	va_end(ap, fmt);
	return total;
}

/**********************************************
 Checks to ensure we don't scroll the function
 keys off the screen.
**********************************************/

void CheckScreen()
{
long iCol, iLine;

	GetXY(&iCol, &iLine);
	if (iLine >= 23)
	{
		ScrollVid(0,1,80,23,1);
		SetXY(0,22);
	}
}

/*********************************************************
    This is called to initialize the sacreen.
*********************************************************/

static void InitScreen(void)
{
	ClrScr();
    xsprintf(&rgStatLine[70], "%d", tick);
    PutVidChars(0,0, rgStatLine, 80, WHITE|BGBLUE);
	PutVidChars(0,  24, rgMonMenu1, 26, BLUE|BGWHITE);
	PutVidChars(27, 24, rgMonMenu2, 26, BLUE|BGWHITE);
	PutVidChars(54, 24, rgMonMenu3, 25, BLUE|BGWHITE);
	SetXY(0,1);
	return;
}

/*********************************************************
    This is the status task for the Monitor.
    Besides displaying the top status line for
    the montitor, it has the job of looking for
    messages from the job code that indicate
    a job has terminated. When it gets one,
    it recovers the last of the resources and
    then notifies the user on the screen.
*********************************************************/

static void StatTask(void)
{
unsigned long erc, i, Exch, Msg[2];
U8 *pPD, *pVid;

for(;;)
{
	GetCMOSTime(&time);
	rgStatLine[10] = '0' + ((time >> 20) & 0x0f);
	rgStatLine[11] = '0' + ((time >> 16) & 0x0f);
	rgStatLine[13] = '0' + ((time >> 12) & 0x0f);
	rgStatLine[14] = '0' + ((time >> 8) & 0x0f);
	rgStatLine[16] = '0' + ((time >> 4) & 0x0f);	/* seconds */
	rgStatLine[17] = '0' + (time & 0x0f);

	GetCMOSDate(&date);
	rgStatLine[0] = '0' + ((date >> 20) & 0x0f); /* month */
	rgStatLine[1] = '0' + ((date >> 16) & 0x0f);
	rgStatLine[3] = '0' + ((date >> 12) & 0x0f); /* Day */
	rgStatLine[4] = '0' + ((date >> 8)  & 0x0f);
	rgStatLine[6] = '0' + ((date >> 28) & 0x0f); /* year */
	rgStatLine[7] = '0' + ((date >> 24) & 0x0f);

	GetTimerTick(&tick);
	xsprintf(&rgStatLine[70], "%d", tick);
	PutVidChars(0,0, rgStatLine, 80, WHITE|BGBLUE);

	Sleep(50);	/* sleep 0.5 second */

	GetTimerTick(&tick);
	xsprintf(&rgStatLine[70], "%d", tick);
	PutVidChars(0,0, rgStatLine, 80, WHITE|BGBLUE);

	Sleep(50);	/* sleep 0.5 second */

	/* Now we check for tasks that are Jobs that are killing
	themselves (either due to fatal errors or no exitjob).
	The message has Error, TSSExch in it.
	*/

	erc =  CheckMsg(KillExch, KillMsg);
	if (!erc)
	{	 /* someones there wanting to terminate...*/

		/* Get and save the error (KillMsg[0]) */
		KillError = KillMsg[0];

		/* Call GetExchOwner which gives us pJCB */

		erc = GetExchOwner(KillMsg[1], &pJCB);
		if (!erc)
		{

			KillJobNum = pJCB->JobNum;
			Tone(440,50);
			xprintf("Job number %d terminated. Error: %d\r\n",
					KillJobNum, KillError);
			CheckScreen();

			pPD  = pJCB->pJcbPD;
			pVid = pJCB->pVirtVid;

			/* Must change video to monitor if this guy owned it */

			GetVidOwner(&i);
			if (i == KillJobNum)
			{
				GetTSSExch(&Exch);	/* Use our TSS exchange for Request */
				SetVidOwner(1);
				erc = Request("KEYBOARD", 4, Exch, &i, 0,
					0, 0,   0, 0,   1, 0, 0);
				erc = WaitMsg(Exch, Msg);
			}

			/* Now we deallocate the JCB and the TSSExch
			which will free the TSS automatically!
			*/

			DeAllocExch(KillMsg[1]);
			DeAllocJCB(pJCB);	/* No error returned */

			/* When the JCB was created, the PD and it's 1st
			PDE (PT) were allocated as two pages next to each other
			in linear memory.  So we can just deallocate both
			pages in one shot. Then we deallocate the single
			page for virtual video.
			*/

			DeAllocPage(pPD, 2);
			DeAllocPage(pVid, 1);

			fKilled = 1;
			/* We're done (and so is he...) */
		}
	}

} /* for EVER */
}

/*********************************************************
    This is the Manager task for the Monitor.
    It allows us to switch jobs with the
    CTRL-ALT-PageDown key.
    We don't actually switch jobs, we just
    ressign video and keyboard to the next
    active job (except the Debugger).
    Also, if the debugger has the video...
    we don't do it at all!
    This also looks for CTRL-ALT-DELETE which kills
    the job that owns the keyboard/Video so long as it's
    not the Monitor or the Debugger.
*********************************************************/

static void MngrTask(void)
{
long erc, i, j, fDone;
char *pJCB;

/* Leave a Global Key Request outstanding with KBD service
   for the status task
*/

erc = Request("KEYBOARD", 2, MngrExch, &MngrHndl, 0, &gcode,
			  4, 0, 0, 0, 0, 0);

for(;;) 
{
	erc = WaitMsg(MngrExch, MngrMsg);

	if (!erc)
	{
		if ((gcode & 0xff) == 0x0C)
		{
			/*  Find next valid Job that is NOT the
		    	debugger and assign Vid and Keyboard to
				it.
			*/

			erc = GetVidOwner(&j);
			fDone = 0;
			i = j;
			while (!fDone)
			{
				i++;
				if (i==2) i = 3;
				else if (i>34) i = 1;
				erc = GetpJCB(i, &pJCB);		/* erc = 0 if valid JCB */
				if ((!erc) || (i==j))
					fDone = 1;
			}
			if (i != j)
			{
				SetVidOwner(i);
				erc = Request("KEYBOARD", 4, MngrExch, &MngrHndl, 0,
					0, 0,   0, 0,   i, 0, 0);
				erc = WaitMsg(MngrExch, MngrMsg);

			}
		}
		else if ((gcode & 0xff) == 0x7F)	/* CTRL-ALT-DEL (Kill)*/
		{
			erc = GetVidOwner(&j);
			erc = KillJob(j);
		}

		/* leave another global key request */
		erc = Request("KEYBOARD", 2, MngrExch, &MngrHndl, 0, &gcode,
				  4, 0, 0, 0, 0, 0);
	}

} /* for EVER */
}

/*********************************************************
  This simply does a software interrupt 03 (Debugger).
*********************************************************/

static void GoDebug(void)
{
;
#asm
	INT 03
#endasm
return;
}

/*********************************************************
  This strobes keyboard data port 60h with 00 after
  sending 0D1 command to Command port.  We have to loop
  reading the status bit of the command port to make
  sure it's OK to send the command. This resets the
  processor which then executes the boot ROM.
*********************************************************/

static void Reboot(void)
{
;
#asm
		CLI                     ;first we clear interrupts
		MOV ECX, 0FFFFh	        ;check port up to 64K times
Reboot0:
		IN AL,64h		        ;Read Status Byte into AL
		TEST AL,02h				;Test The Input Buffer Full Bit
		LOOPNZ Reboot0
		MOV AL,0FEh				;Strobe bit 0 of keyboard crtlr output
		OUT 64h,AL
		STI
#endasm
return;
}

/*********************************************************
  This reads a file called Initial.Job from the system
  directory and loads all jobs specified in the file.
  Video and keyboard are not assigned to any of these
  jobs unless it is cli.run and the last job loaded.
*********************************************************/

void LoadJobFile(void)
{
long erc, fh, sjobfile, cbrunfile, i, j, job;
unsigned char sdisk;
char ajobfile[50];
char arunfile[80];
char fdone, fcli;

	GetSystemDisk(&sdisk);
	sdisk &= 0x7F;
	sdisk += 0x41;		/* 0=A, 1=B, 2=C etc. */
	ajobfile[0] = sdisk;
	CopyData(":\\MMSYS\\INITIAL.JOB\0", &ajobfile[1], 20);
	sjobfile = strlen(ajobfile);

	erc =  OpenFile(ajobfile, sjobfile, 0, 1, &fh);
	if (!erc)
	{
		fdone = 0;
		job = 0;
		fcli = 0;
		while (!fdone)
		{
			i = 0;
			do
			{
				erc = ReadBytes(fh, &arunfile[i++], 1, &j);

			} while ((!erc) && (arunfile[i-1] != 0x0A) && (i < 80));

			if ((!erc) && (i > 1))
			{
				if (arunfile[0] == ';')  /* a comment line */
					continue;

				cbrunfile = 0;
				while ((arunfile[cbrunfile] != 0x0A) &&
                       (arunfile[cbrunfile] != 0x0D) &&
                       (arunfile[cbrunfile] != 0x20) &&
                       (arunfile[cbrunfile] != 0x09) &&
                       (arunfile[cbrunfile]))
					cbrunfile++;

				if (cbrunfile > 2)
				{
					arunfile[cbrunfile] = 0; /* null terminte for display */

					if ((cbrunfile > 8) &&
						(CompareNCS(&arunfile[cbrunfile-7],
									"cli.run", 7) == -1))
						fcli = 1;
					else
						fcli = 0;

					xprintf("Loading: %s...\r\n", arunfile);
					CheckScreen();
					erc =  LoadNewJob(arunfile, cbrunfile, &job);
					if (!erc)
					{
						xprintf("Successfully loaded as job %d\r\n", job);
						CheckScreen();
						Sleep(50);
					}
					else
					{
						xprintf("ERROR %d Loading job\r\n", erc);
						CheckScreen();
						Sleep(50);
						job = 0;
						erc = 0;
					}
				}
			}
			else fdone = 1;
		}
		CloseFile(fh);

		/* if the last succesfully loaded job was a cli,
		   assign the keybaord and video to it.
		*/

		if ((job > 2) && (fcli))
		{
			SetVidOwner(job);
			erc = Request("KEYBOARD", 4, GP1Exch, &GP1Hndl, 0,
				0, 0,   0, 0,   job, 0, 0);
			if (!erc)
				erc = WaitMsg(GP1Exch, GP1Msg);
		}
	}
	else
	{
		xprintf("INITIAL.JOB file not found in system directory.\r\n");
		CheckScreen();
	}
}


/*********************************************************
  This Loads the MMURTL Command Line Interpreter
  and switches video and keyboard to give the
  user access to it.
*********************************************************/

static long LoadCLI(void)
{
long erc, job;
unsigned char sdisk, acli[40];

	GetSystemDisk(&sdisk);
	sdisk &= 0x7F;
	sdisk += 0x41;		/* 0=A, 1=B, 2=C etc. */
	acli[0] = sdisk;
	CopyData(":\\MMSYS\\CLI.RUN\0", &acli[1], 16);
	xprintf("Loading: %s...", acli);

	erc =  LoadNewJob(acli, strlen(acli), &job);
	if (!erc)
	{
		xprintf("New CLI Job Number is: %d\r\n", job);
		CheckScreen();
		Sleep(50);
		SetVidOwner(job);
		erc = Request("KEYBOARD", 4, GP1Exch, &GP1Hndl, 0,
			0, 0,   0, 0,   job, 0, 0);
		if (!erc)
			erc = WaitMsg(GP1Exch, GP1Msg);
	}
return erc;
}

/*********************************************************
    This is the main procedure called from the OS after
    all OS structures and memory are initialized.
*********************************************************/

void Monitor(void)
{
long erc, i, j, k, iCol, iLine;
unsigned long ccode, ccode1;
unsigned char c;
char text[70];

InitScreen();

Tone(250,15);		/* 250 Hz for 150ms */
Tone(1000,33);		/* 250 Hz for 330ms */

/* Allocate an exchange for the Manager task global keycode */

erc = AllocExch(&MngrExch);
if (erc)
  xprintf("AllocExch (Mngr Exch) Error: %d\r\n", erc);

erc = SpawnTask( &StatTask, 24, 0, &StatStack[255], 1 );	/* Task 4 */
if (erc)
  xprintf("SpawnTask (StatTask) Error: %d\r\n", erc);

erc = AllocExch(&KillExch);
if (erc)
  xprintf("AllocExch (Kill Exch) Error: %d\r\n", erc);

Color = YELLOW|BGBLACK;
xprintf("MMURTL (tm) - Message based, MUltitasking, Real-Time kerneL\r\n");
xprintf("Copyright (c) R.A.Burgess, 1991-1995  ALL RIGHTS RESERVED\r\n\r\n");

Color = WHITE|BGBLACK;

c = ((BootDrive & 0x7f) + 0x41);
xprintf("BootDrive: %c\r\n", c);

i = (oMemMax+1)/1024;
xprintf("Total memory (Kb): %d\r\n", i);

erc = QueryPages(&nMemPages);
i = (nMemPages*4096)/1024;
xprintf("Free memory  (Kb): %d\r\n", i);

erc = InitKBDService();
xprintf("Init KBD Service Error: %d\r\n", erc);

erc = coms_setup();
xprintf("Init Serial Comms Device Driver Error: %d\r\n", erc);

erc = lpt_setup();
xprintf("Init Parallel LPT Device Driver Error: %d\r\n", erc);

/* Allocate general purpose exchanges to use in the monitor */

erc = AllocExch(&GPExch);
if (erc)
  xprintf("AllocExch Error: %d\r\n", erc);

erc = AllocExch(&GP1Exch);
if (erc)
  xprintf("AllocExch GP1 Error: %d\r\n", erc);

xprintf("Init floppy device driver... Error: ");
erc = fdisk_setup();
xprintf("%d\r\n", erc);

xprintf("Init hard disk device driver... Error: ");
erc = hdisk_setup();
xprintf("%d\r\n", erc);

xprintf("Initializing file system...\r\n");
erc = InitFS();
xprintf("File System... Error: %d\r\n", erc);

/* Spawn manager task */

SpawnTask( &MngrTask, 10, 0, &MngrStack[255], 1 );

/*
   Call LoadJobFile to read job file from system directory 
   and execute and jobs listed there.
*/

LoadJobFile();

for (;;)  /* Loop forEVER looking for user desires */
{

	/* Make a ReadKbd Key Request with KBD service. Tell it
	to wait for a key.
	*/

	erc = Request("KEYBOARD", 1, GPExch, &GPHndl, 0, &ccode,
   					  4, 0, 0, 1, 0, 0);
	if (erc)
	    xprintf("Kbd Svc Request KERNEL ERROR: %d\r\n", erc);

	/* wait for the keycode to come back */

	erc = WaitMsg(GPExch, GPMsg);
	if (erc)
	 xprintf("KERNEL Error from Wait msg:  %d\r\n", erc);

	c = ccode & 0xff; /* lop off everything but the key value */

	switch (c)
	{
	case 0x0F:		/* F1 Run */
			erc = LoadCLI();
			if (erc)
				xprintf("Error from LoadCLI:  %d\r\n", erc);
			break;
	case 0x10:		/* F2 Jobs */
			InitScreen();
			j = 2; /* Line */
			k = 0; /* Col offset */
			for	(i=1; i<nMaxJCBs; i++)
			{
				if (j > 20)
					k = 40;
				erc = GetpJCB(i, &pJCB);		/* erc = 0 if valid JCB */
				if (!erc)
				{
					SetXY(k,j);
					xprintf("Job: %d\r\n", pJCB->JobNum);
					SetXY(k+10,j);
					CopyData(&pJCB->sbJobName[1], text, 13);
					text[pJCB->sbJobName[0]] = 0;
					xprintf("Name: %s\r\n", text);
					j++;
				}
			}
			break;
	case 0x11:		/* F3 Stats - loops displaying status till key is hit */
			InitScreen();
			while (erc = ReadKbd(&ccode1, 0))
			{ /* ReadKbd no wait until no error */
				SetXY(0,1);
				erc = QueryPages(&nMemPages);
                xprintf("Any key to dismiss status... \r\n");
                xprintf("Free 4K memory pages:      %d\r\n", nMemPages);
                xprintf("Task switches total:       %d\r\n", nSwitches);
                xprintf("Preemptive task switches:  %d\r\n", nSlices);
                xprintf("CPU idle ticks (no work):  %d\r\n", nHalts);
                xprintf("Tasks Ready to Run:        %d\r\n", nReady);
                xprintf("Free Task State Segments:  %d\r\n", nTSSLeft);
                xprintf("Free Job Control Blocks:   %d\r\n", nJCBLeft);
                xprintf("Free Request Blocks:       %d\r\n", nRQBLeft);
                xprintf("Free Link Blocks:          %d\r\n", nLBLeft);
                xprintf("Free Exchanges:            %d\r\n", nEXCHLeft);
				SetXY(0,1);
				PutVidChars(29, 1, "|",  1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, "/",  1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, "-",  1, GREEN|BGBLACK); Sleep(12);
				PutVidChars(29, 1, "\\", 1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, "|",  1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, "/",  1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, "-",  1, GREEN|BGBLACK); Sleep(12);
				PutVidChars(29, 1, "\\", 1, GREEN|BGBLACK); Sleep(9);
				PutVidChars(29, 1, " ",  1, GREEN|BGBLACK);
			}
			SetXY(0,12);
            xprintf ("\r\n");
			break;
	case 0x16:		/* F8 Reboot */
			xprintf("\r\nF8 again to reboot, any other key to cancel");
			erc = ReadKbd(&ccode1, 1);
			if ((ccode1 & 0xff) == 0x16)
				Reboot();
			xprintf("...Cancelled\r\n");
			break;
	case 0x18:		/* F10 Debug */
			GoDebug();
			break;
	case 0x00:		/* No Key */
			Sleep(3);	/* Sleep for 30 ms */
			break;
	case 0x12:		/* F4 - future use */
	case 0x13:		/* F5  */
	case 0x14:		/* F6  */
	case 0x15:		/* F7  */
	case 0x17:		/* F9  */
	case 0x19:		/* F11 */
	case 0x1A:		/* F12 */
			break;
	default:
		if (((c > 0x1F) && (c < 0x80)) ||
			(c==0x0D) || (c==8))
			{
				if (c==0x0D)
					TTYOut (CRLF, 2, WHITE|BGBLACK);
				else
					TTYOut (&c, 1, WHITE|BGBLACK);
		   }
	}

	GetXY(&iCol, &iLine);
	if (iLine >= 23)
	{
		ScrollVid(0,1,80,23,1);
		SetXY(0,22);
	}

} /* for EVER */

}


/*===========  THE END  =========================================*/
