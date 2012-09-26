/* CLI.C V1.0  (A MMURTL Command Line Interpreter) */
/*  Copyright (c) 1993, 1994 R.A. Burgess     */
/*         ALL RIGHTS RESERVED                */
/* Use MakeIt.Bat to build in DOS, or use CM32 and DASM:
   CM32 CLI.C
   CM32 DASM.ATF
*/


#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char
#define TRUE 1
#define FALSE 0


#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Includes for OS public calls and structures */

#include "\OSSOURCE\MKernel.h"
#include "\OSSOURCE\MMemory.h"
#include "\OSSOURCE\MData.h"
#include "\OSSOURCE\MTimer.h"
#include "\OSSOURCE\MVid.h"
#include "\OSSOURCE\MKbd.h"
#include "\OSSOURCE\MJob.h"
#include "\OSSOURCE\MFiles.h"
#include "\OSSOURCE\MStatus.h"

/******************** BEGIN DATA ********************/

/* Define the Foreground and Background colors */

#define EDVID   BLACK|BGWHITE
#define NORMVID WHITE|BGBLACK
#define STATVID BLACK|BGCYAN

long iCol, iLine;

/* aStatLine is exactly 80 characters in length */

char aStatLine[] =
"mm/dd/yy  00:00:00  CLI  V1.0    Job     Path:                                  ";

char aPath[70];
long cbPath;
char fUpdatePath;

char sdisk, syspath[50];	/* holds system disk and system path */
char hlppath[60];			/* Path to help file */
char clipath[60];			/* Path to CLI */
char cmdpath[60];			/* Path to command file */

char aCmd[80];				/* Command line */
long cbCmd = 0;

unsigned char Buffer[512];
unsigned char bigBuf[4096];
char text[70];

char ExitChar;

unsigned long GPExch;		/* Messaging for Main */
unsigned long GPMsg[2];
unsigned long GPHndl;

unsigned long StatExch;	/* Messaging for status task */
long StatStack[256];	/* 1024 byte stack for stat task */

long time, date;

long JobNum;

/* array of internal commands for parsing */

#define NCMDS 16

char paCmds[NCMDS+1][10] = {
	"",					/* 0 external */
	"xxxxx",			/* 1 - not used */
	"Cls",				/* 2  */
	"Copy",				/* 3  */
	"Del",				/* 4  */
	"Dir",				/* 5  */
	"Debug",			/* 6  */
	"Dump",				/* 7  */
	"Exit",				/* 8  */
	"Help",				/* 9  */
	"MD",				/* 10 */
	"Monitor",			/* 11 */
	"Path",				/* 12 */
	"RD",				/* 13 */
	"Ren",				/* 14 */
	"Run",				/* 15 */
	"Type",				/* 16 */
	};

#define EXTCMD 0		/* External Command */
#define RESVDCMD 1
#define CLSCMD  2
#define COPYCMD 3
#define DELCMD  4
#define DIRCMD  5
#define DBGCMD  6
#define DUMPCMD 7
#define EXITCMD 8
#define HELPCMD 9
#define MAKEDIR	10
#define MONCMD  11
#define PATHCMD 12
#define REMDIR	13
#define RENCMD  14
#define RUNCMD  15
#define TYPECMD 16


long CmdNum = 0;

char *apParam[13];		/* Param 0 is cmd name */
long acbParam[13];
#define nParamsMax 13

#define ErcBadParams 80

/* Directory Entry Records, 16 records 32 bytes each */

struct dirstruct {
	U8  Name[8];
	U8  Ext[3];
	U8  Attr;
	U8  Rsvd[10];
	U16 Time;
	U16 Date;
	U16 StartClstr;
	U32 FileSize;
	} dirent[16];

/************************* BEGIN CODE ********************/

/*********************************************************
    This is the status task for the CLI.  It updates
    the status line which has the time and path.
    It runs as a separate task and is started each
    time the CLI is executed.
*********************************************************/

void StatTask(void)
{
	for(;;)
	{
		if (fUpdatePath)
		{
			if (cbPath > 30)
				cbPath = 30;
			FillData(&aStatLine[47], 30, ' ');
			CopyData(aPath, &aStatLine[47], cbPath);
			fUpdatePath = 0;
		}


		GetCMOSDate(&date);
		aStatLine[0] = '0' + ((date >> 20) & 0x0f); /* month */
		aStatLine[1] = '0' + ((date >> 16) & 0x0f);
		aStatLine[3] = '0' + ((date >> 12) & 0x0f); /* Day */
		aStatLine[4] = '0' + ((date >> 8)  & 0x0f);
		aStatLine[6] = '0' + ((date >> 28) & 0x0f); /* year */
		aStatLine[7] = '0' + ((date >> 24) & 0x0f);

		GetCMOSTime(&time);
		aStatLine[10] = '0' + ((time >> 20) & 0x0f);
		aStatLine[11] = '0' + ((time >> 16) & 0x0f);
		aStatLine[13] = '0' + ((time >> 12) & 0x0f);
		aStatLine[14] = '0' + ((time >> 8) & 0x0f);
		aStatLine[16] = '0' + ((time >> 4) & 0x0f);
		aStatLine[17] = '0' + (time & 0x0f);

		PutVidChars(0,0, aStatLine, 80, STATVID);

		Sleep(100);	/* sleep 1 second */
	} /* forEVER */
}

/*********************************************************
  This displays error code text string if listed and
  NON zero.
*********************************************************/

void CheckErc (unsigned long erc)
{
char *pSt;
char st[40];

FillData(st, 40, 0);
switch (erc)
{
	case 0: return;
	case 1:	  pSt = "End of file";		break;
	case 4:	  pSt = "User cancelled";		break;
	case 80:  pSt = "Invalid parameters";	 break;
	case 101: pSt = "Out of memory (need more for this)";	 break;
	case 200: pSt = "Invalid filename (not correct format)";	 break;
	case 201: pSt = "No such drive";	break;
	case 202: pSt = "The name is not a file (it's a directory)"; break;
	case 203: pSt = "File doesn't exist";	break;
	case 204: pSt = "Directory doesn't exist";	break;
	case 205: pSt = "File is ReadOnly"; break;
	case 208: pSt = "File in use"; break;
	case 222: pSt = "Can't rename across drives"; break;
	case 223: pSt = "Can't rename across directories"; break;
	case 226: pSt = "File Already Exists (duplicate name)"; break;
	case 228: pSt = "Root directory is full"; break;
	case 230: pSt = "Disk is full (bummer)"; break;
	case 231: pSt = "Directory is full"; break;

	default:
		sprintf(st, "Error %05d on last command", erc);
		pSt = st;
		break;
}
	printf("%s\r\n", pSt);
}

/*********************************************************
  This simply does a software interrupt 03 (Debugger).
*********************************************************/

void GoDebug(void)
{
;
#asm
	INT 03
#endasm
return;
}

/*********************************************************
    This is called to initialize the screen.
    If we are returning from and external command
    we do not reset the cursor position. fNew is
    used to determine is we do or not.
*********************************************************/

void InitScreen(int fNew)
{
	SetNormVid(NORMVID);
	GetXY(&iCol, &iLine);
	if (fNew)
	{
		iCol = 0;
		iLine = 2;
		ClrScr();
	}
	SetXY(iCol, iLine);
    PutVidChars(0,0, aStatLine, 80, STATVID);
	return;
}

/*********************************************
 This does a hex dump to the screen of a 
 memory area passed in by "pb"
**********************************************/

U32 Dump(unsigned char *pb, long cb, unsigned long addr)
{
U32 erc, i, j, KeyCode;
unsigned char buff[17];

	erc = 0;
	GetXY(&iCol, &iLine);

	while (cb)
	{
		printf("%08x ", addr);
		if (cb > 15) j=16;
		else j = cb;
		for (i=0; i<j; i++)
		{
			printf("%02x ",*pb);
			buff[i] = *pb++;
			if (buff[i] < 0x20)
				buff[i] = 0x2E;
			if (buff[i] > 0x7F)
				buff[i] = 0x2E;
		}
		buff[i+1] = 0;
		printf("%s\r\n", &buff[0]);
		++iLine;
		if (iLine >= 22)
		{
			SetXY(0,23);
			printf("ESC to cancel, any other key to continue...");
			ReadKbd(&KeyCode, 1);  /* ReadKbd, wait */
			if ((KeyCode & 0xff) == 0x1b)
			{
				return 4;
			}
			InitScreen(TRUE);
			SetXY(0,1);
			iLine =1;
		}
		if (cb > 15) cb-=16;
		else cb=0;
        addr+=j;
	}
	return erc;
}

/*********************************************************
  This sets up to call the dump routine for each page.
  This expects to find the filename in param[1].
*********************************************************/

long DoDump(void)
{
unsigned long j, k, l, erc, dret, fh;

erc = 0;
if ((apParam[1]) && (acbParam[1]))
{
	if (iLine >= 23) 
	{
		ScrollVid(0,1,80,23,1);
		SetXY(0,23);
	}
	erc = OpenFile(apParam[1], acbParam[1], ModeRead, 1, &fh);
	if (!erc) 
	{
		j=0;				/* file lfa */
		GetFileSize(fh, &k);
		while ((j<k) && (!erc))
		{
			FillData(Buffer, 512, 0);
			erc = ReadBytes (fh, Buffer, 512, &dret);
			if (k-j > 512)
				l = 512;
			else l=k-j;
			if (erc < 2)
				erc = Dump(Buffer, dret, j);
			j+=512;
		}
		CloseFile (fh);
	}
}
else
	printf("Filename not given\r\n");

return erc;
}

/*********************************************************
  This types a text file to the screen.
*********************************************************/

long DoType(char *pName, long cbName)
{
long i, j, lfa, erc, dret, fh, KeyCode;

erc = 0;
if ((pName) && (cbName)) 
{

	if (iLine >= 23)
	{
		ScrollVid(0,1,80,23,1);
		SetXY(0,23);
	}
	erc = OpenFile(pName, cbName, 0, 1, &fh);
	if (!erc) 
	{
		FillData(Buffer, 512, 0);
		dret = 1;
		while ((erc<2) && (dret)) 
		{
			GetFileLFA(fh, &lfa);
			erc = ReadBytes (fh, Buffer, 78, &dret);
			i = 1;
			while ((Buffer[i-1] != 0x0A) && (i < dret)) 
			{
				i++;
			}
			for (j=0; j<=i; j++) 
			{
				if ((Buffer[j] == 0x09) || (Buffer[j] == 0x0d) ||
                    (Buffer[j] == 0x0a))
                   Buffer[j] = 0x20;
			}
			if (dret)
			{
				PutVidChars(0, iLine, Buffer, i, NORMVID);
				iLine++;
			}
			SetXY(0,iLine);
			SetFileLFA(fh, lfa+i);
			if (iLine >= 22)
			{
				SetXY(0,23);
				printf("ESC to cancel, any other key to continue...");
				ReadKbd(&KeyCode, 1);  /* ReadKbd, wait */
				if ((KeyCode & 0xff) == 0x1b)
			      return(4);
				InitScreen(TRUE);
				SetXY(0,1);
				iLine =1;
			}
		}
		printf("\r\nError: %d\r\n", erc);
		CloseFile (fh);
	}
}
else
	printf("Filename not given\r\n");

return erc;
}

/*******************************************************
  This reads DOS FAT date & time and converts it into
  strings with the format  MM/DD/YY  HH/MM/SS.
  This is used by the directory listing.
********************************************************/
void CnvrtFATTime(U16 time,
				  U16 date,
				  char *pTimeRet,
				  char *pDateRet)
{
U32 yr,mo,da,hr,mi,se;
char st[15];

	yr = ((date & 0xFE00) >> 9) + 1980 - 1900;
	if (yr > 99) yr -=100;
	mo = (date & 0x01E0) >> 5;
	da = date & 0x001F;
	hr = (time & 0xF800) >> 11;
	mi = (time & 0x07E0) >> 5;
	se = (time & 0x001F) * 2;
	sprintf(st, "%02d:%02d:%02d",hr,mi,se);
	CopyData(st, pTimeRet, 8);
	sprintf(st, "%02d-%02d-%02d",mo,da,yr);
	CopyData(st, pDateRet, 8);

}

/*******************************************************
  Copy a single file with overwrite checking.
  Use block mode for speed. This uses a conservative
  4K buffer. THis could be made a good deal faster by
  allocating a large chunk of memory and using it
  instead of the 4K static buffer.
********************************************************/

U32 CopyFile(char *pFrom, U32 cbFrom, char *pTo, U32 cbTo)
{
U32 fhTo, fhFrom, bytesWant, bytesGot, bytesLeft, erc,
    bytesOut, size, dLFA, KeyCode;

	erc = OpenFile(pFrom, cbFrom, ModeRead, 0, &fhFrom);
	if (!erc)
	{
		/* Check to see if it exists already */

		erc = CreateFile(pTo, cbTo, 0);
		if ((!erc) || (erc==ErcDupName))
		{
			if (erc == ErcDupName)
			{
				printf("File already exists. Overwrite? (Y/N)\r\n");
				ReadKbd(&KeyCode, 1);  /* ReadKbd, wait */
				if (((KeyCode & 0xff) == 'Y') || ((KeyCode & 0xff) == 'y'))
				{
					erc = 0;
				}
			}

			if (!erc)
			{
		        erc = OpenFile(pTo, cbTo, ModeModify, 0, &fhTo);
				if (!erc)
				{
					/* both files are open in block mode */

					erc = GetFileSize(fhFrom, &size);
					if (!erc)
						erc = SetFileSize(fhTo, size);

					dLFA = 0;
					bytesLeft = size;
					while ((!erc) && (bytesLeft))
					{   if (bytesLeft >= 4096)
							bytesWant = 4096;
						else
							bytesWant = bytesLeft;
						if (bytesWant & 511)	/* handle last sector */
							bytesWant += 512;
						bytesWant = (bytesWant/512) * 512;
						erc = ReadBlock(fhFrom, bigBuf, bytesWant,
										dLFA, &bytesGot);
						if (bytesGot)
						{
							erc = WriteBlock(fhTo, bigBuf, bytesGot,
											 dLFA, &bytesOut);
							if (bytesLeft < bytesOut)
								bytesLeft = 0;
							else
								bytesLeft-=bytesOut;
						}
						dLFA += bytesGot;
					}
					CloseFile(fhTo);
				}
			}
		}
		CloseFile(fhFrom);
	}
	return(erc);
}

/*********************************************************
  Does a directory listing for param[1] or current path.
*********************************************************/

U32 DoDir(void)
{
U8 fDone;
U32 SectNum, erc, KeyCode, i;
char st[78];

if (iLine >= 23)
{
	ScrollVid(0,1,80,23,1);
	SetXY(0,23);
}
fDone = 0;
SectNum = 0;
while (!fDone)
{
	erc = GetDirSector(apParam[1], acbParam[1], &dirent[0], 512, SectNum);
	if (!erc) 
	{
		for (i=0; i<16; i++) 
		{
			if (!dirent[i].Name[0])
			{
				erc = 1;
				fDone = 1;
			}
			if ((dirent[i].Name[0]) && (dirent[i].Name[0] != 0xE5))
			{
				sprintf(st, "%8s %3s  %8d  xx/xx/xx xx/xx/xx  %2x   %04x\r\n",
			 			dirent[i].Name,
			 			dirent[i].Ext,
			 			dirent[i].FileSize,
			 			dirent[i].Attr,
						dirent[i].StartClstr);
				CnvrtFATTime(dirent[i].Time, dirent[i].Date, &st[33], &st[24]);
				printf("%s", st);
				iLine++;
				if (iLine >= 22)
				{
					SetXY(0,23);
					printf("ESC to cancel, any other key to continue...");
					ReadKbd(&KeyCode, 1);  /* ReadKbd, wait */
					if ((KeyCode & 0xff) == 0x1b)
					{
						erc = 4;
						fDone = TRUE;
					}
					InitScreen(TRUE);
					SetXY(0,1);
					iLine =1;
				}
			}
		}
	}
	else
		fDone = TRUE;
	SectNum++;
}

return(erc);
}

/*********************************************************
  This fills in the pointers to, and sizes of, each parameter
  found on the command line in the arrays apParams and
  acbParams.
*********************************************************/

void ParseCmdLine(void)
{
long iCmd, iPrm, i;		/* iCmd is index to aCmd */
	iCmd = 0;
	for (iPrm=0; iPrm<nParamsMax; iPrm++) 
	{
		acbParam[iPrm] = 0;		/* default the param to empty */
		apParam[iPrm] = 0;		/* Null the ptr */
		if (iCmd < cbCmd)
		{
			if (!isspace(aCmd[iCmd])) 
			{
				apParam[iPrm] = &aCmd[iCmd++];	/* ptr to param */
				i = 1;
				while ((iCmd < cbCmd) && (!isspace(aCmd[iCmd])))
				{
					i++;
					iCmd++;
				}
				acbParam[iPrm] = i;		/* size of param */
			}
			while ((iCmd < cbCmd) && (isspace(aCmd[iCmd])))
				iCmd++;
		}
	}
}

/*************************************************
  This opens and parses Commands.CLI in the system
  directory and looks for the command name in
  apParam[0]. If it finds it, it places the full
  filespec for the run file in "runfile" for the
  caller, then fixes up the aCmd command line
  just as if it had come from the user.
  The format for a line entry in Commands.CLI is:
  name fullspec param param param...
**************************************************/

long GetCLICommand(char *runfile)
{
long i, j, k;
FILE *f;
char rawline[90];	/* used to build a pseudo command line */
char cmdline[90];	/* used to build a pseudo command line */
char fdone;

	cmdline[0] = 0;

	f = fopen(cmdpath, "r");
	if (f)
	{
		fdone = 0;
		while (!fdone)
		{
			if (fgets(rawline, 89, f))
			{
				if (rawline[0] == ';')  /* a comment line */
					continue;
				j = CompareNCS(apParam[0], rawline, acbParam[0]);
				if (j == -1)
				{ 	/* we found a match for the command */
					/* Move the command name into the command line */
					i = 0; /* Index into rawline */
					j = 0; /* Index into cmdline we are building */
					k = 0; /* Index into runfile name we return to caller */
					while (!(isspace(rawline[i])))
						cmdline[j++] = rawline[i++];
					/* follwed by a space */
					cmdline[j++] = ' ';

					/* skip whitespace in rawline */
					while (isspace(rawline[i]))
						i++;

					/* now move runfile name */
					while (!(isspace(rawline[i])))
						runfile[k++] = rawline[i++];
					runfile[k] = 0; /* null terminte */

					/* skip whitespace in rawline */
					while (isspace(rawline[i]))
						i++;
					/* now move arguments if any, and LF */
					while (rawline[i])
						cmdline[j++] = rawline[i++];
					cmdline[j] = 0;

					/* Move cmd line we built to real cmd line */
					strcpy(aCmd, cmdline);
					cbCmd = strlen(aCmd);
					return(1);		/* tell em we got it! */
				}
			}
			else
				fdone = 1;
		}
	}
	else
		printf("Commands.CLI not found.\r\n");

	return(0);
}


/**************************************************
  When a command is specified to the CLI and it
  is not an internal command, we first look for
  a .RUN file in the current path. If we don't
  find one, we go to the system directory, if
  it's not there, we then go to COMMANDS.CLI
  and search it line by line to see if a run
  file is specified there. If so, we set the
  command line with everything after the runfile
  specified and try to execute it.
***************************************************/

void FindRunFile(void)
{
char runfile[80];
long i, erc, fh;

	/* Try to find in current path */

	CopyData(apParam[0], runfile, acbParam[0]);
	runfile[acbParam[0]] = 0;
	strcat(runfile, ".RUN");

	erc = OpenFile(runfile, strlen(runfile), ModeRead, 1, &fh);
	if (!erc)
	{			/* found a run file in sys directory */
		CloseFile(fh);
		SetCmdLine(aCmd, cbCmd);
		erc = Chain(runfile, strlen(runfile), 0);
	}
	else 	/* Try to find in System directory */
	{
		strcpy(runfile, syspath);
		i = strlen(runfile);
		CopyData(apParam[0], &runfile[i], acbParam[0]);
		runfile[acbParam[0]+i] = 0; /* null terminate */
		strcat(runfile, ".RUN");
		erc = OpenFile(runfile, strlen(runfile), ModeRead, 1, &fh);
		if (!erc)
		{			/* found a run file */
			CloseFile(fh);
			SetCmdLine(aCmd, cbCmd);
			erc = Chain(runfile, strlen(runfile), 0);
		}
        	 /* now we call GetCLICommand as a last resort */
		else if (GetCLICommand(runfile))
		{
			erc = OpenFile(runfile, strlen(runfile), ModeRead, 1, &fh);
			if (!erc)
			{			/* found a run file */
				CloseFile(fh);
				SetCmdLine(aCmd, cbCmd);
				erc = Chain(runfile, strlen(runfile), 0);
			}
		}
	}
	printf("Command not found\r\n");
}

/**********************************************
   Main function entry point.
   Note: No params to CLI.
***********************************************/

void main(void)
{
unsigned long erc, i, j, fh;

	erc = AllocExch(&StatExch);
	erc = AllocExch(&GPExch);

	SpawnTask(&StatTask, 24, 0, &StatStack[255], 0);

	GetJobNum(&JobNum);
	sprintf(&aStatLine[37], "%02d", JobNum);

	SetJobName("CLI V1.0", 8);

	/* Get system disk and set up path names for
	cli, command file, and help file.
	*/

	GetSystemDisk(&sdisk);
	sdisk &= 0x7F;
	sdisk += 0x41;		/* 0=A, 1=B, 2=C etc. */
	syspath[0] = sdisk;
	syspath[1] = ':';
	syspath[2] = 0;
	strcat(syspath, "\\MMSYS\\");

	strcpy(clipath, syspath);
	strcat(clipath,"CLI.RUN");

	strcpy(cmdpath, syspath);
	strcat(cmdpath,"COMMANDS.CLI");

	strcpy(hlppath, syspath);
	strcat(hlppath,"HELP.CLI");

	/* If a path was already set we assume that we are re-loading
	   after an external command has been run. We do not
	   want to reset the screen completely so the user can
	   see anything the external command left on the screen.
	*/

	cbPath = 0;
	GetPath(JobNum, aPath, &cbPath);
	if (cbPath)
	{
		GetXY(&iCol, &iLine);
		if (iLine == 0)	/* under status line */
			iLine = 1;
		InitScreen(FALSE);
		SetXY(0, iLine);
	}
	else
	{
		strcpy (aPath, syspath);
		cbPath = strlen(syspath);
		SetPath(syspath, strlen(syspath));
		InitScreen(TRUE);
	}

	fUpdatePath = 1;

	while (1)
	{
		FillData(aCmd, 79, ' ');
		aCmd[79] = 0;
		cbCmd = 0;
		SetXY(0, iLine);
		TTYOut (">", 1, NORMVID);
		EditLine(aCmd, cbCmd, 79, &cbCmd, &ExitChar, EDVID);
		TTYOut ("\r\n", 2, NORMVID);
		GetXY(&iCol, &iLine);

		acbParam[0] = 0;
		apParam[0] = 0;
        CmdNum = 0;

		if (ExitChar == 0x0d)
			ParseCmdLine();			/* set up all params */

		if ((acbParam[0]) && (apParam[0]))
		{	/* It's a command! */

			i = 1;
			while (i <= NCMDS)
			{
				j = strlen(paCmds[i]);
				if ((acbParam[0] == j) &&
					(CompareNCS(apParam[0],	paCmds[i], j) == -1))
				{
	                CmdNum = i;
	                break;
				}
				i++;
			}
			erc = 0;
			switch (CmdNum)
			{
				case EXTCMD:			/* external command */
					SetExitJob(clipath, strlen(clipath));
					FindRunFile();
					break;
				case CLSCMD:
					InitScreen(TRUE);
					break;
				case COPYCMD:
                    if ((acbParam[1]) && (acbParam[2]))
                    	erc = CopyFile(apParam[1], acbParam[1],
                                         apParam[2], acbParam[2]);
					else erc = ErcBadParams;
					break;
				case DELCMD:
					erc = OpenFile(apParam[1], acbParam[1], 1, 0, &fh);
					if (!erc)
						erc = DeleteFile(fh);
					if (!erc)
						printf("Done.\r\n");
					break;
				case DIRCMD:
					erc = DoDir();
					break;
				case DBGCMD:
					GoDebug();
					break;
				case DUMPCMD:
					erc = DoDump();
					break;
				case EXITCMD:
					SetExitJob("", 0);
					ExitJob(0);
					break;
				case HELPCMD:
					erc = DoType(hlppath, strlen(hlppath));
					break;
				case MAKEDIR:
					erc = CreateDir(apParam[1], acbParam[1]);
					break;
				case MONCMD:
					erc = Request("KEYBOARD", 4, GPExch, &GPHndl, 0,
									0, 0,   0, 0,   1, 0, 0);
					erc = WaitMsg(GPExch, GPMsg);
					SetVidOwner(1);
					break;
				case PATHCMD:
					erc = SetPath(apParam[1], acbParam[1]);
					if (!erc)
						erc = GetPath(JobNum, aPath, &cbPath);
					fUpdatePath = 1;
					break;
				case REMDIR:
					erc = DeleteDir(apParam[1], acbParam[1]);
					break;
				case RENCMD:
                    if ((acbParam[1]) && (acbParam[2]))
                    	erc = RenameFile(apParam[1], acbParam[1],
                                         apParam[2], acbParam[2]);
					else erc = ErcBadParams;
					break;
				case RUNCMD:
					if (acbParam[1])
					{
						i = 2;
						while (aCmd[i] != ' ')
							i++;
						SetCmdLine(&aCmd[i], cbCmd-i);
						SetExitJob(clipath, strlen(clipath));
						erc = Chain(apParam[1], acbParam[1], 0);
					}
					break;
				case TYPECMD:
					erc = DoType(apParam[1], acbParam[1]);
					break;
				default:
					break;
			}
			CheckErc(erc);
		}

		GetXY(&iCol, &iLine);
		if (iLine >= 23)
		{
			ScrollVid(0,1,80,23,1);
			SetXY(0,23);
		}
	}
}
