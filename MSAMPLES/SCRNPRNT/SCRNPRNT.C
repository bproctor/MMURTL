/* This is a screen print program for MMURTL. It runs
   like any other application on the system except it
   looks for a Global Hot key (CTRL-ALT-PrintScreen)
   and prints the REAL screen to LPT1 or a file.

   The screen print is straight text with CR/LF at the
   end of each line. Nulls and non-printable characters
   are converted to spaces.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "\OSSource\MKernel.h"
#include "\OSSource\MJob.h"
#include "\OSSource\MVid.h"
#include "\OSSource\MData.h"
#include "\OSSource\MTimer.h"
#include "\OSSource\MDevDrv.h"
#include "\OSSource\Parallel.h"

#define ErcOK         0

struct RqBlkType *pRqBlk;       /* A pointer to a Reqeust Block */
unsigned long AnExch;  			/* Where we wait for Requests */
unsigned long Message[2];       /* The Message with the Request */
long rqHndlG;					/* Used for global print request */
long rqHndlK;					/* Used for keyboard request */
long cbFilename;
char Filename[80];
char fToFile = 0;
long ourJob, printJob;

FILE *fh;

long keycode, keycodeG;

unsigned char buf[4160];		/* for a snapshot of the screen */

/***************************************************
   Copies active screen memory to a working buffer.
   and beeps to let user know it's been done.
***************************************************/
void CaptureScreen(void)
{
 CopyData(0xB8000, buf, 4000);
 Tone(1000,15);		/* 1000 Hz for 100ms */
 Tone(300,15);		/* 300  Hz for 100ms */
}


/***************************************************
   This filles all nulls and NON ASCII chars in
   the capture buffer with spaces.
****************************************************/

ProcessScreen(void)
{
long i;
	for (i=0; i<4000; i+=2)
	{
		if ((buf[i] < 0x20) || (buf[i] > 0x7E))
			buf[i] = ' ';
	}
}

/***************************************************
   Sends the contents of the processed screen buffer
   to the file the user specified. LF is added to
   each line.
****************************************************/

void ScreenToFile(FILE *fh)
{
long i, j;
	for (i=0; i<25; i++)
	{
		for (j=0; j<160; j+=2)
		{
			fputc(buf[(i*160)+j], fh);
		}
		fputc(0x0A, fh);
	}
}


/***************************************************
   Sends the contents of the processed screen buffer
   to the LPT1 device. CR/LF is added to each line.
****************************************************/

void ScreenToLPT(void)
{
long erc;
long i, j;


	/* If device status went OK, we open the printer port */

                /* device, dOpNum,   dLBA, dnBlocks, pData */
	erc = DeviceOp(3,      CmdOpenL, 0,    0,        &i);

	if (erc)
	{
	    printf("OpenLPT ERROR: %d \r\n", erc);
		return;
	}

	for (i=0; i<25; i++)
	{
		for (j=0; j<160; j+=2)
		{
			erc = DeviceOp(3, CmdWriteB, 0,  1, &buf[(i*160)+j]);
			if (erc)
				break;
		}
		erc = DeviceOp(3, CmdWriteB, 0,  1, "\r");
		erc = DeviceOp(3, CmdWriteB, 0,  1, "\n");
		if (erc)
			break;
	}
	erc = DeviceOp(3, CmdWriteB, 0,  1, "\f");

	if (erc)
	 	printf("Can't write to LPT. Error: %d\r\n", erc);

		         /* device, dOpNum, dLBA, dnBlocks, pData */
	erc = DeviceOp(3,  CmdCloseL,   0,   0,        &i);
	if (erc)
	 	printf("Can't close LPT. Error: %d\r\n", erc);

}


/***************************************************
   Displays the menu after a user does something
****************************************************/

void menu(void)
{
	printf("CTRL-ALT-PrintScrn will print the screen from any job you're viewing.\r\n");
	printf("Press:\r\n");
	printf("   1) To direct screen prints to LPT1/Printer (default).\r\n");
	printf("   2) To direct screen prints to a file.\r\n");
	printf("   3) To terminte the screen print utility.\r\n\n");
}

/***************************************************
   Main entry point for SPU.
****************************************************/

void main(long argc, char *argv[])
{
unsigned long OSError, x, y;
unsigned char c, ExitChar;

	GetJobNum(&ourJob);
	SetJobName("ScreenPrint", 11);

	FillData(Filename, 80, ' ');
	cbFilename = 0;
	fToFile = 0;

	OSError = AllocExch(&AnExch);	  /* get an exchange */

	if (OSError)                      /* look for a kernel error */
		ExitJob(OSError);

	SetNormVid(WHITE|BGBLACK);
	ClrScr();

	printf("Screen Print Utility installed as job number: %d.\r\n\n", ourJob);

	/* Leave a global request for a key with the keyboard service */

	OSError = Request("KEYBOARD", 2, AnExch, &rqHndlG, 0, &keycodeG,
   					  4, 0, 0, 0, 0, 0);
	if (OSError)
		printf("Error %d on Global Keyboard Request\r\n", OSError);

	OSError = Request("KEYBOARD", 1, AnExch, &rqHndlK, 0, &keycode,
  					  4, 0, 0, 1, 0, 0); /* 1 in dData0 = Wait for key */

	if (OSError)
		printf("Error %d on ReadKey Request\r\n", OSError);

	menu();

	while(1)
	{
		/* We now have two requests waiting at the keyboard service.
		   One is for the global key, the other is for a ReadKbd.
		   When we Wait(), we will get one or the other back. We find
		   out which one by checking the handle to see which it matches.
		   Eaither someone wants the screen printed or the user
		   has selcted a menu item.
		 */

	    OSError = WaitMsg(AnExch, Message); /* Exch & pointer */

	    if (!OSError)
	    {

			if (Message[0] == rqHndlK)  /* it was a menu item */
			{

				c = keycode & 0x7f;	/* lop off upper stuff in keycode */
				switch (c)
				{
					case '1':
                        fToFile = 0;
						printf("Screen print directed to printer (LPT1)\r\n");
						strcpy(Filename, "LPT");
						cbFilename = 3;
						break;
					case '2':
						printf("\r\nEnter filename to print to: ");
						GetXY(&x, &y);
						EditLine(Filename, cbFilename, 50, &cbFilename,
								&ExitChar, BLACK|BGWHITE);
						if ((ExitChar == 0x0d) && (cbFilename))
						{
                            Filename[cbFilename] = 0; /* null terminte */
	                        fToFile = 1;
						}
						else
	                        fToFile = 0;
						printf("\r\n");
						break;
					case '3':
						/* cancel the global key request */
						OSError = Request("KEYBOARD", 3, AnExch, &rqHndlG, 0, 0,
   								  0, 0, 0, ourJob, 0, 0);
					    OSError = WaitMsg(AnExch, Message);
						DeAllocExch(AnExch);
						exit(0);
						break;
				}
				/* leave another keyboard request */
				OSError = Request("KEYBOARD", 1, AnExch, &rqHndlK, 0, &keycode,
  							  4, 0, 0, 1, 0, 0); /* 1 in dData0 = Wait for key */
				menu();

			}
			else if (Message[0] == rqHndlG) /* It's a screen print!! */
			{
				if ((keycodeG & 0xff) == 0x1C) /* it's us! */
				{
					CaptureScreen();
					ProcessScreen();
					if (fToFile)
					{
						fh = fopen(Filename, "a");
						if (fh)
						{
							ScreenToFile(fh);
							fclose(fh);
						}
						else
							printf("Can't open file: %s\r\n", Filename);
					}
					else
						ScreenToLPT();

					GetVidOwner(&printJob);
					if (fToFile)
						printf("Screen print sent to %s for job %d\r\n\n",
								Filename, printJob);
					else
						printf("Screen print sent to LPT1 for job %d\r\n\n",
								printJob);
					menu();
				}

				/* Leave another global request with the keyboard service */

				OSError = Request("KEYBOARD", 2, AnExch, &rqHndlG, 0, &keycodeG,
   								  4, 0, 0, 0, 0, 0);

			}
		}
		else
			printf("Error %d on WaitMsg from kernel\r\n", OSError);
  	}  /* Loop while(1) */
}
