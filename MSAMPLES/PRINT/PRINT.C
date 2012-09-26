/* A simple program that prints a single file, directly using
   the Parallel Device Driver in MMURTL (Device No. 3 "LPT").
   Print does perform some formatting, but it's crude.

	Print File, Version 1.0
	Usage: Print Filename /1 /2 /4 /8 /F /D /B
		/1 /2 /4 /8 - Tab stop translation value
		/F  no FormFeed at end of file
		/D  Display file while printing
		/B  Binary print. NO translation, no FF

	To build Print.run, Use Makeit.bat in DOS, or use CM32 and DASM:
	CM32 Print.C
	DASM Print.ATF
*/


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "\OSSource\MDevDrv.h"
#include "\OSSource\MJob.h"
#include "\OSSource\MKbd.h"
#include "\OSSource\MTimer.h"
#include "\OSSource\MVid.h"
#include "\OSSource\Parallel.h"

#define     FF            0x0C
#define     LF            0x0A
#define     CR            0x0D
#define     TAB           0x09

unsigned long key;

long tabstops = 4;
long NoFF = 0;
long fDisplay = 0;
long fBinary = 0;
long col = 0;

char name[80];
FILE *f;

struct statRecL lpt;

/*****************************************************/

void main(long argc, unsigned char *argv[])
{
long      erc, erck, i, cl;
unsigned char b, lastb;
char          fdone, *ptr;

	SetJobName("Printing", 8);

	name[0] = 0;

	for(i=1; i < argc; ++i) 	/* start at arg 1 */
	{
		ptr = argv[i];
		if (*ptr == '/')
		{
			ptr++;
			switch(*ptr)
		    {
				case '1' :			/* Tab Translation Width */
				case '2' :
				case '4' :
				case '8' :
					tabstops = *ptr - 0x30;
					break;
				case 'F' :			/* No FF at end of file */
				case 'f' :
					NoFF = 1;
					break;
				case 'D' :			/* Display while printing */
				case 'd' :
					fDisplay = 1;
					break;
				case 'B' :			/* BINARY - No translation at all! */
				case 'b' :
					fBinary = 1;
					break;
				default:
					printf("Invalid switch");
					exit(1);
					break;
		 	}
		}
		else if(!name[0])
			strncpy (name, argv[i], 79);
	}

	if (!name[0])
	{
		/* Input file not explicitly named errors out */

		printf("Print File, Version 1,0\r\n");
		printf("Usage: Filename /1 /2 /4 /8 /F /D /B\r\n");
		printf("/1 /2 /4 /8 - Tab stop translation value\r\n");
		printf("/F  no FormFeed at end of file\r\n");
		printf("/D  Display file while printing\r\n");
		printf("/B  Binary print. NO translation, no FF\r\n\n");
		printf("Error: Source filename required\r\n");
		exit(1);
	}

	/* Get the 64 byte device status block which is specific to the
	   parallel device driver. The structure is defined in parallel.h
	   We do this just to see if it's a valid device.
	*/

	erc = DeviceStat(3, &lpt, 64, &i);

	if (erc)
	{
	 	printf("Error getting LPT Device Status: %d\r\n", erc);
		ExitJob(erc);
	}

	/* If device status went OK, we open the printer port */

                /* device, dOpNum,   dLBA, dnBlocks, pData */
	erc = DeviceOp(3,      CmdOpenL, 0,    0,        &i);

	if (erc)
	{
	    printf("OpenLPT ERROR: %d \r\n", erc);
	    ExitJob(erc);
	}

    printf("Printing %s ...\r\n", name);

    /* This is it... */

	f = fopen(name, "r");

	if (!f)
	{
                /* device, dOpNum,   dLBA, dnBlocks, pData */
		erc = DeviceOp(3,  CmdCloseLU, 0,    0,        &i);
	 	printf("Can't open: %s\r\n", name);
		ExitJob(erc);
	 }

	col = 0;
	i = 0;
	b = 0;
	fdone = 0;

	while ((!fdone) && (!erc))
	{
		i++;
		cl = fgetc(f);
		lastb = b;
		b = (cl & 0xff);

		if (cl == EOF)
		{
			fdone = 1;
		}
		else if (fBinary)
		{
			erc = DeviceOp(3, CmdWriteB, 0,  1, &lastb);
		}
		else
		{
			switch (b)
			{			/* print/translate the char */
				case CR:
					erc = DeviceOp(3, CmdWriteB, 0,  1, &b);
					break;
				case LF:
					if (lastb != CR)
					{
						lastb = CR;
						erc = DeviceOp(3, CmdWriteB, 0,  1, &lastb);
					}
					erc = DeviceOp(3, CmdWriteB, 0,  1, &b);
					if (fDisplay)
					 	printf("\r\n", lastb);
					col = 0;  /* reset */
					break;
				case TAB:
					do
					{
						erc = DeviceOp(3, CmdWriteB, 0,  1, " ");
						col++;
						if (fDisplay)
						 	printf(" ");
					} while (col % tabstops);
					break;
				default:
					if (fDisplay)
					 	printf("%c", b);
					col++;
					erc = DeviceOp(3, CmdWriteB, 0,  1, &b);
						if (erc)
						 	printf("Error Writing Byte: %d\r\n", erc);
					break;
			}
		}

		if (i%100==0)		/* every 100 chars see if they want to abort */
		{
			erck = ReadKbd(&key, 0);
			/* no wait */
			if (!erck)
			{
				if (key & 0xff == 0x1b)
				{
					fdone = 1;
					erc = 4;
				}
			}
		}
	}

	if ((!fBinary) && (!NoFF))
	{
		erc = DeviceOp(3, CmdWriteB, 0,  1, "\f");
	}

	fclose(f);
		         /* device, dOpNum, dLBA, dnBlocks, pData */
	erc = DeviceOp(3,  CmdCloseL,   0,   0,        &i);
	if (erc)
	 	printf("Can't close LPT. Error: %d\r\n", erc);
 	printf("Done\r\n");
	ExitJob(erc);

}
