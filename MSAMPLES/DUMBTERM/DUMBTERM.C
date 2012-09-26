/* Possibly the dumbest terminal program in existance... */
/* Simply here to demo the MMURTL device drive interface for comms */
/* Build with MakeIT.Bat, use CM32 and DASM separately:
   CM32 DumbTerm.C
   DASM DumbTerm.ATF
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "\OSSOURCE\MDevDrv.h"
#include "\OSSOURCE\MJob.h"
#include "\OSSOURCE\MKbd.h"
#include "\OSSOURCE\MTimer.h"
#include "\OSSOURCE\MVid.h"
#include "\OSSOURCE\RS232.h"

#define NORMVID BRITEWHITE|BGBLUE
#define CLIVID WHITE|BGBLACK

unsigned long key;

struct statRecC com;

/*****************************************************/
/*****************************************************/
/*****************************************************/
/*****************************************************/

void    main(void)

{

int           erc, i;
unsigned char b, lastb;
char          fOK;


SetNormVid(NORMVID);
ClrScr();

printf("     Terminally DUMB, Dumb Terminal Program\r\n");
printf("      (MMURTL Comms Device Driver demo) \r\n");

/* Get the 64 byte device status block which is specific to the
RS-232 device driver. The structure is defined in commdrv.h
*/

 erc = DeviceStat(6, &com, 64, &i);

 if (erc) 
 {
	SetNormVid(CLIVID);
	ClrScr();
 	printf("Error on Device Stat: %d\r\n", erc);
	ExitJob(erc);
 }

  /* set the params in the block */

  com.Baudrate = 9600;
  com.parity = NO_PAR;
  com.databits = 8;
  com.stopbits = 1;

/* View other params which we could set, but should already be
   defaulted with standard values when driver was initialized.
*/

  printf("IRQNum: %d\r\n", com.IRQNum);
  printf("IOBase: %d\r\n", com.IOBase);
  printf("sXBuf:  %d\r\n", com.XBufSize);
  printf("sRBuf:  %d\r\n", com.RBufSize);
  printf("RTimeO: %d\r\n", com.RTimeOut);
  printf("XTimeO: %d\r\n", com.XTimeOut);

/* Set the params we changed with a DeviceInit */

 erc = DeviceInit(6, &com, 64);
 if (erc) 
 {
	SetNormVid(CLIVID);
	ClrScr();
 	printf("Error on Device Init: %d\r\n", erc);
	ExitJob(erc);
 }

   /* If device init went OK, we open the comms port */


                /* device, dOpNum,   dLBA, dnBlocks, pData */
	erc = DeviceOp(6,      CmdOpenC, 0,    0,        &i);

	if (erc) 
	{
		SetNormVid(CLIVID);
		ClrScr();
	    printf("OpenCommC ERROR: %d \r\n", erc);
	    ExitJob(erc);
	}

    printf("Communications Port Initialized.\r\n");

	fOK = 1;

    /* This is it... */

	while (fOK) 
	{

		if (!ReadKbd(&key, 0))  	/* no wait */
		{
			b = key & 0x7f;

			if (key & 0x3000) 
			{		/* ALT key is down */
				switch (toupper(b)) 
				{
					case 'Q' :
			         /* device, dOpNum,   dLBA, dnBlocks, pData */
						erc = DeviceOp(6,    CmdCloseC, 0,  0,  &i);
						SetNormVid(CLIVID);
						ClrScr();
						ExitJob(erc);
						break;
					default: break;
				}
			}
			else 
			{
                /* device, dOpNum,   dLBA, dnBlocks, pData */
				erc = DeviceOp(6,    CmdWriteB, 0,  0,  &b);
				if (erc)
				    printf("WriteByteCError: %d \r\n", erc);
				else 
				{
                    if (b == 0x0D) 
                    {
	                    b = 0x0A;
						erc = DeviceOp(6,    CmdWriteB, 0,  0,  &b);
					}
				}
		    }
		}
           /* device, dOpNum,   dLBA, dnBlocks, pData */
		erc = DeviceOp(6,    CmdReadB, 0,  0,  &b);
		if (!erc) 
		{
				TTYOut (&b, 1, NORMVID);
				/* add a LF if it's not there after a CR... */
				if ((lastb == 0x0D) && (b != 0x0A))
					TTYOut ("\n", 1, NORMVID);
				lastb = b;
		}
	}

}
