/*  Centronics Parallel Device Driver (1 channel v1.0). */
/*  Copyright 1991,1992,1993,1994 R.A. Burgess	*/
/*  ALL RIGHTS RESERVED */

/* This driver is NOT interrupt driven because documentation
   on interrupt usage with the parallel I/O device is sketchy at best..
   We compensate for this by creating a separate task that is a
   loop which statuses and continues to try to send all the data
   without eating too much processor bandwidth.
*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char
#define TRUE   1
#define FALSE  0

#include "parallel.h"

/* MMURTL OS Prototypes */

extern far U32 AllocExch(U32 *pExchRet);

extern far U32 InitDevDr(U32 dDevNum,
				    	  S8  *pDCBs,
					  	  U32 nDevices,
					  	  U32 dfReplace);


extern far U32 UnMaskIRQ(U32 IRQNum);
extern far U32 MaskIRQ(U32 IRQNum);
extern far U32 SetIRQVector(U32 IRQNum, S8  *pIRQ);
extern far U32 EndOfIRQ(U32 IRQNum);
extern far U32 SendMsg(U32 Exch, U32 msg1, U32 msg2);
extern far U32 ISendMsg(U32 Exch, U32 msg1, U32 msg2);
extern far U32 WaitMsg(U32 Exch, S8  *pMsgRet);
extern far U32 CheckMsg(U32 Exch, S8  *pMsgRet);
extern far U32 GetTimerTick(U32 *pTickRet);
extern far U32 Alarm(U32 Exch, U32 count);
extern far U32 KillAlarm(U32 Exch);
extern far U32 Sleep(U32 count);
extern far void MicroDelay(U32 us15count);
extern far void OutByte(U8 Byte, U16 wPort);
extern far U8 InByte(U16 wPort);
extern far void CopyData(U8 *pSource, U8 *pDestination, U32 dBytes);

extern far SpawnTask(S8  *pEntry,
		             U32 dPriority,
                     U32 fDebug,
                     S8  *pStack,
           		     U32 fOSCode);

extern far long GetJobNum(long *pJobNumRet);

/* local prototypes. These will be called form the device driver interface. */

static U32 lptdev_stat(U32  dDevice,
                       S8  *pStatRet,
                       U32 dStatusMax,
                       U32 *pdStatusRet);

static S32 lptdev_init(U32  dDevice,
                       S8  *pInitData,
                       U32  sdInitData);

static U32 lptdev_op(U32 dDevice,
                     U32 dOpNum,
                     U32 dLBA,
                     U32 dnBlocks,
                     U8  *pData);


/* The following definitions are used to identify, set
   and reset signal line conditions and functions.
*/

#define SSENDBUF 4096       /* 4K Send Buf */

unsigned char SendBuf[SSENDBUF];

static U32  xmit_timeout = 100; /* 10ms intervals - 1 second */

static U32  head_send;  /* Next char to send */
static U32  tail_send;  /* Where next char goes in buf */
static U32  cSendBuf;   /* Count of bytes in buf */
static U32  sSendBuf;   /* Size of buffer (allocated) */
static U32  burstcount;  /* for burst of chars to lpt */
static U32  strobecount; /* tries to strobe the char out */

static U8   control_byte = 0;

 /* Control, Data & Status Registers for port */

static U16      DAT;        /* Data output register */
static U16      STA;        /* Status Register */
static U16      STC;        /* Status/Control Register */

static U32 lptStk[200];  /* 800 byte stack for this task */
static U32 lptStkTop;

/* Complete description of Register bits follows:

    DAT -- Bits 0-7 of data output pins

                7 6 5 4 3 2 1 0
                 \_\_\_\_\_\_\_\__ Data out Bits

    STA -- Status Register
                1 1 0 1 1 1 1 1
                7 6 5 4 3 2 1 0
                | | | | |  \_\_\_ Not Used
                | | | |  \_______ Error Status (Input (P15)
                | | |  \_________ Select Status (Input P13)
                | |  \___________ Out of Paper Status (Input P12)
                |  \_____________ Acknoledge Status (Input P10)
                 \_______________ Busy Status (Input P11)


    STC -- Status/Control Register

                7 6 5 4 3 2 1 0
                | | | | | | |  \_ Strobe Inverted (Input/Output P1)
                | | | | | |  \___ AutoFeed Inverted (Input/Output P14)
                | | | | |  \_____ Initialize (Input/Output P16)
                | | | |  \_______ Select Inverted (Input/Output P17)
                | | |  \_________ Status IRQ Enable (Input)
                 \_\_\___________ Not Used

*/


/* Record for 64 byte status and init record */
/* This structure is peculiar to the lpt driver */

#define sStatus 64

static struct statRecL lptstat;
static struct statRecL *pPS;

static struct dcbtype
{
    S8   Name[12];
    S8   sbName;
    S8   type;
    S16  nBPB;
    U32  last_erc;
    U32  nBlocks;
    S8  *pDevOp;
    S8  *pDevInit;
    S8  *pDevSt;
    S8   fDevReent;
    S8   fSingleUser;
    S16  wJob;
    U32  OS1;
    U32  OS2;
    U32  OS3;
    U32  OS4;
    U32  OS5;
    U32  OS6;
    };

static struct dcbtype lptdcb;		/* One parallel port */


/********************************************************************
 This small function becomes a thread (task) to feel the printer.
 It checks to see if there is data in the buffer, and if so it
 sends the dat out in bursts of characters.  It also properly sets
 the status byte in the lptstat block so proper errors can be
 returned to callers of the device driver.
**********************************************************************/

static void lpt_task(void)
{
	while (1)
	{
		/* Get lpt status every	half second even if no data
		is in buffer for those that may want it.
		*/

		control_byte = InByte(STA);  /* Get status Byte from STA */
		lptstat.status = control_byte;

		if (cSendBuf)
		{
            burstcount = 10;

			while ((cSendBuf) && (burstcount--))
			{
				/* see if port is busy. If so, sleep 20ms, else send burst */

				control_byte = InByte(STA);  /* Get status Byte from STA */
				lptstat.status = control_byte;

				if (control_byte & LPTBUSY)
			    {
#asm
	CLI
#endasm
    			    OutByte(SendBuf[tail_send], DAT);  /* Send the byte */
	    		    if (++tail_send == sSendBuf)
	        			tail_send = 0;
			        --cSendBuf;
#asm
	STI
#endasm
			        OutByte(0x0d, STC); /* Strobe High */
			        OutByte(0x0c, STC); /* Strobe Low */
				}
			    else
					Sleep(2);
			}
			Sleep(1);  /* eliminate busyloop... */
		}
		else
			Sleep(30);	/* sleep for a .3 seconds */
	}
}

/*********************************************************
    This is called ONCE to initialize the 1 default
    lpt channel with the OS device driver interface.
*********************************************************/

U32  lpt_setup(void)
{
U32  erc;

  /* first we set up the DCB in anticipation of calling InitDevDr */

    lptdcb.Name[0]  = 'L';
    lptdcb.Name[1]  = 'P';
    lptdcb.Name[2]  = 'T';
    lptdcb.sbName   = 3;
    lptdcb.type     = 2;			/* Sequential */
    lptdcb.nBPB     = 1;			/* 1 byte per block */
    lptdcb.nBlocks  = 0;			/* 0 for Sequential devices */
    lptdcb.pDevOp   = &lptdev_op;
    lptdcb.pDevInit = &lptdev_init;
    lptdcb.pDevSt   = &lptdev_stat;

    /* Set default lpt params in stat records */

    lptstat.XTimeOut = 100;	/* 1 second */
    lptstat.IOBase = 0x378;
    lptstat.IRQNum = 7;			/* Not used right now */
    lptstat.XBufSize = 4096;
    sSendBuf = 4096;

	DAT = lptstat.IOBase;		/* Data output register */
	STA = lptstat.IOBase +1;	/* Status Register */
	STC = lptstat.IOBase +2;	/* Status/Control Register */

	cSendBuf = 0;
	head_send = 0;
	tail_send = 0;

    OutByte(0x08, STC); /* Reset (Init) Line Low */
	MicroDelay(100);  /* 1500us ought to do it */
    OutByte(0x0C, STC); /* No ints, No AutoLF, Init High */

	erc = SpawnTask( &lpt_task, 19, 0, &lptStkTop, 1);
	if (erc)
		return(erc);

    return(erc = InitDevDr(3, &lptdcb, 1, 1));
}

/********************************************/
static long WriteByteL(unsigned char b)
{

U32 erc, counter;
U8 *pXBuf;

	erc = 0;
	counter = lptstat.XTimeOut;		/* set up for timeout */

	while (cSendBuf == sSendBuf)
	{
		Sleep(1);
		counter--;
		if  (!counter)
			return (ErcXmitTimeoutL);		/* never got sent */
	}

#asm
	CLI
#endasm

    SendBuf[head_send] = b;
    if (++head_send == sSendBuf)
         head_send  = 0;
	++cSendBuf;				/* one more in buf */

#asm
	STI
#endasm
	return (erc);
}


/********************************************/
static long WriteRecordL(unsigned char *pSendData,
                         unsigned int cbSendData)
{
int erc;

	erc = 0;
    while ((cbSendData) && (!erc))
    {
		erc = WriteByteL(*pSendData++);
		--cbSendData;
	}
	return (erc);
}

/********************************************
 This allocates a buffer for use driver use.
*********************************************/

static U32  OpenLPT(void)

{
U32  erc, Job;
U16  port_base;
U8   c;

	GetJobNum(&Job);

	if (lptstat.lptJob)
	{
		if (Job != lptstat.lptJob)
			return(ErcChannelOpenL);	/* differnet job */
		else
			return(0); /* same job - already open */
	}

	lptstat.lptJob = Job;

	/* Set up buffer variables for this job */

	if (!cSendBuf)
	{
		cSendBuf = 0;
		head_send = 0;
		tail_send = 0;
	}
   	port_base = lptstat.IOBase;

	DAT = port_base;		/* Data output register */
	STA = port_base +1;		/* Status Register */
	STC = port_base +2;		/* Status/Control Register */

	return (0);
}

/********************************************
 This closes the port, sets the owner to 0
 and deallocates the buffers.  If there is
 still data to send, this diables ints,
 kills the buffer, then closes it.
********************************************/

static int  CloseLPT (int fAbort)
{
U32 erc, Job;

	GetJobNum(&Job);

	if (lptstat.lptJob)
	{
		if (Job != lptstat.lptJob)
			return(ErcNotOwnerL);	/* differnet job */
		else
			return(0); /* same job - already open */
	}
	else
		return(ErcNotOpenL);	/* Ports not open! */

	if (fAbort)
	{
		cSendBuf = 0;
		head_send = 0;
		tail_send = 0;
	}

	lptstat.lptJob = 0;
	return(0);
}

/***************************************************************************
Now begins the PUBLIC routines that are interfaced to for all DEVICE DRIVERS
****************************************************************************/

/******************************************
Called for all device operations.  This
assigns physical device from logical number
that outside callers use. For RS-232, 5=0
and 6=1.
*******************************************/

static U32 lptdev_op(U32 dDevice,
 		    U32 dOpNum,
		    U32 dLBA,
		    U32 dnBlocks,
		    U8  *pData)
{
U32 erc;
U32 Job;
U8 c;

	GetJobNum(&Job);

	if ((!lptstat.lptJob) && (dOpNum != CmdOpenL))
		return(ErcNotOpenL);

	if (lptstat.lptJob)
	{
		if ((lptstat.lptJob != Job) &&
			(Job != 1))
			return(ErcNotOwnerL);
	}

	erc = 0;		/* default error */

	switch(dOpNum)
	{
		case(0):
			break;				/* Null Command */
		case CmdWriteB:
			erc = WriteByteL(*pData);
			break;
		case CmdWriteRec:
			erc = WriteRecordL(pData, dnBlocks);
			break;
		case CmdSetXTO:
			lptstat.XTimeOut = dLBA;		/* 10ms intervals */
			break;
		case CmdOpenL:
			erc =  OpenLPT();
			break;
		case CmdCloseL:
			erc =  CloseLPT(0);
			break;
		case CmdCloseLU:
			erc =  CloseLPT(1);
			break;
		default:
			erc = ErcBadOpNum;		/* default error */
			break;
	}

	lptstat.LastErc = erc;
	return(erc);
}


/******************************************
Called for status report on lpt channel.
Returns 64 byte block for channel specified.
This is called by the PUBLIC call DeviceStat
*******************************************/

static U32 lptdev_stat(U32  dDevice,
			  S8  *pStatRet,
			  U32 dStatusMax,
			  U32 *pdStatusRet)
{
U32 i;

	if (dStatusMax > 64)
	 	i = 64;
	else
	    i = dStatusMax;

    lptstat.BufCnt = cSendBuf;

	CopyData(&lptstat, pStatRet, i);		/* copy the status data */
	*pdStatusRet = dStatusMax;		/* give em the size returned */
	return(0);
}

/******************************************
Called to set parameters for the lpt
channel prior to opening or while in use.
If an invalid value is passed in, all params
remain the same as before.
This is called by the PUBLIC call DeviceInit.
Only the timeout value may be changed while
the port is open.
*******************************************/

static S32 lptdev_init(U32  dDevice,
			 S8  *pInitData,
			 U32  sdInitData)

{
U32  erc, Xbufsize,	XTO, job;
U16  port_base;
U8   IRQNUM;

	erc = 0;

	GetJobNum(&job);
	if ((lptstat.lptJob) && (lptstat.lptJob != job))
		return(ErcNotOwnerL);   /* Port is in use, and not by you! */

	if (sdInitData < 40)
		return(ErcBadInitSizeL);

	pPS = pInitData;

	/* Get the callers new params into local vars */

	XTO       = pPS->XTimeOut;	/* Non Volatile */
   	port_base = pPS->IOBase;

	/* Volatile params can not change while port is open. */

	if (lptstat.lptJob)		/* Port is in use */
	{
    	if (lptstat.IOBase != port_base)
    		erc = ErcChannelOpenL;
	}

	/* Non Volatile params can be set whether or not the
	   channel is open. */

    if (!XTO)  XTO = 100;
	lptstat.XTimeOut = XTO;

    if (!port_base)
   		return (ErcBadIOBaseL);

	lptstat.IOBase = port_base;
	DAT = lptstat.IOBase;		/* Data output register */
	STA = lptstat.IOBase +1;	/* Status Register */
	STC = lptstat.IOBase +2;	/* Status/Control Register */

	/* If in use and no data in buf, or no one is using
	   lpt channel then do a HARD reset on it.
	*/

	if ( ((lptstat.lptJob) && (!cSendBuf)) || (!lptstat.lptJob) )
	{
	    OutByte(0x08, STC); /* Reset Line Low */
		MicroDelay(100);	/* 1.5 ms ought to do it */
	    OutByte(0x0C, STC); /* No ints, No AutoLF, Init High */
	}

	erc = 0;
	return(erc);
}
