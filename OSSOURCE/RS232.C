/*  RS232 Device Driver (2 channel v1.0).
    Next version adds flow control XON/XOFF & CTS/RTS 	*/

/*  Copyright 1991,1992,1993,1994 R.A. Burgess	*/


#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char
#define TRUE   1
#define FALSE  0

#include "RS232.h"

/* MMURTL OS Prototypes */

extern far U32 AllocExch(U32 *pExchRet);

extern far U32 InitDevDr(U32 dDevNum,
				    	  S8  *pDCBs,
					  	  U32 nDevices,
					  	  U32 dfReplace);

extern far U32 AllocOSPage(U32 nPages, U8 **ppMemRet);
extern far U32 DeAllocPage(U8 *pOrigMem, U32 nPages);

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

extern far long GetJobNum(long *pJobNumRet);

/* local prototypes. These will be called form the device driver interface. */

static U32 comdev_stat(U32  dDevice,
			  S8  *pStatRet,
			  U32 dStatusMax,
			  U32 *pdStatusRet);

static S32 comdev_init(U32  dDevice,
			 S8  *pInitData,
			 U32  sdInitData);

static U32 comdev_op(U32 dDevice,
 		    U32 dOpNum,
		    U32 dLBA,
		    U32 dnBlocks,
		    U8  *pData);


#define CmdReadRec   1				/* Read one or more bytes */
#define CmdWriteRec  2				/* Write one or more bytes */

#define CmdOpenC    10				/* Open Comm Channel */
#define CmdCloseC   11				/* Close Comm Channel */
#define CmdDiscardRcv 12
#define CmdSetRTO	13
#define CmdSetXTO	14
#define CmdSetDTR   15				/* Set DTR (On) */
#define CmdSetRTS	16				/* Set CTS (On) */
#define CmdReSetDTR 17				/* Set DTR (On) */
#define CmdReSetRTS 18				/* Set CTS (On) */
#define CmdBreak	19
#define CmdGetDC	20
#define CmdGetDSR	21
#define CmdGetCTS	22
#define CmdGetRI	23

#define CmdReadB	31
#define CmdWriteB	32

/* The following definitions are used to identify, set
   and reset signal line condition and functions.
*/

#define CTS         0x10		/* Clear To Send       */
#define DSR         0x20		/* Data Set Ready      */
#define RI          0x40		/* Ring Indicator      */
#define CD          0x80		/* Carrier Detect      */

#define DTR         0x01		/* Data Terminal Ready */
#define RTS         0x02		/* Request To Send     */
#define OUT2		0x08		/* Not used */

/* Values from IIR register */

#define MDMSTAT		0x00
#define NOINT		0x01
#define TXEMPTY 	0x02
#define RVCDATA 	0x04
#define RCVSTAT		0x06

/* For errors returned from Comms calls see COMMDRV.H */

#define SSENDBUF 4096		/* 1 Page Send Buf Default */
#define SRECVBUF 4096		/* 1 Page Recv Buf Default */

static U32  recv_timeout[2] = 1;	/* 10ms intervals */
static U32  xmit_timeout[2] = 10;	/* 10ms intervals */

/* variables for ISRs */

static U8 	f16550[2];
static U8 	stat_byte[2];
static U8 	mstat_byte[2];
static U8	int_id[2];
static U8	fExpectInt[2];

static U32 recv_error[2]; 	/* NON-ZERO = an error has occurred */

static U8   *pSendBuf[2];	/* pointers to Xmit bufs */
static U32  head_send[2];	/* Next char to send */
static U32  tail_send[2];	/* Where next char goes in buf */
static U32  cSendBuf[2];	/* Count of bytes in buf */
static U32  sSendBuf[2];	/* Size of buffer (allocated) */

static U8   *pRecvBuf[2];	/* pointers to Recv bufs */
static U32  head_recv[2];	/* Next char from chip */
static U32  tail_recv[2];	/* Next char to read for caller */
static U32  cRecvBuf[2];	/* Count of bytes in buf */
static U32  sRecvBuf[2];	/* Size of buffer (allocated) */

static U8   control_byte[2] = 0;

 /* array of registers from port base for each channel */

static U16      THR[2];		/* Transmitter Holding Register */
static U16      IER[2];		/* Interrupt Enable Register */
static U16      IIR[2];		/* Interrupt Id Register */
static U16      FCR[2];		/* FIFO control for 16550 */
static U16      LCR[2];		/* Line Control Register */
static U16      MCR[2];		/* Modem Control Register */
static U16      LSR[2];		/* Line Status Register */
static U16      MSR[2];		/* Modem Status Register */
static U16      DLAB_LO[2];	/* same address as THR  */
static U16      DLAB_HI[2];   /* same address as IER  */

/* Complete description of Register bits follows:

	THR -- TX data, RX data, Divisor Latch LSB

	IER -- Interrupt Enable, Divisor Latch MSB

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \_ Data Available
			    | | | | | |  \___ Xmit Holding Reg Empty
			    | | | | |  \_____ Receiver Line Status
			    | | | |  \_______ Modem Status
			    \ _ _ _\_________ Always 0000

	IIR -- Interrupt Identification Register

			    7 6 5 4 3 2 1 0
			    | | | | \_\_\_\__________ 0001 = no interrupt
			    | | | | 			0110 = rcvr status
			    | | | | 			0100 = rcvd data
			    | | | | 			0010 = THR empty
			    | | | | 			0000 = Modem status
			    | | | | 			1101 = Rcv Fifo Timeout (16550)
			    | | |  \_________ = 0.
			    | |  \___________ = 0.
			     \ \_____________ = (16550 only)
			                        00 = FIFOs disabled
			                        non-zero = 16550 enabled

	FCR -- FIFO Control Register (16550 only)
			This is a write only port at the same
			address as the IIR on an 8250/16450

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \__= 1 = FIFO Enable
			    | | | | | |  \___ = 1 = Recv FIFO Reset
			    | | | | |  \_____ =	1 = Xmit FIFO Reset
			    | | | |  \_______ = (DMA) 0 = Single char, 1 = Multichar
			    | | |  \_________ = 0.
			    | |  \___________ = 0.
			    \__\_____________ = FIFO Rcv Trigger Level
			    					00 - 1 char
			    					01 - 2 chars
			    					10 - 4 chars
			    					11 - 8 chars

	LCR --  Line Control Register

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \_ Word Length Select Bit 0.
			    | | | | | |  \___ Word Length Select Bit 1.
			    | | | | |  \_____ Number Stop Bits (0=1, 1=2)
			    | | | |  \_______ Parity Enable
			    | | |  \_________ Even Parity Select
			    | |  \___________ Stick Parity
			    |  \_____________ Set Break
			     \_______________ Divisor Latch Access Bit

	MCR -- Modem Control Register

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \_ Data Terminal Ready
			    | | | | | |  \___ Request to Send
			    | | | | |  \_____ Out 1
			    | | | |  \_______ Out 2  (= 1 to enable ints.)
			    | | |  \_________ Loop
			    \ _ _\___________ = Always 0

	LSR -- Line Status Register

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \_ Data Ready
			    | | | | | |  \___ Overrun Error
			    | | | | |  \_____ Parity Error
			    | | | |  \_______ Framing Error
			    | | |  \_________ Break interrupt
			    | |  \___________ Transmitter Holding Reg Empty
			    |  \_____________ Transmitter Shift Reg Empty
			     \_______________ Recv FIFO Error (16550 Only)

	MSR -- Modem Status Register

			    7 6 5 4 3 2 1 0
			    | | | | | | |  \_ Delta Clear to Send
			    | | | | | |  \___ Delta Data Set Ready
			    | | | | |  \_____ Trailing Edge Ring Indicator
			    | | | |  \_______ Delta Rx Line Signal Detect
			    | | |  \_________ Clear to Send
			    | |  \___________ Data Set Ready
			    |  \_____________ Ring Indicator
			     \_______________ Receive Line Signal Detect

*/


/* Record for 64 byte status and init record */
/* This structure is peculiar to the comms driver */

#define sStatus 64

static struct statRecC comstat[2];
static struct statRecC *pCS;

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

static struct dcbtype comdcb[2];		/* Two RS-232 ports */


/* THE COMMS INTERRUPT FUNCTION PROTOTYPES */

static void interrupt comISR0(void);
static void interrupt comISR1(void);


/*********************************************************
    This is called ONCE to initialize the 2 default
    comms channels with the OS device driver interface.
    It sets up defaults for both channels to 9600, 8N1.
*********************************************************/

U32  coms_setup(void)
{
U32  erc;

  /* first we set up the 2 DCBs in anticipation of calling InitDevDr */

	comdcb[0].Name[0]  = 'C';
	comdcb[0].Name[1]  = 'O';
	comdcb[0].Name[2]  = 'M';
	comdcb[0].Name[3]  = '1';
	comdcb[0].sbName   = 4;
	comdcb[0].type     = 2;			/* Sequential */
	comdcb[0].nBPB     = 1;			/* 1 byte per block */
	comdcb[0].nBlocks  = 0;			/* 0 for Sequential devices */
	comdcb[0].pDevOp   = &comdev_op;
	comdcb[0].pDevInit = &comdev_init;
	comdcb[0].pDevSt   = &comdev_stat;

	comdcb[1].Name[0]  = 'C';
	comdcb[1].Name[1]  = 'O';
	comdcb[1].Name[2]  = 'M';
	comdcb[1].Name[3]  = '2';
	comdcb[1].sbName   = 4;
	comdcb[1].type     = 2;			/* Sequential */
	comdcb[1].nBPB     = 1;			/* 1 byte per block */
	comdcb[1].nBlocks  = 0;			/* 0 for Sequential devices */
	comdcb[1].pDevOp   = &comdev_op;
	comdcb[1].pDevInit = &comdev_init;
	comdcb[1].pDevSt   = &comdev_stat;

	/* Set default comms params in stat records */

	comstat[0].Baudrate = 9600;
	comstat[0].parity   = 0;	/* none */
	comstat[0].databits = 8;
	comstat[0].stopbits = 1;
	comstat[0].XTimeOut = 100;
	comstat[0].RTimeOut = 2;
	comstat[0].IOBase = 0x3F8;
	comstat[0].IRQNum = 4;
	comstat[0].XBufSize = 4096;
	comstat[0].RBufSize = 4096;
	sSendBuf[0] = 4096;
	sRecvBuf[0] = 4096;

	comstat[1].Baudrate = 9600;
	comstat[1].parity   = 0;	/* none */
	comstat[1].databits = 8;
	comstat[1].stopbits = 1;
	comstat[1].XTimeOut = 100;
	comstat[1].RTimeOut = 2;
	comstat[1].IOBase = 0x2F8;
	comstat[1].IRQNum = 3;
	comstat[1].XBufSize = 4096;
	comstat[1].RBufSize = 4096;
	sSendBuf[1] = 4096;
	sRecvBuf[1] = 4096;

	MaskIRQ(4);
	MaskIRQ(3);

	SetIRQVector(4, &comISR0);	/* COM1 */
	SetIRQVector(3, &comISR1);	/* COM2 */

	return(erc = InitDevDr(5, &comdcb, 2, 1));

}

/*********************************************************
  This does the grunt work for each of the two ISRs
  This MUST remain reentrant for two ISRs.
*********************************************************/

static void handleISR(U32 i)		/* i is the device */
{
U8 *pRBuf, *pXBuf;

pRBuf = pRecvBuf[i];
pXBuf = pSendBuf[i];

while (TRUE) 
{
	int_id[i] = InByte (IIR[i]);		/* Get ID Byte from IIR */
	switch (int_id[i]) 
	{
		case RCVSTAT:
		    stat_byte[i] = InByte(LSR[i]);  /* clear error conditions */
			break;

		case RVCDATA:
	       	if (cRecvBuf[i] == sRecvBuf[i]) 
	       	{		/* Overflow!! */
				recv_error[i] = ErcRcvBufOvr;		/* Do not put in buf */
	    	    InByte (THR[i]);					/* trash the byte */
			}
			else 
			{
#asm
	CLI
#endasm
		        pRBuf[head_recv[i]] =
		        	InByte (THR[i]);	/* Get the byte */
				++cRecvBuf[i];
    	    	if (++head_recv[i] == SRECVBUF)
	        	    head_recv[i] = 0;
#asm
	STI
#endasm
				recv_error[i] = 0;
		    }
    	    break;

		case TXEMPTY:
#asm
	CLI
#endasm
	        if (cSendBuf[i]) 
	        {
    		    OutByte(pXBuf[tail_send[i]], THR[i]);  /* Send the byte */
	    	    if (++tail_send[i] == sSendBuf[i])
	        		tail_send[i] = 0;
		        --cSendBuf[i];
				fExpectInt[i] = TRUE;
			}
			else
				fExpectInt[i] = FALSE;
#asm
	STI
#endasm
		break;

		case MDMSTAT:
	    mstat_byte[i] = InByte (MSR[i]);   /* Get Modem Status */
		break;

		case NOINT:
	    stat_byte[i] = InByte(LSR[i]);  /* clear error conditions */
		default:
    	return;
	}
}
}


static void interrupt comISR0(void)
{
;		/* ; needed if asm is first in function */
#asm
	STI
#endasm
	handleISR(0);
	EndOfIRQ(4);
	return;
}


static void interrupt comISR1(void)
{
;		/* ; needed if asm is first in function */
#asm
	STI
#endasm
	handleISR(1);
	EndOfIRQ(3);
	return;
}

/********************************************/


static long ReadByteC(U32 device, unsigned char *pByteRet)
{
U32 counter;
U8 *pRBuf;

	pRBuf = pRecvBuf[device];
	if (recv_error[device]) return (recv_error[device]);

	if (cRecvBuf[device]) 
	{
        *pByteRet = pRBuf[tail_recv[device]];
        if (++tail_recv[device] == sRecvBuf[device])
        	tail_recv[device] = 0;
       	--cRecvBuf[device];
		return (0);
	}

	counter = comstat[device].RTimeOut;		/* set up for timeout */
	while (counter--) 
	{
		Sleep(1);
		if (cRecvBuf[device]) 
		{
	        *pByteRet = pRBuf[tail_recv[device]];
	        if (++tail_recv[device] == sRecvBuf[device])
	        	tail_recv[device] = 0;
        	--cRecvBuf[device];
			return (0);
		}
	}
	return (ErcRecvTimeout);
}

/********************************************/


static long ReadRecordC(U32 device,
				unsigned char *pDataRet,
                unsigned int  sDataMax,
                unsigned int *pcbRet)
{
int  erc, cb;

	erc = 0;
	cb = 0;
	while ((cb < sDataMax) && (!erc)) 
	{
		erc = ReadByteC(device, pDataRet++);
		if (!erc) ++cb;
	}
	*pcbRet = cb;		/* tell em how many bytes */
    return (erc);
}



/********************************************/
static long WriteByteC(U32 device, unsigned char b)
{

U32 erc, counter;
U8 *pXBuf;

	erc = 0;
	pXBuf = pSendBuf[device];
	counter = comstat[device].XTimeOut;		/* set up for timeout */

	while (cSendBuf[device] == sSendBuf[device]) 
	{
		Sleep(1);
		counter--;
		if  (!counter)
			return (ErcXmitTimeout);		/* never got sent */
	}

#asm
	CLI
#endasm

	if (!fExpectInt[device])
	{			/* Xmit buf empty, send ourself */
        OutByte(b, THR[device]);
		fExpectInt[device] = TRUE;
		}
	else 
	{
        pXBuf[head_send[device]] = b;
        if (++head_send[device] == sSendBuf[device])
            head_send[device]  = 0;
		++cSendBuf[device];				/* one more in buf */
	}
#asm
	STI
#endasm
	return (erc);
}


/********************************************/
static long WriteRecordC(U32 device,
				 unsigned char *pSendData,
                 unsigned int cbSendData)
{
int erc;

	erc = 0;
    while ((cbSendData) && (!erc)) 
    {
		erc = WriteByteC(device, *pSendData++);
		--cbSendData;
	}
	return (erc);
}


/********************************************/

static long DiscardRecvC(U32 device)
{
U32	saveto, erc;
U8  b;

	saveto = comstat[device].RTimeOut;
	comstat[device].RTimeOut = 1;
	erc = 0;
	while (!erc)
		erc = ReadByteC(device, &b);
	comstat[device].RTimeOut = saveto;
	return (0);
}



/********************************************
 This sets comms params prior to opening, or
 while a channel is in use.
********************************************/

static U32 SetParams(U32 device)

{
U32  divisor, speed;
U8   c, parity, bits, stop_bit, temp;

	parity = comstat[device].parity;
	bits = comstat[device].databits;
	stop_bit = comstat[device].stopbits;
	speed = comstat[device].Baudrate;

	/* Set up baud rate */

    divisor = 115200/speed;

#asm
	CLI
#endasm
    c=InByte (LCR[device]);
    OutByte  ((c | 0x80), LCR[device]);
    OutByte  ((divisor & 0x00ff), DLAB_LO[device]);
    OutByte  (((divisor>>8) & 0x00ff), DLAB_HI[device]);
    OutByte  (c, LCR[device]);
#asm
	STI
#endasm

	/* set coms params */

    temp = bits - 5;
    temp |= ((stop_bit == 1) ? 0x00 : 0x04);

    switch (parity)
    {
       case NO_PAR : temp |= 0x00; break;
       case OD_PAR : temp |= 0x08; break;
       case EV_PAR : temp |= 0x18; break;
    }

#asm
	CLI
#endasm
    OutByte (temp, LCR[device]);
#asm
	STI
#endasm

	return (0);
}


/********************************************
 This allocates buffers, sets up the ISR
 and IRQ values and open the channel for use.
*********************************************/

static U32  OpenCommC(U32 device)

{
U32  erc;
U16  port_base;
U8   c;

	if (comstat[device].commJob)
		return(ErcChannelOpen);

	GetJobNum(&comstat[device].commJob);

	erc = AllocOSPage(comstat[device].XBufSize/4096,
					  &pSendBuf[device]);

	if (!erc) 
	{
		erc = AllocOSPage(comstat[device].RBufSize/4096,
						  &pRecvBuf[device]);

		if (erc)  /* get rid of Xmit buf if we can't recv */
            DeAllocPage(pSendBuf[device],
            			comstat[device].XBufSize/4096);
	}

	if (erc)
	{
		comstat[device].commJob = 0;
		return (erc);
	}

   	port_base = comstat[device].IOBase;

	/* Set up buffer variables for this port */

	cSendBuf[device] = 0;
	head_send[device] = 0;
	tail_send[device] = 0;

	cRecvBuf[device] = 0;
	head_recv[device] = 0;
	tail_recv[device] = 0;
	recv_error[device] = 0;


    THR[device]     = port_base;
    IER[device]     = port_base + 1;
    IIR[device]     = port_base + 2;
    FCR[device]     = port_base + 2;
    LCR[device]     = port_base + 3;
    MCR[device]     = port_base + 4;
    LSR[device]     = port_base + 5;
    MSR[device]     = port_base + 6;
    DLAB_HI[device] = port_base + 1;
    DLAB_LO[device] = port_base;

    InByte(THR[device]);	/* reset any pending ints on chip */
    InByte(LSR[device]);

#asm
	CLI
#endasm
	control_byte[device] = RTS | DTR | OUT2;
    OutByte(control_byte[device], MCR[device]);	/* Mod Ctrl Reg   */
    OutByte(0x0F, IER[device]);					/* Int Enable Reg */

	/* See if we have a 16550 and set it up if we do!! */

    OutByte(0x03, FCR[device]);
    c = InByte(IIR[device]);
	if (c & 0xC0) 		/* we have a 16550 and it's set to go! */
		f16550[device] = 1;
	else
		f16550[device] = 0;		/* 8250 or 16450 */

#asm
	STI
#endasm

	SetParams(device);

	UnMaskIRQ(comstat[device].IRQNum);
	return (0);
}

/********************************************
 This closes the port, sets the owner to 0
 and deallocates the buffers.
********************************************/

static int  CloseCommC (U32 device)
{
U32 erc;

	MaskIRQ(comstat[device].IRQNum);
    OutByte(0, MCR[device]);
    OutByte(0, IER[device]);
    erc = DeAllocPage(pSendBuf[device],
           			comstat[device].XBufSize/4096);
    erc = DeAllocPage(pRecvBuf[device],
           			comstat[device].RBufSize/4096);
	comstat[device].commJob = 0;
    return (erc);
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

static U32 comdev_op(U32 dDevice,
 		    U32 dOpNum,
		    U32 dLBA,
		    U32 dnBlocks,
		    U8  *pData)
{
U32 erc;
U32 Job, device;
U8 c;

/* Set internal drive number */
/* 5	RS-232 1	COM1	(OS built-in) */
/* 6	RS-232 2	COM2	(OS built-in) */

	if (dDevice == 5)
		device = 0;
	else
	 	device = 1;

	GetJobNum(&Job);

	if ((!comstat[device].commJob) && (dOpNum != CmdOpenC))
		return(ErcNotOpen);

	if (comstat[device].commJob) 
	{
		if ((comstat[device].commJob != Job) &&
			(Job != 1))
			return(ErcNotOwner);
	}

	erc = 0;		/* default error */

	switch(dOpNum) 
	{

		case(0):
			break;				/* Null Command */
		case CmdReadB:
			erc = ReadByteC(device, pData);
			break;
		case CmdWriteB:
			erc = WriteByteC(device, *pData);
			break;
		case CmdReadRec:
			erc = ReadRecordC(device, pData, dnBlocks,
						&comstat[device].LastTotal);
			break;
		case CmdWriteRec:
			erc = WriteRecordC(device, pData, dnBlocks);
			break;
		case CmdSetRTO:
			comstat[device].RTimeOut = dLBA;		/* 10ms intervals */
			break;
		case CmdSetXTO:
			comstat[device].XTimeOut = dLBA;		/* 10ms intervals */
			break;
		case CmdOpenC:
			erc =  OpenCommC(device);
			break;
		case CmdCloseC:
			erc =  CloseCommC(device);
			break;
		case CmdDiscardRcv:
			erc = DiscardRecvC(device);
			break;
		case CmdSetDTR:
			control_byte[device] |= DTR;
		    OutByte(control_byte[device], LCR[device]);
			break;
		case CmdSetRTS:
			control_byte[device] |= RTS;
			OutByte(control_byte[device], MCR[device]);
			break;
		case CmdReSetDTR:
			control_byte[device] &= ~DTR;
			OutByte(control_byte[device], LCR[device]);
		case CmdReSetRTS:
			control_byte[device] &= ~RTS;
		    OutByte(control_byte[device], MCR[device]);
		    break;
		case CmdBreak:
			c = InByte(LCR[device]);
			OutByte((c | 0x40), LCR[device]);
			Sleep(dLBA);
			OutByte(c, LCR[device]);
			break;
		case CmdGetDC:
		    *pData = mstat_byte[device] & CD;
			break;
		case CmdGetDSR:
	        *pData = mstat_byte[device] & DSR;
			break;
		case CmdGetCTS:
			*pData = mstat_byte[device] & CTS;
			break;
		case CmdGetRI:
			*pData = mstat_byte[device] & RI;
			break;
		default:
			break;
	}

	comstat[device].LastErc = erc;
	return(erc);
}


/******************************************
Called for status report on coms channel.
Returns 64 byte block for channel specified.
This is called by the PUBLIC call DeviceStat
*******************************************/

static U32 comdev_stat(U32  dDevice,
			  S8  *pStatRet,
			  U32 dStatusMax,
			  U32 *pdStatusRet)
{
U32 i, device;

	/* Set internal device number */
	if (dDevice == 5)
		device = 0;
	else device = 1;

	if (dStatusMax > 64)
	 	i = 64;
	else
	    i = dStatusMax;

	if (!device)
	{
		CopyData(&comstat[0], pStatRet, i);		/* copy the status data */
	}
	else
	{
		CopyData(&comstat[1], pStatRet, i);		/* copy the status data */
	}

	*pdStatusRet = dStatusMax;		/* give em the size returned */

	return(0);
}

/******************************************
Called to set parameters for the comms
channels prior to opening or while in use.
If an invalid value is passed in, all params
remain the same as before.
Some comms channel params may not be changed
while the channel is in use.
This is called by the PUBLIC call DeviceInit.
*******************************************/

static S32 comdev_init(U32  dDevice,
			 S8  *pInitData,
			 U32  sdInitData)

{
U32  erc, Xbufsize, Rbufsize, device;
U32  speed, XTO, RTO;
U16  port_base;
U8   parity, bits, stop_bit, IRQNUM;

	if (dDevice == 5)
 		device = 0;   			/* Set internal device number */
	else
	 	device = 1;

	if (sdInitData < 40)
		return(ErcBadInitSize);

	pCS = pInitData;

	/* Get the callers new params */

	speed     = pCS->Baudrate;	/* Non Volatile */
	parity    = pCS->parity;  	/* Non Volatile */
	bits      = pCS->databits;	/* Non Volatile */
	stop_bit  = pCS->stopbits;	/* Non Volatile */
	XTO       = pCS->XTimeOut;	/* Non Volatile */
	RTO       = pCS->RTimeOut;	/* Non Volatile */

   	port_base = pCS->IOBase;
	Xbufsize  = pCS->XBufSize;
	Rbufsize  = pCS->RBufSize;
	IRQNUM    = pCS->IRQNum;

	/* Non Volatile params can be set whether or not the
	   channel is open. Do these first and return errors. */

    if ((speed > MAX_BAUD) || (speed < MIN_BAUD))
       return (ErcBadBaud);

    if ((parity < NO_PAR) || (parity > OD_PAR))
    	return (ErcBadParity);

    if ((bits < 5) || (bits > 8))
    	return (ErcBadDataBits);

    if ((stop_bit < 1) || (stop_bit > 2))
    	return (ErcBadStopBits);

    if (!XTO)  XTO = 1;
    if (!RTO)  RTO = 1;

	comstat[device].Baudrate = speed;
	comstat[device].parity   = parity;
	comstat[device].databits = bits;
	comstat[device].stopbits = stop_bit;
	comstat[device].XTimeOut = XTO;
	comstat[device].RTimeOut = RTO;

	/* If we got here, the params are OK.  Now we check
	   to see if the channel is open and call SetParams
	   if so.  The channel is open if the JobNumber
	   in the commstat record is NON-ZERO.
	*/

	if (comstat[device].commJob)
	{  /* Channel Open! */
		SetParams(device);
	}

	/* Channel is not open so we check and set rest of params */

	else
	{

	    if (!port_base)
    		return (ErcBadIOBase);
	    if (IRQNUM < 3)
    		return (ErcBadCommIRQ);

		/* We now round up buffer sizes to whole pages */

		Xbufsize = Xbufsize/4096 * 4096;
		if (Xbufsize % 4096) Xbufsize+=4096; /* another page */


		Rbufsize = Rbufsize/4096 * 4096;
		if (Rbufsize % 4096) Rbufsize+=4096; /* another page */


		comstat[device].IOBase = port_base;
		comstat[device].IRQNum = IRQNUM;
		comstat[device].XBufSize = Xbufsize;
		comstat[device].RBufSize = Rbufsize;

		/* Local copies so we don't work from a structure in ISR */

		sSendBuf[device] = Xbufsize;	/* Size of buffer (allocated) */
		sRecvBuf[device] = Rbufsize;	/* Size of buffer (allocated) */

		erc = 0;
	}

 return(erc);
}
