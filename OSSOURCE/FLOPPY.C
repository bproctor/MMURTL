/* Floppy.C  Floppy Disk Device Driver for MMURTL */

/*
  MMURTL Operating System Source Code
  Copyright 1991,1992,1993,1994 Richard A. Burgess
  ALL RIGHTS RESERVED   Version 1.0
*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

/* MMURTL OS PROTOTYPES */

extern far SpawnTask(S8  *pEntry,
		             U32 dPriority,
                     U32 fDebug,
                     S8  *pStack,
           		     U32 fOSCode);

extern far U32 AllocExch(U32 *pExchRet);

extern far U32 AllocDMAPage(U32 nPages, U8 **ppMemRet U32 *pPhyMemRet);

extern far U32 InitDevDr(U32 dDevNum,
				    	  S8  *pDCBs,
					  	  U32 nDevices,
					  	  U32 dfReplace);

extern far DmaSetUp(S8  *pPhyMem,
					U32 sdMem,			/* size */
					U32 dChannel,		/* channel 2 floppy */
					U32 dType,			/* 0=Verfify, 1=IN,  2=OUT */
					U32 dMode);		/* FDC uses 1 (single cycle) */

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
extern far void OutWord(U16 Word, U16 wPort);
extern far U8 InByte(U16 wPort);
extern far U16 InWord(U16 wPort);
extern far U8 ReadCMOS(U16 Address);
extern far void CopyData(U8 *pSource, U8 *pDestination, U32 dBytes);


/* THE FLOPPY INTERRUPT FUNCTION PROTOTYPE */

static void interrupt fdisk_isr(void);

/*  PROTOTYPE FOR Floppy motor control task and associated stuff */

static void fdmotor_task(void);
static void fd_select(U32 drive);
static void fd_motoroff(U32 drive);

/* THE REST OF THE PROTOTYPES */

U32 fdisk_setup(void);
static U32 RdWrtVerf(U32 op);
static U32 format_track(void);
static U32 Set_Media(U32 drive, U32 type);
static U32 FDC_reset(void);
static U8  cmos_type (U8 drive_nr);
static U32 send_fdc(U8 parm);
static U8  GetParm(U8 index);
static U32 wait_int (void);
static U32 seek(void);
static U32 recal(void);
static U32 read_data(U8 *pDataRet);
static U32 results(U32 expect);
static void purge_fdc (void);
static void wait_for_head(void);
static U32 med_change(void);
static U32 get_fdc_status(void);

/* The following 3 calls are required in every MMURTL device driver */

static U32 dev_op(U32 dDevice,
 		    U32 dOpNum,
		    U32 dLBA,
		    U32 dnBlocks,
		    S8  *pData);

static U32 dev_stat(U32 dDevice,
			  S8 * pStatRet,
			  U32 dStatusMax,
			  U32 *pdSatusRet);

static U32 dev_init(U32 dDevNum,
			  S8  *pInitData,
			  U32  sdInitData);

/* Near External for troubleshooting */

extern long xprintf(char *fmt, ...);

/* LOCAL DEFINITIONS */

#define ok    0
#define TRUE  1
#define FALSE 0

#define RATE_500 		0x00
#define RATE_300 		0x01
#define RATE_250 		0x02
#define RATE_1000       0x03
#define INT_FLAG 		0x80

/* Error Codes to return */

#define ErcNotInstalled 504
#define ErcAddrMark		602
#define ErcReadOnly		603
#define ErcSectNotFound	604
#define ErcNewMedia		605
#define ErcNotMounted	606
#define ErcCRC			607
#define ErcBadFDC		608
#define ErcBadSeek		609
#define ErcFDCTimeOut	610
#define ErcOverRun		611
#define ErcBadLBA		612
#define ErcDriveType	613
#define ErcBadOp		614
#define ErcBadRecal		615
#define ErcSendFDC		616
#define ErcResults		617
#define ErcBadCmd		618
#define ErcReadyLine	619

/* Commands accepted by driver */

#define CmdNull     0
#define CmdRead     1
#define CmdWrite    2
#define CmdVerify   3
#define CmdFmtBlk   4
#define CmdFmtTrk   5
#define CmdSeekTrk  6

/* FDC port definitions */

#define DOR_PORT	0x3f2
#define MSR_PORT	0x3f4
#define DATA_PORT	0x3f5
#define DIR_PORT	0x3f7
#define DRR_PORT	0x3f7

/* FDC Return Status bit definitions */

#define BUSY  		0x10	/* was BIT4 */
#define DSKCHANGE_BIT 0x80
#define BIT7 		0x80
#define BIT6 		0x40
#define BIT5 		0x20
#define BIT4 		0x10
#define BIT3 		0x08
#define BIT2 		0x04
#define BIT1 		0x02
#define BIT0 		0x01

#define RQM			0x80
#define DIO			0x40

/* FDC commands */

#define FDC_READ	0xe6
#define FDC_WRITE	0xc5
#define FDC_FORMAT	0x4d

/* FDC DOR register bits */

#define FD_MOTOR0	0x10
#define FD_MOTOR1   0x20
#define FD_INTS		0x08
#define FD_RESET	0x04
#define FD_DRV1SEL	0x01
#define FD_MOTMASK	0xf0		/* mask to see motor bits */

/* L O C A L   C O N S T A N T S */

/* The drive table contains parameters for each disk
   type.  The values are:
   0  - FDC SPECIFY Command byte 1
   1  - FDC SPECIFY Command byte 2
   2  - Unused
   3  - Bytes per Sector (coded 0=128, 1=256, 2=512, 3=1024);
   4  - number of sectors per track (Last sector)
   5  - Intersector Gap Size
   6  - Data Length (FFh = 512)
   7  - GAP 3 for Format Command
   8  - Fill Byte for Format command
   9  - Head settle time in milliseconds
   10 - Motor start time in milliseconds/10 (mult value by 10 to use)
   11 -	max cylinder index (number of cyls - 1)
   12 -	Xfer rate Command
   13 - Unused
   14 - Double Step Flag (e.g., 360K in a 1.2Mb drive)
   15 - Unused
*/


static U8 fdisk_table[5][16]= {

 /* 0 = NO DRIVE - first two params set to allow FDC_reset */
  {0x0af,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0},

 /* 1 = 360 kb drive */
 {0x0af, 2, 0, 2, 9, 0x2a, -1, 0x50, 0x0f6, 15, 8, 39, RATE_250, 0, 0, 0},

 /* 2 = 1.2 mb drive */
 {0xaf, 2, 0, 2, 15, 0x1b, -1, 0x54, 0x0f6, 15, 8, 79, RATE_500, 0, 0, 0},

 /* 3 = 720 type drive */
 {0x0af, 2, 0, 2, 9, 0x2a, -1, 0x50, 0x0f6, 15, 8, 79, RATE_250, 0, 0, 0},

 /* 4 = 1.44 mb drive */
 {0xaf, 2, 0, 2, 18, 0x1b, -1, 0x6c, 0x0f6, 15, 8, 79, RATE_500, 0, 0, 0},

};

static U32 MotorStk[100];  /* 400 byte stack for motor task */
static U32 MotorStkTop;

static U8 fd_fdc_command;
static U8 fd_drive;
static U8 fd_nr_sectors;
static U8 fd_head;
static U8 fd_sector;
static U8 fd_track;
static U8 seek_status;
static U8 fwrite;
static S8  *fd_pData;

/* current fdisk_table[x] for drive 0 & 1 */

static U8 type0
static U8 type1

/* Record for 64 byte status and init record */

#define sStatus 64

static struct statstruct{
  U32 erc;
  U32 blocks_done;
  U32 BlocksMax;
  U8 fNewMedia;
  U8 type_now;		/* current fdisk_table for drive selected */
  U8 resvd1[2];		/* padding for DWord align  */
  U32 nCyl;			/* total physical cylinders */
  U32 nHead;		/* total heads on device    */
  U32 nSectors;		/* Sectors per track        */
  U32 nBPS;			/* Number of bytes per sect */
  U8 params[16]; 	/* begin device specific fields */
  U8 STATUS[8];		/* status returned from FDC (for user status) */
  U32 resvd3;
  U32 resvd4;		/* 64 bytes total */
  };

static struct statstruct fdstatus;
static struct statstruct FDStatTmp;

static U8 FDC_STATUS[8];	/* status returned from FDC */
static U8 LAST_TRACK[3];  /* holds last track number */

static struct dcbtype {
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

static struct dcbtype fdcb[2];		/* two floppy device control blcocks */


/* Exch, msgs space, and vars for FD Motor task */
static U8 dor_crnt;		/* last value sent to DOR port */

static U8 motor0_want;		/* desired motor0 state, TRUE = want ON */
static U8 motor1_want;		/* desired motor1 state, TRUE = want ON */
static U32 fd_tick;		/* Set to tick everytime we select a floppy */
static U32 fd_newtick;		/* used to check tick time */

/* Exch and msgs space for FD ISR */
static U32 fd_exch;
static U32 fd_msg[2];

static U32 rgSectorMax[10] = {0, 720, 2400, 1440, 2880,
						0, 0, 0, 0, 0};		/* set max sectors */

static U8 *sectbuf;		/* sector buffer, 1 Page with AllocDMAPage */
static U32 physectbuf;	/* physical address of DMA buffer */

/*======================================================*/
/*=================== START OF CODE ====================*/
/*======================================================*/

static void enable_ints(void)
{
;
#asm
	STI
#endasm
}

static void disable_ints(void)
{
;
#asm
	CLI
#endasm
}


/********************************************************************
 This small function becomes a thread (task) to control the motor.
 The floppy motors are controlled serarately from the rest
 of the floppy drive electronics. A single port controls both
 floppies.  After a floppy is used you don't want to shut it right
 off or you can forget about any dreams of decent throughput.
 This wakes up every 3 seconds and checks to see if there was
 any floppy activity in the past 3 seconds. If not, we shut off
 the motor(s).
**********************************************************************/

static void fdmotor_task(void)
{
	enable_ints();

MotorLoop:

	Sleep(300);							/* 3 seconds */

	GetTimerTick(&fd_newtick);

	if ((fd_newtick - fd_tick) > 300) {	/* not used in last 3 seconds */

		if ((!motor0_want) && (dor_crnt & FD_MOTOR0))
		{ /* They want 0 off */
			disable_ints();
			dor_crnt &= ~FD_MOTOR0;
			OutByte( dor_crnt, DOR_PORT);
			enable_ints();
		}

		if ((!motor1_want) && (dor_crnt & FD_MOTOR1)) { /* They want 1 off */
			disable_ints();
			dor_crnt &= ~FD_MOTOR1;
			OutByte( dor_crnt, DOR_PORT);
			enable_ints();
		}
	}
	goto MotorLoop;
}

/*******************************************
 Set desired motor status flags and drive
 select bit.  If drive select bit has changed
 this sends the current DOR value to select
 the correct drive. If the motor bit we wanted
 isn't on, we will also send the command.
 We DO NOT mess with the other motor bit.
*******************************************/

static void fd_select(U32 drive)
{
U8 fsend;
U8 fdelay;

	GetTimerTick(&fd_tick);		/* update last time selected */

	fsend = FALSE;
	fdelay = FALSE;

	if (drive) {					/* drive 1 */
		if (!(dor_crnt & 0x01)) {
			fsend = TRUE;
			dor_crnt |= 0x01;		/* select 1 */
		}
		if (!(dor_crnt & FD_MOTOR1)) {
			fsend = TRUE;
			fdelay = TRUE;
			dor_crnt |= FD_MOTOR1;	/* motor 1 on */
		}
	}
	else {
   		if (dor_crnt & 0x01) {
			fsend = TRUE;
			dor_crnt &= 0xFE;		/* select 0 (turn 1 off) */
		}
		if (!(dor_crnt & FD_MOTOR0)) {
			fsend = TRUE;
			fdelay = TRUE;
			dor_crnt |= FD_MOTOR0;	/* motor 0 on */
		}
	}

	if (fsend) {

		disable_ints();

		OutByte( dor_crnt, DOR_PORT);

		enable_ints();

		if (fdelay)
			Sleep(33);  /* delay 1/3th second if not already on! */
	}

}


/*******************************************
 Sets motor_want bit for specified motor.
*******************************************/

static void fd_motoroff(U32 motor)
{
/* Set what we want so motor task will know */

if (motor)
	motor1_want = FALSE;
else
	motor0_want = FALSE;
}

/*********************************************************
    This is called ONCE to initialize the Driver.
*********************************************************/

U32 fdisk_setup(void)
{
U32 erc;

fdcb[0].Name[0] = 'F';
fdcb[0].Name[1] = 'D';
fdcb[0].Name[2] = '0';
fdcb[0].sbName  = 3;
fdcb[0].type    = 1;
fdcb[0].fDevReent = 0;	/* not reentrant */
fdcb[0].fSingleUser = 0;

fdcb[1].Name[0] = 'F';
fdcb[1].Name[1] = 'D';
fdcb[1].Name[2] = '1';
fdcb[1].sbName  = 3;
fdcb[1].type    = 1;	/* this is set set to zero if not installed */
fdcb[1].fDevReent = 0;	/* not reentrant */
fdcb[1].fSingleUser = 0;

 dor_crnt |= (FD_INTS | FD_RESET);	 /* set ints & reset bits high */
 motor0_want = FALSE;
 motor1_want = FALSE;
 seek_status = 0;       /* clear seek status */
 fdstatus.erc = 0;      /* clear global disk error */

 erc = AllocExch(&fd_exch);		/* allocates exchanges for messaging */
 if (erc)
 	return(erc);
 erc = AllocDMAPage(1, &sectbuf, &physectbuf);
 if (erc)
 	return(erc);

/* Used to test DMA memory call
 xprintf("DMA Address:  %08x, DMA Physical : %08x\r\n", sectbuf, physectbuf);
 Sleep(200);
*/

 /* Set up the motor task */

 SpawnTask( &fdmotor_task, 18, 0, &MotorStkTop, 1);

 SetIRQVector(6, &fdisk_isr);
 UnMaskIRQ(6);    /*  unmask IRQ 6 */

 type0 = cmos_type(0);
 if (type0)
 {
    fdcb[0].nBPB    = 512;
    fdcb[0].nBlocks = rgSectorMax[type0];
 }

 type1 = cmos_type(1);
 if (type1)
 {
    fdcb[1].nBPB    = 512;
    fdcb[1].nBlocks = rgSectorMax[type1];
 }

/* reset the FDC */

 if (fdstatus.erc= FDC_reset())
 	return (fdstatus.erc);

/* now we attempt to select and recal both drives */

 if (type0) {
	fd_drive = 0;		/* select drive 0 */
 	fd_select(0);
	if (erc = recal())
	{
		erc = recal();		/* try twice */
	}
 	fd_motoroff(0);
	if (erc) return (fdstatus.erc = erc);
 	}

 if (type1) {
	fd_drive = 1;		/* select drive 1 */
 	fd_select(1);
	if (erc = recal())
		erc = recal();		/* try twice */
	if (erc) fdcb[1].type = 0;
 	fd_motoroff(1);
 	}

 fdcb[0].pDevOp = &dev_op;
 fdcb[0].pDevInit = &dev_init;
 fdcb[0].pDevSt = &dev_stat;

 fdcb[1].pDevOp = &dev_op;
 fdcb[1].pDevInit = &dev_init;
 fdcb[1].pDevSt = &dev_stat;

 return(erc = InitDevDr(10, &fdcb, 2, 1));

}

/************************************************************
 Reset the disk system
*************************************************************/

static U32 FDC_reset(void)
{
 U32 erc;

 seek_status = 0;					 /* Set to Recal on next seek */

 disable_ints();

 OutByte(0x08, DOR_PORT);	 /* Drop reset signal on disk controller */
 MicroDelay(75);			 /* Wait 1ms */
 OutByte(0x0C, DOR_PORT);	 /* Raise reset line on disk controller */

 enable_ints();

 /* wait for interrupt and return if error (timeout...?) */

 if (erc = wait_int())
 	return(erc);

  /* Send sense command to current drive to see if reset took.
     If this is the first system reset it defaults to drive 0,
     Head 0.
  */


 if (erc = send_fdc(8)) return(erc);

  /* check results */

 if (erc = results(2)) return(erc);

 if (((FDC_STATUS[0] & 0xc0) == 0xc0 ) ||
    ((FDC_STATUS[0] & 0xc0) == 0)) {

     /* Send Specify command to controller */
	 send_fdc(3);
	 send_fdc(GetParm(0));
	 send_fdc(GetParm(1));
	 return(ok);
 }
 return(ErcBadFDC);
}

/*************************************************************
 The ISR is very simple.
 It just waits for an interrupt then sends a message to the
 exchange where the FD Driver will be waiting.
 The call that caused an interrupt to be generated reads the status
 codes from the controller to determine if the last status was OK.
****************************************************************/
static void interrupt fdisk_isr(void)
{
	ISendMsg(fd_exch, 0xfffffffd, 0xfffffffd);
	EndOfIRQ(6);
}

/*=========================================================
Return the drive type for a specified drive.
---------------------------------------------------------*/

static U8 cmos_type (U8 drive_nr)
{
 U8 drive_type;
 drive_type = 0;
 if (drive_nr)
 	drive_type = ReadCMOS(0x10) & 0x0f;
 else
 	drive_type = (ReadCMOS(0x10) >> 4) & 0x0f;
 return(drive_type);
}

/*========================================================
Send a byte to the FDC.
--------------------------------------------------------*/

static U32 send_fdc(U8 parm)
{
U32 i;
U8  b, bjunk;

i = 100;  /* try 100 times to send FDC command */
do
{
	b = InByte(MSR_PORT);				/* Get I/O status byte */
	MicroDelay(100);					/* 1.5 milliseconds between tries */
	if (b & RQM) {
		if (b & DIO) {  				/* He has something to send us... */
			bjunk = InByte(DATA_PORT);	/* Eat it! */
			MicroDelay(100);			/* 1.5 milliseconds between I/O */
		}
		else {							/* OK to send to him */
			OutByte(parm,DATA_PORT);
			MicroDelay(100);			/* 1.5 milliseconds between I/O */
			return (ok);				/* Sent it OK */
		}
	}
 }
 while (i--);
 return (ErcSendFDC);		/* return ERROR */
}

/*==========================================================
Get the indexed value from the disk parameter table.
-------------------------------------------------------*/

static U8 GetParm(U8 index)
{
 return( fdisk_table [fdstatus.type_now][index]);
}


/******************************************
Wait for the hardware interrupt to occur.
Time-out and return if no interrupt.
********************************************/

static U32 wait_int(void)
{
U32 erc;

/* Set alarm for 3 seconds */
erc = Alarm(fd_exch, 300);

erc = WaitMsg(fd_exch, fd_msg);  /* Wait for message from ISR or Alarm */
if (erc) {
	KillAlarm(fd_exch);
	return(erc); /* BAD NEWS FROM KERNEL!!! */
	}

if (fd_msg[0] != 0xfffffffd)
{
	return(fdstatus.erc = ErcFDCTimeOut);
}
else {
	KillAlarm(fd_exch);
	return(fdstatus.erc = ok);
	}
}


/*******************************
    Recalibrate the drive.
********************************/

static U32 recal(void)
{
U32 erc;

 erc = send_fdc(7);							/* recal command (2 bytes) */
 if (!erc) erc = send_fdc(fd_drive);		/* second byte is drive */

 /* wait for int on recal */
 if (!erc) erc = wait_int();
 if (!erc) erc = send_fdc(8);				/* Sense command */
 if (!erc) erc = results(2);				/* expect 2 bytes */
 if (!erc) {
   if (FDC_STATUS[0] & 0x20)   				/* Check seek-Ok bit */
   {
    if (FDC_STATUS[1]) return(ErcBadRecal);	/* Was it track 0? */
    return(ok);
   }
   else return (ErcBadSeek);       			/* Seek bit NOT set */
 }
 return(erc);
}
/*******************************************************
 Move the head to the selected track.
*********************************************************/

static U32 seek(void)
{
U32 erc;

 if ((seek_status & (1 << fd_drive)) == 0)	/* need recal */
 {

  /* try 2 attemps at recalibrate, then error out */
  if (recal())
  	if (erc = recal()) return (erc);  		/* try again */

  seek_status |= (1 << fd_drive); 			/* recal was done */

  LAST_TRACK[fd_drive] = 0; 				/* clear track number */

  /* if we want track zero, then just wait for head and exit */

  if (fd_track == 0) {
  	wait_for_head();
  	return (ok);
  	}
 }

 if (fdisk_table[fdstatus.type_now][14] != 0)
	fd_track *= 2;
 if (LAST_TRACK[fd_drive] == fd_track)		/* already there */
	 return(ok);

  /* update new position */
 LAST_TRACK[fd_drive] = fd_track;

 erc = send_fdc(0x0f);						/*Seek Cmd */
 if (!erc) erc = send_fdc((fd_head << 2) | fd_drive);
 if (!erc) erc = send_fdc(fd_track);

  /* wait for int on seek command */
 if (!erc) erc = wait_int();
 if (!erc) erc = send_fdc(8);				/* Sense command */
 if (!erc) erc = results(2);				/* expect 2 bytes */
 if (!erc)
 	if (!(FDC_STATUS[0] & 0x20))			/* Look for seek-Ok bit */
 	{
     seek_status &= ~(1 << fd_drive);		/* needs recal! */
  	 erc = ErcBadSeek;
  	 return (erc);
 	}
 if (!erc)
	 wait_for_head();
 return (ok);
}

/*=======================================================
Read a single byte from the data port (for status).
Returns TRUE if it got one.
--------------------------------------------------------*/

static U32 read_data(U8 *pDataRet)
{
U16 tries;
U8 status;

	/* try 100 times while Busy */

	for (tries=0; tries<1000; tries++) {
		status = InByte(MSR_PORT);
		MicroDelay(100);
		if (status & RQM) {						/* RQM set */
			if (status & DIO) {					/* Direction IN if set */
				*pDataRet = InByte(DATA_PORT);	/* Get the data byte */
				MicroDelay(100);				/* digest it...*/
				return(TRUE);
			}
		}
	}
	return(FALSE);
}


/*=======================================================
Read anything from the controller following an interrupt.
This may include up to seven bytes of status.
--------------------------------------------------------*/

static U32 results(U32 expect)
{
unsigned indx;
U8 status;

	indx = 0;
	while (indx < expect) {
		if (read_data(&status)) {
			FDC_STATUS[indx++] = status;		/* save status */
		 	MicroDelay(100);
		}
		else return(ErcResults);
	}
	return(0);
}



/****************************************************
  Purge the FDC of any status it is waiting to send.
*****************************************************/

static void purge_fdc (void)
{
 U8 b;

	do {
		b = InByte(MSR_PORT);
		if (b & RQM) {
		  	if (b & DIO) {
				InByte(DATA_PORT);  /* eat the byte */
				MicroDelay(100);   	/* Breath (1.5ms) */
			 }
			 else return;
		}
		else return;
	}
	while (1);
}

/*======================================================
Wait for the head to settle after a seek for write.
MicroDelay is in 15 us increments, while settle value
is in milliseconds, so we turn millies into micros and
divide by 15 to get value for MicroDelay call.
-------------------------------------------------------*/

static void wait_for_head(void)
{
U32 wait;

if (fwrite) {
 wait = GetParm(9); /* Head settle for write in milliseconds */
 wait = (wait * 1000) / 15;
 MicroDelay(wait); 	 /* delay in 15 us increments */
 }
}

/*=========================================================
 Checks for a media change for selected drive.
 Returns:
   0, or ErcNewMedia
-------------------------------------------------------*/

static U32 med_change(void)
{

/* if no disk change indicated return OK */

 if (InByte(DIR_PORT) & DSKCHANGE_BIT) {
	fdstatus.fNewMedia = 1;
 	return (ErcNewMedia);
 }
 else {
	fdstatus.fNewMedia = 0;
 	return (ok);
 }
}


/***********************************************************
 Wait until an operation is complete, then accept the status
 from the controller.
************************************************************/

static U32 get_fdc_status(void)
{
U32 erc;

 if (erc = wait_int()) return(erc);
 if (erc = results(7)) return(erc);
 if (!(FDC_STATUS[0] & 0xc0)) return(ok);
 if ((FDC_STATUS[0] & 0xc0) == 0x80) return(ErcBadCmd);
 if ((FDC_STATUS[0] & 0xc0) == 0xC0) return(ErcReadyLine);

 /* If we got here, get controller error status */

 if (FDC_STATUS[1] & BIT7)
 	erc = ErcSectNotFound;
 else if (FDC_STATUS[1] & BIT5)
 	erc = ErcCRC;
 else if (FDC_STATUS[1] & BIT4)
 	erc = ErcOverRun;
 else if (FDC_STATUS[1] & BIT2)
 	erc = ErcSectNotFound;
 else if (FDC_STATUS[1] & BIT1)
	erc = ErcReadOnly;
 else if (FDC_STATUS[1] & BIT0)
 	erc = ErcAddrMark;
 else fdstatus.erc = ErcBadFDC;
 return(erc);
}


/*************************************************************
 This is called for Read, Write or Verify commmands. The only
 differences in these 3 commands is the DMA type/direction and
 a single byte FDC command.
*************************************************************/

static U32 RdWrtVerf(U32 op)
{
U32 erc;
S8  dmatype;			/* 0 = Verify, 1 = IN, 2 = Out */
S8  retrys;
U32 count;

erc = 0;
retrys = 5;
count = fd_nr_sectors * 512;

while ((fd_nr_sectors) && (!erc)) {

	switch(op) {
		case(CmdRead): 					/* Read */
			dmatype = 1;
			fd_fdc_command = FDC_READ;
			break;
		case(2): 					/* Write */
			fwrite = 1;
			dmatype = 2;
			CopyData(fd_pData, sectbuf, 512);
			fd_fdc_command = FDC_WRITE;
			break;
		case(3): 					/* Verify */
			dmatype = 0;
			fd_fdc_command = FDC_READ;
	}

	/*             PhyAddress  nBytes  ch   Type     Mode  */
	erc = DmaSetUp(physectbuf, 512,    2,   dmatype, 1 );

	if (!erc) {
	  while(retrys--)
	  {
	   erc = 0;
	   if (!erc) erc = seek();
	   if (!erc) erc = send_fdc(fd_fdc_command);
	   if (!erc) erc = send_fdc(((fd_head <<2) & BIT2) | fd_drive);
	   if (!erc) erc = send_fdc(fd_track);
	   if (!erc) erc = send_fdc(fd_head);
	   if (!erc) erc = send_fdc(fd_sector);
	   if (!erc) erc = send_fdc(GetParm(3));
	   if (!erc) erc = send_fdc(GetParm(4));
	   if (!erc) erc = send_fdc(GetParm(5));
	   if (!erc) erc = send_fdc(GetParm(6));
	   if (!erc) erc = get_fdc_status();
	   if (!erc) break;					/* exit loop with good operation */
	   MicroDelay(200);					/* wait 3 milliseconds... */
	  } /* While statement for 5 retrys */
	}

	if (!erc) {
		if (op==CmdRead)
			CopyData(sectbuf, fd_pData, 512);
	    fdstatus.blocks_done++;
	    fd_pData+=512;
        --fd_nr_sectors;				/* One less sector */
        ++fd_sector;					/* Next sector please */
        if (fd_sector > fdisk_table[fdstatus.type_now][4]) {
        	fd_sector = 1;				/* back to sect one */
 	       	++fd_head;					/* next head please */
		}
        if (fd_head > 1) {
        	fd_head = 0;				/* back to head 0 */
        	++fd_track;					/* next track please */
		}
	}
} /* while */
return(erc);
}


/*************************************************************
 This formats the track beginning at the block address given
 in dLBA. dLBA must always be a multiple of the number of
 sectors per track minus 1 for the disk type (usually 9 or 15).
*************************************************************/

static U32 format_track(void)
{
U32 erc;

 fd_fdc_command = FDC_FORMAT;
 fwrite = 1;      	/* indicate write operation */

 erc = DmaSetUp(fd_pData, 512, 2, 1, 1);
 if (!erc) erc = send_fdc(3);			/* specify command */
 if (!erc) erc = send_fdc(GetParm(0));
 if (!erc) erc = send_fdc(GetParm(1));
 if (!erc) erc = seek();
 if (!erc) erc = send_fdc(fd_fdc_command);
 if (!erc) erc = send_fdc(((fd_head <<2) & BIT2) | fd_drive);
 if (!erc) erc = send_fdc(GetParm(3));
 if (!erc) erc = send_fdc(GetParm(4));
 if (!erc) erc = send_fdc(GetParm(7));
 if (!erc) erc = send_fdc(GetParm(8));
 if (!erc) erc = get_fdc_status();
 return(erc);
}

/******************************************************************************
Now begins the PUBLIC routines that are interfaced to for all DEVICE DRIVERS
*/

/******************************************
Called for all device operations.  This
assigns physical device from logical number
that outside callers use. For Floppy, 5=0
and 6=1. This will check to make sure a
drive type is assigned then calc physical
head, cly, sector from dLBA.
*******************************************/

static U32 dev_op(U32 dDevice,
 		    U32 dOpNum,
		    U32 dLBA,
		    U32 dnBlocks,
		    S8  *pData)
{
U32 erc, j;

 fd_pData = pData;
 fwrite = 0;
 fdstatus.blocks_done = 0;	/* Reset values in Status record */
 fdstatus.erc = 0;

 /* Set internal drive number */

 if (dDevice == 10)
 	fd_drive = 0;
 else
 	fd_drive = 1;

/* Check to see if we have a leftover interrupt message from last
   command.  If so then we eat it (do nothing)
*/

 erc = CheckMsg(fd_exch, fd_msg);

 if (fd_drive==0){
	if (!type0) return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type0;
	}
 else {
	if (!type1) return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type1;
	}

 if (dLBA > fdstatus.BlocksMax) 
 	{
/* for troubleshooting
	    xprintf("dLBA: %d, BlocksMax: %d, DriveType: %d\r\n",
    	         dLBA, fdstatus.BlocksMax, fdstatus.type_now);
*/
	 	return(fdstatus.erc = ErcBadLBA);
	}

 fd_select(fd_drive);		/* turn motor on and select drive */

 /* set the data rate register to value the drive */

 OutByte(fdisk_table[fdstatus.type_now][12], DRR_PORT); /* Rate Cmd */

 /* make sure any residual status is unloaded */

 purge_fdc();

 /* if a media change sensed, update status */

/*

 erc = med_change();
 if (erc) {
		erc = recal();
		fd_motoroff(fd_drive);
		if (erc) {
			return(fdstatus.erc=erc);
		}
		else
			return(fdstatus.erc=ErcNewMedia);
 }

*/

 fdstatus.BlocksMax = rgSectorMax[fdstatus.type_now];		/* set max sectors */

 fd_nr_sectors = dnBlocks;

 /* calculate the cylinder, head and sector from LBA */
 /* 3 = nHeads, 4 = sectors/track */

 fd_track = dLBA / (GetParm(3) * GetParm(4));
 j = dLBA % (GetParm(3) * GetParm(4));

 /* We now know what cylinder, calc head and sector */

 fd_head = j / GetParm(4);
 fd_sector = j % GetParm(4) + 1;  /* sector numbers start at 1 !!!! */

/* for troubleshooting...
 xprintf("\r\nDevice: %d, dLBA: %d, dOpNum: %d\r\n", dDevice, dLBA, dOpNum);
*/

 switch(dOpNum) {
	case(CmdNull):
		fdstatus.erc = ok;				/* Null Command */
		break;
	case(CmdRead): 						/* Read */
	case(CmdWrite): 					/* Write */
	case(CmdVerify): 					/* Verify */
		fdstatus.erc = RdWrtVerf(dOpNum);
		break;
	case(CmdFmtBlk): 					/*	Format Block */
		fdstatus.erc = ErcBadOp;
		break;
	case(CmdFmtTrk):
		break;							/* Format Track */
	case(CmdSeekTrk): 					/* Seek Track */
		break;
	default:
		fdstatus.erc = ErcBadOp;
		break;
	}

 fd_motoroff(fd_drive);
 return(fdstatus.erc);
}


/******************************************
Called for indepth status report on ctrlr
and drive specified. Returns 80 byte block
of data including the drive parameter
block (a 16 byte structure with all the
timing and size params of the drive).
This is called by the PUBLIC call DeviceStat!
*******************************************/

static U32 dev_stat(U32 dDevice,
			  S8 * pStatRet,
			  U32 dStatusMax,
			  U32 *pdStatusRet)
{
S32 j;

 /* Set internal drive number */
 if (dDevice == 10)
 	fd_drive=0;
 else fd_drive = 1;

 if (fd_drive==0){
	if (type0==0)
		return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type0;
    fdstatus.nCyl  = fdisk_table[type0][11]+1;  /* total physical cyls */
    fdstatus.nHead = 2;							/* total heads on device */
    fdstatus.nSectors= fdisk_table[type0][4];   /* Sectors per track */
    fdstatus.nBPS = 512;						/* Bytes per sect */
 }
 else {
	if (type1==0) return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type1;
    fdstatus.nCyl  = fdisk_table[type1][11]+1;  /* total physical cyls */
    fdstatus.nHead = 2;							/* total heads on device */
    fdstatus.nSectors= fdisk_table[type1][4];   /* Sectors per track */
    fdstatus.nBPS = 512;						/* Bytes per sect */
 }

 /* set max sectors in status record */

 fdstatus.BlocksMax = rgSectorMax[fdstatus.type_now];

 /* copy in the 16 bytes of floppy specific data */

 CopyData(&fdisk_table[fdstatus.type_now], &fdstatus.params, 16);

 /* Update disk status bytes for status return */

 CopyData(&FDC_STATUS, &fdstatus.STATUS, 8);

 if (dStatusMax < sStatus) j = dStatusMax;
 else j = sStatus;

 CopyData(&fdstatus, pStatRet, j);		/* copy the status data */

 *pdStatusRet = j;		/* give em the size returned */

return(0);
}

/******************************************
Called to reset the floppy disk controller
and to set the TYPE.  The type is one byte
of the 64 byte status record passed to
dev_init (offset 13). See the fdstatus struct.
This should be called once for each floppy
before it is used the first time after
the driver is loaded, or after a fatal
error is received (timeout etc.).
This is called by the PUBLIC call DeviceInit!
*******************************************/

static S32 dev_init(U32  dDevice,
			 S8  *pInitData,
			 U32  sdInitData)

{
U32 i;

 /* Read the init status block in */

 if (sdInitData > sStatus) i = sStatus;		/* no more than 64 bytes! */
 else i = sdInitData;

 CopyData(pInitData, &FDStatTmp, i);		/* copy in their init data */

 if (dDevice == 10)
 	fd_drive=0;   			/* Set internal drive number */
 else
 	fd_drive = 1;

 if (fd_drive==0){						/* set up for drive 0 */
 	type0 = FDStatTmp.type_now;
	if (type0==0) return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type0;
 }
 else {
 	type1 = FDStatTmp.type_now;
	if (type1==0) return(fdstatus.erc = ErcDriveType);
	fdstatus.type_now = type1;
 }
 fd_select(fd_drive);
 fdstatus.erc = FDC_reset();
 fd_motoroff(fd_drive);
 return(fdstatus.erc);
}

/*===========  THE END  =========================================*/
