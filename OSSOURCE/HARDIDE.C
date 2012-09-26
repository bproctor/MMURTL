/* MFM & IDE Hard Disk Device Driver for MMURTL.
   This driver does not depend on the data stored in CMOS RAM for 
   drive geometry.  Three routines determine number of sectors per track, 
   number of cylinders, and number of heads by actually trying to seek 
   and/or read them. This eliminates dependence on some system's proprietary
   CMOS locations.
*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

/* MMURTL OS PROTOTYPES */

extern far AllocExch(U32  *pExchRet);
extern far U32  InitDevDr(U32  dDevNum,
				    	  S8  *pDCBs,
					  	  U32  nDevices,
					  	  U32  dfReplace);

extern far U32  UnMaskIRQ(U32  IRQNum);
extern far U32  MaskIRQ(U32  IRQNum);
extern far U32  SetIRQVector(U32  IRQNum, S8  *pIRQ);
extern far U32  EndOfIRQ(U32  IRQNum);
extern far U32  SendMsg(U32  Exch, U32  msg1, U32  msg2);
extern far U32  ISendMsg(U32  Exch, U32  msg1, U32  msg2);
extern far U32  WaitMsg(U32  Exch, U32  *pMsgRet);
extern far U32  CheckMsg(U32  Exch, U32  *pMsgRet);
extern far U32  Alarm(U32  Exch, U32  count);
extern far U32  KillAlarm(U32  Exch);
extern far U32  Sleep(U32  count);
extern far void MicroDelay(U32  us15count);
extern far void OutByte(U8 Byte, U16 wPort);
extern far void OutWord(U16 Word, U16 wPort);
extern far U8 InByte(U16 wPort);
extern far U16 InWord(U16 wPort);
extern far U8 ReadCMOS(U16 Address);
extern far void CopyData(U8 *pSource, U8 *pDestination, U32 dBytes);
extern far InWords(U32 dPort, U8 *pDataIn, U32 dBytes);
extern far OutWords(U32 dPort, U8 *pDataOut, U32 dBytes);

/* Near External for troubleshooting */

extern long xprintf(char *fmt, ...);


/* LOCAL PROTOTYPES */

U32  hdisk_setup(void);
static void interrupt hdisk_isr(void); 	/* The HD interrupt function */
static U32  hd_format_track(U32 dLBA, U32 dnBlocks);
static void hd_reset(void);
static U32  send_command(U8  parm);
static U32  hd_wait (void);
static U32  check_busy(void);
static U32  hd_seek(U32 dLBA);
static U32  hd_recal(U8 drive);
static U32  hd_write(U32 dLBA, U32 dnBlocks, U8 *pDataOut);
static U32  hd_read(U32 dLBA, U32 dnBlocks, U8 *pDataIn);
static U32  hd_status(U8 LastCmd);
static U32  setupseek(U32 dLBA, U32 nBlks);
static U32  hd_init(U8 drive);
static U32 ReadSector(U32 Cylinder, U32 HdSect, U8 *pDataRet);

/* The following 3 calls are required in every MMURTL device driver */

static U32  hddev_op(U32  dDevice,
 		    U32  dOpNum,
		    U32  dLBA,
		    U32  dnBlocks,
		    U8  *pData);

static U32  hddev_stat(U32  dDevice,
			  S8 * pStatRet,
			  U32  dStatusMax,
			  U32  *pdSatusRet);

static U32  hddev_init(U32  dDevNum,
			  S8  *pInitData,
			  U32   sdInitData);

/* LOCAL DEFINITIONS */

#define ok 0

/* Error Codes to return */

#define ErcNoMsg		20
#define ErcNotInstalled 504

#define ErcBadBlock		651
#define ErcAddrMark		652
#define ErcBadECC		653
#define ErcSectNotFound	654
#define ErcNoDrive0		655
#define ErcNotSupported 656
#define ErcBadHDC		658
#define ErcBadSeek		659
#define ErcHDCTimeOut	660
#define ErcOverRun		661
#define ErcBadLBA		662
#define ErcInvalidDrive	663
#define ErcBadOp		664
#define ErcBadRecal		665
#define ErcSendHDC		666
#define ErcNotReady		667
#define ErcBadCmd		668
#define ErcNeedsInit	669
#define ErcTooManyBlks	670		/* The controller can only do 128 max */
#define ErcZeroBlks		671		/* 0 Blocks not allowed for this cmd */
#define ErcWriteFault	672		/* WriteFault bit set... bummer */

#define ErcMissHDDInt   675

#define ErcHDDMsgBogus  676
#define ErcHDDIntMsg	677
#define ErcHDDAlarmMsg  678

/* Commands accepted by this HD driver */

#define CmdNull     0
#define CmdRead     1
#define CmdWrite    2
#define CmdVerify   3
#define CmdFmtBlk   4
#define CmdFmtTrk   5
#define CmdSeekTrk  6
#define CmdSetMedia 7   /* Not used unless mountable */
#define CmdResetHdw 8   /* Used to reset controller hardware */

/* CmdReadSect is the only device specific call in the IDE/MFM hard
   disk device driver.  This allows you to read ONE sector
   specified by Cylinder, head and Sector number.
   Cylinder is HiWord of dLBA in DeviceOp call,
   Head is LoWord of dLBA in DeviceOp call, and
   Sector number is LowWord in dnBlocks.
*/

#define CmdReadSect 256 /* only device specific call in HDD */

/* HDC port definitions */

#define HD_PORT 0x1f0

/* When writing to the port+X (where X =):
	 0 - write data       (1F0h - 16 bit)
	 1 - pre-comp         (1F1h)
	 2 - sector count     (1F2h)
	 3 - sector number    (1F3h)
	 4 - low cyl          (1F4h)
	 5 - high cyl         (1F5h)
	 6 - size/drive/head  (1F6h)
	 7 - command register (1F7h)

When reading from the port+X (where X =):
	 0 - read data       (1F0h - 16 bit)
	 1 - error register  (1F1h)
	 2 - sector count    (1F2h)
	 3 - sector number   (1F3h)
	 4 - low cyl         (1F4h)
	 5 - high cyl        (1F5h)
	 6 - size/drive/head (1F6h)
	 7 - status register (1F7h)
*/

#define HD_REG_PORT 0x3f6

/* This is a byte wide write only control port
   that allows reset and defines some special
   characteristics of the hard drives.
	Bit		Desc
	0		Not used
	1		Not used
	2		Reset Bit - Set, wait 50us, then Reset
	3		Mucho Heads Flag. Set = More than 8 heads
	4		Not used
	5		Not used
	6		Disable retries
	7		Disable retries (same as six, either one set)
*/

/* HDC Status Register Bit Masks (1F7h) */

#define BUSY        0x80  /* busy.. can't talk now! */
#define READY       0x40  /* Drive Ready  */
#define WRITE_FAULT 0x20  /* Bad news */
#define SEEKOK      0x10  /* Seek Complete */
#define DATA_REQ    0x08  /* Sector buffer needs servicing */
#define CORRECTED   0x04  /* ECC corrected data was read */
#define REV_INDEX   0x02  /* Set once each disk revolution */
#define ERROR       0x01  /* data address mark not found */

/* HDC Error Register Bit Masks (1F1h) */

#define BAD_SECTOR  0x80  /* bad block */
#define BAD_ECC     0x40  /* bad data ecc */
#define BAD_IDMARK  0x10  /* id not found */
#define BAD_CMD     0x04  /* aborted command */
#define BAD_SEEK    0x02  /* trk 0 not found on recalibrate, or bad seek */
#define BAD_ADDRESS 0x01  /* data address mark not found */


/* HDC internal command bytes (HDC_Cmd[7]) */

#define HDC_RECAL      0x10	  /* 0001 0000 */
#define HDC_READ       0x20   /* 0010 0000 */
#define HDC_READ_LONG  0x22   /* 0010 0010 */
#define HDC_WRITE      0x30   /* 0011 0000 */
#define HDC_WRITE_LONG 0x32   /* 0011 0010 */
#define HDC_VERIFY     0x40   /* 0100 0000 */
#define HDC_FORMAT     0x50   /* 0101 0000 */
#define HDC_SEEK       0x70   /* 0111 0000 */
#define HDC_DIAG       0x90   /* 1001 0000 */
#define HDC_SET_PARAMS 0x91   /* 1001 0001 */

/* L O C A L   D A T A  */

static U8  hd_Cmd[8];		/* For all 8 command bytes */

static U8  fDataReq;		/* Flag to indicate is fDataRequest is active */
static U8  statbyte;		/* From HDC status register last time it was read */

static U8  hd_control;		/* Current control byte value */
static U8  hd_command;		/* Current Command */
static U8  hd_drive;		/* Current Physical Drive, 0 or 1 */
static U8  hd_head;		/* Calculated from LBA - which head */
static U8  hd_nsectors;	/* Calculated from LBA - n sectors to read/write */
static U8  hd_sector;		/* Calculated from LBA - Starting sector */

/* Current type drive 0 & 1 found in CMOS or Set by caller. */
/* Current number of heads, cylinders, and sectors set by caller */

static U8  hd0_type;
static U8  hd0_heads;
static U8  hd0_secpertrk;
static U16 hd0_cyls;

static U8  hd1_type;
static U8  hd1_heads;
static U8  hd1_secpertrk;
static U16 hd1_cyls;

#define sStatus 64

static struct statstruct
 {
  U32 erc;
  U32 blocks_done;
  U32 BlocksMax;
  U8 fNewMedia;
  U8 type_now;		/* current fdisk_table for drive selected */
  U8 resvd0[2];		/* padding for DWord align  */
  U32 nCyl;			/* total physical cylinders */
  U32 nHead;		/* total heads on device    */
  U32 nSectors;		/* Sectors per track        */
  U32 nBPS;			/* Number of bytes per sect.  32 bytes out to here.*/

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

static struct statstruct hdstatus;
static struct statstruct HDStatTmp;

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
	U8   fDevReent;
	U8   fSingleUser;
	S16  wJob;
	U32  OS1;
	U32  OS2;
	U32  OS3;
	U32  OS4;
	U32  OS5;
	U32  OS6;
	};

static struct dcbtype hdcb[2];		/* two HD device control blocks */

/* Exch and msgs space for HD ISR */

static U32 hd_exch;

static U32 hd_msg;
static U32 hd_msg2;

static long HDDInt;

/*======================================================*/
/*=================== START OF CODE ====================*/
/*======================================================*/

/*********************************************************
    This is called ONCE to initialize the HD Driver.
*********************************************************/

U32  hdisk_setup(void)
{
U32  erc;

  /* first we set up the 2 DCBs in anticipation of calling InitDevDr */

	hdcb[0].Name[0]  = 'H';
	hdcb[0].Name[1]  = 'D';
	hdcb[0].Name[2]  = '0';
	hdcb[0].sbName   = 3;
	hdcb[0].type     = 1;		 /* Random */
	hdcb[0].nBPB     = 512;
	hdcb[0].nBlocks  = 524288;   /* largest disk handled - 2Gb disks*/
	hdcb[0].pDevOp   = &hddev_op;
	hdcb[0].pDevInit = &hddev_init;
	hdcb[0].pDevSt   = &hddev_stat;

	hdcb[1].Name[0]  = 'H';
	hdcb[1].Name[1]  = 'D';
	hdcb[1].Name[2]  = '1';
	hdcb[1].sbName   = 3;
	hdcb[1].type     = 1;			/* Random */
	hdcb[1].nBPB     = 512;
	hdcb[1].nBlocks  = 524288;	/* largest device handled - 2Gb disks*/
	hdcb[1].pDevOp   = &hddev_op;
	hdcb[1].pDevInit = &hddev_init;
	hdcb[1].pDevSt   = &hddev_stat;

/* These are defaulted to non zero values to
   ensure we don't get a divide by zero during initial calculations
   on the first read.
*/

	hd0_type =	ReadCMOS(0x19);	/* read this but don't use it */
	hd0_heads = 16;			/* Max */
	hd0_secpertrk = 17;		/* most common */
	hd0_cyls = 1024;		/* Max */

	hd1_type =	ReadCMOS(0x1A);
	hd1_heads = 16;
	hd1_secpertrk = 17;
	hd1_cyls = 1024;

	erc = AllocExch(&hd_exch);		/* Exhange for HD Task to use */

	SetIRQVector(14, &hdisk_isr);
	UnMaskIRQ(14);

/* Documentation lists the fixed disk types at CMOS 11h and 12h,
   and also shows them at 19h and 1Ah.  We don't actually read them
   because they are not dependable. They vary from BIOS to BIOS.
   We have to make this sucker work the hard way.
*/

/* Reset the HDC - hd_reset resets the controller (which controlls
   both drives). We have to do it once, then try both physical drives.
   If the second drive is not there, some controllers will lock-up
   (the el-cheapos).  In this case we have to reset it again so it
   will work.  It seems like a lot of work, but to make it function
   with the widest range of IDE and MFM controllers this is the
   only way I have found that works.
*/

	hd_reset();		/* no error is returned */

/* Now we attempt to select and recal both drives.
   The driver MUST be able to recal the first physical drive
   or the Driver won't install.
*/

	erc = hd_recal(0);			/* try to recal */
	if (erc)
	{					/* try one more time! */
		hd_reset();
		erc = hd_recal(0);			/* try to recal */
		if (erc)
		{
	        hdcb[0].last_erc = erc;
			hd0_type = 0;			/* Must not be a valid drive */
			return(ErcNoDrive0);
	 	}
    }

	/* if we got here, drive 0 looks OK and the controller is
	   functioning.  Now we try drive 1 if type > 0.
	*/

	if (hd1_type)
	{
		erc = hd_recal(1);	/* try to recal if CMOS says it's there */
		if (erc)
		{
	        hdcb[1].last_erc = erc;
			hd1_type = 0;			/* Guess it's not a valid drive */

			if (!erc)

		/* We must redo drive 0 cause some cheap controllers lockup
		on us if drive 1 is not there.  They SHOULD simply return
		a Bad Command bit set in the Error register, but they don't. */

			hd_reset();
			erc = hd_recal(0);			/* recal drive 0 */
   	     hdcb[0].last_erc = erc;
		}
	}

	return(erc = InitDevDr(12, &hdcb, 2, 1));

}

/************************************************************
 Reset the HD controller.  This should only be called by
 DeviceInit or hdisk_setup.  This resets the controller
 and reloads parameters for both drives (if present) and
 attempts to recal them.
*************************************************************/

static void hd_reset(void)
{
U32 i;
	UnMaskIRQ(14);      		/*  enable the IRQ */
	OutByte(4, HD_REG_PORT); 	/*  reset the controller */
   	MicroDelay(4);  			/*  Delay 60us */

    /* bit 3 of HD_REG must be 1 for access to heads 8-15 */
    /* Clear "MUCHO" heads bit, and clear the reset bit */

    OutByte(hd_control & 0x0f, HD_REG_PORT);

	Sleep(20);		/* 200ms - seems some controllers are SLOW!! */
    i = CheckMsg(hd_exch, &hd_msg); 	/* Eat Int if one came back  */

	hdstatus.ResetStatByte = statbyte;	/* The ISR gets statbyte */

	if (i) hdstatus.fIntOnReset = 1;
	else hdstatus.fIntOnReset = 0;

}

/*************************************************************
 The ISR is VERY simple. It just waits for an interrupt, gets
 the single status byte from the controller (which clears the
 interrupt condition) then sends an empty message to the
 exchange where the HD Driver task will be waiting.
 This tells the HD task currently running that it's got
 some status to act on!
****************************************************************/
static void interrupt hdisk_isr(void)
{
	statbyte = InByte(HD_PORT+7);
    HDDInt = 1;
	ISendMsg(hd_exch, 0xfffffff0, 0xfffffff0);
	EndOfIRQ(14);
}

/*************************************************************
 This checks the HDC controller to see if it's busy so we can
 send it commands or read the rest of the registers.
 We will wait up to 3 seconds then error out.
 The caller should call check_busy and check the error.
 If it's 0 then the controller became ready in less than
 3 seconds.  ErcNotReady will be returned otherwise.
 It leaves the status byte in the global statbyte.
****************************************************************/

static U32  check_busy(void)
{
S16 count;

  count = 0;
  while (count++ < 60) 
  {
	statbyte = InByte(HD_PORT+7);
	if ((statbyte & BUSY) == 0) return(ok);
	Sleep(5);  /* 50ms shots */
  }
  return(ErcNotReady);	 /* controller out to lunch! */
}

/*************************************************************
  This sends the SetParams command to the controller to set
  up the drive geometry (nHeads, nSectors, etc.).
****************************************************************/

static U32  hd_init(U8 drive)
{
U32 erc;
  /* set max heads, sectors and cylinders */
  if (drive == 0) 
  {								/* Drive 0 */
    hd_Cmd[2] = hd0_secpertrk;					/* sector count */
    hd_Cmd[6] = (drive << 4) | ((hd0_heads-1) & 0x0f) | 0xa0; /* hds & drv */
  }
  else
  {										/* Drive 1 */
    hd_Cmd[2] = hd1_secpertrk;					/* sector count */
    hd_Cmd[6] = (drive << 4) | ((hd1_heads-1) & 0x0f) | 0xa0; /* hds & drv */
  }
  hd_Cmd[1] = 0;
  hd_Cmd[3] = 0;
  hd_Cmd[4] = 0;								/* cyl = 0 for init */
  hd_Cmd[5] = 0;								/* cyl = 0 for init */

  erc = send_command(HDC_SET_PARAMS);		/* Send the command */
  erc = hd_wait();							/* wait for interrupt */
  if (!erc)
      erc = hd_status(HDC_SET_PARAMS);
  return(erc);
}

/******************************************
Wait for the hardware interrupt to occur.
Time-out and return if no interrupt.
********************************************/

static U32  hd_wait(void)
{
U32  erc;

	/* Set alarm for 3 seconds */

	HDDInt = 0;
	KillAlarm(hd_exch);			/* kill any pending alarm */

	erc = Alarm(hd_exch, 300);	/* Set it up again */
	if (erc)
		return(erc);		/* bad problem */

	erc = WaitMsg(hd_exch, &hd_msg);

	KillAlarm(hd_exch);

	if (hd_msg != 0xfffffff0)
	{        /* HD interrupt sends fffffff0 */
	    if (HDDInt)
	      return(ErcMissHDDInt);
		else
			return(ErcHDCTimeOut);		/* Alarm sends 0xffffffff      */
	}
	else
	{
		KillAlarm(hd_exch);
		return(ok);
	}
}

/********************************************
    Recalibrate the drive.
*********************************************/

static U32  hd_recal(U8 drive)
{
U32 erc;

  hd_Cmd[6] = (drive << 4) | (hd_head & 0x0f) | 0xa0;
  erc = send_command(HDC_RECAL);
  if (!erc)
    erc = hd_wait();						/* wait for interrupt */
  if (!erc)
      erc = hd_status(HDC_RECAL);
  if (drive)
    hdstatus.LastRecalErc1 = erc;
  else
    hdstatus.LastRecalErc0 = erc;
  return(erc);
}


/********************************************
    Send the command to the controller.
    Clear the Echange of any left over
    alarm or int messages before we
    send a command.
*********************************************/

static U32  send_command(U8  Cmd)
{
U32 erc, msg[2];

	while (CheckMsg(hd_exch, &msg) == 0);	/* Empty it */

    /* bit 3 of HD_REG must be 1 for access to heads 8-15 */
    if (hd_head > 7)
    {
      hd_control |= 0x08;
      OutByte(hd_control, HD_REG_PORT); 	/*  set bit for head > 7 */
      hd_control &= 0xf7;
    }
    erc = check_busy();
    if (!erc) OutByte(hd_Cmd[1], HD_PORT+1);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(hd_Cmd[2], HD_PORT+2);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(hd_Cmd[3], HD_PORT+3);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(hd_Cmd[4], HD_PORT+4);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(hd_Cmd[5], HD_PORT+5);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(hd_Cmd[6], HD_PORT+6);
    if (!erc) erc = check_busy();
    if (!erc) OutByte(Cmd, HD_PORT+7);
    return(erc);
}

/*************************************************************
 This sets up the cylinder, head and sector variables for all
 commands that require them (read, write, verify, format, seek).
 nBlks ca NOT be greater than the hardware can handle. For
 IDE/MFM controllers this is 128 sectors.
 The caculated values are placed in the proper command byte
 in anticipation of the command being sent.
*************************************************************/

static U32  setupseek(U32 dLBA, U32 nBlks)
{
 U32  j;
 U16  cyl;

  if (nBlks > 256) return ErcTooManyBlks;
  if (nBlks == 0) return ErcZeroBlks;

  hd_nsectors = nBlks;
  if (hd_nsectors == 256) hd_nsectors = 0;   /* 0==256 for controller */

  if (hd_drive == 0) 
  {		/* drive 0 */

	cyl = dLBA / (hd0_heads * hd0_secpertrk);
 	j = dLBA % (hd0_heads * hd0_secpertrk);		/* remainder */

    /* we now know what cylinder, calculate head and sector */

    hd_head = j / hd0_secpertrk;
    hd_sector = j % hd0_secpertrk + 1; /* sector number start at 1 !!! */

  }
  else
  {						/* drive 1 */

	cyl = dLBA / (hd1_heads * hd1_secpertrk);
 	j = dLBA % (hd1_heads * hd1_secpertrk);		/* remainder */

    /* We now know what cylinder. Calculate head and sector */

    hd_head = j / hd1_secpertrk;
    hd_sector = j % hd1_secpertrk + 1; /* sector number start at 1 !!! */
  }

  hd_Cmd[2] = nBlks;					/* How many sectors */
  hd_Cmd[3] = hd_sector;				/* Which sector to start on */
  hd_Cmd[4] = cyl & 0xff;				/* cylinder lobyte */
  hd_Cmd[5] = (cyl >> 8) & 0xff;		/* cylinder hibyte */
  hd_Cmd[6] = (hd_drive << 4) | (hd_head & 0x0f) | 0xa0;

  return ok;
}


/*******************************************************
  Move the head to the selected track (cylinder).
*********************************************************/

static U32  hd_seek(U32 dLBA)
{
U32 erc;

  erc = setupseek(dLBA, 1);					/* sets up for implied seek */
  if (!erc) erc = send_command(HDC_SEEK);	/* Not implied anymore... */
  if (!erc) erc = hd_wait();				/* wait for interrupt */
  if (!erc) erc = hd_status(HDC_SEEK);
  hdstatus.LastSeekErc0 = erc;
  return(erc);
}


/*******************************************************
 Called to read status and errors from the controller
 after an interrupt generated by a command we sent.
 The error checking is based on the command that we sent.
 This is done because certain bits in the status and error
 registers are actually not errors, but simply indicate
 status or indicate an action we must take next.
 ZERO returned indicates no errors for the command status
 we are checking.
*********************************************************/

static U32  hd_status(U8  LastCmd)
{
U32 erc;
U8  statbyte, errbyte;

  /* We shouldn't see the controller busy. After all,
     he interrupted us with status.
  */

  erc = check_busy();		/* puts status byte into global StatByte */
  if (!erc)
    statbyte = InByte(HD_PORT+7);
  else return(erc);

  if (hd_drive)
	  hdstatus.LastStatByte1 = statbyte;
  else
	  hdstatus.LastStatByte0 = statbyte;

  if ((statbyte & ERROR) == 0)
  {		/* Error bit not set in status reg */
    erc = ok;  /* default */

	switch (LastCmd)
	{
	  case HDC_READ:
	  case HDC_READ_LONG:
	  case HDC_WRITE:
	  case HDC_WRITE_LONG:
	  case HDC_SEEK:
	  case HDC_RECAL:
		if (statbyte & WRITE_FAULT) erc = ErcWriteFault;
		else
			if ((statbyte & SEEKOK) == 0) erc =	ErcBadSeek;
		break;
	  case HDC_SET_PARAMS:
	  case HDC_VERIFY:
	  case HDC_FORMAT:
	  case HDC_DIAG:
		break;
	  default:
		break;
	}
    return(erc);
	}
  else
  {
	erc = check_busy();
	if (!erc)
	  errbyte = InByte(HD_PORT+1);
	else return(erc);

	if (hd_drive)
	    hdstatus.LastErcByte1 = errbyte;
	else
	    hdstatus.LastErcByte0 = errbyte;

	if (errbyte & BAD_ADDRESS) erc = ErcAddrMark;
	else if (errbyte & BAD_SEEK) erc = ErcBadSeek;
	else if (errbyte & BAD_CMD) erc = ErcBadCmd;
	else if (errbyte & BAD_IDMARK) erc = ErcSectNotFound;
	else if (errbyte & BAD_ECC) erc = ErcBadECC;
	else if (errbyte & BAD_SECTOR) erc = ErcBadBlock;
	else erc = ErcBadHDC; /* no error bits found but should have been! */
  }
 return erc;
}

/*************************************************************
 This is called for the DeviceOp code Read.
 This reads 1 or more whole sectors from the calculated values
 in hd_head, hd_sector, and hd_cyl
*************************************************************/

static U32  hd_read(U32 dLBA, U32 dnBlocks, U8 *pDataRet)
{
U32 erc, nleft, nBPS;

  nBPS = hdcb[hd_drive].nBPB;			/* From nBytesPerBlock in DCB */
  nleft = dnBlocks;
  erc = setupseek(dLBA, dnBlocks);		/* sets up for implied seek */
  if (!erc) erc = send_command(HDC_READ);

  while ((nleft) && (!erc)) 
  {
	  erc = hd_wait();					/* wait for interrupt */
	  if (!erc)
	  	erc = hd_status(HDC_READ);
	  if (!erc)		/* && (statbyte & DATA_REQ))  */ 
	  {
	  	InWords(HD_PORT, pDataRet, nBPS);
	  	pDataRet+=nBPS;
	  	--nleft;
	  }
  }
  return(erc);
}

/*************************************************************
 This is called for the DeviceOp code Write.
 This writes 1 or more whole sectors from the calculated values
 in hd_head, hd_sector, and hd_cyl
*************************************************************/

static U32  hd_write(U32 dLBA, U32 dnBlocks, U8 *pDataOut)
{
U32 erc, nSoFar, nBPS;

  nBPS = hdcb[hd_drive].nBPB;			/* From n BytesPerBlock in DCB */
  nSoFar = 0;
  erc = setupseek(dLBA, dnBlocks);		/* sets up for implied seek */
  erc = send_command(HDC_WRITE);
  erc = check_busy();			/* No INT occurs for first sector of write */

  if ((!erc) && (statbyte & DATA_REQ)) 
  {
	OutWords(HD_PORT, pDataOut, nBPS);
	pDataOut+=nBPS;
	nSoFar++;
  }

  while ((nSoFar < dnBlocks ) && (erc==ok))
  {
	  erc = hd_wait();					/* wait for interrupt */
      if (erc==ok) erc = hd_status(HDC_WRITE);
	  if ((erc==ok) && (statbyte & DATA_REQ)) 
	  {
		  OutWords(HD_PORT, pDataOut, nBPS);
		  pDataOut+=nBPS;
		  nSoFar++;
	  }
  }
  if (!erc) erc = hd_wait();			/* wait for final interrupt */
  if (!erc) erc = hd_status(HDC_WRITE);

  return(erc);
}


/*************************************************************
 This formats the track beginning at the block address given
 in dLBA. dLBA must always be a multiple of the number of
 sectors per track minus 1 for the disk type.
*************************************************************/

static U32  hd_format_track(U32 dLBA, U32 dnBlocks)
{
U32  erc;
  erc = setupseek(dLBA, dnBlocks);		/* sets up for implied seek */
  erc = send_command(HDC_FORMAT);
  erc = hd_wait();						/* wait for interrupt */
  if (erc==ok)
      erc = hd_status(HDC_FORMAT);
  return(erc);
}

/******************************************************************
   ReadSector is the only device specific call in the IDE/MFM hard
   disk device driver.  This allows you to read ONE sector
   specified by Cylinder, head and Sector number.
   Cylinder is LoWord of dLBA in DeviceOp call,
   Head is LoWord of dnBlocks in DeviceOp call, and
   Sector number is HiWord in dnBlocks.
*******************************************************************/

static U32 ReadSector(U32 Cylinder, U32 HdSect, U8 *pDataRet)
{
U32 erc;
U16  cyl;

  cyl = Cylinder;
  hd_head = HdSect & 0xffff;
  hd_sector = (HdSect >> 16) & 0xffff;

/*	For testing
 xprintf("\r\nCYL %d, HD %d, SEC %d\r\n", cyl, hd_head, hd_sector);
*/

  hd_Cmd[2] = 1;						/* How many sectors */
  hd_Cmd[3] = hd_sector;				/* Which sector to start on */
  hd_Cmd[4] = cyl & 0xff;				/* cylinder lobyte */
  hd_Cmd[5] = (cyl >> 8) & 0xff;		/* cylinder hibyte */
  hd_Cmd[6] = (hd_drive << 4) | (hd_head & 0x0f) | 0xa0;

  erc = send_command(HDC_READ);
  erc = hd_wait();					/* wait for interrupt */
  if (!erc) erc = hd_status(HDC_READ);
  if (!erc)
  	InWords(HD_PORT, pDataRet, 512);
  return(erc);
}

/***************************************************************************
Now begins the PUBLIC routines that are used for all DEVICE DRIVERS
*/

/******************************************
Called for all device operations.  This
assigns physical device from logical number
that outside callers use. For Hard disk,
12=0 and 13=1. This will check to make sure a
drive type is assigned and check to see if 
they are going to exceed max logical blocks.
*******************************************/

static U32  hddev_op(U32  dDevice,
 		      U32  dOpNum,
		      U32  dLBA,
		      U32  dnBlocks,
		      U8  *pData)
{
U32  erc;

 hdstatus.blocks_done = 0;	/* Reset values in Status record */

 erc = 0;

 /* Set drive internal drive number */

 if (dDevice == 12)
 	hd_drive = 0;
 else
 	hd_drive = 1;

 /* Check to see if we have a leftover interrupt message from last
    command.  If so then we eat it (and do nothing) */

  CheckMsg(hd_exch, &hd_msg);		/* Ignore error */

 if (hd_drive==0)
 {
	if (hd0_type==0)
		erc = ErcInvalidDrive;
 }
 else
 {
	if (hd1_type==0)
		erc = ErcInvalidDrive;
 }

 /* make sure they don't exceed max blocks */

 if (!erc)
   if (dLBA > hdcb[hd_drive].nBlocks) erc = ErcBadLBA;

 if (!erc)
 {

   switch(dOpNum)
   {
 	  case(CmdNull):
		erc = ok;					/* Null Command */
		break;
	  case(CmdRead): 									/* Read */
		erc = hd_read(dLBA, dnBlocks, pData);
		break;
	  case(CmdWrite): 									/* Write */
		erc = hd_write(dLBA, dnBlocks, pData);
		break;
	  case(CmdVerify): 									/* Verify */
		erc = ErcNotSupported;

		/* hd_verify is not supported in this version of the driver */
		/*		erc = hd_verify(dLBA, dnBlocks, pData); */
		break;
	  case(CmdSeekTrk): 								/* Seek Track */
		erc = hd_seek(dLBA);
		break;
	  case(CmdFmtTrk):                                  /* Format Track */
		erc = hd_format_track(dLBA, dnBlocks);
		break;
	  case(CmdResetHdw): 								/* Reset Ctrlr */
		hd_reset();
		erc = 0;
		break;
	  case(CmdReadSect): 								/* Read Sector(s) */
		erc = ReadSector(dLBA, dnBlocks, pData);
		break;
	  default:
		erc = ErcBadOp;
		break;
	}
 }
 hdcb[hd_drive].last_erc = erc;			/* update DCB erc */
 return(erc);
}

/******************************************
Called for indepth status report on ctrlr
and drive specified. Returns 64 byte block
of data including current drive geometery.
This is called by the PUBLIC call DeviceStat!
*******************************************/

static U32 hddev_stat(U32  dDevice,
			   S8 * pStatRet,
			   U32  dStatusMax,
			   U32  *pdStatusRet)
{
U32 i;

 /* Set status for proper device */

 if (dDevice == 12)
 {
	hdstatus.erc = hdcb[0].last_erc;
 	hdstatus.type_now = hd0_type;
 	hdstatus.nCyl = hd0_cyls;
 	hdstatus.nHead = hd0_heads;
 	hdstatus.nSectors = hd0_secpertrk;
	hdstatus.nBPS = hdcb[0].nBPB;
 }
 else 
 {
	hdstatus.erc = hdcb[1].last_erc;
 	hdstatus.type_now = hd1_type;
 	hdstatus.nCyl = hd1_cyls;
 	hdstatus.nHead = hd1_heads;
 	hdstatus.nSectors = hd1_secpertrk;
	hdstatus.nBPS = hdcb[1].nBPB;
 }

 /* Calculate size of status to return. Return no more than asked for! */

 if (dStatusMax <= sStatus) i = dStatusMax;
 else i = sStatus;

 CopyData(&hdstatus, pStatRet, i);		/* copy to their status block */

 *pdStatusRet = i;						/* tell em how much it was */

 return ok;
}

/******************************************
Called to reset the hard disk controller
and set drive parameters.  The Initdata
is a copy of the 64 byte status block
that is read from status.  The caller
normally reads the block (DeviceStat),
makes changes to certain fields and
calls DeviceInit pointing to the block
for the changes to take effect.
This should ONLY be called once for each HD
to set it's parameters before it is used
the first time after the driver is loaded,
or after a fatal error is received that
indicates the controller may need to be
reset (multiple timeouts etc.).
The DCB values are updated if this is
successful.
This is called by the PUBLIC call DeviceInit.
*******************************************/

static U32  hddev_init(U32  dDevice,
			  S8  *pInitData,
			  U32   sdInitData)

{
U32 erc, i;

  erc = 0;

 /* Read the init status block in */

 if (sdInitData > sStatus) i = sStatus;		/* no more than 64 bytes! */
 else i = sdInitData;

 CopyData(pInitData, &HDStatTmp, i);		/* copy in their init data */

 /* Set internal drive number */
 if (dDevice == 12) hd_drive=0;
 else hd_drive = 1;

 if (hd_drive==0)
 {
 	hd0_type  = HDStatTmp.type_now;
	if (hd0_type) 
	{
 	  hd0_cyls  = HDStatTmp.nCyl;
 	  hd0_heads = HDStatTmp.nHead;
 	  hd0_secpertrk = HDStatTmp.nSectors;
 	}
 	else erc = ErcInvalidDrive;
 }
 else 
 {
 	hd1_type  = HDStatTmp.type_now;
	if (hd1_type) 
	{
		hd1_cyls  = HDStatTmp.nCyl;
	 	hd1_heads = HDStatTmp.nHead;
	 	hd1_secpertrk = HDStatTmp.nSectors;
	}
 	else erc = ErcInvalidDrive;
 }

/* If no error, initialize it and recal */

 if (!erc) erc = hd_init(hd_drive);
 if (!erc) erc = hd_recal(hd_drive);

/* If no error, update corresponding DCB values */

 if (!erc) 
 {
	hdcb[hd_drive].nBPB = HDStatTmp.nBPS;
	hdcb[hd_drive].last_erc = 0;
	hdcb[hd_drive].nBlocks =
				HDStatTmp.nCyl * HDStatTmp.nSectors * HDStatTmp.nHead;
 }

 hdcb[hd_drive].last_erc = erc;			/* update DCB erc */

 return(erc);
}

/*===========  THE END  =========================================*/
