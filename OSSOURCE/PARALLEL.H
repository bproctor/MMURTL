/* This defines the status record and pertinent data masks
   for values form the parallel port driver.
*/

struct statRecL{
  unsigned long lptJob;		/* Owner of this lpt port, 0 for not in use */
  unsigned long LastErc;	/* Result of last device operation */
  unsigned long LastTotal;	/* Total bytes moved in last operation */
  unsigned char status;		/* Status Byte - Bits defined in Parallel.h */
  unsigned long BufCnt;		/* Bytes left in buffer to send */
  unsigned char resvd1;		/*  */
  unsigned char resvd2;		/*  */
  unsigned char resvd3;		/*  */
  unsigned char IRQNum;		/* IRQNum for this channel */
  unsigned long IOBase;		/* IO base address for hardware */
  unsigned long XBufSize;	/* Size of Xmit buffer */
  unsigned long XTimeOut;	/* Xmit Timeout in 10ms increments */
  unsigned long resvd4;		/* Recv Timeout in 10ms increments */
  unsigned long resvd[6];	/* out to 64 bytes */
  };

/* Device Driver interface commands (Op numbers)
   NOTE: No "Read" commands are defined for this driver.
   This is a WRITE-ONLY device.
*/

#define CmdWriteRec  2			/* Write one or more bytes */
#define CmdOpenL    10			/* Open Comm Channel */
#define CmdCloseL   11			/* Close Comm Channel */
#define CmdCloseLU  12			/* Close Comm Channel UNCONDITIONALLY */
#define CmdSetXTO	14          /* Set Xmit timeout 10ms incs in dLBA */
#define CmdWriteB	32			/* write a single byte */

#define LPTBUSY   0x80		/* 1 = NOT Busy */
#define LPTACK    0x40		/* 1 = Acknowledge */
#define LPTPAPER  0x20		/* 1 = Out of paper */
#define LPTSELECT 0x10		/* 1 = Selected */
#define LPTIOERR  0x08		/* 1 = I/O Error */

#define ErcBadOpNum		503		/* Standard Device error for bad cmd number */
#define ErcXmitTimeoutL	901		/* Xmit Buffer never Emptied */
#define ErcDataNotSent  902		/* Xmit buffer not empty */
#define ErcNotOpenL		907		/* Channel not open... */
#define ErcChannelOpenL	909		/* It's already open... */
#define ErcNotOwnerL	912		/* It's opened by someone else... */
#define ErcBadIOBaseL	924		/* if 0           */
#define ErcBadInitSizeL	927		/* At least 40 bytes for this version */
