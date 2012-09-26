/*   RS232.H     */

#define MIN_BAUD       150l
#define MAX_BAUD       38400l
#define NO_PAR             0
#define EV_PAR             1
#define OD_PAR             2

#define ErcRecvTimeout	800		/* Recv Buffer Empty */
#define ErcXmitTimeout	801		/* Xmit Buffer never Emptied */
#define ErcRcvBufOvr 	802		/* Receive buffer overrun */
#define ErcBadPort 		803		/* Invalid port on OpenCommC */
#define ErcRcvBufOvr	805		/* Buffer full!!! */
#define ErcNotOpen		807		/* Channel not open... */
#define ErcChannelOpen	809		/* It's already open... */
#define ErcNotOwner		812		/* It's opened by someone else... */
#define ErcBadBaud		820		/* 150-38400      */
#define ErcBadParity	821		/* 0, 1 or 2      */
#define ErcBadDataBits	822		/* Must be 5-8    */
#define ErcBadStopBits	823		/* Must be 1 or 2 */
#define ErcBadIOBase	824		/* if 0           */
#define ErcBadCommIRQ	825		/* < 3            */
#define ErcBadInitSize	827		/* At least 40 bytes for this version */


struct statRecC{
  unsigned long commJob;	/* Owner of this comms port, 0 for not in use */
  unsigned long LastErc;	/* Result of last device operation */
  unsigned long LastTotal;	/* Total bytes moved in last operation */
  unsigned long Baudrate;	/* Baudrate for this port, 150 - 38400 */
  unsigned char parity; 	/* Parity for this port, 0=none, 1=even, 2=odd */
  unsigned char databits;	/* nDatabits for this port, 5-8 */
  unsigned char stopbits;	/* stop bits for this port, 1 or 2 */
  unsigned char IRQNum;		/* IRQNum for this channel */
  unsigned long IOBase;		/* IO base address for hardware */
  unsigned long XBufSize;	/* Size of Xmit buffer */
  unsigned long RBufSize;	/* Size of Recv Buffer */
  unsigned long XTimeOut;	/* Xmit Timeout in 10ms increments */
  unsigned long RTimeOut;	/* Recv Timeout in 10ms increments */
  unsigned long resvd[6];	/* out to 64 bytes */
  };

/* Device Driver interface commands (Op numbers) */

#define CmdReadRec   1			/* Read one or more bytes */
#define CmdWriteRec  2			/* Write one or more bytes */
#define CmdOpenC    10			/* Open Comm Channel */
#define CmdCloseC   11			/* Close Comm Channel */
#define CmdDiscardRcv 12		/* Trash input buffer */
#define CmdSetRTO	13          /* Set Recv timeout 10ms incs in dLBA */
#define CmdSetXTO	14          /* Set Xmit timeout 10ms incs in dLBA */
#define CmdSetDTR   15			/* Set DTR (On) */
#define CmdSetRTS	16			/* Set CTS (On) */
#define CmdReSetDTR 17			/* Set DTR (On) */
#define CmdReSetRTS 18			/* Set CTS (On) */
#define CmdBreak	19			/* Send BREAK (10ms incs in dLBA) */
#define CmdGetDC	20			/* Returns byte TRUE to pData if CD ON */
#define CmdGetDSR	21			/* Returns byte TRUE to pData if DSR ON */
#define CmdGetCTS	22			/* Returns byte TRUE to pData if CTS ON */
#define CmdGetRI	23			/* Returns byte TRUE to pData if RI ON */
#define CmdReadB	31			/* Recv a single byte */
#define CmdWriteB	32			/* Xmit a single byte */


/*********************** END of COMMDRV.H *****************************/
