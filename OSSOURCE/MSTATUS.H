#define ErcOK		 	0	/* Alls Well */
#define ErcEOF		 	1	/* DUH... The END */

#define ErcOpCancel		4	/* Operator cancel */
#define ErcNullPtr	 	6	/* Null ptr passed into call or service */

/* 10 - 19 Exchange Errors for Allocation and Use */

#define ErcOutOfRange	10	/* Bad exchange number */
#define ErcNotAlloc		11	/* Exchange number used was not allocated */
#define ErcNotOwner		12	/* Attempt to dealloc Exch that's not yours */

/*20 - 29 Message Management */

#define ErcNoMsg		20	/* No msg waiting at exchange (CheckMsg) */

/*30 - 39 System Service Management */

#define ErcNoSuchSvc	30	/* No service by that name */
#define ErcBadSvcCode	32	/* Service doesn't handle that code */
#define ErcNotRqBlk		34	/* Service received a NON RqBlock at it's exchange*/
#define ErcOutOfSvcDesc 36	/* No more empty Service Descriptors */
#define ErcOwnerAbort	37	/* Svc received SvcCode 0 on owner of this RqBlk*/

/* OS Resource Management Errors */

#define ErcNoMoreExch	40	/* Out of exchanges */
#define ErcNoMoreLBs	41	/* Out of Link Blocks */
#define ErcNoSvcBlks	43	/* No more servcice can install */
#define ErcNoMoreTBs	44	/* Out of timer blocks */
#define ErcNoMoreTSSs	45	/* Out of TSSs */
#define ErcNoMoreRqBlks 46	/* Out of Request Blocks */
#define ErcNoMoreJCBs   47	/* Out of Job Control Blocks */

/* OS Call Gate Management */

#define ErcBadGateNum  	48	/* Tried to add an invalid GDT call gate */
#define ErcBadCallGate	49	/* Called an UNinitialized Call Gate!! */

/*OS Task Management */

#define ErcBadPriority	50	/* Priority is out of range */

/*OS Job Management */

#define ErcBadJobNum	70	/* A Bad job number was specified in an OS call */
#define ErcInvalidJCB	71	/* The Job number specifies an unassigned JCB */
#define ErcBadRunFile	74	/* The run file you specified is NOT a run file! */
#define ErcNoExitJob	76	/* No exit job was specified on ExitJob(n) */
#define ErcBadParams	80	/* Invalid or bad params were passed to a command */

/* Memory Management */

#define ErcNoGdtSlots	100 /* No more free rgMemDesc GDT slots! */
#define ErcNoMem		101 /* Not enough memory (no more pages!!!) */
#define ErcBadMemPage	102 /* Bad physical page specified in Mem call */
#define ErcBadMemReq	104 /* Invalid size for memory request */
#define ErcInternalMem  106 /* Internal inconsistancy. AddPage can't! */
#define ErcNoRuns		107 /* No free runs large enough in PTs (temporary) */
#define ErcBadLinAdd    108 /* Bad linear address was passed to DeallocMem */
#define ErcShortMem     109 /* Passed in too many pages to Dealloc, but */
		                        /*as many as possible were deallocated  */
#define ErcBadAlias     111 /* Address passed in WASN't an alias (it should be) */

/* DOS-FAT File System  200 - 299  */

#define ErcBadFileSpec	200	/* invalid file spec (not correct format) */
#define ErcNoSuchDrive	201	/* Try another letter bozo */
#define ErcNotAFile		202	/* Open a directory?? NOT */
#define ErcNoSuchFile	203	/* No can do! It ain't there...*/
#define ErcNoSuchDir	204	/* Ain't no such dir... */
#define ErcReadOnly		205	/* You can't modify it bubba */
#define ErcNoFreeFCB	206	/* We're really hurtin... */
#define ErcBadOpenMode	207	/* Say what? Mode??? */
#define ErcFileInUse	208	/* File is open in incompatible mode */
#define ErcNoFreeFUB	209	/* Sorry, out of File User Blocks */
#define ErcBadFileHandle 210 /* WHOAAA, bad handle buddy! */
#define ErcBrokenFile	211	/* Cluster chain broken on file */
#define ErcBadFCB		213	/* We got REAL problems... */
#define ErcStreamFile	214	/* Operation not allowed on Stream File */
#define ErcBlockFile	215	/* Operation not allowed on Block File */
#define ErcBeyondEOF	217	/* SetLFA or WriteBlock beyond EOF */
#define ErcNoParTable	218	/* No partiton table found on disk!!! */
#define ErcBadFATClstr  220 /* File system screwed up (or your disk is) */
#define ErcRenameDrv    222	/* They have tried to rename across Dir/Vol*/
#define ErcRenameDir    223	/* They have tried to rename across Dir/Vol*/
#define ErcNoMatch      224	/* No matching directory entry */
#define ErcWriteOnly	225	/* Attempt to read write-only device */
#define ErcDupName		226	/* Name exists as a file or dir already */
#define ErcNotSupported	227	/* Not supported on this file  */
#define ErcRootFull		228	/* The Root Directory is Full  */
#define ErcDiskFull		230	/* No more free CLUSTERS!!!  */
#define ErcDirFull		231	/* No free Dir entries TEMP ERROR  */

/* Character Video Management 300 -399 */

#define ErcVidNum		300 /* Bad vid number passed to vid call */
#define ErcVidparam		301 /* A param was out of range to a Vid call */
#define ErcEditParam	300 /* Bad param to EditLine */
#define ErcBadString	302 /* Invalid sting passed to Math Cnvrt Func */

/* OS hardware Resource Management */

#define ErcDMAChannel	400 /* Invalid DMA channel specified as param */
#define ErcDMAMode		401 /* Bad mode specified */

/* 500-599 Device Management (General) */

#define ErcBadDevNum	500 /* DCB number too large ( > max DCBs) */
#define ErcDevInUse		501 /* DCB already in use */
#define ErcBadDevName	502 /* Size of device name incorrect */
#define ErcBadOpNum		503 /* OpNum to DD that it doesn't handle! */
#define ErcNoDevice		504 /* Driver's installed but NO device is */
#define ErcNoDriver		505 /* No driver installed for that Device Num */
#define ErcDCBInUse		506 /* Attempt to install driver over valid FDC */

/* 700-749 Keyboard Service */

#define ErcNoKeyAvail	700	/* Asked not to wait and no key is available */

/* 600-649 Floppy Device Driver */

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

/* 650-699 Hard Disk Device Driver */

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

/* 800-899 Async Comms Driver */

/* End of Status.c */
