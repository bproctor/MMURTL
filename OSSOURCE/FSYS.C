/* This is the MMURTL, MS-DOS Compatible (FAT) File system.  */

/*
  MMURTL Operating System Source Code
  Copyright 1991,1992,1993,1994 Richard A. Burgess
  ALL RIGHTS RESERVED   Version 1.0
*/

/*
About MS-DOS disk formats and how MMURTL handles them.

Physical Disk Layouts
  From the disk controller's standpoint:
	  Cylinder numbers run from 0 to nMaxCyls-1.
	  Head numbers run from 0 to nMaxheads-1.
	  Sector numbers run from 1 to nMaxSectorsPerTrack.

Physical (Absolute) Sector Numbers

  Physical sector numbers (absolute) begin at Cyl 0, Head 0, Sector 1.
  As the physical sector number rolls over (nMaxSectorsPerTrack+1),
  the Head number is incremented which moves us to the next track
  (same cylinder, next head).  When the head number rolls over
  (nMaxHeads is reached) the cylinder number is incremented.

  Note: Track and cylinder are NOT interchangable terms in the
  above text.  If you have 6 heads on your drive, you have
  6 tracks per cylinder.  This can be confusing because many
  books and documents use the terms interchangably.
  And you can, so long as you know that's what you're doing.

Hidden Sectors

  MS-DOS reserves a section of the physical hard disk. This area
  is called the Hidden Sectors.  This is usually the very first
  track on the disk (begins at Cyl 0, head 0, Sector 1).  The
  partition tables are kept at the very end of the first sector in
  this hidden area (offset 01BEh in the first sector to be exact).
  The partition tables are 16 byte entries that describe
  "logical" sections of the disk that can be treated as separate drives.
  There are usually no "hidden sectors" on floppy disks, nor are there
  any partition tables.


MMURTL Logical Block Address (LBA)

  MMURTL device drivers treat the entire disk as a single physical
  drive.  The MMURTL file system reads the partition tables,
  then sets up the device driver to span the entire physical disk
  as 0 to nMaxBlockNumbers-1.  This is refered to as the Logical Block
  Address (LBA) and is the value passed in to the DeviceOp call for
  the MMURTL hard/floppy disk device drivers (LBAs are used with
  all MMURTL devices).

  Note: DO NOT confuse MMURTL's LBA for the sector number in an MS-DOS
  logical drive.  MMURTL calls these logical blocks because we still
  have to convert them into physical cylinder, head and sector to
  retrieve the data.

MS-DOS Boot Sector

  The first sector of an MS-DOS logical drive will be its boot
  sector. Each of the MS-DOS logical partitions will have a boot
  sector although only the first will be marked as bootable (if any are).
  It's position on the disk is calculated from the partition table
  information.

MMURTL File System Initialization

  The MMURTL-FAT file system reads the partition table and saves
  the starting LBA and length of each of DOS logical disk that is
  found.  Armed with this information, MMURTL can access each of
  the DOS logical disks as a separate disk drive. To maintain some
  sanity, the MMURTL file system gives all of its logical drives
  a letter just like MS-DOS.  MMURTL supports two floppy drives
  (A & B) and up to eight logical hard disk (C-J).  All information
  on the Logical Drives are kept in an array of records (Ldrvs).
  This includes the logical letter to physical drive conversions.

  Once we have the layout of each of the partitons, we read the boot
  sector from the first DOS logical drive.  The boot sector contains
  several pieces of important information about the drive geometry
  (numbers of heads, sectors per track, etc.), which are also placed
  in the Logical Drive stuctures.

  Once we have the drive geometry information we setup the MMURTL
  device driver.  This tells the device driver how many cylinders,
  heads and sectors per track are on the physical disk.
  Until this is done, the device driver assumes a minimum drive size
  and you should only read the partition table (or boot sector if no
  partition table is on the disk).  This provides enough
  information to do a DeviceInit call to set up proper drive
  geometry.

  If you were building a loadable file system to replace the one that
  is included in MMURTL, you would call your routine to initialize
  the file system very early in the main program block.  You must
  not service file system requests until this is done.

*/

#define U32 unsigned long
#define U16 unsigned int
#define U8  unsigned char
#define S32 long
#define S16 int
#define S8  char
#define TRUE 1
#define FALSE 0

/*********** MMURTL Public Prototypes ***************/

/* From MKernel */

extern far AllocExch(long *pExchRet);
extern far U32 GetTSSExch(U32  *pExchRet);
extern far SpawnTask(char *pEntry,
		             long dPriority,
                     long fDebug,
                     char *pStack,
           		     long fOSCode);
extern far long WaitMsg(long Exch, char *pMsgRet);
extern far long CheckMsg(long Exch, char *pMsgRet);
extern far long Request(unsigned char *pSvcName,
						unsigned int  wSvcCode,
						unsigned long dRespExch,
						unsigned long *pRqHndlRet,
						unsigned long dnpSend,
						unsigned char *pData1,
						unsigned long dcbData1,
						unsigned char *pData2,
						unsigned long dcbData2,
						unsigned long dData0,
						unsigned long dData1,
						unsigned long dData2);

extern far long Respond(long dRqHndl, long dStatRet);

/* From MData */
extern far void CopyData(U8 *pSource, U8 *pDestination, U32 dBytes);
extern far void FillData(U8 *pDest, U32 cBytes, U8 bFill);
extern far long CompareNCS(U8 *pS1, U8 *pS2, U32 dSize);

/* From MTimer.h */
extern far long GetCMOSTime(long *pTimeRet);
extern far long GetCMOSDate(long *pTimeRet);
extern far long GetTimerTick(long *pTickRet);

/* From MVid.h */
extern far long TTYOut (char *pTextOut, long ddTextOut, long ddAttrib);
extern far long GetNormVid(long *pNormVidRet);

#include "MKbd.h"

/* From MDevDrv */
extern far U32  DeviceOp(U32  dDevice,
 		                 U32  dOpNum,
						 U32  dLBA,
						 U32  dnBlocks,
						 U8  *pData);

extern far U32  DeviceStat(U32  dDevice,
						   S8 * pStatRet,
						   U32  dStatusMax,
						   U32  *pdSatusRet);

extern far U32  DeviceInit(U32  dDevNum,
						   S8  *pInitData,
						   U32   sdInitData);

/* From MMemory.h */
extern far U32 AllocOSPage(U32 nPages, U8 *ppMemRet);
extern far U32 DeAllocPage(U8 *pOrigMem, U32 nPages);

/* From MJob.h */
extern far U32 GetPath(long JobNum, char *pPathRet, long *pdcbPathRet);
extern far U32 RegisterSvc(S8 *pName, U32 Exch);

/* NEAR support for debugging */

extern long xprintf(char *fmt, ...);
extern U32 Dump(unsigned char *pb, long cb);

/* File System error codes */

#define ErcOK			 0		/* Alls Well */
#define ErcEOF			 1		/* DUH... The END */
#define ErcBadSvcCode	 32		/* Service doesn't handle that code */

#define ErcBadFileSpec	 200	/* invalid file spec (not correct format)*/
#define ErcNoSuchDrive	 201	/* Try another letter bozo */
#define ErcNotAFile		 202	/* Open a directory?? NOT */
#define ErcNoSuchFile	 203	/* No can do! It ain't there...*/
#define ErcNoSuchDir	 204	/* Ain't no such dir... */
#define ErcReadOnly		 205	/* You can't modify it bubba */
#define ErcNoFreeFCB	 206	/* We're really hurtin... */
#define ErcBadOpenMode	 207	/* Say what? Mode??? */
#define ErcFileInUse	 208	/* File is open in an incompatible mode */
#define ErcNoFreeFUB	 209	/* Sorry, out of File User Blocks */
#define ErcBadFileHandle 210	/* WHOAAA, bad handle buddy! */
#define ErcBrokenFile	 211	/* Cluster chain broken on file */
#define ErcBadFCB		 213	/* We got REAL problems... */
#define ErcStreamFile	 214	/* Operation not allowed on Stream File */
#define ErcBlockFile	 215	/* Operation not allowed on Block File */
#define ErcBeyondEOF	 217	/* SetLFA or Read/WriteBlock beyond EOF */
#define ErcNoParTable	 218	/* No partiton table found on disk!!! */
#define ErcBadFATClstr   220    /* File system screwed up (or your disk) */
#define ErcRenameDrv     222	/* They have tried to rename across Dir/Vol*/
#define ErcRenameDir     223	/* They have tried to rename across Dir/Vol*/
#define ErcNoMatch       224	/* No matching directory entry */

#define ErcWriteOnly	 225	/* Attempt to read write-only device */
#define ErcDupName		 226	/* Name exists as a file or dir already */
#define ErcNotSupported	 227	/* Not supported on this file  */
#define ErcRootFull		 228	/* The Root Directory is Full  */
#define ErcDiskFull		 230	/* No more free CLUSTERS!!!  */

#define ErcNewMedia		 605	/* for floppy mounting from FDD */

/**************** FAT Buffer control structure **********************/

/*
   The Fat structures are for keeping track of the FAT buffers.
   We never want to have more than one copy of a FAT sector in
   memory at one time, and we also never want to read one when
   its already here (a waste of time)!  We also keep track
   of the last time it was used and deallocate the oldest (LRU -
   Least Recently Used). Initially filling out the Fat control
   structure is part of the file system initialization.  If the
   FAT sector we are in has been modified (data written to clusters
   in it & FAT updated) we write it ASAP!
   Each FAT buffer is 1 sector long, except the first one which
   is 3 sectors for floppies (FAT12 types). This is because the
   FAT12 entires span sectors!
*/

#define nFATBufs 17		/* 1 Static for floppies + 16 * 512 = 8192, 2 pages */

static struct fattype {				/* */
	U8  *pBuf;			/* points to beginning of fat buffer  */
	U32 LastUsed;		/* Tick when last used (0 = Never) */
	U32 LBASect;		/* LBA of first FAT sect in buf (where it came from) */
	U16 iClstrStart;	/* Starting cluster for each buf  */
	U8  Drive;			/* LDrive this FAT sector is from */
	U8  fModLock;		/* Bit 0 = Modified, bit 1 = Locked  */
	};

static struct fattype Fat[nFATBufs];	/* 16 bytes * 17 */

/* We read 3 sectors worth of floppy fat buf in cause cluster
entries span sectors
*/

U8 FatBufA[1536];  /* floppy fat buffer */

#define FATMOD  0x01
#define FATLOCK 0x02

/**************** File Contol Block Structures (FCBs) **********/
/* One FCB is allocated and filled out for each file that is open.
   The actual directory entry structure from the disk is embedded
   in the FCB so it can be copied directly to/from the directory
   sector on the disk.
*/
#define nFCBs 128
#define sFCB  64

static struct FCB {
	S8  Name[8];		/* From here to Filesize is copy of DirEnt */
	S8  Ext[3];
	S8  Attr;			/* from MS-DOS */
	U8  Resvd1[10];		/* ????????  */
	U16 Time;			/* Only changed when created or updated */
	U16 Date;
	U16 StartClstr;		/* At least one per file!! */
	U32 FileSize;		/* last entry in FAT Dir Ent (32 bytes) */
	U32 LBADirSect;		/* LBA of directory sector this is from */
	U16 oSectDirEnt;	/* Offset in sector for the dir entry */
	U8  Ldrv;			/* Logical drive this is on (A-J, 0-9) */
	U8  Mode;			/* 0 or 1 (Read or Modify). */
	U8  nUsers;			/* Active FUBs for this file (255 MAX). 0= Free FCB */
    U8  fMod;			/* This file was modified! */
	U8  Resvd[22];		/* Out to 64 bytes */
	};

static struct FCB *paFCB;		/* a pointer to array of allocated FCBs. */
static struct FCB *pFCB;		/* pointer to one FCB */

/********************** File User Blocks **************************/

/* Each user of an open file is assigned a FUB.  The FUB number is the
   filehandle (beginning with 3). ) 0, 1 & 2 are reserved for NUL,
   KBD and VID devices.
*/

#define nFUBs 128
#define sFUB  32

/* The FUB contains information on a file related to a user's view
   of the file.  It is used to hold information on files opened
   in stream and block mode. Three important fields in the FUB are:
   LFABuf - LFA of first byte in buffer for a stream file.
   Clstr - Clstr of last block read or stream fill.
   LFAClstr - LFA of first byte in Clstr.

   LFAClstr and Clstr give us a relative starting point when
   reading a file from disk.  If we didn't save this information
   on the last access, we would have to "run" the cluster chain
   everytime we wanted to read or access a file beyond the last
   point we read. 
*/

struct FUB {
	U16 Job;			/* User's Job Number. 0 if FUB is free. */
	U16 iFCB;			/* FCB number for this file (0 to nFCBs-1) */
	U32 CrntLFA;		/* Current Logical File Address (File Ptr) */
	U8  *pBuf;			/* Ptr to buffer if stream mode */
	U32 sBuf;			/* Size of buffer for Stream file in bytes */
	U32	LFABuf;			/* S-First LFA in Clstr Buffer */
	U32 LFAClstr;		/* LFA of Clstr (below). */
	U16 Clstr;			/* Last Cluster read */
	U8  fModified;		/* Data in buffer was modified */
	U8  fStream;		/* NonZero for STREAM mode */
	U8  Rsvd[4];		/* Pad to 32 bytes */
	};

static struct FUB *paFUB;		/* a pointer to allocated FUBs. Set up at init. */
static struct FUB *pFUB;		/* a pointer to allocated FUBs. Set up at init. */

/* Boot sector info (62 byte structure) */
struct fsbtype {
	  U8  Jmp[3];
	  U8  OEMname[8];
	  U16 bps;
	  U8  SecPerClstr;
	  U16 ResSectors;
	  U8  FATs;
	  U16 RootDirEnts;
	  U16 Sectors;
	  U8  Media;
	  U16 SecPerFAT;
	  U16 SecPerTrack;
	  U16 Heads;
	  U32 HiddenSecs;
	  U32 HugeSecs;
	  U8  DriveNum;
	  U8  Rsvd1;
	  U8  BootSig;
	  U32 VolID;
	  U8  VolLabel[11];
	  U8  FileSysType[8];		/* 62 bytes */
	  };
static struct fsbtype  fsb;


/* Partition Table Entry info. 16 bytes */
struct partent {
  U8  fBootable;
  U8  HeadStart;
  U8  SecStart;
  U8  CylStart;
  U8  FATType;
  U8  HeadEnd;
  U8  SecEnd;
  U8  CylEnd;
  U32 nFirstSector;
  U32 nSectorsTotal;
  };

static struct partent partab[4];	/* 4 partition table entries 64 bytes */
static U16 partsig;

/* Bit definitions in attribute field for a directory entry */

#define ATTRNORM  0x00
#define READONLY  0x01
#define HIDDEN    0x02
#define SYSTEM    0x04
#define VOLNAME   0x08
#define DIRECTORY 0x10
#define ARCHIVE   0x20

/* Directory Entry Record, 32 bytes */

struct dirstruct {
	U8  Name[8];
	U8  Ext[3];
	U8  Attr;
	U8  Rsvd[10];
	U16 Time;
	U16 Date;
	U16 StartClstr;
	U32 FileSize;
	};

static struct dirstruct  dirent;

static struct dirstruct *pDirEnt;		/* a pointer to a dir entry */

/* When a file is opened, the filename is parsed into an array
   to facilitate searching the directory tree.  IN MS-DOS all
   dir and file names are SPACE padded (20h).  The FileSpec array
   contains the fully parsed path of the file.  For instance,
   If you were to open "A:\Dog\Food\IsGood.txt" the FileSpec
   array would look like this:
   FileSpec[0] = "DOG        "
   FileSpec[1] = "FOOD       "
   FileSpec[2] = "ISGOOD  TXT"
   FileSpec[3][0] = NULL;
   Note that the DOT is not inlcuded (it's not in the DOS directory
   either), and the next unused FileSpec entry contain NULL in the
   first byte.	SpecDepth tells us how many directories deep the
   name goes.
*/

static U8 FDrive					/* Drive parsed from file operation */
static U8 FileSpec[7][11];			/* Hierarchy from file spec parsing */
static U8 SpecDepth;				/* Depth of parse (0=Root File) */

/* Used for Rename */
static U8 FDrive1					/* Drive parsed from file operation */
static U8 FileSpec1[7][11];		/* Hierarchy from file spec parsing */
static U8 SpecDepth1;				/* Depth of parse (0=Root File) */

/* raw sector buffer for all kinds of stuff */

static U8  abRawSector[516];
static U8  abTmpSector[516];
static U8  abDirSectBuf[516];

/* These arrays keep track of physical drive data (0-4). */
#define nPDrvs 4

static struct phydrv {
	U32 nHeads;     	/* heads per drives   */
	U32 nSecPerTrk; 	/* Sectors per track  */
	U16 BS1Cyl;			/* Cyl of 1st boot sector on disk */
	U8  BS1Head;		/* Head of 1st boot sector on disk */
	U8  BS1Sect;		/* Sector of 1st boot sector on disk */
	}

static struct phydrv  PDrvs[nPDrvs];

/* This array of structures keeps track of logical drive data (A-J). */

#define nLDrvs 10

static struct ldrvtype {
	U32 LBA0;			/* lba for Start of LDrive (bootSect) */
	U32 LBAData;		/* lba for Start of Data Area */
	U32 LBAMax;			/* Max lba for logical drive */
	U32 LBARoot;		/* lba of the Root directory */
	U32 LBAFAT;			/* lba of first FAT */
	U16 nHeads;     	/* Setup after boot sector is read */
	U16 nSecPerTrk; 	/* Setup after boot sector is read */
	U16 nRootDirEnt;	/* Number of Root directory entries */
	U16 sFAT;			/* nSectors in a FAT */
	U8  DevNum;			/* Device Number for this ldrv FF = NONE */
	U8  SecPerClstr;	/* For each logical drive */
	U8  nFATS;			/* number of FATs */
	U8  fFAT16;			/* True for FAT16 else FAT12 */
	};

static struct ldrvtype  Ldrv[nLDrvs];

/* This is the Hard Disk Device Status record.
   It is peculiar to the HD Drvr */

struct hddevtype{
  U32 erc;
  U32 blocks_done;
  U32 BlocksMax;
  U8  fNewMedia;
  U8  type_now;		/* current fdisk_table for drive selected */
  U8  resvd0[2];	/* padding for DWord align  */
  U32 nCyl;			/* total physical cylinders (we really don't care) */
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

static struct hddevtype   HDDevStat;

/* This is the Floppy Device Status record.
   It is peculiar to the FD Drvr */

struct fdstattype{
  U32 erc;			/* Last Error from device */
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

static struct fdstattype  FDDevStat;

static long FSysStack[512];	/* 2048 byte stack for Fsys task */

static long FSysExch;

struct reqtype {			/* 64 byte request block structure */
	long ServiceExch;
	long RespExch;
	long RqOwnerJob;
	long ServiceRoute;
	char *pRqHndlRet;
	long dData0;
	long dData1;
	long dData2;
	int  ServiceCode;
	char npSend;
	char npRecv;
	char *pData1;
	long cbData1;
	char *pData2;
	long cbData2;
	long RQBRsvd1;
	long RQBRsvd2;
	long RQBRsvd3;
	};

static struct reqtype *pRQB;

static char *fsysname = "FILESYSM";

static unsigned long keycode;			/* for testing */

/*========================== BEGIN CODE ============================*/

/************************************************
 Called from read_PE, this gets the starting
 cylinder, head and sector for the first boot
 sector on a physical drive and stores it in the
 phydrv array.  d is the drive, i is the index
 into the partition table we read in.
*************************************************/

static void GetBSInfo(U32 d, U32 i)
{
 PDrvs[d].BS1Head = partab[i].HeadStart;
 PDrvs[d].BS1Sect = partab[i].SecStart;
 PDrvs[d].BS1Cyl  = partab[i].CylStart;

 if (!i) 
 {		/* primary partition info - use it for PDrv info */
	 PDrvs[d].nHeads = partab[i].HeadEnd;
	 PDrvs[d].nSecPerTrk = partab[i].nFirstSector & 0xff;
 }
}

/** InitFloppy *********************************
 This gets status from the floppy drive (device ld)
 and sets the physical & logical drive parameters
 for the type. It is called when the file system
 is first initialized and when there has been
 an error on the floppy.
*************************************************/

static U32 StatFloppy(U8 ld)
{
U32 erc, i;

/* Set gets status for the floppy type from the FDD and
   sets logical paramters for Ldrvs.
*/

Ldrv[0].DevNum= 10;		/* Device Numbers for floppies */
Ldrv[1].DevNum= 11;

erc = DeviceStat(ld+10, &FDDevStat, 64, &i);
if (!erc) 
{
	PDrvs[ld].nHeads = FDDevStat.nHead;
	PDrvs[ld].nSecPerTrk = FDDevStat.nSectors;
	Ldrv[ld].LBA0 = 0;		/* Floppy Boot Sector - always 0 */
	Ldrv[ld].LBAMax= FDDevStat.BlocksMax-1;	/* Max lba for logical drive 0 */

	Ldrv[ld].nHeads = FDDevStat.nHead;
	Ldrv[ld].nSecPerTrk = FDDevStat.nSectors;

    erc = 0;
}
else
   Ldrv[ld].DevNum = 0xff;

 return erc;
}

/************************************************
 Reads the partition table entries from hard
 drives and sets up some of the the logical
 drive array variables for hard Disks.
 It also saves first cylinder, head and sector
 of the first partiton on each physical drive
 so we can get more info for the LDrv arrays
 from the boot sector of that partition.
*************************************************/

static U32 read_PE(void)
{
U32 erc, ercD12, ercD13, i, j;
U8 fFound1, fFound2;

fFound1 = 0;		/* Have we found first valid partition on drive */
fFound2 = 0;

/* Set defaults for 4 physical drives. This info will be set
   correctly when the partition table and boot sectors are read.
*/

for (i=2; i< nLDrvs; i++) 
{	/* default to no logical hard drives */
   Ldrv[i].DevNum = 0xff;
}

i = 2;		/* first Logical Number for hard drives "C" */

for (j=2; j<4; j++) 
{	/* Array index Numbers for 2 physical hard Disks */

  erc = DeviceOp(j+10, 1, 0, 1, abRawSector); /* add 10 for Disk device nums */
  if (j==2) ercD12 = erc;
  else ercD13 = erc;

  if (!erc) 
  {
    CopyData(&abRawSector[0x01fe], &partsig, 2);

	/* It MUST have a partition table or we can't use it! */

	if (partsig != 0xAA55) return ErcNoParTable;

    CopyData(&abRawSector[0x01be], &partab[0].fBootable, 64);

/*
	 Dump(&partab[0].fBootable, 64);
	 ReadKbd(&keycode, 1);
*/

    if (partab[0].nSectorsTotal > 0) 
    {
     Ldrv[i].LBA0 =partab[0].nFirstSector;	/* lba for Start of LDrv (bootSect) */
     Ldrv[i].LBAMax =partab[0].nSectorsTotal;	/* Max lba for logical drive */
	 if (partab[0].FATType > 3)
        Ldrv[i].fFAT16 = 1;
     Ldrv[i].DevNum = j+10;
     if ((j==2) && (!fFound1)) 
     { GetBSInfo(2, 0); fFound1=1; }
       if ((j==3) && (!fFound2))
     { GetBSInfo(3, 0); fFound2=1; }
       i++;					/* if valid partition go to next LDrv */
     }

    if (partab[1].nSectorsTotal > 0) 
    {
     Ldrv[i].LBA0   = partab[1].nFirstSector;
     Ldrv[i].LBAMax = partab[1].nSectorsTotal;
	 if (partab[1].FATType > 3)
        Ldrv[i].fFAT16 = 1;
     Ldrv[i].DevNum = j+10;
     if ((j==2) && (!fFound1)) { GetBSInfo(2, 1); fFound1=1; }
     if ((j==3) && (!fFound2)) { GetBSInfo(3, 1); fFound2=1; }
     i++;					/* if we had a valid partition go to next */
    }

    if (partab[2].nSectorsTotal > 0) 
    {
     Ldrv[i].LBA0   = partab[2].nFirstSector;
     Ldrv[i].LBAMax = partab[2].nSectorsTotal;
	 if (partab[2].FATType > 3)
        Ldrv[i].fFAT16 = 1;
     Ldrv[i].DevNum = j+10;
     if ((j==2) && (!fFound1)) { GetBSInfo(2, 2); fFound1=1; }
     if ((j==3) && (!fFound2)) { GetBSInfo(3, 2); fFound2=1; }
     i++;					/* if we had a valid partition go to next */
    }

    if (partab[3].nSectorsTotal > 0) 
    {
     Ldrv[i].LBA0   = partab[3].nFirstSector;
     Ldrv[i].LBAMax = partab[3].nSectorsTotal;
	 if (partab[3].FATType > 3)
        Ldrv[i].fFAT16 = 1;
     Ldrv[i].DevNum = j+10;
     if ((j==2) && (!fFound1)) 
     { 
     	GetBSInfo(2, 3); 
     	fFound1=1; 
     }
     if ((j==3) && (!fFound2)) 
     { 
     	GetBSInfo(3, 3); 
     	fFound2=1; 
     }
     i++;					/* if we had a valid partition go to next */
     }
    }
  }

 if (ercD12) return ercD12;		/* there may be no Device 13 */
 else return 0;
}

/********************************************************************
  Reads in the first boot sector from each physical drive to get
  drive geometry info not available in partition table.  This includes
  number of heads and sectors per track.  Then we call DeviceInit
  for each physical device to set its internal drive geometry.
  This must be done before we even try to read the other boot sectors
  if the disk has mulitple partitions (otherwise it fails).
*********************************************************************/

static U32 SetDriveGeometry(U32 d)		/* d is the device number (12 or 13) */
{
U32 erc, i;

  if (d==12) 
  {
	erc =  DeviceStat(12, &HDDevStat, 64, &i);
	if (!erc) 
	{
	  HDDevStat.nHead = PDrvs[2].nHeads;
	  HDDevStat.nSectors = PDrvs[2].nSecPerTrk;
      erc = DeviceInit(12, &HDDevStat, 64); /* Set up drive geometry */
    }
  }

  if (d==13) 
  {
	erc =  DeviceStat(13, &HDDevStat, 64, &i);
	if (!erc) 
	{
	  HDDevStat.nHead = PDrvs[3].nHeads;
	  HDDevStat.nSectors = PDrvs[3].nSecPerTrk;
      erc = DeviceInit(13, &HDDevStat, 64); /* Set up drive geometry */
    }
  }

return erc;
}

/********************************************************************
  Read boot sector from logical drive (i) and sets up logical and
  physical drive array variables for the FAT file system found on
  the logical drive (described in the boot sector).
*********************************************************************/

static U32 read_BS(U32 i)
{
U32 erc, j;

if (Ldrv[i].DevNum != 0xff) 
{

    j = Ldrv[i].DevNum;			/* j is MMURTL Device number */

	erc = DeviceOp(j, 1, Ldrv[i].LBA0, 1, abRawSector);

	if ((erc==ErcNewMedia) && (i<2)) 
	{
		erc = DeviceOp(j, 1, Ldrv[i].LBA0, 1, abRawSector);
	}

	CopyData(abRawSector, &fsb.Jmp, 62);

    if (erc==0) 
    {
       Ldrv[i].LBARoot     = fsb.ResSectors + Ldrv[i].LBA0 +
                            (fsb.FATs * fsb.SecPerFAT);
       Ldrv[i].nRootDirEnt = fsb.RootDirEnts;	/* n Root dir entries */
       Ldrv[i].SecPerClstr = fsb.SecPerClstr;
       Ldrv[i].nHeads      = fsb.Heads;
       Ldrv[i].nSecPerTrk  = fsb.SecPerTrack;
       Ldrv[i].sFAT        = fsb.SecPerFAT;		/* nSectors in a FAT */
       Ldrv[i].nFATS       = fsb.FATs;			/* number of FATs */
       Ldrv[i].LBAFAT      = Ldrv[i].LBA0 + fsb.ResSectors;
       Ldrv[i].LBAData     = Ldrv[i].LBARoot + (fsb.RootDirEnts / 16);
	   if (fsb.FileSysType[4] == '2')
         Ldrv[i].fFAT16 = 0;

    } /* if erc */
} /* if valid logical device */
return 0;
}

/*******************************************************
  This gets the CMOS date & time and converts it into the
  format for the DOS FAT file system. This is two words
  with bits representing Year/Mo/day & Hr/Min/SecDIV2.
********************************************************/
static void GetFATTime(U16 *pTimeRet, U16 *pDateRet)
{
U32 date, time;
U16 DDate, DTime, w;

	GetCMOSDate(&date);
	GetCMOSTime(&time);
	/* Do the date */
	DDate = (((date >> 12) & 0x0f) * 10) + ((date >> 8) & 0x0f); /* day */
	w = (((date >> 20) & 0x0f) * 10) + ((date>>16) & 0x0f) + 2;	 /* month */
	DDate |= (w << 4);
	w = (((date >> 28) & 0x0f) * 10) + ((date >> 24)  & 0x0f); 	 /* year */
	DDate |= (w + 1900 - 1980) << 9;
	/* Do the time */
	DTime = (((((time >> 4) & 0x0f) * 10) + (time & 0x0f))/2);	/* secs/2 */
	w = (((time >> 12) & 0x0f) * 10) + ((time >> 8) & 0x0f);
	DTime |= (w << 5);											/* mins */
	w = (((time >> 20) & 0x0f) * 10) + ((time >> 16) & 0x0f);	/* hours */
	DTime |= (w << 11);
	*pTimeRet = DTime;
	*pDateRet = DDate;
}


/*******************************************************
  This updates a directory entry by reading in the
  sector it came from and placing the modifed entry
  into it then writing it back to disk.  The date is
  also updated at this time.
********************************************************/
static U32 UpdateDirEnt(U32 iFCB)
{
U32 erc, i, j;
U8 Drive;
	Drive = paFCB[iFCB]->Ldrv;		/* What logical drive are we on? */
	i = paFCB[iFCB]->LBADirSect;	/* Sector on disk */
	j = paFCB[iFCB]->oSectDirEnt;	/* offset in sector */

	/* update time in dir entry */
	GetFATTime(&paFCB[iFCB].Time, &paFCB[iFCB].Date);

	/* Read sector into a buffer */
	erc = DeviceOp(Ldrv[Drive].DevNum, 1, i, 1, abDirSectBuf);

	if (!erc) 
	{
	    CopyData(&paFCB[iFCB], &abDirSectBuf[j], 32);
		erc = DeviceOp(Ldrv[Drive].DevNum, 2, i, 1, abDirSectBuf);
	}
  return erc;
}


/*******************************************************
  Checks the validity of the a file handle and also
  returns the index to the FCB if the handle is OK.
  The function return OK (0) if handle is good, else
  a proper error code is returned.
********************************************************/
static U32 ValidateHandle(U32 dHandle, U32 *iFCBRet)
{
  /* get some checks out of the way first */

  if (dHandle < 4) return ErcBadFileHandle;
  if (dHandle >= nFUBs) return ErcBadFileHandle;
  if (!paFUB[dHandle].Job) return ErcBadFileHandle;

  /* Looks like a valid handle */
  *iFCBRet = paFUB[dHandle]->iFCB;
  return 0;
}

/*********************************************
  Returns absolute disk address for the
  cluster number you specify. This gives us
  the LBA of the first sector of data that
  the cluster number represents.
  The sector number is returned from the fucntion.
  Uses: Ldrv[CrntDrv].LBAData
  		Ldrv[CrntDrv].SecPerClstr
**********************************************/

static U32 ClsToLBA(U16 Clstr, U8 Drive)
{
U32 LBA;

 Clstr-=2;		/* Minus 2 cause 0 and 1 are reserved clusters */
 LBA = Ldrv[Drive].SecPerClstr * Clstr;
 LBA += Ldrv[Drive].LBAData;
 return LBA;
}


/*******************************************************
  This writes out the specified FAT sector back into
  the FAT. It also checks to see if there is more
  than one copy of the fat and updates the second copy
  if it exists.
********************************************************/
static U32 UpdateFAT(U32 iFAT)
{
U32 erc, i, k;
U8 Drive;

  erc = 0;
  if (Fat[iFAT].fModLock & FATMOD)
  {	/* Modified?? */

	Drive = Fat[iFAT].Drive;		/* What logical drive are we on? */
    i = Fat[iFAT].LBASect;				/* Where to write it back */

	if (!iFAT)
	{			/* This is the floppy buffer [0] */
		/* set up to write upto 3 sectors from the buffer */

		if (i+2 < Ldrv[Drive].sFAT + Ldrv[Drive].LBAFAT)
			k = 3;
		else if (i+1 < Ldrv[Drive].sFAT + Ldrv[Drive].LBAFAT)
			k = 2;
		else
			k = 1;
	}
	else
		k=1;

	erc = DeviceOp(Ldrv[Drive].DevNum, 2, i, k, Fat[iFAT].pBuf);
	if (!erc)
	{
		Fat[iFAT].fModLock &= ~FATMOD;	/* Not modified anymore */
		if (Ldrv[Drive].nFATS > 1)  
		{ 	/* 2 FATS! */
			/* if we have two FATS we must update the second fat
			also. This will be located directly aftrer the first
			FAT (by exactly	LDrv.sFat sectors).
			*/
			i+= Ldrv[Drive].sFAT;
			erc = DeviceOp(Ldrv[Drive].DevNum, 2, i, k, Fat[iFAT].pBuf);
		}
	}
  }
  return erc;
}

/*******************************************************
 Reads in the FAT sector that contains the Cluster we
 specified into a FAT buffer if it isn't already in
 one.  The index to the FAT buffer is returned.
 Returns Error if not in FAT.
 Uses: Ldrv[LDrive].LBAFAT
	   Ldrv[LDrive].fFAT16
	   Ldrv[LDrive].DevNum
 Each sector of the FAT contains 256 cluster entries
 for FAT16 types. To find it, we  Divide the cluster
 number by the count of entries (256), and add this
 to the beginning sector of the FAT.  It is SOOO
 important (for speed) to have the FAT sectors in
 memory, that we allocate the FAT buffers on a Least
 Recently Used (LRU) basis for hard disk drives.

 It's more complicated for a FAT12 types (floppies)
 because cluster entries span fat sectors (they have
 an odd number of nibbles). For this reason, we have
 one 3 sector fat buffer for fat12 devices (floppies).
 We fill it with up to 3 sectors. This is because the
 last entry may span the sectors and we must be able
 to read it.  There are 1024 cluster entries in
 a FAT12 3 sector buffer.
*******************************************************/

static U32 FindFatSect(U8 Drive, U16 Clstr, U32 *piFatRecRet, U8 fLock)
{
U32 i, j, k;
U32 first, oSector, erc, LRU, iLRU, iFound, Tick;
U16 MaxClstr;

  if (Ldrv[Drive].fFAT16)
	MaxClstr = 0xfff8;
  else
	MaxClstr = 0xff8;	/* FAT12 */

 if (Clstr >= MaxClstr)
 	return(ErcEOF);

 if (Clstr < 2)
 {
 	return (ErcBadFATClstr);
 }

 GetTimerTick(&Tick);

 erc = 0;		/* default to no error */

 /* Set oSector to offset of sector in FAT
    There are 256 cluster entries in 1 sector of a FAT16,
    and 1024 in a FAT12 (3 sectors)
 */

 if (Ldrv[Drive].fFAT16)
 {
	oSector = Clstr/256;
	first = Clstr-(Clstr%256);

	/* Set i to LBA of FAT sector we need by adding
	   offset to beginning of FAT
	*/

	i = oSector + Ldrv[Drive].LBAFAT;

	/* If FAT sector is out of range there's a BAD problem... */

	if (i >= Ldrv[Drive].sFAT + Ldrv[Drive].LBAFAT)
	{
	 	return (ErcBadFATClstr);
	}
	else
	{   /* Else we get it for them */

		/* Loop through the Fat bufs and see if its in one already. */
		/* Save the index of the LRU in case it's not there. */
		/* Set iFound to index of FatBuf (if found). */
		/* Otherwise, Set up iLRU to indicate what the oldest buffer is  */

		iFound = 0xffffffff;
		LRU = 0xffffffff;	/* saves tick of oldest one so far */
		iLRU = 1;			/* default */
		for (j=1; j<nFATBufs; j++)
		{
			if (Fat[j].LastUsed > 0)
			{		/* Valid ? (ever been used) */
				if ((first == Fat[j].iClstrStart) &&
					(Drive == Fat[j].Drive))
					{
						iFound = j;
						if (fLock)
		                    Fat[j].fModLock |= FATLOCK;
						break;		/* Already IN! */
					}
			}
			if (Fat[j].LastUsed < LRU)
			{
				LRU = Fat[j].LastUsed;
				iLRU = j;
			}
		}

		if (iFound != 0xffffffff)
		{								/* Its already in memory */
	        Fat[j].LastUsed = Tick;		/* update LRU */
		}
		else
		{       			   		/* else put into oldest buffer */
			j = iLRU;

			/* Check to see if Fat[iLRU] is valid and has been
			   modified. If it is, write it out before we read
			   the next one into this buffer. This done by
			   calling UpdateFAT(iFatRec).
			*/
	        if (Fat[j].fModLock & FATMOD)
		        erc = UpdateFAT(j);

			if (!erc)
			{
				erc = DeviceOp(Ldrv[Drive].DevNum, 1, i, 1, Fat[j].pBuf);
				Fat[j].Drive = Drive;			/* Update Drive */
	    	    Fat[j].LastUsed = Tick;			/* update LRU */
	        	Fat[j].iClstrStart = first;		/* update first cluster num */
		        Fat[j].LBASect = i;				/* LBA this FAt sect came from */
			}
		}
	}
 }

	/* This is for FAT12s */

 else
 {
	oSector = (Clstr/1024) * 3;	 /* X3 cause we read 3 at a time */
	first = Clstr-(Clstr%1024);

	/* Set i to LBA of FAT sector we need by adding offset (oSector)
	   to beginning of FAT */

	i = oSector + Ldrv[Drive].LBAFAT;
	j = 0;

	/* If FAT sector is out of range there's a BAD problem... */

	if (i >= Ldrv[Drive].sFAT)
	 	return (ErcBadFATClstr);
	else
	{   /* Else we get it for them */

		/* Check the single floppy fat buf and see if its already there. */
		/* Set iFound to index of FatBuf (if found). */

		iFound = 0xffffffff;

		if (Fat[0].LastUsed > 0)
		{		/* Valid ? (nonzero means it's been used) */
			if ((first == Fat[0].iClstrStart) &&
				(Drive == Fat[0].Drive))
			{
				iFound = 0;
				if (fLock)
					Fat[0].fModLock |= FATLOCK;
			}
		}

		if (iFound == 0xffffffff)
		{
			/* It's not the one we want or isn't there.
			   Check to see if Fat[0] is valid and has been
			   modified. If it is, write it out before we read
			   the one we want into the buffer. This done by
			   calling UpdateFAT(iFatRec).
			*/
			if (Fat[0].fModLock & FATMOD)
			        erc = UpdateFAT(0);

			/* set up to read upto 3 sectors into buffer */

			if (i+2 < Ldrv[Drive].sFAT + Ldrv[Drive].LBAFAT)
				k = 3;
			else if (i+1 < Ldrv[Drive].sFAT + Ldrv[Drive].LBAFAT)
				k = 2;
			else
				k = 1;

			if (!erc)
			{
				erc = DeviceOp(Ldrv[Drive].DevNum, 1, i, k, Fat[0].pBuf);

				Fat[0].Drive = Drive;			/* Update Drive */
		        Fat[0].LastUsed = Tick;			/* update LRU */
	    	   	Fat[0].iClstrStart = first;		/* update first cluster num */
		        Fat[0].LBASect = i;		/* LBA this FAT sect came from */
			}
		}
 	}
 }


 *piFatRecRet = j;  /* Buffer that holds the sector(s) */

 return (erc);		/* Disk error Bad news */
}

/*********************************************
  Returns the value found for this cluster
  entry in a fat sector buffer.  Values can be:
                              FAT16     FAT12
  	Next entry in the chain (0002-FFF0 (002-FF0)
  	Last entry in chain     (FFF8     )(FF8    )
  	Available cluster       (0        )(0      )
  	Bad Cluster             (FFF7     )(FF7    )
	(other vlaues are reserved).
**********************************************/

static U32 GetClstrValue(U16 Clstr, U8 Drive, U8 fLock,
                         U16 *pValRet, U32 *iFatBufRet)
{
U32 erc, oClstr, iFat;
U16 ClstrVal, *pClstr;

	erc = FindFatSect(Drive, Clstr, &iFat, fLock);

	if (erc)
	{
		*pValRet= 0;
		return(erc);
	}

	pClstr = Fat[iFat].pBuf;
	oClstr = Clstr - Fat[iFat].iClstrStart;    /* offset into FatBuf */

	if (Ldrv[Drive].fFAT16)
	{	/* if drive is FAT16 type */
		pClstr += oClstr * 2;     				/* WORDS in */
		ClstrVal = *pClstr;
	}
		/* FAT12 entries are 1.5 bytes long (what a pain).
		   This means we get the offset and see whether it
		   is an odd or even byte, then take the proper nibble
		   by ANDing or shifting.
		*/
	else
	{						/* a FAT12... */
		pClstr += oClstr + (oClstr/2);			/* 1.5 bytes in */
		ClstrVal = *pClstr;					/* We have 16 bits */
		if (Clstr & 1)                 			/* Odd, must shift */
			ClstrVal >>= 4;
		ClstrVal &= 0xfff;
	}
	*pValRet= ClstrVal;
    *iFatBufRet = iFat;

	return(erc);
}


/*************************************************
  Sets the value in Clstr to the value in
  NextClstr which will be one of the following
  values:                     FAT16     FAT12
  	Next entry in the chain (0002-FFEF (002-FEF)
  	Last entry in chain     (FFFF     )(FFF    )
  	Available cluster       (0        )(0      )
  	Bad Cluster             (FFF7     )(FF7    )
	(other vlaues are reserved).
  This marks the associated fat buffer as modified.
  This is the ONLY call that modifies a FAT buffer!
**************************************************/

static U32 SetClstrValue(U16 Clstr, U16 NewClstrVal, U8 Drive, U32 *iFatBufRet)
{
U32 erc, oClstr, iFat;
U16 ClstrVal, *pClstr, ClstrSave;

	erc = FindFatSect(Drive, Clstr, &iFat, 0);
	if (erc)
	{
		*iFatBufRet = 0;
		return(erc);
	}

	pClstr = Fat[iFat].pBuf;
	oClstr = Clstr - Fat[iFat].iClstrStart;    /* offset into FatBuf*/
	if (Ldrv[Drive].fFAT16) 
	{	/* if drive is FAT16 type */
		pClstr += oClstr * 2;     				/* WORDS in */
		*pClstr = NewClstrVal;
	}
		/* FAT12 entries are 1.5 bytes long (remember??).
		   SAVE THE CORRECT NIBBLE OF THE ADJACENT CLUSTER!!
		*/
	else
	{						/* a FAT12... */
		pClstr += oClstr + (oClstr/2);			/* 1.5 bytes in */
		ClstrSave = *pClstr;						/* We have 16 bits */
		if (Clstr & 1) 
		{        	   			/* Odd, must shift */
			NewClstrVal <<= 4;
			NewClstrVal &= 0xfff0;
			ClstrVal = (ClstrSave & 0x0F) | NewClstrVal;
		}
		else 
		{
			NewClstrVal &= 0x0fff;
			ClstrVal = (ClstrSave & 0xf000) | NewClstrVal;
		}
		*pClstr = ClstrVal;
	}
    Fat[iFat].fModLock |= FATMOD;
    *iFatBufRet = iFat;
	return(erc);
}


/*************************************************
 Read the FAT and get the cluster number for the
 next cluster in the chain for the Clstr specifed.
 This returns 0 and an error if failed.
 0 is an illegal cluster number.
 Remember, the cluster you are on is actually the
 number of the next cluster in a linked list!
**************************************************/

static U32 NextFATClstr(U8 Drive, U16 Clstr, U16 *pNextClstrRet)
{
U32 erc, i;
U16 NextClstr;

	erc = GetClstrValue(Clstr, Drive, 0, &NextClstr, &i);

	if (erc)
	{
		*pNextClstrRet = 0;
		return(erc);
	}
	*pNextClstrRet = NextClstr;
	return(0);
}

/*************************************************
 This allocates the next empty cluster on the disk
 to the tail of the clstr that is passed in.
 LastClstr is a valid cluster of a file or 
 directory (and MUST be the last one).  
 We error out if it isn't!
 This returns 0 and an error if it fails.
 Remember, the cluster you are on is actually the
 number of the next cluster in a linked list!
 This looks through the current and succesive
 FAT sectors (if needed) to add to the file.
 A cluster is available to allocate if it is
 0.  This is strictly a first fit algorithm.
**************************************************/

static U32 ExtendClstrChain(U8 Drive, U16 LastClstr, U16 *pNextClstrRet)
{
U32 erc, i, j, k;
U16 ClstrValue, MaxClstr, CrntClstr;
U8 fFound;

	if (Ldrv[Drive].fFAT16)
		MaxClstr = 0xfff8;
	else
		MaxClstr = 0xff8;	/* FAT12 */

	/* i is index to Fat with last sector of current chain */

	erc = GetClstrValue(LastClstr, Drive, 1, &ClstrValue, &i);
	if (erc) 
	{
		*pNextClstrRet = 0;
		return(erc);
	}

	if (ClstrValue < MaxClstr) 
	{		/* no need to extend it */
    	*pNextClstrRet = ClstrValue;
        Fat[i].fModLock &= ~FATLOCK;		/* unlock it */
		return(0);
	}

	/* OK... now we have the Fat sector and the offset in the Fat
	buf of the last	cluster allocated to this file.  Let's go
	further into the buffer and try to get an empty one.
	*/

	CrntClstr = LastClstr;
	fFound = 0;
	while (!fFound) 
	{
		++CrntClstr;		/* next cluster */
		erc = GetClstrValue(CrntClstr, Drive, 0, &ClstrValue, &j);
		if (erc) 
		{
			*pNextClstrRet = 0;
	        Fat[i].fModLock &= ~FATLOCK;	/* unlock previous lastclstr */
			return(erc);
		}
		if (!ClstrValue) 
		{
			fFound = 1; 	/* found an empty one */
		}
	}

	if (fFound) 
	{	/* CrntClstr is index to empty one */

		/* Set the LastCluster to point to the new cluster found */

		erc = SetClstrValue(LastClstr, CrntClstr, Drive, &k);
		if (erc) 
		{
			*pNextClstrRet = 0;
	        Fat[i].fModLock &= ~FATLOCK;	/* unlock previous lastclstr */
			return(erc);
		}
        Fat[k].fModLock &= ~FATLOCK;		/* unlock it */

		/* Set the newcluster to "end Cluster" chain value */

		erc = SetClstrValue(CrntClstr, 0xFFFF, Drive, &j);
	}
	*pNextClstrRet = CrntClstr;
	return(erc);
}

/*************************************************
 This truncates the file chain to the cluster
 specified (makes it the last cluster).
 This means we walk the rest of the chain setting
 all the entries to 0 (so they can be reallocated).
 This returns an error if failed.
**************************************************/

static U32 TruncClstrChain(U8 Drive, U16 Clstr)
{
U32 erc, i;
U16 MaxClstr, NextClstr, CrntClstr;

	if (Ldrv[Drive].fFAT16)
		MaxClstr = 0xfff8;
	else
		MaxClstr = 0xff8;	/* FAT12 */

	/* i will be index to FatRec with last sector of current chain */

	erc = GetClstrValue(Clstr, Drive, 0, &NextClstr, &i);
	if (erc)
		return(erc);

	if (NextClstr >= MaxClstr)
	{		/* no need to truncate it */
		return(0);						/* It's already the end. */
	}

	/* OK... now we cut it off all the way down the chain.
	We start by placing MaxClstr in the last sector and
	then 0 in all entries to the end of the chain.
	*/

	erc = GetClstrValue(Clstr, Drive, 0, &NextClstr, &i);
	if (erc)
		return(erc);
	erc = SetClstrValue(Clstr, 0xFFFF, Drive, &i);  /* new end of chain */
	if (erc)
		return(erc);

	while ((NextClstr) && (NextClstr < MaxClstr)) 
	{
		CrntClstr = NextClstr;
		erc = GetClstrValue(CrntClstr, Drive, 0, &NextClstr, &i);
		if (erc)
			return(erc);
		erc = SetClstrValue(CrntClstr, 0, Drive, &i);  /* Free it up */
		if (erc)
			return(erc);
	}

	/* DONE! */
	return(0);
}

/********************************************************
  This finds the absolute cluster you want from the
  LFA in a particular file. The file handle must already
  be validated!  It also returns the relative LFA of
  the beginning of this cluster.
*********************************************************/

static U32 GetAbsoluteClstr(U32 dHandle, U32 dLFA,
							U16 *pClstrRet, U32 *prLFARet)
{
U32 erc, iFCB, spc, bpc, rLFA;
U16 rClstrWant, rClstrNow, Clstr, MaxClstr;
U8 Drive;

  iFCB = paFUB[dHandle]->iFCB;
  Drive = paFCB[iFCB]->Ldrv;		/* What logical drive are we on? */
  spc = Ldrv[Drive].SecPerClstr;	/* sectors per cluster */
  bpc = spc * 512;					/* bytes per cluster */

 if (Ldrv[Drive].fFAT16)
 	MaxClstr = 0xfff8;
 else
 	MaxClstr = 0xff8;	/* FAT12 */

/*
  Calculate relative by dividing cluster size in bytes by dLFA.
  If zero, we want the 1st cluster which is listed in the FCB.
  If it is greater than zero, we have to "walk the FAT cluster
  chain" until we reach the one we want, then read it in.

  The FUB fields LFAClstr and Clstr store the file LFA of the last
  cluster in this file that was read or written.  This means if the
  LFA is higher than the last read or written, we don't waste the
  time reading the whole chain. We start from where we are.

  The major difference is we may not be reading the first sector
  in the cluster. We figure this out from the dLFA as compared to
  LFAClstr.

*/

  rClstrWant = dLFA / bpc;  					/* Relative clstr they want */
  rClstrNow = paFUB[dHandle]->LFAClstr / bpc;	/* Rel 'Clstr' in FUB */

  if (rClstrWant < rClstrNow)
  {			/* Is it earlier in the file? */
	Clstr = paFCB[iFCB]->StartClstr;		/* Yes, start at the beginning */
	rClstrNow = 0;
	rLFA = 0;
  }
  else
  {
	Clstr = paFUB[dHandle]->Clstr;			/* No, start at current cluster */
	rLFA = paFUB[dHandle]->LFAClstr;		/* LFA of this cluster */
  }

  /* We need to run the cluster chain if rClstrNow < ClstrWant */

  while ((rClstrNow < rClstrWant) &&	/* haven't reach it yet */
         (Clstr < MaxClstr) &&			/* Not last cluster */
         (Clstr))
 {					/* A valid cluster */
	  erc = NextFATClstr(Drive, Clstr, &Clstr);
	  if (erc)
	  	return(erc);
	  ++rClstrNow;
	  rLFA += bpc;
  }

  if (rClstrNow != rClstrWant)			/* Cluster chain appears broken... */
    return ErcBrokenFile;

  *pClstrRet = Clstr;
  *prLFARet = rLFA;
  return(0);
}


/*******************************************************
  SetFileSize sets the FileSize entry in the FCB for
  the handle specified.  This means we will allocate
  or deallocate clusters as necessary to satisfy this
  request. The file MUST be open in MODE MODIFY.
  This must be done before a file can be written
  to beyond current FileSize (Block and Stream types).
********************************************************/

static U32 SetFileSizeM(U32 dHandle, U32 dSize)
{
U32 erc, i, iFCB, rLFA, lfaEOF;
U32 CrntSize, nCrntClstrs, spc, bpc, nClstrsWant;
U16 Clstr;
U8 Drive;

  erc = ValidateHandle(dHandle, &iFCB);
  if (erc)
  	return erc;
  if (!paFCB[iFCB]->Mode)
  	return ErcReadOnly;

  Drive = paFCB[iFCB]->Ldrv;		/* What logical drive are we on? */
  spc = Ldrv[Drive].SecPerClstr;	/* sectors per cluster */
  bpc = spc * 512;					/* bytes per cluster */

  /* Looks like it's valid to change the size */

  CrntSize = paFCB[iFCB]->FileSize;
  if (CrntSize)
	  lfaEOF = CrntSize - 1;
  else
	  lfaEOF = 0;

  if (CrntSize == dSize)		/* No need to do anything! */
  	return(0);

  nCrntClstrs = CrntSize/bpc; 	/* nClusters currently  */
  if (CrntSize%bpc)
  	nCrntClstrs++;

  if (!CrntSize)
  	nCrntClstrs = 1;			/* ZERO length files have 1 Clstr! */

  nClstrsWant = dSize/bpc;  	/* nClusters they we need  */
  if (dSize%bpc)
  	nClstrsWant++;

  if (!dSize)
  	nClstrsWant = 1;			/* ZERO length files have 1 Clstr! */


  if (nClstrsWant == nCrntClstrs)
	erc = 0;

  else if (nClstrsWant > nCrntClstrs)
  {	/* Need to extend allocation */

	/* get the last cluster in the file */
	erc = GetAbsoluteClstr(dHandle, lfaEOF, &Clstr, &rLFA);
	i = nCrntClstrs;
	while ((!erc) && (i < nClstrsWant))
	{
			erc = ExtendClstrChain(Drive, Clstr, &Clstr);
			i++;
	}
  }
  else if (nClstrsWant < nCrntClstrs)
  { /* Need to truncate it */

	/* Get to cluster where it should be truncated. Set lfaEOF
	to NEW lfaEOF to find where last valid cluster should be. */

	if (dSize)
		lfaEOF = dSize - 1;
	else
  	  lfaEOF = 0;

	erc = GetAbsoluteClstr(dHandle, lfaEOF, &Clstr, &rLFA);
	if (!erc)
		erc = TruncClstrChain(Drive, Clstr);

	/* Now we must ensure that the cluster helper is NOT
	beyond EOF!
	*/
    if (paFUB->LFAClstr >= dSize)
    {
        paFUB->LFAClstr = 0;
        paFUB[dHandle]->Clstr = paFCB[iFCB]->StartClstr;
	}

  }
  if (!erc)
  {
	paFCB[iFCB]->FileSize = dSize;
	paFCB[iFCB]->fMod = 1;
  }

  return erc;
}



/*******************************************************************
 This searches a directory beginning at Clstr for pName and returns
 a pointer to the 32 byte dir entry which is in a temporary buffer.
 If not found, returns NIL. When searching directories, if the
 filename begins with a NULL the search need go no futher!
********************************************************************/

static U32 GetDirEnt(U8  *pName,
              U8  Drive,
              U16 Clstr,
              U32 *pLBARet,
              U32 *poEntRet,
              U8  **pEntRet)
{
unsigned long sector, i, j, k, erc;
U8 fFound, fEnd, *pEnt, *pStart;
U16 MaxClstr;

 j = Ldrv[Drive].SecPerClstr;		/* How many sectors per cluster */
 sector = ClsToLBA(Clstr, Drive);	/* absolute sector of first dir sector */
 if (Ldrv[Drive].fFAT16)
 	MaxClstr = 0xfff8;
 else
 	MaxClstr = 0xff8;	/* FAT12 */
 i = 0;
 fEnd=0;
 fFound=0;

  fFound= 0;
  while ((!fFound) && (!fEnd)) 
  {		/* while there are valid entries */
	if (i==j) 
	{		/* reached last dir sector of this cluster */
		erc = NextFATClstr(Drive, Clstr, &Clstr);
		if (!erc) 
		{
			if (Clstr >= MaxClstr)				/* last sector */
				return(ErcNoSuchFile);			/* not found */
			sector = ClsToLBA(Clstr, Drive);	/* LBA of next dir sector */
			i=0;
		}
		else 
		{
			*pEntRet = 0;
			return(erc);
		}
	}

	erc = DeviceOp(Ldrv[Drive].DevNum, 1, sector++, 1, abDirSectBuf);
	if (erc)
		return(erc);

	++i;		/* next sector in cluster */

    pEnt = &abDirSectBuf[0];
    pStart = pEnt;

	for (k=0; k<16; k++) 
	{		/* 16 entries per sector */
		if (*pEnt==0) 
		{			/* 0 in a DirEnt stops search */
			fEnd=1;
			break;
		}

		if (CompareNCS(pEnt, pName, 11) == -1)
		{
			fFound=1;
            *pLBARet = sector-1;		/* tell em what LBA of DirEnt */
            *poEntRet = pEnt-pStart;	/* Tell em offset in LBA */
			break;
		}
		pEnt+=32;	/* 32 byte per entry */
		}
	}
  if (fFound) 
  {
  	*pEntRet = pEnt;
  	return(0);
  }
  else return (ErcNoSuchFile);
}


/*****************************************************
 This searches the ROOT directory for pName
 and returns a pointer to the 32 byte entry which
 is in a temporary buffer. If not found, returns NIL;
 When searching directories, if the filename begins
 with a NULL the search need go no futher!
*****************************************************/

static U32 GetRootEnt(U8  *pName,
               U8  Drive,
               U32 *pLBARet,
               U32 *poEntRet,
               U8  **pEntRet)
{
unsigned long i, j, k, erc;
U8 fFound, fEnd, *pEnt, *pStart;

 i = Ldrv[Drive].LBARoot;
 j = Ldrv[Drive].nRootDirEnt;

 fFound = 0;
 fEnd = 0;
 while ((j) && (!fFound) && (!fEnd)) 
 {	/* while there are valid entries */

	erc = DeviceOp(Ldrv[Drive].DevNum, 1, i++, 1, abRawSector);

	if (erc)
		return(erc);

    pEnt = abRawSector;
    pStart = pEnt;
	for (k=0; k<16; k++) 
	{
		if (*pEnt==0) 
		{			/* 0 in a DirEnt stops search */
			fEnd=1;
			break;
		}
		if (CompareNCS(pEnt, pName, 11) == -1) 
		{
			fFound=1;
            *pLBARet = i-1;				 /* tell em what LBA of DirEnt */
            *poEntRet = pEnt-pStart;	 /* Tell em offset in LBA */
			break;
		}
		--j;		/* one less dir ent */
		pEnt+=32;	/* 32 byte per entry */
	}
 }
 if (fFound) 
 {
 	*pEntRet = pEnt;
 	return(0);
 }
 else
	return (ErcNoSuchFile);
}

/********************************************
RAB -  This builds a full file specification from
  pName and places it in pDest based on the
  path from iJob. cbDestRet is set to size.
  If the name starts with "DRIVE:"
  then the path from the JCB is NOT used. Otherwise
  the pName is cancatenated to the job's path.
  If it begins with "\" only DRIVE: (first two
  chars of path) are used.
*********************************************/

static void BuildSpec(char *pName,
			   long cbName,
			   char *pDest,
			   long *cbDestRet,
			   long iJob)
{
long i;
char pathtmp[70];

 if ((cbName) && (pName) && (pName[1] == ':'))
 {		/* Do NOT use path */
	 CopyData(pName, pDest, cbName);
	 i = cbName;
 }
    /* use only drive and semicolon */
 else if ((cbName) && (pName) && (pName[0] == 0x5C)) /* begins with backslash */
 {
	GetPath(iJob, pathtmp, &i);
	pDest[0] = pathtmp[0];
	pDest[1] = pathtmp[1];
	i = 2;
	CopyData(pName, &pDest[2], cbName);
	i += cbName;
 }
 else
 {								/* Use whole path as prefix */
	i = 0;
	GetPath(iJob, pDest, &i);
	if ((cbName) && (pName))
	{
		if ((pName) && (cbName))
		{
			CopyData(pName, &pDest[i], cbName);
			i += cbName;
		}
	}
 }
 *cbDestRet = i;
}



/*****************************************************
 The parses out the path name into directories, filename,
 and extension  (Example):
 C:\TEMP\TEST.TXT  (with TEMP being a dir in the root).
 This also uses the current path for the job to build
 the filename.
 SpecDepth is set to the level of the last valid
 11 character string (Specdepth is 0-6).
*****************************************************/

static U32 ParseName(U8 *pName, U32 cbName, U32 iJob)
{
unsigned long i, j, k, erc;
U8 c, *pPart;
char Spec[70];
U32 cbSpec;

 erc = 0;
 FDrive = 0;

 FillData(FileSpec, (7*11), ' '); 		/* Fill parse table with spaces */

 if ((cbName) && (*pName == ' '))
	erc = ErcBadFileSpec;

 BuildSpec(pName, cbName, Spec, &cbSpec, iJob);

 j = 0;		/* index into crnt part of spec */
 k = 0;		/* index into crnt tree level   */
 pPart = Spec;
 for (i=0; i < cbSpec; i++)
 {
	switch (c = *pPart++)
	{
		case 0x5c  : 	/* '\' separates dir or fname */
			if (j>0)
			{  /* if it's not the first one */
				++k;
				j=0;
			}
			break;
		case ':' :
			if ((j==1) && (k==0) && (FDrive==0))
			{
				FDrive = FileSpec[0][0] & 0xdf;  /* Make drive Upper*/
                FileSpec[0][0] = ' ';
				j=0;			/* back to beginning of part */
				k=0;
			}
			else erc = ErcBadFileSpec;
			break;
		case '.'  :  			/* . can only appear once in dir or fname */
			if (j>8) erc = ErcBadFileSpec;
			else j=8;						/* move to extension */
			break;
		case '>'  :							/* not allowed in spec */
		case '<'  :
		case ','  :
		case '+'  :
		case '|'  :
		case ']'  :
		case '['  :
		case '+'  :
		case '='  :
		case '@'  :
		case '*'  :
		case '?'  :
			 erc = ErcBadFileSpec;
			break;
		default   :							/* make chars upper */
			if (j>10)
				erc = ErcBadFileSpec;
			else
			{
				if (((c >= 'A') && (c <= 'Z')) ||
				    ((c >= 'a') && (c <= 'z')))
					   c &= 0xdf;
	            FileSpec[k][j] = c;
	            ++j;
	        }
			break;
		}

	if (erc) break;		/* bad news. Exit for loop */
	}
SpecDepth = k;
return erc;
}


/*** Get Directory Sector *******************************
 Gets a 512 byte directory sector in sequence number from
 the directory path given. SectNum = 0 to nMax for Dir.
 This also returns the LBA for sector itself for
 internal users of this call.
********************************************************/

static U32 GetDirSectorM(char *pPath,
				 long cbPath,
				 char *pSectRet,
				 long cbRetMax,
				 long SectNum,
				 long *LBARet,
				 U16  *ClstrRet,
				 long iJob)
{
U32 sector, i, j, k, erc, spc, level, iSect;
U16 MaxClstr, Clstr, rClstr;
U8  fFound, *pEnt, Drive;

  if (cbRetMax > 512)	/* WHOA Bub, 1 Sector at a time! */
  	cbRetMax = 512;

  erc = ParseName(pPath, cbPath, iJob);

	/* The entire path has now been parsed out into an array of
	   arrays, each 11 bytes long that contain each directory name
	   in the path for the sector they want.
	   The first is always the root (entry 0).
	   The drive will be a letter in FDrive.
	*/

  if ((FDrive > 0x40) && (FDrive < 0x52))	 	/* A to J */
  	Drive = FDrive - 0x41;						/* Make it 0-9 */
  else
  	return(ErcNoSuchDrive);

  if (Drive < 2)
  {
	StatFloppy(Drive);
	erc= read_BS(Drive);
  }

  if (Ldrv[Drive].DevNum == 0xff)
	return(ErcNoSuchDrive);

  i = Ldrv[Drive].LBARoot;
  j = Ldrv[Drive].nRootDirEnt;

  if (FileSpec[0][0] == ' ')
  {		/* They want sector in root */
  	if (SectNum > j/32)				/* Beyond Root entries! */
		return(ErcNoMatch);

	/* Else we can give them the sector NOW */
	erc = DeviceOp(Ldrv[Drive].DevNum, 1, i+SectNum, 1, abRawSector);
	if (!erc) 
	{
		*LBARet = i+SectNum;
	  	CopyData(abRawSector, pSectRet, cbRetMax);
	}
	return(erc);
  }

	/* We have to run the root for a dir name... */

  fFound = 0;
  while ((j) && (!fFound))
  {	/* while there are valid entries */
	erc = DeviceOp(Ldrv[Drive].DevNum, 1, i++, 1, abRawSector);
	if (erc)
		return(erc);
    pEnt = abRawSector;		/* Point to first entry */
	for (k=0; k<16; k++) 
	{
		if (CompareNCS(pEnt, FileSpec[0], 11) == -1)
		{
			fFound=1;
			break;
		}
		--j;		/* one less dir ent */
		pEnt+=32;	/* 32 byte per entry */
	}
 }
 if (!fFound)
	return (ErcNoMatch);

  pDirEnt = pEnt;		/* Entry we just found in root was dir */

  if (!(pDirEnt->Attr & DIRECTORY))
  {
	return(ErcNoSuchDir);
  }

  if (Ldrv[Drive].fFAT16)
 	MaxClstr = 0xfff8;
  else
 	MaxClstr = 0xff8;	/* FAT12 */

  spc = Ldrv[Drive].SecPerClstr;	/* How many sectors per cluster */
  Clstr = pDirEnt->StartClstr;

  level = 1;	/* start at this directory+1, compare to FileSpec */

  while (!erc) 
  {	/* looking for Dir */

	if (FileSpec[level][0] == ' ')
	{ /* They want sector in this dir */

		if (!(pDirEnt->Attr & DIRECTORY))  
		{
			return(ErcNoSuchDir);
		}
		rClstr = SectNum /spc;	/* calc relative cluster from start clstr */
		iSect  = SectNum % spc;  /* Add this to cluster start for sector */
		sector = ClsToLBA(Clstr, Drive);	/* sector of first dir sector */
		while ((rClstr--) && (!erc))
			erc = NextFATClstr(Drive, Clstr, &Clstr);
		if (erc)
			return(erc);
		sector = ClsToLBA(Clstr, Drive);  /* LBA of this clstr */
		sector += iSect;
		erc = DeviceOp(Ldrv[Drive].DevNum, 1, sector, 1, abRawSector);
		if (!erc) 
		{
		  	CopyData(abRawSector, pSectRet, cbRetMax);
			*LBARet = sector;
			*ClstrRet = Clstr;
		}
		return(erc);
	}
	else 
	{  /* Else we must find this sub dir name */

		sector = ClsToLBA(Clstr, Drive);	/* sector of first dir sector */
		fFound=0;
		i = 0;

		while (!fFound)
		{		/* while there are valid entries */

			if (i==spc) 
			{		/* reached last dir sector of this cluster */
				erc = NextFATClstr(Drive, Clstr, &Clstr);
				if (!erc) 
				{
					if (Clstr >= MaxClstr)			 /* last sector */
						return(ErcNoSuchFile);		 /* not found */
					sector = ClsToLBA(Clstr, Drive); /* LBA of next sector */
					i=0;
				}
				else
					return(erc);
			}

			erc = DeviceOp(Ldrv[Drive].DevNum, 1, sector++, 1, abRawSector);
			if (erc)
				return(erc);
			i++;	/* Next sector in this cluster */

		    pEnt = &abRawSector[0];
			for (k=0; k<16; k++)
			{		/* 16 entries per sector */
				if (CompareNCS(pEnt, FileSpec[level], 11) == -1)
				{
					fFound=1;
					break;
				}
				pEnt+=32;	/* 32 byte per entry */
			}
		}
		pDirEnt = pEnt;				  /* Entry we just found */
	    Clstr = pDirEnt->StartClstr;  /* Clstr @ start of dir entry */
	}
	++level;		/* next level of parsed filespec */
  }
return (erc);
}

/*******************************************************
 This is the BLOCK read for the MMURTL DOS file system.
 It reads whole sectors and returns them to pBytesRet.
 There are NO internal filesystem buffers for this call.
 Data is returned directly to your buffer from the Disk.
 This is the fastest method for reading a file.
 This is also used internally to fill a stream buffer.
********************************************************/

static U32 ReadBlockM(U32 dHandle,
               U8  *pBytesRet,
               U32 nBytes,
               U32 dLFA,
               U32 *pdBytesRet,
               U8  fFill)			/* TRUE if filling a stream buffer */
{
U32 erc, j, LBA, iFCB, bpc, spc, nDone, rLFA, nLeft, nBlks;
U16 Clstr, MaxClstr, ClstrSav;
U8 Drive;

  erc = ValidateHandle(dHandle, &iFCB);		/* Sets iFCB if OK */
  if (erc) return erc;

  /* Certain FUB fields have different meanings in stream */

  if ((paFUB[dHandle].fStream) && (!fFill))
  {
	*pdBytesRet = 0;
  	return ErcStreamFile;
  }

  /* set these up in advance */

  nBlks = nBytes/512;					/* nBytes MUST be multiple of 512 */
  Drive = paFCB[iFCB]->Ldrv;			/* What logical drive are we on? */
  spc = Ldrv[Drive].SecPerClstr;		/* sectors per cluster */
  bpc = 512 * spc;                  	/* Bytes per cluster */
  if (Ldrv[Drive].fFAT16)
  	MaxClstr = 0xfff8;
  else
 	MaxClstr = 0xff8;	/* FAT12 */


  /* Call to find the absolute cluster on the the logical disk,
     and also the relative LFA of the cluster in question.
  */

  erc = GetAbsoluteClstr(dHandle, dLFA, &Clstr, &rLFA);

  LBA = ClsToLBA(Clstr, Drive);		/* Get LBA of the target cluster */

  /* Now LBA equals beginning of cluster that dLFA resides in.
     We must see which sector in Clstr is the starting LBA.  To do this
     we MOD (dLFA in sectors) by (sectors per cluster) and
     add the leftover amount (should be 0 to nSPC-1) to LBA before we
     read.  For example: if dLFA was 2560 and spc was 4, this
     would be 5 % 4 = 1.  We would add 1 to the LBA.
     This is only done for the first read in the loop.

     We also set nleft which is how many sectors are left in the
     current cluster we are reading from.
  */

  LBA += (dLFA/512) % spc;
  nLeft = spc - ((dLFA/512) % spc);
  nDone = 0;

  while ((nBlks) && (!erc))
  {	/* while buffer isn't full and no error */
	if (nBlks > nLeft)
		j = nLeft;
	else j = nBlks;

	paFUB[dHandle]->Clstr = Clstr;			/* Save Current cluster */
	paFUB[dHandle]->LFAClstr = rLFA; 		/* Save LFA for Clstr in FUB */

	erc = DeviceOp(Ldrv[Drive].DevNum, 1, LBA, j, pBytesRet);
	if (erc)
		break;
	pBytesRet += j * 512;	/* further into their buffer */
	nBlks -= j;
	nLeft -= j;
	nDone += j;

	if ((nBlks) && (!nLeft))
	{ 		/* current cluster has none left */
		nLeft = spc;
		ClstrSav = Clstr;
		erc = NextFATClstr(Drive, Clstr, &Clstr);	/* next FAT cluster */
		if (erc)
		{
			*pdBytesRet = nDone*512;
			return(erc);
		}
		rLFA += bpc;						 /* Update rel LFA of new cluster*/
	    if (Clstr >= MaxClstr)
	    	erc = ErcEOF; /* Last cluster */
	    if (!Clstr)
	    	erc = ErcBrokenFile;	 /* No good next cluster! */
		LBA = ClsToLBA(Clstr, Drive);		 /* Get LBA of the target cluster*/
	}
  }
  *pdBytesRet = nDone*512;

  return erc;		/* WE'RE DONE, return the error (if any) */
}


/*** Write Block ***************************************
 This is the BLOCK write for the MMURTL FAT file system.
 dLFA must be the LFA of a valid portion of the file and
 nBlks must not extend beyond the last cluster allocated
 to the file. IOW, you must call SetFileSize first.
 In a block write dLFA is always written on
 a sector boundry. We must make sure that the filesize
 will accomidate the sectors we want to write!
********************************************************/

static U32 WriteBlockM(U32 dHandle, char *pData, U32 nBytes,
                U32 dLFA, U32 *pdnBytesRet)
{
U32 erc, i, j, LBA, iFCB, bpc, spc, nDone, rLFA, nLeft, nBlks;
U32 nLeft1, LBA1, nBlks1;
U16 Clstr, MaxClstr;
U8 Drive;

  erc = ValidateHandle(dHandle, &iFCB);		/* Sets iFCB if OK */
  if (erc) return erc;

  nBlks = nBytes/512;
  dLFA = (dLFA/512)*512;		/* round LFA down to nearest sector */

  if (!paFCB[iFCB]->Mode)		/* Is it open in Modify?? */
  	return(ErcReadOnly);

  i = (paFCB[iFCB]->FileSize/512);	/* Set i nBlks in file max */
  if (paFCB[iFCB]->FileSize%512)
  	i++;

  j = (dLFA/512) + nBlks;	/* blocks to write past dLFA*/

  if (j > i)
  	return(ErcBeyondEOF);

  /* It seems OK to write the blocks out, so now let's DO IT! */

  Drive = paFCB[iFCB]->Ldrv;			/* What logical drive are we on? */
  spc = Ldrv[Drive].SecPerClstr;		/* sectors per cluster */
  bpc = 512 * spc;                  	/* Bytes per cluster */
  if (Ldrv[Drive].fFAT16)
  	MaxClstr = 0xfff8;
  else
 	MaxClstr = 0xff8;	/* FAT12 */

  erc = GetAbsoluteClstr(dHandle, dLFA, &Clstr, &rLFA);
  if (erc)
  	return(erc);

  LBA = ClsToLBA(Clstr, Drive);		/* Get LBA of the target cluster */

  /* Now LBA equals beginning of cluster that dLFA resides in.
     We must see which sector in Clstr is the starting LBA.  To do this
     we MOD (dLFA in sectors) by (sectors per cluster) and
     add the leftover amount (should be 0 to nSPC-1) to LBA before we
     write.  For example: if dLFA was 2560 and spc was 4, this
     would be 5 % 4 = 1.  We would add 1 to the LBA.
     This is only done for the first write in the loop.
     We also set nleft which is how many sectors are left in the
     current cluster we are writing to so we know when to
     move to the next cluster.
  */

  LBA += (dLFA/512) % spc;
  LBA1 = LBA;
  nLeft = spc - ((dLFA/512) % spc);
  nLeft1 = nLeft;
  nBlks1 = nBlks;
  nDone = 0;

  while ((nBlks) && (!erc))
  {	/* while blocks are left to write */
	if (nBlks > nLeft)
		j = nLeft;
	else j = nBlks;

	paFUB[dHandle]->Clstr = Clstr;			/* Save Current cluster */
	paFUB[dHandle]->LFAClstr = rLFA; 		/* Save LFA for Clstr in FUB */

	erc = DeviceOp(Ldrv[Drive].DevNum, 2, LBA, j, pData);
	if (erc)
		break;
	pData += (j * 512);	/* Update address */
	nDone += j;			/* Total blocks done so far */
	nBlks -= j;
	nLeft -= j;

	if ((nBlks) && (!nLeft))
	{ 		/* done with current cluster */
		nLeft = spc;
		erc = NextFATClstr(Drive, Clstr, &Clstr);	/* next FAT cluster */
		if (erc)
			return(erc);
		rLFA += bpc;						 /* Update rel LFA of new cluster*/
	    if ((Clstr >= MaxClstr) && (nBlks))	 /* Problem! */
	    {
	    	erc = ErcBeyondEOF; 			 /* Last cluster & they want more*/
		}
	    if (!Clstr) erc = ErcBrokenFile;	 /* No good next cluster! */
		LBA = ClsToLBA(Clstr, Drive);		 /* Get LBA of the target cluster*/
	}
  }
  *pdnBytesRet = nDone * 512;
  return erc;		/* WE'RE DONE, return the error (if any) */
}


/*********************************************************
 Fills the stream buffer for the Current LFA in the FUB.
 We simply figure out which relative LBA we want in the
 buffer and call ReadBlockM to fill it.  We then set
 LFABuf in the FUB to show the LFA of the first byte
 in the buffer.
**********************************************************/

static U32 FillStreamBuff(U32 dHandle, U8 fInitial)
{
U32 erc, i, LFA, cLFA, LFABuf, iFCB;
U32 sBuf;
U8  *pBuf;

  erc = 0;

  /* Set these up in advance */

  cLFA =  paFUB[dHandle]->CrntLFA;		/* Find out where we want to be */
  LFABuf = paFUB[dHandle]->LFABuf;		/* LFA of first byte in buffer now */
  pBuf = paFUB[dHandle]->pBuf;			/* Local ptr to buffer */
  sBuf = paFUB[dHandle]->sBuf;			/* size of buffer */
  iFCB = paFUB[dHandle]->iFCB;			/* FCB for this FUB */

	/*	If the file was just opened we fill with LFA 0 */

  if (fInitial)
  {
	erc = ReadBlockM(dHandle, pBuf, sBuf, 0, &i, TRUE);
    paFUB[dHandle]->LFABuf = 0;
  }

	/* Else If the LFA is already in the buffer we just exit OK */

  else if ((cLFA >= LFABuf) && (cLFA < (LFABuf + sBuf)))
  {
	erc = 0;
  }

	/* ELSE We must figure out what starting LFA we want and fill the buffer */

  else
  {
	LFA = (cLFA/512) * 512;		/* Round down to nearest sector */
	erc = ReadBlockM(dHandle, pBuf, sBuf, LFA, &i, 1);
    paFUB[dHandle]->LFABuf = LFA;
  }

  if (erc == ErcEOF)		/* We ignore this when filling the buffer */
  	erc = 0;
  return erc;		/* WE'RE DONE, return the error (if any, except EOF) */
}

/*******************************************************
 This is the STREAM read for the MMURTL FAT file system.
 Used in conjunction with Get & Set FileLFA, you can
 move to any portion of the file to read one or more bytes.
 Data is buffered in Page sized chunks by the file
 system. This is the easiest method for reading a file.
********************************************************/

static U32 ReadBytesM(U32 dHandle, U8 *pBytesRet, U32 nBytes, U32 *pdReadRet)
{
U32 erc, iFCB, sBuf, cLFA, fSize;
U32 nBytesDone, 	/* Total read so far */
	lfaEOB, 		/* LFA end of Buffer */
	nBytesOut;		/* Number of bytes to copy (this buffer full) */
U8  *pOut, *pBuf;	/* Next data to send caller from buffer. Local pbuf */

  erc = ValidateHandle(dHandle, &iFCB);		/* Sets iFCB if OK */
  if (erc) return erc;

  /* Certain FUB fields have different meanings in stream type file */

  if (!paFUB[dHandle]->fStream)
  	return ErcBlockFile;

  cLFA = paFUB[dHandle]->CrntLFA;			/* local cLFA */
  fSize = paFCB[iFCB]->FileSize;			/* local size */

  /* check and see if we are at EOF */

  if (cLFA >= fSize)
  {						/* early out */
    *pdReadRet = 0;
  	return ErcEOF;
  }

  /* We are not at EOF so we need to fill stream buff
     and then calculate how many bytes we can give them
     from this buffer full of data.
  */

  pBuf = paFUB[dHandle]->pBuf;			/* Local ptr to buffer/size */
  sBuf = paFUB[dHandle]->sBuf;
  nBytesDone = 0;

  while	((nBytesDone < nBytes) &&
		(cLFA < fSize) &&
		(!erc))
  {
	erc = FillStreamBuff(dHandle, 0);				/* Fill the buff */

	/* Find out the LFA at the end of the buffer since we
	   just filled it.  It may be less than the end since
	   we may be near EOF.
	*/

    lfaEOB = paFUB[dHandle]->LFABuf + sBuf -1;	/* LFA at End of Buffer */
    if (lfaEOB > fSize)							/* Beyond EOF? */
    	lfaEOB = fSize - 1;

	/* Calculate pointer to the next chunk out from stream buffer */

	pOut = pBuf + (cLFA - paFUB[dHandle]->LFABuf);

	/* Calc how many bytes we can get out of buffer (belongs to file) */

	nBytesOut = lfaEOB - cLFA + 1;
	if (nBytesOut > nBytes-nBytesDone)
		nBytesOut = nBytes-nBytesDone;

	/* Send bytes to pBytesRet */

	CopyData(pOut, pBytesRet, nBytesOut);

	pBytesRet += nBytesOut;     	/* Update pBytesRet */
	nBytesDone += nBytesOut;		/* Update nBytesDone */
	cLFA += nBytesOut;				/* update CrntLFA */
	paFUB[dHandle]->CrntLFA = cLFA;
  }

  *pdReadRet = nBytesDone;			/* Tell em how many bytes they got */
  if (!erc)
  	if (cLFA == fSize)
  		erc = 1;

  return erc;
}

/*********************************************************
 Flushes the stream buffer if it has been modified.
 We read the current LBA of the buffer and write it
 to disk. Then reset the flag in the FCB.
 This uses WriteBlockM.  We ensure we do not call
 WriteBlockM with more sectors than are allocated
 to the file cause the buffer may extend past the
 actual allocated amount of clusters!
**********************************************************/

static U32 FlushStreamBuff(U32 dHandle)
{
U32 erc, i, j, LFABuf, iFCB, size;
U32 sBuf;
U8  *pBuf;

  erc = 0;

  if (paFUB[dHandle]->fModified)
  {
	  LFABuf = paFUB[dHandle]->LFABuf;		/* LFA of first byte in buf now */
	  pBuf = paFUB[dHandle]->pBuf;			/* Local ptr to buffer */
	  sBuf = paFUB[dHandle]->sBuf;			/* size of buffer */
	  iFCB = paFUB[dHandle]->iFCB;			/* to check filesize */
	  size = paFCB[iFCB]->FileSize;

	  i = (sBuf/512);			/* Total blocks in buff */
	  j = (size-LFABuf)/512;	/* Blocks in buf belonging to file */
	  if ((size-LFABuf)%512)	/* Odd bytes, add one more block */
	  	j++;
	  if (j < i)
	  	i = j;

	  erc = WriteBlockM(dHandle, pBuf, i*512, LFABuf, &i);
      paFUB[dHandle]->fModified = 0;
  }
  return erc;
}


/*** Write Bytes (Stream) ******************************
 This is the STREAM write for the MMURTL FAT file system.
 This uses FillStreamBuff to set up the buffer, and
 FlushStreamBuff to write modified stream buffers.
 This also calls SetFileSizeM to extend the
 filelength when necessary (writing at EOF).
********************************************************/

static U32 WriteBytesM(U32 dHandle, char *pData, U32 nBytes, U32 *nBytesRet)
{
U32 erc, iFCB, sBuf, cLFA, fSize;
U32 nBytesDone, 	/* Total written so far */
	lfaEOB, 		/* LFA end of Buffer */
	nBytesOut;		/* Number of bytes to copy (this buffer full) */
U8  *pOut, *pBuf;	/* Next data to send caller from buffer. Local pbuf */

	erc = ValidateHandle(dHandle, &iFCB);		/* Sets iFCB if OK */
	if (erc) return erc;

	/* Certain FUB fields have different meanings in stream type file */

	if (!paFUB[dHandle]->fStream)
		return(ErcBlockFile);

	if (!paFCB[iFCB]->Mode)		/* Is it open in Modify?? */
		return(ErcReadOnly);

	/* Set up local vars to values for current stream buffer.
	   These are in effect for this call on entry!
	*/

	pBuf = paFUB[dHandle]->pBuf;		/* Local ptr to buffer/size */
	sBuf = paFUB[dHandle]->sBuf;
	cLFA = paFUB[dHandle]->CrntLFA;		/* local cLFA */
	fSize = paFCB[iFCB]->FileSize;		/* local size */

	/* check and see if we are at EOF or will go past it
	when we write. If so, we make the file larger.
	*/

	if (cLFA + nBytes > fSize)
	{		/* Must set file size first */
		erc = SetFileSizeM(dHandle, cLFA + nBytes);
		if (erc)
		{
			return(erc);
		}
		fSize = paFCB[iFCB]->FileSize;			/* local size */
	}

	lfaEOB = paFUB[dHandle]->LFABuf + sBuf -1;	/* LFA at End of Buffer */
    if (lfaEOB > fSize)							/* EOF before EOB? */
    	lfaEOB = fSize - 1;

	/* Now we loop writing the bytes to the stream buffer
     filling it with each new section of the file.
     As this occurs we must call fillStreamBuff with
     each new section of the file if not already in the
     buff to ensure proper continuity of the file in case
     we are overwriting existing sections of the file.
	*/

	nBytesDone = 0;

	while ((nBytesDone < nBytes) &&
		   (cLFA < fSize) &&
		   (!erc))
	{
		/* If the next byte to write goes outside the current buffer
		(before or after it),
		we call FlushStreamBuff which will write the current
		buffer and reset the fModified flag (if needed),
		then call FillStreamBuff for file continuity.
		*/

		if ((cLFA > lfaEOB) ||
			(cLFA < paFUB[dHandle]->LFABuf))
		{
			erc = FlushStreamBuff(dHandle);
				if (erc)
				{
					return(erc);
				}
			erc = FillStreamBuff(dHandle, 0);	/* Fill the buff */
				if (erc)
				{
					return(erc);
				}
		}

		/* Find out the LFA at the end of the buffer since we
		   just filled it.  It may be less than the end since
		   we may be near EOF.
		*/

	    lfaEOB = paFUB[dHandle]->LFABuf + sBuf -1;	/* LFA at End of Buffer */
	    if (lfaEOB > fSize)							/* Beyond EOF? */
	    	lfaEOB = fSize - 1;

		/* Calc pointer to where next chunk goes in stream buffer */

		pOut = pBuf + (cLFA - paFUB[dHandle]->LFABuf);

		/* Calc how many bytes we can write to buffer. We must
		ensure we don't exceed buffer size. */

		nBytesOut = (lfaEOB + 1) - cLFA;
		if (nBytesOut > nBytes-nBytesDone)
			nBytesOut = nBytes-nBytesDone;

		/* Get bytes from pData into stream buffer */

		CopyData(pData, pOut, nBytesOut);
		paFUB[dHandle]->fModified = 1;

		pData += nBytesOut;     		/* Update pData pointer */
		nBytesDone += nBytesOut;		/* Update nBytesDone */
		cLFA += nBytesOut;				/* update CrntLFA */
		paFUB[dHandle]->CrntLFA = cLFA;
	}

	*nBytesRet = nBytesDone;			/* Tell em how many bytes we wrote */
	return erc;
}

/*******************************************************
  GetFileSize returns the FileSize entry from the FCB
  for the handle specified.  This means the file must be
  OPEN.  This call would NOT be used for a directory
  listing function as you would have to open each file
  to get the information.  Use ReadDirSector instead.
********************************************************/

static U32 GetFileSizeM(U32 dHandle, U32 *pdSizeRet)
{
U32 erc, iFCB;
  erc = ValidateHandle(dHandle, &iFCB);
  if (erc) return erc;
  *pdSizeRet = paFCB[iFCB]->FileSize;
  return 0;
}


/*******************************************************
  SetFileLFA sets a Stream mode file pointer to dLFA.
  If attempted on a block mode file an error is returned.
  -1 (0xffffffff) is equal to EOF.  The Stream Buffer
  is filled with the sectors that contains the LFA.
********************************************************/

static U32 SetFileLFAM(U32 dHandle, S32 dLFA)
{
U32 erc, iFCB;

  erc = ValidateHandle(dHandle, &iFCB);
  if (erc) return erc;

  if (!paFUB[dHandle]->fStream)
  	return ErcBlockFile;

  if (paFCB[iFCB]->Mode)	/* Modify mode - Flush will flush if needed */
		erc = FlushStreamBuff(dHandle);

  /* -1 = Set file ptr to EOF */

  if (dLFA == -1)
  	dLFA = paFCB[iFCB]->FileSize;

  if (dLFA > paFCB[iFCB]->FileSize)
    erc = ErcBeyondEOF;

  if (!erc)
  {
	  paFUB[dHandle]->CrntLFA = dLFA;	/* Set where we are in the file */
	  erc = FillStreamBuff(dHandle, 0);
  }

  return erc;
}


/*******************************************************
  GetFileLFA gets a Stream mode file pointer for caller
  returning it to pdLFARet.
  If attempted on a block mode file an error is returned.
  EOF is returned as the FileSize.
********************************************************/

static U32 GetFileLFAM(U32 dHandle, U32 *pdLFARet)
{
U32 erc, iFCB;
  erc = ValidateHandle(dHandle, &iFCB);
  if (erc) return erc;
  if (!paFUB[dHandle]->fStream)
  	return ErcBlockFile;
  *pdLFARet = paFUB[dHandle]->CrntLFA;

  return erc;
}

/*****************************************************
 This calls parse to validate the filename and separate
 it into directories and a filename.  It then walks
 up the tree until it either finds and opens the file,
 or errors out.  The handle that is returned is
 4 higher than the index to the FUB.  This is becuase
 0, 1, 2 & 3 are reserved for NUL, KBD, VID, and LPT
 which are  only accesible from the blocking File calls.
*****************************************************/

static U32 OpenFileM(U8 *pName,
			  U32 cbName,
			  U8 Mode,
			  U8 fStream,
			  U32 *pdHandleRet,
			  U32 iJob)			/* make use of iJob later!!! */
{
U32 erc, level, i, iFCB, iFUB, LBADirEnt, EntOffset;
U16 Clstr;
U8 fFound, *pMem, Drive;

  if (Mode > 1)
  	return ErcBadOpenMode;

  level = 0;	/* start at the root, compare to SpecDepth */

/* RAB B */
 if (((cbName) && (*pName == ' ')) ||
     (!cbName))
	return(ErcBadFileSpec);
/* RAB E */

  erc = ParseName(pName, cbName, iJob);

	/* The entire path has now been parsed out into an array of
	   arrays, each 11 bytes long that contain each directory name
	   up to and inlcuding the filename.  The first is always
	   the root (entry 0). The drive will be a letter in FDrive.
	*/

  if ((FDrive > 0x40) && (FDrive < 0x52)) 	/* A to J */
  	Drive = FDrive - 0x41;							/* Make it 0-9 */
  else erc = ErcNoSuchDrive;

  if (Ldrv[Drive].DevNum == 0xff)
    erc = ErcNoSuchDrive;

  if ((Drive < 2) && (!erc))
  {
	StatFloppy(Drive);
	erc= read_BS(Drive);
  }

  if (!erc)
  {                               	/* Get Root dir entry */
	erc = GetRootEnt(FileSpec[level],
					 Drive,
					 &LBADirEnt,
					 &EntOffset,
					 &pDirEnt);

	if (erc == ErcNoSuchFile)
	{
		if (level == SpecDepth) erc = ErcNoSuchFile;
		else erc = ErcNoSuchDir;
	}
	if (erc)
		return(erc);

	if (!erc)
	    Clstr = pDirEnt->StartClstr;  /* Clstr = beginning of file or dir */

	while ((level < SpecDepth) && (!erc))
	{	/* looking for Dir, not file yet */

		++level;					/* next level of parsed filespec */

		erc = GetDirEnt(FileSpec[level],
						Drive,
						Clstr,
						&LBADirEnt,
						&EntOffset,
						&pDirEnt);
		if (erc == ErcNoSuchFile)
		{
			if (level == SpecDepth)
				erc = ErcNoSuchFile;
			else erc = ErcNoSuchDir;
		}
		else if (erc)
			return(erc);
		else
		    Clstr = pDirEnt->StartClstr;  /* Clstr @ start of dir entry */
	}

	/* if we got here with no error we've got a file or a DIR.
	   If it's DIR then it's an error.
	   pDirEnt points to its directory entry, and Clstr
	   is the starting cluster of the file
	*/

	if (!erc) 
	{
		/* If Attributes say it's not a file then ERROR */

		if (pDirEnt->Attr & (VOLNAME | DIRECTORY))
			return ErcNotAFile;

		/* If ModeModify and File is readOnly then ERROR */

		if ((Mode) && (pDirEnt->Attr & READONLY))
			return ErcReadOnly;

	/* We check to see if it's already open by looking through the
	   valid FCBs to see if we have a Drive, StartClstr& name match.
	   If so, we must see if the modes are compatible.
	   A valid FCB is one where nUsers > 0.
	*/

	fFound = 0;
	i=0;
	while ((i<nFCBs) && (!fFound)) 
	{

		if ((paFCB[i]->nUsers) &&
			(paFCB[i]->Ldrv == Drive) &&
		    (paFCB[i]->StartClstr == pDirEnt->StartClstr) &&
		    (CompareNCS(&paFCB[i],
		    			FileSpec[SpecDepth], 11) == 0xffffffff))
			 fFound = 1;
		else
		    ++i;
	}

	if (fFound)
	{			/* it's open already.  i is index into FCBs  */
		if (paFCB[i]->Mode)		/* it's open in Modify already */
			return ErcFileInUse;
		else
		{
			iFCB = i;			/* Index to this FCB */
			pFCB = &paFCB[i];	/* make pFCB point to FCB found */
			pFCB->nUsers++;		/* One more user */
		}
	}
	else
	{					/* It not already open. Find empty FCB */
		i = 0;
		while ((i<nFCBs) && (paFCB[i]->nUsers)) ++i;	/* Find new FCB */

		if (i==nFCBs) return ErcNoFreeFCB;			/* Couldn't */

		/* i now indexes into FCBs for a free FCB.  New we copy the
		   directory entry for the file into the FCB and set up
		   the other FCB values. */

		iFCB = i;						/* used to add an FUB */
		pFCB = &paFCB[i];				/* make pFCB point to FCB found */
		CopyData(pDirEnt, pFCB, 32);	/* Copy Dir Ent into FCB */
		pFCB->Ldrv = Drive;				/* Set Drive */
		pFCB->nUsers++;					/* Now in use */
		pFCB->Mode = Mode;				/* Open mode */
		pFCB->LBADirSect =	LBADirEnt;	/* So we know where it came from */
		pFCB->oSectDirEnt = EntOffset;	/* "  "  */
	}

	/* Now we have an FCB (either existing or we just built it).
	   Now add an FUB and fill in the info for the user
	   so we can return a handle to it.  The Job fields is 0
	   for free FUBs.
	*/

	i = 4;
	while ((i<nFUBs) && (paFUB[i]->Job)) ++i;	/* Find new FUB */
	if (i==nFUBs) 
	{
		pFCB->nUsers--;					/* Make FCB correct */
		return ErcNoFreeFUB;			/* Couldn't */

	}

	/* If we got here, i is an index to a free FUB. */

	iFUB = i;
	paFUB[iFUB]->Job = iJob;		/* Job Owner */
	paFUB[iFUB]->iFCB = iFCB;		/* Set index to FCB for this file */
	paFUB[iFUB]->CrntLFA = 0;		/* Current Logical File Address */
	paFUB[iFUB]->fModified = 0;		/* Stream buf was modified */
	paFUB[iFUB]->fStream = fStream;	/* NonZero for STREAM mode */
	paFUB[iFUB]->Clstr = pDirEnt->StartClstr;	/* Start Cluster */
	paFUB[iFUB]->LFAClstr = 0;		/* Rel LFA to 0 */
	paFUB[iFUB]->LFABuf = 0;		/* First LFA in Buffer */
	paFUB[iFUB]->sBuf = 0;			/* Default to No Buf */

	if (fStream) 
	{		/* allocate/fill buffer and set rest of FUB */

		erc = AllocOSPage(1, &pMem);		/* Stream Buf is 4K */

	    if (erc) 
	    {							/* No MEM left... Bummer */
			pFCB->nUsers--;					/* Return FCB to pool */
			paFUB[iFUB]->Job = 0;			/* Return FUB to pool */
			return erc;						/* Return Erc to user... */
		}

		paFUB[iFUB]->pBuf = pMem;		/* Ptr to buffer if stream mode */
		paFUB[iFUB]->sBuf = 4096;		/* Size of buffer */

		erc = FillStreamBuff(iFUB, 1);	/* fInitial to TRUE */

		if (erc) 
		{
			pFCB->nUsers--;				/* Return FCB to pool */
			paFUB[iFUB]->Job = 0;		/* Return FUB to pool */
			DeAllocPage(pMem, 1);		/* Free memory for buffer */
			return erc;					/* Return Erc to user... */
		}
	}

    *pdHandleRet = iFUB;		/* File handle */
	}
  }

return erc;

}


/*******************************************************
  CLOSE FILE for the MMURTL DOS file system. This finds
  the FUB, checks to see if the buffer should be flushed
  and deallocated (only for stream mode), invalidates
  the FUB, and the FCB (if this is the last or only user).
********************************************************/

static U32 CloseFileM (U32 dHandle)
{
U32 erc, iFCB, i;

	erc = ValidateHandle(dHandle, &iFCB);
	if (erc) return erc;

	if (paFCB[iFCB]->Mode)
	{  /* Modify mode */
	  	if (paFUB[dHandle]->fStream)
	  	{
			erc = FlushStreamBuff(dHandle);
		}
		UpdateDirEnt(iFCB);			/* ignore error */
	}

	if (paFUB[dHandle]->fStream)
		DeAllocPage(paFUB[dHandle]->pBuf, 1);	/* Free buffer */

	/* This means the FS is screwed up. This shouldn't happen... */
	if (!paFCB[iFCB]->nUsers)
		erc = ErcBadFCB;
	else
		paFCB[iFCB]->nUsers--;

	  /* Now we should be able to close it and free the the FUB.
    	 If the FCB.nUsers flips to 0 it will be free too
	  */

	paFUB[dHandle]->Job = 0;

	/* This will write all modified fat sectors */

	for (i=0; i<nFATBufs; i++)
			UpdateFAT(i);

  return erc;
}


/*** Create File ***************************************
 This is Create File for the MMURTL FAT file system.
 This is also used internally to create directories.
********************************************************/

static U32 CreateFileM(char *pName,
				long cbName,
				long attrib,
				long iJob)
{
unsigned long dHandle, i, j, k, erc, LBA, spc;
char Path[70];
long cbPath;
char filename[12];
U16 CrntClstr, ClstrValue, iStart, DirClstr;
U8 fFound, Drive, fDir;

	/* First we try to open it to see if it exists. If we get back
		ErcOK or ErcFileInUse the name is already
		in use as a file and we give them ErcDupName.
		We return other errors as we find them.
	*/
	if (attrib & DIRECTORY)
	{
		fDir = 1;
		erc = 0;
	}
	else
		fDir = 0;

	erc = OpenFileM(pName, cbName, 0, 0, &dHandle, iJob);

	switch (erc)
	{
	case ErcOK:
		CloseFileM(dHandle);
		erc = ErcDupName;
		break;
	case ErcFileInUse:
		erc = ErcDupName;
		break;
	case ErcNotAFile:	/* It a directory... */
		break;
	case ErcNoSuchFile:
	{	/* OK, this means we can try to create it! */

		erc = 0;

		BuildSpec(pName, cbName, Path, &cbPath, iJob);

		erc = ParseName(Path, cbPath, iJob);
		if (erc)
			return(erc);

		/* FDrive was set up on Parse */
	  	Drive = FDrive - 0x41;			/* Make it 0-9 */

		/* First we setup the filename from what was parsed out
		during the Parse call. Then eliminate it from Path.
		*/

		CopyData(FileSpec[SpecDepth], filename, 11); /* filename */

		filename[11] = 0;

		/* Hack the filename from Path so we can search the
		directory path properly */

		while ((cbPath) && (Path[cbPath-1] != 0x5C)) 
		{
			cbPath--;
		}

		/* Each directory sector has 16 32 byte entries.
		We will now walk thru each sector until we find one
		that is a deleted or empty entry.
		A deleted entry has E5h as its first character in the name.
		An unused entry has 00h as its first character in the name.
		*/

		fFound = 0;
		i = 0;							/* i = sectornum */
		while ((!fFound) && (!erc)) 
		{
			erc = GetDirSectorM(Path, cbPath, abTmpSector,
								512, i++, &LBA, &DirClstr, iJob);
			if (!erc) 
			{
				k = 0;
    	        pDirEnt = abTmpSector;
				while (k<16) 
				{
					if ((pDirEnt->Name[0] == 0xE5) ||
	                    (!pDirEnt->Name[0]))
	                    {
    	                	fFound = 1;
        	            	break;
					}
					pDirEnt += 32;
					k++;
				}
			}
		}
		/* When we get here, we have either found an entry or
		we have run out of sectors!  If we run out of sectors
		and this is the root, we error out (RootFull), otherwise
		we extend the directory.
		*/

		if ((erc == ErcNoMatch) && (!SpecDepth))
			return (ErcRootFull);

        else if (erc == ErcEOF)
        {		/* reach end of dir! */

			/* We must now extend the cluster chain for this
			   directory entry. DirClstr holds the last
			   valid sector of the directory.
			*/

		  FillData(abTmpSector, 512, 0);
		  spc = Ldrv[Drive].SecPerClstr;		/* sectors per cluster */
		  erc = ExtendClstrChain(Drive, DirClstr, &DirClstr);
		  if (erc)
		  	return(erc);
		  LBA = ClsToLBA(DirClstr, Drive);
		  j = LBA;
		  i = spc;
		  erc = 0;
		  while ((i--) && (!erc))
		  {
			erc = DeviceOp(Ldrv[Drive].DevNum, 2, j++,
						   1, abTmpSector);
		  }
		  pDirEnt = abTmpSector;  /* first entry in new sector */
		  fFound = 1;

			/* We now have a new cluster on the end of the
			directory and it is all zeros!. The first sector
			is pointed to by LBA and abTmpSector is still zeros
			just as an new dir sector should be with pDirEnt
			pointing to the first entry.
			*/
		}

		if ((!erc) && (fFound)) 
		{		/* Let's DO IT! */

			/* pDirEnt points to the entry we will use and
			   abTmpSector still has the entire sector in it,
			   so we find an empty clstr on the disk, allocate
			   to this file, fill in the rest of the dir
			   entry and we are almost done.
			*/

			/* Find a fat buf already in memory for this drive */
			/* One WILL be here! */

			k = 0;
			CrntClstr = 0;
			while ((k<nFATBufs) && (!CrntClstr)) 
			{
				if ((Drive == Fat[k].Drive) && (Fat[k].LastUsed))
                    CrntClstr = Fat[k].iClstrStart;	/* valid cluster */
				k++;
			}

			if (!CrntClstr)
				CrntClstr = 2;		/* Can't find it so start at beginning */

			iStart = CrntClstr;		/* where we started looking for empties */

			fFound = 0;
			while (!fFound) 
			{
				++CrntClstr;		/* next cluster */
				if (CrntClstr == iStart)
					return(ErcDiskFull);

				erc = GetClstrValue(CrntClstr, Drive, 0, &ClstrValue, &j);

				if ((!erc) && (!ClstrValue))
						fFound = 1; 	/* found an empty one */

				else if (erc == ErcBadFATClstr) 
				{ /* off the end */
						/* we started AFTER beginning of disk so
						  we will go back and look for empties
						  from beginning to where we started.
						*/

					if (iStart > 2)
						CrntClstr = 2;
					else
						return(ErcDiskFull);
				}
				else if (erc)
						return(erc);
			}

			/* If we got here, we found an empty cluster */

            CopyData(filename, pDirEnt, 11);
			if (!fDir)
				pDirEnt->Attr =
					attrib & (READONLY | HIDDEN | SYSTEM | ARCHIVE);
			else
                pDirEnt->Attr = attrib;
			GetFATTime(&pDirEnt->Time, &pDirEnt->Date);
			pDirEnt->StartClstr = CrntClstr;
			pDirEnt->FileSize = 0;
			erc = SetClstrValue(CrntClstr, 0xFFFF, Drive, &i);
			/* Now we write the dir sector back to disk */
			if (!erc)
				erc = DeviceOp(Ldrv[Drive].DevNum, 2, LBA,
								1, abTmpSector);

			/* If we were creating a directory, we must add
			the two deafult directory entries . and ..
			This is done by filling out abTmpSector as
			the first sector of an empty directory and writing
			it out to the allocated cluster.
			We then zewro it out and write it to the rest
			of the sectors in the new cluster.
			*/

			if (fDir)
			{
			  FillData(abTmpSector, 512, 0);
			  pDirEnt = abTmpSector;  /* first entry in new sector */

				/* do the current dir entry (.) */

			  CopyData(".          ", pDirEnt, 11);
			  pDirEnt->Attr = DIRECTORY;
			  GetFATTime(&pDirEnt->Time, &pDirEnt->Date);
			  pDirEnt->StartClstr = CrntClstr;
			  pDirEnt->FileSize = 0;

				/* do the previous current dir entry (.) */

			  pDirEnt += 32;
			  CopyData("..         ", pDirEnt, 11);
			  pDirEnt->Attr = DIRECTORY;
			  GetFATTime(&pDirEnt->Time, &pDirEnt->Date);
			  pDirEnt->StartClstr = DirClstr;
			  pDirEnt->FileSize = 0;

			  spc = Ldrv[Drive].SecPerClstr;		/* sectors per cluster */
			  LBA = ClsToLBA(CrntClstr, Drive);
			  j = LBA;

			  /* Write this sector out to disk */
			  erc = DeviceOp(Ldrv[Drive].DevNum, 2, j++,
							 1, abTmpSector);

			  FillData(abTmpSector, 512, 0); /* zero the rest */

			  erc = 0;
			  i = spc-1;	/* less one cause we wrote the first */
			  while ((i--) && (!erc))
			  {
				erc = DeviceOp(Ldrv[Drive].DevNum, 2, j++,
							   1, abTmpSector);
			  }

			}
			/* This will write all modified fat sectors */

			for (i=0; i<nFATBufs; i++)
				UpdateFAT(i);

			return(erc);

		}
		else
			return(erc);
	}
	default: ;

	} /* switch */
	return (erc);
}

/*** Delete File ***************************************
 This is Delete File for the MMURTL FAT file system.
 The file must be opened in mode modify which gives
 the caller exclusive access. The file is closed
 even if the Delete fails.
********************************************************/

static U32 DeleteFileM(long *dHandle)
{
U32 erc, iFCB, i;
U16 iStart;
U8 Drive;

	erc = ValidateHandle(dHandle, &iFCB);
	if (erc) return erc;

	Drive = paFCB[iFCB]->Ldrv;			/* What logical drive are we on? */

	if (!paFCB[iFCB]->Mode) 
	{  /* Modify mode? */
		CloseFileM(dHandle);
		return ErcReadOnly;
	}
	if (paFUB[dHandle]->fStream)
		DeAllocPage(paFUB[dHandle]->pBuf, 1);	/* Free buffer */

	iStart = paFCB[iFCB]->StartClstr;
	if (iStart) 
	{
		erc = TruncClstrChain(Drive, iStart);
		if (!erc)
			erc = SetClstrValue(iStart, 0, Drive, &i);
	}

	paFCB[iFCB]->Name[0] = 0xE5;
	UpdateDirEnt(iFCB);			/* ignore error */

	/* This means the FS is screwed up. This shouldn't happen... */
	if (!paFCB[iFCB]->nUsers)
		erc = ErcBadFCB;
	else
		paFCB[iFCB]->nUsers--;

	  /* Now we should be able to close it and free the the FUB.
    	 If the FCB.nUsers flips to 0 it will be free too
	  */

	paFUB[dHandle]->Job = 0;

	/* This writes all modified fat sectors */

	for (i=0; i<nFATBufs; i++)
			UpdateFAT(i);

	return erc;
}

/*** Rename File ***************************************
 This is Rename File for the MMURTL FAT file system.
********************************************************/

static U32 RenameFileM(char *pCrntName, long dcbCrntName,
                char *pNewName, long dcbNewName, U32 iJob)
{
U32 dHandle, erc, erc1, iFCB;

	erc = OpenFileM(pCrntName, dcbCrntName, 1, 0, &dHandle, iJob);
	if (!erc) 
	{
		FDrive1 = FDrive;
		CopyData(FileSpec, FileSpec1, 77);
		SpecDepth1 = SpecDepth;
		erc = ParseName(pNewName, dcbNewName, iJob);
		if (!erc)
			if ((FDrive1 != FDrive) || (SpecDepth1 != SpecDepth))
				erc = ErcRenameDrv;			/* No Rename across drives */
		if (!erc)
			if (SpecDepth)		/* Compare upper tree */
				if (CompareNCS(FileSpec, FileSpec1, SpecDepth * 11) != -1)
					erc = ErcRenameDir;		/* No Rename across dirs */
		if (!erc) 
		{ /* OK to rename */
			iFCB = paFUB[dHandle]->iFCB;		/* FCB for this FUB */
			CopyData(FileSpec[SpecDepth], &paFCB[iFCB], 11);
			erc = UpdateDirEnt(iFCB);
		}
		erc1 = CloseFileM(dHandle);
		if (!erc)
			erc = erc1;
	}
	return (erc);
}

/*** Create Dir ***************************************
 This is Create Directory for the MMURTL FAT file system.
********************************************************/

static U32	CreateDirM(char *pPath, long cbPath, long iJob)
{
long erc;

  erc = CreateFileM(pPath, cbPath, DIRECTORY, iJob);
  return(erc);

}
/*** Delete Directory ***********************************
 This is Delete Directory for the MMURTL FAT file system.
********************************************************/

static U32	DeleteDirM(char *pPath, long cbPath, long fAllFiles, long iJob)
{
	pPath = 0;
	cbPath = 0;
	fAllFiles = 0;
	iJob = 0;
}


/*******************************************************
 This is the File system task. All file system requests
 end up here to be serviced.
********************************************************/

static void FSysTask(void)
{
U32 FMsg[2], merc, erc, i;
U16 i16;

while (1) 
{
  erc = WaitMsg(FSysExch, FMsg);
  if (!erc)
  {

  	pRQB = FMsg[0];		/* first DD in Msg is pointer to RQBlock */

	switch (pRQB->ServiceCode)
	{
		case 0 :		/* JobAbort - CLOSE ALL FILES FOR THIS JOB! */
			i = 4;
			while (i<nFUBs)
			{
	            if (paFUB[i]->Job == pRQB->dData0)
	            {
					CloseFileM(i);
				}
				++i;	/* next FUB */
			}

			erc = 0;
			break;
		case 1 :		/* OpenFile */
			erc = OpenFileM(pRQB->pData1,	    /* pFilename */
			                pRQB->cbData1,      /* dcbFilename */
						    pRQB->dData0,       /* Mode */
						    pRQB->dData1,       /* Type */
						    pRQB->pData2,       /* pdHandleRet */
						    pRQB->RqOwnerJob);	/* iJob Owner */
			break;
		case 2 :		/* CloseFile */
			erc = CloseFileM(pRQB->dData0);	    /* Handle */
			break;
		case 3 :		/* ReadBlock */
			erc = ReadBlockM(pRQB->dData0,       /* Handle */
			                 pRQB->pData1,       /* pDataRet */
			                 pRQB->cbData1,      /* nByes */
						     pRQB->dData1,       /* dLFA */
						     pRQB->pData2,       /* pdnBytesRet */
						     0);                 /* NOT internal */
			break;
		case 4 :		/* WriteBlock */
			erc = WriteBlockM(pRQB->dData0,      /* Handle */
			                  pRQB->pData1,      /* pData */
			                  pRQB->cbData1,     /* nBytes */
						      pRQB->dData1,      /* dLFA */
						      pRQB->pData2);	 /* pdBytesRet */
			break;
		case 5 :		/* ReadBytes */
			erc = ReadBytesM(pRQB->dData0,       /* Handle */
			                 pRQB->pData1,       /* pDataRet */
			                 pRQB->cbData1,      /* nBytes */
						     pRQB->pData2);      /* pdnBytesRet */
			break;
		case 6 :		/* WriteBytes */
			erc = WriteBytesM(pRQB->dData0,      /* Handle */
			                  pRQB->pData1,      /* pData */
			                  pRQB->cbData1,     /* nBytes */
						      pRQB->pData2);     /* pdnBytesRet */
			break;
		case 7 :		/* GetFileLFA */
			erc = GetFileLFAM(pRQB->dData0,      /* Handle */
			                  pRQB->pData1);     /* pdLFARet */
			break;
		case 8 :		/* SetFileLFA */
			erc = SetFileLFAM(pRQB->dData0,      /* Handle */
			                 pRQB->dData1);     /* dNewLFA */
			break;
		case 9 :		/* GetFileSize */
			erc = GetFileSizeM(pRQB->dData0,     /* Handle */
			                  pRQB->pData1);    /* pdSizeRet */
			break;
		case 10 :		/* SetFileSize */
			erc = SetFileSizeM(pRQB->dData0,     /* Handle */
			                   pRQB->dData1);    /*  dSize */
			break;
		case 11 :		/* CreateFile */
			erc = CreateFileM(pRQB->pData1,      /* pFilename  */
			                  pRQB->cbData1,     /* cbFilename */
			                  pRQB->dData0,      /* Attributes */
							  pRQB->RqOwnerJob); /* iJob Owner */
			break;
		case 12 :		/* RenameFile */
			erc = RenameFileM(pRQB->pData1,      /* pCrntName  */
			                  pRQB->cbData1,     /* cbCrntName */
			                  pRQB->pData2,      /* pNewName */
			                  pRQB->cbData2,     /* dcbNewName */
			                  pRQB->RqOwnerJob); /* JobNum */
			break;
		case 13 :		/* DeleteFile */
			erc = DeleteFileM(pRQB->dData0);     /* Handle  */
			break;
		case 14 :		/* CreateDirectory */
			erc = CreateDirM(pRQB->pData1,      /* pPath */
			                 pRQB->cbData1,     /* cbPath */
			                 pRQB->RqOwnerJob); /* JobNum */
			break;
		case 15 :		/* DeleteDirectory */
			erc = DeleteDirM(pRQB->pData1,      /* pPath */
			                 pRQB->cbData1,     /* cbPath */
			                 pRQB->dData0,      /* fAllFiles */
			                 pRQB->RqOwnerJob); /* JobNum */
			break;
		case 16 :		/* GetDirSector */
			erc = GetDirSectorM(pRQB->pData1,      /* pPath    */
			                    pRQB->cbData1,     /* cbPath   */
							    pRQB->pData2,      /* pSectRet */
			                    pRQB->cbData2,     /* cbRetMax */
			                    pRQB->dData0,      /* SectNum */
			                    &i,				   /* for LBARet */
			                    &i16,			   /* for DirClstr */
			                    pRQB->RqOwnerJob); /* JobNum   */
			break;
		default :
			erc = ErcBadSvcCode;
			break;
	}

	merc = Respond(FMsg[0], erc);

  }
}		/* forever */
}

/***************** PUBLIC BLOCKING CALLS FOR FILESYSM *************
 These calls query the TSS Exhange and use it to make Requests
 to the file system service on the behalf of the caller.
 These calls are fully reentrant! No static data!
*******************************************************************/

U32 far _OpenFile(char *pFilename,
			     long dcbFilename,
				 long Mode,
				 long Type,
				 long *pdHandleRet)
{
long erc, exch, rqhndl, i, msg[2];
	if (dcbFilename == 3)
	{
		if (CompareNCS(pFilename, "NUL" , 3) == -1)
		{
            *pdHandleRet = 0;
            return(0);
		}
		else if (CompareNCS(pFilename, "KBD" , 3) == -1)
		{
            *pdHandleRet = 1;
            return(0);
		}
		else if (CompareNCS(pFilename, "VID" , 3) == -1)
		{
            *pdHandleRet = 2;
            return(0);
		}
		else if (CompareNCS(pFilename, "LPT" , 3) == -1)
		{
			erc = DeviceOp(3, 10, 0,    0,  &i);   /* 10=Open */
			if (!erc)
	            *pdHandleRet = 3;
            return(erc);
		}
	}

	GetTSSExch(&exch);		/* No error will come back! */
    erc = Request(fsysname, 1, exch, &rqhndl,
                  1,  							/* 1 Send ptr */
                  pFilename, dcbFilename,
                  pdHandleRet, 4,
                  Mode, Type, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _CloseFile(unsigned long dHandle)
{
long erc, exch, rqhndl, i, msg[2];

	if (dHandle < 3)
           return(0);
	else if (dHandle == 3)
	{
		erc = DeviceOp(3,  11,   0,   0,  &i);    /* 11 = Close */
		return(erc);
	}

	GetTSSExch(&exch);
    erc = Request(fsysname, 2, exch, &rqhndl,
                   0,  							/* 0 Send ptr */
                   0, 0,
                   0, 0,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _ReadBlock(long dHandle,
                  char *pDataRet,
                  long nBlks,
				  long dLFA,
				  long *pdnBlksRet)
{
long erc, exch, rqhndl, msg[2], i;
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 3, exch, &rqhndl,
                  1,  							/* 1 Send ptr */
                  pDataRet, nBlks,
                  pdnBlksRet, 4,
                  dHandle, dLFA, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc)
		return(erc);
	if(msg[1]) 
	{
		DeviceStat(10, &FDDevStat, 64, &i);
	}
	return(msg[1]);
}

/*************************************/
U32 far _WriteBlock(long dHandle,
                   char *pData,
                   long nBlks,
                   long dLFA,
                   long *pdnBlksRet)

{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 4, exch, &rqhndl,
                   1,  							/* 1 Send ptr */
                   pData, nBlks,
                   pdnBlksRet, 4,
                   dHandle, dLFA, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _ReadBytes(long dHandle,
				  char *pDataRet,
				  long nBytes,
				  long *pdnBytesRet)
{
long erc, exch, rqhndl, msg[2], i;
	if (dHandle == 0)
	{
        *pdnBytesRet = 0;
		return(0);
	}
	else if (dHandle == 1)
	{
		i = 0;
		while (i < nBytes)
		{
			ReadKbd(*pDataRet++, 1);
			i++;
		}
	    *pdnBytesRet = i;
		return(0);
	}
	else if	((dHandle == 2) || (dHandle == 3))
	{
        *pdnBytesRet = 0;
		return(ErcWriteOnly);
	}

	GetTSSExch(&exch);
    erc = Request(fsysname, 5, exch, &rqhndl,
                   1,  							/* 1 Send ptr */
                   pDataRet, nBytes,
                   pdnBytesRet, 4,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _WriteBytes(long dHandle,
                   char *pData,
                   long nBytes,
                   long *pdnBytesRet)

{
long erc, exch, rqhndl, VidAttr, msg[2];

	if (dHandle == 0) 
	{
        *pdnBytesRet = nBytes;
		return(0);
	}
	else if (dHandle == 1)
	{
        *pdnBytesRet = 0;
		return(ErcReadOnly);
	}
	else if	(dHandle == 2)
	{
		GetNormVid(&VidAttr);
		TTYOut(pData, nBytes, VidAttr);
	    *pdnBytesRet = nBytes;
		return(0);
	}
	else if (dHandle == 3)
	{
		erc = DeviceOp(3, 2, 0,  nBytes, pData);   /* 2 = CmdWriteRec */
		return(erc);
	}

	GetTSSExch(&exch);
    erc = Request(fsysname, 6, exch, &rqhndl,
                   1,  							/* 1 Send ptr */
                   pData, nBytes,
                   pdnBytesRet, 4,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far	 _GetFileLFA(long dHandle,
                    long *pdLFARet)

{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcEOF);

	GetTSSExch(&exch);
    erc = Request(fsysname, 7, exch, &rqhndl,
                   0,  							/* 0 Send ptrs */
                   pdLFARet, 4,
                   0, 0,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _SetFileLFA(long dHandle,
                   long dNewLFA)
{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 8, exch, &rqhndl,
                   0,  							/* 0 Send ptrs */
                   0, 0,
                   0, 0,
                   dHandle, dNewLFA, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _GetFileSize(long dHandle,
                    long *pdSizeRet)
{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 9, exch, &rqhndl,
                   0,  							/* 0 Send ptrs */
                   pdSizeRet, 4,
                   0, 0,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _SetFileSize(long dHandle,
                    long dSize)
{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 10, exch, &rqhndl,
                   0,  							/* 0 Send ptrs */
                   0, 0,
                   0, 0,
                   dHandle, dSize, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _CreateFile(char *pFilename,
				   long cbFilename,
				   long Attribute)
{
long erc, exch, rqhndl, msg[2];
	GetTSSExch(&exch);
    erc = Request(fsysname, 11, exch, &rqhndl,
                   1,  							/* 1 Send ptrs */
                   pFilename, cbFilename,
                   0, 0,
                   Attribute, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _RenameFile(char *pCrntName,
				   long cbCrntName,
				   char *pNewName,
				   long cbNewName)
{
long erc, exch, rqhndl, msg[2];
	GetTSSExch(&exch);
    erc = Request(fsysname, 12, exch, &rqhndl,
                   2,  							/* 2 Send ptrs */
                   pCrntName, cbCrntName,
                   pNewName, cbNewName,
                   0, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _DeleteFile(long dHandle)
{
long erc, exch, rqhndl, msg[2];
	if (dHandle < 4)
		return(ErcNotSupported);
	GetTSSExch(&exch);
    erc = Request(fsysname, 13, exch, &rqhndl,
                   0,  							/* 0 Send ptrs */
                   0, 0,
                   0, 0,
                   dHandle, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _CreateDir(char *pPath,
				   long cbPath)
{
long erc, exch, rqhndl, msg[2];
	GetTSSExch(&exch);
    erc = Request(fsysname, 14, exch, &rqhndl,
                   1,  							/* 1 Send ptrs */
                   pPath, cbPath,
                   0, 0,
                   0, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _DeleteDir(char *pPath,
				   long cbPath,
				   long fAllFiles)
{
long erc, exch, rqhndl, msg[2];
	GetTSSExch(&exch);
    erc = Request(fsysname, 15, exch, &rqhndl,
                   1,  							/* 1 Send ptrs */
                   pPath, cbPath,
                   0, 0,
                   fAllFiles, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/*************************************/
U32 far _GetDirSector(char *pPathSpec,
				     long cbPathSpec,
					 char *pEntRet,
				     long cbEntRet,
				     long SectNum)
{
long erc, exch, rqhndl, msg[2];
	GetTSSExch(&exch);
    erc = Request(fsysname, 16, exch, &rqhndl,
                   1,  							/* 1 Send ptrs */
                   pPathSpec, cbPathSpec,
                   pEntRet, cbEntRet,
                   SectNum, 0, 0);
	if (!erc) erc = WaitMsg(exch, msg);
	if (erc) return(erc);
	return(msg[1]);
}

/********************************************
 Initialization Routine for the File system.
 This is called	the Monitor where any errors
 will be reported.
**********************************************/

U32 InitFS(void)
{
U32 erc, i, j;
U8 *pMem;

  /* Allocate FAT buffers and initialize FAT related structures.
     We will allocate 24Kb worth of buffers (6 Pages, 16 buffers).
   */

  Fat[0].pBuf = FatBufA;  /* floppy fat buffers */

  erc = AllocOSPage(2, &pMem);

  if (!erc)				 /* hard disk fat buffers from allocated mem */
    for (i=1; i<nFATBufs; i++)
    {
	  Fat[i].pBuf = pMem;			/* Make pBuf point to each buffer */
	  pMem += 512;					/* Next buffer in allocated memory */
	}

  /* Allocate and initialize FCBs and FUBs. This is enough FCBs and
     FUBS for 128 openfiles.  (Good enough to start...)
  */

  if (!erc)
	  erc = AllocOSPage(2, &paFCB);	/* a pointer to allocated FCBs. */
  if (!erc)
	  FillData(paFCB, 8192, 0);
  if (!erc)
	  erc = AllocOSPage(1, &paFUB);	/* a pointer to allocated FUBs. */
  if (!erc)
	  FillData(paFUB, 4096, 0);

  if (!erc)	erc = read_PE();	/* reads partition tables */

  if (!erc)	erc = SetDriveGeometry(12);

  if (!erc)	
  {
  	erc = SetDriveGeometry(13);
    if (erc == 663) erc = 0;		/* may be invalid drive */
  }

  StatFloppy(0);
  StatFloppy(1);

	/* read all logical drive boot sectors */

  if (!erc) 
  {
	for (i=0; i< nLDrvs; i++) 
	{
	  if (Ldrv[i].DevNum != 0xff) 
	  {
		  	read_BS(i);
	  }
	}
  }

  for (i=0; i<nLDrvs; i++)
  	if (Ldrv[i].DevNum != 0xff) 
  	{
	j=12;
  	  if (Ldrv[i].fFAT16)
	  	  j=16;
  	  xprintf("%c: Heads %d, Sec/Trk %d, Sec/Clstr %d, Dev %d, FAT%d \r\n",
  			i+0x41,
  			Ldrv[i].nHeads,
  			Ldrv[i].nSecPerTrk,
  			Ldrv[i].SecPerClstr,
  			Ldrv[i].DevNum,
  			j);
  	}

  if (!erc)
	  erc = AllocExch(&FSysExch);

	/* Start the filesystem task at a decently high priority (5).
	This should be higher than the Monitor status task and even the
	Keyboard. */

  if (!erc)
  	erc = SpawnTask(&FSysTask, 5, 0, &FSysStack[511], 1);

  if (!erc)
   erc = RegisterSvc(fsysname, FSysExch);

return erc;

}
