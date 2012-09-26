/*
  MMURTL Operating System Source Code
  Copyright 1991,1992,1993,1994 Richard A. Burgess
  ALL RIGHTS RESERVED
  Version 1.0
*/

/* This file contains functions and data used to support
   loading or terminating jobs (or services).
   It contains the public functions:

   Chain()			 Loads new job run file in current JCB & PD
   LoadNewJob()		 Loads a new job into a new JCB & PD
   ExitJob()		 Exits current job, loads ExitJob if specified

   GetExitJob()		 Gets run file name that will be loaded upon ExitJob
   SetExitJob()		 Sets run file name to load upon ExitJob

   SetCmdLine()		 Sets the command line for next job
   GetCmdLine()		 Gets the command line for the current job

   GetPath()		 Gets the path prefix for the current job
   Setpath()		 Sets the path prefix for the current job

   SetUserName()	 Sets the Username for the current job
   GetUserName()	 Gets the Username for the current job

*/


#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char
#define TRUE 1
#define FALSE 1

#include "MKernel.h"
#include "MMemory.h"
#include "MData.h"
#include "MTimer.h"
#include "MVid.h"
#include "MKbd.h"
#include "MJob.h"
#include "MFiles.h"

#include "runfile.h"

#define MEMUSERD 7			/* User Writable (Data) */

#define ErcOpCancel     4	/* Operator cancel */
#define ErcOutOfRange  10	/* Bad Exchange specified in OS call */
#define ErcBadJobNum   70	/* A Bad job number was specified */
#define ErcNoExitJob   76	/* No ExitJob specified on ExitJob(n)*/
#define ErcBadRunFile  74	/* Couldn't load specified ExitRunFile */

/* Near Support Calls from JobCode.ASM */

extern long AllocJCB(long *pdJobNumRet, char *ppJCBRet);
extern long RemoveRdyJob(char *pJCB);
extern long	GetExchOwner(long Exch, char *pJCBRet);
extern long	SetExchOwner(long Exch, char *pNewJCB);
extern long	SendAbort(long JobNum, long Exch);

/* Temporary NEAR externals from the monitor program for debugging */

extern long xprintf(char *fmt, ...);
extern U32 Dump(unsigned char *pb, long cb);

/* We switch to this stack when we clean out a user
   PD before we rebuild it.
*/

static long TmpStack[128];		/* 512 byte temporary stack */

/* Used for allocating/filling in new JCB */

static struct JCBRec *pNewJCB;		/* Used to access a JCB */
static struct JCBRec *pTmpJCB;
static struct JCBRec *pCrntJCB;

static long JobNum;

/* For ExitJob and Chain cause They can't have local vars! */

static long JobNumE, job_fhE;
static long ExchE, ercE, iE;
static long BogusMsg[2];
static long *pPDE;
static char *pExchJCBE
static long KeyCodeE;

static char aFileE[80];
static long cbFileE;

extern unsigned long KillExch;	/* From the Monitor */

/* Run file data */

static char *pCode, *pData, *pStack;	/* Ptrs in User mem to load to */
static long sCode,   sData,  sStack;	/* Size of segments */
static unsigned long oCode, oData;		/* Offset in file to Code & Data */

static long  offCode = 0,				/* Virtual Offset for Code & Data Segs */
	  offData = 0;

static unsigned long 	nCDFIX = 0,
				oCDFIX = 0,
			  	nCCFIX = 0,
			  	oCCFIX = 0,
			  	nDDFIX = 0,
				oDDFIX = 0,
			  	nDCFIX = 0,
			  	oDCFIX = 0;

static char *pStart, filetype;

static struct tagtype tag;


/************* INTERNAL SUPPORT CALLS ******************/

/******************************************************
 This deallocates all user memory in a PD.  This is
 used when we ExitJob to free up all the memory.
 We get the pointer to the PD and "walk" through all
 the User PTs deallocating linear memory. When all
 PTs have been cleaned, we eliminate the user PDEs.
 This leaves a blank PD for reuse if needed.
 NOTE: This must be called from a task that is
 running in the JCB that owns the memory!
******************************************************/

static void CleanUserPD(long *pPD)
{
long i, j, k, erc;
unsigned long *pPT;
char *pMem;

	for (i=768; i<1024; i++) {			/* Look at each shadow PDE */
		if (pPD[i]) {					/* If it's a linear address (non 0)*/
			pPT = pPD[i] & 0xFFFFF000;	/* Point to Page Table */
			for (j=0; j<1024; j++) {

			/* Get Beginning address for each run to deallocate */

				k = 0;	/* nPages to deallocate */
				pMem = ((i-512) * 0x400000) + (j * 4096);
				while ((pPT[j++]) && (j<1024))
					k++;			/* one more page */
				if (k)
				{			/* we have pages (one or more) */
					erc =  DeAllocPage(pMem, k);
				}
			}
		}
	}
}

/*********************************************************
  This opens, reads and validates the run file.
  If all is well, it leaves it open and returns the
  file handle, else it closes the run file and returns
  the error to the caller.
  The caller is responsible for closing the file!!!
*********************************************************/

static long GetRunFile(char *pFileName, long cbFileName, long *pfhRet)
{
long erc, i, fh, dret;
char fDone, junk;

	offCode = 0;
	offData = 0;
	nCDFIX = 0;
	oCDFIX = 0;
  	nCCFIX = 0;
  	oCCFIX = 0;
  	nDDFIX = 0;
	oDDFIX = 0;
  	nDCFIX = 0;
  	oDCFIX = 0;
  	fh = 0;

	*pfhRet = 0;	/* default to 0 */

			/* Mode Read, Stream type */
	erc = OpenFile(pFileName, cbFileName, 0, 1, &fh);

	if (!erc) {	/* File opened OK */

		fDone = 0;
		while ((!erc) && (!fDone)) {
			tag.id = 0;
			erc = ReadBytes (fh, &tag, 5, &dret);

			switch (tag.id) {
				case IDTAG:
					erc = ReadBytes (fh, &filetype, 1, &dret);
					if ((filetype < 1) || (filetype > 3))
						erc = ErcBadRunFile + 30000;
					break;
				case SEGTAG:
					erc = ReadBytes (fh, &sStack, 4, &dret);
					if (!erc) erc = ReadBytes (fh, &sCode, 4, &dret);
					if (!erc) erc = ReadBytes (fh, &sData, 4, &dret);
					break;
				case DOFFTAG:
					erc = ReadBytes (fh, &offData, 4, &dret);
					break;
				case COFFTAG:
					erc = ReadBytes (fh, &offCode, 4, &dret);
					break;
				case STRTTAG:
					erc = ReadBytes (fh, &pStart, 4, &dret);
					break;
				case CODETAG:
					erc = GetFileLFA(fh, &oCode);
					if (!erc)
						erc = SetFileLFA(fh, oCode+tag.len);	/* skip it */
					break;
				case DATATAG:
					erc = GetFileLFA(fh, &oData);
					if (!erc)
						erc = SetFileLFA(fh, oData+tag.len);	/* skip it */
					break;
				case CDFIXTAG:
					erc = GetFileLFA(fh, &oCDFIX);
					nCDFIX = tag.len/4;
					if (!erc)
						erc = SetFileLFA(fh, oCDFIX+tag.len);	/* skip it */
					break;
				case CCFIXTAG:
					erc = GetFileLFA(fh, &oCCFIX);
					nCCFIX = tag.len/4;
					if (!erc)
						erc = SetFileLFA(fh, oCCFIX+tag.len);	/* skip it */
					break;
				case DDFIXTAG:
					erc = GetFileLFA(fh, &oDDFIX);
					nDDFIX = tag.len/4;
					if (!erc)
						erc = SetFileLFA(fh, oDDFIX+tag.len);	/* skip it */
					break;
				case DCFIXTAG:
					erc = GetFileLFA(fh, &oDCFIX);
					nDCFIX = tag.len/4;
					if (!erc)
						erc = SetFileLFA(fh, oDCFIX+tag.len);	/* skip it */
					break;
				case ENDTAG:
					fDone = TRUE;
					break;
				default:
					erc = GetFileLFA(fh, &i);
					if (tag.len > 1024)
						erc = ErcBadRunFile;
					if (!erc)
						erc = SetFileLFA(fh, i+tag.len);	/* skip it */
					if (erc)
						erc = ErcBadRunFile;
				break;
			}

		}
		if (erc)
			CloseFile(fh);
	}
	if (!erc)
		*pfhRet = fh;

	return (erc);
}

/********************************************************/
/*********** PUBLIC CALLS FOR JOB MANAGEMENT ************/
/********************************************************/

/**************************************************/
long far _SetExitJob(char *pRunFile, long dcbRunFile)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if (dcbRunFile > 79)
		return (ErcBadJobParam);
	else if (!dcbRunFile)
		pTmpJCB->JcbExitRF[0] = 0;
	else {
		CopyData(pRunFile, &pTmpJCB->JcbExitRF[1], dcbRunFile);
		pTmpJCB->JcbExitRF[0] = dcbRunFile;
	}
	return(0);
}

/**************************************************/
long far _GetExitJob(char *pRunRet, long *pdcbRunRet)
{
long JobNum, i;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	i =	pTmpJCB->JcbExitRF[0];
	if (i)
		CopyData(&pTmpJCB->JcbExitRF[1], pRunRet, i);
	*pdcbRunRet = i;
	return(0);
}

/**************************************************/
long far _SetPath(char *pPath, long dcbPath)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if (dcbPath > 69)
		return (ErcBadJobParam);
	else if (!dcbPath)
		pTmpJCB->sbPath[0] = 0;
	else {
		CopyData(pPath, &pTmpJCB->sbPath[1], dcbPath);
		pTmpJCB->sbPath[0] = dcbPath;
	}
	return(0);
}

/**************************************************/
long far _GetPath(long JobNum, char *pPathRet, long *pdcbPathRet)
{
long i, erc;

	erc = GetpJCB(JobNum, &pTmpJCB);  /* Get pJCB to JobNum */
	if (!erc) {
		i =	pTmpJCB->sbPath[0];
		if (i)
			CopyData(&pTmpJCB->sbPath[1], pPathRet, i);
		*pdcbPathRet = i;
	}
	return(erc);
}
/**************************************************/
long far _SetCmdLine(char *pCmd, long dcbCmd)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if (dcbCmd > 79)
		return (ErcBadJobParam);
	else if (!dcbCmd)
		pTmpJCB->JcbCmdLine[0] = 0;
	else {
		CopyData(pCmd, &pTmpJCB->JcbCmdLine[1], dcbCmd);
		pTmpJCB->JcbCmdLine[0] = dcbCmd;
	}
	return(0);
}

/**************************************************/
long far _GetCmdLine(char *pCmdRet, long *pdcbCmdRet)
{
long JobNum, i;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	i =	pTmpJCB->JcbCmdLine[0];
	if (i)
		CopyData(&pTmpJCB->JcbCmdLine[1], pCmdRet, i);
	*pdcbCmdRet = i;
	return(0);
}


/**************************************************/
long far _SetUserName(char *pUser, long dcbUser)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if (dcbUser > 29)
		return (ErcBadJobParam);
	else if (!dcbUser)
		pTmpJCB->sbUserName[0] = 0;
	else {
		CopyData(pUser, &pTmpJCB->sbUserName[1], dcbUser);
		pTmpJCB->sbUserName[0] = dcbUser;
	}
	return(0);
}

/**************************************************/
long far _GetUserName(char *pUserRet, long *pdcbUserRet)
{
long JobNum, i;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	i =	pTmpJCB->sbUserName[0];
	if (i)
		CopyData(&pTmpJCB->sbUserName[1], pUserRet, i);
	*pdcbUserRet = i;
	return(0);
}

/**************************************************/
long far _SetSysIn(char *pName, long dcbName)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if ((dcbName > 49) || (!dcbName))
		return (ErcBadJobParam);
	else {
		CopyData(pName, &pTmpJCB->JcbSysIn[1], dcbName);
		pTmpJCB->JcbSysIn[0] = dcbName;
	}
	return(0);
}

/**************************************************/
long far _GetSysIn(char *pFileRet, long *pdcbFileRet)
{
long JobNum, i;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	i =	pTmpJCB->JcbSysIn[0];
	if (i)
		CopyData(&pTmpJCB->JcbSysIn[1], pFileRet, i);
	*pdcbFileRet = i;
	return(0);
}

/**************************************************/
long far _SetSysOut(char *pName, long dcbName)
{
long JobNum;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if ((dcbName > 49) || (!dcbName))
		return (ErcBadJobParam);
	else {
		CopyData(pName, &pTmpJCB->JcbSysOut[1], dcbName);
		pTmpJCB->JcbSysOut[0] = dcbName;
	}
	return(0);
}

/**************************************************/
long far _GetSysOut(char *pFileRet, long *pdcbFileRet)
{
long JobNum, i;

	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	i =	pTmpJCB->JcbSysOut[0];
	if (i)
		CopyData(&pTmpJCB->JcbSysOut[1], pFileRet, i);
	*pdcbFileRet = i;
	return(0);
}

/**************************************************/
long far _SetJobName(char *pName, long dcbName)
{
long JobNum;
	GetJobNum(&JobNum);
	GetpJCB(JobNum, &pTmpJCB);		/* Get pJCB to current Job */
	if (dcbName > 13)
		dcbName = 13;
	if (dcbName)
		CopyData(pName, &pTmpJCB->sbJobName[1], dcbName);
	pTmpJCB->sbJobName[0] = dcbName;
	return(0);
}

/*********************************************************
  This creates and loads a new Job from a RUN file.
  This returns ErcOK and new job number if loaded OK
  else an error is returned.
*********************************************************/

long far _LoadNewJob(char *pFileName, long cbFileName, long *pJobNumRet)
{
long erc, i, fh, dret, nPages;
unsigned long *pPD, *pPT, *pVid, *pOSPD;
long *pFix;
U32 PhyAdd;


	erc =  GetRunFile(pFileName, cbFileName, &fh);

	if (!erc) {

		/* We set these to zero so we can tell if they have
		been allocated in case we fail so we know what to deallocate
		*/

		JobNum = 0;
		pNewJCB = 0;
		pPD = 0;
		pPT = 0;
		pVid = 0;

		erc = AllocJCB(&JobNum, &pNewJCB);

		/* Alloc OS memory pages required for new job */

		if (!erc)
			erc = AllocOSPage(3, &pPD);		/* Job's PD, PT * pVirtVid */

		pPT = pPD + 4096;			/* 1 page later */
		pVid = pPD + 8192;			/* 2 pages later */

		if (!erc) {
			FillData(pPT, 4096, 0);		/* Zero user PT */
			FillData(pVid, 4096, 0);	/* Zero user video */

			GetpJCB(1, &pTmpJCB);		/* Get OS pJCB */
			pOSPD = pTmpJCB->pJcbPD;	/* Pointer to OS PD */

			GetPhyAdd(1, pPT, &PhyAdd);	/* Get Phy Add for new PT */

			PhyAdd |= MEMUSERD;			/* Set user code bits in PDE */

			pOSPD[256] = PhyAdd; 		/* Make User PDE in OS PD */
			pOSPD[768] = pPT;			/* Shadow Linear Address of PT */

			/* for serious troubleshooting...
		  	xprintf("pOSPD  : %08x\r\n", pOSPD);
		  	xprintf("pUserPD: %08x\r\n", pPD);
		  	xprintf("pUserPT: %08x\r\n", pPT);
		  	xprintf("PhyAdd : %08x\r\n", PhyAdd);
			ReadKbd(&KeyCodeE, 1);
			*/

			/* Now we can allocate User Job Memory */
			/* Allocate user memory for Stack Code and Data */
			/* This is STILL done in the OS PD */

			nPages = sStack/4096;
			if (sStack%4096) nPages++;
			erc = AllocPage(nPages, &pStack);
			sStack = nPages * 4096;				/* set to whole pages */

			nPages = sCode/4096;
			if (sCode%4096) nPages++;
			if (!erc)
				erc = AllocPage(nPages, &pCode);

			nPages = sData/4096;
			if (sData%4096) nPages++;
			if (!erc)
				erc = AllocPage(nPages, &pData);

			/* Right now, the OS PD looks exacly like we want
			the User PD to look.  We will now copy the entire
			OS PD into the User's New PD.
			*/

			CopyData(pOSPD,	pPD, 4096);		/* Copy OS PD to User PD */

			/* All Job memory is now allocated, so let's LOAD IT! */

			if (!erc)
				erc = SetFileLFA(fh, oCode);
			if (!erc)
				erc = ReadBytes (fh, pCode, sCode, &dret);

			if (!erc)
				erc = SetFileLFA(fh, oData);
			if (!erc)
				erc = ReadBytes (fh, pData, sData, &dret);

			/* Now that we have read in the code and data we
			apply fixups to these segments from the runfile
			*/

			if (!erc) {

				if (nCDFIX) {
					erc = SetFileLFA(fh, oCDFIX);	/* back to fixups */
					while ((nCDFIX--) && (!erc)) {
						erc = ReadBytes (fh, &i, 4, &dret);
						pFix = pCode + i;		/* Where in CSeg */
						*pFix = *pFix - offData + pData;
					}
				}

				if (nCCFIX) {
					erc = SetFileLFA(fh, oCCFIX);	/* back to fixups */
					while ((nCCFIX--) && (!erc)) {
						erc = ReadBytes (fh, &i, 4, &dret);
						pFix = pCode + i;		/* Where in CSeg */
						*pFix = *pFix - offCode + pCode;
					}
				}

				if (nDCFIX) {
					erc = SetFileLFA(fh, oDCFIX);	/* back to fixups */
					while ((nDCFIX--) && (!erc)) {
						erc = ReadBytes (fh, &i, 4, &dret);
						pFix = pData + i;		/* Where in DSeg */
						*pFix = *pFix - offCode + pCode;
					}
				}

				if (nDDFIX) {
					erc = SetFileLFA(fh, oDDFIX);	/* back to fixups */
					while ((nDDFIX--) && (!erc)) {
						erc = ReadBytes (fh, &i, 4, &dret);
						pFix = pData + i;		/* Where in DSeg */
						*pFix = *pFix - offData + pData;
					}
				}

			}

			/* Clean the OS PD of User memory */

			FillData(&pOSPD[256], 1024, 0);	/* Clean OS PD of User PDEs */
			FillData(&pOSPD[768], 1024, 0);	/* Clean OS PD of User Shadow */

			/* Now we fill in the rest of the User's JCB */

			pNewJCB->pJcbPD = pPD;			/* Lin Add of PD */
			pNewJCB->pJcbStack = pStack;	/* Add of Stack */
			pNewJCB->sJcbStack = sStack;	/* Size of Code */
			pNewJCB->pJcbCode  = pCode;		/* Add of Code */
			pNewJCB->sJcbCode  = sCode; 	/* Size of Code */
			pNewJCB->pJcbData  = pData;		/* Add of Code */
			pNewJCB->sJcbData  = sData; 	/* Size of Code */

			pNewJCB->sbUserName[0] = 0; 	/* Zero UserName */
			pNewJCB->sbPath[0] = 0;		 	/* No Default path */
			pNewJCB->JcbExitRF[0] = 0;	 	/* No Exit Run File */
			pNewJCB->JcbCmdLine[0] = 0;	 	/* No Cmd Line */

			CopyData("KBD", &pNewJCB->JcbSysIn[1], 3);
			pNewJCB->JcbSysIn[0] = 3;	 	/* Size */

			CopyData("VID", &pNewJCB->JcbSysOut[1], 3);
			pNewJCB->JcbSysOut[0] = 3;	 	/* Size */

			pNewJCB->pVidMem = pVid;	 	/* Default to Virt Vid */
			pNewJCB->pVirtVid = pVid;	 	/* Virtual Video memory */
			pNewJCB->CrntX = 0;			 	/* Vid X Position */
			pNewJCB->CrntY = 0;			 	/* Vid Y Position */
			pNewJCB->nCols = 80;		 	/* Columns */
			pNewJCB->nLines = 25;		 	/* Lines */

			pNewJCB->VidMode = 0;		 	/* 80x25 VGA Color Text */
			pNewJCB->fCursOn = 1;		 	/* Cursor On */
			pNewJCB->fCursType = 0;		 	/* UnderLine */


			/* Finally, we crank up the new task and schedule it for
			execution!
			*/
			if (!erc)
				erc = AllocExch(&i);

			if (!erc)
				erc = NewTask(JobNum, 0x18, 25, 0, i,
							  pStack+sStack-4,
							  pStart+pCode-offCode);
			if (!erc)
				SetExchOwner(i, pNewJCB);	/* Exch now belongs to new JCB */
		}

		CloseFile(fh);
	}	/* read run file OK */

	if (!erc)
		*pJobNumRet = JobNum;

	return(erc);
}

/*********************************************************
  This loads a job into an existing PD and JCB.  This is
  called by Chain() and may also be called by ExitJob()
  if an ExitJob was specified in the current JCB.
  The PD, pVid & first PT still exist in OS memory.
  (CleanPD left the first PT for us). ExitJob and Chain
  are responsible for opening and validating the runfile
  and setting up the run file variables.
*********************************************************/

static long LoadJob(char *pJCB, long fh)
{
long erc, i, dret, nPages;
long *pFix;

	pNewJCB = pJCB;

	/* Allocate user memory for Stack, Code and Data */
	/* This is done in the context of the USER PD */

	nPages = sStack/4096;
	if (sStack%4096) nPages++;
	erc = AllocPage(nPages, &pStack);
	sStack = nPages * 4096;				/* set to whole pages */

	nPages = sCode/4096;
	if (sCode%4096) nPages++;
	if (!erc)
		erc = AllocPage(nPages, &pCode);

	nPages = sData/4096;
	if (sData%4096) nPages++;

	erc = AllocPage(nPages, &pData);

	/* All Job memory is now allocated, so let's LOAD IT! */

	if (!erc)
		erc = SetFileLFA(fh, oCode);
	if (!erc)
		erc = ReadBytes (fh, pCode, sCode, &dret);

	if (!erc)
		erc = SetFileLFA(fh, oData);
	if (!erc)
		erc = ReadBytes (fh, pData, sData, &dret);

	/* Now that we have read in the code and data we
	apply fixups to these segments from the runfile
	*/

	if (!erc) {
		if (nCDFIX) {
			erc = SetFileLFA(fh, oCDFIX);	/* back to fixups */
			while ((nCDFIX--) && (!erc)) {
				erc = ReadBytes (fh, &i, 4, &dret);
				pFix = pCode + i;		/* Where in CSeg */
				*pFix = *pFix - offData + pData;
			}
		}

		if (nCCFIX) {
			erc = SetFileLFA(fh, oCCFIX);	/* back to fixups */
			while ((nCCFIX--) && (!erc)) {
				erc = ReadBytes (fh, &i, 4, &dret);
				pFix = pCode + i;		/* Where in CSeg */
				*pFix = *pFix - offCode + pCode;
			}
		}

		if (nDCFIX) {
			erc = SetFileLFA(fh, oDCFIX);	/* back to fixups */
			while ((nDCFIX--) && (!erc)) {
				erc = ReadBytes (fh, &i, 4, &dret);
				pFix = pData + i;		/* Where in DSeg */
				*pFix = *pFix - offCode + pCode;
			}
		}

		if (nDDFIX) {
			erc = SetFileLFA(fh, oDDFIX);	/* back to fixups */
			while ((nDDFIX--) && (!erc)) {
				erc = ReadBytes (fh, &i, 4, &dret);
				pFix = pData + i;		/* Where in DSeg */
				*pFix = *pFix - offData + pData;
			}
		}

		/* Now we fill in the rest of the User's JCB */
		pNewJCB->pJcbStack = pStack;	/* Add of Stack */
		pNewJCB->sJcbStack = sStack;	/* Size of Code */
		pNewJCB->pJcbCode  = pCode;		/* Add of Code */
		pNewJCB->sJcbCode  = sCode; 	/* Size of Code */
		pNewJCB->pJcbData  = pData;		/* Add of Code */
		pNewJCB->sJcbData  = sData; 	/* Size of Code */

	}
	CloseFile(fh);
	return(erc);
}


/******************************************************
 This is started as new task in the context of a
 job that is being killed off. This is done to allow
 memory access and also reuse the code for ExitJob
 in the monitor. This task, it's exchange, and TSS
 will be reclaimed by the monitor along with the
 JCB and all OS memory pages for the PD,PT and video.
******************************************************/

void _KillTask(void)
{

	GetJobNum(&JobNumE);
	GetpJCB(JobNumE, &pTmpJCB);		/* Get pJCB to this Job */

	/* Clean the PD of all user memory leaving OS memory to be
	   deallocated by the caller (monitor or whoever).
	*/

	pPDE = pTmpJCB->pJcbPD;
	CleanUserPD(pPDE);

	GetTSSExch(&ExchE);

    ercE = 0;
	while(!ercE)        	/* clear the exchange */
		ercE = CheckMsg(ExchE, BogusMsg);

	ISendMsg(KillExch, ExchE, ErcOpCancel);
	SetPriority(31);
	WaitMsg(ExchE, BogusMsg);

	while(1); /* in case we get scheduled again RAB */

	/* He's History! */
}


/******************************************************
 This called from one job to kill another job or
 service. This cleans up ALL resources that the
 job had allocated.
 This is used to kill run-away jobs, or terminate a
 job just for the fun ot it.
 It results in a violent death for the job specified.
 This must never be called from a task within the job
 to be killed.  A job may terminate itself with ExitJob().
******************************************************/

long far _KillJob(long JobNum)
{
long erc;
	/* Make sure it's not the Monitor, Debugger or the current job. */

	GetJobNum(&JobNumE);
	if ((JobNum == JobNumE) ||
	    (JobNum == 1) ||
	    (JobNum == 2))

		return(ErcBadJobNum);

	erc = GetpJCB(JobNum, &pTmpJCB);			/* Get pJCB to the Job */
	if (erc)
		return(erc);

	pTmpJCB->ExitError = ErcOpCancel; /* Operator said DIE! */

	/* Remove ALL tasks for this job that are at the ReadyQue.
	   The task we are in does not belong to the job we are
	   killing so we can remove them all.
	*/

	RemoveRdyJob(pTmpJCB);	/* It always returns ErcOk */

	/* Deallocate ALL exchanges for this job */

	 ercE = 0;
	 iE = 0;
	 while (ercE != ErcOutOfRange)
	 {
	 	ercE = GetExchOwner(iE, &pExchJCBE);
		if ((!ercE) && (pExchJCBE == pTmpJCB))
			DeAllocExch(iE);
		iE++;
	 }
	 ercE = 0;		/* Clear the error */

	/* Now that the user can't make anymore requests,
	   We send "Abort" messages to all services.
	   This closes all files that were opened by the Job
       and frees up any other resources held for this
       job by any service.

	   We will allocate one exchange for the job
	   that is being killed so we can use it for SendAbort
	   and also as the exchange number we send to the
	   KillExch in the monitor which will kill of the JCB
	   completely (he also switches video and keyboard
	   if needed).
	 */

	erc = AllocExch(&ExchE);		/* Get an Exch */
	SetExchOwner(ExchE, pTmpJCB);	/* make him the owner */
	SendAbort(JobNum, ExchE); 	/* Notify all services */

		/*JobNum, CodeSeg, Priority, fDebug, Exch, ESP, EIP */
	erc = NewTask(JobNum, 0x08, 3, 0, ExchE, &TmpStack[127], &_KillTask);
	return(erc);

	/* He's History! */
}

/******************************************************
 This called from Exit() in C or directly from a user
 job or service. This cleans up ALL resources that the
 job had allocated.
 This also checks for an exit run file to load if
 one is specified. If no exit run file is specified
 we just kill the JCB entirely and if video and
 keyboard are assigned we assign them to the Monitor.
******************************************************/

void far _ExitJob(long dError)
{
/* NO LOCAL VARIABLES BECAUSE WE SWITCH STACKS!! */

	GetJobNum(&JobNumE);
	GetpJCB(JobNumE, &pCrntJCB);		/* Get pJCB to current Job */
	pCrntJCB->ExitError = dError;

	/* Remove ALL tasks for this job that are at the ReadyQue.
	   The task we are in won't be removed because its RUNNING!
	*/

	RemoveRdyJob(pCrntJCB);	/* It always returns ErcOk */

	/* Deallocate all exchanges for this job except the one belonging
	   to current TSS!  The Dealloc Exchange call will invalidate
	   all TSSs found at exchanges belonging to this user, and
	   will also free up RQBs and Link Blocks.  The job will not be
	   able to initiate requests or send messages after this unless
	   it is done with the TSSExchange because it will get a kernel
	   error (invalid exchange).
	*/

	 /* Find out what our TSS exchange is so
	 we don't deallocate it to! We need it. */

	 GetTSSExch(&ExchE);

	 ercE = 0;
	 iE = 0;
	 while (ercE != ErcOutOfRange)
	 {
	 	ercE = GetExchOwner(iE, &pExchJCBE);
		if ((!ercE) && (iE != ExchE) && (pExchJCBE == pCrntJCB))
			DeAllocExch(iE);
		iE++;
	 }

	/* Now that the user can't make anymore requests,
	   Send Abort messages to all services.
	   This closes all files that were opened by the Job
       and frees up any other resources held for this
       job by any service.
	*/

	SendAbort(JobNumE, ExchE);

	ercE = 0;				/* Clear the error */
	while(!ercE)        	/* clear the exchange */
		ercE = CheckMsg(ExchE, BogusMsg);
	ercE = 0;				/* Clear the error */

	/* We must now switch to a temporary stack so we can
	clean out the user PD (we are on his stack right now!).
	*/

#asm
	MOV EAX, OFFSET _TmpStack
	ADD EAX, 508
	MOV ESP, EAX
	MOV EBP, EAX
#endasm

	/* Clean the PD of all user memory leaving OS memory for next
	job if there is one.
	*/

	pPDE = pCrntJCB->pJcbPD;
	CleanUserPD(pPDE);

	/* Look for Exit Run file to load if any exists.  If no exit run
	file, we deallocate the PD and JCB then return to JOB 1. */

	GetExitJob(aFileE, &cbFileE);     	/* Exit Run File!! */

	if (!cbFileE)
		ercE = ErcNoExitJob;

	if (!ercE)
		ercE =  GetRunFile(aFileE, cbFileE, &job_fhE);

	if (!ercE)
		ercE = LoadJob(pCrntJCB, job_fhE);

	if (!ercE) {

			pStart = pStart+pCode-offCode;

			/* Now we RETURN to new job's address after we put him
			on his new stack. */

#asm
			MOV EAX, _pStack
			MOV EBX, _sStack
			ADD EAX, EBX
			SUB EAX, 4
			MOV ESP, EAX
			MOV EBP, EAX
			PUSH 18h
			MOV EAX, _pStart
			PUSH EAX
			RETF				;We are history!
#endasm

	}

	if (ercE) {		/* something failed or we don't have an ExitRF */

		/* In case there is no job to run or a fatal error has happened
		we send a message (ISendMsg) to the monitor status
		task with our TSSExch and the Error. Then he will WIPE US OUT!
		We use ISend (vice send) so he can't run before we get to
		the exchange otherwise we will be placed back on the readyQueue!
	    */

		ISendMsg(KillExch, ExchE, ercE);
		SetPriority(31);
		WaitMsg(ExchE, BogusMsg);

		while(1); /* in case we get scheduled again RAB */

		/* We are NO MORE */
	}
}

/******************************************************
 This is called to execute a program without changing
 the ExitJob. This is so you can run program B from
 program A and return to program A when program B is
 done.  This runs Job B in the "context" of Job A
 which means Job B inherits the JCB and PD of job A
 so it can use things like the command line and
 path that were set up by A.
 Information can be passed to Job B by calling
 SetCmdLine (if Job B reads it), and also by
 setting the ExitError value in the parameter.
 Chain will only return to you if there was an
 error loading the Job.  In other words, if Chain
 fails in a critial section we try to load the
 ExitJob and pass it the error.
******************************************************/

long far _Chain(char *pFileName, long cbFileName, long dExitError)
{
/* NO LOCAL VARIABLES BECAUSE WE SWITCH STACKS!! */

	CopyData(pFileName, aFileE, cbFileName);
	cbFileE = cbFileName;

	ercE =  GetRunFile(pFileName, cbFileName, &job_fhE);
	if (ercE)
	{
		CloseFile(job_fhE);	/* if it had a handle at all */
		return(ercE);
	}

	CloseFile(job_fhE);		/* we will open it again after SendAbort */

	GetJobNum(&JobNumE);
	GetpJCB(JobNumE, &pCrntJCB);		/* Get pJCB to current Job */
	pCrntJCB->ExitError = dExitError;

	/* Remove ALL tasks for this job that are at the ReadyQue.
	   The task we are in won't be removed because its RUNNING!
	*/

	RemoveRdyJob(pCrntJCB);	/* It always returns ErcOk */

	/* Deallocate all exchanges for this job except the one belonging
	   to current TSS!  The Dealloc Exchange call will invalidate
	   all TSSs found at exchanges belonging to this user, and
	   will also free up RQBs and Link Blocks.  The job will not be
	   able to initiate requests or send messages after this unless
	   it is done with the TSSExchange because it will get a kernel
	   error (invalid exchange).
	*/

	 /* Find out what our TSS exchange is so
	 we don't deallocate it to! */

	 GetTSSExch(&ExchE);

	 ercE = 0;
	 iE = 0;
	 while (ercE != ErcOutOfRange) 
	 {
	 	ercE = GetExchOwner(iE, &pExchJCBE);
		if ((!ercE) && (iE != ExchE) && (pExchJCBE == pCrntJCB))
			DeAllocExch(iE);
		iE++;
	 }

	/* Now that the user can't make anymore requests,
	   Send Abort messages to all services.
	   This closes all files that were opened by the Job
       and frees up any other resources held for this
       job by any services.
	*/

	SendAbort(JobNumE, ExchE);

	ercE = 0;				/* Clear the error */
	while(!ercE)        	/* clear the exchange of abort responses*/
		ercE = CheckMsg(ExchE, BogusMsg);
	ercE = 0;				/* Clear the error */

	/* We must now switch to a temporary stack so we can
	clean out the user PD (we are on his stack right now!).
	*/

#asm
	MOV EAX, OFFSET _TmpStack
	ADD EAX, 508
	MOV ESP, EAX
	MOV EBP, EAX
#endasm

	/* Clean the PD of all user memory leaving OS memory for next
	job if there is one.
	*/

	pPDE = pCrntJCB->pJcbPD;
	CleanUserPD(pPDE);

	/* Try to load the Chain file. Don't bother checking error
	   cause it was valid if we got here!
	*/

	GetRunFile(aFileE, cbFileE, &job_fhE); /* it was valid before!*/
	ercE = LoadJob(pCrntJCB, job_fhE);

	if (ercE) 
	{
		/* We have errored in a critical part of Chain (LoadJob).
		The original user's job is destroyed, and we can't run the
		chain file (bummer).
		The only thing left to do is Look for Exit Run file to load
		if any exists.  If no exit run file, we kill this guy
		and return to the monitor if he had the screen. */

		GetExitJob(aFileE, &cbFileE);     	/* Exit Run File!! */

		if (!cbFileE)
			ercE = ErcNoExitJob;

		if (!ercE)
			ercE =  GetRunFile(aFileE, cbFileE, &job_fhE);

		if (!ercE)
			ercE = LoadJob(pCrntJCB, job_fhE);
	}

	if (!ercE) 
	{		/* No error */

		pStart = pStart+pCode-offCode;

		/* Now we RETURN to new job's address after we put him
		on his new stack. */

#asm
		MOV EAX, _pStack
		MOV EBX, _sStack
		ADD EAX, EBX
		SUB EAX, 4
		MOV ESP, EAX
		MOV EBP, EAX
		PUSH 18h
		MOV EAX, _pStart
		PUSH EAX
		RETF				;We are history!
#endasm
	}

	if (ercE) 
	{		/* Something failed loading the job (Chain or Exit) */

		/* In case there is no job to run or a fatal error has happened
		we send a message (ISendMsg) to the monitor status
		task with our TSSExch and the Error. Then he will WIPE US OUT!
		We use ISend (vice send) so he can't run before we get to
		the exchange otherwise we will be placed back on the readyQueue!
	    */

		ISendMsg(KillExch, ExchE, ercE);  /* ISend clears ints! */
#asm
		STI
#endasm
		SetPriority(31);
		WaitMsg(ExchE, BogusMsg);

		while(1); /* in case we get scheduled again RAB */

		/* We are NO MORE */

	}
}

/*********************** End of Module *****************/
