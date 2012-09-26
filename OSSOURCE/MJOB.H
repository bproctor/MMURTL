/* MJob.h    MMURTL Job Management prototypes ****/

/* MMURTL Operating System Source Code
   Copyright 1991,1992,1993, Richard A. Burgess
   ALL RIGHTS RESERVED
   Version x0.8
*/


#define ErcBadJobNum	70	/* Bad job number was specified in OS call */
#define ErcInvalidJCB	71	/* Specified an unassigned JCB */
#define ErcBadRunFile	74	/* Run file you specified is NOT a run file */
#define ErcBadJobParam	76	/* Bad length in parameter to JCB call */

struct JCBRec {
	long 			JobNum;
	char 			sbJobName[14];		/* 13 bytes. First byte is length */
	char			*pJcbPD;			/* Linear add of Job's PD (0=unused) */

	char 			*pJcbCode;			/* User Address of code segment */
	unsigned long	sJcbCode;			/* Size of user code segment */
	char			*pJcbData;			/* User Adresss of data segment */
	unsigned long 	sJcbData;			/* Size of user data segment */
	char 			*pJcbStack;			/* User Addrees of first stack */
	unsigned long	sJcbStack;			/* Size of user first stack */

	char 			sbUserName[30];		/* User Name for Job - LString */
	char 			sbPath[70];			/* path name (prefix) - LString */
	char 			JcbExitRF[80];		/* Exit Run file (if any) - LString */
	char 			JcbCmdLine[80];		/* Command Line string - LString */
	char 			JcbSysIn[50];		/* Standard input  - LString */
	char 			JcbSysOut[50];		/* Standard output - LString */

	long  			ExitError;			/* Error Set by ExitJob */
	char 			*pVidMem;			/* pointer to crnt video buffer */
	char 			*pVirtVid;			/* Virtual Video Buffer Address */
	long 			CrntX;				/* Current cursor position */
	long 			CrntY;
	long 			nCols;				/* Virtual Screen Size */
	long 			nLines;
	long 			VidMode;			/* 0 = 80x25 VGA color text */
	long 			NormVid;			/* 7 = WhiteOnBlack */

	char 			fCursOn;			/* FLAG 1 = Cursor is visible */
	char 			fCursType;			/* FLAG	(0=UL, 1 = Block) */
	unsigned char 	ScrlCnt;			/* Count since last pause  */
	char 			fVidPause;			/* Full screen pause (Text mode) */
	long 			NextJCB;			/* OS Uses to allocate JCBs */
	char JcbRsvd1[512-22-24-360-36-8]	/* Padded to 512 */
	};

extern far long GetpJCB(long dJobNum, char *pJCBRet);
extern far long GetJobNum(long *pJobNumRet);
extern far long LoadNewJob(char *pFileName, long cbFileName, long *pJobNumRet);
extern far long Chain(char *pFileName, long cbFileName, long dExitError);
extern far void ExitJob(long dError);
extern far void KillJob(long JubNum);

extern far long SetUserName(char *pUser, long dcbUser);
extern far long GetUserName(char *pUserRet, long *pdcbUserRet);

extern far long SetCmdLine(char *pCmd, long dcbCmd);
extern far long GetCmdLine(char *pCmdRet, long *pdcbCmdRet);

extern far long SetPath(char *pPath, long dcbPath);
extern far long GetPath(long JobNum, char *pPathRet, long *pdcbPathRet);

extern far long SetExitJob(char *pRunFile, long dcbRunFile);
extern far long GetExitJob(char *pRunRet, long *pdcbRunRet);

extern far long SetSysIn(char *pFile, long dcbFile);
extern far long GetSysIn(char *pFileRet, long *pdcbFileRet);

extern far long SetSysOut(char *pFile, long dcbFile);
extern far long GetSysOut(char *pFileRet, long *pdcbFileRet);

extern far long SetJobName(char *pName, long dcbName);

extern far long RegisterSvc(char *pName, unsigned long Exch);

extern far long UnRegisterSvc(char *pName);

extern far long GetSystemDisk(unsigned char *pDiskRet);

/****** End of Job.h *******/
