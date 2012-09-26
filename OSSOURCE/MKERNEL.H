/* MKernel.h   MMURTL OS KERNEL PROTOTYPES */

/* MMURTL Operating System Source Code
   Copyright 1991,1992,1993, Richard A. Burgess
   ALL RIGHTS RESERVED
   Version x0.8
*/

extern far long AllocExch(long *pExchRet);

extern far long DeAllocExch(long Exch);

extern far long GetTSSExch(unsigned long  *pExchRet);

extern far long SetPriority(long bPriority);

extern far long NewTask(long JobNum,
					   long CodeSeg,
					   long Priority,
					   long fDebug,
					   long Exch,
					   unsigned long ESP,
					   unsigned long EIP);

extern far SpawnTask(char *pEntry,
		             long dPriority,
                     long fDebug,
                     char *pStack,
           		     long fOSCode);

extern far long SendMsg(long Exch, long msg1, long msg2);

extern far long ISendMsg(long Exch, long msg1, long msg2);

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

extern far long MoveRequest(long dRqBlkHndl, long dDestExch);

extern far long Respond(long dRqHndl, long dStatRet);

struct RqBlkType {			/* 64 byte Request block structure */
	long ServiceExch;
	long RespExch;
	long RqOwnerJob;
	long ServiceRoute;
	char *pRqHndlRet;
	long dData0;
	long dData1;
	long dData2;
	short int  ServiceCode;
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


/******* End of MKernel.h ******/
