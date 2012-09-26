/* Super Simple System Service.
   This is expanded from the sample in the Systems Programming
   chapter to show how to properly deinstall a system service.
   The steps to deinstall are:
   1) UnRegister the Service
   2) Serve all remaining requests at service exchange
   3) Deallocate all resources
   4) Exit
*/

#include <stdio.h>
#include "\OSSource\MKernel.h"
#include "\OSSource\MJob.h"
#include "\OSSource\MVid.h"

#define ErcOK         0
#define ErcOpCancel   4
#define ErcNoSuchSvc  30
#define ErcBadSvcCode 32

struct RqBlkType *pRqBlk;        /* A pointer to a Reqeust Block */
unsigned long NextNumber = 0;    /* The number to return */
unsigned long MainExch;          /* Where we wait for Requests */
unsigned long Message[2];        /* The Message with the Request */
long rqHndl;				 /* Used for keyboard request */

void main(void)
{
unsigned long OSError, ErrorToUser, keycode;
long *pDataRet;

  OSError = AllocExch(&MainExch);	  /* get an exchange */

  if (OSError)                      /* look for a kernel error */
    ExitJob(OSError);

  OSError = RegisterSvc("NUMBERS ", MainExch);

  if (OSError)                      /* look for a system error */
    ExitJob(OSError);

  SetNormVid(WHITE|BGBLACK);
  ClrScr();

  printf("NUMBERS Service Installed.\r\n");
  printf("ANY valid keystroke will terminate the service.\r\n");

  OSError = Request("KEYBOARD", 1, MainExch, &rqHndl, 0, &keycode,
   					  4, 0, 0, 1, 0, 0);  /* 1 in dData0 = WAIT for key */
  if (OSError)
	  printf("Error on Keyboard Request:\r\n", OSError);

  while (1)  		/* WHILE forever (almost...) */
  {

	/* Now we wait for a client or for a keystroke to come back */

    OSError = WaitMsg(MainExch, Message); /* Exch & pointer */

    if (!OSError)
    {

	  if (Message[0] == rqHndl)  /* it was a keystroke and NOT a client */
	  {
		UnRegisterSvc("NUMBERS ");
		while (!CheckMsg(MainExch, Message))
		{
            pRqBlk = Message[0];
			Respond(pRqBlk, ErcNoSuchSvc);
		}
		DeAllocExch(MainExch);
		ExitJob(ErcOpCancel);
	  }

      pRqBlk = Message[0];   /* First DWORD contains ptr to RqBlk */

	  if (pRqBlk->ServiceCode == 0)          /* Abort request from OS */
          ErrorToUser = ErcOK;

      else if (pRqBlk->ServiceCode == 1)     /* User Asking for Number */
      {
		 pDataRet = pRqBlk->pData1;
         *pDataRet = NextNumber++;       /* Give them a number */
         ErrorToUser = ErcOK;                 /* Respond with No error */

		printf("NUMBERS Service gave out number: %d.\r\n", NextNumber-1);

      }
      else ErrorToUser = ErcBadSvcCode;       /* Unknown Service code! */

      OSError = Respond(pRqBlk, ErrorToUser); /* Respond to Request */

    }
  }  /* Loop while(1) */
}
