/*  Copyright (c) 1994, R.A. Burgess */

#include <ctype.h>

#define nParamsMax 25
#define sCmdLine 80

extern far void ExitJob(long dError);
extern far long GetCmdLine(char *pCmdRet, long *pdcbCmdRet);
extern void main(int nParams, char *pParams[]);

void exit(long erc)
{
	ExitJob(erc);
}

/* This cleans the command line for the main function
   in a C program. It builds an array of pointers to
   each of the params (zero terminating each of them)
   and also fills in the count for main(argc, *argv[]);
   Then is calls main.
*/

void stdentry(void)
{
long iPrm, fPrm, i;		/* iCmd is index to aCmd */
char aCmd[80];			/* the command line */
long cbCmd;				/* length of command line */
char *apParams[25];		/* Param 0 is cmd name */
long nParams;			/* for main() */
char finquotes;

	cbCmd = 0;		/* length of command line */
	finquotes=0;	/* to start */
	nParams = 0;	/* default */
	for (i=0; i<sCmdLine; i++)
		aCmd[i] = 0;
	for (i=0; i<25; i++)
		apParams[i] = 0;
	GetCmdLine(aCmd, &cbCmd);
	iPrm = 0;   /* index to apParam */
	fPrm = 0;	/* No param to start */
	for (i=0; i<cbCmd; i++)
	{
		if (aCmd[i] == '"')
		{
			if (finquotes)
				finquotes=0;
			else
				finquotes=1;
			aCmd[i] = 0;
			continue;
		}
		if (isspace(aCmd[i]))
		{
			if (finquotes)
				continue;
			aCmd[i] = 0;
			fPrm = 0;
		}

		/* Sets the pointer to the first char of a param */

		if ((aCmd[i]) && (!fPrm))	/* not null and no param yet */
		{
			apParams[iPrm++] = aCmd + i;		/* Set the ptr */
			nParams++;
			fPrm = 1;
		}
	}

	main(nParams, apParams);
	exit(0);
}
