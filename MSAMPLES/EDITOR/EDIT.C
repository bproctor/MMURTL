/* Edit.c  A simple editor using MMURTL file system and keyboard services
   Use MakeIT.Bat to build this MMURTL samples application, or
   use CM32 and DASM spearately:
   CM32 Edit.c
   DASM Edit.bat
*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

#define TRUE 1
#define FALSE 0

#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Includes for OS public calls and structures */

#include "\OSSOURCE\MKernel.h"
#include "\OSSOURCE\MMemory.h"
#include "\OSSOURCE\MData.h"
#include "\OSSOURCE\MTimer.h"
#include "\OSSOURCE\MVid.h"
#include "\OSSOURCE\MKbd.h"
#include "\OSSOURCE\MJob.h"
#include "\OSSOURCE\MFiles.h"

#define EDVID   BRITEWHITE|BGBLUE
#define NORMVID WHITE|BGBLACK
#define MARKVID WHITE|BGRED
#define STATVID BLACK|BGCYAN

#define EMPTY	99999
#define NLINESMAX 26

struct EditRecType {
	U8      *pBuf;
    U8		*pBufWork;			/* For copy and move */
	U32		Line[NLINESMAX];	/* Offset in buf for 1st char in line */
	U32     iBufMax;			/* sBuf - 1 */
	U32     iColMin;			/* Screen coords */
	U32     iRowMin;
	U32     iColMax;
	U32     iRowMax;
	U32     sLine;
	U8      bSpace;
	U8		fVisible;
	U32     iAttrMark;
	U32     iAttrNorm;
	U32     iTabNorm;
    U32     oBufLine0   /* oBufLine0 */
	U32     iCol;		/* cursor, 0..sLine-1 */
	U32     iLine;      /* cursor, 0..cLines-1 */
	U32     oBufInsert; /* offset of next char in */
	U32     oBufLast;   /* offset+1 of last char */
	U32     oBufMark;
	U32     oBufBound;
	};

struct EditRecType  EdRec;
struct EditRecType  *pEdit;
char *pBuf1, *pBuf2;
unsigned char b, b1;
long  erc, fh;
char fModified;
char fOvertype;
char aStat[80];
char aStat1[80];
char aCmd[80];			/* Get our command line */
long cbCmd = 0;
char *apParam[13];		/* Param 0 is cmd name */
long acbParam[13];
#define nParamsMax 13
char Filename[60];
long cbFilename;
unsigned char filler[100];


void clearbuf(void);	/* prototype for forward usage */

/*********************************************************
  Displays errors if they occur for certain file operations.
*********************************************************/

long CheckErc(long call, long erc)
{
char st[40];
long i;

	if (erc) {
		FillData(st, 40, 0);
		Beep();
		switch (call) {
		case 1:
			sprintf(st, "Error %05d occurred on OpenFile", erc);
			break;
		case 2:
			sprintf(st, "Error %05d occurred on ReadBytes", erc);
			break;
		case 3:
			sprintf(st, "Error %05d occurred on WriteBytes", erc);
			break;
		case 4:
			sprintf(st, "Error %05d occurred on CreateFile", erc);
			break;
		case 5:
			sprintf(st, "Error %05d occurred on SetFileSize", erc);
			break;
		case 6:
			sprintf(st, "Error %05d occurred on SetFileLFA", erc);
			break;
		case 7:
			sprintf(st, "Error %05d occurred on ReadKbd", erc);
			break;
		default:
			sprintf(st, "Error %05d occurred on last command", erc);
			break;
		}
		for (i=0; i<40; i++)
			if (!st[i])
				st[i] = ' ';

	    PutVidChars (40, 24, st, 39, STATVID);
	}
    return (erc);
}

/*********************************************************
  Clears the status line with 80 blank chars.
*********************************************************/
void ClearStatus(void)
{
char st[80];
	FillData(st, 80, 0);
    PutVidChars (0, 24, st, 80, NORMVID);
}


/*************************************************************
   Saves a file you are editing. If fPrompt is true, this
   will prompt you to save. If fClose is true, the file will be 
   closed and the buffer will be closed.
**************************************************************/

void SaveFile(int fPrompt, int fClose)
{
U32 i, keycode, fYes;
unsigned char  *pBuff;

  pBuff = pEdit->pBuf;

	if ((fh) && (fModified))
	{
		if (pEdit->fVisible)
		{    /* fix visible characters */
		     for (i=0; i <=pEdit->iBufMax; i++)
		         if (pBuff[i] == 0x07)
		         	  pBuff[i] = 0x20;
                 pEdit->fVisible = FALSE;
	    }
        fYes = 1;
		if (fPrompt)
		{
			ClearStatus();
			SetXY(0, 24);
		    TTYOut("This file has been modified. SAVE IT? (Y/N)", 43, BLACK|BGCYAN);
			ReadKbd(&keycode, 1);
			if (((keycode & 0xff) == 'N') || ((keycode & 0xff) == 'n'))
			{
				fYes = 0;
				ClearStatus();
			}
		}

		if (fYes)
		{
			erc = CheckErc(6, SetFileLFA(fh, 0));
			if (!erc)
				erc = CheckErc(5, SetFileSize(fh,
							pEdit->oBufLast));
			if (!erc)
				erc = CheckErc(3, WriteBytes (fh, pBuf1,
							   pEdit->oBufLast, &i));
			fModified = 0;
			ClearStatus();
		    PutVidChars (0, 24, "DONE...   ", 10, STATVID);
			Sleep(150);
			ClearStatus();
		}

	}

	if (fh && fClose)
	{
		CloseFile(fh);
		fh = 0;
		cbFilename = 0;
		clearbuf();
	}
}

/*************************************************************
   This prompts for a filename to open and opens it if it
   exists. If not, it will prompts to create.
**************************************************************/

void OpenAFile(char *name)
{
U32 filesize, dret, keycode;

	erc = 0;
	cbFilename = 0;
	if (!name)
	{
		SetXY(0,24);
	    PutVidChars (0,24, "Filename: ", 10, BLACK|BGWHITE);
		SetXY(10,24);
		EditLine(Filename, 0, 60, &cbFilename, &b1, BLACK|BGCYAN);
		SetXY(0,0);
	}
	else
	{
	    b1=0x0d;
	    strncpy(Filename, name, 13);
	    cbFilename = strlen(Filename);
	}
	if ((b1==0x0d) && (cbFilename)) {
		erc = OpenFile(Filename, cbFilename, ModeModify, 1, &fh);
		if (!erc) {
			GetFileSize(fh, &filesize);
			if (filesize < 131000)  /* Buf is 131071 */
			{
				erc = ReadBytes (fh, pBuf1, filesize, &dret);
				if (erc > 1)
					erc = CheckErc(2, erc);
				else erc = 0;
			    pEdit->oBufLast	= dret;      /* offset+1 of last char  */
				pBuf1[pEdit->oBufLast] = 0x0F;		/* the SUN */
			}
			else
			{
			  	CloseFile(fh);
				fh = 0;
				Beep();
				SetXY(50, 24);
			    TTYOut("File is too large to edit.", 26, BLACK|BGCYAN);
				ReadKbd(&keycode, 1);
			}
		}
		else if (erc == 203) {	/* no such file */
			Beep();
			SetXY(50, 24);
		    TTYOut("Doesn't exist. Create?? (Y/N)", 29, BLACK|BGCYAN);
			ReadKbd(&keycode, 1);
			if (((keycode & 0xff) == 'Y') || ((keycode & 0xff) == 'y')) {
				erc = CheckErc(4, CreateFile(Filename, cbFilename, 0));

				if (!erc)
					erc = CheckErc(1, OpenFile(Filename, cbFilename,
									ModeModify, 1, &fh));

				if (erc) {
					fh = 0;
					cbFilename = 0;
				}

			}
			else {
				cbFilename = 0;
				ClearStatus();
			}
		}
		else
			CheckErc(1, erc);
	}

	if (!erc)
		ClearStatus();
}

/************************************************************
   This counts ABSOLUTE LINES from the begining of the buffer
   up to point of oBufLine0 (which is the first char displayed
   in the window. ABSOLUTE means LFs were found, even though
   we word wrap always.
*************************************************************/

unsigned long CountEols (void)
{
unsigned long  nEols, i;
unsigned char  *pBuff;

  pBuff = pEdit->pBuf;
  nEols = 0;
  i = 0;

  while (i < pEdit->oBufLine0) /* count LFs */
	if (pBuff[i++] == 0x0A)
		nEols++;

  return(nEols);
}



/************************************************************
   This returns the index to the the last character in a line
   upto	a maximum of sLine-1. iBuf points to the beginning
   point in the buffer to find the end of line for.
*************************************************************/

unsigned long findEol (unsigned long iBuf)
{

unsigned long  iEol, iEolMax;
unsigned char  *pBuff;

  pBuff = pEdit->pBuf;

	/* Calculate the most it could be */

  iEolMax = iBuf + pEdit->sLine-1;

	/* Fix it if EOL is past end of data */

  if (iEolMax > pEdit->oBufLast)
     iEolMax = pEdit->oBufLast;

  iEol = iBuf;
  while ((pBuff[iEol] != 0x0A) && (iEol < iEolMax)) /* Find CR */
    iEol++;
  if ((iEol == iEolMax) && (pBuff[iEol] != 0x0A)) {  /* if no CR... */
    iEol = iEolMax;
    if (iEolMax < pEdit->oBufLast) {

      /* now work back to last space */
      while ((pBuff[iEol] != pEdit->bSpace) && (iEol > iBuf))
         iEol--;

      /*  now find first non-space - allows */
      if ((iEol > iBuf) &&
          (pBuff[iEol] == pEdit->bSpace) &&  /* wrap-around w/ double space */
          (iEol == iEolMax)) {

        if ((pBuff[iEol-1] == pEdit->bSpace) ||
            ((iEol == iEolMax) && (pBuff[iEol+1] == pEdit->bSpace))) {
        	while ((pBuff[iEol] == pEdit->bSpace) && (iEol > iBuf))
            	iEol--;
          	while ((pBuff[iEol] != pEdit->bSpace) && (iEol > iBuf))
            	iEol--;
        }
	  }
      if ((iEol == iBuf) &&
          (pBuff[iBuf] > 0) &&
          (pBuff[iEolMax] > 0))        /* handles "all-char" of full line */
        iEol = iEolMax;
    }
  }
  return(iEol);
}

/************************************************************
   This walks back through the buffer looking for the
   logical end of a line.
*************************************************************/


unsigned long findPrevLine (unsigned long oBufStart)
{
unsigned long  i, j;
char *pBuff;

  pBuff = pEdit->pBuf;

  i = 0;
  if (oBufStart)
  	 i = oBufStart - 1;
  while ((i) && (pBuff[i] != 0x0A))
  	 i--;
  if (i > 0)
  	i--;
  while ((i > 0) && (pBuff[i] != 0x0A))
  	 i--;
  if (i)
  	i++;			/*  Get to known start of line */
  do {
  	j = i;
  	i = (findEol (j)) + 1;
  }
  while (i < oBufStart);
  return(j);
}

/************************************************************
   This executes the BEGIN BLOCK (Mark) command.
*************************************************************/


void doMark (unsigned long iLn)
{
unsigned long  iColStart, iColFinish, iMarkLoc, iBoundLoc;

  if (pEdit->oBufMark < EMPTY) {
      if (pEdit->oBufMark <= pEdit->oBufBound) {
      iMarkLoc = pEdit->oBufMark;
      iBoundLoc = pEdit->oBufBound;
	}
	else {
      iMarkLoc = pEdit->oBufBound;
      iBoundLoc = pEdit->oBufMark;
	}
    if ( ((iMarkLoc >= pEdit->Line[iLn]) && (iMarkLoc < pEdit->Line[iLn+1]))   ||
         ((iBoundLoc >= pEdit->Line[iLn]) && (iBoundLoc < pEdit->Line[iLn+1])) ||
         ((iMarkLoc < pEdit->Line[iLn]) && (iBoundLoc >= pEdit->Line[iLn+1])) )
	{
      if (iMarkLoc >= pEdit->Line[iLn])
        iColStart = pEdit->iColMin + iMarkLoc - pEdit->Line[iLn];
      else
        iColStart = pEdit->iColMin;

      if (iBoundLoc < pEdit->Line[iLn+1])
         iColFinish = pEdit->iColMin + iBoundLoc - pEdit->Line[iLn];
      else
         iColFinish = pEdit->iColMin + pEdit->Line[iLn+1] - pEdit->Line[iLn] - 1;

      if (iColStart > pEdit->iColMin)
          PutVidAttrs (pEdit->iColMin,
          			iLn,
          			iColStart-pEdit->iColMin,
          			pEdit->iAttrNorm);
      PutVidAttrs (iColStart, iLn,	iColFinish - iColStart +1, pEdit->iAttrMark);

      if (iColFinish < pEdit->iColMax)
        PutVidAttrs (iColFinish+1,
        		  iLn,
        		  pEdit->iColMax - iColFinish,
        		  pEdit->iAttrNorm);
    }
    else   /*buf col*/
      PutVidAttrs (pEdit->iColMin,
      			iLn,
      			pEdit->sLine,
      			pEdit->iAttrNorm);
  }
}


/************************************************************
   This inserts data into the main editing buffer.
*************************************************************/


char putInBuf(	unsigned char bPutIn,
				char fOvertype,
                char fSpecInsert)
{
unsigned long  cb;
char  fOK, *pBuff;

	fModified = 1;
	pBuff = pEdit->pBuf;

  if ((pEdit->oBufInsert < pEdit->iBufMax) &&
    ((pEdit->oBufLast < pEdit->iBufMax) ||
     ((fOvertype) && (!fSpecInsert))))
  {
    fOK = 1;
    if ((fOvertype) && (!fSpecInsert)) {
      pBuff[pEdit->oBufInsert] = bPutIn;
      if (pEdit->oBufLast == pEdit->oBufInsert)
         pEdit->oBufLast++;
      pEdit->oBufInsert++;
    }
    else {
      cb = pEdit->oBufLast - pEdit->oBufInsert + 1;
      CopyData (&pBuff[pEdit->oBufInsert], pEdit->pBufWork, cb);
      pBuff[pEdit->oBufInsert] = bPutIn;
      CopyData (pEdit->pBufWork, &pBuff[pEdit->oBufInsert+1], cb);
      pEdit->oBufLast++;
      pEdit->oBufInsert++;
      if (pEdit->oBufMark < EMPTY) {
        if (pEdit->oBufInsert-1 < pEdit->oBufMark)
          pEdit->oBufMark++;
        if (pEdit->oBufInsert-1 <= pEdit->oBufBound)
          pEdit->oBufBound++;
      }
    }
  }
  else {
  	fOK = 0;
    Beep();
    erc = 40400;
  }
  return (fOK);
}

/************************************************************
   This executes the MOVE command which moves a marked
   block to the cursor's current location in the file.
*************************************************************/

void moveData (void)
{
unsigned long  i, iMk, iBd;
char *pBuff, *pBuffWork;

	pBuff = pEdit->pBuf;
	pBuffWork = pEdit->pBufWork;

if (pEdit->oBufMark < EMPTY) {
	fModified = 1;
	if (pEdit->oBufMark <= pEdit->oBufBound) {
	    iMk = pEdit->oBufMark;
	    iBd = pEdit->oBufBound;
	}
	else {
	    iBd = pEdit->oBufMark;
	    iMk = pEdit->oBufBound;
	}
	if ((pEdit->oBufInsert < iMk) || (pEdit->oBufInsert > iBd)) {
	  	for (i=0; i <= pEdit->oBufLast; i++)
    		pBuffWork[i] = pBuff[i];
	    if (pEdit->oBufInsert < iMk) {
    	  for (i=0; i<=iBd-iMk; i++)                /* Move mk/bd */
        	 pBuff[pEdit->oBufInsert+i] = pBuffWork[iMk+i];
      	  for (i=0; i<=iMk - pEdit->oBufInsert - 1; i++) /* Shift overwritten ahead */
       	 	pBuff[pEdit->oBufInsert+iBd-iMk+1+i] =
        	pBuffWork[pEdit->oBufInsert+i];
    	}
    	if (pEdit->oBufInsert > iBd) {
      		for (i=0; pEdit->oBufInsert - iBd - 1; i++)
      	  		pBuff[iMk+i] = pBuffWork[iBd+1+i];
     	 	pEdit->oBufInsert = pEdit->oBufInsert - iBd + iMk - 1;
     	 	for (i=0; i <=iBd-iMk; i++)
     	   		pBuff[pEdit->oBufInsert+i] = pBuffWork[iMk+i];
    	}
    	iBd = pEdit->oBufInsert + iBd - iMk;
    	iMk = pEdit->oBufInsert;
    	if (pEdit->oBufBound > pEdit->oBufMark) {
      		pEdit->oBufBound = iBd;
      		pEdit->oBufMark = iMk;
    	}
    	else {
      		pEdit->oBufMark = iBd;
     	 	pEdit->oBufBound = iMk;
    	}
	}
}
 else Beep();
}

/************************************************************
   This executes the COPY command which copies a marked
   block to the cursor's current location in the file.
*************************************************************/

void CopyIt (void)
{
unsigned long iMk, iBd;
char *pBuff, *pBuffWork;

	pBuff = pEdit->pBuf;
	pBuffWork = pEdit->pBufWork;

if (pEdit->oBufMark < EMPTY) {
	fModified = 1;

  if (pEdit->oBufMark <= pEdit->oBufBound) {
    iMk = pEdit->oBufMark;
    iBd = pEdit->oBufBound;
  } else {
    iBd = pEdit->oBufMark;
    iMk = pEdit->oBufBound;
  }
  if (pEdit->oBufLast+iBd-iMk+1 < pEdit->iBufMax) {
	CopyData(pBuff, pBuffWork, pEdit->oBufLast+1);
	CopyData(&pBuffWork[iMk], &pBuff[pEdit->oBufInsert], iBd-iMk+1);
    if (pEdit->oBufLast >= pEdit->oBufInsert)
    	CopyData(&pBuffWork[pEdit->oBufInsert],
                &pBuff[pEdit->oBufInsert+iBd-iMk+1],
                pEdit->oBufLast - pEdit->oBufInsert+1);
    iBd = pEdit->oBufInsert + iBd - iMk;
    iMk = pEdit->oBufInsert;
    pEdit->oBufInsert = pEdit->oBufInsert + iBd - iMk + 1;
    pEdit->oBufLast = pEdit->oBufLast + iBd - iMk + 1;
    if (pEdit->oBufBound > pEdit->oBufMark) {
      pEdit->oBufBound = iBd;
      pEdit->oBufMark = iMk;
    }
    else {
      pEdit->oBufMark = iBd;
      pEdit->oBufBound = iMk;
    }
  }
}
else Beep();
}

/************************************************************
  This sets the characters on the screen to normal attributes
*************************************************************/

void normAttr (void)
{
unsigned long  i;

  for (i = pEdit->iRowMin; i <= pEdit->iRowMax; i++)
     PutVidAttrs (pEdit->iColMin, i, pEdit->sLine, pEdit->iAttrNorm);
}

/************************************************************
   This unmarks a selected block. (hides it).
*************************************************************/

void nullMarkBound (void)
{
  pEdit->oBufMark = EMPTY;
  pEdit->oBufBound = EMPTY;
  normAttr ();
}

/************************************************************
   This DELETES a selected block.
*************************************************************/

void deleteData (void)
{
unsigned long  i, iMk, iBd;
char  fProb;
char *pBuff, *pBuffWork;

  pBuff = pEdit->pBuf;
  pBuffWork = pEdit->pBufWork;

if (pEdit->oBufMark < EMPTY) {
  fModified = 1;
  if (pEdit->oBufMark <= pEdit->oBufBound)  {
    iMk = pEdit->oBufMark;
    iBd = pEdit->oBufBound;
  } else {
    iBd = pEdit->oBufMark;
    iMk = pEdit->oBufBound;
  }
  if ((pEdit->oBufLine0 >= iMk) && (pEdit->oBufLine0 <= iBd))
     fProb = TRUE;
  else fProb = FALSE;
  CopyData(&pBuff[iBd+1], &pBuff[iMk], pEdit->oBufLast-iBd);
  pEdit->oBufLast = pEdit->oBufLast - iBd + iMk - 1;
  if (pEdit->oBufInsert > iBd)
  	  pEdit->oBufInsert = pEdit->oBufInsert - iBd + iMk;
  else if ((pEdit->oBufInsert > iMk) && (pEdit->oBufInsert <= iBd))
      pEdit->oBufInsert = iMk;
  if (pEdit->oBufInsert > pEdit->oBufLast)
     pEdit->oBufInsert = pEdit->oBufLast;
  if (fProb)  {
    i = findPrevLine (pEdit->oBufInsert);
    pEdit->oBufLine0 = i;
  }
  nullMarkBound ();
}
}

/************************************************************
   After screen movement (such as scrolling), this
   finds the proper location of the cursor in relationship
   to the portion of the file currently displayed.
*************************************************************/

void findCursor (void)
{
	/* locates cursor based on oBufInsert
	   - might be off screen - if it is, this
       will adjust screen */

unsigned long  i, j;

  i = pEdit->iRowMin;
  while ((i <= pEdit->iRowMax) && (pEdit->oBufInsert >= pEdit->Line[i]))
       i++;
  pEdit->iLine = i - 1;
  if (pEdit->iLine < pEdit->iRowMin)
     pEdit->iLine = pEdit->iRowMin;

  j = pEdit->iLine;
  if ((pEdit->Line[j+1] < EMPTY) &&
      (pEdit->oBufInsert >= pEdit->Line[j+1]))
     	pEdit->iLine = pEdit->iLine + 1;
  j = pEdit->iLine;
  pEdit->iCol = pEdit->oBufInsert - pEdit->Line[j] + pEdit->iColMin;
  if (pEdit->iLine > pEdit->iRowMax + 1)
     pEdit->iLine = pEdit->iRowMax;
}

/************************************************************
   This readjusts iCol & iLine to get them back in sync with
   oBufInsert if oBufInsert is on screen.  If oBufInsert is
   not onscreen, this makes it so it is!
*************************************************************/

void coordCursor_oBuf (void)
{
unsigned long  oBuf, i;

  i = pEdit->iRowMax+1;
  if ((pEdit->oBufInsert >= pEdit->oBufLine0) &&
      (pEdit->oBufInsert < pEdit->Line[i]))  {

    /* if bogus line, guarantee end of good */

	i = pEdit->iLine;
    if (pEdit->Line[i] == EMPTY)
       pEdit->iCol = pEdit->iColMax;

     /* if bogus line, find last good line */

    while ((pEdit->Line[i] == EMPTY) &&
           (i > pEdit->iRowMin)) {
        pEdit->iLine--;
		i = pEdit->iLine;
	}

	i = pEdit->iLine;
    pEdit->oBufInsert =
    	pEdit->Line[i] + pEdit->iCol - pEdit->iColMin;
    if (pEdit->oBufInsert > pEdit->oBufLast)
       pEdit->oBufInsert = pEdit->oBufLast;
    oBuf = pEdit->Line[i+1];
    if (pEdit->oBufInsert > oBuf)
       pEdit->oBufInsert = oBuf;   /* get to potential insert - is, if
                                      	   prev char <> CR, is not if prev = CR */
    if (pEdit->oBufInsert == oBuf)            /* if at EOL */
       if (pEdit->pBuf[oBuf-1] == 0x0A)
         pEdit->oBufInsert--;
    pEdit->iCol = pEdit->oBufInsert + pEdit->iColMin -
    			  pEdit->Line[i];
  }
}


/************************************************************
 Adjusts oBufLine0 to make sure that oBufInsert is on the screen.
 This also sets all of the Line array values (Line[n])
*************************************************************/

void makeOnScreen (void)
{
unsigned long  i, j, k;

  /* If oBufInsert is not on screen (above current display)
     then find the previous line beginning and make that
     the new first line 1 until it is!
  */

  while (pEdit->oBufInsert < pEdit->oBufLine0)
  	pEdit->oBufLine0 = findPrevLine (pEdit->oBufLine0);

  /* Set Line[iRowMin] to match oBufLine0 */

  k = pEdit->iRowMin;
  pEdit->Line[k] = pEdit->oBufLine0;

  /* Set all subsequent Line[s] by calling findEol for each. */

  for (i = k; i <= pEdit->iRowMax; i++) {
    if (pEdit->Line[i] < EMPTY) {
      j = findEol (pEdit->Line[i]);
      if (j < pEdit->oBufLast)       /* j = offset of last char of line */
        pEdit->Line[i+1] = j + 1;
      else
      	for (j=i+1; j<NLINESMAX; j++)
          		pEdit->Line[j] = EMPTY;
  	}
  }

	/* If the InsertPoint (your cursor position) is past
	the last line then do this junk to fix it */

  j = pEdit->iRowMin;
  k = pEdit->iRowMax;
  while (pEdit->oBufInsert >= pEdit->Line[k+1]) {
    for (i=j; i<=k; i++)
      pEdit->Line[i] = pEdit->Line[i+1];
    pEdit->oBufLine0 = pEdit->Line[j];

    i = findEol (pEdit->Line[k]);		  /* EOL of iRowMax */
    if (i < pEdit->oBufLast)              /* i = offset of last char of line */
    	pEdit->Line[k+1] = i + 1;
    else pEdit->Line[k+1] = EMPTY;
  }
}

/************************************************************
 Redisplay all data on the screen.
*************************************************************/

void showScreen (char *pFiller)
{
unsigned long i, iLn, cb, oBuf;
char *pBuff;

	pBuff = pEdit->pBuf;

	makeOnScreen ();    /* oBufInsert on screen - Line correct */

	for (iLn = pEdit->iRowMin; iLn <= pEdit->iRowMax; iLn++) {

		/* i = offset in buf of last char on line */
		/* cb = nchars in line */

		cb = 0;
		oBuf = pEdit->Line[iLn];
		if (oBuf < EMPTY) {
			if (pEdit->Line[iLn+1] < EMPTY)
				i = pEdit->Line[iLn+1] - 1;
			else
	    	  	i = pEdit->oBufLast;
		    cb = i - oBuf + 1;      	/* Make size, not offset */


		    if ((!pEdit->fVisible) &&
		    	(pBuff[i] == 0x0A) &&
		    	(cb))
		    	   	cb--;
		}

		if ((cb) && (oBuf < EMPTY))
		  	PutVidChars (pEdit->iColMin, iLn, pBuff+oBuf, cb,
					  pEdit->iAttrNorm);

		if (cb < pEdit->sLine)
		  	PutVidChars (pEdit->iColMin+cb, iLn, pFiller, pEdit->sLine-cb,
		  			  pEdit->iAttrNorm);

		doMark (iLn);
	}
}

/************************************************************
 Resets all variables for the editor and clears the
 buffers. Called before use and after closure.
*************************************************************/

void clearbuf (void)
{
unsigned long i;
char *pBuff;

	pBuff = pEdit->pBuf;
	FillData(filler, 80, 0x20);
	for (i=0; i<NLINESMAX; i++)
       pEdit->Line[i] = EMPTY;
	i = pEdit->iRowMin;
	pEdit->Line[i] = 0;

    pEdit->iCol = pEdit->iColMin;
    pEdit->iLine = pEdit->iRowMin;
  	pEdit->bSpace = 0x20;
  	pEdit->fVisible = FALSE;
	fModified = 0;
	fOvertype = FALSE;
	pEdit->oBufLast	 = 0;
    pEdit->oBufInsert = 0;
    pEdit->oBufLine0 = 0;
	pBuff[pEdit->oBufLast] = 0x0F;		/* the SUN */
	nullMarkBound();
	normAttr ();
}

/************************************************************
 This is the main editing function. It is a HUGE while
 loop which reads keystrokes and processes them.
*************************************************************/

void Editor(char *pbExitRet)
{
unsigned long  i, j, k, key;
char  fSpecInsert;     /* TRUE = insert no matter what fOvertype is */
char  fScreen;   	   /* TRUE = display entire screen */
char  fDone;
unsigned char b;
char *pBuff, *pBuffWork;
long exch;

char testkey[10];

  pBuff = pEdit->pBuf;
  pBuffWork = pEdit->pBufWork;
  fDone = FALSE;

  if (pEdit->fVisible)
  {
    pEdit->bSpace = 0x07;
    for (i=0; i <=pEdit->oBufLast; i++)
      if (pBuff[i] == 0x20)
         pBuff[i] = pEdit->bSpace;
  } else
  	pEdit->bSpace = 0x20;


  normAttr ();
  fScreen = TRUE;

  pBuff[pEdit->oBufLast] = 0x0F;		/* the SUN */

  erc = AllocExch(&exch);

  while (!fDone)
  {
    if (fScreen)
    {
      showScreen (filler);    /* we know oBufInsert on screen */
      findCursor ();
      fScreen = FALSE;
    }
    SetXY (pEdit->iCol, pEdit->iLine);
	FillData(aStat, 80, 0x20);
	i= CountEols() + pEdit->iLine;
	sprintf(aStat,"C: %02d  L: %05d  nChars: %05d",
			pEdit->iCol, i, pEdit->oBufLast);
	if (cbFilename)
		CopyData(Filename, &aStat[40], cbFilename);
	if (fOvertype)
		CopyData("OVR", &aStat[77], 3);
	else
		CopyData("INS", &aStat[77], 3);
    PutVidChars (0, 0, aStat, 80, STATVID);

    fSpecInsert = FALSE;  /* True if char should be inserted even if fOver */

    CheckErc(7, ReadKbd (&key, 1));	  /* Wait for char */

    b = key & 0xff;

	if (key & 0x3000) 		/* ALT key is down */
	{
		switch (b)
		{

      		case 0x42:		/* ALT-B -- Beginning of Text */
      		case 0x62:		/* ALT-b  */
                   pEdit->oBufLine0 = 0;
                   pEdit->oBufInsert = 0;
                   pEdit->iCol = pEdit->iColMin;
                   pEdit->iLine = pEdit->iRowMin;
                   fScreen = TRUE;
				 break;

      		case 0x43:		/* ALT-C -- CloseFile */
      		case 0x63:		/* ALT-c  */
                SaveFile(TRUE, TRUE);
		        fScreen = TRUE;
				break;
      		case 0x45:		/* ALT-E -- End of Text */
      		case 0x65:		/* ALT-e  */
                   pEdit->oBufInsert = pEdit->oBufLast;
                   coordCursor_oBuf ();
				   fScreen = TRUE;
				 break;
      		case 0x01:		/* ALT-UP Arrow - Cursor to top of screen */
                   pEdit->iLine = pEdit->iRowMin;
                   pEdit->iCol = pEdit->iColMin;
				 break;
      		case 0x02:	/* ALT Down Arrow - Cursor to bottom of screen */
                   pEdit->iLine = pEdit->iRowMin;
				   i = pEdit->iLine;
                   while ((pEdit->Line[i+1] < EMPTY) &&
                          (i < pEdit->iRowMax)) {
                       pEdit->iLine++;
					   i = pEdit->iLine;
				   }
                   pEdit->iCol = pEdit->iColMax;
                   coordCursor_oBuf ();
				 break;
      		case 0x03:		/* ALT-Left Arrow - Cursor to BOL */
                   pEdit->iCol = pEdit->iColMin;
				 break;
      		case 0x04:		/* ALT-Right Arrow - Cursor to EOL */
				   i = pEdit->iLine;
                   if (pEdit->Line[i+1] < EMPTY) {
                      pEdit->iCol =
                      	pEdit->iColMin +
                      	pEdit->Line[i+1] -
                      	pEdit->Line[i];
                      	i = pEdit->iLine+1;
                      if ((pBuff[pEdit->Line[i]-1] == 0x0A)
                         && (pEdit->iCol > pEdit->iColMin))
                       pEdit->iCol--;
                   }
                   else pEdit->iCol = pEdit->iColMin +
                   		pEdit->oBufLast - pEdit->Line[i];
				 break;

      		case 0x56:		/* ALT-V   Make nontext chars Visible/Invisible */
      		case 0x76:		/* ALT-v  */
                   if (pEdit->fVisible)  {
                     for (i=0; i <= pEdit->oBufLast; i++)
                          if (pBuff[i] == 0x07)
                          	pBuff[i] = 0x20;
                     pEdit->fVisible = FALSE;
                     pEdit->bSpace = 0x20;
                   } else {
                     for (i=0; i<=pEdit->oBufLast; i++)
                          if (pBuff[i] == 0x20)
                          	pBuff[i] = 0x07;
                     pEdit->fVisible = TRUE;
                     pEdit->bSpace = 0x07;
                   }
                   fScreen = TRUE;
				break;
      		case 0x4F:		/* ALT-O   OpenFile */
      		case 0x6F:		/* ALT-o  */
				if (!fh) {
					clearbuf();
					OpenAFile(0);
                    fScreen = TRUE;
				}
				break;
      		case 0x51:	/* ALT Q - Quit */
      		case 0x71:
                SaveFile(TRUE, TRUE);
                fDone = TRUE;
				break;
      		case 0x53:		/* ALT S - SaveFile */
      		case 0x73:		/* ALT s  */
                SaveFile(FALSE, FALSE);
		        fScreen = TRUE;
				break;
      		case 0x7F:		/* ALT Delete (Delete Marked Block)  */
      			if (pEdit->oBufMark < EMPTY)  {
                   deleteData ();
                   fScreen = TRUE;
                 }
				 break;
			default:
				break;
		}
	}

	else if (key & 0x0300) 		/* CTRL key is down */
	{


	}

	/* Standard editing keys including LF */

	else if (((b >= 0x20) && (b <= 0x7E)) || (b == 0x0D))
	{
         	coordCursor_oBuf ();
			if (b == 0x20)
            	b = pEdit->bSpace;
			if (b == 0x0D)
            	b = 0x0A;
		             /* Don't overwrite CR */
            if (pBuff[pEdit->oBufInsert] == 0x0A)
              fSpecInsert = TRUE;
            if (!(putInBuf (b, fOvertype, fSpecInsert)))
            	Beep();
       		findCursor ();
            fScreen = TRUE;

	}

	else if (key & 0x0C00) {	/* SHIFT key is down & NOT letter keys */
		switch (b)
		{
   	  		case 0x03:	/* SHIFT Left */
           	     if (pEdit->iCol > pEdit->iColMin + 5)
               	    	pEdit->iCol -= 5;
                 else
                 	if (pEdit->iCol > pEdit->iColMin)
                 		pEdit->iCol--;
				 break;
   	  		case 0x04:	/* SHIFT Right */
           	     if (pEdit->iCol < pEdit->iColMax - 5)
               	    	pEdit->iCol += 5;
                 else
                 	if (pEdit->iCol < pEdit->iColMax)
                 		pEdit->iCol++;
				 break;
			 default:
			 	break;
	 	}
    }

	else {			/* Unshifted editing keys */
		switch (b) {
      		case 0x08:	/* Backspace */
      			if (pEdit->oBufLast)  {
                   coordCursor_oBuf ();
                   if (pEdit->oBufInsert)  {
                     pEdit->oBufInsert = pEdit->oBufInsert - 1;
                     if (!fOvertype)  {
                       CopyData(pBuff,
                       			pBuffWork,
                       			pEdit->oBufLast+1);
                       CopyData(&pBuffWork[pEdit->oBufInsert+1],
                         		&pBuff[pEdit->oBufInsert],
                         		pEdit->oBufLast-pEdit->oBufInsert);
                       pBuff[pEdit->oBufLast] = 0;
                       pEdit->oBufLast = pEdit->oBufLast - 1;
                       if ((pEdit->oBufMark == pEdit->oBufBound) &&
                           (pEdit->oBufMark == pEdit->oBufInsert))
                          nullMarkBound ();
                       if (pEdit->oBufMark < EMPTY)  {
                         if (pEdit->oBufInsert <= pEdit->oBufMark)
                            pEdit->oBufMark--;
                         if (pEdit->oBufInsert <= pEdit->oBufBound)
                            pEdit->oBufBound--;
                       }
                     }
                   }
                   if (pEdit->oBufInsert < pEdit->oBufLine0)
                      pEdit->oBufLine0 = findPrevLine (pEdit->oBufLine0);
                   fScreen = TRUE;
				   fModified = TRUE;
                }
				break;
      		case 0x06:		/* Home - Cursor to BOL */
                pEdit->iCol = pEdit->iColMin;
				break;
      		case 0x09:		/* Tab */
      			if (pEdit->oBufLast + pEdit->iTabNorm < pEdit->iBufMax)
      			{
                   coordCursor_oBuf();
                   j = pEdit->iTabNorm - (pEdit->iCol % pEdit->iTabNorm);
                   for (i=1; i <=j; i++)
                        putInBuf (pEdit->bSpace, FALSE, FALSE);
                   fScreen = TRUE;
                }
				break;
      		case 0x10:   /* F2 -- UNMARK BLOCK */
      				nullMarkBound ();
				 break;
      		case 0x11:		/* F3 -- Begin Block */
      			 if (pEdit->oBufLast > 0)  {
                   coordCursor_oBuf ();
                   pEdit->oBufMark = pEdit->oBufInsert;
                   i = pEdit->iLine;
                   if (pEdit->oBufMark >= pEdit->Line[i+1])
                      pEdit->oBufMark = pEdit->oBufMark - 1;
                   if (pEdit->oBufMark == pEdit->oBufLast)
                      pEdit->oBufMark = pEdit->oBufLast - 1;
                   pEdit->oBufBound = pEdit->oBufMark;
                   fScreen = TRUE;
                 }
                 break;
      		case 0x12:		/* F4 -- End Block */
      			if (pEdit->oBufMark < EMPTY)  {
                   coordCursor_oBuf ();
                   pEdit->oBufBound = pEdit->oBufInsert;
                   i = pEdit->iLine;
                   if (pEdit->oBufBound >= pEdit->Line[i+1])
                      pEdit->oBufBound--;
                   if (pEdit->oBufBound == pEdit->oBufLast)
                      pEdit->oBufBound = pEdit->oBufLast - 1;
                   fScreen = TRUE;
                 }
                 break;
      		case 0x17:	/* F9 - MOVE */
                   coordCursor_oBuf ();
                   moveData ();
                   if (pEdit->oBufInsert < pEdit->oBufLine0)
                      pEdit->oBufLine0 = pEdit->oBufInsert;
                   fScreen = TRUE;
				 break;
      		case 0x18:	/* F10 - COPY */
                   coordCursor_oBuf ();
                   CopyIt ();
                   coordCursor_oBuf ();
                   fScreen = TRUE;
				 break;
      		case 0x0C:	/* Page Down */
                   coordCursor_oBuf ();
                   i = pEdit->iRowMax;
                   while ((pEdit->Line[i] == EMPTY) && (i > pEdit->iRowMin))
                        i--;
                   pEdit->oBufLine0 = pEdit->Line[i];
                   		 /*always keep onScreen*/
                   if (pEdit->oBufInsert < pEdit->oBufLine0)
                      pEdit->oBufInsert = pEdit->oBufLine0;
                   pEdit->iLine = pEdit->iRowMin;
                   pEdit->iCol = pEdit->iColMin;
                   fScreen = TRUE;
				 break;
      		case 0x05:	/* Page Up */
      			 if (pEdit->oBufLine0)  {
                   coordCursor_oBuf ();
                   j = pEdit->iRowMax - pEdit->iRowMin;
                   i = pEdit->oBufLine0;
                   k = pEdit->iLine;   /*fix for scrolling when iLine=iRowMax */
                   do {
                     i = findPrevLine (i);
                     j--;
                     k--;
				   }
                   while ((j > 0) && (i > 0));
                   pEdit->oBufLine0 = i;
	                   /*fix for scroll when iLine=iRowMax*/
                   if (pEdit->iLine == pEdit->iRowMax)
                      pEdit->oBufInsert = pEdit->Line[k];
	                     /*keep on screen*/
				   i = pEdit->iRowMax;
                   if (pEdit->oBufInsert >= pEdit->Line[i+1])
                      pEdit->oBufInsert = pEdit->Line[i];
                   fScreen = TRUE;
                 }
				 break;
      		case 0x01:	/* Up */
      			if (pEdit->iLine > pEdit->iRowMin)  {
                   pEdit->iLine--;
                 } else {		/* scroll screen down if we can */
					i = pEdit->oBufLine0;
      				if (i > 0)  {
                  		i = findPrevLine (i);
                   		pEdit->oBufLine0 = i;
                    	pEdit->oBufInsert = i;
                   		fScreen = TRUE;
				    }
                 }
				 break;
      		case 0x02:	/* Down */
      			i = pEdit->iLine;
      			if ((pEdit->Line[i+1] < EMPTY) &&   /*Down Arrow*/
                 	(i < pEdit->iRowMax))  {
                   pEdit->iLine++;
                }
				else {			/* ELSE scroll screen UP if we can */
				  	i = pEdit->iRowMax;
      			  	if (pEdit->Line[i+1] < EMPTY)  {
                   		pEdit->oBufInsert = pEdit->Line[i+1];
						i = pEdit->iCol;
						j = pEdit->iLine;
						coordCursor_oBuf ();
						pEdit->iCol = i;
						pEdit->iLine = j;
                   		fScreen = TRUE;
				  	}
                }
				break;
      		case 0x03:	/* Left */
      			if (pEdit->iCol > pEdit->iColMin)  {          /*Left Arrow*/
                   pEdit->iCol--;
                 }
				 break;
      		case 0x04:	/* Right */
      			if (pEdit->iCol < pEdit->iColMax)  {
                   pEdit->iCol++;
                 }
				 break;
      		case 0x0E:	/* Insert */
                   if (fOvertype)
                      fOvertype = FALSE;
                     else fOvertype = TRUE;
				 break;
      		case 0x7F:	/* Delete */
                   coordCursor_oBuf ();
                   if ((pEdit->oBufLast) &&
                       (pEdit->oBufLast > pEdit->oBufInsert))  {
                     CopyData(pBuff,
                              pBuffWork,
                              pEdit->oBufLast+1);
                     CopyData(&pBuffWork[pEdit->oBufInsert+1],
                              &pBuff[pEdit->oBufInsert],
                              pEdit->oBufLast-pEdit->oBufInsert);
                     pBuff[pEdit->oBufLast] = 0;
                     pEdit->oBufLast--;
                     if ((pEdit->oBufInsert == pEdit->oBufMark) &&
                         (pEdit->oBufMark == pEdit->oBufBound))
                        nullMarkBound ();
                     if (pEdit->oBufMark < EMPTY)  {
                       if (pEdit->oBufInsert < pEdit->oBufMark)
                          pEdit->oBufMark--;
                       if (pEdit->oBufInsert < pEdit->oBufBound)
                          pEdit->oBufBound--;
                       if (pEdit->oBufMark == pEdit->oBufLast)
                          pEdit->oBufMark--;
                       if (pEdit->oBufBound == pEdit->oBufLast)
                          pEdit->oBufBound--;
                     }
                     fScreen = TRUE;
                     fModified = TRUE;
                   }
				   break;
			default:
				break;
		}
	}
  } /* Not fDone */


  for (i=pEdit->iRowMin; i <=pEdit->iRowMax; i++)
    PutVidAttrs (pEdit->iColMin, i, pEdit->sLine+1, 0); /* REM buffer column */

  if (fh) {
	if (pEdit->fVisible)     /* fix visible characters */
	     for (i=0; i <=pEdit->iBufMax; i++)
	         if (pBuff[i] == 0x07)
	         	  pBuff[i] = 0x20;
	pBuff[pEdit->oBufLast] = 0;
	if (fModified) {
			erc = CheckErc(6, SetFileLFA(fh, 0));
			if (!erc)
				erc = CheckErc(5, SetFileSize(fh,pEdit->oBufLast));
			if (!erc)
				erc = CheckErc(3, WriteBytes (fh, pBuf1, pEdit->oBufLast, &i));
			fModified = 0;
	}
  	CloseFile(fh);
	cbFilename = 0;
  }

  *pbExitRet = b;
  return;
}

/************************************************************
 This is the main entry point for the editor. It aloocates
 two buffers of equal size(a main and a working buffer),
 and then checks for a single parameter which should be the
 name of the file to edit.
*************************************************************/

void main(U32 argc, U8 *argv[])
{
long i;

	ClrScr();
	SetJobName("Editor", 6);
	fh = 0;

    pEdit = &EdRec;
	erc = AllocPage(32, &pBuf1);	/* 32 pages = 128K */
    erc = AllocPage(32, &pBuf2);

    pEdit->pBuf			= pBuf1;
    pEdit->pBufWork		= pBuf2;
    pEdit->iBufMax		= 131071;    /*sBuf - 1 */
    pEdit->iColMin  	= 0;        /*Screen coordinates*/
    pEdit->iColMax  	= 79;
    pEdit->iRowMin  	= 1;
    pEdit->iRowMax  	= 23;
    pEdit->sLine    	= 80;       /* iColMax-iColMin+1 */
    pEdit->bSpace   	= 0x20;
    pEdit->fVisible 	= FALSE;
    pEdit->iAttrMark 	= MARKVID;  /* Rev Vid*/
    pEdit->iAttrNorm 	= EDVID;    /* Rev Vid Half Bright    */
    pEdit->iTabNorm  	= 4;        /* Tabs every 4th column  */
    pEdit->oBufLine0 	= 0;        /* oBufLine0 */
    pEdit->iCol      	= 0;        /* cursor, 0..sLine-1  */
    pEdit->iLine     	= 0;        /* cursor, 0..cLines-1    */
    pEdit->oBufInsert   = 0;        /* offset of next char in */
    pEdit->oBufLast 	= 0;        /* offset+1 of last char  */
    pEdit->oBufMark 	= EMPTY;
    pEdit->oBufBound 	= EMPTY;

	SetNormVid(NORMVID);

	FillData(filler, 80, 0x20);
	for (i=0; i<NLINESMAX; i++)
       pEdit->Line[i] = EMPTY;
	i = pEdit->iRowMin;
	pEdit->Line[i] = 0;

	fModified = 0;
	fOvertype = FALSE;    /* Set Overtype OFF */

	if (argc > 1)
	{
		OpenAFile(argv[1]);
	}

	Editor(&b);
	ExitJob(0);
}
