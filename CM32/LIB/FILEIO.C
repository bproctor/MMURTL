/*  fileio.c
* Standard File Operations fopen, fclose, fputc, fputs, fgetc
* CM32 library source for for MMURTL
* Copyright 1994, R.A. Burgess
 **************************************/

#include <string.h>
#include <stdio.h>

#include "C:\OSSOurce\MFiles.h"
#define ErcDupName	226		/* Name exists as a file or dir already */

static long error_num = 0;

/*************** fopen ***************
 opens a file in stream mode for reading,
 or reading and writing (modify mode).
*************************************/

FILE *fopen(char *name, char *mode)
{
long handle, lmode, fcreate, fdiscard, erc, fappend;

	erc = 0;
	handle = 0;
	fappend = 0;
	fcreate = 0;
	fdiscard = 0;

	if (*mode=='r')
		lmode = 0;
	else if (*mode=='w')
	{
		lmode = 1;
		fcreate = 1;
		fdiscard = 1;
	}
	else if (*mode=='a') {
		fappend = 1;
		fcreate = 1;
		lmode = 1;
	}
	else
		return (0);

	/* see if they want to update also */

	if ((mode[1] == '+') || (mode[2] == '+'))
		lmode = 1;

	if (fcreate)		/* try to create with archive bit set */
		erc =  CreateFile(name, strlen(name), 0x20);

	if (erc == ErcDupName)
		erc = 0;

	if (!erc)
		erc =  OpenFile(name, strlen(name), lmode, 1, &handle);
	if (erc)
	{
		error_num = erc;
		handle = 0;
	}
	if ((!erc) && (fappend))
	{
		erc = SetFileLFA(handle, 0xffffffff);		/* EOF */
	}
	if ((!erc) && (fdiscard))
	{
		erc = SetFileSize(handle, 0);		/*  */
	}

	return (handle);
}

/*************** rename ***************/

int rename(char *oldname, char *newname)
{
long  erc;

	erc =  RenameFile(oldname, strlen(oldname), newname, strlen(newname));
	if (erc) {
		error_num = erc;
		return(-1);
	}
	return(0);
}

/*************** remove ***************/

int remove(char *name)
{
long handle, erc;

	erc =  OpenFile(name, strlen(name), 1, 0, &handle);
	if (erc)
	{
		error_num = erc;
		return(-1);
	}
	erc =  DeleteFile(handle);
	if (erc)
	{
		error_num = erc;
		CloseFile(handle);
		return(-1);
	}
	return(0);
}

/*************** fclose ***************/

long fclose(FILE *stream)
{
long erc, i;
erc = CloseFile(stream);
if (erc) {
	error_num = erc;
	i = EOF;
	}
else
	i = 0;
return stream;
}

/********** fgetc ***************************************
*  Read a char and return from function. Return EOF (-1)
*  If EOF. This returns char as an int.
**********************************************************/

long fgetc(FILE *stream)
{
long erc, i, chl;
unsigned char ch;

	erc = ReadBytes (stream, &ch, 1, &i);
	if ((erc) && (!i))		/* RAB */
		return EOF;
	else
		chl = ch;
	return (chl);
}

/******** fgets *****************************************
  Read chars into string (s) until we get a LF or n-1=0.
  Null terminate the string.
**********************************************************/

char *fgets(char *s, long n, FILE *stream)
{
long  ch;
char  *ss, c;
	ss = s;
	while (n > 1)
	{
		ch = fgetc(stream);
		if (ch == EOF)
		{
			*s = 0;		/* null terminate */
			return(0);
		}
		else
		{
			c = ch;
			*s = c;
			n--;
			if (c == 0x0A)
				n = 0;
			s++;
		}
	}
	*s = 0;
	return (ss);
}


/*************** fputc ***************/

/* writes the char (int) in c to the stream.
   No translation is done to the char.
*/

long fputc(long c, FILE *stream)
{
long erc, i, ch;
	erc = WriteBytes (stream, &c, 1, &i);
	if (erc)
		ch = EOF;
	else
		ch = c;
	return (ch);
}

/********************* fputs *************************
 Writes the string pointed to by *s to file *stream
 No EOL translation is done. The terminating NULL is
 not placed in the file.
******************************************************/

long fputs(const char *s, FILE *stream)
{
long erc, i, j, ch;

	erc = 0;
	i = strlen(s);
	if (i)
	{
		erc = WriteBytes (stream, s, i, &j);
	}
	if (erc)
		ch = EOF;
	else
		ch = 1;
	return (ch);
}

/*************** ftell ******************************/
/* returns the file pointer (Logical File Address)  */
/* of stream.                                       */

long ftell(FILE *stream)
{
long erc, i;
	erc = GetFileLFA(stream, &i);
	if (erc)
		i = EOF;
	return (i);
}

/*************** rewind *****************************/
/* Set file LFA to 0.                               */

void rewind(FILE *stream)
{
long erc;
	erc = SetFileLFA(stream, 0);
}

/*************** fseek *****************************/
/* Set file LFA to desired position.               */

void fseek(FILE *stream, long offset, long origin)
{
long erc, crnt;

	if (origin==SEEK_CUR)
		erc = GetFileLFA(stream, &crnt);
	else if (origin==SEEK_END)
	{
		offset = 0;
		crnt = -1;	/* MMURTL Seeks to EOF */
	}
	else if (origin==SEEK_SET)
		crnt = 0;
	else
		return 1;

	erc = SetFileLFA(stream, offset+crnt);
	if (erc)
		return (1);
	else
		return (0);
}

/********** fread ***************************************
*  reads nobjects of nsize from stream and returns them
*  to ptr.
**********************************************************/

long fread(char *ptr, long size, long nobj, FILE *stream)
{
long erc, i, j;

	erc = ReadBytes (stream, ptr, size*nobj, &i);
	if (erc < 2)
		j = i/size;		/* number of objects of size */
	else
		j = 0;			/* nothing! */
	return (j);
}

/********** fwrite ***************************************
*  writes nobjects of nsize to stream from ptr.
**********************************************************/

long fwrite(char *ptr, long size, long nobj, FILE *stream)
{
long erc, i, j;

	erc = WriteBytes (stream, ptr, size*nobj, &i);
	if (!erc)
		j = i/size;		/* number of objects of size */
	else {
		error_num = erc;
		j = 0;			/* nothing! */
	}
	return (j);
}

/******************* End of FileIO.c **************/
