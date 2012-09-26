/* MakeImg.c
   This turns a MMURTL operating system run file
   into an image just as it will appear in memory
   after it is loaded. The MMURTL OS loads at address 0
   and is assembled with that as the virtual address
   so no fixups are required. The data segment is first,
   then there will be padding up to the code segment
   which will begin at address 64K Linear (65536 or FFF:0
   segmented).

   THIS MUST BE COMPILED WITH THE LARGE MODEL (Borland or Turbo C)
   (e.g., BCC -ml MAKEIMG.C)
*/

#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char

#define TRUE 1
#define FALSE 1

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>
#include "runfile.h"

U32 fEOF = 0;

long CSegSize = 0;
long DSegSize = 0;

char *ptest;

long	sStack = 0;
long	sCode = 0;
long	sData = 0;

long	offCode = 0;
long	offData = 0;
long	pStart = 0;

struct tagtype tag;

char osname[50] = {"MMURTL.RUN"};	/* default OS name */

FILE *run_fh = 0;
FILE *out_fh = 0;

char far *pDataBuf1;
char far *pDataBuf2;

char far *pCodeBuf1;
char far *pCodeBuf2;
char far *pCodeBuf3;

/*********************************************************
  This reads the MMURTL run file and writes it to disk
  as an image file as it would have been loaded by
  the MMURTL loader. This is used so we can place this
  on a disk to make it bootable.
  This is a tag/length/value file is described in the
  DASM documentation (in great detail).
*********************************************************/

void MakeImageFile(void)
{
int ii;
long i, dret, nobj, nread;
char filetype, junk, *pin;

	while (1) {
		tag.id = 0;
		nobj = fread (&tag, 1, 5, run_fh);
		if (nobj) {
			ii = tag.id;
		}
		else {
			printf("Invalid OS Run file. No End Tag Found.\r\n");
			fclose(run_fh);
			fclose(out_fh);
			exit(1);
		}

		switch (tag.id) {
			case IDTAG:
				nobj = fread (&filetype, 1, 1, run_fh);
				if (filetype == 1)
					break;
				else {
					printf("File MUST be a run file\r\n");
					exit(1);
				}
				break;
			case SEGTAG:
				nobj = fread (&sStack, 1, 4, run_fh);
				if (nobj) nobj = fread (&sCode, 1, 4, run_fh);
				if (nobj) nobj = fread (&sData, 1, 4, run_fh);
				printf("  Code    %ld\r\n", sCode);
				printf("  Data    %ld\r\n", sData);
				break;
			case DOFFTAG:
				nobj = fread (&offData, 1, 4, run_fh);
				break;
			case COFFTAG:
				nobj = fread (&offCode, 1, 4, run_fh);
				break;
			case STRTTAG:
				nobj = fread (&pStart, 1, 4, run_fh);
				break;
			case CODETAG:
				printf("Reading code:  %ld bytes\r\n", tag.len);
                CSegSize = tag.len;

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;

				pin = pCodeBuf1;
				while (nread--) {
					nobj = fread (pin++, 1, 1, run_fh);
				}

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;

				pin = pCodeBuf2;
				while (nread--) {
					nobj = fread (pin++, 1, 1, run_fh);
				}

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;

				pin = pCodeBuf3;
				while (nread--) {
					nobj = fread (pin++, 1, 1, run_fh);
				}
				break;

			case DATATAG:
				printf("Reading data:  %ld bytes\r\n", tag.len);
                DSegSize = tag.len;
				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;

				pin = pDataBuf1;
				while (nread--) {
					nobj = fread (pin++, 1, 1, run_fh);
				}

				if (tag.len >= 32768)
					nread = 32768;
				else
					nread = tag.len;
				tag.len -= nread;

				pin = pDataBuf2;
				while (nread--) {
					nobj = fread (pin++, 1, 1, run_fh);
				}
				break;
			case CDFIXTAG:
			case CCFIXTAG:
			case DDFIXTAG:
			case DCFIXTAG:
				while ((tag.len) && (nobj)) {
					nobj = fread (&i, 1, 4, run_fh);
					tag.len-=4;
				}
				break;
			case ENDTAG:
				nobj = fread (&i, 1, 4, run_fh); 	/* Eat the end tag */
				fclose(run_fh);
				return;
			default:
				while ((tag.len--) && (nobj))
					nobj = fread (&junk, 1, 1, run_fh);
				break;

		} /* end of switch */
	}
}

/***************************
* Main program -- MMLoader
****************************/

void main(S16 argc, S8   *argv[])
{
S8   *ptr;
S16  i;
char fName;

	out_fh = fopen("MMURTL.IMG", "wb");
	fName = 0;

	for(i=1; i < argc; ++i) {
		ptr = argv[i];
		if (*ptr == '/') {
		  ptr++;
		  switch(*ptr) {
			case 'U' :
			case 'u' :
				break;
			case 'D' :
			case 'd' :
				break;
			case 'O' :
			case 'o' :
				fName = 1;
				break;
			default:
				printf("Invalid option/swtich \n");
				if (run_fh)
					fclose(run_fh);
				exit(1);
				break;
		  }
		}
		else {
			if (fName)
				strncpy(osname, argv[i], 39);
		}
	}

	printf("MMURTL Image File Maker. Ver 1.0 \r\n");

	printf("OS Filename specified: %s \r\n",  osname);
	run_fh = fopen(osname, "rb");
	if (!run_fh) {
		printf("Can't open: %s \r\n",  osname);
		exit(1);
	}

	pDataBuf1 = malloc(32768);	/*  32K */
	pDataBuf2 = malloc(32768);	/*  32K */
	pCodeBuf1 = malloc(32768);	/*  32K */
	pCodeBuf2 = malloc(32768);	/*  32K */
	pCodeBuf3 = malloc(32768);	/*  32K */

	MakeImageFile();

	fwrite(pDataBuf1, 1, 32768, out_fh);  /* Write 32K of the Image */
	fwrite(pDataBuf2, 1, 32768, out_fh);  /* Write 32K of the Image */

	fwrite(pCodeBuf1, 1, 32768, out_fh);  /* Write 32K of the Image */
	fwrite(pCodeBuf2, 1, 32768, out_fh);  /* Write 32K of the Image */
	fwrite(pCodeBuf3, 1, 32768, out_fh);  /* Write 32K of the Image */

	fclose(out_fh);

	printf("\r\nDone.\r\n");


}

/*********************** End of Module *****************/
