/* Runfile.h */

/* MMURTL Operating System Source Code
   Copyright 1991,1992,1993, Richard A. Burgess
   ALL RIGHTS RESERVED
   Version x0.8
*/

/* This contains all the defines to build or read a MMURTL runfile */

#define IDTAG    0x80
#define VERTAG   0x82
#define DATETAG  0x83
#define CMNTTAG  0x84
#define SEGTAG   0x90
#define DOFFTAG  0x92
#define COFFTAG  0x94
#define STRTTAG  0x96
#define DLLIDTAG 0xA0
#define CODETAG  0xB0
#define DATATAG  0xB2
#define CDFIXTAG 0xC0		/* most common (e.g., a variable ref in CSEG) */
#define CCFIXTAG 0xC1		/* CSEG item refers to CSEG item */
#define DDFIXTAG 0xC2		/* DESG item refers to DSEG item */
#define DCFIXTAG 0xC3		/* DESG item refers to CSEG item */
#define DLFIXTAG 0xC5		/* DLL called from CSEG */
#define DLPUBTAG 0xC8		/* DLL Defined in CSEG */
#define ENDTAG   0XFF

/* legal values for run file types */

#define IDRUNFILE 1
#define IDDLLFILE 2
#define IDDEVDRV  3


struct tagtype {
	unsigned char id;
	long len;
	};

#define TAGSIZE 5		/* This includes the TAGID & tag length */

/************ END OF MODULE *****************/
