/* MMURTL Operating System Source Code
   Copyright 1991,1992,1993,1994 Richard A. Burgess
   ALL RIGHTS RESERVED   Version 1.0
*/


/* The colors for TTYOut, PutChars and PutAttrs are made of 16 foreground
   colors, 8 background colors and 1 bit for blinking (all in one byte)
   The High nibble is the background and low is the foreground.
   The high bit of each is the intensity bit, On means HIGH.

	The LOW nibble (foreground) is defined as:

	Normal				w/ Intensity Bit set
	Black   - 0000 (0)	Grey 			- 1000 (8)
	Blue  	- 0001 (1)	Light Blue		- 1001 (9)
	Green 	- 0010 (2)	Light Green 	- 1010 (A)
	Cyan  	- 0011 (3)	Light Cyan		- 1011 (B)
	Red   	- 0100 (4)	Light Red		- 1100 (C)
	Magenta	- 0101 (5)	Light Magenta 	- 1101 (D)
	Brown	- 0110 (6)  Yellow    		- 1110 (E)
	White   - 0111 (7)	Bright White	- 1111 (F)

	The HIGH nibble (background) is defined as:

	Normal				(High bit sets blinking)
	Black   - 0000 (0)
	Blue  	- 0001 (1)
	Green 	- 0010 (2)
	Cyan  	- 0011 (3)
	Red   	- 0100 (4)
	Magenta	- 0101 (5)
	Brown	- 0110 (6)
	Grey    - 0111 (7)

*/

/* To specify an attribute OR (|) the BLINK, FG & BG values
you want together */

#define BLINK		0x80

#define	BLACK		0x00
#define BLUE		0x01
#define GREEN		0x02
#define CYAN		0x03
#define RED			0x04
#define MAGENTA		0x05
#define BROWN		0x06
#define WHITE		0x07
#define	GRAY		0x08
#define LTBLUE		0x09
#define LTGREEN		0x0A
#define LTCYAN		0x0B
#define LTRED		0x0C
#define LTMAGENTA	0x0D
#define YELLOW		0x0E
#define BRITEWHITE	0x0F

#define	BGBLACK		0x00
#define BGBLUE		0x10
#define BGGREEN		0x20
#define BGCYAN		0x30
#define BGRED		0x40
#define BGMAGENTA	0x50
#define BGBROWN		0x60
#define BGWHITE		0x70


/* MMURTL Basic Video calls */

extern far long SetVidOwner(long JobNum);
extern far long SetNormVid(long dAttr);
extern far long GetNormVid(long *pNormVidRet);
extern far long ClrScr(void);
extern far long GetVidChar(long ddCol,
                        long ddLine,
					    char *pCharRet,
					    char *pAttrRet);
extern far long SetXY(long NewX, long NewY);
extern far long PutVidAttrs(long ddCol,
                         long ddLine,
						 long sChars,
						 long dAttr);
extern far long PutVidChars(long ddCol,
                         long ddLine,
						 char *pChars,
						 long sChars,
						 long ddAttrib);
extern far long GetVidOwner(long *pdVidNumRet);
extern far long GetXY(long *pXRet, long *pYRet);
extern far long ScrollVid(long ddULCol,
                          long ddULline,
                          long nddCols,
                          long nddLines,
                          long ddfUp);
extern far long TTYOut (char *pTextOut, long ddTextOut, long ddAttrib);

extern far long EditLine(unsigned char *pStr,
						 unsigned long dCrntLen,
						 unsigned long dMaxLen,
						 unsigned long *pdLenRet,
						 unsigned char *pbExitChar,
						 unsigned long dEditAttr);

/******* End of MVid.h ***************/
