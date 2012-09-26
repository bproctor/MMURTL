/*****  fmtdio.c  *************************
* Formatted input/output routines for CM32
* printf, fprintf, sprintf
* scanf, fscanf, sscanf
*
*  Derived from code Copyright (c) 1989, Dave Dunfield
*  Copyright (c) 1994, R.A. Burgess
   Permission is required from the authors prior to
   any commercial use of this source code or products
   derived from its use.
*****************************************/

#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>

/* ANSI Prototypes of public functions in this file */

long printf(char *fmt, ...);
long sprintf(char *s, char *fmt, ...);
long fprintf(FILE *stream, char *fmt, ...);

long scanf(char *fmt, ...);
long sscanf(char *s, char *fmt, ...);
long fscanf(FILE *stream, char *fmt, ...);


#define	S_SIZE	160


/*************************************************************
 This does the actual parsing of the format and also moves to
 the next arg(s) in the list from the passed in arg pointer.
 The number of chars written is returned (not incl \0).
**************************************************************/
long _formatout(char *outptr, char *fmt, long *argptr)
{
char numstk[33], *ptr, justify, zero, minus, chr, us;
unsigned long width, value, i, total;

	total = 0;
	while(chr = *fmt++) {
		if(chr == '%') {					/* format code */
			chr = *fmt++;
            ptr = &numstk[32];
			*ptr = justify = minus = 0;
			width = value = i = 0;
			zero = ' ';
			if(chr == '-')
			{				/* left justify */
				--justify;
				chr = *fmt++;
			}
			if(chr == '0')					/* leading zeros */
				zero = '0';
			while(isdigit(chr))
			{			/* field width specifier */
				width = (width * 10) + (chr - '0');
				chr = *fmt++;
			}

			value = *--argptr;				/* get parameter value */

			/* first switch allows for ld Ld ld ud Ud etc... */

			switch(chr)
			{
				case 'U' :					/* unsigned number */
				case 'u' :
					i = 10;
					us = 1;
					chr = *fmt++;
					break;
				case 'l' :					/* Long (it is anyway) */
				case 'L' :
					chr = *fmt++;
					break;
				default:					/* all others */
					us = 0;					/* not unsigned */
					break;
			}

			switch(chr)
			{
				case 'd' :					/* decimal number */
					i = 10;
					if (!us)
						if(value & 0x80000000)
						{
							value = -value;
							++minus;
						}
					break;
				case 'X' :					/* hexidecimal number */
				case 'x' :					/* hexidecimal number */
					i = 16;
					break;
				case 'o' :					/* octal number */
					i = 8;
					break;
				case 'b' :					/* binary number */
					i = 2;
					break;
				case 'c' :					/* character data */
					*--ptr = value;
					break;
				case 's' :					/* string */
					ptr = value;			/* value is ptr to string */
					break;
				default:					/* all others */
					*--ptr = chr;
					++argptr;				/* backup to last arg */
			}

			if(i)		/* for all numbers, generate the ASCII string */
				do {
					if((chr = (value % i) + '0') > '9')
						chr += 7;
					*--ptr = chr; }
				while(value /= i);

			/* output sign if any */

			if(minus)
			{
				*outptr++ = '-';
				++total;
				if(width)
					--width;
			}

			/* pad with 'zero' value if right justify enabled  */

			if(width && !justify) {
				for(i = strlen(ptr); i < width; ++i)
					*outptr++ = zero;
					++total;
				}

			/* move in data */

			i = 0;
			value = width - 1;

			while((*ptr) && (i <= value)) {
				*outptr++ = *ptr++;
				++total;
				++i; }

			/* pad with 'zero' value if left justify enabled */

			if(width && justify) {
				while(i < width) {
					*outptr++ = zero;
					++total;
					++i;
					}
				}
			}
		else {
			/* not format char, just move into string  */
			*outptr++ = chr;
			++total;
			}
		}

	*outptr = 0;
	return total;
}

/************************************
    Formatted print to stdout
*************************************/

long printf(char *fmt, ...)
{
	va_list ap;
	long total;
	char buffer[S_SIZE];

	va_start(ap, fmt);		/* set up ap pointer */
	total = _formatout(buffer, fmt, ap);
	fputs(buffer, stdout);
	va_end(ap, fmt);
	return total;
}

/************************************
    Formatted print to string s
*************************************/

long sprintf(char *s, char *fmt, ...)
{
	va_list ap;
	long total;

	va_start(ap, fmt);			/* set up ap pointer */
	total = _formatout(s, fmt, ap);
	va_end(ap, fmt);
	return total;
}

/************************************
    Formatted print to a file
*************************************/

long fprintf(FILE *stream, char *fmt, ...)
{
	va_list ap;
	long total;
	char buffer[S_SIZE];

	va_start(ap, fmt);		/* set up ap pointer */
	total = _formatout(buffer, fmt, ap);
	if (fputs(buffer, stream)==EOF) total = -1;
	va_end(ap, fmt);
	return total;
}

/*************************** INPUT ROUTINES *************************/



/*************************************************************
 This does the actual parsing of the input string and moves
 each arg(s) into the list of pointers (pOut)
 from the passed in arg pointer. The number of items is returned.
**************************************************************/

_formatin(char *input, long *pOut)
{
	unsigned nItems, value, value1, base, *ptr1;
	char *format, *ptr2, chr, mflag, cflag;

	format = *--pOut;
	nItems = 0;

	while(chr = *format++) {
		if(isspace(chr))		/* whitespace */
			continue;
		while(isspace(*input))
			++input;
		if(chr != '%') {		/* Non-format character */
			if(*input == chr)
				++input;
			continue; }
		ptr1 = ptr2 = *--pOut;
		cflag = mflag = base = value = value1 = 0;
		while(isdigit(chr = *format++))		/* get width if any */
			value = (value * 10) + (chr - '0');
		switch(chr) {
			case 'c' :				/* character input */
				if(!value)				/* default to single char */
					value = 1;
				cflag = 1;
				do {
					if(*ptr2++ = *input)
						++input;
					else {
						cflag = 0;
						break; } }
				while(--value);
				break;
			case 's' :				/* string input */
				if(!value)				/* default to full string */
					value = -1;
				do {
					if(*ptr2++ = chr = *input) {
						++input;
						cflag = 1; }
					else
						break; }
				while((!isspace(chr)) && value--);
				--input;
				*(ptr2-1) = 0;
				break;
			case 'd' :				/* signed number */
				if(*input == '-') {
					++input;
					mflag = -1; }
			case 'u' :				/* unsigned number */
			case 'U' :				/* unsigned number */
				base = 10;
				break;
			case 'b' :				/* Binary number */
				base = 2;
				break;
			case 'o' :				/* Octal number */
				base = 8;
				break;
			case 'x' :				/* Hexidecimal number */
				base = 16;
				break;
			case '%' :				/* Doubled percent sign */
				if(*input == '%')
					++input;
				break;
			default:				/* Illegal type character */
				return 0; }

		if(base) {				/* Number conversion required */
			do {
				if(isdigit(chr = *input))
					chr -= '0';
				else if(chr >= 'a')
					chr -= ('a' - 10);
				else if(chr >= 'A')
					chr -= ('A' - 10);
				else
					break;
				if(chr >= base)
					break;
				value1 = (value1 * base) + chr;
				cflag = 1;
				++input; }
			while(--value);
			*ptr1 = (mflag) ? -value1 : value1; }
		nItems += cflag; }
	return nItems;
}


/************************************
    Formatted input from stdin
*************************************/

long scanf(char *fmt, ...)
{
va_list ap;
long total;
char buffer[S_SIZE];

	va_start(ap, fmt);		/* set up ap pointer */
	if(!fgets(buffer, S_SIZE, stdin))
		total = EOF;
	total = _formatin(buffer, ap);
	va_end(ap, fmt);
	return total;
}

/************************************
    Formatted input from a file
*************************************/
long fscanf(FILE *stream, char *fmt, ...)
{
va_list ap;
long total;
char buffer[S_SIZE];

	va_start(ap, fmt);		/* set up ap pointer */
	if(!fgets(buffer, S_SIZE, stream))
		total = EOF;
	total = _formatin(buffer, ap);
	va_end(ap, fmt);
	return total;
}

/************************************
    Formatted input from a string
*************************************/
long sscanf(char *s, char *fmt, ...)

{
va_list ap;
long total;

	va_start(ap, fmt);		/* set up ap pointer */
	total = _formatin(s, ap);
	va_end(ap, fmt);
	return total;
}
