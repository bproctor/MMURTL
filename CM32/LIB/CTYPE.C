/*******************************************
*  ctype.c this contains the simple
*  character functions prototyped in ctype.h.
*
*  isalpha, isdigit, isalnum, isspace,
*  isgraph, isctrl, islower, isupper,
*  ispunct, isxdigit, toupper, tolower
*********************************************/

/************************************************
* Determine if a character is alphabetic
*************************************************/
long isalpha(long chr)
{
return  (((chr >= 'A') && (chr <= 'Z')) ||
		 ((chr >= 'a') && (chr <= 'z')));
}

/*********************************************
* Determine if a character is a numeric digit
**********************************************/

long isdigit(long chr)
{
return  ((chr >= '0') && (chr <= '9'));
}

/*********************************************
* Determine if a character is a alphanumeric
**********************************************/

long isalnum(long chr)
{
return  (((chr >= 'A') && (chr <= 'Z')) ||
		 ((chr >= 'a') && (chr <= 'z')) ||
		 ((chr >= '0') && (chr <= '9')));
}

/*********************************************
* Determine if a character is a whitespace
* (space, ff, lf, cr, tab, vtab)
**********************************************/

long isspace(long chr)
{
return  ((chr == ' ')  ||
		 (chr == '\f') ||
		 (chr == '\r') ||
		 (chr == '\n') ||
		 (chr == '\t') ||
		 (chr == 0x0B));
}

/***************************************************
* Determine if a character is printable ASCII
* (eg, char graphics)
****************************************************/
long isgraph(long chr)
{
	return (chr >= 0x21 && chr <= 0x7e);
}


/*********************************************
* Determine if a character is control char
**********************************************/
long isctrl(long chr)
{
	return chr >= 0 && chr <= 0x1f;
}

/*********************************************
* Determine if a character is lower case
**********************************************/
long islower(long chr)
{
	return ((chr >= 'a') && (chr <= 'z'));
}

/*********************************************
* Determine if a character is UPPER CASE
**********************************************/
long isupper(long chr)
{
	return ((chr >= 'A') && (chr <= 'Z'));
}

/***************************************************
* Determine if a character is punctuation
* (any printing char except space, letter, or digit)
****************************************************/
long ispunct(long chr)
{
	return  ((chr >= 0x21 && chr <= 0x2f) ||  	/*  ! to /  */
			 (chr >= 0x3a && chr <= 0x40) ||	/*  : to @  */
			 (chr >= 0x7a && chr <= 0x7e)); 	/*  { to ~  */
}

/***************************************************
* Determine if char is a hex digit (0-9, a-f, A-F)
****************************************************/
long isxdigit(long chr)
{
	return  (chr >= '0' && chr <= '9') ||
			(chr >= 'a' && chr <= 'f') ||
			(chr >= 'A' && chr <= 'F');
}

/************************************************
* Make char UPPER CASE
*************************************************/
long toupper(long chr)
{
	if ((chr >= 'a') && (chr <= 'z'))
		return (chr - 0x20);
	return(chr);
}

/************************************************
* Make char lower case
*************************************************/
long tolower(long chr)
{
	if ((chr >= 'A') && (chr <= 'Z'))
		return (chr + 0x20);
}

/***************** End ctype.c *****************/
