/* stdarg.h */

/* This header defines the stdarg macros which allow a function
to handle unknown numbers and types of arguments.  It follows the
ANSI conventions, but may be a little more strict than other compilers
because stack elements are always 4 bytes.

C-32 is different from most C compilers in that it uses the
Intel PLM calling conventions (pushing args from left to right).
Because of this, there is a rule that must be followed when
defining functions that use have variable parameter lists:

  !!!! YOU MUST PROTOTYPE THE FUNCTION BEFORE YOU USE IT  !!!!

 The standard library functions do this properly (e.g., stdio.h) in
 their header files. Examples:

        long int printf(char *fmt, ...);
        long int fprintf(*stream, char *fmt, ...);

To see examples of the proper use of the stdarg definitions, see
the library source code stdio.c.  It is used exactly like those found
in K&R 2nd edition (ANSI) Pg. 156.  The only real difference between
the K&R examples and this code is that va_start actually makes ap
point to the last named arg vice the first unnamed. The pointer
is set p in va_arg to point to the next arg before it's assigned.
So long as you use it as shown, you will have no problems.

*/

#define va_list unsigned long *

#define	va_start(ap,arg)   ap=&arg 		/* ap now points to last named arg */

#define va_arg(ap,type)	   ap-=4		/* next arg, then assign it */

#define va_end(ap)		  /*Nothing to do...*/
