/*
 * Character ID functions for CM32 for the MMURTL OS
 *
 * The accepted range of character values is 0 to 127, and EOF.
 * Copyright 1992 R.A. Burgess
 * All rights reserved.
 */

extern long iscntrl(long c);
extern long isspace(long c);
extern long isdigit(long c);
extern long isupper(long c);
extern long islower(long c);
extern long ispunct(long c);
extern long isalpha(long c);
extern long isxdigit(long c);
extern long isalnum(long c);
extern long isgraph(long c);
extern long toupper(long c);
extern long tolower(long c);
