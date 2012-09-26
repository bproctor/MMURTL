/***********************************************
 CM32 standard I/O definitions for MMURTL
 Copyright 1992,1993 R.A. Burgess
************************************************/

#ifndef __stdio

#define __stdio

#define FILE	unsigned long
#define	EOF		-1L
#define NULL	0L

#define stdin  1
#define stdout  2
#define stderr  2

#define SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2

extern FILE *fopen(char *name, char *mode);
extern long *fclose(FILE *stream);
extern long remove(char *name);
extern long rename(char *oldname, char *newname);

extern long fgetc(FILE *stream);
extern char *fgets(char *s, long n, FILE *stream);

extern long fputs(const char *s, FILE *stream);
extern long fputc(long c, FILE *stream);

extern long printf(char *fmt, ...);
extern long sprintf(char *s, char *fmt, ...);
extern long fprintf(FILE *stream, char *fmt, ...);

extern long ftell(FILE *stream);
extern void rewind(FILE *stream);
extern void fseek(FILE *stream, long offset, long origin);
extern long fread(char *ptr, long size, long nobj, FILE *stream);
extern long fwrite(char *ptr, long size, long nobj, FILE *stream);

#endif

/************ end of stdio.h ******************/
