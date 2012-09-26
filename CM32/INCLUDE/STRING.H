/*
 * C-32 standard string definitions for MMURTL
 *
 * Copyright 1992 R.A. Burgess
 */

extern char *strcpy(char *s, const char *ct);
extern char *strncpy(char *s, const char *ct, long n);
extern char *strcat(char *s, const char *ct);
extern char *strncat(char *s, const char *ct, long n);
extern long strcmp(const char *cs, const char *ct);
extern long strncmp(const char *cs, const char *ct, long n);

extern long strlen(char *cs);
extern char *strchr(char *string, char chr);
