/* ..............................................
 *  FP32DATA.H 
 */

/***********************************************
 Local & global definitions for the C MINUS 32
 real number extensions. Added by D W Pattee to
 code Copyrighted (c) 1989, Dave Dunfield and,
 Copyrighted 1991,1992,1993,1994 R.A. Burgess
************************************************/

#define     NO              0
#define     YES             1
#define     MAXEXPSIZE      3       /* declared 'long': increase these values */
#define     MAXEXPVAL       256     /* to your heart's content or at least to */
                                    /* the other side of the universe */
#define     MAXPOINTSIZE    16
#define     MAXNUMSIZE      10
#define     MAXNUMVAL       2147483647
#define     MAXNUM          "2147483647"
#define     BUFSIZE         32
#define     NULL            0

struct _real_type {
    long point;
    long value;
};

#define real    struct _real_type

#ifdef  fpdef

long radd(real *d, real *a, real *b);
long rdiv(real *d, real *a, real *b);
long rmul(real *d, real *a, real *b);
long rsub(real *d, real *a, real *b);

long req(real *a, real *b);
long rne(real *a, real *b);
long rgt(real *a, real *b);
long rge(real *a, real *b);
long rlt(real *a, real *b);
long rle(real *a, real *b);

long digit_scan(real *n);
long is_max(long n, char *z);
long is_greater(long n, long m);
long left_adjust(real *n);
long rasgc(real *d, long v, long e);                /* Assign Constant */
long rator(real *d, char *v, char *e);              /* ascii to Real */
long rcomp(real *a, real *b);
long rdiff(real *a, real *b, long c);

void message(long m);
void rasgr(real *d, real *s);
void right_adjust(real *n);
void rrtoa(real *s, char *v, char *e, char f);      /* Real to ascii */
void rzero(real *n);

#else

extern long radd(real *d, real *a, real *b);
extern long rdiv(real *d, real *a, real *b);
extern long rmul(real *d, real *a, real *b);
extern long rsub(real *d, real *a, real *b);

extern long req(real *a, real *b);
extern long rne(real *a, real *b);
extern long rgt(real *a, real *b);
extern long rge(real *a, real *b);
extern long rlt(real *a, real *b);
extern long rle(real *a, real *b);

extern long digit_scan(real *n);
extern long is_max(long n, char *z);
extern long is_greater(long n, long m);
extern long left_adjust(real *n);
extern long rasgc(real *d, long v, long e);
extern long rator(real *d, char *v, char *e);
extern long rcomp(real *a, real *b);
extern long rdiff(real *a, real *b, long c);

extern void message(long m);
extern void rasgr(real *d, real *s);
extern void right_adjust(real *n);
extern void rrtoa(real *s, char *v, char *e, char f);
extern void rzero(real *n);

#endif

