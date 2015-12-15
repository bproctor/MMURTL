/*  FP32MATH.C .......................................................
 */

#include    "c:\cm32\inc\ctype.h"
#include    "c:\cm32\inc\stdarg.h"
#include    "c:\cm32\inc\stdio.h"
#include    "c:\cm32\inc\stdlib.h"
#include    "c:\cm32\inc\string.h"
#include    "c:\cm32\inc\mjob.h"

#define     fpdef

#include    "c:\cm32\inc\fp32data.h"
#include    "c:\cm32\inc\fp32msgs.h"

/*....................................................................
 *  Routines that interface fp32 math functions to cm32.
 *....................................................................*/

/*....................................................................
 *  Add to real numbers and assign answer to another real number. Use
 *  local variables for all adjustments.
 *
 *  real a + real b = real c
 */
long
radd(real *dnum, real *n1, real *n2)
{
    real anum, bnum, *lptr, *sptr;
    long diff, digits, i;

        rzero(dnum);                        /* initialize variables */
        rasgr(&anum, n1);                   /* transfer passed variables to */
        rasgr(&bnum, n2);                   /* local variables */

        if (anum.value == 0) {              /* check for zero */
            rasgr(dnum, &bnum);
            return (NO);
        }
        if (bnum.value == 0) {              /* check for zero */
            rasgr(dnum, &anum);
            return (NO);
        }
        lptr = sptr = NULL;             /* initialize more local variables */
        diff = digits = i = 0;
        right_adjust(&anum);            /* remove zeros right of val */
        right_adjust(&bnum);
        if (anum.point != bnum.point) {         /* exponents not equal */
            if (anum.point > bnum.point) {      /* find the smaller exp */
                lptr = &anum; sptr = &bnum;
            } else {
                lptr = &bnum; sptr = &anum;
            }
            diff = lptr->point - sptr->point;   /* get the difference */
            digits = digit_scan(lptr);      /* unused digits left of val */
            if (digits > diff) {            /* line up decimal points ? */
                i = diff;                   /* more unused than needed */
                while (i-- > 0)
                    lptr->value *= 10;      /* move val w/larger exp left */
                lptr->point -= diff;
            } else {                        /* not more than needed */
                digits = left_adjust(lptr); /* move val w/larger exp left */
                i = diff - digits;
                while (i-- > 0)             /* then */
                    sptr->value /= 10;      /* move val w/smaller exp right */
                i = diff - digits;          /* droping smallest precission */
                sptr->point += i;           /* update exponent */
            }
        }
        if (is_greater(anum.point, MAXEXPVAL))
            return (5);
        i = 0;          /* dummy line to force the compiler to behave */
        dnum->value = anum.value + bnum.value;
        dnum->point = (dnum->value == 0) ? 0 : anum.point;
        if (dnum->value > 0)
            right_adjust(dnum);
        return (NO);
}

/*....................................................................
 *  Setup two real number for subtraction and call 'radd' to add them.
 *  Use local variables for all adjustments.
 *
 *  real a - real b = real c
 */
long
rsub(real *dnum, real *n1, real *n2)
{
        if (n2->value != 0) {
            n2->value = -n2->value;
            radd(dnum, n1, n2);
            n2->value = -n2->value;
        } else
            rasgr(dnum, n1);
        return (NO);
}

/*....................................................................
 *  Multiply two real numbers and assign answer to another real number.
 *  Use local variables for all adjustments. Weed out myltiply by 0.
 *
 *  real a * real b = real c
 */
long
rmul(real *dnum, real *n1, real *n2)
{
    real anum, bnum;
    long i, point;
    char sflag;

        rzero(dnum); sflag = NO; point = 0;
        if (n1->value == 0 || n2->value == 0)
            return (NO);
        rasgr(&anum, n1); rasgr(&bnum, n2);
        if (anum.value < 0) {
            anum.value = -anum.value;
            sflag = YES;
        }
        if (bnum.value < 0) {
            bnum.value = -bnum.value;
            sflag = (sflag == YES) ? NO : YES;
        }
        right_adjust(&anum); right_adjust(&bnum);
        point = anum.point + bnum.point;
        if (is_greater(point, MAXEXPVAL))
            return (5);
        i = 0;          /* dummy line to force the compiler to behave */
        dnum->value = anum.value * bnum.value;
        dnum->point = (dnum->value == 0) ? 0 : point;
        if (dnum->value > 0) {
            right_adjust(dnum);
            if (sflag == YES)
                dnum->value = -dnum->value;
        }
        return (NO);
}

/*....................................................................
 *  Divide two real numbers and assign answer to another real number.
 *  Weed out a zero dividend and divide by zero. Move the dividend as
 *  far left and the divisor as far right as possible to maximize
 *  pressision. Use local variables for all adjustments.
 *
 *  real a / real b = real c
 */
long
rdiv(real *dnum, real *n1, real *n2)
{
    real anum, bnum;
    long i, point;
    char sflag;

        rzero(dnum); sflag = NO; point = 0;
        if (n1->value == 0)
            return (NO);
        if (n2->value == 0)
            return (7);
        rasgr(&anum, n1); rasgr(&bnum, n2);
        if (anum.value < 0) {
            anum.value = -anum.value;
            sflag = YES;
        }
        if (bnum.value < 0) {
            bnum.value = -bnum.value;
            sflag = (sflag == YES) ? NO : YES;
        }
        left_adjust(&anum); right_adjust(&bnum);
        point = anum.point - bnum.point;
        if (is_greater(point, MAXEXPVAL))
            return (5);
        i = 0;          /* dummy line to force the compiler to behave */
        dnum->value = anum.value / bnum.value;
        dnum->point = (dnum->value == 0) ? 0 : point;
        if (dnum->value > 0) {
            right_adjust(dnum);
            if (sflag == YES)
                dnum->value = -dnum->value;
        }
        return (NO);
}

/*....................................................................
 *  Routines that interface fp32 logical functions to cm32.
 *....................................................................*/

/*....................................................................
 *  Logically compare two real numbers and return true if equal and
 *  false if not. On error, return early with error code.
 */
long
req(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err == 0) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Logically compare two real numbers and return true if not equal
 *  and false if not. On error, return early with error code.
 */
long
rne(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err != 0) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Logically compare two real numbers and return true if greater than
 *  and false if not. On error, return early with error code.
 */
long
rgt(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err == 1) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Logically compare two real numbers and return true if greater than
 *  or equal and false if not. On error, return early with error code.
 */
long
rge(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err == 0 || err == 1) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Logically compare two real numbers and return true if less than
 *  and false if not. On error, return early with error code.
 */
long
rlt(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err == -1) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Logically compare two real numbers and return true if less than or
 *  equal to and false if not. On error, return early with error code.
 */
long
rle(real *a, real *b)
{
    long cv, err;

        cv = 0; err = NO;
        err = rcomp(a, b);
        if (err > 1)
            return (err);
        cv = (err == 0 || err == -1) ? YES : NO;
        return (cv);
}

/*....................................................................
 *  Routines that support fp32 math & logical functions.
 *....................................................................*/

/*....................................................................
 *  Convert ascii representations of a value and exponent to a real
 *  number.
 */
long
rator(real *dnum, char *snum, char *sexp)
{
    char *ebp, *nbp, *sbp, nbuf[BUFSIZE], *texp, *tnum, esign, tsign;
    long j, k, point, value;
    int  i;

    /* Complaint Department */

        i = strlen(sexp); ebp = sexp;
        j = strlen(snum); sbp = snum;
        if (i == 0 || (i == 1 && ! isdigit(*ebp)) ||
            j == 0 || (j == 1 && ! isdigit(*sbp)))
                return (1);

        rzero(dnum);                        /* Initialize the real number */

        nbp = nbuf; i = 0;
        while (*sbp && *sbp != '.')         /* Find decimal point, if any */
            *nbp++ = *sbp++;
        if (! *sbp)                         /* No dot found */
            *nbp = *sbp;
        else {
            sbp++;                          /* Dot found, skip it */
            while (*sbp) {
                *nbp++ = *sbp++;
                i--;                        /* Shift dot right */
            }                               /* And adjust exponent */
            *nbp = *sbp;
        }
        tnum = nbuf; texp = sexp;
        esign = tsign = NO;
        if (*texp == '-') {                 /* check exponent polarity */
            texp++;
            esign = YES;                    /* get ready to set exponent sign */
        }
        while (*texp && *texp == '0')       /* bypass leading exp zeros */
            texp++;
        if (*tnum == '-') {                 /* check number polarity */
            tnum++;
            tsign = YES;                    /* get ready to set number sign */
        }
        while (*tnum && *tnum == '0')       /* bypass leading num zeros */
            tnum++;
        nbp = strlen(tnum) - 1;
        while (nbp >= tnum && *nbp == '0') {    /* remove trailing num zeros */
            *nbp = 0; i++;                      /* and adjust exponent */
        }
        if (! nbp)                          /* no number */
            return (NO);
        if (strlen(texp) > MAXEXPSIZE ||
            strlen(tnum) > MAXNUMSIZE)
                return (2);                 /* too many digits */
        if (strcmp(tnum, MAXNUM) > 0)
            return (6);                     /* value to large */
        j = 0; k = 1; point = 0;
        ebp = texp + strlen(texp) - 1;
        while (ebp >= texp) {               /* build exponent */
            if (! isdigit(*ebp))
                return (3);                 /* non-numerical character */
            j = *ebp - 48; ebp--;
            point += (j * k); k *= 10;
        }
        point = (esign) ? -point : point;   /* sign point */
        point += i;                         /* adjust point */
        j = 0; k = 1; value = 0;
        nbp = tnum + strlen(tnum) - 1;
        while (nbp >= tnum) {               /* build number */
            if (! isdigit(*nbp))
                return (4);                 /* non-numerical character */
            j = *nbp - 48; nbp--;
            value += (j * k); k *= 10;
        }
        value = (tsign) ? -value : value;   /* sign value */

        if (is_greater(point, MAXEXPVAL))
            return (5);                     /* exponent too large */
/*
        if (is_greater(value, MAXNUMVAL))       /* see above */
            return (6);                     /* value too large */
*/
        dnum->point = point;                /* set point */
        dnum->value = value;                /* set value */

        return (NO);
}

/*....................................................................
 *  Convert a real number to ascii representations of a value and an
 *  exponent. If value is greater than a given size or eflag is true,
 *  convert to the exponential format otherwise show number w/decimal
 *  point.
 */
void
rrtoa(real *snum, char *dnum, char *dexp, char eflag /*, char ljust, char zero, long width, long prec */)
{
    long dp, i, len;
    char *ep, *np, *vp, *xp, expbuf[BUFSIZE], valbuf[BUFSIZE];

        sprintf(expbuf, "%ld", snum->point);
        sprintf(valbuf, "%ld", snum->value);
        vp = valbuf; ep = expbuf;               /* initilize local variables */
        np = dnum; *np = 0;
        xp = dexp; *xp = 0;
        dp = len = 0;

        if (*vp == '-')                 /* transfer minus sign for */
            *np++ = *vp++;              /* negative values */

        len = strlen(vp);               /* find decimal point from */
        dp = len + snum->point;         /* beginning of number */

        if (eflag || dp > MAXPOINTSIZE || (len - dp) > MAXPOINTSIZE) {
            *np++ = *vp++;
            *np++ = '.';                    /* begin w/one num and dp */
            i = dp - len + strlen(vp);      /* get ready to update */
            sprintf(expbuf, "%ld", i);      /* exponent image */
            while (*vp > 0)
                *np++ = *vp++;              /* copy value */
            *np = *vp;                      /* terminate number */

            *xp++ = 'E';                    /* begin exponent with big E */
            if (*ep == '-')
                *xp++ = *ep++;              /* determine + or - sign */
            else
                *xp++ = '+';
            i = MAXEXPSIZE - strlen(ep);    /* find padding value */
            while (i-- > 0)
                *xp++ = '0';                /* pad with zeros if any */
            while (*ep > 0)
                *xp++ = *ep++;              /* copy exponent */
            *xp = *ep;                      /* terminate exponent */
            strcat(dnum, dexp);             /* add exponent to number */
        } else {
            if (dp <= 0) {                  /* value is behind decimal point */
                *np++ = '0';                /* insert a beginning zero */
                *np++ = '.';                /* insert the decimal point */
                while (dp++ < 0)            /* insert zeros, pad right if any */
                    *np++ = '0';
                while (*vp)                 /* copy value */
                    *np++ = *vp++;
            } else {                        /* decimal is in or behind value */
                if (dp < len) {             /* decimal point is within value */
                    while (dp-- > 0)        /* copy up to decimal position */
                        *np++ = *vp++;
                    *np++ = '.';            /* insert the decimal point */
                    while (*vp)             /* copy the rest of the value */
                        *np++ = *vp++;
                } else {                    /* decimal point is behind value */
                    while (*vp && dp--)     /* copy entire value up to end */
                        *np++ = *vp++;
                    if (dp > 0)             /* when decimal is beyond end */
                        while (dp-- > 0)
                            *np++ = '0';    /* insert zeros to pad right if any */
                    *np++ = '.';            /* insert the decimal point */
                    *np++ = '0';            /* insert a trailing zero */
                }
            }
            *np = *vp;                      /* terminate number */
        }
}

/*................................................
 *  Assign a constant number and exponent to a
 *  real number. Return error's if exponent or
 *  number are to large. Null exponent if value
 *  is zero.
 */
long
rasgc(real *dnum, long value, long point)
{
    long tpoint, tvalue;

        rzero(dnum);
        tpoint = point; tvalue = value;
        if (is_greater(tpoint, MAXEXPVAL))
            return (5);
/*
        if (is_greater(tvalue, MAXNUMVAL))
            return (6);
*/
        dnum->point = (value == 0) ? 0: point;
        dnum->value = value;
        return (NO);
}

/*................................................
 *  Copy or assign the exponent and value of one
 *  real number to another real number.
 */
void
rasgr(real *dnum, real *snum)
{
        rzero(dnum);
        dnum->point = snum->point;
        dnum->value = snum->value;
        return;
}

/*....................................................................
 *  Compare two real values and return 1 if a > b, 0 if a = b or
 *  -1 if a < b. When the out come is not known, calls 'diff()' to do
 *  more processing.
 */
long
rcomp(real *a, real *b)
{
    long result;

        result = 0;

        if (a->point == b->point && a->value == b->value)
            return (0);

        if (a->value > 0 && b->value < 0)
            return (1);
        if (a->point >= b->point && a->value > b->value)
            return (1);
        if (a->point > b->point && a->value >= b->value)
            return (1);

        if (a->value < 0 && b->value > 0)
            return (-1);
        if (a->point <= b->point && a->value < b->value)
            return (-1);
        if (a->point < b->point && a->value <= b->value)
            return (-1);

        if (a->point > b->point && a->value < b->value) {
            result = rdiff(a, b, 1);
            return (result);
        }
        if (a->point < b->point && a->value > b->value) {
            result = rdiff(b, a, -1);
            return (result);
        }
}

/*....................................................................
 *  Provides more processing when simple logical comparison results
 *  are not known. Uses the two exponents to expand numbers to test
 *  for hidden comarisons. Compensates for when one number can be
 *  equal to the other and still be greater and when it MUST be
 *  greater than the other. Returns only a 1 or -1, normally but will
 *  also return an error if the difference of the exponents (first
 *  line of code) blows up (when first is positive and second is
 *  negative).
 */
long
rdiff(real *a, real *b, long cv)
{
    real x, y;
    long dexp, digt, i;
    char f;

        dexp = 0; digt = 0; f = 0; i = 0;
        rasgr(&x, a); rasgr(&y, b);
        dexp = x.point - y.point;
        digt = digit_scan(&x);
        if (dexp >= digt && is_max(x.value, MAXNUM) > 0)
            return (cv);
        i = (digt >= dexp) ? dexp : digt;
        f = (digt == dexp) ? YES : NO;
        while (i-- > 0) {
            x.value *= 10;
            if (((! f || i > 0) && x.value >= y.value) || x.value > y.value)
                return (cv);
        }
        return (-cv);
}

/*................................................
 *  Assign zeros to real number exponent and
 *  number.
 */
void
rzero(real *rnum)
{
        rnum->point = 0;
        rnum->value = 0;
}

/*................................................
 *  For any zeros to the right of a value, move
 *  a value right as many digits as zeros and
 *  update the exponent.
 */
void
right_adjust(real *snum)
{
    char *bp, *cp, buf[BUFSIZE];
    long i, digits;

        sprintf(buf, "%ld", snum->value);
        bp = buf; i = 0; digits = 0;
        if (*bp == '-')
            bp++;
        if (*bp > 0) {
            cp = bp + strlen(bp) - 1;
            if (*cp == 48) {
                while (cp >= bp && *cp-- == 48)
                    digits++;
                i = digits;
                while (i--)
                    snum->value /= 10;
                snum->point += digits;
            }
        }
}

/*................................................
 *  For any unused digits left of a value, move a
 *  value left as many digits as unused and update
 *  the exponent. If the number is too large for
 *  a signed long, adjust the digits. Return the
 *  value of digits.
 */
long
left_adjust(real *snum)
{
    long i, digits;

        i = 0; digits = 0;
        digits = digit_scan(snum);
        if (is_max(snum->value, MAXNUM) > 0)
            digits--;
        if (digits > 0) {
            i = digits;
            while (i-- > 0)
                snum->value *= 10;
            snum->point -= digits;
        }
        return (digits);
}

/*................................................
 *  Return the unused digits available to the left
 *  of a value. Ignore any sign indicator.
 */
long
digit_scan(real *snum)
{
    char *bp, buf[BUFSIZE];

        sprintf(buf, "%ld", snum->value);
        bp = buf;
        if (*bp == '-')
            bp++;
        return (MAXNUMSIZE - strlen(bp));
}

long
is_max(long num, char *maxnum)
{
    char *sp, sbuf[BUFSIZE];

        sprintf(sbuf, "%ld", num);
        sp = sbuf;
        if (*sp == '-')
            sp++;
        return (strncmp(sp, maxnum, strlen(sp)));
}

long
is_greater(long num, long maxnum)
{
        if (num < 0)
            num = -num;
        if (num > maxnum)
            return (YES);
        return (NO);
}

void
message(long i)
{
        printf("\r\n\n%s\r\n\n", fp32msgs[i]);
}

