/* C program for cm32 C w/fp32 to test the Real type functions */

#include    "c:\cm32\inc\ctype.h"
#include    "c:\cm32\inc\stdarg.h"
#include    "c:\cm32\inc\stdio.h"
#include    "c:\cm32\inc\stdlib.h"
#include    "c:\cm32\inc\string.h"
#include    "c:\cm32\inc\fp32data.h"
#include    "c:\cm32\inc\fp32msgs.h"

void out_data(real *a, real *b, real *c, char s);
void out_comp(real *a, real *b, long c, char *s);

void
main(void)
{
    real anum, bnum, dnum;
    long err;

        err = 0;
        rzero(&anum); rzero(&bnum);
        if ((err = rasgc(&anum, 6, 5)) > 0)
            message(err);
        if ((err = rasgc(&bnum, 5, 6)) > 0)
            message(err);

        if ((err = radd(&dnum, &anum, &bnum)) > 0)
            message(err);
        else
            out_data(&dnum, &anum, &bnum, '+');

        if ((err = rsub(&dnum, &anum, &bnum)) > 0)
            message(err);
        else
            out_data(&dnum, &anum, &bnum, '-');

        if ((err = rmul(&dnum, &anum, &bnum)) > 0)
            message(err);
        else
            out_data(&dnum, &anum, &bnum, '*');

        if ((err = rdiv(&dnum, &anum, &bnum)) > 0)
            message(err);
        else
            out_data(&dnum, &anum, &bnum, '/');

        if ((err = req(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, "==");

        if ((err = rne(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, "!=");

        if ((err = rlt(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, "< ");

        if ((err = rle(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, "<=");

        if ((err = rgt(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, "> ");

        if ((err = rge(&anum, &bnum)) > 1)
            message(err);
        else
            out_comp(&anum, &bnum, err, ">=");

        printf("\r\n");
        return;
}

void
out_comp(real *n1, real *n2, long val, char *sign)
{
    char nbuf[BUFSIZE], ebuf[BUFSIZE];

        rrtoa(n1, nbuf, ebuf, NO);
        printf("\r\nReal: %s", nbuf);
        rrtoa(n2, nbuf, ebuf, NO);
        printf(" %s", sign);
        printf(" %s =", nbuf);
        printf(" %s", (val == 1) ? "YES" : "NO");
        printf("\r\n");
        return;
}

void
out_data(real *an, real *n1, real *n2, char sign)
{
    char nbuf[BUFSIZE], ebuf[BUFSIZE];

        rrtoa(n1, nbuf, ebuf, NO);
        printf("\r\nReal: %s", nbuf);
        rrtoa(n2, nbuf, ebuf, NO);
        printf(" %c", sign);
        printf(" %s =", nbuf);
        rrtoa(an, nbuf, ebuf, NO);
        printf(" %s\r\n", nbuf);
/*
        printf("\n\nReal: %ld + %ld = %ld\r\n\n",
            n1->value, n2->value, n1->value + n2->value);
        printf("Real: %ld * %ld = %ld\r\n\n",
            n1->value, n2->value, n1->value * n2->value);
*/
        return;
}

