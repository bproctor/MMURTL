/*....................................................................
 *  FP32MSGS.H
 *
 *  Status, debugging and troubleshooting messages; can be used for
 *  finding out what is going on with real number arithmetic.
 */

#ifdef  fpdef

char *fp32msgs[] = {
    "This is NOT a message",
    "Real Number: Unknown Character",
    "Real Number: Too Many Digits in Value or Exponent",
    "Real Number: Non Digit in Exponent",
    "Real Number: Non Digit in Value",
    "Real Number: Exponent Out of Range",
    "Real Number: Value Out of Range",
    "Real Number: Divide by Zero"
}

#else

extern char *fp32msgs[];

#endif

