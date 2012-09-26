/* Test client for the NUMBERS System Service.
   Run this program from another CLI when Service is running
*/

#include <stdio.h>
#include "\OSSOurce\MKernel.h"

unsigned long Number; 			/* The number to return */
unsigned long Exch;          	/* Where hte service will respond */
unsigned long Message[2];       /* The Message from the service */

void main(void)
{
unsigned long Error, rqhndl;

	Error = AllocExch(&Exch);	  /* get an exchange */

	if (Error)
		printf("Error %d allocating Exchange.\r\n", Error);

	Error = Request("NUMBERS ", 1, Exch, &rqhndl,
					0, 							/* No Send ptrs */
                    &Number, 4, 0, 0,
                    0,0,0);
	if (Error)
		printf("Error %d from Request.\r\n", Error);

	if (!Error)
	{
		Error = WaitMsg(Exch, Message);

		if (Error)
			printf("Error %d from WaitMsg.\r\n", Error);
		else
		{
			if (Message[1])
				printf("Error %d from NUMBERS Service.\r\n", Message[1]);
			else
				printf("NUMBERS Service gave out number: %d.\r\n", Number);
		}
	}

	DeAllocExch(Exch);	/* it's nice to do this for the OS */
}
