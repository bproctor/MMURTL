/* Busy loop demonstrates the preemptive capabilites of MMURTL.
   It doesn't go through the kernel and never sleeps. It is
   a task that is ALWAYS ready to run.

   It runs at the default priority of 25, and rest assured,
   other higher priority tasks will receive messages and be
   queued to run.

   If you run it, you will have to use the CTRL-ALT-DELETE keys
   to kill it.
   To build, use MakeIt.bat, or use CM32 or DASM separately:
   CM32 BusyLoop.c
   DASM BusyLoop.atf
*/

#include <stdio.h>
#include "\OSSource\Mtimer.h"
#include "\OSSource\MJob.h"

unsigned long tick;

void main(void)
{
long i;
	SetJobName("BusyLoop", 8);
	Sleep(100); /* A small delay before we start eating CPU time */
	while (1) 
	{
		GetTimerTick(&tick);
	    printf("%d \r\n", tick);
		i++;
	}
}
