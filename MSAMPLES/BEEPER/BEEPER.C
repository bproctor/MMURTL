/* This program allows your ears to hear whether or not
   a program gets equal time while running with other programs
   in MMURTL. Every half second, this beeps. That's all it does.

To build this Beeper.RUN, use Makeit.bat in DOS, or CM32 and DASM:
  CM32 Beeper.C
  DASM Beeper.ATF
*/

#include "\OSSource\MTimer.h"
#include "\OSSource\MKernel.h"
#include "\OSSource\MJob.h"

void main(void)
{
	SetJobName("Beeper", 6);
	while (1) {
		Sleep(50);
		Tone(150,10);
	}
}
