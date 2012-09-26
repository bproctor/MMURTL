/* MTimer.h     MMURTL timer call prototypes */

/* MMURTL Operating System Source Code
   Copyright 1991,1992,1993, Richard A. Burgess
   ALL RIGHTS RESERVED
   Version x0.8
*/

extern far long Alarm(long Exch, long count);
extern far long KillAlarm(long Exch);
extern far long Sleep(long count);
extern far void MicroDelay(long us15count);
extern far long GetCMOSTime(long *pTimeRet);
extern far long GetCMOSDate(long *pTimeRet);
extern far long GetTimerTick(long *pTickRet);
extern far void Tone(long freq, long ms10);
extern far void Beep(void);

/************ End of MTimer.h ***************/
