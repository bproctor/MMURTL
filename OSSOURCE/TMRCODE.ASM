;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED
;   Version 1.0

; This file contains the following internal and Public calls
; dealing with time:
;
; IntTimer  - The Timer ISR
; Sleep() 	- A function to delay execution of a task
; Alarm()	- A function to notify a task of a timer event
; KillAlarm()	- Cancels an Alarm
; GetCMOSTime()	- Reads the CMOS Time
; GetCMOSDate()	- Reads the CMOS Date
; MicroDelay()	- Very small delays with no task suspension
; GetTimerTick()   - Gets system tick count
;
;Beep() and Tone() are also here for lack of a better place
;to put them.

.DATA
.INCLUDE MOSEDF.INC
.INCLUDE TSS.INC

.ALIGN DWORD

; TIMER INTERRUPT COUNTER and TimerBlocks
;-------------------------------------------
; The Timer Block is a structure that contains
; the exchange number of a task that is sleeping,
; or one that has requested an alarm.
; Each TimerBlock is 12 Bytes long
; These are the offsets into the structure
;
sTmrBlk 	EQU 12
fInUse		EQU 0	;DD 00000000h
TmrRespExch	EQU 4	;DD 00000000h
CountDown	EQU 8	;DD 00000000h
;
EXTRN SwitchTick DD
EXTRN dfHalted   DD

PUBLIC TimerTick	DD 0			;Incremented every 10ms (0 on bootup).
PUBLIC nTmrBlksUsed	DD 0			;Number of timer blocks in use
PUBLIC rgTmrBlks	DB (sTmrBlk * nTmrBlks) DUP (0)

;
;==== Begin Code =============================================================
;
.CODE
;
EXTRN ChkRdyQ NEAR
EXTRN enQueueRdy NEAR
EXTRN deQueueRdy NEAR
;
;
; The timer interrupt checks up to n timer blocks for values to decrement.
; The timer interrupt fires off every 10 milliseconds.  This sounds
; like a lot (and it is), but even on a 20 Mhz processor it doesn't consume
; too much bandwidth at all (CPU Time).
;
; Interrupt latency can be a problem for high speed non-DMA comms.
; A single channel operating at 19,200 bps will interrupt every 520 us.
; Two channels will do it every 260us (continuous comms).
; (with non-buffered UARTS that is...).
;
; The timer int code also performs the important function of keeping
; tabs on CPU hogs. It is really the onyl part of task scheduling that
; isn't cooperative. It is a small but VERY important part.

; At all times on the system, only one task is actually executing.
; We have now interrupted that task.  Other tasks maybe waiting
; at the ReadyQ to run. They may have been placed there by other
; ISRs, and they may be an equal or higher priority than the task
; that is now running (the one we interrupted).
; We check to see if the same task has been running for 100ms or
; more. If so.. we call ChkRdyQ and check the priority of that
; task. If it is the same or higher, we switch to it. Then we place
; the task we interrupted on the RdyQ in exactly the state we
; interrupted it.
; We couldn't even do it this way if we were using "interrupt tasks"
; because this would "nest" hardware task swtiches. It would
; require manipulating items in the TSS to make it look as if it
; wasn't nested. Keep this in mind if you use the interrupt tasks.
;
PUBLIC IntTimer:
		PUSHAD	      				;INTS are disabled automatically
		INC TimerTick               ;Timer Tick, INT 20
		CMP nTmrBlksUsed, 0			;Anyone sleeping or have an alarm set?
		JE  SHORT TaskCheck			;No...

IntTmr00:							;Yes!
		LEA EAX,rgTmrBlks           ;EAX has addr of Timer Blocks
		MOV ECX,nTmrBlks            ;ECX has count of Timer Blocks
		CLD							;Move forward thru the blocks

IntTmr01:
		CMP DWORD PTR [EAX+fInUse],FALSE		;Timer Block found?
		JE IntTmr03					;No  - goto next block
		CMP DWORD PTR [EAX+CountDown],0h      ;Yes - is count at zero?
		JNE IntTmr02                ;No  - goto decrement it
		PUSH EAX                    ;save ptr to rgTmpBlk
		PUSH ECX                    ;save current count
		PUSH DWORD PTR [EAX+TmrRespExch]	;Yes - ISend Message
		MOV EAX, -1					;FFFFFFFFh
		PUSH EAX		            ;bogus msg
		PUSH EAX        		    ;bogus msg
		CALL FWORD PTR _ISendMsg	;tell him his times up!
		POP ECX                     ;get count back
		POP EAX                     		;get ptr back
		MOV DWORD PTR [EAX+fInUse],FALSE      ;Free up the timer block
		DEC nTmrBlksUsed					;Correct count of used blocks
		JMP IntTmr03                		;skip decrement - empty blk

IntTmr02:
		DEC DWORD PTR [EAX+CountDown]   ;10ms more gone...

IntTmr03:
		ADD EAX,sTmrBlk             ;next block please!
		LOOP IntTmr01               ;unless were done

		;We will now check to see if this guy has been running
		;for more then 30ms (3 ticks). If so, we will
		;switch him out if someone with an equal or higher pri
		;is on the RdyQ

TaskCheck:
		MOV EAX, dfHalted
		OR EAX, EAX
		JNZ TaskCheckDone

		MOV	EAX, TimerTick
		MOV EBX, SwitchTick
		SUB EAX, EBX
		CMP EAX, 3					;Change this to change the "time slice"
		JL SHORT TaskCheckDone		;Hasn't been 30ms yet for this guy!

		CALL ChkRdyQ			;Get next highest Pri in EAX (Leave Queued)
		OR EAX, EAX				;Is there one at all??
		JZ TaskCheckDone		;No...

		MOV ESI, pRunTSS
		MOV DL, [ESI+Priority]	;DL is pri of current
		CMP DL, [EAX+Priority]	;Compare current pri to highest queued
								;The CMP subtracts Pri of Queued from
								;current pri. If the Queued Pri is LOWER
								;or Equal, we want to Switch. That means
								;if we got a CARRY on the subtract, his
								;number was higher and we DON'T
		JC TaskCheckDone		;If current is higher(lower num), keep going!

		CALL deQueueRdy        	; Get high priority TSS off the RdyQ
		MOV EDI, EAX			;  and save in EDI
		MOV EAX, ESI			; Put current one in EAX
		CALL enQueueRdy        	;  and on the RdyQ

        MOV pRunTSS,EDI         ; Make the TSS in EDI the Running TSS
		MOV BX,[EDI+Tid]        ; Get the task Id (TR)
		MOV TSS_Sel,BX          ; Put it in the JumpAddr for Task Swtich
		INC _nSwitches			; Keep track of how many switches for stats
		INC _nSlices			; Keep track of how many sliced switches
		MOV EAX, TimerTick		; Save time of this switch for scheduler
		MOV SwitchTick, EAX		;
		PUSH 0						;Must do this before we switch!
		CALL FWORD PTR _EndOfIRQ    ;
		JMP FWORD PTR [TSS]     ; JMP TSS (This is the task swtich)
		POPAD                   ;
		IRETD                   ;

TaskCheckDone:
		PUSH 0						;Must do this before we switch!
		CALL FWORD PTR _EndOfIRQ    ;
		POPAD                   ;
		IRETD                   ;

;=============================================================================
;
; Sleep - A Public routine that delays the calling process by setting
;       up a timer block with CountDown value and an Exchange to
;       send a message to when the countdown reaches zero.
;       The timer interrupt sends the message and clears the block
;       when it reaches zero.
;       This requires an exchange. The exchange
;       used is the TSS_Exch in the TSS for the current task.
;
DelayCnt	EQU [EBP+0Ch]
;
;
PUBLIC __Sleep:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		MOV EAX, DelayCnt
		CMP EAX, 0					;See if there's no delay
		JE Delay03
		LEA EAX,rgTmrBlks           ;EAX points to timer blocks
		MOV ECX,nTmrBlks            ;Count of timer blocks
		CLD                         ;clear direction flag
Delay01:
		CLI                         ;can't let others interfere
		CMP DWORD PTR [EAX+fInUse],FALSE      ;Empty block?
		JNE Delay02                 ;No  - goto next block
		MOV EBX,DelayCnt			;Get delay count
		MOV [EAX+CountDown],EBX     ;
		MOV DWORD PTR [EAX+fInUse],TRUE       ;Use the Timer Block
		INC nTmrBlksUsed			;Up the blocksInUse count
		MOV ECX,pRunTSS             ;Get TSS_Exch for our use
		MOV EBX,[ECX+TSS_Exch]      ;
		MOV [EAX+TmrRespExch],EBX   ;put it in timer block!
		STI
		PUSH EBX                    ;Pass exchange (for WaitMsg)
		ADD ECX,TSS_Msg             ;Offset of msg area
		PUSH ECX
		CALL FWORD PTR _WaitMsg       ;and Wait for it to come back
		MOV EAX,ErcOk               ;all is well
		JMP Delay03
Delay02:
		STI
		ADD EAX,sTmrBlk
		LOOP Delay01                ;unless were done
		MOV EAX,ErcNoMoreTBs        ;Sorry, out of timer blocks
Delay03:
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 4                      ;FAR return from publics

;=============================================================================
;
; Alarm - A Public routine that sends a message to the exchange that the
;       caller specifies after the number of ticks specified.
;       The message is NOT repeatable (must be set up each time).
;       The message will always be two DWords with 0FFFFFFFFh in each.
;
;		Alarm(nAlarmExch, AlarmCnt):dErc
;
AlarmExch	EQU [EBP+10h]
AlarmCnt	EQU [EBP+0Ch]
;
;
PUBLIC __Alarm:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		MOV EAX, AlarmCnt
		CMP EAX, 0					;See if there's no delay
		JE Alarm03
		LEA EAX,rgTmrBlks           ;EAX points to timer blocks
		MOV ECX,nTmrBlks            ;Count of timer blocks
		CLD                         ;clear direction flag
Alarm01:
		CLI                         ;can't let others interfere
		CMP DWORD PTR [EAX+fInUse],FALSE      ;Empty block?
		JNE Alarm02                 ;No  - goto next block
		MOV EBX,AlarmCnt			;Get delay count
		MOV [EAX+CountDown],EBX     ;
		MOV DWORD PTR [EAX+fInUse],TRUE       ;Use the Timer Block
		INC nTmrBlksUsed			;Up the blocksInUse count
		MOV EBX, AlarmExch
		MOV [EAX+TmrRespExch],EBX   ;put it in timer block!
		STI                         ;It's OK to interrupt now
		MOV EAX,ErcOk               ;all is well
		JMP Alarm03
Alarm02:
		STI                         ;It's OK to interrupt now
		ADD EAX,sTmrBlk
		LOOP Alarm01                ;unless were done
		MOV EAX,ErcNoMoreTBs        ;Sorry, out of timer blocks
Alarm03:
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 8                      ;
;
;=============================================================================
;
; KillAlarm - A Public routine that kills an alarm message that is
;       set to be sent to an exchange.  ALL alarms set to fire off
;		to that exchange are killed. If the alarm is already queued
;		through the kernel, NOTHING will stop it...
;
;		KillAlarm(nAlarmExch):dErc
;
KAlarmExch	EQU [EBP+0Ch]
;
;
PUBLIC __KillAlarm:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		CMP nTmrBlksUsed, 0			;No blocks in use
		JE KAlarm03					; so we get out!
		MOV EBX,KAlarmExch			;Get exchange for killing alarms to
		LEA EAX,rgTmrBlks           ;EAX points to timer blocks
		MOV ECX,nTmrBlks            ;Count of timer blocks
		CLD                         ;clear direction flag
KAlarm01:
		CLI                         ;can't let others interfere
		CMP DWORD PTR [EAX+fInUse],TRUE		;Block in use?
		JNE KAlarm02                ;No - goto next block
		CMP [EAX+TmrRespExch],EBX   ;Does this match the Exchange?
		JNE KAlarm02
		MOV DWORD PTR [EAX+fInUse],FALSE     ;Make Empty
		DEC nTmrBlksUsed			;Make blocksInUse correct
KAlarm02:
		STI                         ;It's OK to interrupt now
		ADD EAX,sTmrBlk
		LOOP KAlarm01               ;unless were done
KAlarm03:
		XOR EAX,EAX			        ;ALl done -- ErcOk
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 4                      ;
;
;=============================================================================
;   MicroDelay(dDelay):derror  Delay count is in 15 us increments
;
; The timing for this delay is based on the toggle of the refresh bit
; from the System Status port.  The toggle is approximately 15us.  This
; means this call will not be very accurate for values less than
; 3 or 4 (45 to 60 microseconds).  BUT, it's still very much needed.
;
PUBLIC __MicroDelay:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		MOV ECX, [EBP+0Ch]			;Get delay count
		CMP ECX, 0
		JE MDL01					;get out if they came in with 0!
MDL00:
		IN AL, 61h					;Get system status port
		AND AL, 10h					;check refrest bit
		CMP AH, AL					;Check toggle of bit
		JE MDL00					;No toggle yet
		MOV AH, AL					;Toggle! Move to AH for next compare
		LOOP MDL00
MDL01:
		XOR EAX, EAX
		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 4                      ;

;=============================================================================
;   GetCMOSTime(pdTimeRet):derror
;
; The Time is returned from the CMOS clock as a DWord.
; Low order byte is the Seconds (BCD),
; Next byte is the Minutes (BCD),
; Next byte is the Hours (BCD),
; High order byte is 0.
;
pCMOSTimeRet EQU [EBP+12]

PUBLIC __GetCMOSTime:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		XOR EBX, EBX				;Clear time return

	    MOV EAX,04h					;Hours
		OUT 70h,AL
		IN AL,71h
		MOV BL, AL

		SHL EBX, 8					;Minutes
	    MOV EAX,02h
		OUT 70h,AL
		IN AL,71h
		MOV BL,AL

		SHL EBX, 8              	;Seconds
	    MOV EAX,00h
		OUT 70h,AL
		IN AL,71h
		MOV BL,AL

		MOV ESI, pCMOSTimeRet		;Give 'em the time
		MOV [ESI], EBX
		XOR EAX, EAX				; No Error

		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 4                      ;

;=============================================================================
;   GetCMOSDate(pdTimeRet):derror
;
; The Date is returned from the CMOS clock as a DWord.
; Low order byte is the Day of Week (BCD 0-6 0=Sunday),
; Next byte is the Day (BCD 1-31),
; Next byte is the Month (BCD 1-12),
; High order byte is year (BCD 0-99).
;
pCMOSDateRet EQU [EBP+12]

PUBLIC __GetCMOSDate:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		XOR EBX, EBX				;Clear date return

	    MOV EAX,09h					;Year
		OUT 70h,AL
		IN AL,71h
		MOV BL, AL
		SHL EBX, 8					;

	    MOV EAX,08h					;Month
		OUT 70h,AL
		IN AL,71h
		MOV BL, AL
		SHL EBX, 8

	    MOV EAX,07h					;Day of month
		OUT 70h,AL
		IN AL,71h
		MOV BL,AL
		SHL EBX, 8              	;

	    MOV EAX,06h					;Day of week
		OUT 70h,AL
		IN AL,71h
		MOV BL,AL

		MOV ESI, pCMOSDateRet		;Give 'em the time
		MOV [ESI], EBX
		XOR EAX, EAX				; No Error

		MOV ESP,EBP                 ;
		POP EBP                     ;
		RETF 4                      ;

;=============================================================================
;   GetTimerTick(pdTickRet):derror
;
; The Current Timer Tick is returned (it's a DWord).
;
pTickRet EQU [EBP+12]

PUBLIC __GetTimerTick:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		MOV ESI, pTickRet
		MOV EAX, TimerTick
		MOV [ESI], EAX
		XOR EAX, EAX				;No Error
		POP EBP                     ;
		RETF 4                      ;

;=======================================================
; Noise makers:
; BEEP is a FAR call with no params that beeps the speaker
;      at 300 Hz for 350ms.
; TONE is a FAR call with two parmas:
;      1) FREQ in HZ (a DD)
;      2) "ON" time in 10ms increments (a DD)
;
;The clock freq to Timer 2 is 1.193182 Mhz
;To find the divisor of the clock, divide 1.193182Mhz by Desired Freq.
;
;================================================
;This does all work for BEEP and TONE
;EBX needs FREQ in HERTZ
;ECX needs length of tone in 10ms increments

BEEP_Work:
		MOV AL, 10110110b	    ;Timer 2, LSB, MSB, Binary
		OUT 43h, AL
		XOR EDX, EDX
		MOV EAX, 1193182	    ;1.193182Mhz
		DIV EBX 		    ;DIVISOR is in EBX (Freq)
		OUT 42h, AL		    ;Send quotient (left in AX)
		MOV AL, AH
		NOP
		NOP
		NOP
		NOP
		OUT 42h, AL
		IN AL, 61h
		OR AL, 00000011b
		PUSH EAX
		POP EAX
		OUT 61h, AL
		PUSH ECX		    ;
		CALL FWORD PTR _Sleep	    ;ECX is TIME ON in 50ms incs.
		IN AL, 61h
		NOP
		NOP
		NOP
		NOP
		AND AL, 11111100b
		OUT 61h, AL
		RETN


;================================================
PUBLIC __Beep:
		PUSH EBP                    ;
		MOV EBP,ESP                 ;
		MOV EBX, 800				;Freq
		MOV ECX, 35					;350ms
		CALL Beep_Work
		POP EBP                     ;
		RETF

;================================================
;TONE allows the caller to specify a frequency and duration of a tone
;from the speaker.  This call uses Beep_Work (A NEAR call to do the job)
; PARAM 1 is a DD which is the FREQUENCY in HERTZ
; PARAM 2 is a DD which is the length of the tone in 50ms increments
;
ToneFreq	EQU [EBP+10h]
ToneTime	EQU [EBP+0Ch]

PUBLIC __Tone:
		PUSH EBP                        ;
		MOV EBP,ESP                     ;
		MOV EBX, ToneFreq
		MOV ECX, ToneTime
		CALL Beep_Work
		POP EBP                         ;
		RETF 8


;====================== Module End =================================
