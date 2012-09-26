;   MMURTL Operating System Source Code
;   Copyright 1991,1992,1993,1994 Richard A. Burgess
;   ALL RIGHTS RESERVED    Version 1.0
;
.DATA
.INCLUDE MOSEDF.INC
.ALIGN DWORD
;These are the Equates and data are for the device
;driver entry and exit code.

nDevices		EQU	24		;Number of Device Control Blocks

;Device Control Block.  This structure defines the DCB layout for all
;device drivers.  Each driver is responsible for creating and initially
;setting up a DCB prior to calling InitDevDr.  The OS maintains an array
;of pointers to the device drivers.  The physical device number
;(0-nDevices) is determined by the position in the array. An array
;position that contains a NULL pointer indicates a device is not installed.
;For instance, if the first pointer in the rgpDCBs is NULL, device "0"
;is not available. The DCB also maintains a 12 byte name for the device
;which the installer specifies.  A PUBLIC call is provided to translate
;the name to a device number so callers and services can find the
;physical number from the name.
;The name of a device is case aware but NOT case sensitive
;
; The DCB structure layout follows:
;
sDevCB		EQU 64		;size of the Device Control Block

;Define offsets into structure

DevName		EQU 0  ;DB 12 DUP (0)	;Device Name
sbDevName	EQU 12 ;DB 0		;Length of Devname
DevType		EQU 13 ;DB 0		;1 = RANDOM, 2 = SEQUENTIAL, 0 = No Device
nBPB		EQU 14 ;DW 0		;Bytes per Block (1 to 65535 max)
LastDevErc	EQU 16 ;DD 0		;Last error code from an operation
nDevBlocks	EQU 20 ;DD 0		;Number of blocks in device (0 for sequential)
			;24
pDevOp		EQU 24 ;DD 0		;Offset to device Operation handler
pDevInit	EQU 28 ;DD 0		;Offset to device Init handler
pDevSt		EQU 32 ;DD 0		;Offset to device Status handler
			;36
fDevReent	EQU 36 ;DB 0		;Is device handler reentrant?
fSingleUser	EQU 37 ;DB 0		;Is device assignable?
wJob		EQU 38 ;DW 0		;If assignable, is it? (0 for no, else Job Num)
DevSemExch	EQU 40 ;DD 0		;Exch for device semaphore (if not reentrant)
DevSemMsg	EQU 44 ;DD 0		;Msg holder for msg from WAIT on queued Tasks
DevSemMsg1	EQU 48 ;DD 0		;Msg holder (second DWord)
			;52

rgpDCBs		DD nDevices DUP (0)		;
rgReDIR		DD nDevices DUP (0)		;For redirection of devices

;Standard Devices are:

;#		Device					Standard name

;0		NULL device 			NUL		(OS built-in)
;1		Keyboard (sequential)	KBD		(OS built-in, ReadOnly)
;2		Video (sequential)		VID		(OS built-in, WriteOnly)
;3		Printer (parallel 1)	LPT		(OS built-in)
;4		Printer (parallel 2)	LPT2	(OS built-in)
;5		RS-232 1				COM1	(OS built-in)
;6		RS-232 2				COM2	(OS built-in)
;7		RS-232 3				COM3	(OS built-in)
;8		RS-232 4				COM4	(OS built-in)
;9
;10		Floppy					FD0 	(OS built-in)
;11		Floppy					FD1 	(OS built-in)
;12		Hard disk				HD0 	(OS built-in)
;13		Hard disk				HD1 	(OS built-in)
;14
;15
;16
;17
;18
;19
;20
;21
;22
;23
;------------- End Device Driver Data, Begin Code -----------------
.CODE
;
;
;InitDevDr is called ONCE by a device driver after loading. pDCBs points to
;an array of DCBs (one for each device the driver handles).  The devices
;will be numbered dDevNum, dDevNum+1, etc.
;For example, if the hard disk driver controls two disks it calls it once
;pointing to an array of two DCBs. The DCb must be contiguous in RAM!!
;
;  InitDevDr(dDevNum,  pDCBs,  nDevices, dfReplace):dError
;            EBP+24    EBP+20  EBP+16	 EBP+12     sParam 16
;
;Local vars
dDevX    EQU DWORD PTR [EBP-4]
prgDevs  EQU DWORD PTR [EBP-8]
nDevs    EQU DWORD PTR [EBP-12]
dExchTmp EQU DWORD PTR [EBP-16]

PUBLIC __InitDevDr:
		PUSH EBP                		;
		MOV EBP,ESP             		;
		SUB ESP, 16
		MOV EAX, [EBP+24]				;Set up local vars
		MOV dDevX, EAX
		MOV EAX, [EBP+20]
		MOV prgDevs, EAX
		MOV EAX, [EBP+16]
		MOV nDevs, EAX

InitDev00:
		CMP dDevX, nDevices				;Valid DCB num?
		JB InitDev01
		MOV EAX, ErcBadDevNum			;Not valid DCB number
		JMP InitDevEnd

InitDev01:	;Now check to see if device is already installed
		;and whether it's to be replaced

		LEA EBX, rgpDCBs				;Point EBX to rgpDCB
		MOV EAX, dDevX					;dDevNum
		SHL EAX, 2
		ADD EBX, EAX
		CMP DWORD PTR [EBX], 0			;pDCBx = 0 if not used yet
		JZ InitDev02					;Empty, OK to use
		CMP DWORD PTR [EBP+12], 0		;OK to replace existing driver?
		JNZ InitDev02					;Yes
		MOV EAX, ErcDCBInUse			;No - error exit
		JMP InitDevEnd

InitDev02:	;If we got here, we can check DCB items then move ptr

		MOV EAX, prgDevs				;EAX points to DCB
		CMP BYTE PTR [EAX+sbDevName],12 ;Check Device name size
		JA InitDev03
		CMP BYTE PTR [EAX+sbDevName], 0 ;is Devname OK?
		JA InitDev04
InitDev03:
		MOV EAX, ErcBadDevName
		JMP InitDevEnd

InitDev04:
		;Now see if there are more devices for this driver

		DEC nDevs						;Decrement nDevices
		JZ InitDev05					;NONE left
		ADD prgDevs, 64					;Next caller DCB
		INC dDevX						;Next devnum
		JMP SHORT InitDev00				;

		;All error checking on DCB(s) should be done at this point

InitDev05:								;Alloc Exch if driver in NOT reentrant
		MOV EBX, [EBP+20]				;pDCBs
		CMP BYTE PTR [EBX+fDevReent], 0
		JNZ InitDev06					;device IS reentrant!
		LEA EAX, dExchTmp				;Allocate device Exchange
		PUSH EAX						;into temp storage
		CALL FWORD PTR _AllocExch
		CMP EAX, 0
		JNZ SHORT InitDevEnd

InitDev06:
		;All went OK so far, now move the DCB pointer(s) into array
		; and assign exchange from temp storage to each DCB

		MOV EAX, [EBP+16]				;nDevices
		MOV nDevs, EAX					;Set nDev to number of devices again
		LEA EBX, rgpDCBs				;Point EBX to OS rgpDCBs
		MOV EAX, [EBP+24]				;dDevNum
		SHL EAX, 2
		ADD EBX, EAX					;EBX now points to correct pointer
		MOV EAX, [EBP+20]				;EAX points to first DCB
		MOV ECX, dExchTmp				;ECX has semaphore exchange
InitDev07:
		;Now that EBX, EAX and ECX are set up, loop through each
		;DCB (if more than 1) and set up OS pointer to it, and
		;also place Exchange into DCB.  This is the same exchange
		;for all devices that one driver controls.

		MOV [EAX+DevSemExch], ECX
		MOV	[EBX], EAX
		ADD EBX, 4						;next p in rgp of DCBs
		ADD EAX, 64						;next DCB
		DEC	nDevs
		JNZ InitDev07					;Any more DCBs??
		XOR EAX, EAX					;Set up for no error

		;If the device driver was NOT reentrant
		;we send a semaphore message to the exchange for
		;the first customer to use.

		MOV EBX, [EBP+20]				;pDCBs
		CMP BYTE PTR [EBX+fDevReent], 0
		JNZ InitDev06					;device IS reentrant!
		PUSH ECX						;ECX is still the exchange
		PUSH 0FFFFFFFEh					;Dummy message
		PUSH 0FFFFFFFEh
		CALL FWORD PTR _SendMsg			;Let erc in EAX fall through (Was ISend)

InitDevEnd:
		MOV ESP,EBP             		;
		POP EBP                 		;
		RETF 16							;
;
;=======================================================================
; Device Driver call for DeviceInit.  Some up-front checking is done and
; then this call is forwarded to the destination driver
;
;  DeviceInit(dDevNum, pInitData,  sdInitData);
;             EBP+20   EBP+16      EBP+12         Count = 12
;
PUBLIC __DeviceInit:					;
		PUSH EBP            	   	 	;
		MOV EBP,ESP         	    	;
		CMP DWORD PTR [EBP+20], nDevices		;Valid Device number?
		JB DevInit01
		MOV EAX, ErcBadDevNum			;Sorry no valid DCB
		JMP DevInitEnd
DevInit01:
		LEA EAX, rgpDCBs
		MOV EBX, [EBP+20]				;
		SHL EBX, 2						;
		ADD EAX, EBX					;
		MOV EBX, [EAX]					;now EBX points to DCB (maybe)
		CMP EBX, 0						;Is there a pointer to a DCB?
		JNZ DevInit1A					;Yes
		MOV EAX, ErcNoDriver			;NO driver!
		JMP DevInitEnd
DevInit1A:
		CMP BYTE PTR [EBX+DevType], 0	;Is there a physical device?
		JNZ DevInit02
		MOV EAX, ErcNoDevice
		JMP DevInitEnd

DevInit02:	;All looks good with device number
		;so we check to see if driver is reentrant. If not we
		;call WAIT to get semaphore ticket...

		CMP BYTE PTR [EBX+fDevReent], 0
		JNZ DevInit03					;Device IS reentrant
		PUSH EBX						;save ptr to DCB
		PUSH DWORD PTR [EBX+DevSemExch]		;Push exchange number
		LEA EAX, [EBX+DevSemMsg]		;Ptr to message area
		PUSH EAX
		CALL FWORD PTR _WaitMsg			;Get semaphore ticket
		POP EBX							;Get DCB ptr back
		CMP EAX, 0
		JNE DevInitEnd					;Serious kernel error!

DevInit03:
		PUSH EBX						;Save ptr to DCB
		PUSH DWORD PTR [EBP+20]			;Push all params for call to DD
		PUSH DWORD PTR [EBP+16]
		PUSH DWORD PTR [EBP+12]
		CALL DWORD PTR [EBX+pDevInit]
		POP EBX							;Get ptr to DCB back into EBX
		PUSH EAX						;save error (if any)

		CMP BYTE PTR [EBX+fDevReent], 0	;Reentrant?
		JNZ DevInit04					;YES

		PUSH DWORD PTR [EBX+DevSemExch]		;No, Send semaphore message to Exch
		PUSH 0FFFFFFFEh					;Bogus Message
		PUSH 0FFFFFFFEh					;
		CALL FWORD PTR _SendMsg			;Ignore kernel error (unlikely)
DevInit04:
		POP EAX							;Get device error back

DevInitEnd:
		MOV ESP,EBP             		;
		POP EBP                	 		;
		RETF 12                 		;dump params


;=======================================================================
; Device Driver call for DeviceOp.  Some up-front checking is done and
; then this call is forwarded to the destination driver
;
;  DeviceOp(dDevice, dOpNum, dLBA,   dnBlocks, pData);
;           EBP+28   EBP+24  EBP+20  EBP+16    EBP+12         Count = 20
;
PUBLIC __DeviceOp:
		PUSH EBP            	    	;
		MOV EBP,ESP         	    	;
		CMP DWORD PTR [EBP+28], nDevices  ;Valid Device number?
		JB DevOp01
		MOV EAX, ErcBadDevNum			;Sorry no valid DCB
		JMP DevOpEnd
DevOp01:
		LEA EAX, rgpDCBs
		MOV EBX, [EBP+28]				;
		SHL EBX, 2						;
		ADD EAX, EBX					;
		MOV EBX, [EAX]					;EBX points to DCB (maybe)
		CMP EBX, 0						;Is there a pointer to a DCB?
		JNZ DevOp1A						;Yes
		MOV EAX, ErcNoDriver			;NO driver!
		JMP DevOpEnd
DevOp1A:
		CMP BYTE PTR [EBX+DevType], 0	;Is it valid Device
		JNZ DevOp02
		MOV EAX, ErcNoDevice
		JMP DevOpEnd

DevOp02:	;All looks good with device number
			;so we check to see if driver is reentrant. If not we
			;call WAIT to get semaphore ticket...

		CMP BYTE PTR [EBX+fDevReent], 0
		JNZ DevOp03						;Device IS reentrant
		PUSH EBX						;save ptr to DCB
		PUSH DWORD PTR [EBX+DevSemExch]	;Push exchange number
		LEA EAX, [EBX+DevSemMsg]		;Ptr to message area
		PUSH EAX
		CALL FWORD PTR _WaitMsg			;Get semaphore ticket
		POP EBX							;Get DCB ptr back
		CMP EAX, 0
		JNE DevOpEnd					;Serious kernel error!

DevOp03:
		PUSH EBX						;Save ptr to DCB
		PUSH DWORD PTR [EBP+28]			;Push all params for call to DD
		PUSH DWORD PTR [EBP+24]			;
		PUSH DWORD PTR [EBP+20]
		PUSH DWORD PTR [EBP+16]
		PUSH DWORD PTR [EBP+12]
		CALL DWORD PTR [EBX+pDevOp]
		POP EBX							;Get ptr to DCB back into EBX
		PUSH EAX						;save error (if any)

		CMP BYTE PTR [EBX+fDevReent], 0	;Reentrant?
		JNZ DevOp04						;YES
		PUSH DWORD PTR [EBX+DevSemExch]	;Send semaphore message to Exch
		PUSH 0FFFFFFFEh					;Bogus Message
		PUSH 0FFFFFFFEh					;
		CALL FWORD PTR _SendMsg			;Ignore kernel error
DevOp04:
		POP EAX							;Get device error back

DevOpEnd:
		MOV ESP,EBP             		;
		POP EBP                 		;
		RETF 20                 		;dump params

;=======================================================================
; Device Driver call for DeviceStat.  Some up-front checking is done and
; then this call is forwarded to the destination driver
;
;  DeviceStat(dDevNum, pStatRet,  dStatMax, pdStatRet);
;             EBP+24   EBP+20     EBP+16    EBP+12      Count = 16
;
PUBLIC __DeviceStat:					;
		PUSH EBP            	    	;
		MOV EBP,ESP         	    	;
		CMP DWORD PTR [EBP+24], nDevices	;Valid Device number?
		JB DevStat01
		MOV EAX, ErcBadDevNum			;Sorry no valid DCB
		JMP DevStatEnd
DevStat01:
		LEA EAX, rgpDCBs				;Ptr to array of ptrs to DCBs
		MOV EBX, [EBP+24]				;Device number
		SHL EBX, 2						;Times 4 (index into 4 byte ptrs)
		ADD EAX, EBX					;Add em up so EAX points to a pointer!
		MOV EBX, [EAX]					;now EBX points to DCB (maybe)
		CMP EBX, 0						;Is there a pointer to a DCB?
		JNZ DevStat1A					;Yes
		MOV EAX, ErcNoDriver			;NO driver!
		JMP DevStatEnd
DevStat1A:
		CMP BYTE PTR [EBX+DevType], 0	;Is it valid Device
		JNZ DevStat02
		MOV EAX, ErcNoDevice
		JMP DevStatEnd

DevStat02:	;All looks good with device driver DCB
		;so we check to see if driver is reentrant. If not we
		;call WAIT to get semaphore ticket...

		CMP BYTE PTR [EBX+fDevReent], 0
		JNZ DevStat03					;Device IS reentrant
		PUSH EBX						;save ptr to DCB
		PUSH DWORD PTR [EBX+DevSemExch]	;Push exchange number
		LEA EAX, [EBX+DevSemMsg]		;Ptr to message area
		PUSH EAX
		CALL FWORD PTR _WaitMsg			;Get semaphore ticket
		POP EBX							;Get DCB ptr back
		CMP EAX, 0
		JNE DevStatEnd					;Serious kernel error!

DevStat03:
		PUSH EBX						;Save ptr to DCB
		PUSH DWORD PTR [EBP+24]			;Push all params for call to DD
		PUSH DWORD PTR [EBP+20]
		PUSH DWORD PTR [EBP+16]
		PUSH DWORD PTR [EBP+12]
		CALL DWORD PTR [EBX+pDevSt]
		POP EBX							;Get ptr to DCB back into EBX
		PUSH EAX						;save error (if any)
		CMP BYTE PTR [EBX+fDevReent], 0	;Reentrant?
		JNZ DevStat04					;YES
		PUSH DWORD PTR [EBX+DevSemExch]	;Send semaphore message to Exch
		PUSH 0FFFFFFFEh					;Bogus Message
		PUSH 0FFFFFFFEh					;Bogus Message
		CALL FWORD PTR _SendMsg			;Ignore kernel error (unlikely)
DevStat04:
		POP EAX						;Get device error back

DevStatEnd:
		MOV ESP,EBP             	;
		POP EBP                 	;
		RETF 16                 	;dump params


;========= END of MODULE ================
