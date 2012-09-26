;------------------------------------
; Request Blocks are 64 bytes long.  This is a
; structure allocated from OS memory for "Request" type
; messages sent to, and received from, System Services.
; 64 RqBlks will fit in one 4K memory page.
;
;
nRQBs			EQU 128		;number of request blocks (All Dynamic)
sRQB			EQU 64
ServiceExch		EQU 0	;DD 0h    ; System Exchange (OS Fill from name supplied)
RespExch		EQU 4	;DD 0h			; Exchange to respond to (User Fill)
RqOwnerJob		EQU 8 	;DD 0h			; JobNum of Owner of the RqBlk - (OS Fill)
ServiceRoute	EQU 12	;DD 0h			; Used for net routing (Rsvd for now)
pRqHndlRet		EQU 16	;DD 0h			; For User to identify Rq at Wait
dData0			EQU 20	;DD 0h			; User fill / Srvc Defined (No Pointers)
dData1			EQU 24	;DD 0h			; User fill / Srvc Defined (No Pointers)
dData2			EQU 28	;DD 0h			; User fill / Srvc Defined (No Pointers)
ServiceCode		EQU 32	;DW 0h			; System Service Command Number (User Fill)
npSend			EQU 34	;DB 0h			; Number of Send PbCbs
npRecv			EQU 35	;DB 0h			; Number of Recv PbCbs
pData1			EQU 36	;DD 0h			; User fill / Srvc Defined
cbData1			EQU 40	;DD 0h			; User fill / Srvc Defined
pData2			EQU 44	;DD 0h			; User fill / Srvc Defined
cbData2			EQU 48	;DD 0h			; User fill / Srvc Defined
Rsvd2			EQU 52	;DD 0h			;
Rsvd3			EQU 56	;DD 0h			;
pNextRQB		EQU 60	;DD 0h			; Pointer to next Free Rq Blk (NIL if last)

EXTRN  pRQBs	DD NEAR					; RQBs are in allocated memory

;============ Module End ====================
