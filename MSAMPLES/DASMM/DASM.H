/* 386 D-Group ASSEMBLER.  Header file.
   Copyright 1992 R.A. Burgess
*/

 /* The DASM parser returns a TOKEN type and fills in certain
   global variables with the values found.  The token type is
   placed in Token as well as being returned to the caller.
*/

/* The following are the TOKEN types returned by the parser or
   the expression analyzer :
*/

#define ZERO	0	/* Parser returned NOTHING (blank or EOL) */
#define INSTRU	1	/* 386 instruction. Number placed in TInst. */
#define REGIST	2	/* 386 register is placed in TReg */
#define SYMBOL	3	/* In symbol table. Sym number placed in TSymnum */
#define LSYMBOL	4	/* In symbol table. LSym number placed in TSymnum */
#define STRING	5	/* 'quoted' string of characters left in TString */
#define NUMBER	6	/* Valid value placed in TNumber */
#define NUMOFF	7	/* Valid TNumber derived from OFFSET (needs fix) */
#define UNKSYM	8	/* Qualifies as a sym but not found. Left in TString */
#define SYMOFF	9   /* Symbol offset value (from Expression analyzer) */
#define ERROR	10	/* Bad char found in line. Line terminated by Parse(). */

/* alphabetized list of reserved words excluding registers and
   instructions.  They must be alphabetized for binary search
   function to work. They begin with #89 so they don't conflict
   with other token values. */

#define nreserved 30
#define srsvd 8			/* size of a reserved storage */
#define nrsvd1 94		/* must be the first number of rgReserved */
char rgReserved[nreserved][srsvd]  = {		/* 109 bytes */
         "ALIGN",     /*  94 */
         "BYTE",      /*  95 */
         "CODE",      /*  96 */
         "DATA",      /*  97 */
         "DB",        /*  98 */
         "DD",        /*  99 */
         "DF",        /* 100 */
         "DUP",       /* 101 */
         "DW",        /* 102 */
         "DWORD",     /* 103 */
	     "END",       /* 104 */
         "EQU",       /* 105 */
         "EXTRN",     /* 106 */
         "FAR",       /* 107 */
         "FWORD",     /* 108 */
         "INCLUDE",   /* 109 */
         "NAME",      /* 110 */
         "NEAR",      /* 111 */
         "OFFSET",    /* 112 */
         "PARA",      /* 113 */
         "PTR",       /* 114 */
		 "PUBLIC",    /* 115 */
         "SEARCH",    /* 116 */
         "SHORT",     /* 117 */
         "STACK",     /* 118 */
         "START",     /* 119 */
         "USE16",     /* 120 */
         "USE32",     /* 121 */
         "VIRTUAL",   /* 122 */
         "WORD",      /* 123  up to 127 MAX */
         };

/* The following reserved "words" are recognized by Parse.
   This list MUST agree with the numbering of the table
   rgReserved above.
*/

#define  rALIGN		94	/* .ALIGN n - Align code segment   */
#define  rBYTE		95	/* for forcing types on operands   */
#define  rCODE		96	/* .CODE - begin/continue code segment   */
#define	 rDATA		97	/* .DATA - begin/continue data segment   */
#define  rDB		98	/* storage of byte(s) or (strings) */
#define  rDD		99	/* double words */
#define  rDF		100 /* FWord (OFF:SEL) */
#define  rDUP		101	/* duplicated storage of DB, DW or DD */
#define  rDW		102	/* storage of words follows */
#define  rDWORD		103	/* for forcing types on operands */
#define  rEND		104	/* .END - file end - no more segments    */
#define  rEQU		105	/* EQUATE for numbers/addresses only */
#define  rEXTRN     106  /* for EXTERNAL definition */
#define  rFAR		107	/* FAR operator for calls */
#define  rFWORD		108	/* for forcing types on operands */
#define  rINCLUDE	109	/* .INCLUDE - Include file for assembly  */
#define  rNAME		110	/* .NAME to name a segment */
#define  rNEAR		111	/* NEAR operator for calls (default) */
#define  rOFFSET	112	/* offset in segment of ... */
#define  rPARA		113	/* PTR operator for mem ops */
#define  rPTR		114	/* PTR operator for mem ops */
#define  rPUBLIC    115	/* for PUBLIC declarations */
#define  rSEARCH	116	/* SEARCH command for Lib files  */
#define  rSHORT		117	/* SHORT operator for relative jumps   */
#define  rSTACK		118	/* .START - Entry point for execution    */
#define  rSTART		119	/* .STACK - Sets initial stack size   */
#define  rUSE16     120 /* .USE16 (16 bit segment) */
#define  rUSE32     121 /* .USE16 (16 bit segment) */
#define  rVIRTUAL	122 /* .VIRTUAL n - Data @ virtual address n */
#define  rWORD		123 /*  WORD operator for mem ops */

/* Special characters. They are treated as reserved words but are
   not searched alphabetically.  They must maintain their ASCII
   values (other tokens are numbers "around" them).
*/

#define  DOLLAR		0x24   /* 36 */
#define  SQUOTE		0x27   /* 39 */
#define  OPENRND 	0x28   /* 40 */
#define  CLOSRND 	0x29   /* 41 */
#define  STAR		0x2a   /* 42 */
#define  PLUS		0x2b   /* 43 */
#define  COMMA		0x2c   /* 44 */
#define  MINUS		0x2d   /* 45 */
#define  DOT 		0x2e   /* 46 */
#define  SLASH		0x2f   /* 47 */
#define  COLON   	0x3a   /* 58 */
#define  SEMI		0x3b   /* 59 */
#define  OPENSQR	0x5b   /* 91 */
#define  CLOSSQR	0x5d   /* 93 */

/* The following bits identify symbol types in GST & LST */

#define CLABEL  0x0001  /* Code label - SymPtr has Offset in Code seg */
#define DLABEL  0x0002  /* Data label - SymPtr has Offset in Data seg */
#define MACRO   0x0004  /* No offset associated -  pts to string */
#define sBYTE	0x0008  /* Data label for BYTE (8 bit)   */
#define sWORD	0x0010  /* Data label for WORD (16 bit)  */
#define sDWORD	0x0020  /* Data label for DWORD (32 bit) */
#define sFWORD	0x0040  /* Data label for FWORD (48 bit) */
#define tEXTRN  0x0080  /* Defined External, Public not found yet */
#define tFAR    0x0100  /* Defined as FAR, default is near */
#define tPUBLIC 0x0200  /* Defined as PUBLIC */


/* This is an alphabetical array of all the 386 registers.
   It it used for a binary search for a register name. If
   the beginning number (128) is changed here you must make
   sure to change the define nreg1 to match to the search
   routine will give you the correct number They start at 128
   so they don't conflict with operand types in instruction table
   rgINS[].
*/

#define nregs 52
#define sregs 4
#define nreg1 128

char rgreg[nregs][4]  = {		/* 176 bytes */

      "AH",    /* 128 */
      "AL",    /* 129 */
      "AX",    /* 130 */
	  "BH",    /* 131 */
	  "BL",    /* 132 */
	  "BP",    /* 133 */
	  "BX",    /* 134 */
	  "CH",    /* 135 */
	  "CL",    /* 136 */
	  "CR0",   /* 137 */
	  "CR1",   /* 138 */
	  "CR2",   /* 139 */
      "CR3",   /* 140 */
	  "CS",    /* 141 */
	  "CX",    /* 142 */
	  "DH",    /* 143 */
	  "DI",    /* 144 */
	  "DL",    /* 145 */
	  "DR0",   /* 146 */
	  "DR1",   /* 147 */
	  "DR2",   /* 148 */
	  "DR3",   /* 149 */
	  "DR6",   /* 150 */
	  "DR7",   /* 151 */
	  "DS",    /* 152 */
	  "DX",    /* 153 */
      "EAX",   /* 154 */
	  "EBP",   /* 155 */
	  "EBX",   /* 156 */
	  "ECX",   /* 157 */
	  "EDI",   /* 158 */
	  "EDX",   /* 159 */
	  "ESI",   /* 160 */
	  "ES",    /* 161 */
	  "ESP",   /* 162 */
	  "FS",    /* 163 */
	  "GS",    /* 164 */
	  "SI",    /* 165 */
	  "SP",    /* 166 */
	  "SS",    /* 167 */
	  "TR6",   /* 168 */
	  "TR7"};  /* 169 */

#define rAH  128
#define rAL  129
#define rAX  130
#define rBH  131
#define rBL  132
#define rBP  133
#define rBX  134
#define rCH  135
#define rCL  136
#define rCR0 137
#define rCR1 138
#define rCR2 139
#define rCR3 140
#define rCS  141
#define rCX  142
#define rDH  143
#define rDI  144
#define rDL  145
#define rDR0 146
#define rDR1 147
#define rDR2 148
#define rDR3 149
#define rDR6 150
#define rDR7 151
#define rDS  152
#define rDX  153
#define rEAX 154
#define rEBP 155
#define rEBX 156
#define rECX 157
#define rEDI 158
#define rEDX 159
#define rESI 160
#define rES  161
#define rESP 162
#define rFS  163
#define rGS  164
#define rSI  165
#define rSP  166
#define rSS  167
#define rTR6 168
#define rTR7 169

/* This is an alaphebetical array of all the 386 instructions */
/* It it used for a binary search for an instruction  */

#define ninst 208
#define sinst 7					/* size of a instruction storage */

long rgInsLookUp[ninst+1];		/* Lookup for instruction table */

char rginst[ninst][sinst] = {	/*  1456 bytes */

    "AAA",     /* 001 */
    "AAD",     /* 002 */
    "AAM",     /* 003 */
	"AAS",     /* 004 */
	"ADC",     /* 005 */
	"ADD",     /* 006 */
	"AND",     /* 007 */
	"ARPL",    /* 008 */

	"BOUND",   /* 009 */
	"BSF",     /* 010 */
	"BSR",     /* 011 */
	"BT",      /* 012 */
	"BTC",     /* 013 */
	"BTR",     /* 014 */
	"BTS",     /* 015 */

	"CALL",    /* 016 */
	"CBW",     /* 017 */
	"CDQ",     /* 018 */
	"CLC",     /* 019 */
	"CLD",     /* 020 */
	"CLI",     /* 021 */
	"CLTS",    /* 022 */
	"CMC",     /* 023 */
	"CMP",     /* 024 */
	"CMPS",    /* 025 */
	"CMPSB",   /* 026 */
	"CMPSD",   /* 027 */
	"CMPSW",   /* 028 */
	"CWD",     /* 029 */
	"CWDE",    /* 030 */

	"DAA",     /* 031 */
	"DAS",     /* 032 */
	"DEC",     /* 033 */
	"DIV",     /* 034 */

	"ENTER",   /* 035 */

	"HLT",     /* 036 */

	"IDIV",    /* 037 */
	"IMUL",    /* 038 */
	"IN",      /* 039 */
	"INC",     /* 040 */
	"INS",     /* 041 */
	"INSB",    /* 042 */
	"INSD",    /* 043 */
	"INSW",    /* 044 */
	"INT",     /* 045 */
	"INTO",    /* 046 */
	"IRET",    /* 047 */
	"IRETD",   /* 048 */

	"JA",      /* 049 */
	"JAE",     /* 050 */
	"JB",      /* 051 */
	"JBE",     /* 052 */
	"JC",      /* 053 */
	"JCXZ",    /* 054 */
	"JE",      /* 055 */
	"JECXZ",   /* 056 */
	"JG",      /* 057 */
	"JGE",     /* 058 */
	"JL",      /* 059 */
	"JLE",     /* 060 */
	"JMP",     /* 061 */
	"JNA",     /* 062 */
	"JNAE",    /* 063 */
	"JNB",     /* 064 */
	"JNBE",    /* 065 */
	"JNC",     /* 066 */
	"JNE",     /* 067 */
	"JNG",     /* 068 */
	"JNGE",    /* 069 */
	"JNL",     /* 070 */
	"JNLE",    /* 071 */
	"JNO",     /* 072 */
	"JNP",     /* 073 */
	"JNS",     /* 074 */
	"JNZ",     /* 075 */
	"JO",      /* 076 */
	"JP",      /* 077 */
	"JPE",     /* 078 */
	"JPO",     /* 079 */
	"JS",      /* 080 */
	"JZ",      /* 081 */

	"LAHF",    /* 082 */
	"LAR",     /* 083 */
	"LDS",     /* 084 */
	"LEA",     /* 085 */
	"LEAVE",   /* 086 */
	"LES",     /* 087 */
	"LFS",     /* 088 */
	"LGDT",    /* 089 */
	"LGS",     /* 090 */
	"LIDT",    /* 091 */
	"LLDT",    /* 092 */
	"LMSW",    /* 093 */
	"LOCK",    /* 094 */
	"LODS",    /* 095 */
	"LODSB",   /* 096 */
	"LODSD",   /* 097 */
	"LODSW",   /* 098 */
	"LOOP",    /* 099 */
	"LOOPE",   /* 100 */
	"LOOPNE",  /* 101 */
	"LOOPNZ",  /* 102 */
	"LOOPZ",   /* 103 */
	"LSL",     /* 104 */
	"LSS",     /* 105 */
	"LTR",     /* 106 */

	"MOV",     /* 107 */
	"MOVS",    /* 108 */
	"MOVSB",   /* 109 */
	"MOVSD",   /* 110 */
	"MOVSW",   /* 111 */
	"MOVSX",   /* 112 */
	"MOVZX",   /* 113 */
	"MUL",     /* 114 */

	"NEG",     /* 115 */
	"NOP",     /* 116 */
	"NOT",     /* 117 */

	"OR",      /* 118 */
	"OUT",     /* 119 */
	"OUTS",    /* 120 */
	"OUTSB",   /* 121 */
	"OUTSD",   /* 122 */
	"OUTSW",   /* 123 */

	"POP",     /* 124 */
	"POPA",    /* 125 */
	"POPAD",   /* 126 */
	"POPF",    /* 127 */
	"POPFD",   /* 128 */
	"PUSH",    /* 129 */
	"PUSHA",   /* 130 */
    "PUSHAD",  /* 131 */
    "PUSHF",   /* 132 */
    "PUSHFD",  /* 133 */

    "RCL",     /* 134 */
    "RCR",     /* 135 */
    "REP",     /* 136 */
    "REPE",    /* 137 */
    "REPNE",   /* 138 */
    "REPNZ",   /* 139 */
    "REPZ",    /* 140 */
    "RET",     /* 141 */
    "RETF",    /* 142 */
    "RETN",    /* 143 */
    "ROL",     /* 144 */
    "ROR",     /* 145 */

    "SAHF",    /* 146 */
    "SAL",     /* 147 */
    "SAR",     /* 148 */
    "SBB",     /* 149 */
    "SCAS",    /* 150 */
    "SCASB",   /* 151 */
    "SCASD",   /* 152 */
    "SCASW",   /* 153 */
    "SETA",    /* 154 */
    "SETAE",   /* 155 */
    "SETB",    /* 156 */
    "SETBE",   /* 157 */
    "SETC",    /* 158 */
    "SETE",    /* 159 */
    "SETG",    /* 160 */
    "SETGE",   /* 161 */
    "SETL",    /* 162 */
    "SETLE",   /* 163 */
    "SETNA",   /* 164 */
    "SETNAE",  /* 165 */
    "SETNB",   /* 166 */
    "SETNBE",  /* 167 */
    "SETNC",   /* 168 */
	"SETNE",   /* 169 */
	"SETNG",   /* 170 */
	"SETNGE",  /* 171 */
	"SETNL",   /* 172 */
    "SETNLE",  /* 173 */
    "SETNO",   /* 174 */
    "SETNP",   /* 175 */
    "SETNS",   /* 176 */
    "SETNZ",   /* 177 */
    "SETO",    /* 178 */
    "SETP",    /* 179 */
    "SETPE",   /* 180 */
    "SETPO",   /* 181 */
    "SETS",    /* 182 */
    "SETZ",    /* 183 */
    "SGDT",    /* 184 */
    "SHL",     /* 185 */
    "SHLD",    /* 186 */
    "SHR",     /* 187 */
    "SHRD",    /* 188 */
    "SIDT",    /* 189 */
    "SLDT",    /* 190 */
    "SMSW",    /* 191 */
    "STC",     /* 192 */
    "STD",     /* 193 */
    "STI",     /* 194 */
    "STOS",    /* 195 */
    "STOSB",   /* 196 */
    "STOSD",   /* 197 */
    "STOSW",   /* 198 */
    "STR",     /* 199 */
    "SUB",     /* 200 */

    "TEST",    /* 201 */

    "VERR",    /* 202 */
    "VERW",    /* 203 */

    "WAIT",    /* 204 */
	"XCHG",    /* 205 */
    "XLAT",    /* 206 */
    "XLATB",   /* 207 */
    "XOR"};    /* 208 */

#define xAAA    1
#define xAAD    2
#define xAAM    3
#define xAAS    4
#define xADC    5
#define xADD    6
#define xAND    7
#define xARPL   8

#define xBOUND  9
#define xBSF    10
#define xBSR    11
#define xBT     12
#define xBTC    13
#define xBTR    14
#define xBTS    15

#define xCALL   16
#define xCBW    17
#define xCDQ    18
#define xCLC    19
#define xCLD    20
#define xCLI    21
#define xCLTS   22
#define xCMC    23
#define xCMP    24
#define xCMPS   25
#define xCMPSB  26
#define xCMPSD  27
#define xCMPSW  28
#define xCWD    29
#define xCWDE   30

#define xDAA    31
#define xDAS    32
#define xDEC    33
#define xDIV    34

#define xENTER  35
#define xHLT    36

#define xIDIV   37
#define xIMUL   38
#define xIN     39
#define xINC    40
#define xINS    41
#define xINSB   42
#define xINSD   43
#define xINSW   44
#define xINT    45
#define xINTO   46
#define xIRET   47
#define xIRETD  48

#define xJA     49
#define xJAE    50
#define xJB     51
#define xJBE    52
#define xJC     53
#define xJCXZ   54
#define xJE     55
#define xJECXZ  56
#define xJG     57
#define xJGE    58
#define xJL     59
#define xJLE    60
#define xJMP    61
#define xJNA    62
#define xJNAE   63
#define xJNB    64
#define xJNBE   65
#define xJNC    66
#define xJNE    67
#define xJNG    68
#define xJNGE   69
#define xJNL    70
#define xJNLE   71
#define xJNO    72
#define xJNP    73
#define xJNS    74
#define xJNZ    75
#define xJO     76
#define xJP     77
#define xJPE    78
#define xJPO    79
#define xJS     80
#define xJZ     81

#define xLAHF   82
#define xLAR    83
#define xLDS    84
#define xLEA    85
#define xLEAVE  86
#define xLES    87
#define xLFS    88
#define xLGDT   89
#define xLGS    90
#define xLIDT   91
#define xLLDT   92
#define xLMSW   93
#define xLOCK   94
#define xLODS   95
#define xLODSB  96
#define xLODSD  97
#define xLODSW  98
#define xLOOP   99
#define xLOOPE  100
#define xLOOPNE 101
#define xLOOPNZ 102
#define xLOOPZ  103
#define xLSL    104
#define xLSS    105
#define xLTR    106

#define xMOV    107
#define xMOVS   108
#define xMOVSB  109
#define xMOVSD  110
#define xMOVSW  111
#define xMOVSX  112
#define xMOVZX  113
#define xMUL    114

#define xNEG    115
#define xNOP    116
#define xNOT    117

#define xOR     118
#define xOUT    119
#define xOUTS   120
#define xOUTSB  121
#define xOUTSD  122
#define xOUTSW  123

#define xPOP    124
#define xPOPA   125
#define xPOPAD  126
#define xPOPF   127
#define xPOPFD  128
#define xPUSH   129
#define xPUSHA  130
#define xPUSHAD 131
#define xPUSHF  132
#define xPUSHFD 133

#define xRCL    134
#define xRCR    135
#define xREP    136
#define xREPE   137
#define xREPNE  138
#define xREPNZ  139
#define xREPZ   140
#define xRET    141
#define xRETF   142
#define xRETN   143
#define xROL    144
#define xROR    145

#define xSAHF   146
#define xSAL    147
#define xSAR    148
#define xSBB    149
#define xSCAS   150
#define xSCASB  151
#define xSCASD  152
#define xSCASW  153
#define xSETA   154
#define xSETAE  155
#define xSETB   156
#define xSETBE  157
#define xSETC   158
#define xSETE   159
#define xSETG   160
#define xSETGE  161
#define xSETL   162
#define xSETLE  163
#define xSETNA  164
#define xSETNAE 165
#define xSETNB  166
#define xSETNBE 167
#define xSETNC  168
#define xSETNE  169
#define xSETNG  170
#define xSETNGE 171
#define xSETNL  172
#define xSETNLE 173
#define xSETNO  174
#define xSETNP  175
#define xSETNS  176
#define xSETNZ  177
#define xSETO   178
#define xSETP   179
#define xSETPE  180
#define xSETPO  181
#define xSETS   182
#define xSETZ   183
#define xSGDT   184
#define xSHL    185
#define xSHLD   186
#define xSHR    187
#define xSHRD   188
#define xSIDT   189
#define xSLDT   190
#define xSMSW   191
#define xSTC    192
#define xSTD    193
#define xSTI    194
#define xSTOS   195
#define xSTOSB  196
#define xSTOSD  197
#define xSTOSW  198
#define xSTR    199
#define xSUB    200

#define xTEST   201

#define xVERR   202
#define xVERW   203

#define xWAIT   204
#define xXCHG   205
#define xXLAT   206
#define xXLATB  207
#define xXOR    208


#define nrinst 80			/* number of float instructions */
#define srinst 8
char rgrinst[nrinst][srinst] = {		/*  560 bytes */

	"F2XM1",     /* 01 */
	"FABS",      /* 02 */
	"FADD",      /* 03 */
	"FADDP",     /* 04 */
	"FBLD",      /* 05 */
	"FBSTP",     /* 06 */
	"FCHS",      /* 07 */
	"FCLEX",     /* 08 */
	"FCOM",      /* 09 */
	"FCOMP",     /* 10 */
	"FCOMPP",    /* 11 */
	"FCOS",      /* 12 */
	"FDECSTP",   /* 13 */
	"FDIV",      /* 14 */
	"FDIVP",     /* 15 */
	"FDIVR",     /* 16 */
	"FDIVRP",    /* 17 */
	"FFREE",     /* 18 */
	"FIADD",     /* 19 */
	"FICOM",     /* 20 */
	"FICOMP",    /* 21 */
	"FIDIV",     /* 22 */
	"FIDIVR",    /* 23 */
	"FILD",      /* 24 */
	"FIMUL",     /* 25 */
	"FINCSTP",   /* 26 */
	"FINIT",     /* 27 */
	"FIST",      /* 28 */
	"FISTP",     /* 29 */
	"FISUB",     /* 30 */
	"FISUBR",    /* 31 */
	"FLD",       /* 32 */
	"FLD1",      /* 33 */
	"FLDCW",     /* 34 */
	"FLDENV",    /* 35 */
	"FLDL2E",    /* 36 */
	"FLDL2T",    /* 37 */
	"FLDLG2",    /* 38 */
	"FLDLN2",    /* 39 */
	"FLDPI",     /* 40 */
	"FLDZ",      /* 41 */
	"FMUL",      /* 42 */
	"FMULP",     /* 43 */
	"FNCLEX",    /* 44 */
	"FNINIT",    /* 45 */
	"FNOP",      /* 46 */
	"FNSAVE",    /* 47 */
	"FNSTCW",    /* 48 */
	"FNSTENV",   /* 49 */
	"FNSTSW",    /* 50 */
	"FPATAN",    /* 51 */
	"FPREM",     /* 52 */
	"FPREM1",    /* 53 */
	"FPTAN",     /* 54 */
	"FRNDINT",   /* 55 */
	"FRSTOR",    /* 56 */
	"FSAVE",     /* 57 */
	"FSCALE",    /* 58 */
	"FSIN",      /* 59 */
	"FSINCOS",   /* 60 */
	"FSQRT",     /* 61 */
	"FST",       /* 62 */
	"FSTCW",     /* 63 */
	"FSTENV",    /* 64 */
	"FSTP",      /* 65 */
	"FSTSW",     /* 66 */
	"FSUB",      /* 67 */
	"FSUBP",     /* 68 */
	"FSUBPR",    /* 69 */
	"FSUBR",     /* 70 */
	"FTST",      /* 71 */
	"FUCOM",     /* 72 */
	"FUCOMP",    /* 73 */
	"FUCOMPP",   /* 74 */
	"FWAIT",     /* 75 */
	"FXAM",      /* 76 */
	"FXCH",      /* 77 */
	"FXTRACT",   /* 78 */
	"FYL2X",     /* 79 */
	"FYL2XP1" }; /* 80 */


/*  END OF INSTRUCTIONS  */

/* Allowed Operand types for instruction sequences
   Start numbering at 61 so they don't conflict with
   token values or special characters */

#define rel8   61  	/* mem address within +/-127 bytes of currnt address */
#define relW   62  	/* mem address relative (WORD/DWORD) of current addr */
#define iSAD   63	/* immediate segment address (seg16:offset16/32)  */
#define r8     64	/* Reg: AL, BL, CL, DL, AH, BH, CH, DH            */
#define r16    65	/* Reg: AX, BX, CX, DX, SP, BP, SI, DI            */
#define r32    66	/* Reg: EAX, EBX, ECX, EDX, ESP, EBP, ESI, EDI    */
#define rREG   67   /* Any 8, 16 or 32 bit general register */
#define rRGW   68   /* Any 16 or 32 bit general register    */
#define rACC   69   /* Reg: Accumulator AL, AX, EAX (any size) */
#define rSEG   70   /* Reg: CS, DS, SS, ES, FS, GS   */
#define rCRG   71   /* Reg: CR0, CR2, CR3 */
#define rDRG   72   /* Reg: DR0, DR1, DR2, DR3, DR6, DR7 */
#define rTRG   73   /* Reg: TR6, TR7 */
#define imm8   74	/* Immediate BYTE - UNSIGNED*/
#define ims8   75	/* Immediate BYTE - Will be SIGN Extended!!! */
#define imm16  76	/* Immediate WORD signed value  */
#define immX   77   /* immediate size value to match the other operand */
#define rm8    78	/* r8 or memory address to BYTE                   */
#define rm16   79	/* r16 or memory address to WORD                  */
#define rRM    80   /* Any 8, 16 or 32 bit register or memory ref     */
#define rRMW   81   /* Any 16 or 32 bit register or memory ref        */
#define mem    82	/* Memory address. Any legal data mem reference   */
#define memF   83	/* Memory address of a 16:32 ptr value   */
#define moff   84	/* Memory address (Immediate, Disp only, Seg relative)*/
#define immv3  85   /* immediate value 3 for INT 03 debug */
#define immv1  86   /* immediate value 1 for some instructions (SHL etc.) */

/* these are only used in the rgMEM32 table */

#define val8   87	/* Indicates numeric value for displacement, */
#define val16  88   /*  immediate, address, etc. Used in memory model */
#define val32  89	/*  template. */


/*  Instruction sequence lookup table is made of:
	- Instruction number (from definitions above)
	- Opr1-3 are the legal operands for the instructions
	- Opc is base opcode.
	- Oprm is the ModR/M byte (if needed). It is partially filled in
	  and used as indicated by Om1 below.
	- Opfx - Operand prefix and operand count for instruction.
		qP0F - Prefix Opc with 0x0f
		qUW  - Use 0x66 prefix always
		qU16 - If USE16	prefix Opcodes with 0x66.
		qU32 - If USE32	prefix Opcodes with 0x66.

	- Om1 -  Modifications or additions to OpCode to complete instruction:
		zMOP - Use Oprm as ModR/M byte (which is already filled in
			   partially with Opcode in bits 3-5). If operand 1 is
			   register, placed code in R/M field of ModR/M byte (bits 0-2).
			   If operand 1 is mem, handle normally. In special case of
			   AAD and AAM instructions Oprm is actually second byte of
			   opcode (no operands to handle).
		zMR1 - Use Oprm as ModR/M byte. Register in operand 1
			   must be encoded in REG/OPC field of ModR/M. If reg in
			   operand 2 it must be incoded in R/M field.
		zMR2 - Use Oprm as ModR/M byte. Register in operand 2
			   must be encoded in REG/OPC field of ModR/M. If reg in
			   operand 1, it must be encoded in R/M field.
		zORD - OR Opc1 with 01 if WORD or DWORD reg/mem
		zAR1 - No ModRM, Add register code in operand 1 to Opc byte to
		       complete the opcode.
		zAR2 - No ModRM, Add register code in operand 2 to Opc byte to
		       complete the opcode.
		zRG1 - If register in operand 1, it must be encoded in
		       REG/OPC field of ModR/M, else handle memory operand normally.
	    zSIZ - This is not actually a modification to the instruction,
	           but tells the assembler that the operand sizes will not
	           match and that it's ok.  The operand size should be the
	           size of the first operand.

 */

/* Prefix instruction/operand count byte */

#define qP0F 0x01
#define qU16 0x02
#define qU32 0x04
#define qUW  0x08

/* OM1 instruction byte  (Opcode Modifier/Special build instructions) */
#define zMOP 0x01
#define zMR1 0x02
#define zMR2 0x04
#define zORD 0x08
#define zAR1 0x10
#define zAR2 0x20
#define zRG1 0x40
#define zSIZ 0x80


/* Certain table entries were added for optimization to ensure that
   the shortest instruction is used in all cases.  This adds about
   50 lines to the table, but it's well worth the overhead!
*/

#define nrgINS 338
unsigned char rgINS[nrgINS][8] = {  /* 338 * 8 = */

/*Inst   Opr1    Opr2   Opr3  Opfx        Opc   Oprm   Om1          */

{0      , 0,     0,     0,    0,          0,     0,    0            },
{xAAA   , 0,     0,     0,    0,          0x37,  0,    0            },
{xAAD   , 0,     0,     0,    0,          0xd5,  0x0A, zMOP         },
{xAAM   , 0,     0,     0,    0,          0xd4,  0x0A, zMOP         },
{xADC   , rACC,  immX,  0,    0,          0x14,  0,    zORD         },
{xADC   , rRMW,  imm8,  0,    0,          0x83,  0x10, zMOP         },
{xADC   , rRM,   immX,  0,    0,          0x80,  0x10, zMOP|zORD    },
{xADC   , rRM,   rREG,  0,    0,          0x10,  0,    zMR2|zORD    },
{xADC   , rREG,  rRM,   0,    0,          0x12,  0,    zMR1|zORD    },

{xADD   , rRMW,  ims8,  0,    0,          0x83,  0,    zMOP         },
{xADD   , rACC,  immX,  0,    0,          0x04,  0,    zORD         },
{xADD   , rRM,   immX,  0,    0,          0x80,  0,    zMOP|zORD    },
{xADD   , rREG,  rRM,   0,    0,          0x02,  0,    zMR1|zORD    },
{xADD   , rRM,   rREG,  0,    0,          0x00,  0,    zMR2|zORD    },

{xAND   , rRMW,  ims8,  0,    0,          0x83,  0x20, zMOP         },
{xAND   , rACC,  immX,  0,    0,          0x24,  0,    zORD         },
{xAND   , rRM,   immX,  0,    0,          0x80,  0x20, zMOP|zORD    },
{xAND   , rREG,  rRM,   0,    0,          0x22,  0,    zMR1|zORD    },
{xAND   , rRM,   rREG,  0,    0,          0x20,  0,    zMR2|zORD    },

{xARPL  , rm16,  r16,   0,    0,          0x63,  0,    zMR2         },
{xBOUND , rRMW,  mem,   0,    0,          0x62,  0,    zMR1         },
{xBSF   , rRGW,  rRMW,  0,    qP0F,       0xbc,  0,    zMR1         },
{xBSR   , rRGW,  rRMW,  0,    qP0F,       0xbd,  0,    zMR1         },
{xBT    , rRMW,  rRGW,  0,    qP0F,       0xa3,  0,    zMR2         },
{xBT    , rRMW,  imm8,  0,    qP0F,       0xba,  0x20, zMOP         },
{xBTC   , rRMW,  rRGW,  0,    qP0F,       0xbb,  0,    zMR2         },
{xBTC   , rRMW,  imm8,  0,    qP0F,       0xba,  0x38, zMOP         },
{xBTR   , rRMW,  rRGW,  0,    qP0F,       0xb3,  0,    zMR2         },
{xBTR   , rRMW,  imm8,  0,    qP0F,       0xba,  0x30, zMOP         },
{xBTS   , rRMW,  rRGW,  0,    qP0F,       0xab,  0,    zMR2         },
{xBTS   , rRMW,  imm8,  0,    qP0F,       0xba,  0x28, zMOP         },
{xCALL  , relW,  0,     0,    0,          0xe8,  0,    0            },
{xCALL  , rRMW,  0,     0,    0,          0xff,  0x10, zMOP         },
{xCALL  , iSAD,  0,     0,    0,          0x9a,  0,    0            },
{xCALL  , memF,  0,     0,    0,          0xff,  0x18, zMOP         },
{xCBW   , 0,     0,     0,    qU32,       0x98,  0,    0            },
{xCWDE  , 0,     0,     0,    qU16,       0x98,  0,    0            },
{xCLC   , 0,     0,     0,    0,          0xf8,  0,    0            },
{xCLD   , 0,     0,     0,    0,          0xfc,  0,    0            },
{xCLI   , 0,     0,     0,    0,          0xfa,  0,    0            },
{xCLTS  , 0,     0,     0,    qP0F,       0x06,  0,    0            },
{xCMC   , 0,     0,     0,    0,          0xf5,  0,    0            },

{xCMP   , rRMW,  ims8,  0,    0,          0x83,  0x38, zMOP         },
{xCMP   , rACC,  immX,  0,    0,          0x3c,  0,    zORD         },
{xCMP   , rRM,   immX,  0,    0,          0x80,  0x38, zMOP|zORD    },
{xCMP   , rREG,  rRM,   0,    0,          0x3a,  0,    zMR1|zORD    },
{xCMP   , rRM,   rREG,  0,    0,          0x38,  0,    zMR2|zORD    },

{xCMPSB , 0,     0,     0,     0,         0xa6,  0,    0            },
{xCMPSD , 0,     0,     0,     0,         0xa7,  0,    0            },
{xCMPSW , 0,     0,     0,     qUW,       0xa7,  0,    0            },
{xCWD   , 0,     0,     0,     qUW,      0x99,  0,    0            },
{xCDQ   , 0,     0,     0,     qU16,      0x99,  0,    0            },
{xDAA   , 0,     0,     0,     0,         0x27,  0,    0            },
{xDAS   , 0,     0,     0,     0,         0x2f,  0,    0            },
{xDEC   , rRGW,  0,     0,     0,         0x48,  0,    zAR1         },
{xDEC   , rRM,   0,     0,     0,         0xfe,  0x08, zMOP|zORD    },
{xDIV   , rRM,   0,     0,     0,         0xf6,  0x30, zMOP|zORD    },
{xENTER , imm16, imm8,  0,     0,         0xc8,  0,    zSIZ         },
{xHLT   , 0,     0,     0,     0,         0xf4,  0,    0            },
{xIDIV  , rRM,   0,     0,     0,         0xf6,  0x38, zMOP|zORD    },
{xIMUL  , rRM,   0,     0,     0,         0xf6,  0x28, zMOP|zORD    },
{xIMUL  , rRGW,  rRMW,  0,     qP0F,      0xaf,  0,    zMR1         },
{xIMUL  , rRGW,  rRMW,  imm8,  0,         0x6b,  0,    zMR1         },
{xIMUL  , rRGW,  imm8,  0,     0,         0x6b,  0,    zMR1|zSIZ    },
{xIMUL  , rRGW,  rRMW,  immX,  0,         0x69,  0,    zMR1         },
{xIMUL  , rRGW,  immX,  0,     0,         0x69,  0,    zMR1         },
{xIN    , rACC,  imm8,  0,     0,         0xe4,  0,    zORD|zSIZ    },
{xIN    , rACC,  rDX,   0,     0,         0xec,  0,    zORD|zSIZ    },
{xINC   , rRGW,  0,     0,     0,         0x40,  0,    zAR1         },
{xINC   , rRM,   0,     0,     0,         0xfe,  0,    zMOP|zORD    },
{xINSB  , 0,     0,     0,     0,         0x6C,  0,    0            },
{xINSD  , 0,     0,     0,     0,         0x6D,  0,    0            },
{xINSW  , 0,     0,     0,     qUW,       0x6D,  0,    0            },
{xINT   , immv3, 0,     0,     0,         0xcc,  0,    0            },
{xINT   , imm8,  0,     0,     0,         0xcd,  0,    0            },
{xINTO  , 0,     0,     0,     0,         0xce,  0,    0            },
{xIRET  , 0,     0,     0,     qUW,       0xcf,  0,    0            },
{xIRETD , 0,     0,     0,     0,         0xcf,  0,    0            },
{xJA    , rel8,  0,     0,     0,         0x77,  0,    0            },
{xJA    , relW,  0,     0,     qP0F,      0x87,  0,    0            },
{xJAE   , rel8,  0,     0,     0,         0x73,  0,    0            },
{xJAE   , relW,  0,     0,     qP0F,      0x83,  0,    0            },
{xJB    , rel8,  0,     0,     0,         0x72,  0,    0            },
{xJB    , relW,  0,     0,     qP0F,      0x82,  0,    0            },
{xJBE   , rel8,  0,     0,     0,         0x76,  0,    0            },
{xJBE   , relW,  0,     0,     qP0F,      0x86,  0,    0            },
{xJC    , rel8,  0,     0,     0,         0x72,  0,    0            },
{xJC    , relW,  0,     0,     qP0F,      0x82,  0,    0            },
{xJCXZ  , rel8,  0,     0,     0,         0xe3,  0,    0            },
{xJNBE  , rel8,  0,     0,     0,         0x77,  0,    0            },
{xJNBE  , relW,  0,     0,     qP0F,      0x87,  0,    0            },
{xJNB   , rel8,  0,     0,     0,         0x73,  0,    0            },
{xJNB   , relW,  0,     0,     qP0F,      0x83,  0,    0            },
{xJNC   , rel8,  0,     0,     0,         0x73,  0,    0            },
{xJNC   , relW,  0,     0,     qP0F,      0x83,  0,    0            },
{xJNA   , rel8,  0,     0,     0,         0x76,  0,    0            },
{xJNA   , relW,  0,     0,     qP0F,      0x86,  0,    0            },
{xJNAE  , rel8,  0,     0,     0,         0x72,  0,    0            },
{xJNAE  , relW,  0,     0,     qP0F,      0x82,  0,    0            },
{xJECXZ , rel8,  0,     0,     0,         0xe3,  0,    0            },
{xJE    , rel8,  0,     0,     0,         0x74,  0,    0            },
{xJE    , relW,  0,     0,     qP0F,      0x84,  0,    0            },
{xJG    , rel8,  0,     0,     0,         0x7f,  0,    0            },
{xJG    , relW,  0,     0,     qP0F,      0x8f,  0,    0            },
{xJGE   , rel8,  0,     0,     0,         0x7d,  0,    0            },
{xJGE   , relW,  0,     0,     qP0F,      0x8d,  0,    0            },
{xJNL   , rel8,  0,     0,     0,         0x7d,  0,    0            },
{xJNL   , relW,  0,     0,     qP0F,      0x8d,  0,    0            },
{xJL    , rel8,  0,     0,     0,         0x7c,  0,    0            },
{xJL    , relW,  0,     0,     qP0F,      0x8c,  0,    0            },
{xJNGE  , rel8,  0,     0,     0,         0x7c,  0,    0            },
{xJNGE  , relW,  0,     0,     qP0F,      0x8c,  0,    0            },
{xJLE   , rel8,  0,     0,     0,         0x7e,  0,    0            },
{xJLE   , relW,  0,     0,     qP0F,      0x8e,  0,    0            },
{xJNG   , rel8,  0,     0,     0,         0x7e,  0,    0            },
{xJNG   , relW,  0,     0,     qP0F,      0x8e,  0,    0            },
{xJNE   , rel8,  0,     0,     0,         0x75,  0,    0            },
{xJNE   , relW,  0,     0,     qP0F,      0x85,  0,    0            },
{xJNLE  , rel8,  0,     0,     0,         0x7f,  0,    0            },
{xJNLE  , relW,  0,     0,     qP0F,      0x8f,  0,    0            },
{xJNO   , rel8,  0,     0,     0,         0x71,  0,    0            },
{xJNO   , relW,  0,     0,     qP0F,      0x81,  0,    0            },
{xJNP   , rel8,  0,     0,     0,         0x7b,  0,    0            },
{xJNP   , relW,  0,     0,     qP0F,      0x8b,  0,    0            },
{xJNS   , rel8,  0,     0,     0,         0x79,  0,    0            },
{xJNS   , relW,  0,     0,     qP0F,      0x89,  0,    0            },
{xJNZ   , rel8,  0,     0,     0,         0x75,  0,    0            },
{xJNZ   , relW,  0,     0,     qP0F,      0x85,  0,    0            },
{xJO    , rel8,  0,     0,     0,         0x70,  0,    0            },
{xJO    , relW,  0,     0,     qP0F,      0x80,  0,    0            },
{xJP    , rel8,  0,     0,     0,         0x7a,  0,    0            },
{xJP    , relW,  0,     0,     qP0F,      0x8a,  0,    0            },
{xJPO   , rel8,  0,     0,     0,         0x7b,  0,    0            },
{xJPO   , relW,  0,     0,     qP0F,      0x8b,  0,    0            },
{xJPE   , rel8,  0,     0,     0,         0x7a,  0,    0            },
{xJPE   , relW,  0,     0,     qP0F,      0x8a,  0,    0            },
{xJS    , rel8,  0,     0,     0,         0x78,  0,    0            },
{xJS    , relW,  0,     0,     qP0F,      0x88,  0,    0            },
{xJZ    , rel8,  0,     0,     0,         0x74,  0,    0            },
{xJZ    , relW,  0,     0,     qP0F,      0x84,  0,    0            },
{xJMP   , rel8,  0,     0,     0,         0xeb,  0,    0            },
{xJMP   , relW,  0,     0,     0,         0xe9,  0,    0            },
{xJMP   , rRMW,  0,     0,     0,         0xff,  0x20, zMOP         },
{xJMP   , iSAD,  0,     0,     0,         0xea,  0,    0            },
{xJMP   , memF,  0,     0,     0,         0xff,  0x28, zMOP         },
{xLAHF  , 0,     0,     0,     0,         0x9f,  0,    0            },
{xLAR   , rRGW,  rRMW,  0,     qP0F,      0x02,  0,    zMR1         },
{xLEA   , rRGW,  mem,   0,     0,         0x8d,  0,    zMR1|zSIZ    },
{xLEAVE , 0,     0,     0,     0,         0xc9,  0,    0            },
{xLGDT  , memF,  0,     0,     qP0F,      0x01,  0x10, zMOP         },
{xLIDT  , memF,  0,     0,     qP0F,      0x01,  0x18, zMOP         },
{xLDS   , rRGW,  memF,  0,     0,         0xc5,  0,    zMR1         },
{xLSS   , rRGW,  memF,  0,     qP0F,      0xb2,  0,    zMR1         },
{xLES   , rRGW,  memF,  0,     0,         0xc4,  0,    zMR1         },
{xLFS   , rRGW,  memF,  0,     qP0F,      0xb4,  0,    zMR1         },
{xLGS   , rRGW,  memF,  0,     qP0F,      0xb5,  0,    zMR1         },
{xLLDT  , rm16,  0,     0,     qP0F,      0x00,  0x10, zMOP         },
{xLMSW  , rm16,  0,     0,     qP0F,      0x01,  0x30, zMOP         },
{xLOCK  , 0,     0,     0,     0,         0xf0,  0,    0            },
{xLODSB , 0,     0,     0,     0,         0xac,  0,    0            },
{xLODSD , 0,     0,     0,     0,         0xad,  0,    0            },
{xLODSW , 0,     0,     0,     qUW,       0xad,  0,    0            },
{xLOOP  , rel8,  0,     0,     0,         0xe2,  0,    0            },
{xLOOPE , rel8,  0,     0,     0,         0xe1,  0,    0            },
{xLOOPZ , rel8,  0,     0,     0,         0xe1,  0,    0            },
{xLOOPNE, rel8,  0,     0,     0,         0xe0,  0,    0            },
{xLOOPNZ, rel8,  0,     0,     0,         0xe0,  0,    0            },
{xLSL   , rRGW,  rRMW,  0,     qP0F,      0x03,  0,    zMR1         },
{xLTR   , rm16,  0,     0,     qP0F,      0x00,  0x18, zMOP         },
{xMOV   , rACC,  moff,  0,     0,         0xA0,  0,    zORD         },
{xMOV   , moff,  rACC,  0,     0,         0xA2,  0,    zORD         },
{xMOV   , r8,    imm8,  0,     0,         0xb0,  0,    zAR1         },
{xMOV   , rRGW,  immX,  0,     0,         0xb8,  0,    zAR1         },
{xMOV   , rREG,  rRM,   0,     0,         0x8a,  0,    zMR1|zORD    },
{xMOV   , rRM,   rREG,  0,     0,         0x88,  0,    zMR2|zORD    },
{xMOV   , rRM,   immX,  0,     0,         0xc6,  0,    zMR1|zORD    },/*?*/
{xMOV   , rm16,  rSEG,  0,     0,         0x8c,  0,    zMR2         },
{xMOV   , rSEG,  rm16,  0,     0,         0x8e,  0,    zMR1         },
{xMOV   , r32,   rCRG,  0,     qP0F,      0x20,  0,    zMR2         },
{xMOV   , rCRG,  r32,   0,     qP0F,      0x22,  0,    zMR1         },
{xMOV   , r32,   rDRG,  0,     qP0F,      0x21,  0,    zMR2         },
{xMOV   , rDRG,  r32,   0,     qP0F,      0x23,  0,    zMR1         },
{xMOV   , r32,   rTRG,  0,     qP0F,      0x24,  0,    zMR2         },
{xMOV   , rTRG,  r32,   0,     qP0F,      0x26,  0,    zMR1         },
{xMOVSB , 0,     0,     0,     0,         0xa4,  0,    0            },
{xMOVSD , 0,     0,     0,     0,         0xa5,  0,    0            },
{xMOVSW , 0,     0,     0,     qUW,       0xa5,  0,    0            },
{xMOVSX , r32,   rm16,  0,     qP0F,      0xbf,  0,    zMR1|zSIZ    },
{xMOVSX , rRGW,  rm8,   0,     qP0F,      0xbe,  0,    zMR1|zSIZ    },
{xMOVZX , r32,   rm16,  0,     qP0F,      0xb7,  0,    zMR1|zSIZ    },
{xMOVZX , rRGW,  rm8,   0,     qP0F,      0xb6,  0,    zMR1|zSIZ    },
{xMUL   , rRM,   0,     0,     0,         0xf6,  0x20, zMOP|zORD    },
{xNEG   , rRM,   0,     0,     0,         0xf6,  0x18, zMOP|zORD    },
{xNOP   , 0,     0,     0,     0,         0x90,  0,    0            },
{xNOT   , rRM,   0,     0,     0,         0xf6,  0x10, zMOP|zORD    },

{xOR    , rRMW,  ims8,  0,     0,         0x83,  0x08, zMOP         },
{xOR    , rACC,  immX,  0,     0,         0x0c,  0,    zORD         },
{xOR    , rRM,   immX,  0,     0,         0x80,  0x08, zMOP|zORD    },
{xOR    , rREG,  rRM,   0,     0,         0x0a,  0,    zMR1|zORD    },
{xOR    , rRM,   rREG,  0,     0,         0x08,  0,    zMR2|zORD    },

{xOUT   , imm8,  rACC,  0,     0,         0xe6,  0,    zORD         },
{xOUT   , rDX,   rACC,  0,     0,         0xee,  0,    zORD         },
{xOUTSB , 0,     0,     0,     0,         0x6e,  0,    0            },
{xOUTSD , 0,     0,     0,     0,         0x6f,  0,    0            },
{xOUTSW , 0,     0,     0,     qUW,       0x6f,  0,    0            },
{xPOP   , mem,   0,     0,     0,         0x8f,  0,    zMOP         },
{xPOP   , rRGW,  0,     0,     0,         0x58,  0,    zAR1         },
{xPOP   , rDS,   0,     0,     0,         0x1f,  0,    0            },
{xPOP   , rES,   0,     0,     0,         0x07,  0,    0            },
{xPOP   , rSS,   0,     0,     0,         0x17,  0,    0            },
{xPOP   , rFS,   0,     0,     qP0F,      0xa1,  0,    0            },
{xPOP   , rGS,   0,     0,     qP0F,      0xa9,  0,    0            },
{xPOPA  , 0,     0,     0,     0,         0x61,  0,    0            },
{xPOPAD , 0,     0,     0,     0,         0x61,  0,    0            },
{xPOPF  , 0,     0,     0,     0,         0x9d,  0,    0            },
{xPOPFD , 0,     0,     0,     0,         0x9d,  0,    0            },
{xPUSH  , mem,   0,     0,     0,         0xff,  0x30, zMOP         },
{xPUSH  , rRGW,  0,     0,     0,         0x50,  0,    zAR1         },
{xPUSH  , ims8,  0,     0,     0,         0x6a,  0,    0            },
{xPUSH  , immX,  0,     0,     0,         0x68,  0,    0            },
{xPUSH  , rCS,   0,     0,     0,         0x0e,  0,    0            },
{xPUSH  , rSS,   0,     0,     0,         0x16,  0,    0            },
{xPUSH  , rDS,   0,     0,     0,         0x1e,  0,    0            },
{xPUSH  , rES,   0,     0,     0,         0x06,  0,    0            },
{xPUSH  , rFS,   0,     0,     qP0F,      0xa0,  0,    0            },
{xPUSH  , rGS,   0,     0,     qP0F,      0xa8,  0,    0            },
{xPUSHA , 0,     0,     0,     0,         0x60,  0,    0            },
{xPUSHAD, 0,     0,     0,     0,         0x60,  0,    0            },
{xPUSHF , 0,     0,     0,     0,         0x9c,  0,    0            },
{xPUSHFD, 0,     0,     0,     0,         0x9c,  0,    0            },
{xRCL   , rRM,   immv1, 0,     0,         0xd0,  0x10, zMOP|zORD|zSIZ },
{xRCL   , rRM,   rCL,   0,     0,         0xd2,  0x10, zMOP|zORD|zSIZ },
{xRCL   , rRM,   imm8,  0,     0,         0xc0,  0x10, zMOP|zORD|zSIZ },
{xRCR   , rRM,   immv1, 0,     0,         0xd0,  0x18, zMOP|zORD|zSIZ },
{xRCR   , rRM,   rCL,   0,     0,         0xd2,  0x18, zMOP|zORD|zSIZ },
{xRCR   , rRM,   imm8,  0,     0,         0xc0,  0x18, zMOP|zORD|zSIZ },
{xROL   , rRM,   immv1, 0,     0,         0xd0,  0,    zMOP|zORD|zSIZ },
{xROL   , rRM,   rCL,   0,     0,         0xd2,  0,    zMOP|zORD|zSIZ },
{xROL   , rRM,   imm8,  0,     0,         0xc0,  0,    zMOP|zORD|zSIZ },
{xROR   , rRM,   immv1, 0,     0,         0xd0,  0x08, zMOP|zORD|zSIZ },
{xROR   , rRM,   rCL,   0,     0,         0xd2,  0x08, zMOP|zORD|zSIZ },
{xROR   , rRM,   imm8,  0,     0,         0xc0,  0x08, zMOP|zORD|zSIZ },
{xREP   , 0,     0,     0,     0,         0xf3,  0,    0            },
{xREPE  , 0,     0,     0,     0,         0xf3,  0,    0            },
{xREPNE , 0,     0,     0,     0,         0xf2,  0,    0            },
{xRETN  , 0,     0,     0,     0,         0xc3,  0,    0            },
{xRETN  , imm16, 0,     0,     0,         0xc2,  0,    0            },
{xRETF  , 0,     0,     0,     0,         0xcb,  0,    0            },
{xRETF  , imm16, 0,     0,     0,         0xca,  0,    0            },
{xSAL   , rRM,   immv1, 0,     0,         0xd0,  0x20, zMOP|zORD|zSIZ },
{xSAL   , rRM,   rCL,   0,     0,         0xd2,  0x20, zMOP|zORD|zSIZ },
{xSAL   , rRM,   imm8,  0,     0,         0xc0,  0x20, zMOP|zORD|zSIZ },
{xSAR   , rRM,   immv1, 0,     0,         0xd0,  0x38, zMOP|zORD|zSIZ },
{xSAR   , rRM,   rCL,   0,     0,         0xd2,  0x38, zMOP|zORD|zSIZ },
{xSAR   , rRM,   imm8,  0,     0,         0xc0,  0x38, zMOP|zORD|zSIZ },
{xSHL   , rRM,   immv1, 0,     0,         0xd0,  0x20, zMOP|zORD|zSIZ },
{xSHL   , rRM,   rCL,   0,     0,         0xd2,  0x20, zMOP|zORD|zSIZ },
{xSHL   , rRM,   imm8,  0,     0,         0xc0,  0x20, zMOP|zORD|zSIZ },
{xSHR   , rRM,   immv1, 0,     0,         0xd0,  0x28, zMOP|zORD|zSIZ },
{xSHR   , rRM,   rCL,   0,     0,         0xd2,  0x28, zMOP|zORD|zSIZ },
{xSHR   , rRM,   imm8,  0,     0,         0xc0,  0x28, zMOP|zORD|zSIZ },

{xSBB   , rRMW,  ims8,  0,     0,         0x83,  0x18, zMOP         },
{xSBB   , rACC,  immX,  0,     0,         0x1c,  0,    zORD         },
{xSBB   , rRM,   immX,  0,     0,         0x80,  0x18, zMOP|zORD    },
{xSBB   , rREG,  rRM,   0,     0,         0x1a,  0,    zMR1|zORD    },
{xSBB   , rRM,   rREG,  0,     0,         0x18,  0,    zMR2|zORD    },

{xSCASB , 0,     0,     0,     0,         0xae,  0,    0            },
{xSCASD , 0,     0,     0,     0,         0xaf,  0,    0            },
{xSCASW , 0,     0,     0,     qUW,       0xaf,  0,    0            },
{xSETA  , rm8,   0,     0,     qP0F,      0x97,  0,    zRG1         },
{xSETAE , rm8,   0,     0,     qP0F,      0x93,  0,    zRG1         },
{xSETB  , rm8,   0,     0,     qP0F,      0x92,  0,    zRG1         },
{xSETBE , rm8,   0,     0,     qP0F,      0x96,  0,    zRG1         },
{xSETC  , rm8,   0,     0,     qP0F,      0x92,  0,    zRG1         },
{xSETE  , rm8,   0,     0,     qP0F,      0x94,  0,    zRG1         },
{xSETG  , rm8,   0,     0,     qP0F,      0x9F,  0,    zRG1         },
{xSETGE , rm8,   0,     0,     qP0F,      0x9D,  0,    zRG1         },
{xSETL  , rm8,   0,     0,     qP0F,      0x9C,  0,    zRG1         },
{xSETLE , rm8,   0,     0,     qP0F,      0x9E,  0,    zRG1         },
{xSETNA , rm8,   0,     0,     qP0F,      0x96,  0,    zRG1         },
{xSETNAE, rm8,   0,     0,     qP0F,      0x92,  0,    zRG1         },
{xSETNB , rm8,   0,     0,     qP0F,      0x93,  0,    zRG1         },
{xSETNBE, rm8,   0,     0,     qP0F,      0x97,  0,    zRG1         },
{xSETNC , rm8,   0,     0,     qP0F,      0x93,  0,    zRG1         },
{xSETNE , rm8,   0,     0,     qP0F,      0x95,  0,    zRG1         },
{xSETNG , rm8,   0,     0,     qP0F,      0x9E,  0,    zRG1         },
{xSETNGE, rm8,   0,     0,     qP0F,      0x9C,  0,    zRG1         },
{xSETNL , rm8,   0,     0,     qP0F,      0x9D,  0,    zRG1         },
{xSETNLE, rm8,   0,     0,     qP0F,      0x9F,  0,    zRG1         },
{xSETNO , rm8,   0,     0,     qP0F,      0x91,  0,    zRG1         },
{xSETNP , rm8,   0,     0,     qP0F,      0x9B,  0,    zRG1         },
{xSETNS , rm8,   0,     0,     qP0F,      0x99,  0,    zRG1         },
{xSETNZ , rm8,   0,     0,     qP0F,      0x95,  0,    zRG1         },
{xSETO  , rm8,   0,     0,     qP0F,      0x90,  0,    zRG1         },
{xSETP  , rm8,   0,     0,     qP0F,      0x9A,  0,    zRG1         },
{xSETPE , rm8,   0,     0,     qP0F,      0x9A,  0,    zRG1         },
{xSETPO , rm8,   0,     0,     qP0F,      0x9B,  0,    zRG1         },
{xSETS  , rm8,   0,     0,     qP0F,      0x98,  0,    zRG1         },
{xSETZ  , rm8,   0,     0,     qP0F,      0x94,  0,    zRG1         },
{xSGDT  , memF,  0,     0,     qP0F,      0x01,  0,    zMOP         },
{xSIDT  , memF,  0,     0,     qP0F,      0x01,  0x08, zMOP         },
{xSHLD  , rRMW,  rRGW,  imm8,  qP0F,      0xa4,  0,    0            },
{xSHLD  , rRMW,  rRGW,  rCL,   qP0F,      0xa5,  0,    0            },
{xSHRD  , rRMW,  rRGW,  imm8,  qP0F,      0xac,  0,    0            },
{xSHRD  , rRMW,  rRGW,  rCL,   qP0F,      0xad,  0,    0            },
{xSLDT  , rm16,  0,     0,     qP0F,      0x00,  0,    zMOP         },
{xSMSW  , rm16,  0,     0,     qP0F,      0x01,  0x20, zMOP         },
{xSTC   , 0,     0,     0,     0,         0xf9,  0,    0            },
{xSTI   , 0,     0,     0,     0,         0xfb,  0,    0            },
{xSTD   , 0,     0,     0,     0,         0xfd,  0,    0            },
{xSTOSB , 0,     0,     0,     0,         0xaa,  0,    0            },
{xSTOSD , 0,     0,     0,     0,         0xab,  0,    0            },
{xSTOSW , 0,     0,     0,     qUW,       0xab,  0,    0            },
{xSTR   , rm16,  0,     0,     qP0F,      0x00,  0x08, zMOP         },

{xSUB   , rRMW,  ims8,  0,     0,         0x83,  0x28, zMOP         },
{xSUB   , rACC,  immX,  0,     0,         0x2c,  0,    zORD         },
{xSUB   , rRM,   immX,  0,     0,         0x80,  0x28, zMOP|zORD    },
{xSUB   , rREG,  rRM,   0,     0,         0x2a,  0,    zMR1|zORD    },
{xSUB   , rRM,   rREG,  0,     0,         0x28,  0,    zMR2|zORD    },

{xTEST  , rACC,  immX,  0,     0,         0xa8,  0,    zORD         },
{xTEST  , rRM,   immX,  0,     0,         0xf6,  0,    zMOP|zORD    },
{xTEST  , rREG,  rRM,   0,     0,         0x84,  0,    zMR1|zORD    },
{xVERR  , rm16,  0,     0,     qP0F,      0x00,  0x20, zMOP         },
{xVERW  , rm16,  0,     0,     qP0F,      0x00,  0x28, zMOP         },
{xWAIT  , 0,     0,     0,     0,         0x9b,  0,    0            },
{xXCHG  , r16,   rAX,   0,     0,         0x90,  0,    zAR1         },
{xXCHG  , rAX,   r16,   0,     0,         0x90,  0,    zAR2         },
{xXCHG  , r32,   rEAX,  0,     0,         0x90,  0,    zAR1         },
{xXCHG  , rEAX,  r32,   0,     0,         0x90,  0,    zAR2         },
{xXCHG  , rRM,   rREG,  0,     0,         0x86,  0,    zMR2|zORD    },
{xXCHG  , rREG,  rRM,   0,     0,         0x86,  0,    zMR1|zORD    },
{xXLAT  , mem,   0,     0,     0,         0x00,  0,    0            },
{xXLATB , 0,     0,     0,     0,         0x00,  0,    0            },

{xXOR   , rRMW,  ims8,  0,     0,         0x83,  0x30, zMOP         },
{xXOR   , rACC,  immX,  0,     0,         0x34,  0,    zORD         },
{xXOR   , rRM,   immX,  0,     0,         0x80,  0x30, zMOP|zORD    },
{xXOR   , rREG,  rRM,   0,     0,         0x32,  0,    zMR1|zORD    },
{xXOR   , rRM,   rREG,  0,     0,         0x30,  0,    zMR2|zORD    },
};

/* END of instruction sequence definitions  */

/* This table provides look-up instructions on building the ModR/M and
   SIB bytes (optional) for all memory references.
   THIS IS ONLY FOR MEMORY REFERENCES.
   The Mod value in this table can be ORed to the ModR/M byte
   and SS value in this table can be ORed to the SIB byte
   for the instruction you're building.
*/


#define fBase   0x01
#define fIndx   0x02	/* fScale = 1 by default when fIndx is present */
#define fScale2 0x04
#define fScale4 0x08
#define fScale8 0x10
#define fDisp8  0x20
#define fDisp32 0x40

#define UseSIB  1
#define nrgM32 20

U8 rgM32[20][4] = {
/*                                    UseSIB  Mod   SS */
{   0,                                 0,    0x00, 0x00 },
{   fBase,                             0,    0x00, 0x00 },
{   fBase | fIndx,                     1,    0x04, 0x00 },
{   fIndx | fScale2,                   1,    0x04, 0x40 },
{   fIndx | fScale4,                   1,    0x04, 0x80 },
{   fIndx | fScale8,                   1,    0x04, 0xC0 },
{   fDisp32,                           0,    0x05, 0x00 },
{   fBase | fIndx | fScale2,           1,    0x04, 0x40 },
{   fBase | fIndx | fScale4,           1,    0x04, 0x80 },
{   fBase | fIndx | fScale8,           1,    0x04, 0xC0 },
{   fBase | fDisp8,                    0,    0x40, 0x00 },
{   fBase | fDisp32,                   0,    0x80, 0x00 },
{   fBase | fIndx | fDisp8,            1,    0x44, 0x00 },
{   fBase | fIndx | fDisp32,           1,    0x84, 0x00 },
{   fBase | fIndx | fScale2 | fDisp8,  1,    0x44, 0x40 },
{   fBase | fIndx | fScale4 | fDisp8,  1,    0x44, 0x80 },
{   fBase | fIndx | fScale8 | fDisp8,  1,    0x44, 0xC0 },
{   fBase | fIndx | fScale2 | fDisp32, 1,    0x84, 0x40 },
{   fBase | fIndx | fScale4 | fDisp32, 1,    0x84, 0x80 },
{   fBase | fIndx | fScale8 | fDisp32, 1,    0x84, 0xC0 },
};


/* The following 16 bit addressing modes are recognized: */
/* UseSIB is always 0 in 16 bit addressing modes */

#define nrgM16 24

int rgM16[24][8] = {

/*	                                  UseSIB  Mod     SS */
	{rBX,    rSI,    0,     0,   0,   0,     0,      0},
	{rBX,    rDI,    0,     0,   0,   0,     0,      0},
	{rBP,    rSI,    0,     0,   0,   0,     0,      0},
	{rBP,    rDI,    0,     0,   0,   0,     0,      0},
	{rSI,    0,      0,     0,   0,   0,     0,      0},
	{rDI,    0,      0,     0,   0,   0,     0,      0},
	{val16,  0,      0,     0,   0,   0,     0,      0},
	{rBX,    0,      0,     0,   0,   0,     0,      0},
	{rBX,    rSI,    val8,  0,   0,   0,     0,      0},
	{rBX,    rDI,    val8,  0,   0,   0,     0,      0},
	{rBP,    rSI,    val8,  0,   0,   0,     0,      0},
	{rBP,    rDI,    val8,  0,   0,   0,     0,      0},
	{rSI,    val8,   0,     0,   0,   0,     0,      0},
	{rDI,    val8,   0,	    0,   0,   0,     0,      0},
	{rBP,    val8,   0,	    0,   0,   0,     0,      0},
	{rBX,    val8,   0,	    0,   0,   0,     0,      0},
	{rBX,    rSI,    val16, 0,   0,   0,     0,      0},
	{rBX,    rDI,    val16, 0,   0,   0,     0,      0},
	{rBP,    rSI,    val16, 0,   0,   0,     0,      0},
	{rBP,    rDI,    val16, 0,   0,   0,     0,      0},
	{rSI,    val16,  0,     0,   0,   0,     0,      0},
	{rDI,    val16,  0,	    0,   0,   0,     0,      0},
	{rBP,    val16,  0,	    0,   0,   0,     0,      0},
	{rBX,    val16,  0,     0,   0,   0,     0,      0},
	};

/* End of memory Look-up tables */

/* The following flag bits are used to determine memory operand size
   either by looking at the register involved, or by seeing the user
   force the size with WORD PTR (etc.).  They are put in variable
   OpSize and used by the functions that encode the instruction.
*/

#define fByte  0x01
#define fWord  0x02
#define fDWord 0x04
#define fFWord 0x08
#define fNear  0x10
#define fFar   0x20
#define fShort 0x40
#define fPtr   0x80		/* not currently used... */

/* The following flag bits are used to determine if a register prefix
   was specified as part of a memory address. The flag bits are set in
   char variable OpPrefix.
*/

#define fDSp   0x01
#define fESp   0x02
#define fSSp   0x04
#define fFSp   0x08
#define fGSp   0x10
#define fCSp   0x20

/*  END OF HEADER FILE */
