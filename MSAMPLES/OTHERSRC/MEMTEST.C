/* Test memory Aliasing for MMURTL V1.0 */
/*  Copyright (c) 1994 R.A. Burgess     */
/*  All Rights Reserved                 */

/*
This simple program shows how memory aliasing works in MMURTL.
To build this, use MakeIt.bat in DOS, or use CM32 and DASM:
  CM32 MemTest.c
  DASM MemTest.atf
*/


#define U32 unsigned long
#define S32 long
#define U16 unsigned int
#define S16 int
#define U8 unsigned char
#define S8 char
#define TRUE 1
#define FALSE 0


#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* Includes for OS public calls and structures */

#include "\OSSource\MMemory.h"
#include "\OSSource\MKbd.h"
#include "\OSSource\MStatus.h"

/**************** BEGIN Data ********************/

char *Alias;
U32 AliasPhy;
U32 OSPhy;

/**************** BEGIN Code ********************/

void main(U32 argc, U8 argv[])
{
unsigned long erc, key;

	erc = GetPhyAdd(1, 3072, &OSPhy);
	if (erc)
		printf("ERROR %d on GetPhyAdd.\r\n", erc);

	printf("Aliasing Job 1 Linear Address %08x\r\n", 3072);
	printf("Physical Address is           %08x\r\n", OSPhy);

	erc = AliasMem(3072, 8000, 1, &Alias);

	if (erc)
		printf("ERROR %d on AliasMem.\r\n", erc);

	erc = GetPhyAdd(3, Alias, &AliasPhy);

	if (erc)
		printf("ERROR %d on GetPhyAdd.\r\n", erc);

	printf("Alias Linear address is   %08x\r\n", Alias);
	printf("Physical Address is       %08x\r\n", AliasPhy);

	printf("Press a key to continue...\r\n");
	ReadKbd(&key, 1);

	erc = DeAliasMem(Alias, 8000, 3);
	if (erc)
		printf("ERROR %d on DeAliasMem.\r\n", erc);

	exit(0);

}
