/* i386_regs.h - global register variables for the intel 386 architecture. */

/* This file is included by machregs.h.
   It defines the machine registers mr0 - mr31.
   The first NUMBER_REAL_REGS of them are actually real machine
   registers with much faster access time than the rest, which
   are just global variables.

   At the moment we're only using the callee-save registers.
   We should modify this to optionally use the caller-save registers. */

reg 	Word	mr0 __asm__("bx");
reg	Word	mr1 __asm__("si");
reg	Word	mr2 __asm__("di");

#define NUM_REAL_REGS 3

extern Word mr3, mr4, mr5, mr6, mr7;
extern Word mr8, mr9, mr10, mr11, mr12, mr13, mr14, mr15;
extern Word mr16, mr17, mr18, mr19, mr20, mr21, mr22, mr23;
extern Word mr24, mr25, mr26, mr27, mr28, mr29, mr30, mr31;

