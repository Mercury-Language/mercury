/* sparc_regs.h - global register variables for the sparc architecture. */

/* This file is included by machregs.h.
   It defines the machine registers mr0 - mr31.
   The first NUMBER_REAL_REGS of them are actually real machine
   registers with much faster access time than the rest, which
   are just global variables.

   At the moment we're only using the callee-save registers.
   We should modify this to optionally use the caller-save registers. */

reg 	Word	mr0 __asm__("i0");
reg	Word	mr1 __asm__("i1");
reg	Word	mr2 __asm__("i2");
reg	Word	mr3 __asm__("i3");
reg	Word	mr4 __asm__("i4");
reg	Word	mr5 __asm__("i5");
reg	Word	mr6 __asm__("l0");
reg	Word	mr7 __asm__("l1");
reg	Word	mr8 __asm__("l2");
reg	Word	mr9 __asm__("l3");
reg	Word	mr10 __asm__("l4");
/* we could use l5, l6, and l7 as well, but for the moment at least I'll
   leave them for gcc */

#define NUM_REAL_REGS 11

extern Word mr2, mr3, mr4, mr5, mr6;
extern Word mr7, mr8, mr9, mr10;
extern Word mr11, mr12, mr13, mr14, mr15, mr16;
extern Word mr17, mr18, mr19, mr20, mr21, mr22, mr23, mr24;
extern Word mr25, mr26, mr27, mr28, mr29, mr30, mr31;

