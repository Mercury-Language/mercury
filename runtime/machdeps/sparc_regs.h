/* sparc_regs.h - global register variables for the sparc architecture. */

/*
** This file is included by machregs.h.
** It defines the machine registers mr0 - mr31.
** The first NUMBER_REAL_REGS of them are actually real machine
** registers with much faster access time than the rest, which
** are just global variables.
**
** This is a bit tricky on sparcs, because of the sliding register
** windows.  There are only seven global registers (g1-g7), and of
** these three (g5-g7) are reserved for use by the operating system,
** and the other four (g1-g4) are all temp registers that get clobbered
** by calls to the C standard library functions.
**
** So it looks like we'll have to use the sliding registers.
** It won't work at all unless we are using gcc's non-local gotos.
*/

#ifdef USE_GCC_NONLOCAL_GOTOS
reg 	Word	mr0 __asm__("i0");
reg	Word	mr1 __asm__("i1");	/* potentially non-clobbered */
reg	Word	mr2 __asm__("i2");	/* potentially non-clobbered */
reg	Word	mr3 __asm__("i3");	/* potentially non-clobbered */
reg	Word	mr4 __asm__("i4");	/* potentially non-clobbered */
reg	Word	mr5 __asm__("i5");	/* potentially non-clobbered */
reg	Word	mr6 __asm__("l1");
reg	Word	mr7 __asm__("l2");
reg	Word	mr8 __asm__("l3");
reg	Word	mr9 __asm__("l4");

/* we could use l5, l6, and l7 as well, */
/* but for the moment at least I'll leave them for gcc */

#define NUM_REAL_REGS 10

extern Word saved_regs[];

#define save_registers()			\
	(					\
		saved_regs[0] = mr0,		\
		saved_regs[1] = mr1,		\
		saved_regs[2] = mr2,		\
		saved_regs[3] = mr3,		\
		saved_regs[4] = mr4,		\
		saved_regs[5] = mr5,		\
		saved_regs[6] = mr6,		\
		saved_regs[7] = mr7,		\
		saved_regs[8] = mr8,		\
		saved_regs[9] = mr9,		\
		(void)0				\
	)
#define restore_registers()			\
	(					\
		mr0 = saved_regs[0],		\
		mr1 = saved_regs[1],		\
		mr2 = saved_regs[2],		\
		mr3 = saved_regs[3],		\
		mr4 = saved_regs[4],		\
		mr5 = saved_regs[5],		\
		mr6 = saved_regs[6],		\
		mr7 = saved_regs[7],		\
		mr8 = saved_regs[8],		\
		mr9 = saved_regs[9],		\
		(void)0				\
	)

extern Word             mr10, mr11, mr12, mr13, mr14, mr15;
extern Word mr16, mr17, mr18, mr19, mr20, mr21, mr22, mr23;
extern Word mr24, mr25, mr26, mr27, mr28, mr29, mr30, mr31;
extern Word mr32, mr33, mr34, mr35, mr36;

#else

#define NUM_REAL_REGS 0

extern Word mr0,  mr1,  mr2,  mr3,  mr4,  mr5,  mr6,  mr7;
extern Word mr8,  mr9,  mr10, mr11, mr12, mr13, mr14, mr15;
extern Word mr16, mr17, mr18, mr19, mr20, mr21, mr22, mr23;
extern Word mr24, mr25, mr26, mr27, mr28, mr29, mr30, mr31;
extern Word mr32, mr33, mr34, mr35, mr36;
#endif
