/*
** Copyright (C) 1993-1997, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef SPARC_REGS_H
#define SPARC_REGS_H

/*
** Machine registers mr0 - mr36 for the SPARC architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** This is a bit tricky on sparcs, because of the sliding register
** windows. There are only seven global registers (g1-g7), and of
** these three (g5-g7) are reserved for use by the operating system,
** and the other four (g1-g4) are all temp registers that get clobbered
** by calls to the C standard library functions.
**
** So it looks like we'll have to use the sliding registers.
** This won't work at all unless we are using gcc's non-local gotos.
*/

#ifndef USE_GCC_NONLOCAL_GOTOS
  #error "On SPARCs, you must use non-local gotos if you want global registers"
#else

#define NUM_REAL_REGS 10

register 	MR_Word	mr0 __asm__("i0");
register	MR_Word	mr1 __asm__("i1");	/* potentially non-clobbered */
register	MR_Word	mr2 __asm__("i2");	/* potentially non-clobbered */
register	MR_Word	mr3 __asm__("i3");	/* potentially non-clobbered */
register	MR_Word	mr4 __asm__("i4");	/* potentially non-clobbered */
register	MR_Word	mr5 __asm__("i5");	/* potentially non-clobbered */
register	MR_Word	mr6 __asm__("l1");
register	MR_Word	mr7 __asm__("l2");
register	MR_Word	mr8 __asm__("l3");
register	MR_Word	mr9 __asm__("l4");

/* we could use l5, l6, and l7 as well, */
/* but for the moment at least I'll leave them for gcc */

#define save_regs_to_mem(save_area)		\
	(					\
		save_area[0] = mr0,		\
		save_area[1] = mr1,		\
		save_area[2] = mr2,		\
		save_area[3] = mr3,		\
		save_area[4] = mr4,		\
		save_area[5] = mr5,		\
		save_area[6] = mr6,		\
		save_area[7] = mr7,		\
		save_area[8] = mr8,		\
		save_area[9] = mr9,		\
		(void)0				\
	)

#define restore_regs_from_mem(save_area)	\
	(					\
		mr0 = save_area[0],		\
		mr1 = save_area[1],		\
		mr2 = save_area[2],		\
		mr3 = save_area[3],		\
		mr4 = save_area[4],		\
		mr5 = save_area[5],		\
		mr6 = save_area[6],		\
		mr7 = save_area[7],		\
		mr8 = save_area[8],		\
		mr9 = save_area[9],		\
		(void)0				\
	)

/* for save_transient_regs_to_mem(), we probably don't have to save
  the registers marked above as `potentially non-clobbered', but
  I haven't verified that yet */

#define save_transient_regs_to_mem(save_area) \
	save_regs_to_mem(save_area)
#define restore_transient_regs_from_mem(save_area) \
	restore_regs_from_mem(save_area)

#define	mr10	MR_fake_reg[10]
#define	mr11	MR_fake_reg[11]
#define	mr12	MR_fake_reg[12]
#define	mr13	MR_fake_reg[13]
#define	mr14	MR_fake_reg[14]
#define	mr15	MR_fake_reg[15]
#define	mr16	MR_fake_reg[16]
#define	mr17	MR_fake_reg[17]
#define	mr18	MR_fake_reg[18]
#define	mr19	MR_fake_reg[19]
#define	mr20	MR_fake_reg[20]
#define	mr21	MR_fake_reg[21]
#define	mr22	MR_fake_reg[22]
#define	mr23	MR_fake_reg[23]
#define	mr24	MR_fake_reg[24]
#define	mr25	MR_fake_reg[25]
#define	mr26	MR_fake_reg[26]
#define	mr27	MR_fake_reg[27]
#define	mr28	MR_fake_reg[28]
#define	mr29	MR_fake_reg[29]
#define	mr30	MR_fake_reg[30]
#define	mr31	MR_fake_reg[31]
#define	mr32	MR_fake_reg[32]
#define	mr33	MR_fake_reg[33]
#define	mr34	MR_fake_reg[34]
#define	mr35	MR_fake_reg[35]
#define	mr36	MR_fake_reg[36]
#define	mr37	MR_fake_reg[37]

#endif

#endif /* not SPARC_REGS_H */
