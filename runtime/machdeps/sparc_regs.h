/*
** Copyright (C) 1993-1997, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MR_MACHDEPS_SPARC_REGS_H
#define MR_MACHDEPS_SPARC_REGS_H

/*
** Machine registers MR_mr0 - MR_mr36 for the SPARC architecture.
**
** The first MR_NUM_REAL_REGS of these are real machine registers.
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

#ifndef MR_USE_GCC_NONLOCAL_GOTOS
  #error "On SPARCs, you must use non-local gotos if you want global registers"
#else

#define MR_NUM_REAL_REGS 10

register 	MR_Word	MR_mr0 __asm__("i0");
register	MR_Word	MR_mr1 __asm__("i1");	/* potentially non-clobbered */
register	MR_Word	MR_mr2 __asm__("i2");	/* potentially non-clobbered */
register	MR_Word	MR_mr3 __asm__("i3");	/* potentially non-clobbered */
register	MR_Word	MR_mr4 __asm__("i4");	/* potentially non-clobbered */
register	MR_Word	MR_mr5 __asm__("i5");	/* potentially non-clobbered */
register	MR_Word	MR_mr6 __asm__("l1");
register	MR_Word	MR_mr7 __asm__("l2");
register	MR_Word	MR_mr8 __asm__("l3");
register	MR_Word	MR_mr9 __asm__("l4");

/* we could use l5, l6, and l7 as well, */
/* but for the moment at least I'll leave them for gcc */

#define MR_save_regs_to_mem(save_area)		\
	(					\
		save_area[0] = MR_mr0,		\
		save_area[1] = MR_mr1,		\
		save_area[2] = MR_mr2,		\
		save_area[3] = MR_mr3,		\
		save_area[4] = MR_mr4,		\
		save_area[5] = MR_mr5,		\
		save_area[6] = MR_mr6,		\
		save_area[7] = MR_mr7,		\
		save_area[8] = MR_mr8,		\
		save_area[9] = MR_mr9,		\
		(void)0				\
	)

#define MR_restore_regs_from_mem(save_area)	\
	(					\
		MR_mr0 = save_area[0],		\
		MR_mr1 = save_area[1],		\
		MR_mr2 = save_area[2],		\
		MR_mr3 = save_area[3],		\
		MR_mr4 = save_area[4],		\
		MR_mr5 = save_area[5],		\
		MR_mr6 = save_area[6],		\
		MR_mr7 = save_area[7],		\
		MR_mr8 = save_area[8],		\
		MR_mr9 = save_area[9],		\
		(void)0				\
	)

/*
** For MR_save_transient_regs_to_mem(), we probably don't have to save
** the registers marked above as `potentially non-clobbered', but
** I haven't verified that yet
*/

#define MR_save_transient_regs_to_mem(save_area) \
	MR_save_regs_to_mem(save_area)
#define MR_restore_transient_regs_from_mem(save_area) \
	MR_restore_regs_from_mem(save_area)

#define	MR_mr10	MR_fake_reg[10]
#define	MR_mr11	MR_fake_reg[11]
#define	MR_mr12	MR_fake_reg[12]
#define	MR_mr13	MR_fake_reg[13]
#define	MR_mr14	MR_fake_reg[14]
#define	MR_mr15	MR_fake_reg[15]
#define	MR_mr16	MR_fake_reg[16]
#define	MR_mr17	MR_fake_reg[17]
#define	MR_mr18	MR_fake_reg[18]
#define	MR_mr19	MR_fake_reg[19]
#define	MR_mr20	MR_fake_reg[20]
#define	MR_mr21	MR_fake_reg[21]
#define	MR_mr22	MR_fake_reg[22]
#define	MR_mr23	MR_fake_reg[23]
#define	MR_mr24	MR_fake_reg[24]
#define	MR_mr25	MR_fake_reg[25]
#define	MR_mr26	MR_fake_reg[26]
#define	MR_mr27	MR_fake_reg[27]
#define	MR_mr28	MR_fake_reg[28]
#define	MR_mr29	MR_fake_reg[29]
#define	MR_mr30	MR_fake_reg[30]
#define	MR_mr31	MR_fake_reg[31]
#define	MR_mr32	MR_fake_reg[32]
#define	MR_mr33	MR_fake_reg[33]
#define	MR_mr34	MR_fake_reg[34]
#define	MR_mr35	MR_fake_reg[35]
#define	MR_mr36	MR_fake_reg[36]
#define	MR_mr37	MR_fake_reg[37]

#endif

#endif /* not MR_MACHDEPS_SPARC_REGS_H */
