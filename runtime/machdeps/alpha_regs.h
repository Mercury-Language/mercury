/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef ALPHA_REGS_H
#define ALPHA_REGS_H

/*
** Machine registers mr0 - mr36 for the Alpha architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** At the moment we're only using the callee-save registers.
** We have to use *all* of the callee-save registers if we
** want non-local gotos to work, because otherwise there are
** problems where a register such as $15 is saved in the
** function prologue and restored in the function epilogue,
** but since we jump into and out of the middle of the function
** it gets clobbered.
*/

#define NUM_REAL_REGS 7

register 	Word	mr0 __asm__("$9");	/* register s0 */
register	Word	mr1 __asm__("$10");	/* register s1 */
register	Word	mr2 __asm__("$11");	/* register s2 */
register	Word	mr3 __asm__("$12");	/* register s3 */
register	Word	mr4 __asm__("$13");	/* register s4 */
register	Word	mr5 __asm__("$14");	/* register s5 */
register	Word	mr6 __asm__("$15");	/* the frame pointer (fp) */

#define save_regs_to_mem(save_area)	(	\
	save_area[0] = mr0,			\
	save_area[1] = mr1,			\
	save_area[2] = mr2,			\
	save_area[3] = mr3,			\
	save_area[4] = mr4,			\
	save_area[5] = mr5,			\
	save_area[6] = mr6,			\
	(void)0					\
)

#define restore_regs_from_mem(save_area)	(	\
	mr0 = save_area[0],			\
	mr1 = save_area[1],			\
	mr2 = save_area[2],			\
	mr3 = save_area[3],			\
	mr4 = save_area[4],			\
	mr5 = save_area[5],			\
	mr6 = save_area[6],			\
	(void)0					\
)

#define save_transient_regs_to_mem(save_area)		((void)0)
#define restore_transient_regs_from_mem(save_area)	((void)0)

#define	mr7	fake_reg[7]
#define	mr8	fake_reg[8]
#define	mr9	fake_reg[9]
#define	mr10	fake_reg[10]
#define	mr11	fake_reg[11]
#define	mr12	fake_reg[12]
#define	mr13	fake_reg[13]
#define	mr14	fake_reg[14]
#define	mr15	fake_reg[15]
#define	mr16	fake_reg[16]
#define	mr17	fake_reg[17]
#define	mr18	fake_reg[18]
#define	mr19	fake_reg[19]
#define	mr20	fake_reg[20]
#define	mr21	fake_reg[21]
#define	mr22	fake_reg[22]
#define	mr23	fake_reg[23]
#define	mr24	fake_reg[24]
#define	mr25	fake_reg[25]
#define	mr26	fake_reg[26]
#define	mr27	fake_reg[27]
#define	mr28	fake_reg[28]
#define	mr29	fake_reg[29]
#define	mr30	fake_reg[30]
#define	mr31	fake_reg[31]
#define	mr32	fake_reg[32]
#define	mr33	fake_reg[33]
#define	mr34	fake_reg[34]
#define	mr35	fake_reg[35]
#define	mr36	fake_reg[36]

#endif /* not ALPHA_REGS_H */
