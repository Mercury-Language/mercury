/*
** Copyright (C) 1994-1998, 2000 The University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/

#ifndef MR_MACHDEPS_ALPHA_REGS_H
#define MR_MACHDEPS_ALPHA_REGS_H

/*
** Machine registers MR_mr0 - MR_mr36 for the Alpha architecture.
**
** The first MR_NUM_REAL_REGS of these are real machine registers.
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

#define MR_NUM_REAL_REGS 7

register 	MR_Word	MR_mr0 __asm__("$9");	/* register s0 */
register	MR_Word	MR_mr1 __asm__("$10");	/* register s1 */
register	MR_Word	MR_mr2 __asm__("$11");	/* register s2 */
register	MR_Word	MR_mr3 __asm__("$12");	/* register s3 */
register	MR_Word	MR_mr4 __asm__("$13");	/* register s4 */
register	MR_Word	MR_mr5 __asm__("$14");	/* register s5 */
register	MR_Word	MR_mr6 __asm__("$15");	/* the frame pointer (fp) */

#define MR_real_reg_number_mr0	9
#define MR_real_reg_number_mr1	10
#define MR_real_reg_number_mr2	11
#define MR_real_reg_number_mr3	12
#define MR_real_reg_number_mr4	13
#define MR_real_reg_number_mr5	14
#define MR_real_reg_number_mr6	15

#define MR_save_regs_to_mem(save_area)	(	\
	save_area[0] = MR_mr0,			\
	save_area[1] = MR_mr1,			\
	save_area[2] = MR_mr2,			\
	save_area[3] = MR_mr3,			\
	save_area[4] = MR_mr4,			\
	save_area[5] = MR_mr5,			\
	save_area[6] = MR_mr6,			\
	(void)0					\
)

#define MR_restore_regs_from_mem(save_area)	(	\
	MR_mr0 = save_area[0],			\
	MR_mr1 = save_area[1],			\
	MR_mr2 = save_area[2],			\
	MR_mr3 = save_area[3],			\
	MR_mr4 = save_area[4],			\
	MR_mr5 = save_area[5],			\
	MR_mr6 = save_area[6],			\
	(void)0					\
)

#define MR_save_transient_regs_to_mem(save_area)	((void)0)
#define MR_restore_transient_regs_from_mem(save_area)	((void)0)

#define	MR_mr7	MR_fake_reg[7]
#define	MR_mr8	MR_fake_reg[8]
#define	MR_mr9	MR_fake_reg[9]
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

#endif /* not MR_MACHDEPS_ALPHA_REGS_H */
