/*
** Copyright (C) 1996-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef RS6000_REGS_H
#define RS6000_REGS_H

/*
** Machine registers mr0 - mr36 for the RS/6000 architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** For the RS/6000, registers r13 to r31 are callee-save.
** Currently we're just using r13-r22 and leaving r23-r31 for gcc.
*/

#define NUM_REAL_REGS 10

register 	Word	mr0 __asm__("r13");
register	Word	mr1 __asm__("r14");
register	Word	mr2 __asm__("r15");
register	Word	mr3 __asm__("r16");
register	Word	mr4 __asm__("r17");
register	Word	mr5 __asm__("r18");
register	Word	mr6 __asm__("r19");
register	Word	mr7 __asm__("r20");
register	Word	mr8 __asm__("r21");
register	Word	mr9 __asm__("r22");

#define save_regs_to_mem(save_area) (		\
	save_area[0] = mr0,			\
	save_area[1] = mr1,			\
	save_area[2] = mr2,			\
	save_area[3] = mr3,			\
	save_area[4] = mr4,			\
	save_area[5] = mr5,			\
	save_area[6] = mr6,			\
	save_area[7] = mr7,			\
	save_area[8] = mr8,			\
	save_area[9] = mr9,			\
	(void)0					\
)

#define restore_regs_from_mem(save_area) (	\
	mr0 = save_area[0],			\
	mr1 = save_area[1],			\
	mr2 = save_area[2],			\
	mr3 = save_area[3],			\
	mr4 = save_area[4],			\
	mr5 = save_area[5],			\
	mr6 = save_area[6],			\
	mr7 = save_area[7],			\
	mr8 = save_area[8],			\
	mr9 = save_area[9],			\
	(void)0					\
)

#define save_transient_regs_to_mem(save_area)		((void)0)
#define restore_transient_regs_from_mem(save_area)	((void)0)

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

#endif /* not RS6000_REGS_H */
