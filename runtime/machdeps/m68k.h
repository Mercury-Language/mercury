/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef M68K_REGS_H
#define M68K_REGS_H

/*
** Machine registers mr0 - mr36 for the Motorola 68000 architecture.
*/

/*
** WARNING: THIS FILE IS COMPLETELY UNTESTED!
**
** We don't have a 68k machine to test it on.
** We'd be happy to accept donations, though...
**
** To enable the use of this file, add a case for it to `../regs.h'.
*/

/*
** Register a7 is the C stack pointer.
** Register a6 is the C frame pointer.
** Register a5 is the GOT (Global Offset Table) register used for
**       position independent code.
** Registers d0, d1, a0, and a1 are callee-save.
** That leaves us registers a2-a4, and d2-d7 to play with.
**
** It's a pity that the m68k has separate address and data registers,
** it doesn't really suit our RISC-inspired virtual machine model.
** Oh well, I guess we'll just have to use the data registers and
** see what happens.
*/

#define NUM_REAL_REGS 5

register	Word	mr0 __asm__("a2");	/* sp */
register	Word	mr1 __asm__("a3");	/* succip */
register	Word	mr2 __asm__("d2");	/* r1 */
register	Word	mr3 __asm__("d3");	/* r2 */
register	Word	mr4 __asm__("d4");	/* r3 */

#define save_regs_to_mem(save_area) (		\
	save_area[0] = mr0,			\
	save_area[1] = mr1,			\
	save_area[2] = mr2,			\
	save_area[3] = mr3,			\
	save_area[4] = mr4,			\
	(void)0					\
)

#define restore_regs_from_mem(save_area) (	\
	mr0 = save_area[0],			\
	mr1 = save_area[1],			\
	mr2 = save_area[2],			\
	mr3 = save_area[3],			\
	mr4 = save_area[4],			\
	(void)0					\
)

#define save_transient_regs_to_mem(save_area)		((void)0)
#define restore_transient_regs_from_mem(save_area)	((void)0)

#define	mr5	fake_reg[5]
#define	mr6	fake_reg[6]
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

#endif /* not M68K_REGS_H */
