/*
** Copyright (C) 1997, 2000 The University of Melbourne.
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

register	MR_Word	mr0 __asm__("a2");	/* sp */
register	MR_Word	mr1 __asm__("a3");	/* succip */
register	MR_Word	mr2 __asm__("d2");	/* r1 */
register	MR_Word	mr3 __asm__("d3");	/* r2 */
register	MR_Word	mr4 __asm__("d4");	/* r3 */

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

#define	mr5	MR_fake_reg[5]
#define	mr6	MR_fake_reg[6]
#define	mr7	MR_fake_reg[7]
#define	mr8	MR_fake_reg[8]
#define	mr9	MR_fake_reg[9]
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

#endif /* not M68K_REGS_H */
