/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef I386_REGS_H
#define I386_REGS_H

/*
** Machine registers mr0 - mr36 for the Intel 386 architecture.
**
** The first NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** At the moment we're only using the callee-save registers
** (ebx, esi, edi).
**
** For the 386, if `ebp' is not used as a global register variable,
** then the code *must not* be compiled with `-fomit-frame-pointer'.
** If it is, gcc may generate code which saves `ebp' in the function
** prologue, uses `ebp' to hold a local variable in the middle of the
** function, and restores `ebp' in the function epilogue.  This causes
** problems if we jump into and out of the middle of functions, because
** `ebp' will get clobbered.
**
** Conversely, if `ebp' were to be used as a global register variable,
** then the code would *have* be compiled with `-fomit-frame-pointer'.
** Otherwise, gcc will try to use it to point to local variables,
** and gcc's use of `ebp' will conflict with our use of it.
** Unfortunately, even `-fomit-frame-pointer' is not enough, since
** there are some functions for which gcc cannot avoid the use of
** the frame pointer.  (E.g. the one containing io__init_state/2.)
*/

#define NUM_REAL_REGS 3

reg	Word	mr0 __asm__("esi");	/* sp */
reg	Word	mr1 __asm__("ebx");	/* succip */
reg	Word	mr2 __asm__("edi");	/* r1 */

#define save_registers()	(		\
	fake_reg[0] = mr0,			\
	fake_reg[1] = mr1,			\
	fake_reg[2] = mr2,			\
	(void)0					\
)

#define restore_registers()	(		\
	mr0 = fake_reg[0],			\
	mr1 = fake_reg[1],			\
	mr2 = fake_reg[2],			\
	(void)0					\
)

#define save_transient_registers()	((void)0)
#define restore_transient_registers()	((void)0)

#define	mr3	fake_reg[3]
#define	mr4	fake_reg[4]
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

#endif /* I386_REGS_H */
