/*
** Copyright (C) 1993-1998, 2000, 2003, 2007 The University of Melbourne.
** Copyright (C) 2016, 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/
#ifndef MR_MACHDEPS_I386_REGS_H
#define MR_MACHDEPS_I386_REGS_H

/*
** Machine registers MR_mr0 - MR_mr36 for the Intel 386 architecture.
**
** The first MR_NUM_REAL_REGS of these are real machine registers.
** The others are just slots in a global array.
**
** At the moment we're only using the callee-save registers esi and edi.
** If we are using position-independent code (PIC), then ebx is reserved.
** We avoid using it even in non-PIC code, both to simplify linking
** PIC and non-PIC code, and because gcc bugs tend to bite if we use ebx.
**
** For the 386, if `ebp' is not used as a global register variable,
** then the code *must not* be compiled with `-fomit-frame-pointer'.
** If it is, gcc may generate code which saves `ebp' in the function
** prologue, uses `ebp' to hold a local variable in the middle of the
** function, and restores `ebp' in the function epilogue. This causes
** problems if we jump into and out of the middle of functions, because
** `ebp' will get clobbered.
**
** Conversely, if `ebp' were to be used as a global register variable,
** then the code would *have* be compiled with `-fomit-frame-pointer'.
** Otherwise, gcc will try to use it to point to local variables,
** and gcc's use of `ebp' will conflict with our use of it.
** Unfortunately, even `-fomit-frame-pointer' is not enough, since
** there are some functions for which gcc cannot avoid the use of
** the frame pointer. (E.g. the one containing io.init_state/2.)
*/

#define MR_NUM_REAL_REGS 2

register	MR_Word	MR_mr0 __asm__("esi");	/* sp */
register	MR_Word	MR_mr1 __asm__("edi");	/* succip */

#define MR_real_reg_number_mr0	esi
#define MR_real_reg_number_mr1	edi

#define MR_save_regs_to_mem(save_area) (	\
	save_area[0] = MR_mr0,			\
	save_area[1] = MR_mr1,			\
	(void)0					\
)

#define MR_restore_regs_from_mem(save_area) (	\
	MR_mr0 = save_area[0],			\
	MR_mr1 = save_area[1],			\
	(void)0					\
)

#define MR_save_transient_regs_to_mem(save_area)	((void)0)
#define MR_restore_transient_regs_from_mem(save_area)	((void)0)

#define MR_mr2	MR_fake_reg[2]
#define	MR_mr3	MR_fake_reg[3]
#define	MR_mr4	MR_fake_reg[4]
#define	MR_mr5	MR_fake_reg[5]
#define	MR_mr6	MR_fake_reg[6]
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

#endif /* not MR_MACHDEPS_I386_REGS_H */
