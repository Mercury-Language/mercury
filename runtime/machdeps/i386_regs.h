/*
** Copyright (C) 1993-1997 The University of Melbourne.
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
** (ebx, esi, edi).  If we're using position-independent code (PIC),
** i.e. if this code is being compiled with `-fpic' or `-fPIC',
** then ebx is reserved, so we use only esi and edi.
** We also avoid using ebx if compiled with -DPIC_REG;
** compiling with -DPIC_REG but not -fpic allows one to
** preserve link-compatibility with PIC code while generating
** non-PIC code.
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

/*
** Are we using PIC?
*/
#if (defined(__PIC__) || defined(__pic__)) && !defined(PIC)
  #define PIC 1
#endif

/*
** Should we keep the GOT register (ebx) free for PIC code?
*/
#if PIC && !defined(PIC_REG)
  #define PIC_REG 1
#endif

#if PIC_REG
  #define NUM_REAL_REGS 2
#else
  #define NUM_REAL_REGS 3
#endif

register	Word	mr0 __asm__("esi");	/* sp */
register	Word	mr1 __asm__("edi");	/* succip */
#endif

#if PIC_REG
  #define mr2	MR_fake_reg[2]
#else
  register	Word	mr2 __asm__("ebx");	/* r1 */
#endif

#if PIC_REG

#define save_regs_to_mem(save_area) (		\
	save_area[0] = mr0,			\
	save_area[1] = mr1,			\
	(void)0					\
)

#define restore_regs_from_mem(save_area) (	\
	mr0 = save_area[0],			\
	mr1 = save_area[1],			\
	(void)0					\
)

#else /* ! PIC_REG */

#define save_regs_to_mem(save_area) (		\
	save_area[0] = mr0,			\
	save_area[1] = mr1,			\
	save_area[2] = mr2,			\
	(void)0					\
)

#define restore_regs_from_mem(save_area) (	\
	mr0 = save_area[0],			\
	mr1 = save_area[1],			\
	mr2 = save_area[2],			\
	(void)0					\
)

#endif	/* ! PIC_REG */

#define save_transient_regs_to_mem(save_area)		((void)0)
#define restore_transient_regs_from_mem(save_area)	((void)0)

#define	mr3	MR_fake_reg[3]
#define	mr4	MR_fake_reg[4]
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

#endif /* not I386_REGS_H */
