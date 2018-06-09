/*
** Copyright (C) 1994-1998, 2000 The University of Melbourne.
** Copyright (C) 2018 The Mercury team.
** This file is distributed under the terms specified in COPYING.LIB.
*/
#ifndef MR_MACHDEPS_NO_REGS_H
#define MR_MACHDEPS_NO_REGS_H

/*
** Portable version.
** All "machine registers" are just slots in a global array.
*/

/*
** The number of real physical machine registers that we can use.
**
** Note that when adding a header file for a new machine, you also
** need to add code to set NUM_REAL_R_REGS in ../configure.in.
*/
#define MR_NUM_REAL_REGS 0

/*
** A pair of macros to save/restore all of the physical machine
** registers.  They should copy/restore the physical machine
** registers to/from the memory pointed to by the supplied `save_area'
** parameter (which should be a pointer of type `MR_Word *').
*/

#define MR_save_regs_to_mem(save_area)			((void) 0)
#define MR_restore_regs_from_mem(save_area)		((void) 0)

/*
** A pair of macros to save/restore any of the physical machine
** registers that get clobbered by calling a C function.  This is used
** for machines with register windows.  They should copy/restore the windowed
** registers to/from the memory pointed to by the supplied `save_area'
** parameter (which should be a pointer of type `MR_Word *').
*/

#define MR_save_transient_regs_to_mem(save_area)	((void) 0)
#define MR_restore_transient_regs_from_mem(save_area)	((void) 0)

/*
** MR_mr0, ..., MR_mr36 are macros that map to either the underlying physical
** machine register, if there is one, or otherwise to MR_fake_reg[n].
** For register numbers greater than 36, use MR_mr(n).
*/

#define	MR_mr0	MR_fake_reg[0]
#define	MR_mr1	MR_fake_reg[1]
#define	MR_mr2	MR_fake_reg[2]
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

#endif /* not MR_MACHDEPS_NO_REGS_H */
