/*
** Copyright (C) 1994-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#ifndef NO_REGS_H
#define NO_REGS_H

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
#define NUM_REAL_REGS 0

/* A pair of macros to save/restore all of the physical machine
   registers.  They should copy/restore the physical machine
   registers to/from the memory pointed to by the supplied `save_area'
   parameter (which should be a pointer of type `Word *').
*/
#define save_regs_to_mem(save_area)		((void)0)
#define restore_regs_from_mem(save_area)	((void)0)

/* A pair of macros to save/restore any of the physical machine
   registers that get clobbered by calling a C function.  This is used
   for machines with register windows.  They should copy/restore the windowed
   registers to/from the memory pointed to by the supplied `save_area'
   parameter (which should be a pointer of type `Word *').
*/

#define save_transient_regs_to_mem(save_area)		((void)0)
#define restore_transient_regs_from_mem(save_area)	((void)0)

/* mr0, ..., mr36 are macros that map to either the underlying physical
   machine register, if there is one, or otherwise to MR_fake_reg[n].
   For register numbers greater than 36, use mr(n).
*/

#define	mr0	MR_fake_reg[0]
#define	mr1	MR_fake_reg[1]
#define	mr2	MR_fake_reg[2]
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

#endif /* not NO_REGS_H */
