#ifndef NO_REGS_H
#define NO_REGS_H

/*
** Portable version.
** All "machine registers" are just slots in a global array.
*/

/* The number of real physical machine registers that we can use */
#define NUM_REAL_REGS 0

/* A pair of macros to save/restore all of the physical machine
   registers.  They should copy/restore the windowed
   registers to/from the corresponding fake_reg[n].
   This is used so that the register n can be simply addressed
   as fake_reg[n] without requiring a switch.
*/
#define save_registers()		((void)0)
#define restore_registers()		((void)0)

/* A pair of macros to save/restore any of the physical machine
   registers that get clobbered by calling a C function.  This is used
   for machines with register windows.  They should copy/restore the windowed
   registers to/from the corresponding fake_reg[n].
*/

#define save_transient_registers()	((void)0)
#define restore_transient_registers()	((void)0)

/* mr0, ..., mr36 are macros that map to either the underlying physical
   machine register, if there is one, or otherwise to fake_reg[n].
   For register numbers greater than 36, use mr(n).
*/

#define	mr0	fake_reg[0]
#define	mr1	fake_reg[1]
#define	mr2	fake_reg[2]
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

#endif /* NO_REGS_H */
