/*
** Copyright (C) 1994-1995, 1997-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_regorder.h - defines the mapping from the Mercury abstract machine
** registers (r1, r2, ..., hp, sp, etc.) to the underlying intermediate-level
** abstract machine memory (mr0, mr1, ...).
**
** This file should be #included from "mercury_regs.h" and nowhere else.
** The reason this is separate from "mercury_regs.h" is so that it could,
** at least in theory, be generated automatically based on
** profiling feedback from the register usage counts for a particular
** application.  However, currently we don't do that.
**
** If you change this file, you should also change the settings of
** NUM_REAL_R_REGS in ../configure.in.
*/

#ifndef MERCURY_REGORDER_H
#define MERCURY_REGORDER_H

/*
** If we are using an engine base register, then shift all
** the machine registers across by 1, and allocate mr0 to
** MR_engine_base
*/
#if defined(MR_THREAD_SAFE) && NUM_REAL_REGS > 0


/*
** r1 .. r32: the general-purpose Mercury registers.
**
** If you modify the r<N> to mr<N> mapping, make sure that you update
** the definition of MR_VIRTUAL_REG_MAP_BODY below and BOTH copies of
** the definitions of r1-r32.
*/
#define r1		count_usage(R_RN(1), mr3)
#define r2		count_usage(R_RN(2), mr4)
#define r3		count_usage(R_RN(3), mr5)
#define r4		count_usage(R_RN(4), mr7)
#define r5		count_usage(R_RN(5), mr8)
#define r6		count_usage(R_RN(6), mr11)
#define r7		count_usage(R_RN(7), mr12)
#define r8		count_usage(R_RN(8), mr13)
#define r9		count_usage(R_RN(9), mr14)
#define r10		count_usage(R_RN(10), mr15)
#define r11		count_usage(R_RN(11), mr16)
#define r12		count_usage(R_RN(12), mr17)
#define r13		count_usage(R_RN(13), mr18)
#define r14		count_usage(R_RN(14), mr19)
#define r15		count_usage(R_RN(15), mr20)
#define r16		count_usage(R_RN(16), mr21)
#define r17		count_usage(R_RN(17), mr22)
#define r18		count_usage(R_RN(18), mr23)
#define r19		count_usage(R_RN(19), mr24)
#define r20		count_usage(R_RN(20), mr25)
#define r21		count_usage(R_RN(21), mr26)
#define r22		count_usage(R_RN(22), mr27)
#define r23		count_usage(R_RN(23), mr28)
#define r24		count_usage(R_RN(24), mr29)
#define r25		count_usage(R_RN(25), mr30)
#define r26		count_usage(R_RN(26), mr31)
#define r27		count_usage(R_RN(27), mr32)
#define r28		count_usage(R_RN(28), mr33)
#define r29		count_usage(R_RN(29), mr34)
#define r30		count_usage(R_RN(30), mr35)
#define r31		count_usage(R_RN(31), mr36)
#define r32		count_usage(R_RN(32), mr37)

	/* Keep this in sync with the actual defintions below */
#define MR_real_reg_number_sp MR_real_reg_number_mr1

/*
** The special-purpose Mercury registers:
**	hp, sp, succip, etc.
**
** If you modify the following block, make sure that you update
** the definitions of MR_NUM_SPECIAL_REG, MR_MAX_SPECIAL_REG_MR,
** and MR_saved_*.
*/

/*
** first, the "very special" registers -- these may go in real machine regs
*/
#define MR_engine_base	LVALUE_CAST(Word *, count_usage(MR_SP_RN, mr0))
#define MR_succip	LVALUE_CAST(Code *, count_usage(MR_SI_RN, mr2))
#define MR_hp		LVALUE_CAST(Word *, count_usage(MR_HP_RN, mr6))
#define MR_sp		LVALUE_CAST(Word *, count_usage(MR_SP_RN, mr1))
#define MR_curfr	LVALUE_CAST(Word *, count_usage(MR_CF_RN, mr9))
#define MR_maxfr	LVALUE_CAST(Word *, count_usage(MR_MF_RN, mr10))
/*
** next, the remainder of the special registers -- these go in the
** fake_reg array, or in some cases in ordinary global variables.
*/
#define MR_sol_hp	LVALUE_CAST(Word *, count_usage(MR_SOL_HP_RN, mr(38)))
#define MR_min_hp_rec	LVALUE_CAST(Word *, count_usage(MR_MIN_HP_REC, mr(39)))
#define MR_min_sol_hp_rec	LVALUE_CAST(Word *,	\
			count_usage(MR_MIN_HP_REC, mr(40)))
#define MR_global_hp	LVALUE_CAST(Word *,	\
			count_usage(MR_GLOBAL_HP_RN, mr(41)))
#define MR_gen_next	LVALUE_CAST(Integer,	\
			count_usage(MR_GEN_NEXT_RN, mr(42)))
#define MR_gen_stack	LVALUE_CAST(struct MR_GeneratorStackFrameStruct *, \
			count_usage(MR_GEN_STACK_RN, mr(43)))
#define MR_cut_next	LVALUE_CAST(Integer,	\
			count_usage(MR_CUT_NEXT_RN, mr(44)))
#define MR_cut_stack	LVALUE_CAST(struct MR_CutStackFrameStruct *, \
			count_usage(MR_CUT_STACK_RN, mr(45)))

#define MR_trail_ptr	count_usage(MR_TRAIL_PTR_RN, MR_trail_ptr_var)
#define MR_ticket_counter	 \
		count_usage(MR_TICKET_COUNTER_RN, MR_ticket_counter_var)
#define MR_ticket_high_water	 \
		count_usage(MR_TICKET_HIGH_WATER_RN, MR_ticket_high_water_var)

/*
** the number of "very special" registers, i.e. special registers that can
** be allocated in real machine registers:
** MR_engine_base, MR_succip, MR_hp, MR_sp, MR_curfr, MR_maxfr
*/
#define MR_NUM_VERY_SPECIAL_REG	6

/* the number of special-purpose Mercury registers */
#define MR_NUM_SPECIAL_REG	16

/* the maximum mrN number of special registers */
#define	MR_MAX_SPECIAL_REG_MR	45

/*
** The MR_saved_foo macros are like MR_foo except that
** they access the underlying fake_reg slot rather than
** the real machine register.
*/

#define MR_saved_succip(save_area)	LVALUE_CAST(Code *, save_area[2])
#define MR_saved_hp(save_area)		LVALUE_CAST(Word *, save_area[6])
#define MR_saved_sp(save_area)		LVALUE_CAST(Word *, save_area[1])
#define MR_saved_curfr(save_area)	LVALUE_CAST(Word *, save_area[9])
#define MR_saved_maxfr(save_area)	LVALUE_CAST(Word *, save_area[10])
#define MR_saved_sol_hp(save_area)	LVALUE_CAST(Word *, save_area[38])
#define MR_saved_min_hp_rec(save_area)	LVALUE_CAST(Word *, save_area[39])
#define MR_saved_min_sol_hp_rec(save_area) LVALUE_CAST(Word *, save_area[40])
#define MR_saved_global_hp(save_area)	LVALUE_CAST(Word *, save_area[41])
#define MR_saved_gen_next(save_area)	LVALUE_CAST(Integer, save_area[42])
#define MR_saved_gen_stack(save_area)	LVALUE_CAST(			      \
					struct MR_GeneratorStackFrameStruct *,\
					save_area[43])
#define MR_saved_cut_next(save_area)	LVALUE_CAST(Integer, save_area[44])
#define MR_saved_cut_stack(save_area)	LVALUE_CAST(			      \
					struct MR_CutStackFrameStruct *,      \
					save_area[45])

#define VIRTUAL_REG_MAP_BODY	{ \
	3, \
	4, \
	5, \
	7, \
	8, \
	11, \
	12, \
	13, \
	14, \
	15, \
	16, \
	17, \
	18, \
	19, \
	20, \
	21, \
	22, \
	23, \
	24, \
	25, \
	26, \
	27, \
	28, \
	29, \
	30, \
	31, \
	32, \
	33, \
	34, \
	35, \
	36, \
	37, \
}

#else /* !MR_THREAD_SAFE or NUM_REAL_REGS == 0 */

/*
** If you modify the r<N> to mr<N> mapping, make sure that you update
** the definition of MR_VIRTUAL_REG_MAP_BODY below and BOTH copies of
** the definitions of r1-r32.
*/
#define r1		count_usage(R_RN(1), mr2)
#define r2		count_usage(R_RN(2), mr3)
#define r3		count_usage(R_RN(3), mr4)
#define r4		count_usage(R_RN(4), mr6)
#define r5		count_usage(R_RN(5), mr7)
#define r6		count_usage(R_RN(6), mr10)
#define r7		count_usage(R_RN(7), mr11)
#define r8		count_usage(R_RN(8), mr12)
#define r9		count_usage(R_RN(9), mr13)
#define r10		count_usage(R_RN(10), mr14)
#define r11		count_usage(R_RN(11), mr15)
#define r12		count_usage(R_RN(12), mr16)
#define r13		count_usage(R_RN(13), mr17)
#define r14		count_usage(R_RN(14), mr18)
#define r15		count_usage(R_RN(15), mr19)
#define r16		count_usage(R_RN(16), mr20)
#define r17		count_usage(R_RN(17), mr21)
#define r18		count_usage(R_RN(18), mr22)
#define r19		count_usage(R_RN(19), mr23)
#define r20		count_usage(R_RN(20), mr24)
#define r21		count_usage(R_RN(21), mr25)
#define r22		count_usage(R_RN(22), mr26)
#define r23		count_usage(R_RN(23), mr27)
#define r24		count_usage(R_RN(24), mr28)
#define r25		count_usage(R_RN(25), mr29)
#define r26		count_usage(R_RN(26), mr30)
#define r27		count_usage(R_RN(27), mr31)
#define r28		count_usage(R_RN(28), mr32)
#define r29		count_usage(R_RN(29), mr33)
#define r30		count_usage(R_RN(30), mr34)
#define r31		count_usage(R_RN(31), mr35)
#define r32		count_usage(R_RN(32), mr36)

	/* Keep this in sync with the actual defintions below */
#define MR_real_reg_number_sp MR_real_reg_number_mr0

/*
** The special-purpose Mercury registers:
**	hp, sp, succip, etc.
**
** If you modify the following block, make sure that you update
** the definitions of MR_NUM_SPECIAL_REG, MR_MAX_SPECIAL_REG_MR,
** and MR_saved_*.
*/

/*
** first, the "very special" registers -- these may go in real machine regs
*/
#define MR_succip	LVALUE_CAST(Code *, count_usage(MR_SI_RN, mr1))
#define MR_hp		LVALUE_CAST(Word *, count_usage(MR_HP_RN, mr5))
#define MR_sp		LVALUE_CAST(Word *, count_usage(MR_SP_RN, mr0))
#define MR_curfr	LVALUE_CAST(Word *, count_usage(MR_CF_RN, mr8))
#define MR_maxfr	LVALUE_CAST(Word *, count_usage(MR_MF_RN, mr9))
/*
** next, the remainder of the special registers -- these go in the
** fake_reg array, or in some cases in ordinary global variables.
*/
#define MR_sol_hp	LVALUE_CAST(Word *, count_usage(MR_SOL_HP_RN, mr(37)))
#define MR_min_hp_rec	LVALUE_CAST(Word *, count_usage(MR_MIN_HP_REC, mr(38)))
#define MR_min_sol_hp_rec	LVALUE_CAST(Word *,	\
			count_usage(MR_MIN_HP_REC, mr(39)))
#define MR_global_hp	LVALUE_CAST(Word *,	\
			count_usage(MR_GLOBAL_HP_RN, mr(40)))
#define MR_trail_ptr	count_usage(MR_TRAIL_PTR_RN, MR_trail_ptr_var)
#define MR_ticket_counter	 \
		count_usage(MR_TICKET_COUNTER_RN, MR_ticket_counter_var)
#define MR_ticket_high_water	 \
		count_usage(MR_TICKET_HIGH_WATER_RN, MR_ticket_high_water_var)
#define MR_gen_next	LVALUE_CAST(Integer,	\
			count_usage(MR_GEN_NEXT_RN, mr(41)))
#define MR_gen_stack	LVALUE_CAST(struct MR_GeneratorStackFrameStruct *, \
			count_usage(MR_GEN_STACK_RN, mr(42)))
#define MR_cut_next	LVALUE_CAST(Integer,	\
			count_usage(MR_CUT_NEXT_RN, mr(43)))
#define MR_cut_stack	LVALUE_CAST(struct MR_CutStackFrameStruct *, \
			count_usage(MR_CUT_STACK_RN, mr(44)))

/*
** the number of "very special" registers, i.e. special registers that can
** be allocated in real machine registers:
** MR_succip, MR_hp, MR_sp, MR_curfr, MR_maxfr
*/
#define MR_NUM_VERY_SPECIAL_REG	5

/* the number of special registers */
#define MR_NUM_SPECIAL_REG	15

/* the maximum mrN number of special, non rN registers */
#define	MR_MAX_SPECIAL_REG_MR	44

/*
** The MR_saved_foo macros are like MR_foo except that
** they access the underlying fake_reg slot rather than
** the real machine register.
*/

#define MR_saved_succip(save_area)	LVALUE_CAST(Code *, save_area[1])
#define MR_saved_hp(save_area)		LVALUE_CAST(Word *, save_area[5])
#define MR_saved_sp(save_area)		LVALUE_CAST(Word *, save_area[0])
#define MR_saved_curfr(save_area)	LVALUE_CAST(Word *, save_area[8])
#define MR_saved_maxfr(save_area)	LVALUE_CAST(Word *, save_area[9])
#define MR_saved_sol_hp(save_area)	LVALUE_CAST(Word *, save_area[37])
#define MR_saved_min_hp_rec(save_area)	LVALUE_CAST(Word *, save_area[38])
#define MR_saved_min_sol_hp_rec(save_area) LVALUE_CAST(Word *, save_area[39])
#define MR_saved_global_hp(save_area)	LVALUE_CAST(Word *, save_area[40])
#define MR_saved_gen_stack(save_area)	LVALUE_CAST(Integer, save_area[41])
#define MR_saved_gen_next(save_area)	LVALUE_CAST(			      \
					struct MR_GeneratorStackFrameStruct *,\
					save_area[42])
#define MR_saved_cut_stack(save_area)	LVALUE_CAST(Integer, save_area[43])
#define MR_saved_cut_next(save_area)	LVALUE_CAST(			      \
					struct MR_CutStackFrameStruct *,      \
					save_area[44])

#define VIRTUAL_REG_MAP_BODY	{ \
	2, \
	3, \
	4, \
	6, \
	7, \
	10, \
	11, \
	12, \
	13, \
	14, \
	15, \
	16, \
	17, \
	18, \
	19, \
	20, \
	21, \
	22, \
	23, \
	24, \
	25, \
	26, \
	27, \
	28, \
	29, \
	30, \
	31, \
	32, \
	33, \
	34, \
	35, \
	36, \
}

#endif

#endif /* not MERCURY_REGORDER_H */
