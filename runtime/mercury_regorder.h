/*
** Copyright (C) 1994-1995,1997-2000,2003 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_regorder.h - defines the mapping from the Mercury abstract machine
** registers (MR_r1, MR_r2, ..., MR_hp, MR_sp, etc.) to the underlying
** intermediate-level abstract machine memory (MR_mr0, MR_mr1, ...).
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
** the machine registers across by 1, and allocate MR_mr0 to
** MR_engine_base
*/

#if defined(MR_THREAD_SAFE) && MR_NUM_REAL_REGS > 0

/*
** MR_r1 .. MR_r32: the general-purpose Mercury registers.
**
** If you modify the MR_r<N> to MR_mr<N> mapping, make sure that you update
** the definition of MR_VIRTUAL_REG_MAP_BODY below and BOTH copies of
** the definitions of MR_r1-MR_r32.
*/

#define MR_r1		MR_count_usage(R_RN(1), MR_mr3)
#define MR_r2		MR_count_usage(R_RN(2), MR_mr4)
#define MR_r3		MR_count_usage(R_RN(3), MR_mr5)
#define MR_r4		MR_count_usage(R_RN(4), MR_mr7)
#define MR_r5		MR_count_usage(R_RN(5), MR_mr8)
#define MR_r6		MR_count_usage(R_RN(6), MR_mr11)
#define MR_r7		MR_count_usage(R_RN(7), MR_mr12)
#define MR_r8		MR_count_usage(R_RN(8), MR_mr13)
#define MR_r9		MR_count_usage(R_RN(9), MR_mr14)
#define MR_r10		MR_count_usage(R_RN(10), MR_mr15)
#define MR_r11		MR_count_usage(R_RN(11), MR_mr16)
#define MR_r12		MR_count_usage(R_RN(12), MR_mr17)
#define MR_r13		MR_count_usage(R_RN(13), MR_mr18)
#define MR_r14		MR_count_usage(R_RN(14), MR_mr19)
#define MR_r15		MR_count_usage(R_RN(15), MR_mr20)
#define MR_r16		MR_count_usage(R_RN(16), MR_mr21)
#define MR_r17		MR_count_usage(R_RN(17), MR_mr22)
#define MR_r18		MR_count_usage(R_RN(18), MR_mr23)
#define MR_r19		MR_count_usage(R_RN(19), MR_mr24)
#define MR_r20		MR_count_usage(R_RN(20), MR_mr25)
#define MR_r21		MR_count_usage(R_RN(21), MR_mr26)
#define MR_r22		MR_count_usage(R_RN(22), MR_mr27)
#define MR_r23		MR_count_usage(R_RN(23), MR_mr28)
#define MR_r24		MR_count_usage(R_RN(24), MR_mr29)
#define MR_r25		MR_count_usage(R_RN(25), MR_mr30)
#define MR_r26		MR_count_usage(R_RN(26), MR_mr31)
#define MR_r27		MR_count_usage(R_RN(27), MR_mr32)
#define MR_r28		MR_count_usage(R_RN(28), MR_mr33)
#define MR_r29		MR_count_usage(R_RN(29), MR_mr34)
#define MR_r30		MR_count_usage(R_RN(30), MR_mr35)
#define MR_r31		MR_count_usage(R_RN(31), MR_mr36)
#define MR_r32		MR_count_usage(R_RN(32), MR_mr37)

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

#define MR_engine_base		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_SP_RN, MR_mr0))
#define MR_succip		MR_LVALUE_CAST(MR_Code *,		\
				MR_count_usage(MR_SI_RN, MR_mr2))
#define MR_hp			MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_HP_RN, MR_mr6))
#define MR_sp			MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_SP_RN, MR_mr1))
#define MR_curfr		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_CF_RN, MR_mr9))
#define MR_maxfr		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_MF_RN, MR_mr10))
/*
** next, the remainder of the special registers -- these go in the
** fake_reg array, or in some cases in ordinary global variables.
*/

#define MR_sol_hp		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_SOL_HP_RN, MR_mr(38)))
#define MR_min_hp_rec		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_MIN_HP_REC, MR_mr(39)))
#define MR_min_sol_hp_rec	MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_MIN_HP_REC, MR_mr(40)))
#define MR_global_hp		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_GLOBAL_HP_RN, MR_mr(41)))
#define MR_gen_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_GEN_NEXT_RN, MR_mr(42)))
#define MR_gen_stack		MR_LVALUE_CAST(				\
				struct MR_GenStackFrameStruct *,	\
				MR_count_usage(MR_GEN_STACK_RN, MR_mr(43)))
#define MR_cut_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_CUT_NEXT_RN, MR_mr(44)))
#define MR_cut_stack		MR_LVALUE_CAST(				\
				struct MR_CutStackFrameStruct *,	\
				MR_count_usage(MR_CUT_STACK_RN, MR_mr(45)))
#define MR_pneg_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_CUT_NEXT_RN, MR_mr(46)))
#define MR_pneg_stack		MR_LVALUE_CAST(				\
				struct MR_PNegStackFrameStruct *,	\
				MR_count_usage(MR_CUT_STACK_RN, MR_mr(47)))
#define MR_trail_ptr		MR_count_usage(MR_TRAIL_PTR_RN,		\
				MR_trail_ptr_var)
#define MR_ticket_counter	MR_count_usage(MR_TICKET_COUNTER_RN,	\
				MR_ticket_counter_var)
#define MR_ticket_high_water	MR_count_usage(MR_TICKET_HIGH_WATER_RN,	\
				MR_ticket_high_water_var)

/*
** the number of "very special" registers, i.e. special registers that can
** be allocated in real machine registers:
** MR_engine_base, MR_succip, MR_hp, MR_sp, MR_curfr, MR_maxfr
*/

#define MR_NUM_VERY_SPECIAL_REG	6

/* the number of special-purpose Mercury registers */
#define MR_NUM_SPECIAL_REG	16

/* the maximum MR_mrN number of special registers */
#define	MR_MAX_SPECIAL_REG_MR	47

/*
** The MR_saved_foo macros are like MR_foo except that
** they access the underlying fake_reg slot rather than
** the real machine register.
*/

#define MR_saved_succip(save_area)	MR_LVALUE_CAST(MR_Code *, \
						save_area[2])
#define MR_saved_hp(save_area)		MR_LVALUE_CAST(MR_Word *, \
						save_area[6])
#define MR_saved_sp(save_area)		MR_LVALUE_CAST(MR_Word *, \
						save_area[1])
#define MR_saved_curfr(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[9])
#define MR_saved_maxfr(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[10])
#define MR_saved_sol_hp(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[38])
#define MR_saved_min_hp_rec(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[39])
#define MR_saved_min_sol_hp_rec(save_area) MR_LVALUE_CAST(MR_Word *, \
						save_area[40])
#define MR_saved_global_hp(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[41])
#define MR_saved_gen_next(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[42])
#define MR_saved_gen_stack(save_area)	MR_LVALUE_CAST(struct \
						MR_GenStackFrameStruct *, \
						save_area[43])
#define MR_saved_cut_next(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[44])
#define MR_saved_cut_stack(save_area)	MR_LVALUE_CAST(struct \
						MR_CutStackFrameStruct *, \
						save_area[45])
#define MR_saved_pneg_next(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[46])
#define MR_saved_pneg_stack(save_area)	MR_LVALUE_CAST(struct \
						MR_PNegStackFrameStruct *, \
						save_area[47])

#define MR_VIRTUAL_REG_MAP_BODY	{ \
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

#else /* !MR_THREAD_SAFE or MR_NUM_REAL_REGS == 0 */

/*
** If you modify the MR_r<N> to MR_mr<N> mapping, make sure that you update
** the definition of MR_VIRTUAL_REG_MAP_BODY below and BOTH copies of
** the definitions of MR_r1-MR_r32.
*/

#define MR_r1		MR_count_usage(R_RN(1), MR_mr2)
#define MR_r2		MR_count_usage(R_RN(2), MR_mr3)
#define MR_r3		MR_count_usage(R_RN(3), MR_mr4)
#define MR_r4		MR_count_usage(R_RN(4), MR_mr6)
#define MR_r5		MR_count_usage(R_RN(5), MR_mr7)
#define MR_r6		MR_count_usage(R_RN(6), MR_mr10)
#define MR_r7		MR_count_usage(R_RN(7), MR_mr11)
#define MR_r8		MR_count_usage(R_RN(8), MR_mr12)
#define MR_r9		MR_count_usage(R_RN(9), MR_mr13)
#define MR_r10		MR_count_usage(R_RN(10), MR_mr14)
#define MR_r11		MR_count_usage(R_RN(11), MR_mr15)
#define MR_r12		MR_count_usage(R_RN(12), MR_mr16)
#define MR_r13		MR_count_usage(R_RN(13), MR_mr17)
#define MR_r14		MR_count_usage(R_RN(14), MR_mr18)
#define MR_r15		MR_count_usage(R_RN(15), MR_mr19)
#define MR_r16		MR_count_usage(R_RN(16), MR_mr20)
#define MR_r17		MR_count_usage(R_RN(17), MR_mr21)
#define MR_r18		MR_count_usage(R_RN(18), MR_mr22)
#define MR_r19		MR_count_usage(R_RN(19), MR_mr23)
#define MR_r20		MR_count_usage(R_RN(20), MR_mr24)
#define MR_r21		MR_count_usage(R_RN(21), MR_mr25)
#define MR_r22		MR_count_usage(R_RN(22), MR_mr26)
#define MR_r23		MR_count_usage(R_RN(23), MR_mr27)
#define MR_r24		MR_count_usage(R_RN(24), MR_mr28)
#define MR_r25		MR_count_usage(R_RN(25), MR_mr29)
#define MR_r26		MR_count_usage(R_RN(26), MR_mr30)
#define MR_r27		MR_count_usage(R_RN(27), MR_mr31)
#define MR_r28		MR_count_usage(R_RN(28), MR_mr32)
#define MR_r29		MR_count_usage(R_RN(29), MR_mr33)
#define MR_r30		MR_count_usage(R_RN(30), MR_mr34)
#define MR_r31		MR_count_usage(R_RN(31), MR_mr35)
#define MR_r32		MR_count_usage(R_RN(32), MR_mr36)

	/* Keep this in sync with the actual defintions below */
#define MR_real_reg_number_sp MR_real_reg_number_mr0

/*
** The special-purpose Mercury registers:
**	MR_hp, MR_sp, MR_succip, etc.
**
** If you modify the following block, make sure that you update
** the definitions of MR_NUM_SPECIAL_REG, MR_MAX_SPECIAL_REG_MR,
** and MR_saved_*.
*/

/*
** first, the "very special" registers -- these may go in real machine regs
*/

#define MR_succip		MR_LVALUE_CAST(MR_Code *, \
					MR_count_usage(MR_SI_RN, MR_mr1))
#define MR_hp			MR_LVALUE_CAST(MR_Word *, \
					MR_count_usage(MR_HP_RN, MR_mr5))
#define MR_sp			MR_LVALUE_CAST(MR_Word *, \
					MR_count_usage(MR_SP_RN, MR_mr0))
#define MR_curfr		MR_LVALUE_CAST(MR_Word *, \
					MR_count_usage(MR_CF_RN, MR_mr8))
#define MR_maxfr		MR_LVALUE_CAST(MR_Word *, \
					MR_count_usage(MR_MF_RN, MR_mr9))

/*
** next, the remainder of the special registers -- these go in the
** MR_fake_reg array, or in some cases in ordinary global variables.
*/

#define MR_sol_hp		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_SOL_HP_RN, MR_mr(37)))
#define MR_min_hp_rec		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_MIN_HP_REC, MR_mr(38)))
#define MR_min_sol_hp_rec	MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_MIN_HP_REC, MR_mr(39)))
#define MR_global_hp		MR_LVALUE_CAST(MR_Word *,		\
				MR_count_usage(MR_GLOBAL_HP_RN, MR_mr(40)))
#define MR_trail_ptr		MR_count_usage(MR_TRAIL_PTR_RN,		\
				MR_trail_ptr_var)
#define MR_ticket_counter	MR_count_usage(MR_TICKET_COUNTER_RN,	\
				MR_ticket_counter_var)
#define MR_ticket_high_water	MR_count_usage(MR_TICKET_HIGH_WATER_RN,	\
				MR_ticket_high_water_var)
#define MR_gen_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_GEN_NEXT_RN, MR_mr(41)))
#define MR_gen_stack		MR_LVALUE_CAST(				\
				struct MR_GenStackFrameStruct *,	\
				MR_count_usage(MR_GEN_STACK_RN, MR_mr(42)))
#define MR_cut_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_CUT_NEXT_RN, MR_mr(43)))
#define MR_cut_stack		MR_LVALUE_CAST(				\
				struct MR_CutStackFrameStruct *,	\
				MR_count_usage(MR_CUT_STACK_RN, MR_mr(44)))
#define MR_pneg_next		MR_LVALUE_CAST(MR_Integer,		\
				MR_count_usage(MR_CUT_NEXT_RN, MR_mr(45)))
#define MR_pneg_stack		MR_LVALUE_CAST(				\
				struct MR_PNegStackFrameStruct *,	\
				MR_count_usage(MR_CUT_STACK_RN, MR_mr(46)))

/*
** the number of "very special" registers, i.e. special registers that can
** be allocated in real machine registers:
** MR_succip, MR_hp, MR_sp, MR_curfr, MR_maxfr
*/

#define MR_NUM_VERY_SPECIAL_REG	5

/* the number of special registers */
#define MR_NUM_SPECIAL_REG	15

/* the maximum MR_mrN number of special, non rN registers */
#define	MR_MAX_SPECIAL_REG_MR	46

/*
** The MR_saved_foo macros are like MR_foo except that
** they access the underlying fake_reg slot rather than
** the real machine register.
*/

#define MR_saved_succip(save_area)	MR_LVALUE_CAST(MR_Code *, \
						save_area[1])
#define MR_saved_hp(save_area)		MR_LVALUE_CAST(MR_Word *, \
						save_area[5])
#define MR_saved_sp(save_area)		MR_LVALUE_CAST(MR_Word *, \
						save_area[0])
#define MR_saved_curfr(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[8])
#define MR_saved_maxfr(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[9])
#define MR_saved_sol_hp(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[37])
#define MR_saved_min_hp_rec(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[38])
#define MR_saved_min_sol_hp_rec(save_area) MR_LVALUE_CAST(MR_Word *, \
						save_area[39])
#define MR_saved_global_hp(save_area)	MR_LVALUE_CAST(MR_Word *, \
						save_area[40])
#define MR_saved_gen_stack(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[41])
#define MR_saved_gen_next(save_area)	MR_LVALUE_CAST(struct \
						MR_GenStackFrameStruct *, \
						save_area[42])
#define MR_saved_cut_stack(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[43])
#define MR_saved_cut_next(save_area)	MR_LVALUE_CAST(struct \
						MR_CutStackFrameStruct *,\
						save_area[44])
#define MR_saved_pneg_stack(save_area)	MR_LVALUE_CAST(MR_Integer, \
						save_area[45])
#define MR_saved_pneg_next(save_area)	MR_LVALUE_CAST(struct \
						MR_PNegStackFrameStruct *,\
						save_area[46])

#define MR_VIRTUAL_REG_MAP_BODY	{ \
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
