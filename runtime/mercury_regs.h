// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-2007, 2011 The University of Melbourne.
// Copyright (C) 2016-2018, 2020 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_REGS_H
#define MERCURY_REGS_H

#include "mercury_conf.h"
// NOTE: we need to include mercury_conf_param.h separately here since
// some of the tests in the configure script need MR_GNUC to be defined,
// and mercury_conf.h might not exist until *after* the configure script
// has been run.

#include "mercury_conf_param.h"
#include "mercury_types.h"

////////////////////////////////////////////////////////////////////////////
// GNU C allows lvalue casts, so if we have gcc, use them.
// If we don't have gcc, then we can use * (type *) &lval,
// but that wouldn't work for gcc: since lval might be a global register,
// in which case we couldn't take its address.
// Similarly for comma expressions and conditional expressions.

#ifdef MR_GNUC
  #define MR_LVALUE_CAST(type, lval)    ((type)(lval))
  #define MR_LVALUE_SEQ(expr, lval)     ((expr),(lval))
  #define MR_LVALUE_COND(expr, x, y)    ((expr)?(x):(y))
#else
  #define MR_LVALUE_CAST(type, lval)    (*(type*)&(lval))
  #define MR_LVALUE_SEQ(expr, lval)     (*((expr),&(lval)))
  #define MR_LVALUE_COND(expr, x, y)    (*((expr)?&(x):&(y)))
#endif

////////////////////////////////////////////////////////////////////////////
// The registers of the Mercury virtual machine are built up using
// three levels of abstraction.
//
// The bottom level is the hardware description layer.
// This layer is defined separately for each architecture,
// in the header files in the machdeps subdirectory.
// The hardware description layer defines the first MR_NUM_REAL_REGS register
// variables MR_mr0, ..., MR_mr<MR_NUM_REAL_REGS-1> as the physical machine
// registers, with the remaining "registers" MR_mr<MR_NUM_REAL_REGS>, ...,
// MR_mr36 defined as corresponding slots in the MR_fake_reg array.
// (The MR_fake_reg array is part of the engine defined in mercury_engine.h.)
// This level also provides the macros MR_save_regs_to_mem(),
// MR_save_transient_regs_to_mem(), MR_restore_regs_from_mem(),
// and MR_restore_transient_regs_from_mem(), which copy MR_mr0, ...,
// MR_mr<MR_NUM_REAL_REGS-1> to and from their MR_fake_reg slots.
//
// The next level is the hardware abstraction layer. The hardware abstraction
// layer is at a similar level to the hardware description layer, and includes
// that as a subset, but in addition it provides a few more conveniences.
// This layer defines macros MR_mr(n) for n>36, and the macros
// MR_save_mhal_registers(), MR_restore_mhal_registers(),
// MR_save_transient_registers(), MR_restore_transient_registers(),
// MR_save_transient_hp(), and MR_restore_transient_hp().
// This layer is defined here in mercury_regs.h.
//
// The hardware abstraction layer thus provides a very large number
// of registers, which may be either real or fake. The lower the number,
// the greater the probability that the storage referred to will be
// a real machine register, and not a simulated one. The number of
// real machine registers is given by the macro MR_NUM_REAL_REGS.
//
// The final level is the Mercury abstract machine registers layer.
// This layer maps the Mercury virtual machine registers to the set
// MR_mr0..MR_mr37, MR_mr(38), MR_mr(39), ..., MR_mr(MR_MAX_FAKE_REG-1)
// which were provided by the hardware abstraction layer, and to some
// global variables.

////////////////////////////////////////////////////////////////////////////
// The hardware description layer.

#if defined(MR_USE_GCC_GLOBAL_REGISTERS)
  #ifndef MR_GNUC
    #error "You must use gcc if you define MR_USE_GCC_GLOBAL_REGISTERS."
  #endif

  #if defined(__mips__)
    #include "machdeps/mips_regs.h"
  #elif defined(__i386__)
    #include "machdeps/i386_regs.h"
  #elif defined(__x86_64__)
    #include "machdeps/x86_64_regs.h"
  #elif defined(__sparc__)
    #include "machdeps/sparc_regs.h"
  #elif defined(__hppa__)
    #include "machdeps/pa_regs.h"
  #elif defined(_POWER) || defined(__powerpc__) || defined(__ppc__)
    #include "machdeps/rs6000_regs.h"
  #elif defined(__ia64__)
    #include "machdeps/ia64_regs.h"
  #elif defined(__arm__)
    #include "machdeps/arm_regs.h"
  #else
    #error "MR_USE_GCC_GLOBAL_REGISTERS not yet supported on this machine."
  #endif
#else
    #include "machdeps/no_regs.h"
#endif

////////////////////////////////////////////////////////////////////////////
// The hardware abstraction layer.

// The machdeps header defines MR_mr0 .. MR_mr37;
// now define MR_mr(n) for n > 37.
//
// The value of n should pass MR_assert((n) > 37 && (n) < MR_MAX_FAKE_REG)

#define MR_mr(n)            (MR_fake_reg[n])

// The MR_save_mhal_registers() macro copies the physical machine registers
// to their corresponding slots in the MR_fake_reg array.

#define MR_save_mhal_registers()    MR_save_regs_to_mem(MR_fake_reg)

// The MR_restore_mhal_registers() macro sets the physical machine registers
// to the values in their corresponding slots in the fake_reg array
// If we are using a register for the engine base, then we'd better
// restore that from the thread specific data area, since the fake_reg
// array is accessed via that register.

#if defined(MR_THREAD_SAFE) && MR_NUM_REAL_REGS > 0
  #define MR_restore_mhal_registers()                                   \
    do {                                                                \
        MR_engine_base_word = (MR_Word) MR_thread_engine_base;          \
        MR_fake_reg[0] = (MR_Word) MR_engine_base;                      \
        MR_restore_regs_from_mem(MR_fake_reg);                          \
    } while (0)
#else
  #define MR_restore_mhal_registers()   MR_restore_regs_from_mem(MR_fake_reg)
#endif

// The MR_save_transient_registers() and MR_restore_transient_registers()
// macros are similar to MR_save_mhal_registers() and
// MR_restore_mhal_registers() except that they only save/restore registers
// which can be affected by calling or returning from a C function (e.g.
// by sliding register windows on SPARCs).

#define MR_save_transient_registers()                                   \
    MR_save_transient_regs_to_mem(MR_fake_reg)

#if defined(MR_THREAD_SAFE) && MR_NUM_REAL_REGS > 0
  #define MR_restore_transient_registers()                              \
    do {                                                                \
        MR_engine_base_word = (MR_Word) MR_thread_engine_base;          \
        MR_fake_reg[0] = (MR_Word) MR_engine_base;                      \
        MR_restore_transient_regs_from_mem(MR_fake_reg);                \
    } while (0)
#else
  #define MR_restore_transient_registers()                              \
        MR_restore_transient_regs_from_mem(MR_fake_reg)
#endif

// The MR_save_transient_hp() and MR_restore_transient_hp() macros
// are similar to MR_save_transient_regs()/MR_restore_transient_regs(),
// except that they only guarantee to save/restore the heap pointer, if any.
// (They might save/restore other regs too, though.)

#ifdef MR_CONSERVATIVE_GC
  #define MR_save_transient_hp()        // nothing
  #define MR_restore_transient_hp()     // nothing
#else
  // This code is suboptimal -- it would be more efficient to save/restore
  // just a single register rather than all the transient registers.

  #define MR_save_transient_hp()        MR_save_transient_registers()
  #define MR_restore_transient_hp()     MR_restore_transient_registers()
#endif

////////////////////////////////////////////////////////////////////////////
// The Mercury abstract machine registers layer.
//
// The Mercury abstract machine registers consist of five groups.
//
// - The general purpose registers that may be allocated to real machine
//   registers, MR_rN for 1 <= N <= MR_MAX_REAL_R_REG.
//
// - The general purpose registers that cannot be allocated to real machine
//   registers, MR_r(N) for MR_MAX_REAL_R_REG < N <= MR_MAX_VIRTUAL_R_REG.
//
// - The special purpose registers that may be allocated to real machine
//   registers, namely
//
//      MR_succip
//      MR_hp
//      MR_sp
//      MR_curfr
//      MR_maxfr
//
//   and maybe MR_engine_base. The number of these registers is given by
//   MR_NUM_SPECIAL_MAYBE_REAL_REG.
//
// - The special purpose registers that are always stored in global variables,
//   namely
//
//      MR_sol_hp
//      MR_min_hp_rec
//      MR_min_sol_hp_rec
//      MR_global_hp
//
//      MR_trail_ptr            if trailing is enabled
//      MR_ticket_counter       if trailing is enabled
//      MR_ticket_high_water    if trailing is enabled
//
//      MR_gen_next             if minimal model is enabled
//      MR_gen_stack            if minimal model is enabled
//      MR_cut_next             if minimal model is enabled
//      MR_cut_stack            if minimal model is enabled
//      MR_pneg_next            if minimal model is enabled
//      MR_pneg_stack           if minimal model is enabled
//
//      MR_parent_sp            if parallelism is enabled
//
//   The number of these registers is given by MR_NUM_SPECIAL_GLOBAL_REG.
//
//   XXX In parallel grades we cannot use global variables for these registers.
//   They need to be fields in the Mercury engine structure. We already do this
//   for MR_parent_sp, but incompletely for a few of the others.
//
// - The floating point registers, if present, are always stored in the
//   float_reg array and not a physical register.
//
// The Mercury abstract machine registers layer also provides MR_virtual_r(),
// MR_virtual_succip, MR_virtual_hp, etc., which are similar to mr<N>,
// MR_succip, MR_hp, etc. except that they always map to the underlying
// fake_reg rather than to the physical register.

#define MR_MAX_REAL_R_REG                 32
#define MR_MAX_VIRTUAL_R_REG            1024
#define MR_NUM_SPECIAL_MAYBE_REAL_REG      6
#define MR_NUM_SPECIAL_GLOBAL_REG         14
#define MR_NUM_SPECIAL_REG                                              \
    (MR_NUM_SPECIAL_MAYBE_REAL_REG + MR_NUM_SPECIAL_GLOBAL_REG)

// The values here should be in sync with the values of the functions
// max_real_r_reg and max_virtual_r_reg in compiler/llds_out.m.

#ifdef MR_MEASURE_REGISTER_USAGE
  #define MR_count_usage(num,reg)   MR_LVALUE_SEQ(MR_num_uses[num]++, reg)
#else
  #define MR_count_usage(num,reg)   (reg)
#endif

// The float registers only exist if MR_BOXED_FLOAT is defined. To reduce the
// need for #ifdefs, we define MR_MAX_VIRTUAL_F_REG even when float registers
// aren't used, to a small number to minimise space allocated in arrays.

#ifdef MR_BOXED_FLOAT
  #define MR_MAX_VIRTUAL_F_REG      1024
#else
  #define MR_MAX_VIRTUAL_F_REG         1
#endif

// The MR_fake_reg array has a slot for every register of the Mercury abstract
// machine, both general and special purpose.
//
// The first MR_NUM_SPECIAL_MAYBE_REAL_REG + MR_MAX_REAL_R_REG slots are
// allocated to the virtual machine registers that may be allocated machine
// registers. The mapping from the virtual machine register to the MR_fake_reg
// slot depends on whether we use a register to hold MR_engine_base. If we
// don't, then the last slot in this part of MR_fake_reg is unused. The next
// MR_NUM_SPECIAL_GLOBAL_REG slots are allocated to the rest of the special
// purpose abstract machine registers, and the remainder to the rest of the
// general purpose abstract machine registers.

#define MR_MAX_FAKE_REG     (MR_NUM_SPECIAL_REG + MR_MAX_VIRTUAL_R_REG)

#define MR_fake_reg         (MR_ENGINE(MR_eng_fake_reg))

// The definitions of the general purpose and special registers
// that may go into machine registers.
//
// If we are using an engine base register, then allocate MR_mr0 to
// MR_engine_base, and increase all other MR_mrN numbers by 1.
//
// If you change this section, you should also change the settings of
// NUM_REAL_R_REGS in ../configure.ac.
//
// You may also wish to use the MR_verify_fake_registers function
// to check that no MR_fake_reg slot is assigned to two abstract machine
// registers. (You need to set MR_VERIFY_FAKE_REGISTERS; you can then invoke
// MR_verify_fake_registers by including -X in MERCURY_OPTIONS.)

#define MR_R_SLOT(n)            (MR_real_r_reg_map[(n) - 1])

#if defined(MR_THREAD_SAFE) && MR_NUM_REAL_REGS > 0

  #define MR_engine_base_word       MR_count_usage(MR_SP_SLOT, MR_mr0)

  #define MR_engine_base            ((MR_Word *) MR_engine_base_word)

  #define MR_SI_SLOT                2
  #define MR_HP_SLOT                6
  #define MR_SP_SLOT                1
  #define MR_CF_SLOT                9
  #define MR_MF_SLOT                10

  #define MR_succip_word            MR_count_usage(MR_SI_SLOT, MR_mr2)
  #define MR_hp_word                MR_count_usage(MR_HP_SLOT, MR_mr6)
  #define MR_sp_word                MR_count_usage(MR_SP_SLOT, MR_mr1)
  #define MR_curfr_word             MR_count_usage(MR_CF_SLOT, MR_mr9)
  #define MR_maxfr_word             MR_count_usage(MR_MF_SLOT, MR_mr10)

  #define MR_real_reg_number_sp     MR_real_reg_number_mr1

  #define MR_r1                     MR_count_usage(MR_R_SLOT(1), MR_mr3)
  #define MR_r2                     MR_count_usage(MR_R_SLOT(2), MR_mr4)
  #define MR_r3                     MR_count_usage(MR_R_SLOT(3), MR_mr5)
  #define MR_r4                     MR_count_usage(MR_R_SLOT(4), MR_mr7)
  #define MR_r5                     MR_count_usage(MR_R_SLOT(5), MR_mr8)
  #define MR_r6                     MR_count_usage(MR_R_SLOT(6), MR_mr11)
  #define MR_r7                     MR_count_usage(MR_R_SLOT(7), MR_mr12)
  #define MR_r8                     MR_count_usage(MR_R_SLOT(8), MR_mr13)
  #define MR_r9                     MR_count_usage(MR_R_SLOT(9), MR_mr14)
  #define MR_r10                    MR_count_usage(MR_R_SLOT(10), MR_mr15)
  #define MR_r11                    MR_count_usage(MR_R_SLOT(11), MR_mr16)
  #define MR_r12                    MR_count_usage(MR_R_SLOT(12), MR_mr17)
  #define MR_r13                    MR_count_usage(MR_R_SLOT(13), MR_mr18)
  #define MR_r14                    MR_count_usage(MR_R_SLOT(14), MR_mr19)
  #define MR_r15                    MR_count_usage(MR_R_SLOT(15), MR_mr20)
  #define MR_r16                    MR_count_usage(MR_R_SLOT(16), MR_mr21)
  #define MR_r17                    MR_count_usage(MR_R_SLOT(17), MR_mr22)
  #define MR_r18                    MR_count_usage(MR_R_SLOT(18), MR_mr23)
  #define MR_r19                    MR_count_usage(MR_R_SLOT(19), MR_mr24)
  #define MR_r20                    MR_count_usage(MR_R_SLOT(20), MR_mr25)
  #define MR_r21                    MR_count_usage(MR_R_SLOT(21), MR_mr26)
  #define MR_r22                    MR_count_usage(MR_R_SLOT(22), MR_mr27)
  #define MR_r23                    MR_count_usage(MR_R_SLOT(23), MR_mr28)
  #define MR_r24                    MR_count_usage(MR_R_SLOT(24), MR_mr29)
  #define MR_r25                    MR_count_usage(MR_R_SLOT(25), MR_mr30)
  #define MR_r26                    MR_count_usage(MR_R_SLOT(26), MR_mr31)
  #define MR_r27                    MR_count_usage(MR_R_SLOT(27), MR_mr32)
  #define MR_r28                    MR_count_usage(MR_R_SLOT(28), MR_mr33)
  #define MR_r29                    MR_count_usage(MR_R_SLOT(29), MR_mr34)
  #define MR_r30                    MR_count_usage(MR_R_SLOT(30), MR_mr35)
  #define MR_r31                    MR_count_usage(MR_R_SLOT(31), MR_mr36)
  #define MR_r32                    MR_count_usage(MR_R_SLOT(32), MR_mr37)

  #define MR_REAL_R_REG_MAP_BODY    {                                   \
    3,                                                                  \
    4,                                                                  \
    5,                                                                  \
    7,                                                                  \
    8,                                                                  \
    11,                                                                 \
    12,                                                                 \
    13,                                                                 \
    14,                                                                 \
    15,                                                                 \
    16,                                                                 \
    17,                                                                 \
    18,                                                                 \
    19,                                                                 \
    20,                                                                 \
    21,                                                                 \
    22,                                                                 \
    23,                                                                 \
    24,                                                                 \
    25,                                                                 \
    26,                                                                 \
    27,                                                                 \
    28,                                                                 \
    29,                                                                 \
    30,                                                                 \
    31,                                                                 \
    32,                                                                 \
    33,                                                                 \
    34,                                                                 \
    35,                                                                 \
    36,                                                                 \
    37,                                                                 \
}

#else // !MR_THREAD_SAFE or MR_NUM_REAL_REGS == 0

  #define MR_SI_SLOT                1
  #define MR_HP_SLOT                5
  #define MR_SP_SLOT                0
  #define MR_CF_SLOT                8
  #define MR_MF_SLOT                9

  #define MR_succip_word            MR_count_usage(MR_SI_SLOT, MR_mr1)
  #define MR_hp_word                MR_count_usage(MR_HP_SLOT, MR_mr5)
  #define MR_sp_word                MR_count_usage(MR_SP_SLOT, MR_mr0)
  #define MR_curfr_word             MR_count_usage(MR_CF_SLOT, MR_mr8)
  #define MR_maxfr_word             MR_count_usage(MR_MF_SLOT, MR_mr9)

  #define MR_real_reg_number_sp     MR_real_reg_number_mr0

  #define MR_r1                     MR_count_usage(MR_R_SLOT(1), MR_mr2)
  #define MR_r2                     MR_count_usage(MR_R_SLOT(2), MR_mr3)
  #define MR_r3                     MR_count_usage(MR_R_SLOT(3), MR_mr4)
  #define MR_r4                     MR_count_usage(MR_R_SLOT(4), MR_mr6)
  #define MR_r5                     MR_count_usage(MR_R_SLOT(5), MR_mr7)
  #define MR_r6                     MR_count_usage(MR_R_SLOT(6), MR_mr10)
  #define MR_r7                     MR_count_usage(MR_R_SLOT(7), MR_mr11)
  #define MR_r8                     MR_count_usage(MR_R_SLOT(8), MR_mr12)
  #define MR_r9                     MR_count_usage(MR_R_SLOT(9), MR_mr13)
  #define MR_r10                    MR_count_usage(MR_R_SLOT(10), MR_mr14)
  #define MR_r11                    MR_count_usage(MR_R_SLOT(11), MR_mr15)
  #define MR_r12                    MR_count_usage(MR_R_SLOT(12), MR_mr16)
  #define MR_r13                    MR_count_usage(MR_R_SLOT(13), MR_mr17)
  #define MR_r14                    MR_count_usage(MR_R_SLOT(14), MR_mr18)
  #define MR_r15                    MR_count_usage(MR_R_SLOT(15), MR_mr19)
  #define MR_r16                    MR_count_usage(MR_R_SLOT(16), MR_mr20)
  #define MR_r17                    MR_count_usage(MR_R_SLOT(17), MR_mr21)
  #define MR_r18                    MR_count_usage(MR_R_SLOT(18), MR_mr22)
  #define MR_r19                    MR_count_usage(MR_R_SLOT(19), MR_mr23)
  #define MR_r20                    MR_count_usage(MR_R_SLOT(20), MR_mr24)
  #define MR_r21                    MR_count_usage(MR_R_SLOT(21), MR_mr25)
  #define MR_r22                    MR_count_usage(MR_R_SLOT(22), MR_mr26)
  #define MR_r23                    MR_count_usage(MR_R_SLOT(23), MR_mr27)
  #define MR_r24                    MR_count_usage(MR_R_SLOT(24), MR_mr28)
  #define MR_r25                    MR_count_usage(MR_R_SLOT(25), MR_mr29)
  #define MR_r26                    MR_count_usage(MR_R_SLOT(26), MR_mr30)
  #define MR_r27                    MR_count_usage(MR_R_SLOT(27), MR_mr31)
  #define MR_r28                    MR_count_usage(MR_R_SLOT(28), MR_mr32)
  #define MR_r29                    MR_count_usage(MR_R_SLOT(29), MR_mr33)
  #define MR_r30                    MR_count_usage(MR_R_SLOT(30), MR_mr34)
  #define MR_r31                    MR_count_usage(MR_R_SLOT(31), MR_mr35)
  #define MR_r32                    MR_count_usage(MR_R_SLOT(32), MR_mr36)

#define MR_REAL_R_REG_MAP_BODY  {                                       \
    2,                                                                  \
    3,                                                                  \
    4,                                                                  \
    6,                                                                  \
    7,                                                                  \
    10,                                                                 \
    11,                                                                 \
    12,                                                                 \
    13,                                                                 \
    14,                                                                 \
    15,                                                                 \
    16,                                                                 \
    17,                                                                 \
    18,                                                                 \
    19,                                                                 \
    20,                                                                 \
    21,                                                                 \
    22,                                                                 \
    23,                                                                 \
    24,                                                                 \
    25,                                                                 \
    26,                                                                 \
    27,                                                                 \
    28,                                                                 \
    29,                                                                 \
    30,                                                                 \
    31,                                                                 \
    32,                                                                 \
    33,                                                                 \
    34,                                                                 \
    35,                                                                 \
    36,                                                                 \
}

#endif // !MR_THREAD_SAFE or MR_NUM_REAL_REGS == 0

// The *_SLOT macros define the mapping from special registers and from
// general purpose registers that may be mapped to machine registers
// to the slot in the MR_fake_reg array that stores that Mercury abstract
// machine register. This mapping is also used to map these Mercury abstract
// machine registers to other save areas, and to map them to their slots in the
// MR_num_uses array.
//
// Any changes to the *_SLOT definitions will also require changes to
// print_register_usage_counts() in mercury_wrapper.c.

// MR_FIRST_UNREAL_SLOT should be the first slot in MR_fake_reg that is
// not occupied by a Mercury abstract machine register that may possibly be
// assigned to a real machine register.

#define MR_FIRST_UNREAL_SLOT                                            \
    (MR_MAX_REAL_R_REG + MR_NUM_SPECIAL_MAYBE_REAL_REG)

#define MR_TRAIL_PTR_SLOT           (MR_FIRST_UNREAL_SLOT + 0)
#define MR_TICKET_COUNTER_SLOT      (MR_FIRST_UNREAL_SLOT + 1)
#define MR_TICKET_HIGH_WATER_SLOT   (MR_FIRST_UNREAL_SLOT + 2)

#define MR_SOL_HP_SLOT              (MR_FIRST_UNREAL_SLOT + 3)
#define MR_MIN_HP_REC_SLOT          (MR_FIRST_UNREAL_SLOT + 4)
#define MR_MIN_SOL_HP_REC_SLOT      (MR_FIRST_UNREAL_SLOT + 5)
#define MR_GLOBAL_HP_SLOT           (MR_FIRST_UNREAL_SLOT + 6)

#define MR_GEN_STACK_SLOT           (MR_FIRST_UNREAL_SLOT + 7)
#define MR_GEN_NEXT_SLOT            (MR_FIRST_UNREAL_SLOT + 8)
#define MR_CUT_STACK_SLOT           (MR_FIRST_UNREAL_SLOT + 9)
#define MR_CUT_NEXT_SLOT            (MR_FIRST_UNREAL_SLOT + 10)
#define MR_PNEG_STACK_SLOT          (MR_FIRST_UNREAL_SLOT + 11)
#define MR_PNEG_NEXT_SLOT           (MR_FIRST_UNREAL_SLOT + 12)

#define MR_PARENT_SP_SLOT           (MR_FIRST_UNREAL_SLOT + 13)

#define MR_FIRST_UNREAL_R_SLOT                                          \
    (MR_FIRST_UNREAL_SLOT + MR_NUM_SPECIAL_GLOBAL_REG)

#define MR_r(n)                     MR_mr((n) + MR_NUM_SPECIAL_REG - 1)

// The definitions of the special registers themselves, and their saved
// versions.
//
// The MR_saved_foo macros are like MR_foo except that they access the
// underlying fake_reg slot rather than the real machine register or global.
//
// For the ones that may be allocated to real machine registers, and for all
// saved versions, assignments should be done to the _word form. Direct
// assignments to e.g. MR_hp will cause warnings from C compilers that don't
// like lvalue casts, e.g. gcc 3.4.

#define MR_succip           ((MR_Code *) MR_succip_word)
#define MR_hp               ((MR_Word *) MR_hp_word)
#define MR_sp               ((MR_Word *) MR_sp_word)
#define MR_curfr            ((MR_Word *) MR_curfr_word)
#define MR_maxfr            ((MR_Word *) MR_maxfr_word)

#define MR_sol_hp           MR_count_usage(MR_SOL_HP_SLOT,              \
                                MR_sol_hp_var)
#define MR_min_hp_rec       MR_count_usage(MR_MIN_HP_REC_SLOT,          \
                                MR_min_hp_rec_var)
#define MR_min_sol_hp_rec   MR_count_usage(MR_MIN_HP_REC_SLOT,          \
                                MR_min_sol_hp_rec_var)
#define MR_global_hp        MR_count_usage(MR_GLOBAL_HP_SLOT,           \
                                MR_global_hp_var)

#if defined(MR_THREAD_SAFE)

#define MR_trail_ptr        MR_count_usage(MR_TRAIL_PTR_SLOT,           \
                                MR_ENGINE(MR_eng_trail_ptr))
#define MR_ticket_counter   MR_count_usage(MR_TICKET_COUNTER_SLOT,      \
                                MR_ENGINE(MR_eng_ticket_counter))
#define MR_ticket_high_water  MR_count_usage(MR_TICKET_HIGH_WATER_SLOT, \
                                MR_ENGINE(MR_eng_ticket_high_water))
#else // ! MR_THREAD_SAFE

#define MR_trail_ptr        MR_count_usage(MR_TRAIL_PTR_SLOT,           \
                                MR_trail_ptr_var)
#define MR_ticket_counter   MR_count_usage(MR_TICKET_COUNTER_SLOT,      \
                                MR_ticket_counter_var)
#define MR_ticket_high_water    MR_count_usage(MR_TICKET_HIGH_WATER_SLOT,\
                                MR_ticket_high_water_var)
#endif // ! MR_THREAD_SAFE

#define MR_gen_next         MR_count_usage(MR_GEN_NEXT_SLOT,            \
                                MR_gen_next_var)
#define MR_gen_stack        MR_count_usage(MR_GEN_STACK_SLOT,           \
                                MR_gen_stack_var)
#define MR_cut_next     MR_count_usage(MR_CUT_NEXT_SLOT,                \
                                MR_cut_next_var)
#define MR_cut_stack        MR_count_usage(MR_CUT_STACK_SLOT,           \
                                MR_cut_stack_var)
#define MR_pneg_next        MR_count_usage(MR_CUT_NEXT_SLOT,            \
                                MR_pneg_next_var)
#define MR_pneg_stack       MR_count_usage(MR_CUT_STACK_SLOT,           \
                                MR_pneg_stack_var)

#define MR_parent_sp        MR_count_usage(MR_PARENT_SP_SLOT,           \
                                MR_ENGINE(MR_eng_parent_sp))

#define MR_saved_succip_word(save_area)     (save_area[MR_SI_SLOT])
#define MR_saved_hp_word(save_area)         (save_area[MR_HP_SLOT])
#define MR_saved_sp_word(save_area)         (save_area[MR_SP_SLOT])
#define MR_saved_curfr_word(save_area)      (save_area[MR_CF_SLOT])
#define MR_saved_maxfr_word(save_area)      (save_area[MR_MF_SLOT])

#define MR_saved_trail_ptr_word(save_area)                              \
                            (save_area[MR_TRAIL_PTR_SLOT])
#define MR_saved_ticket_counter_word(save_area)                         \
                            (save_area[MR_TICKET_COUNTER_SLOT])
#define MR_saved_ticket_high_water_word(save_area)                      \
                            (save_area[MR_TICKET_HIGH_WATER_SLOT])

#define MR_saved_sol_hp_word(save_area)                                 \
                            (save_area[MR_SOL_HP_SLOT])
#define MR_saved_min_hp_rec_word(save_area)                             \
                            (save_area[MR_MIN_HP_REC_SLOT])
#define MR_saved_min_sol_hp_rec_word(save_area)                         \
                            (save_area[MR_MIN_SOL_HP_REC_SLOT])
#define MR_saved_global_hp_word(save_area)                              \
                            (save_area[MR_GLOBAL_HP_SLOT])

#define MR_saved_gen_next_word(save_area)   (save_area[MR_GEN_NEXT_SLOT])
#define MR_saved_gen_stack_word(save_area)  (save_area[MR_GEN_STACK_SLOT])
#define MR_saved_cut_next_word(save_area)   (save_area[MR_CUT_NEXT_SLOT])
#define MR_saved_cut_stack_word(save_area)  (save_area[MR_CUT_STACK_SLOT])
#define MR_saved_pneg_next_word(save_area)  (save_area[MR_PNEG_NEXT_SLOT])
#define MR_saved_pneg_stack_word(save_area) (save_area[MR_PNEG_STACK_SLOT])

#define MR_saved_parent_sp_word(save_area)  (save_area[MR_PARENT_SP_SLOT])

#define MR_saved_succip(save_area)                                      \
    ((MR_Code *) MR_saved_succip_word(save_area))
#define MR_saved_hp(save_area)                                          \
    ((MR_Word *) MR_saved_hp_word(save_area))
#define MR_saved_sp(save_area)                                          \
    ((MR_Word *) MR_saved_sp_word(save_area))
#define MR_saved_curfr(save_area)                                       \
    ((MR_Word *) MR_saved_curfr_word(save_area))
#define MR_saved_maxfr(save_area)                                       \
    ((MR_Word *) MR_saved_maxfr_word(save_area))

#define MR_saved_sol_hp(save_area)                                      \
    ((MR_Word *) MR_saved_sol_hp_word(save_area))
#define MR_saved_min_hp_rec(save_area)                                  \
    ((MR_Word *) MR_saved_min_hp_rec_word(save_area))
#define MR_saved_min_sol_hp_rec(save_area)                              \
    ((MR_Word *) MR_saved_min_sol_hp_rec_word(save_area))
#define MR_saved_global_hp(save_area)                                   \
    ((MR_Word *) MR_saved_global_hp_word(save_area))

#define MR_saved_trail_ptr(save_area)                                   \
    ((MR_TrailEntryPtr) MR_saved_trail_ptr_word(save_area))
#define MR_saved_ticket_counter(save_area)                              \
    ((MR_Unsigned) MR_saved_ticket_counter_word(save_area))
#define MR_saved_ticket_high_water(save_area)                           \
    ((MR_Unsigned) MR_saved_ticket_high_water_word(save_area))

#define MR_saved_gen_next(save_area)                                    \
    ((MR_Integer) MR_saved_gen_next_word(save_area))
#define MR_saved_gen_stack(save_area)                                   \
    ((MR_GenStackFrame *) MR_saved_gen_stack_word(save_area))
#define MR_saved_cut_next(save_area)                                    \
    ((MR_Integer) MR_saved_cut_next_word(save_area))
#define MR_saved_cut_stack(save_area)                                   \
    ((MR_CutStackFrame *) MR_saved_cut_stack_word(save_area))
#define MR_saved_pneg_next(save_area)                                   \
    ((MR_Integer) MR_saved_pneg_next_word(save_area))
#define MR_saved_pneg_stack(save_area)                                  \
    ((MR_PNegStackFrame *) MR_saved_pneg_stack_word(save_area))

#define MR_saved_parent_sp(save_area)                                   \
    ((MR_Word *) MR_saved_parent_sp_word(save_area))

// MR_virtual_reg_value(n) accesses the underlying fake_reg for general
// register n, while MR_virtual_reg_assign assigns to it.
//
// Similarly, MR_virtual_foo access the underlying fake_reg slot for special
// register foo.

#define MR_saved_reg_value(save_area, n)                                \
    ((n) > MR_MAX_REAL_R_REG ?                                          \
        (save_area)[(n) + MR_NUM_SPECIAL_REG - 1] :                     \
        (save_area)[MR_real_r_reg_map[(n) - 1]])
#define MR_saved_reg_assign(save_area, n, val)                          \
    do {                                                                \
        if ((n) > MR_MAX_REAL_R_REG) {                                  \
            (save_area)[(n) + MR_NUM_SPECIAL_REG - 1] = (val);          \
        } else {                                                        \
            (save_area)[MR_real_r_reg_map[(n) - 1]] = (val);            \
        }                                                               \
    } while (0)

#define MR_virtual_reg_value(n)         MR_saved_reg_value(MR_fake_reg, n)
#define MR_virtual_reg_assign(n, val)   MR_saved_reg_assign(MR_fake_reg, n, val)

#define MR_virtual_succip               MR_saved_succip(MR_fake_reg)
#define MR_virtual_hp                   MR_saved_hp(MR_fake_reg)
#define MR_virtual_hp_word              MR_saved_hp_word(MR_fake_reg)
#define MR_virtual_sp                   MR_saved_sp(MR_fake_reg)
#define MR_virtual_curfr                MR_saved_curfr(MR_fake_reg)
#define MR_virtual_maxfr                MR_saved_maxfr(MR_fake_reg)

#define MR_virtual_sol_hp               MR_saved_sol_hp(MR_fake_reg)
#define MR_virtual_min_hp_rec           MR_saved_min_hp_rec(MR_fake_reg)
#define MR_virtual_min_sol_hp_rec       MR_saved_min_sol_hp_rec(MR_fake_reg)
#define MR_virtual_global_hp            MR_saved_global_hp(MR_fake_reg)

#define MR_virtual_trail_ptr            MR_saved_trail_ptr(MR_fake_reg)
#define MR_virtual_ticket_counter       MR_saved_ticket_counter(MR_fake_reg)
#define MR_virtual_ticket_high_water    MR_saved_ticket_high_water(MR_fake_reg)

#define MR_virtual_gen_next             MR_saved_gen_next(MR_fake_reg)
#define MR_virtual_gen_stack            MR_saved_gen_stack(MR_fake_reg)
#define MR_virtual_cut_next             MR_saved_cut_next(MR_fake_reg)
#define MR_virtual_cut_stack            MR_saved_cut_stack(MR_fake_reg)
#define MR_virtual_pneg_next            MR_saved_pneg_next(MR_fake_reg)
#define MR_virtual_pneg_stack           MR_saved_pneg_stack(MR_fake_reg)

#define MR_virtual_parent_sp            MR_saved_parent_sp(MR_fake_reg)

#ifdef  MR_USE_TRAIL
  #define MR_save_trail_registers()                                     \
    do {                                                                \
        MR_saved_trail_ptr_word(MR_fake_reg) = (MR_Word)                \
            MR_trail_ptr;                                               \
        MR_saved_ticket_counter_word(MR_fake_reg) = (MR_Word)           \
            MR_ticket_counter;                                          \
        MR_saved_ticket_high_water_word(MR_fake_reg) = (MR_Word)        \
            MR_ticket_high_water;                                       \
    } while (0)
  #define MR_restore_trail_registers()                                  \
    do {                                                                \
        MR_trail_ptr = MR_virtual_trail_ptr;                            \
        MR_ticket_counter = MR_virtual_ticket_counter;                  \
        MR_ticket_high_water = MR_virtual_ticket_high_water;            \
    } while (0)
#else
  #define MR_save_trail_registers() ((void) 0)
  #define MR_restore_trail_registers()  ((void) 0)
#endif

#ifdef  MR_USE_MINIMAL_MODEL
  #define MR_save_mm_registers()                                        \
    do {                                                                \
        MR_saved_gen_next_word(MR_fake_reg) = (MR_Word)                 \
            MR_gen_next;                                                \
        MR_saved_gen_stack_word(MR_fake_reg) = (MR_Word)                \
            MR_gen_stack;                                               \
        MR_saved_cut_next_word(MR_fake_reg) = (MR_Word)                 \
            MR_cut_next;                                                \
        MR_saved_cut_stack_word(MR_fake_reg) = (MR_Word)                \
            MR_cut_stack;                                               \
        MR_saved_pneg_next_word(MR_fake_reg) = (MR_Word)                \
            MR_pneg_next;                                               \
        MR_saved_pneg_stack_word(MR_fake_reg) = (MR_Word)               \
            MR_pneg_stack;                                              \
    } while (0)
  #define MR_restore_mm_registers()                                     \
    do {                                                                \
        MR_gen_next_var = MR_virtual_gen_next;                          \
        MR_gen_stack_var = MR_virtual_gen_stack;                        \
        MR_cut_next_var = MR_virtual_cut_next;                          \
        MR_cut_stack_var = MR_virtual_cut_stack;                        \
        MR_pneg_next_var = MR_virtual_pneg_next;                        \
        MR_pneg_stack_var = MR_virtual_pneg_stack;                      \
    } while (0)
#else
  #define MR_save_mm_registers()        ((void) 0)
  #define MR_restore_mm_registers()     ((void) 0)
#endif

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
  #define MR_save_par_registers()                                       \
    do {                                                                \
        MR_saved_parent_sp_word(MR_fake_reg) = (MR_Word)                \
            MR_parent_sp;                                               \
    } while (0)
  #define MR_restore_par_registers()                                    \
    do {                                                                \
        MR_parent_sp = MR_virtual_parent_sp;                            \
    } while (0)
#else
  #define MR_save_par_registers()       ((void) 0)
  #define MR_restore_par_registers()    ((void) 0)
#endif

#ifdef MR_BOXED_FLOAT
  #define MR_float_reg          (MR_ENGINE(MR_eng_float_reg))
  #define MR_f(n)               (MR_float_reg[n])

  #define MR_saved_f_reg_value(save_area, n)                            \
    (save_area)[(n)]
  #define MR_saved_f_reg_assign(save_area, n, val)                      \
    do {                                                                \
        (save_area)[(n)] = (val);                                       \
    } while (0)
#endif

// The MR_save_registers() macro copies the physical machine registers
// and the global variables holding special purpose abstract machine registers
// to their corresponding slots in the MR_fake_reg array.
//
// MR_restore_registers() does the same transfer in the opposite direction.
//
// For speed, neither copies the special purpose registers that are known
// not to be needed in the current grade.

#define MR_save_registers()                                             \
    do {                                                                \
        MR_save_mhal_registers();                                       \
        MR_saved_sol_hp_word(MR_fake_reg) = (MR_Word)                   \
            MR_sol_hp;                                                  \
        MR_saved_min_hp_rec_word(MR_fake_reg) = (MR_Word)               \
            MR_min_hp_rec;                                              \
        MR_saved_min_sol_hp_rec_word(MR_fake_reg) = (MR_Word)           \
            MR_min_sol_hp_rec;                                          \
        MR_saved_global_hp_word(MR_fake_reg) = (MR_Word)                \
            MR_global_hp;                                               \
        MR_save_trail_registers();                                      \
        MR_save_mm_registers();                                         \
        MR_save_par_registers();                                        \
    } while (0)

#define MR_restore_registers()                                          \
    do {                                                                \
        MR_restore_mhal_registers();                                    \
        MR_sol_hp = MR_virtual_sol_hp;                                  \
        MR_min_hp_rec = MR_virtual_min_hp_rec;                          \
        MR_min_sol_hp_rec = MR_virtual_min_sol_hp_rec;                  \
        MR_global_hp = MR_virtual_global_hp;                            \
        MR_restore_trail_registers();                                   \
        MR_restore_mm_registers();                                      \
        MR_restore_par_registers();                                     \
    } while (0)

// MR_clear_regs_for_GC() clears all of the Mercury general-purpose registers.
// It is used to avoid unwanted memory retention due to false hits
// in the conservative garbage collector.

#define MR_clear_regs_for_GC()                                          \
    do {                                                                \
        int i;                                                          \
        MR_save_registers();                                            \
        for (i = 1; i <= MR_MAX_VIRTUAL_R_REG; i++) {                   \
            MR_virtual_reg_assign(i, 0);                                \
        }                                                               \
        MR_restore_registers();                                         \
    } while (0)

extern  MR_Word *MR_sol_hp_var;
extern  MR_Word *MR_min_hp_rec_var;
extern  MR_Word *MR_min_sol_hp_rec_var;
extern  MR_Word *MR_global_hp_var;

// Used to lookup the MR_fake_reg slot for a real general purpose register.

extern  MR_Word         MR_real_r_reg_map[MR_MAX_REAL_R_REG];

// Used for counting register usage.

#ifdef  MR_MEASURE_REGISTER_USAGE
extern  unsigned long   MR_num_uses[MR_MAX_REAL_R_REG + MR_NUM_SPECIAL_REG];

static  void            MR_print_register_usage_counts(void);
#endif

#ifdef  MR_VERIFY_FAKE_REGISTERS
extern  void            MR_verify_fake_registers(void);
#endif

// MR_get_reg() and MR_set_reg() provide a different way of addressing
// the general purpose registers; unlike MR_virtual_reg_value(), you don't
// need to wrap them inside MR_save_registers()/MR_restore_regs() to copy
// the real regs to/from the MR_fake_reg, so they may perhaps be more
// efficient if you are just getting or setting one or two registers?
// They are designed to work only for registers at or below MR_MAX_REAL_R_REG
// and are not used except for debugging.

extern  MR_Word         MR_get_reg(int);
extern  MR_Word         MR_set_reg(int, MR_Word);

#endif // not MERCURY_REGS_H
