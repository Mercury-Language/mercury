/*
** Copyright (C) 1993-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_REGS_H
#define MERCURY_REGS_H

#include "mercury_conf.h"
#include "mercury_types.h"

/*---------------------------------------------------------------------------*/
/*
** GNU C allows lvalue casts, so if we have gcc, use them.
** If we don't have gcc, then we can use *(type *)&lval,
** but that wouldn't work for gcc since lval might be a global
** register in which case we couldn't take it's address.
** Similarly for comma expressions and conditional expressions.
*/

#ifdef __GNUC__
  #define LVALUE_CAST(type, lval)	((type)(lval))
  #define LVALUE_SEQ(expr, lval)	((expr),(lval))
  #define LVALUE_COND(expr, x, y)	((expr)?(x):(y))
#else
  #define LVALUE_CAST(type, lval)	(*(type*)&(lval))
  #define LVALUE_SEQ(expr, lval)	(*((expr),&(lval)))
  #define LVALUE_COND(expr, x, y)	(*((expr)?&(x):&(y)))
#endif

#define MR_fake_reg	(MR_ENGINE(fake_reg))

/*---------------------------------------------------------------------------*/
/*
** The registers of the Mercury virtual machine are built up using
** three levels of abstraction.
**
** The bottom level is the hardware description layer. 
** This layer is defined separately for each architecture,
** in the header files in the machdeps subdirectory.
** The hardware description layer defines the first NUM_REAL_REGS register
** variables mr0, mr1, etc. as the physical machine registers, and defines an
** array fake_regs[n] of pseudo registers, with the remaining "registers"
** mr<NUM_REAL_REGS>, ..., mr36 defined as corresponding slots in
** this fake_reg array. 
** This level also provides the macros save_regs_to_mem(),
** save_transient_regs_to_mem(), restore_regs_from_mem(),
** and restore_transient_regs_from_mem().
**
** The next level is the hardware abstraction layer.
** The hardware abstraction layer is at a similar level to the
** hardware description layer, and includes that as a subset,
** but in addition it provides a few more conveniences.
** This layer defines macros mr(n) for n>36, and the macros
** save_registers(), restore_registers(), save_transient_registers(),
** and restore_transient_registers().
** This layer is defined here in mercury_regs.h.
**
** The hardware abstraction layer thus provides a very large number
** of registers, which may be either real or fake.  The lower the number,
** the greater the probability that the storage referred to will be
** a real machine register, and not a simulated one. The number of
** real machine registers is given by the macro NUM_REAL_REGS.
**
** The final level is the Mercury abstract machine registers layer.
** This layer maps the Mercury virtual machine registers
**
**	MR_succip, MR_hp, MR_sp, MR_curfr, MR_maxfr and
**	r1, ..., r32, r(33), ..., r(MAX_VIRTUAL_REG)
**
** to the set mr0..mr36, mr(37), mr(38), ..., mr(MAX_FAKE_REG-1)
** which were provided by the hardware abstraction layer.
** It also provides MR_virtual_r(), MR_virtual_succip, MR_virtual_hp, etc.,
** which are similar to mr<N>, MR_succip, MR_hp, etc. except that they
** always map to the underlying fake_reg rather than to the physical register.
**
** Since the set of most frequently used Mercury virtual machine
** registers can be different for each program or grade, we want to make
** this mapping as easy to change as possible. This is why the
** map is in a minimal header file, mercury_regorder.h.
*/

/*---------------------------------------------------------------------------*/
/*
** The hardware description layer
*/
#if defined(USE_GCC_GLOBAL_REGISTERS)
  #ifndef __GNUC__
    #error "You must use gcc if you define USE_GCC_GLOBAL_REGISTERS."
  #endif

  #if defined(__mips__)
    #include "machdeps/mips_regs.h"
  #elif defined(__i386__)
    #include "machdeps/i386_regs.h"
  #elif defined(__sparc__)
    #include "machdeps/sparc_regs.h"
  #elif defined(__alpha__)
    #include "machdeps/alpha_regs.h"
  #elif defined(__hppa__)
    #include "machdeps/pa_regs.h"
  #elif defined(_POWER) || defined(__powerpc__)
    #include "machdeps/rs6000_regs.h"
  #else
    #error "USE_GCC_GLOBAL_REGISTERS not yet supported on this machine."
  #endif
#else
    #include "machdeps/no_regs.h"
#endif

/*---------------------------------------------------------------------------*/
/*
** Extra stuff for the hardware abstraction layer
*/

/* The machdeps header defines mr0 .. mr36; now define mr(n) for n > 36 */

#define mr(n) LVALUE_SEQ(MR_assert((n) >= MAX_REAL_REG + NUM_SPECIAL_REG && \
				(n) < MAX_FAKE_REG),\
		MR_fake_reg[n])

/* 
** the save_registers() macro copies the physical machine registers
** to their corresponding slots in the MR_fake_reg array 
*/

#define save_registers() 	save_regs_to_mem(MR_fake_reg)

/* 
** the restore_registers() macro sets the physical machine registers
** to the values in their corresponding slots in the fake_reg array 
** If we're using a register for the engine base, then we'd better
** restore that from the thread specific data area, since the fake_reg
** array is accessed via that register.
*/

#if defined(MR_THREAD_SAFE) && NUM_REAL_REGS > 0
#define	restore_registers()	do {				\
		MR_engine_base = MR_thread_engine_base;		\
		MR_fake_reg[0] = (Word) MR_engine_base;		\
		restore_regs_from_mem(MR_fake_reg);		\
	} while (0)
#else
#define restore_registers() 	restore_regs_from_mem(MR_fake_reg)
#endif

/* 
** the save_transient_registers() and restore_transient_registers()
** macros are similar to save_registers() and restore_registers()
** except that they only save/restore registers which can be
** affected by calling or returning from a C function (e.g.
** by sliding register windows on SPARCs).
*/

#define save_transient_registers()    save_transient_regs_to_mem(MR_fake_reg)
#if defined(MR_THREAD_SAFE) && NUM_REAL_REGS > 0
#define restore_transient_registers()	do {				\
		MR_engine_base = MR_thread_engine_base;			\
		MR_fake_reg[0] = (Word) MR_engine_base;			\
		restore_transient_regs_from_mem(MR_fake_reg);		\
	} while (0)
#else
#define restore_transient_registers()	\
		restore_transient_regs_from_mem(MR_fake_reg)
#endif

/*---------------------------------------------------------------------------*/
/*
** The Mercury abstract machine registers layer
*/

#ifdef MEASURE_REGISTER_USAGE
  #define count_usage(num,reg)		LVALUE_SEQ(num_uses[num]++, reg)
#else
  #define count_usage(num,reg)		(reg)
#endif

#include	"mercury_regorder.h"

/* mercury_regorder.h defines r1 .. r32; now define r(n) for n > 32 */

#define r(n) mr((n) + NUM_SPECIAL_REG - 1)

/*
** saved_reg(save_area, n) accesses the underlying slot in save_area
** for register n
*/
#define saved_reg(save_area, n)	\
	LVALUE_COND((n) > MAX_REAL_REG, \
		save_area[(n) + NUM_SPECIAL_REG - 1], \
		save_area[virtual_reg_map[(n) - 1]])

/* virtual_reg(n) accesses the underlying fake_reg for register n */
/* similarly MR_virtual_foo access the underlying fake_reg slot for foo */

#define virtual_reg(n) 			saved_reg(MR_fake_reg, n)
#define MR_virtual_succip 		MR_saved_succip(MR_fake_reg)
#define MR_virtual_hp 			MR_saved_hp(MR_fake_reg)
#define MR_virtual_sp 			MR_saved_sp(MR_fake_reg)
#define MR_virtual_curfr 		MR_saved_curfr(MR_fake_reg)
#define MR_virtual_maxfr 		MR_saved_maxfr(MR_fake_reg)
#define MR_virtual_sol_hp 		MR_saved_sol_hp(MR_fake_reg)
#define MR_virtual_min_hp_rec 		MR_saved_min_hp_rec(MR_fake_reg)
#define MR_virtual_min_sol_hp_rec 	MR_saved_min_sol_hp_rec(MR_fake_reg)

/*
** get_reg() and set_reg() provide a different way of addressing
** the registers; unlike virtual_reg(), you don't need to wrap them
** inside save_registers()/restore_regs() to copy the real regs to/from
** the MR_fake_reg, so they may perhaps be more efficient if you are just
** getting or setting one or two registers?
** Currently they're buggy for n>32 and are not used except for debugging.
*/

extern	Word	get_reg(int);
extern	Word	set_reg(int, Word);

/*
** the following macros define a mapping from registers to indices into the
** num_uses array used for counting register usage
**
** any changes to these will also require changes to
** print_register_usage_counts() in wrapper.mod.
*/

#define	MR_SI_RN		0
#define MR_R_RN(n)		(n)
#define MR_ORD_RN		MAX_REAL_REG
#define	MR_HP_RN		(MR_ORD_RN + 1)
#define	MR_SP_RN		(MR_ORD_RN + 2)
#define	MR_CF_RN		(MR_ORD_RN + 3)
#define	MR_MF_RN		(MR_ORD_RN + 4)
#define MR_TRAIL_PTR_RN		(MR_ORD_RN + 5)
#define MR_TICKET_COUNTER_RN	(MR_ORD_RN + 6)
#define	MR_SOL_HP_RN		(MR_ORD_RN + 7)
#define	MR_MIN_HP_REC		(MR_ORD_RN + 8)
#define	MR_MIN_SOL_HP_REC	(MR_ORD_RN + 9)
#define MAX_RN			(MR_ORD_RN + 10)

#endif /* not MERCURY_REGS_H */
