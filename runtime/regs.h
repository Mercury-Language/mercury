/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef REGS_H
#define REGS_H

/*
** GNU C allows lvalue casts, so if we have gcc, use them.
** If we don't have gcc, then we can use *(type *)&lval,
** but that wouldn't work for gcc since lval might be a global
** register in which case we couldn't take it's address.
** Similarly for comma expressions.
*/

#ifdef __GNUC__
  #define LVALUE_CAST(type, lval)	((type)(lval))
  #define LVALUE_SEQ(expr, lval)	((expr),(lval))
#else
  #define LVALUE_CAST(type, lval)	(*(type*)&(lval))
  #define LVALUE_SEQ(expr, lval)	(*((expr),&(lval)))
#endif

/*
** The registers of the Mercury virtual machine are built up using
** three levels of abstraction.
**
** The first level defines the first NUM_REAL_REGS register variables
** mr0, mr1, etc. as the physical machine registers, and defines an
** array fake_regs[n] of pseudo registers.
**
** The next level defines macroes mr0 through mr36 and also mr(n) for
** n>36.  The lower the number,
** the greater the probability that the storage referred to will be
** a real machine register, and not a simulated one. The number of
** real machine registers is given by the macro NUM_REAL_REGS.
**
** The second level maps the Mercury virtual machine registers
**
**	succip, hp, sp, curfr, maxfr and
**	r1, ..., r32, r(33), ..., r(MAX_VIRTUAL_REG)
**
** to the set mr0..mr36, mr(37), mr(38), ..., mr(MAX_FAKE_REG-1)
**
** Since the set of most frequently used Mercury virtual machine
** registers can be different for each program, we want to make
** this mapping as easy to change as possible. This is why the
** map is in a minimal header file, regorder.h.
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
  #else
    #error "USE_GCC_GLOBAL_REGISTERS not yet supported on this machine."
  #endif
#else
    #include "machdeps/no_regs.h"
#endif

/* The machdeps header defines mr0 .. mr36; now define mr(n) for n > 36 */

#define mr(n) LVALUE_SEQ(assert((n) >= MAX_REAL_REG + NUM_SPECIAL_REG && \
				(n) < MAX_FAKE_REG),\
		fake_reg[n])

#ifdef MEASURE_REGISTER_USAGE
  #define count_usage(num,reg)		LVALUE_SEQ(num_uses[num]++, reg)
#else
  #define count_usage(num,reg)		(reg)
#endif

#include	"regorder.h"

/* regorrder.h defines r1 .. r32; now define r(n) for n > 32 */

#define r(n) mr((n) + NUM_SPECIAL_REG - 1)

/* virtual_reg(n) accesses the underlying fake_reg for register n */

#define virtual_reg(n)	\
	((n) > MAX_REAL_REG ? r(n) : fake_reg[virtual_reg_map[(n) - 1]])

/*
** the following macros define a mapping from registers to indices into the
** num_uses array used for counting register usage
**
** any changes to these will also require changes to
** print_register_usage_counts() in wrapper.mod.
*/

#define	SI_RN	0
#define R_RN(n)	(n)
#define ORD_RN	MAX_REAL_REG
#define	HP_RN	(ORD_RN + 1)
#define	SP_RN	(ORD_RN + 2)
#define	CF_RN	(ORD_RN + 3)
#define	MF_RN	(ORD_RN + 4)
#define MAX_RN	(ORD_RN + 5)

#endif /* REGS_H */
