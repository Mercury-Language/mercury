#ifndef REGS_H
#define REGS_H

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
**	succip, hp, sp, curfr, maxfr and r1 - r64
**
** to the set mr0..mr36, mr(37)..mr(69)
**
** Since the set of most frequently used Mercury virtual machine
** registers can be different for each program, we want to make
** this mapping as easy to change as possible. This is why the
** map is in a minimal header file, regorder.h.
*/

/*
** if any of the following change, you must inspect memory.[ch],
** the headers in machdeps, as well as all uses of all macros defined here.
*/

#define	SI_RN	 0
#define	ORD_RN	32
#define	HP_RN	(ORD_RN + 1)
#define	SP_RN	(ORD_RN + 2)
#define	CF_RN	(ORD_RN + 3)
#define	MF_RN	(ORD_RN + 4)
#define MAX_RN	(ORD_RN + 5)

#if defined(USE_GCC_GLOBAL_REGISTERS)
  #ifndef __GNUC__
    #error "You must use gcc if you define USE_GCC_GLOBAL_REGISTERS."
  #endif

  #if defined(__mips__)
    #include "machdeps/mips_regs.h"
  #elif defined(__sparc__)
    #include "machdeps/sparc_regs.h"
  #else
    #error "USE_GCC_GLOBAL_REGISTERS not yet supported on this machine."
  #endif
#else
    #include "machdeps/no_regs.h"
#endif

#ifndef save_transient_registers
    #error "Software error: machdeps does not define save_transient_registers."
#endif

#ifndef restore_transient_registers
    #error "Software error: machdeps does not define restore_transient_registers."
#endif

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

#ifdef MEASURE_REGISTER_USAGE
  #define count_usage(num,reg)		LVALUE_SEQ(num_uses[num]++, reg)
#else
  #define count_usage(num,reg)		(reg)
#endif

#include	"regorder.h"

/* define the macro for doing reg-number translations */

#define virtual_reg(num)	(fake_reg[virtual_reg_map[(num)]])

#endif /* REGS_H */
