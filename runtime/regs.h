#ifndef REGS_H
#define REGS_H

/*
** The registers of the Mercury virtual machine are built up using
** two levels of abstraction.
**
** The first level defines mr0 through mr36. The lower the number,
** the greater the probability that the storage referred to will be
** a real machine register, and not a simulated one. The number of
** real machine registers is given by the macro NUM_REAL_REGS.
**
** The second level maps the Mercury virtual machine registers
**
**	succip, hp, sp, curfr, maxfr and r1 - r32
**
** to the set mr0..mr36.
**
** Since the set of most frequently used Mercury virtual machine
** registers can be different for each program, we want to make
** this mapping as easy to change as possible. This is why the
** map is in a minimal header file, regorder.h.
*/

extern	Word	unreal_reg_0;
extern	Word	unreal_reg_1;
extern	Word	unreal_reg_2;
extern	Word	unreal_reg_3;
extern	Word	unreal_reg_4;
extern	Word	unreal_reg_5;
extern	Word	unreal_reg_6;
extern	Word	unreal_reg_7;
extern	Word	unreal_reg_8;
extern	Word	unreal_reg_9;
extern	Word	unreal_reg_10;
extern	Word	unreal_reg_11;
extern	Word	unreal_reg_12;
extern	Word	unreal_reg_13;
extern	Word	unreal_reg_14;
extern	Word	unreal_reg_15;
extern	Word	unreal_reg_16;
extern	Word	unreal_reg_17;
extern	Word	unreal_reg_18;
extern	Word	unreal_reg_19;
extern	Word	unreal_reg_20;
extern	Word	unreal_reg_21;
extern	Word	unreal_reg_22;
extern	Word	unreal_reg_23;
extern	Word	unreal_reg_24;
extern	Word	unreal_reg_25;
extern	Word	unreal_reg_26;
extern	Word	unreal_reg_27;
extern	Word	unreal_reg_28;
extern	Word	unreal_reg_29;
extern	Word	unreal_reg_30;
extern	Word	unreal_reg_31;
extern	Word	unreal_reg_32;
extern	Word	unreal_reg_33;
extern	Word	unreal_reg_34;
extern	Word	unreal_reg_35;
extern	Word	unreal_reg_36;

/*
** if any of the following change, you must inspect all the files
** that may be included from here, as well as all uses of all macros
** defined here.
*/

#define	SI_RN	 0
#define	ORD_RN	32
#define	HP_RN	33
#define	SP_RN	34
#define	CF_RN	35
#define	MF_RN	36
#define	MAX_RN	37

/* both these are arrays of size MAX_RN */
extern	Word	*saved_regs;
extern	Word	*num_uses;

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

#ifndef save_registers
    #error "Software error: machdeps does not define save_registers."
#endif

#ifndef restore_registers
    #error "Software error: machdeps does not define restore_registers."
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

#endif /* REGS_H */
