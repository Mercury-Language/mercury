#ifndef REGS_H
#define REGS_H

/*
** The virtual machine registers are built up using two levels
** of abstraction.  This file defines the virtual machine registers
** succip, hp, sp, curfr, maxfr, r1 - r32
** using the registers mr0 - mr31 defined in machregs.h.
*/

#include "machregs.h"

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

#define	ORD_RN	32
#define	SI_RN	33
#define	HP_RN	34
#define	SP_RN	35
#define	CF_RN	36
#define	MF_RN	37
#define	MAX_RN	38

#ifdef MEASURE_REGISTER_USAGE
  extern long num_uses[MAX_RN];
  #define count_usage(num,reg)		LVALUE_SEQ(num_uses[num]++, reg)
#else
  #define count_usage(num,reg)		(reg)
#endif

#include	"regorder.h"

#endif /* REGS_H */
