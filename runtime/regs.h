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

#ifdef MEASURE_REGISTER_USAGE
  extern long num_uses[32];
  #define count_usage(num,reg)		LVALUE_SEQ(num_uses[num]++, reg)
#else
  #define count_usage(num,reg)		(reg)
#endif


/*
** These are defined in order of priority.
** We should experiment to determine how often each is used in real code.
*/

#define SUCCIP_REG_NUM	0
#define HP_REG_NUM	6
#define SP_REG_NUM	7
#define CURFR_REG_NUM	9

#ifndef OPTIMIZE_FOR_NQUEENS
  #define MAXFR_REG_NUM	9
#else
  #define MAXFR_REG_NUM	5
#endif

#define succip		LVALUE_CAST(Code *, count_usage(0, mr0))
#define r1		count_usage(1, mr1)
#define r2		count_usage(2, mr2)
#define r3		count_usage(3, mr3)
#define r4		count_usage(4, mr4)
#ifndef OPTIMIZE_FOR_NQUEENS
  #define r5		count_usage(5, mr5)
  #define maxfr		LVALUE_CAST(Word *, count_usage(9, mr9))
#else
  #define r5		count_usage(5, mr9)
  #define maxfr		LVALUE_CAST(Word *, count_usage(9, mr5))
#endif
#define hp		LVALUE_CAST(Word *, count_usage(6, mr6))
#define sp		LVALUE_CAST(Word *, count_usage(7, mr7))
#define curfr		LVALUE_CAST(Word *, count_usage(8, mr8))
#define r6		count_usage(10, mr10)
#define r7		count_usage(11, mr11)
#define r8		count_usage(12, mr12)
#define r9		count_usage(13, mr13)
#define r10		count_usage(14, mr14)
#define r11		count_usage(15, mr15)
#define r12		count_usage(16, mr16)
#define r13		count_usage(17, mr17)
#define r14		count_usage(18, mr18)
#define r15		count_usage(19, mr19)
#define r16		count_usage(20, mr20)
#define r17		count_usage(21, mr21)
#define r18		count_usage(22, mr22)
#define r19		count_usage(23, mr23)
#define r20		count_usage(24, mr24)
#define r21		count_usage(25, mr25)
#define r22		count_usage(26, mr26)
#define r23		count_usage(27, mr27)
#define r24		count_usage(28, mr28)
#define r25		count_usage(29, mr29)
#define r26		count_usage(30, mr30)
#define r27		count_usage(31, mr31)

extern Word r28;
extern Word r29;
extern Word r30;
extern Word r31;
extern Word r32;

#endif /* REGS_H */
