/*
** The virtual machine registers are built up using two levels
** of abstraction.  This file defines the virtual machine registers
** hp, sp, curcp, maxcp, r1 - r32
** using the registers mr0 - mr31 defined in machregs.h.
*/

#include "machregs.h"

/* GNU C allows lvalue casts, so if we have gcc, use them.
** If we don't have gcc, then we can use *(type *)&val,
** but that wouldn't work for gcc since val might be a global
** register in which case we couldn't take it's address.
*/
#ifdef __GNUC__
#define LVALUE_CAST(type, lval) ((type)(lval))
#else
#define LVALUE_CAST(type, lval) (*(type*)&(lval))
#endif

/*
** These are defined in order of priority.
** We should experiment to determine how often each is used in real code.
*/

#define succip		LVALUE_CAST(Code *, mr0)
#define r1		mr1
#define r2		mr2
#define r3		mr3
#define r4		mr4
#define r5		mr5
#define hp		LVALUE_CAST(Word *, mr6)
#define sp		LVALUE_CAST(Word *, mr7)
#define childcp		LVALUE_CAST(Word *, mr8)
#define curcp		LVALUE_CAST(Word *, mr9)
#define maxcp		LVALUE_CAST(Word *, mr10)
#define r6		mr11
#define r7		mr12
#define r8		mr13
#define r9		mr14
#define r10		mr15
#define r11		mr16
#define r12		mr17
#define r13		mr18
#define r14		mr19
#define r15		mr20
#define r16		mr21
#define r17		mr22
#define r18		mr23
#define r19		mr24
#define r20		mr25
#define r21		mr26
#define r22		mr27
#define r23		mr28
#define r24		mr29
#define r25		mr30
#define r26		mr31

extern Word r27;
extern Word r28;
extern Word r29;
extern Word r30;
extern Word r31;
extern Word r32;
