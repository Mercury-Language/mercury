
/* DEFINITIONS FOR MANIPULATING THE HEAP */

#ifdef CONSERVATIVE_GC

#include "gc.h"

#define	tag_incr_hp(dest,tag,count) \
	((dest) = mkword(tag, (Word)GC_MALLOC(count * sizeof(Word))))
#define	tag_incr_hp_atomic(dest,tag,count) \
	((dest) = mkword(tag, (Word)GC_MALLOC_ATOMIC(count * sizeof(Word))))
#define	mark_hp(dest)	((void)0)
#define	restore_hp(src)	((void)0)

			/* we use `hp' as a convenient temporary here */
#define hp_alloc(count) (incr_hp(hp,(count)), hp += (count), (void)0)
#define hp_alloc_atomic(count) \
			(incr_hp_atomic(hp,(count)), hp += (count), (void)0)

#else

#define	tag_incr_hp(dest,tag,count)	(			\
				(dest) = mkword(tag, (Word)hp),	\
				debugincrhp(count, hp),		\
				hp += (count),			\
				heap_overflow_check(),		\
				(void)0				\
			)
#define tag_incr_hp_atomic(dest,tag,count) tag_incr_hp((dest),(tag),(count))

#define	mark_hp(dest)	(					\
				(dest) = (Word)hp,		\
				(void)0				\
			)

#define	restore_hp(src)	(					\
				LVALUE_CAST(Word,hp) = (src),	\
				(void)0				\
			)

#define hp_alloc(count)  incr_hp(hp,count)
#define hp_alloc_atomic(count) incr_hp_atomic(count)

#endif

#define	incr_hp(dest,count)	tag_incr_hp((dest),mktag(0),(count))
#define	incr_hp_atomic(dest,count) \
				tag_incr_hp_atomic((dest),mktag(0),(count))

/*
** Note that gcc optimizes `hp += 2; return hp - 2;'
** to `tmp = hp; hp += 2; return tmp;', so we don't need to use
** gcc's expression statements here
*/

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create1(w1)	(					\
				hp_alloc(1),			\
				hp[-1] = (Word) (w1),		\
				debugcr1(hp[-1], hp),		\
				/* return */ (Word) (hp - 1)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2(w1, w2)	(					\
				hp_alloc(2),			\
				hp[-2] = (Word) (w1),		\
				hp[-1] = (Word) (w2),		\
				debugcr2(hp[-2], hp[-1], hp),	\
				/* return */ (Word) (hp - 2)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create3(w1, w2, w3)	(				\
				hp_alloc(3),			\
				hp[-3] = (Word) (w1),		\
				hp[-2] = (Word) (w2),		\
				hp[-1] = (Word) (w3),		\
				/* return */ (Word) (hp - 3)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_bf(w1)	(					\
				hp = hp + 2,			\
				hp[-2] = (Word) (w1),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_fb(w2)	(					\
				hp = hp + 2,			\
				hp[-1] = (Word) (w2),		\
				heap_overflow_check(),		\
				/* return */ (Word) (hp - 2)	\
			)
