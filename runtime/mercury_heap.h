/*
** Copyright (C) 1995-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_heap.h - definitions for manipulating the Mercury heap */

#ifndef MERCURY_HEAP_H
#define MERCURY_HEAP_H

#include "mercury_types.h"		/* for `Word' */
#include "mercury_context.h"		/* for min_heap_reclamation_point() */
#include "mercury_heap_profile.h"	/* for MR_record_allocation() */

#ifdef CONSERVATIVE_GC

  #include "gc.h"

  #define tag_incr_hp_n(dest, tag, count) \
	((dest) = (Word) mkword((tag), \
			(Word) GC_MALLOC((count) * sizeof(Word))))
  #define tag_incr_hp_atomic(dest, tag, count) \
	((dest) = (Word) mkword((tag), \
			(Word) GC_MALLOC_ATOMIC((count) * sizeof(Word))))

  #ifdef INLINE_ALLOC

    /*
    ** The following stuff uses the macros in the `gc_inl.h' header file in the
    ** Boehm garbage collector.  They improve performance a little for
    ** highly allocation-intensive programs (e.g. the `nrev' benchmark).
    ** You'll probably need to fool around with the `-I' options to get this
    ** to work.  Also, you must make sure that you compile with the same
    ** setting for -DSILENT that the boehm_gc directory was compiled with.
    **
    ** We only want to inline allocations if the allocation size is a
    ** compile-time constant.  This should be true for almost all the code that
    ** we generate, but with GCC we can use the `__builtin_constant_p()'
    ** extension to find out.
    **
    ** The inline allocation macros are used only for allocating amounts
    ** of less than 16 words, to avoid fragmenting memory by creating too
    ** many distinct free lists.  The garbage collector also requires that
    ** if we're allocating more than one word, we round up to an even number
    ** of words.
    */

    #ifndef __GNUC__
      /*
      ** Without the gcc extensions __builtin_constant_p() and ({...}),
      ** INLINE_ALLOC would probably be a performance _loss_.
      */
      #error "INLINE_ALLOC requires the use of GCC"
    #endif

    #include "gc_inl.h"
    #define tag_incr_hp(dest, tag, count) 				\
	( __builtin_constant_p(count) && (count) < 16 			\
	? ({ 	void * temp; 						\
		/* if size > 1, round up to an even number of words */	\
		Word num_words = ((count) == 1 ? 1 : 2 * (((count) + 1) / 2));\
		GC_MALLOC_WORDS(temp, num_words);			\
		(dest) = (Word)mkword((tag), temp);			\
	  })								\
	: tag_incr_hp_n((dest), (tag), (count))				\
	)

  #else /* not INLINE_ALLOC */

    #define tag_incr_hp(dest, tag, count) \
	tag_incr_hp_n((dest), (tag), (count))

  #endif /* not INLINE_ALLOC */

  #define mark_hp(dest)		((void)0)
  #define restore_hp(src)	((void)0)

			/* we use `MR_hp' as a convenient temporary here */
  #define hp_alloc(count) (						\
		incr_hp(LVALUE_CAST(Word, MR_hp), (count)),		\
		MR_hp += (count),					\
		(void)0							\
	)
  #define hp_alloc_atomic(count) (					\
		incr_hp_atomic(LVALUE_CAST(Word, MR_hp), (count)), 	\
		MR_hp += (count),					\
		(void)0							\
	)

  #define MR_free_heap(ptr)	GC_FREE((ptr))

#else /* not CONSERVATIVE_GC */

  #define tag_incr_hp(dest, tag, count) 			\
	(							\
		(dest) = (Word) mkword(tag, (Word) MR_hp),	\
		debugincrhp(count, MR_hp),			\
		MR_hp += (count),				\
		heap_overflow_check(),				\
		(void)0						\
	)
  #define tag_incr_hp_atomic(dest, tag, count) \
	tag_incr_hp((dest), (tag), (count))

  #define mark_hp(dest)		((dest) = (Word) MR_hp)

  /*
  ** When restoring MR_hp, we must make sure that we don't truncate the heap
  ** further than it is safe to. We can only truncate it as far as
  ** min_heap_reclamation_point. See the comments in mercury_context.h next to
  ** the set_min_heap_reclamation_point() macro.
  */
  #define	restore_hp(src)	(					\
  			LVALUE_CAST(Word, MR_hp) = (src),		\
  			(void)0						\
  		)

  /*
  #define	restore_hp(src)	(					\
  			LVALUE_CAST(Word, MR_hp) =			\
  			  ( (Word) MR_min_hp_rec < (src) ?		\
  			  (src) : (Word) MR_min_hp_rec ),		\
  			(void)0						\
  		)
  */
  
  #define hp_alloc(count)  	 incr_hp(LVALUE_CAST(Word, MR_hp), count)
  #define hp_alloc_atomic(count) incr_hp_atomic(LVALUE_CAST(Word, MR_hp), count)

  #define MR_free_heap(ptr)	((void)0)
  
#endif /* not CONSERVATIVE_GC */
  
#ifdef	PROFILE_MEMORY
  #define tag_incr_hp_msg(dest, tag, count, proclabel, type)		\
	(								\
		MR_record_allocation((count), LABEL(proclabel), 	\
			MR_STRINGIFY(proclabel), (type)),		\
		tag_incr_hp((dest), (tag), (count))			\
	)
  #define tag_incr_hp_atomic_msg(dest, tag, count, proclabel, type) 	\
	(								\
		MR_record_allocation((count), LABEL(proclabel), 	\
			MR_STRINGIFY(proclabel), (type)),		\
		tag_incr_hp_atomic((dest), (tag), (count))		\
	)
#else /* not PROFILE_MEMORY */
  #define tag_incr_hp_msg(dest, tag, count, proclabel, type) \
		tag_incr_hp((dest), (tag), (count))
  #define tag_incr_hp_atomic_msg(dest, tag, count, proclabel, type) \
		tag_incr_hp_atomic((dest), (tag), (count))
#endif /* not PROFILE_MEMORY */

/*
** The incr_hp*() macros are defined in terms of the tag_incr_hp*() macros.
** Note: the `proclabel' argument is not parenthesized, since it must
** be a label name; we may need to prefix `_entry_' in front of it,
** which wouldn't work if it was parenthesized.
*/
#define	incr_hp(dest, count) \
		tag_incr_hp((dest), mktag(0), (count))
#define	incr_hp_msg(dest, count, proclabel, type) \
		tag_incr_hp_msg((dest), mktag(0), (count), proclabel, (type))
#define	incr_hp_atomic(dest, count) \
		tag_incr_hp_atomic((dest), mktag(0), (count))
#define	incr_hp_atomic_msg(dest, count, proclabel, type) \
		tag_incr_hp_atomic_msg((dest), mktag(0), (count), \
			proclabel, (type))

/*
** Note that gcc optimizes `hp += 2; return hp - 2;'
** to `tmp = hp; hp += 2; return tmp;', so we don't need to use
** gcc's expression statements in the code below.
*/

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create1(w1)						\
	(							\
		hp_alloc(1),					\
		MR_hp[-1] = (Word) (w1),			\
		debugcr1(MR_hp[-1], MR_hp),			\
		/* return */ (Word) (MR_hp - 1)			\
	)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2(w1, w2)						\
	(							\
		hp_alloc(2),					\
		MR_hp[-2] = (Word) (w1),			\
		MR_hp[-1] = (Word) (w2),			\
		debugcr2(MR_hp[-2], MR_hp[-1], MR_hp),		\
		/* return */ (Word) (MR_hp - 2)			\
	)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create3(w1, w2, w3)					\
	(							\
		hp_alloc(3),					\
		MR_hp[-3] = (Word) (w1),			\
		MR_hp[-2] = (Word) (w2),			\
		MR_hp[-1] = (Word) (w3),			\
		/* return */ (Word) (MR_hp - 3)			\
	)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_bf(w1)						\
	(							\
		MR_hp = MR_hp + 2,				\
		MR_hp[-2] = (Word) (w1),			\
		heap_overflow_check(),				\
		/* return */ (Word) (MR_hp - 2)			\
	)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2_fb(w2)						\
	(							\
		MR_hp = MR_hp + 2,				\
		MR_hp[-1] = (Word) (w2),			\
		heap_overflow_check(),				\
		/* return */ (Word) (MR_hp - 2)			\
	)

/*
** Indended for use in handwritten C code where the Mercury registers
** may have been clobbered due to C function calls (eg, on the SPARC due
** to sliding register windows).
** Remember to save_transient_registers() before calls to such code, and
** restore_transient_registers() after.
*/

#define incr_saved_hp(A, B)					\
	do { 							\
		restore_transient_registers();			\
		incr_hp((A), (B));				\
		save_transient_registers();			\
	} while (0)

#define incr_saved_hp_atomic(A, B) 				\
	do { 							\
		restore_transient_registers();			\
		incr_hp_atomic((A), (B));			\
		save_transient_registers();			\
	} while (0)

#endif /* not MERCURY_HEAP_H */
