/*
** Copyright (C) 1995-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_heap.h - definitions for manipulating the Mercury heap */

#ifndef MERCURY_HEAP_H
#define MERCURY_HEAP_H

#include "mercury_conf.h"		/* for MR_CONSERVATIVE_GC */
#include "mercury_types.h"		/* for `MR_Word' */
#include "mercury_context.h"		/* for min_heap_reclamation_point() */
#include "mercury_heap_profile.h"	/* for MR_record_allocation() */
#include "mercury_deep_profiling.h"	/* for MR_current_call_site_dynamic */
#include "mercury_std.h"		/* for MR_EXTERN_INLINE */
#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.h"			/* for MR_new_object() */
#endif

#ifdef MR_CONSERVATIVE_GC

  #ifdef MR_MPS_GC
    #include "mercury_mps.h"
  #endif
  #ifdef MR_BOEHM_GC
    #include "gc.h"
  #endif

  #define MR_tag_incr_hp_n(dest, tag, count) 				\
	((dest) = (MR_Word) MR_mkword((tag),				\
			(MR_Word) GC_MALLOC((count) * sizeof(MR_Word))))
  #define MR_tag_incr_hp_atomic(dest, tag, count)			\
	((dest) = (MR_Word) MR_mkword((tag),				\
			(MR_Word) GC_MALLOC_ATOMIC((count) * sizeof(MR_Word))))

  #ifdef MR_INLINE_ALLOC

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
      ** MR_INLINE_ALLOC would probably be a performance _loss_.
      */
      #error "MR_INLINE_ALLOC requires the use of GCC"
    #endif

    #include "gc_inl.h"
    #define MR_tag_incr_hp(dest, tag, count) 				\
	( __builtin_constant_p(count) && (count) < 16 			\
	? ({ 	void * temp; 						\
		/* if size > 1, round up to an even number of words */	\
		MR_Word num_words = ((count) == 1 ? 1 :			\
			2 * (((count) + 1) / 2));			\
		GC_MALLOC_WORDS(temp, num_words);			\
		(dest) = (MR_Word) MR_mkword((tag), temp);		\
	  })								\
	: MR_tag_incr_hp_n((dest), (tag), (count))			\
	)

  #else /* not MR_INLINE_ALLOC */

    #define MR_tag_incr_hp(dest, tag, count) \
	MR_tag_incr_hp_n((dest), (tag), (count))

  #endif /* not MR_INLINE_ALLOC */

  #define MR_mark_hp(dest)		((void)0)
  #define MR_restore_hp(src)		((void)0)

			/* we use `MR_hp' as a convenient temporary here */
  #define MR_hp_alloc(count) (						\
		MR_incr_hp(MR_LVALUE_CAST(MR_Word, MR_hp), (count)),	\
		MR_hp += (count),					\
		(void) 0						\
	)
  #define MR_hp_alloc_atomic(count) (					\
		MR_incr_hp_atomic(MR_LVALUE_CAST(MR_Word, MR_hp), (count)),\
		MR_hp += (count),					\
		(void) 0						\
	)

  #define MR_free_heap(ptr)	GC_FREE((ptr))

#else /* not MR_CONSERVATIVE_GC */

  #define MR_tag_incr_hp(dest, tag, count) 				\
		(							\
			(dest) = (MR_Word) MR_mkword(tag, (MR_Word) MR_hp),\
			MR_debugincrhp(count, MR_hp),			\
			MR_hp += (count),				\
			MR_heap_overflow_check(),			\
			(void) 0					\
		)
  #define MR_tag_incr_hp_atomic(dest, tag, count) 			\
		MR_tag_incr_hp((dest), (tag), (count))

  #define MR_mark_hp(dest)		((dest) = (MR_Word) MR_hp)

  /*
  ** When restoring MR_hp, we must make sure that we don't truncate the heap
  ** further than it is safe to. We can only truncate it as far as
  ** min_heap_reclamation_point. See the comments in mercury_context.h next to
  ** the set_min_heap_reclamation_point() macro.
  */
  #define MR_restore_hp(src)						\
		(							\
  			MR_LVALUE_CAST(MR_Word, MR_hp) = (src),		\
  			(void) 0					\
  		)

  /*
  #define	MR_restore_hp(src)					\
  		(							\
  			MR_LVALUE_CAST(MR_Word, MR_hp) =		\
  			  ( (MR_Word) MR_min_hp_rec < (src) ?		\
  			  (src) : (MR_Word) MR_min_hp_rec ),		\
  			(void) 0					\
  		)
  */
  
  #define MR_hp_alloc(count)  	 	MR_incr_hp(			\
		  			MR_LVALUE_CAST(MR_Word, MR_hp), count)
  #define MR_hp_alloc_atomic(count)	MR_incr_hp_atomic(		\
		  			MR_LVALUE_CAST(MR_Word, MR_hp), count)

  #define MR_free_heap(ptr)		((void) 0)
  
#endif /* not MR_CONSERVATIVE_GC */
  
#if	defined (MR_DEEP_PROFILING) && defined(MR_DEEP_PROFILING_MEMORY)
  #define MR_maybe_record_allocation(count, proclabel, type)		\
	(								\
		MR_current_call_site_dynamic->MR_csd_own.MR_own_allocs	\
			+= 1,						\
		MR_current_call_site_dynamic->MR_csd_own.MR_own_words	\
			+= (count)					\
	)
#elif	defined(MR_MPROF_PROFILE_MEMORY)
  #define MR_maybe_record_allocation(count, proclabel, type)		\
	MR_record_allocation((count), MR_ENTRY(proclabel), 		\
		MR_STRINGIFY(proclabel), (type))
#else
  #define MR_maybe_record_allocation(count, proclabel, type)		\
  	((void) 0)
#endif
  	
#define MR_tag_incr_hp_msg(dest, tag, count, proclabel, type)		\
	(								\
		MR_maybe_record_allocation((count), proclabel, (type)),	\
		MR_tag_incr_hp((dest), (tag), (count))			\
	)
#define MR_tag_incr_hp_atomic_msg(dest, tag, count, proclabel, type) 	\
	(								\
		MR_maybe_record_allocation((count), proclabel, (type)),	\
		MR_tag_incr_hp_atomic((dest), (tag), (count))		\
	)

/*
** The MR_incr_hp*() macros are defined in terms of the MR_tag_incr_hp*()
** macros. Note: the `proclabel' argument is not parenthesized, since it must
** be a label name; we may need to prefix `_entry_' in front of it,
** which wouldn't work if it was parenthesized.
*/
#define	MR_incr_hp(dest, count) 					\
		MR_tag_incr_hp((dest), MR_mktag(0), (count))
#define	MR_incr_hp_msg(dest, count, proclabel, type)			\
		MR_tag_incr_hp_msg((dest), MR_mktag(0), (count),	\
			proclabel, (type))
#define	MR_incr_hp_atomic(dest, count)					\
		MR_tag_incr_hp_atomic((dest), MR_mktag(0), (count))
#define	MR_incr_hp_atomic_msg(dest, count, proclabel, type)		\
		MR_tag_incr_hp_atomic_msg((dest), MR_mktag(0), (count), \
			proclabel, (type))
#define	MR_incr_hp_type(dest, typename)					\
		do {							\
			MR_Word	tmp;					\
			MR_tag_incr_hp(tmp, MR_mktag(0),		\
				(MR_bytes_to_words(sizeof(typename))));	\
			(dest) = (typename *) tmp;			\
		} while (0)
#define	MR_incr_hp_type_msg(dest, typename, proclabel, type)		\
		do {							\
			MR_Word	tmp;					\
			MR_tag_incr_hp_msg(tmp, MR_mktag(0),		\
				(MR_bytes_to_words(sizeof(typename))),	\
				proclabel, (type));			\
			(dest) = (typename *) tmp;			\
		} while (0)

#ifdef MR_HIGHLEVEL_CODE

/*
** Note that this code is also duplicated in mercury.c.
*/

MR_EXTERN_INLINE MR_Word MR_create1(MR_Word w1);
MR_EXTERN_INLINE MR_Word MR_create2(MR_Word w1, MR_Word w2);
MR_EXTERN_INLINE MR_Word MR_create3(MR_Word w1, MR_Word w2, MR_Word w3) ;

MR_EXTERN_INLINE MR_Word
MR_create1(MR_Word w1) 
{
	MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 1 * sizeof(MR_Word),
			"create1");
	p[0] = w1;
	return (MR_Word) p;
}

MR_EXTERN_INLINE MR_Word
MR_create2(MR_Word w1, MR_Word w2) 
{
	MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 2 * sizeof(MR_Word),
			"create2");
	p[0] = w1;
	p[1] = w2;
	return (MR_Word) p;
}

MR_EXTERN_INLINE MR_Word
MR_create3(MR_Word w1, MR_Word w2, MR_Word w3) 
{
	MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 3 * sizeof(MR_Word),
			"create3");
	p[0] = w1;
	p[1] = w2;
	p[2] = w3;
	return (MR_Word) p;
}

#define MR_create1_msg(w1, proclabel, type) \
	MR_create1((w1))
#define MR_create2_msg(w1, w2, proclabel, type)	\
	MR_create2((w1), (w2))
#define MR_create3_msg(w1, w2, w3, proclabel, type) \
	MR_create3((w1), (w2), (w3))

#else /* ! MR_HIGHLEVEL_CODE */

/*
** Note that gcc optimizes `hp += 2; return hp - 2;'
** to `tmp = hp; hp += 2; return tmp;', so we don't need to use
** gcc's expression statements in the code below.
*/

/* used only by hand-written code not by the automatically generated code */
#define MR_create1(w1)						\
	(							\
		MR_hp_alloc(1),					\
		MR_hp[-1] = (MR_Word) (w1),			\
		MR_debugcr1(MR_hp[-1], MR_hp),			\
		/* return */ (MR_Word) (MR_hp - 1)			\
	)

/* used only by hand-written code not by the automatically generated code */
#define MR_create2(w1, w2)						\
	(							\
		MR_hp_alloc(2),					\
		MR_hp[-2] = (MR_Word) (w1),			\
		MR_hp[-1] = (MR_Word) (w2),			\
		MR_debugcr2(MR_hp[-2], MR_hp[-1], MR_hp),		\
		/* return */ (MR_Word) (MR_hp - 2)			\
	)

/* used only by hand-written code not by the automatically generated code */
#define MR_create3(w1, w2, w3)					\
	(							\
		MR_hp_alloc(3),					\
		MR_hp[-3] = (MR_Word) (w1),			\
		MR_hp[-2] = (MR_Word) (w2),			\
		MR_hp[-1] = (MR_Word) (w3),			\
		/* return */ (MR_Word) (MR_hp - 3)			\
	)

/* used only by hand-written code not by the automatically generated code */
#define MR_create1_msg(w1,proclabel,type)				\
	(								\
		MR_maybe_record_allocation(1, proclabel, (type)),	\
		MR_hp_alloc(1),						\
		MR_hp[-1] = (MR_Word) (w1),				\
		MR_debugcr1(MR_hp[-1], MR_hp),				\
		/* return */ (MR_Word) (MR_hp - 1)			\
	)

/* used only by hand-written code not by the automatically generated code */
#define MR_create2_msg(w1, w2, proclabel, type)				\
	(								\
		MR_maybe_record_allocation(2, proclabel, (type)),	\
		MR_hp_alloc(2),						\
		MR_hp[-2] = (MR_Word) (w1),				\
		MR_hp[-1] = (MR_Word) (w2),				\
		MR_debugcr2(MR_hp[-2], MR_hp[-1], MR_hp),		\
		/* return */ (MR_Word) (MR_hp - 2)			\
	)

/* used only by hand-written code not by the automatically generated code */
#define MR_create3_msg(w1, w2, w3, proclabel, type)			\
	(								\
		MR_maybe_record_allocation(3, proclabel, (type)),	\
		MR_hp_alloc(3),						\
		MR_hp[-3] = (MR_Word) (w1),				\
		MR_hp[-2] = (MR_Word) (w2),				\
		MR_hp[-1] = (MR_Word) (w3),				\
		/* return */ (MR_Word) (MR_hp - 3)			\
	)

#endif /* ! MR_HIGHLEVEL_CODE */

/*
** Indended for use in handwritten C code where the Mercury registers
** may have been clobbered due to C function calls (eg, on the SPARC due
** to sliding register windows).
** Remember to MR_save_transient_hp() before calls to such code, and
** MR_restore_transient_hp() after.
*/

#define MR_incr_saved_hp(A, B)					\
	do { 							\
		MR_restore_transient_hp();			\
		MR_incr_hp((A), (B));				\
		MR_save_transient_hp();				\
	} while (0)

#define MR_incr_saved_hp_atomic(A, B) 				\
	do { 							\
		MR_restore_transient_hp();			\
		MR_incr_hp_atomic((A), (B));			\
		MR_save_transient_hp();				\
	} while (0)

/*
** Code to box/unbox types declared with `pragma foreign_type'.
*/

/*
** void MR_MAYBE_BOX_FOREIGN_TYPE(type T, const T &value, MR_Box &box);
**	Copy a value of type T from `value' to `box',
**	boxing it if necessary (i.e. if type T won't fit in type MR_Box).
*/
#define MR_MAYBE_BOX_FOREIGN_TYPE(T, value, box)			\
   	do {								\
		MR_CHECK_EXPR_TYPE((value), T);				\
		MR_CHECK_EXPR_TYPE((box), MR_Box);			\
		if (sizeof(T) > sizeof(MR_Box)) {			\
			size_t size_in_words =				\
				(sizeof(T) + sizeof(MR_Word) - 1)	\
				 / sizeof(MR_Word);			\
			/* XXX this assumes that nothing requires */	\
			/* stricter alignment than MR_Float */		\
			MR_make_hp_float_aligned();			\
			MR_incr_hp(MR_LVALUE_CAST(MR_Word, (box)),	\
					size_in_words);			\
			*(T *)(box) = (value);				\
			MR_maybe_record_allocation(size_in_words,	\
				"", "foreign type: " MR_STRINGIFY(T));	\
		} else {						\
			/* We can't take the address of `box' here, */	\
			/* since it might be a global register. */	\
			/* Hence we need to use a temporary copy. */	\
			MR_Box box_copy;				\
			if (sizeof(T) < sizeof(MR_Box)) {		\
				/* make sure we don't leave any */	\
				/* part of it uninitialized */		\
				box_copy = 0;				\
			}						\
			memcpy(&box_copy, &(value), sizeof(T));		\
			(box) = box_copy;				\
		}							\
	} while (0)
   
/*
** void MR_MAYBE_UNBOX_FOREIGN_TYPE(type T, MR_Box box, T &value);
**	Copy a value of type T from `box' to `value',
**	unboxing it if necessary.
*/
#define MR_MAYBE_UNBOX_FOREIGN_TYPE(T, box, value)			\
   	do {								\
		MR_CHECK_EXPR_TYPE((value), T);				\
		MR_CHECK_EXPR_TYPE((box), MR_Box);			\
		if (sizeof(T) > sizeof(MR_Word)) {			\
			(value) = *(T *)(box);				\
		} else {						\
			/* We can't take the address of `box' here, */	\
			/* since it might be a global register. */	\
			/* Hence we need to use a temporary copy. */	\
			MR_Box box_copy = (box);			\
			memcpy(&(value), &box_copy, sizeof(T));		\
		}							\
	} while (0)
   
#endif /* not MERCURY_HEAP_H */
