/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** imp.h - defines the interface to the Mercury abstract machine.
**
** IMPORTANT: this must be the *first* header file that is #included.
** It must come before any system header files.  This is because on some
** systems, the system header files include inline functions, and this
** causes problems when using global register variables, as gcc requires
** global register variable declarations to precede any function definitions.
*/

#ifndef IMP_H
#define IMP_H

#ifdef SPEED
/* turn off `assert()'s */
#define NDEBUG
#endif

/* GENERAL DEFINITIONS */

#include	"conf.h"

/* Note that we require sizeof(Word) == sizeof(Integer) == sizeof(Code*) */
/* this is assured by the autoconfiguration script */

typedef	unsigned WORD_TYPE	Word;
typedef WORD_TYPE		Integer;
typedef unsigned WORD_TYPE	Unsigned;
typedef void			Code;	/* code addresses are `void *' */

#include	"regs.h"	/* must come before system headers */

#include	<stddef.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<assert.h>
#include	<sys/types.h>

#include	"std.h"

/* continuation function type, for --high-level-C option */
typedef void (*Cont) (void);

/*
** semidet predicates indicate success or failure by leaving nonzero or zero
** respectively in register r1
*/
#define SUCCESS_INDICATOR r1

#include	"prof.h"

/* DEFINITIONS FOR CALLS AND RETURNS */

#define	noprof_localcall(label, succ_cont)			\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			succip = (succ_cont);			\
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

#if defined(__alpha__) && defined(USE_ASM_LABELS)
#define	noprof_call(proc, succ_cont)				\
		({						\
			__label__ fixup_gp;			\
			debugcall((proc), (succ_cont));		\
			succip = (&&fixup_gp);			\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		fixup_gp:					\
			__asm__ __volatile__ (			\
				"ldgp $gp, 0($27)"		\
				: : : "memory"			\
			);					\
			GOTO(succ_cont); 			\
		})
	/* same as above, but with GOTO_LABEL rather than GOTO */
#define	noprof_call_localret(proc, succ_cont)			\
		({						\
			__label__ fixup_gp;			\
			debugcall((proc), (succ_cont));		\
			succip = (&&fixup_gp);			\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		fixup_gp:					\
			__asm__ __volatile__ (			\
				"ldgp $gp, 0($27)"		\
				: : : "memory"			\
			);					\
			GOTO_LABEL(succ_cont); 			\
		})
#else
#define	noprof_call(proc, succ_cont)				\
		do {						\
			debugcall((proc), (succ_cont));		\
			succip = (succ_cont);			\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		} while (0)
#define noprof_call_localret(proc, succ_cont) 			\
		noprof_call((proc), LABEL(succ_cont))
#endif

#define	localcall(label, succ_cont, current_label)		\
		do {						\
			debugcall(LABEL(label), (succ_cont));	\
			succip = (succ_cont);			\
			PROFILE(LABEL(label), (current_label));	\
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

#define	call(proc, succ_cont, current_label)			\
		do {						\
			PROFILE((proc), (current_label));	\
			noprof_call((proc), (succ_cont));	\
		} while (0)

#define	call_localret(proc, succ_cont, current_label)		\
		do {						\
			PROFILE((proc), (current_label));	\
			noprof_call_localret(proc, succ_cont); \
		} while (0)

#define	call_det_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_det_closure);	\
			call(ENTRY(do_call_det_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	call_semidet_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_semidet_closure); \
			call(ENTRY(do_call_semidet_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	call_nondet_closure(succ_cont, current_label)		\
		do {						\
			Declare_entry(do_call_nondet_closure);	\
			call(ENTRY(do_call_nondet_closure),	\
				(succ_cont), (current_label));	\
		} while (0)

#define	localtailcall(label, current_label)			\
		do {						\
			debugtailcall(LABEL(label));		\
			PROFILE(LABEL(label), (current_label)); \
			set_prof_current_proc(LABEL(label));	\
			GOTO_LABEL(label);			\
		} while (0)

#define	tailcall(proc, current_label)				\
		do {						\
			debugtailcall(proc);			\
			PROFILE((proc), (current_label));	\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
		} while (0)

#define	proceed()						\
		do {						\
			debugproceed();				\
			GOTO(succip);				\
		} while (0)

/* STRING HANDLING */

typedef char Char;	/* we may eventually move to using wchar_t */

typedef Char *String;

#define string_const(string, len) ((Word)string)
#define string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)

/*
** Note that hash_string is also defined in compiler/string.m.
** The two definitions here and the definition in string.m
** must be kept equivalent.
*/

#define do_hash_string(hash,s)				\
	{						\
	   int len = 0;					\
	   hash = 0;					\
	   while(((const char *)(s))[len]) {		\
		hash ^= (hash << 5);			\
		hash ^= ((const char *)(s))[len];	\
		len++;					\
	   }						\
	   hash ^= len;					\
	}

#ifdef __GNUC__

#define hash_string(s)					\
	({ int hash;					\
	   do_hash_string(hash,s);			\
	   hash;					\
	})

#else

/* the actual definition of hash_string is in aux.c */
/* it uses the macro below */

extern	int	hash_string(Word);

#endif

#define HASH_STRING_FUNC_BODY				\
	   int hash;					\
	   do_hash_string(hash, s);			\
	   return hash;

/* FLOATING POINT HANDLING */

#ifdef USE_SINGLE_PREC_FLOAT

typedef float Float;

#else

typedef double Float;
#define BOXED_FLOAT

#endif

#ifdef BOXED_FLOAT 

#define word_to_float(w) (*(Float *)(w))

#define FLOAT_WORDS ((sizeof(Float) + sizeof(Word) - 1) / sizeof(Word))

#ifdef CONSERVATIVE_GC
#define float_to_word(f) ( \
		hp_alloc(FLOAT_WORDS), \
		*(Float *)(void *)(hp - FLOAT_WORDS) = (f), \
		/* return */ (Word) (hp - FLOAT_WORDS) \
	)
#else
/* we need to ensure that what we allocated on the heap is properly
   aligned */
#define float_to_word(f) ( \
		( (Word)hp & (sizeof(Float) - 1) ? hp_alloc(1) : (void)0 ), \
		hp_alloc(FLOAT_WORDS), \
		*(Float *)(void *)(hp - FLOAT_WORDS) = (f), \
		/* return */ (Word) (hp - FLOAT_WORDS) \
	)
#endif

#ifdef __GNUC__
#define float_const(f) ({ static const Float d = f; (Word)&d; })
#else
#define float_const(f) float_to_word(f)	/* inefficient */
#endif

#else /* not BOXED_FLOAT */

/* unboxed float means we can assume sizeof(Float) == sizeof(Word) */

#define float_const(f) ((Float)f)

union FloatWord {
	Float f;
	Word w;
};

#ifdef __GNUC__

#define float_to_word(f) (__extension__ ((union FloatWord)(f)).w)
#define word_to_float(w) (__extension__ ((union FloatWord)(w)).f)

#else

static Word float_to_word(Float f) { union FloatWord tmp = f; return tmp.w; }
static Float word_to_float(Word w) { union FloatWord tmp = w; return tmp.f; }

#endif

#endif

#include	"tags.h"
#include	"goto.h"
#include	"engine.h"

#include	"heap.h"
#include	"stacks.h"
#include	"overflow.h"
#include	"debug.h"

#include	"aux.h"
#include	"label.h"
#include	"memory.h"
#include	"wrapper.h"
#ifdef CONSTRAINTS
#include	"mercury_solver_backtrack.h"
#endif

#endif /* IMP_H */
