/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef IMP_H
#define IMP_H

#ifdef SPEED
/* turn off `assert()'s */
#define NDEBUG
#endif

#include	<stdio.h>
#include	<stdlib.h>
#include	<assert.h>
#include	<sys/types.h>
#include	"std.h"
#include	"conf.h"

/* GENERAL DEFINITIONS */

typedef	unsigned WORD_TYPE	Word;
typedef WORD_TYPE		Integer;
typedef void			Code; /* should be `typedef function_t Code' */

/* Note that we require sizeof(Word) == sizeof(Integer) == sizeof(Code*) */
/* this is assured by the autoconfiguration script */

#define	WORDSIZE	sizeof(Word)

#include	"tags.h"
#include	"regs.h"
#include	"goto.h"
#include	"engine.h"

/* DEFINITIONS FOR PROFILING */

#ifdef	PROFILE_CALLS

#include	"prof.h"
#define	PROFILE(callee, caller)		prof_call_profile((callee), (caller))

#else

#define	PROFILE(callee, caller)		((void)0)

#endif

#ifdef PROFILE_TIME
#include "prof.h"

#define set_prof_current_proc(target)		(prof_current_proc = (target))
#define update_prof_current_proc(target)	(prof_current_proc = (target))	

#else

#define set_prof_current_proc(target)		((void)0)
#define update_prof_current_proc(target)	((void)0)

#endif

/* DEFINITIONS FOR CALLS AND RETURNS */

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
			debugcall((proc), (succ_cont));		\
			succip = (succ_cont);			\
			PROFILE((proc), (current_label));	\
			set_prof_current_proc(proc);		\
			GOTO(proc);				\
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

#include	"heap.h"
#include	"stacks.h"
#include	"overflow.h"
#include	"debug.h"

/* STRING HANDLING */

#define string_const(string, len) ((Word)string)
#define string_equal(s1,s2) (strcmp((char*)(s1),(char*)(s2))==0)

#ifdef __GNUC__

/*
** Note that hash_string is also defined in compiler/string.m.
** The two definitions here and the definition in string.m
** must be kept equivalent.
*/

#define hash_string(s)					\
	({						\
	   int len = 0;					\
	   int hash = 0;				\
	   while(((char *)s)[len]) {			\
		hash ^= (hash << 5);			\
		hash ^= ((char *)s)[len];		\
		len++;					\
	   }						\
	   hash ^= len;					\
	   hash;					\
	})

#else

/* the actual definition of hash_string is in aux.c */
/* it uses the macro below */

extern	int	hash_string(const char *);

#define HASH_STRING_FUNC_BODY				\
	   int len = 0;					\
	   int hash = 0;				\
	   while(((char *)s)[len]) {			\
		hash ^= (hash << 5);			\
		hash ^= ((char *)s)[len];		\
		len++;					\
	   }						\
	   hash ^= len;					\
	   return hash;

#endif

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
		*(Float *)(void *)(hp - FLOAT_WORDS) = f, \
		/* return */ (Word) (hp - FLOAT_WORDS) \
	)
#else
/* we need to ensure that what we allocated on the heap is properly
   aligned */
#define float_to_word(f) ( \
		hp_alloc(FLOAT_WORDS), /* XXX alignment!!! */ \
		*(Float *)(void *)(hp - FLOAT_WORDS) = f, \
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

static Word float_to_word(Float f) { union FloatWord tmp = f; return f.w; }
static Float word_to_float(Word w) { union FloatWord tmp = w; return w.f; }

#endif

#endif

#include	"aux.h"
#include	"label.h"
#include	"memory.h"
#include	"wrapper.h"

#endif /* IMP_H */
