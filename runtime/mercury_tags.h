/*
** Copyright (C) 1993-1999 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_tags.h - defines macros for tagging and untagging words.
** Also defines macros for accessing the Mercury list type from C.
*/

#ifndef MERCURY_TAGS_H
#define	MERCURY_TAGS_H

#include <limits.h>		/* for `CHAR_BIT' */
#include "mercury_conf.h"	/* for `LOW_TAG_BITS' */
#include "mercury_types.h"	/* for `Word' */

/* DEFINITIONS FOR WORD LAYOUT */

#define	MR_WORDBITS	(CHAR_BIT * sizeof(Word))

/* TAGBITS specifies the number of bits in each word that we can use for tags */
#ifndef TAGBITS
  #ifdef HIGHTAGS
    #error "HIGHTAGS defined but TAGBITS undefined"
  #else
    #define TAGBITS	LOW_TAG_BITS
  #endif
#endif

#if TAGBITS > 0 && defined(HIGHTAGS) && defined(CONSERVATIVE_GC)
  #error "Conservative GC does not work with high tag bits"
#endif

#ifdef	HIGHTAGS

#define	MR_mktag(t)	((Word)(t) << (WORDBITS - TAGBITS))
#define	MR_unmktag(w)	((Word)(w) >> (WORDBITS - TAGBITS))
#define	MR_tag(w)	((w) & ~(~(Word)0 >> TAGBITS))
#define	MR_mkbody(i)	(i)
#define	MR_unmkbody(w)	(w)
#define	MR_body(w, t)	((w) & (~(Word)0 >> TAGBITS))
#define	MR_strip_tag(w)	((w) & (~(Word)0 >> TAGBITS))

#else /* ! HIGHTAGS */

#define	MR_mktag(t)	(t)
#define	MR_unmktag(w)	(w)
#define	MR_tag(w)	((w) & ((1 << TAGBITS) - 1))
#define	MR_mkbody(i)	((i) << TAGBITS)
#define	MR_unmkbody(w)	((Word) (w) >> TAGBITS)
#define	MR_body(w, t)	((Word) (w) - (t))
#define	MR_strip_tag(w)	((w) & (~(Word)0 << TAGBITS))

#endif /* ! HIGHTAGS */

/*
** the result of MR_mkword() is cast to (Word *), not to (Word)
** because MR_mkword() may be used in initializers for static constants
** and casts from pointers to integral types are not valid
** constant-expressions in ANSI C.  It cannot be (const Word *) because
** some ANSI C compilers won't allow assignments where the RHS is of type
** const and the LHS is not declared const.
*/

#define	MR_mkword(t, p)			((Word *)((char *)(p) + (t)))

#define	MR_field(t, p, i)		((Word *) body((p), (t)))[i]
#define	MR_const_field(t, p, i)		((const Word *) body((p), (t)))[i]

#define	MR_mask_field(p, i)		((Word *) strip_tag(p))[i]
#define	MR_const_mask_field(p, i)	((const Word *) strip_tag(p))[i]

/*
** the following MR_list_* macros are used by handwritten C code
** that needs to access Mercury lists. The definitions of these macros
** depend on the data representation scheme used by compiler/make_tags.m.
*/

#define	MR_RAW_TAG_NIL		0
#define	MR_RAW_TAG_CONS		1

#define	MR_TAG_NIL		MR_mktag(MR_RAW_TAG_NIL)
#define	MR_TAG_CONS		MR_mktag(MR_RAW_TAG_CONS)

#if TAGBITS > 0

#define	MR_list_is_empty(list)	(MR_tag(list) == MR_TAG_NIL)
#define	MR_list_head(list)	MR_field(MR_TAG_CONS, (list), 0)
#define	MR_list_tail(list)	MR_field(MR_TAG_CONS, (list), 1)
#define	MR_list_empty()		((Word) MR_mkword(MR_TAG_NIL, MR_mkbody(0)))
#define	MR_list_cons(head,tail)	((Word) MR_mkword(MR_TAG_CONS, \
					create2((head),(tail))))
#define	MR_list_empty_msg(proclabel)	\
				((Word) MR_mkword(MR_TAG_NIL, MR_mkbody(0)))
#define	MR_list_cons_msg(head,tail,proclabel) \
				((Word) MR_mkword(MR_TAG_CONS, \
					MR_create2_msg((head),(tail), \
						proclabel, "list:list/1")))

#else

#define	MR_list_is_empty(list)	(MR_field(MR_mktag(0), (list), 0) \
					== MR_RAW_TAG_NIL)
#define	MR_list_head(list)	MR_field(MR_mktag(0), (list), 1)
#define	MR_list_tail(list)	MR_field(MR_mktag(0), (list), 2)
#define	MR_list_empty()		((Word) MR_mkword(MR_mktag(0), \
					create1(MR_RAW_TAG_NIL)))
#define	MR_list_cons(head,tail)	((Word) MR_mkword(MR_mktag(0), \
					create3(MR_RAW_TAG_CONS, \
						(head), (tail))))
#define	MR_list_empty_msg(proclabel) \
				((Word) MR_mkword(MR_mktag(0), \
					MR_create1_msg(MR_RAW_TAG_NIL, \
						proclabel, "list:list/1")))
#define	MR_list_cons_msg(head,tail,proclabel) \
				((Word) MR_mkword(MR_mktag(0), \
					MR_create3_msg(MR_RAW_TAG_CONS, \
						(head), (tail), \
						proclabel, "list:list/1")))

#endif

#endif	/* not MERCURY_TAGS_H */
