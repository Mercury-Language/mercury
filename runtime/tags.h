/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef TAGS_H
#define TAGS_H

#include <limits.h>

/* DEFINITIONS FOR WORD LAYOUT */

#define	WORDBITS	(CHAR_BIT * sizeof(Word))

/* TAGBITS specifies the number of bits in each word that we can use for tags */
#ifndef TAGBITS
#ifdef HIGHTAGS
#error "HIGHTAGS defined but TAGBITS undefined"
#else
#define	TAGBITS		LOW_TAG_BITS
#endif
#endif

#if TAGBITS > 0 && defined(HIGHTAGS) && defined(CONSERVATIVE_GC)
#error "Conservative GC does not work with high tag bits"
#endif

#ifdef	HIGHTAGS

#define	mktag(t)	((Word)(t) << (WORDBITS - TAGBITS))
#define	unmktag(w)	((Word)(w) >> (WORDBITS - TAGBITS))
#define	tag(w)		((w) & ~(~(Word)0 >> TAGBITS))
#define mkbody(i)	(i)
#define unmkbody(w)	(w)
#define	body(w, t)	((w) & (~(Word)0 >> TAGBITS))
/*
** the result of mkword() is cast to (const Word *), not to (Word)
** because mkword() may be used in initializers for static constants
** and casts from pointers to integral types are not valid
** constant-expressions in ANSI C.
*/
#define	mkword(t, p)	((const Word *)((const char *)(p) + (Word)(t)))
#define	field(t, p, i)	((Word *) body((p), (t)))[i]

#else

#define	mktag(t)	(t)
#define	unmktag(w)	(w)
#define	tag(w)		((w) & ((1 << TAGBITS) - 1))
#define mkbody(i)	((i) << TAGBITS)
#define unmkbody(w)	((Word) (w) >> TAGBITS)
#define	body(w, t)	((w) - (t))
/*
** the result of mkword() is cast to (const Word *), not to (Word)
** because mkword() may be used in initializers for static constants
** and casts from pointers to integral types are not valid
** constant-expressions in ANSI C.
*/
/* XXX can't implement the above comment yet due to bootstrapping problems! */
#define	mkword(t, p)	((Word)((const char *)(p) + (t)))
#define	field(t, p, i)	((Word *) body((p), (t)))[i]

#endif

/*
** the following macros are used by the handwritten .mod code in io.mod etc.
** to access lists
*/

#if TAGBITS > 0

#define list_is_empty(list)	(tag(list) == mktag(0))
#define list_head(list)		field(TAG_CONS, (list), 0)
#define list_tail(list)		field(TAG_CONS, (list), 1)
#define list_empty()		mkword(mktag(0), mkbody(0))
#define list_cons(head,tail)	mkword(mktag(1), create2((head),(tail)))

#else

#define list_is_empty(list)	(field(mktag(0), (list), 0) == 0)
#define list_head(list)		field(mktag(0), (list), 1)
#define list_tail(list)		field(mktag(0), (list), 2)
#define list_empty()		mkword(mktag(0), create1(0))
#define list_cons(head,tail)	mkword(mktag(0), create3(1, (head), (tail)))

#endif

/* the rest of this file is for archaic code only */

#define	bTAG_NIL	0
#define	bTAG_CONS	1
#define	bTAG_VAR	3

#define	TAG_NIL		mktag(bTAG_NIL)
#define	TAG_CONS	mktag(bTAG_CONS)
#define	TAG_VAR		mktag(bTAG_VAR)

#define	deref(pt)	do {						\
				while (tag(pt) == TAG_VAR)		\
					(pt) = * (Word *)		\
						body((pt), TAG_VAR);	\
			} while(0)

#endif	/* TAGS_H */
