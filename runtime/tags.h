#ifndef TAGS_H
#define TAGS_H

#include <limits.h>

/* DEFINITIONS FOR WORD LAYOUT */

#define	WORDBITS	(CHAR_BIT * WORDSIZE)
#ifndef TAGBITS
#define	TAGBITS		2
#endif

#ifdef	HIGHTAGS

#define	mktag(t)	((t) << (WORDBITS - TAGBITS))
#define	unmktag(w)	((Word) (w) >> (WORDBITS - TAGBITS))
#define	tag(w)		((w) & ~(((Word) (~0) >> TAGBITS)))
#define mkbody(i)	(i)
#define unmkbody(w)	(w)
#define	body(w, t)	((w) & ((Word) (~0) >> TAGBITS)
#define	mkword(t, p)	((Word)(t) + (Word)(p))
#define	field(t, p, i)	((Word *) body((p), (t)))[i]

#else

#define	mktag(t)	(t)
#define	unmktag(w)	(w)
#define	tag(w)		((w) & ((1 << TAGBITS) - 1))
#define mkbody(i)	((i) << TAGBITS)
#define unmkbody(w)	((Word) (w) >> TAGBITS)
#define	body(w, t)	((w) - (t))
#define	mkword(t, p)	((Word)(t) + (Word)(p))
#define	field(t, p, i)	((Word *) body((p), (t)))[i]

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
