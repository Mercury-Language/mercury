#ifndef TAGS_H
#define TAGS_H

/* DEFINITIONS FOR WORD LAYOUT */

#ifdef	HIGHTAGS

#define	mktag(t)	((t) << 30)
#define mkbody(i)	(i)
#define	tag(w)		((w) & 0xc0000000)
#define	body(w, t)	((w) & ~0xc0000000)
#define	mkword(t, p)	((uint)(t) + (uint)(p))
#define	field(t, p, i)	(* (((Word *) body((p), (t))) + (i)))

#else

#define	mktag(t)	(t)
#define mkbody(i)	((i) << 2)
#define	tag(w)		((w) & 0x3)
#define	body(w, t)	((w) - (t))
#define	mkword(t, p)	((uint)(t) + (uint)(p))
#define	field(t, p, i)	((Word *) body((p), (t)))[i]

/* 
#define	field(t, p, i)	(* (((Word *) body((p), (t))) + (i)))
old def	field(t, p, i)	(* (Word *) (body((p), (t)) + (i) * WORDSIZE))
*/

#endif

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
