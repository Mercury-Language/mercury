#ifndef TAGS_H
#define TAGS_H

/* DEFINITIONS FOR WORD LAYOUT */

#ifdef	HIGHTAGS

#define	mktag(t)	(t << 30)
#define	tag(w)		(w & 0xc0000000)
#define	body(w, t)	(w & ~0xc0000000)
#define	mkword(t, p)	((uint) t | (uint) p)
#define	field(t, p, i)	(* (Word *) (body(p) + i * WORDSIZE))

#else

#define	mktag(t)	(t)
#define	tag(w)		(w & 0x3)
#define	body(w, t)	(w - t)
#define	mkword(t, p)	((uint) p | (uint) t)
#define	field(t, p, i)	(* (Word *) (body(p, t) + i * WORDSIZE))

#endif

#define	bTAG_NIL	0
#define	bTAG_CONS	1
#define	bTAG_VAR	3

#define	TAG_NIL		mktag(bTAG_NIL)
#define	TAG_CONS	mktag(bTAG_CONS)
#define	TAG_VAR		mktag(bTAG_VAR)

	/* the macro is more efficient than a function, but it requires
	   GNU C's statement-expressions extension, since we need a loop
	   inside an expression */
#ifdef __GNUC__
#define	deref(p)	__extension__ ({			\
				reg	Word	pt;		\
								\
				pt = p;				\
				while (tag(pt) == TAG_VAR)	\
					pt = * (Word *)		\
						body(pt, TAG_VAR);\
				/* return */ pt;		\
			})
#else	/* not __GNUC__ */
static Word deref(Word p) {
	reg	Word	pt;		
					
	pt = p;				
	while (tag(pt) == TAG_VAR)	
		pt = * (Word *)	body(pt, TAG_VAR);
	return pt;
}
#endif	/* not __GNUC__ */

#endif	/* TAGS_H */
