/*
** Copyright (C) 1993-1995, 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** std.h - "standard" [sic] definitions for C:
**	bool, TRUE, FALSE, min(), max(), streq(), etc.
*/

#ifndef STD_H
#define STD_H

#include <stdlib.h>	/* for size_t */
#include <assert.h>	/* for assert() */

#ifndef	reg
#define	reg		register
#endif
#ifndef	bool
#define	bool		char
#endif

#ifndef max
#define	max(a, b)	((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define	min(a, b)	((a) < (b) ? (a) : (b))
#endif

#define streq(s1, s2)		(strcmp(s1, s2) == 0)
#define strdiff(s1, s2)		(strcmp(s1, s2) != 0)
#define strtest(s1, s2)		(strcmp(s1, s2))
#define strneq(s1, s2, n)	(strncmp(s1, s2, n) == 0)
#define strndiff(s1, s2, n)	(strncmp(s1, s2, n) != 0)
#define strntest(s1, s2, n)	(strncmp(s1, s2, n))

#define	ungetchar(c)		ungetc(c, stdin)

/* XXX these should go in memory.h or heap.h */
#define make(t)			((t *) newmem(sizeof(t)))
#define make_many(t, n)		((t *) newmem((n) * sizeof(t)))
#define resize_many(t, p, n)	((t *) resizemem((p), (n) * sizeof(t)))

#ifndef	TRUE
#define	TRUE		1
#endif
#ifndef	FALSE
#define	FALSE		0
#endif

/*
** turn assertions off for speed
*/
#ifdef SPEED
#define MR_assert(ASSERTION)	((void)0)
#else
#define	MR_assert(ASSERTION)	assert(ASSERTION)
#endif


/* XXX these should go in memory.h or heap.h */
extern	void	*newmem(size_t);
extern	void	*resizemem(void *, size_t);
extern	void	oldmem(void *);

#endif /* not STD_H */
