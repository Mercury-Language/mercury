/*
** Copyright (C) 1993-1995, 1997-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** std.h - "standard" [sic] definitions for C:
**	bool, TRUE, FALSE, min(), max(), streq(), etc.
*/

#ifndef MERCURY_STD_H
#define MERCURY_STD_H

#include <stdlib.h>	/* for size_t */
#include <assert.h>	/* for assert() */
#include <ctype.h>	/* for isalnum(), etc. */

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

/*
** The ANSI C isalnum(), etc. macros require that the argument be cast to
** `unsigned char'; if you pass a signed char, the behaviour is undefined.
** Hence we define `MR_' versions of these that do the cast -- you should
** make sure to always use the `MR_' versions rather than the standard ones.
*/
#define	MR_isalnum(c)	isdigit((unsigned char) (c))
#define	MR_isdigit(c)	isdigit((unsigned char) (c))
#define	MR_isspace(c)	isspace((unsigned char) (c))

#define streq(s1, s2)		(strcmp(s1, s2) == 0)
#define strdiff(s1, s2)		(strcmp(s1, s2) != 0)
#define strtest(s1, s2)		(strcmp(s1, s2))
#define strneq(s1, s2, n)	(strncmp(s1, s2, n) == 0)
#define strndiff(s1, s2, n)	(strncmp(s1, s2, n) != 0)
#define strntest(s1, s2, n)	(strncmp(s1, s2, n))

#define	ungetchar(c)		ungetc(c, stdin)

/* XXX these should go in mercury_memory.h or mercury_heap.h */
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
** For speed, turn assertions off,
** unless low-level debugging is enabled.
*/
#ifdef MR_LOWLEVEL_DEBUG
  #define MR_assert(ASSERTION)	assert(ASSERTION)
#else
  #define MR_assert(ASSERTION)	((void)0)
#endif


/* XXX these should go in mercury_memory.h or mercury_heap.h */
extern	void	*newmem(size_t);
extern	void	*resizemem(void *, size_t);
extern	void	oldmem(void *);

#endif /* not MERCURY_STD_H */
