/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef STD_H
#define STD_H

/*
**	Standard definitions for C
*/

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

#ifdef SLOWSTRCMP
#define streq(s1, s2)		(strcmp(s1, s2) == 0)
#define strdiff(s1, s2)		(strcmp(s1, s2) != 0)
#define strtest(s1, s2)		(strcmp(s1, s2))
#define strneq(s1, s2, n)	(strncmp(s1, s2, n) == 0)
#define strndiff(s1, s2, n)	(strncmp(s1, s2, n) != 0)
#define strntest(s1, s2, n)	(strncmp(s1, s2, n))
#else
#define streq(s1, s2)		((*(s1) == *(s2)) && \
				(strcmp((s1)+1, (s2)+1) == 0))
#define strdiff(s1, s2)		((*(s1) != *(s2)) || \
				(strcmp((s1)+1, (s2)+1) != 0))
#define strtest(s1, s2)		((*(s1) != *(s2)) ? (*(s1) - *(s2)) : \
				strcmp((s1)+1, (s2)+1))
#define strneq(s1, s2, n)	((*(s1) == *(s2)) && \
				(strncmp((s1)+1, (s2)+1, n-1) == 0))
#define strndiff(s1, s2, n)	((*(s1) != *(s2)) || \
				(strncmp((s1)+1, (s2)+1, n-1) != 0))
#define strntest(s1, s2, n)	((*(s1) != *(s2)) ? (*(s1) - *(s2)) : \
				strncmp((s1)+1, (s2)+1, n-1))
#endif

#define	ungetchar(c)		ungetc(c, stdin)

#define make(t)			((t *) newmem(sizeof(t)))
#define make_many(t, n)		((t *) newmem((n) * sizeof(t)))
#define resize_many(t, p, n)	((t *) resizemem((p), (n) * sizeof(t)))

#ifndef	TRUE
#define	TRUE		1
#endif
#ifndef	FALSE
#define	FALSE		0
#endif

extern	void	*newmem(size_t);
extern	void	*resizemem(void *, size_t);
extern	void	oldmem(void *);

#endif /* STD_H */
