/*
**	Standard definitions for C
*/

#define	or		else if
#define	when		break;case
#define	otherwise	break;default
#define	loop		for(;;)
#define	until(expr)	while(!(expr))
#ifndef	reg
#define	reg		register
#endif
#ifndef	bool
#define	bool		char
#endif

#define	ulong		unsigned long
#define	uint		unsigned int
#define	ushort		unsigned short
#define	uchar		unsigned char

#define	IGNORE		(void)

#define	max(a, b)	((a) > (b) ? (a) : (b))
#define	min(a, b)	((a) < (b) ? (a) : (b))

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

#ifndef NOMALLOCCAST
#define malloc_type(t)		((t *) malloc((unsigned) sizeof(t)))
#define malloc_mtype(t, n)	((t *) malloc((unsigned) ((n) * sizeof(t))))
#define realloc_type(t, r, n) 	((t *) realloc((char *) (r), (unsigned) ((n) * sizeof(t))))
#else
#define malloc_type(t)		((t *) malloc(sizeof(t)))
#define malloc_mtype(t, n)	((t *) malloc((n) * sizeof(t)))
#define realloc_type(t, r, n) 	((t *) realloc((char *) (r), (n) * sizeof(t)))
#endif

#ifndef	TRUE
#define	TRUE		1
#endif
#ifndef	FALSE
#define	FALSE		0
#endif
#ifndef	NULL
#define	NULL		0
#endif

#undef	CTRL
#define	CTRL(c)		('c' & 037)
#define	BELL		'\007'
#define	ESC		'\033'
