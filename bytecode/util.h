/*
 *	$Id: util.h,v 1.4 1997-02-01 09:22:13 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

#if	! defined(UTIL_H)
#define	UTIL_H

typedef int
	Bool;

/* 
 * XXX: For some bizzare reason TRUE and FALSE are often defined by the C
 * libraries! Are they defined in pure POSIX or pure ANSI?
 */
#if	! defined(TRUE)
#define	TRUE	1
#endif	/* ! TRUE */

#if	! defined(FALSE)
#define	FALSE	0
#endif	/* ! FALSE */

#define	INT_SIZE	(sizeof(int))
#define	FLOAT_SIZE	(sizeof(float))
#define	DOUBLE_SIZE	(sizeof(double))

/*
 *	For debugging. E.g. XXXdebug("Bad integer value", "%d", value).
 *	XXX: We should implement some smarter tracing stuff that allows
 *	us to select a specific module or procedure to trace, or even
 *	a specific trace statement.
 */
#if	defined(DEBUGGING)
#define	XXXdebug(msg, fmt, val) \
	{ fprintf(stderr, "%s: %s = %" #fmt "\n", msg, #val, val); }
#else
#define	XXXdebug(msg, fmt, val)	{}
#endif	/* DEBUGGING */

void
util_init(void);

void
util_error(char* message);

void
fatal(char* message);

char*
strdup(char *str);


#endif	/* UTIL_H */
