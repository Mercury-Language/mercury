/*
 *	$Id: util.h,v 1.2 1997-01-28 02:02:10 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

#if	! defined(UTIL_H)
#define	UTIL_H

/* XXX: What naming convention for types? byte_t Byte Byte_t ? */
/* XXX: Note we assume the following:
 *	sizeof(byte) = 1
 *	sizeof(short) = 2
 *	sizeof(int) = 4
 *	sizeof(long) = 8
 *	sizeof(float) = 4
 *	sizeof(double) = 8
 */
typedef unsigned char
	Byte;
typedef	short
	Short;
typedef int
	Int;
typedef long
	Long;
typedef char*
	CString;
typedef int
	Bool;

/* For some bizzare reason TRUE and FALSE are often defined by the C
 * libraries!
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
	{ fprintf(stderr, "%s: %s = " #fmt "\n", msg, #val, val); }
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
