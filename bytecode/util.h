/*
 *	$Id: util.h,v 1.1 1997-01-24 07:12:27 aet Exp $
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
 */
#define	XXXdebug(msg, fmt, val) \
	{ fprintf(stderr, "%s: %s = " #fmt "\n", msg, #val, val); }

void
util_init(void);

void
util_error(char* message);

void
util_fatal_error(char* message);

#endif	/* UTIL_H */
