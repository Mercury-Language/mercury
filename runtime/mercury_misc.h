/*
** Copyright (C) 1995-2000,2002, 2004, 2006 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_misc.h -	MR_warning(),
**			MR_fatal_error()
*/

#ifndef	MERCURY_MISC_H
#define	MERCURY_MISC_H

#include "mercury_std.h"	/* for `MR_NO_RETURN' */
#include <stdlib.h>		/* for `size_t' */

extern	void	MR_warning(const char *msg, ...);

	/* For warnings from the debugger */
extern	void	MR_mdb_warning(const char *msg, ...);

extern	void	MR_perror(const char *msg);

	/* For errors from the debugger */
extern	void	MR_mdb_perror(const char *msg);

extern	void	MR_fatal_error(const char *msg, ...) MR_NO_RETURN;

/*
** Register a function to be called (as func(data)) when the program is
** about to be terminated due to an uncaught exception. 
*/

extern	void	MR_register_exception_cleanup(void (*func)(void *),
			void *data);

/*
** Call all the functions registered with MR_register_exception_cleanup.
** Should be invoked only when the program is about to be terminated
** due to an uncaught exception. 
*/

extern	void	MR_perform_registered_exception_cleanups(void);

/*
** These macro are shorthands to allow reductions in the size of compiler
** generated C source files.
*/

#define	MR_COMMON_TYPE(typenum)					\
	MR_PASTE2(mercury_type_, typenum)

#define	MR_COMMON_NAME(cellnum)					\
	MR_PASTE2(mercury_common_, cellnum)

#define	MR_COMMON(cellnum)					\
	((MR_Word *) &MR_COMMON_NAME(cellnum))

#define	MR_XCOMMON(typenum, cellnum)				\
	((MR_Word *) &MR_COMMON_NAME(typenum)[cellnum])

#define	MR_TAG_COMMON(tag, cellnum)				\
	(MR_mkword(MR_mktag(tag), MR_COMMON(cellnum)))

#define	MR_TAG_XCOMMON(tag, typenum, cellnum)			\
	(MR_mkword(MR_mktag(tag), MR_XCOMMON(typenum, cellnum)))

#define	MR_DEF_COMMON1(typenum,c1)				\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1);
#define	MR_DEF_COMMON2(typenum,c1,c2)				\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2);
#define	MR_DEF_COMMON3(typenum,c1,c2,c3)			\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3);
#define	MR_DEF_COMMON4(typenum,c1,c2,c3,c4)			\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4);
#define	MR_DEF_COMMON5(typenum,c1,c2,c3,c4,c5)			\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5);
#define	MR_DEF_COMMON6(typenum,c1,c2,c3,c4,c5,c6)		\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5),				\
		MR_COMMON_NAME(c6);
#define	MR_DEF_COMMON7(typenum,c1,c2,c3,c4,c5,c6,c7)		\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5),				\
		MR_COMMON_NAME(c6),				\
		MR_COMMON_NAME(c7);
#define	MR_DEF_COMMON8(typenum,c1,c2,c3,c4,c5,c6,c7,c8)		\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5),				\
		MR_COMMON_NAME(c6),				\
		MR_COMMON_NAME(c7),				\
		MR_COMMON_NAME(c8);
#define	MR_DEF_COMMON9(typenum,c1,c2,c3,c4,c5,c6,c7,c8,c9)	\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5),				\
		MR_COMMON_NAME(c6),				\
		MR_COMMON_NAME(c7),				\
		MR_COMMON_NAME(c8),				\
		MR_COMMON_NAME(c9);
#define	MR_DEF_COMMON10(typenum,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)	\
	static const struct MR_COMMON_TYPE(typenum)		\
		MR_COMMON_NAME(c1),				\
		MR_COMMON_NAME(c2),				\
		MR_COMMON_NAME(c3),				\
		MR_COMMON_NAME(c4),				\
		MR_COMMON_NAME(c5),				\
		MR_COMMON_NAME(c6),				\
		MR_COMMON_NAME(c7),				\
		MR_COMMON_NAME(c8),				\
		MR_COMMON_NAME(c9),				\
		MR_COMMON_NAME(c10);

#endif /* not MERCURY_MISC_H */
