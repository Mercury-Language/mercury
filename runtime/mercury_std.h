/*
** Copyright (C) 1993-1995, 1997-1999 The University of Melbourne.
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

#define	MR_isupper(c)		isupper((unsigned char) (c))
#define	MR_islower(c)		islower((unsigned char) (c))
#define	MR_isalpha(c)		isalpha((unsigned char) (c))
#define	MR_isalnum(c)		isalnum((unsigned char) (c))
#define	MR_isdigit(c)		isdigit((unsigned char) (c))
#define	MR_isspace(c)		isspace((unsigned char) (c))
#define	MR_isalnumunder(c)	(isalnum((unsigned char) (c)) || c == '_')

#define streq(s1, s2)		(strcmp(s1, s2) == 0)
#define strdiff(s1, s2)		(strcmp(s1, s2) != 0)
#define strtest(s1, s2)		(strcmp(s1, s2))
#define strneq(s1, s2, n)	(strncmp(s1, s2, n) == 0)
#define strndiff(s1, s2, n)	(strncmp(s1, s2, n) != 0)
#define strntest(s1, s2, n)	(strncmp(s1, s2, n))

#define	ungetchar(c)		ungetc(c, stdin)

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

/*---------------------------------------------------------------------------*/

/*
** MR_VARIABLE_SIZED -- what to put between the []s when declaring
**			a variable length array at the end of a struct.
**
** The preferred values, if the compiler understands them, convey to the
** implementation that the array has a variable length. The default value
** is the maximum length of the variable-length arrays that we construct,
** since giving too small a value may lead the compiler to use inappropriate
** optimizations (e.g. using small offsets to index into the array).
** At the moment, we use variable length arrays that are indexed by
** closure argument numbers or by type parameter numbers. We therefore
** use a default MR_VARIABLE_SIZED value that is at least as big as
** both MAX_VIRTUAL_REG and TYPE_CTOR_LAYOUT_MAX_VARINT.
*/

#if __STDC_VERSION__ >= 199901	/* January 1999 */
  /* Use C9X-style variable-length arrays. */
  #define	MR_VARIABLE_SIZED	/* nothing */
#elif defined(__GNUC__)
  /* Use GNU-style variable-length arrays */
  #define	MR_VARIABLE_SIZED	0
#else
  /* Just fake it by pretending that the array has a fixed size */
  #define	MR_VARIABLE_SIZED	1024
#endif

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_STD_H */
