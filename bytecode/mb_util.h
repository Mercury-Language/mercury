
/*
** Copyright (C) 1997,2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

#ifndef MB_UTIL_H
#define MB_UTIL_H

#include "mb_basetypes.h"

typedef char *
	MB_CString;

typedef const char *
	MB_CString_Const;

#define MB_NULL_STR	((MB_CString) NULL)

/* Standard TRUE & FALSE macros, if not defined */
#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

/* Prints an error (doesn't exit) */
void		MB_util_error(const char *fmt, ...);

/* Debugging printf */
void		MB_SAY(const char *fmt, ...);

/* Prints an error message and exits */
void		MB_fatal(const char *message);

/* allocate space for a new string */
MB_CString	MB_str_new(MB_Word len);    /* len is without null terminator */

/* return a new string created from two strings concatenated together */
MB_CString	MB_str_new_cat(MB_CString_Const a, MB_CString_Const b);

/* free the memory allocated for a string */
void		MB_str_delete(MB_CString str);

/* duplicate a null terminated string */
MB_CString	MB_str_dup(MB_CString_Const str);

/* compare two strings (returns zero for equality) */
int		MB_str_cmp(MB_CString_Const a, MB_CString_Const b);

/* deallocate space for string */
void		MB_str_delete(MB_CString str);

/*
** Given an arbitrary blocksize, returns how many blocks are required to 
** contain something of size x
*/
#define MB_NUMBLOCKS(x, blocksize) \
	(((x) + (blocksize) - 1) / (blocksize))

#define MB_MULTIPLEOF(x, blocksize) \
	(MB_NUMBLOCKS((x), (blocksize))*(blocksize))

#endif	/* MB_UTIL_H */

