
/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mb_util.h,v 1.1 2001-01-24 07:42:28 lpcam Exp $
*/


#ifndef MB_UTIL_H
#define	MB_UTIL_H

typedef char *
	MB_CString;

typedef const char *
	MB_CString_Const;

#define MB_NULL_STR	((MB_CString)NULL)

/* Standard TRUE & FALSE macros, if not defined */
#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

/* Prints an error (doesn't exit) */
void
MB_util_error(const char *fmt, ...);

/* Prints an error message and exits */
void
MB_fatal(const char* message);

/* compare two strings */
int MB_strcmp(MB_CString_Const a, MB_CString_Const b);

#endif	/* MB_UTIL_H */

