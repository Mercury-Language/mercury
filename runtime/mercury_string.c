/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_string.c - string handling */

#include "mercury_imp.h"
#include "mercury_string.h"

#if defined(HAVE__VSNPRINTF) && ! defined(HAVE_VSNPRINTF)
  #define vsnprintf	_vsnprintf
#endif

MR_String
MR_make_string(MR_Code *proclabel, const char *fmt, ...) {
	va_list		ap;
	MR_String	result;
	int 		n;

#if defined(HAVE_VSNPRINTF) || defined(HAVE__VSNPRINTF)
	/* Guess that 100 bytes should be sufficient */
	int 		size = 100;
	char		*p;
	
	p = MR_NEW_ARRAY(char, size);

	while (1) {
		/* Try to print in the allocated space. */
		va_start(ap, fmt);
		n = vsnprintf(p, size, fmt, ap);
		va_end(ap);

		/* If that worked, return the string.  */
		if (n > -1 && n < size) {
			break;
		}

		/* Else try again with more space.  */
		if (n > -1) {	/* glibc 2.1 */
			size = n + 1; /* precisely what is needed */
		} else {	/* glibc 2.0 */
			size *= 2;  /* twice the old size */
		}

		MR_RESIZE_ARRAY(p, char, size);
	}
#else
		/* 
		** It is possible for this buffer to overflow and
		** then bad things may happen
		*/
	char p[40960];

	va_start(ap, fmt);
	n = vsprintf(p, fmt, ap);
	va_end(ap);
#endif
	      
	MR_allocate_aligned_string_msg(result, strlen(p),
			proclabel);
	strcpy(result, p);

#ifdef HAVE_VSNPRINTF
	MR_free(p);
#endif

	return result;
}
