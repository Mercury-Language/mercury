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

#define SIZE	4096

MR_String
MR_make_string(MR_Code *proclabel, const char *fmt, ...) {
	va_list		ap;
	MR_String	result;
	int 		n;
	char		*p;

#if defined(HAVE_VSNPRINTF) || defined(HAVE__VSNPRINTF)
	int 		size = 2 * SIZE;
	char		fixed[SIZE];
	int		dynamically_allocated = FALSE;
	
	va_start(ap, fmt);
	n = vsnprintf(fixed, SIZE, fmt, ap);
	va_end(ap);

	/*
	** If that didn't work, use a dynamically allocated array twice
	** the size of the fixed array and keep growing the array until
	** the string fits.
	*/
	if (!(n > -1 && n < size)) {
		p = MR_NEW_ARRAY(char, size);

		while (1) {
			dynamically_allocated = TRUE;

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
	} else {
		p = fixed;
	}

#else
		/* 
		** It is possible for this buffer to overflow and
		** then bad things may happen
		*/
	char fixed[40960];

	va_start(ap, fmt);
	n = vsprintf(fixed, fmt, ap);
	va_end(ap);

	p = fixed;
#endif
	      
	MR_allocate_aligned_string_msg(result, strlen(p),
			proclabel);
	strcpy(result, p);

#ifdef HAVE_VSNPRINTF
	if (dynamically_allocated) {
		MR_free(p);
	}
#endif

	return result;
}
