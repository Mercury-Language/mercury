/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_string.c - string handling */

#include "mercury_imp.h"
#include "mercury_string.h"

#if defined(MR_HAVE__VSNPRINTF) && ! defined(MR_HAVE_VSNPRINTF)
  #define vsnprintf	_vsnprintf
#endif

#if defined(MR_HAVE_VSNPRINTF) || defined(MR_HAVE__VSNPRINTF)
  #define MR_HAVE_A_VSNPRINTF
#endif

#define BUFFER_SIZE	4096

MR_String
MR_make_string(MR_Code *proclabel, const char *fmt, ...) {
	va_list		ap;
	MR_String	result;
	int 		n;
	char		*p;

#ifdef MR_HAVE_A_VSNPRINTF
	int 		size = BUFFER_SIZE;
	char		fixed[BUFFER_SIZE];
	MR_bool		dynamically_allocated = MR_FALSE;
	
	/*
	** On the first iteration we try with a fixed-size buffer.
	** If that didn't work, use a dynamically allocated array twice
	** the size of the fixed array and keep growing the array until
	** the string fits.
	*/
	p = fixed;

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
		if (n > -1) {   /* glibc 2.1 */
			size = n + 1; /* precisely what is needed */
		} else {        /* glibc 2.0 */
			size *= 2;  /* twice the old size */
		}

		if (!dynamically_allocated) {
			p = MR_NEW_ARRAY(char, size);
			dynamically_allocated = MR_TRUE;
		} else {
			p = MR_RESIZE_ARRAY(p, char, size);
		}
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
	MR_restore_transient_hp();      
	MR_allocate_aligned_string_msg(result, strlen(p), proclabel);
	MR_save_transient_hp();
	strcpy(result, p);

#ifdef MR_HAVE_A_VSNPRINTF
	if (dynamically_allocated) {
		MR_free(p);
	}
#endif

	return result;
}
