/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: mem.h,v 1.7 1997-07-27 14:59:28 fjh Exp $
*/

/*
** This file provides memory allocation functions for use by the bytecode
** utilities.
*/

#ifndef MB_MEMALLOC_H
#define	MB_MEMALLOC_H

#include	<stdlib.h>	/* for size_t */

/*
** Do not use MB_malloc() or MB_realloc() directly, unless you want
** to allocate raw memory.  Normally you should use the macros
** MB_new(), MB_new_array(), and MB_resize_array() instead.
*/

void*
MB_malloc(size_t size);

void*
MB_realloc(void *mem, size_t size);

void
MB_free(void *mem);

#define MB_new(type)		((type *) MB_malloc(sizeof(type)))
#define MB_new_array(type, num)	((type *) MB_malloc((num) * sizeof(type)))
#define MB_resize_array(array, type, num) \
	((type *) MB_realloc((array), (num) * sizeof(type)))

#endif	/* MB_MEMALLOC_H */
