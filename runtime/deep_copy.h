/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	DEEP_COPY_H
#define	DEEP_COPY_H

#include "imp.h"


	/* Deep Copy:
	 * 	Copy a data item, completely.
	 *	The copying is done depth first. Any part of the data
	 *	structure that is outside the given upper and lower
	 *	bounds not be copied, instead a reference to the
	 *	original data will be used. For conservative gc grades,
	 *	the entire data structure will be copied, as there is no
	 *	heap.  
	 *	The caller must provide the type_info describing
	 *	the type of this data structure. It must also
	 *	provide the heap_limit - if no limit is desired,
	 *	NULL or the bottom of the heap may be passed.
	 *	deep_copy returns the address of the new, copied
	 *	data structure.
	 *	Deep copy returns the actual data that it copied,
	 *	which may need to be stored on the heap, or put in
	 *	a register or stack slot (depending on what you
	 *	are using deep copy for). This may be a tagged pointer,
	 *	or if the data is just a simple type like a constant
	 *	or integer, it will be the constant or integer itself.
	 */

Word deep_copy(Word data, Word *type_info, Word *lower_limit, 
	Word *upper_limit);

#endif
