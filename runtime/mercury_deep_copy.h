/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* deepcopy.h - declares the deep_copy() function. */

#ifndef	MERCURY_DEEP_COPY_H
#define	MERCURY_DEEP_COPY_H

#include "mercury_types.h"	/* for `Word' */

/* Deep Copy:
**
** 	Copy a data item, completely.
**
**	The copying is done depth first. Any part of the data
**	structure that is outside the given upper and lower
**	bounds not be copied, instead a reference to the
**	original data will be used. For conservative gc grades,
**	the entire data structure will be copied, as there is no
**	heap.  
**
**	The caller must provide the type_info describing
**	the type of this data structure. It must also
**	provide the heap_limit - if no limit is desired,
**	NULL or the bottom of the heap may be passed.
**	deep_copy returns the address of the new, copied
**	data structure.
**
**	Deep copy returns the actual data that it copied,
**	which may need to be stored on the heap, or put in
**	a register or stack slot (depending on what you
**	are using deep copy for). This may be a tagged pointer,
**	or if the data is just a simple type like a constant
**	or integer, it will be the constant or integer itself.
**
** 	Please note - deep_copy increments the heap pointer, 
** 	however on some platforms (notably, SPARCs) the 
** 	register-windows mean the transient Mercury registers
** 	may be lost. So before calling deep_copy, call
** 		save_transient_registers();
**
**	deep_copy will use restore_transient_registers()
**	to restore the registers and modify the heap pointer, and
**	then call save_transient_registers() to save them again.
**	(This behaviour may change in future - it may be more
**	efficient to just change the saved register - so do not rely on 
**	it).
**
**	After calling deep_copy, be sure to do a 
**		restore_transient_registers();
**	so that the registers are restored.
**
**	If writing a C function that calls deep_copy, make sure
**	you document that around your function,
**	save_transient_registers()/restore_transient_registers()
**	need to be used.
**
*/

Word deep_copy(Word data, Word *type_info, Word *lower_limit, 
	Word *upper_limit);

#endif /* not MERCURY_DEEP_COPY_H */
