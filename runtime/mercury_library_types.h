/*
** Copyright (C) 1998-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_library_types.h - definitions of some basic types used by the
** Mercury library.
*/

#ifndef MERCURY_LIBRARY_TYPES_H
#define MERCURY_LIBRARY_TYPES_H

#include <stdio.h>		/* for `FILE' */
#include "mercury_types.h"	/* for `MR_Word' and `MR_Integer' */
#include "mercury_std.h"	/* for MR_VARIABLE_SIZED */

/*
** The C `MercuryFile' type is used for the Mercury `io__stream' type
** in library/io.m.
** Mercury files are not quite the same as C stdio FILEs,
** because we keep track of a little bit more information.
*/

typedef struct mercury_file {
	FILE *file;
	int line_number;
} MercuryFile;

/*
** definitions for accessing the representation of the
** Mercury `array' type
*/

typedef struct {
	MR_Integer size;
	MR_Word elements[MR_VARIABLE_SIZED];
} MR_ArrayType;

#define MR_make_array(sz) ((MR_ArrayType *) MR_GC_NEW_ARRAY(MR_Word, (sz) + 1))

#endif /* not MERCURY_LIBRARY_TYPES_H */
