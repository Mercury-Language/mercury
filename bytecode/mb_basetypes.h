
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** This file contains the basic type definitions
*/

#ifndef MB_BASETYPES_H
#define	MB_BASETYPES_H

#include <stdio.h>

#include "mercury_conf.h"
#include "mercury_tags.h"
#include "mercury_types.h"
#include "mercury_float.h"

typedef unsigned char
	MB_Byte;

typedef MR_INT_LEAST16_TYPE
	MB_Short;

typedef MR_Word
	MB_Word;

typedef MR_Unsigned
	MB_Unsigned;

#define MB_WORD_BITS	MR_WORDBITS

typedef MR_Integer
	MB_Integer;

typedef MR_Float
	MB_Float;

typedef MR_Float64
	MB_Float64;

typedef MB_Byte
	MB_Bool;

/*
** These shouldn't really be in here (they're not really basic types)
** but putting them here stops circular header dependencies that otherwise
** cause problems.
*/
	
/*
** Native code instruction pointer
** Native_Addr_Struct doesn't actually exist; it is only
** used to force type checking
*/
struct MB_Native_Addr_Struct;
typedef struct MB_Native_Addr_Struct*
	MB_Native_Addr;

/* Bytecode instruction pointer */
struct MB_BCId_Struct;
typedef struct MB_BCId_Struct MB_BCId;
typedef MB_BCId*
	MB_Bytecode_Addr;

#endif	/* MB_BASETYPES_H */

