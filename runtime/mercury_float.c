/*
** Copyright (C) 1997, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_imp.h"

/*
** The function `hash_float()' is used by the library predicate `float__hash'
** and also for hashing floats for `pragma fact_table' indexing.
** It computes a non-negative MR_Integer hash value for a MR_Float.
** The exact hash function used depend on the relative sizes of MR_Float and
** MR_Integer.
*/

union MR_Float_Integer {
	MR_Float   f;
	MR_Integer i;
	MR_Integer j[(sizeof(MR_Float)/sizeof(MR_Integer) > 0 
			? sizeof(MR_Float)/sizeof(MR_Integer) : 1)];
	char    c[sizeof(MR_Float)/sizeof(char)];
};

MR_Integer
hash_float(MR_Float f)
{
	union MR_Float_Integer fi;
	size_t i;
	MR_Integer h = 0;

	fi.i = 0;
	fi.f = f;

	if (sizeof(MR_Float) <= sizeof(MR_Integer)) {
		h = fi.i;
	} else if (sizeof(MR_Float) % sizeof(MR_Integer) == 0) {
		for (i = 0; i < sizeof(MR_Float)/sizeof(MR_Integer); i++) {
			h ^= fi.j[i];
		}
	} else {
		for (i = 0; i < sizeof(MR_Float)/sizeof(char); i++) {
			h ^= (h << 5);
			h ^= fi.c[i];
		}
	}
	return (h >= 0 ? h : -h);
}
