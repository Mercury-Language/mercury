/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_imp.h"

/*
** The function `hash_float()' is used by the library predicate `float__hash'
** and also for hashing floats for `pragma fact_table' indexing.
** It computes a non-negative Integer hash value for a Float.
** The exact hash function used depend on the relative sizes of Float and
** Integer.
*/

union FloatInteger {
	Float   f;
	Integer i;
	Integer j[(sizeof(Float)/sizeof(Integer) > 0 
			? sizeof(Float)/sizeof(Integer) : 1)];
	char    c[sizeof(Float)/sizeof(char)];
};

Integer
hash_float(Float f)
{
	union FloatInteger fi;
	size_t i;
	Integer h = 0;

	fi.i = 0;
	fi.f = f;

	if (sizeof(Float) <= sizeof(Integer)) {
		h = fi.i;
	} else if (sizeof(Float) % sizeof(Integer) == 0) {
		for (i = 0; i < sizeof(Float)/sizeof(Integer); i++) {
			h ^= fi.j[i];
		}
	} else {
		for (i = 0; i < sizeof(Float)/sizeof(char); i++) {
			h ^= (h << 5);
			h ^= fi.c[i];
		}
	}
	return (h >= 0 ? h : -h);
}
