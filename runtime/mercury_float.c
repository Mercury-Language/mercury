/*
** Copyright (C) 1997, 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"mercury_imp.h"
#include	<math.h>

/*
** The function `MR_hash_float()' is used by the library predicate
** `float__hash' and also for hashing floats for `pragma fact_table' indexing.
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
MR_hash_float(MR_Float f)
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

/*
** MR_sprintf_float(buf, f)
**
** fills buff with the string representation of the float, f, such that
** the string representation has enough precision to represent the
** float, f.
**
** Note that buf must have size at least ML_SPRINTF_FLOAT_BUF_SIZE.
*/
void
MR_sprintf_float(char *buf, MR_Float f)
{
	MR_Float round = 0.0;
	int 	 i = MR_FLT_MIN_PRECISION;

	/*
	** Print the float at increasing precisions until the float
	** is round-trippable.
	*/
	do {
		sprintf(buf, "%#.*g", i, f);
		if (i >= MR_FLT_MAX_PRECISION) {
			/*
			** This should be sufficient precision to
			** round-trip any value.  Don't bother checking
			** whether it can actually be round-tripped,
			** since if it can't, this is a bug in the C
			** implementation.
			*/
			break;
		}
		sscanf(buf, MR_FLT_FMT, &round);
		i++;
	} while (round != f);

    return;
}

MR_bool
MR_is_nan(MR_Float Flt)
{
#if defined(MR_USE_SINGLE_PREC_FLOAT) && defined(MR_HAVE_ISNANF)
	return isnanf(Flt);
#elif defined(MR_HAVE_ISNAN)
	return isnan(Flt);
#else
	return (Flt != Flt);
#endif
}

MR_bool
MR_is_inf(MR_Float Flt)
{
#if defined(MR_USE_SINGLE_PREC_FLOAT) && defined(MR_HAVE_ISINFF)
	return isinff(Flt);
#elif defined(MR_HAVE_ISINF)
	return isinf(Flt);
#else
	return (Flt == Flt / 2.0 && Flt != 0.0);
#endif
}
