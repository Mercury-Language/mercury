// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997, 2000, 2002, 2006-2007 The University of Melbourne.
// Copyright (C) 2013-2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include    "mercury_imp.h"
#include    <math.h>

#if defined(MR_MSVC)
  #include <float.h>  // For _FPCLASS_* etc.
#endif

// The function `MR_hash_float()' is used by the library predicate
// `float__hash' and also for hashing floats for `pragma fact_table' indexing.
// It computes a non-negative MR_Integer hash value for a MR_Float.
// The exact hash function used depend on the relative sizes of MR_Float and
// MR_Integer.

union MR_Float_Integer {
    MR_Float    f;
    MR_Integer  i;
    MR_Integer  j[(sizeof(MR_Float)/sizeof(MR_Integer) > 0
                    ? sizeof(MR_Float)/sizeof(MR_Integer) : 1)];
    char        c[sizeof(MR_Float)/sizeof(char)];
};

MR_Integer
MR_hash_float(MR_Float f)
{
    union MR_Float_Integer  fi;
    size_t                  i;
    MR_Integer              h = 0;

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

// MR_sprintf_float(buf, f)
//
// Fills buf with the string representation of the float, f, such that
// the string representation has enough precision to represent the
// float, f.
//
// Note that buf must have size at least ML_SPRINTF_FLOAT_BUF_SIZE.

void
MR_sprintf_float(char *buf, MR_Float f)
{
    MR_Float round_trip = 0.0;
    int      i = MR_FLT_MIN_PRECISION;

    if (MR_is_nan(f)) {
        strcpy(buf, "nan");
        return;
    }

    if (MR_is_infinite(f)) {
        if (f < 0) {
            strcpy(buf, "-infinity");
        } else {
            strcpy(buf, "infinity");
        }
        return;
    }

    // Print the float at increasing precisions until the float
    // is round-trippable.

    do {
        sprintf(buf, "%.*g", i, f);
        if (i >= MR_FLT_MAX_PRECISION) {
            // This should be sufficient precision to round-trip any value.
            // Don't bother checking whether it can actually be round-tripped,
            // since if it can't, this is a bug in the C implementation.

            break;
        }
        sscanf(buf, MR_FLT_FMT, &round_trip);
        i++;
    } while (round_trip != f);

    // Append ".0" if there is no "e" or "." in the string.

    while (1) {
        if (*buf == 'e' || *buf == '.') {
            return;
        }
        if (*buf == '\0') {
            // We only get here if there is no '.' or 'e' in the string.
            strcpy(buf, ".0");
            return;
        }
        buf++;
    }

    return;
}

MR_bool
MR_is_nan_func(MR_Float Flt)
{
#if defined(MR_USE_SINGLE_PREC_FLOAT) && defined(MR_HAVE_ISNANF)
    return isnanf(Flt);
#elif defined(MR_HAVE_ISNAN)
    return isnan(Flt);
#elif defined(MR_MSVC)
    return _isnan(Flt);
#else
    return (Flt != Flt);
#endif
}

MR_bool
MR_is_infinite_func(MR_Float Flt)
{
    // On Solaris, isinf() is detected by configure but we pass -fno-builtin
    // for global registers on x86/x86-64, and that causes an undefined
    // reference to `isinf' when linking. finite() works though.

#if defined(MR_USE_SINGLE_PREC_FLOAT) && defined(MR_HAVE_ISINFF) && !defined(MR_SOLARIS)
    return isinff(Flt);
#elif defined(MR_HAVE_ISINF) && !defined(MR_SOLARIS)
    return isinf(Flt);
#elif defined(MR_HAVE_FINITE)
    return !finite(Flt) && (Flt == Flt);
#elif defined(MR_MSVC)
    int sw;
    sw = _fpclass(Flt);
    return (sw == _FPCLASS_NINF) || (sw == _FPCLASS_PINF);
#else
    return (Flt == Flt / 2.0 && Flt != 0.0);
#endif
}
