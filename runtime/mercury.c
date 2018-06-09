// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1999-2003, 2006, 2011 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury.c - This file defines the builtin functions, constants, etc. that
// are used when generating high-level C code.
// (For the low-level C code, see mercury_imp.h.)

#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif

#include "mercury.h"
#include "mercury_type_info.h"      // for MR_TYPECTOR_REP*
#include "mercury_type_desc.h"      // for MR_TypeCtorDesc
#include "mercury_misc.h"           // for MR_fatal_error()
#include "mercury_heap.h"           // for MR_create[1-3]() prototypes
#include "mercury_builtin_types.h"

#ifdef MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
// Variable definitions.

#ifdef MR_NATIVE_GC
  void *mercury__private_builtin__stack_chain;
#endif

MR_Word mercury__private_builtin__dummy_var;

////////////////////////////////////////////////////////////////////////////

// Provide definitions for functions declared `extern inline'.
// Note that this code duplicates the code in mercury.h/mercury_heap.h.

MR_OUTLINE_DEFN(
    MR_Word
    MR_create1_func(MR_Word w1)
,
    {
        MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 1 * sizeof(MR_Word),
            NULL, "create1");
        p[0] = w1;
        return (MR_Word) p;
    }
)

MR_OUTLINE_DEFN(
    MR_Word
    MR_create2_func(MR_Word w1, MR_Word w2)
,
    {
        MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 2 * sizeof(MR_Word),
            NULL, "create2");
        p[0] = w1;
        p[1] = w2;
        return (MR_Word) p;
    }
)

MR_OUTLINE_DEFN(
    MR_Word
    MR_create3_func(MR_Word w1, MR_Word w2, MR_Word w3)
,
    {
        MR_Word *p = (MR_Word *) MR_new_object(MR_Word, 3 * sizeof(MR_Word),
            NULL, "create3");
        p[0] = w1;
        p[1] = w2;
        p[2] = w3;
        return (MR_Word) p;
    }
)

#if defined(MR_BOXED_FLOAT) && !defined(MR_GNUC)

MR_OUTLINE_DEFN(
    MR_Box
    MR_box_float(MR_Float f)
,
    {
        MR_Float *ptr;

        MR_make_hp_float_aligned();
        ptr = (MR_Float *) MR_new_object_atomic(MR_Float, sizeof(MR_Float),
            MR_ALLOC_SITE_FLOAT, NULL);
        *ptr = f;
        return (MR_Box) ptr;
    }
)

#endif // MR_BOXED_FLOAT && !MR_GNUC

#endif // ! MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
