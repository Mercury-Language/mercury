// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1995, 1997, 1999-2000, 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#include "mercury_dummy.h"
#include "mercury_imp.h"    // we need libmer_dll.h for Windows DLLs

// This dummy function is in a file of its own to ensure that
// gcc can't inline it. Similarly for the two pointers.

void    *volatile MR_global_pointer;
void    *volatile MR_global_pointer_2;

void
MR_dummy_function_call(void)
{
    return;
}

void *
MR_dummy_identify_function(void *p)
{
    return p;
}
