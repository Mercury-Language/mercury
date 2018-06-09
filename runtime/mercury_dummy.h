// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-1995, 1997, 1999-2000 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// File: mercury_dummy.h
// Author: fjh
//
// Global variables and functions used purely for the purpose
// of suppressing over-zealous compiler optimizations.

#ifndef MERCURY_DUMMY_H
#define MERCURY_DUMMY_H

extern  void    MR_dummy_function_call(void);
extern  void    *MR_dummy_identify_function(void *);
extern  void    *volatile MR_global_pointer;
extern  void    *volatile MR_global_pointer_2;

#endif // not MERCURY_DUMMY_H
