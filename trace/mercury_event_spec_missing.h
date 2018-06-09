// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2006 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// This file contains the declarations that flex and bison should put,
// or let us programmers put, into the header files they create, but don't.
//
// Main author: Zoltan Somogyi.

#ifndef MERCURY_TRACE_EVENT_MISSING_H
#define MERCURY_TRACE_EVENT_MISSING_H

#include <stdio.h>
#include "mercury_event_spec.h"

extern  char            *mercury_event_text;
extern  const char      *mercury_event_filename;
extern  int             mercury_event_linenum;

extern  MR_EventSet     mercury_event_parsetree;
extern  int             mercury_event_parse(void);

#endif  // not MERCURY_TRACE_EVENT_MISSING_H
