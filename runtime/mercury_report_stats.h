// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2021, 2024 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_report_stats.h - functions for reporting statistics
// about the execution of the current program.

#ifndef MERCURY_REPORT_STATS_H
#define MERCURY_REPORT_STATS_H

#include <stdio.h>
#include "mercury_timing.h"
#include "mercury_heap.h"

extern int MR_report_standard_stats(FILE *fp, int *line_number);

// Return a positive value if full memory statistics are available
// (i.e. if MR_report_full_memory_stats will print them).
// Return a zero value if full memory statistics are NOT available
// (i.e. if MR_report_full_memory_stats will print a message about them
// not being available).
//
extern int MR_full_memory_stats_available(void);

extern int MR_report_full_memory_stats(FILE *fp, int *line_number);

#endif  // not MERCURY_REPORT_STATS_H
