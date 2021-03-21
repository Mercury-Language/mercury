// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2021 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_report_stats.h - functions for reporting statistics
// about the execution of the current program.

#ifndef MERCURY_REPORT_STATS_H
#define MERCURY_REPORT_STATS_H

#include <stdio.h>
#include "mercury_timing.h"
#include "mercury_heap.h"

extern void MR_report_standard_stats(FILE *fp);

extern void MR_report_full_memory_stats(FILE *fp);

#endif  // not MERCURY_REPORT_STATS_H
