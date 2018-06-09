// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1998, 2000, 2005 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

#ifndef MERCURY_AGC_DEBUG_H
#define MERCURY_AGC_DEBUG_H

// mercury_agc_debug.h:
//
// Support for debugging accurate garbage collection.

#include "mercury_regs.h"           // needs to come first
#include "mercury_types.h"          // for MR_Word
#include "mercury_label.h"          // for MR_Internal
#include "mercury_memory_zones.h"   // for MR_MemoryZone
#include "mercury_accurate_gc.h"    // for MR_RootList

// MR_agc_dump_stack_frames:
//
// Dump the det stack, writing all information available about
// each stack frame. Label is the topmost label on the stack,
// heap_zone is the zone which the data is stored upon.

extern  void    MR_agc_dump_stack_frames(MR_Internal *label,
                    MR_MemoryZone *heap_zone,
                    MR_Word *stack_pointer, MR_Word *current_frame);

// MR_agc_dump_nondet_stack_frames:
//
// Dump the nondet stack, writing all information available about
// each stack frame. Label is the topmost label on the stack,
// heap_zone is the zone which the data is stored upon.

extern  void    MR_agc_dump_nondet_stack_frames(MR_Internal *label,
                    MR_MemoryZone *heap_zone, MR_Word *stack_pointer,
                    MR_Word *current_frame, MR_Word *max_frame);

// MR_agc_dump_roots:
//
// Dump the extra rootset, writing all information about each root.

extern  void    MR_agc_dump_roots(MR_RootList roots);

#endif // not MERCURY_AGC_DEBUG_H
