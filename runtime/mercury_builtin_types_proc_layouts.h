// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2004-2005, 2007 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_builtin_types_proc_layouts.h
//
// Declares the proc layout structures of the unify, compare and
// compare_representation procedures of the builtin types.
//
// You should not include this header file in any source file in the trace
// or browser directories, or in any file included from there. The reason
// is that this file contains extern declarations for the global variables
// holding the procedure layout structure of the unify, compare and
// compare_representation procedures of the builtin types, and the C compiler
// handles such declarations as Fortran style "common blocks", which it can
// use as *definitions* of the symbols rather than simply their uses if
// the program contains no unequivocal definitions of those symbols at all.
// If one of these symbols is undefined when the linker considers whether the
// trace or browser library should be linked in, and those libraries contain
// a common block reference to these symbols, the linker will pull in those
// libraries, even though the actual definition of those symbols is in the
// runtime library (in mercury_builtin_types.o) which comes later on the link
// command line. Pulling in the trace and/or browser libraries will then
// cause a link failure, due to the absence from the link command line of
// system libraries required by the debugger, such as the dynamic link
// library -ldl. We could in theory fix the link error by *always* linking
// in all the libraries required by the debugger, but that would cause
// significant unnecessary bloat in executable sizes.

#ifndef MERCURY_BUILTIN_TYPES_PROC_LAYOUTS_H
#define MERCURY_BUILTIN_TYPES_PROC_LAYOUTS_H

#include "mercury_stack_layout.h"

#ifdef  MR_DEEP_PROFILING

MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, int, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, uint, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, int8, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, uint8, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, int16, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, uint16, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, int32, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, uint32, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, string, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, float, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, character, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, void, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, c_pointer, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, pred, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, func, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, tuple, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, succip, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, hp, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, curfr, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, maxfr, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, redofr, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, redoip, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, trailptr, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, ticket, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, heap_pointer, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, ref, 1);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, type_ctor_info, 1);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, type_info, 1);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, base_typeclass_info, 1);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(private_builtin, typeclass_info, 1);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(type_desc, type_ctor_desc, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(type_desc, pseudo_type_desc, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(type_desc, type_desc, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, user_by_rtti, 0);
MR_DECLARE_UCI_PROC_STATIC_LAYOUTS(builtin, dummy, 0);

#endif // MR_DEEP_PROFILING

#endif  // MERCURY_BUILTIN_TYPES_PROC_LAYOUTS_H
