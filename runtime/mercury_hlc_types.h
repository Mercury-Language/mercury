// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2007 The University of Melbourne.
// Copyright (C) 2016-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_hlc_types.h -
// Definitions of types needed by the high level C back end.

#ifndef MERCURY_HLC_TYPES_H
#define MERCURY_HLC_TYPES_H

#ifdef  MR_HIGHLEVEL_CODE

#include "mercury_types.h"
#include "mercury_std.h"    // for MR_CALL

// The continuation function types used for implementing
// nondeterministic procedures.

typedef void MR_CALL (*MR_Cont) (void *);

// The types uses to represent the Mercury builtin types,
// MR_Char, MR_Float, MR_Integer, MR_String, and MR_ConstString,
// are defined in mercury_types.h and mercury_float.h.

// The MR_Word type, which is used for representing user-defined
// types when we are using the low-level data representation,
// is defined in runtime/mercury_types.h.

// The MR_Box type, which is used for representing polymorphic
// types, is defined in runtime/mercury_types.h.

// Define some names for types whose definitions used to differ
// depending on whether --high-level-data was enabled.
// These types all correspond to Mercury data types.
// Some of the have `Mercury_' in their name, to distinguish
// them from the corresponding C data type.

typedef MR_Word MR_Comparison_Result;
typedef MR_Word MR_Void;
typedef MR_Word MR_C_Pointer;
typedef MR_Word MR_Heap_Pointer;
typedef MR_Word MR_Reference;
typedef MR_Word MR_Pred;
typedef MR_Word MR_Func;
typedef MR_ArrayPtr MR_Array;
typedef MR_Word MR_Univ;
typedef MR_Word MR_Type_Desc;
typedef MR_Word MR_Pseudo_Type_Desc;
typedef MR_Word MR_Type_Ctor_Desc;
typedef MR_Word MR_Mercury_Type_Info;
typedef MR_Word MR_Mercury_Type_Ctor_Info;
typedef MR_Word MR_Mercury_TypeClass_Info;
typedef MR_Word MR_Mercury_Base_TypeClass_Info;

#endif  // MR_HIGHLEVEL_CODE

#endif  // MERCURY_HLC_TYPES_H
