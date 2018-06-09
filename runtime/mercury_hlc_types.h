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

// Define some names for types that differ depending
// on whether --high-level-data is enabled.
// These types all correspond to Mercury data types.
// Some of the have `Mercury_' in their name, to distinguish
// them from the corresponding C data type.
// E.g. `MR_Mercury_Type_Info' (below) is the abstract type that the
// Mercury compiler generates for a type_info argument, whereas
// `MR_TypeInfo' (defined in runtime/mercury_type_info.h) is the
// concrete C type that is used by the C code in the runtime.

#ifdef MR_HIGHLEVEL_DATA
  typedef MR_Integer // really `enum mercury__builtin__comparison_result_0'
    MR_Comparison_Result;
  typedef struct mercury__builtin__void_0_s * MR_Void;
  typedef struct mercury__builtin__c_pointer_0_s * MR_C_Pointer;
  typedef struct mercury__private_builtin__heap_pointer_0_s * MR_Heap_Pointer;
  typedef struct mercury__private_builtin__ref_1_s * MR_Reference;
  typedef MR_ClosurePtr MR_Pred;
  typedef MR_ClosurePtr MR_Func;
  typedef struct mercury__array__array_1_s * MR_Array;
  typedef struct mercury__univ__univ_0_s * MR_Univ;
  typedef struct mercury__type_desc__type_desc_0_s * MR_Type_Desc;
  typedef struct mercury__type_desc__pseudo_type_desc_0_s * MR_Pseudo_Type_Desc;
  typedef struct mercury__type_desc__type_ctor_desc_0_s * MR_Type_Ctor_Desc;
  typedef struct mercury__private_builtin__type_info_0_s *
    MR_Mercury_Type_Info;
  typedef struct mercury__private_builtin__type_ctor_info_0_s *
    MR_Mercury_Type_Ctor_Info;
  typedef struct mercury__private_builtin__typeclass_info_0_s *
    MR_Mercury_TypeClass_Info;
  typedef struct mercury__private_builtin__base_typeclass_info_0_s *
    MR_Mercury_Base_TypeClass_Info;
#else
  // For --no-high-level-data, they are all just `MR_Word'.
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
#endif

#endif  // MR_HIGHLEVEL_CODE

#endif  // MERCURY_HLC_TYPES_H
