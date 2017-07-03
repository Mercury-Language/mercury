// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2007, 2010 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_builtin_types.c
//
// This file defines the operations on the builtin types of Mercury.
// It has separate implementations for the high level and low level C
// back ends.

// Note that the routines here don't need any special handling for accurate GC,
// since they only do tail-calls (or equivalent); their stack stack frames
// will never be live on the stack when a garbage collection occurs (or if
// they are, will never contain any live variables that might contain pointers
// to the Mercury heap).

#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.h"
#else
  #include "mercury_imp.h"
#endif

#include "mercury_type_info.h"          // for MR_TYPECTOR_REP*
#include "mercury_type_desc.h"          // for MR_TypeCtorDesc
#include "mercury_misc.h"               // for MR_fatal_error()
#include "mercury_heap.h"               // for MR_create[1-3]() prototypes
#include "mercury_deep_profiling.h"
#include "mercury_deep_profiling_hand.h"
#include "mercury_profiling_builtin.h"
#include "mercury_builtin_types.h"
#include "mercury_builtin_types_proc_layouts.h"

////////////////////////////////////////////////////////////////////////////

// Define MR_TypeCtorInfos for the builtin types.

#undef VOID

MR_DEFINE_TYPE_CTOR_INFO(builtin, int, 0, INT);
MR_DEFINE_TYPE_CTOR_INFO(builtin, uint, 0, UINT);
MR_DEFINE_TYPE_CTOR_INFO(builtin, int8, 0, INT8);
MR_DEFINE_TYPE_CTOR_INFO(builtin, uint8, 0, UINT8);
MR_DEFINE_TYPE_CTOR_INFO(builtin, int16, 0, INT16);
MR_DEFINE_TYPE_CTOR_INFO(builtin, uint16, 0, UINT16);
MR_DEFINE_TYPE_CTOR_INFO(builtin, int32, 0, INT32);
MR_DEFINE_TYPE_CTOR_INFO(builtin, uint32, 0, UINT32);
MR_DEFINE_TYPE_CTOR_INFO(builtin, character, 0, CHAR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, string, 0, STRING);
MR_DEFINE_TYPE_CTOR_INFO(builtin, float, 0, FLOAT);
MR_DEFINE_TYPE_CTOR_INFO(builtin, void, 0, VOID);
MR_DEFINE_TYPE_CTOR_INFO(builtin, c_pointer, 0, C_POINTER);

MR_DEFINE_TYPE_CTOR_INFO_FLAG(builtin, pred, 0, PRED,
    MR_TYPE_CTOR_FLAG_VARIABLE_ARITY);
MR_DEFINE_TYPE_CTOR_INFO_FLAG(builtin, func, 0, FUNC,
    MR_TYPE_CTOR_FLAG_VARIABLE_ARITY);

static MR_Integer MR_tuple_functor_number_map[] = {0};
MR_DEFINE_TYPE_CTOR_INFO_FLAG_FUNCTORS(builtin, tuple, 0, TUPLE,
    MR_TYPE_CTOR_FLAG_VARIABLE_ARITY, MR_tuple_functor_number_map);

#ifndef MR_HIGHLEVEL_CODE
MR_DEFINE_TYPE_CTOR_INFO(builtin, succip, 0, SUCCIP);
MR_DEFINE_TYPE_CTOR_INFO(builtin, hp, 0, HP);
MR_DEFINE_TYPE_CTOR_INFO(builtin, curfr, 0, CURFR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, maxfr, 0, MAXFR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, redofr, 0, REDOFR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, redoip, 0, REDOIP);
MR_DEFINE_TYPE_CTOR_INFO(builtin, trailptr, 0, TRAIL_PTR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, ticket, 0, TICKET);
#endif

MR_DEFINE_TYPE_CTOR_INFO(private_builtin, heap_pointer, 0, HP);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, ref, 1, REFERENCE);

MR_DEFINE_TYPE_CTOR_INFO(private_builtin, type_ctor_info, 0,
    TYPECTORINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, type_info, 0,
    TYPEINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, base_typeclass_info, 0,
    BASETYPECLASSINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, typeclass_info, 0,
    TYPECLASSINFO);

MR_DEFINE_TYPE_CTOR_INFO(type_desc, type_ctor_desc, 0, TYPECTORDESC);
MR_DEFINE_TYPE_CTOR_INFO(type_desc, pseudo_type_desc, 0, PSEUDOTYPEDESC);
MR_DEFINE_TYPE_CTOR_INFO(type_desc, type_desc, 0, TYPEDESC);

////////////////////////////////////////////////////////////////////////////

#ifdef MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
// Definitions of the type-specific __Unify__ and __Compare__ procedures
// for the builtin types.
//
// There are two versions of each of these. The first version, __Unify__,
// which is called when the type is known at compile time,
// has the arguments unboxed. The second version, do_unify_, which is
// stored in the type_ctor_info and called from the generic
// unify/2 or compare/3, is a wrapper that has the arguments boxed,
// and just calls the first version.

////////////////////////////////////////////////////////////////////////////
// Unification procedures with the arguments unboxed.

MR_bool MR_CALL
mercury__builtin____Unify____int_0_0(MR_Integer x, MR_Integer y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____uint_0_0(MR_Unsigned x, MR_Unsigned y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____int8_0_0(int8_t x, int8_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____uint8_0_0(uint8_t x, uint8_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____int16_0_0(int16_t x, int16_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____uint16_0_0(uint16_t x, uint16_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____int32_0_0(int32_t x, int32_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____uint32_0_0(uint32_t x, uint32_t y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____string_0_0(MR_String x, MR_String y)
{
    return strcmp(x, y) == 0;
}

MR_bool MR_CALL
mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y)
{
    // XXX What should this function do when x and y are both NaNs?
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____void_0_0(MR_Void x, MR_Void y)
{
    MR_fatal_error("called unify/2 for `void' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin____Unify____c_pointer_0_0(MR_C_Pointer x, MR_C_Pointer y)
{
    return (void *) x == (void *) y;
}

MR_bool MR_CALL
mercury__private_builtin____Unify____heap_pointer_0_0(MR_Heap_Pointer x,
    MR_Heap_Pointer y)
{
    MR_fatal_error(
        "called unify/2 for `private_builtin.heap_pointer' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__private_builtin____Unify____ref_1_0(MR_Mercury_Type_Info type_info,
    MR_Reference x, MR_Reference y)
{
    return x == y;
}

MR_bool MR_CALL
mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y)
{
    MR_fatal_error("called unify/2 for `func' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y)
{
    MR_fatal_error("called unify/2 for `pred' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin____Unify____tuple_0_0(MR_Tuple x, MR_Tuple y)
{
    MR_fatal_error("called unify/2 for `tuple' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__type_desc____Unify____type_ctor_desc_0_0(
    MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y)
{
    return MR_unify_type_ctor_desc((MR_TypeCtorDesc) x,
        (MR_TypeCtorDesc) y);
}

MR_bool MR_CALL
mercury__type_desc____Unify____pseudo_type_desc_0_0(MR_Pseudo_Type_Desc x,
    MR_Pseudo_Type_Desc y)
{
    return MR_unify_pseudo_type_info((MR_PseudoTypeInfo) x,
        (MR_PseudoTypeInfo) y);
}

MR_bool MR_CALL
mercury__type_desc____Unify____type_desc_0_0(MR_Type_Desc x, MR_Type_Desc y)
{
    return MR_unify_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_ctor_info_0_0(
    MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
    return MR_unify_type_ctor_info((MR_TypeCtorInfo) x,
        (MR_TypeCtorInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_info_0_0(
    MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
    return MR_unify_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____typeclass_info_0_0(
    MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
    MR_fatal_error("called unify/2 for `typeclass_info' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__private_builtin____Unify____base_typeclass_info_0_0(
    MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
    MR_SORRY("unify for base_typeclass_info");

    return MR_TRUE;
}

////////////////////////////////////////////////////////////////////////////

// Comparison procedures with the arguments unboxed.

void MR_CALL
mercury__builtin____Compare____int_0_0(
    MR_Comparison_Result *result, MR_Integer x, MR_Integer y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____uint_0_0(
    MR_Comparison_Result *result, MR_Unsigned x, MR_Unsigned y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____int8_0_0(
    MR_Comparison_Result *result, int8_t x, int8_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____uint8_0_0(
    MR_Comparison_Result *result, uint8_t x, uint8_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____int16_0_0(
    MR_Comparison_Result *result, int16_t x, int16_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____uint16_0_0(
    MR_Comparison_Result *result, uint16_t x, uint16_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____int32_0_0(
    MR_Comparison_Result *result, int32_t x, int32_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____uint32_0_0(
    MR_Comparison_Result *result, uint32_t x, uint32_t y)
{
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____string_0_0(MR_Comparison_Result *result,
    MR_String x, MR_String y)
{
    int res = strcmp(x, y);
    *result = (res > 0 ? MR_COMPARE_GREATER :
          res == 0 ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____float_0_0(
    MR_Comparison_Result *result, MR_Float x, MR_Float y)
{
    // XXX What should this function do when x and y are both NaNs?
    *result = (x > y ? MR_COMPARE_GREATER :
          x == y ? MR_COMPARE_EQUAL :
          x < y ? MR_COMPARE_LESS :
              (MR_fatal_error("incomparable floats in compare/3"),
              MR_COMPARE_EQUAL));
}

void MR_CALL
mercury__builtin____Compare____character_0_0(
    MR_Comparison_Result *result, MR_Char x, MR_Char y)
{
    MR_Integer xi = (MR_UnsignedChar) x;
    MR_Integer yi = (MR_UnsignedChar) y;
    *result = (xi > yi ? MR_COMPARE_GREATER :
          xi == yi ? MR_COMPARE_EQUAL :
          MR_COMPARE_LESS);
}

void MR_CALL
mercury__builtin____Compare____void_0_0(MR_Comparison_Result *result,
    MR_Void x, MR_Void y)
{
    MR_fatal_error("called compare/3 for `void' type");
}

void MR_CALL
mercury__builtin____Compare____c_pointer_0_0(
    MR_Comparison_Result *result, MR_C_Pointer x, MR_C_Pointer y)
{
    *result =
        ( (void *) x == (void *) y ? MR_COMPARE_EQUAL
        : (void *) x <  (void *) y ? MR_COMPARE_LESS
        : MR_COMPARE_GREATER
        );
}

void MR_CALL
mercury__private_builtin____Compare____heap_pointer_0_0(
    MR_Comparison_Result *result, MR_Heap_Pointer x, MR_Heap_Pointer y)
{
    MR_fatal_error("called compare/3 for `private_builtin.heap_pointer' type");
}

void MR_CALL
mercury__private_builtin____Compare____ref_1_0(MR_Mercury_Type_Info type_info,
    MR_Comparison_Result *result, MR_Reference x, MR_Reference y)
{
    MR_fatal_error("called compare/3 for `private_builtin.ref' type");
}

void MR_CALL
mercury__builtin____Compare____func_0_0(MR_Comparison_Result *result,
    MR_Func x, MR_Func y)
{
    MR_fatal_error("called compare/3 for `func' type");
}

void MR_CALL
mercury__builtin____Compare____pred_0_0(MR_Comparison_Result *result,
    MR_Pred x, MR_Pred y)
{
    MR_fatal_error("called compare/3 for `pred' type");
}

void MR_CALL
mercury__builtin____Compare____tuple_0_0(MR_Comparison_Result *result,
    MR_Tuple x, MR_Tuple y)
{
    MR_fatal_error("called compare/3 for `tuple' type");
}

void MR_CALL
mercury__type_desc____Compare____type_ctor_desc_0_0(
    MR_Comparison_Result *result, MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y)
{
    *result = MR_compare_type_ctor_desc((MR_TypeCtorDesc) x,
        (MR_TypeCtorDesc) y);
}

void MR_CALL
mercury__type_desc____Compare____pseudo_type_desc_0_0(
    MR_Comparison_Result *result,
    MR_Pseudo_Type_Desc x, MR_Pseudo_Type_Desc y)
{
    *result = MR_compare_pseudo_type_info((MR_PseudoTypeInfo) x,
        (MR_PseudoTypeInfo) y);
}

void MR_CALL
mercury__type_desc____Compare____type_desc_0_0(
    MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y)
{
    *result = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____type_ctor_info_0_0(
    MR_Comparison_Result *result,
    MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
    *result = MR_compare_type_ctor_info((MR_TypeCtorInfo) x,
        (MR_TypeCtorInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____type_info_0_0(
    MR_Comparison_Result *result,
    MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
    *result = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____typeclass_info_0_0(
    MR_Comparison_Result *result,
    MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
    MR_fatal_error("called compare/3 for `typeclass_info' type");
}

void MR_CALL
mercury__private_builtin____Compare____base_typeclass_info_0_0(
    MR_Comparison_Result *result,
    MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
    MR_SORRY("compare for base_typeclass_info");
}

////////////////////////////////////////////////////////////////////////////

// Unification procedures with the arguments boxed.
// These are just wrappers which call the unboxed version.

MR_bool MR_CALL
mercury__builtin__do_unify__int_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____int_0_0(
        (MR_Integer) x, (MR_Integer) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__uint_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____uint_0_0(
        (MR_Unsigned) x, (MR_Unsigned) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__int8_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____int8_0_0(
        (int8_t) x, (int8_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__uint8_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____uint8_0_0(
        (uint8_t) x, (uint8_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__int16_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____int16_0_0(
        (int16_t) x, (int16_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__uint16_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____uint16_0_0(
        (uint16_t) x, (uint16_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__int32_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____int32_0_0(
        (int32_t) x, (int32_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__uint32_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____uint32_0_0(
        (uint32_t) x, (uint32_t) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__string_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____string_0_0(
        (MR_String) x, (MR_String) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__float_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____float_0_0(
        MR_unbox_float(x), MR_unbox_float(y));
}

MR_bool MR_CALL
mercury__builtin__do_unify__character_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____character_0_0(
        (MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__void_0_0(MR_Box x, MR_Box y)
{
    MR_fatal_error("called unify/2 for `void' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin__do_unify__c_pointer_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____c_pointer_0_0(
        (MR_C_Pointer) x, (MR_C_Pointer) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__heap_pointer_0_0(MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____heap_pointer_0_0(
        (MR_Heap_Pointer) x, (MR_Heap_Pointer) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__ref_1_0(MR_Mercury_Type_Info type_info,
    MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____ref_1_0(type_info,
        (MR_Reference) x, (MR_Reference) y);
}

MR_bool MR_CALL
mercury__builtin__do_unify__func_0_0(MR_Box x, MR_Box y)
{
    MR_fatal_error("called unify/2 for `func' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin__do_unify__pred_0_0(MR_Box x, MR_Box y)
{
    MR_fatal_error("called unify/2 for `pred' type");

    return MR_TRUE;
}

MR_bool MR_CALL
mercury__builtin__do_unify__tuple_0_0(MR_Box x, MR_Box y)
{
    return mercury__builtin____Unify____tuple_0_0(
        (MR_Tuple) x, (MR_Tuple) y);
}

MR_bool MR_CALL
mercury__type_desc__do_unify__type_ctor_desc_0_0(MR_Box x, MR_Box y)
{
    return mercury__type_desc____Unify____type_ctor_desc_0_0(
        (MR_Type_Ctor_Desc) x, (MR_Type_Ctor_Desc) y);
}

MR_bool MR_CALL
mercury__type_desc__do_unify__pseudo_type_desc_0_0(MR_Box x, MR_Box y)
{
    return mercury__type_desc____Unify____pseudo_type_desc_0_0(
        (MR_Pseudo_Type_Desc) x, (MR_Pseudo_Type_Desc) y);
}

MR_bool MR_CALL
mercury__type_desc__do_unify__type_desc_0_0(MR_Box x, MR_Box y)
{
    return mercury__type_desc____Unify____type_desc_0_0(
        (MR_Type_Desc) x, (MR_Type_Desc) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__type_ctor_info_0_0(
    MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____type_ctor_info_0_0(
        (MR_Mercury_Type_Ctor_Info) x, (MR_Mercury_Type_Ctor_Info) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__type_info_0_0(
    MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____type_info_0_0(
        (MR_Mercury_Type_Info) x, (MR_Mercury_Type_Info) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__typeclass_info_0_0(
    MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____typeclass_info_0_0(
        (MR_Mercury_TypeClass_Info) x, (MR_Mercury_TypeClass_Info) y);
}

MR_bool MR_CALL
mercury__private_builtin__do_unify__base_typeclass_info_0_0(
    MR_Box x, MR_Box y)
{
    return mercury__private_builtin____Unify____base_typeclass_info_0_0(
        (MR_Mercury_Base_TypeClass_Info) x,
        (MR_Mercury_Base_TypeClass_Info) y);
}

////////////////////////////////////////////////////////////////////////////

// Comparison procedures with the arguments boxed.
// These are just wrappers which call the unboxed version.

void MR_CALL
mercury__builtin__do_compare__int_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____int_0_0(result,
        (MR_Integer) x, (MR_Integer) y);
}

void MR_CALL
mercury__builtin__do_compare__uint_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____uint_0_0(result,
        (MR_Unsigned) x, (MR_Unsigned) y);
}

void MR_CALL
mercury__builtin__do_compare__int8_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____int8_0_0(result,
        (int8_t) x, (int8_t) y);
}

void MR_CALL
mercury__builtin__do_compare__uint8_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____uint8_0_0(result,
        (uint8_t) x, (uint8_t) y);
}

void MR_CALL
mercury__builtin__do_compare__int16_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____int16_0_0(result,
        (int16_t) x, (int16_t) y);
}

void MR_CALL
mercury__builtin__do_compare__uint16_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____uint16_0_0(result,
        (uint16_t) x, (uint16_t) y);
}

void MR_CALL
mercury__builtin__do_compare__int32_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____int32_0_0(result,
        (int32_t) x, (int32_t) y);
}

void MR_CALL
mercury__builtin__do_compare__uint32_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____uint32_0_0(result,
        (uint32_t) x, (uint32_t) y);
}

void MR_CALL
mercury__builtin__do_compare__string_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____string_0_0(result,
        (MR_String) x, (MR_String) y);
}

void MR_CALL
mercury__builtin__do_compare__float_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____float_0_0(result,
        MR_unbox_float(x), MR_unbox_float(y));
}

void MR_CALL
mercury__builtin__do_compare__character_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____character_0_0(result,
        (MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

void MR_CALL
mercury__builtin__do_compare__void_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    MR_fatal_error("called compare/3 for `void' type");
}

void MR_CALL
mercury__builtin__do_compare__c_pointer_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____c_pointer_0_0(result,
        (MR_C_Pointer) x, (MR_C_Pointer) y);
}

void MR_CALL
mercury__private_builtin__do_compare__heap_pointer_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    MR_fatal_error("called compare/3 for `private_builtin.heap_pointer' type");
}

void MR_CALL
mercury__private_builtin__do_compare__ref_1_0(
    MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
    MR_Box x, MR_Box y)
{
    MR_fatal_error("called compare/3 for `private_builtin.ref' type");
}

void MR_CALL
mercury__builtin__do_compare__func_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    MR_fatal_error("called compare/3 for `func' type");
}

void MR_CALL
mercury__builtin__do_compare__pred_0_0(MR_Comparison_Result *result,
    MR_Box x, MR_Box y)
{
    MR_fatal_error("called compare/3 for `pred' type");
}

void MR_CALL
mercury__builtin__do_compare__tuple_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__builtin____Compare____tuple_0_0(result,
        (MR_Tuple) x, (MR_Tuple) y);
}

void MR_CALL
mercury__type_desc__do_compare__type_ctor_desc_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__type_desc____Compare____type_ctor_desc_0_0(result,
        (MR_Type_Ctor_Desc) x, (MR_Type_Ctor_Desc) y);
}

void MR_CALL
mercury__type_desc__do_compare__pseudo_type_desc_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__type_desc____Compare____pseudo_type_desc_0_0(result,
        (MR_Pseudo_Type_Desc) x, (MR_Pseudo_Type_Desc) y);
}

void MR_CALL
mercury__type_desc__do_compare__type_desc_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__type_desc____Compare____type_desc_0_0(result,
        (MR_Type_Desc) x, (MR_Type_Desc) y);
}

void MR_CALL
mercury__private_builtin__do_compare__type_ctor_info_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__private_builtin____Compare____type_ctor_info_0_0(result,
        (MR_Mercury_Type_Ctor_Info) x, (MR_Mercury_Type_Ctor_Info) y);
}

void MR_CALL
mercury__private_builtin__do_compare__type_info_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__private_builtin____Compare____type_info_0_0(result,
        (MR_Mercury_Type_Info) x, (MR_Mercury_Type_Info) y);
}

void MR_CALL
mercury__private_builtin__do_compare__typeclass_info_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__private_builtin____Compare____typeclass_info_0_0(result,
        (MR_Mercury_TypeClass_Info) x, (MR_Mercury_TypeClass_Info) y);
}

void MR_CALL
mercury__private_builtin__do_compare__base_typeclass_info_0_0(
    MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
    mercury__private_builtin____Compare____base_typeclass_info_0_0(result,
        (MR_Mercury_Base_TypeClass_Info) x,
        (MR_Mercury_Base_TypeClass_Info) y);
}

#else   // ! MR_HIGHLEVEL_CODE

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc mercury_builtin_types;

  #define MR_UNIFY_COMPARE_REP_DEFNS(m, n, a)                                \
    MR_define_extern_entry(MR_proc_entry_uci_name(m, __Unify__, n, a, 0));   \
    MR_define_extern_entry(MR_proc_entry_uci_name(m, __Compare__, n, a, 0)); \
    MR_define_extern_entry(MR_proc_entry_uci_name(m, __CompareRep__, n, a, 0));

  #ifdef MR_DEEP_PROFILING

    #define MR_UNIFY_COMPARE_REP_DECLS(m, n, a)                             \
      MR_declare_entry(MR_proc_entry_uci_name(m, __Unify__, n, a, 0));      \
      MR_declare_entry(MR_proc_entry_uci_name(m, __Compare__, n, a, 0));    \
      MR_declare_entry(MR_proc_entry_uci_name(m, __CompareRep__, n, a, 0)); \
      MR_declare_label(MR_label_uci_name(m, __Unify__, n, a, 0, 1));        \
      MR_declare_label(MR_label_uci_name(m, __Unify__, n, a, 0, 2));        \
      MR_declare_label(MR_label_uci_name(m, __Unify__, n, a, 0, 3));        \
      MR_declare_label(MR_label_uci_name(m, __Unify__, n, a, 0, 4));        \
      MR_declare_label(MR_label_uci_name(m, __Compare__, n, a, 0, 1));      \
      MR_declare_label(MR_label_uci_name(m, __Compare__, n, a, 0, 2));      \
      MR_declare_label(MR_label_uci_name(m, __CompareRep__, n, a, 0, 1));   \
      MR_declare_label(MR_label_uci_name(m, __CompareRep__, n, a, 0, 2));

    #define MR_UNIFY_COMPARE_REP_LABELS(m, n, a)                            \
      MR_init_entry(MR_proc_entry_uci_name(m, __Unify__, n, a, 0));         \
      MR_init_entry(MR_proc_entry_uci_name(m, __Compare__, n, a, 0));       \
      MR_init_entry(MR_proc_entry_uci_name(m, __CompareRep__, n, a, 0));    \
      MR_init_label(MR_label_uci_name(m, __Unify__, n, a, 0, 1));           \
      MR_init_label(MR_label_uci_name(m, __Unify__, n, a, 0, 2));           \
      MR_init_label(MR_label_uci_name(m, __Unify__, n, a, 0, 3));           \
      MR_init_label(MR_label_uci_name(m, __Unify__, n, a, 0, 4));           \
      MR_init_label(MR_label_uci_name(m, __Compare__, n, a, 0, 1));         \
      MR_init_label(MR_label_uci_name(m, __Compare__, n, a, 0, 2));         \
      MR_init_label(MR_label_uci_name(m, __CompareRep__, n, a, 0, 1));      \
      MR_init_label(MR_label_uci_name(m, __CompareRep__, n, a, 0, 2));

  #else  // ! MR_DEEP_PROFILING

    #define MR_UNIFY_COMPARE_REP_DECLS(m, n, a)                             \
      MR_declare_entry(MR_proc_entry_uci_name(m, __Unify__, n, a, 0));      \
      MR_declare_entry(MR_proc_entry_uci_name(m, __Compare__, n, a, 0));    \
      MR_declare_entry(MR_proc_entry_uci_name(m, __CompareRep__, n, a, 0));

    #define MR_UNIFY_COMPARE_REP_LABELS(m, n, a)                            \
      MR_init_entry(MR_proc_entry_uci_name(m, __Unify__, n, a, 0));         \
      MR_init_entry(MR_proc_entry_uci_name(m, __Compare__, n, a, 0));       \
      MR_init_entry(MR_proc_entry_uci_name(m, __CompareRep__, n, a, 0));

  #endif // MR_DEEP_PROFILING

MR_UNIFY_COMPARE_REP_DECLS(builtin, int, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, uint, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, int8, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, uint8, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, int16, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, uint16, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, int32, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, uint32, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, string, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, float, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, character, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, void, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, c_pointer, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, pred, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, func, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, tuple, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, succip, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, hp, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, curfr, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, maxfr, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, redofr, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, redoip, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, trailptr, 0)
MR_UNIFY_COMPARE_REP_DECLS(builtin, ticket, 0)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, heap_pointer, 0)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, ref, 1)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, type_ctor_info, 0)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, type_info, 0)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, base_typeclass_info, 0)
MR_UNIFY_COMPARE_REP_DECLS(private_builtin, typeclass_info, 0)
MR_UNIFY_COMPARE_REP_DECLS(type_desc, type_ctor_desc, 0);
MR_UNIFY_COMPARE_REP_DECLS(type_desc, pseudo_type_desc, 0);
MR_UNIFY_COMPARE_REP_DECLS(type_desc, type_desc, 0);
MR_UNIFY_COMPARE_REP_DECLS(builtin, user_by_rtti, 0);
MR_UNIFY_COMPARE_REP_DECLS(builtin, dummy, 0);

MR_UNIFY_COMPARE_REP_DEFNS(builtin, int, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, uint, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, int8, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, uint8, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, int16, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, uint16, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, int32, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, uint32, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, string, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, float, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, character, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, void, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, c_pointer, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, pred, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, func, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, tuple, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, succip, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, hp, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, curfr, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, maxfr, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, redofr, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, redoip, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, trailptr, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, ticket, 0)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, heap_pointer, 0)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, ref, 1)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, type_ctor_info, 0)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, type_info, 0)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, base_typeclass_info, 0)
MR_UNIFY_COMPARE_REP_DEFNS(private_builtin, typeclass_info, 0)
MR_UNIFY_COMPARE_REP_DEFNS(type_desc, type_ctor_desc, 0)
MR_UNIFY_COMPARE_REP_DEFNS(type_desc, pseudo_type_desc, 0)
MR_UNIFY_COMPARE_REP_DEFNS(type_desc, type_desc, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, user_by_rtti, 0)
MR_UNIFY_COMPARE_REP_DEFNS(builtin, dummy, 0)

#ifdef MR_DEEP_PROFILING

// The generic unify, compare and compare_rep predicates do different things
// for different kinds of type constructors, but these things all fall into
// one of two categories: either the implementation is entirely in C code,
// or the implementation is a tailcall to a Mercury predicate. In neither
// case do the generic predicates allocate a stack frame, which is why
// the stack traversal component of the procedure layouts won't ever be
// referenced.

    #define MR_DEFINE_PROC_STATIC_LAYOUTS(mod, tname, tarity)               \
      MR_proc_static_uci_no_site(mod, __Unify__, tname, tarity, 0,          \
          MR_STRINGIFY(mod) ".m", 0, MR_TRUE);                              \
          MR_EXTERN_UCI_PROC_STATIC_PROC_LAYOUT(                            \
          MR_DETISM_SEMI, 0, MR_LONG_LVAL_TYPE_UNKNOWN,                     \
          mod, __Unify__, tname, tarity, 0);                                \
      MR_proc_static_uci_no_site(mod, __Compare__, tname, tarity, 0,        \
          MR_STRINGIFY(mod) ".m", 0, MR_TRUE);                              \
          MR_EXTERN_UCI_PROC_STATIC_PROC_LAYOUT(                            \
          MR_DETISM_DET, 0, MR_LONG_LVAL_TYPE_UNKNOWN,                      \
          mod, __Compare__, tname, tarity, 0);                              \
      MR_proc_static_uci_no_site(mod, __CompareRep__, tname, tarity, 0,     \
          MR_STRINGIFY(mod) ".m", 0, MR_TRUE);                              \
          MR_EXTERN_UCI_PROC_STATIC_PROC_LAYOUT(                            \
          MR_DETISM_DET, 0, MR_LONG_LVAL_TYPE_UNKNOWN,                      \
          mod, __CompareRep__, tname, tarity, 0);

    #define MR_WRITE_OUT_PROC_STATIC_LAYOUTS(fp, m, n, a)                   \
      do {                                                                  \
          MR_write_out_uci_proc_static(fp, NULL,                            \
            &MR_proc_layout_uci_name(m, __Unify__, n, a, 0));               \
          MR_write_out_uci_proc_static(fp, NULL,                            \
            &MR_proc_layout_uci_name(m, __Compare__, n, a, 0));             \
          MR_write_out_uci_proc_static(fp, NULL,                            \
            &MR_proc_layout_uci_name(m, __CompareRep__, n, a, 0));          \
      } while (0)

// If you add another entry to this list, you should also add the corresponding
// declaration to mercury_builtin_types_proc_layouts.h.
// You should also make sure that any changes made here are reflected in
// the definition of the function
// mercury_sys_init_mercury_builtin_types_write_out_proc_statics() below.

MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, int, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, uint, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, int8, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, uint8, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, int16, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, uint16, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, int32, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, uint32, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, string, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, float, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, character, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, void, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, c_pointer, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, pred, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, func, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, tuple, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, succip, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, hp, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, curfr, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, maxfr, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, redofr, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, redoip, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, trailptr, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, ticket, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, heap_pointer, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, ref, 1);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, type_ctor_info, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, type_info, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, base_typeclass_info, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(private_builtin, typeclass_info, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(type_desc, type_ctor_desc, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(type_desc, pseudo_type_desc, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(type_desc, type_desc, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, user_by_rtti, 0);
MR_DEFINE_PROC_STATIC_LAYOUTS(builtin, dummy, 0);

#endif // MR_DEEP_PROFILING

MR_BEGIN_MODULE(mercury_builtin_types)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, int, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, uint, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, int8, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, uint8, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, int16, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, uint16, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, int32, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, uint32, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, string, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, float, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, character, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, void, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, c_pointer, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, pred, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, func, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, tuple, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, succip, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, hp, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, curfr, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, maxfr, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, redofr, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, redoip, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, trailptr, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, ticket, 0)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, heap_pointer, 0)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, ref, 1)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, type_ctor_info, 0)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, type_info, 0)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, base_typeclass_info, 0)
    MR_UNIFY_COMPARE_REP_LABELS(private_builtin, typeclass_info, 0)
    MR_UNIFY_COMPARE_REP_LABELS(type_desc, type_ctor_desc, 0)
    MR_UNIFY_COMPARE_REP_LABELS(type_desc, pseudo_type_desc, 0)
    MR_UNIFY_COMPARE_REP_LABELS(type_desc, type_desc, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, user_by_rtti, 0)
    MR_UNIFY_COMPARE_REP_LABELS(builtin, dummy, 0)
MR_BEGIN_CODE

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            int
#define arity           0
#define unify_code      MR_r1 = ((MR_Integer) MR_r1 == (MR_Integer) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((MR_Integer) MR_r1 == (MR_Integer) MR_r2 ? \
                                MR_COMPARE_EQUAL :                      \
                            (MR_Integer) MR_r1 < (MR_Integer) MR_r2 ?   \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            uint
#define arity           0
#define unify_code      MR_r1 = ((MR_Unsigned) MR_r1 == (MR_Unsigned) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((MR_Unsigned) MR_r1 == (MR_Unsigned) MR_r2 ? \
                                MR_COMPARE_EQUAL :                      \
                            (MR_Unsigned) MR_r1 < (MR_Unsigned) MR_r2 ?   \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            int8
#define arity           0
#define unify_code      MR_r1 = ((int8_t) MR_r1 == (int8_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((int8_t) MR_r1 == (int8_t) MR_r2 ?         \
                                MR_COMPARE_EQUAL :                      \
                            (int8_t) MR_r1 < (int8_t) MR_r2 ?           \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            uint8
#define arity           0
#define unify_code      MR_r1 = ((uint8_t) MR_r1 == (uint8_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((uint8_t) MR_r1 == (uint8_t) MR_r2 ?       \
                                MR_COMPARE_EQUAL :                      \
                            (uint8_t) MR_r1 < (uint8_t) MR_r2 ?         \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            int16
#define arity           0
#define unify_code      MR_r1 = ((int16_t) MR_r1 == (int16_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((int16_t) MR_r1 == (int16_t) MR_r2 ?       \
                                MR_COMPARE_EQUAL :                      \
                            (int16_t) MR_r1 < (int16_t) MR_r2 ?         \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            uint16
#define arity           0
#define unify_code      MR_r1 = ((uint16_t) MR_r1 == (uint16_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((uint16_t) MR_r1 == (uint16_t) MR_r2 ?     \
                                MR_COMPARE_EQUAL :                      \
                            (uint16_t) MR_r1 < (uint16_t) MR_r2 ?       \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            int32
#define arity           0
#define unify_code      MR_r1 = ((int32_t) MR_r1 == (int32_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((int32_t) MR_r1 == (int32_t) MR_r2 ?       \
                                MR_COMPARE_EQUAL :                      \
                            (int32_t) MR_r1 < (int32_t) MR_r2 ?         \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            uint32
#define arity           0
#define unify_code      MR_r1 = ((uint32_t) MR_r1 == (uint32_t) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((uint32_t) MR_r1 == (uint32_t) MR_r2 ?     \
                                MR_COMPARE_EQUAL :                      \
                            (uint32_t) MR_r1 < (uint32_t) MR_r2 ?       \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            string
#define arity           0
#define unify_code      MR_r1 = strcmp((char *) MR_r1, (char *) MR_r2) == 0;
#define compare_code    int result = strcmp((char *) MR_r1, (char *) MR_r2);  \
                        MR_r1 = (result > 0) ? MR_COMPARE_GREATER :           \
                            (result < 0 ? MR_COMPARE_LESS :                   \
                            MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            float
#define arity           0
#define unify_code      MR_r1 = (MR_word_to_float(MR_r1)                \
                            == MR_word_to_float(MR_r2));
#define compare_code    MR_Float f1 = MR_word_to_float(MR_r1);          \
                        MR_Float f2 = MR_word_to_float(MR_r2);          \
                        MR_r1 = ((f1 > f2) ? MR_COMPARE_GREATER :       \
                            (f1 < f2) ? MR_COMPARE_LESS :               \
                            MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            character
#define arity           0
#define unify_code      MR_r1 = ((MR_Char) MR_r1 == (MR_Char) MR_r2);
#define compare_code    MR_r1 =                                         \
                            ((MR_Char) MR_r1 > (MR_Char) MR_r2 ?        \
                                MR_COMPARE_GREATER :                    \
                            (MR_Char) MR_r1 < (MR_Char) MR_r2 ?         \
                                MR_COMPARE_LESS :                       \
                            MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          builtin
#define type            void
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `void' type");
#define compare_code    MR_fatal_error("called compare/3 for `void' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// For c_pointer, we assume that equality and comparison
// can be based on object identity (i.e. using address comparisons).
// This is correct for types like io__stream, and necessary since
// the io__state contains a map(io__stream, filename).
// However, it might not be correct in general...

#define module          builtin
#define type            c_pointer
#define arity           0
#define unify_code      MR_r1 = (MR_r1 == MR_r2);
#define compare_code    MR_r1 = (MR_r1 > MR_r2 ? MR_COMPARE_GREATER :   \
                            MR_r1 < MR_r2 ? MR_COMPARE_LESS :           \
                            MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Predicates cannot be unified or compared.

#define module          builtin
#define type            pred
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `pred' type");
#define compare_code    MR_fatal_error("called compare/3 for `pred' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Functions cannot be unified or compared.

#define module          builtin
#define type            func
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `func' type");
#define compare_code    MR_fatal_error("called compare/3 for `func' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of tuples are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            tuple
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `tuple' type");
#define compare_code    MR_fatal_error("called compare/3 for `tuple' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of succips are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            succip
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `succip' type");
#define compare_code    MR_fatal_error("called compare/3 for `succip' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of hps are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            hp
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `hp' type");
#define compare_code    MR_fatal_error("called compare/3 for `hp' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of curfrs are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            curfr
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `curfr' type");
#define compare_code    MR_fatal_error("called compare/3 for `curfr' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of maxfrs are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            maxfr
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `maxfr' type");
#define compare_code    MR_fatal_error("called compare/3 for `maxfr' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of redofrs are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            redofr
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `redofr' type");
#define compare_code    MR_fatal_error("called compare/3 for `redofr' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of redoips are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            redoip
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `redoip' type");
#define compare_code    MR_fatal_error("called compare/3 for `redoip' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of trailptrs are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            trailptr
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `trailptr' type");
#define compare_code    MR_fatal_error("called compare/3 for `trailptr' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of tickets are always handled by the generic unify/2
// and compare/3 predicates.

#define module          builtin
#define type            ticket
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `ticket' type");
#define compare_code    MR_fatal_error("called compare/3 for `ticket' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of heap_pointers are always handled by the generic unify/2
// and compare/3 predicates.

#define module          private_builtin
#define type            heap_pointer
#define arity           0
#define unify_code      MR_fatal_error(                                       \
                            "called unify/2 for `heap_pointer' type");
#define compare_code    MR_fatal_error(                                       \
                            "called compare/3 for `heap_pointer' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of references are usually handled by the generic unify/2
// and compare/3 predicates.

#define module          private_builtin
#define type            ref
#define arity           1
// The inputs are type_info in r1, first ref in r2, second ref in r3.
#define unify_code      MR_r1 = (MR_r2 == MR_r3);
#define compare_code    MR_fatal_error("called compare/3 for `ref' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of type_ctor_infos are usually handled by the generic
// unify/2 and compare/3 predicates.

#define module          private_builtin
#define type            type_ctor_info
#define arity           0
#define unify_code      int comp;                                             \
                                                                              \
                        MR_save_transient_registers();                        \
                        comp = MR_compare_type_ctor_info(                     \
                            (MR_TypeCtorInfo) MR_r1,                          \
                            (MR_TypeCtorInfo) MR_r2);                         \
                        MR_restore_transient_registers();                     \
                        MR_r1 = (comp == MR_COMPARE_EQUAL);
#define compare_code    int comp;                                             \
                                                                              \
                        MR_save_transient_registers();                        \
                        comp = MR_compare_type_ctor_info(                     \
                            (MR_TypeCtorInfo) MR_r1,                          \
                            (MR_TypeCtorInfo) MR_r2);                         \
                        MR_restore_transient_registers();                     \
                        MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of type_infos are usually handled by the generic
// unify/2 and compare/3 predicates.

#define module          private_builtin
#define type            type_info
#define arity           0
#define unify_code      int comp;                                             \
                                                                              \
                        MR_save_transient_registers();                        \
                        comp = MR_compare_type_info(                          \
                            (MR_TypeInfo) MR_r1,                              \
                            (MR_TypeInfo) MR_r2);                             \
                        MR_restore_transient_registers();                     \
                        MR_r1 = (comp == MR_COMPARE_EQUAL);
#define compare_code    int comp;                                             \
                                                                              \
                        MR_save_transient_registers();                        \
                        comp = MR_compare_type_info(                          \
                            (MR_TypeInfo) MR_r1,                              \
                            (MR_TypeInfo) MR_r2);                             \
                        MR_restore_transient_registers();                     \
                        MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of base_typeclass_infos are always handled by the generic
// unify/2 and compare/3 predicates.

#define module          private_builtin
#define type            base_typeclass_info
#define arity           0
#define unify_code      MR_fatal_error( \
                            "called unify/2 for `base_typeclass_info' type");
#define compare_code    MR_fatal_error( \
                            "called compare/3 for `base_typeclass_info' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of typeclass_infos are always handled by the generic
// unify/2 and compare/3 predicates.

#define module          private_builtin
#define type            typeclass_info
#define arity           0
#define unify_code      MR_fatal_error( \
                            "called unify/2 for `base_typeclass_info' type");
#define compare_code    MR_fatal_error( \
                            "called compare/3 for `base_typeclass_info' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of type_ctor_descs are usually handled by the generic
// unify/2 and compare/3 predicates.

#define module          type_desc
#define type            type_ctor_desc
#define arity           0
#define unify_code      int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_type_ctor_desc(               \
                            (MR_TypeCtorDesc) MR_r1,                    \
                            (MR_TypeCtorDesc) MR_r2);                   \
                        MR_restore_transient_registers();               \
                        MR_r1 = (comp == MR_COMPARE_EQUAL);
#define compare_code    int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_type_ctor_desc(               \
                            (MR_TypeCtorDesc) MR_r1,                    \
                            (MR_TypeCtorDesc) MR_r2);                   \
                        MR_restore_transient_registers();               \
                        MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

#define module          type_desc
#define type            pseudo_type_desc
#define arity           0
#define unify_code      int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_pseudo_type_info(             \
                            (MR_PseudoTypeInfo) MR_r1,                  \
                            (MR_PseudoTypeInfo) MR_r2);                 \
                        MR_restore_transient_registers();               \
                        MR_r1 = (comp == MR_COMPARE_EQUAL);
#define compare_code    int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_pseudo_type_info(             \
                            (MR_PseudoTypeInfo) MR_r1,                  \
                            (MR_PseudoTypeInfo) MR_r2);                 \
                        MR_restore_transient_registers();               \
                        MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// Unify and compare of type_descs are usually handled by the generic
// unify/2 and compare/3 predicates.

#define module          type_desc
#define type            type_desc
#define arity           0
#define unify_code      int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_type_info(                    \
                            (MR_TypeInfo) MR_r1,                        \
                            (MR_TypeInfo) MR_r2);                       \
                        MR_restore_transient_registers();               \
                        MR_r1 = (comp == MR_COMPARE_EQUAL);
#define compare_code    int comp;                                       \
                                                                        \
                        MR_save_transient_registers();                  \
                        comp = MR_compare_type_info(                    \
                            (MR_TypeInfo) MR_r1,                        \
                            (MR_TypeInfo) MR_r2);                       \
                        MR_restore_transient_registers();               \
                        MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// We need a proc_static structure with which to record profiling information
// about compare_representation when it compares the representations of
// user-defined types. The proc_layout structure which contains the proc_static
// structure also needs a procedure label. The simplest way to provide one
// is to define unify, compare and compare_rep procedures, all of which are
// designed to be unused (if they *are* called, they will abort).

#define module          builtin
#define type            user_by_rtti
#define arity           0
#define unify_code      MR_fatal_error( \
                            "called unify/2 for `user_by_rtti' type");
#define compare_code    MR_fatal_error(  \
                            "called compare/3 for `user_by_rtti' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

// We need a proc_static structure with which to record profiling information
// about compare_representation when it compares the representations of
// dummy types. The proc_layout structure which contains the proc_static
// structure also needs a procedure label. The simplest way to provide one
// is to define unify, compare and compare_rep procedures, all of which are
// designed to be unused (if they *are* called, they will abort).

#define module          builtin
#define type            dummy
#define arity           0
#define unify_code      MR_fatal_error("called unify/2 for `dummy' type");
#define compare_code    MR_fatal_error("called compare/3 for `dummy' type");

#include "mercury_hand_unify_compare_body.h"

#undef  module
#undef  type
#undef  arity
#undef  unify_code
#undef  compare_code

////////////////////////////////////////////////////////////////////////////

MR_END_MODULE

#endif // ! MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

/*
INIT mercury_sys_init_mercury_builtin_types
ENDINIT
*/

// Forward decls, to suppress gcc -Wmissing-decl warnings.
void mercury_sys_init_mercury_builtin_types_init(void);
void mercury_sys_init_mercury_builtin_types_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_mercury_builtin_types_write_out_proc_statics(
    FILE *deep_fp, FILE *procrep_fp);
#endif

void
mercury_sys_init_mercury_builtin_types_init(void)
{
#ifdef MR_HIGHLEVEL_CODE

    // We need to call MR_init_entry() for the unification and comparison
    // predicates for the types that are automatically predefined
    // by the type checker. (Note that c_pointer is *not* predefined
    // by the type checker, instead it is explicitly declared in
    // library/builtin.m.)

    MR_init_entry(mercury__builtin____Unify____int_0_0);
    MR_init_entry(mercury__builtin____Unify____uint_0_0);
    MR_init_entry(mercury__builtin____Unify____int8_0_0);
    MR_init_entry(mercury__builtin____Unify____uint8_0_0);
    MR_init_entry(mercury__builtin____Unify____int16_0_0);
    MR_init_entry(mercury__builtin____Unify____uint16_0_0);
    MR_init_entry(mercury__builtin____Unify____int32_0_0);
    MR_init_entry(mercury__builtin____Unify____uint32_0_0);
    MR_init_entry(mercury__builtin____Unify____string_0_0);
    MR_init_entry(mercury__builtin____Unify____float_0_0);
    MR_init_entry(mercury__builtin____Unify____character_0_0);
    MR_init_entry(mercury__builtin____Unify____void_0_0);
    MR_init_entry(mercury__builtin____Unify____pred_0_0);
    MR_init_entry(mercury__builtin____Unify____func_0_0);
    MR_init_entry(mercury__builtin____Unify____tuple_0_0);

    MR_init_entry(mercury__builtin____Compare____int_0_0);
    MR_init_entry(mercury__builtin____Compare____uint_0_0);
    MR_init_entry(mercury__builtin____Compare____int8_0_0);
    MR_init_entry(mercury__builtin____Compare____uint8_0_0);
    MR_init_entry(mercury__builtin____Compare____int16_0_0);
    MR_init_entry(mercury__builtin____Compare____uint16_0_0);
    MR_init_entry(mercury__builtin____Compare____int32_0_0);
    MR_init_entry(mercury__builtin____Compare____uint32_0_0);
    MR_init_entry(mercury__builtin____Compare____float_0_0);
    MR_init_entry(mercury__builtin____Compare____string_0_0);
    MR_init_entry(mercury__builtin____Compare____character_0_0);
    MR_init_entry(mercury__builtin____Compare____void_0_0);
    MR_init_entry(mercury__builtin____Compare____pred_0_0);
    MR_init_entry(mercury__builtin____Compare____func_0_0);
    MR_init_entry(mercury__builtin____Compare____tuple_0_0);

#else   // ! MR_HIGHLEVEL_CODE

    mercury_builtin_types();

#endif  // MR_HIGHLEVEL_CODE

    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, int, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, uint, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, int8, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, uint8, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, int16, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, uint16, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, int32, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, uint32, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, string, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, float, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, character, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, void, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, c_pointer, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, pred, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, func, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, tuple, 0);
#ifndef MR_HIGHLEVEL_CODE
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, succip, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, hp, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, curfr, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, maxfr, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, redofr, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, redoip, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, trailptr, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(builtin, ticket, 0);
#endif
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, heap_pointer, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, ref, 1);
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, type_ctor_info, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, type_info, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, base_typeclass_info, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, typeclass_info, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(type_desc, type_ctor_desc, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(type_desc, pseudo_type_desc, 0);
    MR_INIT_TYPE_CTOR_INFO_MNA(type_desc, type_desc, 0);
}

void
mercury_sys_init_mercury_builtin_types_init_type_tables(void)
{
    MR_REGISTER_TYPE_CTOR_INFO(builtin, int, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, uint, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, int8, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, uint8, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, int16, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, uint16, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, int32, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, uint32, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, string, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, float, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, character, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, void, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, c_pointer, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, pred, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, func, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, tuple, 0);
#ifndef MR_HIGHLEVEL_CODE
    MR_REGISTER_TYPE_CTOR_INFO(builtin, succip, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, hp, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, curfr, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, maxfr, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, redofr, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, redoip, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, trailptr, 0);
    MR_REGISTER_TYPE_CTOR_INFO(builtin, ticket, 0);
#endif
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, heap_pointer, 0);
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, ref, 1);
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, type_ctor_info, 0);
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, type_info, 0);
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, base_typeclass_info, 0);
    MR_REGISTER_TYPE_CTOR_INFO(private_builtin, typeclass_info, 0);
    MR_REGISTER_TYPE_CTOR_INFO(type_desc, type_ctor_desc, 0);
    MR_REGISTER_TYPE_CTOR_INFO(type_desc, pseudo_type_desc, 0);
    MR_REGISTER_TYPE_CTOR_INFO(type_desc, type_desc, 0);
}

#ifdef  MR_DEEP_PROFILING
void
mercury_sys_init_mercury_builtin_types_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp)
{
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, int, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, uint, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, int8, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, uint8, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, int16, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, uint16, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, int32, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, uint32, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, string, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, float, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, character, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, void, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, c_pointer, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, pred, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, func, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, tuple, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, succip, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, hp, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, curfr, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, maxfr, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, redofr, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, redoip, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, trailptr, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, ticket, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin,
        heap_pointer, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin, ref, 1);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin,
        type_ctor_info, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin, type_info, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin,
        base_typeclass_info, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, private_builtin,
        typeclass_info, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, type_desc, type_ctor_desc, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, type_desc, pseudo_type_desc, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, type_desc, type_desc, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, user_by_rtti, 0);
    MR_WRITE_OUT_PROC_STATIC_LAYOUTS(deep_fp, builtin, dummy, 0);
}
#endif // MR_DEEP_PROFILING

////////////////////////////////////////////////////////////////////////////
