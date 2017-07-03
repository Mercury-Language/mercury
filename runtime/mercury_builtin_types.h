// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2002-2005 The University of Melbourne.
// This file may only be copied under the terms of the GNU Library General
// Public License - see the file COPYING.LIB in the Mercury distribution.

// mercury_builtin_types.h
//

#ifndef MERCURY_BUILTIN_TYPES_H
#define MERCURY_BUILTIN_TYPES_H

#include "mercury_types.h"
#include "mercury_std.h"            // for MR_CALL
#include "mercury_float.h"          // for MR_Float etc
#include "mercury_hlc_types.h"      // for MR_Mercury_Type_Info etc
#include "mercury_type_info.h"      // for MR_TypeCtorInfo_Struct

////////////////////////////////////////////////////////////////////////////
// Declarations of constants.
//
// Declare MR_TypeCtorInfo_Structs for the builtin types.

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, int, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, uint, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, int8, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, uint8, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, int16, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, uint16, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, int32, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, uint32, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, string, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, float, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, character, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, void, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, c_pointer, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, pred, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, func, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(builtin, tuple, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, heap_pointer, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, ref, 1));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, type_ctor_info, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, type_info, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, typeclass_info, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(private_builtin, base_typeclass_info, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(type_desc, type_ctor_desc, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(type_desc, pseudo_type_desc, 0));
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(
    MR_TYPE_CTOR_INFO_NAME(type_desc, type_desc, 0));

////////////////////////////////////////////////////////////////////////////

#ifdef MR_HIGHLEVEL_CODE

// Function declarations for manipulating builtin types. All these functions
// are specific to the high-level-code back-end.

MR_bool MR_CALL mercury__builtin____Unify____int_0_0(MR_Integer x,
                    MR_Integer y);
MR_bool MR_CALL mercury__builtin____Unify____uint_0_0(MR_Unsigned x,
                    MR_Unsigned y);
MR_bool MR_CALL mercury__builtin____Unify____int8_0_0(int8_t x,
                    int8_t y);
MR_bool MR_CALL mercury__builtin____Unify____uint8_0_0(uint8_t x,
                    uint8_t y);
MR_bool MR_CALL mercury__builtin____Unify____int16_0_0(int16_t x,
                    int16_t y);
MR_bool MR_CALL mercury__builtin____Unify____uint16_0_0(uint16_t x,
                    uint16_t y);
MR_bool MR_CALL mercury__builtin____Unify____int32_0_0(int32_t x,
                    int32_t y);
MR_bool MR_CALL mercury__builtin____Unify____uint32_0_0(uint32_t x,
                    uint32_t y);
MR_bool MR_CALL mercury__builtin____Unify____string_0_0(MR_String x,
                    MR_String y);
MR_bool MR_CALL mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y);
MR_bool MR_CALL mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char);
MR_bool MR_CALL mercury__builtin____Unify____void_0_0(MR_Void x, MR_Void y);
MR_bool MR_CALL mercury__builtin____Unify____c_pointer_0_0(
                    MR_C_Pointer x, MR_C_Pointer y);
MR_bool MR_CALL mercury__private_builtin____Unify____heap_pointer_0_0(
                    MR_Heap_Pointer x, MR_Heap_Pointer y);
MR_bool MR_CALL mercury__private_builtin____Unify____ref_1_0(
                    MR_Mercury_Type_Info type_info,
                    MR_Reference x, MR_Reference y);
MR_bool MR_CALL mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y);
MR_bool MR_CALL mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y);
MR_bool MR_CALL mercury__builtin____Unify____tuple_0_0(MR_Tuple x, MR_Tuple y);
MR_bool MR_CALL mercury__type_desc____Unify____type_ctor_desc_0_0(
                    MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y);
MR_bool MR_CALL mercury__type_desc____Unify____pseudo_type_desc_0_0(
                    MR_Pseudo_Type_Desc x, MR_Pseudo_Type_Desc y);
MR_bool MR_CALL mercury__type_desc____Unify____type_desc_0_0(
                    MR_Type_Desc x, MR_Type_Desc y);
MR_bool MR_CALL mercury__private_builtin____Unify____type_ctor_info_0_0(
                    MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y);
MR_bool MR_CALL mercury__private_builtin____Unify____type_info_0_0(
                    MR_Mercury_Type_Info x, MR_Mercury_Type_Info y);
MR_bool MR_CALL mercury__private_builtin____Unify____typeclass_info_0_0(
                    MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y);
MR_bool MR_CALL mercury__private_builtin____Unify____base_typeclass_info_0_0(
                    MR_Mercury_Base_TypeClass_Info x,
                    MR_Mercury_Base_TypeClass_Info y);

void MR_CALL    mercury__builtin____Compare____int_0_0(
                    MR_Comparison_Result *result, MR_Integer x, MR_Integer y);
void MR_CALL    mercury__builtin____Compare____uint_0_0(
                    MR_Comparison_Result *result, MR_Unsigned x, MR_Unsigned y);
void MR_CALL    mercury__builtin____Compare____int8_0_0(
                    MR_Comparison_Result *result, int8_t x, int8_t y);
void MR_CALL    mercury__builtin____Compare____uint8_0_0(
                    MR_Comparison_Result *result, uint8_t x, uint8_t y);
void MR_CALL    mercury__builtin____Compare____int16_0_0(
                    MR_Comparison_Result *result, int16_t x, int16_t y);
void MR_CALL    mercury__builtin____Compare____uint16_0_0(
                    MR_Comparison_Result *result, uint16_t x, uint16_t y);
void MR_CALL    mercury__builtin____Compare____int32_0_0(
                    MR_Comparison_Result *result, int32_t x, int32_t y);
void MR_CALL    mercury__builtin____Compare____uint32_0_0(
                    MR_Comparison_Result *result, uint32_t x, uint32_t y);
void MR_CALL    mercury__builtin____Compare____string_0_0(
                    MR_Comparison_Result *result, MR_String x, MR_String y);
void MR_CALL    mercury__builtin____Compare____float_0_0(
                    MR_Comparison_Result *result, MR_Float x, MR_Float y);
void MR_CALL    mercury__builtin____Compare____character_0_0(
                    MR_Comparison_Result *result, MR_Char x, MR_Char y);
void MR_CALL    mercury__builtin____Compare____void_0_0(
                    MR_Comparison_Result *result, MR_Void x, MR_Void y);
void MR_CALL    mercury__builtin____Compare____c_pointer_0_0(
                    MR_Comparison_Result *result,
                    MR_C_Pointer x, MR_C_Pointer y);
void MR_CALL    mercury__private_builtin____Compare____heap_pointer_0_0(
                    MR_Comparison_Result *result,
                    MR_Heap_Pointer x, MR_Heap_Pointer y);
void MR_CALL    mercury__private_builtin____Compare____ref_1_0(
                    MR_Mercury_Type_Info type_info,
                    MR_Comparison_Result *result,
                    MR_Reference x, MR_Reference y);
void MR_CALL    mercury__builtin____Compare____func_0_0(
                    MR_Comparison_Result *result, MR_Func x, MR_Func y);
void MR_CALL    mercury__builtin____Compare____pred_0_0(
                    MR_Comparison_Result *result, MR_Pred x, MR_Pred y);
void MR_CALL    mercury__builtin____Compare____tuple_0_0(
                    MR_Comparison_Result *result, MR_Tuple x, MR_Tuple y);
void MR_CALL    mercury__type_desc____Compare____type_ctor_desc_0_0(
                    MR_Comparison_Result *result,
                    MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y);
void MR_CALL    mercury__type_desc____Compare____pseudo_type_desc_0_0(
                    MR_Comparison_Result *result,
                    MR_Pseudo_Type_Desc x, MR_Pseudo_Type_Desc y);
void MR_CALL    mercury__type_desc____Compare____type_desc_0_0(
                    MR_Comparison_Result *result,
                    MR_Type_Desc x, MR_Type_Desc y);
void MR_CALL    mercury__private_builtin____Compare____type_ctor_info_0_0(
                    MR_Comparison_Result *result,
                    MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y);
void MR_CALL    mercury__private_builtin____Compare____type_info_0_0(
                    MR_Comparison_Result *result,
                    MR_Mercury_Type_Info x, MR_Mercury_Type_Info y);
void MR_CALL    mercury__private_builtin____Compare____typeclass_info_0_0(
                    MR_Comparison_Result *result,
                    MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y);
void MR_CALL    mercury__private_builtin____Compare____base_typeclass_info_0_0(
                    MR_Comparison_Result *result,
                    MR_Mercury_Base_TypeClass_Info x,
                    MR_Mercury_Base_TypeClass_Info y);

#endif // MR_HIGHLEVEL_CODE

#endif // not MERCURY_BUILTIN_TYPES_H
