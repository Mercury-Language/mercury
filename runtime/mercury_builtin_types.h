/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_builtin_types.h
**
*/

#ifndef MERCURY_BUILTIN_TYPES_H
#define MERCURY_BUILTIN_TYPES_H

/* Everything in this file is specific to the high-level-code back-end */
#ifdef MR_HIGHLEVEL_CODE

#include "mercury_types.h"
#include "mercury_std.h" 		/* for MR_CALL */
#include "mercury_float.h" 		/* for MR_Float etc */
#include "mercury_hlc_types.h" 		/* for MR_Mercury_Type_Info etc */
#include "mercury_type_info.h" 		/* for MR_TypeCtorInfo_Struct */

/*---------------------------------------------------------------------------*/
/*
** Declarations of constants
*/

/* declare MR_TypeCtorInfo_Structs for the builtin types */
extern const MR_TypeCtorInfo_Struct
	mercury__builtin__builtin__type_ctor_info_int_0,
	mercury__builtin__builtin__type_ctor_info_string_0,
	mercury__builtin__builtin__type_ctor_info_float_0,
	mercury__builtin__builtin__type_ctor_info_character_0,
	mercury__builtin__builtin__type_ctor_info_void_0,
	mercury__builtin__builtin__type_ctor_info_c_pointer_0,
	mercury__builtin__builtin__type_ctor_info_pred_0,
	mercury__builtin__builtin__type_ctor_info_func_0,
	mercury__builtin__builtin__type_ctor_info_tuple_0,
	mercury__private_builtin__private_builtin__type_ctor_info_heap_pointer_0,
	mercury__private_builtin__private_builtin__type_ctor_info_type_ctor_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_type_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_typeclass_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_base_typeclass_info_1,
	mercury__array__array__type_ctor_info_array_1,
	mercury__std_util__std_util__type_ctor_info_univ_0,
	mercury__type_desc__type_desc__type_ctor_info_type_ctor_desc_0,
	mercury__type_desc__type_desc__type_ctor_info_type_desc_0;

/*---------------------------------------------------------------------------*/
/*
** Function declarations
*/

MR_bool MR_CALL mercury__builtin____Unify____int_0_0(MR_Integer x,
	MR_Integer y); 
MR_bool MR_CALL mercury__builtin____Unify____string_0_0(MR_String x,
	MR_String y); 
MR_bool MR_CALL mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y); 
MR_bool MR_CALL mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char); 
MR_bool MR_CALL mercury__builtin____Unify____void_0_0(MR_Void x, MR_Void y); 
MR_bool MR_CALL mercury__builtin____Unify____c_pointer_0_0(
	MR_C_Pointer x, MR_C_Pointer y); 
MR_bool MR_CALL mercury__private_builtin____Unify____heap_pointer_0_0(
	MR_Heap_Pointer x, MR_Heap_Pointer y); 
MR_bool MR_CALL mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y); 
MR_bool MR_CALL mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y); 
MR_bool MR_CALL mercury__builtin____Unify____tuple_0_0(MR_Tuple x, MR_Tuple y); 
MR_bool MR_CALL mercury__type_desc____Unify____type_ctor_desc_0_0(
	MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y); 
MR_bool MR_CALL mercury__type_desc____Unify____type_desc_0_0(
	MR_Type_Desc x, MR_Type_Desc y); 
MR_bool MR_CALL mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____type_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Mercury_Base_TypeClass_Info x,
	MR_Mercury_Base_TypeClass_Info y); 

void MR_CALL mercury__builtin____Compare____int_0_0(
	MR_Comparison_Result *result, MR_Integer x, MR_Integer y);
void MR_CALL mercury__builtin____Compare____string_0_0(
	MR_Comparison_Result *result, MR_String x, MR_String y);
void MR_CALL mercury__builtin____Compare____float_0_0(
	MR_Comparison_Result *result, MR_Float x, MR_Float y);
void MR_CALL mercury__builtin____Compare____character_0_0(
	MR_Comparison_Result *result, MR_Char x, MR_Char y);
void MR_CALL mercury__builtin____Compare____void_0_0(
	MR_Comparison_Result *result, MR_Void x, MR_Void y);
void MR_CALL mercury__builtin____Compare____c_pointer_0_0(
	MR_Comparison_Result *result, MR_C_Pointer x, MR_C_Pointer y);
void MR_CALL mercury__private_builtin____Compare____heap_pointer_0_0(
	MR_Comparison_Result *result, MR_Heap_Pointer x, MR_Heap_Pointer y);
void MR_CALL mercury__builtin____Compare____func_0_0(
	MR_Comparison_Result *result, MR_Func x, MR_Func y);
void MR_CALL mercury__builtin____Compare____pred_0_0(
	MR_Comparison_Result *result, MR_Pred x, MR_Pred y); 
void MR_CALL mercury__builtin____Compare____tuple_0_0(
	MR_Comparison_Result *result, MR_Tuple x, MR_Tuple y); 
void MR_CALL mercury__type_desc____Compare____type_ctor_desc_0_0(
	MR_Comparison_Result *result,
	MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y);
void MR_CALL mercury__type_desc____Compare____type_desc_0_0(
	MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y);
void MR_CALL mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y);
void MR_CALL mercury__private_builtin____Compare____type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y);
void MR_CALL mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y);
void MR_CALL mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y);

/*---------------------------------------------------------------------------*/

#endif /* MR_HIGHLEVEL_CODE */

#endif /* not MERCURY_BUILTIN_TYPES_H */
