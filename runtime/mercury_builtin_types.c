/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_builtin_types.c
**
** This file defines the operations on the builtin types of Mercury.
** It has separate implementations for the high level and low level C back
** ends.
*/

#ifdef MR_HIGHLEVEL_CODE
  #include "mercury.h"
#else
  #include "mercury_imp.h"
#endif

#include "mercury_type_info.h"	/* for MR_TYPECTOR_REP* */
#include "mercury_type_desc.h"	/* for MR_TypeCtorDesc */
#include "mercury_misc.h"	/* for MR_fatal_error() */
#include "mercury_heap.h"	/* for MR_create[1-3]() prototypes */
#include "mercury_deep_profiling.h"
#include "mercury_deep_profiling_hand.h"
#include "mercury_profiling_builtin.h"
#include "mercury_builtin_types.h"

/*---------------------------------------------------------------------------*/

#ifdef	MR_DEEP_PROFILING

MR_DEFINE_PROC_STATICS(builtin, int, 0);
MR_DEFINE_PROC_STATICS(builtin, string, 0);
MR_DEFINE_PROC_STATICS(builtin, float, 0);
MR_DEFINE_PROC_STATICS(builtin, character, 0);
MR_DEFINE_PROC_STATICS(builtin, void, 0);
MR_DEFINE_PROC_STATICS(builtin, c_pointer, 0);
MR_DEFINE_PROC_STATICS(builtin, pred, 0);
MR_DEFINE_PROC_STATICS(builtin, func, 0);
MR_DEFINE_PROC_STATICS(builtin, tuple, 0);
MR_DEFINE_PROC_STATICS(builtin, succip, 0);
MR_DEFINE_PROC_STATICS(builtin, hp, 0);
MR_DEFINE_PROC_STATICS(builtin, curfr, 0);
MR_DEFINE_PROC_STATICS(builtin, maxfr, 0);
MR_DEFINE_PROC_STATICS(builtin, redofr, 0);
MR_DEFINE_PROC_STATICS(builtin, redoip, 0);
MR_DEFINE_PROC_STATICS(builtin, trailptr, 0);
MR_DEFINE_PROC_STATICS(builtin, ticket, 0);
MR_DEFINE_PROC_STATICS(private_builtin, heap_pointer, 0);
MR_DEFINE_PROC_STATICS(private_builtin, type_ctor_info, 1);
MR_DEFINE_PROC_STATICS(private_builtin, type_info, 1);
MR_DEFINE_PROC_STATICS(private_builtin, base_typeclass_info, 1);
MR_DEFINE_PROC_STATICS(private_builtin, typeclass_info, 1);
MR_DEFINE_PROC_STATICS(type_desc, type_ctor_desc, 0);
MR_DEFINE_PROC_STATICS(type_desc, type_desc, 0);

#endif

/*
** Define MR_TypeCtorInfos for the builtin types
*/

MR_DEFINE_TYPE_CTOR_INFO(builtin, int, 0, INT);
MR_DEFINE_TYPE_CTOR_INFO(builtin, character, 0, CHAR);
MR_DEFINE_TYPE_CTOR_INFO(builtin, string, 0, STRING);
MR_DEFINE_TYPE_CTOR_INFO(builtin, float, 0, FLOAT);
MR_DEFINE_TYPE_CTOR_INFO(builtin, void, 0, VOID);
MR_DEFINE_TYPE_CTOR_INFO(builtin, c_pointer, 0, C_POINTER);
MR_DEFINE_TYPE_CTOR_INFO(builtin, pred, 0, PRED);
MR_DEFINE_TYPE_CTOR_INFO(builtin, func, 0, FUNC);
MR_DEFINE_TYPE_CTOR_INFO(builtin, tuple, 0, TUPLE);
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
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, type_ctor_info, 1, TYPECTORINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, type_info, 1, TYPEINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, base_typeclass_info, 1,
	BASETYPECLASSINFO);
MR_DEFINE_TYPE_CTOR_INFO(private_builtin, typeclass_info, 1, TYPECLASSINFO);

MR_DEFINE_TYPE_CTOR_INFO(type_desc, type_ctor_desc, 0, TYPECTORDESC);
MR_DEFINE_TYPE_CTOR_INFO(type_desc, type_desc, 0, TYPEDESC);

/*---------------------------------------------------------------------------*/

#ifdef MR_HIGHLEVEL_CODE

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*
** Definitions of the type-specific __Unify__ and __Compare__ procedures
** for the builtin types.
**
** There are two versions of each of these.  The first version, __Unify__,
** which is called when the type is known at compile time,
** has the arguments unboxed.  The second version, do_unify_, which is
** stored in the type_ctor_info and called from the generic
** unify/2 or compare/3, is a wrapper that has the arguments boxed,
** and just calls the first version.
*/

/*---------------------------------------------------------------------------*/
/*
** Unification procedures with the arguments unboxed.
*/

MR_bool MR_CALL
mercury__builtin____Unify____int_0_0(MR_Integer x, MR_Integer y)
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
	/* XXX what should this function do when x and y are both NaNs? */
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
		"called unify/2 for `private_builtin:heap_pointer' type");
}

MR_bool MR_CALL
mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y)
{
	MR_fatal_error("called unify/2 for `func' type");
}

MR_bool MR_CALL
mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y)
{
	MR_fatal_error("called unify/2 for `pred' type");
}

MR_bool MR_CALL
mercury__builtin____Unify____tuple_0_0(MR_Tuple x, MR_Tuple y)
{
	MR_fatal_error("called unify/2 for `tuple' type");
}

MR_bool MR_CALL
mercury__type_desc____Unify____type_ctor_desc_0_0(
	MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y)
{
	return MR_unify_type_ctor_desc((MR_TypeCtorDesc) x,
		(MR_TypeCtorDesc) y);
}

MR_bool MR_CALL
mercury__type_desc____Unify____type_desc_0_0(MR_Type_Desc x, MR_Type_Desc y)
{
	return MR_unify_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
	return MR_unify_type_ctor_info((MR_TypeCtorInfo) x,
		(MR_TypeCtorInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
	return MR_unify_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
	MR_fatal_error("called unify/2 for `typeclass_info' type");
}

MR_bool MR_CALL
mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
	MR_SORRY("unify for base_typeclass_info");
}

/*---------------------------------------------------------------------------*/

/*
** Comparison procedures with the arguments unboxed.
*/

void MR_CALL
mercury__builtin____Compare____int_0_0(
	MR_Comparison_Result *result, MR_Integer x, MR_Integer y)
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
	/* XXX what should this function do when x and y are both NaNs? */
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
	*result = (x > y ? MR_COMPARE_GREATER :
		  x == y ? MR_COMPARE_EQUAL :
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
	MR_fatal_error(
		"called compare/3 for `private_builtin:heap_pointer' type");
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
mercury__type_desc____Compare____type_desc_0_0(
	MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y)
{
	*result = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
	*result = MR_compare_type_ctor_info((MR_TypeCtorInfo) x,
		(MR_TypeCtorInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
	*result = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
}

void MR_CALL
mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
	MR_fatal_error("called compare/3 for `typeclass_info' type");
}

void MR_CALL
mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
	MR_SORRY("compare for base_typeclass_info");
}

/*---------------------------------------------------------------------------*/

/*
** Unification procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static MR_bool MR_CALL
mercury__builtin__do_unify__int_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____int_0_0(
		(MR_Integer) x, (MR_Integer) y);
}

static MR_bool MR_CALL
mercury__builtin__do_unify__string_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____string_0_0(
		(MR_String) x, (MR_String) y);
}

static MR_bool MR_CALL
mercury__builtin__do_unify__float_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____float_0_0(
		MR_unbox_float(x), MR_unbox_float(y));
}

static MR_bool MR_CALL
mercury__builtin__do_unify__character_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____character_0_0(
		(MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

static MR_bool MR_CALL
mercury__builtin__do_unify__void_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify/2 for `void' type");
}

static MR_bool MR_CALL
mercury__builtin__do_unify__c_pointer_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____c_pointer_0_0(
		(MR_C_Pointer) x, (MR_C_Pointer) y);
}

static MR_bool MR_CALL
mercury__private_builtin__do_unify__heap_pointer_0_0(MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____heap_pointer_0_0(
		(MR_Heap_Pointer) x, (MR_Heap_Pointer) y);
}

static MR_bool MR_CALL
mercury__builtin__do_unify__func_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify/2 for `func' type");
}

static MR_bool MR_CALL
mercury__builtin__do_unify__pred_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify/2 for `pred' type");
}

static MR_bool MR_CALL
mercury__builtin__do_unify__tuple_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____tuple_0_0(
		(MR_Tuple) x, (MR_Tuple) y);
}

static MR_bool MR_CALL
mercury__type_desc__do_unify__type_ctor_desc_0_0(MR_Box x, MR_Box y)
{
	return mercury__type_desc____Unify____type_ctor_desc_0_0(
		(MR_Type_Ctor_Desc) x, (MR_Type_Ctor_Desc) y);
}

static MR_bool MR_CALL
mercury__type_desc__do_unify__type_desc_0_0(MR_Box x, MR_Box y)
{
	return mercury__type_desc____Unify____type_desc_0_0(
		(MR_Type_Desc) x, (MR_Type_Desc) y);
}

static MR_bool MR_CALL
mercury__private_builtin__do_unify__type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____type_ctor_info_1_0(
		type_info, (MR_Mercury_Type_Ctor_Info) x, (MR_Mercury_Type_Ctor_Info) y);
}

static MR_bool MR_CALL
mercury__private_builtin__do_unify__type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____type_info_1_0(
		type_info, (MR_Mercury_Type_Info) x, (MR_Mercury_Type_Info) y);
}

static MR_bool MR_CALL
mercury__private_builtin__do_unify__typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____typeclass_info_1_0(
		type_info, (MR_Mercury_TypeClass_Info) x, (MR_Mercury_TypeClass_Info) y);
}

static MR_bool MR_CALL
mercury__private_builtin__do_unify__base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____base_typeclass_info_1_0(
		type_info,
		(MR_Mercury_Base_TypeClass_Info) x,
		(MR_Mercury_Base_TypeClass_Info) y);
}

/*---------------------------------------------------------------------------*/

/*
** Comparison procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static void MR_CALL
mercury__builtin__do_compare__int_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____int_0_0(result,
		(MR_Integer) x, (MR_Integer) y);
}

static void MR_CALL
mercury__builtin__do_compare__string_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____string_0_0(result,
		(MR_String) x, (MR_String) y);
}

static void MR_CALL
mercury__builtin__do_compare__float_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____float_0_0(result,
		MR_unbox_float(x), MR_unbox_float(y));
}

static void MR_CALL
mercury__builtin__do_compare__character_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____character_0_0(
		result, (MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

static void MR_CALL
mercury__builtin__do_compare__void_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for `void' type");
}

static void MR_CALL
mercury__builtin__do_compare__c_pointer_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____c_pointer_0_0(
		result, (MR_C_Pointer) x, (MR_C_Pointer) y);
}

static void MR_CALL
mercury__private_builtin__do_compare__heap_pointer_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	MR_fatal_error(
		"called compare/3 for `private_builtin:heap_pointer' type");
}

static void MR_CALL
mercury__builtin__do_compare__func_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for `func' type");
}

static void MR_CALL
mercury__builtin__do_compare__pred_0_0(MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for `pred' type");
}

static void MR_CALL
mercury__builtin__do_compare__tuple_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____tuple_0_0(
		result, (MR_Tuple) x, (MR_Tuple) y);
}

static void MR_CALL
mercury__type_desc__do_compare__type_ctor_desc_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__type_desc____Compare____type_ctor_desc_0_0(
		result, (MR_Type_Ctor_Desc) x, (MR_Type_Ctor_Desc) y);
}

static void MR_CALL
mercury__type_desc__do_compare__type_desc_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	mercury__type_desc____Compare____type_desc_0_0(
		result, (MR_Type_Desc) x, (MR_Type_Desc) y);
}

static void MR_CALL
mercury__private_builtin__do_compare__type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____type_ctor_info_1_0(
		type_info, result,
		(MR_Mercury_Type_Ctor_Info) x, (MR_Mercury_Type_Ctor_Info) y);
}

static void MR_CALL
mercury__private_builtin__do_compare__type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____type_info_1_0(
		type_info, result, (MR_Mercury_Type_Info) x, (MR_Mercury_Type_Info) y);
}

static void MR_CALL
mercury__private_builtin__do_compare__typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____typeclass_info_1_0(
		type_info, result,
		(MR_Mercury_TypeClass_Info) x, (MR_Mercury_TypeClass_Info) y);
}

static void MR_CALL
mercury__private_builtin__do_compare__base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____base_typeclass_info_1_0(
		type_info, result,
		(MR_Mercury_Base_TypeClass_Info) x,
		(MR_Mercury_Base_TypeClass_Info) y);
}

#else	/* ! MR_HIGHLEVEL_CODE */

MR_MODULE_STATIC_OR_EXTERN MR_ModuleFunc mercury_builtin_types;

MR_UNIFY_COMPARE_DEFNS(builtin, int, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, string, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, float, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, character, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, void, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, c_pointer, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, pred, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, func, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, tuple, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, succip, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, hp, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, curfr, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, maxfr, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, redofr, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, redoip, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, trailptr, 0)
MR_UNIFY_COMPARE_DEFNS(builtin, ticket, 0)
MR_UNIFY_COMPARE_DEFNS(private_builtin, heap_pointer, 0)
MR_UNIFY_COMPARE_DEFNS(private_builtin, type_ctor_info, 1)
MR_UNIFY_COMPARE_DEFNS(private_builtin, type_info, 1)
MR_UNIFY_COMPARE_DEFNS(private_builtin, base_typeclass_info, 1)
MR_UNIFY_COMPARE_DEFNS(private_builtin, typeclass_info, 1)
MR_UNIFY_COMPARE_DEFNS(type_desc, type_ctor_desc, 0)
MR_UNIFY_COMPARE_DEFNS(type_desc, type_desc, 0)

MR_BEGIN_MODULE(mercury_builtin_types)
	MR_UNIFY_COMPARE_LABELS(builtin, int, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, string, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, float, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, character, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, void, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, c_pointer, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, pred, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, func, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, tuple, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, succip, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, hp, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, curfr, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, maxfr, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, redofr, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, trailptr, 0)
	MR_UNIFY_COMPARE_LABELS(builtin, ticket, 0)
	MR_UNIFY_COMPARE_LABELS(private_builtin, heap_pointer, 0)
	MR_UNIFY_COMPARE_LABELS(private_builtin, type_ctor_info, 1)
	MR_UNIFY_COMPARE_LABELS(private_builtin, type_info, 1)
	MR_UNIFY_COMPARE_LABELS(private_builtin, base_typeclass_info, 1)
	MR_UNIFY_COMPARE_LABELS(private_builtin, typeclass_info, 1)
MR_BEGIN_CODE

/*****************************************************************************/

#define	module		builtin
#define	type		int
#define	arity		0
#define	unify_code	MR_r1 = ((MR_Integer) MR_r1 == (MR_Integer) MR_r2);
#define	compare_code	MR_r1 = ((MR_Integer) MR_r1 == (MR_Integer) MR_r2 ? \
					MR_COMPARE_EQUAL : 		    \
				(MR_Integer) MR_r1 < (MR_Integer) MR_r2 ?   \
					MR_COMPARE_LESS :    		    \
				MR_COMPARE_GREATER);

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

#define	module		builtin
#define	type		string
#define	arity		0
#define	unify_code	MR_r1 = strcmp((char *) MR_r1, (char *) MR_r2) == 0;
#define	compare_code	int result = strcmp((char *) MR_r1, (char *) MR_r2); \
			MR_r1 = (result > 0) ? MR_COMPARE_GREATER : 	     \
				(result < 0 ? MR_COMPARE_LESS :    	     \
				MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

#define	module		builtin
#define	type		float
#define	arity		0
#define	unify_code	MR_r1 = (MR_word_to_float(MR_r1) == MR_word_to_float(MR_r2));
#define	compare_code	MR_Float f1 = MR_word_to_float(MR_r1);		\
			MR_Float f2 = MR_word_to_float(MR_r2);		\
			MR_r1 = ((f1 > f2) ? MR_COMPARE_GREATER :	\
				(f1 < f2) ? MR_COMPARE_LESS :		\
				MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

#define	module		builtin
#define	type		character
#define	arity		0
#define	unify_code	MR_r1 = ((MR_Char) MR_r1 == (MR_Char) MR_r2);
#define	compare_code	MR_r1 = ((MR_Char) MR_r1 > (MR_Char) MR_r2 ?	\
					MR_COMPARE_GREATER : 		\
				(MR_Char) MR_r1 < (MR_Char) MR_r2 ?	\
					MR_COMPARE_LESS :    		\
				MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

#define	module		builtin
#define	type		void
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `void' type");
#define	compare_code	MR_fatal_error("called compare/3 for `void' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

	/*
	** For c_pointer, we assume that equality and comparison
	** can be based on object identity (i.e. using address comparisons).
	** This is correct for types like io__stream, and necessary since
	** the io__state contains a map(io__stream, filename).
	** However, it might not be correct in general...
	*/

#define	module		builtin
#define	type		c_pointer
#define	arity		0
#define	unify_code	MR_r1 = (MR_r1 == MR_r2);
#define	compare_code	MR_r1 = (MR_r1 > MR_r2 ? MR_COMPARE_GREATER : 	\
				MR_r1 < MR_r2 ? MR_COMPARE_LESS :    	\
				MR_COMPARE_EQUAL);

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/* Predicates cannot be unified or compared */

#define	module		builtin
#define	type		pred
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `pred' type");
#define	compare_code	MR_fatal_error("called compare/3 for `pred' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/* Functions cannot be unified or compared */

#define	module		builtin
#define	type		func
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `func' type");
#define	compare_code	MR_fatal_error("called compare/3 for `func' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of tuples are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		tuple
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `tuple' type");
#define	compare_code	MR_fatal_error("called compare/3 for `tuple' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of succips are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		succip
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `succip' type");
#define	compare_code	MR_fatal_error("called compare/3 for `succip' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of hps are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		hp
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `hp' type");
#define	compare_code	MR_fatal_error("called compare/3 for `hp' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of curfrs are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		curfr
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `curfr' type");
#define	compare_code	MR_fatal_error("called compare/3 for `curfr' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of maxfrs are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		maxfr
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `maxfr' type");
#define	compare_code	MR_fatal_error("called compare/3 for `maxfr' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of redofrs are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		redofr
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `redofr' type");
#define	compare_code	MR_fatal_error("called compare/3 for `redofr' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of redoips are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		redoip
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `redoip' type");
#define	compare_code	MR_fatal_error("called compare/3 for `redoip' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of trailptrs are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		trailptr
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `trailptr' type");
#define	compare_code	MR_fatal_error("called compare/3 for `trailptr' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of tickets are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		builtin
#define	type		ticket
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `ticket' type");
#define	compare_code	MR_fatal_error("called compare/3 for `ticket' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of heap_pointers are always handled by the generic unify/2
** and compare/3 predicates.
*/

#define	module		private_builtin
#define	type		heap_pointer
#define	arity		0
#define	unify_code	MR_fatal_error("called unify/2 for `heap_pointer' type");
#define	compare_code	MR_fatal_error("called compare/3 for `heap_pointer' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of type_ctor_infos are usually handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		private_builtin
#define	type		type_ctor_info
#define	arity		1
#define	unify_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_ctor_info(		\
				(MR_TypeCtorInfo) MR_r1,		\
				(MR_TypeCtorInfo) MR_r2);		\
			MR_restore_transient_registers();		\
			MR_r1 = (comp == MR_COMPARE_EQUAL);
#define	compare_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_ctor_info(		\
				(MR_TypeCtorInfo) MR_r1,		\
				(MR_TypeCtorInfo) MR_r2);		\
			MR_restore_transient_registers();		\
			MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of type_infos are usually handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		private_builtin
#define	type		type_info
#define	arity		1
#define	unify_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_info(			\
				(MR_TypeInfo) MR_r1,			\
				(MR_TypeInfo) MR_r2);			\
			MR_restore_transient_registers();		\
			MR_r1 = (comp == MR_COMPARE_EQUAL);
#define	compare_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_info(			\
				(MR_TypeInfo) MR_r1,			\
				(MR_TypeInfo) MR_r2);			\
			MR_restore_transient_registers();		\
			MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of base_typeclass_infos are always handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		private_builtin
#define	type		base_typeclass_info
#define	arity		1
#define	unify_code	MR_fatal_error("called unify/2 for `base_typeclass_info' type");
#define	compare_code	MR_fatal_error("called compare/3 for `base_typeclass_info' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of typeclass_infos are always handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		private_builtin
#define	type		typeclass_info
#define	arity		1
#define	unify_code	MR_fatal_error("called unify/2 for `base_typeclass_info' type");
#define	compare_code	MR_fatal_error("called compare/3 for `base_typeclass_info' type");

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of type_ctor_descs are usually handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		type_desc
#define	type		type_ctor_desc
#define	arity		0

#define	unify_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_ctor_desc(		\
				(MR_TypeCtorDesc) MR_r1,		\
				(MR_TypeCtorDesc) MR_r2);		\
			MR_restore_transient_registers();		\
			MR_r1 = (comp == MR_COMPARE_EQUAL);

#define	compare_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_ctor_desc(		\
				(MR_TypeCtorDesc) MR_r1,		\
				(MR_TypeCtorDesc) MR_r2);		\
			MR_restore_transient_registers();		\
			MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

/*
** Unify and compare of type_descs are usually handled by the generic
** unify/2 and compare/3 predicates.
*/

#define	module		type_desc
#define	type		type_desc
#define	arity		0

#define	unify_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_info(			\
				(MR_TypeInfo) MR_r1,			\
				(MR_TypeInfo) MR_r2);			\
			MR_restore_transient_registers();		\
			MR_r1 = (comp == MR_COMPARE_EQUAL);

#define	compare_code	int	comp;					\
									\
			MR_save_transient_registers();			\
			comp = MR_compare_type_info(			\
				(MR_TypeInfo) MR_r1,			\
				(MR_TypeInfo) MR_r2);			\
			MR_restore_transient_registers();		\
			MR_r1 = comp;

#include "mercury_hand_unify_compare_body.h"

#undef	module
#undef	type
#undef	arity
#undef	unify_code
#undef	compare_code

/*****************************************************************************/

MR_END_MODULE

#endif /* ! MR_HIGHLEVEL_CODE */

/*---------------------------------------------------------------------------*/

/*
INIT mercury_sys_init_mercury_builtin_types
ENDINIT
*/

/* forward decls, to suppress gcc -Wmissing-decl warnings. */
void mercury_sys_init_mercury_builtin_types_init(void);
void mercury_sys_init_mercury_builtin_types_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_mercury_builtin_types_write_out_proc_statics(FILE *fp);
#endif

void
mercury_sys_init_mercury_builtin_types_init(void)
{
#ifdef MR_HIGHLEVEL_CODE

	/*
	** We need to call MR_init_entry() for the unification and comparison
	** predicates for the types that are automatically predefined
	** by the type checker.
	*/
	
	MR_init_entry(mercury__builtin____Unify____int_0_0);
	MR_init_entry(mercury__builtin____Unify____string_0_0);
	MR_init_entry(mercury__builtin____Unify____float_0_0);
	MR_init_entry(mercury__builtin____Unify____character_0_0);
	MR_init_entry(mercury__builtin____Unify____void_0_0);
	MR_init_entry(mercury__builtin____Unify____c_pointer_0_0);
	MR_init_entry(mercury__builtin____Unify____pred_0_0);
	MR_init_entry(mercury__builtin____Unify____func_0_0);
	MR_init_entry(mercury__builtin____Unify____tuple_0_0);

	MR_init_entry(mercury__builtin____Compare____int_0_0);
	MR_init_entry(mercury__builtin____Compare____float_0_0);
	MR_init_entry(mercury__builtin____Compare____string_0_0);
	MR_init_entry(mercury__builtin____Compare____character_0_0);
	MR_init_entry(mercury__builtin____Compare____void_0_0);
	MR_init_entry(mercury__builtin____Compare____c_pointer_0_0);
	MR_init_entry(mercury__builtin____Compare____pred_0_0);
	MR_init_entry(mercury__builtin____Compare____func_0_0);
	MR_init_entry(mercury__builtin____Compare____tuple_0_0);

#else	/* ! MR_HIGHLEVEL_CODE */

	mercury_builtin_types();

#endif	/* MR_HIGHLEVEL_CODE */

	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, int, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, string, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, float, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, character, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, void, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, c_pointer, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, pred, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, func, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(builtin, tuple, 0);
#ifndef	MR_HIGHLEVEL_CODE
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
	MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, type_ctor_info, 1);
	MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, type_info, 1);
	MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, base_typeclass_info, 1);
	MR_INIT_TYPE_CTOR_INFO_MNA(private_builtin, typeclass_info, 1);
	MR_INIT_TYPE_CTOR_INFO_MNA(type_desc, type_ctor_desc, 0);
	MR_INIT_TYPE_CTOR_INFO_MNA(type_desc, type_desc, 0);
}

void
mercury_sys_init_mercury_builtin_types_init_type_tables(void)
{
	MR_REGISTER_TYPE_CTOR_INFO(builtin, int, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, string, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, float, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, character, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, void, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, c_pointer, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, pred, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, func, 0);
	MR_REGISTER_TYPE_CTOR_INFO(builtin, tuple, 0);
#ifndef	MR_HIGHLEVEL_CODE
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
	MR_REGISTER_TYPE_CTOR_INFO(private_builtin, type_ctor_info, 1);
	MR_REGISTER_TYPE_CTOR_INFO(private_builtin, type_info, 1);
	MR_REGISTER_TYPE_CTOR_INFO(private_builtin, base_typeclass_info, 1);
	MR_REGISTER_TYPE_CTOR_INFO(private_builtin, typeclass_info, 1);
	MR_REGISTER_TYPE_CTOR_INFO(type_desc, type_ctor_desc, 0);
	MR_REGISTER_TYPE_CTOR_INFO(type_desc, type_desc, 0);
}

#ifdef	MR_DEEP_PROFILING
void
mercury_sys_init_mercury_builtin_types_write_out_proc_statics(FILE *fp)
{
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, int, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, string, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, float, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, character, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, void, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, c_pointer, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, pred, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, func, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, tuple, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, succip, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, hp, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, curfr, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, maxfr, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, redofr, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, redoip, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, trailptr, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, builtin, ticket, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, private_builtin, heap_pointer, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, private_builtin, type_ctor_info, 1);
	MR_WRITE_OUT_PROC_STATICS(fp, private_builtin, type_info, 1);
	MR_WRITE_OUT_PROC_STATICS(fp, private_builtin, base_typeclass_info, 1);
	MR_WRITE_OUT_PROC_STATICS(fp, private_builtin, typeclass_info, 1);
	MR_WRITE_OUT_PROC_STATICS(fp, type_desc, type_ctor_desc, 0);
	MR_WRITE_OUT_PROC_STATICS(fp, type_desc, type_desc, 0);
}
#endif

/*---------------------------------------------------------------------------*/
