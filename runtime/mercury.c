/*
** Copyright (C) 1999-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury.c - This file defines the builtin functions, constants, etc. that
** are used when generating high-level C code.
** (For the low-level C code, see mercury_imp.h.)
*/

#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif

#include "mercury.h"
#include "mercury_type_info.h"	/* for MR_TYPECTOR_REP* */
#include "mercury_misc.h"	/* for MR_fatal_error() */
#include "mercury_heap.h"	/* for MR_create[1-3]() prototypes */

#ifdef MR_HIGHLEVEL_CODE

/*---------------------------------------------------------------------------*/
/*
** Variable definitions
*/

#ifdef MR_NATIVE_GC
  void *mercury__private_builtin__stack_chain;
#endif

MR_Word mercury__private_builtin__dummy_var;

/*---------------------------------------------------------------------------*/
/*
** Type definitions
*/

/* Types for the wrapper versions of type-specific unify/compare procedures. */

typedef MR_bool MR_CALL MR_UnifyFunc_0(MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_1(MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_2(MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_3(MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Mercury_Type_Info,
				MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_4(MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Box, MR_Box);
typedef MR_bool MR_CALL MR_UnifyFunc_5(MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Mercury_Type_Info,
				MR_Mercury_Type_Info, MR_Mercury_Type_Info,
				MR_Box, MR_Box);

typedef void MR_CALL MR_CompareFunc_0(MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_1(MR_Mercury_Type_Info,
			MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_2(MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Comparison_Result *,
			MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_3(MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Mercury_Type_Info,
			MR_Comparison_Result *, MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_4(MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Comparison_Result *,
			MR_Box, MR_Box);
typedef void MR_CALL MR_CompareFunc_5(MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Mercury_Type_Info,
			MR_Mercury_Type_Info, MR_Mercury_Type_Info,
			MR_Comparison_Result *, MR_Box, MR_Box);

/*---------------------------------------------------------------------------*/
/*
** Forward declarations of static functions.
** These functions are used in the initializers
** for the type_ctor_info constants defined below.
*/

static MR_UnifyFunc_0
	mercury__builtin__do_unify__int_0_0,
	mercury__builtin__do_unify__string_0_0,
	mercury__builtin__do_unify__float_0_0,
	mercury__builtin__do_unify__character_0_0,
	mercury__builtin__do_unify__void_0_0,
	mercury__builtin__do_unify__c_pointer_0_0,
	mercury__private_builtin__do_unify__heap_pointer_0_0,
	mercury__builtin__do_unify__func_0_0,
	mercury__builtin__do_unify__pred_0_0,
	mercury__type_desc__do_unify__type_desc_0_0;

static MR_UnifyFunc_1
	mercury__builtin__do_unify__tuple_0_0,
	mercury__private_builtin__do_unify__type_ctor_info_1_0,
	mercury__private_builtin__do_unify__type_info_1_0,
	mercury__private_builtin__do_unify__typeclass_info_1_0,
	mercury__private_builtin__do_unify__base_typeclass_info_1_0;

static MR_CompareFunc_0
	mercury__builtin__do_compare__int_0_0,
	mercury__builtin__do_compare__string_0_0,
	mercury__builtin__do_compare__float_0_0,
	mercury__builtin__do_compare__character_0_0,
	mercury__builtin__do_compare__void_0_0,
	mercury__builtin__do_compare__c_pointer_0_0,
	mercury__private_builtin__do_compare__heap_pointer_0_0,
	mercury__builtin__do_compare__func_0_0,
	mercury__builtin__do_compare__pred_0_0,
	mercury__type_desc__do_compare__type_desc_0_0;

static MR_CompareFunc_1
	mercury__builtin__do_compare__tuple_0_0,
	mercury__private_builtin__do_compare__type_ctor_info_1_0,
	mercury__private_builtin__do_compare__type_info_1_0,
	mercury__private_builtin__do_compare__typeclass_info_1_0,
	mercury__private_builtin__do_compare__base_typeclass_info_1_0;

/*---------------------------------------------------------------------------*/
/*
** Constant definitions
*/

/*
** Define MR_TypeCtorInfos for the builtin types
*/

MR_define_type_ctor_info(builtin, int, 0, MR_TYPECTOR_REP_INT);
MR_define_type_ctor_info(builtin, string, 0, MR_TYPECTOR_REP_STRING);
MR_define_type_ctor_info(builtin, float, 0, MR_TYPECTOR_REP_FLOAT);
MR_define_type_ctor_info(builtin, character, 0, MR_TYPECTOR_REP_CHAR);
MR_define_type_ctor_info(builtin, void, 0, MR_TYPECTOR_REP_VOID);
MR_define_type_ctor_info(builtin, c_pointer, 0, MR_TYPECTOR_REP_C_POINTER);
MR_define_type_ctor_info(private_builtin, heap_pointer, 0, MR_TYPECTOR_REP_HP);
MR_define_type_ctor_info(builtin, pred, 0, MR_TYPECTOR_REP_PRED);
MR_define_type_ctor_info(builtin, func, 0, MR_TYPECTOR_REP_FUNC);
MR_define_type_ctor_info(builtin, tuple, 0, MR_TYPECTOR_REP_TUPLE);
MR_define_type_ctor_info(type_desc, type_desc, 0, MR_TYPECTOR_REP_TYPEINFO);
MR_define_type_ctor_info(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPECTORINFO);
MR_define_type_ctor_info(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);
MR_define_type_ctor_info(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);
MR_define_type_ctor_info(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_BASETYPECLASSINFO);

/*---------------------------------------------------------------------------*/

#define SORRY(msg) MR_fatal_error("Sorry, not yet implemented: " msg);

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*
** Function definitions
*/

/*
** Define the generic unify/2 and compare/3 functions.
*/

MR_bool MR_CALL
mercury__builtin__unify_2_p_0(MR_Mercury_Type_Info ti, MR_Box x, MR_Box y)
{
	MR_TypeInfo		type_info;
	MR_TypeCtorInfo		type_ctor_info;
	MR_TypeCtorRep		type_ctor_rep;
	int			arity;
	MR_TypeInfoParams	params;
	MR_Mercury_Type_Info	*args;

	type_info = (MR_TypeInfo) ti;
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

	/*
	** Tuple and higher-order types do not have a fixed arity,
	** so they need to be special cased here.
	*/
	type_ctor_rep = MR_type_ctor_rep(type_ctor_info);
	if (type_ctor_rep == MR_TYPECTOR_REP_TUPLE) {
		return mercury__builtin____Unify____tuple_0_0(ti,
			(MR_Tuple) x, (MR_Tuple) y);
	} else if (type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		return mercury__builtin____Unify____pred_0_0((MR_Pred) x,
			(MR_Pred) y);
	} else if (type_ctor_rep == MR_TYPECTOR_REP_FUNC) {
		return mercury__builtin____Unify____pred_0_0((MR_Pred) x,
			(MR_Pred) y);
	}

	arity = type_ctor_info->MR_type_ctor_arity;
	params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
	args = (MR_Mercury_Type_Info *) params;

	switch(arity) {
		/*
		** cast type_ctor_info->unify_pred to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: return ((MR_UnifyFunc_0 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(x, y);
		case 1: return ((MR_UnifyFunc_1 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(args[1], x, y);
		case 2: return ((MR_UnifyFunc_2 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(args[1], args[2], x, y);
		case 3: return ((MR_UnifyFunc_3 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(args[1], args[2], args[3],
				 x, y);
		case 4: return ((MR_UnifyFunc_4 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(args[1], args[2], args[3],
				 args[4], x, y);
		case 5: return ((MR_UnifyFunc_5 *)
				type_ctor_info->MR_type_ctor_unify_pred)
				(args[1], args[2], args[3],
				 args[4], args[5], x, y);
		default:
			MR_fatal_error(
				"unify/2: type arity > 5 not supported");
	}
}

void MR_CALL
mercury__builtin__compare_3_p_0(MR_Mercury_Type_Info ti,
	MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
	MR_TypeInfo		type_info;
	MR_TypeCtorInfo		type_ctor_info;
	MR_TypeCtorRep		type_ctor_rep;
	int			arity;
	MR_TypeInfoParams	params;
	MR_Mercury_Type_Info	*args;

	type_info = (MR_TypeInfo) ti;
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

	/*
	** Tuple and higher-order types do not have a fixed arity,
	** so they need to be special cased here.
	*/
	type_ctor_rep = MR_type_ctor_rep(type_ctor_info);
	if (type_ctor_rep == MR_TYPECTOR_REP_TUPLE) {
		mercury__builtin____Compare____tuple_0_0(ti,
			res, (MR_Tuple) x, (MR_Tuple) y);
		return;
	} else if (type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		mercury__builtin____Compare____pred_0_0(res,
			(MR_Pred) x, (MR_Pred) y);
	} else if (type_ctor_rep == MR_TYPECTOR_REP_FUNC) {
		mercury__builtin____Compare____pred_0_0(res,
			(MR_Pred) x, (MR_Pred) y);
	    	return;
	}

	arity = type_ctor_info->MR_type_ctor_arity;
	params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
	args = (MR_Mercury_Type_Info *) params;

	switch(arity) {
		/*
		** cast type_ctor_info->compare to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: ((MR_CompareFunc_0 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (res, x, y);
			 break;
		case 1: ((MR_CompareFunc_1 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (args[1], res, x, y);
			 break;
		case 2: ((MR_CompareFunc_2 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (args[1], args[2], res, x, y);
			 break;
		case 3: ((MR_CompareFunc_3 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (args[1], args[2], args[3], res, x, y);
			 break;
		case 4: ((MR_CompareFunc_4 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (args[1], args[2], args[3],
			  args[4], res, x, y);
			 break;
		case 5: ((MR_CompareFunc_5 *)
			 type_ctor_info->MR_type_ctor_compare_pred)
			 (args[1], args[2], args[3],
			  args[4], args[5], res, x, y);
			 break;
		default:
			MR_fatal_error(
				"index/2: type arity > 5 not supported");
	}
}

void MR_CALL
mercury__builtin__compare_3_p_1(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
	mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void MR_CALL
mercury__builtin__compare_3_p_2(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
	mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void MR_CALL
mercury__builtin__compare_3_p_3(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *res, MR_Box x, MR_Box y)
{
	mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

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
	MR_fatal_error("called unify for type `void'");
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
	MR_fatal_error("called unify for type `private_builtin:heap_pointer'");
}

MR_bool MR_CALL
mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y)
{
	MR_fatal_error("called unify for `func' type");
}

MR_bool MR_CALL
mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y)
{
	MR_fatal_error("called unify for `pred' type");
}

MR_bool MR_CALL
mercury__builtin____Unify____tuple_0_0(MR_Mercury_Type_Info ti,
	MR_Tuple x, MR_Tuple y)
{
	int i, arity;
	MR_bool result;
	MR_TypeInfo type_info;
	MR_TypeInfo arg_type_info;

	type_info = (MR_TypeInfo) ti;
	arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);

	for (i = 0; i < arity; i++) {
		/* type_infos are counted starting at one. */
		arg_type_info =
			MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info)[i + 1];
		result = mercury__builtin__unify_2_p_0(
			(MR_Mercury_Type_Info) arg_type_info, x[i], y[i]);
		if (result == MR_FALSE) {
			return MR_FALSE;
		}
	}
	return MR_TRUE;
}

MR_bool MR_CALL
mercury__type_desc____Unify____type_desc_0_0(MR_Type_Desc x, MR_Type_Desc y)
{
	int             comp;

	comp = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);

	return (comp == MR_COMPARE_EQUAL);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
	SORRY("unify for type_ctor_info");
}

MR_bool MR_CALL
mercury__private_builtin____Unify____type_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
	int             comp;

	comp = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);

	return (comp == MR_COMPARE_EQUAL);
}

MR_bool MR_CALL
mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
	MR_fatal_error("attempt to unify typeclass_info");
}

MR_bool MR_CALL
mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
	SORRY("unify for base_typeclass_info");
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
	MR_fatal_error("called compare/3 for type `void'");
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
		"called compare/3 for type `private_builtin:heap_pointer'");
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
mercury__builtin____Compare____tuple_0_0(MR_Mercury_Type_Info ti,
	MR_Comparison_Result *result, MR_Tuple x, MR_Tuple y)
{
	int i, arity;
	MR_TypeInfo type_info;
	MR_TypeInfo arg_type_info;

	type_info = (MR_TypeInfo) ti;
	arity = MR_TYPEINFO_GET_TUPLE_ARITY(type_info);

	for (i = 0; i < arity; i++) {
		/* type_infos are counted starting at one. */
		arg_type_info =
			MR_TYPEINFO_GET_TUPLE_ARG_VECTOR(type_info)[i + 1];
		mercury__builtin__compare_3_p_0((MR_Mercury_Type_Info) arg_type_info,
				result, x[i], y[i]);
		if (*result != MR_COMPARE_EQUAL) {
			return;
		}
	}
	*result = MR_COMPARE_EQUAL;
}

void MR_CALL
mercury__type_desc____Compare____type_desc_0_0(
	MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y)
{
	int             comp;

	comp = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
	*result = comp;
}

void MR_CALL
mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y)
{
	SORRY("compare for type_ctor_info");
}

void MR_CALL
mercury__private_builtin____Compare____type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y)
{
	int             comp;

	comp = MR_compare_type_info((MR_TypeInfo) x, (MR_TypeInfo) y);
	*result = comp;
}

void MR_CALL
mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y)
{
	MR_fatal_error("attempt to compare typeclass_info");
}

void MR_CALL
mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y)
{
	SORRY("compare for base_typeclass_info");
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
	MR_fatal_error("called unify for type `void'");
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
	MR_fatal_error("called unify for `func' type");
}

static MR_bool MR_CALL
mercury__builtin__do_unify__pred_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify for `pred' type");
}

static MR_bool MR_CALL
mercury__builtin__do_unify__tuple_0_0(MR_Mercury_Type_Info type_info,
		MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____tuple_0_0(
		type_info, (MR_Tuple) x, (MR_Tuple) y);
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
	MR_fatal_error("called compare/3 for type `void'");
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
		"called compare/3 for type `private_builtin:heap_pointer'");
}

static void MR_CALL
mercury__builtin__do_compare__func_0_0(
	MR_Comparison_Result *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for func type");
}

static void MR_CALL
mercury__builtin__do_compare__pred_0_0(MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for pred type");
}

static void MR_CALL
mercury__builtin__do_compare__tuple_0_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____tuple_0_0(
		type_info, result, (MR_Tuple) x, (MR_Tuple) y);
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

/*---------------------------------------------------------------------------*/

/*
** Provide definitions for functions declared `extern inline'.
** Note that this code duplicates the code in mercury.h/mercury_heap.h.
*/

MR_OUTLINE_DEFN(
	MR_Word
	MR_create1(MR_Word w1) 
,
	{
		MR_Word *p = (MR_Word *) MR_new_object(MR_Word,
			1 * sizeof(MR_Word), "create1");
		p[0] = w1;
		return (MR_Word) p;
	}
)

MR_OUTLINE_DEFN(
	MR_Word
	MR_create2(MR_Word w1, MR_Word w2) 
,
	{
		MR_Word *p = (MR_Word *) MR_new_object(MR_Word,
			2 * sizeof(MR_Word), "create2");
		p[0] = w1;
		p[1] = w2;
		return (MR_Word) p;
	}
)

MR_OUTLINE_DEFN(
	MR_Word
	MR_create3(MR_Word w1, MR_Word w2, MR_Word w3) 
,
	{
		MR_Word *p = (MR_Word *) MR_new_object(MR_Word,
			3 * sizeof(MR_Word), "create3");
		p[0] = w1;
		p[1] = w2;
		p[2] = w3;
		return (MR_Word) p;
	}
)

#if defined(MR_AVOID_MACROS) || !defined(__GNUC__)

MR_OUTLINE_DEFN(
	MR_Box
	MR_box_float(MR_Float f)
,
	{
		MR_Float *ptr;

		MR_make_hp_float_aligned();
		ptr = (MR_Float *) MR_new_object(MR_Float, sizeof(MR_Float),
			"float");
		*ptr = f;
		return (MR_Box) ptr;
	}
)

#endif /* MR_AVOID_MACROS || !__GNUC__ */

#if defined(MR_AVOID_MACROS)

MR_OUTLINE_DEFN(
	MR_Float
	MR_unbox_float(MR_Box b)
,
	{
		return *(MR_Float *)b;
	}
)

#endif /* MR_AVOID_MACROS */

/*
** This is exactly the same as MR_box_float(), except that
** it is unconditionally defined as an external function,
** not as a macro, static function, or inline function.
** It is used by the `--target asm' GCC back-end interface.
*/
MR_Box
MR_asm_box_float(MR_Float f)
{
	MR_Float *ptr;

	MR_make_hp_float_aligned();
	ptr = (MR_Float *) MR_new_object(MR_Float, sizeof(MR_Float), "float");
	*ptr = f;
	return (MR_Box) ptr;
}

#endif /* ! MR_HIGHLEVEL_CODE */

/*---------------------------------------------------------------------------*/

/*
INIT mercury_sys_init_mercury_hlc
ENDINIT
*/

/* forward decls, to suppress gcc -Wmissing-decl warnings. */
void mercury_sys_init_mercury_hlc_init(void);
void mercury_sys_init_mercury_hlc_init_type_tables(void);
#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_mercury_hlc_write_out_proc_statics(FILE *fp);
#endif

void
mercury_sys_init_mercury_hlc_init(void)
{
#ifdef MR_HIGHLEVEL_CODE
	/*
	** We need to call MR_init_entry() for the unification and comparison
	** predicates for builtin types.  Note that we don't need to do this
	** for types such as `c_pointer' which are declared in the library;
	** this is only needed for types which are automatically predefined
	** by the type checker.
	*/
	
	MR_init_entry(mercury__builtin____Unify____int_0_0);
	MR_init_entry(mercury__builtin____Unify____string_0_0);
	MR_init_entry(mercury__builtin____Unify____float_0_0);
	MR_init_entry(mercury__builtin____Unify____character_0_0);
	MR_init_entry(mercury__builtin____Unify____void_0_0);
	MR_init_entry(mercury__builtin____Unify____func_0_0);
	MR_init_entry(mercury__builtin____Unify____pred_0_0);

	MR_init_entry(mercury__builtin____Compare____int_0_0);
	MR_init_entry(mercury__builtin____Compare____string_0_0);
	MR_init_entry(mercury__builtin____Compare____float_0_0);
	MR_init_entry(mercury__builtin____Compare____character_0_0);
	MR_init_entry(mercury__builtin____Compare____void_0_0);
	MR_init_entry(mercury__builtin____Compare____func_0_0);
	MR_init_entry(mercury__builtin____Compare____pred_0_0);
#else
	/* no initialization needed */
#endif
}

void mercury_sys_init_mercury_hlc_init_type_tables(void)
{
	/* no types to register */
}

#ifdef	MR_DEEP_PROFILING
void mercury_sys_init_mercury_hlc_write_out_proc_statics(FILE *fp)
{
	/* no proc_statics to write out */
}
#endif

/*---------------------------------------------------------------------------*/
