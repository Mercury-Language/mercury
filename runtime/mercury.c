/*
** Copyright (C) 1999-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury.c - This file defines the builtin functions, constants, etc. that
** are used when generating high-level C code.
** (For the low-level C code, see mercury_imp.h.)
*/

#include "mercury.h"
#include "mercury_type_info.h"	/* for MR_TYPECTOR_REP* */
#include "mercury_misc.h"	/* for MR_fatal_error() */

#ifdef MR_HIGHLEVEL_CODE

/*---------------------------------------------------------------------------*/
/*
** Variable definitions
*/

MR_Word mercury__private_builtin__dummy_var;

/*---------------------------------------------------------------------------*/
/*
** Type definitions
*/

/* Types for the wrapper versions of type-specific unify/compare procedures. */

typedef bool MR_UnifyFunc_0(MR_Box, MR_Box);
typedef bool MR_UnifyFunc_1(MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_2(MR_Word, MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_3(MR_Word, MR_Word, MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_4(MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Box);
typedef bool MR_UnifyFunc_5(MR_Word, MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Box);

typedef void MR_CompareFunc_0(MR_Word *, MR_Box, MR_Box);
typedef void MR_CompareFunc_1(MR_Word, MR_Word *, MR_Box, MR_Box);
typedef void MR_CompareFunc_2(MR_Word, MR_Word, MR_Word *, MR_Box, MR_Box);
typedef void MR_CompareFunc_3(MR_Word, MR_Word, MR_Word,
			      MR_Word *, MR_Box, MR_Box);
typedef void MR_CompareFunc_4(MR_Word, MR_Word, MR_Word, MR_Word,
			      MR_Word *, MR_Box, MR_Box);
typedef void MR_CompareFunc_5(MR_Word, MR_Word, MR_Word, MR_Word, MR_Word,
			      MR_Word *, MR_Box, MR_Box);

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
	mercury__builtin__do_unify__func_0_0,
	mercury__builtin__do_unify__pred_0_0,
	mercury__std_util__do_unify__univ_0_0,
	mercury__std_util__do_unify__type_desc_0_0;

static MR_UnifyFunc_1
	mercury__array__do_unify__array_1_0,
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
	mercury__builtin__do_compare__func_0_0,
	mercury__builtin__do_compare__pred_0_0,
	mercury__std_util__do_compare__univ_0_0,
	mercury__std_util__do_compare__type_desc_0_0;

static MR_CompareFunc_1
	mercury__array__do_compare__array_1_0,
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

#define MR_type_ctor_info_name(MODULE, TYPE, ARITY)			      \
	MR_PASTE2(mercury__,						      \
	MR_PASTE2(MODULE,						      \
	MR_PASTE2(__,							      \
	MR_PASTE2(MODULE,						      \
	MR_PASTE2(__type_ctor_info_,					      \
	MR_PASTE2(TYPE,							      \
	MR_PASTE2(_,							      \
	          ARITY)))))))

#define MR_type_ctor_info_func_name(MODULE, TYPE, ARITY, FUNC)		      \
	MR_PASTE2(mercury__,						      \
	MR_PASTE2(MODULE,						      \
	MR_PASTE2(__,							      \
	MR_PASTE2(FUNC,							      \
	MR_PASTE2(__,							      \
	MR_PASTE2(TYPE,							      \
	MR_PASTE2(_,							      \
	MR_PASTE2(ARITY,						      \
	          _0))))))))

#define MR_special_func_type(NAME, ARITY) \
	MR_PASTE2(MR_, MR_PASTE2(NAME, MR_PASTE2(Func_, ARITY)))

#define MR_define_type_ctor_info(module, type, arity, type_rep)		      \
	const struct MR_TypeCtorInfo_Struct				      \
		MR_type_ctor_info_name(module, type, arity) =		      \
	{								      \
		arity,							      \
		(MR_Box) MR_type_ctor_info_func_name(module, type, arity,     \
				do_unify),				      \
		(MR_Box) MR_type_ctor_info_func_name(module, type, arity,     \
				do_unify),				      \
		(MR_Box) MR_type_ctor_info_func_name(module, type, arity,     \
				do_compare),				      \
		type_rep,						      \
		NULL,							      \
		NULL,							      \
		MR_STRINGIFY(module),					      \
		MR_STRINGIFY(type),					      \
		MR_RTTI_VERSION,					      \
		{ 0 },							      \
		{ 0 },							      \
		-1,							      \
		-1							      \
	}

MR_define_type_ctor_info(builtin, int, 0, MR_TYPECTOR_REP_INT);
MR_define_type_ctor_info(builtin, string, 0, MR_TYPECTOR_REP_STRING);
MR_define_type_ctor_info(builtin, float, 0, MR_TYPECTOR_REP_FLOAT);
MR_define_type_ctor_info(builtin, character, 0, MR_TYPECTOR_REP_CHAR);
MR_define_type_ctor_info(builtin, void, 0, MR_TYPECTOR_REP_VOID);
MR_define_type_ctor_info(builtin, c_pointer, 0, MR_TYPECTOR_REP_C_POINTER);
MR_define_type_ctor_info(builtin, pred, 0, MR_TYPECTOR_REP_PRED);
MR_define_type_ctor_info(builtin, func, 0, MR_TYPECTOR_REP_PRED);
MR_define_type_ctor_info(array, array, 1, MR_TYPECTOR_REP_ARRAY);
MR_define_type_ctor_info(std_util, univ, 0, MR_TYPECTOR_REP_UNIV);
MR_define_type_ctor_info(std_util, type_desc, 0, MR_TYPECTOR_REP_TYPEINFO);
MR_define_type_ctor_info(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);
MR_define_type_ctor_info(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);
MR_define_type_ctor_info(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);
MR_define_type_ctor_info(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);

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

bool
mercury__builtin__unify_2_p_0(MR_Word ti, MR_Box x, MR_Box y)
{
	MR_TypeInfo		type_info;
	MR_TypeCtorInfo		type_ctor_info;
	int			arity;
	MR_TypeInfoParams	params;
	MR_Word			*args;

	type_info = (MR_TypeInfo) ti;
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
	if (type_ctor_info->type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		arity = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info);
		params = MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(type_info);
	} else {
		arity = type_ctor_info->arity;
		params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
	}
	args = (MR_Word *) params;

	switch(arity) {
		/*
		** cast type_ctor_info->unify_pred to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: return ((MR_UnifyFunc_0 *) type_ctor_info->unify_pred)
				(x, y);
		case 1: return ((MR_UnifyFunc_1 *) type_ctor_info->unify_pred)
				(args[1], x, y);
		case 2: return ((MR_UnifyFunc_2 *) type_ctor_info->unify_pred)
				(args[1], args[2], x, y);
		case 3: return ((MR_UnifyFunc_3 *) type_ctor_info->unify_pred)
				(args[1], args[2], args[3],
				 x, y);
		case 4: return ((MR_UnifyFunc_4 *) type_ctor_info->unify_pred)
				(args[1], args[2], args[3],
				 args[4], x, y);
		case 5: return ((MR_UnifyFunc_5 *) type_ctor_info->unify_pred)
				(args[1], args[2], args[3],
				 args[4], args[5], x, y);
		default:
			MR_fatal_error(
				"unify/2: type arity > 5 not supported");
	}
}

void
mercury__builtin__compare_3_p_0(MR_Word ti, MR_Word *res, MR_Box x, MR_Box y)
{
	MR_TypeInfo		type_info;
	MR_TypeCtorInfo		type_ctor_info;
	int			arity;
	MR_TypeInfoParams	params;
	MR_Word			*args;

	type_info = (MR_TypeInfo) ti;
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
	if (type_ctor_info->type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		arity = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info);
		params = MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(type_info);
	} else {
		arity = type_ctor_info->arity;
		params = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info);
	}
	args = (MR_Word *) params;

	switch(arity) {
		/*
		** cast type_ctor_info->compare to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: ((MR_CompareFunc_0 *) type_ctor_info->compare_pred)
			 (res, x, y);
			 break;
		case 1: ((MR_CompareFunc_1 *) type_ctor_info->compare_pred)
			 (args[1], res, x, y);
			 break;
		case 2: ((MR_CompareFunc_2 *) type_ctor_info->compare_pred)
			 (args[1], args[2], res, x, y);
			 break;
		case 3: ((MR_CompareFunc_3 *) type_ctor_info->compare_pred)
			 (args[1], args[2], args[3], res, x, y);
			 break;
		case 4: ((MR_CompareFunc_4 *) type_ctor_info->compare_pred)
			 (args[1], args[2], args[3],
			  args[4], res, x, y);
			 break;
		case 5: ((MR_CompareFunc_5 *) type_ctor_info->compare_pred)
			 (args[1], args[2], args[3],
			  args[4], args[5], res, x, y);
			 break;
		default:
			MR_fatal_error(
				"index/2: type arity > 5 not supported");
	}
}

void
mercury__builtin__compare_3_p_1(
	MR_Word type_info, MR_Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void
mercury__builtin__compare_3_p_2(
	MR_Word type_info, MR_Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void
mercury__builtin__compare_3_p_3(
	MR_Word type_info, MR_Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
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

bool
mercury__builtin____Unify____int_0_0(MR_Integer x, MR_Integer y)
{
	return x == y;
}

bool
mercury__builtin____Unify____string_0_0(MR_String x, MR_String y)
{
	return strcmp(x, y) == 0;
}

bool
mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y)
{
	/* XXX what should this function do when x and y are both NaNs? */
	return x == y;
}

bool
mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char y)
{
	return x == y;
}

bool
mercury__builtin____Unify____void_0_0(MR_Word x, MR_Word y)
{
	MR_fatal_error("called unify for type `void'");
}

bool
mercury__builtin____Unify____c_pointer_0_0(MR_Word x, MR_Word y)
{
	return (void *) x == (void *) y;
}

bool
mercury__builtin____Unify____func_0_0(MR_Word x, MR_Word y)
{
	MR_fatal_error("called unify for `func' type");
}

bool
mercury__builtin____Unify____pred_0_0(MR_Word x, MR_Word y)
{
	MR_fatal_error("called unify for `pred' type");
}

bool
mercury__array____Unify____array_1_0(MR_Word type_info, MR_Word x, MR_Word y)
{
	SORRY("unify for array");
}

bool
mercury__std_util____Unify____univ_0_0(MR_Word x, MR_Word y)
{
	SORRY("unify for univ");
}

bool
mercury__std_util____Unify____type_desc_0_0(MR_Word x, MR_Word y)
{
	SORRY("unify for type_desc");
}

bool
mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	SORRY("unify for type_ctor_info");
}

bool
mercury__private_builtin____Unify____type_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	SORRY("unify for type_info");
}

bool
mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	SORRY("unify for typeclass_info");
}

bool
mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Word type_info, MR_Word x, MR_Word y)
{
	SORRY("unify for base_typeclass_info");
}

/*---------------------------------------------------------------------------*/
/*
** Comparison procedures with the arguments unboxed.
*/

void
mercury__builtin____Compare____int_0_0(
	MR_Word *result, MR_Integer x, MR_Integer y)
{
	*result = (x > y ? MR_COMPARE_GREATER :
		  x == y ? MR_COMPARE_EQUAL :
		  MR_COMPARE_LESS);
}

void
mercury__builtin____Compare____string_0_0(MR_Word *result,
	MR_String x, MR_String y)
{
	int res = strcmp(x, y);
	*result = (res > 0 ? MR_COMPARE_GREATER :
		  res == 0 ? MR_COMPARE_EQUAL :
		  MR_COMPARE_LESS);
}

void
mercury__builtin____Compare____float_0_0(
	MR_Word *result, MR_Float x, MR_Float y)
{
	/* XXX what should this function do when x and y are both NaNs? */
	*result = (x > y ? MR_COMPARE_GREATER :
		  x == y ? MR_COMPARE_EQUAL :
		  x < y ? MR_COMPARE_LESS :
			  (MR_fatal_error("incomparable floats in compare/3"),
			  MR_COMPARE_EQUAL));
}

void
mercury__builtin____Compare____character_0_0(
	MR_Word *result, MR_Char x, MR_Char y)
{
	*result = (x > y ? MR_COMPARE_GREATER :
		  x == y ? MR_COMPARE_EQUAL :
		  MR_COMPARE_LESS);
}

void
mercury__builtin____Compare____void_0_0(MR_Word *result, MR_Word x, MR_Word y)
{
	MR_fatal_error("called compare/3 for type `void'");
}

void
mercury__builtin____Compare____c_pointer_0_0(
	MR_Word *result, MR_Word x, MR_Word y)
{
	*result = 
		( (void *) x == (void *) y ? MR_COMPARE_EQUAL
		: (void *) x <  (void *) y ? MR_COMPARE_LESS
		: MR_COMPARE_GREATER
		);
}

void
mercury__builtin____Compare____func_0_0(MR_Word *result, MR_Word x, MR_Word y)
{
	MR_fatal_error("called compare/3 for `func' type");
}

void
mercury__builtin____Compare____pred_0_0(MR_Word *result, MR_Word x, MR_Word y)
{
	MR_fatal_error("called compare/3 for `pred' type");
}

void
mercury__array____Compare____array_1_0(
	MR_Word type_info, MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for array");
}

void
mercury__std_util____Compare____univ_0_0(MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for univ");
}

void
mercury__std_util____Compare____type_desc_0_0(
	MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for type_desc");
}

void
mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for type_ctor_info");
}

void
mercury__private_builtin____Compare____type_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for type_info");
}

void
mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for typeclass_info");
}

void
mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Word x, MR_Word y)
{
	SORRY("compare for base_typeclass_info");
}

/*---------------------------------------------------------------------------*/
/*
** Unification procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static bool
mercury__builtin__do_unify__int_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____int_0_0(
		(MR_Integer) x, (MR_Integer) y);
}

static bool
mercury__builtin__do_unify__string_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____string_0_0(
		(MR_String) x, (MR_String) y);
}

static bool
mercury__builtin__do_unify__float_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____float_0_0(
		MR_unbox_float(x), MR_unbox_float(y));
}

static bool
mercury__builtin__do_unify__character_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____character_0_0(
		(MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

static bool
mercury__builtin__do_unify__void_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify for type `void'");
}

static bool
mercury__builtin__do_unify__c_pointer_0_0(MR_Box x, MR_Box y)
{
	return mercury__builtin____Unify____c_pointer_0_0(
		(MR_Word) x, (MR_Word) y);
}

static bool
mercury__builtin__do_unify__func_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify for `func' type");
}

static bool
mercury__builtin__do_unify__pred_0_0(MR_Box x, MR_Box y)
{
	MR_fatal_error("called unify for `pred' type");
}

static bool
mercury__array__do_unify__array_1_0(MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury__array____Unify____array_1_0(
		type_info, (MR_Word) x, (MR_Word) y);
}

static bool
mercury__std_util__do_unify__univ_0_0(MR_Box x, MR_Box y)
{
	return mercury__std_util____Unify____univ_0_0(
		(MR_Word) x, (MR_Word) y);
}

static bool
mercury__std_util__do_unify__type_desc_0_0(MR_Box x, MR_Box y)
{
	return mercury__std_util____Unify____type_desc_0_0(
		(MR_Word) x, (MR_Word) y);
}

static bool
mercury__private_builtin__do_unify__type_ctor_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____type_ctor_info_1_0(
		type_info, (MR_Word) x, (MR_Word) y);
}

static bool
mercury__private_builtin__do_unify__type_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____type_info_1_0(
		type_info, (MR_Word) x, (MR_Word) y);
}

static bool
mercury__private_builtin__do_unify__typeclass_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____typeclass_info_1_0(
		type_info, (MR_Word) x, (MR_Word) y);
}

static bool
mercury__private_builtin__do_unify__base_typeclass_info_1_0(
	MR_Word type_info, MR_Box x, MR_Box y)
{
	return mercury__private_builtin____Unify____base_typeclass_info_1_0(
		type_info, (MR_Word) x, (MR_Word) y);
}

/*---------------------------------------------------------------------------*/
/*
** Comparison procedures with the arguments boxed.
** These are just wrappers which call the unboxed version.
*/

static void
mercury__builtin__do_compare__int_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____int_0_0(result,
		(Integer) x, (Integer) y);
}

static void
mercury__builtin__do_compare__string_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____string_0_0(result,
		(MR_String) x, (MR_String) y);
}

static void
mercury__builtin__do_compare__float_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____float_0_0(result,
		MR_unbox_float(x), MR_unbox_float(y));
}

static void
mercury__builtin__do_compare__character_0_0(
	MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____character_0_0(
		result, (MR_Char) (MR_Word) x, (MR_Char) (MR_Word) y);
}

static void
mercury__builtin__do_compare__void_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for type `void'");
}

static void
mercury__builtin__do_compare__c_pointer_0_0(
	MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__builtin____Compare____c_pointer_0_0(
		result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__builtin__do_compare__func_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for func type");
}

static void
mercury__builtin__do_compare__pred_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	MR_fatal_error("called compare/3 for pred type");
}

static void
mercury__array__do_compare__array_1_0(
	MR_Word type_info, MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__array____Compare____array_1_0(
		type_info, result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__std_util__do_compare__univ_0_0(MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__std_util____Compare____univ_0_0(
		result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__std_util__do_compare__type_desc_0_0(
	MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__std_util____Compare____type_desc_0_0(
		result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__private_builtin__do_compare__type_ctor_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____type_ctor_info_1_0(
		type_info, result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__private_builtin__do_compare__type_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____type_info_1_0(
		type_info, result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__private_builtin__do_compare__typeclass_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____typeclass_info_1_0(
		type_info, result, (MR_Word) x, (MR_Word) y);
}

static void
mercury__private_builtin__do_compare__base_typeclass_info_1_0(
	MR_Word type_info, MR_Word *result, MR_Box x, MR_Box y)
{
	mercury__private_builtin____Compare____base_typeclass_info_1_0(
		type_info, result, (MR_Word) x, (MR_Word) y);
}

/*---------------------------------------------------------------------------*/

/*
** XXX this is a hack to work-around the current lack of
** support for `pragma export'.
*/

extern void mercury__io__print_3_p_0(MR_Word ti, MR_Box x);
extern void mercury__io__print_4_p_0(MR_Word ti, MR_Word stream, MR_Box x);

void
ML_io_print_to_cur_stream(MR_Word ti, MR_Word x) {
	mercury__io__print_3_p_0(ti, (MR_Box) x);
}

void
ML_io_print_to_stream(MR_Word ti, MR_Word stream, MR_Word x) {
	mercury__io__print_4_p_0(ti, stream, (MR_Box) x);
}

/*---------------------------------------------------------------------------*/

#endif /* MR_HIGHLEVEL_CODE */
