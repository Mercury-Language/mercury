/*
** Copyright (C) 1999 The University of Melbourne.
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
#include "mercury_misc.h"	/* for fatal_error() */

/*---------------------------------------------------------------------------*/
/*
** Variable definitions
*/

MR_Word mercury__private_builtin__dummy_var;

/*---------------------------------------------------------------------------*/
/*
** Type definitions
*/

typedef bool MR_UnifyFunc_0(MR_Box, MR_Box);
typedef bool MR_UnifyFunc_1(MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_2(MR_Word, MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_3(MR_Word, MR_Word, MR_Word, MR_Box, MR_Box);
typedef bool MR_UnifyFunc_4(MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Box);
typedef bool MR_UnifyFunc_5(MR_Word, MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Box);

typedef void MR_IndexFunc_0(MR_Box, MR_Integer *);
typedef void MR_IndexFunc_1(MR_Word, MR_Box, MR_Integer *);
typedef void MR_IndexFunc_2(MR_Word, MR_Word, MR_Box, MR_Integer *);
typedef void MR_IndexFunc_3(MR_Word, MR_Word, MR_Word, MR_Box, MR_Integer *);
typedef void MR_IndexFunc_4(MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Integer *);
typedef void MR_IndexFunc_5(MR_Word, MR_Word, MR_Word, MR_Word, MR_Word,
			    MR_Box, MR_Integer *);

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
** Constant definitions
*/

/*
** Define MR_BaseTypeInfos for the builtin types
*/

#define MR_base_type_info_name(MODULE, TYPE, ARITY)			      \
	MR_PASTE2(mercury__,						      \
	MR_PASTE2(MODULE,						      \
	MR_PASTE2(__base_type_info_,					      \
	MR_PASTE2(TYPE,							      \
	MR_PASTE2(_,							      \
	          ARITY)))))

#define MR_base_type_info_func_name(MODULE, TYPE, ARITY, FUNC)		      \
	MR_PASTE2(mercury__,						      \
	MR_PASTE2(MODULE,						      \
	MR_PASTE2(__,							      \
	MR_PASTE2(FUNC,							      \
	MR_PASTE2(__,							      \
	MR_PASTE2(TYPE,							      \
	MR_PASTE2(_,							      \
	          ARITY)))))))

#define MR_special_func_type(NAME, ARITY) \
	MR_PASTE2(MR_, MR_PASTE2(NAME, MR_PASTE2(Func_, ARITY)))

#define MR_define_base_type_info(module, type, arity, type_rep)		      \
									      \
	extern MR_special_func_type(Unify, arity)			      \
		MR_base_type_info_func_name(module, type, arity, __Unify__);  \
	extern MR_special_func_type(Index, arity)			      \
		MR_base_type_info_func_name(module, type, arity, __Index__);  \
	extern MR_special_func_type(Compare, arity)			      \
		MR_base_type_info_func_name(module, type, arity, __Compare__);\
									      \
	const MR_BaseTypeInfo MR_base_type_info_name(module, type, arity) =   \
	{								      \
		arity,							      \
		(MR_Box) MR_base_type_info_func_name(module, type, arity,     \
				__Unify__),				      \
		(MR_Box) MR_base_type_info_func_name(module, type, arity,     \
				__Index__),				      \
		(MR_Box) MR_base_type_info_func_name(module, type, arity,     \
				__Compare__),				      \
		type_rep,						      \
		NULL,							      \
		NULL,							      \
		MR_STRINGIFY(module),					      \
		MR_STRINGIFY(type),					      \
		MR_RTTI_VERSION						      \
	}

MR_define_base_type_info(builtin, int, 0, MR_TYPECTOR_REP_INT);
MR_define_base_type_info(builtin, string, 0, MR_TYPECTOR_REP_STRING);
MR_define_base_type_info(builtin, float, 0, MR_TYPECTOR_REP_FLOAT);
MR_define_base_type_info(builtin, character, 0, MR_TYPECTOR_REP_CHAR);
MR_define_base_type_info(builtin, void, 0, MR_TYPECTOR_REP_VOID);
MR_define_base_type_info(builtin, c_pointer, 0, MR_TYPECTOR_REP_C_POINTER);
MR_define_base_type_info(builtin, pred, 0, MR_TYPECTOR_REP_PRED);
MR_define_base_type_info(builtin, func, 0, MR_TYPECTOR_REP_PRED);
MR_define_base_type_info(array, array, 1, MR_TYPECTOR_REP_ARRAY);
MR_define_base_type_info(std_util, univ, 0, MR_TYPECTOR_REP_UNIV);
MR_define_base_type_info(std_util, type_info, 0, MR_TYPECTOR_REP_TYPEINFO);
MR_define_base_type_info(private_builtin, type_ctor_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);
MR_define_base_type_info(private_builtin, type_info, 1,
	MR_TYPECTOR_REP_TYPEINFO);
MR_define_base_type_info(private_builtin, base_typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);
MR_define_base_type_info(private_builtin, typeclass_info, 1,
	MR_TYPECTOR_REP_TYPECLASSINFO);

/*---------------------------------------------------------------------------*/
/*
** Function definitions
*/

/*
** Define the builtin unify/2, index/2, and compare/3 functions.
*/

bool
mercury__builtin__unify_2_p_0(Word ti, MR_Box x, MR_Box y)
{
	Word *type_info;
	MR_BaseTypeInfo *base_type_info;
	int arity;

	type_info = (Word *) ti;
	if (*type_info == 0) {
		base_type_info = (MR_BaseTypeInfo *)type_info;
	} else {
		base_type_info = *(MR_BaseTypeInfo **)type_info;
	}

	/*
	** For higher-order predicates and functions,
	** the arity is stored in the type_info, not in the
	** base_type_info, and so we need to skip past the
	** arity field.
	*/
	if (base_type_info->type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		arity = type_info[1];
		type_info++;
	} else {
		arity = base_type_info->type_arity;
	}
		
	switch(arity) {
		/*
		** cast base_type_info->unify to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: return ((MR_UnifyFunc_0 *) base_type_info->unify)
				(x, y);
		case 1: return ((MR_UnifyFunc_1 *) base_type_info->unify)
				(type_info[1], x, y);
		case 2: return ((MR_UnifyFunc_2 *) base_type_info->unify)
				(type_info[1], type_info[2], x, y);
		case 3: return ((MR_UnifyFunc_3 *) base_type_info->unify)
				(type_info[1], type_info[2], type_info[3],
				 x, y);
		case 4: return ((MR_UnifyFunc_4 *) base_type_info->unify)
				(type_info[1], type_info[2], type_info[3],
				 type_info[4], x, y);
		case 5: return ((MR_UnifyFunc_5 *) base_type_info->unify)
				(type_info[1], type_info[2], type_info[3],
				 type_info[4], type_info[5], x, y);
		default:
			fatal_error("unify/2: type arity > 5 not supported");
	}
}

void
mercury__builtin__index_2_p_3(Word ti, MR_Box x, Integer *y)
{
	Word *type_info;
	MR_BaseTypeInfo *base_type_info;
	int arity;

	type_info = (Word *) ti;
	if (*type_info == 0) {
		base_type_info = (MR_BaseTypeInfo *)type_info;
	} else {
		base_type_info = *(MR_BaseTypeInfo **)type_info;
	}

	/*
	** For higher-order predicates and functions,
	** the arity is stored in the type_info, not in the
	** base_type_info, and so we need to skip past the
	** arity field.
	*/
	if (base_type_info->type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		arity = type_info[1];
		type_info++;
	} else {
		arity = base_type_info->type_arity;
	}
		
	switch(arity) {
		/*
		** cast base_type_info->index to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: return ((MR_IndexFunc_0 *) base_type_info->index)
				(x, y);
		case 1: return ((MR_IndexFunc_1 *) base_type_info->index)
				(type_info[1], x, y);
		case 2: return ((MR_IndexFunc_2 *) base_type_info->index)
				(type_info[1], type_info[2], x, y);
		case 3: return ((MR_IndexFunc_3 *) base_type_info->index)
				(type_info[1], type_info[2], type_info[3],
				 x, y);
		case 4: return ((MR_IndexFunc_4 *) base_type_info->index)
				(type_info[1], type_info[2], type_info[3],
				 type_info[4], x, y);
		case 5: return ((MR_IndexFunc_5 *) base_type_info->index)
				(type_info[1], type_info[2], type_info[3],
				 type_info[4], type_info[5], x, y);
		default:
			fatal_error("index/2: type arity > 5 not supported");
	}
}

void
mercury__builtin__compare_3_p_0(Word ti, Word *res, MR_Box x, MR_Box y)
{
	Word *type_info;
	MR_BaseTypeInfo *base_type_info;
	int arity;

	type_info = (Word *) ti;
	if (*type_info == 0) {
		base_type_info = (MR_BaseTypeInfo *)type_info;
	} else {
		base_type_info = *(MR_BaseTypeInfo **)type_info;
	}

	/*
	** For higher-order predicates and functions,
	** the arity is stored in the type_info, not in the
	** base_type_info, and so we need to skip past the
	** arity field.
	*/
	if (base_type_info->type_ctor_rep == MR_TYPECTOR_REP_PRED) {
		arity = type_info[1];
		type_info++;
	} else {
		arity = base_type_info->type_arity;
	}
		
	switch(arity) {
		/*
		** cast base_type_info->compare to the right type
		** and then call it, passing the right number of
		** type_info arguments
		*/
		case 0: ((MR_CompareFunc_0 *) base_type_info->compare)
			 (res, x, y);
		case 1: ((MR_CompareFunc_1 *) base_type_info->compare)
			 (type_info[1], res, x, y);
		case 2: ((MR_CompareFunc_2 *) base_type_info->compare)
			 (type_info[1], type_info[2], res, x, y);
		case 3: ((MR_CompareFunc_3 *) base_type_info->compare)
			 (type_info[1], type_info[2], type_info[3], res, x, y);
		case 4: ((MR_CompareFunc_4 *) base_type_info->compare)
			 (type_info[1], type_info[2], type_info[3],
			  type_info[4], res, x, y);
		case 5: ((MR_CompareFunc_5 *) base_type_info->compare)
			 (type_info[1], type_info[2], type_info[3],
			  type_info[4], type_info[5], res, x, y);
		default:
			fatal_error("index/2: type arity > 5 not supported");
	}
}

void
mercury__builtin__compare_3_p_1(Word type_info, Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void
mercury__builtin__compare_3_p_2(Word type_info, Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
}

void
mercury__builtin__compare_3_p_3(Word type_info, Word *res, MR_Box x, MR_Box y)
{
	return mercury__builtin__compare_3_p_0(type_info, res, x, y);
}
