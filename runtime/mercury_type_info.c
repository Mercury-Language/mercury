/*
** Copyright (C) 1995-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_type_info.c -
**	Definitions for dealing with type_infos needed by the Mercury
**	runtime system.
*/

#include "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_misc.h"	/* for MR_fatal_error() */
#include "mercury_heap.h"	/* for incr_saved_hp() */

/*---------------------------------------------------------------------------*/

static MR_TypeInfo
MR_get_arg_type_info(const MR_TypeInfoParams type_info_params, 
	const MR_PseudoTypeInfo pseudo_type_info, const MR_Word *data_value, 
	const MR_DuFunctorDesc *functor_desc);

/*---------------------------------------------------------------------------*/

#define	usual_func		MR_make_type_info
#define	exist_func		MR_make_type_info_maybe_existq
#define	exist_func_string	"MR_make_type_info_maybe_existq"
#define	MAYBE_DECLARE_ALLOC_ARG	, MR_MemoryList *allocated
#define	MAYBE_PASS_ALLOC_ARG	, allocated
#define	ALLOCATE_WORDS(target, size)					      \
				do {					      \
					MR_MemoryList node;		      \
					(target) = MR_GC_NEW_ARRAY(MR_Word,   \
						(size));		      \
					node = MR_GC_malloc(sizeof(*node));   \
					node->data = (target);		      \
					node->next = *allocated;	      \
					*allocated = node;		      \
				} while (0)
					
#include "mercury_make_type_info_body.h"
#undef	usual_func
#undef	exist_func
#undef	exist_func_string
#undef	MAYBE_DECLARE_ALLOC_ARG
#undef	MAYBE_PASS_ALLOC_ARG
#undef	ALLOCATE_WORDS

#define	usual_func		MR_create_type_info
#define	exist_func		MR_create_type_info_maybe_existq
#define	exist_func_string	"MR_create_type_info_maybe_existq"
#define	MAYBE_DECLARE_ALLOC_ARG
#define	MAYBE_PASS_ALLOC_ARG
#define	ALLOCATE_WORDS(target, size)	MR_incr_saved_hp(		      \
						MR_LVALUE_CAST(MR_Word,	      \
							(target)),	      \
						(size))
#include "mercury_make_type_info_body.h"
#undef	usual_func
#undef	exist_func
#undef	exist_func_string
#undef	MAYBE_DECLARE_ALLOC_ARG
#undef	MAYBE_PASS_ALLOC_ARG
#undef	ALLOCATE_WORDS

static MR_TypeInfo
MR_get_arg_type_info(const MR_TypeInfoParams type_info_params, 
	const MR_PseudoTypeInfo pseudo_type_info, const MR_Word *data_value, 
	const MR_DuFunctorDesc *functor_desc)
{
	MR_Unsigned		arg_num;
	const MR_DuExistInfo	*exist_info;
	MR_DuExistLocn		exist_locn;
	int			exist_varnum;
	int			slot;
	int			offset;

	arg_num = (MR_Unsigned) pseudo_type_info;

	if (MR_TYPE_VARIABLE_IS_UNIV_QUANT(pseudo_type_info)) {
		/*
		** This is a universally quantified type variable.
		*/
		return type_info_params[arg_num];
	}

	/*
	** This is an existentially quantified type variable.
	*/

	exist_info = functor_desc->MR_du_functor_exist_info;
	if (exist_info == NULL) {
		MR_fatal_error("MR_get_arg_type_info: no exist_info");
	}

	exist_varnum = arg_num - MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1;
	exist_locn = exist_info->MR_exist_typeinfo_locns[exist_varnum];
	slot = exist_locn.MR_exist_arg_num;
	offset = exist_locn.MR_exist_offset_in_tci;
	if (offset < 0) {
		return (MR_TypeInfo) data_value[slot];
	} else {
		return (MR_TypeInfo) MR_typeclass_info_type_info(
			data_value[slot], offset);
	}
}

/*
** MR_compare_type_info(type_info_1, type_info_2):
**
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the type_ctor_infos, or in
** the case of higher order types, the arity).
**
** You need to wrap MR_{save/restore}_transient_hp() around
** calls to this function.
*/

int
MR_compare_type_info(MR_TypeInfo t1, MR_TypeInfo t2)
{
	MR_TypeInfo	type_info_1;
	MR_TypeInfo	type_info_2;
	MR_TypeCtorInfo	type_ctor_info_1;
	MR_TypeCtorInfo	type_ctor_info_2;
	MR_TypeInfo	*arg_vector_1;
	MR_TypeInfo	*arg_vector_2;
	int		num_arg_types;
	int		i;

	/* 
	** Try to optimize a common case:
	** If type_info addresses are equal, they must represent the
	** same type.
	*/

	if (t1 == t2) {
		return MR_COMPARE_EQUAL;
	}

	/* 
	** Otherwise, we need to expand equivalence types, if any.
	*/

	type_info_1 = MR_collapse_equivalences(t1);
	type_info_2 = MR_collapse_equivalences(t2);

	/* 
	** Perhaps they are equal now...
	*/

	if (type_info_1 == type_info_2) {
		return MR_COMPARE_EQUAL;
	}

	/*
	** Otherwise find the addresses of the type_ctor_infos,
	** and compare those.
	**
	** Note: this is an arbitrary ordering. It doesn't matter
	** what the ordering is, just so long as it is consistent.
	** ANSI C doesn't guarantee much about pointer comparisons,
	** so it is possible that this might not do the right thing
	** on some obscure systems.
	** The casts to (MR_Word) here are in the hope of increasing
	** the chance that this will work on a segmented architecture.
	*/

	type_ctor_info_1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info_1);
	type_ctor_info_2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info_2);

	if ((MR_Unsigned) type_ctor_info_1 < (MR_Unsigned) type_ctor_info_2) {
		return MR_COMPARE_LESS;
	} else if ((MR_Unsigned) type_ctor_info_1 > (MR_Unsigned) type_ctor_info_2) {
		return MR_COMPARE_GREATER;
	}

	/*
	** If the type_ctor_info addresses are equal, we don't need to
	** compare the arity of the types - they must be the same -
	** unless they are higher-order (which are all mapped to
	** pred/0) or tuples (which are all mapped to tuple/0). 
	** But we need to recursively compare the argument types, if any.
	*/
		/* Check for higher order or tuples */
	if (MR_type_ctor_rep_is_variable_arity(
		MR_type_ctor_rep(type_ctor_info_1)))
	{
		int	num_arg_types_2;

			/* Get number of arguments from type_info */
		num_arg_types = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(
				type_info_1);
		num_arg_types_2 = MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(
				type_info_2);

			/* Check arity */
		if (num_arg_types < num_arg_types_2) {
			return MR_COMPARE_LESS;
		} else if (num_arg_types > num_arg_types_2) {
			return MR_COMPARE_GREATER;
		}

			/*
			** Increment, so arguments are at the
			** expected offset.
			*/
		arg_vector_1 = MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(
				type_info_1);
		arg_vector_2 = MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(
				type_info_2);
	} else {
		num_arg_types = type_ctor_info_1->MR_type_ctor_arity;
		arg_vector_1 = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
				type_info_1);
		arg_vector_2 = MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
				type_info_2);
	}

		/* compare the argument types */
	for (i = 1; i <= num_arg_types; i++) {
		int comp = MR_compare_type_info(
				arg_vector_1[i], arg_vector_2[i]);
		if (comp != MR_COMPARE_EQUAL)
			return comp;
	}

	return MR_COMPARE_EQUAL;
}

	/*
	** MR_collapse_equivalences:
	**
	** Keep looking past equivalences until the there are no more.
	** This only looks past equivalences of the top level type, not
	** the argument typeinfos.
	** 
	** You need to wrap MR_{save/restore}_transient_hp() around
	** calls to this function.
	*/

MR_TypeInfo
MR_collapse_equivalences(MR_TypeInfo maybe_equiv_type_info) 
{
	MR_TypeCtorInfo	type_ctor_info;
	
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(maybe_equiv_type_info);

		/* Look past equivalences */
	while (MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV_GROUND
		|| MR_type_ctor_rep(type_ctor_info) == MR_TYPECTOR_REP_EQUIV)
	{

		maybe_equiv_type_info = MR_create_type_info(
				MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(
					maybe_equiv_type_info),
				MR_type_ctor_layout(type_ctor_info).layout_equiv);

		type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(
				maybe_equiv_type_info);
	}

	return maybe_equiv_type_info;
}

/*
** MR_deallocate() frees up a list of memory cells
*/

void
MR_deallocate(MR_MemoryList allocated)
{
	while (allocated != NULL) {
		MR_MemoryList next = allocated->next;
		MR_GC_free(allocated->data);
		MR_GC_free(allocated);
		allocated = next;
	}
}
