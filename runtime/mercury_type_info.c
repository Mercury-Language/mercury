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
#ifdef MR_NATIVE_GC
  #define ALLOCATE_WORDS(target, size)					     \
	do {								     \
		/* reserve one extra word for GC forwarding pointer */	     \
		/* (see comments in compiler/mlds_to_c.m for details) */     \
		MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, (target)), 1);      \
		MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, (target)), (size)); \
	} while (0)
#else /* !MR_NATIVE_GC */
  #define ALLOCATE_WORDS(target, size)					     \
	do {								     \
		MR_incr_saved_hp(MR_LVALUE_CAST(MR_Word, (target)), (size)); \
	} while (0)
#endif /* !MR_NATIVE_GC */

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

int
MR_compare_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2)
{
	MR_TypeCtorInfo	tci1;
	MR_TypeCtorInfo	tci2;
	MR_TypeInfo	*arg_vector_1;
	MR_TypeInfo	*arg_vector_2;
	int		num_arg_types_1;
	int		num_arg_types_2;
	int		i;
	int		comp;

	/* 
	** Try to optimize a common case:
	** If type_info addresses are equal, they must represent the
	** same type.
	*/

	if (ti1 == ti2) {
		return MR_COMPARE_EQUAL;
	}

	/* 
	** Otherwise, we need to expand equivalence types, if any.
	*/

	ti1 = MR_collapse_equivalences(ti1);
	ti2 = MR_collapse_equivalences(ti2);

	/* 
	** Perhaps they are equal now...
	*/

	if (ti1 == ti2) {
		return MR_COMPARE_EQUAL;
	}

	/*
	** Otherwise find the type_ctor_infos, and compare those.
	*/

	tci1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti1);
	tci2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti2);

	comp = MR_compare_type_ctor_info(tci1, tci2);
	if (comp != MR_COMPARE_EQUAL) {
		return comp;
	}

	/*
	** If the type_ctor_infos are equal, we don't need to compare
	** the arity of the types - they must be the same - unless they are
	** higher-order (which are all mapped to pred/0 or func/0) or tuples
	** (which are all mapped to tuple/0), in which cases we must compare
	** the arities before we can check the argument types.
	*/

	if (MR_type_ctor_rep_is_variable_arity(MR_type_ctor_rep(tci1))) {
		num_arg_types_1 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti1);
		num_arg_types_2 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti2);

			/* Check arity */
		if (num_arg_types_1 < num_arg_types_2) {
			return MR_COMPARE_LESS;
		} else if (num_arg_types_1 > num_arg_types_2) {
			return MR_COMPARE_GREATER;
		}

		arg_vector_1 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti1);
		arg_vector_2 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti2);
	} else {
		num_arg_types_1 = tci1->MR_type_ctor_arity;
		arg_vector_1 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti1);
		arg_vector_2 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti2);
	}

		/* compare the argument types */
	for (i = 1; i <= num_arg_types_1; i++) {
		comp = MR_compare_type_info(arg_vector_1[i], arg_vector_2[i]);
		if (comp != MR_COMPARE_EQUAL)
			return comp;
	}

	return MR_COMPARE_EQUAL;
}

MR_bool
MR_unify_type_info(MR_TypeInfo ti1, MR_TypeInfo ti2)
{
	MR_TypeCtorInfo	tci1;
	MR_TypeCtorInfo	tci2;
	MR_TypeInfo	*arg_vector_1;
	MR_TypeInfo	*arg_vector_2;
	int		num_arg_types_1;
	int		num_arg_types_2;
	int		i;
	int		comp;

	/* 
	** Try to optimize a common case:
	** If type_info addresses are equal, they must represent the
	** same type.
	*/

	if (ti1 == ti2) {
		return MR_TRUE;
	}

	/* 
	** Otherwise, we need to expand equivalence types, if any.
	*/

	ti1 = MR_collapse_equivalences(ti1);
	ti2 = MR_collapse_equivalences(ti2);

	/* 
	** Perhaps they are equal now...
	*/

	if (ti1 == ti2) {
		return MR_TRUE;
	}

	/*
	** Otherwise find the type_ctor_infos, and compare those.
	*/

	tci1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti1);
	tci2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(ti2);

	if (! MR_unify_type_ctor_info(tci1, tci2)) {
		return MR_FALSE;
	}

	/*
	** If the type_ctor_infos are equal, we don't need to compare
	** the arity of the types - they must be the same - unless they are
	** higher-order (which are all mapped to pred/0 or func/0) or tuples
	** (which are all mapped to tuple/0), in which cases we must compare
	** the arities before we can check the argument types.
	*/

	if (MR_type_ctor_rep_is_variable_arity(MR_type_ctor_rep(tci1))) {
		num_arg_types_1 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti1);
		num_arg_types_2 = MR_TYPEINFO_GET_VAR_ARITY_ARITY(ti2);

			/* Check arity */
		if (num_arg_types_1 != num_arg_types_2) {
			return MR_FALSE;
		}

		arg_vector_1 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti1);
		arg_vector_2 = MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(ti2);
	} else {
		num_arg_types_1 = tci1->MR_type_ctor_arity;
		arg_vector_1 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti1);
		arg_vector_2 = MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(ti2);
	}

		/* compare the argument types */
	for (i = 1; i <= num_arg_types_1; i++) {
		if (! MR_unify_type_info(arg_vector_1[i], arg_vector_2[i])) {
			return MR_FALSE;
		}
	}

	return MR_TRUE;
}

int
MR_compare_type_ctor_info(MR_TypeCtorInfo tci1, MR_TypeCtorInfo tci2)
{
	int		i;
	int		comp;
	MR_ConstString	modulename1;
	MR_ConstString	modulename2;
	MR_ConstString	typename1;
	MR_ConstString	typename2;
	int		arity1;
	int		arity2;

	/*
	** We are relying on the fact that type_ctor_infos are always
	** statically allocated to ensure that two type_ctor_infos are
	** for the same type iff their address is the same.
	**
	** The casts to (MR_Unsigned) here are in the hope of increasing
	** the chance that this will work on a segmented architecture.
	*/

	if ((MR_Unsigned) tci1 == (MR_Unsigned) tci2) {
		return MR_COMPARE_EQUAL;
	}

	modulename1 = tci1->MR_type_ctor_module_name;
	modulename2 = tci2->MR_type_ctor_module_name;

	comp = strcmp(modulename1, modulename2);
	if (comp < 0) {
		return MR_COMPARE_LESS;
	} else if (comp > 0) {
		return MR_COMPARE_GREATER;
	}

	typename1 = tci1->MR_type_ctor_name;
	typename2 = tci2->MR_type_ctor_name;
	comp = strcmp(typename1, typename2);
	if (comp < 0) {
		return MR_COMPARE_LESS;
	} else if (comp > 0) {
		return MR_COMPARE_GREATER;
	}

	arity1 = tci1->MR_type_ctor_arity;
	arity2 = tci2->MR_type_ctor_arity;
	if (arity1 < arity2) {
		return MR_COMPARE_LESS;
	} else if (arity1 > arity2) {
		return MR_COMPARE_GREATER;
	}

	MR_fatal_error("type_ctor_info match at distinct addresses");
}

MR_bool
MR_unify_type_ctor_info(MR_TypeCtorInfo tci1, MR_TypeCtorInfo tci2)
{
	int		i;

	/*
	** We are relying on the fact that type_ctor_infos are always
	** statically allocated to ensure that two type_ctor_infos are
	** for the same type iff their address is the same.
	**
	** The casts to (MR_Unsigned) here are in the hope of increasing
	** the chance that this will work on a segmented architecture.
	*/

	if ((MR_Unsigned) tci1 == (MR_Unsigned) tci2) {
		return MR_TRUE;
	} else {
		return MR_FALSE;
	}
}

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
			MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(
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

MR_Word
MR_type_params_vector_to_list(int arity, MR_TypeInfoParams type_params)
{
	MR_TypeInfo	arg_type;
	MR_Word		type_info_list;

	MR_restore_transient_registers();
	type_info_list = MR_list_empty();
	while (arity > 0) {
		type_info_list = MR_list_cons((MR_Word) type_params[arity],
			type_info_list);
		--arity;
	}

	MR_save_transient_registers();
	return type_info_list;
}

MR_Word
MR_arg_name_vector_to_list(int arity, const MR_ConstString *arg_names)
{
	MR_TypeInfo	arg_type;
	MR_Word		arg_names_list;

	MR_restore_transient_registers();
	arg_names_list = MR_list_empty();

	while (arity > 0) {
		--arity;
		arg_names_list = MR_list_cons((MR_Word) arg_names[arity],
			arg_names_list);
	}

	MR_save_transient_registers();
	return arg_names_list;
}

MR_Word
MR_pseudo_type_info_vector_to_type_info_list(int arity,
	MR_TypeInfoParams type_params,
	const MR_PseudoTypeInfo *arg_pseudo_type_infos)
{
	MR_TypeInfo arg_type_info;
	MR_Word     type_info_list;

	MR_restore_transient_registers();
	type_info_list = MR_list_empty();

	while (--arity >= 0) {
			/* Get the argument type_info */

		MR_save_transient_registers();
		arg_type_info = MR_create_type_info(type_params,
			arg_pseudo_type_infos[arity]);
		MR_restore_transient_registers();

		MR_save_transient_registers();
		arg_type_info = MR_collapse_equivalences(arg_type_info);
		MR_restore_transient_registers();

		type_info_list = MR_list_cons((MR_Word) arg_type_info,
			type_info_list);
	}

	MR_save_transient_registers();
	return type_info_list;
}
