/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module exists to handle user-visible descriptions of types and type
** constructors.
*/

#include "mercury_conf.h"
#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_type_desc.h"
#include "mercury_heap.h"	/* for MR_incr_hp_atomic_msg() */
#include "mercury_misc.h"	/* for MR_fatal_error() */

MR_TypeCtorDesc
MR_make_type_ctor_desc(MR_TypeInfo type_info, MR_TypeCtorInfo type_ctor_info)
{
	MR_TypeCtorDesc type_ctor_desc;

	if (MR_TYPE_CTOR_INFO_IS_HO_PRED(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_PRED(
			MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error("MR_make_type_ctor_desc"
				" - arity out of range.");
		}
	} else if (MR_TYPE_CTOR_INFO_IS_HO_FUNC(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FUNC(
			MR_TYPEINFO_GET_HIGHER_ORDER_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error("MR_make_type_ctor_desc"
				" - arity out of range.");
		}
	} else if (MR_TYPE_CTOR_INFO_IS_TUPLE(type_ctor_info)) {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_TUPLE(
			MR_TYPEINFO_GET_TUPLE_ARITY(type_info));
		if (! MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
			MR_fatal_error("MR_make_type_ctor_desc"
				" - arity out of range.");
		}
	} else {
		type_ctor_desc = MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(
			type_ctor_info);
	}

	return type_ctor_desc;
}

void
MR_type_ctor_and_args(MR_TypeInfo type_info, MR_bool collapse_equivalences,
	MR_TypeCtorDesc *type_ctor_desc_ptr, MR_Word *arg_type_info_list_ptr)
{
	MR_TypeCtorInfo type_ctor_info;
	MR_TypeCtorDesc type_ctor_desc;
	MR_Integer	arity;

	if (collapse_equivalences) {
		type_info = MR_collapse_equivalences(type_info);
	}

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
	type_ctor_desc = MR_make_type_ctor_desc(type_info, type_ctor_info);
	*type_ctor_desc_ptr = type_ctor_desc;

	if (MR_type_ctor_rep_is_variable_arity(
		MR_type_ctor_rep(type_ctor_info)))
	{
		arity = MR_TYPECTOR_DESC_GET_VA_ARITY(type_ctor_desc);
		*arg_type_info_list_ptr = MR_type_params_vector_to_list(arity,
			MR_TYPEINFO_GET_HIGHER_ORDER_ARG_VECTOR(type_info));
	} else {
		arity = type_ctor_info->MR_type_ctor_arity;
		*arg_type_info_list_ptr = MR_type_params_vector_to_list(arity,
			MR_TYPEINFO_GET_FIRST_ORDER_ARG_VECTOR(type_info));
	}
}

MR_TypeInfo
MR_make_type(int arity, MR_TypeCtorDesc type_ctor_desc, MR_Word arg_types_list)
{
	MR_TypeCtorInfo type_ctor_info;
	MR_Word		*new_type_info_arena;
	MR_TypeInfo	*new_type_info_args;
	int		i;

	/*
	** We need to treat variable-arity types as a special case here.
	*/

	if (MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(type_ctor_desc)) {
		type_ctor_info = MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(
			type_ctor_desc);

		MR_restore_transient_registers();
		MR_incr_hp_atomic_msg(
			MR_LVALUE_CAST(MR_Word, new_type_info_arena),
			MR_higher_order_type_info_size(arity),
			"MR_make_type", "type_info");
		MR_save_transient_registers();
		MR_fill_in_higher_order_type_info(new_type_info_arena,
			type_ctor_info, arity, new_type_info_args);
	} else {
		type_ctor_info =
			MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(
				type_ctor_desc);

		if (arity == 0) {
			return (MR_TypeInfo) type_ctor_info;
		}

		MR_restore_transient_registers();
		MR_incr_hp_atomic_msg(
			MR_LVALUE_CAST(MR_Word, new_type_info_arena),
			MR_first_order_type_info_size(arity),
			"MR_make_type", "type_info");
		MR_save_transient_registers();
		MR_fill_in_first_order_type_info(new_type_info_arena,
			type_ctor_info, new_type_info_args);
	}

	for (i = 1; i <= arity; i++) {
		new_type_info_args[i] = (MR_TypeInfo)
			MR_list_head(arg_types_list);
		arg_types_list = MR_list_tail(arg_types_list);
	}

	return (MR_TypeInfo) new_type_info_arena;
}
