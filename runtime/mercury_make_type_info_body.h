/*
** Copyright (C) 2000-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This file is intended to be #included in mercury_type_info.c to provide
** the definitions of MR_create_type_info and MR_make_type_info, and their
** helper functions MR_create_type_info_maybe_existq and
** MR_make_type_info_maybe_existq.
*/

MR_TypeInfo
usual_func(const MR_TypeInfoParams type_info_params,
	const MR_PseudoTypeInfo pseudo_type_info
	MAYBE_DECLARE_ALLOC_ARG)
{
	return exist_func(type_info_params, 
		pseudo_type_info, NULL, NULL
		MAYBE_PASS_ALLOC_ARG);
}

MR_TypeInfo
exist_func(const MR_TypeInfoParams type_info_params, 
	const MR_PseudoTypeInfo pseudo_type_info, const MR_Word *data_value, 
	const MR_DuFunctorDesc *functor_desc
	MAYBE_DECLARE_ALLOC_ARG)
{
	MR_TypeCtorInfo		type_ctor_info;
	MR_TypeInfo		expanded_type_info;
	MR_Word			*type_info_arena;
	MR_PseudoTypeInfo	*pseudo_type_info_arena;
	int			arity;
	int			start_region_size;
	int			i;

	/* 
	** The pseudo_type_info might be a polymorphic variable.
	** If so, substitute its value, and we are done.
	*/
	if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(pseudo_type_info)) {

		expanded_type_info = MR_get_arg_type_info(type_info_params, 
			pseudo_type_info, data_value, functor_desc);

		if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(
			(MR_PseudoTypeInfo) expanded_type_info))
		{
			MR_fatal_error(exist_func_string
				": unbound type variable");
		}

		return expanded_type_info;
	}

	type_ctor_info = MR_PSEUDO_TYPEINFO_GET_TYPE_CTOR_INFO(
			pseudo_type_info);

	/* no arguments - optimise common case */
	if ((MR_Word) type_ctor_info == (MR_Word) pseudo_type_info) {
		return MR_pseudo_type_info_is_ground(pseudo_type_info);
	}

	if (MR_type_ctor_rep_is_variable_arity(
		MR_type_ctor_rep(type_ctor_info)))
	{
		arity = MR_PSEUDO_TYPEINFO_GET_VAR_ARITY_ARITY(
			pseudo_type_info);
		start_region_size = 2;
	} else {
		arity = type_ctor_info->MR_type_ctor_arity;
		start_region_size = 1;
	}

	/*
	** Iterate over the arguments, figuring out whether we
	** need to make any substitutions.
	** If so, copy the resulting argument type-infos into
	** a new type_info.
	*/

	type_info_arena = NULL;
	pseudo_type_info_arena = (MR_PseudoTypeInfo *) pseudo_type_info;
	for (i = start_region_size; i < arity + start_region_size; i++) {
		expanded_type_info = exist_func(type_info_params,
				pseudo_type_info_arena[i],
				data_value, functor_desc
				MAYBE_PASS_ALLOC_ARG);

		if (MR_PSEUDO_TYPEINFO_IS_VARIABLE(
			(MR_PseudoTypeInfo) expanded_type_info))
		{
			MR_fatal_error(exist_func_string
				": unbound type variable");
		}

		if (expanded_type_info !=
			(MR_TypeInfo) pseudo_type_info_arena[i])
		{
			/*
			** We made a substitution.
			** We need to allocate a new type_info,
			** if we haven't done so already.
			*/
			if (type_info_arena == NULL) {
				ALLOCATE_WORDS(type_info_arena,
					arity + start_region_size);
				memcpy(type_info_arena,
					(MR_Word *) pseudo_type_info,
					(arity + start_region_size)
						* sizeof(MR_Word));
			}
			type_info_arena[i] = (MR_Word) expanded_type_info;
		}
	}

	if (type_info_arena == NULL) {
		return (MR_TypeInfo) pseudo_type_info;
	} else {
		return (MR_TypeInfo) type_info_arena;
	}
}
