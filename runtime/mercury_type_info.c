/*
** Copyright (C) 1995-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** type_info.c -
**	Definitions for dealing with type_infos needed by the Mercury
**	runtime system.
*/

#include "mercury_imp.h"
#include "mercury_type_info.h"

/*---------------------------------------------------------------------------*/

static Word *
MR_get_arg_type_info(const Word *term_type_info, 
	const Word *arg_pseudo_type_info, const Word *data_value, 
	int rtti_version, const MR_DuFunctorDesc *functor_desc);

/*---------------------------------------------------------------------------*/

extern	struct MR_TypeCtorInfo_struct	mercury_data___type_ctor_info_pred_0;
extern	struct MR_TypeCtorInfo_struct	mercury_data___type_ctor_info_func_0;

	/* 
	** MR_create_type_info():
	**
	** Given a type_info `term_type_info' which contains a
	** type_ctor_info pointer and possibly other type_infos
	** giving the values of the type parameters of this type,
	** and given a pseudo-type_info `arg_pseudo_type_info', which contains
	** a type_ctor_info pointer and possibly other type_infos
	** giving EITHER
	** 	- the values of the type parameters of this type
	** or	- an indication of the type parameter of the
	** 	  term_type_info that should be substituted here,
	** this returns a fully instantiated type_info, a version of the
	** arg_pseudo_type_info with all the type variables filled in.
	**
	** We allocate memory for a new type_info on the Mercury heap,
	** copy the necessary information, and return a pointer to the
	** new type_info.  You need to wrap save_transient_hp()
	** and restore_transient_hp() around calls to this function.
	**
	** In the case where the argument's pseudo_type_info is a
	** type_ctor_info with no arguments, we don't copy the
	** type_ctor_info - we just return a pointer to it - no memory
	** is allocated. The caller can check this by looking at the
	** first cell of the returned pointer - if it is zero, this is a
	** type_ctor_info. Otherwise, it is an allocated copy of a
	** type_info.
	**
	** If arg_pseudo_type_info does not contain any type variables,
	** then it is OK for term_type_info to be NULL.
	**
	** NOTE: If you are changing this code, you might also need
	** to change the code in MR_make_type_info in this module 
	** which does much the same thing, only allocating using MR_GC_malloc()
	** instead of on the Mercury heap.
	**
	** The rtti version number we pass in the call below is a placeholder;
	** its value does not matter because the functor_desc we pass, whose
	** format it describes, is NULL.
	*/

Word * 
MR_create_type_info(const Word *term_type_info, const Word *arg_pseudo_type_info)
{
	return MR_create_type_info_maybe_existq(term_type_info, 
		arg_pseudo_type_info, NULL,
		MR_RTTI_VERSION__CLEAN_LAYOUT, NULL);
}

	/*
	** MR_create_type_info_maybe_existq():
	**
	** The same as MR_create_type_info except that the type-info being
	** created may be for an existentially typed argument of a constructor.
	** In order to handle this, it also takes the data value from which
	** the values whose pseudo type-info we are looking at was taken, as
	** well as the functor descriptor for that functor.
	**
	** If the term_type_info has a NULL type_ctor_info,
	** or if the arg_pseudo_type_info does not contain any
	** existentially typed type variables, then it is OK
	** for the data_value and functor_desc to be NULL.
	**
	** XXX The rtti_version argument is only temporary; it should not be
	** needed once we have bootstrapped the CLEAN_LAYOUT change and
	** dropped support for older type_ctor_info versions.
	*/

Word * 
MR_create_type_info_maybe_existq(const Word *term_type_info, 
	const Word *arg_pseudo_type_info, const Word *data_value, 
	int rtti_version, const MR_DuFunctorDesc *functor_desc)
{
	MR_TypeCtorInfo	type_ctor_info;
	Word		*arg_type_info;
	Word		*type_info;
	int		arity;
	int		extra_args;
	int		i;

	/* 
	** The arg_pseudo_type_info might be a polymorphic variable.
	** If so, then substitute it's value, and then we're done.
	*/
	if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {

		arg_type_info = MR_get_arg_type_info(term_type_info, 
			arg_pseudo_type_info, data_value,
			rtti_version, functor_desc);

		if (TYPEINFO_IS_VARIABLE(arg_type_info)) {
			fatal_error("MR_create_type_info: "
					"unbound type variable");
		}

		return arg_type_info;
	}

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(arg_pseudo_type_info);

	/* no arguments - optimise common case */
	if ((Word) type_ctor_info == (Word) arg_pseudo_type_info) {
		return (Word *) arg_pseudo_type_info;
	}

	if (MR_TYPE_CTOR_INFO_IS_HO(type_ctor_info)) {
		arity = MR_TYPEINFO_GET_HIGHER_ARITY(arg_pseudo_type_info);
		extra_args = 2;
	} else {
		arity = MR_TYPE_CTOR_INFO_GET_TYPE_ARITY(type_ctor_info);
		extra_args = 1;
	}

	/*
	** Iterate over the arguments, figuring out whether we
	** need to make any substitutions.
	** If so, copy the resulting argument type-infos into
	** a new type_info.
	*/
	type_info = NULL;
	for (i = extra_args; i < arity + extra_args; i++) {
		arg_type_info = MR_create_type_info_maybe_existq(term_type_info,
				(Word *) arg_pseudo_type_info[i],
				data_value, rtti_version, functor_desc);
		if (TYPEINFO_IS_VARIABLE(arg_type_info)) {
			fatal_error("MR_create_type_info_maybe_existq: "
				"unbound type variable");
		}
		if (arg_type_info != (Word *) arg_pseudo_type_info[i]) {
			/*
			** We made a substitution.
			** We need to allocate a new type_info,
			** if we haven't done so already.
			*/
			if (type_info == NULL) {
				incr_saved_hp(LVALUE_CAST(Word, type_info),
					arity + extra_args);
				memcpy(type_info, arg_pseudo_type_info,
					(arity + extra_args) * sizeof(Word));
			}
			type_info[i] = (Word) arg_type_info;
		}
	}
	if (type_info == NULL) {
		return (Word *) arg_pseudo_type_info;
	} else {
		return type_info;
	}
}

static Word *
MR_get_arg_type_info(const Word *term_type_info, 
	const Word *arg_pseudo_type_info, const Word *data_value, 
	int rtti_version, const MR_DuFunctorDesc *functor_desc)
{
	Word		*arg_type_info;
	Unsigned	arg_num;

	arg_num = (Unsigned) arg_pseudo_type_info;

	if (MR_TYPE_VARIABLE_IS_UNIV_QUANT(arg_num)) {
		/*
		** This is a universally quantified type variable
		*/
		return (Word *) term_type_info[arg_num];
	}

	/*
	** This is an existentially quantified type variable
	*/

	if (rtti_version <= MR_RTTI_VERSION__USEREQ) {
		const Word	*functor_descriptor;
		Word		*type_info_locns;
		Word		type_info_locn;

		functor_descriptor = (Word *) functor_desc;
		type_info_locns = (Word *) 
			MR_TYPE_CTOR_LAYOUT_FUNCTOR_DESCRIPTOR_TYPE_INFO_LOCNS(
				functor_descriptor);
		type_info_locn = type_info_locns[arg_num
				- MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1];

		if (MR_TYPE_INFO_LOCN_IS_INDIRECT(type_info_locn)) {
			/*
			** This is indirect; the type-info
			** is inside a typeclass-info 
			*/

			int typeinfo_number;
			int arg_number;

			typeinfo_number =
				MR_TYPE_INFO_LOCN_INDIRECT_GET_TYPEINFO_NUMBER(
					type_info_locn);
			arg_number =
				MR_TYPE_INFO_LOCN_INDIRECT_GET_ARG_NUMBER(
					type_info_locn);
			arg_type_info = (Word *) MR_typeclass_info_type_info(
				data_value[arg_number], typeinfo_number);
		} else {
			/*
			** This is direct
			*/
			int typeinfo_number;

			typeinfo_number =
				MR_TYPE_INFO_LOCN_DIRECT_GET_TYPEINFO_NUMBER(
					type_info_locn);
			arg_type_info = (Word *) data_value[typeinfo_number];
		}
	} else {
		const MR_DuExistInfo	*exist_info;
		MR_DuExistLocn		exist_locn;
		int			exist_varnum;
		int			slot;
		int			offset;

		exist_info = functor_desc->MR_du_functor_exist_info;
		if (exist_info == NULL) {
			fatal_error("MR_get_arg_type_info: no exist_info");
		}

		exist_varnum = arg_num - MR_PSEUDOTYPEINFO_EXIST_VAR_BASE - 1;
		exist_locn = exist_info->MR_exist_typeinfo_locns[exist_varnum];
		slot = exist_locn.MR_exist_arg_num;
		offset = exist_locn.MR_exist_offset_in_tci;
		if (offset < 0) {
			arg_type_info = (Word *) data_value[slot];
		} else {
			arg_type_info = (Word *) MR_typeclass_info_type_info(
					data_value[slot], offset);
		}
	}

	return arg_type_info;
}

/*
** MR_compare_type_info(type_info_1, type_info_2):
**
** Compare two type_info structures, using an arbitrary ordering
** (based on the addresses of the type_ctor_infos, or in
** the case of higher order types, the arity).
**
** You need to wrap save/restore_transient_hp() around
** calls to this function.
*/

int
MR_compare_type_info(Word t1, Word t2)
{
	Word		*type_info_1;
	Word		*type_info_2;
	MR_TypeCtorInfo	type_ctor_info_1;
	MR_TypeCtorInfo	type_ctor_info_2;
	int		num_arg_types;
	int		i;

	/* 
	** Try to optimize a common case:
	** If type_info addresses are equal, they must represent the
	** same type.
	*/
	if (t1 == t2) {
		return COMPARE_EQUAL;
	}

	/* 
	** Otherwise, we need to expand equivalence types, if any.
	*/
	type_info_1 = (Word *) MR_collapse_equivalences(t1);
	type_info_2 = (Word *) MR_collapse_equivalences(t2);

	/* 
	** Perhaps they are equal now...
	*/
	if (type_info_1 == type_info_2) {
		return COMPARE_EQUAL;
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
	** The casts to (Word) here are in the hope of increasing
	** the chance that this will work on a segmented architecture.
	*/
	type_ctor_info_1 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info_1);
	type_ctor_info_2 = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info_2);
	if ((Word) type_ctor_info_1 < (Word) type_ctor_info_2) {
		return COMPARE_LESS;
	}
	if ((Word) type_ctor_info_1 > (Word) type_ctor_info_2) {
		return COMPARE_GREATER;
	}

	/*
	** If the type_ctor_info addresses are equal, we don't need to
	** compare the arity of the types - they must be the same -
	** unless they are higher-order (which are all mapped to
	** pred/0). 
	** But we need to recursively compare the argument types, if any.
	*/
		/* Check for higher order */
	if (MR_TYPE_CTOR_INFO_IS_HO(type_ctor_info_1)) 
	{
		int	num_arg_types_2;

			/* Get number of arguments from type_info */
		num_arg_types = MR_field(MR_mktag(0), type_info_1, 
			TYPEINFO_OFFSET_FOR_PRED_ARITY);

		num_arg_types_2 = MR_field(MR_mktag(0), type_info_2, 
			TYPEINFO_OFFSET_FOR_PRED_ARITY);

			/* Check arity */
		if (num_arg_types < num_arg_types_2) {
			return COMPARE_LESS;
		}
		if (num_arg_types > num_arg_types_2) {
			return COMPARE_GREATER;
		}

			/*
			** Increment, so arguments are at the
			** expected offset.
			*/
		type_info_1++;
		type_info_2++;
	} else {
		num_arg_types = MR_field(MR_mktag(0), type_ctor_info_1,
				OFFSET_FOR_COUNT);
	}

		/* compare the argument types */
	for (i = 0; i < num_arg_types; i++) {
		Word arg_type_info_1 = MR_field(MR_mktag(0), type_info_1,
			OFFSET_FOR_ARG_TYPE_INFOS + i);
		Word arg_type_info_2 = MR_field(MR_mktag(0), type_info_2,
			OFFSET_FOR_ARG_TYPE_INFOS + i);
		int comp = MR_compare_type_info(
				arg_type_info_1, arg_type_info_2);
		if (comp != COMPARE_EQUAL)
			return comp;
	}

	return COMPARE_EQUAL;
}

	/*
	** MR_collapse_equivalences:
	**
	** Keep looking past equivalences until the there are no more.
	** This only looks past equivalences of the top level type, not
	** the argument typeinfos.
	** 
	** You need to wrap save/restore_transient_hp() around
	** calls to this function.
	*/

Word
MR_collapse_equivalences(Word maybe_equiv_type_info) 
{
	MR_TypeCtorInfo	type_ctor_info;
	Word		equiv_type_info;
	
	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO((Word *) 
					maybe_equiv_type_info);

		/* Look past equivalences */
	while (type_ctor_info->type_ctor_rep == MR_TYPECTOR_REP_EQUIV_GROUND
		|| type_ctor_info->type_ctor_rep == MR_TYPECTOR_REP_EQUIV_VAR
		|| type_ctor_info->type_ctor_rep == MR_TYPECTOR_REP_EQUIV)
	{
		if (type_ctor_info->type_ctor_version <=
			MR_RTTI_VERSION__USEREQ)
		{
			equiv_type_info = (Word)
				MR_TYPE_CTOR_FUNCTORS_EQUIV_TYPE(
				type_ctor_info->type_ctor_functors);
		} else {
			equiv_type_info = (Word) type_ctor_info->type_layout.
				layout_equiv;
		}

		equiv_type_info = (Word) MR_create_type_info(
				(Word *) maybe_equiv_type_info, 
				(Word *) equiv_type_info);

		maybe_equiv_type_info = equiv_type_info;
		type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(
				(Word *) maybe_equiv_type_info);
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

	/* 
	** Given a type_info `term_type_info' which contains a
	** type_ctor_info pointer and possibly other type_infos
	** giving the values of the type parameters of this type,
	** and given a pseudo-type_info `arg_pseudo_type_info', which contains
	** a type_ctor_info pointer and possibly other type_infos
	** giving EITHER
	** 	- the values of the type parameters of this type,
	** or	- an indication of the type parameter of the
	** 	  term_type_info that should be substituted here,
	** this returns a fully instantiated type_info, a version of the
	** arg_pseudo_type_info with all the type variables filled in.
	**
	** If there are no type variables to fill in, we return the
	** arg_pseudo_type_info, unchanged. Otherwise, we allocate
	** memory using MR_GC_malloc().  Any such memory allocated will be
	** inserted into the list of allocated memory cells.
	** It is the caller's responsibility to free these cells
	** by calling MR_deallocate() on the list when they are no longer
	** needed.
	**
	** If arg_pseudo_type_info does not contain any type variables,
	** then it is OK for term_type_info to be NULL.
	**
	** This code could be tighter. In general, we want to
	** handle our own allocations rather than using MR_GC_malloc().
	** (Note: we need to use MR_GC_malloc() rather than malloc()
	** or MR_malloc() because the Boehm collector doesn't trace memory
	** allocated with malloc() or MR_malloc().)
	**
	** NOTE: If you are changing this code, you might also need
	** to change the code in MR_create_type_info (defined above),
	** which does much the same thing, only allocating on the 
	** Mercury heap instead of using MR_GC_malloc().
	*/

Word *
MR_make_type_info(const Word *term_type_info, const Word *arg_pseudo_type_info,
	MR_MemoryList *allocated) 
{
	return MR_make_type_info_maybe_existq(term_type_info, 
		arg_pseudo_type_info, NULL,
		MR_RTTI_VERSION__CLEAN_LAYOUT, NULL, allocated);
}

	/*
	** The same as MR_make_type_info except that the type-info being
	** created may be for an existentially typed argument of a constructor.
	** In order to handle this, it also takes the data value from which
	** the values whose pseudo type-info we are looking at was taken, as
	** well as the functor descriptor for that functor.
	*/

Word *
MR_make_type_info_maybe_existq(const Word *term_type_info, 
	const Word *arg_pseudo_type_info, const Word *data_value, 
	int rtti_version, const MR_DuFunctorDesc *functor_desc,
	MR_MemoryList *allocated) 
{
	MR_TypeCtorInfo	type_ctor_info;
	Word		*arg_type_info;
	Word		*type_info;
	int		extra_args;
	int		arity;
	int		i;

	/* 
	** The arg_pseudo_type_info might be a polymorphic variable.
	** If so, then substitute its value, and then we're done.
	*/
	if (TYPEINFO_IS_VARIABLE(arg_pseudo_type_info)) {

		arg_type_info = MR_get_arg_type_info(term_type_info, 
			arg_pseudo_type_info, data_value, rtti_version,
			functor_desc);

		if (TYPEINFO_IS_VARIABLE(arg_type_info)) {
			fatal_error("make_type_info: "
				"unbound type variable");
		}
		return arg_type_info;
	}

	type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(arg_pseudo_type_info);

	/* no arguments - optimise common case */
	if ((Word) type_ctor_info == (Word) arg_pseudo_type_info) {
		return (Word *) type_ctor_info;
	} 

	if (MR_TYPE_CTOR_INFO_IS_HO(type_ctor_info)) {
		arity = MR_TYPEINFO_GET_HIGHER_ARITY(arg_pseudo_type_info);
		extra_args = 2;
	} else {
		arity = MR_TYPE_CTOR_INFO_GET_TYPE_ARITY(type_ctor_info);
		extra_args = 1;
	}

	/*
	** Iterate over the arguments, figuring out whether we
	** need to make any substitutions.
	** If so, copy the resulting argument type-infos into
	** a new type_info.
	*/
	type_info = NULL;
	for (i = extra_args; i < arity + extra_args; i++) {
		arg_type_info = MR_make_type_info(term_type_info,
			(Word *) arg_pseudo_type_info[i], allocated);
		if (TYPEINFO_IS_VARIABLE(arg_type_info)) {
			fatal_error("MR_make_type_info: "
				"unbound type variable");
		}
		if (arg_type_info != (Word *) arg_pseudo_type_info[i]) {
			/*
			** We made a substitution.
			** We need to allocate a new type_info,
			** if we haven't done so already.
			*/
			if (type_info == NULL) {
				MR_MemoryList node;
				/*
				** allocate a new type_info and copy the
				** data across from arg_pseudo_type_info
				*/
				type_info = MR_GC_NEW_ARRAY(Word,
					arity + extra_args);
				memcpy(type_info, arg_pseudo_type_info,
					(arity + extra_args) * sizeof(Word));
				/*
				** insert this type_info cell into the linked
				** list of allocated memory cells, so we can
				** free it later on
				*/
				node = MR_GC_malloc(sizeof(*node));
				node->data = type_info;
				node->next = *allocated;
				*allocated = node;
			}

			type_info[i] = (Word) arg_type_info;
		}
	}

	if (type_info == NULL) {
		return (Word *) (Word) arg_pseudo_type_info;
	} else {
		return type_info;
	}

} /* end MR_make_type_info() */

/*---------------------------------------------------------------------------*/

enum MR_DiscUnionTagRepresentation
MR_get_tag_representation(Word layout_entry)
{
	switch ((int) MR_tag(layout_entry)) {
		case TYPE_CTOR_LAYOUT_UNSHARED_TAG:
			return MR_DISCUNIONTAG_UNSHARED;
		case TYPE_CTOR_LAYOUT_SHARED_REMOTE_TAG:
			return MR_DISCUNIONTAG_SHARED_REMOTE;
		case TYPE_CTOR_LAYOUT_CONST_TAG:
			return MR_DISCUNIONTAG_SHARED_LOCAL;
		default:
		fatal_error("MR_get_tag_representation: unknown tag representation");
	}
}

/*---------------------------------------------------------------------------*/
