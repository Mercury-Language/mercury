%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: construct.m.
% Main author: zs.
% Stability: low.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module construct.

:- interface.

:- import_module std_util, list, type_desc.

	% num_functors(TypeInfo)
	%
	% Returns the number of different functors for the top-level
	% type constructor of the type specified by TypeInfo, or -1
	% if the type is not a discriminated union type.
	%
	% The functors of a discriminated union type are numbered from
	% zero to N-1, where N is the value returned by num_functors.
	% The functors are numbered in lexicographic order. If two
	% functors have the same name, the one with the lower arity
	% will have the lower number.
	%
:- func num_functors(type_desc__type_desc) = int.

	% get_functor(Type, FunctorNumber, FunctorName, Arity, ArgTypes)
	%
	% Binds FunctorName and Arity to the name and arity of functor number
	% FunctorNumber for the specified type, and binds ArgTypes to the
	% type_descs for the types of the arguments of that functor.
	% Fails if the type is not a discriminated union type, or if
	% FunctorNumber is out of range.
	%
:- pred get_functor(type_desc__type_desc::in, int::in, string::out, int::out,
		list(type_desc__type_desc)::out) is semidet.

	% get_functor(Type, FunctorNumber, FunctorName, Arity, ArgTypes,
	%	ArgNames)
	%
	% Binds FunctorName and Arity to the name and arity of functor number
	% FunctorNumber for the specified type, ArgTypes to the type_descs
	% for the types of the arguments of that functor, and ArgNames to the
	% field name of each functor argument, if any.  Fails if the type is
	% not a discriminated union type, or if FunctorNumber is out of range.
	%
:- pred get_functor(type_desc__type_desc::in, int::in, string::out, int::out,
		list(type_desc__type_desc)::out, list(maybe(string))::out)
		is semidet.

	% get_functor_ordinal(Type, I, Ordinal)
	%
	% Returns Ordinal, where Ordinal is the position in declaration order
	% for the specified type of the function symbol that is in position I
	% in lexicographic order. Fails if the type is not a discriminated
	% union type, or if I is out of range.
:- pred get_functor_ordinal(type_desc__type_desc::in, int::in, int::out)
	is semidet.

	% construct(TypeInfo, I, Args) = Term
	%
	% Returns a term of the type specified by TypeInfo whose functor
	% is functor number I of the type given by TypeInfo, and whose
	% arguments are given by Args.  Fails if the type is not a
	% discriminated union type, or if I is out of range, or if the
	% number of arguments supplied doesn't match the arity of the selected
	% functor, or if the types of the arguments do not match
	% the expected argument types of that functor.
	%
:- func construct(type_desc__type_desc, int, list(univ)) = univ.
:- mode construct(in, in, in) = out is semidet.

	% construct_tuple(Args) = Term
	%
	% Returns a tuple whose arguments are given by Args.
:- func construct_tuple(list(univ)) = univ.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "

#include ""mercury_type_desc.h""
#include ""mercury_construct.h""

").

:- pragma foreign_proc("C",
	num_functors(TypeInfo::in) = (Functors::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
	MR_save_transient_registers();
	Functors = MR_get_num_functors((MR_TypeInfo) TypeInfo);
	MR_restore_transient_registers();
}").

num_functors(_) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__num_functors").

:- pragma foreign_proc("C",
	get_functor(TypeDesc::in, FunctorNumber::in, FunctorName::out,
		Arity::out, TypeInfoList::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    int                 arity;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber,
                type_info, &construct_info);
    MR_restore_transient_registers();

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, (MR_String) (MR_Word)
                construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                        MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            MR_save_transient_registers();
            TypeInfoList = MR_type_params_vector_to_list(Arity,
                    MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info));
            MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            TypeInfoList = MR_pseudo_type_info_vector_to_type_info_list(
                arity,
                MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                construct_info.arg_pseudo_type_infos);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}").

get_functor(_, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__get_functor").

get_functor(TypeDesc, I, Functor, Arity, TypeInfoList, ArgNameList) :-
    get_functor_2(TypeDesc, I, Functor, Arity, TypeInfoList, ArgNameList0),
    ArgNameList = map(null_to_no, ArgNameList0).

:- func null_to_no(string) = maybe(string).

null_to_no(S) = ( if null(S) then no else yes(S) ).

:- pred null(string).
:- mode null(in) is semidet.

:- pragma foreign_proc("C",
	null(S::in),
	[will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = (S == NULL);
").

:- pragma foreign_proc("MC++",
	null(S::in),
	[will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = (S == NULL);
").

null(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__null").

:- pred get_functor_2(type_desc__type_desc::in, int::in, string::out, int::out,
	list(type_desc__type_desc)::out, list(string)::out) is semidet.

:- pragma foreign_proc("C",
	get_functor_2(TypeDesc::in, FunctorNumber::in, FunctorName::out,
		Arity::out, TypeInfoList::out, ArgNameList::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    int                 arity;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber,
                type_info, &construct_info);
    MR_restore_transient_registers();

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, (MR_String) (MR_Word)
                construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
                        MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            MR_save_transient_registers();
            TypeInfoList = MR_type_params_vector_to_list(Arity,
                    MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info));
            ArgNameList = MR_list_empty();
                MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            TypeInfoList = MR_pseudo_type_info_vector_to_type_info_list(
                arity, MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                construct_info.arg_pseudo_type_infos);
            ArgNameList = MR_arg_name_vector_to_list(
                arity, construct_info.arg_names);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}").

get_functor_2(_, _, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__get_functor_2").

:- pragma foreign_proc("C", 
	get_functor_ordinal(TypeDesc::in, FunctorNumber::in, Ordinal::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Get information for this functor number and
        ** store in construct_info. If this is a discriminated union
        ** type and if the functor number is in range, we
        ** succeed.
        */
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber, type_info,
        &construct_info);
    MR_restore_transient_registers();

    if (success) {
        switch (construct_info.type_ctor_rep) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            Ordinal = construct_info.functor_info.
                enum_functor_desc->MR_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
        case MR_TYPECTOR_REP_TUPLE:
            Ordinal = 0;
            break;

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
            Ordinal = construct_info.functor_info.
                du_functor_desc->MR_du_functor_ordinal;
            break;

        default:
            success = MR_FALSE;

        }
    }
    SUCCESS_INDICATOR = success;
}").

get_functor_ordinal(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__get_functor_ordinal").

:- pragma foreign_proc("C", 
	construct(TypeDesc::in, FunctorNumber::in, ArgList::in) = (Term::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_Word             new_data;
    MR_Construct_Info   construct_info;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** Check range of FunctorNum, get info for this
        ** functor.
        */
    MR_save_transient_registers();
    success =
        MR_get_functors_check_range(FunctorNumber, type_info, &construct_info)
        && MR_typecheck_arguments(type_info, construct_info.arity, ArgList,
                construct_info.arg_pseudo_type_infos);
    MR_restore_transient_registers();

        /*
        ** Build the new term in `new_data'.
        */
    if (success) {

        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        if (MR_type_ctor_rep(type_ctor_info) != construct_info.type_ctor_rep) {
            MR_fatal_error(""construct:construct: type_ctor_rep mismatch"");
        }

        switch (MR_type_ctor_rep(type_ctor_info)) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            new_data = construct_info.functor_info.enum_functor_desc->
                MR_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_NOTAG:
        case MR_TYPECTOR_REP_NOTAG_USEREQ:
        case MR_TYPECTOR_REP_NOTAG_GROUND:
        case MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            if (MR_list_is_empty(ArgList)) {
                MR_fatal_error(""notag arg list is empty"");
            }

            if (! MR_list_is_empty(MR_list_tail(ArgList))) {
                MR_fatal_error(""notag arg list is too long"");
            }

            new_data = MR_field(MR_UNIV_TAG, MR_list_head(ArgList),
                MR_UNIV_OFFSET_FOR_DATA);
            break;

        case MR_TYPECTOR_REP_RESERVED_ADDR:
        case MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
	    /*
	    ** First check whether the functor we want is one of the
	    ** reserved addresses.
	    */
	    {
		int i;
		MR_ReservedAddrTypeLayout ra_layout;
		int total_reserved_addrs;
		const MR_ReservedAddrFunctorDesc *functor_desc;

		ra_layout = MR_type_ctor_layout(type_ctor_info).
			MR_layout_reserved_addr;
		total_reserved_addrs = ra_layout->MR_ra_num_res_numeric_addrs
			+ ra_layout->MR_ra_num_res_symbolic_addrs;

		for (i = 0; i < total_reserved_addrs; i++) {
		    functor_desc = ra_layout->MR_ra_constants[i];
		    if (functor_desc->MR_ra_functor_ordinal == FunctorNumber)
		    {
		    	new_data = (MR_Word)
			    functor_desc->MR_ra_functor_reserved_addr;

		        /* `break' here would just exit the `for' loop */
			goto end_of_main_switch;
		    }
		}
	    }
		    
	    /*
	    ** Otherwise, it is not one of the reserved addresses,
	    ** so handle it like a normal DU type.
	    */

	    /* fall through */

        case MR_TYPECTOR_REP_DU:
        case MR_TYPECTOR_REP_DU_USEREQ:
            {
                const MR_DuFunctorDesc  *functor_desc;
                MR_Word                 arg_list;
                MR_Word                 ptag;
                MR_Word                 arity;
                int                     i;

                functor_desc = construct_info.functor_info.du_functor_desc;
                if (functor_desc->MR_du_functor_exist_info != NULL) {
                    MR_fatal_error(""not yet implemented: construction ""
                        ""of terms containing existentially types"");
                }

                arg_list = ArgList;
                ptag = functor_desc->MR_du_functor_primary;
                switch (functor_desc->MR_du_functor_sectag_locn) {
                case MR_SECTAG_LOCAL:
                    new_data = (MR_Word) MR_mkword(ptag,
                        MR_mkbody((MR_Word)
                            functor_desc->MR_du_functor_secondary));
                    break;

                case MR_SECTAG_REMOTE:
                    arity = functor_desc->MR_du_functor_orig_arity;

                    MR_tag_incr_hp_msg(new_data, ptag, arity + 1,
                        MR_PROC_LABEL, ""<created by construct:construct/3>"");

                    MR_field(ptag, new_data, 0) =
                        functor_desc->MR_du_functor_secondary;
                    for (i = 0; i < arity; i++) {
                        MR_field(ptag, new_data, i + 1) =
                            MR_field(MR_UNIV_TAG, 
			    	MR_list_head(arg_list),
                                MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    break;

                case MR_SECTAG_NONE:
                    arity = functor_desc->MR_du_functor_orig_arity;

                    MR_tag_incr_hp_msg(new_data, ptag, arity,
                        MR_PROC_LABEL, ""<created by construct:construct/3>"");

                    for (i = 0; i < arity; i++) {
                        MR_field(ptag, new_data, i) =
                            MR_field(MR_UNIV_TAG, 
			    	MR_list_head(arg_list),
                                MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    break;
                case MR_SECTAG_VARIABLE:
		    MR_fatal_error(""construct(): cannot construct variable"");
                }

                if (! MR_list_is_empty(arg_list)) {
                    MR_fatal_error(""excess arguments in construct:construct"");
                }
            }
            break;

        case MR_TYPECTOR_REP_TUPLE:
            {
                int    	arity;
		int	i;
                MR_Word    arg_list;

                arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);
    
                if (arity == 0) {
                    new_data = (MR_Word) NULL;
                } else {
                    MR_incr_hp_msg(new_data, arity, MR_PROC_LABEL,
                            ""<created by construct:construct/3>"");
            
                    arg_list = ArgList;
                    for (i = 0; i < arity; i++) {
                        MR_field(MR_mktag(0), new_data, i) =
                            MR_field(MR_UNIV_TAG, MR_list_head(arg_list),
                                    MR_UNIV_OFFSET_FOR_DATA);
                        arg_list = MR_list_tail(arg_list);
                    }

                    if (! MR_list_is_empty(arg_list)) {
                        MR_fatal_error(
                                ""excess arguments in construct:construct"");
                    }
                }
            }
            break;

        default:
            MR_fatal_error(""bad type_ctor_rep in construct:construct"");
        }

    end_of_main_switch:

        /*
        ** Create a univ.
        */

        MR_new_univ_on_hp(Term, type_info, new_data);
    }

    SUCCESS_INDICATOR = success;
}").

construct(_, _, _) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__construct").

construct_tuple(Args) =
	construct_tuple_2(Args,
		list__map(univ_type, Args),
		list__length(Args)).

:- func construct_tuple_2(list(univ), list(type_desc__type_desc), int) = univ.

:- pragma foreign_proc("C", 
	construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Term::out),
	[will_not_call_mercury, thread_safe, promise_pure],
"{
	MR_TypeInfo type_info;
	MR_Word new_data;
	MR_Word arg_value;
	int i;

	/*
	** Construct a type_info for the tuple.
	*/
	MR_save_transient_registers();
	type_info = MR_make_type(Arity, MR_TYPECTOR_DESC_MAKE_TUPLE(Arity),
			ArgTypes);
	MR_restore_transient_registers();

	/*
	** Create the tuple.
	*/
	if (Arity == 0) {
		new_data = (MR_Word) NULL;
	} else {
		MR_incr_hp_msg(new_data, Arity, MR_PROC_LABEL,
			""<created by construct:construct_tuple/1>"");
		for (i = 0; i < Arity; i++) {
			arg_value = MR_field(MR_UNIV_TAG, 
					MR_list_head(Args),
					MR_UNIV_OFFSET_FOR_DATA);
			MR_field(MR_mktag(0), new_data, i) = arg_value;
			Args = MR_list_tail(Args);
		}
	}

	/*
	** Create a univ.
	*/
	MR_new_univ_on_hp(Term, type_info, new_data);
}").

construct_tuple_2(_, _, _) = _ :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("construct__construct_tuple_2").
