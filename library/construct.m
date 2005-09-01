% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
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

:- import_module list.
:- import_module std_util.
:- import_module type_desc.

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
    list(type_desc__pseudo_type_desc)::out) is semidet.

    % get_functor_with_names(Type, FunctorNumber, FunctorName, Arity, ArgTypes,
    %   ArgNames)
    %
    % Binds FunctorName and Arity to the name and arity of functor number
    % FunctorNumber for the specified type, ArgTypes to the type_descs
    % for the types of the arguments of that functor, and ArgNames to the
    % field name of each functor argument, if any.  Fails if the type is
    % not a discriminated union type, or if FunctorNumber is out of range.
    %
:- pred get_functor_with_names(type_desc__type_desc::in, int::in, string::out,
    int::out, list(type_desc__pseudo_type_desc)::out, list(maybe(string))::out)
    is semidet.

    % get_functor_ordinal(Type, I, Ordinal)
    %
    % Returns Ordinal, where Ordinal is the position in declaration order
    % for the specified type of the function symbol that is in position I
    % in lexicographic order. Fails if the type is not a discriminated
    % union type, or if I is out of range.
    %
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
:- func construct(type_desc__type_desc::in, int::in, list(univ)::in)
    = (univ::out) is semidet.

    % construct_tuple(Args) = Term
    %
    % Returns a tuple whose arguments are given by Args.
    %
:- func construct_tuple(list(univ)) = univ.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module rtti_implementation.

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

num_functors(TypeDesc) = rtti_implementation__num_functors(TypeDesc).

get_functor(TypeInfo, FunctorNumber, FunctorName, Arity,
            PseudoTypeInfoList) :-
    get_functor_internal(TypeInfo, FunctorNumber, FunctorName, Arity,
            PseudoTypeInfoList).

get_functor_with_names(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList) :-
    get_functor_with_names_internal(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList0),
    ArgNameList = map(null_to_no, ArgNameList0).

:- pred get_functor_internal(type_desc__type_desc::in, int::in, string::out,
    int::out, list(type_desc__pseudo_type_desc)::out) is semidet.

get_functor_internal(TypeInfo, FunctorNumber, FunctorName, Arity,
        MaybeTypeInfoList) :-
    rtti_implementation__get_functor(TypeInfo, FunctorNumber,
        FunctorName, Arity, TypeInfoList),
    % The backends in which we use this definition of this predicate
    % don't yet support function symbols with existential types, which is
    % the only kind of function symbol in which we may want to return unbound.
    MaybeTypeInfoList = list__map(type_desc_to_pseudo_type_desc, TypeInfoList).

:- pragma foreign_proc("C",
    get_functor_internal(TypeDesc::in, FunctorNumber::in, FunctorName::out,
        Arity::out, PseudoTypeInfoList::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    int                 arity;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** If type_info is an equivalence type, expand it.
        */
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

        /*
        ** Get information for this functor number and store in construct_info.
        ** If this is a discriminated union type and if the functor number
        ** is in range, we succeed.
        */
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber, type_info,
        &construct_info);
    MR_restore_transient_registers();

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
            MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            MR_save_transient_registers();
            PseudoTypeInfoList = MR_type_params_vector_to_list(Arity,
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info));
            MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            PseudoTypeInfoList =
                MR_pseudo_type_info_vector_to_pseudo_type_info_list(arity,
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    construct_info.arg_pseudo_type_infos);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}").

:- pred get_functor_with_names_internal(type_desc__type_desc::in, int::in,
    string::out, int::out, list(type_desc__pseudo_type_desc)::out,
    list(string)::out) is semidet.

get_functor_with_names_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        MaybeTypeInfoList, Names) :-
    rtti_implementation__get_functor_with_names(TypeDesc, FunctorNumber,
        FunctorName, Arity, TypeInfoList, Names),
    % The backends in which we use this definition of this predicate
    % don't yet support function symbols with existential types, which is
    % the only kind of function symbol in which we may want to return unbound.
    MaybeTypeInfoList = list__map(type_desc_to_pseudo_type_desc, TypeInfoList).

:- pragma foreign_proc("C",
    get_functor_with_names_internal(TypeDesc::in, FunctorNumber::in,
        FunctorName::out, Arity::out, PseudoTypeInfoList::out,
        ArgNameList::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    int                 arity;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

        /*
        ** If type_info is an equivalence type, expand it.
        */
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

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

        /*
        ** Get the functor name and arity, construct the list
        ** of type_infos for arguments.
        */

    if (success) {
        MR_make_aligned_string(FunctorName, construct_info.functor_name);
        arity = construct_info.arity;
        Arity = arity;

        if (MR_TYPE_CTOR_INFO_IS_TUPLE(
            MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info)))
        {
            int i;

            MR_save_transient_registers();
            PseudoTypeInfoList = MR_type_params_vector_to_list(Arity,
                MR_TYPEINFO_GET_VAR_ARITY_ARG_VECTOR(type_info));
            ArgNameList = MR_list_empty();
            for (i = 0; i < Arity; i++) {
                ArgNameList = MR_string_list_cons_msg((MR_Word) NULL,
                    ArgNameList, MR_PROC_LABEL);
            }
            MR_restore_transient_registers();
        } else {
            MR_save_transient_registers();
            PseudoTypeInfoList =
                MR_pseudo_type_info_vector_to_pseudo_type_info_list(arity,
                    MR_TYPEINFO_GET_FIXED_ARITY_ARG_VECTOR(type_info),
                    construct_info.arg_pseudo_type_infos);
            ArgNameList = MR_arg_name_vector_to_list(arity,
                construct_info.arg_names);
            MR_restore_transient_registers();
        }
    }
    SUCCESS_INDICATOR = success;
}").

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

:- pragma foreign_proc("C#",
    null(S::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = (S == null);
").

:- pragma foreign_proc("Java",
    null(S::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    succeeded = (S == null);
").

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
        ** If type_info is an equivalence type, expand it.
        */
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

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
            int                                 i;
            MR_ReservedAddrTypeLayout           ra_layout;
            int                                 total_reserved_addrs;
            const MR_ReservedAddrFunctorDesc    *functor_desc;

            ra_layout = MR_type_ctor_layout(type_ctor_info).
                MR_layout_reserved_addr;
            total_reserved_addrs = ra_layout->MR_ra_num_res_numeric_addrs
                + ra_layout->MR_ra_num_res_symbolic_addrs;

            for (i = 0; i < total_reserved_addrs; i++) {
                functor_desc = ra_layout->MR_ra_constants[i];
                if (functor_desc->MR_ra_functor_ordinal == FunctorNumber) {
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
                MR_Word                 arg_data;
                MR_TypeInfo             arg_type_info;
                int                     size;
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

                    MR_tag_offset_incr_hp_msg(new_data, ptag,
                        MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + 1 + arity,
                        MR_PROC_LABEL, ""<created by construct:construct/3>"");

                    size = MR_cell_size(arity);
                    MR_field(ptag, new_data, 0) =
                        functor_desc->MR_du_functor_secondary;
                    for (i = 0; i < arity; i++) {
                        arg_data = MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_DATA);
                        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_TYPEINFO);
                        MR_field(ptag, new_data, i + 1) = arg_data;
                        size += MR_term_size(arg_type_info, arg_data);
                        arg_list = MR_list_tail(arg_list);
                    }

                    MR_define_size_slot(ptag, new_data, size);
                    break;

                case MR_SECTAG_NONE:
                    arity = functor_desc->MR_du_functor_orig_arity;

                    MR_tag_offset_incr_hp_msg(new_data, ptag,
                        MR_SIZE_SLOT_SIZE, MR_SIZE_SLOT_SIZE + arity,
                        MR_PROC_LABEL, ""<created by construct:construct/3>"");

                    size = MR_cell_size(arity);
                    for (i = 0; i < arity; i++) {
                        arg_data = MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_DATA);
                        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_TYPEINFO);
                        MR_field(ptag, new_data, i) = arg_data;
                        size += MR_term_size(arg_type_info, arg_data);
                        arg_list = MR_list_tail(arg_list);
                    }

                    MR_define_size_slot(ptag, new_data, size);
                    break;

                case MR_SECTAG_VARIABLE:
                    new_data = (MR_Word) 0;     /* avoid a warning */
                    MR_fatal_error(""construct(): cannot construct variable"");

                default:
                    new_data = (MR_Word) 0;     /* avoid a warning */
                    MR_fatal_error(""construct(): unrecognised sectag locn"");
                }

                if (! MR_list_is_empty(arg_list)) {
                    MR_fatal_error(""excess arguments in construct:construct"");
                }
            }
            break;

        case MR_TYPECTOR_REP_TUPLE:
            {
                int         arity;
                int         i;
                int         size;
                MR_Word     arg_list;
                MR_Word     arg_data;
                MR_TypeInfo arg_type_info;

                arity = MR_TYPEINFO_GET_VAR_ARITY_ARITY(type_info);

                if (arity == 0) {
                    new_data = (MR_Word) NULL;
                } else {
                    MR_offset_incr_hp_msg(new_data, MR_SIZE_SLOT_SIZE,
                        MR_SIZE_SLOT_SIZE + arity, MR_PROC_LABEL,
                        ""<created by construct:construct/3>"");

                    size = MR_cell_size(arity);
                    arg_list = ArgList;
                    for (i = 0; i < arity; i++) {
                        arg_data = MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_DATA);
                        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_TYPEINFO);
                        MR_field(MR_mktag(0), new_data, i) = arg_data;
                        size += MR_term_size(arg_type_info, arg_data);
                        arg_list = MR_list_tail(arg_list);
                    }

                    MR_define_size_slot(MR_mktag(0), new_data, size);
                    if (! MR_list_is_empty(arg_list)) {
                        MR_fatal_error(
                            ""excess arguments in construct:construct"");
                    }
                }
            }
            break;

        default:
            new_data = (MR_Word) 0;     /* avoid a warning */
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

construct_tuple(Args) =
    construct_tuple_2(Args, list__map(univ_type, Args), list__length(Args)).

:- func construct_tuple_2(list(univ), list(type_desc__type_desc), int) = univ.

:- pragma foreign_proc("C",
    construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Term::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo type_info;
    MR_Word     new_data;
    int         i;
    MR_Word     arg_data;
    MR_TypeInfo arg_type_info;
    int         size;

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
        MR_offset_incr_hp_msg(new_data, MR_SIZE_SLOT_SIZE,
            MR_SIZE_SLOT_SIZE + Arity, MR_PROC_LABEL,
            ""<created by construct:construct_tuple/1>"");

        size = MR_cell_size(Arity);
        for (i = 0; i < Arity; i++) {
            arg_data = MR_field(MR_UNIV_TAG, MR_list_head(Args),
                MR_UNIV_OFFSET_FOR_DATA);
            arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                MR_list_head(Args), MR_UNIV_OFFSET_FOR_TYPEINFO);
            MR_field(MR_mktag(0), new_data, i) = arg_data;
            size += MR_term_size(arg_type_info, arg_data);
            Args = MR_list_tail(Args);
        }

        MR_define_size_slot(MR_mktag(0), new_data, size);
    }

    /*
    ** Create a univ.
    */
    MR_new_univ_on_hp(Term, type_info, new_data);
}").
