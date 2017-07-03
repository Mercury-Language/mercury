%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: construct.m.
% Main author: zs.
% Stability: low.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module construct.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module univ.
:- import_module type_desc.

%---------------------------------------------------------------------------%

    % The functors of a discriminated union type are numbered from
    % zero to N-1, where N is the value returned by num_functors.
    % The functors are numbered in lexicographic order. If two
    % functors have the same name, the one with the lower arity
    % will have the lower number.
    %
:- type functor_number_ordinal == int.
:- type functor_number_lex == int.

    % num_functors(Type).
    %
    % Returns the number of different functors for the top-level
    % type constructor of the type specified by Type.
    % Fails if the type is not a discriminated union type.
    %
    % deconstruct.functor_number/3, deconstruct.deconstruct_du/5
    % and the semidet predicates and functions in this module will
    % only succeed for types for which num_functors/1 succeeds.
    %
:- func num_functors(type_desc) = int is semidet.

:- func det_num_functors(type_desc) = int.

    % get_functor(Type, FunctorNumber, FunctorName, Arity, ArgTypes).
    %
    % Binds FunctorName and Arity to the name and arity of functor number
    % FunctorNumber for the specified type, and binds ArgTypes to the
    % type_descs for the types of the arguments of that functor.
    % Fails if the type is not a discriminated union type, or if
    % FunctorNumber is out of range.
    %
:- pred get_functor(type_desc::in, functor_number_lex::in,
    string::out, int::out, list(pseudo_type_desc)::out) is semidet.

    % get_functor_with_names(Type, FunctorNumber, FunctorName, Arity, ArgTypes,
    %   ArgNames).
    %
    % Binds FunctorName and Arity to the name and arity of functor number
    % FunctorNumber for the specified type, ArgTypes to the type_descs
    % for the types of the arguments of that functor, and ArgNames to the
    % field name of each functor argument, if any. Fails if the type is
    % not a discriminated union type, or if FunctorNumber is out of range.
    %
:- pred get_functor_with_names(type_desc::in, functor_number_lex::in,
    string::out, int::out, list(pseudo_type_desc)::out,
    list(maybe(string))::out) is semidet.

    % get_functor_ordinal(Type, I) = Ordinal.
    %
    % Returns Ordinal, where Ordinal is the position in declaration order
    % for the specified type of the function symbol that is in position I
    % in lexicographic order. Fails if the type is not a discriminated
    % union type, or if I is out of range.
    %
:- func get_functor_ordinal(type_desc, functor_number_lex) =
    functor_number_ordinal is semidet.
:- pred get_functor_ordinal(type_desc::in, functor_number_lex::in,
    functor_number_ordinal::out) is semidet.

    % get_functor_lex(Type, Ordinal) = I.
    %
    % Returns I, where I is the position in lexicographic order for the
    % specified type of the function symbol that is in position Ordinal
    % in declaration order. Fails if the type is not a discriminated
    % union type, or if Ordinal is out of range.
    %
:- func get_functor_lex(type_desc, functor_number_ordinal) =
    functor_number_lex is semidet.

    % find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes).
    %
    % Given a type descriptor, a functor name and arity, finds the functor
    % number and the types of its arguments. It thus serves as the converse
    % to get_functor/5.
    %
:- pred find_functor(type_desc::in, string::in, int::in,
    functor_number_lex::out, list(type_desc)::out) is semidet.

    % construct(Type, I, Args) = Term.
    %
    % Returns a term of the type specified by Type whose functor
    % is functor number I of the type given by Type, and whose
    % arguments are given by Args. Fails if the type is not a
    % discriminated union type, or if I is out of range, or if the
    % number of arguments supplied doesn't match the arity of the selected
    % functor, or if the types of the arguments do not match
    % the expected argument types of that functor.
    %
:- func construct(type_desc, functor_number_lex, list(univ)) = univ is semidet.

    % construct_tuple(Args) = Term.
    %
    % Returns a tuple whose arguments are given by Args.
    %
:- func construct_tuple(list(univ)) = univ.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

% For use by the Erlang backends.
%
:- use_module erlang_rtti_implementation.

% For use by the Java and C# backends.
%
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
    SUCCESS_INDICATOR = (Functors >= 0);
}").

num_functors(TypeDesc) = NumFunctors :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        NumFunctors = erlang_rtti_implementation.num_functors(TypeDesc)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        rtti_implementation.type_info_num_functors(TypeInfo, NumFunctors)
    ).

det_num_functors(TypeInfo) =
    ( if N = num_functors(TypeInfo) then
        N
    else
        unexpected($module, $pred, "type does not have functors")
    ).

get_functor(TypeDesc, FunctorNumber, FunctorName, Arity, PseudoTypeInfoList) :-
    get_functor_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList).

get_functor_with_names(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList) :-
    get_functor_with_names_internal(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList0),
    ArgNameList = map(null_to_no, ArgNameList0).

:- pred get_functor_internal(type_desc::in, int::in, string::out,
    int::out, list(pseudo_type_desc)::out) is semidet.

get_functor_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeDescList) :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        erlang_rtti_implementation.get_functor(TypeDesc, FunctorNumber,
            FunctorName, Arity, TypeDescList),
        % XXX This old comment is wrong now:
        % The backends in which we use this definition of this predicate don't
        % yet support function symbols with existential types, which is the
        % only kind of function symbol in which we may want to return unbound.
        PseudoTypeDescList = list.map(type_desc_to_pseudo_type_desc,
            TypeDescList)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        rtti_implementation.type_info_get_functor(TypeInfo, FunctorNumber,
            FunctorName, Arity, PseudoTypeInfoList),
        % Assumes they have the same representation.
        private_builtin.unsafe_type_cast(PseudoTypeInfoList,
            PseudoTypeDescList)
    ).

:- pragma foreign_proc("C",
    get_functor_internal(TypeDesc::in, FunctorNumber::in, FunctorName::out,
        Arity::out, PseudoTypeInfoList::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
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

:- pred get_functor_with_names_internal(type_desc::in, int::in,
    string::out, int::out, list(pseudo_type_desc)::out, list(string)::out)
    is semidet.

get_functor_with_names_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeDescList, Names) :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        erlang_rtti_implementation.get_functor_with_names(TypeDesc,
            FunctorNumber, FunctorName, Arity, TypeDescList, Names),
        % XXX This old comment is wrong now:
        % The backends in which we use this definition of this predicate don't
        % yet support function symbols with existential types, which is the
        % only kind of function symbol in which we may want to return unbound.
        PseudoTypeDescList = list.map(type_desc_to_pseudo_type_desc,
            TypeDescList)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        rtti_implementation.type_info_get_functor_with_names(TypeInfo,
            FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names),
        % Assumes they have the same representation.
        private_builtin.unsafe_type_cast(PseudoTypeInfoList,
            PseudoTypeDescList)
    ).

:- pragma foreign_proc("C",
    get_functor_with_names_internal(TypeDesc::in, FunctorNumber::in,
        FunctorName::out, Arity::out, PseudoTypeInfoList::out,
        ArgNameList::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
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
                    ArgNameList, MR_ALLOC_ID);
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

:- pred null(string::in) is semidet.

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
    SUCCESS_INDICATOR = (S == null);
").

:- pragma foreign_proc("Erlang",
    null(S::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    % Erlang doesn't have null pointers, but in erlang_rtti_implementation we
    % return empty strings (binaries) for the cases where functor arguments
    % have no names.
    SUCCESS_INDICATOR = (S =:= <<>>)
").

get_functor_ordinal(TypeDesc, FunctorNumber) = Ordinal :-
    get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal).

get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal) :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        erlang_rtti_implementation.get_functor_ordinal(TypeDesc, FunctorNumber,
            Ordinal)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        rtti_implementation.type_info_get_functor_ordinal(TypeInfo,
            FunctorNumber, Ordinal)
    ).

:- pragma foreign_proc("C",
    get_functor_ordinal(TypeDesc::in, FunctorNumber::in, Ordinal::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

    /*
    ** Get information for this functor number and store in construct_info.
    ** If this is a discriminated union type and if the functor number is
    ** in range, we succeed.
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

        case MR_TYPECTOR_REP_FOREIGN_ENUM:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            Ordinal = construct_info.functor_info.
                foreign_enum_functor_desc->MR_foreign_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_DUMMY:
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

        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
        case MR_TYPECTOR_REP_FUNC:
        case MR_TYPECTOR_REP_PRED:
        case MR_TYPECTOR_REP_INT:
        case MR_TYPECTOR_REP_UINT:
        case MR_TYPECTOR_REP_INT8:
        case MR_TYPECTOR_REP_UINT8:
        case MR_TYPECTOR_REP_INT16:
        case MR_TYPECTOR_REP_UINT16:
        case MR_TYPECTOR_REP_INT32:
        case MR_TYPECTOR_REP_UINT32:
        case MR_TYPECTOR_REP_FLOAT:
        case MR_TYPECTOR_REP_CHAR:
        case MR_TYPECTOR_REP_STRING:
        case MR_TYPECTOR_REP_BITMAP:
        case MR_TYPECTOR_REP_SUBGOAL:
        case MR_TYPECTOR_REP_VOID:
        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_STABLE_C_POINTER:
        case MR_TYPECTOR_REP_TYPEINFO:
        case MR_TYPECTOR_REP_TYPECTORINFO:
        case MR_TYPECTOR_REP_TYPECLASSINFO:
        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
        case MR_TYPECTOR_REP_TYPEDESC:
        case MR_TYPECTOR_REP_TYPECTORDESC:
        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
        case MR_TYPECTOR_REP_ARRAY:
        case MR_TYPECTOR_REP_REFERENCE:
        case MR_TYPECTOR_REP_SUCCIP:
        case MR_TYPECTOR_REP_HP:
        case MR_TYPECTOR_REP_CURFR:
        case MR_TYPECTOR_REP_MAXFR:
        case MR_TYPECTOR_REP_REDOFR:
        case MR_TYPECTOR_REP_REDOIP:
        case MR_TYPECTOR_REP_TRAIL_PTR:
        case MR_TYPECTOR_REP_TICKET:
        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
        case MR_TYPECTOR_REP_UNKNOWN:
            success = MR_FALSE;

        }
    }
    SUCCESS_INDICATOR = success;
}").

get_functor_lex(TypeDesc, Ordinal) = FunctorNumber :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        erlang_rtti_implementation.get_functor_lex(TypeDesc, Ordinal,
            FunctorNumber)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        rtti_implementation.type_info_get_functor_lex(TypeInfo, Ordinal,
            FunctorNumber)
    ).

:- pragma foreign_proc("C",
    get_functor_lex(TypeDesc::in, Ordinal::in) = (FunctorNumber::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_Construct_Info   construct_info;
    int                 num_functors;

    type_info = (MR_TypeInfo) TypeDesc;

    /*
    ** Get information for this functor number and store in construct_info.
    ** If this is a discriminated union type and if the functor number is
    ** in range, we succeed.
    */
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    num_functors = MR_get_num_functors(type_info);
    MR_restore_transient_registers();
    type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);
    if (Ordinal < 0 || Ordinal >= num_functors
            || !type_ctor_info->MR_type_ctor_functor_number_map)
    {
        SUCCESS_INDICATOR = MR_FALSE;
    } else {
        FunctorNumber =
            type_ctor_info->MR_type_ctor_functor_number_map[Ordinal];
        SUCCESS_INDICATOR = MR_TRUE;
    }
}").

find_functor(Type, Functor, Arity, FunctorNumber, ArgTypes) :-
    N = construct.num_functors(Type),
    find_functor_2(Type, Functor, Arity, N, FunctorNumber, ArgTypes).

:- pred find_functor_2(type_desc::in, string::in, int::in,
    int::in, int::out, list(type_desc)::out) is semidet.

find_functor_2(TypeInfo, Functor, Arity, Num0, FunctorNumber, ArgTypes) :-
    Num0 >= 0,
    Num = Num0 - 1,
    ( if get_functor(TypeInfo, Num, Functor, Arity, ArgPseudoTypes) then
        ArgTypes = list.map(det_ground_pseudo_type_desc_to_type_desc,
            ArgPseudoTypes),
        FunctorNumber = Num
    else
        find_functor_2(TypeInfo, Functor, Arity, Num, FunctorNumber, ArgTypes)
    ).

:- pragma no_inline(construct/3).
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

    /* If type_info is an equivalence type, expand it. */
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

    /* Check range of FunctorNum, get info for this functor. */
    MR_save_transient_registers();
    success =
        MR_get_functors_check_range(FunctorNumber, type_info, &construct_info)
        && MR_typecheck_arguments(type_info, construct_info.arity, ArgList,
            construct_info.arg_pseudo_type_infos);
    MR_restore_transient_registers();

    /* Build the new term in `new_data'. */
    if (success) {
        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        if (MR_type_ctor_rep(type_ctor_info) != construct_info.type_ctor_rep) {
            MR_fatal_error(""construct.construct: type_ctor_rep mismatch"");
        }

        switch (MR_type_ctor_rep(type_ctor_info)) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            new_data = construct_info.functor_info.enum_functor_desc->
                MR_enum_functor_ordinal;
            break;

        case MR_TYPECTOR_REP_FOREIGN_ENUM:
        case MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            new_data = construct_info.functor_info.foreign_enum_functor_desc->
                MR_foreign_enum_functor_value;
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

            if (!MR_notag_subtype_none(type_ctor_info,
                construct_info.functor_info.notag_functor_desc))
            {
                MR_fatal_error(""not yet implemented: construction ""
                    ""of terms containing subtype constraints"");
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
                const MR_DuArgLocn      *arg_locns;
                MR_Word                 arg_list;
                MR_Word                 ptag;
                MR_Word                 arity;
                MR_Word                 arg_data;
                MR_TypeInfo             arg_type_info;
                int                     args_size;
                int                     alloc_size;
                int                     size;
                MR_Unsigned             i;

                functor_desc = construct_info.functor_info.du_functor_desc;
                arg_locns = functor_desc->MR_du_functor_arg_locns;
                if (functor_desc->MR_du_functor_exist_info != NULL) {
                    MR_fatal_error(""not yet implemented: construction ""
                        ""of terms containing existential types"");
                }

                if (!MR_du_subtype_none(type_ctor_info, functor_desc)) {
                    MR_fatal_error(""not yet implemented: construction ""
                        ""of terms containing subtype constraints"");
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
                    args_size = MR_cell_size_for_args(arity, arg_locns);
                    alloc_size = MR_SIZE_SLOT_SIZE + 1 + args_size;

                    MR_tag_offset_incr_hp_msg(new_data, ptag,
                        MR_SIZE_SLOT_SIZE, alloc_size,
                        MR_ALLOC_ID, ""<created by construct.construct/3>"");

                    /*
                    ** Ensure words holding packed arguments are zeroed before
                    ** filling them in.
                    */
                  #ifndef MR_BOEHM_GC
                    if (arg_locns != NULL) {
                        MR_memset(new_data, 0, alloc_size * sizeof(MR_Word));
                    }
                  #endif

                    size = MR_cell_size(args_size);
                    MR_field(ptag, new_data, 0) =
                        functor_desc->MR_du_functor_secondary;
                    for (i = 0; i < arity; i++) {
                        arg_data = MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_DATA);
                        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_TYPEINFO);
                        if (arg_locns == NULL) {
                            MR_field(ptag, new_data, 1 + i) = arg_data;
                        } else {
                            const MR_DuArgLocn *locn = &arg_locns[i];

                            if (locn->MR_arg_bits == -1) {
                              #ifdef MR_BOXED_FLOAT
                                MR_memcpy(
                                    &MR_field(ptag, new_data,
                                        1 + locn->MR_arg_offset),
                                    (MR_Word *) arg_data, sizeof(MR_Float));
                              #else
                                MR_fatal_error(
                                    ""construct(): double precision float"");
                              #endif
                            } else {
                                MR_field(ptag, new_data,
                                    1 + locn->MR_arg_offset)
                                    |= (arg_data << locn->MR_arg_shift);
                            }
                        }
                        size += MR_term_size(arg_type_info, arg_data);
                        arg_list = MR_list_tail(arg_list);
                    }

                    MR_define_size_slot(ptag, new_data, size);
                    break;

                case MR_SECTAG_NONE:
                    arity = functor_desc->MR_du_functor_orig_arity;
                    args_size = MR_cell_size_for_args(arity, arg_locns);
                    alloc_size = MR_SIZE_SLOT_SIZE + args_size;

                    MR_tag_offset_incr_hp_msg(new_data, ptag,
                        MR_SIZE_SLOT_SIZE, alloc_size,
                        MR_ALLOC_ID, ""<created by construct.construct/3>"");

                    /*
                    ** Ensure words holding packed arguments are zeroed before
                    ** filling them in.
                    */
                  #ifndef MR_BOEHM_GC
                    if (arg_locns != NULL) {
                        MR_memset(new_data, 0, alloc_size * sizeof(MR_Word));
                    }
                  #endif

                    size = MR_cell_size(args_size);
                    for (i = 0; i < arity; i++) {
                        arg_data = MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_DATA);
                        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
                            MR_list_head(arg_list),
                            MR_UNIV_OFFSET_FOR_TYPEINFO);
                        if (arg_locns == NULL) {
                            MR_field(ptag, new_data, i) = arg_data;
                        } else {
                            const MR_DuArgLocn *locn = &arg_locns[i];

                            if (locn->MR_arg_bits == -1) {
                              #ifdef MR_BOXED_FLOAT
                                MR_memcpy(&MR_field(ptag, new_data,
                                    locn->MR_arg_offset),
                                    (MR_Word *) arg_data, sizeof(MR_Float));
                              #else
                                MR_fatal_error(
                                    ""construct(): double-precision float"");
                              #endif
                            } else {
                                MR_field(ptag, new_data, locn->MR_arg_offset)
                                    |= (arg_data << locn->MR_arg_shift);
                            }
                        }
                        size += MR_term_size(arg_type_info, arg_data);
                        arg_list = MR_list_tail(arg_list);
                    }

                    MR_define_size_slot(ptag, new_data, size);
                    break;

                case MR_SECTAG_NONE_DIRECT_ARG:
                    arity = functor_desc->MR_du_functor_orig_arity;
                    if (arity != 1) {
                        MR_fatal_error(
                            ""construct(): direct_arg_tag arity != 1"");
                    }

                    arg_data = MR_field(MR_UNIV_TAG, MR_list_head(arg_list),
                        MR_UNIV_OFFSET_FOR_DATA);
                    new_data = (MR_Word) MR_mkword(MR_mktag(ptag), arg_data);
                    arg_list = MR_list_tail(arg_list);
                    break;

                case MR_SECTAG_VARIABLE:
                    new_data = (MR_Word) 0;     /* avoid a warning */
                    MR_fatal_error(""construct(): cannot construct variable"");

#ifdef MR_INCLUDE_SWITCH_DEFAULTS
                default:
                    new_data = (MR_Word) 0;     /* avoid a warning */
                    MR_fatal_error(""construct(): unrecognised sectag locn"");
#endif

                }

                if (! MR_list_is_empty(arg_list)) {
                    MR_fatal_error(""excess arguments in construct.construct"");
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
                        MR_SIZE_SLOT_SIZE + arity, MR_ALLOC_ID,
                        ""<created by construct.construct/3>"");

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
                            ""excess arguments in construct.construct"");
                    }
                }
            }
            break;

        case MR_TYPECTOR_REP_DUMMY:
            /*
            ** The value of the dummy type will never be looked at,
            ** so it doesn't matter what new_data is set to.
            */
            new_data = (MR_Word) 0;
            break;

        case MR_TYPECTOR_REP_INT:
            /* ints don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct int with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT:
            /* uints don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct uint with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT8:
            /* int8s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct int8 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT8:
            /* uint8s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct uint8 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT16:
            /* int16s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct int16 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT16:
            /* uint16s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct uint16 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT32:
            /* int32s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct int32 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT32:
            /* uint32s don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct uint32 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_FLOAT:
            /* floats don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct floats with construct.construct"");
            break;

        case MR_TYPECTOR_REP_CHAR:
            /* chars don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct chars with construct.construct"");
            break;

        case MR_TYPECTOR_REP_STRING:
            /* strings don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct strings with construct.construct"");
            break;

        case MR_TYPECTOR_REP_BITMAP:
            /* bitmaps don't have functor ordinals. */
            MR_fatal_error(
                ""cannot construct bitmaps with construct.construct"");
            break;

        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
            /* These should be eliminated by MR_collapse_equivalences above. */
            MR_fatal_error(""equiv type in construct.construct"");
            break;

        case MR_TYPECTOR_REP_VOID:
            MR_fatal_error(
                ""cannot construct void values with construct.construct"");
            break;

        case MR_TYPECTOR_REP_FUNC:
            MR_fatal_error(
                ""cannot construct functions with construct.construct"");
            break;

        case MR_TYPECTOR_REP_PRED:
            MR_fatal_error(
                ""cannot construct predicates with construct.construct"");
            break;

        case MR_TYPECTOR_REP_SUBGOAL:
            MR_fatal_error(
                ""cannot construct subgoals with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TYPEDESC:
            MR_fatal_error(
                ""cannot construct type_descs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TYPECTORDESC:
            MR_fatal_error(
                ""cannot construct type_descs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_PSEUDOTYPEDESC:
            MR_fatal_error(
                ""cannot construct pseudotype_descs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TYPEINFO:
            MR_fatal_error(
                ""cannot construct type_infos with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TYPECTORINFO:
            MR_fatal_error(
                ""cannot construct type_ctor_infos with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TYPECLASSINFO:
            MR_fatal_error(
                ""cannot construct type_class_infos with construct.construct"");
            break;

        case MR_TYPECTOR_REP_BASETYPECLASSINFO:
            MR_fatal_error(
                ""cannot construct base_type_class_infos ""
                ""with construct.construct"");
            break;

        case MR_TYPECTOR_REP_SUCCIP:
            MR_fatal_error(
                ""cannot construct succips with construct.construct"");
            break;

        case MR_TYPECTOR_REP_HP:
            MR_fatal_error(
                ""cannot construct hps with construct.construct"");
            break;

        case MR_TYPECTOR_REP_CURFR:
            MR_fatal_error(
                ""cannot construct curfrs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_MAXFR:
            MR_fatal_error(
                ""cannot construct maxfrs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_REDOFR:
            MR_fatal_error(
                ""cannot construct redofrs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_REDOIP:
            MR_fatal_error(
                ""cannot construct redoips with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TRAIL_PTR:
            MR_fatal_error(
                ""cannot construct trail_ptrs with construct.construct"");
            break;

        case MR_TYPECTOR_REP_TICKET:
            MR_fatal_error(
                ""cannot construct tickets with construct.construct"");
            break;

        case MR_TYPECTOR_REP_C_POINTER:
        case MR_TYPECTOR_REP_STABLE_C_POINTER:
            MR_fatal_error(
                ""cannot construct c_pointers with construct.construct"");
            break;

        case MR_TYPECTOR_REP_ARRAY:
            MR_fatal_error(
                ""cannot construct arrays with construct.construct"");
            break;

        case MR_TYPECTOR_REP_REFERENCE:
            MR_fatal_error(
                ""cannot construct references with construct.construct"");
            break;

        case MR_TYPECTOR_REP_FOREIGN:
        case MR_TYPECTOR_REP_STABLE_FOREIGN:
            MR_fatal_error(
                ""cannot construct values of foreign types ""
                ""with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error(
                ""cannot construct values of unknown types ""
                ""with construct.construct"");
            break;

#ifdef MR_INCLUDE_SWITCH_DEFAULTS

        default:
            new_data = (MR_Word) 0;     /* avoid a warning */
            MR_fatal_error(""bad type_ctor_rep in construct.construct"");

#endif
        }

    end_of_main_switch:

        /*
        ** Create a univ.
        */

        MR_new_univ_on_hp(Term, type_info, new_data);
    }

    SUCCESS_INDICATOR = success;
}").

construct(TypeDesc, Index, Args) = Term :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        Term = erlang_rtti_implementation.construct(TypeDesc, Index, Args)
    else
        type_desc_to_type_info(TypeDesc, TypeInfo),
        Term = rtti_implementation.construct(TypeInfo, Index, Args)
    ).

construct_tuple(Args) =
    construct_tuple_2(Args, list.map(univ_type, Args), list.length(Args)).

:- func construct_tuple_2(list(univ), list(type_desc), int) = univ.

:- pragma foreign_proc("C",
    construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Term::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
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
            MR_SIZE_SLOT_SIZE + Arity, MR_ALLOC_ID,
            ""<created by construct.construct_tuple/1>"");

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

construct_tuple_2(Args, ArgTypeDescs, Arity) = Term :-
    ( if erlang_rtti_implementation.is_erlang_backend then
        Term = erlang_rtti_implementation.construct_tuple_2(Args, ArgTypeDescs,
            Arity)
    else
        list.map(type_desc_to_type_info, ArgTypeDescs, ArgTypeInfos),
        Term = rtti_implementation.construct_tuple_2(Args, ArgTypeInfos, Arity)
    ).

%---------------------------------------------------------------------------%
