%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2022, 2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: construct.m.
% Main author: zs.
% Stability: high.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module construct.
:- interface.

:- import_module list.
:- import_module maybe.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%

    % The functors of a discriminated union type are numbered from
    % zero to N-1, where N is the value returned by num_functors.
    % The functors are numbered in lexicographic order. If two
    % functors have the same name, the one with the lower arity
    % will have the lower number.
    %
:- type functor_number_ordinal == int.
:- type functor_number_lex == int.

    % num_functors(Type) = NumFunctors.
    % num_functors(Type, NumFunctors).
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
% NOTE_TO_IMPLEMENTORS CFF :- pragma obsolete(func(num_functors/1), [num_functors/2]).
:- pred num_functors(type_desc::in, int::out) is semidet.

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
    % get_functor_ordinal(Type, I, Ordinal).
    %
    % Returns Ordinal, where Ordinal is the position in declaration order
    % for the specified type of the function symbol that is in position I
    % in lexicographic order. Fails if the type is not a discriminated
    % union type, or if I is out of range.
    %
:- func get_functor_ordinal(type_desc, functor_number_lex) =
    functor_number_ordinal is semidet.
% NOTE_TO_IMPLEMENTORS CFF :- pragma obsolete(func(get_functor_ordinal/2), [get_functor_ordinal/3]).
:- pred get_functor_ordinal(type_desc::in, functor_number_lex::in,
    functor_number_ordinal::out) is semidet.

    % get_functor_lex(Type, Ordinal) = I.
    % get_functor_lex(Type, Ordinal, I).
    %
    % Returns I, where I is the position in lexicographic order for the
    % specified type of the function symbol that is in position Ordinal
    % in declaration order. Fails if the type is not a discriminated
    % union type, or if Ordinal is out of range.
    %
:- func get_functor_lex(type_desc, functor_number_ordinal) =
    functor_number_lex is semidet.
% NOTE_TO_IMPLEMENTORS CFF :- pragma obsolete(func(get_functor_lex/2), [get_functor_lex/3]).
:- pred get_functor_lex(type_desc::in, functor_number_ordinal::in,
    functor_number_lex::out) is semidet.

    % find_functor(Type, FunctorName, Arity, FunctorNumber, ArgTypes).
    %
    % Given a type descriptor, a functor name and arity, finds the functor
    % number and the types of its arguments. It thus serves as the converse
    % to get_functor/5.
    %
:- pred find_functor(type_desc::in, string::in, int::in,
    functor_number_lex::out, list(type_desc)::out) is semidet.

    % construct(Type, I, Args) = Term.
    % construct(Type, I, Args, Term).
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
% NOTE_TO_IMPLEMENTORS CFF :- pragma obsolete(func(construct/3), [construct/4]).
:- pred construct(type_desc::in, functor_number_lex::in, list(univ)::in,
    univ::out) is semidet.

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

% For use by the Java and C# backends.
%
:- use_module rtti_implementation.

:- pragma foreign_decl("C", "

#include ""mercury_type_desc.h""
#include ""mercury_construct.h""

").

%---------------------------------------------------------------------------%

num_functors(TypeDesc) = NumFunctors :-
    num_functors(TypeDesc, NumFunctors).

:- pragma foreign_proc("C",
    num_functors(TypeInfo::in, Functors::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_save_transient_registers();
    Functors = MR_get_num_functors((MR_TypeInfo) TypeInfo);
    MR_restore_transient_registers();
    SUCCESS_INDICATOR = (Functors >= 0);
}").

num_functors(TypeDesc, NumFunctors) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_info_num_functors(TypeInfo, NumFunctors).

%---------------------%

det_num_functors(TypeInfo) =
    ( if num_functors(TypeInfo, N) then
        N
    else
        unexpected($pred, "type does not have functors")
    ).

%---------------------%

get_functor(TypeDesc, FunctorNumber, FunctorName, Arity, PseudoTypeInfoList) :-
    get_functor_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList).

get_functor_with_names(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList) :-
    get_functor_with_names_internal(TypeDesc, I, Functor, Arity,
        PseudoTypeInfoList, ArgNameList0),
    ArgNameList = map(null_to_no, ArgNameList0).

%---------------------%

:- pred get_functor_internal(type_desc::in, int::in, string::out,
    int::out, list(pseudo_type_desc)::out) is semidet.

get_functor_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeDescList) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_info_get_functor(TypeInfo, FunctorNumber,
        FunctorName, Arity, PseudoTypeInfoList),
    % Assumes they have the same representation.
    private_builtin.unsafe_type_cast(PseudoTypeInfoList, PseudoTypeDescList).

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

    // If type_info is an equivalence type, expand it.
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

    // Get information for this functor number and store in construct_info.
    // If this is a discriminated union type and if the functor number
    // is in range, we succeed.
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber, type_info, MR_FALSE,
        &construct_info);
    MR_restore_transient_registers();

    // Get the functor name and arity, construct the list  of type_infos
    // for arguments.
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

%---------------------%

:- pred get_functor_with_names_internal(type_desc::in, int::in,
    string::out, int::out, list(pseudo_type_desc)::out, list(string)::out)
    is semidet.

get_functor_with_names_internal(TypeDesc, FunctorNumber, FunctorName, Arity,
        PseudoTypeDescList, Names) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_info_get_functor_with_names(TypeInfo,
        FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names),
    % Assumes they have the same representation.
    private_builtin.unsafe_type_cast(PseudoTypeInfoList, PseudoTypeDescList).

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

    // If type_info is an equivalence type, expand it.
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

    // Get information for this functor number and store in construct_info.
    // If this is a discriminated union type and if the functor number
    // is in range, we succeed.
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber, type_info, MR_FALSE,
        &construct_info);
    MR_restore_transient_registers();

    // Get the functor name and arity, construct the list of type_infos
    // for arguments.

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

%---------------------%

:- func null_to_no(string) = maybe(string).

null_to_no(S) = ( if null(S) then no else yes(S) ).

%---------------------%

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

%---------------------%

get_functor_ordinal(TypeDesc, FunctorNumber) = Ordinal :-
    get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal).

get_functor_ordinal(TypeDesc, FunctorNumber, Ordinal) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_info_get_functor_ordinal(TypeInfo,
        FunctorNumber, Ordinal).

:- pragma foreign_proc("C",
    get_functor_ordinal(TypeDesc::in, FunctorNumber::in, Ordinal::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_Construct_Info   construct_info;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

    // Get information for this functor number and store in construct_info.
    // If this is a discriminated union type and if the functor number is
    // in range, we succeed.
    MR_save_transient_registers();
    success = MR_get_functors_check_range(FunctorNumber, type_info, MR_TRUE,
        &construct_info);
    MR_restore_transient_registers();

    if (success) {
        Ordinal = construct_info.functor_ordinal;
    } else {
        Ordinal = 0;
    }
    SUCCESS_INDICATOR = success;
}").

%---------------------%

get_functor_lex(TypeDesc, Ordinal) = FunctorNumber :-
    get_functor_lex(TypeDesc, Ordinal, FunctorNumber).

get_functor_lex(TypeDesc, Ordinal, FunctorNumber) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    rtti_implementation.type_info_get_functor_lex(TypeInfo, Ordinal,
        FunctorNumber).

:- pragma foreign_proc("C",
    get_functor_lex(TypeDesc::in, Ordinal::in, FunctorNumber::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_Construct_Info   construct_info;
    int                 num_functors;

    type_info = (MR_TypeInfo) TypeDesc;

    // Get information for this functor number and store in construct_info.
    // If this is a discriminated union type and if the functor number is
    // in range, we succeed.
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

%---------------------%

find_functor(Type, Functor, Arity, FunctorNumber, ArgTypes) :-
    construct.num_functors(Type, N),
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

%---------------------%

:- pragma foreign_decl("C",
"
// This function exists to allow us to handle both the MR_SECTAG_REMOTE_*
// and the MR_SECTAG_NONE cases of constructing values of discriminated
// union types without code duplication.
//
// Using a macro would be faster, but a function is easier to get right
// in terms of issues such as scopes, and it is easier to debug.
//
// XXX ARG_PACK
// Once this code has been operation for a while without problems,
// we should consider turning it into a macro.

extern void         ML_copy_memory_cell_args(MR_Word *arg_list_ptr,
                        MR_Word *new_data_ptr, const MR_Word ptag,
                        const MR_DuFunctorDesc *functor_desc,
                        const MR_bool has_sectag,
                        const MR_AllocSiteInfoPtr alloc_id);

// This is a version of ML_copy_memory_cell_args that puts arguments
// not into a memory cell, but next to the primary and secondary tag.
extern MR_Unsigned  ML_copy_tagword_args(MR_Word *arg_list_ptr,
                        const MR_Word ptag,
                        const MR_DuFunctorDesc *functor_desc);
").

:- pragma foreign_code("C",
"
void
ML_copy_memory_cell_args(MR_Word *arg_list_ptr, MR_Word *new_data_ptr,
    const MR_Word ptag, const MR_DuFunctorDesc *functor_desc,
    const MR_bool has_sectag, const MR_AllocSiteInfoPtr alloc_id)
{
    MR_Word             arg_list = *arg_list_ptr;
    MR_Word             new_data;
    const MR_Word       arity = functor_desc->MR_du_functor_orig_arity;
    const MR_DuArgLocn  *arg_locns = functor_desc->MR_du_functor_arg_locns;
    const int           sectag01 = (has_sectag ? 1 : 0);
    int                 args_size = MR_cell_size_for_args(arity, arg_locns);
    int                 alloc_size = MR_SIZE_SLOT_SIZE + sectag01 + args_size;
    int                 size;
    MR_Unsigned         i;

    MR_tag_offset_incr_hp_msg(new_data, ptag, MR_SIZE_SLOT_SIZE, alloc_size,
        alloc_id, ""<created by construct.construct/3>"");
    *new_data_ptr = new_data;

    // Ensure words holding packed arguments are zeroed before filling them in.
  #ifndef MR_BOEHM_GC
    if (arg_locns != NULL) {
        MR_memset((void *) new_data, 0, alloc_size * sizeof(MR_Word));
    }
  #endif

    size = MR_cell_size(args_size);
    if (has_sectag) {
        MR_field(ptag, new_data, 0) = functor_desc->MR_du_functor_secondary;
    }

    for (i = 0; i < arity; i++) {
        MR_Word         arg_data;
        MR_TypeInfo     arg_type_info;
        MR_Unsigned     bits_to_or;

        arg_data = MR_field(MR_UNIV_TAG, MR_list_head(arg_list),
            MR_UNIV_OFFSET_FOR_DATA);
        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
            MR_list_head(arg_list), MR_UNIV_OFFSET_FOR_TYPEINFO);
        // XXX ARG_PACK This test is loop-invariant; lift it out of the loop.
        if (arg_locns == NULL) {
            MR_field(ptag, new_data, sectag01 + i) = arg_data;
        } else {
            const MR_DuArgLocn *locn = &arg_locns[i];

            // The meanings of the various special values of MR_arg_bits
            // and MR_arg_offset are documented next to the definition of
            // the MR_DuArgLocn type in mercury_type_info.h.

            switch (locn->MR_arg_bits) {

            case 0:
                if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(""construct(): full word arg in tagword"");
                }
                MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset)
                    = arg_data;
                break;

            case -1:
                // This is a double-precision floating point argument
                // that takes two words.
                if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(
                        ""construct(): double word arg in tagword"");
                }
  #ifdef MR_BOXED_FLOAT
                MR_memcpy(
                    &MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset),
                    (MR_Word *) arg_data, sizeof(MR_Float));
  #else
                MR_fatal_error(""construct(): double word float"");
  #endif
                break;

            case -2:
                // This is an int64 argument that takes two words.
                if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(
                        ""construct(): double word arg in tagword"");
                }
  #ifdef MR_BOXED_INT64S
                MR_memcpy(
                    &MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset),
                    (MR_Word *) arg_data, sizeof(int64_t));
  #else
                MR_fatal_error(""construct(): double word int64"");
  #endif
                break;

            case -3:
                // This is a uint64 argument that takes two words.
                if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(
                        ""construct(): double word arg in tagword"");
                }
  #ifdef MR_BOXED_INT64S
                MR_memcpy(
                    &MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset),
                    (MR_Word *) arg_data, sizeof(uint64_t));
  #else
                MR_fatal_error(""construct(): double word uint64"");
  #endif
                break;

            case -4:    // fall-through
            case -5:
                // This is an int8 (-4) or uint8 (-5) argument.
                bits_to_or = (((MR_Unsigned) arg_data) & 0xff);
                if (locn->MR_arg_offset == -1) {
                    MR_field(ptag, new_data, 0)
                        |= (bits_to_or << locn->MR_arg_shift);
                } else if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(""construct(): unknown negative offset"");
                } else {
                    MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset)
                        |= (bits_to_or << locn->MR_arg_shift);
                }
                break;

            case -6:    // fall-through
            case -7:
                // This is an int16 (-6) or uint16 (-7) argument.
                bits_to_or = (((MR_Unsigned) arg_data) & 0xffff);
                if (locn->MR_arg_offset == -1) {
                    MR_field(ptag, new_data, 0)
                        |= (bits_to_or << locn->MR_arg_shift);
                } else if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(""construct(): unknown negative offset"");
                } else {
                    MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset)
                        |= (bits_to_or << locn->MR_arg_shift);
                }
                break;

            case -8:    // fall-through
            case -9:
                // This is an int32 (-8) or uint32 (-9) argument.
                bits_to_or = (((MR_Unsigned) arg_data) & 0xffffffff);
                if (locn->MR_arg_offset == -1) {
                    MR_field(ptag, new_data, 0)
                        |= (bits_to_or << locn->MR_arg_shift);
                } else if (locn->MR_arg_offset < 0) {
                    MR_fatal_error(""construct(): unknown negative offset"");
                } else {
                    MR_field(ptag, new_data, sectag01 + locn->MR_arg_offset)
                        |= (bits_to_or << locn->MR_arg_shift);
                }
                break;

            case -10:
                // This is a dummy argument, which does not need setting.
                break;

            default:
                if (locn->MR_arg_bits > 0) {
                    bits_to_or = arg_data;
                    if (locn->MR_arg_offset == -1) {
                        MR_field(ptag, new_data, 0)
                            |= (bits_to_or << locn->MR_arg_shift);
                    } else if (locn->MR_arg_offset < 0) {
                        MR_fatal_error(
                            ""construct(): unknown negative offset"");
                    } else {
                        MR_field(ptag, new_data,
                                sectag01 + locn->MR_arg_offset)
                            |= (bits_to_or << locn->MR_arg_shift);
                    }
                } else {
                    MR_fatal_error(""unknown MR_arg_bits value"");
                }
                break;
            }
        }

        size += MR_term_size(arg_type_info, arg_data);
        arg_list = MR_list_tail(arg_list);
    }

    *arg_list_ptr = arg_list;
    MR_define_size_slot(ptag, new_data, size);
}

MR_Unsigned
ML_copy_tagword_args(MR_Word *arg_list_ptr, const MR_Word ptag,
    const MR_DuFunctorDesc *functor_desc)
{
    MR_Word             arg_list = *arg_list_ptr;
    MR_Unsigned         new_data;
    const MR_Word       arity = functor_desc->MR_du_functor_orig_arity;
    const MR_DuArgLocn  *arg_locns = functor_desc->MR_du_functor_arg_locns;
    MR_Unsigned         i;

    new_data = ptag | (functor_desc->MR_du_functor_secondary << MR_TAGBITS);

    for (i = 0; i < arity; i++) {
        MR_Word         arg_data;
        MR_TypeInfo     arg_type_info;

        arg_data = MR_field(MR_UNIV_TAG, MR_list_head(arg_list),
            MR_UNIV_OFFSET_FOR_DATA);
        arg_type_info = (MR_TypeInfo) MR_field(MR_UNIV_TAG,
            MR_list_head(arg_list), MR_UNIV_OFFSET_FOR_TYPEINFO);
        if (arg_locns == NULL) {
            MR_fatal_error(""construct(): arg_locns == NULL"");
        }

        const MR_DuArgLocn *locn = &arg_locns[i];

        // The meanings of the various special values of MR_arg_bits
        // are documented next to the definition of the MR_DuArgLocn type
        // in mercury_type_info.h.

        switch (locn->MR_arg_bits) {

        case 0:
            MR_fatal_error(""construct(): full word argument in tagword"");
            break;

        case -1:    // fall-through
        case -2:    // fall-through
        case -3:
            // This is an argument that takes two words, the type being
            // float, int64, or uint64.
            MR_fatal_error(""construct(): double word argument in tagword"");
            break;

        case -4:    // fall-through
        case -5:
            // This is an int8 (-4) or uint8 (-5) argument.
            new_data = new_data |
                ((((MR_Unsigned) arg_data) & 0xff) << locn->MR_arg_shift);
            break;

        case -6:    // fall-through
        case -7:
            // This is an int16 (-6) or uint16 (-7) argument.
            new_data = new_data |
                ((((MR_Unsigned) arg_data) & 0xffff) << locn->MR_arg_shift);
            break;

        case -8:    // fall-through
        case -9:
            // This is an int32 (-8) or uint32 (-9) argument.
            new_data = new_data |
                ((((MR_Unsigned) arg_data) & 0xffffffff)
                    << locn->MR_arg_shift);
            break;

        case -10:
            // This is a dummy argument, which does not need setting.
            break;

        default:
            if (locn->MR_arg_bits > 0) {
                MR_Unsigned arg_value = ((MR_Unsigned) arg_data) &
                    ((1 << locn->MR_arg_bits) - 1);
                new_data = new_data | (arg_value << locn->MR_arg_shift);
            } else {
                MR_fatal_error(""unknown MR_arg_bits value"");
            }
            break;
        }

        arg_list = MR_list_tail(arg_list);
    }

    *arg_list_ptr = arg_list;
    return new_data;
}
").

%---------------------------------------------------------------------------%

construct(TypeDesc, Index, Args) = Term :-
    construct(TypeDesc, Index, Args, Term).

:- pragma no_inline(pred(construct/4)).

construct(TypeDesc, Index, Args, Term) :-
    type_desc_to_type_info(TypeDesc, TypeInfo),
    Term = rtti_implementation.construct(TypeInfo, Index, Args).

:- pragma foreign_proc("C",
    construct(TypeDesc::in, FunctorNumber::in, ArgList::in, Term::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"{
    MR_TypeInfo         type_info;
    MR_TypeCtorInfo     type_ctor_info;
    MR_Word             new_data;
    MR_Construct_Info   construct_info;
    MR_bool             success;

    type_info = (MR_TypeInfo) TypeDesc;

    // If type_info is an equivalence type, expand it.
    MR_save_transient_registers();
    type_info = MR_collapse_equivalences(type_info);
    MR_restore_transient_registers();

    // Check range of FunctorNum, get info for this functor.
    MR_save_transient_registers();
    success =
        MR_get_functors_check_range(FunctorNumber, type_info, MR_FALSE,
            &construct_info);
    if (success) {
        success =
            MR_typecheck_arguments(type_info, construct_info.arity, ArgList,
                construct_info.arg_pseudo_type_infos);
    }
    MR_restore_transient_registers();

    // Build the new term in `new_data'.
    if (success) {
        type_ctor_info = MR_TYPEINFO_GET_TYPE_CTOR_INFO(type_info);

        if (MR_type_ctor_rep(type_ctor_info) != construct_info.type_ctor_rep) {
            MR_fatal_error(""construct.construct: type_ctor_rep mismatch"");
        }

        switch (MR_type_ctor_rep(type_ctor_info)) {

        case MR_TYPECTOR_REP_ENUM:
        case MR_TYPECTOR_REP_ENUM_USEREQ:
            new_data = construct_info.functor_info.enum_functor_desc->
                MR_enum_functor_value;
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

                case MR_SECTAG_LOCAL_REST_OF_WORD:
                    new_data = (MR_Word) MR_mkword(ptag,
                        MR_mkbody((MR_Word)
                            functor_desc->MR_du_functor_secondary));
                    break;

                case MR_SECTAG_LOCAL_BITS:
                    new_data = ML_copy_tagword_args(&arg_list, ptag,
                        functor_desc);
                    break;

                case MR_SECTAG_REMOTE_FULL_WORD:        // fall-through
                case MR_SECTAG_REMOTE_BITS:
                    MR_save_transient_registers();
                    ML_copy_memory_cell_args(&arg_list, &new_data, ptag,
                        functor_desc, MR_TRUE, MR_ALLOC_ID);
                    MR_restore_transient_registers();
                    break;

                case MR_SECTAG_NONE:
                    MR_save_transient_registers();
                    ML_copy_memory_cell_args(&arg_list, &new_data, ptag,
                        functor_desc, MR_FALSE, MR_ALLOC_ID);
                    MR_restore_transient_registers();
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
                    new_data = (MR_Word) 0;     // avoid a warning
                    MR_fatal_error(""construct(): cannot construct variable"");

#ifdef MR_INCLUDE_SWITCH_DEFAULTS
                default:
                    new_data = (MR_Word) 0;     // avoid a warning
                    MR_fatal_error(""construct(): unrecognised sectag locn"");
#endif

                }

                if (! MR_list_is_empty(arg_list)) {
                    MR_fatal_error(
                        ""excess arguments in construct.construct"");
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
            // The value of the dummy type will never be looked at,
            // so it doesn't matter what new_data is set to.
            new_data = (MR_Word) 0;
            break;

        case MR_TYPECTOR_REP_INT:
            // ints don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct int with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT:
            // uints don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct uint with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT8:
            // int8s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct int8 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT8:
            // uint8s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct uint8 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT16:
            // int16s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct int16 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT16:
            // uint16s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct uint16 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT32:
            // int32s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct int32 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT32:
            // uint32s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct uint32 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_INT64:
            // int64s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct int64 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_UINT64:
            // uint64s don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct uint64 with construct.construct"");
            break;

        case MR_TYPECTOR_REP_FLOAT:
            // floats don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct floats with construct.construct"");
            break;

        case MR_TYPECTOR_REP_CHAR:
            // chars don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct chars with construct.construct"");
            break;

        case MR_TYPECTOR_REP_STRING:
            // strings don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct strings with construct.construct"");
            break;

        case MR_TYPECTOR_REP_BITMAP:
            // bitmaps don't have functor ordinals.
            MR_fatal_error(
                ""cannot construct bitmaps with construct.construct"");
            break;

        case MR_TYPECTOR_REP_EQUIV:
        case MR_TYPECTOR_REP_EQUIV_GROUND:
            // These should be eliminated by MR_collapse_equivalences above.
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

        case MR_TYPECTOR_REP_UNUSED1:
        case MR_TYPECTOR_REP_UNUSED2:
        case MR_TYPECTOR_REP_UNKNOWN:
            MR_fatal_error(
                ""cannot construct values of unknown types ""
                ""with construct.construct"");
            break;

#ifdef MR_INCLUDE_SWITCH_DEFAULTS

        default:
            new_data = (MR_Word) 0;     // avoid a warning
            MR_fatal_error(""bad type_ctor_rep in construct.construct"");

#endif
        }   // end of main switch

        // Create a univ.
        MR_new_univ_on_hp(Term, type_info, new_data);
    }

    SUCCESS_INDICATOR = success;
}").

%---------------------%

construct_tuple(Args) = Univ :-
    construct_tuple_2(Args, list.map(univ_type, Args), list.length(Args),
        Univ).

:- pred construct_tuple_2(list(univ)::in, list(type_desc)::in, int::in,
    univ::out) is det.

:- pragma foreign_proc("C",
    construct_tuple_2(Args::in, ArgTypes::in, Arity::in, Univ::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
"{
    MR_TypeInfo type_info;
    MR_Word     new_data;
    int         i;
    MR_Word     arg_data;
    MR_TypeInfo arg_type_info;
    int         size;

    // Construct a type_info for the tuple.
    MR_save_transient_registers();
    type_info = MR_make_type(Arity, MR_TYPECTOR_DESC_MAKE_TUPLE(Arity),
        ArgTypes);
    MR_restore_transient_registers();

    // Create the tuple.
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

    // Create a univ.
    MR_new_univ_on_hp(Univ, type_info, new_data);
}").

construct_tuple_2(Args, ArgTypeDescs, Arity, Univ) :-
    list.map(type_desc_to_type_info, ArgTypeDescs, ArgTypeInfos),
    Univ = rtti_implementation.construct_tuple_2(Args, ArgTypeInfos, Arity).

%---------------------------------------------------------------------------%
:- end_module construct.
%---------------------------------------------------------------------------%
