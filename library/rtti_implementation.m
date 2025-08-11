%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rtti_implementation.m.
% Main author: trd, petdr, wangp.
% Stability: medium.
%
% This file is intended to provide portable RTTI functionality by implementing
% most of Mercury's RTTI functionality in Mercury.
%
% This is simpler than writing large amounts of low-level C code, and is much
% easier to maintain and port to new platforms.
%
% This module is not complete, the majority of the functionality is
% implemented, but the following still needs to be implemented
%   * functionality required to support construct.construct
%   * functionality required to support type_desc.type_ctor
%   * currently we recognise RTTI for higher order types, however
%     we don't interpret that RTTI fully.
%
% The plan is to migrate most of the Mercury level data structures in
% compiler/rtti.m here, and to interpret them, instead of relying on access
% to C level data structures.
%
% At least, that may have been the plan at some point. Currently this module
% is used for the Java and C# backends only.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rtti_implementation.
:- interface.

:- import_module deconstruct.
:- import_module list.
:- import_module univ.

%---------------------------------------------------------------------------%

:- pred generic_unify(T::in, T::in) is semidet.

:- pred generic_compare(comparison_result::out, T::in, T::in) is det.

%---------------------------------------------------------------------------%

    % Our type_info, pseudo_type_info and type_ctor_info implementations
    % are all abstract types.
:- type type_info.
:- type type_ctor_info.
:- type pseudo_type_info.

%---------------------------------------------------------------------------%
%
% Operations on type_infos.
%

:- func get_type_info(T::unused) = (type_info::out) is det.

:- pred compare_type_infos(comparison_result::out,
    type_info::in, type_info::in) is det.

:- func get_type_ctor_info(type_info) = type_ctor_info.

:- pred type_ctor_and_args(type_info::in, type_ctor_info::out,
    list(type_info)::out) is det.

:- pred type_info_num_functors(type_info::in, int::out) is semidet.

:- pred type_info_get_functor(type_info::in, int::in, string::out, int::out,
    list(pseudo_type_info)::out) is semidet.

:- pred type_info_get_functor_with_names(type_info::in, int::in, string::out,
    int::out, list(pseudo_type_info)::out, list(string)::out) is semidet.

:- pred type_info_get_functor_ordinal(type_info::in, int::in, int::out)
    is semidet.

:- pred type_info_get_functor_lex(type_info::in, int::in, int::out)
    is semidet.

%---------------------------------------------------------------------------%
%
% Operations on type_ctor_infos.
%

:- pred type_ctor_name_and_arity(type_ctor_info::in,
    string::out, string::out, int::out) is det.

%---------------------------------------------------------------------------%
%
% Operations on pseudo_type_infos.
%

:- pred pseudo_type_ctor_and_args(pseudo_type_info::in,
    type_ctor_info::out, list(pseudo_type_info)::out) is semidet.

:- pred is_univ_pseudo_type_info(pseudo_type_info::in, int::out) is semidet.
:- pred is_exist_pseudo_type_info(pseudo_type_info::in, int::out) is semidet.

%---------------------------------------------------------------------------%
%
% Constructing terms.
%

:- func construct(type_info, int, list(univ)) = univ is semidet.

:- func construct_tuple_2(list(univ), list(type_info), int) = univ.

%---------------------------------------------------------------------------%
%
% Deconstructing terms.
%

:- pred deconstruct(T, noncanon_handling, string, int, int, list(univ)).
:- mode deconstruct(in, in(do_not_allow), out, out, out, out) is det.
:- mode deconstruct(in, in(canonicalize), out, out, out, out) is det.
:- mode deconstruct(in, in(include_details_cc), out, out, out, out)
    is cc_multi.
:- mode deconstruct(in, in, out, out, out, out) is cc_multi.

:- pred functor_number_cc(T::in, int::out, int::out) is cc_nondet.

:- pred univ_named_arg(T, noncanon_handling, string, univ).
:- mode univ_named_arg(in, in(do_not_allow), in, out) is semidet.
:- mode univ_named_arg(in, in(canonicalize), in, out) is semidet.
:- mode univ_named_arg(in, in(include_details_cc), in, out)
    is semidet. % conceptually committed-choice

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bitmap.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module int16.
:- import_module int32.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term_io.
:- import_module type_desc.

%---------------------------------------------------------------------------%

    % It is convenient to represent the type_ctor_rep as a Mercury
    % enumeration, so we can switch on the values.
    %
    % The type_ctor_rep needs to be kept up to date with the real
    % definition in runtime/mercury_type_info.h.
    %
:- type type_ctor_rep
    --->    tcr_enum
    ;       tcr_enum_usereq
    ;       tcr_du
    ;       tcr_du_usereq
    ;       tcr_notag
    ;       tcr_notag_usereq
    ;       tcr_equiv
    ;       tcr_func
    ;       tcr_int
    ;       tcr_uint
    ;       tcr_char
    ;       tcr_float
    ;       tcr_string
    ;       tcr_pred
    ;       tcr_subgoal
    ;       tcr_void
    ;       tcr_c_pointer
    ;       tcr_typeinfo
    ;       tcr_typeclassinfo
    ;       tcr_array
    ;       tcr_succip
    ;       tcr_hp
    ;       tcr_curfr
    ;       tcr_maxfr
    ;       tcr_redofr
    ;       tcr_redoip
    ;       tcr_trail_ptr
    ;       tcr_ticket
    ;       tcr_notag_ground
    ;       tcr_notag_ground_usereq
    ;       tcr_equiv_ground
    ;       tcr_tuple
    ;       tcr_reserved_addr
    ;       tcr_reserved_addr_usereq
    ;       tcr_type_ctor_info
    ;       tcr_base_typeclass_info
    ;       tcr_type_desc
    ;       tcr_type_ctor_desc
    ;       tcr_foreign
    ;       tcr_reference
    ;       tcr_stable_c_pointer
    ;       tcr_stable_foreign
    ;       tcr_pseudo_type_desc
    ;       tcr_dummy
    ;       tcr_bitmap
    ;       tcr_foreign_enum
    ;       tcr_foreign_enum_usereq
    ;       tcr_int8
    ;       tcr_uint8
    ;       tcr_int16
    ;       tcr_uint16
    ;       tcr_int32
    ;       tcr_uint32
    ;       tcr_int64
    ;       tcr_uint64
    ;       tcr_unknown.

    % We keep all the other types abstract.

:- type type_ctor_info ---> type_ctor_info(c_pointer).
:- pragma foreign_type("C#", type_ctor_info,
    "runtime.TypeCtorInfo_Struct").
:- pragma foreign_type("Java", type_ctor_info,
    "jmercury.runtime.TypeCtorInfo_Struct").

:- type type_info ---> type_info(c_pointer).
:- pragma foreign_type("C#", type_info, "runtime.TypeInfo_Struct").
:- pragma foreign_type("Java", type_info, "jmercury.runtime.TypeInfo_Struct").

:- type type_layout ---> type_layout(c_pointer).
:- pragma foreign_type("C#", type_layout, "runtime.TypeLayout").
:- pragma foreign_type("Java", type_layout, "jmercury.runtime.TypeLayout").

:- type pseudo_type_info ---> pseudo_type_info(c_pointer).
:- pragma foreign_type("C#", pseudo_type_info,
    "runtime.PseudoTypeInfo").
:- pragma foreign_type("Java", pseudo_type_info,
    "jmercury.runtime.PseudoTypeInfo").

:- type typeclass_info ---> typeclass_info(c_pointer).
:- pragma foreign_type("C#", typeclass_info, "object[]").
:- pragma foreign_type("Java", typeclass_info, "java.lang.Object[]").

:- pragma foreign_decl("C#", local,
"
    using mercury.runtime;
").

:- pragma foreign_decl("Java", local,
"
    import java.lang.reflect.Constructor;
    import java.lang.reflect.Field;
    import java.lang.reflect.InvocationTargetException;

    import jmercury.runtime.DuFunctorDesc;
    import jmercury.runtime.EnumFunctorDesc;
    import jmercury.runtime.ForeignEnumFunctorDesc;
    import jmercury.runtime.PseudoTypeInfo;
    import jmercury.runtime.Ref;
    import jmercury.runtime.TypeCtorInfo_Struct;
    import jmercury.runtime.TypeInfo_Struct;
").

:- pragma foreign_code("Java",
"
    public static final Type_ctor_rep_0[] static_type_ctor_rep = new
        Type_ctor_rep_0[jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_MAX];
    static {
        for (int i = 0;
            i < jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_MAX;
            i++)
        {
            static_type_ctor_rep[i] = new Type_ctor_rep_0(i);
        }
    }
").

%---------------------------------------------------------------------------%

generic_unify(X, Y) :-
    TypeInfo = get_type_info(X),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    ( if
        TypeCtorRep = tcr_tuple
    then
        unify_tuple(TypeInfo, X, Y)
    else if
        ( TypeCtorRep = tcr_pred ; TypeCtorRep = tcr_func )
    then
        unexpected($pred, "unimplemented: higher order unification")
    else
        Arity = TypeCtorInfo ^ type_ctor_arity,
        UnifyPred = TypeCtorInfo ^ type_ctor_unify_pred,
        ( if Arity = 0 then
            semidet_call_3(UnifyPred, X, Y)
        else if Arity = 1 then
            semidet_call_4(UnifyPred,
                type_info_index_as_ti(TypeInfo, 1),
                X, Y)
        else if Arity = 2 then
            semidet_call_5(UnifyPred,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                X, Y)
        else if Arity = 3 then
            semidet_call_6(UnifyPred,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                X, Y)
        else if Arity = 4 then
            semidet_call_7(UnifyPred,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                type_info_index_as_ti(TypeInfo, 4),
                X, Y)
        else if Arity = 5 then
            semidet_call_8(UnifyPred,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                type_info_index_as_ti(TypeInfo, 4),
                type_info_index_as_ti(TypeInfo, 5),
                X, Y)
        else
            error("unify/2: type arity > 5 not supported")
        )
    ).

generic_compare(Res, X, Y) :-
    TypeInfo = get_type_info(X),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    ( if
        TypeCtorRep = tcr_tuple
    then
        compare_tuple(TypeInfo, Res, X, Y)
    else if
        ( TypeCtorRep = tcr_pred ; TypeCtorRep = tcr_func )
    then
        unexpected($pred, "unimplemented: higher order comparisons")
    else
        Arity = TypeCtorInfo ^ type_ctor_arity,
        ComparePred = TypeCtorInfo ^ type_ctor_compare_pred,
        ( if Arity = 0 then
            result_call_4(ComparePred, Res, X, Y)
        else if Arity = 1 then
            result_call_5(ComparePred, Res,
                type_info_index_as_ti(TypeInfo, 1), X, Y)
        else if Arity = 2 then
            result_call_6(ComparePred, Res,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                X, Y)
        else if Arity = 3 then
            result_call_7(ComparePred, Res,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                X, Y)
        else if Arity = 4 then
            result_call_8(ComparePred, Res,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                type_info_index_as_ti(TypeInfo, 4),
                X, Y)
        else if Arity = 5 then
            result_call_9(ComparePred, Res,
                type_info_index_as_ti(TypeInfo, 1),
                type_info_index_as_ti(TypeInfo, 2),
                type_info_index_as_ti(TypeInfo, 3),
                type_info_index_as_ti(TypeInfo, 4),
                type_info_index_as_ti(TypeInfo, 5),
                X, Y)
        else
            error("compare/3: type arity > 5 not supported")
        )
    ).

%---------------------%

:- pred unify_tuple(type_info::in, T::in, T::in) is semidet.

unify_tuple(TypeInfo, TermA, TermB) :-
    Arity = get_var_arity_typeinfo_arity(TypeInfo),
    unify_tuple_pos(1, Arity, TypeInfo, TermA, TermB).

:- pred unify_tuple_pos(int::in, int::in, type_info::in, T::in, T::in)
    is semidet.

unify_tuple_pos(Loc, TupleArity, TypeInfo, TermA, TermB) :-
    ( if Loc > TupleArity then
        true
    else
        ArgTypeInfo = var_arity_type_info_index_as_ti(TypeInfo, Loc),
        % XXX just count from 0
        get_tuple_subterm(TermA, Loc - 1, ArgTypeInfo, SubTermA),
        get_tuple_subterm(TermB, Loc - 1, ArgTypeInfo, SubTermB),

        private_builtin.unsafe_type_cast(SubTermB, CastSubTermB),
        generic_unify(SubTermA, CastSubTermB),

        unify_tuple_pos(Loc + 1, TupleArity, TypeInfo, TermA, TermB)
    ).

%---------------------%

:- pred compare_tuple(type_info::in, comparison_result::out, T::in, T::in)
    is det.

compare_tuple(TypeInfo, Result, TermA, TermB) :-
    Arity = get_var_arity_typeinfo_arity(TypeInfo),
    compare_tuple_pos(1, Arity, TypeInfo, Result, TermA, TermB).

:- pred compare_tuple_pos(int::in, int::in, type_info::in,
    comparison_result::out, T::in, T::in) is det.

compare_tuple_pos(Loc, TupleArity, TypeInfo, Result, TermA, TermB) :-
    ( if Loc > TupleArity then
        Result = (=)
    else
        ArgTypeInfo = var_arity_type_info_index_as_ti(TypeInfo, Loc),
        % XXX just count from 0
        get_tuple_subterm(TermA, Loc - 1, ArgTypeInfo, SubTermA),
        get_tuple_subterm(TermB, Loc - 1, ArgTypeInfo, SubTermB),

        private_builtin.unsafe_type_cast(SubTermB, CastSubTermB),
        generic_compare(SubResult, SubTermA, CastSubTermB),
        (
            SubResult = (=),
            compare_tuple_pos(Loc + 1, TupleArity, TypeInfo, Result,
                TermA, TermB)
        ;
            ( SubResult = (<)
            ; SubResult = (>)
            ),
            Result = SubResult
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_type_info(_T::unused) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("C#",
    get_type_info(_T::unused) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = TypeInfo_for_T;
").

:- pragma foreign_proc("C",
    get_type_info(_T::unused) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = TypeInfo_for_T;
").

get_type_info(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("get_type_info").

%---------------------------------------------------------------------------%

:- pragma foreign_export("C#", compare_type_infos(out, in, in),
    "ML_compare_type_infos").
:- pragma foreign_export("Java", compare_type_infos(out, in, in),
    "ML_compare_type_infos").

compare_type_infos(Res, TypeInfo1, TypeInfo2) :-
    ( if same_pointer_value(TypeInfo1, TypeInfo2) then
        Res = (=)
    else
        NewTypeInfo1 = collapse_equivalences(TypeInfo1),
        NewTypeInfo2 = collapse_equivalences(TypeInfo2),
        ( if same_pointer_value(NewTypeInfo1, NewTypeInfo2) then
            Res = (=)
        else
            compare_collapsed_type_infos(Res, NewTypeInfo1, NewTypeInfo2)
        )
    ).

:- pred compare_collapsed_type_infos(comparison_result::out,
    type_info::in, type_info::in) is det.

compare_collapsed_type_infos(Res, TypeInfo1, TypeInfo2) :-
    TypeCtorInfo1 = get_type_ctor_info(TypeInfo1),
    TypeCtorInfo2 = get_type_ctor_info(TypeInfo2),

    % cf. MR_compare_type_info
    compare(ModNameRes, TypeCtorInfo1 ^ type_ctor_module_name,
        TypeCtorInfo2 ^ type_ctor_module_name),
    (
        ModNameRes = (=),
        compare(NameRes, TypeCtorInfo1 ^ type_ctor_name,
            TypeCtorInfo2 ^ type_ctor_name),
        (
            NameRes = (=),
            ( if type_ctor_is_variable_arity(TypeCtorInfo1) then
                Arity1 = get_var_arity_typeinfo_arity(TypeInfo1),
                Arity2 = get_var_arity_typeinfo_arity(TypeInfo2),
                compare(ArityRes, Arity1, Arity2),
                (
                    ArityRes = (=),
                    compare_var_arity_type_info_args(1, Arity1, Res,
                        TypeInfo1, TypeInfo2)
                ;
                    ( ArityRes = (<)
                    ; ArityRes = (>)
                    ),
                    Res = ArityRes
                )
            else
                Arity1 = type_ctor_arity(TypeCtorInfo1),
                Arity2 = type_ctor_arity(TypeCtorInfo2),
                compare(ArityRes, Arity1, Arity2),
                (
                    ArityRes = (=),
                    compare_type_info_args(1, Arity1, Res,
                        TypeInfo1, TypeInfo2)
                ;
                    ( ArityRes = (<)
                    ; ArityRes = (>)
                    ),
                    Res = ArityRes
                )
            )
        ;
            ( NameRes = (<)
            ; NameRes = (>)
            ),
            Res = NameRes
        )
    ;
        ( ModNameRes = (<)
        ; ModNameRes = (>)
        ),
        Res = ModNameRes
    ).

:- pred compare_type_ctor_infos(comparison_result::out,
    type_ctor_info::in, type_ctor_info::in) is det.

:- pragma foreign_export("C#", compare_type_ctor_infos(out, in, in),
    "ML_compare_type_ctor_infos").
:- pragma foreign_export("Java", compare_type_ctor_infos(out, in, in),
    "ML_compare_type_ctor_infos").

compare_type_ctor_infos(Res, TypeCtorInfo1, TypeCtorInfo2) :-
    % cf. MR_compare_type_ctor_info
    compare(ModNameRes,
        TypeCtorInfo1 ^ type_ctor_module_name,
        TypeCtorInfo2 ^ type_ctor_module_name),
    (
        ModNameRes = (=),
        compare(NameRes,
            TypeCtorInfo1 ^ type_ctor_name,
            TypeCtorInfo2 ^ type_ctor_name),
        (
            NameRes = (=),
            Arity1 = type_ctor_arity(TypeCtorInfo1),
            Arity2 = type_ctor_arity(TypeCtorInfo2),
            compare(Res, Arity1, Arity2)
        ;
            ( NameRes = (<)
            ; NameRes = (>)
            ),
            Res = NameRes
        )
    ;
        ( ModNameRes = (<)
        ; ModNameRes = (>)
        ),
        Res = ModNameRes
    ).

:- pred compare_type_info_args(int::in, int::in, comparison_result::out,
    type_info::in, type_info::in) is det.

compare_type_info_args(Loc, Arity, Result, TypeInfoA, TypeInfoB) :-
    ( if Loc > Arity then
        Result = (=)
    else
        SubTypeInfoA = type_info_index_as_ti(TypeInfoA, Loc),
        SubTypeInfoB = type_info_index_as_ti(TypeInfoB, Loc),

        compare_collapsed_type_infos(SubResult, SubTypeInfoA, SubTypeInfoB),
        (
            SubResult = (=),
            compare_type_info_args(Loc + 1, Arity, Result,
                TypeInfoA, TypeInfoB)
        ;
            ( SubResult = (<)
            ; SubResult = (>)
            ),
            Result = SubResult
        )
    ).

:- pred compare_var_arity_type_info_args(int::in, int::in,
    comparison_result::out, type_info::in, type_info::in) is det.

compare_var_arity_type_info_args(Loc, Arity, Result, TypeInfoA, TypeInfoB) :-
    ( if Loc > Arity then
        Result = (=)
    else
        SubTypeInfoA = var_arity_type_info_index_as_ti(TypeInfoA, Loc),
        SubTypeInfoB = var_arity_type_info_index_as_ti(TypeInfoB, Loc),

        compare_collapsed_type_infos(SubResult, SubTypeInfoA, SubTypeInfoB),
        (
            SubResult = (=),
            compare_var_arity_type_info_args(Loc + 1, Arity, Result,
                TypeInfoA, TypeInfoB)
        ;
            ( SubResult = (<)
            ; SubResult = (>)
            ),
            Result = SubResult
        )
    ).

:- pred type_ctor_is_variable_arity(type_ctor_info::in) is semidet.

type_ctor_is_variable_arity(TypeCtorInfo) :-
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    ( TypeCtorRep = tcr_pred
    ; TypeCtorRep = tcr_func
    ; TypeCtorRep = tcr_tuple
    ).

:- pred compare_pseudo_type_infos(comparison_result::out,
    pseudo_type_info::in, pseudo_type_info::in) is det.
:- pragma consider_used(pred(compare_pseudo_type_infos/3)).

:- pragma foreign_export("C#", compare_pseudo_type_infos(out, in, in),
    "ML_compare_pseudo_type_infos").
:- pragma foreign_export("Java", compare_pseudo_type_infos(out, in, in),
    "ML_compare_pseudo_type_infos").

compare_pseudo_type_infos(Res, PTI1, PTI2) :-
    % Try to optimize a common case:
    % If type_info addresses are equal, they must represent the same type.
    ( if same_pointer_value(PTI1, PTI2) then
        Res = (=)
    else
        % Otherwise, we need to expand equivalence types, if any.
        NewPTI1 = collapse_equivalences_pseudo(PTI1),
        NewPTI2 = collapse_equivalences_pseudo(PTI2),

        % Perhaps they are equal now...
        ( if same_pointer_value(NewPTI1, NewPTI2) then
            Res = (=)
        else
            % Handle the comparison if either pseudo_type_info is a variable.
            % Any non-variable is greater than a variable.
            ( if
                pseudo_type_info_is_variable(NewPTI1, VarNum1),
                pseudo_type_info_is_variable(NewPTI2, VarNum2)
            then
                compare(Res, VarNum1, VarNum2)
            else if
                pseudo_type_info_is_variable(NewPTI1, _)
            then
                Res = (<)
            else if
                pseudo_type_info_is_variable(NewPTI2, _)
            then
                Res = (>)
            else if
                % Otherwise find the type_ctor_infos, and compare those.
                pseudo_type_ctor_and_args(NewPTI1, TypeCtorInfo1, Args1),
                pseudo_type_ctor_and_args(NewPTI2, TypeCtorInfo2, Args2)
            then
                compare_type_ctor_infos(ResTCI, TypeCtorInfo1, TypeCtorInfo2),
                (
                    ResTCI = (<),
                    Res = (<)
                ;
                    ResTCI = (>),
                    Res = (>)
                ;
                    ResTCI = (=),
                    list.length(Args1, NumArgs1),
                    list.length(Args2, NumArgs2),
                    compare(ResNumArgs, NumArgs1, NumArgs2),
                    (
                        ResNumArgs = (<),
                        Res = (<)
                    ;
                        ResNumArgs = (>),
                        Res = (>)
                    ;
                        ResNumArgs = (=),
                        compare_pseudo_type_info_args(Res, Args1, Args2)
                    )
                )
            else
                unexpected($pred, "impossible pseudo_type_infos")
            )
        )
    ).

:- pred compare_pseudo_type_info_args(comparison_result::out,
    list(pseudo_type_info)::in, list(pseudo_type_info)::in) is det.

compare_pseudo_type_info_args(Res, Args1, Args2) :-
    (
        Args1 = [],
        Args2 = [],
        Res = (=)
    ;
        Args1 = [H1 | T1],
        Args2 = [H2 | T2],
        compare_pseudo_type_infos(ResPTI, H1, H2),
        (
            ResPTI = (<),
            Res = (<)
        ;
            ResPTI = (>),
            Res = (>)
        ;
            ResPTI = (=),
            compare_pseudo_type_info_args(Res, T1, T2)
        )
    ;
        Args1 = [_ | _],
        Args2 = [],
        unexpected($pred, "argument list mismatch")
    ;
        Args1 = [],
        Args2 = [_ | _],
        unexpected($pred, "argument list mismatch")
    ).

%---------------------------------------------------------------------------%

:- func collapse_equivalences(type_info) = type_info.
:- pragma foreign_export("C#", collapse_equivalences(in) = out,
    "ML_collapse_equivalences").
:- pragma foreign_export("Java", collapse_equivalences(in) = out,
    "ML_collapse_equivalences").

collapse_equivalences(TypeInfo) = NewTypeInfo :-
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    ( if
        % Look past equivalences.
        ( TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_equiv
        )
    then
        TypeLayout = get_type_layout(TypeCtorInfo),
        EquivTypeInfo = get_layout_equiv(TypeLayout),
        NewTypeInfo = collapse_equivalences(EquivTypeInfo)
    else
        NewTypeInfo = TypeInfo
    ).

:- func collapse_equivalences_pseudo(pseudo_type_info) = pseudo_type_info.

collapse_equivalences_pseudo(PTI) = NewPTI :-
    ( if
        pseudo_type_ctor_and_args(PTI, TypeCtorInfo, _Args),
        TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
        % Look past equivalences.
        ( TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_equiv
        )
    then
        TypeLayout = get_type_layout(TypeCtorInfo),
        EquivTypeInfo = get_layout_equiv(TypeLayout),
        NewPTI0 = create_pseudo_type_info(EquivTypeInfo, PTI),
        NewPTI = collapse_equivalences_pseudo(NewPTI0)
    else
        NewPTI = PTI
    ).

:- func get_layout_equiv(type_layout) = type_info.
:- pragma no_determinism_warning(func(get_layout_equiv/1)).

:- pragma foreign_proc("C#",
    get_layout_equiv(TypeLayout::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.PseudoTypeInfo pti = TypeLayout.layout_equiv();
    TypeInfo = runtime.TypeInfo_Struct.maybe_new(pti);
").

:- pragma foreign_proc("Java",
    get_layout_equiv(TypeLayout::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.PseudoTypeInfo pti = TypeLayout.layout_equiv();
    TypeInfo = jmercury.runtime.TypeInfo_Struct.maybe_new(pti);
").

get_layout_equiv(_) = _ :-
    private_builtin.sorry("get_layout_equiv").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorInfo = TypeInfo.type_ctor;
").

:- pragma foreign_proc("Java",
    get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorInfo = TypeInfo.type_ctor;
").

:- pragma foreign_proc("C",
    get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorInfo = (MR_Word) MR_TYPEINFO_GET_TYPE_CTOR_INFO(
        (MR_TypeInfo) TypeInfo);
").

get_type_ctor_info(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("get_type_ctor_info").

%---------------------------------------------------------------------------%

type_ctor_and_args(TypeInfo0, TypeCtorInfo, TypeArgs) :-
    TypeInfo = collapse_equivalences(TypeInfo0),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    ( if
        type_ctor_is_variable_arity(TypeCtorInfo)
    then
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        TypeArgs = iterate(1, Arity, var_arity_type_info_index_as_ti(TypeInfo))
    else
        Arity = type_ctor_arity(TypeCtorInfo),
        TypeArgs = iterate(1, Arity, type_info_index_as_ti(TypeInfo))
    ).

%---------------------------------------------------------------------------%

type_info_num_functors(TypeInfo, NumFunctors) :-
    % See MR_get_num_functors in runtime/mercury_construct.c

    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    require_complete_switch [TypeCtorRep]
    (
        ( TypeCtorRep = tcr_du
        ; TypeCtorRep = tcr_du_usereq
        ; TypeCtorRep = tcr_reserved_addr
        ; TypeCtorRep = tcr_reserved_addr_usereq
        ; TypeCtorRep = tcr_enum
        ; TypeCtorRep = tcr_enum_usereq
        ; TypeCtorRep = tcr_foreign_enum
        ; TypeCtorRep = tcr_foreign_enum_usereq
        ),
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        ( TypeCtorRep = tcr_dummy
        ; TypeCtorRep = tcr_notag
        ; TypeCtorRep = tcr_notag_usereq
        ; TypeCtorRep = tcr_notag_ground
        ; TypeCtorRep = tcr_notag_ground_usereq
        ; TypeCtorRep = tcr_tuple
        ),
        NumFunctors = 1
    ;
        ( TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_equiv
        ),
        NewTypeInfo = collapse_equivalences(TypeInfo),
        type_info_num_functors(NewTypeInfo, NumFunctors)
    ;
        ( TypeCtorRep = tcr_subgoal
        ; TypeCtorRep = tcr_int
        ; TypeCtorRep = tcr_uint
        ; TypeCtorRep = tcr_int8
        ; TypeCtorRep = tcr_uint8
        ; TypeCtorRep = tcr_int16
        ; TypeCtorRep = tcr_uint16
        ; TypeCtorRep = tcr_int32
        ; TypeCtorRep = tcr_uint32
        ; TypeCtorRep = tcr_int64
        ; TypeCtorRep = tcr_uint64
        ; TypeCtorRep = tcr_char
        ; TypeCtorRep = tcr_float
        ; TypeCtorRep = tcr_string
        ; TypeCtorRep = tcr_bitmap
        ; TypeCtorRep = tcr_func
        ; TypeCtorRep = tcr_pred
        ; TypeCtorRep = tcr_void
        ; TypeCtorRep = tcr_c_pointer
        ; TypeCtorRep = tcr_stable_c_pointer
        ; TypeCtorRep = tcr_typeinfo
        ; TypeCtorRep = tcr_type_ctor_info
        ; TypeCtorRep = tcr_type_desc
        ; TypeCtorRep = tcr_pseudo_type_desc
        ; TypeCtorRep = tcr_type_ctor_desc
        ; TypeCtorRep = tcr_typeclassinfo
        ; TypeCtorRep = tcr_base_typeclass_info
        ; TypeCtorRep = tcr_array
        ; TypeCtorRep = tcr_succip
        ; TypeCtorRep = tcr_hp
        ; TypeCtorRep = tcr_curfr
        ; TypeCtorRep = tcr_maxfr
        ; TypeCtorRep = tcr_redofr
        ; TypeCtorRep = tcr_redoip
        ; TypeCtorRep = tcr_trail_ptr
        ; TypeCtorRep = tcr_ticket
        ; TypeCtorRep = tcr_foreign
        ; TypeCtorRep = tcr_stable_foreign
        ; TypeCtorRep = tcr_reference
        ),
        fail
    ;
        TypeCtorRep = tcr_unknown,
        unexpected($pred, "unknown type_ctor_rep")
    ).

%---------------------------------------------------------------------------%

type_info_get_functor(TypeInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList) :-
    get_functor_impl(TypeInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList, _Names).

type_info_get_functor_with_names(TypeInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList, Names) :-
    get_functor_impl(TypeInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList, Names).

:- pred get_functor_impl(type_info::in, int::in, string::out, int::out,
    list(pseudo_type_info)::out, list(string)::out) is semidet.

get_functor_impl(TypeInfo, FunctorNumber,
        FunctorName, Arity, PseudoTypeInfoList, Names) :-
    type_info_num_functors(TypeInfo, NumFunctors),
    FunctorNumber >= 0,
    FunctorNumber < NumFunctors,
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    require_complete_switch [TypeCtorRep]
    (
        ( TypeCtorRep = tcr_du
        ; TypeCtorRep = tcr_du_usereq
        ; TypeCtorRep = tcr_reserved_addr
        ; TypeCtorRep = tcr_reserved_addr_usereq
        ),
        get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names)
    ;
        ( TypeCtorRep = tcr_enum
        ; TypeCtorRep = tcr_enum_usereq
        ; TypeCtorRep = tcr_dummy
        ),
        get_functor_enum(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names)
    ;
        ( TypeCtorRep = tcr_foreign_enum
        ; TypeCtorRep = tcr_foreign_enum_usereq
        ),
        get_functor_foreign_enum(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names)
    ;
        ( TypeCtorRep = tcr_notag
        ; TypeCtorRep = tcr_notag_usereq
        ; TypeCtorRep = tcr_notag_ground
        ; TypeCtorRep = tcr_notag_ground_usereq
        ),
        get_functor_notag(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, PseudoTypeInfoList, Names)
    ;
        ( TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_equiv
        ),
        NewTypeInfo = collapse_equivalences(TypeInfo),
        get_functor_impl(NewTypeInfo, FunctorNumber,
            FunctorName, Arity, PseudoTypeInfoList, Names)
    ;
        TypeCtorRep = tcr_tuple,
        FunctorName = "{}",
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        PseudoTypeInfoList = iterate(1, Arity,
            var_arity_type_info_index_as_pti(TypeInfo)),
        Names = list.duplicate(Arity, null_string)
    ;
        ( TypeCtorRep = tcr_subgoal
        ; TypeCtorRep = tcr_int
        ; TypeCtorRep = tcr_uint
        ; TypeCtorRep = tcr_int8
        ; TypeCtorRep = tcr_uint8
        ; TypeCtorRep = tcr_int16
        ; TypeCtorRep = tcr_uint16
        ; TypeCtorRep = tcr_int32
        ; TypeCtorRep = tcr_uint32
        ; TypeCtorRep = tcr_int64
        ; TypeCtorRep = tcr_uint64
        ; TypeCtorRep = tcr_char
        ; TypeCtorRep = tcr_float
        ; TypeCtorRep = tcr_string
        ; TypeCtorRep = tcr_bitmap
        ; TypeCtorRep = tcr_func
        ; TypeCtorRep = tcr_pred
        ; TypeCtorRep = tcr_void
        ; TypeCtorRep = tcr_c_pointer
        ; TypeCtorRep = tcr_stable_c_pointer
        ; TypeCtorRep = tcr_typeinfo
        ; TypeCtorRep = tcr_type_ctor_info
        ; TypeCtorRep = tcr_type_desc
        ; TypeCtorRep = tcr_pseudo_type_desc
        ; TypeCtorRep = tcr_type_ctor_desc
        ; TypeCtorRep = tcr_typeclassinfo
        ; TypeCtorRep = tcr_base_typeclass_info
        ; TypeCtorRep = tcr_array
        ; TypeCtorRep = tcr_succip
        ; TypeCtorRep = tcr_hp
        ; TypeCtorRep = tcr_curfr
        ; TypeCtorRep = tcr_maxfr
        ; TypeCtorRep = tcr_redofr
        ; TypeCtorRep = tcr_redoip
        ; TypeCtorRep = tcr_trail_ptr
        ; TypeCtorRep = tcr_ticket
        ; TypeCtorRep = tcr_foreign
        ; TypeCtorRep = tcr_stable_foreign
        ; TypeCtorRep = tcr_reference
        ),
        fail
    ;
        TypeCtorRep = tcr_unknown,
        unexpected($pred, "unknown type_ctor_rep")
    ).

%---------------------%

:- pred get_functor_du(type_ctor_rep::in(du), type_info::in,
    type_ctor_info::in, int::in, string::out, int::out,
    list(pseudo_type_info)::out, list(string)::out) is det.

get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo, FunctorNumber,
        FunctorName, Arity, PseudoTypeInfoList, Names) :-
    TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
    DuFunctorDesc = TypeFunctors ^ du_functor_desc(TypeCtorRep, FunctorNumber),

    FunctorName = DuFunctorDesc ^ du_functor_name,
    Arity = int16.to_int(DuFunctorDesc ^ du_functor_arity),

    ArgTypes = DuFunctorDesc ^ du_functor_arg_types,
    F =
        ( func(I) = ArgPseudoTypeInfo :-
            PseudoTypeInfo = get_pti_from_arg_types(ArgTypes, I),
            ArgPseudoTypeInfo =
                create_pseudo_type_info(TypeInfo, PseudoTypeInfo)
        ),
    PseudoTypeInfoList = iterate(0, Arity - 1, F),

    ( if get_du_functor_arg_names(DuFunctorDesc, ArgNames) then
        Names = iterate(0, Arity - 1, arg_names_index(ArgNames))
    else
        Names = list.duplicate(Arity, null_string)
    ).

%---------------------%

:- pred get_functor_enum(type_ctor_rep::in(enum), type_ctor_info::in, int::in,
    string::out, int::out, list(pseudo_type_info)::out, list(string)::out)
    is det.

get_functor_enum(TypeCtorRep, TypeCtorInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList, Names) :-
    TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
    EnumFunctorDesc = get_enum_functor_desc(TypeCtorRep, FunctorNumber,
        TypeFunctors),

    FunctorName = EnumFunctorDesc ^ enum_functor_name,
    Arity = 0,
    PseudoTypeInfoList = [],
    Names = [].

%---------------------%

:- pred get_functor_foreign_enum(type_ctor_rep::in(foreign_enum),
    type_ctor_info::in, int::in, string::out, int::out,
    list(pseudo_type_info)::out, list(string)::out) is det.

get_functor_foreign_enum(TypeCtorRep, TypeCtorInfo, FunctorNumber,
        FunctorName, Arity, PseudoTypeInfoList, Names) :-
    TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
    ForeignEnumFunctorDesc = get_foreign_enum_functor_desc(TypeCtorRep,
        FunctorNumber, TypeFunctors),

    FunctorName = ForeignEnumFunctorDesc ^ foreign_enum_functor_name,
    Arity = 0,
    PseudoTypeInfoList = [],
    Names = [].

%---------------------%

:- pred get_functor_notag(type_ctor_rep::in(notag), type_ctor_info::in,
    int::in, string::out, int::out, list(pseudo_type_info)::out,
    list(string)::out) is det.

get_functor_notag(TypeCtorRep, TypeCtorInfo, FunctorNumber, FunctorName, Arity,
        PseudoTypeInfoList, Names) :-
    TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
    NoTagFunctorDesc = TypeFunctors ^
        notag_functor_desc(TypeCtorRep, FunctorNumber),

    FunctorName = NoTagFunctorDesc ^ notag_functor_name,
    Arity = 1,

    ArgType = NoTagFunctorDesc ^ notag_functor_arg_type,
    ArgName = NoTagFunctorDesc ^ notag_functor_arg_name,

    PseudoTypeInfoList = [ArgType],
    Names = [ArgName].

%---------------------%

:- func get_var_arity_typeinfo_arity(type_info) = int.
:- pragma no_determinism_warning(func(get_var_arity_typeinfo_arity/1)).

:- pragma foreign_proc("Java",
    get_var_arity_typeinfo_arity(TypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = TypeInfo.args.length;
").

:- pragma foreign_proc("C#",
    get_var_arity_typeinfo_arity(TypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = TypeInfo.args.Length;
").

get_var_arity_typeinfo_arity(_) = _ :-
    private_builtin.sorry("get_var_arity_typeinfo_arity").

%---------------------------------------------------------------------------%

    % Unlike get_arg_type_info, existentially quantified type variables are
    % simply returned with no attempt to extract the type infos from terms.
    % cf. MR_create_pseudo_type_info
    %
:- func create_pseudo_type_info(type_info, pseudo_type_info)
    = pseudo_type_info.

create_pseudo_type_info(TypeInfo, PseudoTypeInfo) = ArgPseudoTypeInfo :-
    ( if is_exist_pseudo_type_info(PseudoTypeInfo, _VarNum) then
        ArgPseudoTypeInfo = PseudoTypeInfo
    else if is_univ_pseudo_type_info(PseudoTypeInfo, VarNum) then
        % In some cases we may need to call var_arity_type_info_index_as_pti.
        ArgPseudoTypeInfo = type_info_index_as_pti(TypeInfo, VarNum)
    else if pseudo_type_ctor_and_args(PseudoTypeInfo, TypeCtorInfo, Args0) then
        Args = list.map(create_pseudo_type_info(TypeInfo), Args0),
        NewTypeInfo = make_type_info(TypeCtorInfo, list.length(Args), Args),
        private_builtin.unsafe_type_cast(NewTypeInfo, ArgPseudoTypeInfo)
    else
        unexpected($pred, "create_pseudo_type_info")
    ).

:- func make_type_info(type_ctor_info, int, list(pseudo_type_info)) =
    type_info.
:- pragma no_determinism_warning(func(make_type_info/3)).

:- pragma foreign_proc("C#",
    make_type_info(TypeCtorInfo::in, Arity::in, Args::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    PseudoTypeInfo[] args = new PseudoTypeInfo[Arity];
    int i = 0;
    list.List_1 lst = Args;
    while (!list.is_empty(lst)) {
        args[i] = (PseudoTypeInfo) list.det_head(lst);
        lst = list.det_tail(lst);
        i++;
    }

    TypeInfo = new TypeInfo_Struct();
    TypeInfo.init(TypeCtorInfo, Arity, args);
").

:- pragma foreign_proc("Java",
    make_type_info(TypeCtorInfo::in, Arity::in, Args::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    PseudoTypeInfo[] as = new PseudoTypeInfo[Arity];
    int i = 0;
    list.List_1<PseudoTypeInfo> lst = Args;
    while (!list.is_empty(lst)) {
        as[i] = list.det_head(lst);
        lst = list.det_tail(lst);
        i++;
    }

    TypeInfo = new TypeInfo_Struct();
    TypeInfo.init(TypeCtorInfo, Arity, as);
").

make_type_info(_, _, _) = _ :-
   private_builtin.sorry("make_type_info/3").

%---------------------------------------------------------------------------%

type_info_get_functor_ordinal(TypeInfo, FunctorNum, Ordinal) :-
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    require_complete_switch [TypeCtorRep]
    (
        ( TypeCtorRep = tcr_enum
        ; TypeCtorRep = tcr_enum_usereq
        ),
        TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
        EnumFunctorDesc = get_enum_functor_desc(TypeCtorRep, FunctorNum,
            TypeFunctors),
        EnumValue = enum_functor_value(EnumFunctorDesc),
        index_or_search_enum_functor_ordinal(TypeCtorInfo, EnumValue, Ordinal)
    ;
        ( TypeCtorRep = tcr_foreign_enum
        ; TypeCtorRep = tcr_foreign_enum_usereq
        ),
        TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
        ForeignEnumFunctorDesc = get_foreign_enum_functor_desc(TypeCtorRep,
            FunctorNum, TypeFunctors),
        Ordinal = foreign_enum_functor_ordinal(ForeignEnumFunctorDesc)
    ;
        ( TypeCtorRep = tcr_dummy
        ; TypeCtorRep = tcr_notag
        ; TypeCtorRep = tcr_notag_usereq
        ; TypeCtorRep = tcr_notag_ground
        ; TypeCtorRep = tcr_notag_ground_usereq
        ; TypeCtorRep = tcr_tuple
        ),
        FunctorNum = 0,
        Ordinal = 0
    ;
        ( TypeCtorRep = tcr_du
        ; TypeCtorRep = tcr_du_usereq
        ; TypeCtorRep = tcr_reserved_addr
        ; TypeCtorRep = tcr_reserved_addr_usereq
        ),
        TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
        DuFunctorDesc = TypeFunctors ^ du_functor_desc(TypeCtorRep,
            FunctorNum),
        Ordinal = int32.to_int(du_functor_ordinal(DuFunctorDesc))
    ;
        ( TypeCtorRep = tcr_equiv
        ; TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_func
        ; TypeCtorRep = tcr_pred
        ; TypeCtorRep = tcr_int
        ; TypeCtorRep = tcr_uint
        ; TypeCtorRep = tcr_int8
        ; TypeCtorRep = tcr_uint8
        ; TypeCtorRep = tcr_int16
        ; TypeCtorRep = tcr_uint16
        ; TypeCtorRep = tcr_int32
        ; TypeCtorRep = tcr_uint32
        ; TypeCtorRep = tcr_int64
        ; TypeCtorRep = tcr_uint64
        ; TypeCtorRep = tcr_float
        ; TypeCtorRep = tcr_char
        ; TypeCtorRep = tcr_string
        ; TypeCtorRep = tcr_bitmap
        ; TypeCtorRep = tcr_subgoal
        ; TypeCtorRep = tcr_void
        ; TypeCtorRep = tcr_c_pointer
        ; TypeCtorRep = tcr_stable_c_pointer
        ; TypeCtorRep = tcr_typeinfo
        ; TypeCtorRep = tcr_type_ctor_info
        ; TypeCtorRep = tcr_typeclassinfo
        ; TypeCtorRep = tcr_base_typeclass_info
        ; TypeCtorRep = tcr_type_desc
        ; TypeCtorRep = tcr_type_ctor_desc
        ; TypeCtorRep = tcr_pseudo_type_desc
        ; TypeCtorRep = tcr_array
        ; TypeCtorRep = tcr_reference
        ; TypeCtorRep = tcr_succip
        ; TypeCtorRep = tcr_hp
        ; TypeCtorRep = tcr_curfr
        ; TypeCtorRep = tcr_maxfr
        ; TypeCtorRep = tcr_redofr
        ; TypeCtorRep = tcr_redoip
        ; TypeCtorRep = tcr_trail_ptr
        ; TypeCtorRep = tcr_ticket
        ; TypeCtorRep = tcr_foreign
        ; TypeCtorRep = tcr_stable_foreign
        ; TypeCtorRep = tcr_unknown
        ),
        fail
    ).

%---------------------------------------------------------------------------%

type_info_get_functor_lex(TypeInfo0, Ordinal, FunctorNumber) :-
    TypeInfo = collapse_equivalences(TypeInfo0),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    type_ctor_search_functor_number_map(TypeCtorInfo, Ordinal, FunctorNumber).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Implement generic calls -- we could use call/N but then we would
    % have to create a real closure.
    %
    % We first give "unimplemented" definitions in Mercury, which will be
    % used by default.
    %
    % NOTE: semidet_call_* and result_call_* are declared as no-typeinfo
    % builtins.

:- type unify_or_compare_pred
    --->    unify_or_compare_pred.

:- pragma foreign_type("C#", unify_or_compare_pred, "object").
:- pragma foreign_type("Java", unify_or_compare_pred,
    "jmercury.runtime.MethodPtr").

% We override the Mercury definitions in the Java and .NET backends.

%---------------------%

:- pred semidet_call_3(P::in, T::in, U::in) is semidet.

:- pragma foreign_proc("C#",
    semidet_call_3(Pred::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr2_r1<object, object, bool> pred
        = (runtime.MethodPtr2_r1<object, object, bool>) Pred;
    SUCCESS_INDICATOR = pred(X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_3(Pred::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr2 P = (jmercury.runtime.MethodPtr2) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(X, Y);
").

semidet_call_3(_, _, _) :-
    semidet_unimplemented("semidet_call_3").

%---------------------%

:- pred semidet_call_4(P::in, A::in, T::in, U::in) is semidet.

:- pragma foreign_proc("C#",
    semidet_call_4(Pred::in, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr3_r1<object, object, object, bool> pred
        = (runtime.MethodPtr3_r1<object, object, object, bool>) Pred;
    SUCCESS_INDICATOR = pred(A, X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_4(Pred::in, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr3 P = (jmercury.runtime.MethodPtr3) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(A, X, Y);
").

semidet_call_4(_, _, _, _) :-
    semidet_unimplemented("semidet_call_4").

%---------------------%

:- pred semidet_call_5(P::in, A::in, B::in, T::in, U::in) is semidet.

:- pragma foreign_proc("C#",
    semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr4_r1<object, object, object, object, bool> pred
        = (runtime.MethodPtr4_r1<object, object, object, object, bool>) Pred;
    SUCCESS_INDICATOR = pred(A, B, X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr4 P = (jmercury.runtime.MethodPtr4) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(A, B, X, Y);
").

semidet_call_5(_, _, _, _, _) :-
    semidet_unimplemented("semidet_call_5").

%---------------------%

:- pred semidet_call_6(P::in, A::in, B::in, C::in, T::in, U::in) is semidet.

:- pragma foreign_proc("C#",
    semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr5_r1<object, object, object, object, object, bool> pred
        = (runtime.MethodPtr5_r1<object, object, object, object, object, bool>)
            Pred;
    SUCCESS_INDICATOR = pred(A, B, C, X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr5 P = (jmercury.runtime.MethodPtr5) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(A, B, C, X, Y);
").

semidet_call_6(_, _, _, _, _, _) :-
    semidet_unimplemented("semidet_call_6").

%---------------------%

:- pred semidet_call_7(P::in, A::in, B::in, C::in, D::in, T::in, U::in)
    is semidet.

:- pragma foreign_proc("C#",
    semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr6_r1<object, object, object, object, object, object, bool>
        pred
        = (runtime.MethodPtr6_r1<object, object, object, object, object,
            object, bool>) Pred;
    SUCCESS_INDICATOR = pred(A, B, C, D, X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr6 P = (jmercury.runtime.MethodPtr6) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(A, B, C, D, X, Y);
").

semidet_call_7(_, _, _, _, _, _, _) :-
    semidet_unimplemented("semidet_call_7").

%---------------------%

:- pred semidet_call_8(P::in, A::in, B::in, C::in, D::in, E::in, T::in, U::in)
    is semidet.

:- pragma foreign_proc("C#",
    semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr7_r1<object, object, object, object, object, object,
        object, bool> pred =
        (runtime.MethodPtr7_r1<object, object, object, object, object, object,
            object, bool>) Pred;
    SUCCESS_INDICATOR = pred(A, B, C, D, E, X, Y);
").
:- pragma foreign_proc("Java",
    semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr7 P = (jmercury.runtime.MethodPtr7) Pred;
    SUCCESS_INDICATOR = (Boolean) P.call___0_0(A, B, C, D, E, X, Y);
").

semidet_call_8(_, _, _, _, _, _, _, _) :-
    semidet_unimplemented("semidet_call_8").

%---------------------%

:- pred result_call_4(P::in, comparison_result::out,
    T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_4(Pred::in, Res::out, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX C# should the return type be object of Comparison_result_0?
    // Indeed; this is causing unnecessary boxing.
    runtime.MethodPtr2_r1<object, object, object> pred
        = (runtime.MethodPtr2_r1<object, object, object>) Pred;
    Res = (builtin.Comparison_result_0) pred(X, Y);
").
:- pragma foreign_proc("Java",
    result_call_4(Pred::in, Res::out, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr2 P = (jmercury.runtime.MethodPtr2) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(X, Y);
").

result_call_4(_, (=), _, _) :-
    det_unimplemented("result_call_4").

%---------------------%

:- pred result_call_5(P::in, comparison_result::out,
    A::in, T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_5(Pred::in, Res::out, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr3_r1<object, object, object, object> pred
        = (runtime.MethodPtr3_r1<object, object, object, object>) Pred;
    Res = (builtin.Comparison_result_0) pred(A, X, Y);
").
:- pragma foreign_proc("Java",
    result_call_5(Pred::in, Res::out, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr3 P = (jmercury.runtime.MethodPtr3) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(A, X, Y);
").

result_call_5(_, (=), _, _, _) :-
    det_unimplemented("comparison_result").

%---------------------%

:- pred result_call_6(P::in, comparison_result::out,
    A::in, B::in, T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr4_r1<object, object, object, object, object> pred
        = (runtime.MethodPtr4_r1<object, object, object, object, object>) Pred;
    Res = (builtin.Comparison_result_0) pred(A, B, X, Y);
").
:- pragma foreign_proc("Java",
    result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr4 P = (jmercury.runtime.MethodPtr4) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(A, B, X, Y);
").

result_call_6(_, (=), _, _, _, _) :-
    det_unimplemented("comparison_result").

%---------------------%

:- pred result_call_7(P::in, comparison_result::out,
    A::in, B::in, C::in, T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr5_r1<object, object, object, object, object, object> pred
        = (runtime.MethodPtr5_r1<object, object, object, object, object,
            object>) Pred;
    Res = (builtin.Comparison_result_0) pred(A, B, C, X, Y);
").
:- pragma foreign_proc("Java",
    result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr5 P = (jmercury.runtime.MethodPtr5) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(A, B, C, X, Y);
").

result_call_7(_, (=), _, _, _, _, _) :-
    det_unimplemented("comparison_result").

%---------------------%

:- pred result_call_8(P::in, comparison_result::out,
    A::in, B::in, C::in, D::in, T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in,
        Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr6_r1<object, object, object, object, object, object,
            object> pred
        = (runtime.MethodPtr6_r1<object, object, object, object, object,
            object, object>) Pred;
    Res = (builtin.Comparison_result_0) pred(A, B, C, D, X, Y);
").
:- pragma foreign_proc("Java",
    result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in,
        Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr6 P = (jmercury.runtime.MethodPtr6) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(A, B, C, D, X, Y);
").

result_call_8(_, (=), _, _, _, _, _, _) :-
    det_unimplemented("comparison_result").

%---------------------%

:- pred result_call_9(P::in, comparison_result::out,
    A::in, B::in, C::in, D::in, E::in, T::in, U::in) is det.

:- pragma foreign_proc("C#",
    result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.MethodPtr7_r1<object, object, object, object, object, object,
            object, object> pred
        = (runtime.MethodPtr7_r1<object, object, object, object, object,
            object, object, object>) Pred;
    Res = (builtin.Comparison_result_0) pred(A, B, C, D, E, X, Y);
").
:- pragma foreign_proc("Java",
    result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.MethodPtr7 P = (jmercury.runtime.MethodPtr7) Pred;
    Res = (builtin.Comparison_result_0) P.call___0_0(A, B, C, D, E, X, Y);
").

result_call_9(_, (=), _, _, _, _, _, _, _) :-
    det_unimplemented("result_call_9").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

type_ctor_name_and_arity(TypeCtorInfo, ModuleName, Name, Arity) :-
    ModuleName = type_ctor_module_name(TypeCtorInfo),
    Name = type_ctor_name(TypeCtorInfo),
    Arity = type_ctor_arity(TypeCtorInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(pred(pseudo_type_ctor_and_args/3)).

:- pragma foreign_proc("C#",
    pseudo_type_ctor_and_args(PseudoTypeInfo::in, TypeCtorInfo::out,
        ArgPseudoTypeInfos::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    if (PseudoTypeInfo.variable_number == -1) {
        if (PseudoTypeInfo is TypeCtorInfo_Struct) {
            TypeCtorInfo = (TypeCtorInfo_Struct) PseudoTypeInfo;
            ArgPseudoTypeInfos = list.empty_list();
        } else {
            TypeInfo_Struct ti = (TypeInfo_Struct) PseudoTypeInfo;
            TypeCtorInfo = ti.type_ctor;

            list.List_1 lst = list.empty_list();
            if (ti.args != null) {
                for (int i = ti.args.Length - 1; i >= 0; i--) {
                    lst = list.cons(ti.args[i], lst);
                }
            }
            ArgPseudoTypeInfos = lst;
        }
        SUCCESS_INDICATOR = true;
    } else {
        // Fail if input is a variable.
        TypeCtorInfo = null;
        ArgPseudoTypeInfos = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    pseudo_type_ctor_and_args(PseudoTypeInfo::in, TypeCtorInfo::out,
        ArgPseudoTypeInfos::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    if (PseudoTypeInfo.variable_number == -1) {
        if (PseudoTypeInfo instanceof TypeCtorInfo_Struct) {
            TypeCtorInfo = (TypeCtorInfo_Struct) PseudoTypeInfo;
            ArgPseudoTypeInfos = list.empty_list();
        } else {
            TypeInfo_Struct ti = (TypeInfo_Struct) PseudoTypeInfo;
            TypeCtorInfo = ti.type_ctor;

            list.List_1<PseudoTypeInfo> lst = list.empty_list();
            if (ti.args != null) {
                for (int i = ti.args.length - 1; i >= 0; i--) {
                    lst = list.cons(ti.args[i], lst);
                }
            }
            ArgPseudoTypeInfos = lst;
        }
        SUCCESS_INDICATOR = true;
    } else {
        // Fail if input is a variable.
        TypeCtorInfo = null;
        ArgPseudoTypeInfos = null;
        SUCCESS_INDICATOR = false;
    }
").

pseudo_type_ctor_and_args(_, _, _) :-
    private_builtin.sorry("pseudo_type_ctor_and_args/3").

%---------------------%

:- pragma no_determinism_warning(pred(is_univ_pseudo_type_info/2)).

:- pragma foreign_proc("C#",
    is_univ_pseudo_type_info(PseudoTypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = PseudoTypeInfo.variable_number;
    SUCCESS_INDICATOR =
        (VarNum >= 0 && VarNum <= rtti_implementation.last_univ_quant_varnum);
").

:- pragma foreign_proc("Java",
    is_univ_pseudo_type_info(PseudoTypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = PseudoTypeInfo.variable_number;
    SUCCESS_INDICATOR =
        (VarNum >= 0 && VarNum <= rtti_implementation.last_univ_quant_varnum);
").

is_univ_pseudo_type_info(_, _) :-
    private_builtin.sorry("is_univ_pseudo_type_info/2").

%---------------------%

:- pragma no_determinism_warning(pred(is_exist_pseudo_type_info/2)).

:- pragma foreign_proc("C#",
    is_exist_pseudo_type_info(PseudoTypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = PseudoTypeInfo.variable_number;
    SUCCESS_INDICATOR =
        (VarNum >= rtti_implementation.first_exist_quant_varnum);
").

:- pragma foreign_proc("Java",
    is_exist_pseudo_type_info(PseudoTypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = PseudoTypeInfo.variable_number;
    SUCCESS_INDICATOR =
        (VarNum >= rtti_implementation.first_exist_quant_varnum);
").

is_exist_pseudo_type_info(_, _) :-
    private_builtin.sorry("is_exist_pseudo_type_info/2").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pragma foreign_code("C#",
"
    private static bool
    ML_construct(runtime.TypeInfo_Struct TypeInfo, int FunctorNumber,
        list.List_1 ArgList, out univ.Univ_0 Term)
    {
        // If type_info is an equivalence type, expand it.
        TypeInfo = ML_collapse_equivalences(TypeInfo);

        object new_data = null;

        // XXX catch exceptions
        {
            runtime.TypeCtorInfo_Struct tc = TypeInfo.type_ctor;

            switch (tc.type_ctor_rep) {

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_ENUM:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_ENUM_USEREQ:
                runtime.EnumFunctorDesc[] functors_enum =
                    tc.type_functors.functors_enum();
                if (FunctorNumber >= 0 && FunctorNumber < functors_enum.Length)
                {
                    new_data = ML_construct_static_member(tc,
                        functors_enum[FunctorNumber].enum_functor_value);
                }
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN_ENUM:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
                runtime.ForeignEnumFunctorDesc[] functors_foreign_enum =
                    tc.type_functors.functors_foreign_enum();
                if (
                    FunctorNumber >= 0 &&
                    FunctorNumber < functors_foreign_enum.Length
                ) {
                    new_data = ML_construct_static_member(tc,
                        functors_foreign_enum[FunctorNumber].foreign_enum_functor_value);
                }
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_USEREQ:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_GROUND:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_RESERVED_ADDR:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
                // These don't exist in the C# backend yet.
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_DU:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_DU_USEREQ:
                runtime.DuFunctorDesc[] functor_descs =
                    tc.type_functors.functors_du();
                if (FunctorNumber >= 0 && FunctorNumber < functor_descs.Length)
                {
                    runtime.DuFunctorDesc functor_desc =
                        functor_descs[FunctorNumber];
                    if (functor_desc.du_functor_subtype_info !=
                        runtime.FunctorSubtypeInfo.MR_FUNCTOR_SUBTYPE_NONE)
                    {
                        runtime.Errors.SORRY(""construction of terms "" +
                            ""containing subtype constraints"");
                    }
                    new_data = ML_construct_du(tc, functor_desc, ArgList);
                }
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
                int arity = TypeInfo.args.Length;
                new_data = ML_univ_list_to_array(ArgList, arity);
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_DUMMY:
                if (FunctorNumber == 0 && ArgList is list.List_1.F_nil_0) {
                    new_data = ML_construct_static_member(tc, 0);
                }
                break;

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_INT:
                // ints don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct int with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_INT8:
                // int8s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct int8 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_INT16:
                // int16s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct int16 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_INT32:
                // int32s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct int32 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_INT64:
                // int64s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct int64 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT:
                // uints don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct uint with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT8:
                // uint8s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct uint8 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT16:
                // uint16s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct uint16 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT32:
                // uint32s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct uint32 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT64:
                // uint64s don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct uint64 with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_FLOAT:
                // floats don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct float with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_CHAR:
                // chars don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct chars with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_STRING:
                // strings don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct strings with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_BITMAP:
                // bitmaps don't have functor ordinals.
                throw new System.Exception(
                    ""cannot construct bitmaps with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_EQUIV:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_EQUIV_GROUND:
                // These should be eliminated above.
                throw new System.Exception(
                    ""equiv type in construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_VOID:
                // These should be eliminated above.
                throw new System.Exception(
                    ""cannot construct void values with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
                throw new System.Exception(
                    ""cannot construct functions with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
                throw new System.Exception(
                    ""cannot construct predicates with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_SUBGOAL:
                throw new System.Exception(
                    ""cannot construct subgoals with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPEDESC:
                throw new System.Exception(
                    ""cannot construct type_descs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECTORDESC:
                throw new System.Exception(
                    ""cannot construct type_descs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_PSEUDOTYPEDESC:
                throw new System.Exception(
                    ""cannot construct pseudotype_descs with "" +
                    ""construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPEINFO:
                throw new System.Exception(
                    ""cannot construct type_infos with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECTORINFO:
                throw new System.Exception(
                    ""cannot construct type_ctor_infos with "" +
                    ""construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECLASSINFO:
                throw new System.Exception(
                    ""cannot construct type_class_infos with "" +
                    ""construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_BASETYPECLASSINFO:
                throw new System.Exception(
                    ""cannot construct base_type_class_infos "" +
                    ""with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_SUCCIP:
                throw new System.Exception(
                    ""cannot construct succips with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_HP:
                throw new System.Exception(
                    ""cannot construct hps with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_CURFR:
                throw new System.Exception(
                    ""cannot construct curfrs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_MAXFR:
                throw new System.Exception(
                    ""cannot construct maxfrs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_REDOFR:
                throw new System.Exception(
                    ""cannot construct redofrs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_REDOIP:
                throw new System.Exception(
                    ""cannot construct redoips with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TRAIL_PTR:
                throw new System.Exception(
                    ""cannot construct trail_ptrs with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_TICKET:
                throw new System.Exception(
                    ""cannot construct tickets with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_C_POINTER:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_STABLE_C_POINTER:
                throw new System.Exception(
                    ""cannot construct c_pointers with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_ARRAY:
                throw new System.Exception(
                    ""cannot construct arrays with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_REFERENCE:
                throw new System.Exception(
                    ""cannot construct references with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN:
            case runtime.TypeCtorRep.MR_TYPECTOR_REP_STABLE_FOREIGN:
                throw new System.Exception(
                    ""cannot construct values of foreign types "" +
                    ""with construct.construct"");

            case runtime.TypeCtorRep.MR_TYPECTOR_REP_UNKNOWN:
                throw new System.Exception(
                    ""cannot construct values of unknown types "" +
                    ""with construct.construct"");

            default:
                throw new System.Exception(
                    ""bad type_ctor_rep in construct.construct"");
            }
        }

        if (new_data != null) {
            Term = new univ.Univ_0(TypeInfo, new_data);
            return true;
        } else {
            Term = null;
            return false;
        }
    }

    private static object
    ML_construct_du(runtime.TypeCtorInfo_Struct tc,
        runtime.DuFunctorDesc functor_desc, list.List_1 arg_list)
    {
        TypeCtorInfo_Struct base_tc = tc.type_ctor_base;
        if (base_tc != null) {
            // For subtypes, we must derive the class and field names from the
            // base type ctor.
            byte ptag = functor_desc.du_functor_primary;
            DuPtagLayout ptag_layout =
                base_tc.index_or_search_ptag_layout(ptag);

            int sectag = functor_desc.du_functor_secondary;
            if (sectag == -1) {
                sectag = 0;
            }
            DuFunctorDesc base_functor_desc =
                ptag_layout.index_or_search_sectag_functor(sectag);

            return ML_construct_du_2(base_tc, base_functor_desc, arg_list);
        } else {
            return ML_construct_du_2(tc, functor_desc, arg_list);
        }
    }

    private static object
    ML_construct_du_2(runtime.TypeCtorInfo_Struct tc,
        runtime.DuFunctorDesc functor_desc, list.List_1 arg_list)
    {
        string typename;
        System.Type type;

        if (tc.type_ctor_num_functors == 1) {
            typename =
                ""mercury."" + ML_name_mangle(tc.type_ctor_module_name)
                + ""+"" + ML_flipInitialCase(ML_name_mangle(tc.type_ctor_name))
                + ""_"" + tc.arity;
        } else {
            typename =
                ""mercury."" + ML_name_mangle(tc.type_ctor_module_name)
                + ""+"" + ML_flipInitialCase(ML_name_mangle(tc.type_ctor_name))
                + ""_"" + tc.arity
                + ""+"" + ML_flipInitialCase(ML_name_mangle(
                            functor_desc.du_functor_name))
                + ""_"" + functor_desc.du_functor_orig_arity;
        }
        type = ML_search_type(typename);

        int arity = functor_desc.du_functor_orig_arity;
        object[] args = ML_univ_list_to_array(arg_list, arity);

        if (args == null) {
            // Argument list length doesn't match arity.
            return null;
        }

        // XXX C# catch exceptions
        return System.Activator.CreateInstance(type, args);
    }

    private static object[]
    ML_univ_list_to_array(list.List_1 lst, int arity)
    {
        object[] args = new object[arity];

        for (int i = 0; i < arity; i++) {
            TypeInfo_Struct ti;
            univ.ML_unravel_univ(out ti,
                (univ.Univ_0) list.det_head(lst), out args[i]);
            lst = list.det_tail(lst);
        }

        if (list.is_empty(lst)) {
            return args;
        } else {
            return null;
        }
    }

    private static object
    ML_construct_static_member(runtime.TypeCtorInfo_Struct tc, int i)
    {
        // For subtypes, we must derive the class name from the base type ctor.
        if (tc.type_ctor_base != null) {
            tc = tc.type_ctor_base;
        }

        string typename =
            ""mercury."" + ML_name_mangle(tc.type_ctor_module_name)
            + ""+"" + ML_flipInitialCase(ML_name_mangle(tc.type_ctor_name))
            + ""_"" + tc.arity;
        System.Type type = ML_search_type(typename);
        return System.Enum.ToObject(type, i);
    }

    private static System.Type
    ML_search_type(string typename)
    {
        // Do we need to optimise this?  e.g. search the current assembly,
        // then that which contains the standard library, or cache old results.
        System.Reflection.Assembly[] av =
            System.AppDomain.CurrentDomain.GetAssemblies();
        foreach (System.Reflection.Assembly a in av) {
            System.Type t = a.GetType(typename);
            if (t != null) {
                return t;
            }
        }
        return null;
    }

    // XXX fix name
    private static string
    ML_flipInitialCase(string s)
    {
        if (s.Length > 0) {
            char first = s[0];
            string rest = s.Substring(1);
            if (System.Char.IsLower(first)) {
                return System.Char.ToUpper(first) + rest;
            }
            if (System.Char.IsUpper(first)) {
                return System.Char.ToLower(first) + rest;
            }
        }
        return s;
    }

    // Duplicated and modified from Java version.
    private static string
    ML_name_mangle(string s)
    {
        bool all_ok = true;
        if (s.Length < 1) {
            all_ok = false;
        }
        if (all_ok && System.Char.IsDigit(s, 0)) {
            all_ok = false;
        }
        if (all_ok) {
            foreach (char c in s) {
                if ((c >= 'A' && c <= 'Z') ||
                    (c >= 'a' && c <= 'z') ||
                    (c >= '0' && c <= '9') ||
                    (c == '_'))
                {
                    // do nothing
                } else {
                    all_ok = false;
                    break;
                }
            }
        }
        if (all_ok) {
            if (s.StartsWith(""f_"")) {
                return ""f__"" + s.Substring(2);
            } else {
                return s;
            }
        }

        // This is from prog_foreign.name_conversion_table.
        if (s.Equals(""\\\\="")) return ""f_not_equal"";
        if (s.Equals("">="")) return ""f_greater_or_equal"";
        if (s.Equals(""=<"")) return ""f_less_or_equal"";
        if (s.Equals(""="")) return ""f_equal"";
        if (s.Equals(""<"")) return ""f_less_than"";
        if (s.Equals("">"")) return ""f_greater_than"";
        if (s.Equals(""-"")) return ""f_minus"";
        if (s.Equals(""+"")) return ""f_plus"";
        if (s.Equals(""*"")) return ""f_times"";
        if (s.Equals(""/"")) return ""f_slash"";
        if (s.Equals("","")) return ""f_comma"";
        if (s.Equals("";"")) return ""f_semicolon"";
        if (s.Equals(""!"")) return ""f_cut"";
        if (s.Equals(""{}"")) return ""f_tuple"";
        if (s.Equals(""[|]"")) return ""f_cons"";
        if (s.Equals(""[]"")) return ""f_nil"";

        System.Text.StringBuilder sb = new System.Text.StringBuilder(""f"");
        foreach (char c in s.ToCharArray()) {
            sb.Append('_');
            sb.Append((int) c);
        }
        return sb.ToString();
    }

    private static object[]
    ML_list_to_array(list.List_1 lst, int arity)
    {
        object[] array = new object[arity];

        for (int i = 0; i < arity; i++) {
            array[i] = list.det_head(lst);
            lst = list.det_tail(lst);
        }

        return array;
    }
").

:- pragma foreign_code("Java",
"
    private static Object[]
    ML_construct(TypeInfo_Struct TypeInfo, int FunctorNumber,
        list.List_1<univ.Univ_0> ArgList)
    {
        // If type_info is an equivalence type, expand it.
        TypeInfo = ML_collapse_equivalences(TypeInfo);

        Object new_data = null;

        try {
            final jmercury.runtime.TypeCtorInfo_Struct tc = TypeInfo.type_ctor;

            switch (tc.type_ctor_rep.value) {

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_ENUM:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_ENUM_USEREQ:
                EnumFunctorDesc[] functors_enum =
                    tc.type_functors.functors_enum();
                if (FunctorNumber >= 0 && FunctorNumber < functors_enum.length)
                {
                    new_data = ML_construct_static_member(tc,
                        functors_enum[FunctorNumber].enum_functor_value);
                }
                break;

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN_ENUM:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN_ENUM_USEREQ:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_USEREQ:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_GROUND:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_NOTAG_GROUND_USEREQ:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_RESERVED_ADDR:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_RESERVED_ADDR_USEREQ:
                // These don't exist in the Java backend yet.
                break;

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_DU:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_DU_USEREQ:
                DuFunctorDesc[] functor_descs = tc.type_functors.functors_du();
                if (FunctorNumber >= 0 && FunctorNumber < functor_descs.length)
                {
                    DuFunctorDesc functor_desc = functor_descs[FunctorNumber];
                    if (functor_desc.du_functor_subtype_info.value !=
                        private_builtin.MR_FUNCTOR_SUBTYPE_NONE)
                    {
                        throw new Error(""not yet implemented: construction ""
                            + ""of terms containing subtype constraints"");
                    }
                    new_data = ML_construct_du(tc, functor_desc, ArgList);
                }
                break;

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE:
                int arity = TypeInfo.args.length;
                new_data = ML_univ_list_to_array(ArgList, arity);
                break;

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_DUMMY:
                if (FunctorNumber == 0 &&
                    ArgList instanceof list.List_1.F_nil_0)
                {
                    new_data = ML_construct_static_member(tc, 0);
                }
                break;

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_INT:
                // ints don't have functor ordinals.
                throw new Error(
                    ""cannot construct int with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_INT8:
                // int8s don't have functor ordinals.
                throw new Error(
                    ""cannot construct int8 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_INT16:
                // int16s don't have functor ordinals.
                throw new Error(
                    ""cannot construct int16 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_INT32:
                // int32s don't have functor ordinals.
                throw new Error(
                    ""cannot construct int32 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_INT64:
                // int64s don't have functor ordinals.
                throw new Error(
                    ""cannot construct int64 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT:
                // ints don't have functor ordinals.
                throw new Error(
                    ""cannot construct uint with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT8:
                // uint8s don't have functor ordinals.
                throw new Error(
                    ""cannot construct uint8 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT16:
                // uint16s don't have functor ordinals.
                throw new Error(
                    ""cannot construct uint16 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT32:
                // uint32s don't have functor ordinals.
                throw new Error(
                    ""cannot construct uint32 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UINT64:
                // uint64s don't have functor ordinals.
                throw new Error(
                    ""cannot construct uint64 with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FLOAT:
                // floats don't have functor ordinals.
                throw new Error(
                    ""cannot construct float with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_CHAR:
                // chars don't have functor ordinals.
                throw new Error(
                    ""cannot construct chars with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_STRING:
                // strings don't have functor ordinals.
                throw new Error(
                    ""cannot construct strings with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_BITMAP:
                // bitmaps don't have functor ordinals.
                throw new Error(
                    ""cannot construct bitmaps with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_EQUIV:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_EQUIV_GROUND:
                // These should be eliminated above.
                throw new Error(""equiv type in construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_VOID:
                // These should be eliminated above.
                throw new Error(
                    ""cannot construct void values with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FUNC:
                throw new Error(
                    ""cannot construct functions with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_PRED:
                throw new Error(
                    ""cannot construct predicates with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_SUBGOAL:
                throw new Error(
                    ""cannot construct subgoals with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPEDESC:
                throw new Error(
                    ""cannot construct type_descs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECTORDESC:
                throw new Error(
                    ""cannot construct type_descs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_PSEUDOTYPEDESC:
                throw new Error(
                    ""cannot construct pseudotype_descs with "" +
                    ""construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPEINFO:
                throw new Error(
                    ""cannot construct type_infos with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECTORINFO:
                throw new Error(
                    ""cannot construct type_ctor_infos with "" +
                    ""construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TYPECLASSINFO:
                throw new Error(
                    ""cannot construct type_class_infos with "" +
                    ""construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_BASETYPECLASSINFO:
                throw new Error(
                    ""cannot construct base_type_class_infos "" +
                    ""with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_SUCCIP:
                throw new Error(
                    ""cannot construct succips with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_HP:
                throw new Error(
                    ""cannot construct hps with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_CURFR:
                throw new Error(
                    ""cannot construct curfrs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_MAXFR:
                throw new Error(
                    ""cannot construct maxfrs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_REDOFR:
                throw new Error(
                    ""cannot construct redofrs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_REDOIP:
                throw new Error(
                    ""cannot construct redoips with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TRAIL_PTR:
                throw new Error(
                    ""cannot construct trail_ptrs with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TICKET:
                throw new Error(
                    ""cannot construct tickets with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_C_POINTER:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_STABLE_C_POINTER:
                throw new Error(
                    ""cannot construct c_pointers with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_ARRAY:
                throw new Error(
                    ""cannot construct arrays with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_REFERENCE:
                throw new Error(
                    ""cannot construct references with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_FOREIGN:
            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_STABLE_FOREIGN:
                throw new Error(
                    ""cannot construct values of foreign types "" +
                    ""with construct.construct"");

            case jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_UNKNOWN:
                throw new Error(
                    ""cannot construct values of unknown types "" +
                    ""with construct.construct"");

            default:
                throw new Error(""bad type_ctor_rep in construct.construct"");
            }
        } catch (ClassNotFoundException e) {
            throw new Error(e.getMessage());
        } catch (NoSuchFieldException e) {
            throw new Error(e.getMessage());
        } catch (IllegalAccessException e) {
            throw new Error(e.getMessage());
        } catch (InstantiationException e) {
            throw new Error(e.getMessage());
        } catch (InvocationTargetException e) {
            throw new Error(e.getMessage());
        }

        boolean succeeded;
        Object Term;

        if (new_data != null) {
            succeeded = true;
            Term = new univ.Univ_0(TypeInfo, new_data);
        } else {
            succeeded = false;
            Term = null;
        }

        return new Object[] { succeeded, Term };
    }

    private static Object
    ML_construct_du(TypeCtorInfo_Struct tc, DuFunctorDesc functor_desc,
        list.List_1<univ.Univ_0> arg_list)
        throws ClassNotFoundException, NoSuchFieldException,
            IllegalAccessException, InstantiationException,
            InvocationTargetException
    {
        final jmercury.runtime.TypeCtorInfo_Struct base_tc = tc.type_ctor_base;
        if (base_tc != null) {
            // For subtypes, we must derive the class and field names from the
            // base type ctor.
            byte ptag = functor_desc.du_functor_primary;
            jmercury.runtime.DuPtagLayout ptag_layout =
                base_tc.index_or_search_ptag_layout(ptag);

            int sectag = functor_desc.du_functor_secondary;
            if (sectag == -1) {
                sectag = 0;
            }
            jmercury.runtime.DuFunctorDesc base_functor_desc =
                ptag_layout.index_or_search_sectag_functor(sectag);

            return ML_construct_du_2(base_tc, base_functor_desc, arg_list);
        } else {
            return ML_construct_du_2(tc, functor_desc, arg_list);
        }
    }

    private static Object
    ML_construct_du_2(TypeCtorInfo_Struct tc, DuFunctorDesc functor_desc,
        list.List_1<univ.Univ_0> arg_list)
        throws
            ClassNotFoundException, NoSuchFieldException,
            IllegalAccessException, InstantiationException,
            InvocationTargetException
    {
        String clsname;

        if (tc.type_ctor_num_functors == 1) {
            clsname = ""jmercury."" + ML_name_mangle(tc.type_ctor_module_name)
                + ""$"" + ML_flipInitialCase(ML_name_mangle(tc.type_ctor_name))
                + ""_"" + tc.arity;
        } else {
            String mangled_mod_name = ML_name_mangle(tc.type_ctor_module_name);
            String mangled_ctor_name = ML_name_mangle(tc.type_ctor_name) +
                ""_"" + tc.arity;

            String mangled_functor_name;
            if (tc.type_ctor_name.equals(functor_desc.du_functor_name) &&
                tc.arity == functor_desc.du_functor_orig_arity)
            {
                mangled_functor_name =
                    ML_name_mangle(""mr_"" + functor_desc.du_functor_name) +
                    ""_"" + functor_desc.du_functor_orig_arity;
            } else {
                mangled_functor_name =
                    ML_name_mangle(functor_desc.du_functor_name) +
                    ""_"" + functor_desc.du_functor_orig_arity;
            }

            clsname = ""jmercury."" + mangled_mod_name + ""$"" +
                ML_flipInitialCase(mangled_ctor_name) + ""$"" +
                ML_flipInitialCase(mangled_functor_name);
        }

        final Class<?> cls = Class.forName(clsname);

        final int arity = functor_desc.du_functor_orig_arity;
        final Object[] args = ML_univ_list_to_array(arg_list, arity);

        if (args == null) {
            // Argument list length doesn't match arity.
            return null;
        }

        for (Constructor ctor : cls.getConstructors()) {
            Class<?>[] param_types = ctor.getParameterTypes();
            if (param_types.length == arity) {
                try {
                    return ctor.newInstance(args);
                } catch (IllegalArgumentException e) {
                    // e.g. argument type mismatch
                    return null;
                }
            }
        }

        throw new Error(
            ""construct.construct: could not find constructor for functor"");
    }

    private static Object[]
    ML_univ_list_to_array(list.List_1<univ.Univ_0> lst, int arity)
    {
        final Object[] args = new Object[arity];
        final Ref<TypeInfo_Struct> ti_ref = new Ref<TypeInfo_Struct>();
        final Ref<Object> obj_ref = new Ref<Object>();

        for (int i = 0; i < arity; i++) {
            univ.ML_unravel_univ(ti_ref, list.det_head(lst), obj_ref);
            args[i] = obj_ref.val;
            lst = list.det_tail(lst);
        }

        if (list.is_empty(lst)) {
            return args;
        } else {
            return null;
        }
    }

    private static Object
    ML_construct_static_member(TypeCtorInfo_Struct tc, int i)
        throws ClassNotFoundException, NoSuchFieldException,
            IllegalAccessException
    {
        // For subtypes, we must derive the class name from the base type ctor.
        if (tc.type_ctor_base != null) {
            tc = tc.type_ctor_base;
        }

        Class<?> cls = Class.forName(
            ""jmercury."" + ML_name_mangle(tc.type_ctor_module_name)
            + ""$"" + ML_flipInitialCase(ML_name_mangle(tc.type_ctor_name))
            + ""_"" + tc.arity);

        String field_name = ""K"" + i;
        Field field = cls.getField(field_name);

        return field.get(cls);
    }

    // XXX fix name
    private static String
    ML_flipInitialCase(String s)
    {
        if (s.length() > 0) {
            char first = s.charAt(0);
            String rest = s.substring(1);
            if (Character.isLowerCase(first)) {
                return Character.toString(Character.toUpperCase(first)) + rest;
            }
            if (Character.isUpperCase(first)) {
                return Character.toString(Character.toLowerCase(first)) + rest;
            }
        }
        return s;
    }

    private static String
    ML_name_mangle(String s)
    {
        if (s.matches(""[A-Za-z_][A-Za-z0-9_]*"")) {
            if (s.startsWith(""f_"")) {
                return ""f__"" + s.substring(2);
            } else {
                return s;
            }
        }

        // This is from prog_foreign.name_conversion_table.
        if (s.equals(""\\\\="")) return ""f_not_equal"";
        if (s.equals("">="")) return ""f_greater_or_equal"";
        if (s.equals(""=<"")) return ""f_less_or_equal"";
        if (s.equals(""="")) return ""f_equal"";
        if (s.equals(""<"")) return ""f_less_than"";
        if (s.equals("">"")) return ""f_greater_than"";
        if (s.equals(""-"")) return ""f_minus"";
        if (s.equals(""+"")) return ""f_plus"";
        if (s.equals(""*"")) return ""f_times"";
        if (s.equals(""/"")) return ""f_slash"";
        if (s.equals("","")) return ""f_comma"";
        if (s.equals("";"")) return ""f_semicolon"";
        if (s.equals(""!"")) return ""f_cut"";
        if (s.equals(""{}"")) return ""f_tuple"";
        if (s.equals(""[|]"")) return ""f_cons"";
        if (s.equals(""[]"")) return ""f_nil"";

        StringBuilder sb = new StringBuilder(""f"");
        for (int i = 0; i < s.length(); i++) {
            sb.append('_');
            sb.append(s.codePointAt(i));
        }
        return sb.toString();
    }

    private static Object[]
    ML_list_to_array(list.List_1 lst, int arity)
    {
        final Object[] array = new Object[arity];

        for (int i = 0; i < arity; i++) {
            array[i] = list.det_head(lst);
            lst = list.det_tail(lst);
        }

        return array;
    }
").

:- pragma no_determinism_warning(func(construct/3)).

:- pragma foreign_proc("C#",
    construct(TypeInfo::in, FunctorNumber::in, ArgList::in) = (Term::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = ML_construct(TypeInfo, FunctorNumber, ArgList,
        out Term);
").

:- pragma foreign_proc("Java",
    construct(TypeInfo::in, FunctorNumber::in, ArgList::in) = (Term::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    Object[] rc = ML_construct(TypeInfo, FunctorNumber, ArgList);
    SUCCESS_INDICATOR = (Boolean) rc[0];
    Term = (univ.Univ_0) rc[1];
").

construct(_, _, _) = _ :-
    private_builtin.sorry("construct/3").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(func(construct_tuple_2/3)).

:- pragma foreign_proc("C#",
    construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Tuple::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    list.List_1 args_list = Args;
    object[] args_array = new object[Arity];

    for (int i = 0; i < Arity; i++) {
        TypeInfo_Struct ti_tmp;
        univ.ML_unravel_univ(out ti_tmp,
            (univ.Univ_0) list.det_head(args_list), out args_array[i]);
        args_list = list.det_tail(args_list);
    }

    object[] args = ML_list_to_array(ArgTypes, Arity);
    runtime.TypeInfo_Struct ti = new TypeInfo_Struct();
    ti.init(builtin.builtin__type_ctor_info_tuple_0, args);

    Tuple = univ.ML_construct_univ(ti, args_array);
").

:- pragma foreign_proc("Java",
    construct_tuple_2(Args::in, ArgTypes::in, Arity::in) = (Tuple::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    list.List_1<univ.Univ_0> args_list = Args;
    Object[] args_array = new Object[Arity];
    Ref<TypeInfo_Struct> ti_ref = new Ref<TypeInfo_Struct>();
    Ref<Object> obj_ref = new Ref<Object>();

    for (int i = 0; i < Arity; i++) {
        univ.ML_unravel_univ(ti_ref, list.det_head(args_list), obj_ref);
        args_array[i] = obj_ref.val;
        args_list = list.det_tail(args_list);
    }

    Object[] args = ML_list_to_array(ArgTypes, Arity);
    TypeInfo_Struct ti = new TypeInfo_Struct();
    ti.init(builtin.builtin__type_ctor_info_tuple_0, args);

    Tuple = univ.ML_construct_univ(ti, args_array);
").

construct_tuple_2(_Args, _ArgTypes, _Arity) = _ :-
    private_builtin.sorry("construct_tuple_2/3").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

deconstruct(Term, NonCanon, Functor, FunctorNumber, Arity, Arguments) :-
    TypeInfo = get_type_info(Term),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    deconstruct_2(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
        Functor, Ordinal, Arity, Arguments),
    ( if
        Ordinal >= 0,
        type_ctor_search_functor_number_map(TypeCtorInfo, Ordinal,
            FunctorNumber0)
    then
        FunctorNumber = FunctorNumber0
    else
        FunctorNumber = 0
    ).

functor_number_cc(Term, FunctorNumber, Arity) :-
    TypeInfo = get_type_info(Term),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    promise_equivalent_solutions [Ordinal, Arity] (
        deconstruct_2(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            include_details_cc, _Functor, Ordinal, Arity, _Arguments)
    ),
    Ordinal >= 0,
    type_ctor_search_functor_number_map(TypeCtorInfo, Ordinal, FunctorNumber0),
    cc_multi_equal(FunctorNumber0, FunctorNumber).

:- pred deconstruct_2(T, type_info, type_ctor_info, type_ctor_rep,
    noncanon_handling, string, int, int, list(univ)).
:- mode deconstruct_2(in, in, in, in, in(do_not_allow), out, out, out, out)
    is det.
:- mode deconstruct_2(in, in, in, in, in(canonicalize), out, out, out, out)
    is det.
:- mode deconstruct_2(in, in, in, in, in(include_details_cc), out, out, out,
    out) is cc_multi.
:- mode deconstruct_2(in, in, in, in, in, out, out, out, out) is cc_multi.

deconstruct_2(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
        Functor, Ordinal, Arity, Arguments) :-
    % Code to perform deconstructions (XXX not yet complete).
    %
    % There are many cases to implement here, only the ones that were
    % immediately useful (e.g. called by io.write) have been implemented
    % so far.
    (
        TypeCtorRep = tcr_enum_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            NonCanon, Functor, Ordinal, Arity, Arguments)
    ;
        TypeCtorRep = tcr_enum,
        EnumValue = unsafe_get_enum_value(Term),
        det_index_or_search_enum_functor_ordinal(TypeCtorInfo, EnumValue,
            Ordinal),
        index_enum_functor_desc(TypeCtorInfo, Ordinal, EnumFunctorDesc),
        Functor = enum_functor_name(EnumFunctorDesc),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_foreign_enum,
        TypeFunctors = get_type_ctor_functors(TypeCtorInfo),
        ForeignEnumFunctorDesc = foreign_enum_functor_desc(TypeCtorRep,
            unsafe_get_foreign_enum_value(Term), TypeFunctors),
        Functor = foreign_enum_functor_name(ForeignEnumFunctorDesc),
        Ordinal = foreign_enum_functor_ordinal(ForeignEnumFunctorDesc),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_foreign_enum_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            NonCanon, Functor, Ordinal, Arity, Arguments)
    ;
        TypeCtorRep = tcr_dummy,
        Ordinal = 0,
        index_enum_functor_desc(TypeCtorInfo, Ordinal, EnumFunctorDesc),
        Functor = enum_functor_name(EnumFunctorDesc),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_du_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            NonCanon, Functor, Ordinal, Arity, Arguments)
    ;
        TypeCtorRep = tcr_du,
        PTag = get_primary_tag(Term),
        det_index_or_search_ptag_layout(TypeCtorInfo, PTag, PTagEntry),
        SecTagLocn = PTagEntry ^ sectag_locn,
        (
            (
                SecTagLocn = stag_none,
                index_sectag_functor(PTagEntry, 0, FunctorDesc)
            ;
                SecTagLocn = stag_none_direct_arg,
                index_sectag_functor(PTagEntry, 0, FunctorDesc)
            ;
                SecTagLocn = stag_remote,
                SecTag = get_remote_secondary_tag(Term),
                det_index_or_search_sectag_functor(PTagEntry, SecTag,
                    FunctorDesc)
            ),
            Functor = FunctorDesc ^ du_functor_name,
            Ordinal = int32.to_int(FunctorDesc ^ du_functor_ordinal),
            Arity = int16.to_int(FunctorDesc ^ du_functor_arity),
            Arguments = iterate(0, Arity - 1,
                get_arg_univ(Term, TypeInfo, SecTagLocn, FunctorDesc))
        ;
            SecTagLocn = stag_local_rest_of_word,
            Functor = "some_du_local_sectag",
            % XXX incomplete
            Ordinal = -1,
            Arity = 0,
            Arguments = []
        ;
            SecTagLocn = stag_variable,
            Functor = "some_du_variable_sectag",
            Ordinal = -1,
            Arity = 0,
            Arguments = []
        )
    ;
        TypeCtorRep = tcr_notag_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Ordinal, Arity, Arguments)
    ;
        TypeCtorRep = tcr_notag,
        % XXX incomplete
        Functor = "some_notag",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_notag_ground_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Ordinal, Arity, Arguments)
    ;
        TypeCtorRep = tcr_notag_ground,
        % XXX incomplete
        Functor = "some_notag_ground",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_equiv_ground,
        NewTypeInfo = collapse_equivalences(TypeInfo),
        NewTypeCtorInfo = get_type_ctor_info(NewTypeInfo),
        NewTypeCtorRep = get_type_ctor_rep(NewTypeCtorInfo),
        deconstruct_2(Term, NewTypeInfo, NewTypeCtorInfo, NewTypeCtorRep,
            NonCanon, Functor, Ordinal, Arity, Arguments)
    ;
        % XXX noncanonical term
        TypeCtorRep = tcr_func,
        Functor = "<<function>>",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_equiv,
        % XXX incomplete
        Functor = "some_equiv",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_int,
        det_dynamic_cast(Term, Int),
        Functor = string.int_to_string(Int),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_uint,
        det_dynamic_cast(Term, UInt),
        Functor = string.uint_to_string(UInt) ++ "u",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_int8,
        det_dynamic_cast(Term, Int8),
        Functor = string.int8_to_string(Int8) ++ "i8",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_uint8,
        det_dynamic_cast(Term, UInt8),
        Functor = string.uint8_to_string(UInt8) ++ "u8",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_int16,
        det_dynamic_cast(Term, Int16),
        Functor = string.int16_to_string(Int16) ++ "i16",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_uint16,
        det_dynamic_cast(Term, UInt16),
        Functor = string.uint16_to_string(UInt16) ++ "u16",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_int32,
        det_dynamic_cast(Term, Int32),
        Functor = string.int32_to_string(Int32) ++ "i32",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_uint32,
        det_dynamic_cast(Term, UInt32),
        Functor = string.uint32_to_string(UInt32) ++ "u32",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_int64,
        det_dynamic_cast(Term, Int64),
        Functor = string.int64_to_string(Int64) ++ "i64",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_uint64,
        det_dynamic_cast(Term, UInt64),
        Functor = string.uint64_to_string(UInt64) ++ "u64",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        % Changes here may need to reflected in the handling of
        % MR_TYPCTOR_REP_CHAR in runtime/mercury_ml_expand_body.h.
        TypeCtorRep = tcr_char,
        det_dynamic_cast(Term, Char),
        ( if quote_special_escape_char(Char, EscapedChar) then
            Functor = EscapedChar
        else if
            char.is_control(Char)
        then
            char.to_int(Char, Int),
            string.int_to_base_string(Int, 8, OctalString0),
            string.pad_left(OctalString0, '0', 3, OctalString),
            Functor  = "'\\" ++ OctalString ++ "\\'"
        else if
            char.is_surrogate(Char)
        then
            unexpected($pred, "attempt to deconstruct surrogate code point")
        else
            Functor = string.from_char_list(['\'', Char, '\''])
        ),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_float,
        det_dynamic_cast(Term, Float),
        Functor = float_to_string(Float),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_string,
        det_dynamic_cast(Term, String),
        Functor = term_io.quoted_string(String),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_bitmap,
        det_dynamic_cast(Term, Bitmap),
        String = bitmap.to_string(Bitmap),
        Functor = "\"" ++ String ++ "\"",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = tcr_pred,
        Functor = "<<predicate>>",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_tuple,
        type_ctor_and_args(TypeInfo, _TypeCtorInfo, TypeArgs),
        Functor = "{}",
        Ordinal = 0,
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        list.map_foldl(
            ( pred(ArgTypeInfo::in, Univ::out, Index::in, Next::out) is det :-
                get_tuple_subterm(Term, Index, ArgTypeInfo, SubTerm),
                Univ = univ(SubTerm),
                Next = Index + 1
            ), TypeArgs, Arguments, 0, _)
    ;
        % XXX noncanonical term
        TypeCtorRep = tcr_subgoal,
        Functor = "<<subgoal>>",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        % There is no way to create values of type `void', so this
        % should never happen.
        TypeCtorRep = tcr_void,
        unexpected($pred, "cannot deconstruct void types")
    ;
        TypeCtorRep = tcr_c_pointer,
        det_dynamic_cast(Term, CPtr),
        Functor = string.c_pointer_to_string(CPtr),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_stable_c_pointer,
        det_dynamic_cast(Term, CPtr),
        Functor = "stable_" ++ string.c_pointer_to_string(CPtr),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_array,

        % Constrain the T in array(T) to the correct element type.
        type_ctor_and_args(type_of(Term), _, Args),
        ( if Args = [ElemType] then
            has_type(Elem, ElemType),
            same_array_elem_type(Array, Elem)
        else
            unexpected($pred, "array without a type_ctor arg")
        ),

        det_dynamic_cast(Term, Array),

        Functor = "<<array>>",
        Ordinal = -1,
        Arity = array.size(Array),
        Arguments = array.foldr(
            (func(Elem, List) = [univ(Elem) | List]),
            Array, [])
    ;
        (
            TypeCtorRep = tcr_succip,
            Functor = "<<succip>>"
        ;
            TypeCtorRep = tcr_hp,
            Functor = "<<hp>>"
        ;
            TypeCtorRep = tcr_curfr,
            Functor = "<<curfr>>"
        ;
            TypeCtorRep = tcr_maxfr,
            Functor = "<<maxfr>>"
        ;
            TypeCtorRep = tcr_redofr,
            Functor = "<<redofr>>"
        ;
            TypeCtorRep = tcr_redoip,
            Functor = "<<redoip>>"
        ;
            TypeCtorRep = tcr_trail_ptr,
            Functor = "<<trail_ptr>>"
        ;
            TypeCtorRep = tcr_ticket,
            Functor = "<<ticket>>"
        ),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        % XXX FIXME!!!
        TypeCtorRep = tcr_reserved_addr,
        Functor = "some_reserved_addr",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_reserved_addr_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Ordinal, Arity, Arguments)
    ;
        % XXX inconsistent: <<xxx>> vs some_xxx
        (
            % XXX noncanonical term
            TypeCtorRep = tcr_typeinfo,
            Functor = "some_typeinfo"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_typeclassinfo,
            Functor = "<<typeclassinfo>>"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_type_ctor_info,
            Functor = "some_typectorinfo"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_base_typeclass_info,
            Functor = "<<basetypeclassinfo>>"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_type_desc,
            Functor = "some_type_desc"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_pseudo_type_desc,
            Functor = "some_pseudo_type_desc"
        ;
            % XXX noncanonical term
            TypeCtorRep = tcr_type_ctor_desc,
            Functor = "some_type_ctor_desc"
        ),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        (
            TypeCtorRep = tcr_foreign,
            TypeCtorName = TypeCtorInfo ^ type_ctor_name,
            TargetLangRep = get_target_lang_rep(Term),
            string.format("<<foreign(%s, %s)>>",
                [s(TypeCtorName), s(TargetLangRep)], Functor)
        ;
            TypeCtorRep = tcr_stable_foreign,
            Functor = "<<stable_foreign>>"
        ),
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = tcr_reference,
        Functor = "<<reference>>",
        Ordinal = -1,
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tcr_unknown,
        unexpected($pred, "unknown type_ctor rep")
    ).

%---------------------------------------------------------------------------%

univ_named_arg(Term, NonCanon, Name, Argument) :-
    TypeInfo = get_type_info(Term),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = get_type_ctor_rep(TypeCtorInfo),
    univ_named_arg_2(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon, Name,
        MaybeArgument),
    MaybeArgument = yes(Argument).

:- pred univ_named_arg_2(T, type_info, type_ctor_info, type_ctor_rep,
    noncanon_handling, string, maybe(univ)).
:- mode univ_named_arg_2(in, in, in, in, in(do_not_allow), in, out) is det.
:- mode univ_named_arg_2(in, in, in, in, in(canonicalize), in, out) is det.
:- mode univ_named_arg_2(in, in, in, in, in(include_details_cc), in, out)
    is det.

univ_named_arg_2(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon, Name,
        MaybeArgument) :-
    (
        TypeCtorRep = tcr_du_usereq,
        (
            NonCanon = do_not_allow,
            unexpected($pred, "attempt to deconstruct noncanonical term")
        ;
            NonCanon = canonicalize,
            MaybeArgument = no
        ;
            NonCanon = include_details_cc,
            univ_named_arg_2(Term, TypeInfo, TypeCtorInfo, tcr_du, NonCanon,
                Name, MaybeArgument)
        )
    ;
        TypeCtorRep = tcr_du,
        PTag = get_primary_tag(Term),
        det_index_or_search_ptag_layout(TypeCtorInfo, PTag, PTagEntry),
        SecTagLocn = PTagEntry ^ sectag_locn,
        (
            (
                SecTagLocn = stag_none,
                index_sectag_functor(PTagEntry, 0, FunctorDesc)
            ;
                SecTagLocn = stag_none_direct_arg,
                index_sectag_functor(PTagEntry, 0, FunctorDesc)
            ;
                SecTagLocn = stag_remote,
                SecTag = get_remote_secondary_tag(Term),
                det_index_or_search_sectag_functor(PTagEntry, SecTag,
                    FunctorDesc)
            ),
            Arity = int16.to_int(FunctorDesc ^ du_functor_arity),
            ( if
                get_du_functor_arg_names(FunctorDesc, Names),
                search_arg_names(Names, 0, Arity, Name, Index)
            then
                ArgUniv = get_arg_univ(Term, TypeInfo, SecTagLocn, FunctorDesc,
                    Index),
                MaybeArgument = yes(ArgUniv)
            else
                MaybeArgument = no
            )
        ;
            SecTagLocn = stag_local_rest_of_word,
            MaybeArgument = no
        ;
            SecTagLocn = stag_variable,
            MaybeArgument = no
        )
    ;
        ( TypeCtorRep = tcr_enum
        ; TypeCtorRep = tcr_enum_usereq
        ; TypeCtorRep = tcr_notag
        ; TypeCtorRep = tcr_notag_usereq
        ; TypeCtorRep = tcr_equiv
        ; TypeCtorRep = tcr_func
        ; TypeCtorRep = tcr_int
        ; TypeCtorRep = tcr_uint
        ; TypeCtorRep = tcr_int8
        ; TypeCtorRep = tcr_uint8
        ; TypeCtorRep = tcr_int16
        ; TypeCtorRep = tcr_uint16
        ; TypeCtorRep = tcr_int32
        ; TypeCtorRep = tcr_uint32
        ; TypeCtorRep = tcr_int64
        ; TypeCtorRep = tcr_uint64
        ; TypeCtorRep = tcr_char
        ; TypeCtorRep = tcr_float
        ; TypeCtorRep = tcr_string
        ; TypeCtorRep = tcr_pred
        ; TypeCtorRep = tcr_subgoal
        ; TypeCtorRep = tcr_c_pointer
        ; TypeCtorRep = tcr_typeinfo
        ; TypeCtorRep = tcr_typeclassinfo
        ; TypeCtorRep = tcr_array
        ; TypeCtorRep = tcr_succip
        ; TypeCtorRep = tcr_hp
        ; TypeCtorRep = tcr_curfr
        ; TypeCtorRep = tcr_maxfr
        ; TypeCtorRep = tcr_redofr
        ; TypeCtorRep = tcr_redoip
        ; TypeCtorRep = tcr_trail_ptr
        ; TypeCtorRep = tcr_ticket
        ; TypeCtorRep = tcr_notag_ground
        ; TypeCtorRep = tcr_notag_ground_usereq
        ; TypeCtorRep = tcr_equiv_ground
        ; TypeCtorRep = tcr_tuple
        ; TypeCtorRep = tcr_reserved_addr
        ; TypeCtorRep = tcr_reserved_addr_usereq
        ; TypeCtorRep = tcr_type_ctor_info
        ; TypeCtorRep = tcr_base_typeclass_info
        ; TypeCtorRep = tcr_type_desc
        ; TypeCtorRep = tcr_type_ctor_desc
        ; TypeCtorRep = tcr_foreign
        ; TypeCtorRep = tcr_reference
        ; TypeCtorRep = tcr_stable_c_pointer
        ; TypeCtorRep = tcr_stable_foreign
        ; TypeCtorRep = tcr_pseudo_type_desc
        ; TypeCtorRep = tcr_dummy
        ; TypeCtorRep = tcr_bitmap
        ; TypeCtorRep = tcr_foreign_enum
        ; TypeCtorRep = tcr_foreign_enum_usereq
        ),
        MaybeArgument = no
    ;
        TypeCtorRep = tcr_void,
        unexpected($pred, "cannot deconstruct void types")
    ;
        TypeCtorRep = tcr_unknown,
        unexpected($pred, "unknown type_ctor rep")
    ).

%---------------------------------------------------------------------------%

    % Note: the code here is similar (but not identical) to escape_char
    % in mercury_term_lexer.m, and the places listed there.
    % Any changes here may require similar changes there.
    %
:- pred quote_special_escape_char(character::in, string::out) is semidet.

quote_special_escape_char('\a', "'\\a'").
quote_special_escape_char('\b', "'\\b'").
quote_special_escape_char('\e', "'\\e'").
quote_special_escape_char('\f', "'\\f'").
quote_special_escape_char('\n', "'\\n'").
quote_special_escape_char('\r', "'\\r'").
quote_special_escape_char('\t', "'\\t'").
quote_special_escape_char('\v', "'\\v'").
quote_special_escape_char('\\', "'\\\\'").
quote_special_escape_char('\'', "'\\''").

:- pred det_dynamic_cast(T::in, U::out) is det.

det_dynamic_cast(Term, Actual) :-
    type_to_univ(Term, Univ),
    det_univ_to_type(Univ, Actual).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

%---------------------------------------------------------------------------%

:- inst usereq for type_ctor_rep/0
    --->    tcr_enum_usereq
    ;       tcr_foreign_enum_usereq
    ;       tcr_du_usereq
    ;       tcr_notag_usereq
    ;       tcr_notag_ground_usereq
    ;       tcr_reserved_addr_usereq.

    % Part of deconstruct_2.
    %
:- pred handle_usereq_type(T, type_info, type_ctor_info, type_ctor_rep,
    noncanon_handling, string, int, int, list(univ)).
:- mode handle_usereq_type(in, in, in, in(usereq),
    in(do_not_allow), out, out, out, out) is erroneous.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in(canonicalize), out, out, out, out) is det.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in(include_details_cc), out, out, out, out) is cc_multi.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in, out, out, out, out) is cc_multi.

handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
        Functor, Ordinal, Arity, Arguments) :-
    (
        NonCanon = do_not_allow,
        unexpected($pred, "attempt to deconstruct noncanonical term")
    ;
        NonCanon = canonicalize,
        Functor = expand_type_name(TypeCtorInfo, yes),
        Ordinal = -1, % not supported anyway
        Arity = 0,
        Arguments = []
    ;
        NonCanon = include_details_cc,
        (
            TypeCtorRep = tcr_enum_usereq,
            BaseTypeCtorRep = tcr_enum
        ;
            TypeCtorRep = tcr_foreign_enum_usereq,
            BaseTypeCtorRep = tcr_foreign_enum
        ;
            TypeCtorRep = tcr_du_usereq,
            BaseTypeCtorRep = tcr_du
        ;
            TypeCtorRep = tcr_notag_usereq,
            BaseTypeCtorRep = tcr_notag
        ;
            TypeCtorRep = tcr_notag_ground_usereq,
            BaseTypeCtorRep = tcr_notag_ground
        ;
            TypeCtorRep = tcr_reserved_addr_usereq,
            BaseTypeCtorRep = tcr_reserved_addr
        ),
        deconstruct_2(Term, TypeInfo, TypeCtorInfo, BaseTypeCtorRep, NonCanon,
            Functor, Ordinal, Arity, Arguments)
    ).

    % MR_expand_type_name from mercury_deconstruct.c
    %
:- func expand_type_name(type_ctor_info, bool) = string.

expand_type_name(TypeCtorInfo, Wrap) = Name :-
    (
        Wrap = yes,
        LeftWrapper = "<<",
        RightWrapper = ">>"
    ;
        Wrap = no,
        LeftWrapper = "",
        RightWrapper = ""
    ),
    Name = string.format("%s%s.%s/%d%s",
        [s(LeftWrapper),
        s(TypeCtorInfo ^ type_ctor_module_name),
        s(TypeCtorInfo ^ type_ctor_name),
        i(TypeCtorInfo ^ type_ctor_arity),
        s(RightWrapper)]).

%---------------------------------------------------------------------------%

    % Retrieve an argument number from a term, given the functor descriptor.
    %
:- some [ArgT] pred get_arg(T::in, type_info::in, sectag_locn::in,
    du_functor_desc::in, int::in, ArgT::out) is det.

get_arg(Term, TypeInfo, SecTagLocn, FunctorDesc, Index, Arg) :-
    ( if get_du_functor_exist_info(FunctorDesc, ExistInfo) then
        NumExtraArgs0 =
            int16.to_int(exist_info_typeinfos_plain(ExistInfo)) +
            int16.to_int(exist_info_tcis(ExistInfo))
    else
        NumExtraArgs0 = 0
    ),

    ArgTypes = FunctorDesc ^ du_functor_arg_types,
    PseudoTypeInfo = get_pti_from_arg_types(ArgTypes, Index),
    get_arg_type_info(Term, TypeInfo, FunctorDesc, PseudoTypeInfo,
        ArgTypeInfo),
    ( if
        ( SecTagLocn = stag_none
        ; SecTagLocn = stag_none_direct_arg
        ; high_level_data
        )
    then
        NumExtraArgs = NumExtraArgs0
    else
        NumExtraArgs = NumExtraArgs0 + 1
    ),
    get_subterm(Term, TypeInfo, FunctorDesc, Index, NumExtraArgs,
        ArgTypeInfo, Arg).

:- func get_arg_univ(T, type_info, sectag_locn, du_functor_desc, int) = univ.

get_arg_univ(Term, TypeInfo, SecTagLocn, FunctorDesc, Index) = Univ :-
    get_arg(Term, TypeInfo, SecTagLocn, FunctorDesc, Index, Arg),
    type_to_univ(Arg, Univ).

%---------------------%

:- pred get_arg_type_info(T::in, type_info::in, du_functor_desc::in,
    pseudo_type_info::in, type_info::out) is det.

get_arg_type_info(Term, TypeInfoParams, FunctorDesc, PseudoTypeInfo,
        ArgTypeInfo) :-
    ( if pseudo_type_info_is_variable(PseudoTypeInfo, VarNum) then
        get_type_info_for_var(TypeInfoParams, VarNum, Term,
            FunctorDesc, ArgTypeInfo)
    else
        CastTypeInfo = type_info_from_pseudo_type_info(PseudoTypeInfo),
        TypeCtorInfo = get_type_ctor_info(CastTypeInfo),
        ( if type_ctor_is_variable_arity(TypeCtorInfo) then
            Arity = type_info_get_higher_order_arity(CastTypeInfo),
            StartRegionSize = 2
        else
            Arity = TypeCtorInfo ^ type_ctor_arity,
            StartRegionSize = 1
        ),
        ArgTypeInfo0 = new_type_info(CastTypeInfo, Arity),
        get_arg_type_info_2(TypeInfoParams, CastTypeInfo, Term, FunctorDesc,
            StartRegionSize, 0, Arity, ArgTypeInfo0, ArgTypeInfo)
    ).

:- pred get_arg_type_info_2(type_info::in, type_info::in, T::in,
    du_functor_desc::in, int::in, int::in, int::in,
    type_info::di, type_info::uo) is det.

get_arg_type_info_2(TypeInfoParams, TypeInfo, Term, FunctorDesc,
        Offset, I, Max, !ArgTypeInfo) :-
    ( if I < Max then
        get_pti_from_type_info_index(TypeInfo, Offset, I, PTI),
        get_arg_type_info(Term, TypeInfoParams, FunctorDesc, PTI, ETypeInfo),
        set_type_info_index(Offset, I, ETypeInfo, !ArgTypeInfo),
        get_arg_type_info_2(TypeInfoParams, TypeInfo, Term, FunctorDesc,
            Offset, I + 1, Max, !ArgTypeInfo)
    else
        true
    ).

%---------------------%

:- func type_info_get_higher_order_arity(type_info) = int.

:- pragma foreign_proc("C#",
    type_info_get_higher_order_arity(PseudoTypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.TypeInfo_Struct ti = (runtime.TypeInfo_Struct) PseudoTypeInfo;
    Arity = ti.args.Length;
").

:- pragma foreign_proc("Java",
    type_info_get_higher_order_arity(PseudoTypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.TypeInfo_Struct ti =
        (jmercury.runtime.TypeInfo_Struct) PseudoTypeInfo;
    Arity = ti.args.length;
").

type_info_get_higher_order_arity(_) = 1 :-
    det_unimplemented("type_info_get_higher_order_arity").

%---------------------%

    % Make a new type-info with the given arity, using the given type_info
    % as the basis.
    %
:- func new_type_info(type_info::in, int::in) = (type_info::uo) is det.

new_type_info(TypeInfo, _) = NewTypeInfo :-
    unsafe_promise_unique(TypeInfo, NewTypeInfo),
    det_unimplemented("new_type_info").

:- pragma foreign_proc("C#",
    new_type_info(OldTypeInfo::in, _Arity::in) = (NewTypeInfo::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NewTypeInfo = OldTypeInfo.copy();
").

:- pragma foreign_proc("Java",
    new_type_info(OldTypeInfo::in, _Arity::in) = (NewTypeInfo::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NewTypeInfo = OldTypeInfo.copy();
").

%---------------------%

    % Get the pseudo-typeinfo at the given index from the argument types.
    %
:- func get_pti_from_arg_types(arg_types, int) = pseudo_type_info.
:- pragma no_determinism_warning(func(get_pti_from_arg_types/2)).

get_pti_from_arg_types(_, _) = _ :-
    sorry($module, "get_pti_from_arg_types").

:- pragma foreign_proc("Java",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgTypeInfo = ArgTypes[Index];
").

:- pragma foreign_proc("C#",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgTypeInfo = ArgTypes[Index];
").

%---------------------%

    % Get the pseudo-typeinfo at the given index from a type-info.
    %
:- pred get_pti_from_type_info_index(type_info::in, int::in, int::in,
    pseudo_type_info::out) is det.
:- pragma no_determinism_warning(pred(get_pti_from_type_info_index/4)).

get_pti_from_type_info_index(_, _, _, _) :-
    private_builtin.sorry("get_pti_from_type_info_index").

:- pragma foreign_proc("C#",
    get_pti_from_type_info_index(TypeInfo::in, _Offset::in, Index::in,
        PTI::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PTI = TypeInfo.args[Index];
").

:- pragma foreign_proc("Java",
    get_pti_from_type_info_index(TypeInfo::in, _Offset::in, Index::in,
        PTI::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PTI = TypeInfo.args[Index];
").

%---------------------%

    % Get the type info for a particular type variable number
    % (it might be in the type_info or in the term itself).
    %
:- pred get_type_info_for_var(type_info::in, int::in, T::in,
    du_functor_desc::in, type_info::out) is det.

get_type_info_for_var(TypeInfo, VarNum, Term, FunctorDesc, ArgTypeInfo) :-
    ( if type_variable_is_univ_quant(VarNum) then
        ArgTypeInfo = type_info_index_as_ti(TypeInfo, VarNum)
    else
        % Existentially qualified.
        ( if get_du_functor_exist_info(FunctorDesc, ExistInfo0) then
            ExistInfo = ExistInfo0
        else
            unexpected($pred, "no exist_info")
        ),

        % We count variables from one so we need to add 1.
        ExistVarNum = VarNum - first_exist_quant_varnum + 1,
        ExistLocn = typeinfo_locns_index(ExistVarNum, ExistInfo),
        Slot = ExistLocn ^ exist_arg_num,
        Offset = int16.to_int(ExistLocn ^ exist_offset_in_tci),

        ( if Offset < 0 then
            ArgTypeInfo = get_type_info_from_term(Term, Slot)
        else
            TypeClassInfo = get_typeclass_info_from_term(Term, Slot),
            ArgTypeInfo = typeclass_info_type_info(TypeClassInfo, Offset)
        )
    ).

%---------------------%

    % An unchecked cast to type_info (for pseudo-typeinfos).
    %
:- func type_info_from_pseudo_type_info(pseudo_type_info) = type_info.

type_info_from_pseudo_type_info(PseudoTypeInfo) = TypeInfo :-
    private_builtin.unsafe_type_cast(PseudoTypeInfo, TypeInfo).

:- pragma foreign_proc("C#",
    type_info_from_pseudo_type_info(PseudoTypeInfo::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    runtime.TypeCtorInfo_Struct tci =
        PseudoTypeInfo as runtime.TypeCtorInfo_Struct;
    if (tci != null) {
        TypeInfo = new runtime.TypeInfo_Struct(tci);
    } else {
        TypeInfo = (runtime.TypeInfo_Struct) PseudoTypeInfo;
    }
").

:- pragma foreign_proc("Java",
    type_info_from_pseudo_type_info(PseudoTypeInfo::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (PseudoTypeInfo instanceof jmercury.runtime.TypeCtorInfo_Struct) {
        TypeInfo = new jmercury.runtime.TypeInfo_Struct(
            (jmercury.runtime.TypeCtorInfo_Struct) PseudoTypeInfo);
    } else {
        TypeInfo = (jmercury.runtime.TypeInfo_Struct) PseudoTypeInfo;
    }
").

%---------------------%

:- some [ArgT] pred get_subterm(T::in, type_info::in, du_functor_desc::in,
    int::in, int::in, type_info::in, ArgT::out) is det.

get_subterm(_, _, _, _, _, _, -1) :-
    det_unimplemented("get_subterm").

:- pragma foreign_proc("C#",
    get_subterm(Term::in, TypeInfo::in, FunctorDesc::in,
        Index::in, NumExtraArgs::in, ArgTypeInfo::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    // Mention TypeInfo_for_U to avoid a warning.

    Arg = ML_get_subterm(Term, TypeInfo, FunctorDesc, Index, NumExtraArgs);
    TypeInfo_for_ArgT = ArgTypeInfo;
").

:- pragma foreign_proc("Java",
    get_subterm(Term::in, TypeInfo::in, FunctorDesc::in,
        Index::in, NumExtraArgs::in, ArgTypeInfo::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    // Mention TypeInfo_for_T to avoid a warning.

    // Currently we use reflection to extract the field.
    // It probably would be more efficient to generate
    // a method for each class to return its n'th field.
    Arg = ML_get_subterm(Term, TypeInfo, FunctorDesc, Index, NumExtraArgs);
    assert Arg != null;

    TypeInfo_for_ArgT = ArgTypeInfo;
").

:- pragma foreign_code("C#",
"
    private static object
    ML_get_subterm(object term,
        TypeInfo_Struct type_info,
        DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        if (term is object[]) {
            int i = index + num_extra_args;
            return ((object[]) term)[i];
        } else {
            return ML_get_subterm_non_array(term, type_info, functor_desc,
                index, num_extra_args);
        }
    }

    private static object
    ML_get_subterm_non_array(object term,
        TypeInfo_Struct type_info,
        DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        TypeCtorInfo_Struct base_tc = type_info.type_ctor.type_ctor_base;
        string field_name;

        if (base_tc != null) {
            // For subtypes, we need to get the corresponding DuFunctorDesc
            // from the base type ctor.
            DuFunctorDesc base_functor_desc =
                ML_get_functor_desc_by_tags(base_tc,
                    functor_desc.du_functor_primary,
                    functor_desc.du_functor_secondary);
            field_name =
                ML_get_field_name_by_index(base_functor_desc,
                    index, num_extra_args);
        } else {
            field_name =
                ML_get_field_name_by_index(functor_desc,
                    index, num_extra_args);
        }

        return ML_get_subterm_by_field_name(term, field_name);
    }

    private static DuFunctorDesc
    ML_get_functor_desc_by_tags(TypeCtorInfo_Struct base_tc,
        byte ptag, int sectag)
    {
        DuPtagLayout ptag_layout = base_tc.index_or_search_ptag_layout(ptag);
        if (sectag == -1) {
            sectag = 0;
        }
        return ptag_layout.index_or_search_sectag_functor(sectag);
    }

    private static string
    ML_get_field_name_by_index(DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        // Look up the field name if it exists, otherwise recreate the field
        // name that would have been used.
        string field_name = null;
        if (functor_desc.du_functor_arg_names != null) {
            field_name = functor_desc.du_functor_arg_names[index];
        }
        if (field_name != null) {
            field_name = ML_name_mangle(field_name);
        } else {
            // The F<i> field variables are numbered from 1.
            int i = 1 + index + num_extra_args;
            field_name = ""F"" + i;
        }
        return field_name;
    }

    private static object
    ML_get_subterm_by_field_name(object term, string field_name)
    {
        System.Reflection.FieldInfo f = term.GetType().GetField(field_name);
        if (f == null) {
            throw new System.Exception(""no such field: "" + field_name);
        }
        return f.GetValue(term);
    }
").

:- pragma foreign_code("Java",
"
    private static Object
    ML_get_subterm(Object term,
        jmercury.runtime.TypeInfo_Struct type_info,
        jmercury.runtime.DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        if (term instanceof Object[]) {
            int i = index + num_extra_args;
            return ((Object[]) term)[i];
        } else {
            return ML_get_subterm_non_array(term, type_info, functor_desc,
                index, num_extra_args);
        }
    }

    private static Object
    ML_get_subterm_non_array(Object term,
        jmercury.runtime.TypeInfo_Struct type_info,
        jmercury.runtime.DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        jmercury.runtime.TypeCtorInfo_Struct base_tc =
            type_info.type_ctor.type_ctor_base;
        String field_name;

        if (base_tc != null) {
            // For subtypes, we need to get the corresponding DuFunctorDesc in
            // the base type ctor.
            jmercury.runtime.DuFunctorDesc base_functor_desc =
                ML_get_functor_desc_by_tags(base_tc,
                    functor_desc.du_functor_primary,
                    functor_desc.du_functor_secondary);
            field_name =
                ML_get_field_name_by_index(base_functor_desc,
                    index, num_extra_args);
        } else {
            field_name =
                ML_get_field_name_by_index(functor_desc,
                    index, num_extra_args);
        }

        return ML_get_subterm_by_field_name(term, field_name);
    }

    private static jmercury.runtime.DuFunctorDesc
    ML_get_functor_desc_by_tags(jmercury.runtime.TypeCtorInfo_Struct base_tc,
        byte ptag, int sectag)
    {
        jmercury.runtime.DuPtagLayout ptag_layout =
            base_tc.index_or_search_ptag_layout(ptag);
        if (sectag == -1) {
            sectag = 0;
        }
        return ptag_layout.index_or_search_sectag_functor(sectag);
    }

    private static String
    ML_get_field_name_by_index(jmercury.runtime.DuFunctorDesc functor_desc,
        int index, int num_extra_args)
    {
        // Look up the field name if it exists, otherwise recreate the field
        // name that would have been used.
        String field_name = null;
        if (functor_desc.du_functor_arg_names != null) {
            field_name = functor_desc.du_functor_arg_names[index];
        }
        if (field_name != null) {
            field_name = ML_name_mangle(field_name);
        } else {
            // The F<i> field variables are numbered from 1.
            int i = 1 + index + num_extra_args;
            field_name = ""F"" + i;
        }
        return field_name;
    }

    private static Object
    ML_get_subterm_by_field_name(Object term, String field_name)
    {
        try {
            Field f = term.getClass().getDeclaredField(field_name);
            return f.get(term);
        } catch (IllegalAccessException e) {
            throw new Error(e);
        } catch (NoSuchFieldException e) {
            throw new Error(e);
        }
    }
").

%---------------------%

:- some [ArgT] pred get_tuple_subterm(T::in, int::in, type_info::in, ArgT::out)
    is det.
:- pragma no_determinism_warning(pred(get_tuple_subterm/4)).

get_tuple_subterm(_, _, _, -1) :-
    private_builtin.sorry("get_tuple_subterm").

:- pragma foreign_proc("C#",
    get_tuple_subterm(Term::in, Index::in, ArgTypeInfo::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Arg = ((object[]) Term)[Index];
    TypeInfo_for_ArgT = ArgTypeInfo;
").

:- pragma foreign_proc("Java",
    get_tuple_subterm(Term::in, Index::in, ArgTypeInfo::in, Arg::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Arg = ((Object[]) Term)[Index];
    TypeInfo_for_ArgT = ArgTypeInfo;
").

%---------------------%

    % Test whether a (pseudo-) type info is variable.
    % The argument type is pseudo_type_info, because when we call this,
    % we have a pseudo_type_info but aren't sure if it is actually
    % a variable or a type_info.
    %
:- pred pseudo_type_info_is_variable(pseudo_type_info::in, int::out)
    is semidet.

pseudo_type_info_is_variable(_, -1) :-
    semidet_unimplemented("pseudo_type_info_is_variable").

:- pragma foreign_proc("C#",
    pseudo_type_info_is_variable(TypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = TypeInfo.variable_number;
    SUCCESS_INDICATOR = (VarNum != -1);
").

:- pragma foreign_proc("Java",
    pseudo_type_info_is_variable(TypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarNum = TypeInfo.variable_number;
    SUCCESS_INDICATOR = (VarNum != -1);

    // Variables number from one, not zero.
    // This is in keeping with mercury_type_info.h.
    assert VarNum != 0;
").

%---------------------%

    % Tests for universal and existentially quantified variables.
    % (The existential version is not used.)
    %
:- pred type_variable_is_univ_quant(int::in) is semidet.
% :- pred type_variable_is_exist_quant(int::in) is semidet.

    % In keeping with mercury_type_info.h.
type_variable_is_univ_quant(X) :- X =< last_univ_quant_varnum.
% type_variable_is_exist_quant(X) :- X >= first_exist_quant_varnum.

:- func last_univ_quant_varnum = int.

last_univ_quant_varnum = 512.

:- func first_exist_quant_varnum = int.

first_exist_quant_varnum = 513.

:- pragma foreign_code("C#", "
public const int last_univ_quant_varnum = 512;
public const int first_exist_quant_varnum = 513;
").

:- pragma foreign_code("Java", "
public static final int last_univ_quant_varnum = 512;
public static final int first_exist_quant_varnum = 513;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred same_pointer_value(T::in, T::in) is semidet.

same_pointer_value(X, Y) :-
    same_pointer_value_untyped(X, Y).

:- pred same_pointer_value_untyped(T::in, U::in) is semidet.

:- pragma foreign_proc("C#",
    same_pointer_value_untyped(T1::in, T2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (T1 == T2);
").

:- pragma foreign_proc("C",
    same_pointer_value_untyped(T1::in, T2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (T1 == T2);
").

:- pragma foreign_proc("Java",
    same_pointer_value_untyped(T1::in, T2::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (T1 == T2);
").

same_pointer_value_untyped(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("same_pointer_value_untyped").

%---------------------------------------------------------------------------%

:- func get_target_lang_rep(T) = string.

:- pragma foreign_proc("C",
    get_target_lang_rep(Term::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // This should be kept in sync with the MR_TYEPCTOR_REP_FOREIGN
    // case in runtime/mercury_ml_expand_body.h.
    char buf[256];
    MR_snprintf(buf, 256, ""%p"", (void *) Term);
    MR_make_aligned_string_copy(S, buf);
").

:- pragma foreign_proc("C#",
    get_target_lang_rep(Term::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Term == null) {
        S = ""null"";
    } else {
        S = Term.ToString();
    }
").

:- pragma foreign_proc("Java",
    get_target_lang_rep(Term::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Term == null) {
        S = ""null"";
    } else {
        S = Term.toString();
    }
").

get_target_lang_rep(_) = "some_foreign_value".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func get_primary_tag(T) = uint8.

:- pragma foreign_proc("C#",
    get_primary_tag(X::in) = (Tag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // We don't look at X to find the tag, for .NET low-level data
    // there is no primary tag, so we always return zero.
    Tag = 0;
").
:- pragma foreign_proc("Java",
    get_primary_tag(_X::in) = (Tag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // For the Java back-end, there is no primary tag, so always return 0.
    Tag = 0;
").

get_primary_tag(_) = 0u8 :-
    det_unimplemented("get_primary_tag").

%---------------------------------------------------------------------------%

:- func get_remote_secondary_tag(T) = int.

:- pragma foreign_proc("C#",
    get_remote_secondary_tag(X::in) = (Tag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Tag = (int) X.GetType().GetField(""data_tag"").GetValue(X);
").
:- pragma foreign_proc("Java",
    get_remote_secondary_tag(X::in) = (Tag::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // If there is a secondary tag, it will be in a member called
    // `data_tag', which we obtain by reflection.

    try {
        Tag = X.getClass().getField(""data_tag"").getInt(X);
    }
    catch (java.lang.Exception e) {
        throw new java.lang.RuntimeException(
            ""get_remote_secondary_tag: `data_tag' not "" +
            ""found"");
    }
").

get_remote_secondary_tag(_) = 0 :-
    det_unimplemented("get_remote_secondary_tag").

%---------------------------------------------------------------------------%

:- type sectag_locn
    --->    stag_none
    ;       stag_none_direct_arg
    ;       stag_local_rest_of_word
    % We now use rtti_implementation.m for the C# and Java grades.
    % Type representations do not use stag_local_bits in those grades.
    % ;     stag_local_bits
    ;       stag_remote
    ;       stag_variable.

:- type du_sectag_alternatives ---> du_sectag_alternatives(c_pointer).
:- pragma foreign_type("C#", du_sectag_alternatives,
    "runtime.DuFunctorDesc[]").
:- pragma foreign_type("Java", du_sectag_alternatives,
    "jmercury.runtime.DuFunctorDesc[]").

    % XXX just use the same names as the C runtime
:- type ptag_entry ---> ptag_entry(c_pointer).
:- pragma foreign_type("C#", ptag_entry, "runtime.DuPtagLayout").
:- pragma foreign_type("Java", ptag_entry, "jmercury.runtime.DuPtagLayout").

:- type arg_types ---> arg_types(c_pointer).
:- pragma foreign_type("C#", arg_types, "runtime.PseudoTypeInfo[]").
:- pragma foreign_type("Java", arg_types, "jmercury.runtime.PseudoTypeInfo[]").

:- type arg_names ---> arg_names(c_pointer).
:- pragma foreign_type("C#", arg_names, "string[]").
:- pragma foreign_type("Java", arg_names, "java.lang.String[]").

:- type exist_info ---> exist_info(c_pointer).
:- pragma foreign_type("C#", exist_info, "runtime.DuExistInfo").
:- pragma foreign_type("Java", exist_info, "jmercury.runtime.DuExistInfo").

:- type typeinfo_locn ---> typeinfo_locn(c_pointer).
:- pragma foreign_type("C#", typeinfo_locn, "runtime.DuExistLocn").
:- pragma foreign_type("Java", typeinfo_locn, "jmercury.runtime.DuExistLocn").

%---------------------%

:- pred det_index_or_search_ptag_layout(type_ctor_info::in, uint8::in,
    ptag_entry::out) is det.

det_index_or_search_ptag_layout(TypeCtorInfo, PTag, PTagEntry) :-
    ( if index_or_search_ptag_layout(TypeCtorInfo, PTag, PTagEntry0) then
        PTagEntry = PTagEntry0
    else
        unexpected($pred, "no functor for ptag")
    ).

:- pred index_or_search_ptag_layout(type_ctor_info::in, uint8::in,
    ptag_entry::out) is semidet.
:- pragma no_determinism_warning(pred(index_or_search_ptag_layout/3)).

index_or_search_ptag_layout(_, _, _) :-
    private_builtin.sorry("index_or_search_ptag_layout").

:- pragma foreign_proc("C#",
    index_or_search_ptag_layout(TypeCtorInfo::in, PTag::in, PTagEntry::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PTagEntry = TypeCtorInfo.index_or_search_ptag_layout(PTag);
    SUCCESS_INDICATOR = (PTagEntry != null);
").

:- pragma foreign_proc("Java",
    index_or_search_ptag_layout(TypeCtorInfo::in, PTag::in, PTagEntry::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PTagEntry = TypeCtorInfo.index_or_search_ptag_layout(PTag);
    SUCCESS_INDICATOR = (PTagEntry != null);
").

%---------------------%

:- func sectag_locn(ptag_entry) = sectag_locn.
:- pragma no_determinism_warning(func(sectag_locn/1)).

sectag_locn(_) = _ :-
    private_builtin.sorry("sectag_locn").

:- pragma foreign_proc("C#",
    sectag_locn(PTagEntry::in) = (SectagLocn::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SectagLocn = (Sectag_locn_0) PTagEntry.sectag_locn;
").

:- pragma foreign_proc("Java",
    sectag_locn(PTagEntry::in) = (SectagLocn::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    jmercury.runtime.Sectag_Locn SL_struct = PTagEntry.sectag_locn;

    SectagLocn = new rtti_implementation.Sectag_locn_0(SL_struct.value);
").

%---------------------%

:- pred index_sectag_functor(ptag_entry::in, int::in, du_functor_desc::out)
    is det.
:- pragma no_determinism_warning(pred(index_sectag_functor/3)).

index_sectag_functor(_, _, _) :-
    private_builtin.sorry("index_sectag_functor").

:- pragma foreign_proc("C#",
    index_sectag_functor(PTagEntry::in, SecTag::in, FunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FunctorDesc = PTagEntry.sectag_alternatives[SecTag];
").

:- pragma foreign_proc("Java",
    index_sectag_functor(PTagEntry::in, SecTag::in, FunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FunctorDesc = PTagEntry.sectag_alternatives[SecTag];
").

%---------------------%

:- pred det_index_or_search_sectag_functor(ptag_entry::in, int::in,
    du_functor_desc::out) is det.

det_index_or_search_sectag_functor(PTagEntry, SecTag, FunctorDesc) :-
    ( if index_or_search_sectag_functor(PTagEntry, SecTag, FunctorDesc0) then
        FunctorDesc = FunctorDesc0
    else
        unexpected($pred, "no functor for sectag")
    ).

:- pred index_or_search_sectag_functor(ptag_entry::in, int::in,
    du_functor_desc::out) is semidet.
:- pragma no_determinism_warning(pred(index_or_search_sectag_functor/3)).

index_or_search_sectag_functor(_, _, _) :-
    private_builtin.sorry("index_or_search_sectag_alternatives").

:- pragma foreign_proc("C#",
    index_or_search_sectag_functor(PTagEntry::in, SecTag::in,
        FunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FunctorDesc = PTagEntry.index_or_search_sectag_functor(SecTag);
    SUCCESS_INDICATOR = (FunctorDesc != null);
").

:- pragma foreign_proc("Java",
    index_or_search_sectag_functor(PTagEntry::in, SecTag::in,
        FunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FunctorDesc = PTagEntry.index_or_search_sectag_functor(SecTag);
    SUCCESS_INDICATOR = (FunctorDesc != null);
").

%---------------------%

:- func typeinfo_locns_index(int, exist_info) = typeinfo_locn.
:- pragma no_determinism_warning(func(typeinfo_locns_index/2)).

typeinfo_locns_index(_, _) = _ :-
    private_builtin.sorry("typeinfo_locns_index").

:- pragma foreign_proc("C#",
    typeinfo_locns_index(VarNum::in, ExistInfo::in) = (TypeInfoLocn::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Variables count from one.
    TypeInfoLocn = ExistInfo.exist_typeinfo_locns[VarNum - 1];
").

:- pragma foreign_proc("Java",
    typeinfo_locns_index(VarNum::in, ExistInfo::in) = (TypeInfoLocn::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Variables count from one.
    TypeInfoLocn = ExistInfo.exist_typeinfo_locns[VarNum - 1];
").

%---------------------%

:- func exist_info_typeinfos_plain(exist_info) = int16.

exist_info_typeinfos_plain(_) = -1i16 :-
    det_unimplemented("exist_info_typeinfos_plain").

:- pragma foreign_proc("C#",
    exist_info_typeinfos_plain(ExistInfo::in) = (TypeInfosPlain::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfosPlain = ExistInfo.exist_typeinfos_plain;
").

:- pragma foreign_proc("Java",
    exist_info_typeinfos_plain(ExistInfo::in) = (TypeInfosPlain::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfosPlain = ExistInfo.exist_typeinfos_plain;
").

%---------------------%

:- func exist_info_tcis(exist_info) = int16.

exist_info_tcis(_) = -1i16 :-
    det_unimplemented("exist_info_tcis").

:- pragma foreign_proc("C#",
    exist_info_tcis(ExistInfo::in) = (TCIs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TCIs = ExistInfo.exist_tcis;
").

:- pragma foreign_proc("Java",
    exist_info_tcis(ExistInfo::in) = (TCIs::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TCIs = ExistInfo.exist_tcis;
").

%---------------------%

:- func exist_arg_num(typeinfo_locn) = int16.

exist_arg_num(_) = -1i16 :-
    det_unimplemented("exist_arg_num").

:- pragma foreign_proc("C#",
    exist_arg_num(TypeInfoLocn::in) = (ArgNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNum = TypeInfoLocn.exist_arg_num;
").

:- pragma foreign_proc("Java",
    exist_arg_num(TypeInfoLocn::in) = (ArgNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNum = TypeInfoLocn.exist_arg_num;
").

%---------------------%

:- func exist_offset_in_tci(typeinfo_locn) = int16.

exist_offset_in_tci(_) = -1i16 :-
    det_unimplemented("exist_offset_in_tci").

:- pragma foreign_proc("C#",
    exist_offset_in_tci(TypeInfoLocn::in) = (ArgNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNum = TypeInfoLocn.exist_offset_in_tci;
").

:- pragma foreign_proc("Java",
    exist_offset_in_tci(TypeInfoLocn::in) = (ArgNum::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNum = TypeInfoLocn.exist_offset_in_tci;
").

%---------------------%

:- func get_type_info_from_term(U, int16) = type_info.
:- pragma no_determinism_warning(func(get_type_info_from_term/2)).

get_type_info_from_term(_, _) = _ :-
    private_builtin.sorry("get_type_info_from_term").

:- pragma foreign_proc("C#",
    get_type_info_from_term(Term::in, Index::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Term is object[]) {
        TypeInfo = (runtime.TypeInfo_Struct) ((object[]) Term)[Index];
    } else {
        // The F<i> field variables are numbered from 1.
        string fieldName = ""F"" + (1 + Index);
        System.Reflection.FieldInfo f = Term.GetType().GetField(fieldName);
        if (f == null) {
            throw new System.Exception(""no such field: "" + fieldName);
        }
        TypeInfo = (runtime.TypeInfo_Struct) f.GetValue(Term);
    }
").

:- pragma foreign_proc("Java",
    get_type_info_from_term(Term::in, Index::in) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    if (Term instanceof Object[]) {
        TypeInfo = (jmercury.runtime.TypeInfo_Struct) ((Object[]) Term)[Index];
    } else {
        try {
            // The F<i> field variables are numbered from 1.
            int i = 1 + Index;
            Field f = Term.getClass().getDeclaredField(""F"" + i);
            TypeInfo = (jmercury.runtime.TypeInfo_Struct) f.get(Term);
        } catch (IllegalAccessException e) {
            throw new Error(e);
        } catch (NoSuchFieldException e) {
            throw new Error(e);
        }
    }
").

%---------------------%

:- func get_typeclass_info_from_term(U, int16) = typeclass_info.
:- pragma no_determinism_warning(func(get_typeclass_info_from_term/2)).

get_typeclass_info_from_term(_, _) = _ :-
    private_builtin.sorry("get_type_info_from_term").

:- pragma foreign_proc("C#",
    get_typeclass_info_from_term(Term::in, Index::in) = (TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    if (Term is object[]) {
        TypeClassInfo = /*typeclass_info*/ (object[]) ((object[]) Term)[Index];
    } else {
        // The F<i> field variables are numbered from 1.
        string fieldName = ""F"" + (1 + Index);
        System.Reflection.FieldInfo f = Term.GetType().GetField(fieldName);
        if (f == null) {
            throw new System.Exception(""no such field: "" + fieldName);
        }
        TypeClassInfo = /*typeclass_info*/ (object[]) f.GetValue(Term);
    }
").

:- pragma foreign_proc("Java",
    get_typeclass_info_from_term(Term::in, Index::in) = (TypeClassInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    if (Term instanceof Object[]) {
        TypeClassInfo = /*typeclass_info*/ (Object[]) ((Object[]) Term)[Index];
    } else {
        try {
            // The F<i> field variables are numbered from 1.
            int i = 1 + Index;
            Field f = Term.getClass().getDeclaredField(""F"" + i);
            TypeClassInfo = /*typeclass_info*/ (Object[]) f.get(Term);
        } catch (IllegalAccessException e) {
            throw new Error(e);
        } catch (NoSuchFieldException e) {
            throw new Error(e);
        }
    }
").

%---------------------%

:- func typeclass_info_type_info(typeclass_info, int) = type_info.

typeclass_info_type_info(TypeClassInfo, Index) = TypeInfo :-
    private_builtin.unsafe_type_cast(TypeClassInfo, PrivateTypeClassInfo),
    private_builtin.type_info_from_typeclass_info(PrivateTypeClassInfo, Index,
        PrivateTypeInfo),
    private_builtin.unsafe_type_cast(PrivateTypeInfo, TypeInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The generic definitions of var_arity_type_info_index_as_ti/pti assume
% that variable arity type_infos store the arity in the first word but
% that is not true for the jmercury.runtime.TypeInfo_Struct in Java.
%
% Keep this in sync with the Java version of type_info_index_as_ti/pti.
%

:- func var_arity_type_info_index_as_ti(type_info, int) = type_info.

:- pragma foreign_proc("C#",
    var_arity_type_info_index_as_ti(TypeInfo::in, VarNum::in)
        = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfoAtIndex = (runtime.TypeInfo_Struct) TypeInfo.args[VarNum - 1];
").
:- pragma foreign_proc("Java",
    var_arity_type_info_index_as_ti(TypeInfo::in, VarNum::in)
        = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    assert TypeInfo.args != null;
    // Variable numbers count from one.
    assert VarNum > 0;

    TypeInfoAtIndex =
        (jmercury.runtime.TypeInfo_Struct) TypeInfo.args[VarNum - 1];
").

var_arity_type_info_index_as_ti(TypeInfo, Index) =
    type_info_index_as_ti(TypeInfo, Index + 1).

%---------------------%

:- func var_arity_type_info_index_as_pti(type_info, int) = pseudo_type_info.

:- pragma foreign_proc("C#",
    var_arity_type_info_index_as_pti(TypeInfo::in, VarNum::in)
        = (PseudoTypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PseudoTypeInfoAtIndex = TypeInfo.args[VarNum - 1];
").
:- pragma foreign_proc("Java",
    var_arity_type_info_index_as_pti(TypeInfo::in, VarNum::in)
        = (PseudoTypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    assert TypeInfo.args != null;
    // Variable numbers count from one.
    assert VarNum > 0;

    PseudoTypeInfoAtIndex = TypeInfo.args[VarNum - 1];
").

var_arity_type_info_index_as_pti(TypeInfo, Index) =
    type_info_index_as_pti(TypeInfo, Index + 1).

%---------------------%

:- func type_info_index_as_ti(type_info, int) = type_info.

:- pragma foreign_proc("C#",
    type_info_index_as_ti(TypeInfo::in, VarNum::in) = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfoAtIndex = (runtime.TypeInfo_Struct) TypeInfo.args[VarNum - 1];
").
:- pragma foreign_proc("Java",
    type_info_index_as_ti(TypeInfo::in, VarNum::in) = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    assert TypeInfo.variable_number == -1;
    assert TypeInfo.args != null;
    // Variable numbers count from one.
    assert VarNum > 0;

    TypeInfoAtIndex =
        (jmercury.runtime.TypeInfo_Struct) TypeInfo.args[VarNum - 1];
").

type_info_index_as_ti(TypeInfo, _) = TypeInfo :-
    % This is an "unimplemented" definition in Mercury, which will be
    % used by default.
    det_unimplemented("type_info_index").

%---------------------%

:- func type_info_index_as_pti(type_info, int) = pseudo_type_info.

:- pragma foreign_proc("C#",
    type_info_index_as_pti(TypeInfo::in, VarNum::in) = (PseudoTypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PseudoTypeInfo = TypeInfo.args[VarNum - 1];
").
:- pragma foreign_proc("Java",
    type_info_index_as_pti(TypeInfo::in, VarNum::in) = (PseudoTypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    assert TypeInfo.variable_number == -1;
    assert TypeInfo.args != null;
    // Variable numbers count from one.
    assert VarNum > 0;

    PseudoTypeInfo = TypeInfo.args[VarNum - 1];
").

type_info_index_as_pti(TypeInfo, _) = PseudoTypeInfo :-
    det_unimplemented("type_info_index_as_pti"),
    private_builtin.unsafe_type_cast(TypeInfo, PseudoTypeInfo).

%---------------------%

:- pred set_type_info_index(int::in, int::in, type_info::in,
    type_info::di, type_info::uo) is det.

:- pragma foreign_proc("C#",
    set_type_info_index(_Offset::in, Index::in, Value::in,
        TypeInfo0::di, TypeInfo::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo0.args[Index] = Value;
    TypeInfo = TypeInfo0;
").

:- pragma foreign_proc("Java",
    set_type_info_index(_Offset::in, Index::in, Value::in,
        TypeInfo0::di, TypeInfo::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo0.args[Index] = Value;
    TypeInfo = TypeInfo0;
").

set_type_info_index(_, _, _, TypeInfo, TypeInfo) :-
    det_unimplemented("set_type_info_index").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func type_ctor_arity(type_ctor_info) = int.
:- pragma foreign_proc("C#",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = TypeCtorInfo.arity;
").
:- pragma foreign_proc("Java",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = TypeCtorInfo.arity;
").
:- pragma foreign_proc("C",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    Arity = tci->MR_type_ctor_arity;
").
type_ctor_arity(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_arity").

%---------------------------------------------------------------------------%

:- func type_ctor_unify_pred(type_ctor_info) = unify_or_compare_pred.
:- pragma foreign_proc("C#",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UnifyPred = TypeCtorInfo.unify_pred;
").
:- pragma foreign_proc("C",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci;

    tci = (MR_TypeCtorInfo) TypeCtorInfo;
    UnifyPred = (MR_Integer) tci->MR_type_ctor_unify_pred;
").
:- pragma foreign_proc("Java",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UnifyPred = TypeCtorInfo.unify_pred;
").
type_ctor_unify_pred(_) = unify_or_compare_pred :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_unify_pred").

%---------------------------------------------------------------------------%

:- func type_ctor_compare_pred(type_ctor_info) = unify_or_compare_pred.

:- pragma foreign_proc("C#",
    type_ctor_compare_pred(TypeCtorInfo::in) = (ComparePred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ComparePred = TypeCtorInfo.compare_pred;
").

:- pragma foreign_proc("C",
    type_ctor_compare_pred(TypeCtorInfo::in) = (ComparePred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci;

    tci = (MR_TypeCtorInfo) TypeCtorInfo;
    ComparePred = (MR_Integer) tci->MR_type_ctor_compare_pred;
").

:- pragma foreign_proc("Java",
    type_ctor_compare_pred(TypeCtorInfo::in) = (ComparePred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ComparePred = TypeCtorInfo.compare_pred;
").

type_ctor_compare_pred(_) = unify_or_compare_pred :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_compare_pred").

%---------------------------------------------------------------------------%

:- func get_type_ctor_rep(type_ctor_info) = type_ctor_rep.

:- pragma foreign_proc("C#",
    get_type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorRep = (Type_ctor_rep_0) TypeCtorInfo.type_ctor_rep;
").

:- pragma foreign_proc("Java",
    get_type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorRep = rtti_implementation.static_type_ctor_rep[
        TypeCtorInfo.type_ctor_rep.value];
").

:- pragma foreign_proc("C",
    get_type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    TypeCtorRep = MR_type_ctor_rep(tci);
").

get_type_ctor_rep(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_rep").

%---------------------------------------------------------------------------%

:- func type_ctor_module_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
    type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = TypeCtorInfo.type_ctor_module_name;
").

:- pragma foreign_proc("Java",
    type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = TypeCtorInfo.type_ctor_module_name;
").

:- pragma foreign_proc("C",
    type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    Name = (MR_String) MR_type_ctor_module_name(tci);
").

type_ctor_module_name(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_module_name").

%---------------------------------------------------------------------------%

:- func type_ctor_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
    type_ctor_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (TypeCtorInfo.type_ctor_rep
            == runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE) {
        Name = ""{}"";
    } else {
        Name = TypeCtorInfo.type_ctor_name;
    }
").
:- pragma foreign_proc("Java",
    type_ctor_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (TypeCtorInfo.type_ctor_rep.value
            == jmercury.runtime.TypeCtorRep.MR_TYPECTOR_REP_TUPLE) {
        Name = ""{}"";
    } else {
        Name = TypeCtorInfo.type_ctor_name;
    }
").
:- pragma foreign_proc("C",
    type_ctor_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    Name = (MR_String) MR_type_ctor_name(tci);
").

type_ctor_name(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_name").

%---------------------------------------------------------------------------%

:- func get_type_ctor_functors(type_ctor_info) = type_functors.
:- pragma no_determinism_warning(func(get_type_ctor_functors/1)).

:- pragma foreign_proc("C#",
    get_type_ctor_functors(TypeCtorInfo::in) = (Functors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Functors = TypeCtorInfo.type_functors;
").

:- pragma foreign_proc("Java",
    get_type_ctor_functors(TypeCtorInfo::in) = (Functors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Functors = TypeCtorInfo.type_functors;
").

get_type_ctor_functors(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("get_type_ctor_functors").

%---------------------------------------------------------------------------%

:- func get_type_layout(type_ctor_info) = type_layout.
:- pragma no_determinism_warning(func(get_type_layout/1)).

:- pragma foreign_proc("C#",
    get_type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeLayout = TypeCtorInfo.type_layout;
").
:- pragma foreign_proc("Java",
    get_type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeLayout = TypeCtorInfo.type_layout;
").
:- pragma foreign_proc("C",
    get_type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    TypeLayout = (MR_Word) &(MR_type_ctor_layout(tci));
").

get_type_layout(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("get_type_layout").

%---------------------------------------------------------------------------%

:- func type_ctor_num_functors(type_ctor_info) = int.
:- pragma no_determinism_warning(func(type_ctor_num_functors/1)).

:- pragma foreign_proc("C#",
    type_ctor_num_functors(TypeCtorInfo::in) = (NumFunctors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NumFunctors = TypeCtorInfo.type_ctor_num_functors;
").

:- pragma foreign_proc("Java",
    type_ctor_num_functors(TypeCtorInfo::in) = (NumFunctors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NumFunctors = TypeCtorInfo.type_ctor_num_functors;
").

type_ctor_num_functors(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_num_functors").

%---------------------------------------------------------------------------%

:- pred type_ctor_search_functor_number_map(type_ctor_info::in,
    int::in, int::out) is semidet.
:- pragma no_determinism_warning(pred(type_ctor_search_functor_number_map/3)).

:- pragma foreign_proc("C#",
    type_ctor_search_functor_number_map(TypeCtorInfo::in, Ordinal::in,
        FunctorNumber::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Ordinal >= 0 && Ordinal < TypeCtorInfo.type_ctor_num_functors) {
        FunctorNumber = TypeCtorInfo.type_functor_number_map[Ordinal];
        SUCCESS_INDICATOR = true;
    } else if (Ordinal == 0 && TypeCtorInfo.type_ctor_num_functors == -1) {
        // This is for tuples.
        FunctorNumber = 0;
        SUCCESS_INDICATOR = true;
    } else {
        FunctorNumber = -1;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    type_ctor_search_functor_number_map(TypeCtorInfo::in, Ordinal::in,
        FunctorNumber::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Ordinal >= 0 && Ordinal < TypeCtorInfo.type_ctor_num_functors) {
        FunctorNumber = TypeCtorInfo.type_functor_number_map[Ordinal];
        SUCCESS_INDICATOR = true;
    } else if (Ordinal == 0 && TypeCtorInfo.type_ctor_num_functors == -1) {
        // This is for tuples.
        FunctorNumber = 0;
        SUCCESS_INDICATOR = true;
    } else {
        FunctorNumber = -1;
        SUCCESS_INDICATOR = false;
    }
").

type_ctor_search_functor_number_map(_, _, _) :-
    private_builtin.sorry("type_ctor_search_functor_number_map/3").

%---------------------------------------------------------------------------%
%
% TypeFunctors
%

:- type type_functors ---> type_functors(c_pointer).
:- pragma foreign_type("C#", type_functors,
    "runtime.TypeFunctors").
:- pragma foreign_type("Java", type_functors,
    "jmercury.runtime.TypeFunctors").

:- type du_functor_desc ---> du_functor_desc(c_pointer).
:- pragma foreign_type("C#", du_functor_desc,
    "runtime.DuFunctorDesc").
:- pragma foreign_type("Java", du_functor_desc,
    "jmercury.runtime.DuFunctorDesc").

:- type enum_functor_desc ---> enum_functor_desc(c_pointer).
:- pragma foreign_type("C#", enum_functor_desc,
    "runtime.EnumFunctorDesc").
:- pragma foreign_type("Java", enum_functor_desc,
    "jmercury.runtime.EnumFunctorDesc").

:- type foreign_enum_functor_desc ---> foreign_enum_functor_desc(c_pointer).
:- pragma foreign_type("C#", foreign_enum_functor_desc,
    "runtime.ForeignEnumFunctorDesc").
:- pragma foreign_type("Java", foreign_enum_functor_desc,
    "jmercury.runtime.ForeignEnumFunctorDesc").

:- type notag_functor_desc ---> notag_functor_desc(c_pointer).
:- pragma foreign_type("C#", notag_functor_desc,
    "runtime.NotagFunctorDesc").
:- pragma foreign_type("Java", notag_functor_desc,
    "jmercury.runtime.NotagFunctorDesc").

:- inst du for type_ctor_rep/0
    --->    tcr_du
    ;       tcr_du_usereq
    ;       tcr_reserved_addr
    ;       tcr_reserved_addr_usereq.

:- inst enum for type_ctor_rep/0
    --->    tcr_enum
    ;       tcr_enum_usereq
    ;       tcr_dummy.

:- inst foreign_enum for type_ctor_rep/0
    --->    tcr_foreign_enum
    ;       tcr_foreign_enum_usereq.

:- inst notag for type_ctor_rep/0
    --->    tcr_notag
    ;       tcr_notag_usereq
    ;       tcr_notag_ground
    ;       tcr_notag_ground_usereq.

%---------------------%

:- func du_functor_desc(type_ctor_rep, int, type_functors) = du_functor_desc.
:- mode du_functor_desc(in(du), in, in) = out is det.

du_functor_desc(_, Num, TypeFunctors) = DuFunctorDesc :-
    DuFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("C#",
    du_functor_desc(_TypeCtorRep::in(du), X::in, TypeFunctors::in) =
        (DuFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DuFunctorDesc = TypeFunctors.functors_du()[X];
").

:- pragma foreign_proc("Java",
    du_functor_desc(_TypeCtorRep::in(du), X::in, TypeFunctors::in) =
        (DuFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DuFunctorDesc = TypeFunctors.functors_du()[X];
").

%---------------------%

:- func du_functor_name(du_functor_desc) = string.

du_functor_name(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("C#",
    du_functor_name(DuFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = DuFunctorDesc.du_functor_name;
").

:- pragma foreign_proc("Java",
    du_functor_name(DuFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = DuFunctorDesc.du_functor_name;
").

%---------------------%

:- func du_functor_arity(du_functor_desc) = int16.

du_functor_arity(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(1).

:- pragma foreign_proc("C#",
    du_functor_arity(DuFunctorDesc::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = DuFunctorDesc.du_functor_orig_arity;
").

:- pragma foreign_proc("Java",
    du_functor_arity(DuFunctorDesc::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = DuFunctorDesc.du_functor_orig_arity;
").

%---------------------%

:- func du_functor_ordinal(du_functor_desc) = int32.

du_functor_ordinal(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(6).

:- pragma foreign_proc("C#",
    du_functor_ordinal(DuFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = DuFunctorDesc.du_functor_ordinal;
").

:- pragma foreign_proc("Java",
    du_functor_ordinal(DuFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = DuFunctorDesc.du_functor_ordinal;
").

%---------------------%

:- func du_functor_arg_types(du_functor_desc) = arg_types.

du_functor_arg_types(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(7).

:- pragma foreign_proc("C#",
    du_functor_arg_types(DuFunctorDesc::in) = (ArgTypes::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgTypes = DuFunctorDesc.du_functor_arg_types;
").

:- pragma foreign_proc("Java",
    du_functor_arg_types(DuFunctorDesc::in) = (ArgTypes::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgTypes = DuFunctorDesc.du_functor_arg_types;
").

%---------------------%

:- pred get_du_functor_arg_names(du_functor_desc::in, arg_names::out)
    is semidet.

get_du_functor_arg_names(DuFunctorDesc, ArgNames) :-
    ArgNames = DuFunctorDesc ^ unsafe_index(8),
    not null(ArgNames).

:- pragma foreign_proc("C#",
    get_du_functor_arg_names(DuFunctorDesc::in, ArgNames::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNames = DuFunctorDesc.du_functor_arg_names;

    SUCCESS_INDICATOR = (ArgNames != null);
").

:- pragma foreign_proc("Java",
    get_du_functor_arg_names(DuFunctorDesc::in, ArgNames::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNames = DuFunctorDesc.du_functor_arg_names;

    SUCCESS_INDICATOR = (ArgNames != null);
").

%---------------------%

:- func arg_names_index(arg_names, int) = string.
:- pragma no_determinism_warning(func(arg_names_index/2)).

:- pragma foreign_proc("C#",
    arg_names_index(ArgNames::in, Index::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ArgNames[Index];
").

:- pragma foreign_proc("Java",
    arg_names_index(ArgNames::in, Index::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ArgNames[Index];
").

arg_names_index(_, _) = _ :-
    private_builtin.sorry("arg_names_index/2").

%---------------------%

:- pred search_arg_names(arg_names::in, int::in, int::in, string::in, int::out)
    is semidet.

search_arg_names(ArgNames, I, Arity, Name, Index) :-
    I < Arity,
    ( if arg_names_index(ArgNames, I) = Name then
        Index = I
    else
        search_arg_names(ArgNames, I + 1, Arity, Name, Index)
    ).

%---------------------%

:- pred get_du_functor_exist_info(du_functor_desc::in, exist_info::out)
    is semidet.

get_du_functor_exist_info(DuFunctorDesc, ExistInfo) :-
    ExistInfo = DuFunctorDesc ^ unsafe_index(9),
    not null(ExistInfo).

:- pragma foreign_proc("C#",
    get_du_functor_exist_info(DuFunctorDesc::in, ExistInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ExistInfo = DuFunctorDesc.du_functor_exist_info;

    SUCCESS_INDICATOR = (ExistInfo != null);
").

:- pragma foreign_proc("Java",
    get_du_functor_exist_info(DuFunctorDesc::in, ExistInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ExistInfo = DuFunctorDesc.du_functor_exist_info;

    SUCCESS_INDICATOR = (ExistInfo != null);
").

%---------------------%

:- func get_enum_functor_desc(type_ctor_rep::in(enum), int::in,
    type_functors::in) = (enum_functor_desc::out) is det.

get_enum_functor_desc(_, Num, TypeFunctors) = EnumFunctorDesc :-
    EnumFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("C#",
    get_enum_functor_desc(_TypeCtorRep::in(enum), X::in, TypeFunctors::in) =
        (EnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    EnumFunctorDesc = (TypeFunctors.functors_enum())[X];
").

:- pragma foreign_proc("Java",
    get_enum_functor_desc(_TypeCtorRep::in(enum), X::in, TypeFunctors::in) =
        (EnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    EnumFunctorDesc = (TypeFunctors.functors_enum())[X];
").

%---------------------%

:- pred index_enum_functor_desc(type_ctor_info::in, int::in,
    enum_functor_desc::out) is det.
:- pragma no_determinism_warning(pred(index_enum_functor_desc/3)).

index_enum_functor_desc(_, _, _) :-
    private_builtin.sorry("index_enum_functor_desc").

:- pragma foreign_proc("C#",
    index_enum_functor_desc(TypeCtorInfo::in, Ordinal::in,
        EnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    EnumFunctorDesc = TypeCtorInfo.type_layout.layout_enum()[Ordinal];
").

:- pragma foreign_proc("Java",
    index_enum_functor_desc(TypeCtorInfo::in, Ordinal::in,
        EnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    EnumFunctorDesc = TypeCtorInfo.type_layout.layout_enum()[Ordinal];
").

%---------------------%

:- pred det_index_or_search_enum_functor_ordinal(type_ctor_info::in, int::in,
    int::out) is det.

det_index_or_search_enum_functor_ordinal(TypeCtorInfo, EnumValue, Ordinal) :-
    ( if
        index_or_search_enum_functor_ordinal(TypeCtorInfo, EnumValue,
            Ordinal0)
    then
        Ordinal = Ordinal0
    else
        unexpected($pred, "no functor for enum value")
    ).

:- pred index_or_search_enum_functor_ordinal(type_ctor_info::in, int::in,
    int::out) is semidet.
:- pragma no_determinism_warning(pred(index_or_search_enum_functor_ordinal/3)).

index_or_search_enum_functor_ordinal(_, _, _) :-
    private_builtin.sorry("index_or_search_enum_functor_ordinal").

:- pragma foreign_proc("C#",
    index_or_search_enum_functor_ordinal(TypeCtorInfo::in, EnumValue::in,
        Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = TypeCtorInfo.index_or_search_enum_functor_ordinal(EnumValue);
    SUCCESS_INDICATOR = (Ordinal >= 0);
").

:- pragma foreign_proc("Java",
    index_or_search_enum_functor_ordinal(TypeCtorInfo::in, EnumValue::in,
        Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = TypeCtorInfo.index_or_search_enum_functor_ordinal(EnumValue);
    SUCCESS_INDICATOR = (Ordinal >= 0);
").

%---------------------%

:- func enum_functor_name(enum_functor_desc) = string.

enum_functor_name(EnumFunctorDesc) = EnumFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("C#",
    enum_functor_name(EnumFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = EnumFunctorDesc.enum_functor_name;
").

:- pragma foreign_proc("Java",
    enum_functor_name(EnumFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = EnumFunctorDesc.enum_functor_name;
").

%---------------------%

:- func enum_functor_value(enum_functor_desc) = int.
:- pragma no_determinism_warning(func(enum_functor_value/1)).

enum_functor_value(_) = _ :-
    private_builtin.sorry("enum_functor_value").

:- pragma foreign_proc("C#",
    enum_functor_value(EnumFunctorDesc::in) = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = EnumFunctorDesc.enum_functor_value;
").

:- pragma foreign_proc("Java",
    enum_functor_value(EnumFunctorDesc::in) = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = EnumFunctorDesc.enum_functor_value;
").

%---------------------%

:- func get_foreign_enum_functor_desc(type_ctor_rep, int, type_functors)
    = foreign_enum_functor_desc.
:- mode get_foreign_enum_functor_desc(in(foreign_enum), in, in) = out is det.
:- pragma no_determinism_warning(func(get_foreign_enum_functor_desc/3)).

get_foreign_enum_functor_desc(_, _, _) = _ :-
    private_builtin.sorry("get_foreign_enum_functor_desc").

:- pragma foreign_proc("C#",
    get_foreign_enum_functor_desc(_TypeCtorRep::in(foreign_enum), X::in,
        TypeFunctors::in) = (ForeignEnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ForeignEnumFunctorDesc = TypeFunctors.functors_foreign_enum()[X];
").

:- pragma foreign_proc("Java",
    get_foreign_enum_functor_desc(_TypeCtorRep::in(foreign_enum), X::in,
        TypeFunctors::in) = (ForeignEnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ForeignEnumFunctorDesc = TypeFunctors.functors_foreign_enum()[X];
").

%---------------------%

:- func foreign_enum_functor_desc(type_ctor_rep, int, type_functors)
    = foreign_enum_functor_desc.
:- mode foreign_enum_functor_desc(in(foreign_enum), in, in) = out is det.
:- pragma no_determinism_warning(func(foreign_enum_functor_desc/3)).

foreign_enum_functor_desc(_, _, _) = _ :-
    private_builtin.sorry("foreign_enum_functor_desc").

:- pragma foreign_proc("C#",
    foreign_enum_functor_desc(_TypeCtorRep::in(foreign_enum), X::in,
        TypeFunctors::in) = (ForeignEnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ForeignEnumFunctorDesc = null;
    foreach (ForeignEnumFunctorDesc desc
        in TypeFunctors.functors_foreign_enum())
    {
        if (desc.foreign_enum_functor_value == X) {
            ForeignEnumFunctorDesc = desc;
            break;
        }
    }
").

:- pragma foreign_proc("Java",
    foreign_enum_functor_desc(_TypeCtorRep::in(foreign_enum), X::in,
        TypeFunctors::in) = (ForeignEnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ForeignEnumFunctorDesc = null;
    for (ForeignEnumFunctorDesc desc : TypeFunctors.functors_foreign_enum()) {
        if (desc.foreign_enum_functor_value == X) {
            ForeignEnumFunctorDesc = desc;
            break;
        }
    }
").

%---------------------%

:- func foreign_enum_functor_name(foreign_enum_functor_desc) = string.

foreign_enum_functor_name(ForeignEnumFunctorDesc) =
    ForeignEnumFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("C#",
    foreign_enum_functor_name(ForeignEnumFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ForeignEnumFunctorDesc.foreign_enum_functor_name;
").

:- pragma foreign_proc("Java",
    foreign_enum_functor_name(ForeignEnumFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ForeignEnumFunctorDesc.foreign_enum_functor_name;
").

%---------------------%

:- func foreign_enum_functor_ordinal(foreign_enum_functor_desc) = int.

foreign_enum_functor_ordinal(_) = -1.

:- pragma foreign_proc("C#",
    foreign_enum_functor_ordinal(ForeignEnumFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = ForeignEnumFunctorDesc.foreign_enum_functor_ordinal;
").

:- pragma foreign_proc("Java",
    foreign_enum_functor_ordinal(ForeignEnumFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = ForeignEnumFunctorDesc.foreign_enum_functor_ordinal;
").

%---------------------%

:- func notag_functor_desc(type_ctor_rep, int, type_functors)
    = notag_functor_desc.

:- mode notag_functor_desc(in(notag), in, in) = out is det.

notag_functor_desc(_, Num, TypeFunctors) = NoTagFunctorDesc :-
    NoTagFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("C#",
    notag_functor_desc(_TypeCtorRep::in(notag), _X::in, TypeFunctors::in) =
        (NotagFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NotagFunctorDesc = TypeFunctors.functors_notag();
").

:- pragma foreign_proc("Java",
    notag_functor_desc(_TypeCtorRep::in(notag), _X::in, TypeFunctors::in) =
        (NotagFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NotagFunctorDesc = TypeFunctors.functors_notag();
").

%---------------------%

:- func notag_functor_name(notag_functor_desc) = string.

notag_functor_name(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("C#",
    notag_functor_name(NotagFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = NotagFunctorDesc.no_tag_functor_name;
").

:- pragma foreign_proc("Java",
    notag_functor_name(NotagFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = NotagFunctorDesc.no_tag_functor_name;
").

%---------------------%

:- func notag_functor_arg_type(notag_functor_desc) = pseudo_type_info.

notag_functor_arg_type(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(1).

:- pragma foreign_proc("C#",
    notag_functor_arg_type(NotagFunctorDesc::in) = (ArgType::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgType = NotagFunctorDesc.no_tag_functor_arg_type;
").

:- pragma foreign_proc("Java",
    notag_functor_arg_type(NotagFunctorDesc::in) = (ArgType::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgType = NotagFunctorDesc.no_tag_functor_arg_type;
").

%---------------------%

:- func notag_functor_arg_name(notag_functor_desc) = string.
:- pragma no_determinism_warning(func(notag_functor_arg_name/1)).

notag_functor_arg_name(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(2).

:- pragma foreign_proc("C#",
    notag_functor_arg_name(NotagFunctorDesc::in) = (ArgName::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgName = NotagFunctorDesc.no_tag_functor_arg_name;
").

:- pragma foreign_proc("Java",
    notag_functor_arg_name(NotagFunctorDesc::in) = (ArgName::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgName = NotagFunctorDesc.no_tag_functor_arg_name;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % XXX get rid of this
:- func unsafe_index(int, T) = U.
:- pragma consider_used(func(unsafe_index/2)).
:- pragma no_determinism_warning(func(unsafe_index/2)).

:- pragma foreign_proc("C#",
    unsafe_index(_Num::in, _Array::in) = (Item::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Item = null;
").

unsafe_index(_, _) = _ :-
    private_builtin.sorry("rtti_implementation.unsafe_index").

%---------------------------------------------------------------------------%

    % XXX get rid of this
:- pred null(T::in) is semidet.
:- pragma no_determinism_warning(pred(null/1)).
:- pragma consider_used(pred(null/1)).

:- pragma foreign_proc("C",
    null(S::in),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    SUCCESS_INDICATOR = ((void *)S == NULL);
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

null(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("rtti_implementation.null/1").

%---------------------------------------------------------------------------%

    % XXX get rid of this
:- func null_string = string.

:- pragma foreign_proc("C",
    null_string = (Str::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Str = NULL;
").
:- pragma foreign_proc("C#",
    null_string = (Str::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Str = null;
").
:- pragma foreign_proc("Java",
    null_string = (Str::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Str = null;
").

null_string = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("rtti_implementation.null_string/0").

%---------------------------------------------------------------------------%

:- func unsafe_get_enum_value(T) = int.
:- pragma no_determinism_warning(func(unsafe_get_enum_value/1)).

:- pragma foreign_proc("C#",
    unsafe_get_enum_value(Enum::in) = (Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = (int) Enum;
").

:- pragma foreign_proc("Java",
    unsafe_get_enum_value(Enum::in) = (Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    try {
        Value = Enum.getClass().getField(""MR_value"").getInt(Enum);
    }
    catch (java.lang.Exception e) {
        throw new java.lang.RuntimeException(
            ""unsafe_get_enum_value/1 called on an "" +
            ""object which is not of enumerated type."");
    }
").

unsafe_get_enum_value(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("rtti_implementation.unsafe_get_enum_value/1").

%---------------------------------------------------------------------------%

:- func unsafe_get_foreign_enum_value(T) = int.
:- pragma no_determinism_warning(func(unsafe_get_foreign_enum_value/1)).

:- pragma foreign_proc("C#",
    unsafe_get_foreign_enum_value(T::in) = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Value = (int) T;
").

unsafe_get_foreign_enum_value(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    % XXX We cannot provide a Java version of this until mlds_to_java_*.m
    % are updated to support foreign enumerations.
    private_builtin.sorry(
        "rtti_implementation.unsafe_get_foreign_enum_value/1").

%---------------------------------------------------------------------------%

:- func iterate(int, int, (func(int) = T)) = list(T).

iterate(Start, Max, Func) = Results :-
    ( if Start =< Max then
        Res = Func(Start),
        Results = [Res | iterate(Start + 1, Max, Func)]
    else
        Results = []
    ).

%---------------------------------------------------------------------------%

:- pred high_level_data is semidet.
:- pragma foreign_proc("Java",
    high_level_data,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("C#",
    high_level_data,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
high_level_data :-
    ( if semidet_succeed then
        private_builtin.sorry("high_level_data")
    else
        semidet_succeed
    ).

%---------------------------------------------------------------------------%

:- pred semidet_unimplemented(string::in) is semidet.
:- pragma consider_used(pred(semidet_unimplemented/1)).

semidet_unimplemented(S) :-
    ( if semidet_succeed then
        sorry($module, S)
    else
        semidet_succeed
    ).

:- pred det_unimplemented(string::in) is det.
:- pragma consider_used(pred(det_unimplemented/1)).

det_unimplemented(S) :-
    ( if semidet_succeed then
        sorry($module, S)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module rtti_implementation.
%---------------------------------------------------------------------------%
