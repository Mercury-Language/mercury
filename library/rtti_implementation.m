%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: rtti_implementation.m.
% Main author: trd, petdr.
% Stability: low.
%
% This file is intended to provide portable RTTI functionality by implementing
% most of Mercury's RTTI functionality in Mercury.
%
% This is simpler writing large amounts of low-level C code, and is much
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
% XXX The Java implementation of this module is incomplete.  Currently, it can
% handle strings, ints, floats and enumerated types, but anything more
% complicated will throw exceptions.  This is mainly due to the fact that a lot
% of the low-level procedures have yet to be implemented for the Java back-end.
%
% XXX Also, the existing Java code needs to be reviewed.
%
%-----------------------------------------------------------------------------%

:- module rtti_implementation.

:- interface.

:- import_module deconstruct.
:- import_module list.

:- use_module std_util.
:- use_module type_desc.

    % Our type_info and type_ctor_info implementations are both
    % abstract types.
:- type type_info.
:- type type_ctor_info.

:- func get_type_info(T::unused) = (type_info::out) is det.

:- pred generic_unify(T::in, T::in) is semidet.

:- pred generic_compare(comparison_result::out, T::in, T::in) is det.

:- pred compare_type_infos(comparison_result::out,
        type_info::in, type_info::in) is det.

:- pred type_ctor_and_args(type_info::in,
        type_ctor_info::out,
        list(type_info)::out) is det.

:- pred type_ctor_name_and_arity(type_ctor_info::in,
        string::out, string::out, int::out) is det.

:- pred deconstruct(T, noncanon_handling, string, int, list(std_util.univ)).
:- mode deconstruct(in, in(do_not_allow), out, out, out) is det.
:- mode deconstruct(in, in(canonicalize), out, out, out) is det.
:- mode deconstruct(in, in(include_details_cc), out, out, out) is cc_multi.
:- mode deconstruct(in, in, out, out, out) is cc_multi.

    % This is useful in a few places, so we'd like to share the code, but
    % it's better to put it into an implementation module such as this one.
    %
:- func unsafe_cast(T1::in) = (T2::out) is det.

%-----------------------------------------------------------------------------%
%
% Implementations for use from construct.

:- func num_functors(type_desc.type_desc) = int.

:- pred get_functor(type_desc.type_desc::in, int::in, string::out, int::out,
    list(type_desc.type_desc)::out) is semidet.

:- pred get_functor_with_names(type_desc.type_desc::in, int::in, string::out,
    int::out, list(type_desc.type_desc)::out, list(string)::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module string.
:- import_module type_desc.

    % Std_util has a lot of types and functions with the same names,
    % so we prefer to keep the namespace separate.
:- use_module std_util.

    % It is convenient to represent the type_ctor_rep as a Mercury
    % enumeration, so
    %
    % The type_ctor_rep needs to be kept up to date with the real
    % definition in runtime/mercury_type_info.h.
:- type type_ctor_rep
    --->    enum
    ;       enum_usereq
    ;       du
    ;       du_usereq
    ;       notag
    ;       notag_usereq
    ;       equiv
    ;       (func)
    ;       int
    ;       char
    ;       float
    ;       string
    ;       (pred)
    ;       subgoal
    ;       void
    ;       c_pointer
    ;       typeinfo
    ;       typeclassinfo
    ;       array
    ;       succip
    ;       hp
    ;       curfr
    ;       maxfr
    ;       redofr
    ;       redoip
    ;       trail_ptr
    ;       ticket
    ;       notag_ground
    ;       notag_ground_usereq
    ;       equiv_ground
    ;       tuple
    ;       reserved_addr
    ;       reserved_addr_usereq
    ;       type_ctor_info
    ;       base_typeclass_info
    ;       type_desc
    ;       type_ctor_desc
    ;       foreign
    ;       reference
    ;       stable_c_pointer
    ;       stable_foreign
    ;       pseudo_type_desc
    ;       dummy
    ;       unknown.

    % We keep all the other types abstract.

:- type type_ctor_info ---> type_ctor_info(c_pointer).
:- pragma foreign_type("Java", type_ctor_info,
    "mercury.runtime.TypeCtorInfo_Struct").

:- type type_info ---> type_info(c_pointer).
:- pragma foreign_type("Java", type_info,
    "mercury.runtime.TypeInfo_Struct").

:- type compare_pred ---> compare_pred(c_pointer).

:- type type_layout ---> type_layout(c_pointer).
:- pragma foreign_type("Java", type_layout, "mercury.runtime.TypeLayout").

:- type pred_type ---> pred_type(c_pointer).
:- type pseudo_type_info ---> pred_type(c_pointer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Implementation of the interface to construct.
%

    % See MR_get_num_functors in runtime/mercury_construct.c
num_functors(TypeDesc) = NumFunctors :-
    TypeCtorInfo = get_type_ctor_info(unsafe_cast(TypeDesc)),
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = du,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = du_usereq,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = reserved_addr,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = reserved_addr_usereq,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = enum,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = enum_usereq,
        NumFunctors = TypeCtorInfo ^ type_ctor_num_functors
    ;
        TypeCtorRep = dummy,
        NumFunctors = 1
    ;
        TypeCtorRep = notag,
        NumFunctors = 1
    ;
        TypeCtorRep = notag_usereq,
        NumFunctors = 1
    ;
        TypeCtorRep = notag_ground,
        NumFunctors = 1
    ;
        TypeCtorRep = notag_ground_usereq,
        NumFunctors = 1
    ;
        TypeCtorRep = tuple,
        NumFunctors = 1
    ;
        TypeCtorRep = subgoal,
        NumFunctors = -1
    ;
        TypeCtorRep = equiv_ground,
        error("rtti_implementation num_functors for equiv types")
    ;
        TypeCtorRep = equiv,
        error("rtti_implementation num_functors for equiv types")
    ;
        TypeCtorRep = int,
        NumFunctors = -1
    ;
        TypeCtorRep = char,
        NumFunctors = -1
    ;
        TypeCtorRep = float,
        NumFunctors = -1
    ;
        TypeCtorRep = string,
        NumFunctors = -1
    ;
        TypeCtorRep = (func),
        NumFunctors = -1
    ;
        TypeCtorRep = (pred),
        NumFunctors = -1
    ;
        TypeCtorRep = void,
        NumFunctors = -1
    ;
        TypeCtorRep = c_pointer,
        NumFunctors = -1
    ;
        TypeCtorRep = stable_c_pointer,
        NumFunctors = -1
    ;
        TypeCtorRep = typeinfo,
        NumFunctors = -1
    ;
        TypeCtorRep = type_ctor_info,
        NumFunctors = -1
    ;
        TypeCtorRep = type_desc,
        NumFunctors = -1
    ;
        TypeCtorRep = pseudo_type_desc,
        NumFunctors = -1
    ;
        TypeCtorRep = type_ctor_desc,
        NumFunctors = -1
    ;
        TypeCtorRep = typeclassinfo,
        NumFunctors = -1
    ;
        TypeCtorRep = base_typeclass_info,
        NumFunctors = -1
    ;
        TypeCtorRep = array,
        NumFunctors = -1
    ;
        TypeCtorRep = succip,
        NumFunctors = -1
    ;
        TypeCtorRep = hp,
        NumFunctors = -1
    ;
        TypeCtorRep = curfr,
        NumFunctors = -1
    ;
        TypeCtorRep = maxfr,
        NumFunctors = -1
    ;
        TypeCtorRep = redofr,
        NumFunctors = -1
    ;
        TypeCtorRep = redoip,
        NumFunctors = -1
    ;
        TypeCtorRep = trail_ptr,
        NumFunctors = -1
    ;
        TypeCtorRep = ticket,
        NumFunctors = -1
    ;
        TypeCtorRep = foreign,
        NumFunctors = -1
    ;
        TypeCtorRep = stable_foreign,
        NumFunctors = -1
    ;
        TypeCtorRep = reference,
        NumFunctors = -1
    ;
        TypeCtorRep = unknown,
        error("num_functors: unknown type_ctor_rep")
    ).

get_functor(TypeDesc, FunctorNumber, FunctorName, Arity, TypeInfoList) :-
    get_functor_impl(TypeDesc, FunctorNumber, FunctorName, Arity,
        TypeInfoList, _Names).

get_functor_with_names(TypeDesc, FunctorNumber, FunctorName, Arity,
        TypeInfoList, Names) :-
    get_functor_impl(TypeDesc, FunctorNumber, FunctorName, Arity,
        TypeInfoList, Names).

:- pred get_functor_impl(type_desc.type_desc::in, int::in,
    string::out, int::out, list(type_desc.type_desc)::out,
    list(string)::out) is semidet.

get_functor_impl(TypeDesc, FunctorNumber,
        FunctorName, Arity, TypeInfoList, Names) :-
    FunctorNumber >= 0,
    FunctorNumber < TypeDesc ^ num_functors,
    TypeInfo = unsafe_cast(TypeDesc),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = du,
        get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = du_usereq,
        get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = reserved_addr,
        get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = reserved_addr_usereq,
        get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = subgoal,
        fail
    ;
        TypeCtorRep = enum,
        get_functor_enum(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = enum_usereq,
        get_functor_enum(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = dummy,
        get_functor_enum(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = notag,
        get_functor_notag(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = notag_usereq,
        get_functor_notag(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = notag_ground,
        get_functor_notag(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = notag_ground_usereq,
        get_functor_notag(TypeCtorRep, TypeCtorInfo,
            FunctorNumber, FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = equiv_ground,
        NewTypeInfo = collapse_equivalences(TypeInfo),
        get_functor_impl(unsafe_cast(NewTypeInfo), FunctorNumber,
            FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = equiv,
        NewTypeInfo = collapse_equivalences(TypeInfo),
        get_functor_impl(unsafe_cast(NewTypeInfo), FunctorNumber,
            FunctorName, Arity, TypeInfoList, Names)
    ;
        TypeCtorRep = tuple,
        FunctorName = "{}",
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        TypeInfoList = iterate(1, Arity, (func(I) =
            unsafe_cast(TypeInfo ^ var_arity_type_info_index(I)))
        ),
        Names = list.duplicate(Arity, null_string)
    ;
        TypeCtorRep = int,
        fail
    ;
        TypeCtorRep = char,
        fail
    ;
        TypeCtorRep = float,
        fail
    ;
        TypeCtorRep = string,
        fail
    ;
        TypeCtorRep = (func),
        fail
    ;
        TypeCtorRep = (pred),
        fail
    ;
        TypeCtorRep = void,
        fail
    ;
        TypeCtorRep = c_pointer,
        fail
    ;
        TypeCtorRep = stable_c_pointer,
        fail
    ;
        TypeCtorRep = typeinfo,
        fail
    ;
        TypeCtorRep = type_ctor_info,
        fail
    ;
        TypeCtorRep = type_desc,
        fail
    ;
        TypeCtorRep = pseudo_type_desc,
        fail
    ;
        TypeCtorRep = type_ctor_desc,
        fail
    ;
        TypeCtorRep = typeclassinfo,
        fail
    ;
        TypeCtorRep = base_typeclass_info,
        fail
    ;
        TypeCtorRep = array,
        fail
    ;
        TypeCtorRep = succip,
        fail
    ;
        TypeCtorRep = hp,
        fail
    ;
        TypeCtorRep = curfr,
        fail
    ;
        TypeCtorRep = maxfr,
        fail
    ;
        TypeCtorRep = redofr,
        fail
    ;
        TypeCtorRep = redoip,
        fail
    ;
        TypeCtorRep = trail_ptr,
        fail
    ;
        TypeCtorRep = ticket,
        fail
    ;
        TypeCtorRep = foreign,
        fail
    ;
        TypeCtorRep = stable_foreign,
        fail
    ;
        TypeCtorRep = reference,
        fail
    ;
        TypeCtorRep = unknown,
        error("get_functor: unknown type_ctor_rep")
    ).

:- pred get_functor_du(type_ctor_rep::in(du), type_info::in,
    type_ctor_info::in, int::in, string::out, int::out,
    list(type_desc.type_desc)::out, list(string)::out) is semidet.

get_functor_du(TypeCtorRep, TypeInfo, TypeCtorInfo, FunctorNumber,
        FunctorName, Arity, TypeDescList, Names) :-
    TypeFunctors = TypeCtorInfo ^ type_ctor_functors,
    DuFunctorDesc = TypeFunctors ^ du_functor_desc(TypeCtorRep, FunctorNumber),

    % XXX We don't handle functors with existentially quantified arguments.
    not (_ = DuFunctorDesc ^ du_functor_exist_info),

    FunctorName = DuFunctorDesc ^ du_functor_name,
    Arity = DuFunctorDesc ^ du_functor_arity,

    ArgTypes = DuFunctorDesc ^ du_functor_arg_types,
    F = (func(I) = ArgTypeDesc :-
        PseudoTypeInfo = get_pti_from_arg_types(ArgTypes, I),
            % XXX we can pass 0 instead of an instance of the functor because
            % that is only needed for functors with existentially quantified
            % arguments.
        get_arg_type_info(TypeInfo, PseudoTypeInfo, 0, DuFunctorDesc,
            ArgTypeInfo),
        ArgTypeDesc = unsafe_cast(ArgTypeInfo)
    ),
    TypeDescList = iterate(0, Arity - 1, F),

    ( ArgNames = DuFunctorDesc ^ du_functor_arg_names ->
        Names = iterate(0, Arity - 1, (func(I) = ArgNames ^ unsafe_index(I)))
    ;
        Names = list.duplicate(Arity, null_string)
    ).

:- pred get_functor_enum(type_ctor_rep::in(enum), type_ctor_info::in, int::in,
    string::out, int::out, list(type_desc.type_desc)::out, list(string)::out)
    is det.

get_functor_enum(TypeCtorRep, TypeCtorInfo, FunctorNumber, FunctorName, Arity,
        TypeDescList, Names) :-
    TypeFunctors = TypeCtorInfo ^ type_ctor_functors,
    EnumFunctorDesc = TypeFunctors ^
        enum_functor_desc(TypeCtorRep, FunctorNumber),

    FunctorName = EnumFunctorDesc ^ enum_functor_name,
    Arity = 0,
    TypeDescList = [],
    Names = [].

:- pred get_functor_notag(type_ctor_rep::in(notag), type_ctor_info::in,
    int::in, string::out, int::out, list(type_desc.type_desc)::out,
    list(string)::out) is det.

get_functor_notag(TypeCtorRep, TypeCtorInfo, FunctorNumber, FunctorName, Arity,
        TypeDescList, Names) :-
    TypeFunctors = TypeCtorInfo ^ type_ctor_functors,
    NoTagFunctorDesc = TypeFunctors ^
        notag_functor_desc(TypeCtorRep, FunctorNumber),

    FunctorName = NoTagFunctorDesc ^ notag_functor_name,
    Arity = 1,

    ArgType = NoTagFunctorDesc ^ notag_functor_arg_type,
    ArgName = NoTagFunctorDesc ^ notag_functor_arg_name,

    TypeDescList = [unsafe_cast(ArgType)],
    Names = [ArgName].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_type_info(_T::unused) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX why is the cast needed here?
    TypeInfo = (mercury.runtime.TypeInfo_Struct) TypeInfo_for_T;
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

:- func get_var_arity_typeinfo_arity(type_info) = int.

:- pragma foreign_proc("Java",
    get_var_arity_typeinfo_arity(TypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = ((TypeInfo_Struct) TypeInfo).args.length;
").

:- pragma foreign_proc("C#",
    get_var_arity_typeinfo_arity(TypeInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = (int) TypeInfo[(int) var_arity_ti.arity];
").

get_var_arity_typeinfo_arity(_) = _ :-
    private_builtin.sorry("get_var_arity_typeinfo_arity").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generic_compare(Res, X, Y) :-
    TypeInfo = get_type_info(X),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = tuple
    ->
        compare_tuple(TypeInfo, Res, X, Y)
    ;
        ( TypeCtorRep = (pred) ; TypeCtorRep = (func) )
    ->
        error("rtti_implementation.m: unimplemented: higher order comparisons")
    ;
        Arity = TypeCtorInfo ^ type_ctor_arity,
        ComparePred = TypeCtorInfo ^ type_ctor_compare_pred,
        ( Arity = 0 ->
            result_call_4(ComparePred, Res, X, Y)
        ; Arity = 1 ->
            result_call_5(ComparePred, Res,
                TypeInfo ^ type_info_index(1), X, Y)
        ; Arity = 2 ->
            result_call_6(ComparePred, Res,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                X, Y)
        ; Arity = 3 ->
            result_call_7(ComparePred, Res,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                X, Y)
        ; Arity = 4 ->
            result_call_8(ComparePred, Res,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                TypeInfo ^ type_info_index(4),
                X, Y)
        ; Arity = 5 ->
            result_call_9(ComparePred, Res,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                TypeInfo ^ type_info_index(4),
                TypeInfo ^ type_info_index(5),
                X, Y)
        ;
            error("compare/3: type arity > 5 not supported")
        )
    ).

generic_unify(X, Y) :-
    TypeInfo = get_type_info(X),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = tuple
    ->
        unify_tuple(TypeInfo, X, Y)
    ;
        ( TypeCtorRep = (pred) ; TypeCtorRep = (func) )
    ->
        error("rtti_implementation.m: unimplemented: higher order unification")
    ;
        Arity = TypeCtorInfo ^ type_ctor_arity,
        UnifyPred = TypeCtorInfo ^ type_ctor_unify_pred,
        ( Arity = 0 ->
            semidet_call_3(UnifyPred, X, Y)
        ; Arity = 1 ->
            semidet_call_4(UnifyPred,
                TypeInfo ^ type_info_index(1), X, Y)
        ; Arity = 2 ->
            semidet_call_5(UnifyPred,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                X, Y)
        ; Arity = 3 ->
            semidet_call_6(UnifyPred,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                X, Y)
        ; Arity = 4 ->
            semidet_call_7(UnifyPred,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                TypeInfo ^ type_info_index(4),
                X, Y)
        ; Arity = 5 ->
            semidet_call_8(UnifyPred,
                TypeInfo ^ type_info_index(1),
                TypeInfo ^ type_info_index(2),
                TypeInfo ^ type_info_index(3),
                TypeInfo ^ type_info_index(4),
                TypeInfo ^ type_info_index(5),
                X, Y)
        ;
            error("unify/2: type arity > 5 not supported")
        )
    ).

:- pred unify_tuple(type_info::in, T::in, T::in) is semidet.

unify_tuple(TypeInfo, TermA, TermB) :-
    Arity = get_var_arity_typeinfo_arity(TypeInfo),
    unify_tuple_pos(1, Arity, TypeInfo, TermA, TermB).

:- pred unify_tuple_pos(int::in, int::in,
        type_info::in, T::in, T::in) is semidet.

unify_tuple_pos(Loc, TupleArity, TypeInfo, TermA, TermB) :-
    ( Loc > TupleArity ->
        true
    ;
        ArgTypeInfo = TypeInfo ^ var_arity_type_info_index(Loc),

        SubTermA = get_subterm(ArgTypeInfo, TermA, Loc - 1, 0),
        SubTermB = get_subterm(ArgTypeInfo, TermB, Loc - 1, 0),

        generic_unify(SubTermA, unsafe_cast(SubTermB)),

        unify_tuple_pos(Loc + 1, TupleArity, TypeInfo, TermA, TermB)
    ).

:- pred compare_tuple(type_info::in, comparison_result::out, T::in, T::in)
    is det.

compare_tuple(TypeInfo, Result, TermA, TermB) :-
    Arity = get_var_arity_typeinfo_arity(TypeInfo),
    compare_tuple_pos(1, Arity, TypeInfo, Result, TermA, TermB).

:- pred compare_tuple_pos(int::in, int::in, type_info::in,
    comparison_result::out, T::in, T::in) is det.

compare_tuple_pos(Loc, TupleArity, TypeInfo, Result, TermA, TermB) :-
    ( Loc > TupleArity ->
        Result = (=)
    ;
        ArgTypeInfo = TypeInfo ^ var_arity_type_info_index(Loc),

        SubTermA = get_subterm(ArgTypeInfo, TermA, Loc - 1, 0),
        SubTermB = get_subterm(ArgTypeInfo, TermB, Loc - 1, 0),

        generic_compare(SubResult, SubTermA, unsafe_cast(SubTermB)),
        ( SubResult = (=) ->
            compare_tuple_pos(Loc + 1, TupleArity, TypeInfo, Result,
                TermA, TermB)
        ;
            Result = SubResult
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Implement generic calls -- we could use call/N but then we would
    % have to create a real closure.
    %
    % We first give "unimplemented" definitions in Mercury, which will be
    % used by default.

:- pred semidet_call_3(P::in, T::in, U::in) is semidet.
semidet_call_3(_::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_3").

:- pred semidet_call_4(P::in, A::in, T::in, U::in) is semidet.
semidet_call_4(_::in, _::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_4").

:- pred semidet_call_5(P::in, A::in, B::in, T::in, U::in) is semidet.
semidet_call_5(_::in, _::in, _::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_5").

:- pred semidet_call_6(P::in, A::in, B::in, C::in, T::in, U::in) is semidet.
semidet_call_6(_::in, _::in, _::in, _::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_6").

:- pred semidet_call_7(P::in, A::in, B::in, C::in, D::in, T::in, U::in)
    is semidet.
semidet_call_7(_::in, _::in, _::in, _::in, _::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_7").

:- pred semidet_call_8(P::in, A::in, B::in, C::in, D::in, E::in, T::in, U::in)
    is semidet.
semidet_call_8(_::in, _::in, _::in, _::in, _::in, _::in, _::in, _::in) :-
    semidet_unimplemented("semidet_call_8").

:- pred result_call_4(P::in, comparison_result::out,
    T::in, U::in) is det.
result_call_4(_::in, (=)::out, _::in, _::in) :-
    det_unimplemented("result_call_4").

:- pred result_call_5(P::in, comparison_result::out,
    A::in, T::in, U::in) is det.
result_call_5(_::in, (=)::out, _::in, _::in, _::in) :-
    det_unimplemented("comparison_result").

:- pred result_call_6(P::in, comparison_result::out,
    A::in, B::in, T::in, U::in) is det.
result_call_6(_::in, (=)::out, _::in, _::in, _::in, _::in) :-
    det_unimplemented("comparison_result").

:- pred result_call_7(P::in, comparison_result::out,
    A::in, B::in, C::in, T::in, U::in) is det.
result_call_7(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in) :-
    det_unimplemented("comparison_result").

:- pred result_call_8(P::in, comparison_result::out,
    A::in, B::in, C::in, D::in, T::in, U::in) is det.
result_call_8(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in, _::in) :-
    det_unimplemented("comparison_result").

:- pred result_call_9(P::in, comparison_result::out,
    A::in, B::in, C::in, D::in, E::in, T::in, U::in) is det.
result_call_9(_::in, (=)::out, _::in, _::in, _::in, _::in, _::in,
        _::in, _::in) :-
    det_unimplemented("result_call_9").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % We override the above definitions in the .NET backend.

:- pragma foreign_proc("C#",
    semidet_call_3(Pred::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_3(Pred, X, Y);
").
:- pragma foreign_proc("C#",
    semidet_call_4(Pred::in, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_4(Pred, A, X, Y);
").
:- pragma foreign_proc("C#",
    semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_5(Pred, A, B, X, Y);
").
:- pragma foreign_proc("C#",
    semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_6(Pred, A, B, C, X, Y);
").
:- pragma foreign_proc("C#",
    semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_7(Pred, A, B, C, D, X, Y);
").
:- pragma foreign_proc("C#",
    semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        mercury.runtime.GenericCall.semidet_call_8(Pred, A, B, C, D, E, X, Y);
").

:- pragma foreign_proc("C#",
    result_call_4(Pred::in, Res::out, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_4(Pred, ref Res, X, Y);
").

:- pragma foreign_proc("C#",
    result_call_5(Pred::in, Res::out, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_5(Pred, A, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
    result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_6(Pred, A, B, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
    result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_7(Pred, A, B, C, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
    result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_8(Pred, A, B, C, D, ref Res, X, Y);
").
:- pragma foreign_proc("C#",
    result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    mercury.runtime.GenericCall.result_call_9(Pred, A, B, C, D, E, ref Res,
        X, Y);
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

compare_type_infos(Res, TypeInfo1, TypeInfo2) :-
    ( same_pointer_value(TypeInfo1, TypeInfo2) ->
        Res = (=)
    ;
        NewTypeInfo1 = collapse_equivalences(TypeInfo1),
        NewTypeInfo2 = collapse_equivalences(TypeInfo2),
        ( same_pointer_value(NewTypeInfo1, NewTypeInfo2) ->
            Res = (=)
        ;
            compare_collapsed_type_infos(Res, TypeInfo1, TypeInfo2)
        )
    ).

:- pred compare_collapsed_type_infos(comparison_result::out,
    type_info::in, type_info::in) is det.

compare_collapsed_type_infos(Res, TypeInfo1, TypeInfo2) :-
    TypeCtorInfo1 = get_type_ctor_info(TypeInfo1),
    TypeCtorInfo2 = get_type_ctor_info(TypeInfo2),

    % The comparison here is arbitrary. In the past we just compared pointers
    % to the type_ctor_infos.
    compare(NameRes, TypeCtorInfo1 ^ type_ctor_name,
        TypeCtorInfo2 ^ type_ctor_name),
    ( NameRes = (=) ->
        compare(ModNameRes,
            TypeCtorInfo1 ^ type_ctor_module_name,
            TypeCtorInfo2 ^ type_ctor_module_name),
        (
            ModNameRes = (=),
            type_ctor_is_variable_arity(TypeCtorInfo1)
        ->
            Arity1 = get_var_arity_typeinfo_arity(TypeInfo1),
            Arity2 = get_var_arity_typeinfo_arity(TypeInfo2),
            compare(ArityRes, Arity1, Arity2),
            ( ArityRes = (=) ->
                compare_var_arity_typeinfos(1, Arity1, Res,
                    TypeInfo1, TypeInfo2)
            ;
                Res = ArityRes
            )
        ;
            Res = ModNameRes
        )
    ;
        Res = NameRes
    ).

:- pred compare_var_arity_typeinfos(int::in, int::in,
    comparison_result::out, type_info::in, type_info::in) is det.

compare_var_arity_typeinfos(Loc, Arity, Result, TypeInfoA, TypeInfoB) :-
    ( Loc > Arity ->
        Result = (=)
    ;
        SubTypeInfoA = TypeInfoA ^ var_arity_type_info_index(Loc),
        SubTypeInfoB = TypeInfoB ^ var_arity_type_info_index(Loc),

        compare_collapsed_type_infos(SubResult, SubTypeInfoA, SubTypeInfoB),
        ( SubResult = (=) ->
            compare_var_arity_typeinfos(Loc + 1, Arity, Result,
                TypeInfoA, TypeInfoB)
        ;
            Result = SubResult
        )
    ).

:- pred type_ctor_is_variable_arity(type_ctor_info::in) is semidet.

type_ctor_is_variable_arity(TypeCtorInfo) :-
    ( TypeCtorInfo ^ type_ctor_rep = (pred)
    ; TypeCtorInfo ^ type_ctor_rep = (func)
    ; TypeCtorInfo ^ type_ctor_rep = tuple
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % In the .NET backend, we don't generally have to collapse equivalences
    % because they are already collapsed (il grades require
    % intermodule optimization, which will collapse them for us).
    %
    % XXX For other backends this code may have to be completed.
    %
:- func collapse_equivalences(type_info) = type_info.

collapse_equivalences(TypeInfo) = NewTypeInfo :-
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    (
        (
            TypeCtorInfo ^ type_ctor_rep = equiv_ground
        ;
            TypeCtorInfo ^ type_ctor_rep = equiv
        )
    ->
        error("rtti_implementation.m: unimplemented: " ++
            "collapsing equivalence types")
    ;
        NewTypeInfo = TypeInfo
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

type_ctor_name_and_arity(TypeCtorInfo, ModuleName, Name, Arity) :-
    ModuleName = type_ctor_module_name(TypeCtorInfo),
    Name = type_ctor_name(TypeCtorInfo),
    Arity = type_ctor_arity(TypeCtorInfo).

type_ctor_and_args(TypeInfo0, TypeCtorInfo, TypeArgs) :-
    TypeInfo = collapse_equivalences(TypeInfo0),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    (
        type_ctor_is_variable_arity(TypeCtorInfo)
    ->
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        % XXX Do indexes start at 0?
        TypeArgs = iterate(0, Arity - 1,
            (func(X) = TypeInfo ^ var_arity_type_info_index(X))
        )
    ;
        Arity = type_ctor_arity(TypeCtorInfo),
        TypeArgs = iterate(0, Arity - 1,
            (func(X) = TypeInfo ^ type_info_index(X))
        )
    ).

:- func iterate(int, int, (func(int) = T)) = list(T).

iterate(Start, Max, Func) = Results :-
    ( Start =< Max ->
        Res = Func(Start),
        Results = [Res | iterate(Start + 1, Max, Func)]
    ;
        Results = []
    ).

:- pred iterate_foldl(int::in, int::in,
    pred(int, T, T)::in(pred(in, in, out) is det), T::in, T::out) is det.

iterate_foldl(Start, Max, Pred, !Acc) :-
    ( Start =< Max ->
        Pred(Start, !Acc),
        iterate_foldl(Start + 1, Max, Pred, !Acc)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

deconstruct(Term, NonCanon, Functor, Arity, Arguments) :-
    TypeInfo = get_type_info(Term),
    TypeCtorInfo = get_type_ctor_info(TypeInfo),
    TypeCtorRep = type_ctor_rep(TypeCtorInfo),
    deconstruct(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
        Functor, Arity, Arguments).

:- pred deconstruct(T, type_info, type_ctor_info, type_ctor_rep,
    noncanon_handling, string, int, list(std_util.univ)).
:- mode deconstruct(in, in, in, in, in(do_not_allow), out, out, out) is det.
:- mode deconstruct(in, in, in, in, in(canonicalize), out, out, out) is det.
:- mode deconstruct(in, in, in, in,
    in(include_details_cc), out, out, out) is cc_multi.
:- mode deconstruct(in, in, in, in, in, out, out, out) is cc_multi.

    % Code to perform deconstructions (XXX not yet complete).
    %
    % There are many cases to implement here, only the ones that were
    % immediately useful (e.g. called by io.write) have been implemented
    % so far.

deconstruct(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
        Functor, Arity, Arguments) :-
    (
        TypeCtorRep = enum_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            NonCanon, Functor, Arity, Arguments)
    ;
        TypeCtorRep = enum,
        TypeFunctors = type_ctor_functors(TypeCtorInfo),
        EnumFunctorDesc = enum_functor_desc(TypeCtorRep,
            unsafe_get_enum_value(Term), TypeFunctors),
        Functor = enum_functor_name(EnumFunctorDesc),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = dummy,
        TypeFunctors = type_ctor_functors(TypeCtorInfo),
        EnumFunctorDesc = enum_functor_desc(TypeCtorRep, 0, TypeFunctors),
        Functor = enum_functor_name(EnumFunctorDesc),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = du_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep,
            NonCanon, Functor, Arity, Arguments)
    ;
        TypeCtorRep = du,

        LayoutInfo = type_layout(TypeCtorInfo),
        PTag = get_primary_tag(Term),
        PTagEntry = LayoutInfo ^ ptag_index(PTag),
        SecTagLocn = PTagEntry ^ sectag_locn,
        (
            SecTagLocn = none,
            FunctorDesc = PTagEntry ^ du_sectag_alternatives(0),
            Functor = FunctorDesc ^ du_functor_name,
            Arity = FunctorDesc ^ du_functor_arity,
            Arguments = iterate(0, Arity - 1,
                (func(X) = std_util.univ(
                    get_arg(Term, X, SecTagLocn, FunctorDesc, TypeInfo))
                ))
        ;
            SecTagLocn = local,
            Functor = "some_du_local_sectag",
            Arity = 0,
            Arguments = []
        ;
            SecTagLocn = remote,
            SecTag = get_remote_secondary_tag(Term),
            FunctorDesc = PTagEntry ^ du_sectag_alternatives(SecTag),
            Functor = FunctorDesc ^ du_functor_name,
            Arity = FunctorDesc ^ du_functor_arity,
            Arguments = iterate(0, Arity - 1,
                (func(X) = std_util.univ(
                    get_arg(Term, X, SecTagLocn, FunctorDesc, TypeInfo))
                ))
        ;
            SecTagLocn = variable,
            Functor = "some_du_variable_sectag",
            Arity = 0,
            Arguments = []
        )
    ;
        TypeCtorRep = notag_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Arity, Arguments)
    ;
        TypeCtorRep = notag,
        Functor = "some_notag",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = notag_ground_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Arity, Arguments)
    ;
        TypeCtorRep = notag_ground,
        Functor = "some_notag_ground",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = equiv_ground,
        Functor = "some_equiv_ground",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = (func),
        Functor = "<<function>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = equiv,
        Functor = "some_equiv",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = int,
        det_dynamic_cast(Term, Int),
        Functor = string.int_to_string(Int),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = char,
        det_dynamic_cast(Term, Char),

        % XXX should escape characters correctly
        Functor = "'" ++ char_to_string(Char) ++ "'",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = float,
        det_dynamic_cast(Term, Float),
        Functor = float_to_string(Float),
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = string,
        det_dynamic_cast(Term, String),

        % XXX should escape characters in the string correctly
        Functor = "\"" ++ String ++ "\"",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = (pred),
        Functor = "<<predicate>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = tuple,
        type_ctor_and_args(TypeInfo, _TypeCtorInfo, TypeArgs),
        Functor = "{}",
        Arity = get_var_arity_typeinfo_arity(TypeInfo),
        list.map_foldl(
            (pred(TI::in, U::out, Index::in, Next::out) is det :-
                SubTerm = get_subterm(TI, Term, Index, 0),
                U = std_util.univ(SubTerm),
                Next = Index + 1
            ), TypeArgs, Arguments, 0, _)
    ;
        % XXX noncanonical term
        TypeCtorRep = subgoal,
        Functor = "<<subgoal>>",
        Arity = 0,
        Arguments = []
    ;
        % There is no way to create values of type `void', so this
        % should never happen.
        TypeCtorRep = void,
        error("rtti_implementation.m: cannot deconstruct void types")
    ;
        % XXX noncanonical term
        TypeCtorRep = c_pointer,
        Functor = "<<c_pointer>>",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = stable_c_pointer,
        Functor = "<<stable_c_pointer>>",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = typeinfo,
        Functor = "some_typeinfo",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = typeclassinfo,
        Functor = "<<typeclassinfo>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = array,

        % Constrain the T in array(T) to the correct element type.
        type_ctor_and_args(type_of(Term), _, Args),
        ( Args = [ElemType] ->
            has_type(Elem, ElemType),
            same_array_elem_type(Array, Elem)
        ;
            error("An array which doesn't have a type_ctor arg")
        ),

        det_dynamic_cast(Term, Array),

        Functor = "<<array>>",
        Arity = array.size(Array),
        Arguments = array.foldr(
            (func(Elem, List) = [std_util.univ(Elem) | List]),
            Array, [])
    ;
        TypeCtorRep = succip,
        Functor = "<<succip>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = hp,
        Functor = "<<hp>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = curfr,
        Functor = "<<curfr>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = maxfr,
        Functor = "<<maxfr>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = redofr,
        Functor = "<<redofr>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = redoip,
        Functor = "<<redoip>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = trail_ptr,
        Functor = "<<trail_ptr>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = ticket,
        Functor = "<<ticket>>",
        Arity = 0,
        Arguments = []
    ;
        % XXX FIXME!!!
        TypeCtorRep = reserved_addr,
        Functor = "some_reserved_addr",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = reserved_addr_usereq,
        handle_usereq_type(Term, TypeInfo, TypeCtorInfo, TypeCtorRep, NonCanon,
            Functor, Arity, Arguments)
    ;
        % XXX noncanonical term
        TypeCtorRep = type_ctor_info,
        Functor = "some_typectorinfo",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = base_typeclass_info,
        Functor = "<<basetypeclassinfo>>",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = type_desc,
        Functor = "some_type_desc",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = pseudo_type_desc,
        Functor = "some_pseudo_type_desc",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = type_ctor_desc,
        Functor = "some_type_ctor_desc",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = foreign,
        Functor = "<<foreign>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = stable_foreign,
        Functor = "<<stable_foreign>>",
        Arity = 0,
        Arguments = []
    ;
        % XXX noncanonical term
        TypeCtorRep = reference,
        Functor = "<<reference>>",
        Arity = 0,
        Arguments = []
    ;
        TypeCtorRep = unknown,
        error("rtti_implementation: unknown type_ctor rep in deconstruct")
    ).

:- pred det_dynamic_cast(T::in, U::out) is det.

det_dynamic_cast(Term, Actual) :-
    std_util.type_to_univ(Term, Univ),
    std_util.det_univ_to_type(Univ, Actual).

:- pred same_array_elem_type(array(T)::unused, T::unused) is det.

same_array_elem_type(_, _).

:- inst usereq == bound(enum_usereq; du_usereq; notag_usereq;
    notag_ground_usereq; reserved_addr_usereq).

:- pred handle_usereq_type(T, type_info, type_ctor_info, type_ctor_rep,
        noncanon_handling, string, int, list(std_util.univ)).

:- mode handle_usereq_type(in, in, in, in(usereq),
    in(do_not_allow), out, out, out) is erroneous.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in(canonicalize), out, out, out) is det.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in(include_details_cc), out, out, out) is cc_multi.
:- mode handle_usereq_type(in, in, in, in(usereq),
    in, out, out, out) is cc_multi.

handle_usereq_type(Term, TypeInfo, TypeCtorInfo,
        TypeCtorRep, NonCanon, Functor, Arity, Arguments) :-
    (
        NonCanon = do_not_allow,
        error("attempt to deconstruct noncanonical term")
    ;
        NonCanon = canonicalize,
        Functor = expand_type_name(TypeCtorInfo, yes),
        Arity = 0,
        Arguments = []
    ;
        NonCanon = include_details_cc,
        (
            TypeCtorRep = enum_usereq,
            BaseTypeCtorRep = enum
        ;
            TypeCtorRep = du_usereq,
            BaseTypeCtorRep = du
        ;
            TypeCtorRep = notag_usereq,
            BaseTypeCtorRep = notag
        ;
            TypeCtorRep = notag_ground_usereq,
            BaseTypeCtorRep = notag_ground
        ;
            TypeCtorRep = reserved_addr_usereq,
            BaseTypeCtorRep = reserved_addr
        ),
        deconstruct(Term, TypeInfo, TypeCtorInfo, BaseTypeCtorRep, NonCanon,
            Functor, Arity, Arguments)
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

    % Retrieve an argument number from a term, given the functor descriptor.
    %
:- some [T] func get_arg(U, int, sectag_locn, du_functor_desc, type_info) = T.

get_arg(Term, Index, SecTagLocn, FunctorDesc, TypeInfo) = (Arg) :-
    ( ExistInfo = FunctorDesc ^ du_functor_exist_info ->
        ExtraArgs = (ExistInfo ^ exist_info_typeinfos_plain) +
            (ExistInfo ^ exist_info_tcis)
    ;
        ExtraArgs = 0
    ),

    ArgTypes = FunctorDesc ^ du_functor_arg_types,
    PseudoTypeInfo = get_pti_from_arg_types(ArgTypes, Index),
    get_arg_type_info(TypeInfo, PseudoTypeInfo, Term, FunctorDesc,
        ArgTypeInfo),
    ( ( SecTagLocn = none ; high_level_data ) ->
        TagOffset = 0
    ;
        TagOffset = 1
    ),
    RealArgsOffset = TagOffset + ExtraArgs,
    Arg = get_subterm(ArgTypeInfo, Term, Index, RealArgsOffset).

:- pred high_level_data is semidet.
:- pragma promise_pure(high_level_data/0).
:- pragma foreign_proc("Java",
    high_level_data,
    [will_not_call_mercury, thread_safe],
"
    succeeded = true;
").
:- pragma foreign_proc("C#",
    high_level_data,
    [will_not_call_mercury, thread_safe],
"
#if MR_HIGHLEVEL_DATA
    SUCCESS_INDICATOR = true;
#else
    SUCCESS_INDICATOR = false;
#endif
").
high_level_data :-
    ( std_util.semidet_succeed ->
        private_builtin.sorry("high_level_data")
    ;
        std_util.semidet_succeed
    ).

:- pred get_arg_type_info(type_info::in, P::in, T::in,
    du_functor_desc::in, type_info::out) is det.

get_arg_type_info(TypeInfoParams, PseudoTypeInfo, Term, FunctorDesc,
        ArgTypeInfo) :-
    (
        typeinfo_is_variable(PseudoTypeInfo, VarNum)
    ->
        get_type_info_for_var(TypeInfoParams, VarNum, Term, FunctorDesc,
            ExpandedTypeInfo),
        ( typeinfo_is_variable(ExpandedTypeInfo, _) ->
            error("get_arg_type_info: unbound type variable")
        ;
            ArgTypeInfo = ExpandedTypeInfo
        )
    ;
        CastTypeInfo = type_info_cast(PseudoTypeInfo),
        TypeCtorInfo = get_type_ctor_info(CastTypeInfo),
        (
            type_ctor_is_variable_arity(TypeCtorInfo)
        ->
            Arity = pseudotypeinfo_get_higher_order_arity(CastTypeInfo),
            StartRegionSize = 2
        ;
            Arity = TypeCtorInfo ^ type_ctor_arity,
            StartRegionSize = 1
        ),
        ArgTypeInfo0 = std_util.no,
        UpperBound = Arity + StartRegionSize - 1,

        iterate_foldl(StartRegionSize, UpperBound,
            (pred(I::in, TI0::in, TI::out) is det :-
                PTI = get_pti_from_type_info(CastTypeInfo, I),
                get_arg_type_info(TypeInfoParams, PTI, Term, FunctorDesc,
                    ETypeInfo),
                (
                    same_pointer_value_untyped(ETypeInfo, PTI)
                ->
                    TI = TI0
                ;
                    TI0 = std_util.yes(TypeInfo0)
                ->
                    unsafe_promise_unique(TypeInfo0, TypeInfo1),
                    update_type_info_index(I, ETypeInfo, TypeInfo1, TypeInfo),
                    TI = std_util.yes(TypeInfo)
                ;
                    NewTypeInfo0 = new_type_info(CastTypeInfo, UpperBound),
                    update_type_info_index(I, ETypeInfo, NewTypeInfo0,
                        NewTypeInfo),
                    TI = std_util.yes(NewTypeInfo)
                )
            ), ArgTypeInfo0, MaybeArgTypeInfo),
        (
            MaybeArgTypeInfo = std_util.yes(ArgTypeInfo1),
            ArgTypeInfo = ArgTypeInfo1
        ;
            MaybeArgTypeInfo = std_util.no,
            ArgTypeInfo = CastTypeInfo
        )
    ).

    % XXX This is completely unimplemented.
    %
:- func pseudotypeinfo_get_higher_order_arity(type_info) = int.

pseudotypeinfo_get_higher_order_arity(_) = 1 :-
    det_unimplemented("pseudotypeinfo_get_higher_order_arity").

    % Make a new type-info with the given arity, using the given type_info
    % as the basis.
    %
:- func new_type_info(type_info::in, int::in) = (type_info::uo) is det.

new_type_info(TypeInfo::in, _::in) = (NewTypeInfo::uo) :-
    unsafe_promise_unique(TypeInfo, NewTypeInfo),
    det_unimplemented("new_type_info").

:- pragma foreign_proc("C#",
    new_type_info(OldTypeInfo::in, Arity::in) = (NewTypeInfo::uo),
    [promise_pure],
"
    NewTypeInfo = new object[Arity + 1];
    System.Array.Copy(OldTypeInfo, NewTypeInfo, OldTypeInfo.Length);
").

    % Get the pseudo-typeinfo at the given index from the argument types.
    %
:- some [T] func get_pti_from_arg_types(arg_types, int) = T.

get_pti_from_arg_types(_::in, _::in) = (42::out) :-
    det_unimplemented("get_pti_from_arg_types").

:- pragma foreign_proc("Java",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [will_not_call_mercury, promise_pure],
"
    // XXX This should be something else.
    TypeInfo_for_T = null;

    ArgTypeInfo = ArgTypes[Index];
").

:- pragma foreign_proc("C#",
    get_pti_from_arg_types(ArgTypes::in, Index::in) = (ArgTypeInfo::out),
    [promise_pure],
"
    // XXX This should be something else.
    // TypeInfo_for_T

    ArgTypeInfo = ArgTypes[Index];
").

    % Get the pseudo-typeinfo at the given index from a type-info.
    %
:- some [T] func get_pti_from_type_info(type_info, int) = T.

get_pti_from_type_info(_::in, _::in) = (42::out) :-
    det_unimplemented("get_pti_from_type_info").

:- pragma foreign_proc("C#",
    get_pti_from_type_info(TypeInfo::in, Index::in) = (PTI::out),
    [promise_pure],
"
    // XXX This should be something else.
    TypeInfo_for_T = NULL;

    PTI = TypeInfo[Index];
").

    % Get the type info for a particular type variable number
    % (it might be in the type_info or in the term itself).
    %
    % XXX Existentially quantified vars are not yet handled.
    %
:- pred get_type_info_for_var( type_info::in, int::in, T::in,
    du_functor_desc::in, type_info::out) is det.

get_type_info_for_var(TypeInfo, VarNum, Term, FunctorDesc, ArgTypeInfo) :-
    ( type_variable_is_univ_quant(VarNum) ->
        ArgTypeInfo = TypeInfo ^ type_info_index(VarNum)
    ;
        ( ExistInfo0 = FunctorDesc ^ du_functor_exist_info ->
            ExistInfo = ExistInfo0
        ;
            error("get_type_info_for_var no exist_info")
        ),

        ExistVarNum = VarNum - pseudotypeinfo_exist_var_base - 1,
        ExistLocn = ExistInfo ^ typeinfo_locns_index(ExistVarNum),
        Slot = ExistLocn ^ exist_arg_num,
        Offset = ExistLocn ^ exist_offset_in_tci,

        SlotMaybeTypeInfo = get_typeinfo_from_term(Term, Slot),
        ( Offset < 0 ->
            ArgTypeInfo = SlotMaybeTypeInfo
        ;
            ArgTypeInfo = typeclass_info_type_info(SlotMaybeTypeInfo, Offset)
        )
    ).

    % An unchecked cast to type_info (for pseudo-typeinfos).
    %
:- func type_info_cast(T) = type_info.

type_info_cast(X) = unsafe_cast(X).

    % Get a subterm term, given its type_info, the original term, its index
    % and the start region size.
    %
:- some [T] func get_subterm(type_info, U, int, int) = T.

get_subterm(_::in, _::in, _::in, _::in) = (42::out) :-
    det_unimplemented("get_subterm").

:- pragma foreign_proc("C#",
    get_subterm(TypeInfo::in, Term::in, Index::in, ExtraArgs::in) = (Arg::out),
    [promise_pure],
"
    // Mention TypeInfo_for_U to avoid a warning.
    
    int i = Index + ExtraArgs;
    try {
        // try low level data
        Arg = ((object[]) Term)[i];
    } catch (System.InvalidCastException) {
        // try high level data
        Arg = Term.GetType().GetFields()[i].GetValue(Term);
    }
    TypeInfo_for_T = TypeInfo;
").

    % Test whether a type info is variable.
    %
:- pred typeinfo_is_variable(T::in, int::out) is semidet.

typeinfo_is_variable(_::in, 42::out) :-
    semidet_unimplemented("typeinfo_is_variable").

:- pragma foreign_proc("C#",
    typeinfo_is_variable(TypeInfo::in, VarNum::out),
    [promise_pure],
"
    try {
        VarNum = System.Convert.ToInt32(TypeInfo);
        SUCCESS_INDICATOR = true;
    }
    catch (System.Exception e) {
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    typeinfo_is_variable(TypeInfo::in, VarNum::out),
    [will_not_call_mercury, promise_pure],
"
    succeeded = (TypeInfo.getClass() == mercury.runtime.PseudoTypeInfo.class);
    if (succeeded) {
        // This number is used to index into an array, hence the -1
        VarNum = ((mercury.runtime.PseudoTypeInfo)TypeInfo).variable_number
            - 1;
    } else {
        VarNum = -1; // just to keep the compiler happy
    }
").

    % Tests for universal and existentially quantified variables.

:- pred type_variable_is_univ_quant(int::in) is semidet.
:- pred type_variable_is_exist_quant(int::in) is semidet.

type_variable_is_exist_quant(X) :- X > pseudotypeinfo_exist_var_base.
type_variable_is_univ_quant(X) :- X =< pseudotypeinfo_exist_var_base.

:- func pseudotypeinfo_exist_var_base = int.
:- func pseudotypeinfo_max_var = int.

pseudotypeinfo_exist_var_base = 512.
pseudotypeinfo_max_var = 1024.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% XXX we have only implemented the .NET backend for the low-level data case.

:- func get_type_ctor_info(type_info) = type_ctor_info is det.

:- pragma foreign_code("C#", "

    // The field numbers of the contents of type_infos.
    enum fixed_arity_ti {
        type_ctor_info                      = 0,
        arg_type_infos                      = 1
    }

    enum var_arity_ti {
        type_ctor_info                      = 0,
        arity                               = 1,
        arg_type_infos                      = 2
    }

    // The field numbers of the contents of type_ctor_infos.
    // Fill this in as you add new field accessors.

    enum type_ctor_info_field_nums {
        type_ctor_arity                     = 0,
        // type_ctor_version                = 1,
        type_ctor_num_ptags                 = 2,
        type_ctor_rep                       = 3,
        type_ctor_unify_pred                = 4,
        type_ctor_compare_pred              = 5,
        type_ctor_module_name               = 6,
        type_ctor_name                      = 7,
        type_functors                       = 8,
        type_layout                         = 9,
        type_ctor_num_functors              = 10,
        type_ctor_flags                     = 11
    }

    enum ptag_layout_field_nums {
        sectag_sharers                      = 0,
        sectag_locn                         = 1,
        sectag_alternatives                 = 2
    }

    enum du_functor_field_nums {
        du_functor_name                     = 0,
        du_functor_orig_arity               = 1,
        du_functor_arg_type_contains_var    = 2,
        du_functor_sectag_locn              = 3,
        du_functor_primary                  = 4,
        du_functor_secondary                = 5,
        du_functor_ordinal                  = 6,
        du_functor_arg_types                = 7,
        du_functor_arg_names                = 8,
        du_functor_exist_info               = 9
    }

    enum exist_info_field_nums {
        typeinfos_plain                     = 0,
        typeinfos_in_tci                    = 1,
        tcis                                = 2,
        typeinfo_locns                      = 3
    }

    enum exist_locn_field_nums {
        exist_arg_num                       = 0,
        exist_offset_in_tci                 = 1
    }

").

:- pragma foreign_proc("C#",
    get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        TypeCtorInfo = (object[]) TypeInfo[0];
    } catch (System.InvalidCastException) {
        TypeCtorInfo = TypeInfo;
    }
").

:- pragma foreign_proc("Java",
    get_type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorInfo = ((mercury.runtime.TypeInfo_Struct) TypeInfo).type_ctor;
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

:- pred same_pointer_value(T::in, T::in) is semidet.
:- pred same_pointer_value_untyped(T::in, U::in) is semidet.

same_pointer_value(X, Y) :- same_pointer_value_untyped(X, Y).

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

same_pointer_value_untyped(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("same_pointer_value_untyped").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func get_primary_tag(T) = int.
:- func get_remote_secondary_tag(T) = int.

get_primary_tag(_::in) = (0::out) :-
    det_unimplemented("get_primary_tag").

get_remote_secondary_tag(_::in) = (0::out) :-
    det_unimplemented("get_remote_secondary_tag").

:- pragma foreign_proc("C#",
    get_primary_tag(X::in) = (Tag::out),
    [promise_pure],
"
    // We don't look at X to find the tag, for .NET low-level data
    // there is no primary tag, so we always return zero.
    Tag = 0;
").

:- pragma foreign_proc("C#",
    get_remote_secondary_tag(X::in) = (Tag::out),
    [promise_pure],
"
    try {
        // try the low-level data representation
        object[] data = (object[]) X;
        Tag = (int) data[0];
    } catch (System.InvalidCastException) {
        // try the high-level data representation
        Tag = (int) X.GetType().GetField(""data_tag"").GetValue(X);
    }
").

:- pragma foreign_proc("Java",
    get_primary_tag(_X::in) = (Tag::out),
    [promise_pure],
"
    // For the Java back-end, there is no primary tag, so always return 0.
    Tag = 0;
").

:- pragma foreign_proc("Java",
    get_remote_secondary_tag(X::in) = (Tag::out),
    [promise_pure],
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

:- type sectag_locn ---> none ; local ; remote ; variable.
% :- pragma foreign_type("Java", sectag_locn, "mercury.runtime.Sectag_Locn").

:- type du_sectag_alternatives ---> du_sectag_alternatives(c_pointer).
:- pragma foreign_type("Java", du_sectag_alternatives,
    "mercury.runtime.DuFunctorDesc[]").

:- type ptag_entry ---> ptag_entry(c_pointer).
:- pragma foreign_type("Java", ptag_entry, "mercury.runtime.DuPtagLayout").

:- type arg_types ---> arg_types(c_pointer).
:- pragma foreign_type("Java", arg_types, "mercury.runtime.PseudoTypeInfo[]").

:- type arg_names ---> arg_names(c_pointer).
:- pragma foreign_type("Java", arg_names, "java.lang.String[]").

:- type exist_info ---> exist_info(c_pointer).
:- pragma foreign_type("Java", exist_info, "mercury.runtime.DuExistInfo").

:- type typeinfo_locn ---> typeinfo_locn(c_pointer).
:- pragma foreign_type("Java", typeinfo_locn, "mercury.runtime.DuExistLocn").

:- func ptag_index(int, type_layout) = ptag_entry.

    % This is an "unimplemented" definition in Mercury, which will be
    % used by default.

ptag_index(_::in, TypeLayout::in) = (unsafe_cast(TypeLayout)::out) :-
    det_unimplemented("ptag_index").

:- pragma foreign_proc("C#",
    ptag_index(X::in, TypeLayout::in) = (PtagEntry::out),
    [promise_pure],
"
    PtagEntry = (object[]) TypeLayout[X];
").

:- pragma foreign_proc("Java",
    ptag_index(X::in, TypeLayout::in) = (PtagEntry::out),
    [promise_pure],
"
    PtagEntry = TypeLayout.layout_du()[X];
").

:- func sectag_locn(ptag_entry) = sectag_locn.

sectag_locn(PTagEntry::in) = (unsafe_cast(PTagEntry)::out) :-
    det_unimplemented("sectag_locn").

:- pragma foreign_proc("C#",
    sectag_locn(PTagEntry::in) = (SectagLocn::out),
    [promise_pure],
"
    SectagLocn = mercury.runtime.LowLevelData.make_enum((int)
        PTagEntry[(int) ptag_layout_field_nums.sectag_locn]);
").

:- pragma foreign_proc("Java",
    sectag_locn(PTagEntry::in) = (SectagLocn::out),
    [promise_pure],
"
    mercury.runtime.Sectag_Locn SL_struct = PTagEntry.sectag_locn;

    SectagLocn = new mercury.rtti_implementation.Sectag_locn_0(
        SL_struct.value);
").

:- func du_sectag_alternatives(int, ptag_entry) = du_functor_desc.

du_sectag_alternatives(_::in, PTagEntry::in) = (unsafe_cast(PTagEntry)::out) :-
    det_unimplemented("sectag_alternatives").

:- pragma foreign_proc("C#",
    du_sectag_alternatives(X::in, PTagEntry::in) = (FunctorDescriptor::out),
    [promise_pure],
"
    object[] sectag_alternatives;
    sectag_alternatives = (object [])
        PTagEntry[(int) ptag_layout_field_nums.sectag_alternatives];
    FunctorDescriptor = (object []) sectag_alternatives[X];
").

:- pragma foreign_proc("Java",
    du_sectag_alternatives(X::in, PTagEntry::in) = (FunctorDescriptor::out),
    [promise_pure],
"
    FunctorDescriptor = PTagEntry.sectag_alternatives[X];
").

:- func typeinfo_locns_index(int, exist_info) = typeinfo_locn.

typeinfo_locns_index(X::in, _::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("typeinfo_locns_index").

:- pragma foreign_proc("C#",
    typeinfo_locns_index(X::in, ExistInfo::in) = (TypeInfoLocn::out),
    [promise_pure],
"
    TypeInfoLocn = (object[]) ((object[]) ExistInfo[(int)
        exist_info_field_nums.typeinfo_locns])[X];
").

:- func exist_info_typeinfos_plain(exist_info) = int.

exist_info_typeinfos_plain(X::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("exist_info_typeinfos_plain").

:- pragma foreign_proc("C#",
    exist_info_typeinfos_plain(ExistInfo::in) = (TypeInfosPlain::out),
    [promise_pure],
"
    TypeInfosPlain = (int)
        ExistInfo[(int) exist_info_field_nums.typeinfos_plain];
").

:- func exist_info_tcis(exist_info) = int.

exist_info_tcis(X::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("exist_info_tcis").

:- pragma foreign_proc("C#",
    exist_info_tcis(ExistInfo::in) = (TCIs::out),
    [promise_pure],
"
    TCIs = (int) ExistInfo[(int) exist_info_field_nums.tcis];
").

:- func exist_arg_num(typeinfo_locn) = int.

exist_arg_num(X::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("exist_arg_num").

:- pragma foreign_proc("C#",
    exist_arg_num(TypeInfoLocn::in) = (ArgNum::out),
    [promise_pure],
"
    ArgNum = (int) TypeInfoLocn[(int) exist_locn_field_nums.exist_arg_num];

").

:- func exist_offset_in_tci(typeinfo_locn) = int.

exist_offset_in_tci(X::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("exist_arg_num").

:- pragma foreign_proc("C#",
    exist_offset_in_tci(TypeInfoLocn::in) = (ArgNum::out),
    [promise_pure],
"
    ArgNum = (int)
        TypeInfoLocn[(int) exist_locn_field_nums.exist_offset_in_tci];
").

:- func get_typeinfo_from_term(U, int) = type_info.

get_typeinfo_from_term(_::in, X::in) = (unsafe_cast(X)::out) :-
    det_unimplemented("get_typeinfo_from_term").

:- pragma foreign_proc("C#",
    get_typeinfo_from_term(Term::in, Index::in) = (TypeInfo::out),
    [promise_pure],
"
    try {
        TypeInfo = (object[]) ((object[]) Term)[Index];
    } catch (System.InvalidCastException) {
        // try high level data
        TypeInfo = (object[]) Term.GetType().GetFields()[Index].GetValue(Term);
    }
").

:- func typeclass_info_type_info(type_info, int) = type_info.

typeclass_info_type_info(TypeClassInfo, Index) = unsafe_cast(TypeInfo) :-
    private_builtin.type_info_from_typeclass_info(
        unsafe_cast(TypeClassInfo) `with_type` private_builtin.typeclass_info,
        Index, TypeInfo `with_type` private_builtin.type_info).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func var_arity_type_info_index(int, type_info) = type_info.

var_arity_type_info_index(Index, TypeInfo) =
    TypeInfo ^ type_info_index(Index + 1).

:- func type_info_index(int, type_info) = type_info.

type_info_index(_::in, TypeInfo::in) = (TypeInfo::out) :-
    % This is an "unimplemented" definition in Mercury, which will be
    % used by default.
    det_unimplemented("type_info_index").

:- pragma foreign_proc("Java",
    type_info_index(X::in, TypeInfo::in) = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure],
"
    TypeInfoAtIndex = (TypeInfo_Struct) ((TypeInfo_Struct) TypeInfo).args[X];
").

:- pragma foreign_proc("C#",
    type_info_index(X::in, TypeInfo::in) = (TypeInfoAtIndex::out),
    [will_not_call_mercury, promise_pure],
"
    TypeInfoAtIndex = (object[]) TypeInfo[X];
").

:- pred update_type_info_index(int::in, type_info::in, type_info::di,
    type_info::uo) is det.

update_type_info_index(_::in, _::in, X::di, X::uo) :-
    det_unimplemented("type_info_index").

:- pragma foreign_proc("C#",
    update_type_info_index(X::in, NewValue::in,
        OldTypeInfo::di, NewTypeInfo::uo),
    [will_not_call_mercury, promise_pure],
"
    OldTypeInfo[X] = NewValue;
    NewTypeInfo = OldTypeInfo;
").

:- pred semidet_unimplemented(string::in) is semidet.

semidet_unimplemented(S) :-
    ( std_util.semidet_succeed ->
        error("rtti_implementation: unimplemented: " ++ S)
    ;
        std_util.semidet_succeed
    ).

:- pred det_unimplemented(string::in) is det.

det_unimplemented(S) :-
    ( std_util.semidet_succeed ->
        error("rtti_implementation: unimplemented: " ++ S)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_ctor_arity(type_ctor_info) = int.
:- pragma foreign_proc("C#",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = (int) TypeCtorInfo[
        (int) type_ctor_info_field_nums.type_ctor_arity];
").
:- pragma foreign_proc("Java",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = ((TypeCtorInfo_Struct) TypeCtorInfo).arity;
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

:- some [P] func type_ctor_unify_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX This should be something else.
    // TypeInfo_for_P

    UnifyPred = TypeCtorInfo[
        (int) type_ctor_info_field_nums.type_ctor_unify_pred];
").
:- pragma foreign_proc("C",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci;

    /* XXX This should be something else. */
    TypeInfo_for_P = 0;

    tci = (MR_TypeCtorInfo) TypeCtorInfo;
    UnifyPred = (MR_Integer) tci->MR_type_ctor_unify_pred;
").
type_ctor_unify_pred(_) = "dummy value" :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_unify_pred").

:- some [P] func type_ctor_compare_pred(type_ctor_info) = P.
:- pragma foreign_proc("C#",
    type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // XXX This should be something else.
    TypeInfo_for_P = NULL;

    UnifyPred = TypeCtorInfo[
        (int) type_ctor_info_field_nums.type_ctor_compare_pred];
").

:- pragma foreign_proc("C",
    type_ctor_compare_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci;

    /* XXX This should be something else. */
    TypeInfo_for_P = 0;

    tci = (MR_TypeCtorInfo) TypeCtorInfo;
    UnifyPred = (MR_Integer) tci->MR_type_ctor_compare_pred;
").

type_ctor_compare_pred(_) = "dummy value" :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_compare_pred").

:- func type_ctor_rep(type_ctor_info) = type_ctor_rep.
:- pragma foreign_proc("C#",
    type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int rep;
    rep = (int) TypeCtorInfo[(int) type_ctor_info_field_nums.type_ctor_rep];
    TypeCtorRep = mercury.runtime.LowLevelData.make_enum(rep);
").
:- pragma foreign_proc("Java",
    type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorRep = new Type_ctor_rep_0(
        ((mercury.runtime.TypeCtorInfo_Struct) TypeCtorInfo).
        type_ctor_rep.value);
").
:- pragma foreign_proc("C",
    type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    TypeCtorRep = MR_type_ctor_rep(tci);
").
type_ctor_rep(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_rep").

:- func type_ctor_module_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
    type_ctor_module_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = (string)
        TypeCtorInfo[(int) type_ctor_info_field_nums.type_ctor_module_name];
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

:- func type_ctor_name(type_ctor_info) = string.

:- pragma foreign_proc("C#",
    type_ctor_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = (string)
        TypeCtorInfo[(int) type_ctor_info_field_nums.type_ctor_name];
").
:- pragma foreign_proc("Java",
    type_ctor_name(TypeCtorInfo::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = TypeCtorInfo.type_ctor_name;
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

:- func type_ctor_functors(type_ctor_info) = type_functors.

:- pragma foreign_proc("C#",
    type_ctor_functors(TypeCtorInfo::in) = (Functors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Functors = (object[])
        TypeCtorInfo[(int) type_ctor_info_field_nums.type_functors];
").

:- pragma foreign_proc("Java",
    type_ctor_functors(TypeCtorInfo::in) = (Functors::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Functors = TypeCtorInfo.type_functors;
").

type_ctor_functors(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_functors").

:- func type_layout(type_ctor_info) = type_layout.

:- pragma foreign_proc("C#",
    type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeLayout = (object[])
        TypeCtorInfo[(int) type_ctor_info_field_nums.type_layout];
").
:- pragma foreign_proc("Java",
    type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeLayout = TypeCtorInfo.type_layout;
").
:- pragma foreign_proc("C",
    type_layout(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_TypeCtorInfo tci = (MR_TypeCtorInfo) TypeCtorInfo;
    TypeLayout = (MR_Word) &(MR_type_ctor_layout(tci));
").

type_layout(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_layout").

:- func type_ctor_num_functors(type_ctor_info) = int.

:- pragma foreign_proc("C#",
    type_ctor_num_functors(TypeCtorInfo::in) = (TypeLayout::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeLayout = (int) TypeCtorInfo[(int)
        type_ctor_info_field_nums.type_ctor_num_functors];
").

type_ctor_num_functors(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_num_functors").

:- pragma foreign_proc("C",
    unsafe_cast(VarIn::in) = (VarOut::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarOut = VarIn;
").
:- pragma foreign_proc("C#",
    unsafe_cast(VarIn::in) = (VarOut::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarOut = VarIn;
").
:- pragma foreign_proc("Java",
    unsafe_cast(VarIn::in) = (VarOut::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarOut = VarIn;
").

unsafe_cast(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("unsafe_cast").

%-----------------------------------------------------------------------------%
%
% TypeFunctors
%
:- type type_functors ---> type_functors(c_pointer).
:- pragma foreign_type("Java", type_functors,
    "mercury.runtime.TypeFunctors").

:- type du_functor_desc ---> du_functor_desc(c_pointer).
:- pragma foreign_type("Java", du_functor_desc,
    "mercury.runtime.DuFunctorDesc").

:- type enum_functor_desc ---> enum_functor_desc(c_pointer).
:- pragma foreign_type("Java", enum_functor_desc,
    "mercury.runtime.EnumFunctorDesc").

:- type notag_functor_desc ---> notag_functor_desc(c_pointer).
:- pragma foreign_type("Java", notag_functor_desc,
    "mercury.runtime.NotagFunctorDesc").

:- inst du == bound(du; du_usereq; reserved_addr; reserved_addr_usereq).
:- inst enum == bound(enum ; enum_usereq ; dummy).
:- inst notag == bound(notag ; notag_usereq ;
    notag_ground ; notag_ground_usereq).

:- func du_functor_desc(type_ctor_rep, int, type_functors) = du_functor_desc.
:- mode du_functor_desc(in(du), in, in) = out is det.

du_functor_desc(_, Num, TypeFunctors) = DuFunctorDesc :-
    DuFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("Java",
    du_functor_desc(_TypeCtorRep::in(du), X::in, TypeFunctors::in) =
        (DuFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DuFunctorDesc = TypeFunctors.functors_du()[X];
").

:- func du_functor_name(du_functor_desc) = string.

du_functor_name(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("Java",
    du_functor_name(DuFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = DuFunctorDesc.du_functor_name;
").

:- func du_functor_arity(du_functor_desc) = int.

du_functor_arity(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(1).

:- pragma foreign_proc("Java",
    du_functor_arity(DuFunctorDesc::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = DuFunctorDesc.du_functor_orig_arity;
").

:- func du_functor_arg_type_contains_var(du_functor_desc) = int.

du_functor_arg_type_contains_var(DuFunctorDesc) =
    DuFunctorDesc ^ unsafe_index(2).

:- pragma foreign_proc("Java",
    du_functor_arg_type_contains_var(DuFunctorDesc::in) = (Contains::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Contains = DuFunctorDesc.du_functor_arg_type_contains_var;
").

:- func du_functor_sectag_locn(du_functor_desc) = sectag_locn.

du_functor_sectag_locn(DuFunctorDesc) =
    unsafe_make_enum(DuFunctorDesc ^ unsafe_index(3)).

:- pragma foreign_proc("Java",
    du_functor_sectag_locn(DuFunctorDesc::in) = (SectagLocn::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SectagLocn = DuFunctorDesc.du_functor_sectag_locn;
").

:- func du_functor_primary(du_functor_desc) = int.

du_functor_primary(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(4).

:- pragma foreign_proc("Java",
    du_functor_primary(DuFunctorDesc::in) = (Primary::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Primary = DuFunctorDesc.du_functor_primary;
").

:- func du_functor_secondary(du_functor_desc) = int.

du_functor_secondary(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(5).

:- pragma foreign_proc("Java",
    du_functor_secondary(DuFunctorDesc::in) = (Secondary::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Secondary = DuFunctorDesc.du_functor_secondary;
").

:- func du_functor_ordinal(du_functor_desc) = int.

du_functor_ordinal(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(6).

:- pragma foreign_proc("Java",
    du_functor_ordinal(DuFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = DuFunctorDesc.du_functor_ordinal;
").

:- func du_functor_arg_types(du_functor_desc) = arg_types.

du_functor_arg_types(DuFunctorDesc) = DuFunctorDesc ^ unsafe_index(7).

:- pragma foreign_proc("Java",
    du_functor_arg_types(DuFunctorDesc::in) = (ArgTypes::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgTypes = DuFunctorDesc.du_functor_arg_types;
").

:- func du_functor_arg_names(du_functor_desc::in) = (arg_names::out)
    is semidet.

du_functor_arg_names(DuFunctorDesc) = ArgNames :-
    ArgNames = DuFunctorDesc ^ unsafe_index(8),
    not null(ArgNames).

:- pragma foreign_proc("Java",
    du_functor_arg_names(DuFunctorDesc::in) = (ArgNames::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgNames = DuFunctorDesc.du_functor_arg_names;

    succeeded = (ArgNames != null);
").

:- func du_functor_exist_info(du_functor_desc::in) = (exist_info::out)
    is semidet.

du_functor_exist_info(DuFunctorDesc) = ExistInfo :-
    ExistInfo = DuFunctorDesc ^ unsafe_index(9),
    not null(ExistInfo).

:- pragma foreign_proc("Java",
    du_functor_exist_info(DuFunctorDesc::in) = (ExistInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ExistInfo = DuFunctorDesc.du_functor_exist_info;

    succeeded = (ExistInfo != null);
").

%--------------------------%

:- func enum_functor_desc(type_ctor_rep, int, type_functors)
    = enum_functor_desc.
:- mode enum_functor_desc(in(enum), in, in) = out is det.

enum_functor_desc(_, Num, TypeFunctors) = EnumFunctorDesc :-
    EnumFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("Java",
    enum_functor_desc(_TypeCtorRep::in(enum), X::in, TypeFunctors::in) =
        (EnumFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    EnumFunctorDesc = (TypeFunctors.functors_enum())[X];
").

:- func enum_functor_name(enum_functor_desc) = string.

enum_functor_name(EnumFunctorDesc) = EnumFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("Java",
    enum_functor_name(EnumFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = EnumFunctorDesc.enum_functor_name;
").

:- func enum_functor_ordinal(enum_functor_desc) = int.

enum_functor_ordinal(EnumFunctorDesc) = EnumFunctorDesc ^ unsafe_index(1).

:- pragma foreign_proc("Java",
    enum_functor_ordinal(EnumFunctorDesc::in) = (Ordinal::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ordinal = EnumFunctorDesc.enum_functor_ordinal;
").

 %--------------------------%

:- func notag_functor_desc(type_ctor_rep, int, type_functors)
    = notag_functor_desc.

:- mode notag_functor_desc(in(notag), in, in) = out is det.

notag_functor_desc(_, Num, TypeFunctors) = NoTagFunctorDesc :-
    NoTagFunctorDesc = TypeFunctors ^ unsafe_index(Num).

:- pragma foreign_proc("Java",
    notag_functor_desc(_TypeCtorRep::in(notag), _X::in, TypeFunctors::in) =
        (NotagFunctorDesc::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NotagFunctorDesc = TypeFunctors.functors_notag();
").

:- func notag_functor_name(notag_functor_desc) = string.

notag_functor_name(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(0).

:- pragma foreign_proc("Java",
    notag_functor_name(NotagFunctorDesc::in) = (Name::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = NotagFunctorDesc.no_tag_functor_name;
").

    % XXX This is a bug. This function should actually return a PseudoTypeInfo.
    % The Java code below should work once this is corrected.
    %
:- func notag_functor_arg_type(notag_functor_desc) = type_info.

notag_functor_arg_type(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(1).

% :- pragma foreign_proc("Java",
%   notag_functor_arg_type(NotagFunctorDesc::in) = (ArgType::out),
%   [will_not_call_mercury, promise_pure, thread_safe],
% "
%   ArgType = NotagFunctorDesc.no_tag_functor_arg_type;
% ").

:- func notag_functor_arg_name(notag_functor_desc) = string.

notag_functor_arg_name(NoTagFunctorDesc) = NoTagFunctorDesc ^ unsafe_index(2).

:- pragma foreign_proc("Java",
    notag_functor_arg_name(NotagFunctorDesc::in) = (ArgName::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ArgName = NotagFunctorDesc.no_tag_functor_arg_name;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func unsafe_index(int, T) = U.
:- pragma foreign_proc("C#",
    unsafe_index(Num::in, Array::in) = (Item::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Item = ((object []) Array)[Num];
").
unsafe_index(_, _) = _ :-
    private_builtin.sorry("rtti_implementation.unsafe_index").

 %--------------------------%

:- func unsafe_make_enum(int) = T.
:- pragma foreign_proc("C#",
    unsafe_make_enum(Num::in) = (Enum::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Enum = mercury.runtime.LowLevelData.make_enum(Num);
").
unsafe_make_enum(_) = _ :-
    private_builtin.sorry("rtti_implementation.unsafe_make_enum").

 %--------------------------%

:- pred null(T::in) is semidet.
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
    succeeded = (S == null);
").
null(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("rtti_implementation.null/1").

 %--------------------------%

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

 %--------------------------%

:- func unsafe_get_enum_value(T) = int.

:- pragma foreign_proc("Java",
    unsafe_get_enum_value(Enum::in) = (Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    try {
        Value = Enum.getClass().getField(""value"").getInt(Enum);
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

%-----------------------------------------------------------------------------%
