%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: erlang_rtti_implementation.m.
% Main author: petdr.
% Stability: low.
%
% This file is intended to provide the RTTI implementation for the Erlang
% backend.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module erlang_rtti_implementation.
:- interface.

    %
    % Check if two values are equal.
    % Note this is not structural equality because a type
    % can have user-defined equality.
    %
:- pred generic_unify(T::in, T::in) is semidet.

:- pred generic_compare(comparison_result::out, T::in, T::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

    %
    % A type_info can be represented in one of three ways
    % For a type with arity 0
    %   TypeCtorInfo
    % a type with arity > 0
    %   { TypeCtorInfo, TypeInfo0, ..., TypeInfoN }
    % a type with variable arity of size N
    %   { TypeCtorInfo, N, TypeCtorInfo0, ..., TypeCtorInfoN }
    %   
:- type type_info.
:- pragma foreign_type("Erlang", type_info, "").
:- type type_info ---> type_info.

    %
    % For the representation of a type_ctor_info
    % see erlang_rtti:type_ctor_data_to_elds
    %
:- type type_ctor_info.
:- pragma foreign_type("Erlang", type_ctor_info, "").
:- type type_ctor_info ---> type_ctor_info.

    % The type_ctor_rep needs to be kept up to date with the alternatives
    % given by the function erl_rtti.erlang_type_ctor_rep/1
    %
:- type erlang_type_ctor_rep
    --->    etcr_du
    ;       etcr_list
    ;       etcr_eqv
    ;       etcr_int
    ;       etcr_float
    ;       etcr_char
    ;       etcr_string
    ;       etcr_void
    ;       etcr_stable_c_pointer
    ;       etcr_c_pointer
    ;       etcr_pred
    ;       etcr_func
    ;       etcr_tuple
    ;       etcr_ref
    ;       etcr_type_desc
    ;       etcr_pseudo_type_desc
    ;       etcr_type_ctor_desc
    ;       etcr_type_info
    ;       etcr_type_ctor_info
    ;       etcr_typeclass_info
    ;       etcr_base_typeclass_info
    ;       etcr_foreign

            % These types shouldn't be needed they are
            % introduced for library predicates which
            % don't apply on this backend.
    ;       etcr_hp
    ;       etcr_subgoal
    ;       etcr_ticket
    .

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generic_unify(X, Y) :-
    TypeInfo = X ^ type_info,
    TypeCtorInfo = TypeInfo ^ type_ctor_info,
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = etcr_tuple
    ->
        unify_tuple(TypeInfo, X, Y)
    ;
        ( TypeCtorRep = etcr_pred ; TypeCtorRep = etcr_func )
    ->
        error("unify/2: higher order unification not possible")
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

unify_tuple(TypeInfo, X, Y) :-
    Arity = TypeInfo ^ var_arity_type_info_arity,
    unify_tuple_pos(1, Arity, TypeInfo, X, Y).

:- pred unify_tuple_pos(int::in, int::in,
        type_info::in, T::in, T::in) is semidet.

unify_tuple_pos(Loc, TupleArity, TypeInfo, TermA, TermB) :-
    ( Loc > TupleArity ->
        true
    ;
        ArgTypeInfo = TypeInfo ^ var_arity_type_info_index(Loc),

        SubTermA = get_subterm(ArgTypeInfo, TermA, Loc, 0),
        SubTermB = get_subterm(ArgTypeInfo, TermB, Loc, 0),

        generic_unify(SubTermA, unsafe_cast(SubTermB)),

        unify_tuple_pos(Loc + 1, TupleArity, TypeInfo, TermA, TermB)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generic_compare(Res, X, Y) :-
    TypeInfo = X ^ type_info,
    TypeCtorInfo = TypeInfo ^ type_ctor_info,
    TypeCtorRep = TypeCtorInfo ^ type_ctor_rep,
    (
        TypeCtorRep = etcr_tuple
    ->
        compare_tuple(TypeInfo, Res, X, Y)
    ;
        ( TypeCtorRep = etcr_pred ; TypeCtorRep = etcr_func )
    ->
        error("compare/3: higher order comparison not possible")
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
    
:- pred compare_tuple(type_info::in, comparison_result::out, T::in, T::in)
    is det.

compare_tuple(TypeInfo, Result, TermA, TermB) :-
    Arity = TypeInfo ^ var_arity_type_info_arity,
    compare_tuple_pos(1, Arity, TypeInfo, Result, TermA, TermB).

:- pred compare_tuple_pos(int::in, int::in, type_info::in,
    comparison_result::out, T::in, T::in) is det.

compare_tuple_pos(Loc, TupleArity, TypeInfo, Result, TermA, TermB) :-
    ( Loc > TupleArity ->
        Result = (=)
    ;
        ArgTypeInfo = TypeInfo ^ var_arity_type_info_index(Loc),

        SubTermA = get_subterm(ArgTypeInfo, TermA, Loc, 0),
        SubTermB = get_subterm(ArgTypeInfo, TermB, Loc, 0),

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

:- pragma foreign_code("Erlang", "
        % Location of element in a type_info
    ti_type_ctor_info() -> 1.
    ti_var_arity() -> 2.

        % Location of elements in a type_ctor_info
    tci_arity() -> 1.
    tci_version() -> 2.
    tci_unify_pred() -> 3.
    tci_compare_pred() -> 4.
    tci_module_name() -> 5.
    tci_type_name() -> 6.
    tci_type_ctor_rep() -> 7.
    tci_functors() -> 8.
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_info(T::unused) = (type_info::out) is det.

:- pragma foreign_proc("Erlang",
    type_info(_T::unused) = (TypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeInfo = TypeInfo_for_T
").

type_info(_) = type_info :-
    det_unimplemented("type_info").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_ctor_info(type_info) = type_ctor_info.

:- pragma foreign_proc("Erlang",
    type_ctor_info(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
        % 
        % If the type_info is for a type with arity 0,
        % then the type_info is already the type_ctor info.
        % The first field of a type_ctor_info is the integer
        % zero in this case.
        %
    FirstElement = element(ti_type_ctor_info(), TypeInfo),
    if
            % XXX is the test FirstElement =:= 0 better?
        is_integer(FirstElement)
            -> TypeCtorInfo = TypeInfo ;
        true
            -> TypeCtorInfo = FirstElement
    end
").

type_ctor_info(_) = type_ctor_info :-
    det_unimplemented("type_ctor_info").

:- func var_arity_type_info_arity(type_info) = int.

:- pragma foreign_proc("Erlang",
    var_arity_type_info_arity(TypeInfo::in) = (TypeCtorInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorInfo = element(ti_var_arity(), TypeInfo)
").

var_arity_type_info_arity(_) = 0 :-
    det_unimplemented("var_arity_type_info_arity").

    %
    % TI ^ type_info_index(I)
    %
    % returns the I'th type_info from the given standard type_info TI.
    % NOTE indexes start at one.
    % 
:- func type_info_index(int, type_info) = type_info.

type_info_index(I, TI) = TI ^ unsafe_type_info_index(I + 1).

    %
    % TI ^ var_arity_type_info_index(I)
    %
    % returns the I'th type_info from the given variable arity type_info TI.
    % NOTE indexes start at one.
    % 
:- func var_arity_type_info_index(int, type_info) = type_info.

var_arity_type_info_index(I, TI) = TI ^ unsafe_type_info_index(I + 2).

    %
    % Use type_info_index or var_arity_type_info_index, never this predicate
    % directly.
    %
:- func unsafe_type_info_index(int, type_info) = type_info.

:- pragma foreign_proc("Erlang",
    unsafe_type_info_index(Index::in, TypeInfo::in) = (SubTypeInfo::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SubTypeInfo = element(Index, TypeInfo)
").

unsafe_type_info_index(_, _) = type_info :-
    det_unimplemented("unsafe_type_info_index").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func type_ctor_rep(type_ctor_info) = erlang_type_ctor_rep.

:- pragma foreign_proc("Erlang",
    type_ctor_rep(TypeCtorInfo::in) = (TypeCtorRep::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TypeCtorRep = element(tci_type_ctor_rep(), TypeCtorInfo)
").

type_ctor_rep(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("type_ctor_rep").

:- some [P] func type_ctor_unify_pred(type_ctor_info) = P.

:- pragma foreign_proc("Erlang",
    type_ctor_unify_pred(TypeCtorInfo::in) = (UnifyPred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
        % The TypeInfo is never used so this is safe
    TypeInfo_for_P = 0,
    UnifyPred = element(tci_unify_pred(), TypeCtorInfo)
").

type_ctor_unify_pred(_) = "dummy value" :-
    det_unimplemented("type_ctor_unify_pred").

:- some [P] func type_ctor_compare_pred(type_ctor_info) = P.

:- pragma foreign_proc("Erlang",
    type_ctor_compare_pred(TypeCtorInfo::in) = (ComparePred::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
        % The TypeInfo is never used so this is safe
    TypeInfo_for_P = 0,
    ComparePred = element(tci_compare_pred(), TypeCtorInfo)
").

type_ctor_compare_pred(_) = "dummy value" :-
    det_unimplemented("type_ctor_compare_pred").

:- func type_ctor_arity(type_ctor_info) = int.

:- pragma foreign_proc("Erlang",
    type_ctor_arity(TypeCtorInfo::in) = (Arity::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Arity = element(tci_arity(), TypeCtorInfo)
").

type_ctor_arity(_) = 0 :-
    det_unimplemented("type_ctor_arity").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Get a subterm term, given its type_info, the original term, its index
    % and the start region size.
    %
:- some [T] func get_subterm(type_info, U, int, int) = T.

get_subterm(_::in, _::in, _::in, _::in) = (42::out) :-
    det_unimplemented("get_subterm").

:- pragma foreign_proc("Erlang",
    get_subterm(TypeInfo::in, Term::in, Index::in, ExtraArgs::in) = (Arg::out),
    [promise_pure],
"
    % TypeInfo_for_U to avoid compiler warning
    TypeInfo_for_T = TypeInfo,
    Arg = element(Index + ExtraArgs, Term)
").

:- func unsafe_cast(T) = U.

:- pragma foreign_proc("Erlang",
    unsafe_cast(VarIn::in) = (VarOut::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    VarOut = VarIn
").

unsafe_cast(_) = _ :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    private_builtin.sorry("unsafe_cast").

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

:- pred semidet_unimplemented(string::in) is semidet.

semidet_unimplemented(S) :-
    ( semidet_succeed ->
        error("rtti_implementation: unimplemented: " ++ S)
    ;
        semidet_succeed
    ).

:- pred det_unimplemented(string::in) is det.

det_unimplemented(S) :-
    ( semidet_succeed ->
        error("rtti_implementation: unimplemented: " ++ S)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % We override the above definitions in the Erlang backend.

:- pragma foreign_proc("Erlang",
    semidet_call_3(Pred::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = case Pred(X, Y) of {} -> true; fail -> false end
").
:- pragma foreign_proc("Erlang",
    semidet_call_4(Pred::in, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = case Pred(A, X, Y) of {} -> true; fail -> false end
").
:- pragma foreign_proc("Erlang",
    semidet_call_5(Pred::in, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = case Pred(A, B, X, Y) of {} -> true; fail -> false end
").
:- pragma foreign_proc("Erlang",
    semidet_call_6(Pred::in, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        case Pred(A, B, C, X, Y) of
            {} -> true;
            fail -> false
        end
").
:- pragma foreign_proc("Erlang",
    semidet_call_7(Pred::in, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        case Pred(A, B, C, D, X, Y) of
            {} -> true;
            fail -> false
        end
").
:- pragma foreign_proc("Erlang",
    semidet_call_8(Pred::in, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR =
        case Pred(A, B, C, D, E, X, Y) of
            {} -> true;
            fail -> false
        end
").

:- pragma foreign_proc("Erlang",
    result_call_4(Pred::in, Res::out, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(X, Y)
").

:- pragma foreign_proc("Erlang",
    result_call_5(Pred::in, Res::out, A::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(A, X, Y)
").
:- pragma foreign_proc("Erlang",
    result_call_6(Pred::in, Res::out, A::in, B::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(A, B, X, Y)
").
:- pragma foreign_proc("Erlang",
    result_call_7(Pred::in, Res::out, A::in, B::in, C::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(A, B, C, X, Y)
").
:- pragma foreign_proc("Erlang",
    result_call_8(Pred::in, Res::out, A::in, B::in, C::in, D::in, X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(A, B, C, D, X, Y)
").
:- pragma foreign_proc("Erlang",
    result_call_9(Pred::in, Res::out, A::in, B::in, C::in, D::in, E::in,
        X::in, Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    {Res} = Pred(A, B, C, D, E, X, Y)
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
