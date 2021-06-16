%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: term_rep.m.
% Author: Ian MacLarty.
%
% This module implements an abstract type, term_rep, values of which are the
% representation of some other value. Constructing a representation from a
% term is cc_multi, but then doing comparisons on the representation is
% deterministic.
%
% This is useful when we only want to consider the representation of a term
% and don't care about it's actual value.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.term_rep.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.

:- import_module univ.

%---------------------------------------------------------------------------%

:- type term_rep.

:- pred univ_to_rep(univ::in, term_rep::out) is cc_multi.

:- pred rep_to_univ(term_rep::in, univ::out) is det.

:- pred deref_path(term_rep::in, term_path::in, term_rep::out) is semidet.

    % argument(Term, N, Subterm):
    %
    % True iff Subterm is the Nth argument of Term.
    %
:- pred argument(term_rep::in, int::in, term_rep::out) is semidet.

    % field_pos(FieldName, Term, N):
    %
    % True iff argument N of Term has the name FieldName.
    %
:- pred field_pos(string::in, term_rep::in, int::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_debugger.

:- import_module construct.
:- import_module deconstruct.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module type_desc.

%---------------------------------------------------------------------------%

:- type term_rep
    --->    term_rep(univ)
    where
        equality is term_rep_equal,
        comparison is term_rep_compare.

:- pred term_rep_equal(term_rep::in, term_rep::in) is semidet.
:- pragma terminates(pred(term_rep_equal/2)).

term_rep_equal(Rep1, Rep2) :-
    promise_equivalent_solutions [Result] (
        comp_rep_2(Rep1, Rep2, Result)
    ),
    Result = (=).

:- pred comp_rep_2(term_rep::in, term_rep::in, builtin.comparison_result::uo)
    is cc_multi.

comp_rep_2(Rep1, Rep2, Result) :-
    builtin.compare_representation(Result, Rep1, Rep2).

:- pred term_rep_compare(builtin.comparison_result::uo, term_rep::in,
    term_rep::in) is det.
:- pragma terminates(pred(term_rep_compare/3)).

term_rep_compare(Result, Rep1, Rep2) :-
    promise_equivalent_solutions [Result] (
        comp_rep_2(Rep1, Rep2, Result)
    ).

univ_to_rep(Univ0, term_rep(Univ)) :-
    cc_multi_equal(Univ0, Univ).

rep_to_univ(Rep, Univ) :-
    promise_equivalent_solutions [Univ] (
        Rep = term_rep(Univ)
    ).

deref_path(Term, Path, SubTerm):-
    (
        Path = [],
        SubTerm = Term
    ;
        Path = [Head | Tail],
        argument(Term, Head, NextSubTerm),
        deref_path(NextSubTerm, Tail, SubTerm)
    ).

argument(Term, N, Arg) :-
    % There is only one representation of a subterm, given
    % the representation of the containing term and a term path.
    promise_equivalent_solutions [MaybeArg] (
        rep_to_univ(Term, Univ),
        % Argument indexes in the term path start from one, but the argument
        % function wants argument indexes to start from zero.
        arg_cc(univ_value(Univ), N - 1, MaybeSubUniv),
        (
            MaybeSubUniv = arg(SubValue),
            univ_to_rep(univ(SubValue), Arg0),
            MaybeArg = yes(Arg0)
        ;
            MaybeSubUniv = no_arg,
            MaybeArg = no
        )
    ),
    MaybeArg = yes(Arg).

field_pos(FieldName, Term, Pos) :-
    % There is only one or zero positions of a field
    % given a representation of a term and the field name.
    promise_equivalent_solutions [MaybePos] (
        rep_to_univ(Term, Univ),
        Value = univ_value(Univ),
        deconstruct(Value, include_details_cc, Functor, Arity, _Args),
        Type = type_of(Value),
        ( if NumFunctors = num_functors(Type) then
            find_functor(1, NumFunctors, Type, Functor, Arity,
                MaybeFunctorNum)
        else
            MaybeFunctorNum = no
        ),
        (
            MaybeFunctorNum = yes(FunctorNum),
            ( if
                get_functor_with_names(Type, FunctorNum - 1,
                    _FunctorName, _Arity, _ArgTypes, ArgNames)
            then
                ( if
                    list.index1_of_first_occurrence(ArgNames, yes(FieldName),
                        Pos0)
                then
                    MaybePos = yes(Pos0)
                else
                    MaybePos = no
                )
            else
                throw(internal_error("field_pos",
                    "get_functor_with_names couldn't find functor"))
            )
        ;
            MaybeFunctorNum = no,
            throw(internal_error("field_pos",
                "find_functor couldn't find functor"))
        )
    ),
    MaybePos = yes(Pos).

:- pred find_functor(int::in, int::in, type_desc::in, string::in, int::in,
    maybe(int)::out) is det.

find_functor(Current, NumFunctors, Type, FunctorName, Arity,
        MaybeFunctorNum) :-
    ( if Current =< NumFunctors then
        ( if get_functor(Type, Current - 1, FunctorName, Arity, _) then
            MaybeFunctorNum = yes(Current)
        else
            find_functor(Current + 1, NumFunctors, Type,
                FunctorName, Arity, MaybeFunctorNum)
        )
    else
        MaybeFunctorNum = no
    ).
