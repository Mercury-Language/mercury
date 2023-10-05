%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type_repn.m.
%
% Tests u
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_repn.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module one_or_more.

%---------------------------------------------------------------------------%

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors a notag type?
    %
:- pred non_sub_du_type_is_notag(one_or_more(constructor)::in,
    maybe_canonical::in) is semidet.

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors an enum? If yes, return the number of enum values.
    %
:- pred non_sub_du_type_is_enum(type_details_du::in, int::out) is semidet.

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors a dummy type?
    %
:- pred non_sub_du_type_is_dummy(type_details_du::in) is semidet.

    % Return the number of bits required to represent
    % the given number of values, 0 to n-1.
    %
:- pred num_bits_needed_for_n_dense_values(int::in, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

non_sub_du_type_is_notag(OoMCtors, MaybeCanonical) :-
    OoMCtors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _FunctorName, [_CtorArg], 1,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    MaybeCanonical = canon.

non_sub_du_type_is_enum(DuDetails, NumFunctors) :-
    DuDetails = type_details_du(OoMCtors, _MaybeCanon, _MaybeDirectArgCtors),
    Ctors = one_or_more_to_list(OoMCtors),
    Ctors = [_, _ | _],
    all_functors_are_constants(Ctors, 0, NumFunctors).

:- pred all_functors_are_constants(list(constructor)::in,
    int::in, int::out) is semidet.

all_functors_are_constants([], !NumFunctors).
all_functors_are_constants([Ctor | Ctors], !NumFunctors) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _Name, ArgTypes, _Arity,
        _Context),
    ArgTypes = [],
    MaybeExistConstraints = no_exist_constraints,
    !:NumFunctors = !.NumFunctors + 1,
    all_functors_are_constants(Ctors, !NumFunctors).

non_sub_du_type_is_dummy(DuDetails) :-
    DuDetails = type_details_du(Ctors, MaybeCanonical, MaybeDirectArgCtors),
    Ctors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _FunctorName, [], 0,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    MaybeCanonical = canon,
    MaybeDirectArgCtors = no.

num_bits_needed_for_n_dense_values(NumValues, NumBits) :-
    int.log2(NumValues, NumBits).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_type_repn.
%---------------------------------------------------------------------------%
