%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2016-2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: maybe.m.
% Main author: fjh.
% Stability: high.
%
% This module defines the "maybe" type.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module maybe.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- type maybe(T)
    --->    no
    ;       yes(T).

:- inst maybe(I) for maybe/1
    --->    no
    ;       yes(I).

:- inst maybe_yes(I) for maybe/1
    --->    yes(I).

:- type maybe_error
    --->    ok
    ;       error(string).

:- type maybe_error(T) == maybe_error(T, string).

    % Either a T, or an error E.
:- type maybe_error(T, E)
    --->    ok(T)
    ;       error(E).

:- inst maybe_error(I) for maybe_error/2
    --->    ok(I)
    ;       error(ground).

:- inst maybe_error_ok(I) for maybe_error/2
    --->    ok(I).

:- type maybe_errors(T) == maybe_errors(T, string).

    % Either a T, or one or more errors E.
:- type maybe_errors(T, E)
    --->    ok(T)
    ;       error(E, list(E)).

:- inst maybe_errors_ok(I) for maybe_errors/2
    --->    ok(I).

    % map_maybe(_, no) = no.
    % map_maybe(F, yes(Value)) = yes(F(Value)).
    %
:- func map_maybe(func(T) = U, maybe(T)) = maybe(U).

    % map_maybe(_, no, no).
    % map_maybe(P, yes(Value0), yes(Value)) :- P(Value, Value).
    %
:- pred map_maybe(pred(T, U), maybe(T), maybe(U)).
:- mode map_maybe(pred(in, out) is det, in, out) is det.
:- mode map_maybe(pred(in, out) is semidet, in, out) is semidet.
:- mode map_maybe(pred(in, out) is multi, in, out) is multi.
:- mode map_maybe(pred(in, out) is nondet, in, out) is nondet.

    % fold_maybe(_, no, Acc) = Acc.
    % fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).
    %
:- func fold_maybe(func(T, U) = U, maybe(T), U) = U.

    % fold_maybe(_, no, !Acc).
    % fold_maybe(P, yes(Value), !Acc) :- P(Value, !Acc).
    %
:- pred fold_maybe(pred(T, A, A), maybe(T), A, A).
:- mode fold_maybe(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_maybe(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_maybe(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold_maybe(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_maybe(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_maybe(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred fold2_maybe(pred(T, A, A, B, B), maybe(T), A, A, B, B).
:- mode fold2_maybe(pred(in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode fold2_maybe(pred(in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode fold2_maybe(pred(in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode fold2_maybe(pred(in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode fold2_maybe(pred(in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode fold2_maybe(pred(in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

    % As above, but with three accumulators.
    %
:- pred fold3_maybe(pred(T, A, A, B, B, C, C), maybe(T), A, A, B, B, C, C).
:- mode fold3_maybe(pred(in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode fold3_maybe(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode fold3_maybe(pred(in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode fold3_maybe(pred(in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode fold3_maybe(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode fold3_maybe(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

    % As above, but with four accumulators.
    %
:- pred fold4_maybe(pred(T, A, A, B, B, C, C, D, D),
    maybe(T), A, A, B, B, C, C, D, D).
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold4_maybe(pred(in, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred fold5_maybe(pred(T, A, A, B, B, C, C, D, D, E, E),
    maybe(T), A, A, B, B, C, C, D, D, E, E).
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode fold5_maybe(
    pred(in, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % map_fold_maybe(_, no, no, !Acc).
    % map_fold_maybe(P, yes(Value0), yes(Value), !Acc) :-
    %      P(Value, Value, !Acc).
    %
:- pred map_fold_maybe(pred(T, U, A, A), maybe(T), maybe(U), A, A).
:- mode map_fold_maybe(pred(in, out, in, out) is det,
    in, out, in, out) is det.
:- mode map_fold_maybe(pred(in, out, mdi, muo) is det,
    in, out, mdi, muo) is det.
:- mode map_fold_maybe(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode map_fold_maybe(pred(in, out, in, out) is semidet,
    in, out, in, out) is semidet.
:- mode map_fold_maybe(pred(in, out, mdi, muo) is semidet,
    in, out, mdi, muo) is semidet.
:- mode map_fold_maybe(pred(in, out, di, uo) is semidet,
    in, out, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred map_fold2_maybe(pred(T, U, A, A, B, B),
    maybe(T), maybe(U), A, A, B, B).
:- mode map_fold2_maybe(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_fold2_maybe(pred(in, out, in, out, mdi, muo) is det,
    in, out, in, out, mdi, muo) is det.
:- mode map_fold2_maybe(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_fold2_maybe(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.
:- mode map_fold2_maybe(pred(in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_fold2_maybe(pred(in, out, in, out, di, uo) is semidet,
    in, out, in, out, di, uo) is semidet.

    % As above, but with three accumulators.
    %
:- pred map_fold3_maybe(pred(T, U, A, A, B, B, C, C),
    maybe(T), maybe(U), A, A, B, B, C, C).
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, mdi, muo) is det,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_fold3_maybe(pred(in, out, in, out, in, out, di, uo) is semidet,
    in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with four accumulators.
    %
:- pred map_fold4_maybe(pred(T, U, A, A, B, B, C, C, D, D),
    maybe(T), maybe(U), A, A, B, B, C, C, D, D).
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_fold4_maybe(
    pred(in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, out, in, out, in, out, in, out, di, uo) is semidet.

    % As above, but with five accumulators.
    %
:- pred map_fold5_maybe(pred(T, U, A, A, B, B, C, C, D, D, E, E),
    maybe(T), maybe(U), A, A, B, B, C, C, D, D, E, E).
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out, in, out, in, out) is det.
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, mdi, muo) is det,
    in, out, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_fold5_maybe(
    pred(in, out, in, out, in, out, in, out, in, out, di, uo) is semidet,
    in, out, in, out, in, out, in, out, in, out, di, uo) is semidet.

    % maybe_is_yes(yes(X), X).
    %
    % This is useful as an argument to list.filter_map.
    %
:- pred maybe_is_yes(maybe(T)::in, T::out) is semidet.

    % pred_to_maybe(Pred) = MaybeResult.
    %
    % Make a maybe value from a semidet predicate.
    %
:- func pred_to_maybe(pred(T)) = maybe(T).
:- mode pred_to_maybe(pred(out) is semidet) = out is det.

:- func func_to_maybe((func) = T) = maybe(T).
:- mode func_to_maybe((func) = out is semidet) = out is det.

    % Return the value from within the maybe or a default value if there is
    % none.
    %
:- func maybe_default(T, maybe(T)) = T.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

map_maybe(_, no, no).
map_maybe(P, yes(T), yes(U)) :- P(T, U).

fold_maybe(_, no, Acc) = Acc.
fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).

fold_maybe(_, no, !Acc).
fold_maybe(P, yes(Value), !Acc) :-
    P(Value, !Acc).

fold2_maybe(_, no, !Acc1, !Acc2).
fold2_maybe(P, yes(Value), !Acc1, !Acc2) :-
    P(Value, !Acc1, !Acc2).

fold3_maybe(_, no, !Acc1, !Acc2, !Acc3).
fold3_maybe(P, yes(Value), !Acc1, !Acc2, !Acc3) :-
    P(Value, !Acc1, !Acc2, !Acc3).

fold4_maybe(_, no, !Acc1, !Acc2, !Acc3, !Acc4).
fold4_maybe(P, yes(Value), !Acc1, !Acc2, !Acc3, !Acc4) :-
    P(Value, !Acc1, !Acc2, !Acc3, !Acc4).

fold5_maybe(_, no, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5).
fold5_maybe(P, yes(Value), !Acc1, !Acc2, !Acc3, !Acc4, !Acc5) :-
    P(Value, !Acc1, !Acc2, !Acc3, !Acc4, !Acc5).

map_fold_maybe(_, no, no, Acc, Acc).
map_fold_maybe(P, yes(T), yes(U), Acc0, Acc) :-
    P(T, U, Acc0, Acc).

map_fold2_maybe(_, no, no, !A, !B).
map_fold2_maybe(P, yes(T), yes(U), !A, !B) :-
    P(T, U, !A, !B).

map_fold3_maybe(_, no, no, !A, !B, !C).
map_fold3_maybe(P, yes(T), yes(U), !A, !B, !C) :-
    P(T, U, !A, !B, !C).

map_fold4_maybe(_, no, no, !A, !B, !C, !D).
map_fold4_maybe(P, yes(T), yes(U), !A, !B, !C, !D) :-
    P(T, U, !A, !B, !C, !D).

map_fold5_maybe(_, no, no, !A, !B, !C, !D, !E).
map_fold5_maybe(P, yes(T), yes(U), !A, !B, !C, !D, !E) :-
    P(T, U, !A, !B, !C, !D, !E).

maybe_is_yes(yes(X), X).

pred_to_maybe(Pred) = Result :-
    ( if Pred(X) then
        Result = yes(X)
    else
        Result = no
    ).

func_to_maybe(PF) =
    ( if Y = apply(PF) then yes(Y) else no ).

maybe_default(_, yes(X)) = X.
maybe_default(D, no) = D.

%---------------------------------------------------------------------------%
:- end_module maybe.
%---------------------------------------------------------------------------%
