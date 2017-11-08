%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2010-2011 The University of Melbourne.
% Copyright (C) 2016 The Mercury Team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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

    % map_maybe(P, yes(Value0), yes(Value)) :- P(Value, Value).
    % map_maybe(_, no, no).
    %
:- pred map_maybe(pred(T, U), maybe(T), maybe(U)).
:- mode map_maybe(pred(in, out) is det, in, out) is det.
:- mode map_maybe(pred(in, out) is semidet, in, out) is semidet.
:- mode map_maybe(pred(in, out) is multi, in, out) is multi.
:- mode map_maybe(pred(in, out) is nondet, in, out) is nondet.

    % map_maybe(_, no) = no.
    % map_maybe(F, yes(Value)) = yes(F(Value)).
    %
:- func map_maybe(func(T) = U, maybe(T)) = maybe(U).

    % fold_maybe(_, no, Acc) = Acc.
    % fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).
    %
:- func fold_maybe(func(T, U) = U, maybe(T), U) = U.

    % fold_maybe(_, no, !Acc).
    % fold_maybe(P, yes(Value), !Acc) :- P(Value, !Acc).
    %
:- pred fold_maybe(pred(T, U, U), maybe(T), U, U).
:- mode fold_maybe(pred(in, in, out) is det, in, in, out) is det.
:- mode fold_maybe(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode fold_maybe(pred(in, di, uo) is det, in, di, uo) is det.
:- mode fold_maybe(pred(in, in, out) is semidet, in, in, out) is semidet.
:- mode fold_maybe(pred(in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode fold_maybe(pred(in, di, uo) is semidet, in, di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred fold2_maybe(pred(T, U, U, V, V), maybe(T), U, U, V, V).
:- mode fold2_maybe(pred(in, in, out, in, out) is det, in, in, out,
    in, out) is det.
:- mode fold2_maybe(pred(in, in, out, mdi, muo) is det, in, in, out,
    mdi, muo) is det.
:- mode fold2_maybe(pred(in, in, out, di, uo) is det, in, in, out,
    di, uo) is det.
:- mode fold2_maybe(pred(in, in, out, in, out) is semidet, in, in, out,
    in, out) is semidet.
:- mode fold2_maybe(pred(in, in, out, mdi, muo) is semidet, in, in, out,
    mdi, muo) is semidet.
:- mode fold2_maybe(pred(in, in, out, di, uo) is semidet, in, in, out,
    di, uo) is semidet.

    % map_fold_maybe(_, no, no, !Acc).
    % map_fold_maybe(P, yes(Value0), yes(Value), !Acc) :-
    %      P(Value, Value, !Acc).
    %
:- pred map_fold_maybe(pred(T, U, Acc, Acc), maybe(T), maybe(U), Acc, Acc).
:- mode map_fold_maybe(pred(in, out, in, out) is det, in, out,
    in, out) is det.
:- mode map_fold_maybe(pred(in, out, mdi, muo) is det, in, out,
    mdi, muo) is det.
:- mode map_fold_maybe(pred(in, out, di, uo) is det, in, out,
    di, uo) is det.
:- mode map_fold_maybe(pred(in, out, in, out) is semidet, in, out,
    in, out) is semidet.
:- mode map_fold_maybe(pred(in, out, mdi, muo) is semidet, in, out,
    mdi, muo) is semidet.
:- mode map_fold_maybe(pred(in, out, di, uo) is semidet, in, out,
    di, uo) is semidet.

    % As above, but with two accumulators.
    %
:- pred map_fold2_maybe(pred(T, U, Acc1, Acc1, Acc2, Acc2),
    maybe(T), maybe(U), Acc1, Acc1, Acc2, Acc2).
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
:- pred map_fold3_maybe(pred(T, U, Acc1, Acc1, Acc2, Acc2, Acc3, Acc3),
    maybe(T), maybe(U), Acc1, Acc1, Acc2, Acc2, Acc3, Acc3).
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

map_maybe(_, no, no).
map_maybe(P, yes(T0), yes(T)) :- P(T0, T).

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

fold_maybe(_, no, Acc) = Acc.
fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).

fold_maybe(_, no, !Acc).
fold_maybe(P, yes(Value), !Acc) :-
    P(Value, !Acc).

fold2_maybe(_, no, !Acc1, !Acc2).
fold2_maybe(P, yes(Value), !Acc1, !Acc2) :-
    P(Value, !Acc1, !Acc2).

map_fold_maybe(_, no, no, Acc, Acc).
map_fold_maybe(P, yes(T0), yes(T), Acc0, Acc) :-
    P(T0, T, Acc0, Acc).

map_fold2_maybe(_, no, no, !A, !B).
map_fold2_maybe(P, yes(T0), yes(T), !A, !B) :-
    P(T0, T, !A, !B).

map_fold3_maybe(_, no, no, !A, !B, !C).
map_fold3_maybe(P, yes(T0), yes(T), !A, !B, !C) :-
    P(T0, T, !A, !B, !C).

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
