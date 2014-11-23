%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006, 2010-2011 The University of Melbourne.
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

%---------------------------------------------------------------------------%

:- type maybe(T)
    --->    no
    ;       yes(T).

:- inst maybe(I)
    --->    no
    ;       yes(I).

:- inst maybe_yes(I)
    --->    yes(I).

:- type maybe_error
    --->    ok
    ;       error(string).

:- type maybe_error(T) == maybe_error(T, string).

:- type maybe_error(T, E)
    --->    ok(T)
    ;       error(E).

:- inst maybe_error(I)
    --->    ok(I)
    ;       error(ground).

:- inst maybe_error_ok(I)
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

    % fold_maybe(_, no, Acc) = Acc.
    % fold_maybe(F, yes(Value), Acc0) = F(Acc0).
    %
:- func fold_maybe(func(T, U) = U, maybe(T), U) = U.

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
    % This is useful as an argument to list.filter_map
    %
:- pred maybe_is_yes(maybe(T)::in, T::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

map_maybe(_, no, no).
map_maybe(P, yes(T0), yes(T)) :- P(T0, T).

map_maybe(_, no) = no.
map_maybe(F, yes(T)) = yes(F(T)).

fold_maybe(_, no, !Acc).
fold_maybe(P, yes(Value), !Acc) :- P(Value, !Acc).

fold_maybe(_, no, Acc) = Acc.
fold_maybe(F, yes(Value), Acc0) = F(Value, Acc0).

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

%---------------------------------------------------------------------------%
:- end_module maybe.
%---------------------------------------------------------------------------%
