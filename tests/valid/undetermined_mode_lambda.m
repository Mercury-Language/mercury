%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Any copyright is dedicated to the Public Domain.
% https://creativecommons.org/publicdomain/zero/1.0/
%
% Released by Transnat Games for testing purposes.
%
% Crashes with:
% Uncaught Mercury exception:
% Software Error: predicate `check_hlds.modecheck_unify.
% modecheck_unification_rhs_undetermined_mode_lambda'/8:
% Unexpected: expecting single call
%
%---------------------------------------------------------------------------%

:- module undetermined_mode_lambda.
:- interface.

:- use_module io.

:- pred main(io.io::di, io.io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- typeclass empty_typeclass(T) where [].

:- pred nothing(_::in) is det.

nothing(_).

% There must be multiple modes for this pred.
:- pred do(pred(X), X).
:- mode do(pred(in) is det, in) is det.
:- mode do(pred(out) is det, out) is det.

do(Pred, X) :- Pred(X).

% The pred must have a typeclass constraint, even if it isn't used in the pred.
:- pred crash(T::in) is det <= empty_typeclass(T).

crash(T) :-
    do(do(nothing), T).

%---------------------------------------------------------------------------%

main(!IO).
