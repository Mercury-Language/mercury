%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether --show-pred-movability works.
%
%---------------------------------------------------------------------------%

:- module show_movability.
:- interface.

:- pred pred_a(int::in, int::out) is det.
:- pred pred_b(int::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

pred_a(X, Y) :-
    pred_c(X, Y).

pred_b(X, Y) :-
    Y = X.

:- pred pred_c(int::in, int::out) is det.

pred_c(X, Y) :-
    Y = 2 * X.

%---------------------------------------------------------------------------%

