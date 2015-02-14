%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that we can still introduce accumulators even though we
% don't initialise the base case to be the identity element for
% append.
%

:- module identity.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main -->
    io__write_string("r: "),
    { r([1, 10, 100], Reverse) },
    io__write(Reverse),
    io__nl.

:- pred r(list(int), list(int)).
:- mode r(in, out) is det.

r(X, R) :-
    X = [],
    R = [1000].
r(X, R) :-
    X = [H | T],
    r(T, R0),
    Tmp = [H],
    append(R0, Tmp, R).
