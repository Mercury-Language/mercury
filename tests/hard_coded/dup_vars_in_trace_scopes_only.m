%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dup_vars_in_trace_scopes_only.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    p(1, 2, X),
    io.format("X = %d\n", [i(X)], !IO).

:- pred p(int::in, int::in, int::out) is det.

p(A, B, X) :-
    trace [io(!IO)] (
        ToPrint = A,
        io.format("A = %d\n", [i(ToPrint)], !IO)
    ),
    trace [io(!IO)] (
        ToPrint = B,
        io.format("B = %d\n", [i(ToPrint)], !IO)
    ),
    X = A + B,
    trace [io(!IO)] (
        ToPrint = X,
        io.format("X = %d\n", [i(ToPrint)], !IO)
    ).
