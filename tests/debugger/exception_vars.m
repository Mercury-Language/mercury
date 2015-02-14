%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module exception_vars.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require.
:- import_module int.

main -->
    { test(42, X) },
    print(X).

:- pred test(int::in, int::out) is det.

test(X, Y) :-
    ( X > 0 ->
        error("oops")
    ;
        Y = X + 1
    ).
