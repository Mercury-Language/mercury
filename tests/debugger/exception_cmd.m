%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exception_cmd.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.
:- import_module int.

main(!IO) :-
    test(42, X),
    io.print(X, !IO).

:- pred test(int::in, int::out) is det.

test(X, Y) :-
    ( if X > 0 then
        error("oops")
    else
        Y = X + 1
    ).
