%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module arg_permutation.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module string.

main(!IO) :-
    A = 3.0,
    B = 2,
    ( if test(A, B, 3.0, 4.0, "abc") then
        io.write_string("ok\n", !IO)
    else
        io.write_string("not ok\n", !IO)
    ).

:- pred test(int::in, float::in, float::in, string::in, float::in) is semidet.

test(A, B, C, D, E) :-
    A < 5,
    B < 5.0,
    C < 5.0,
    string.length(D) < 5,
    E < 5.0.
