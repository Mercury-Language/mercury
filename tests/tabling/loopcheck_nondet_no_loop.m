%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that loopcheck isn't overzealous.

:- module loopcheck_nondet_no_loop.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(non(10), N),
    io.write(N, !IO),
    io.write_string("\n", !IO),
    solutions(mul(20), M),
    io.write(M, !IO),
    io.write_string("\n", !IO).

:- pred non(int::in, int::out) is nondet.
:- pragma loop_check(non/2).

non(A, B) :-
    ( if A < 0 then
        fail
    else
        (
            B = A
        ;
            B = A + 1
        ;
            A > 1,
            non(A / 2, B)
        )
    ).

:- pred mul(int::in, int::out) is nondet.
:- pragma loop_check(mul/2).

mul(A, B) :-
    ( if A < 0 then
        B = -1
    else
        (
            B = A
        ;
            B = A + 1
        ;
            A > 1,
            mul(A / 2, B)
        )
    ).
