%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module loopcheck_nondet.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module solutions.

:- pragma require_feature_set([memo]).

main(!IO) :-
    solutions(loop(2), Y),
    io.write(Y, !IO),
    io.write_string("\n", !IO).

:- pragma loop_check(loop/2).
:- pred loop(int::in, int::out) is nondet.

loop(X, Y) :-
    ( if X < 0 then
        fail
    else if X > 100 then
        fail
    else
        (
            Y = X
        ;
            loop(X - 2, Y)
        ;
            loop(X * 2, Y)
        )
    ).
