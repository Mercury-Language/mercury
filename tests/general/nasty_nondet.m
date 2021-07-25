%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nasty_nondet.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    solutions(p(100), List),
    write_int_list(List, !IO).

:- pred p(int::in, int::out) is multi.

p(Z, Y) :-
    ( if some [X, R]
        (
            ( X = 1
            ; X = 2
            ),
            q(Z, R),
            X = R
        )
    then
        Y = X * X + R
    else
        Y = 42
    ).

:- pred q(int::in, int::out) is multi.

q(_, 0).
q(_, 1).
q(_, 2).

:- pred write_int_list(list(int)::in, io::di, io::uo) is det.

write_int_list([], !IO).
write_int_list([X | Xs], !IO) :-
    io.write_int(X, !IO),
    io.nl(!IO),
    write_int_list(Xs, !IO).
