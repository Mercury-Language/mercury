%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% nondet_ite.m: test nondet if-then-else with nondet condition.

:- module nondet_ite.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    solutions(r, List),
    write_int_list(List, !IO).

:- pred q(int::out, int::out) is nondet.

q(X, Y) :-
    p(X),
    ( if some [Y1] p(Y1) then
        Y = Y1
    else
        Y = 42
    ).

:- pred p(int::out) is nondet.

p(0).
p(1).
p(2).

:- pred r(int::out) is nondet.

r(Z) :-
    q(X, Y),
    Z = X * 100 + Y.

:- pred write_int_list(list(int)::in, io::di, io::uo) is det.

write_int_list([], !IO).
write_int_list([X | Xs], !IO) :-
    io.write_int(X, !IO),
    io.nl(!IO),
    write_int_list(Xs, !IO).
