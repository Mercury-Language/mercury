%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% nondet_ite_4.m: test nondet if-then-else with det `else' part
% followed by a test, in a context with a known failure cont.
% Same as nondet_ite_2, except that the call to p/1 has been
% manually inlined.
% (This is a regression test - the compiler used to fail this
% test case.)

:- module nondet_ite_4.
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
    (
        ( if some [Y1]
            ( Y1 = 0
            ; Y1 = 1
            ; Y1 = 2
            )
        then
            Y = Y1
        else
            Y = 42
        ),
        Y > 0
    ;
        Y = 44
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
