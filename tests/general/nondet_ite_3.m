%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% nondet_ite_3.m: test nondet if-then-else with semidet condition.

:- module nondet_ite_3.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    solutions(q, List),
    write_int_list(List, !IO).

:- pred q(int::out) is nondet.

q(X) :-
    ( if semidet_fail then
        X = 41
    else
        (
            X = 42
        ;
            X = 43
        )
    ).

:- pred write_int_list(list(int)::in, io::di, io::uo) is det.

write_int_list([], !IO).
write_int_list([X | Xs], !IO) :-
    io.write_int(X, !IO),
    io.nl(!IO),
    write_int_list(Xs, !IO).
