%------------------------------------------------------------------------------%
% test10.m
% <lmika@csse.unimelb.edu.au>
% Thu Oct 11 12:39:01 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module test10.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

% :- import_module stm_builtin.
% :- import_module exception.
% :- import_module univ.
:- import_module int.
:- import_module string.

:- pred do_nothing_with_tvar(stm_var(T)::in) is det.

do_nothing_with_tvar(_).

:- pred retry_det(stm::ui) is det.

retry_det(STM) :-
    retry(STM).

main(IO0, IO) :-
    new_stm_var(123, TVar, IO0, IO1),
    atomic [outer(IO1, IO2), inner(A0, A)] (
        atomic [outer(A0, A1), inner(B0, B)] (
            X = 1,
            write_stm_var(TVar, X, B0, B),
            retry_det(B)
        or_else
            X = 2,
            B = B0
        or_else
            B0 = B1,
            X = 3,
            write_stm_var(TVar, 111, B1, B)
        ),
        read_stm_var(TVar, R, A1, A)
    ),
    io.write_string("X = " ++ string(X) ++ "\n", IO2, IO3),
    io.write_string("R = " ++ string(R) ++ "\n", IO3, IO).
