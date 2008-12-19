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



:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module int.
:- import_module string.

:- pred retry_det(stm::ui) is det.

retry_det(STM) :-
    retry(STM).

main(IO0, IO) :-
    new_stm_var(123, TVar, IO0, IO1),
    atomic [outer(IO1, IO2), inner(B0, B)] (
        X = 1,
        write_stm_var(TVar, X, B0, B),
        retry_det(B)
    or_else
        X = 2,
        B0 = B
    ),
    io.write_string("X = " ++ string(X) ++ "\n", IO2, IO).
    
