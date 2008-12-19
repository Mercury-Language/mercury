%------------------------------------------------------------------------------%
% test9.m
% <lmika@csse.unimelb.edu.au>
% Sat Oct  6 16:53:50 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module test9.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module univ.
:- import_module stm_builtin.
:- import_module exception.
:- import_module list.

:- pred or_else1(stm_var(int)::in, int::out, stm::di, stm::uo) is det.

or_else1(TVar, X, !STM) :-
    X = 1,
    write_stm_var(TVar, X, !STM),
    retry(!.STM).

:- pred or_else2(stm_var(int)::in, int::out, stm::di, stm::uo) is det.

or_else2(TVar, X, !STM) :-
    X = 5.

:- pred atomic(stm_var(int)::in, int::out, stm::di, stm::uo) is det.

atomic(TVar, X, !STM) :-
    or_else(or_else1(TVar), or_else2(TVar), X, !STM).

:- pred det_retry(stm::ui) is det.

det_retry(Stm) :-
    retry(Stm).

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    new_stm_var(123, TVar, IO0, IO1),

    atomic [outer(IO1, IO), inner(STM0, STM)] (
        X = 1,
        STM = STM0,
        det_retry(STM)
    or_else
        X = 5,
        write_stm_var(TVar, X, STM0, STM)
    ).
%     atomic_transaction(atomic(TVar), _, IO1, IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
