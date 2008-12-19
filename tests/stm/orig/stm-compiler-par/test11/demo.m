%------------------------------------------------------------------------------%
% demo.m
% <lmika@csse.unimelb.edu.au>
% Sun Oct 28 16:50:34 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module demo.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.

:- type account == int.

main(!IO) :-
    new_stm_var(0, AccA, !IO),
    new_stm_var(0, AccB, !IO),
    transfer(AccA, AccB, 123, !IO).

%-----------------------------------------------------------------------------%

:- pred transfer(stm_var(account)::in, stm_var(account)::in, int::in,
    io::di, io::uo) is cc_multi.

transfer(AccountA, AccountB, Amount, IO0, IO) :-
    atomic [outer(IO0, IO), inner(STM0, STM)] (
        withdraw(AccountA, Amount, STM0, STM1),
        deposit(AccountB, Amount, STM1, STM)
    ).

%------------------------------------------------------------------------------%

:- pred deposit(stm_var(account)::in, int::in, stm::di, stm::uo) is det.

:- pred withdraw(stm_var(account)::in, int::in, stm::di, stm::uo) is det.

deposit(TVar, X, !STM) :-
    write_stm_var(TVar, X, !STM).

withdraw(TVar, X, !STM) :-
    deposit(TVar, X, !STM).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
