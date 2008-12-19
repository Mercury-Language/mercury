%------------------------------------------------------------------------------%
% test7.m
% <lmika@csse.unimelb.edu.au>
% Wed Sep 19 22:48:31 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module test7.

:- interface.

:- import_module io.

:- type bla_action
    --->    bla_validate
    ;       bla_retry
    ;       bla_throw.

:- pred main(io::di, io::uo) is cc_multi.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module thread.
:- import_module stm_builtin.
:- import_module univ.
:- import_module exception.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module private_builtin.

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    new_stm_var(123, TVar, IO0, IO0a),
    atomic [outer(IO0a, IO), inner(STM_BLA0, STM_BLA)] (
        atomic [outer(STM_BLA0, STM_BLA), inner(FLA0, FLA)] (
            FLA0 = FLA
        )
    ).
%     io.write_int(X, IO2, IO3),
%     io.write_int(Y, IO3, IO4),
%     io.write_int(Z, IO4, IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
