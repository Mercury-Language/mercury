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

:- pred det_retry(stm::ui) is det.

det_retry(X) :-
    retry(X).

%------------------------------------------------------------------------------%

main(IO0, IO) :-
    new_stm_var(123, TVar, IO0, IO1),

    
    atomic [outer(IO1, IO), inner(STM0, STM)] (
            trace [io(!IO)] (
                io.write_string("I am branch 1.\n", !IO)
            ),
            write_stm_var(TVar, 123, STM0, STM),
            det_retry(STM)
        or_else
            trace [io(!IO)] (
                io.write_string("I am branch 2.\n", !IO)
            ),
            STM0 = STM,
            det_retry(STM)
        or_else
            trace [io(!IO)] (
                io.write_string("I am branch 3.\n", !IO)
            ),
            STM0 = STM
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
