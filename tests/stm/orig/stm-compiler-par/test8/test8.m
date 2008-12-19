%------------------------------------------------------------------------------%
% test8.m
% <lmika@csse.unimelb.edu.au>
% Sun Oct  7 21:34:39 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module test8.

:- interface.

:- import_module io.



:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module stm_builtin.
:- import_module exception.
:- import_module univ.
:- import_module thread.
:- import_module int.

%------------------------------------------------------------------------------%

:- pred goal1(stm_dummy_output::out, stm::di, stm::uo) is det.

goal1(stm_dummy_output, !STM) :-
    trace [io(!IO)] (
        io.write_string("I am alternative 1 ... retry", !IO),
        io.nl(!IO)
    ),
    retry(!.STM).


:- pred goal2(stm_dummy_output::out, stm::di, stm::uo) is det.

goal2(stm_dummy_output, !STM) :-
    trace [io(!IO)] (
        io.write_string("I am alternative 2", !IO),
        io.nl(!IO)
    ),
    retry(!.STM).



:- pred main_goal(stm_dummy_output::out, stm::di, stm::uo) is det.

main_goal(X, !STM) :-
    or_else(goal1, goal2, X, !STM).


% :- pred wait_for_a_while

main(!IO) :-
    io.write_string("Begin", !IO),
    delay_thread(10000),
    io.write_string("End", !IO),
    atomic_transaction(main_goal, _, !IO).

%-----------------------------------------------------------------------------%
%
:- pred delay_thread(int::in) is det.

delay_thread(X) :-
    delay_thread2(X, _).


:- pred delay_thread2(int::in, int::out) is det.

delay_thread2(X, Y) :-
    ( X = 0 ->
        Y = 0
    ;
        delay_thread3(50000, _),
        delay_thread2(X - 1, Y)
    ).


:- pred delay_thread3(int::in, int::out) is det.

delay_thread3(X, Y) :-
    ( X = 0 ->
        Y = 0
    ;
        delay_thread3(X - 1, Y)
    ).

   
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
