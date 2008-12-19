%------------------------------------------------------------------------------%
% test6.m
% <lmika@csse.unimelb.edu.au>
% Tue Sep 25 20:58:44 EST 2007
% vim: ft=mercury ff=unix ts=4 sw=4 et
%
%------------------------------------------------------------------------------%

:- module test6.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module stm_builtin.
:- import_module exception.
:- import_module bool.
:- import_module univ.

:- pred write_me({int, int}::in, io::di, io::uo) is det.

write_me(X, !IO) :-
    io.write(X, !IO).

main(IO0, IO) :-
    atomic [outer(IO0, IO1), inner(STM0, STM)] (
        STM0 = STM
    ),
    X = {1, 2},
    write_me(X, IO1, IO).
