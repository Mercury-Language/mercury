%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% From orig/stm-compiler/test4
%
% Tests that an predicate argument with mode out can be bound within a
% atomic scope.
%

:- module basic2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module int.
:- import_module exception.
:- import_module bool.

:- pred do_atomic(bool::out, io::di, io::uo) is det.

do_atomic(Y, IO0, IO1) :-
    atomic [outer(IO0, IO1), inner(STM, STM)] (
        Y = yes
    ).

main(!IO) :-
    do_atomic(K, !IO),
    (
        K = yes,
        Y = 1
    ;
        K = no,
        Y = 2
    ),
    io.write_int(Y, !IO),
    io.nl(!IO).
