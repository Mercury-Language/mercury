%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test, for making sure that tabling inserts meaningful contexts
% into the goal_infos of the goals it inserts into procedure bodies, for use
% by the debugger.

:- module loopcheck.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    loop(10) ,
    io.write_string("Hello, world\n", !IO).

:- pragma loop_check(loop/1).
:- pred loop(int::in) is det.

loop(X) :-
    loop(X).
