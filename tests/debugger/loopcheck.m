% Regression test, for making sure that tabling inserts meaningful contexts
% into the goal_infos of the goals it inserts into procedure bodies, for use
% by the debugger.

:- module loopcheck.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ loop(10) },
	io__write_string("Hello, world\n").

:- pragma loop_check(loop/1).
:- pred loop(int::in) is det.

loop(X) :-
	loop(X).
