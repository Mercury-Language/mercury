% This test checks that the type checker is working.
%

:- module test2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.

main(!IO) :-
	io.write_string("I am main\n", !IO),
	not_main(!IO),
	io.write_string("From not_main: ", !IO).


:- pred not_main(io::di, io::uo) is det.

not_main(X, Y) :-
	stm_create_state(Z),
	atomic [outer((X, Z)), inner((S, T))] (
		trace [io(!IO)] (
			io.write_string("This is inside the STM goal\n", !IO)
		)
	),
	stm_drop_state(Z),
	Y = X.
