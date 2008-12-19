% From orig/stm-compiler/test2
%
% This test checks that the type checker is working.
% The parameters of outer in the atomic scope can be of type io
% or stm, but must both be the same. This test should
% therefore fail.
%

:- module outer_types.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.

main(!IO) :-
	io.write_string("I am main\n", !IO),
	not_main(!IO),
	io.write_string("From not_main: ", !IO).

:- pragma promise_pure(not_main/2).
:- pred not_main(io::di, io::uo) is det.

not_main(X, Y) :-
	impure stm_create_transaction_log(Z),
	atomic [outer(X, Z), inner(S, S)] (
		trace [io(!IO)] (
			io.write_string("This is inside the STM goal\n", !IO)
		)
	),
	impure stm_discard_transaction_log(Z),
	Y = X.
