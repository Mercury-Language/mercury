:- module test4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module int.
:- import_module exception.
:- import_module bool.

:- pred do_atomic(bool::out, io::di, io::uo) is det.

do_atomic(Y, IO, IO) :-
	atomic [outer(IO0, IO0), inner(STM0, STM)] (
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
