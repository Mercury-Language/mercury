:- module test1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module int.

main(IO0, IO) :-
	X = 143,
	atomic [outer((IO0, IO1)), inner((STM0, STM))]
	(
		X = 2,
		Y = X,
		trace [io(!BLA)]
		(
			io.write_string("Just a test\n", !BLA),
			io.write_int(Y, !BLA),
			io.nl(!BLA)
		)
	),
	IO0 = IO1,
	io.write_string("Hello world\n", IO1, IO2),
	io.write_int(X, IO2, IO3),
	io.nl(IO3, IO).
