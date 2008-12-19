:- module test1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stm_builtin.
:- import_module int.

main(IO0, IO) :-
	X = 2,
	atomic [outer((IO0, IO1)), inner((STM0, STM))]
	(
		trace [io(!BLA)] (
			io.write_string("This is inside the atomic goal\n", 
				!BLA)
		)
	),
	io.write_string("Hello world\n", IO0, IO).
