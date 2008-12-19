% From orig/stm-compiler/test5
%
% Tests that an exception thrown from within an atomic scope propogates
% outside it.
%

:- module exception_propogate.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module stm_builtin.
:- import_module univ.
:- import_module int.

:- pred call_err is det.

call_err :-
	throw(123).

main(IO0, IO) :-
	atomic [outer(IO0, IO), inner(STM, STM)]
	(
		call_err
	).
