:- module qual_basic_test.

% A test to ensure qualified predicates, function calls
% and higher-order constants are parsed correctly.

:- interface.

:- import_module io.

:- pred qual_basic_test:main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

qual_basic_test:main -->
	io:io__write_string("Gotcha1!\n"),
	{ A is qual_basic_test:test },
	{ X is int:(A + 2) },
	io:io__write_int(X),	
	io__write_string("\n"),
	{ Pred = int:int__max },
	{ call(Pred, 1, 2, Y) },
	io__write_int(Y),
	io__write_string("\n").

:- func test = int.

test = 2.
