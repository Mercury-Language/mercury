:- module qual_basic_test.

% A test to ensure qualified predicates, function calls
% and higher-order constants are parsed correctly.

:- interface.

:- import_module io.

:- pred qual_basic_test.main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module int.

qual_basic_test.main -->
	io.write_string("Gotcha1!\n"),
	{ A = qual_basic_test.test },
	{ X = int.(A + 2) },
	io.write_int(X),	
	io.write_string("\n"),
	{ Pred = int.max },
	{ call(Pred, 1, 2, Y) },
	io.write_int(Y),
	write_string("\n").

:- func test = int.

test = 2.
