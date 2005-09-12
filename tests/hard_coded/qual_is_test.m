:- module qual_is_test.

% A test to ensure parsing of the functor `is/2' is done correctly.

:- interface.

:- import_module qual_is_test_imported, io, list, string.

:- pred qual_is_test.main(io.state::di, io.state::uo) is det.

:- implementation.

qual_is_test.main -->
	io.write_string(W),
	{ is("Hi!.\n", W) }.
