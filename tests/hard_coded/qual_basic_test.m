:- module qual_basic_test.

% A test to ensure a qualified predicate is parsed correctly.

:- interface.

:- import_module io.

:- pred qual_basic_test:main(io__state::di, io__state::uo) is det.

:- implementation.

qual_basic_test:main --> io:io__write_string("Gotcha!\n").
