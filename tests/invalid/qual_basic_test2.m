:- module qual_basic_test2.

% A test to ensure a qualified predicate is parsed correctly.

:- interface.

:- import_module io.

:- pred qual_basic_test2.main(io, io).
:- mode qual_basic_test2.main(di, uo) is det.

:- implementation.

qual_basic_test2.main --> io.io__write_string("Gotcha!\n").

