:- module qual_is_test_imported.

:- interface.

:- import_module string.

:- pred is(string::in, string::out) is det.
:- pred foobar(string::in, string::out) is det.

:- implementation.

is(X, X).

foobar(Y, Y).
