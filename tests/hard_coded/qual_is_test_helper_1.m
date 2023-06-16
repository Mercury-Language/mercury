%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module qual_is_test_helper_1.

:- interface.

:- import_module string.

:- pred is(string::in, string::out) is det.
:- pred foobar(string::in, string::out) is det.

:- implementation.

is(X, X).

foobar(Y, Y).
