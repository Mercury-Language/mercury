:- module pragma_non_term.

:- interface.

:- pred a is det.

:- implementation.

:- pragma does_not_terminate(a/0).

a :- true.

