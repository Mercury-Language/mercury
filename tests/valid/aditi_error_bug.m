:- module aditi_error_bug.

:- interface.

:- import_module aditi.

:- pred query(aditi__state::aditi_mui, int::out) is nondet.
:- pragma aditi(query/2).

:- implementation.

:- import_module aditi_error_bug2.

query(DB, Number) :-
	murders(DB, "New York", "New York", 1995, Number).

