:- module aditi_calls_mercury.

:- interface.

:- import_module aditi, list.
:- pred query(aditi.state, list(int)) is nondet.
:- mode query(aditi.aditi_mui ,out) is nondet.
:- pragma aditi(query/2).

:- implementation.

:- pragma aditi_no_memo(query/2).

:- import_module int.
:- import_module float.

query(DB,X ++ X) :-
	p(DB, X).

:- pred p(aditi__state, list(int)).
:- mode p(aditi_mui, out) is nondet.

:- pragma base_relation(p/2).

