:- module aditi_query.

:- interface.

:- import_module aditi.
:- mode query(aditi__aditi_mui, out) is nondet.
:- pragma aditi(query/2).

:- implementation.

:- pragma aditi_no_memo(query/2).

:- import_module int.
:- import_module float.
:- import_module list.

query(DB,X) :-
	p(DB, X).

:- pred p(aditi__state, int).
:- mode p(aditi_mui, out) is nondet.

:- pragma base_relation(p/2).

