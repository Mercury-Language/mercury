% Test for the context transformation (compiler/context.m).
:- module context_anc.

:- interface.

:- import_module aditi.

:- pred left_anc(aditi__state, int).
:- mode left_anc(aditi__aditi_mui, out) is nondet.
:- pragma aditi(left_anc/2).

:- pred right_anc(aditi__state, int, int).
:- mode right_anc(aditi__aditi_mui, in, out) is nondet.
:- pragma aditi(right_anc/3).

:- pred multi_anc(aditi__state, int, int).
:- mode multi_anc(aditi__aditi_mui, in, out) is nondet.
:- pragma aditi(multi_anc/3).

:- pred chain(aditi__state, int, int).
:- mode chain(aditi__aditi_mui, out, out) is nondet.
:- pragma base_relation(chain/3).

:- implementation.

:- pragma aditi_no_memo(left_anc/3).
left_anc(DB, Y) :-
	left_anc(DB, 2, Y).

:- pred left_anc(aditi__state, int, int).
:- mode left_anc(aditi__aditi_mui, in, out) is nondet.
:- pragma aditi(left_anc/3).
:- pragma aditi_no_memo(left_anc/3).
:- pragma context(left_anc/3).

left_anc(DB, X, Y) :-
	left_anc(DB, X, Z),
	chain(DB, Z, Y).
left_anc(DB, X, Y) :-
	chain(DB, X, Y).

:- pragma aditi_no_memo(right_anc/3).
:- pragma context(right_anc/3).

right_anc(DB, X, Y) :-
	chain(DB, X, Z),
	right_anc(DB, Z, Y).
right_anc(DB, X, Y) :-
	chain(DB, X, Y).

:- pragma aditi_no_memo(multi_anc/3).
:- pragma context(multi_anc/3).

multi_anc(DB, X, Y) :-
	multi_anc(DB, X, Z),
	multi_anc(DB, Z, Y).
multi_anc(DB, X, Y) :-
	chain(DB, X, Y).

