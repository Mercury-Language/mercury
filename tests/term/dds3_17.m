:- module dds3_17.

:- interface.

:- type	expr	--->	true ; false ; or(expr, expr) ; and(expr, expr).

:- pred dis(expr::in) is semidet.
:- pred con(expr::in) is semidet.

:- implementation.

dis(or(B1, B2)) :-
	con(B1),
	dis(B2).
dis(B) :-
	con(B).

con(and(B1, B2)) :-
	dis(B1),
	con(B2).
con(B) :-
	bool(B).

:- pred bool(expr).
:- mode bool(in).

bool(true).
bool(false).
