:- module pl8_4_2.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- type token	--->	a ; b ; c ; plus ; times ; lparen ; rparen.

:- pred e(list(token)::in, list(token)::out) is nondet.

:- implementation.

e(L, T) :- t(L, T).
e(L, T) :- t(L, [plus | C]), e(C, T).

:- pred t(list(token), list(token)).
:- mode t(in, out).

t(L, T) :- n(L, T).
t(L, T) :- n(L, [times | C]), t(C, T).

:- pred n(list(token), list(token)).
:- mode n(in, out).

n([L | T], T) :-
	z(L).
n([lparen | A], B) :-
	e(A, [rparen | B]).

:- pred z(token).
:- mode z(in).

z(a).
z(b).
z(c).
