:- module pl5_2_2.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred turing(t_tuple, state, list(p_tuple), t_tuple).
:- mode turing(in, in, in, out) is nondet.

:- type t_tuple	--->	t(list(sym), sym, list(sym)).
:- type p_tuple	--->	p(state, sym, state, sym, dir).

:- type sym	--->	blank ; nonblank.
:- type dir	--->	r ; l.

:- type	state	--->	halt ; nonhalt.

:- implementation.

turing(t(X, Y, Z), S, P, t(X, Y, Z)) :-
	member(p(S, Y, halt, _W, _D), P).
turing(t(X, Y, [R | L]), S, P, T) :-
	member(p(S, Y, S1, W, r), P),
	turing(t([W | X], R, L), S1, P, T).
turing(t(X, Y, []), S, P, T) :-
	member(p(S, Y, S1, W, r), P),
	turing(t([W | X], blank, []), S1, P, T).
turing(t([X | L], Y, R), S, P, T) :-
	member(p(S, Y, S1, W, l), P),
	turing(t(L, X, [W | R]), S1, P, T).
turing(t([], Y, R), S, P, T) :-
	member(p(S, Y, S1, W, l), P),
	turing(t([], blank, [W | R]), S1, P, T).

:- pred member(T, list(T)).
:- mode member(out, in) is nondet.

member(H, [H | _L]).
member(X, [_H | L]) :-
	member(X, L).
