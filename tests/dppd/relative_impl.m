
:- module relative_impl.

:- interface.

:- type person
	--->	jap
	;	carol
	;	jonas
	;	maria
	;	paulina
	;	albertina
	;	peter
	;	mary
	;	jose
	;	anna
	;	john.

:- pred relative_john(person).
:- mode relative_john(in) is semidet.
:- mode relative_john(out) is nondet.

:- implementation.

relative_john(X) :- relative(john, X).

:- pred relative(person, person).
:- mode relative(in, in) is semidet.
:- mode relative(in, out) is nondet.
:- mode relative(out, out) is nondet.

relative(X,Y) :- ancestor(Z,X), ancestor(Z,Y).

:- pred ancestor(person, person).
:- mode ancestor(in, in) is semidet.
:- mode ancestor(in, out) is nondet.
:- mode ancestor(out, in) is nondet.
:- mode ancestor(out, out) is multi.

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).

:- pred parent(person, person).
:- mode parent(in, out) is nondet.
:- mode parent(in, in) is semidet.
:- mode parent(out, in) is nondet.
:- mode parent(out, out) is multi.

parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).

:- pred father(person, person).
:- mode father(in, out) is nondet.
:- mode father(out, out) is multi.
:- mode father(in, in) is semidet.
:- mode father(out, in) is semidet.

father(jap,carol).
father(jap,jonas).
father(jonas,maria).

:- pred mother(person, person).
:- mode mother(in, out) is nondet.
:- mode mother(out, out) is multi.
:- mode mother(in, in) is semidet.
:- mode mother(out, in) is semidet.

mother(carol,paulina).
mother(carol,albertina).
mother(albertina,peter).
mother(maria,mary).
mother(maria,jose).
mother(mary,anna).
mother(mary,john).

