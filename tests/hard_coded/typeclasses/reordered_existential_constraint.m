
:- module reordered_existential_constraint.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> { foobar }, io__write_string("Hi!\n").

:- typeclass c(T) where [].

:- instance c(int) where [].

%:- pred q(T).
:- pred q(T) <= c(T).
:- mode q(in) is det.

q(_).

%:- some [T] pred p(T).
:- some [T] pred p(T) => c(T).
:- mode p(out) is det.

p(1).

:- pred foobar is det.

foobar :-
	q(X),	% XXX polymorphism aborts here, looking for the variable that
		% contains the type class info for c(T).
	p(X).
