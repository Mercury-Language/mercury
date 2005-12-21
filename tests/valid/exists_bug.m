:- module exists_bug.
:- interface.

:- typeclass bar(T) where [].

:- some [T] pred new_bar(T::out) is det => bar(T).

:- implementation.

:- instance bar(foo(S)) where [].
new_bar(T) :-
	% There was a bug here: polymorphism.m was attempting to construct
	% a type_info for the existentially quantified type variable of
	% new_foo/1.  It was doing this because there was a type_info_locn
	% (pointing inside the typeclass_info, which doesn't get constructed
	% until later).
	new_foo(T).

:- type foo(S) ---> f(S).
:- some [S] pred new_foo(foo(S)::out) is det.
new_foo(f(1)).

