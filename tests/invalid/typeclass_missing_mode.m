% test the case of a type class containing a predicate method with no modes

:- module typeclass_missing_mode.
:- interface.

:- typeclass c(T) where [
	pred p(T)		% error -- missing mode declaration for p/1
].
