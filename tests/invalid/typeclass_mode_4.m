:- module typeclass_mode_4.
:- interface.

:- pred p(T) <= c(T).

:- typeclass c(T) where [
	mode p(in) is det
].

:- implementation.

p(_).

