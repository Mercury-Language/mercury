:- module typeclass_mode_2.
:- interface.

:- typeclass c(T) where [
	mode p(in) is det
].

:- implementation.

p(_).
