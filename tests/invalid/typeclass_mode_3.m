:- module typeclass_mode_3.
:- interface.

:- pred p(T).

:- typeclass c(T) where [
	mode p(in) is det
].

:- implementation.

p(_).

