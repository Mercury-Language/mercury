:- module typeclass_mode.
:- interface.

:- typeclass c(T) where [
	mode p(in) is det
].

