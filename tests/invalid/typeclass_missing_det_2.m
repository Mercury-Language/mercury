% test the case of a type class mode declaration without any determinism

:- module typeclass_missing_det_2.
:- interface.
:- type dummy.

:- implementation.

:- typeclass c(T) where [
	pred p(T),
	mode p(in)		% error -- missing det declaration for p/1
].

:- type dummy ---> dummy.
