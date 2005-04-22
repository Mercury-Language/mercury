% test the case of a type class pred-mode declaration without any determinism

:- module typeclass_missing_det.
:- interface.
:- type dummy.

:- implementation.

:- typeclass c(T) where [
	pred p(T::in)		% error -- missing det declaration for p/1
].

:- type dummy ---> dummy.
