:- module missing_instance_decl.

:- interface.

:- typeclass foo(A) where [
	pred b(A::in) is semidet
].

:- instance foo(int).
