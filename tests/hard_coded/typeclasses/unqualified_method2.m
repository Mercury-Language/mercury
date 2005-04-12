:- module unqualified_method2.

:- interface.

:- import_module io.
:- import_module unqualified_method3.

:- typeclass class(T) where [
		pred print_modified(T, io__state, io__state),
		mode print_modified(in, di, uo) is det
	].

:- instance class(int).

:- implementation.

:- instance class(int) where [
		pred(print_modified/3) is print_modified_int
	].	

