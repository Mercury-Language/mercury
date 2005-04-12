:- module operator_classname.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass +(T) where [
	pred p(T::in, io__state::di, io__state::uo) is det
].

:- implementation.

:- instance +(int) where [
	pred(p/3) is io__write_int
].

main --> foo(1), io__nl.


:- pred foo(T, io__state, io__state) <= +(T).
:- mode foo(in, di, uo) is det.

foo(X) --> p(X).
