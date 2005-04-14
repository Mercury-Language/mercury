:- module instance_unconstrained_tvar.

:- interface.

:- import_module io, list.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass p(T) where [
	pred m(T, io__state, io__state),
	mode m(in, di, uo) is det
].

:- implementation.

:- instance p(list(T)) where [
	pred(m/3) is io__write
].

main -->
	m([1,2,3]),
	io__nl.
