:- module impure_method_impl.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- typeclass c(T) where [
	pred m1(T::in, int::out) is det,
	semipure pred m2(T::in, int::out) is det
].

:- type foo ---> foo.

:- instance c(foo) where [
	pred(m1/2) is foo_m1,
	pred(m2/2) is foo_m2
].

:- semipure pred foo_m1(foo::in, int::out) is det.
:- impure pred foo_m2(foo::in, int::out) is det.

:- implementation.

main -->
	[].

:- pragma c_header_code("int foo_counter = 0;").

:- pragma c_code(foo_m1(_F::in, Val::out), "Val = foo_counter;").
:- pragma c_code(foo_m2(_F::in, Val::out), "Val = foo_counter++;").
foo_m1(_, 0).
foo_m2(_, 0).
