:- module class_decl.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- typeclass foo(T) where [
	pred foo_method(T::in, io__state::di, io__state::uo) is det
].

:- typeclass bar(T, U) where [
	pred bar_method(T::in, U::in, io__state::di, io__state::uo) is det
].

:- type t1 ---> t1(int).
:- type t2(T) ---> t2a(T) ; t2b(T, T).

main -->
	foo_method("zero"),
	foo_method(t1(10)),
	foo_method(t2a(20)),
	foo_method(t2b(30, 40)),
	foo_method(t2b("thirty", "forty")),
	bar_method(11, 22),
	bar_method("eleven", 22),
	bar_method("eleven", "twentytwo"),
	bar_method(t1(111), 222),
	bar_method(t1(333), t2a(444)),
	bar_method(t1(333), t2b(444, 555)),
	bar_method(t1(888), t2b("sixsixsix", "sevensevenseven")).

:- instance foo(string) where [pred(foo_method/3) is foo_string].
:- instance foo(t1) where [pred(foo_method/3) is foo_t1].
:- instance foo(t2(U)) where [pred(foo_method/3) is foo_t2].

:- pred foo_string(string::in, io__state::di, io__state::uo) is det.
:- pred foo_t1(t1::in, io__state::di, io__state::uo) is det.
:- pred foo_t2(t2(V)::in, io__state::di, io__state::uo) is det.

foo_string(S) -->
	io__write_string("string: "),
	io__write_string(S),
	io__nl.

foo_t1(t1(I)) -->
	io__write_string("t1: "),
	io__write_int(I),
	io__nl.

foo_t2(t2a(I)) -->
	io__write_string("t2a: "),
	io__write(I),
	io__nl.
foo_t2(t2b(I1, I2)) -->
	io__write_string("t2b: "),
	io__write(I1),
	io__write_string(", "),
	io__write(I2),
	io__nl.

:- instance bar(int, int) where [pred(bar_method/4) is bar_int_int].
:- instance bar(string, int) where [pred(bar_method/4) is bar_str_int].
:- instance bar(string, string) where [pred(bar_method/4) is bar_str_str].
:- instance bar(t1, int) where [pred(bar_method/4) is bar_t1_int].
:- instance bar(t1, t2(U)) where [pred(bar_method/4) is bar_t1_t2].

:- pred bar_int_int(int::in, int::in,
	io__state::di, io__state::uo) is det.
:- pred bar_str_int(string::in, int::in,
	io__state::di, io__state::uo) is det.
:- pred bar_str_str(string::in, string::in,
	io__state::di, io__state::uo) is det.
:- pred bar_t1_int(t1::in, int::in,
	io__state::di, io__state::uo) is det.
:- pred bar_t1_t2(t1::in, t2(V)::in,
	io__state::di, io__state::uo) is det.

bar_int_int(I1, I2) -->
	io__write_string("ii: "),
	io__write_int(I1),
	io__write_string(", "),
	io__write_int(I2),
	io__nl.

bar_str_int(S1, I2) -->
	io__write_string("si: "),
	io__write_string(S1),
	io__write_string(", "),
	io__write_int(I2),
	io__nl.

bar_str_str(S1, S2) -->
	io__write_string("ss: "),
	io__write_string(S1),
	io__write_string(", "),
	io__write_string(S2),
	io__nl.

bar_t1_int(t1(I1), I2) -->
	io__write_string("t1int: "),
	io__write_int(I1),
	io__write_string(", "),
	io__write_int(I2),
	io__nl.

bar_t1_t2(t1(I1), t2a(I2)) -->
	io__write_string("t1t2a: "),
	io__write_int(I1),
	io__write_string(", "),
	io__write(I2),
	io__nl.
bar_t1_t2(t1(I1), t2b(I2, I3)) -->
	io__write_string("t1t2b: "),
	io__write_int(I1),
	io__write_string(", "),
	io__write(I2),
	io__write_string(", "),
	io__write(I3),
	io__nl.
