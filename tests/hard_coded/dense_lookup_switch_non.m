:- module dense_lookup_switch_non.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions.

:- type foo
	--->	a
	;	b
	;	c
	;	d
	;	e
	;	f
	;	g
	;	h.

:- type bar
	--->	f1
	;	f2
	;	f3(int)
	;	f4(string)
	;	f5(float).

main(!IO) :-
	test(a, !IO),
	test(b, !IO),
	test(c, !IO),
	test(d, !IO),
	test(e, !IO),
	test(f, !IO),
	test(g, !IO),
	test(h, !IO).

:- pred test(foo::in, io::di, io::uo) is det.

test(Foo, !IO) :-
	solutions(p_tuple(Foo), Solns),
	io.write(Foo, !IO),
	io.write_string(" ->\n", !IO),
	io.write_list(Solns, "", write_tp, !IO),
	io.write_string("end\n\n", !IO).

:- type tp
	--->	tp(string, bar, float).

:- pred write_tp(tp::in, io::di, io::uo) is det.

write_tp(tp(Str, Bar, Float), !IO) :-
	io.write_string(Str, !IO),
	io.write_string(" ", !IO),
	io.write(Bar, !IO),
	io.write_string(" ", !IO),
	io.write_float(Float, !IO),
	io.nl(!IO).

:- pred p_tuple(foo::in, tp::out) is nondet.

p_tuple(Foo, Tuple) :-
	p(Foo, Str, Bar, Float),
	Tuple = tp(Str, Bar, Float).

:- pred p(foo::in, string::out, bar::out, float::out) is nondet.
:- pragma no_inline(p/4).

p(d, "four", f1, 4.4).
p(e, "five", f2, 5.5).
p(e, "five2", f3(5), 55.5).
p(f, "six", f4("hex"), 6.6).
p(g, "seven", f5(77.7), 7.7).
p(g, "seven2", f1, 777.7).
p(g, "seven3", f2, 7777.7).
