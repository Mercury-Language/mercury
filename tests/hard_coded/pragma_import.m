:- module pragma_import.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	foo(100, X, 200.0, Y, "Foo", S),
	print("X = "), print(X), nl,
	print("Y = "), print(Y), nl,
	print("S = "), print(S), nl,
	{ bar(X, X1) = X2 },
	print("X1 = "), print(X1), nl,
	print("X2 = "), print(X2), nl,
	{ bar2(X, XX1) = XX2 },
	print("XX1 = "), print(XX1), nl,
	print("XX2 = "), print(XX2), nl,
	( { baz(300, Y1) = Y2 } ->
		print("Y1 = "), print(Y1), nl,
		print("Y2 = "), print(Y2), nl
	;
		print("baz failed unexpectedly"), nl
	),
	( { baz(-300, _) = _ } ->
		print("baz succeeded unexpectedly"), nl
	;
		print("baz failed, as expected"), nl
	),
	( { quux(400, Z) } ->
		print("Z = "), print(Z), nl
	;
		print("quux failed unexpectedly"), nl
	),
	( { quux(-400, _) } ->
		print("quux succeeded unexpectedly"), nl
	;
		print("quux failed, as expected"), nl
	).

:- pred foo(int::in, int::out, float::in, float::out, string::in, string::out,
		io__state::di, io__state::uo) is det.
:- func bar(int::in, int::out) = (int::out) is det.
:- func bar2(int::in, int::out) = (int::out) is det.
:- func baz(int::in, int::out) = (int::out) is semidet.
:- pred quux(int::in, int::out) is semidet.

:- pragma import(foo(in, out, in, out, in, out, di, uo), "cfoo").
:- pragma import(bar(in, out) = out, will_not_call_mercury, "cbar").
:- pragma export(bar(in, out) = out, "mbar").
:- pragma import(bar2(in, out) = out, [may_call_mercury, thread_safe], "mbar").
:- pragma import(baz(in, out) = out, "cbaz").
:- pragma import(quux(in, out), may_call_mercury, "cquux").

:- pragma c_header_code("
	typedef MR_Integer Int;
	void cfoo(Int, Int *, MR_Float, MR_Float *, MR_String, MR_String *);
	Int cbar(Int, Int *);
	MR_bool cbaz(Int, Int *, Int *);
	MR_bool cquux(Int, Int *);
").

:- pragma c_code("

void cfoo(Int a1, Int *a2, MR_Float a3, MR_Float *a4, MR_String a5, MR_String *a6) {
	*a2 = a1 + 1;
	*a4 = a3 + 1.0;
	*a6 = a5;
}

Int cbar(Int a1, Int *a2) {
	*a2 = 1;
	return a1 + *a2;
}

MR_bool cbaz(Int a1, Int *a2, Int *a3) {
	*a2 = a1 + 1;
	*a3 = a1 + 2;
	return a1 + *a2 + *a3 > 0;
}

MR_bool cquux(Int a1, Int *a2) {
	*a2 = a1 + 1;
	return a1 + *a2 > 0;
}

").
