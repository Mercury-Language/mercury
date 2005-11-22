% Various checks that impurity declarations are treated properly.
% XXX We miss a couple of things that we should warn about: see the XXXs below.
:- module purity_warnings.
:- interface.
:- import_module io.
:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, list, string.

main -->
	impure impure_pred1,
	impure impure_pred2,
	semipure semipure_pred,
	impure impure_method1a,
	impure impure_method2a,
	semipure semipure_method_a,
	impure impure_method1b,
	impure impure_method2b,
	semipure semipure_method_b,
	semipure io__write_string("main 1\n"),	% warn
	impure io__print("main 2\n").		% warn

:- impure pred impure_pred1(io__state::di, io__state::uo) is det. % warn
impure_pred1 -->
	io__write_string("impure_pred1\n").

:- impure pred impure_pred2(io__state::di, io__state::uo) is det. % warn
impure_pred2 -->
	io__write_string("impure_pred2\n"),
	{ semipure get_x(X) },
	print("X = "), print(X), nl.

:- semipure pred semipure_pred(io__state::di, io__state::uo) is det.
semipure_pred -->
	semipure io__write_string("semipure_pred1\n").		% warn

:- typeclass foo(IO) where [
	(impure pred impure_method1a(IO::di, IO::uo) is det),
	(impure pred impure_method1b(IO::di, IO::uo) is det),
	(impure pred impure_method2a(IO::di, IO::uo) is det),
	(impure pred impure_method2b(IO::di, IO::uo) is det),
	(semipure pred semipure_method_a(IO::di, IO::uo) is det),
	(semipure pred semipure_method_b(IO::di, IO::uo) is det)
].
:- instance foo(io) where [
	pred(impure_method1a/2) is impure_method1a_impl,
	pred(impure_method2a/2 )is impure_method2a_impl,
	pred(semipure_method_a/2 )is semipure_method_a_impl,
	(impure_method1b -->
		impure print("impure_method1b\n")),	% XXX should warn
	(impure_method2b -->
		io__write_string("impure_method2b\n"),
		{ semipure get_x(X) },
		print("X = "), print(X), nl),
	(semipure_method_b -->
		semipure print("semipure_method_b\n"))	% XXX should warn
].

:- impure pred impure_method1a_impl(io::di, io::uo) is det.
:- semipure pred impure_method2a_impl(io::di, io::uo) is det.
:- semipure pred semipure_method_a_impl(io::di, io::uo) is det.

impure_method1a_impl -->
	impure print("impure_method1a_impl\n").		% warn
impure_method2a_impl -->
	io__write_string("impure_method2a_impl\n"),
	{ semipure get_x(X) },
	print("X = "), print(X), nl.
semipure_method_a_impl -->
	semipure print("semipure_method_a_impl\n").	% warn

:- pragma c_header_code("extern int x;").
:- pragma c_code("int x = 0;").
:- pragma foreign_code("C#", "
static int x = 0;
").
:- pragma foreign_code("Java", "
static int x = 0;
").

:- impure pred set_x(int::in) is det.
:- pragma c_code(set_x(X::in), will_not_call_mercury, "x=X;" ).
:- pragma foreign_proc("C#", set_x(X::in), will_not_call_mercury, "x=X;" ).
:- pragma foreign_proc("Java", set_x(X::in), will_not_call_mercury, "x=X;" ).

:- impure pred incr_x is det.
:- pragma c_code(incr_x, will_not_call_mercury, "++x;" ).
:- pragma foreign_proc("C#", incr_x, will_not_call_mercury, "++x;" ).
:- pragma foreign_proc("Java", incr_x, will_not_call_mercury, "++x;" ).

:- semipure pred get_x(int::out) is det.
:- pragma promise_semipure(get_x/1).
:- pragma c_code(get_x(X::out), will_not_call_mercury, "X=x;").
:- pragma foreign_proc("C#", get_x(X::out), will_not_call_mercury, "X=x;").
:- pragma foreign_proc("Java", get_x(X::out), will_not_call_mercury, "X=x;").
