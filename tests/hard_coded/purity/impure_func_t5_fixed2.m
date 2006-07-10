:- module impure_func_t5_fixed2.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require, list.

:- type foo ---> foo(impure func(int) = int).

:- pragma promise_pure(main/2).

main -->
	{ X = get_counter },
	print("X(4) = "),
	{ impure X4 = impure_apply(X,4) },
	print(X4), nl,
	print("X(4) = "),
	{ impure X4b = impure_apply(X,4) },
	print(X4b), nl.

:- impure func get_counter(int) = int is det.

:- pragma foreign_decl("C", "extern Integer counter;").
:- pragma foreign_code("C", "Integer counter = 42;").
:- pragma foreign_proc("C",
	get_counter(Y::in) = (X::out),
	[will_not_call_mercury],
"
	X = counter + Y; counter++;
").
get_counter(X) = X.
