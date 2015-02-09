% XXX we issue a poor error message for this example

:- module impure_func_t5_fixed.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require, list.

:- type foo ---> foo(impure func(int) = int).

:- pragma promise_pure(main/2).

main -->
	{ Y = foo(get_counter) },
	{ Y = foo(X) },
	print("X(4) = "), 
	{ X4 = impure_apply(X,4) }, % missing `impure'
	print(X4), 
	nl.

:- impure func get_counter(int) = int is det.

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").
:- pragma foreign_proc("C",
	get_counter(Y::in) = (X::out),
	[will_not_call_mercury],
"
	X = counter + Y;
").

get_counter(X) = X.
