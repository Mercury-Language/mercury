
:- module impure_func_t6.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require.

:- pragma promise_pure(main/2).


	% A test of functions with arguments.
main -->
	{ impure X = get_counter(4) },
	print("X = "), 
	print(X), 
	nl.

:- impure func get_counter(int) = int.
:- impure pred some_pred(int::in, int::out) is det.

:- pragma c_header_code("extern MR_Integer counter;").
:- pragma c_code("MR_Integer counter = 0;").
:- pragma c_code(get_counter(Y::in) = (X::out), will_not_call_mercury,
	"X = counter + Y;").
get_counter(X) = X.

:- pragma c_code(some_pred(Y::in, X::out), will_not_call_mercury,
	"X = counter + Y;").
some_pred(X, X).
