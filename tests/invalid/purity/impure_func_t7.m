
:- module impure_func_t7.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require.

:- pragma promise_pure(main/2).

:- type blah ---> blah(foo :: int).

main -->
	{ impure X = get_counter(4) },
	print("X = "), 
	print(X), 
	{ impure Z = ( X = 3 -> 4 ; 5 ) },
	print("Z = "), 
	print(Z), 
	{ impure L = (pred(X5::out) is det :- X5 = 4) },
	{ L(P) },
	print("P = "), 
	print(P), 
	{ impure M = blah(7) ^ foo },
	print("M = "), 
	print(M), 
	{ impure B = 4 },
	print(B), 
	nl.

:- impure func get_counter(int) = int.
:- impure pred some_pred(int::in, int::out) is det.

:- pragma c_header_code("extern Integer counter;").
:- pragma c_code("Integer counter = 0;").
:- pragma c_code(get_counter(Y::in) = (X::out), will_not_call_mercury,
	"X = counter + Y;").
get_counter(X) = X.

:- pragma c_code(some_pred(Y::in, X::out), will_not_call_mercury,
	"X = counter + Y;").
some_pred(X, X).
