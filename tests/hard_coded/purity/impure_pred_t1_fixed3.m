
:- module impure_pred_t1_fixed3.

:- interface.

:- import_module io.

:- impure pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require, list.

:- type foo ---> foo(impure pred(int,int)).

main -->
	{ Y = foo(get_counter) },
	impure main2(Y).

:- impure pred main2(foo::in(bound(foo(pred(in,out) is det))),
	state::di, state::uo) is det.
main2(Y) -->
	{ Y = foo(X) },
	{ impure X(4, Z) },
	print("X = "), 
	print(Z), 
	nl.

:- impure pred get_counter(int::in, int::out) is det.

:- pragma c_header_code("extern Integer counter;").
:- pragma c_code("Integer counter = 0;").
:- pragma c_code(get_counter(Y::in, X::out), will_not_call_mercury, "X = counter + Y;").
get_counter(X, X).
