
:- module impure_func_t7.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require.

:- pragma promise_pure(main/2).

:- type blah ---> blah(foo :: int).

main(!IO) :-
	impure bad_impure_if_then_else_expr(!IO),
	impure bad_impure_lambda_unification(!IO),
	impure bad_impure_field_access(!IO),
	impure bad_impure_assignment(!IO).

:- impure pred bad_impure_if_then_else_expr(io::di, io::uo) is det.

bad_impure_if_then_else_expr(!IO) :-
	impure X = get_counter(4),
	print("X = ", !IO), 
	print(X, !IO), 
	impure Z = ( X = 3 -> 4 ; 5 ),
	print("Z = ", !IO), 
	print(Z, !IO), 
	nl(!IO).

:- impure pred bad_impure_lambda_unification(io::di, io::uo) is det.

bad_impure_lambda_unification(!IO) :-
	impure L = (pred(X5::out) is det :- X5 = 4),
	L(P),
	print("P = ", !IO), 
	print(P, !IO), 
	nl(!IO).

:- impure pred bad_impure_field_access(io::di, io::uo) is det.

bad_impure_field_access(!IO) :-
	impure M = blah(7) ^ foo,
	print("M = ", !IO), 
	print(M, !IO), 
	nl(!IO).

:- impure pred bad_impure_assignment(io::di, io::uo) is det.

bad_impure_assignment(!IO) :-
	impure B = 4,
	print(B, !IO), 
	nl(!IO).

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
