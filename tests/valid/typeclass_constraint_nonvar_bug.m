:- module typeclass_constraint_nonvar_bug.

:- interface.

:- typeclass eq(T) where [
	pred unify_oo(T::in, T::in) is semidet
].

:- typeclass add(T) <= eq(T) where [].
:- typeclass neq(T) <= eq(T) where [].
:- typeclass ord(T) <= eq(T) where [].
:- typeclass mult(T) <= add(T) where [].
:- typeclass sord(T) <= ord(T) where [].
:- typeclass strict(T) <= (neq(T),sord(T)) where [].
:- typeclass arith(T) <= (mult(T),strict(T)) where [].

:- typeclass solver(T) <= eq(T) where [].

:- typeclass solver_for(B, S) <= solver(S) where [].
:- typeclass lin_mult(B, S) <= arith(B) where [].
:- typeclass lin_arith_solver(B, S) <=
	(add(S), ord(S), solver(S), lin_mult(B, S), solver_for(B, S)) where [].
:- typeclass arith_solver(B, S) <= (arith(S), lin_arith_solver(B, S)) where [].

:- typeclass lin_int_solver(T) <= lin_arith_solver(int, T) where [].
:- typeclass int_solver(T) <= (arith(T), lin_int_solver(T)) where [].

:- instance eq(int).

:- instance add(int).
:- instance neq(int).
:- instance ord(int).
:- instance mult(int).
:- instance sord(int).
:- instance strict(int).
:- instance arith(int).

:- type cint ---> a ; b.

:- instance eq(cint).

:- instance add(cint).
:- instance neq(cint).
:- instance ord(cint).
:- instance mult(cint).
:- instance sord(cint).
:- instance strict(cint).
:- instance arith(cint).

:- instance solver(cint).

:- instance solver_for(int, cint).
:- instance lin_mult(int, cint).
:- instance lin_arith_solver(int, cint).
:- instance arith_solver(int, cint).

:- instance lin_int_solver(cint).
:- instance int_solver(cint).

:- type list(T) ---> [] ; [T | list(T)].
:- instance eq(list(T)).

:- type arc ---> arc(int, int).
:- instance eq(arc).

:- type graph ---> graph(list(arc)).
:- instance eq(graph).

:- pred int_unify_oo(int::in, int::in) is semidet.

:- pred cint_unify_oo(cint::in, cint::in) is semidet.

:- pred list_unify_oo(list(T)::in, list(T)::in) is semidet <= eq(T).

:- pred arc_unify_oo(arc::in, arc::in) is semidet.

:- pred graph_unify_oo(graph::in, graph::in) is semidet.

:- implementation.

:- instance add(int) where [].
:- instance neq(int) where [].
:- instance ord(int) where [].
:- instance mult(int) where [].
:- instance sord(int) where [].
:- instance strict(int) where [].
:- instance arith(int) where [].

:- instance eq(cint) where [
	pred(unify_oo/2) is cint_unify_oo
].

:- instance add(cint) where [].
:- instance neq(cint) where [].
:- instance ord(cint) where [].
:- instance mult(cint) where [].
:- instance sord(cint) where [].
:- instance strict(cint) where [].
:- instance arith(cint) where [].

:- instance solver(cint) where [].

:- instance solver_for(int, cint) where [].
:- instance lin_mult(int, cint) where [].
:- instance lin_arith_solver(int, cint) where [].
:- instance arith_solver(int, cint) where [].

:- instance lin_int_solver(cint) where [].
:- instance int_solver(cint) where [].

:- instance eq(list(T)) <= eq(T) where [
	pred(unify_oo/2) is list_unify_oo
].

:- instance eq(arc) where [
	pred(unify_oo/2) is arc_unify_oo
].

:- instance eq(graph) where [
	pred(unify_oo/2) is graph_unify_oo
].

int_unify_oo(X, X).

cint_unify_oo(X, X).

list_unify_oo([], []).
list_unify_oo([X | Xs], [Y | Ys]) :-
	unify_oo(X, Y),
	list_unify_oo(Xs, Ys).

arc_unify_oo(arc(X1, Y1), arc(X2, Y2)) :-
	unify_oo(X1, X2),
	unify_oo(Y1, Y2).

graph_unify_oo(graph(As1), graph(As2)) :-
	unify_oo(As1, As2).
