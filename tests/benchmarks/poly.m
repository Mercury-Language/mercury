%   poly_10
%
%   Ralph Haygood (based on Prolog version by Rick McGeer
%                  based on Lisp version by R. P. Gabriel)
%
%   raise a polynomial (1+x+y+z) to the 10th power (symbolically)

:- module poly.

:- interface.

:- import_module int, io, list.

:- type poly__var --->	x ; y ; z.
:- type poly__term --->	term(int, poly).
:- type poly --->	poly(poly__var, list(poly__term)) ; const(int).

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main3(poly, io__state, io__state).
:- mode main3(out, di, uo) is det.

:- pred main1(poly).
:- mode main1(out) is det.

:- implementation.

main --> main3(_).

main3(Out) -->
	{ main1(Out) },
	print_poly(Out),
	io__write_string("\n").

main1(Out) :-
	test_poly(P),
	poly_exp(10, P, Out).

:- pred test_poly1(poly).
:- mode test_poly1(out) is det.

:- pred test_poly2(poly).
:- mode test_poly2(out) is det.

:- pred test_poly3(poly).
:- mode test_poly3(out) is det.

:- pred test_poly(poly).
:- mode test_poly(out) is det.

:- pred poly_add(poly, poly, poly).
:- mode poly_add(in, in, out) is det.

:- pred term_add(list(poly__term), list(poly__term), list(poly__term)).
:- mode term_add(in, in, out) is det.

:- pred add_to_order_zero_term(list(poly__term), poly, list(poly__term)).
:- mode add_to_order_zero_term(in, in, out) is det.

:- pred poly_exp(int, poly, poly).
:- mode poly_exp(in, in, out) is det.

:- pred poly_mul(poly, poly, poly).
:- mode poly_mul(in, in, out) is det.

:- pred term_mul(list(poly__term), list(poly__term), list(poly__term)).
:- mode term_mul(in, in, out) is det.

:- pred single_term_mul(list(poly__term), poly__term, list(poly__term)).
:- mode single_term_mul(in, in, out) is det.

:- pred mul_through(list(poly__term), poly, list(poly__term)).
:- mode mul_through(in, in, out) is det.

:- pred lt(poly__var, poly__var).
:- mode lt(in, in) is semidet.

:- pred even(int).
:- mode even(in) is semidet.

:- pred print_poly(poly, io__state, io__state).
:- mode print_poly(in, di, uo) is det.

:- pred print_var(poly__var, io__state, io__state).
:- mode print_var(in, di, uo) is det.

:- pred print_terms(list(poly__term), io__state, io__state).
:- mode print_terms(in, di, uo) is det.

:- pred print_terms_2(list(poly__term), io__state, io__state).
:- mode print_terms_2(in, di, uo) is det.

:- pred print_term(poly__term, io__state, io__state).
:- mode print_term(in, di, uo) is det.

print_poly(const(N)) -->
	io__write_string("const("),
	io__write_int(N),
	io__write_string(")").
print_poly(poly(Var, Terms)) -->
	io__write_string("poly("),
	print_var(Var),
	io__write_string(", "),
	print_terms(Terms),
	io__write_string(")").

print_var(x) -->
	io__write_string("x").
print_var(y) -->
	io__write_string("y").
print_var(z) -->
	io__write_string("z").

print_terms(Terms) -->
	( { Terms = [] } ->
		io__write_string("[]\n")
	;
		io__write_string("["),
		print_terms_2(Terms),
		io__write_string("]")
	).

print_terms_2([]) --> [].
print_terms_2([Term|Terms]) --> 
	print_term(Term),
	( { Terms = [] } ->
		[]
	;
		io__write_string(", "),
		print_terms_2(Terms)
	).

print_term(term(N, Poly)) -->
	io__write_string("term("),
	io__write_int(N),
	io__write_string(", "),
	print_poly(Poly),
	io__write_string(")").

test_poly1(P) :-
	P = poly(x, [term(0,const(1)), term(1,const(1))]).

test_poly2(P) :-
	P = poly(y, [term(1,const(1))]).

test_poly3(P) :-
	P = poly(z, [term(1,const(1))]).

test_poly(P) :-
	poly_add(poly(x, [term(0,const(1)), term(1,const(1))]), poly(y, [term(1, const(1))]), Q),
	poly_add(poly(z, [term(1,const(1))]), Q, P).


poly_add(Poly1, Poly2, Result) :-
	(
		Poly1 = poly(Var1, Terms1),
		(
			Poly2 = poly(Var2, Terms2),
			( Var1 = Var2 ->
				term_add(Terms1, Terms2, Terms),
				Result = poly(Var1, Terms)
			; lt(Var1, Var2) ->
				add_to_order_zero_term(Terms1, Poly2, Terms),
				Result = poly(Var1, Terms)
			;
				add_to_order_zero_term(Terms2, Poly1, Terms),
				Result = poly(Var2, Terms)
			)
		;
			Poly2 = const(_),
			add_to_order_zero_term(Terms1, Poly2, Terms),
			Result = poly(Var1, Terms)
		)
	;
		Poly1 = const(C1),
		(
			Poly2 = poly(Var2, Terms2),
			add_to_order_zero_term(Terms2, Poly1, Terms),
			Result = poly(Var2, Terms)
		;
			Poly2 = const(C2),
			C is C1 + C2,
			Result = const(C)
		)
	).

term_add(List1, List2, Result) :-
	(
		List1 = [],
		Result = List2
	;
		List1 = [term(E1,C1)|Terms1],
		(
			List2 = [],
			Result = List1
		;
			List2 = [term(E2,C2)|Terms2],
			( E1 = E2 ->
				poly_add(C1, C2, C),
				term_add(Terms1, Terms2, Terms),
				Result = [term(E1,C)|Terms]
			; E1 < E2 ->
				term_add(Terms1, List2, Terms),
				Result = [term(E1,C1)|Terms]
			;
				term_add(List1, Terms2, Terms),
				Result = [term(E2,C2)|Terms]
			)
		)
	).

add_to_order_zero_term(List, C2, Result) :-
	( List = [term(0,C1)|Terms] ->
		poly_add(C1, C2, C),
		Result = [term(0,C)|Terms]
	;
		Result = [term(0,C2)|List]
	).

poly_exp(N, Poly, Result) :-
	( N = 0 ->
		Result = const(1)
	; poly__even(N) ->
		M is N // 2,
		poly_exp(M, Poly, Part),
		poly_mul(Part, Part, Result)
	;
		M is N - 1,
		poly_exp(M, Poly, Part),
		poly_mul(Poly, Part, Result)
	).

poly_mul(Poly1, Poly2, Result) :-
	(
		Poly1 = poly(Var1, Terms1),
		(
			Poly2 = poly(Var2, Terms2),
			( Var1 = Var2 ->
				term_mul(Terms1, Terms2, Terms),
				Result = poly(Var1, Terms)
			; lt(Var1, Var2) ->
				mul_through(Terms1, Poly2, Terms),
				Result = poly(Var1, Terms)
			;
				mul_through(Terms2, Poly1, Terms),
				Result = poly(Var2, Terms)
			)
		;
			Poly2 = const(_),
			mul_through(Terms1, Poly2, Terms),
			Result = poly(Var1, Terms)
		)
	;
		Poly1 = const(C1),
		(
			Poly2 = poly(Var2, Terms2),
			mul_through(Terms2, Poly1, Terms),
			Result = poly(Var2, Terms)
		;
			Poly2 = const(C2),
			C is C1 * C2,
			Result = const(C)
		)
	).

term_mul(List1, List2, Result) :-
	(
		List1 = [],
		Result = []
	;
		List1 = [Term|Terms1],
		(
			List2 = [],
			Result = []
		;
			List2 = [_|_],
			single_term_mul(List2, Term, PartA),
			term_mul(Terms1, List2, PartB),
			term_add(PartA, PartB, Result)
		)
	).

single_term_mul(List, Term, Result) :-
	(
		List = [],
		Result = []
	;
		List = [term(E1,C1)|Terms1],
		Term = term(E2,C2),
		E is E1 + E2,
		poly_mul(C1, C2, C),
		single_term_mul(Terms1, Term, Terms),
		Result = [term(E,C)|Terms]
	).

mul_through(List, Poly, Result) :-
	(
		List = [],
		Result = []
	;
		List = [term(E,Term)|Terms],
		poly_mul(Term, Poly, NewTerm),
		mul_through(Terms, Poly, NewTerms),
		Result = [term(E,NewTerm)|NewTerms]
	).

lt(x, y).
lt(y, z).
lt(x, z).

even(N) :-
	M is N // 2,
	N1 is M * 2,
	N = N1.
