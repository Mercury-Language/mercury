% generated: 7 March 1990
% option(s): 
%
%   (deriv) times10
%
%   David H. D. Warren
%
%   symbolic derivatives

:- module deriv.

:- interface.

:- import_module int, io.

:- type expr --->	x
		;	num(int)
		;	expr + expr
		;	expr - expr
		;	expr * expr
		;	expr / expr
		;	- expr
		;	^(expr, int)
		;	log(expr)
		;	exp(expr)
		.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main4(expr, expr, expr, expr).
:- mode main4(out, out, out, out) is semidet.

:- implementation.

:- import_module prolog.

main -->
	( { main4(E1, E2, E3, E4) } ->
		print_expr(E1),
		io__write_string("\n\n"),
		print_expr(E2),
		io__write_string("\n\n"),
		print_expr(E3),
		io__write_string("\n\n"),
		print_expr(E4),
		io__write_string("\n")
	;
		[]
	).

:- pred times10(expr).
:- mode times10(out) is semidet.

:- pred log10(expr).
:- mode log10(out) is semidet.

:- pred ops8(expr).
:- mode ops8(out) is semidet.

:- pred divide10(expr).
:- mode divide10(out) is semidet.

:- pred d(expr, expr, expr).
:- mode d(in, in, out) is semidet.

:- pred print_expr(expr, io__state, io__state).
:- mode print_expr(in, di, uo) is det.

print_expr(x) -->
	io__write_string("x").
print_expr(num(N)) -->
	io__write_int(N).
print_expr(log(E)) -->
	io__write_string("log("),
	print_expr(E),
	io__write_string(")").
print_expr(exp(E)) -->
	io__write_string("exp("),
	print_expr(E),
	io__write_string(")").
print_expr(^(E, N)) -->
	io__write_string("^("),
	print_expr(E),
	io__write_string(", "),
	io__write_int(N),
	io__write_string(")").
print_expr(E1 + E2) -->
	io__write_string("("),
	print_expr(E1),
	io__write_string(" + "),
	print_expr(E2),
	io__write_string(")").
print_expr(E1 - E2) -->
	io__write_string("("),
	print_expr(E1),
	io__write_string(" + "),
	print_expr(E2),
	io__write_string(")").
print_expr(E1 * E2) -->
	io__write_string("("),
	print_expr(E1),
	io__write_string(" * "),
	print_expr(E2),
	io__write_string(")").
print_expr(E1 / E2) -->
	io__write_string("("),
	print_expr(E1),
	io__write_string(" / "),
	print_expr(E2),
	io__write_string(")").
print_expr(-E) -->
	io__write_string("- ("),
	print_expr(E),
	io__write_string(")").

main4(E1, E2, E3, E4) :-
	ops8(E1),
	divide10(E2),
	log10(E3),
	times10(E4).

times10(E) :-
	d(x * x * x * x * x * x * x * x * x * x * x, x, E).

log10(E) :-
	d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

ops8(E) :-
	d((x + num(1)) * ((^(x, 2) + num(2)) * (^(x, 3) + num(3))), x, E).

divide10(E) :-
	d(x / x / x / x / x / x / x / x / x / x / x, x, E).

d(U + V, X, DU + DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U - V, X, DU - DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / ^(V, 2)) :-
    d(U, X, DU),
    d(V, X, DV).
d(^(U, N), X, DU * num(N) * ^(U, N1)) :-
    N1 is N - 1,
    d(U, X, DU).
d(-U, X, -DU) :-
    d(U, X, DU).
d(exp(U), X, exp(U) * DU) :-
    d(U, X, DU).
d(log(U), X, DU / U) :-
    d(U, X, DU).
d(x, x, num(1)).
d(num(_), _, num(0)).
