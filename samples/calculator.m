% A simpler calculator - parses and evaluates integer expressions.

% For an example of a parser with better error handling, see parser.m in
% the Mercury library source code.

% Author: fjh.

% This source file is hereby placed in the public domain.  -fjh.

:- module calculator.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, char, int, string.

:- type expr
	--->	number(int)
	;	plus(expr, expr)
	;       minus(expr, expr)
	;       times(expr, expr)
	;       div(expr, expr).

main --> 
	io__write_string("calculator> "),
	io__flush_output,
	io__read_line(Res),
	( { Res = error(_) },
		io__write_string("Error reading from stdin\n")
	; { Res = eof },
		io__write_string("EOF\n")
	; { Res = ok(Line) },
		( { fullexpr(X,Line,[]) } ->
			{ Num = evalexpr(X) },
			io__write_int(Num),
			io__write_string("\n")
		;
			io__write_string("Syntax error\n")
		),
		main	% recursively call ourself for the next line(s)
	).

:- func evalexpr(expr) = int.
evalexpr(number(Num)) = Num.
evalexpr(plus(X,Y)) = evalexpr(X) + evalexpr(Y).
evalexpr(minus(X,Y)) = evalexpr(X) - evalexpr(Y).
evalexpr(times(X,Y)) = evalexpr(X) * evalexpr(Y).
evalexpr(div(X,Y)) = evalexpr(X) // evalexpr(Y).

% Simple recursive-descent parser.

:- pred fullexpr(expr::out, list(char)::in, list(char)::out) is semidet.
fullexpr(X) -->
	expr(X),
	['\n'].

:- pred expr(expr::out, list(char)::in, list(char)::out) is semidet.
expr(Expr) -->
	factor(Factor),
	expr2(Factor, Expr).

:- pred expr2(expr::in, expr::out, list(char)::in, list(char)::out) is semidet.
expr2(Factor, Expr) -->
	( ['+'] -> factor(Factor2), expr2(plus( Factor, Factor2), Expr)
	; ['-'] -> factor(Factor2), expr2(minus(Factor, Factor2), Expr)
	; { Expr = Factor }
	).

:- pred factor(expr::out, list(char)::in, list(char)::out) is semidet.
factor(Factor) -->
	term(Term),
	factor2(Term, Factor).

:- pred factor2(expr::in, expr::out, list(char)::in, list(char)::out)
	is semidet.
factor2(Term, Factor) -->
	( ['*'] -> term(Term2), factor2(times(Term,Term2), Factor)
	; ['/'] -> term(Term2), factor2(div(  Term,Term2), Factor)
	; { Factor = Term }
	).

:- pred term(expr::out, list(char)::in, list(char)::out) is semidet.
term(Term)	-->
	( const(Const) ->
		{ string__from_char_list(Const, ConstString) },
		{ string__to_int(ConstString, Num) },
		{ Term = number(Num) }
	;
		['('], expr(Term), [')']
	).

:- pred const(list(char)::out, list(char)::in, list(char)::out) is semidet.
const([Digit|Rest]) -->
	digit(Digit),
	( const(Const) ->
		{ Rest = Const }
	;
		{ Rest = [] }
	).

:- pred digit(char::out, list(char)::in, list(char)::out) is semidet.
digit(Char) -->
	[Char],
	{ char__is_digit(Char) }.
