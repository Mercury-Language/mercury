
:- module mode_inference_reorder.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, char, int, string.

:- type expr
	--->	expr_number(int)
	;	expr_plus(expr, expr)
	;       expr_minus(expr, expr)
	;       expr_times(expr, expr)
	;       expr_div(expr, expr).

main -->  main2.
main2 -->
	io__write_string("calculator> "),
	io__flush_output,
	io__read_line(Res),
	( { Res = error(_) },
		io__write_string("Error reading from stdin\n")
	; { Res = eof },
		io__write_string("EOF\n")
	; { Res = ok(Line0) },
		{ list__delete_all(Line0, ' ', Line) },
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
evalexpr(expr_number(Num)) = Num.
evalexpr(expr_plus(X,Y)) = evalexpr(X) + evalexpr(Y).
evalexpr(expr_minus(X,Y)) = evalexpr(X) - evalexpr(Y).
evalexpr(expr_times(X,Y)) = evalexpr(X) * evalexpr(Y).
evalexpr(expr_div(X,Y)) = evalexpr(X) // evalexpr(Y).

% Simple recursive-descent parser.

% :- pred fullexpr(expr::out, list(char)::in, list(char)::out) is semidet.
fullexpr(X) -->
	ord_expr(X),
	['\n'].

% :- pred expr(expr::out, list(char)::in, list(char)::out) is semidet.
ord_expr(Expr, DCG0, DCG) :-
	expr2(Factor, Expr, DCG1, DCG),
	factor(Factor, DCG0, DCG1),
	true.

% :- pred expr2(expr::in, expr::out, list(char)::in, list(char)::out) is semidet.
% :- pred expr2(expr::out(free), expr::out(free), list(char)::out(free),
% 	list(char)::out(free)) is semidet.
expr2(Factor, Expr) -->
	( [('+') `with_type` char] -> factor(Factor2), expr2(expr_plus( Factor, Factor2), Expr)
	; [('-') `with_type` char] -> factor(Factor2), expr2(expr_minus(Factor, Factor2), Expr)
	; { Expr = Factor }
	).

% :- pred factor(expr::out, list(char)::in, list(char)::out) is semidet.
factor(Factor) -->
	term(Term),
	factor2(Term, Factor).

% :- pred factor2(expr::in, expr::out, list(char)::in, list(char)::out)
% 	is semidet.
factor2(Term, Factor) -->
	( [('*') `with_type` char] -> term(Term2), factor2(expr_times(Term,Term2), Factor)
	; [('/') `with_type` char] -> term(Term2), factor2(expr_div(  Term,Term2), Factor)
	; { Factor = Term }
	).

% :- pred term(expr::out, list(char)::in, list(char)::out) is semidet.
term(Term)	-->
	( const(Const) ->
		{ string__from_char_list(Const, ConstString) },
		{ string__to_int(ConstString, Num) },
		{ Term = expr_number(Num) }
	;
		['('], ord_expr(Term), [')']
	).

% :- pred const(list(char)::out, list(char)::in, list(char)::out) is semidet.
const([Digit|Rest]) -->
	digit(Digit),
	( const(Const) ->
		{ Rest = Const }
	;
		{ Rest = [] }
	).

% :- pred digit(char::out, list(char)::in, list(char)::out) is semidet.
digit(Char) -->
	[Char],
	{ char__is_digit(Char) }.


