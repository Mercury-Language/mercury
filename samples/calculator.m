% A simpler calculator - parses and evaluates integer expressions.
% Elegant, but not efficient - backtracking causes exponential performance :-(.
% (Try this on the input "((((((((((((1))))))))))))".)

% For an example of a more efficient parser, see ../library/parser.m.

:- module calculator.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is nondet.

:- implementation.
:- import_module list, char, int, string.

:- type expr
	--->	number(int)
	;	plus(expr, expr)
	;       minus(expr, expr)
	;       times(expr, expr)
	;       div(expr, expr).

:- pred evalexpr(expr::in, int::out) is det.

evalexpr(number(NUM), NUM).
evalexpr(plus(X,Y),  Z) :- Z is A + B, evalexpr(X,A), evalexpr(Y,B).
evalexpr(minus(X,Y), Z) :- Z is A - B, evalexpr(X,A), evalexpr(Y,B).
evalexpr(times(X,Y), Z) :- Z is A * B, evalexpr(X,A), evalexpr(Y,B).
evalexpr(div(X,Y),   Z) :- Z is A // B, evalexpr(X,A), evalexpr(Y,B).

main --> 
	io__read_line(Res),
	( { Res = error(_) },
		io__write_string("Error reading from stdin\n")
	; { Res = eof },
		io__write_string("EOF\n")
	; { Res = ok(Line) },
		( { fullexpr(X,Line, []) } ->
			{ evalexpr(X,NUM)},
			io__write_int(NUM),
			io__write_string("\n")
		;
			io__write_string("Syntax error\n")
		),
		main	% recursively call itself for the next line(s)
	).

:- pred fullexpr( expr::out,	list(char)::in, list(char)::out) is nondet.
:- pred expr(     expr::out,	list(char)::in, list(char)::out) is nondet.
:- pred factor(   expr::out,	list(char)::in, list(char)::out) is nondet.
:- pred term(     expr::out,	list(char)::in, list(char)::out) is nondet.
:- pred const(list(char)::out,	list(char)::in, list(char)::out) is nondet.
:- pred digit(char::out,	list(char)::in, list(char)::out) is semidet.

fullexpr(X)		-->	expr(X), ['\n'].

expr(X)			-->	factor(X).
expr(plus(X,Y))  	-->	factor(X), ['+'], expr(Y).
expr(minus(X,Y))  	--> 	factor(X), ['-'], expr(Y).

factor(X)		-->	term(X).
factor(times(X,Y)) 	--> 	term(X), ['*'], factor(Y).
factor(div(X,Y)) 	--> 	term(X), ['/'], factor(Y).

term(number(N))		-->	const(L),
				{ string__from_char_list(L,S) },
				{ string__to_int(S,N) } .
term(X)			-->	['('], expr(X), [')'].

const([Char])		-->	digit(Char).
const([Char|L])		-->	digit(Char), const(L).

digit(Char)		-->	[Char], { char__is_digit(Char) }.
