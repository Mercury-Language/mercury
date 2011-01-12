%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% A simpler calculator - parses and evaluates integer expressions.
%
% For an example of a parser with better error handling, see parser.m in
% the Mercury library source code.
%
% Author: fjh.
%
% This source file is hereby placed in the public domain.  -fjh.
%
%-----------------------------------------------------------------------------%

:- module calculator.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type expr
    --->    number(int)
    ;       plus(expr, expr)
    ;       minus(expr, expr)
    ;       times(expr, expr)
    ;       div(expr, expr).

main(!IO) :-
    io.write_string("calculator> ", !IO),
    io.flush_output(!IO),
    io.read_line(Res, !IO),
    (
        Res = error(_),
        io.write_string("Error reading from stdin\n", !IO)
    ;
        Res = eof,
        io.write_string("EOF\n", !IO)
    ;
        Res = ok(Line0),
        list.delete_all(Line0, ' ', Line),
        ( fullexpr(X, Line, []) ->
            Num = evalexpr(X),
            io.write_int(Num, !IO),
            io.write_string("\n", !IO)
        ;
            io.write_string("Syntax error\n", !IO)
        ),
        main(!IO) % recursively call ourself for the next line(s)
    ).

:- func evalexpr(expr) = int.

evalexpr(number(Num)) = Num.
evalexpr(plus(X, Y)) = evalexpr(X) + evalexpr(Y).
evalexpr(minus(X, Y)) = evalexpr(X) - evalexpr(Y).
evalexpr(times(X, Y)) = evalexpr(X) * evalexpr(Y).
evalexpr(div(X, Y)) = evalexpr(X) // evalexpr(Y).

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
    ( ['*'] -> term(Term2), factor2(times(Term, Term2), Factor)
    ; ['/'] -> term(Term2), factor2(div(  Term, Term2), Factor)
    ; { Factor = Term }
    ).

:- pred term(expr::out, list(char)::in, list(char)::out) is semidet.

term(Term) -->
    ( const(Const) ->
        { string.from_char_list(Const, ConstString) },
        { string.to_int(ConstString, Num) },
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
    { char.is_digit(Char) }.

%-----------------------------------------------------------------------------%
:- end_module calculator.
%-----------------------------------------------------------------------------%
