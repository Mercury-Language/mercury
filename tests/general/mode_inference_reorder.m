%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_inference_reorder.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

:- type expr
    --->    expr_number(int)
    ;       expr_plus(expr, expr)
    ;       expr_minus(expr, expr)
    ;       expr_times(expr, expr)
    ;       expr_div(expr, expr).

main(!IO) :-
    main2(!IO).

% This is separate from main/2 so that we can require the compiler
% to infer its mode.
main2(!IO) :-
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
        ( if full_expr(X, Line, []) then
            Num = eval_expr(X),
            io.write_int(Num, !IO),
            io.write_string("\n", !IO)
        else
            io.write_string("Syntax error\n", !IO)
        ),
        main(!IO)    % recursively call ourself for the next line(s)
    ).

:- func eval_expr(expr) = int.

eval_expr(expr_number(Num)) = Num.
eval_expr(expr_plus(X, Y)) =  eval_expr(X) + eval_expr(Y).
eval_expr(expr_minus(X, Y)) = eval_expr(X) - eval_expr(Y).
eval_expr(expr_times(X, Y)) = eval_expr(X) * eval_expr(Y).
eval_expr(expr_div(X, Y)) =   eval_expr(X) // eval_expr(Y).

% Simple recursive-descent parser for expressions.
% Note: this code uses the two terms "factor" and "term" ass backwards.
% People usually name the nonterminal for identifiers and parenthesized
% expressions "factor", but this parser calls it "term". On the other hand,
% people usually call one or more such entities connected by multiply and/or
% divide operations "terms", but this parser calls them "factors".

% :- pred full_expr(expr::out, list(char)::in, list(char)::out) is semidet.
full_expr(X) -->
    ord_expr(X),
    ['\n'].

% :- pred expr(expr::out, list(char)::in, list(char)::out) is semidet.
ord_expr(Expr, DCG0, DCG) :-
    expr2(Factor, Expr, DCG1, DCG),
    factor(Factor, DCG0, DCG1),
    true.

% :- pred expr2(expr::in, expr::out, list(char)::in, list(char)::out)
%   is semidet.
% :- pred expr2(expr::out(free), expr::out(free), list(char)::out(free),
%   list(char)::out(free)) is semidet.
expr2(Factor1, Expr) -->
    ( if [('+') `with_type` char] then
        factor(Factor2),
        expr2(expr_plus(Factor1, Factor2), Expr)
    else if [('-') `with_type` char] then
        factor(Factor2),
        expr2(expr_minus(Factor1, Factor2), Expr)
    else
        { Expr = Factor1 }
    ).

% :- pred factor(expr::out, list(char)::in, list(char)::out) is semidet.
factor(Factor) -->
    term(Term),
    factor2(Term, Factor).

% :- pred factor2(expr::in, expr::out, list(char)::in, list(char)::out)
%   is semidet.
factor2(Term1, Factor) -->
    ( if [('*') `with_type` char] then
        term(Term2),
        factor2(expr_times(Term1, Term2), Factor)
    else if [('/') `with_type` char] then
        term(Term2),
        factor2(expr_div(Term1, Term2), Factor)
    else
        { Factor = Term1 }
    ).

% :- pred term(expr::out, list(char)::in, list(char)::out) is semidet.
term(Term) -->
    ( if const(Const) then
        { string.from_char_list(Const, ConstString) },
        { string.to_int(ConstString, Num) },
        { Term = expr_number(Num) }
    else
        ['('], ord_expr(Term), [')']
    ).

% :- pred const(list(char)::out, list(char)::in, list(char)::out) is semidet.
const([Digit | Rest]) -->
    digit(Digit),
    ( if const(Const) then
        { Rest = Const }
    else
        { Rest = [] }
    ).

% :- pred digit(char::out, list(char)::in, list(char)::out) is semidet.
digit(Char) -->
    [Char],
    { char.is_digit(Char) }.
