%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
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

:- import_module int.
:- import_module io.

:- type expr
    --->    x
    ;       num(int)
    ;       expr + expr
    ;       expr - expr
    ;       expr * expr
    ;       expr / expr
    ;       - expr
    ;       power(expr, int)
    ;       log(expr)
    ;       exp(expr).

:- pred main(io::di, io::uo) is det.

:- pred main4(expr::out, expr::out, expr::out, expr::out) is semidet.

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

main4(E1, E2, E3, E4) :-
    ops8(E1),
    divide10(E2),
    log10(E3),
    times10(E4).

:- pred print_expr(expr::in, io::di, io::uo) is det.

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
print_expr(power(E, N)) -->
    io__write_string("power("),
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

:- pred times10(expr::out) is semidet.

times10(E) :-
    d(x * x * x * x * x * x * x * x * x * x * x, x, E).

:- pred log10(expr::out) is semidet.

log10(E) :-
    d(log(log(log(log(log(log(log(log(log(log(x)))))))))), x, E).

:- pred ops8(expr::out) is semidet.

ops8(E) :-
    d((x + num(1)) * ((power(x, 2) + num(2)) * (power(x, 3) + num(3))), x, E).

:- pred divide10(expr::out) is semidet.

divide10(E) :-
    d(x / x / x / x / x / x / x / x / x / x / x, x, E).

:- pred d(expr::in, expr::in, expr::out) is semidet.

d(U + V, X, DU + DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U - V, X, DU - DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U * V, X, DU * V + U * DV) :-
    d(U, X, DU),
    d(V, X, DV).
d(U / V, X, (DU * V - U * DV) / power(V, 2)) :-
    d(U, X, DU),
    d(V, X, DV).
d(power(U, N), X, DU * num(N) * power(U, N1)) :-
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
