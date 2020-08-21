%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
%   poly_10
%
%   Ralph Haygood (based on Prolog version by Rick McGeer
%                  based on Lisp version by R. P. Gabriel)
%
%   raise a polynomial (1+x+y+z) to the 10th power (symbolically)

:- module poly.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module prolog.

main(!IO) :-
    test_poly(P),
    poly_exp(10, P, Out),
    print_poly(Out, !IO),
    io.nl(!IO).

:- type var
    --->    x
    ;       y
    ;       z.

:- type term
    --->    term(int, poly).

:- type poly
    --->    poly(var, list(term))
    ;       const(int).

:- pred test_poly1(poly::out) is det.

test_poly1(P) :-
    P = poly(x, [term(0, const(1)), term(1, const(1))]).

:- pred test_poly2(poly::out) is det.

test_poly2(P) :-
    P = poly(y, [term(1, const(1))]).

:- pred test_poly3(poly::out) is det.

test_poly3(P) :-
    P = poly(z, [term(1, const(1))]).

:- pred test_poly(poly::out) is det.

test_poly(P) :-
    poly_add(poly(x, [term(0, const(1)),
        term(1, const(1))]), poly(y, [term(1, const(1))]), Q),
    poly_add(poly(z, [term(1, const(1))]), Q, P).

:- pred poly_add(poly::in, poly::in, poly::out) is det.

poly_add(Poly1, Poly2, Result) :-
    (
        Poly1 = poly(Var1, Terms1),
        (
            Poly2 = poly(Var2, Terms2),
            ( if Var1 = Var2 then
                term_add(Terms1, Terms2, Terms),
                Result = poly(Var1, Terms)
            else if lt(Var1, Var2) then
                add_to_order_zero_term(Terms1, Poly2, Terms),
                Result = poly(Var1, Terms)
            else
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
            C = C1 + C2,
            Result = const(C)
        )
    ).

:- pred term_add(list(term)::in, list(term)::in, list(term)::out) is det.

term_add(List1, List2, Result) :-
    (
        List1 = [],
        Result = List2
    ;
        List1 = [term(E1, C1) | Terms1],
        (
            List2 = [],
            Result = List1
        ;
            List2 = [term(E2, C2) | Terms2],
            ( if E1 = E2 then
                poly_add(C1, C2, C),
                term_add(Terms1, Terms2, Terms),
                Result = [term(E1, C) | Terms]
            else if E1 < E2 then
                term_add(Terms1, List2, Terms),
                Result = [term(E1, C1) | Terms]
            else
                term_add(List1, Terms2, Terms),
                Result = [term(E2, C2) | Terms]
            )
        )
    ).

:- pred add_to_order_zero_term(list(term)::in, poly::in, list(term)::out)
    is det.

add_to_order_zero_term(List, C2, Result) :-
    ( if List = [term(0, C1) | Terms] then
        poly_add(C1, C2, C),
        Result = [term(0, C) | Terms]
    else
        Result = [term(0, C2) | List]
    ).

:- pred poly_exp(int::in, poly::in, poly::out) is det.

poly_exp(N, Poly, Result) :-
    ( if N = 0 then
        Result = const(1)
    else if poly_even(N) then
        M = N // 2,
        poly_exp(M, Poly, Part),
        poly_mul(Part, Part, Result)
    else
        M = N - 1,
        poly_exp(M, Poly, Part),
        poly_mul(Poly, Part, Result)
    ).

:- pred poly_mul(poly::in, poly::in, poly::out) is det.

poly_mul(Poly1, Poly2, Result) :-
    (
        Poly1 = poly(Var1, Terms1),
        (
            Poly2 = poly(Var2, Terms2),
            ( if Var1 = Var2 then
                term_mul(Terms1, Terms2, Terms),
                Result = poly(Var1, Terms)
            else if lt(Var1, Var2) then
                mul_through(Terms1, Poly2, Terms),
                Result = poly(Var1, Terms)
            else
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
            C = C1 * C2,
            Result = const(C)
        )
    ).

:- pred term_mul(list(term)::in, list(term)::in, list(term)::out) is det.

term_mul(List1, List2, Result) :-
    (
        List1 = [],
        Result = []
    ;
        List1 = [Term | Terms1],
        (
            List2 = [],
            Result = []
        ;
            List2 = [_ | _],
            single_term_mul(List2, Term, PartA),
            term_mul(Terms1, List2, PartB),
            term_add(PartA, PartB, Result)
        )
    ).

:- pred single_term_mul(list(term)::in, term::in, list(term)::out) is det.

single_term_mul(List, Term, Result) :-
    (
        List = [],
        Result = []
    ;
        List = [term(E1, C1) | Terms1],
        Term = term(E2, C2),
        E = E1 + E2,
        poly_mul(C1, C2, C),
        single_term_mul(Terms1, Term, Terms),
        Result = [term(E, C) | Terms]
    ).

:- pred mul_through(list(term)::in, poly::in, list(term)::out) is det.

mul_through(List, Poly, Result) :-
    (
        List = [],
        Result = []
    ;
        List = [term(E, Term) | Terms],
        poly_mul(Term, Poly, NewTerm),
        mul_through(Terms, Poly, NewTerms),
        Result = [term(E, NewTerm) | NewTerms]
    ).

:- pred lt(var::in, var::in) is semidet.

lt(x, y).
lt(y, z).
lt(x, z).

:- pred poly_even(int::in) is semidet.

poly_even(N) :-
    M = N // 2,
    N1 = M * 2,
    N = N1.

:- pred print_poly(poly::in, io::di, io::uo) is det.

print_poly(const(N), !IO) :-
    io.write_string("const(", !IO),
    io.write_int(N, !IO),
    io.write_string(")", !IO).
print_poly(poly(Var, Terms), !IO) :-
    io.write_string("poly(", !IO),
    print_var(Var, !IO),
    io.write_string(", ", !IO),
    print_terms(Terms, !IO),
    io.write_string(")", !IO).

:- pred print_var(var::in, io::di, io::uo) is det.

print_var(x, !IO) :-
    io.write_string("x", !IO).
print_var(y, !IO) :-
    io.write_string("y", !IO).
print_var(z, !IO) :-
    io.write_string("z", !IO).

:- pred print_terms(list(term)::in, io::di, io::uo) is det.

print_terms(Terms, !IO) :-
    (
        Terms = [],
        io.write_string("[]\n", !IO)
    ;
        Terms = [_ | _],
        io.write_string("[", !IO),
        print_terms_2(Terms, !IO),
        io.write_string("]", !IO)
    ).

:- pred print_terms_2(list(term)::in, io::di, io::uo) is det.

print_terms_2([], !IO).
print_terms_2([Term | Terms], !IO) :-
    print_term(Term, !IO),
    (
        Terms = []
    ;
        Terms = [_ | _],
        io.write_string(", ", !IO),
        print_terms_2(Terms, !IO)
    ).

:- pred print_term(term::in, io::di, io::uo) is det.

print_term(term(N, Poly), !IO) :-
    io.write_string("term(", !IO),
    io.write_int(N, !IO),
    io.write_string(", ", !IO),
    print_poly(Poly, !IO),
    io.write_string(")", !IO).
