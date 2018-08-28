%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4
%---------------------------------------------------------------------------%

:- module chooser_tag_test.
:- interface.

:- import_module io.

:- type a
    --->    a(a1 :: int, a2 :: int).

:- type b
    --->    b(b1 :: int, b2 :: string).

:- type c
    --->    c(c1 :: string, c2 :: int).

:- type x
    --->    xa(xaf:: a)
    ;       xb(xbf:: b)
    ;       xc(xcf:: c)
    ;       xd
    ;       xe.

:- pred wrap_a(a::in, x::out) is det.
:- pred wrap_b(b::in, x::out) is det.
:- pred wrap_c(c::in, x::out) is det.

:- pred unwrap_a(x::in, a::out) is semidet.
:- pred unwrap_b(x::in, b::out) is semidet.
:- pred unwrap_c(x::in, c::out) is semidet.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module solutions.
:- import_module string.

wrap_a(A, xa(A)).
wrap_b(B, xb(B)).
wrap_c(C, xc(C)).

unwrap_a(xa(A), A).
unwrap_b(xb(B), B).
unwrap_c(xc(C), C).

main(!IO) :-
    test_wraps(!IO),
    test_solutions(30, !IO),
    test_solutions(130, !IO).

:- pred test_wraps(io::di, io::uo) is det.

test_wraps(!IO) :-
    list.foldl(test_wrap_a, [a(10, 11), a(12, 20)], !IO),
    list.foldl(test_wrap_b, [b(10, "eleven"), b(12, "twenty")], !IO),
    list.foldl(test_wrap_c, [c("ten", 11), c("twelve", 20)], !IO).

:- pred test_wrap_a(a::in, io::di, io::uo) is det.

test_wrap_a(A0, !IO) :-
    wrap_a(A0, X),
    ( if unwrap_a(X, A1_Prime) then
        MaybeA1 = yes(A1_Prime)
    else
        MaybeA1 = no
    ),
    io.write_string("test_wrap_a: A0 = ", !IO),
    io.write(A0, !IO),
    io.write_string(", X = ", !IO),
    io.write(X, !IO),
    io.write_string(", A1 = ", !IO),
    (
        MaybeA1 = yes(A1),
        io.write(A1, !IO)
    ;
        MaybeA1 = no,
        io.write_string("unwrap failed", !IO)
    ),
    io.nl(!IO).

:- pred test_wrap_b(b::in, io::di, io::uo) is det.

test_wrap_b(B0, !IO) :-
    wrap_b(B0, X),
    ( if unwrap_b(X, B1_Prime) then
        MaybeB1 = yes(B1_Prime)
    else
        MaybeB1 = no
    ),
    io.write_string("test_wrap_b: B0 = ", !IO),
    io.write(B0, !IO),
    io.write_string(", X = ", !IO),
    io.write(X, !IO),
    io.write_string(", B1 = ", !IO),
    (
        MaybeB1 = yes(B1),
        io.write(B1, !IO)
    ;
        MaybeB1 = no,
        io.write_string("unwrap failed", !IO)
    ),
    io.nl(!IO).

:- pred test_wrap_c(c::in, io::di, io::uo) is det.

test_wrap_c(C0, !IO) :-
    wrap_c(C0, X),
    ( if unwrap_c(X, C1_Prime) then
        MaybeC1 = yes(C1_Prime)
    else
        MaybeC1 = no
    ),
    io.write_string("test_wrap_c: C0 = ", !IO),
    io.write(C0, !IO),
    io.write_string(", X = ", !IO),
    io.write(X, !IO),
    io.write_string(", C1 = ", !IO),
    (
        MaybeC1 = yes(C1),
        io.write(C1, !IO)
    ;
        MaybeC1 = no,
        io.write_string("unwrap failed", !IO)
    ),
    io.nl(!IO).

:- pred test_solutions(int::in, io::di, io::uo) is det.

test_solutions(N, !IO) :-
    solutions(get_solutions(N), Solns),
    io.format("solns for %d = ", [i(N)], !IO),
    io.write(Solns, !IO),
    io.nl(!IO).

:- pred get_solutions(int::in, x::out) is nondet.

get_solutions(N, X) :-
    ( get_solutions_a(N, X)
    ; get_solutions_b(N, X)
    ; get_solutions_c(N, X)
    ).

:- pred get_solutions_a(int::in, x::out) is nondet.

get_solutions_a(N, X) :-
    N < 100,
    ( X = xa(a(N, N))
    ; X = xa(a(N+1, N+1))
    ).

:- pred get_solutions_b(int::in, x::out) is nondet.

get_solutions_b(N, X) :-
    N < 100,
    ( X = xb(b(N, "b2"))
    ; X = xb(b(N+1, "b2"))
    ).

:- pred get_solutions_c(int::in, x::out) is nondet.

get_solutions_c(N, X) :-
    N < 100,
    ( X = xc(c("c1", N))
    ; X = xc(c("c1", N+1))
    ).
