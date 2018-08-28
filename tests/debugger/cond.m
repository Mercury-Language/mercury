%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module cond.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.
:- import_module maybe.

:- type t
    --->    empty
    ;       node(t, int, t).

main(!IO) :-
    test_maybe(!IO),
    test_maybe(!IO),
    test_maybe(!IO),
    test_maybe(!IO),
    test_string(!IO),
    test_string(!IO),
    test_tree(!IO),
    test_tree(!IO),
    test_tree(!IO),
    test_tree(!IO),
    test_tree(!IO).

:- pred test_maybe(io::di, io::uo) is det.

test_maybe(!IO) :-
    p(no, A),
    p(yes(2), B),
    p(yes(3), C),
    io.write([A, B, C], !IO),
    io.nl(!IO).

:- pred test_string(io::di, io::uo) is det.

test_string(!IO) :-
    q("abc", A),
    io.write_string(A, !IO),
    io.nl(!IO),
    q("def", B),
    io.write_string(B, !IO),
    io.nl(!IO),
    q("ghi", C),
    io.write_string(C, !IO),
    io.nl(!IO).

:- pred test_tree(io::di, io::uo) is det.

test_tree(!IO) :-
    r(1, A),
    s(A, AA),
    io.write(AA, !IO),
    io.nl(!IO),
    r(2, B),
    s(B, BB),
    io.write(BB, !IO),
    io.nl(!IO).

:- pred p(maybe(int)::in, maybe(int)::out) is det.

p(X, Y) :-
    (
        X = no,
        Y = no
    ;
        X = yes(Z),
        Y = yes(Z + 1)
    ).

:- pred q(string::in, string::out) is det.

q(X, Y) :-
    ( if X = "abc" then
        Y = "xabcx"
    else if X = "def" then
        Y = "ydefy"
    else
        Y = "else"
    ).

:- pred r(int::in, t::out) is det.

r(X, Y) :-
    ( if X = 0 then
        Y = empty
    else
        r(X - 1, S),
        Y = node(S, X, S)
    ).

:- pred s(t::in, t::out) is det.

s(X, X).
