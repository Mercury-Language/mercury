%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module string_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    test("a", !IO),
    test("b", !IO),
    test("c", !IO),
    test("aa", !IO),
    test("ab", !IO),
    test("ac", !IO),
    test("ba", !IO),
    test("bb", !IO),
    test("bc", !IO),
    test("ca", !IO),
    test("cb", !IO),
    test("cc", !IO).

:- pred test(string::in, io::di, io::uo) is det.

test(S, !IO) :-
    ( p(S, N) ->
        io.format("%s -> %d\n", [s(S), i(N)], !IO)
    ;
        io.format("%s failed\n", [s(S)], !IO)
    ).

:- pred p(string::in, int::out) is semidet.

p(S, N) :-
    (
        S = "a",
        N = 1
    ;
        S = "b",
        N = 2
    ;
        ( S = "aa"
        ; S = "ab"
        ),
        N = 11
    ;
        ( S = "ba"
        ; S = "bb"
        ),
        N = 12
    ;
        ( S = "ca"
        ; S = "cd"
        ; S = "ce"
        ),
        N = 13
    ;
        S = "xxx",
        N = 21
    ;
        S = "xxy",
        N = 22
    ;
        S = "xxz",
        N = 23
    ;
        S = "xyx",
        N = 24
    ).
