%-----------------------------------------------------------------------------%
% base_string_to_int_test.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Jul  1 16:12:57 EST 2004
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module base_string_to_int_test.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    test("123", !IO),
    test("+123", !IO),
    test("-123", !IO),
    test("", !IO),
    test("+", !IO),
    test("-", !IO),
    test("123abc", !IO),
    test("abc", !IO),
    test("+abc", !IO),
    test("-abc", !IO).


:- pred test(string::in, io::di, io::uo) is det.

test(S, !IO) :-
    io.format("string.base_string_to_int(10, \"%s\", ", [s(S)], !IO),
    ( if   string.base_string_to_int(10, S, N)
      then io.format("%d).\n", [i(N)], !IO)
      else io.format("_) failed.\n", [], !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
