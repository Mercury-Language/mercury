%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fold_tests.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    TestList = 1 .. 10,
    test_write(TestList, !IO),
    list.foldl(prepend_int_to_string, TestList, "", Str0),
    list.chunk_foldl(3, prepend_int_to_string, TestList, "", Str1),
    io.format("using foldl:       %s\n", [s(Str0)], !IO),
    io.format("using chunk_foldl: %s\n", [s(Str1)], !IO).

:- pred prepend_int_to_string(int::in, string::in, string::out) is det.

prepend_int_to_string(N, !Str) :-
    ( if !.Str = "" then
        !:Str = string.int_to_string(N)
    else
        !:Str = string.int_to_string(N) ++ "_" ++ !.Str
    ).

:- pred test_write(list(int)::in, io::di, io::uo) is det.

test_write(TestList, !IO) :-
    list.gap_foldl(io.write_int, io.write_string(", "), TestList, !IO),
    io.nl(!IO),
    list.last_gap_foldl(io.write_int, io.write_string(", "),
        io.write_string(" and "), TestList, !IO),
    io.nl(!IO).
