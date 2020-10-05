%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test to check the correctness of the string__join_list predicate.

:- module join_list.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module string.

main(!IO) :-
    test_join_list([], !IO),
    test_join_list(["a", "b"], !IO),
    test_join_list(["a", "b", "c"], !IO),
    test_join_list(["a", "", "c"], !IO),
    test_join_list(["abc", "def", "ghi"], !IO),
    test_join_list(["the", "quick", "brown", "fox", "jumped", "over",
        "the", "lazy", "dog"], !IO),
    test_join_list(["this", "is", "a", "test", "to", "check",
        "the correctness", " of the", "join_list ", "predicate\n"], !IO),
    test_join_list([" ", "\t", " \t ", "x"], !IO).

:- pred test_join_list(list(string)::in, io::di, io::uo) is det.

test_join_list(Pieces, !IO) :-
    Joined1 = string__join_list(", ", Pieces),
    io__write_string(Joined1, !IO),
    io__write_string("\n", !IO),
    Joined2 = string__join_list(" ", Pieces),
    io__write_string(Joined2, !IO),
    io__write_string("\n", !IO),
    Joined3 = string__join_list(" x ", Pieces),
    io__write_string(Joined3, !IO),
    io__write_string("\n", !IO),
    Joined4 = string__join_list("", Pieces),
    io__write_string(Joined4, !IO),
    io__write_string("\n", !IO).
