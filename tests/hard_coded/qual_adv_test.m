%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test using module qualification to resolve ambiguous overloading.
% The three versions of `format' should all produce different output,
% despite having the same name, and both helper modules importing string.m.
%
% The implementation predicates mostly have the same names too,
% and a couple have different clauses.

:- module qual_adv_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- import_module qual_adv_test_helper_1.
:- import_module qual_adv_test_helper_2.

main(!IO) :-
    String = "asdfjkfhaslks",
    FString = "iii %s.\n",

    string.format(FString, [s(String)], Out1),
    qual_adv_test_helper_1.format(FString, [s(String)], Out2),
    qual_adv_test_helper_2.format(FString, [s(String)], Out3),
    io.write_string(Out1, !IO),
    io.write_string(Out2, !IO),
    io.write_string(Out3, !IO),

    Out4 = qual_adv_test_helper_1.format_func(FString, [s(String)]),
    Out5 = qual_adv_test_helper_2.format_func(FString, [s(String)]),
    ( if
        Out4 = Out2,
        Out5 = Out3
    then
        io.write_string("ok\n", !IO)
    else
        io.write_string("failed\n", !IO)
    ).
