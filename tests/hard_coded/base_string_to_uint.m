%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the non-overflow behaviour of string.base_string_to_uint/3.
% (The overflow behaviour of this predicate is tested separately, by
% string_to_uint_overflow.m.)
%
%---------------------------------------------------------------------------%

:- module base_string_to_uint.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    header("Empty and blank strings", !IO),
    test(10, "", !IO),
    test(2, "", !IO),
    test(36, "", !IO),
    test(10, " ", !IO),
    test(10, "   ", !IO),
    test(10, "\t", !IO),
    test(10, "\n", !IO),
    test(10, " \t ", !IO),
    test(2, " ", !IO),
    test(36, " ", !IO),
    io.nl(!IO),

    header("Signs are not accepted", !IO),
    test(10, "-1", !IO),
    test(10, "+1", !IO),
    test(10, "-0", !IO),
    test(10, "+0", !IO),
    test(10, "-", !IO),
    test(10, "+", !IO),
    test(16, "-f", !IO),
    test(36, "+z", !IO),
    test(10, "--1", !IO),
    test(10, "1-", !IO),
    test(10, "1+", !IO),
    io.nl(!IO),

    header("Single digits", !IO),
    test(10, "0", !IO),
    test(10, "7", !IO),
    test(2, "0", !IO),
    test(2, "1", !IO),
    test(8, "7", !IO),
    test(16, "f", !IO),
    test(16, "F", !IO),
    test(36, "z", !IO),
    test(36, "Z", !IO),
    io.nl(!IO),

    header("Upper and lower case digits", !IO),
    test(16, "abcdef", !IO),
    test(16, "ABCDEF", !IO),
    test(16, "AbCdEf", !IO),
    test(36, "abz", !IO),
    test(36, "ABZ", !IO),
    test(36, "AbZ", !IO),
    io.nl(!IO),

    header("Leading zeros", !IO),
    test(10, "00", !IO),
    test(10, "007", !IO),
    test(2, "000101", !IO),
    test(8, "00017", !IO),
    test(16, "000ff", !IO),
    test(36, "000z", !IO),
    test(10, "0000000000000000000000000000000", !IO),
    test(10, "0000000000000000000000000000001", !IO),
    io.nl(!IO),

    header("Digits not valid in the specified base", !IO),
    test(2, "1", !IO),
    test(2, "2", !IO),
    test(3, "2", !IO),
    test(3, "3", !IO),
    test(8, "7", !IO),
    test(8, "8", !IO),
    test(9, "8", !IO),
    test(9, "9", !IO),
    test(10, "9", !IO),
    test(10, "a", !IO),
    test(10, "A", !IO),
    test(11, "a", !IO),
    test(11, "A", !IO),
    test(11, "b", !IO),
    test(11, "B", !IO),
    test(16, "f", !IO),
    test(16, "g", !IO),
    test(16, "G", !IO),
    test(17, "g", !IO),
    test(17, "h", !IO),
    test(35, "y", !IO),
    test(35, "z", !IO),
    test(35, "Z", !IO),
    test(36, "z", !IO),
    test(36, "Z", !IO),
    test(2, "102", !IO),
    test(8, "178", !IO),
    test(16, "1fg", !IO),
    io.nl(!IO),

    header("Non-digit characters", !IO),
    test(10, " 1", !IO),
    test(10, "1 ", !IO),
    test(10, "1 2", !IO),
    test(10, "1.0", !IO),
    test(10, "1,000", !IO),
    test(10, "1_000", !IO),
    test(10, "1e5", !IO),
    test(16, "0x10", !IO),
    test(10, "\t1", !IO),
    test(10, "1\n", !IO),
    test(36, "1!", !IO),
    test(10, "\uff13", !IO),
    io.nl(!IO),

    header("The string \"10\" in every base", !IO),
    list.foldl(test_10_in_base, 2 .. 36, !IO),
    io.nl(!IO),

    % base_string_to_uint/3 throws an exception if the base is not in 2..36.
    % It checks the base before it looks at the string, so the string is
    % irrelevant in these tests.

    header("Invalid bases", !IO),
    test(1, "1", !IO),
    test(0, "1", !IO),
    test(-1, "1", !IO),
    test(37, "1", !IO),
    test(100, "1", !IO),
    io.nl(!IO),

    header("det_base_string_to_uint", !IO),
    test_det(10, "123", !IO),
    test_det(16, "ff", !IO),
    test_det(10, "", !IO),
    test_det(10, "12a", !IO),
    test_det(10, "-1", !IO),
    test_det(1, "1", !IO),
    io.nl(!IO),

    % to_uint/2 is base_string_to_uint/3 with the base fixed at 10.

    header("to_uint", !IO),
    test_to_uint("123", !IO),
    test_to_uint("-123", !IO),
    test_to_uint("", !IO),
    test_to_uint("0x10", !IO),
    test_to_uint(" 1", !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_10_in_base(int::in, io::di, io::uo) is cc_multi.

test_10_in_base(Base, !IO) :-
    test(Base, "10", !IO).

%---------------------------------------------------------------------------%

    % test(Base, Str, !IO):
    %
    % Test string.base_string_to_uint(Base, Str, UInt), writing out the call
    % and then its result: the value of UInt if the call succeeds, "no" if
    % it fails, or "exception" if it throws.
    %
:- pred test(int::in, string::in, io::di, io::uo) is cc_multi.

test(Base, Str, !IO) :-
    io.format("base_string_to_uint(%d, %s) ==> ",
        [i(Base), s(string(Str))], !IO),
    ( try []
        ( if string.base_string_to_uint(Base, Str, UInt0) then
            MaybeUInt = yes(UInt0)
        else
            MaybeUInt = no
        )
    then
        (
            MaybeUInt = yes(UInt),
            io.write_line(UInt, !IO)
        ;
            MaybeUInt = no,
            io.write_string("no\n", !IO)
        )
    catch_any _ ->
        io.write_string("exception\n", !IO)
    ).

:- pred test_det(int::in, string::in, io::di, io::uo) is cc_multi.

test_det(Base, Str, !IO) :-
    io.format("det_base_string_to_uint(%d, %s) ==> ",
        [i(Base), s(string(Str))], !IO),
    ( try []
        UInt = string.det_base_string_to_uint(Base, Str)
    then
        io.write_line(UInt, !IO)
    catch_any _ ->
        io.write_string("exception\n", !IO)
    ).

:- pred test_to_uint(string::in, io::di, io::uo) is det.

test_to_uint(Str, !IO) :-
    io.format("to_uint(%s) ==> ", [s(string(Str))], !IO),
    ( if string.to_uint(Str, UInt) then
        io.write_line(UInt, !IO)
    else
        io.write_string("no\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred header(string::in, io::di, io::uo) is det.

header(Title, !IO) :-
    io.format("### %s ###\n\n", [s(Title)], !IO).

%---------------------------------------------------------------------------%
:- end_module base_string_to_uint.
%---------------------------------------------------------------------------%
