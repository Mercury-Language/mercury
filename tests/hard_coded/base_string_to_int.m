%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the non-overflow behaviour of string.base_string_to_int/3 and
% string.base_string_to_uint/3.
% (The overflow behaviour of these predicates is tested separately, by
% ../general/test_string_to_int_overflow.m and string_to_uint_overflow.m
% respectively.)
%
%---------------------------------------------------------------------------%

:- module base_string_to_int.

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

%---------------------------------------------------------------------------%

main(!IO) :-
    header("Empty strings", !IO),
    test(10, "", !IO),
    test(2, "", !IO),
    test(36, "", !IO),
    io.nl(!IO),

    header("Blank strings", !IO),
    test(10, " ", !IO),
    test(10, "   ", !IO),
    test(10, "\t", !IO),
    test(10, "\n", !IO),
    test(10, " \t ", !IO),
    test(2, " ", !IO),
    test(36, " ", !IO),
    io.nl(!IO),

    header("Signs", !IO),
    test(10, "-1", !IO),
    test(10, "+1", !IO),
    test(10, "-0", !IO),
    test(10, "+0", !IO),
    test(10, "-", !IO),
    test(10, "+", !IO),
    test(16, "-", !IO),
    test(2, "+", !IO),
    test(10, "--", !IO),
    test(10, "++", !IO),
    test(10, "+-1", !IO),
    test(10, "-+1", !IO),
    test(10, "- ", !IO),
    test(10, "+ ", !IO),
    io.nl(!IO),

    header("Single digits", !IO),
    test(10, "0", !IO),
    test(10, "7", !IO),
    test(10, "-7", !IO),
    test(10, "+7", !IO),
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
    test(16, "-Ff", !IO),
    test(36, "abz", !IO),
    test(36, "ABZ", !IO),
    test(36, "AbZ", !IO),
    io.nl(!IO),

    header("Leading zeros", !IO),
    test(10, "00", !IO),
    test(10, "007", !IO),
    test(10, "-007", !IO),
    test(10, "+007", !IO),
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

    header("Signs in the wrong place", !IO),
    test(10, "1-", !IO),
    test(10, "1+", !IO),
    test(10, "1-2", !IO),
    test(10, "12-34", !IO),
    test(10, "--1", !IO),
    test(10, "++1", !IO),
    test(10, "-1-", !IO),
    io.nl(!IO),

    header("The string \"10\" in every base", !IO),
    list.foldl(test_10_in_base, 2 .. 36, !IO),
    io.nl(!IO),

    % Both base_string_to_int/3 and base_string_to_uint/3 throw an exception if
    % the base is not in 2..36. They check the base before they look at the
    % string, so the string is irrelevant in these tests.

    header("Invalid bases", !IO),
    test(1, "1", !IO),
    test(0, "1", !IO),
    test(-1, "1", !IO),
    test(37, "1", !IO),
    test(100, "1", !IO),
    io.nl(!IO),

    header("det_base_string_to_{int,uint}", !IO),
    test_det(10, "123", !IO),
    test_det(16, "ff", !IO),
    test_det(10, "", !IO),
    test_det(10, "12a", !IO),
    test_det(1, "1", !IO),
    io.nl(!IO),

    % to_int/2 is base_string_to_int/3 with the base fixed at 10.
    % to_uint/2 is base_string_to_uint/3 with the base fixed at 10.

    header("to_{int,uint}", !IO),
    test_to_int("123", !IO),
    test_to_int("-123", !IO),
    test_to_int("", !IO),
    test_to_int("0x10", !IO),
    test_to_int(" 1", !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_10_in_base(int::in, io::di, io::uo) is cc_multi.

test_10_in_base(Base, !IO) :-
    test(Base, "10", !IO).

%---------------------------------------------------------------------------%

    % test(Base, Str, !IO):
    %
    % Test string.base_string_to_int(Base, Str, Int) and
    % string.base_string_to_uint(Base, Str, UInt) writing out
    % the call then results: the numeric value of the Int / UInt if both
    % calls succeed, "no" if they fail, or "exception" if they throw
    % an exception. If the results differ between the signed and signed
    % version, write out both results.
    %
:- pred test(int::in, string::in, io::di, io::uo) is cc_multi.

test(Base, Str, !IO) :-
    io.format("base_string_to_{int,uint}(%3d, %8s) ==> ",
        [i(Base), s(string(Str))], !IO),
    ( try []
        ( if string.base_string_to_int(Base, Str, Int0) then
            MaybeInt = yes(Int0)
        else
            MaybeInt = no
        )
    then
        (
            MaybeInt = yes(Int),
            IntResult = int_to_string(Int)
        ;
            MaybeInt = no,
            IntResult = "no"
        )
    catch_any _ ->
        IntResult = "exception"
    ),
    ( try []
        ( if string.base_string_to_uint(Base, Str, UInt0) then
            MaybeUInt = yes(UInt0)
        else
            MaybeUInt = no
        )
    then
        (
            MaybeUInt = yes(UInt),
            UIntResult = uint_to_string(UInt)
        ;
            MaybeUInt = no,
            UIntResult = "no"
        )
    catch_any _ ->
        UIntResult = "exception"
    ),
    ( if IntResult = UIntResult then
        io.print_line(IntResult, !IO)
    else
        io.format("int = %3s, uint = %3s\n",
            [s(IntResult), s(UIntResult)], !IO)
    ).

:- pred test_det(int::in, string::in, io::di, io::uo) is cc_multi.

test_det(Base, Str, !IO) :-
    io.format("det_base_string_to_{int,uint}(%2d, %5s) ==> ",
        [i(Base), s(string(Str))], !IO),
    ( try []
        Int = string.det_base_string_to_int(Base, Str)
    then
        IntResult = int_to_string(Int)
    catch_any _ ->
        IntResult = "exception"
    ),
    ( try []
        UInt = string.det_base_string_to_uint(Base, Str)
    then
        UIntResult = uint_to_string(UInt)
    catch_any _ ->
        UIntResult = "exception"
    ),
    ( if IntResult = UIntResult then
        io.print_line(IntResult, !IO)
    else
        io.format("int = %3s, uint = %3s\n",
            [s(IntResult), s(UIntResult)], !IO)
    ).

:- pred test_to_int(string::in, io::di, io::uo) is det.

test_to_int(Str, !IO) :-
    io.format("to_{int,uint}(%6s) ==> ", [s(string(Str))], !IO),
    ( if string.to_int(Str, Int) then
        IntResult = int_to_string(Int)
    else
        IntResult = "no"
    ),
    ( if string.to_uint(Str, UInt) then
        UIntResult = uint_to_string(UInt)
    else
        UIntResult = "no"
    ),
    ( if IntResult = UIntResult then
        io.print_line(IntResult, !IO)
    else
        io.format("int = %3s, uint = %3s\n",
            [s(IntResult), s(UIntResult)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred header(string::in, io::di, io::uo) is det.

header(Title, !IO) :-
    io.format("### %s ###\n\n", [s(Title)], !IO).

%---------------------------------------------------------------------------%
:- end_module base_string_to_int.
%---------------------------------------------------------------------------%
