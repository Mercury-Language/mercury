%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    test("foo", "bar", !IO).

:- pred test(string::in, string::in, io::di, io::uo) is det.

test(X, Y, !IO) :-
    write_message("X", X, !IO),
    write_message("Y", Y, !IO),
    string.append(X, Y, Z),
    write_message("X append Y", Z, !IO),
    string.capitalize_first(X, CapX),
    write_message("capitalize_first X", CapX, !IO),
    string.uncapitalize_first(CapX, UnCapX),
    write_message("uncapitalize_first CapX", UnCapX, !IO),
    string.int_to_string(1234, Num),
    write_message("int_to_string 1234", Num, !IO),
    string.int_to_base_string(1234, 8, Num8),
    write_message("octal 1234", Num8, !IO),
    string.int_to_base_string(1234, 16, Num16),
    write_message("hexadecimal 1234", Num16, !IO),
    NumG1 = string.int_to_base_string_group(1234, 10, 3, ","),
    write_message("Grouped 1234", NumG1, !IO),
    NumG2 = string.int_to_base_string_group(113, 2, 1, "--"),
    write_message("Grouped 113", NumG2, !IO),
    NumG3 = string.int_to_string_thousands(1300000),
    write_message("Grouped 1300000", NumG3, !IO),
    NumG4 = string.int_to_base_string_group(45999, 10, 0, ","),
    write_message("Non Grouped 45999", NumG4, !IO),
    string.duplicate_char('f', 5, FiveFs),
    string.duplicate_char('φ', 5, FivePhis),
    ( if string.to_int("5678", Num5678) then
        io.format("string_to_int 5678: %d\n", [i(Num5678)], !IO)
    else
        error("string.to_int(""5678"", _) failed")
    ),
    ( if string.to_int("asdf", _) then
        error("string.to_int(""asdf"", _) succeeded")
    else
        true
    ),
    write_message("Five f's", FiveFs, !IO),
    string.pad_right(FiveFs, '.', 10, FsAndDots),
    write_message("Five f's and five dots", FsAndDots, !IO),
    string.pad_left(FsAndDots, '-', 15, DashesFsAndDots),
    write_message("Five dashes, five f's and five dots", DashesFsAndDots, !IO),
    write_message("Five φ's", FivePhis, !IO),
    string.pad_right(FivePhis, '.', 10, PhisAndDots),
    write_message("Five φ's and five dots", PhisAndDots, !IO),
    string.pad_left(PhisAndDots, '-', 15, DashesPhisAndDots),
    write_message("Five dashes, five φ's and five dots",
        DashesPhisAndDots, !IO),
    Table = string.format_table([
        left(["aaa", "b", "cc"]),
        left(["áaȧ", "б", "цц"]),
        right(["1111111", "", "333"]),
        right(["¹½⅓¼⅕⅙⅛", "", "¾⅘⅚"]),
        right(["1,300,000.00", "9,999.00", "123,456,789.99"])
    ], "|"),
    io.write_string(Table, !IO),
    io.nl(!IO),
    Wrapped = string.word_wrap("*aaaaaaaaaaaaaaaaaaaa*  bbbbb bbb  b\t"
        ++ " ccccc c c c   cccc c c c c ccccc ccc cccc c  ccc ccc ccc "
        ++ "*dddddddddddddddddddddddddddddddddddddddddddddddddddddd*"
        ++ "                                                    eee",
        10),
    WrappedHyphen =
        string.word_wrap_separator(
        "*aaaaaaaaaaaaaaaaaaaa*  bbbbb bbb  b\t"
        ++ " ccccc c c c   cccc c c c c ccccc ccc cccc c  ccc ccc ccc "
        ++ "*dddddddddddddddddddddddddddddddddddddddddddddddddddddd*"
        ++ "                                                    eee",
        10, "-"),
    WrappedDots =
        string.word_wrap_separator("*aaaaaa*  bbbbb bbb  b\t"
        ++ " ccccc c c c   cccc c c c c ccccc ccc cccc c  ccc ccc ccc "
        ++ "*dddddddddddddd*"
        ++ "                                                    eee",
        5, "..."),
    SepTooLong = string.word_wrap_separator("whatever", 2, "..."),
    io.write_string("\nWrapped string:\n", !IO),
    io.write_string(Wrapped, !IO),
    io.write_string("\nWrapped string with hyphens:\n", !IO),
    io.write_string(WrappedHyphen, !IO),
    io.write_string("\nWrapped string with dots:\n", !IO),
    io.write_string(WrappedDots, !IO),
    io.write_string("\nWrapped string where separator is too long:\n", !IO),
    io.write_string(SepTooLong, !IO),
    io.nl(!IO).

:- pred write_message(string::in, string::in, io::di, io::uo) is det.

write_message(Msg, String, !IO) :-
    io.format("%s: %s\n", [s(Msg), s(String)], !IO).
