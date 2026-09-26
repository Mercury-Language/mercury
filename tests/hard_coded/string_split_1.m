%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_split_1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.write_list(
        split_at_separator(char.is_upper, ""),
        ":", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_separator(char.is_upper, "!"),
        ":", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_separator(char.is_upper, "helloXworldXhowXareYyou!"),
        ":", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_separator(char.is_whitespace, "hello world\thow are\t\tyou!"),
        "<tab>", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_char(':', "user:group:id1:id2"),
        "<tab>", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_string("aa", "xaaayaaaz"),
        "<tab>", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_string("aaa", "xaaaa aaaaax aaa x"),
        "<tab>", io.write_string, !IO),
    io.nl(!IO),
    io.write_list(
        split_at_string(":::", "col1:::col2:val2:::col3:::"),
        "<tab>", io.write_string, !IO),
    io.nl(!IO),

    io.nl(!IO),
    list.foldl(io.write_line,
        split_into_lines("line1\nline2\nline3\nline4\n"), !IO),
    io.nl(!IO),
    list.foldl(io.write_line,
        split_into_lines("line1\nline2\nline3\nline4nonl"), !IO),
    io.nl(!IO),

    list.foldl(do_test_split_into_lines, split_into_line_tests, !IO),
    io.nl(!IO),

    true.

:- pred do_test_split_into_lines(string::in, io::di, io::uo) is det.

do_test_split_into_lines(Str, !IO) :-
    io.format("split_into_lines(%s) = ", [s(string(Str))], !IO),
    Lines = split_into_lines(Str),
    io.write_line(Lines, !IO).

:- func split_into_line_tests = list(string).

split_into_line_tests = [
    % Examples from the documentation comment for split_into_lines/1.
    "",
    "a",
    "a\n",
    "\n",
    "a\n\nb",

    " ",

    % Examples containing carriage returns.
    "\r",
    "\r\n",
    "a\r",
    "a\r\n",

    % Mercury backslash escapes
    "\a\b\e\f\t\v\n",

    % Other code points that are treated as line breaks
    % in various contexts, but not by split_into_lines/1.
    "a\u001Cb", % FILE SEPARATOR
    "a\u001Db", % GROUP SEPARATOR
    "a\u001Eb", % RECORD SEPARATOR
    "a\u0085b"  % NEXT LINE
% XXX we should test these as well but string/1 does not currently
% escape them.
%    "a\u2028b", % LINE SEPARATOR
%    "a\u2029b"  % PARAGRAPH SEPARATOR
].
