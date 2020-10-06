%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_split.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
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
    true.
