:- module string_split.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string, char.

main(!IO) :-
  io__write_list(
    split_at_separator(char__is_upper, "helloXworldXhowXareYyou!"),
    ":", io__write_string, !IO),
  io__nl(!IO),
  io__write_list(
    split_at_separator(char__is_whitespace, "hello world\thow are\t\tyou!"),
    "<tab>", io__write_string, !IO),
  io__nl(!IO),
  io__write_list(
    split_at_char(':', "user:group:id1:id2"),
    "<tab>", io__write_string, !IO),
  io__nl(!IO),
  io__write_list(
    split_at_string("aa", "xaaayaaaz"),
    "<tab>", io__write_string, !IO),
  io__nl(!IO),
  io__write_list(
    split_at_string("aaa", "xaaaa aaaaax aaa x"),
    "<tab>", io__write_string, !IO),
  io__nl(!IO),
  io__write_list(
    split_at_string(":::", "col1:::col2:val2:::col3:::"),
    "<tab>", io__write_string, !IO),
  io__nl(!IO),
  true.
