%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This file is hereby placed into the public domain by the author (rejj).
%
% A simple test Mercury curses binding.
%
%-----------------------------------------------------------------------------%

:- module smalltest.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mcurses.
:- import_module mcurses.basics.
:- import_module mcurses.user.

:- import_module int.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

main(!IO) :-
    init(Root, !IO),
    start_colour(!IO),
    cols(Cols, !IO),
    rows(Rows, !IO),

    create(Root, [], 0, 0, Cols, 3, TopWindow, !IO),
    create(Root, [], 0, 4, Cols, Rows - 4, BottomWindow, !IO),

    place_string(TopWindow, 0, 2, "Hi!", !IO),
    place_char(BottomWindow, 10, 10, '@' - [bold, colour(yellow)], !IO),
    redraw(!IO),
    getkey(_, !IO),

    scroll(TopWindow, 1, !IO),
    place_string(TopWindow, 0, 2, "Bye!", !IO),
    clear(BottomWindow, !IO),
    place_char(BottomWindow, 10, 5, '@' - [bold, colour(green)], !IO),
    redraw(!IO),
    getkey(_, !IO),

    endwin(!IO).

%-----------------------------------------------------------------------------%
:- end_module smalltest.
%-----------------------------------------------------------------------------%
