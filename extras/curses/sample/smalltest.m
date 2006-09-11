%-----------------------------------------------------------------------------%
%
% this file is hereby placed into the public domain by the author (rejj)
%
% A simple test, using the mercury binding to curses.
%
%-----------------------------------------------------------------------------%

:- module smalltest.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

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

main -->
        init(Root),
        start_colour,
        cols(Cols),
        rows(Rows),

        create(Root, [], 0, 0, Cols, 3, TopWindow),
        create(Root, [], 0, 4, Cols, Rows - 4, BottomWindow),

        place_string(TopWindow, 0, 2, "Hi!"),
        place_char(BottomWindow, 10, 10, '@' - [bold, colour(yellow)]),
        redraw,
        getkey(_),

        scroll(TopWindow, 1),
        place_string(TopWindow, 0, 2, "Bye!"),
        clear(BottomWindow),
        place_char(BottomWindow, 10, 5, '@' - [bold, colour(green)]),
        redraw,
        getkey(_),

        endwin.
