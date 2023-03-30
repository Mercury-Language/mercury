%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ff=unix ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2001-2002, 2004-2006 The University of Melbourne.
% Copyright (C) 2021-2022 The Mercury team.
%---------------------------------------------------------------------------%
%
% File: curs.panel.m
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%---------------------------------------------------------------------------%

:- module curs.panel.
:- interface.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module string.

%---------------------------------------------------------------------------%

    % Panels are windows over the main display; they may be
    % stacked, moved, ordered and hidden. Contents of panels
    % closer to the top of the stack obscure the parts of panels
    % they overlap that are lower in the stack.
    %
:- type panel.

    % new(Rows, Cols, Row, Col, Attr, Panel) creates a new panel Panel
    % whose size is given by (Rows, Cols) and whose position on the display
    % is given by (Row, Col). The new panel starts visible and at the top
    % of the stack. The default attributes for the panel are set to Attr.
    %
:- pred new(int::in, int::in, int::in, int::in, attr::in, panel::out,
    io::di, io::uo) is det.

    % Destroy a panel.
    %
:- pred delete(panel::in, io::di, io::uo) is det.

    % Raise/lower a panel to the top/bottom of the stack.
    %
:- pred raise(panel::in, io::di, io::uo) is det.
:- pred lower(panel::in, io::di, io::uo) is det.

    % Hide/reveal a panel (revealing places it at the top of the stack).
    %
:- pred hide(panel::in, io::di, io::uo) is det.
:- pred reveal(panel::in, io::di, io::uo) is det.

    % Move a panel to (Row, Col) on the display.
    %
:- pred relocate(panel::in, int::in, int::in, io::di, io::uo) is det.

    % Clear a panel.
    %
:- pred clear(panel::in, io::di, io::uo) is det.

    % Move the virtual cursor to given row and column; (0, 0) are the
    % coordinates for the upper left hand corner of the panel.
    %
:- pred move(panel::in, int::in, int::in, io::di, io::uo) is det.

    % Add a char/string to a panel with the given attributes.
    % Note that char codes are passed rather than plain chars.
    %
:- pred addch(panel::in, attr::in, int::in, io::di, io::uo) is det.
:- pred addstr(panel::in, attr::in, string::in, io::di, io::uo) is det.

    % Turn on/off or set attributes that will be applied by default.
    %
:- pred attr_on(panel::in, attr::in, io::di, io::uo) is det.
:- pred attr_off(panel::in, attr::in, io::di, io::uo) is det.
:- pred attr_set(panel::in, attr::in, io::di, io::uo) is det.

    % Update the display (also calls doupdate).
    % NOTE: doupdate does not call update_panels.
    %
:- pred update_panels(io::di, io::uo) is det.

    % Draws a border around the inside edge of the display.
    %
:- pred border(panel::in, io::di, io::uo) is det.

    % Draws an horizontal line of length N moving to the right.
    %
:- pred hline(panel::in, int::in, int::in, io::di, io::uo) is det.

    % Draws a vertical line of length N moving down.
    %
:- pred vline(panel::in, int::in, int::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <ncurses.h>
    #include <panel.h>
").

:- pragma foreign_type("C", panel, "PANEL *").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new(Rows::in, Cols::in, Row::in, Col::in, Attr::in, Panel::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    WINDOW *w = newwin(Rows, Cols, Row, Col);
    scrollok(w, TRUE);
    wattrset(w, Attr);
    wcolor_set(w, Attr, NULL);
    wclear(w);
    Panel = new_panel(w);

    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    delete(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    delwin(panel_window(Panel));
    del_panel(Panel);

    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    raise(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    top_panel(Panel);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    lower(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    bottom_panel(Panel);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    hide(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    hide_panel(Panel);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    reveal(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    show_panel(Panel);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    relocate(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    move_panel(Panel, Row, Col);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    clear(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wclear(panel_window(Panel));
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    move(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wmove(panel_window(Panel), Row, Col);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    addch(Panel::in, Attr::in, CharCode::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    waddch(panel_window(Panel), (chtype)Attr | (chtype)CharCode);
    IO = IO0;
").

%---------------------------------------------------------------------------%

addstr(Panel, Attr, Str, !IO) :-
    string.foldl(
        ( pred(Char::in, !.IO::di, !:IO::uo) is det :-
            addch(Panel, Attr, char.to_int(Char), !IO)
        ),
        Str, !IO
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    attr_on(Panel::in, Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wattron(panel_window(Panel), Attr);
    IO = IO0;
").

:- pragma foreign_proc("C",
    attr_off(Panel::in, Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wattroff(panel_window(Panel), Attr);
    IO = IO0;
").

:- pragma foreign_proc("C",
    attr_set(Panel::in, Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wattrset(panel_window(Panel), Attr);
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    update_panels(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    update_panels();
    doupdate();
    IO = IO0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    border(Panel::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wborder(panel_window(Panel), 0, 0, 0, 0, 0, 0, 0, 0);
    IO = IO0;
").

:- pragma foreign_proc("C",
    hline(Panel::in, C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    whline(panel_window(Panel), C, N);
    IO = IO0;
").

:- pragma foreign_proc("C",
    vline(Panel::in, C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    wvline(panel_window(Panel), C, N);
    IO = IO0;
").

%---------------------------------------------------------------------------%
:- end_module curs.panel.
%---------------------------------------------------------------------------%
