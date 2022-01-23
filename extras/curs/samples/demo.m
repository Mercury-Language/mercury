%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Tue Jan 23 10:05:05 GMT 2001
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module demo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module curs.
:- import_module curs.panel.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type panel_data
    --->    panel_data(
                panel   ::  panel,
                row     ::  int,
                col     ::  int
            ).

%-----------------------------------------------------------------------------%

main(!IO) :-

    start(!IO),

    rows_cols(Rows, Cols, !IO),
    MidRow = Rows // 2,
    MidCol = Cols // 2,
    PanelRows = MidRow // 2,
    PanelCols = MidCol // 2,

    move(0, 0, !IO),
    addstr(normal, string.format("display size: %dx%d", [i(Rows), i(Cols)]),
        !IO),
    move(1, 0, !IO),
    addstr(normal, "1 2 3 4 : select & raise panel", !IO),
    move(2, 0, !IO),
    addch(normal, acs_larrow, !IO), addch(normal, 0' , !IO),
    addch(normal, acs_darrow, !IO), addch(normal, 0' , !IO),
    addch(normal, acs_uarrow, !IO), addch(normal, 0' , !IO),
    addch(normal, acs_rarrow, !IO), addch(normal, 0' , !IO),
    addstr(normal, ": move panel", !IO),
    move(3, 0, !IO),
    addstr(normal, "      q : quit", !IO),

    R1 = MidRow - PanelRows - 1, C1 = MidCol - PanelCols - 1,
    R2 = MidRow - PanelRows - 2, C2 = MidCol             - 2,
    R3 = MidRow             - 3, C3 = MidCol - PanelCols - 3,
    R4 = MidRow             - 4, C4 = MidCol             - 4,

    new(PanelRows, PanelCols, R1, C1, fg_bg(black, yellow), Panel1, !IO),
    new(PanelRows, PanelCols, R2, C2, fg_bg(white, blue  ), Panel2, !IO),
    new(PanelRows, PanelCols, R3, C3, fg_bg(black, green ), Panel3, !IO),
    new(PanelRows, PanelCols, R4, C4, fg_bg(white, red   ), Panel4, !IO),

    border(Panel1, !IO),
    move(Panel1, 0, 1, !IO),
    addstr(Panel1, normal, " 1 ", !IO),

    border(Panel2, !IO),
    move(Panel2, 0, 1, !IO),
    addstr(Panel2, normal, " 2 ", !IO),

    border(Panel3, !IO),
    move(Panel3, 0, 1, !IO),
    addstr(Panel3, normal, " 3 ", !IO),

    border(Panel4, !IO),
    move(Panel4, 0, 1, !IO),
    addstr(Panel4, normal, " 4 ", !IO),

    PanelData = [
        panel_data(Panel1, R1, C1),
        panel_data(Panel2, R2, C2),
        panel_data(Panel3, R3, C3),
        panel_data(Panel4, R4, C4)
    ],

    main_loop(1, PanelData, !IO),

    stop(!IO).

%-----------------------------------------------------------------------------%

:- pred main_loop(int::in, list(panel_data)::in, io::di, io::uo) is det.

main_loop(P, PanelData, !IO) :-
    update_panels(!IO),
    getch(K, !IO),
    (      if K = key_left  then move_panel(P, PanelData,  0, -1, !IO)
      else if K = key_right then move_panel(P, PanelData,  0,  1, !IO)
      else if K = key_up    then move_panel(P, PanelData, -1,  0, !IO)
      else if K = key_down  then move_panel(P, PanelData,  1,  0, !IO)
      else if K = 0'1       then raise_panel(1, PanelData, !IO)
      else if K = 0'2       then raise_panel(2, PanelData, !IO)
      else if K = 0'3       then raise_panel(3, PanelData, !IO)
      else if K = 0'4       then raise_panel(4, PanelData, !IO)
      else if K = 0'q       then true
      else main_loop(P, PanelData, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred move_panel(int::in, list(panel_data)::in, int::in, int::in,
    io::di, io::uo) is det.

move_panel(P, PanelData0, DR, DC, !IO) :-
    PD0 = list.det_index1(PanelData0, P),
    PD  = ((PD0
                    ^ row := PD0 ^ row + DR)
                    ^ col := PD0 ^ col + DC),
    PanelData = list.det_replace_nth(PanelData0, P, PD),
    relocate(PD ^ panel, PD ^ row, PD ^ col, !IO),
    main_loop(P, PanelData, !IO).

%-----------------------------------------------------------------------------%

:- pred raise_panel(int::in, list(panel_data)::in, io::di, io::uo) is det.

raise_panel(P, PanelData, !IO) :-
    PD = list.det_index1(PanelData, P),
    raise(PD ^ panel, !IO),
    main_loop(P, PanelData, !IO).

%-----------------------------------------------------------------------------%
:- end_module demo.
%-----------------------------------------------------------------------------%
