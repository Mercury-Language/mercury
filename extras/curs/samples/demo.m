% ---------------------------------------------------------------------------- %
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Tue Jan 23 10:05:05 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%
% ---------------------------------------------------------------------------- %

:- module demo.

:- interface.

:- import_module io.



:- pred main(io__state::di, io__state::uo) is det.

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- import_module int, list, string, curs, curs__panel.

:- type panel_data
    --->    panel_data(
                panel   ::  panel,
                row     ::  int,
                col     ::  int
            ).

% ---------------------------------------------------------------------------- %

main -->

    start,

    rows_cols(Rows, Cols),
    { MidRow = Rows // 2 },
    { MidCol = Cols // 2 },
    { PanelRows = MidRow // 2 },
    { PanelCols = MidCol // 2 },

    move(0, 0),
    addstr(normal, string__format("display size: %dx%d", [i(Rows), i(Cols)])),
    move(1, 0),
    addstr(normal, "1 2 3 4 : select & raise panel"),
    move(2, 0),
    addch(normal, acs_larrow), addch(normal, 0' ),
    addch(normal, acs_darrow), addch(normal, 0' ),
    addch(normal, acs_uarrow), addch(normal, 0' ),
    addch(normal, acs_rarrow), addch(normal, 0' ),
    addstr(normal, ": move panel"),
    move(3, 0),
    addstr(normal, "      q : quit"),

    { R1 = MidRow - PanelRows - 1, C1 = MidCol - PanelCols - 1 },
    { R2 = MidRow - PanelRows - 2, C2 = MidCol             - 2 },
    { R3 = MidRow             - 3, C3 = MidCol - PanelCols - 3 },
    { R4 = MidRow             - 4, C4 = MidCol             - 4 },

    new(PanelRows, PanelCols, R1, C1, fg_bg(black, yellow), Panel1),
    new(PanelRows, PanelCols, R2, C2, fg_bg(white, blue  ), Panel2),
    new(PanelRows, PanelCols, R3, C3, fg_bg(black, green ), Panel3),
    new(PanelRows, PanelCols, R4, C4, fg_bg(white, red   ), Panel4),

    border(Panel1), move(Panel1, 0, 1), addstr(Panel1, normal, " 1 "),
    border(Panel2), move(Panel2, 0, 1), addstr(Panel2, normal, " 2 "),
    border(Panel3), move(Panel3, 0, 1), addstr(Panel3, normal, " 3 "),
    border(Panel4), move(Panel4, 0, 1), addstr(Panel4, normal, " 4 "),

    { PanelData = [
        panel_data(Panel1, R1, C1),
        panel_data(Panel2, R2, C2),
        panel_data(Panel3, R3, C3),
        panel_data(Panel4, R4, C4)
    ] },

    main_loop(1, PanelData),

    stop.

% ---------------------------------------------------------------------------- %

:- pred main_loop(int, list(panel_data), io__state, io__state).
:- mode main_loop(in, in, di, uo) is det.

main_loop(P, PanelData) -->

    update_panels,

    getch(K),

    (      if { K = key_left  } then move_panel(P, PanelData,  0, -1)
      else if { K = key_right } then move_panel(P, PanelData,  0,  1)
      else if { K = key_up    } then move_panel(P, PanelData, -1,  0)
      else if { K = key_down  } then move_panel(P, PanelData,  1,  0)
      else if { K = 0'1       } then raise_panel(1, PanelData)
      else if { K = 0'2       } then raise_panel(2, PanelData)
      else if { K = 0'3       } then raise_panel(3, PanelData)
      else if { K = 0'4       } then raise_panel(4, PanelData)
      else if { K = 0'q       } then []
      else main_loop(P, PanelData)
    ).

% ---------------------------------------------------------------------------- %

:- pred move_panel(int, list(panel_data), int, int, io__state, io__state).
:- mode move_panel(in, in, in, in, di, uo) is det.

move_panel(P, PanelData0, DR, DC) -->
    { PD0 = list__index1_det(PanelData0, P) },
    { PD  = ((PD0
                    ^ row := PD0 ^ row + DR)
                    ^ col := PD0 ^ col + DC) },
    { PanelData = list__replace_nth_det(PanelData0, P, PD) },
    relocate(PD ^ panel, PD ^ row, PD ^ col),
    main_loop(P, PanelData).

% ---------------------------------------------------------------------------- %

:- pred raise_panel(int, list(panel_data), io__state, io__state).
:- mode raise_panel(in, in, di, uo) is det.

raise_panel(P, PanelData) -->
    { PD = list__index1_det(PanelData, P) },
    raise(PD ^ panel),
    main_loop(P, PanelData).

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
