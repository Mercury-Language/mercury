% ---------------------------------------------------------------------------- %
% curs.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Thu Jan 11 13:47:25 GMT 2001
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%
% Simplified Mercury interface to the ncurses and panel libraries.
%
% This is largely inspired by Tomas Conway and Robert Jeschofnik's
% mcurses module; it is intended to more closely match the facilities
% offered by the ncurses package and leave the issue of window management
% to the ncurses and panel libraries rather than doing so in Mercury.
%
% XXX This module no error checking.
%
% NOTE: you will need to include `-lpanel -lncurses' in MLLIBS when
% linking against this module.
%
% ---------------------------------------------------------------------------- %

:- module curs.

:- interface.

:- import_module io, string, char, int.



    % Start a curses session (colour, unbuffered input, no echoing, 
    % invisible cursor if possible, scrolling on when output past
    % the bottom of the main display and any windows).
    %
:- pred start(io__state::di, io__state::uo) is det.

    % Close a curses session; necessary to return the tty to a sensible
    % state.
    %
:- pred stop(io__state::di, io__state::uo) is det.

    % A wrapper predicate that handles calling start and stop.
    %
% :- pred session(pred(io__state::di, io__state::uo) is det,
            % io__state::di, io__state::uo) is det.
:- pred session(pred(io__state, io__state), io__state, io__state).
:- mode session(pred(di, uo) is det, di, uo) is det.

    % Number of rows and columns on the physical screen.
    %
:- pred rows_cols(int::out, int::out, io__state::di, io__state::uo) is det.

    % Move the virtual cursor to given row and column; (0, 0) are the
    % coordinates for the upper left hand corner of the display.
    %
:- pred move(int::in, int::in, io__state::di, io__state::uo) is det.

    % Clear the whole display.
    %
:- pred clear(io__state::di, io__state::uo) is det.

    % Output a character (with the given attributes) and advance the cursor.
    % Note that char codes are passed rather than plain chars.
    %
:- pred addch(attr::in, int::in, io__state::di, io__state::uo) is det.

    % Output a string (with the given attributes) and advance the cursor.
    %
:- pred addstr(attr::in, string::in, io__state::di, io__state::uo) is det.

    % Turn on/off or set attributes that will be applied by default.
    %
:- pred attr_on(attr::in, io__state::di, io__state::uo) is det.
:- pred attr_off(attr::in, io__state::di, io__state::uo) is det.
:- pred attr_set(attr::in, io__state::di, io__state::uo) is det.

    % Update the display.  Changes made to the display are not made
    % visible until doupdate is called.
    %
:- pred doupdate(io__state::di, io__state::uo) is det.

    % Read a character from the keyboard (unbuffered) and translate it
    % if necessary.
    %
:- pred getch(int::out, io__state::di, io__state::uo) is det.



    % Draws a border around the inside edge of the display.
    %
:- pred border(io__state::di, io__state::uo) is det.

    % Draws an horizontal line of char codes C length N moving to the right.
    %
:- pred hline(int::in, int::in, io__state::di, io__state::uo) is det.

    % Draws a vertical line of char codes C length N moving down.
    %
:- pred vline(int::in, int::in, io__state::di, io__state::uo) is det.



    % Various key code translations outside the normal ASCII range.
    %
:- func key_down = int.
:- func key_up = int.
:- func key_left = int.
:- func key_right = int.
:- func key_home = int.
:- func key_backspace = int.
:- func key_f(int) = int.               % Function key no. (0 to 63).
:- func key_del = int.
:- func key_ins = int.
:- func key_pageup = int.
:- func key_pagedown = int.
:- func key_a1 = int.                   % Key pad upper left.
:- func key_a3 = int.                   % Key pad upper right.
:- func key_b2 = int.                   % Key pad middle centre.
:- func key_c1 = int.                   % Key pad lower left.
:- func key_c3 = int.                   % Key pad lower right.
:- func key_enter = int.                % Key pad enter.
:- func key_end = int.
:- func key_resize = int.               % Resize event.



    % Special char codes (not always available).
    %
                                        % Default   Description
                                        % -------   -----------
:- func acs_block = int.                % #         solid square block
:- func acs_board = int.                % #         board of squares
:- func acs_btee = int.                 % +         bottom tee
:- func acs_bullet = int.               % o         bullet
:- func acs_ckboard = int.              % :         checker board (stipple)
:- func acs_darrow = int.               % v         arrow pointing down
:- func acs_degree = int.               % '         degree symbol
:- func acs_diamond = int.              % +         diamond
:- func acs_gequal = int.               % >         greater-than-or-equal-to
:- func acs_hline = int.                % -         horizontal line
:- func acs_lantern = int.              % #         lantern symbol
:- func acs_larrow = int.               % <         arrow pointing left
:- func acs_lequal = int.               % <         less-than-or-equal-to
:- func acs_llcorner = int.             % +         lower left-hand corner
:- func acs_lrcorner = int.             % +         lower right-hand corner
:- func acs_ltee = int.                 % +         left tee
:- func acs_nequal = int.               % !         not-equal
:- func acs_pi = int.                   % *         greek pi
:- func acs_plminus = int.              % #         plus/minus
:- func acs_plus = int.                 % +         plus
:- func acs_rarrow = int.               % >         arrow pointing right
:- func acs_rtee = int.                 % +         right tee
:- func acs_s1 = int.                   % -         scan line 1
:- func acs_s3 = int.                   % -         scan line 3
:- func acs_s7 = int.                   % -         scan line 7
:- func acs_s9 = int.                   % _         scan line 9
:- func acs_sterling = int.             % f         pound-sterling symbol
:- func acs_ttee = int.                 % +         top tee
:- func acs_uarrow = int.               % ^         arrow pointing up
:- func acs_ulcorner = int.             % +         upper left-hand corner
:- func acs_urcorner = int.             % +         upper right-hand corner
:- func acs_vline = int.                % |         vertical line



    % Character attributes.
    %
:- type attr.

:- func attr + attr = attr.             % Combines attributes.

:- func normal = attr.
:- func standout = attr.
:- func underline = attr.
:- func reverse = attr.
:- func blink = attr.
:- func dim = attr.
:- func bold = attr.
:- func invis = attr.
:- func fg_bg(colour, colour) = attr.   % Provide the appropriate colour pair no

    % Colour attributes.
    %
:- type colour.

:- func black = colour.
:- func red = colour.
:- func green = colour.
:- func yellow = colour.
:- func blue = colour.
:- func magenta = colour.
:- func cyan = colour.
:- func white = colour.

    % ------------------------------------------------------------------------ %
    % ------------------------------------------------------------------------ %

        % Panels are windows over the main display; they may be
        % stacked, moved, ordered and hidden.  Contents of panels
        % closer to the top of the stack obscure the parts of panels
        % they overlap that are lower in the stack.
        %
    :- module panel.

    :- interface.



    :- type panel.

        % new(Rows, Cols, Row, Col, Attr, Panel) creates a new panel
        % Panel whose size is given by (Rows, Cols) and whose position
        % on the display is given by (Row, Col).  The new panel starts
        % visible and at the top of the stack.  The default attributes
        % for the panel are set to Attr.
        %
    :- pred new(int::in, int::in, int::in, int::in, attr::in, panel::out,
                io__state::di, io__state::uo) is det.

        % Destroy a panel.
        %
    :- pred delete(panel::in, io__state::di, io__state::uo) is det.

        % Raise/lower a panel to the top/bottom of the stack.
        %
    :- pred raise(panel::in, io__state::di, io__state::uo) is det.

    :- pred lower(panel::in, io__state::di, io__state::uo) is det.

        % Hide/reveal a panel (revealing places it at the top of the stack).
        %
    :- pred hide(panel::in, io__state::di, io__state::uo) is det.

    :- pred reveal(panel::in, io__state::di, io__state::uo) is det.

        % Move a panel to (Row, Col) on the display.
        %
    :- pred relocate(panel::in, int::in, int::in,
                io__state::di, io__state::uo) is det.

        % Clear a panel.
        %
    :- pred clear(panel::in, io__state::di, io__state::uo) is det.

        % Move the virtual cursor to given row and column; (0, 0) are the
        % coordinates for the upper left hand corner of the panel.
        %
    :- pred move(panel::in, int::in, int::in,
                io__state::di, io__state::uo) is det.

        % Add a char/string to a panel with the given attributes.
        % Note that char codes are passed rather than plain chars.
        %
    :- pred addch(panel::in, attr::in, int::in,
                io__state::di, io__state::uo) is det.

    :- pred addstr(panel::in, attr::in, string::in,
                io__state::di, io__state::uo) is det.

        % Turn on/off or set attributes that will be applied by default.
        %
    :- pred attr_on(panel::in, attr::in, io__state::di, io__state::uo) is det.
    :- pred attr_off(panel::in, attr::in, io__state::di, io__state::uo) is det.
    :- pred attr_set(panel::in, attr::in, io__state::di, io__state::uo) is det.

        % Update the display (also calls doupdate).
        % NOTE that doupdate does not call update_panels.
        %
    :- pred update_panels(io__state::di, io__state::uo) is det.



        % Draws a border around the inside edge of the display.
        %
    :- pred border(panel::in, io__state::di, io__state::uo) is det.

        % Draws an horizontal line of length N moving to the right.
        %
    :- pred hline(panel::in, int::in, int::in,
                io__state::di, io__state::uo) is det.

        % Draws a vertical line of length N moving down.
        %
    :- pred vline(panel::in, int::in, int::in,
                io__state::di, io__state::uo) is det.

    :- end_module panel.

    % ------------------------------------------------------------------------ %
    % ------------------------------------------------------------------------ %

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %

:- implementation.

:- type attr == int.

:- type colour == int.

%----------------------------------------------------------------------------%

    % Untimely ripp'd from Thomas Conway and Robert Jeschofnik's
    % basics.m module in their ncurses interface.

:- pragma foreign_code("C","

#ifdef CONSERVATIVE_GC

/*      
** The addresses of the closures that we pass to curses
** will be stored by curses in malloc()'ed memory.
** However, it is essential that these pointers be
** visible to the garbage collector, otherwise it will
** think that the closures are unreferenced and reuse the storage.
** Hence we redefine malloc() and friends to call GC_malloc().
*/

void *malloc(size_t s)
{
        return GC_MALLOC(s);
}

void *calloc(size_t s, size_t n)
{
        void *t;
        t = GC_MALLOC(s*n);
        memset(t, 0, s*n);
        return t;
}

void *realloc(void *ptr, size_t s)
{
        return GC_REALLOC(ptr, s);
}

void free(void *ptr)
{
        GC_FREE(ptr);
}

#endif

").

%----------------------------------------------------------------------------%

:- pragma c_header_code("

#include <ncurses.h>
#include <panel.h>

        /*
        ** XXX We assume 64 available colour pairs and that the COLOR_s
        ** are assigned 0..7 (this is true in ncurses.h)
        */
#define FG_BG(fg, bg)          (((fg) << 3) | (bg))

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", start(IO0::di, IO::uo),
    [will_not_call_mercury], "

    initscr();                          /* Start the show */

    start_color();                      /* Enable colour */

    nonl();                             /* Don't translate \n */
    scrollok(stdscr, TRUE);             /* Scroll when output past bottom */
    leaveok(stdscr, TRUE);              /* Turn off the cursor */
    keypad(stdscr, TRUE);               /* Translate compound input chars */
    noecho();                           /* Don't echo typed characters */
    cbreak();                           /* Disable line buffering */

                                        /* Set up default colour pairs */
init_pair(FG_BG(COLOR_BLACK, COLOR_BLACK),      COLOR_BLACK, COLOR_BLACK);
init_pair(FG_BG(COLOR_BLACK, COLOR_RED),        COLOR_BLACK, COLOR_RED);
init_pair(FG_BG(COLOR_BLACK, COLOR_GREEN),      COLOR_BLACK, COLOR_GREEN);
init_pair(FG_BG(COLOR_BLACK, COLOR_YELLOW),     COLOR_BLACK, COLOR_YELLOW);
init_pair(FG_BG(COLOR_BLACK, COLOR_BLUE),       COLOR_BLACK, COLOR_BLUE);
init_pair(FG_BG(COLOR_BLACK, COLOR_MAGENTA),    COLOR_BLACK, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_BLACK, COLOR_CYAN),       COLOR_BLACK, COLOR_CYAN);
init_pair(FG_BG(COLOR_BLACK, COLOR_WHITE),      COLOR_BLACK, COLOR_WHITE);
init_pair(FG_BG(COLOR_RED, COLOR_BLACK),        COLOR_RED, COLOR_BLACK);
init_pair(FG_BG(COLOR_RED, COLOR_RED),          COLOR_RED, COLOR_RED);
init_pair(FG_BG(COLOR_RED, COLOR_GREEN),        COLOR_RED, COLOR_GREEN);
init_pair(FG_BG(COLOR_RED, COLOR_YELLOW),       COLOR_RED, COLOR_YELLOW);
init_pair(FG_BG(COLOR_RED, COLOR_BLUE),         COLOR_RED, COLOR_BLUE);
init_pair(FG_BG(COLOR_RED, COLOR_MAGENTA),      COLOR_RED, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_RED, COLOR_CYAN),         COLOR_RED, COLOR_CYAN);
init_pair(FG_BG(COLOR_RED, COLOR_WHITE),        COLOR_RED, COLOR_WHITE);
init_pair(FG_BG(COLOR_GREEN, COLOR_BLACK),      COLOR_GREEN, COLOR_BLACK);
init_pair(FG_BG(COLOR_GREEN, COLOR_RED),        COLOR_GREEN, COLOR_RED);
init_pair(FG_BG(COLOR_GREEN, COLOR_GREEN),      COLOR_GREEN, COLOR_GREEN);
init_pair(FG_BG(COLOR_GREEN, COLOR_YELLOW),     COLOR_GREEN, COLOR_YELLOW);
init_pair(FG_BG(COLOR_GREEN, COLOR_BLUE),       COLOR_GREEN, COLOR_BLUE);
init_pair(FG_BG(COLOR_GREEN, COLOR_MAGENTA),    COLOR_GREEN, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_GREEN, COLOR_CYAN),       COLOR_GREEN, COLOR_CYAN);
init_pair(FG_BG(COLOR_GREEN, COLOR_WHITE),      COLOR_GREEN, COLOR_WHITE);
init_pair(FG_BG(COLOR_YELLOW, COLOR_BLACK),     COLOR_YELLOW, COLOR_BLACK);
init_pair(FG_BG(COLOR_YELLOW, COLOR_RED),       COLOR_YELLOW, COLOR_RED);
init_pair(FG_BG(COLOR_YELLOW, COLOR_GREEN),     COLOR_YELLOW, COLOR_GREEN);
init_pair(FG_BG(COLOR_YELLOW, COLOR_YELLOW),    COLOR_YELLOW, COLOR_YELLOW);
init_pair(FG_BG(COLOR_YELLOW, COLOR_BLUE),      COLOR_YELLOW, COLOR_BLUE);
init_pair(FG_BG(COLOR_YELLOW, COLOR_MAGENTA),   COLOR_YELLOW, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_YELLOW, COLOR_CYAN),      COLOR_YELLOW, COLOR_CYAN);
init_pair(FG_BG(COLOR_YELLOW, COLOR_WHITE),     COLOR_YELLOW, COLOR_WHITE);
init_pair(FG_BG(COLOR_BLUE, COLOR_BLACK),       COLOR_BLUE, COLOR_BLACK);
init_pair(FG_BG(COLOR_BLUE, COLOR_RED),         COLOR_BLUE, COLOR_RED);
init_pair(FG_BG(COLOR_BLUE, COLOR_GREEN),       COLOR_BLUE, COLOR_GREEN);
init_pair(FG_BG(COLOR_BLUE, COLOR_YELLOW),      COLOR_BLUE, COLOR_YELLOW);
init_pair(FG_BG(COLOR_BLUE, COLOR_BLUE),        COLOR_BLUE, COLOR_BLUE);
init_pair(FG_BG(COLOR_BLUE, COLOR_MAGENTA),     COLOR_BLUE, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_BLUE, COLOR_CYAN),        COLOR_BLUE, COLOR_CYAN);
init_pair(FG_BG(COLOR_BLUE, COLOR_WHITE),       COLOR_BLUE, COLOR_WHITE);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_BLACK),    COLOR_MAGENTA, COLOR_BLACK);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_RED),      COLOR_MAGENTA, COLOR_RED);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_GREEN),    COLOR_MAGENTA, COLOR_GREEN);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_YELLOW),   COLOR_MAGENTA, COLOR_YELLOW);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_BLUE),     COLOR_MAGENTA, COLOR_BLUE);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_MAGENTA),  COLOR_MAGENTA, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_CYAN),     COLOR_MAGENTA, COLOR_CYAN);
init_pair(FG_BG(COLOR_MAGENTA, COLOR_WHITE),    COLOR_MAGENTA, COLOR_WHITE);
init_pair(FG_BG(COLOR_CYAN, COLOR_BLACK),       COLOR_CYAN, COLOR_BLACK);
init_pair(FG_BG(COLOR_CYAN, COLOR_RED),         COLOR_CYAN, COLOR_RED);
init_pair(FG_BG(COLOR_CYAN, COLOR_GREEN),       COLOR_CYAN, COLOR_GREEN);
init_pair(FG_BG(COLOR_CYAN, COLOR_YELLOW),      COLOR_CYAN, COLOR_YELLOW);
init_pair(FG_BG(COLOR_CYAN, COLOR_BLUE),        COLOR_CYAN, COLOR_BLUE);
init_pair(FG_BG(COLOR_CYAN, COLOR_MAGENTA),     COLOR_CYAN, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_CYAN, COLOR_CYAN),        COLOR_CYAN, COLOR_CYAN);
init_pair(FG_BG(COLOR_CYAN, COLOR_WHITE),       COLOR_CYAN, COLOR_WHITE);
init_pair(FG_BG(COLOR_WHITE, COLOR_BLACK),      COLOR_WHITE, COLOR_BLACK);
init_pair(FG_BG(COLOR_WHITE, COLOR_RED),        COLOR_WHITE, COLOR_RED);
init_pair(FG_BG(COLOR_WHITE, COLOR_GREEN),      COLOR_WHITE, COLOR_GREEN);
init_pair(FG_BG(COLOR_WHITE, COLOR_YELLOW),     COLOR_WHITE, COLOR_YELLOW);
init_pair(FG_BG(COLOR_WHITE, COLOR_BLUE),       COLOR_WHITE, COLOR_BLUE);
init_pair(FG_BG(COLOR_WHITE, COLOR_MAGENTA),    COLOR_WHITE, COLOR_MAGENTA);
init_pair(FG_BG(COLOR_WHITE, COLOR_CYAN),       COLOR_WHITE, COLOR_CYAN);
init_pair(FG_BG(COLOR_WHITE, COLOR_WHITE),      COLOR_WHITE, COLOR_WHITE);

    IO = IO0;
").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", stop(IO0::di, IO::uo),
    [will_not_call_mercury], "

    endwin();
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

session(P) -->
    start,
    P,
    stop.

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", rows_cols(Rows::out, Cols::out, IO0::di, IO::uo),
    [will_not_call_mercury], "

    getmaxyx(stdscr, Rows, Cols);
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", move(Row::in, Col::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    move(Row, Col);
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", clear(IO0::di, IO::uo),
    [will_not_call_mercury], "

    clear();
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", addch(Attr::in, CharCode::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    addch((chtype)Attr | (chtype)CharCode);
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

addstr(Attr, Str) -->
    string__foldl(
        ( pred(Char::in, di, uo) is det --> addch(Attr, char__to_int(Char)) ),
        Str
    ).

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", attr_on(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    attron(Attr);
    IO = IO0;

").
:- pragma foreign_proc("C", attr_off(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    attroff(Attr);
    IO = IO0;

").
:- pragma foreign_proc("C", attr_set(Attr::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    attrset(Attr);
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", doupdate(IO0::di, IO::uo),
    [will_not_call_mercury], "

    doupdate();
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", getch(CharCode::out, IO0::di, IO::uo),
    [will_not_call_mercury], "

    CharCode = getch();
    IO = IO0;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", key_down = (K::out),
    [will_not_call_mercury], "
    K = KEY_DOWN;
").
:- pragma foreign_proc("C", key_up = (K::out),
    [will_not_call_mercury], "
    K = KEY_UP;
").
:- pragma foreign_proc("C", key_left = (K::out),
    [will_not_call_mercury], "
    K = KEY_LEFT;
").
:- pragma foreign_proc("C", key_right = (K::out),
    [will_not_call_mercury], "
    K = KEY_RIGHT;
").
:- pragma foreign_proc("C", key_home = (K::out),
    [will_not_call_mercury], "
    K = KEY_HOME;
").
:- pragma foreign_proc("C", key_backspace = (K::out),
    [will_not_call_mercury], "
    K = KEY_BACKSPACE;
").
:- pragma foreign_proc("C", key_f(N::in) = (K::out),
    [will_not_call_mercury], "
    K = KEY_F( N);
").
:- pragma foreign_proc("C", key_del = (K::out),
    [will_not_call_mercury], "
    K = KEY_DC;
").
:- pragma foreign_proc("C", key_ins = (K::out),
    [will_not_call_mercury], "
    K = KEY_IC;
").
:- pragma foreign_proc("C", key_pageup = (K::out),
    [will_not_call_mercury], "
    K = KEY_NPAGE;
").
:- pragma foreign_proc("C", key_pagedown = (K::out),
    [will_not_call_mercury], "
    K = KEY_PPAGE;
").
:- pragma foreign_proc("C", key_a1 = (K::out),
    [will_not_call_mercury], "
    K = KEY_A1;
").
:- pragma foreign_proc("C", key_a3 = (K::out),
    [will_not_call_mercury], "
    K = KEY_A3;
").
:- pragma foreign_proc("C", key_b2 = (K::out),
    [will_not_call_mercury], "
    K = KEY_B2;
").
:- pragma foreign_proc("C", key_c1 = (K::out),
    [will_not_call_mercury], "
    K = KEY_C1;
").
:- pragma foreign_proc("C", key_c3 = (K::out),
    [will_not_call_mercury], "
    K = KEY_C3;
").
:- pragma foreign_proc("C", key_enter = (K::out),
    [will_not_call_mercury], "
    K = KEY_ENTER;
").
:- pragma foreign_proc("C", key_end = (K::out),
    [will_not_call_mercury], "
    K = KEY_END;
").
:- pragma foreign_proc("C", key_resize = (K::out),
    [will_not_call_mercury], "
    K = KEY_RESIZE;
").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", acs_block = (C::out),
    [will_not_call_mercury], "
    C = ACS_BLOCK;
").
:- pragma foreign_proc("C", acs_board = (C::out),
    [will_not_call_mercury], "
    C = ACS_BOARD;
").
:- pragma foreign_proc("C", acs_btee = (C::out),
    [will_not_call_mercury], "
    C = ACS_BTEE;
").
:- pragma foreign_proc("C", acs_bullet = (C::out),
    [will_not_call_mercury], "
    C = ACS_BULLET;
").
:- pragma foreign_proc("C", acs_ckboard = (C::out),
    [will_not_call_mercury], "
    C = ACS_CKBOARD;
").
:- pragma foreign_proc("C", acs_darrow = (C::out),
    [will_not_call_mercury], "
    C = ACS_DARROW;
").
:- pragma foreign_proc("C", acs_degree = (C::out),
    [will_not_call_mercury], "
    C = ACS_DEGREE;
").
:- pragma foreign_proc("C", acs_diamond = (C::out),
    [will_not_call_mercury], "
    C = ACS_DIAMOND;
").
:- pragma foreign_proc("C", acs_gequal = (C::out),
    [will_not_call_mercury], "
    C = ACS_GEQUAL;
").
:- pragma foreign_proc("C", acs_hline = (C::out),
    [will_not_call_mercury], "
    C = ACS_HLINE;
").
:- pragma foreign_proc("C", acs_lantern = (C::out),
    [will_not_call_mercury], "
    C = ACS_LANTERN;
").
:- pragma foreign_proc("C", acs_larrow = (C::out),
    [will_not_call_mercury], "
    C = ACS_LARROW;
").
:- pragma foreign_proc("C", acs_lequal = (C::out),
    [will_not_call_mercury], "
    C = ACS_LEQUAL;
").
:- pragma foreign_proc("C", acs_llcorner = (C::out),
    [will_not_call_mercury], "
    C = ACS_LLCORNER;
").
:- pragma foreign_proc("C", acs_lrcorner = (C::out),
    [will_not_call_mercury], "
    C = ACS_LRCORNER;
").
:- pragma foreign_proc("C", acs_ltee = (C::out),
    [will_not_call_mercury], "
    C = ACS_LTEE;
").
:- pragma foreign_proc("C", acs_nequal = (C::out),
    [will_not_call_mercury], "
    C = ACS_NEQUAL;
").
:- pragma foreign_proc("C", acs_pi = (C::out),
    [will_not_call_mercury], "
    C = ACS_PI;
").
:- pragma foreign_proc("C", acs_plminus = (C::out),
    [will_not_call_mercury], "
    C = ACS_PLMINUS;
").
:- pragma foreign_proc("C", acs_plus = (C::out),
    [will_not_call_mercury], "
    C = ACS_PLUS;
").
:- pragma foreign_proc("C", acs_rarrow = (C::out),
    [will_not_call_mercury], "
    C = ACS_RARROW;
").
:- pragma foreign_proc("C", acs_rtee = (C::out),
    [will_not_call_mercury], "
    C = ACS_RTEE;
").
:- pragma foreign_proc("C", acs_s1 = (C::out),
    [will_not_call_mercury], "
    C = ACS_S1;
").
:- pragma foreign_proc("C", acs_s3 = (C::out),
    [will_not_call_mercury], "
    C = ACS_S3;
").
:- pragma foreign_proc("C", acs_s7 = (C::out),
    [will_not_call_mercury], "
    C = ACS_S7;
").
:- pragma foreign_proc("C", acs_s9 = (C::out),
    [will_not_call_mercury], "
    C = ACS_S9;
").
:- pragma foreign_proc("C", acs_sterling = (C::out),
    [will_not_call_mercury], "
    C = ACS_STERLING;
").
:- pragma foreign_proc("C", acs_ttee = (C::out),
    [will_not_call_mercury], "
    C = ACS_TTEE;
").
:- pragma foreign_proc("C", acs_uarrow = (C::out),
    [will_not_call_mercury], "
    C = ACS_UARROW;
").
:- pragma foreign_proc("C", acs_ulcorner = (C::out),
    [will_not_call_mercury], "
    C = ACS_ULCORNER;
").
:- pragma foreign_proc("C", acs_urcorner = (C::out),
    [will_not_call_mercury], "
    C = ACS_URCORNER;
").
:- pragma foreign_proc("C", acs_vline = (C::out),
    [will_not_call_mercury], "
    C = ACS_VLINE;
").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", curs:((Attr1::in) + (Attr2::in)) = (Attr::out),
    [will_not_call_mercury], "

    Attr = (chtype)Attr1 | (chtype)Attr2;

").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", normal = (A::out),
    [will_not_call_mercury], "
    A = A_NORMAL;
").
:- pragma foreign_proc("C", standout = (A::out),
    [will_not_call_mercury], "
    A = A_STANDOUT;
").
:- pragma foreign_proc("C", underline = (A::out),
    [will_not_call_mercury], "
    A = A_UNDERLINE;
").
:- pragma foreign_proc("C", reverse = (A::out),
    [will_not_call_mercury], "
    A = A_REVERSE;
").
:- pragma foreign_proc("C", blink = (A::out),
    [will_not_call_mercury], "
    A = A_BLINK;
").
:- pragma foreign_proc("C", dim = (A::out),
    [will_not_call_mercury], "
    A = A_DIM;
").
:- pragma foreign_proc("C", bold = (A::out),
    [will_not_call_mercury], "
    A = A_BOLD;
").
:- pragma foreign_proc("C", invis = (A::out),
    [will_not_call_mercury], "
    A = A_INVIS;
").
:- pragma foreign_proc("C", fg_bg(Fg::in, Bg::in) = (A::out),
    [will_not_call_mercury], "
    A = COLOR_PAIR(FG_BG(Fg, Bg));
").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", black = (C::out),
    [will_not_call_mercury], "
    C = COLOR_BLACK;
").
:- pragma foreign_proc("C", red = (C::out),
    [will_not_call_mercury], "
    C = COLOR_RED;
").
:- pragma foreign_proc("C", green = (C::out),
    [will_not_call_mercury], "
    C = COLOR_GREEN;
").
:- pragma foreign_proc("C", yellow = (C::out),
    [will_not_call_mercury], "
    C = COLOR_YELLOW;
").
:- pragma foreign_proc("C", blue = (C::out),
    [will_not_call_mercury], "
    C = COLOR_BLUE;
").
:- pragma foreign_proc("C", magenta = (C::out),
    [will_not_call_mercury], "
    C = COLOR_MAGENTA;
").
:- pragma foreign_proc("C", cyan = (C::out),
    [will_not_call_mercury], "
    C = COLOR_CYAN;
").
:- pragma foreign_proc("C", white = (C::out),
    [will_not_call_mercury], "
    C = COLOR_WHITE;
").

% ---------------------------------------------------------------------------- %

:- pragma foreign_proc("C", border(IO0::di, IO::uo),
    [will_not_call_mercury], "

    border(0, 0, 0, 0, 0, 0, 0, 0);
    IO = IO0;

").

:- pragma foreign_proc("C", hline(C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    hline(C, N);
    IO = IO0;

").

:- pragma foreign_proc("C", vline(C::in, N::in, IO0::di, IO::uo),
    [will_not_call_mercury], "

    vline(C, N);
    IO = IO0;

").

    % ------------------------------------------------------------------------ %
    % ------------------------------------------------------------------------ %

    :- module panel.

    :- implementation.

    :- type panel == c_pointer.

    % ------------------------------------------------------------------------ %

    :- pragma c_header_code("

    #include <ncurses.h>
    #include <panel.h>

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", new(Rows::in, Cols::in, Row::in, Col::in, Attr::in,
                            Panel::out, IO0::di, IO::uo),
        [will_not_call_mercury], "

        WINDOW *w = newwin(Rows, Cols, Row, Col);
        scrollok(w, TRUE);
        wattrset(w, Attr);
        wcolor_set(w, Attr, NULL);
        wclear(w);
        (PANEL *)Panel = new_panel(w);

        IO = IO0;
    
    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", delete(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        delwin(panel_window((PANEL *)Panel));
        del_panel((PANEL *)Panel);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", raise(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        top_panel((PANEL *)Panel);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", lower(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        bottom_panel((PANEL *)Panel);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", hide(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        hide_panel((PANEL *)Panel);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", reveal(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        show_panel((PANEL *)Panel);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", relocate(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        move_panel((PANEL *)Panel, Row, Col);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", clear(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wclear(panel_window((PANEL *)Panel));

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", move(Panel::in, Row::in, Col::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wmove(panel_window((PANEL *)Panel), Row, Col);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", addch(Panel::in, Attr::in, CharCode::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        waddch(panel_window((PANEL *)Panel), (chtype)Attr | (chtype)CharCode);

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    addstr(Panel, Attr, Str) -->
        string__foldl(
            ( pred(Char::in, di, uo) is det -->
                addch(Panel, Attr, char__to_int(Char))
            ),
            Str
        ).

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", attr_on(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wattron(panel_window((PANEL *)Panel), Attr);
        IO = IO0;

    ").
    :- pragma foreign_proc("C", attr_off(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wattroff(panel_window((PANEL *)Panel), Attr);
        IO = IO0;

    ").
    :- pragma foreign_proc("C", attr_set(Panel::in, Attr::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wattrset(panel_window((PANEL *)Panel), Attr);
        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", update_panels(IO0::di, IO::uo),
        [will_not_call_mercury], "

        update_panels();
        doupdate();

        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- pragma foreign_proc("C", border(Panel::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wborder(panel_window((PANEL *)Panel), 0, 0, 0, 0, 0, 0, 0, 0);
        IO = IO0;

    ").

    :- pragma foreign_proc("C", hline(Panel::in, C::in, N::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        whline(panel_window((PANEL *)Panel), C, N);
        IO = IO0;

    ").

    :- pragma foreign_proc("C", vline(Panel::in, C::in, N::in, IO0::di, IO::uo),
        [will_not_call_mercury], "

        wvline(panel_window((PANEL *)Panel), C, N);
        IO = IO0;

    ").

    % ------------------------------------------------------------------------ %

    :- end_module panel.

    % ------------------------------------------------------------------------ %
    % ------------------------------------------------------------------------ %

% ---------------------------------------------------------------------------- %
% ---------------------------------------------------------------------------- %
