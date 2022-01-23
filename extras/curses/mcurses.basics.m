%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000, 2010 The University of Melbourne.
% Copyright (C) 2019, 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%
%
% File:          mcurses.basics.m
% Main author:   conway
% Maintained by: rejj
% Stability:     Medium
%
% This module defines the low-level bindings to the C library for (n)curses.
%
% Please note that this is still a partial binding; it does not provide
% complete curses functionality.
% Major things this binding implements:
%     * Creation, destruction, clearing, raising, and lowering of arbitary
%       windows.
%     * Scrolling.
%     * Colour on a character by character basis.
%
% See the man pages for ncurses for detailed information about using the
% curses libraries.
%
%----------------------------------------------------------------------------%

:- module mcurses.basics.
:- interface.

:- import_module char.
:- import_module int.
:- import_module io.
:- import_module string.

%----------------------------------------------------------------------------%

    % Initialise curses.
    % This is used by user.m, and should not be called by the programmer.
    %
:- pred init(io::di, io::uo) is det.

    % Shutdown curses.
    % This is required before exiting your program, or else you will be left
    % with a practically unusable terminal.
    %
:- pred endwin(io::di, io::uo) is det.

    % Initialise the colour mode for curses.
    % This must be called before attempting to use anything with colour.
    %
:- pred start_colour(io::di, io::uo) is det.

    % Update the curses screen.
    %
:- pred update(io::di, io::uo) is det.

    % Perform a doupdate.
    % (see the curses man page for descriptions of update and doupdate)
:- pred doupdate(io::di, io::uo) is det.

    % Clear the curses screen.
    %
:- pred clear(io::di, io::uo) is det.

    % cursor(X, Y, !IO):
    % Places the cursor at position X, Y.
    %
:- pred cursor(int::in, int::in, io::di, io::uo) is det.

    % Place a string on the screen, starting at the current cursor position.
    %
:- pred putstr(string::in, io::di, io::uo) is det.

    % Place a single character on the screen at the current cursor position.
    %
:- pred putchar(char::in, io::di, io::uo) is det.

    % cols(Cols, !IO):
    % Retrieves the number of columns in the screen.
    %
:- pred cols(int::out, io::di, io::uo) is det.

    % rows(Rows, !IO):
    % Retrieves the number of rows in the screen.
    %
:- pred rows(int::out, io::di, io::uo) is det.

    % getkey(Key, !IO):
    % Wait for the next keypress from the user, and return it as Key.
    %
:- pred getkey(int::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%

    % Functions to return scancodes for some common keypresses.
    %
:- func break = int.
:- func down = int.
:- func up = int.
:- func left = int.
:- func right = int.
:- func home = int.
:- func backspace = int.
:- func fn(int) = int.
:- func pageup = int.
:- func pagedown = int.

%----------------------------------------------------------------------------%

    % Functions to return colours for characters.
    %
:- func black = int.
:- func green = int.
:- func red = int.
:- func cyan = int.
:- func white = int.
:- func magenta = int.
:- func blue = int.
:- func yellow = int.

    % Functions to return attributes for characters.
    %
:- func normal = int.
:- func standout = int.
:- func underline = int.
:- func reverse = int.
:- func blink = int.
:- func dim = int.
:- func bold = int.
:- func protect = int.
:- func invis = int.
:- func altcharset = int.
:- func chartext = int.
:- func colour(int) = int.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- pragma foreign_decl("C", "
    #include <curses.h>
    #include <term.h>
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    init(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    WINDOW *w;
    w = initscr();
    noecho();
    cbreak();
    keypad(w, TRUE);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    endwin(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    endwin();
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    start_colour(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    start_color();
    init_pair(COLOR_BLACK, COLOR_BLACK, COLOR_BLACK);
    init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
    init_pair(COLOR_RED, COLOR_RED, COLOR_BLACK);
    init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK);
    init_pair(COLOR_WHITE, COLOR_WHITE, COLOR_BLACK);
    init_pair(COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
    init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK);
    init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    doupdate(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    doupdate();
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    update(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    refresh();
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    clear(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    clear();
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cursor(X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    move(Y, X);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    putstr(Str::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    addstr(Str);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    putchar(C::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    addch((chtype) C);
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    cols(C::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    C = tigetnum((char *) ""cols"");
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rows(R::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    R = tigetnum((char *) ""lines"");
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getkey(C::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    C = getch();
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    break = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_BREAK;
").
:- pragma foreign_proc("C",
    down = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_DOWN;
").
:- pragma foreign_proc("C",
    up = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_UP;
").
:- pragma foreign_proc("C",
    left = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_LEFT;
").
:- pragma foreign_proc("C",
    right = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_RIGHT;
").
:- pragma foreign_proc("C",
    home = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_HOME;
").
:- pragma foreign_proc("C",
    backspace = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_BACKSPACE;
").
:- pragma foreign_proc("C",
    fn(N::in) = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_F(N);
").
:- pragma foreign_proc("C",
    pageup = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_PPAGE;
").
:- pragma foreign_proc("C",
    pagedown = (I::out),
    [promise_pure, will_not_call_mercury],
"
    I = KEY_NPAGE;
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    black = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_BLACK;
").
:- pragma foreign_proc("C",
    green = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_GREEN;
").
:- pragma foreign_proc("C",
    red = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_RED;
").
:- pragma foreign_proc("C",
    cyan = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_CYAN;
").
:- pragma foreign_proc("C",
    white = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_WHITE;
").
:- pragma foreign_proc("C",
    magenta = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_MAGENTA;
").
:- pragma foreign_proc("C",
    blue = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_BLUE;
").
:- pragma foreign_proc("C",
    yellow = (C::out),
    [promise_pure, will_not_call_mercury],
"
    C = COLOR_YELLOW;
").

%----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    normal = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_NORMAL;
").
:- pragma foreign_proc("C",
    standout = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_STANDOUT;
").
:- pragma foreign_proc("C",
    underline = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_UNDERLINE;
").
:- pragma foreign_proc("C",
    reverse = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_REVERSE;
").
:- pragma foreign_proc("C",
    blink = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_BLINK;
").
:- pragma foreign_proc("C",
    dim = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_DIM;
").
:- pragma foreign_proc("C",
    bold = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_BOLD;
").
:- pragma foreign_proc("C",
    protect = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_PROTECT;
").
:- pragma foreign_proc("C",
    invis = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_INVIS;
").
:- pragma foreign_proc("C",
    altcharset = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_ALTCHARSET;
").
:- pragma foreign_proc("C",
    chartext = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = A_CHARTEXT;
").
:- pragma foreign_proc("C",
    colour(C::in) = (A::out),
    [promise_pure, will_not_call_mercury],
"
    A = COLOR_PAIR(C);
").

%----------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#ifdef MR_CONSERVATIVE_GC

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
:- end_module mcurses.basics.
%----------------------------------------------------------------------------%
