%----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
%
% File:          basics.m
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

:- module mcurses__basics.
:- interface.

:- import_module char, int, io, string.

  % Initialise curses. This is used by user.m, and should not be called by the
  % programmer.
:- pred init(io__state, io__state).
:- mode init(di, uo) is det.

  % Shutdown curses. This is required before exiting your program, or else you
  % will be left with a practically unusable terminal.
:- pred endwin(io__state, io__state).
:- mode endwin(di, uo) is det.

  % Initialise the colour mode for curses. This must be called before
  % attempting to use anything with colour.
:- pred start_colour(io__state, io__state).
:- mode start_colour(di, uo) is det.

  % Update the curses screen.
:- pred update(io__state, io__state).
:- mode update(di, uo) is det.

  % Perform a doupdate.
  % (see the curses man page for descriptions of update and doupdate)
:- pred doupdate(io__state, io__state).
:- mode doupdate(di, uo) is det.

  % Clear the curses screen.
:- pred clear(io__state, io__state).
:- mode clear(di, uo) is det.

  % cursor(X, Y, IO0, IO)
  % places the cursor at position X,Y
:- pred cursor(int, int, io__state, io__state).
:- mode cursor(in, in, di, uo) is det.

  % Place a string on the screen, starting at the current cursor position
:- pred putstr(string, io__state, io__state).
:- mode putstr(in, di, uo) is det.

  % Place a single character on the screen at the current cursor position
:- pred putchar(char, io__state, io__state).
:- mode putchar(in, di, uo) is det.

  % cols(Cols, IO0, IO)
  % retrieves the number of columns in the screen
:- pred cols(int, io__state, io__state).
:- mode cols(out, di, uo) is det.

  % rows(Rows, IO0, IO)
  % retrieves the number of rows in the screen
:- pred rows(int, io__state, io__state).
:- mode rows(out, di, uo) is det.

  % getkey(Key, IO0, IO)
  % Wait for the next keypress from the user, and return it as Key
:- pred getkey(int, io__state, io__state).
:- mode getkey(out, di, uo) is det.

%----------------------------------------------------------------------------%

  % Functions to return scancodes for some common keypresses
  
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

  % Functions to return colours for characters
  
:- func black = int.
:- func green = int.
:- func red = int.
:- func cyan = int.
:- func white = int.
:- func magenta = int.
:- func blue = int.
:- func yellow = int.

  % Functions to return attributes for characters
  
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

:- pragma c_header_code("
	#include <curses.h>
	#include <term.h>
").

%----------------------------------------------------------------------------%

:- pragma c_code(init(IO0::di, IO::uo),
        [will_not_call_mercury], "
	WINDOW *w;
	w = initscr();
	noecho();
	cbreak();
	keypad(w, TRUE);
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(endwin(IO0::di, IO::uo),
        [will_not_call_mercury], "
	endwin();
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(start_colour(IO0::di, IO::uo),
	[will_not_call_mercury], "
        start_color();
	init_pair(COLOR_BLACK, COLOR_BLACK, COLOR_BLACK);
	init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
	init_pair(COLOR_RED, COLOR_RED, COLOR_BLACK);
	init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK);
	init_pair(COLOR_WHITE, COLOR_WHITE, COLOR_BLACK);
	init_pair(COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
	init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK);
	init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(doupdate(IO0::di, IO::uo),
        [will_not_call_mercury], "
	doupdate();
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(update(IO0::di, IO::uo),
        [will_not_call_mercury], "
	refresh();
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(clear(IO0::di, IO::uo),
        [will_not_call_mercury], "
	clear();
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(cursor(X::in, Y::in, IO0::di, IO::uo),
        [will_not_call_mercury], "
	move(Y, X);
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(putstr(Str::in, IO0::di, IO::uo),
        [will_not_call_mercury], "
	      addstr(Str);
	      IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(putchar(C::in, IO0::di, IO::uo),
	[will_not_call_mercury], "
	addch((chtype) C);
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(cols(C::out, IO0::di, IO::uo),
        [will_not_call_mercury], "
	C = tigetnum(""cols"");
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(rows(R::out, IO0::di, IO::uo),
        [will_not_call_mercury], "
	R = tigetnum(""lines"");
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(getkey(C::out, IO0::di, IO::uo),
        [will_not_call_mercury], "
	C = getch();
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pragma c_code(break = (I::out), [will_not_call_mercury], "I = KEY_BREAK;").
:- pragma c_code(down = (I::out), [will_not_call_mercury], "I = KEY_DOWN;").
:- pragma c_code(up = (I::out), [will_not_call_mercury], "I = KEY_UP;").
:- pragma c_code(left = (I::out), [will_not_call_mercury], "I = KEY_LEFT;").
:- pragma c_code(right = (I::out), [will_not_call_mercury], "I = KEY_RIGHT;").
:- pragma c_code(home = (I::out), [will_not_call_mercury], "I = KEY_HOME;").
:- pragma c_code(backspace = (I::out), [will_not_call_mercury],
        "I = KEY_BACKSPACE;").
:- pragma c_code(fn(N::in) = (I::out), [will_not_call_mercury], "I=KEY_F(N);").
:- pragma c_code(pageup = (I::out), [will_not_call_mercury], "I = KEY_PPAGE;").
:- pragma c_code(pagedown = (I::out), [will_not_call_mercury], "I=KEY_NPAGE;").

%----------------------------------------------------------------------------%

:- pragma c_code(black = (C::out), [will_not_call_mercury], "C=COLOR_BLACK;").
:- pragma c_code(green = (C::out), [will_not_call_mercury], "C=COLOR_GREEN;").
:- pragma c_code(red = (C::out), [will_not_call_mercury], "C = COLOR_RED;").
:- pragma c_code(cyan = (C::out), [will_not_call_mercury], "C = COLOR_CYAN;").
:- pragma c_code(white = (C::out), [will_not_call_mercury], "C=COLOR_WHITE;").
:- pragma c_code(magenta = (C::out), [will_not_call_mercury],
        "C = COLOR_MAGENTA;").
:- pragma c_code(blue = (C::out), [will_not_call_mercury], "C = COLOR_BLUE;").
:- pragma c_code(yellow = (C::out), [will_not_call_mercury],
        "C = COLOR_YELLOW;").

%----------------------------------------------------------------------------%

:- pragma c_code(normal = (A::out), [will_not_call_mercury], "A = A_NORMAL;").
:- pragma c_code(standout = (A::out), [will_not_call_mercury],
        "A = A_STANDOUT;").
:- pragma c_code(underline = (A::out), [will_not_call_mercury],
        "A = A_UNDERLINE;").
:- pragma c_code(reverse = (A::out), [will_not_call_mercury], "A=A_REVERSE;").
:- pragma c_code(blink = (A::out), [will_not_call_mercury], "A = A_BLINK;").
:- pragma c_code(dim = (A::out), [will_not_call_mercury], "A = A_DIM;").
:- pragma c_code(bold = (A::out), [will_not_call_mercury], "A = A_BOLD;").
:- pragma c_code(protect = (A::out), [will_not_call_mercury],
        "A = A_PROTECT;").
:- pragma c_code(invis = (A::out), [will_not_call_mercury], "A = A_INVIS;").
:- pragma c_code(altcharset = (A::out), [will_not_call_mercury],
        "A = A_ALTCHARSET;").
:- pragma c_code(chartext = (A::out), [will_not_call_mercury],
        "A = A_CHARTEXT;").
:- pragma c_code(colour(C::in) = (A::out), [will_not_call_mercury],
        "A = COLOR_PAIR(C);").

%----------------------------------------------------------------------------%

:- pragma c_code("

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

