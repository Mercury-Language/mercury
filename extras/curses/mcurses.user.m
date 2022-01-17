%----------------------------------------------------------------------------%
% Copyright (C) 1994-2000, 2005-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
%
% File:          user.m
% Main author:   conway
% Maintained by: rejj
% Stability:     Medium
%
% This module provides the user-level functionality for the (n)curses binding.
%
% Please note that this is still a partial binding; it does not provide
% complete curses functionality.
% Major things this binding implements:
%     * Creation, destruction, clearing, raising, and lowering of arbitary
%       windows.
%     * Scrolling.
%     * Colour on a character by character basis.
%
%----------------------------------------------------------------------------%
:- module mcurses.user.
:- interface.

:- import_module char, io, list, pair.

  % The ADT used to represent a curses window.
:- type win.

  % Window options.
:- type wopt
	--->	border      % Place a border around the window
	;	title(string)   % Give the window a title
	.

  % Character attributes.
  % These modify the way a character is drawn on the screen. See the curses
  % documentation for a detailed description of each attribute.
:- type cattr
	--->
		normal;
		standout;
		underline;
		reverse;
		blink;
		dim;
		bold;
		protect;
		invis;
		altcharset;
		chartext;
		colour(colour).

  % Colours available for use in displaying characters.
  %
  % XXX Currently, there is no functionality provided for the customisation of
  % colours, only the 8 default colours from curses can be used.
:- type colour
	--->
		black;
		green;
		red;
		cyan;
		white;
		magenta;
		blue;
		yellow.

  % The type used to represent a character with its attributes.
:- type chtype == pair(char, list(cattr)).

  % init(Root, IO0, IO)
  % Initialise curses, giving back the root window.
	% The initialisation procedures in this library turn off echoing, and
	% enable character-at-a-time input.
:- pred init(win, io, io).
:- mode init(out, di, uo) is det.

  % Redraw the screen
:- pred redraw(io, io).
:- mode redraw(di, uo) is det.

  % Refresh the screen.
:- pred refresh(io, io).
:- mode refresh(di, uo) is det.

  % create(Parent, Options, ParentX, ParentY, NumCols, NumRows, Child, IO0, IO)
  % create a new window, which will be a child of the window Parent. It is
  % created at position ParentX, ParentY in the parent window, and is of size
  % NumCols, NumRows.
:- pred create(win, list(wopt), int, int, int, int, win, io, io).
:- mode create(in, in, in, in, in, in, out, di, uo) is det.

  % destroy the specified window.
:- pred destroy(win, io, io).
:- mode destroy(in, di, uo) is det.

  % Hide the specified window.
:- pred hide(win, io, io).
:- mode hide(in, di, uo) is det.

  % Show the (previously hidden) specified window
:- pred show(win, io, io).
:- mode show(in, di, uo) is det.

  % Raise the specified window.
:- pred raise(win, io, io).
:- mode raise(in, di, uo) is det.

  % Lower the specified window.
:- pred lower(win, io, io).
:- mode lower(in, di, uo) is det.

  % Clear the specified window. Fills the window with spaces.
:- pred clear(win, io, io).
:- mode clear(in, di, uo) is det.

  % place_char(Window, X, Y, (Char - Attributes), IO0, IO)
  % Place a character into Window at position X, Y.
:- pred place_char(win, int, int, chtype, io, io).
:- mode place_char(in, in, in, in, di, uo) is det.

  % place_string(Window, X, Y, String, IO0, IO)
  % Place a string into Window at position X, Y.
  %
  % XXX Note that presently, character attributes are not supported for
  % strings
:- pred place_string(win, int, int, string, io, io).
:- mode place_string(in, in, in, in, di, uo) is det.

  % scroll(Window, Amount, IO0, IO)
  % Scroll Window upwards by Amount lines.
:- pred scroll(win, int, io, io).
:- mode scroll(in, in, di, uo) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
	
	#include <curses.h>
	#include <term.h>
").

%----------------------------------------------------------------------------%

:- import_module mcurses.basics.
:- import_module mcurses.misc.

:- import_module array.
:- import_module int.
:- import_module require.
:- import_module solutions.
:- import_module store.
:- import_module string.

:- type curse_store_type ---> curse_store_type.
:- type curse_store == store(curse_store_type).
:- type win	== store_mutvar(window, curse_store_type).

:- type window
	--->	win(
			win,		% parent
			int,		% width
			int,		% height
			list(wopt),
			array(chtype),	% contents
			list(child),	% visible
			list(child)	% hidden
		).

:- type child
	--->	child(
			int,		% x
			int,		% y
			win
		).

:- type cursor
	--->	cursor(int, int). % X, Y

%----------------------------------------------------------------------------%

init(Win) -->
	init,
	cols(Cols),
	rows(Rows),
	{ array.init(Cols*Rows, ' ' - [], Data) },
	{ MakeWin = (func(Self) = win(Self, Cols, Rows, [], Data, [], [])) },
	{ init_curse_store(Curse0) },
	{ store.new_cyclic_mutvar(MakeWin, Win, Curse0, Curse) },
	set_curse_store(Curse),
	set_root(Win),
	refresh.

%----------------------------------------------------------------------------%

create(Parent, Opts, X, Y, W, H, Child) -->
	get_win(Parent, PWindow0),
	{ PWindow0 = win(P0, W0, H0, Opts0, PData, Visi0, Hidden) },
	{ require(((pred) is semidet :-
		X >= 0, Y >= 0,
		X+W =< W0,
		Y+H =< H0
	), "create: window out of range!") },
	{ array.init(W*H, ' ' -[], Data) },
	{ CWindow = win(P0, W, H, Opts, Data, [], []) },
	new_win(CWindow, Child),
	{ list.append(Visi0, [child(X, Y, Child)], Visi) },
	{ PWindow = win(Parent, W0, H0, Opts0, PData, Visi, Hidden) },
	set_win(Parent, PWindow).

%----------------------------------------------------------------------------%

destroy(Win) -->
	get_win(Win, Window),
	{ Window = win(Parent, _, _, _, _, _, _) },
	( { Parent \= Win } -> % can't kill the root window
		get_win(Parent, PWindow0),
		{ PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden) },
		{ filter((pred(Child::in) is semidet :-
			\+ Child = child(_, _, Win)
		), Visi0, Visi) },
		{ PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden) },
		set_win(Parent, PWindow)
	;
		[]
	).

%----------------------------------------------------------------------------%

redraw -->
	get_root(Root),
	set_cursor(cursor(0, 0)),
	refresh(Root),
	doupdate.

%----------------------------------------------------------------------------%

refresh -->
	get_root(Root),
	clear,
	set_cursor(cursor(0, 0)),
	refresh(Root),
	update.

:- pred refresh(win, io, io).
:- mode refresh(in, di, uo) is det.

refresh(Win) -->
	get_win(Win, Window),
	{ Window = win(_Parent, Cols, Rows, Opts, Data, Visi, _Hidden) },
	get_cursor(cursor(X0, Y0)),
	{ solutions((pred(Ti::out) is nondet :-
		list.member(ZZ, Opts),
		ZZ = title(Ti)
	), Titles) },
	( { list.member(border, Opts) } ->
		for(Y0+1, Y0+Rows, (pred(By::in, di, uo) is det -->
			cursor(X0, By),
			putchar('|'),
			cursor(X0+Cols+1, By),
			putchar('|')
		)),
		for(X0+1, X0+Cols, (pred(Bx::in, di, uo) is det -->
			cursor(Bx, Y0),
			(
				{ Titles = [] },
				putchar('-')
			;
				{ Titles = [_|_] },
				putchar('=')
			),
			cursor(Bx, Y0+Rows+1),
			putchar('-')
		)),
		cursor(X0, Y0), putchar('+'),
		cursor(X0+Cols+1, Y0), putchar('+'),
		cursor(X0, Y0+Rows+1), putchar('+'),
		cursor(X0+Cols+1, Y0+Rows+1), putchar('+'),
		( { Titles = [Title0|_] } ->
			{ string.length(Title0, N0) },
			( { N0 > Cols-2 } ->
				{ N = Cols - 2 },
				{ split(Title0, N, Title, _) }
			;
				{ N = N0 },
				{ Title = Title0 }
			),
			{ Xst = X0 + (Cols - N)//2 },
			cursor(Xst, Y0),
			putstr(Title)
		;
			[]
		),
		{ A = 1 }
	;
		{ A = 0 }
	),
	{ Xb = X0+A },
	{ Yb = Y0+A },
	for(0, Rows-1, (pred(Y::in, di, uo) is det -->
		{ Offset = Y*Cols },
		for(0, Cols-1, (pred(X::in, di, uo) is det -->
			cursor(Xb+X, Yb+Y),
			{ lookup(Data, X+Offset, Char - Attribs) },
			putch(Char, Attribs)
		))
	)),
	foldl(refresh_child, Visi).

:- pred refresh_child(child, io, io).
:- mode refresh_child(in, di, uo) is det.

refresh_child(child(X, Y, Win)) -->
	get_cursor(cursor(X0, Y0)),
	set_cursor(cursor(X0+X, Y0+Y)),
	refresh(Win),
	set_cursor(cursor(X0, Y0)).

%----------------------------------------------------------------------------%

:- pred putch(char, list(cattr), io, io).
:- mode putch(in, in, di, uo) is det.

putch(Char, []) --> putchar(Char).
putch(Char, [A|B]) -->
	{ chtype(Char, Chtype) },
	putch2(Chtype, [A|B]).

:- pred putch2(int, list(cattr), io, io).
:- mode putch2(in, in, di, uo) is det.

putch2(Chtype, []) --> putch3(Chtype).
putch2(Chtype0, [Attrib | Attribs], IO0, IO) :-
	(
	    Attrib = normal,
	    mod_chtype(Chtype0, normal, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = standout,
	    mod_chtype(Chtype0, standout, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = underline,
	    mod_chtype(Chtype0, underline, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = reverse,
	    mod_chtype(Chtype0, reverse, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = blink,
	    mod_chtype(Chtype0, blink, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = dim,
	    mod_chtype(Chtype0, dim, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = bold,
	    mod_chtype(Chtype0, bold, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = protect,
	    mod_chtype(Chtype0, protect, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = invis,
	    mod_chtype(Chtype0, invis, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = altcharset,
	    mod_chtype(Chtype0, altcharset, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;
	    Attrib = chartext,
	    mod_chtype(Chtype0, chartext, Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	;	
	    Attrib = colour(Colour0),
	    get_colour(Colour0, Colour),
	    mod_chtype(Chtype0, colour(Colour), Chtype),
	    putch2(Chtype, Attribs, IO0, IO)
	).

:- pred putch3(int, io, io).
:- mode putch3(in, di, uo) is det.
:- pragma foreign_proc("C",
	putch3(C::in, IO0::di, IO::uo),
	[promise_pure, will_not_call_mercury],
"
	addch((chtype) C);
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pred get_colour(colour, int).
:- mode get_colour(in, out) is det.

get_colour(black, black).
get_colour(green, green).
get_colour(red, red).
get_colour(cyan, cyan).
get_colour(white, white).
get_colour(magenta, magenta).
get_colour(blue, blue).
get_colour(yellow, yellow).

:- pred chtype(char, int).
:- mode chtype(in, out) is det.
:- pragma foreign_proc("C",
	chtype(C::in, Ch::out),
	[promise_pure, will_not_call_mercury],
"
	Ch = (chtype) C;
").

:- pred mod_chtype(int, int, int).
:- mode mod_chtype(in, in, out) is det.
:- pragma foreign_proc("C",
	mod_chtype(Ch0::in, Attr::in, Ch::out),
	[promise_pure, will_not_call_mercury],
"
	Ch = (chtype) Ch0 | Attr;
").

%----------------------------------------------------------------------------%

hide(Win) -->
	get_win(Win, Window),
	{ Window = win(Parent, _, _, _, _, _, _) },
	get_win(Parent, PWindow0),
	{ PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden0) },
	{ filter((pred(Child::in) is semidet :-
		Child = child(_, _, Win)
	), Visi0, This, Visi) },
	{ append(This, Hidden0, Hidden) },
	{ PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden) },
	set_win(Parent, PWindow).

%----------------------------------------------------------------------------%

show(Win) -->
	get_win(Win, Window),
	{ Window = win(Parent, _, _, _, _, _, _) },
	get_win(Parent, PWindow0),
	{ PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden0) },
	{ filter((pred(Child::in) is semidet :-
		Child = child(_, _, Win)
	), Hidden0, This, Hidden) },
	{ append(Visi0, This, Visi) },
	{ PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden) },
	set_win(Parent, PWindow).

%----------------------------------------------------------------------------%

raise(Win) -->
	get_win(Win, Window),
	{ Window = win(Parent, _, _, _, _, _, _) },
	get_win(Parent, PWindow0),
	{ PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden) },
	{ filter((pred(Child::in) is semidet :-
		Child = child(_, _, Win)
	), Visi0, This, Rest) },
	{ append(Rest, This, Visi) },
	{ PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden) },
	set_win(Parent, PWindow).

%----------------------------------------------------------------------------%

lower(Win) -->
	get_win(Win, Window),
	{ Window = win(Parent, _, _, _, _, _, _) },
	get_win(Parent, PWindow0),
	{ PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden) },
	{ filter((pred(Child::in) is semidet :-
		Child = child(_, _, Win)
	), Visi0, This, Rest) },
	{ append(This, Rest, Visi) },
	{ PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden) },
	set_win(Parent, PWindow).

%----------------------------------------------------------------------------%

clear(Win) -->
	get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden)),
	{ for(0, Rows-1, (pred(Y::in, array_di, array_uo) is det -->
		for(0, Cols-1, (pred(X::in, D0::array_di, D::array_uo) is det :-
			set(X+Y*Cols, ' ' - [], D0, D)
		))
	), u(Data0), Data) },
	set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden)).

%----------------------------------------------------------------------------%

scroll(Win, N) -->
	get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden)),
	{ require(((pred) is semidet :-
		N > 0,
		N < Cols
	), "scroll: out of range") },
	{ for(0, Rows-N-1, (pred(Y::in, array_di, array_uo) is det -->
		for(0, Cols-1, (pred(X::in, D0::array_di, D::array_uo) is det :-
			lookup(D0, X+(Y+N)*Cols, C),
			set(X+Y*Cols, C, D0, D)
		))
	), u(Data0), Data1) },
	{ for(Rows-N, Rows-1, (pred(Y::in, array_di, array_uo) is det -->
		for(0, Cols-1, (pred(X::in, D1::array_di, Q::array_uo) is det :-
			set(X+Y*Cols, ' ' - [], D1, Q)
		))
	), Data1, Data) },
	set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden)).

%----------------------------------------------------------------------------%

place_char(Win, X, Y, C - As) -->
	get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden)),
	{ require(((pred) is semidet :-
		X >= 0, Y >= 0,
		X < Cols, Y < Cols
	), "place_char: out of range") },
	{ set(X+Y*Cols, C - As, u(Data0), Data) },
	set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden)).

:- func u(array(T)) = array(T).
:- mode (u(in) = array_uo) is det.
:- pragma foreign_proc("C",
	u(A::in) = (B::array_uo),
	[promise_pure, will_not_call_mercury],
"
	B = A;
").

%----------------------------------------------------------------------------%

place_string(Win, X, Y, Str) -->
	get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden)),
	{ require(((pred) is semidet :-
		X >= 0, Y >= 0,
		X < Cols, Y < Cols
	), "place_string: out of range") },
	{ string.to_char_list(Str, Chars) },
	{ update_data(Chars, Y*Cols, X, X+Cols, u(Data0), Data) },
	set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden)).

:- pred update_data(list(char), int, int, int, array(pair(char, list(cattr))),
		    array(pair(char, list(cattr)))).
:- mode update_data(in, in, in, in, array_di, array_uo) is det.

update_data([], _, _, _, Data, Data).
update_data([C|Cs], Y, X, Xmax, Data0, Data) :-
	( X < Xmax ->
		set(X+Y, C - [], Data0, Data1),
		update_data(Cs, Y, X+1, Xmax, Data1, Data)
	;
		Data = Data0
	).

%----------------------------------------------------------------------------%

:- pred get_root(win, io, io).
:- mode get_root(out, di, uo) is det.

:- pred set_root(win, io, io).
:- mode set_root(in, di, uo) is det.

:- pragma foreign_decl("C", "
	extern MR_Word	curse_root;
").

:- pragma foreign_code("C", "
	MR_Word		curse_root;
").

:- pragma foreign_proc("C",
	get_root(W::out, IO0::di, IO::uo),
	[promise_pure, will_not_call_mercury],
"
	W = curse_root;
	IO = IO0;
").

:- pragma foreign_proc("C",
	set_root(W::in, IO0::di, IO::uo),
	[promise_pure, will_not_call_mercury],
"
	curse_root = W;
	IO = IO0;
").

%----------------------------------------------------------------------------%

:- pred new_win(window::in, win::out, io::di, io::uo) is det.

new_win(Window, Win) -->
	get_curse_store(Curse0),
	{ store.new_mutvar(Window, Win, Curse0, Curse) },
	set_curse_store(Curse).

:- pred get_win(win::in, window::out, io::di, io::uo) is det.

get_win(Win, Window) -->
	get_curse_store(Curse0),
	{ store.get_mutvar(Win, Window, Curse0, Curse) },
	set_curse_store(Curse).

:- pred set_win(win::in, window::in, io::di, io::uo) is det.

set_win(Win, Window) -->
	get_curse_store(Curse0),
	{ store.set_mutvar(Win, Window, Curse0, Curse) },
	set_curse_store(Curse).

%----------------------------------------------------------------------------%

:- pred get_cursor(cursor::out, io::di, io::uo) is det.

:- pred set_cursor(cursor::in, io::di, io::uo) is det.

:- pragma foreign_decl("C", "
	extern MR_Word	curse_cursor;
").

:- pragma foreign_code("C", "
	MR_Word		curse_cursor;
").

:- pragma foreign_proc("C",
	get_cursor(C::out, I0::di, I::uo),
	[promise_pure, will_not_call_mercury],
"
	C = curse_cursor;
	I = I0;
").

:- pragma foreign_proc("C",
	set_cursor(C::in, I0::di, I::uo),
	[promise_pure, will_not_call_mercury],
"
	curse_cursor = C;
	I = I0;
").

%----------------------------------------------------------------------------%

% XXX get_curse_store is not unique-mode-correct.
% You need to be careful to ensure that get_curse_store
% and set_curse_store are only ever used in pairs.

:- pred init_curse_store(curse_store::uo) is det.

:- pred get_curse_store(curse_store::uo, io::di, io::uo) is det.

:- pred set_curse_store(curse_store::di, io::di, io::uo) is det.

:- pragma foreign_decl("C", "
	extern MR_Word	curse_store;
").

:- pragma foreign_code("C", "
	MR_Word		curse_store;
").

:- pragma foreign_proc("C",
	init_curse_store(C::uo),
	[promise_pure, will_not_call_mercury],
"
	/*
	** Here we rely on the fact that stores have no
	** real representation, so we can fill in any
	** dummy value for C.
	*/
	C = 0;
").

:- pragma foreign_proc("C",
	get_curse_store(C::uo, I0::di, I::uo),
	[promise_pure, will_not_call_mercury],
"
	C = curse_store;
	I = I0;
").

:- pragma foreign_proc("C",
	set_curse_store(C::di, I0::di, I::uo),
	[promise_pure, will_not_call_mercury],
"
	curse_store = C;
	I = I0;
").

%----------------------------------------------------------------------------%

