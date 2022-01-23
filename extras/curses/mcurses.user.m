%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%----------------------------------------------------------------------------%
% Copyright (C) 1994-2000, 2005-2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2021-2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%
%
% File:          mcurses.user.m
% Main author:   conway
% Maintained by: rejj
% Stability:     Medium
%
% This module provides the user-level functionality for the (n)curses binding.
%
% Please note that this is still a partial binding; it does not provide
% complete curses functionality.
% Major things this binding implements:
%     * Creation, destruction, clearing, raising, and lowering of arbitrary
%       windows.
%     * Scrolling.
%     * Colour on a character by character basis.
%
%----------------------------------------------------------------------------%

:- module mcurses.user.
:- interface.

:- import_module char.
:- import_module io.
:- import_module list.
:- import_module pair.

%----------------------------------------------------------------------------%

    % The ADT used to represent a curses window.
    %
:- type win.

    % Window options.
    %
:- type wopt
    --->    border          % Place a border around the window.
    ;       title(string).  % Give the window a title.

    % Character attributes.
    % These modify the way a character is drawn on the screen. See the curses
    % documentation for a detailed description of each attribute.
    %
:- type cattr
    --->    normal
    ;       standout
    ;       underline
    ;       reverse
    ;       blink
    ;       dim
    ;       bold
    ;       protect
    ;       invis
    ;       altcharset
    ;       chartext
    ;       colour(colour).

    % Colours available for use in displaying characters.
    %
    % XXX Currently, there is no functionality provided for the customisation
    % of colours, only the 8 default colours from curses can be used.
    %
:- type colour
    --->    black
    ;       green
    ;       red
    ;       cyan
    ;       white
    ;       magenta
    ;       blue
    ;       yellow.

    % The type used to represent a character with its attributes.
    %
:- type chtype == pair(char, list(cattr)).

    % init(Root, !IO):
    %
    % Initialise curses, giving back the root window.
    % The initialisation procedures in this library turn off echoing, and
    % enable character-at-a-time input.
    %
:- pred init(win::out, io::di, io::uo) is det.

    % Redraw the screen.
    %
:- pred redraw(io::di, io::uo) is det.

    % Refresh the screen.
    %
:- pred refresh(io::di, io::uo) is det.

    % create(Parent, Options, ParentX, ParentY, NumCols, NumRows, Child, !IO):
    %
    % Create a new window, which will be a child of the window Parent. It is
    % created at position ParentX, ParentY in the parent window, and is of size
    % NumCols, NumRows.
    %
:- pred create(win::in, list(wopt)::in, int::in, int::in, int::in, int::in,
    win::out, io::di, io::uo) is det.

    % Destroy the specified window.
    %
:- pred destroy(win::in, io::di, io::uo) is det.

    % Hide the specified window.
    %
:- pred hide(win::in, io::di, io::uo) is det.

    % Show the (previously hidden) specified window.
    %
:- pred show(win::in, io::di, io::uo) is det.

    % Raise the specified window.
    %
:- pred raise(win::in, io::di, io::uo) is det.

    % Lower the specified window.
    %
:- pred lower(win::in, io::di, io::uo) is det.

    % Clear the specified window. Fills the window with spaces.
    %
:- pred clear(win::in, io::di, io::uo) is det.

    % place_char(Window, X, Y, (Char - Attributes), !IO):
    % Place a character into Window at position X, Y.
    %
:- pred place_char(win::in, int::in, int::in, chtype::in,
    io::di, io::uo) is det.

    % place_string(Window, X, Y, String, !IO):
    %
    % Place a string into Window at position X, Y.
    %
    % XXX Note that presently, character attributes are not supported for
    % strings
    %
:- pred place_string(win::in, int::in, int::in, string::in,
    io::di, io::uo) is det.

    % scroll(Window, Amount, !IO):
    %
    % Scroll Window upwards by Amount lines.
    %
:- pred scroll(win::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
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

%----------------------------------------------------------------------------%

:- type curse_store_type
    --->    curse_store_type.

:- type curse_store == store(curse_store_type).

:- type win == store_mutvar(window, curse_store_type).

:- type window
    --->    win(
            win,        % parent
            int,        % width
            int,        % height
            list(wopt),
            array(chtype),  % contents
            list(child),    % visible
            list(child) % hidden
        ).

:- type child
    --->    child(
            int,        % x
            int,        % y
            win
        ).

:- type cursor
    --->    cursor(int, int). % X, Y

%----------------------------------------------------------------------------%

init(Win, !IO) :-
    init(!IO),
    cols(Cols, !IO),
    rows(Rows, !IO),
    array.init(Cols * Rows, ' ' - [], Data),
    MakeWin = (func(Self) = win(Self, Cols, Rows, [], Data, [], [])),
    init_curse_store(Curse0),
    store.new_cyclic_mutvar(MakeWin, Win, Curse0, Curse),
    set_curse_store(Curse, !IO),
    set_root(Win, !IO),
    refresh(!IO).

%----------------------------------------------------------------------------%

create(Parent, Opts, X, Y, W, H, Child, !IO) :-
    get_win(Parent, PWindow0, !IO),
    PWindow0 = win(P0, W0, H0, Opts0, PData, Visi0, Hidden),
    require(((pred) is semidet :-
        X >= 0, Y >= 0,
        X+W =< W0,
        Y+H =< H0
    ), "create: window out of range!"),
    array.init(W * H, ' ' - [], Data),
    CWindow = win(P0, W, H, Opts, Data, [], []),
    new_win(CWindow, Child, !IO),
    list.append(Visi0, [child(X, Y, Child)], Visi),
    PWindow = win(Parent, W0, H0, Opts0, PData, Visi, Hidden),
    set_win(Parent, PWindow, !IO).

%----------------------------------------------------------------------------%

destroy(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(Parent, _, _, _, _, _, _),
    ( if Parent \= Win then % Cannot kill the root window.
        get_win(Parent, PWindow0, !IO),
        PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden),
        filter((pred(Child::in) is semidet :-
            \+ Child = child(_, _, Win)
        ), Visi0, Visi),
        PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden),
        set_win(Parent, PWindow, !IO)
    else
        true
    ).

%----------------------------------------------------------------------------%

redraw(!IO) :-
    get_root(Root, !IO),
    set_cursor(cursor(0, 0), !IO),
    refresh(Root, !IO),
    doupdate(!IO).

%----------------------------------------------------------------------------%

refresh(!IO) :-
    get_root(Root, !IO),
    clear(!IO),
    set_cursor(cursor(0, 0), !IO),
    refresh(Root, !IO),
    update(!IO).

:- pred refresh(win::in, io::di, io::uo) is det.

refresh(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(_Parent, Cols, Rows, Opts, Data, Visi, _Hidden),
    get_cursor(cursor(X0, Y0), !IO),
    solutions((pred(Ti::out) is nondet :-
        list.member(ZZ, Opts),
        ZZ = title(Ti)
    ), Titles),
    ( if list.member(border, Opts) then
        for(Y0+1, Y0+Rows, (pred(By::in, !.IO::di, !:IO::uo) is det :-
            cursor(X0, By, !IO),
            putchar('|', !IO),
            cursor(X0 + Cols + 1, By, !IO),
            putchar('|', !IO)
        ), !IO),
        for(X0 + 1, X0 + Cols, (pred(Bx::in, !.IO::di, !:IO::uo) is det :-
            cursor(Bx, Y0, !IO),
            (
                Titles = [],
                putchar('-', !IO)
            ;
                Titles = [_ | _],
                putchar('=', !IO)
            ),
            cursor(Bx, Y0 + Rows + 1, !IO),
            putchar('-', !IO)
        ), !IO),
        cursor(X0, Y0, !IO), putchar('+', !IO),
        cursor(X0 + Cols + 1, Y0, !IO), putchar('+', !IO),
        cursor(X0, Y0 + Rows + 1, !IO), putchar('+', !IO),
        cursor(X0 + Cols + 1, Y0 + Rows + 1, !IO), putchar('+', !IO),
        ( if Titles = [Title0 | _] then
            string.length(Title0, N0),
            ( if N0 > Cols - 2  then
                N = Cols - 2,
                split(Title0, N, Title, _)
            else
                N = N0,
                Title = Title0
            ),
            Xst = X0 + (Cols - N) // 2,
            cursor(Xst, Y0, !IO),
            putstr(Title, !IO)
        else
            true
        ),
        A = 1
    else
        A = 0
    ),
    Xb = X0 + A,
    Yb = Y0 + A,
    for(0, Rows - 1, (pred(Y::in, !.IO::di, !:IO::uo) is det :-
        Offset = Y*Cols,
        for(0, Cols - 1, (pred(X::in, !.IO::di, !:IO::uo) is det :-
            cursor(Xb + X, Yb + Y, !IO),
            lookup(Data, X + Offset, Char - Attribs),
            putch(Char, Attribs, !IO)
        ), !IO)
    ), !IO),
    foldl(refresh_child, Visi, !IO).

:- pred refresh_child(child::in, io::di, io::uo) is det.

refresh_child(child(X, Y, Win), !IO):-
    get_cursor(cursor(X0, Y0), !IO),
    set_cursor(cursor(X0 + X, Y0 + Y), !IO),
    refresh(Win, !IO),
    set_cursor(cursor(X0, Y0), !IO).

%----------------------------------------------------------------------------%

:- pred putch(char::in, list(cattr)::in, io::di, io::uo) is det.

putch(Char, [], !IO) :-
    putchar(Char, !IO).
putch(Char, [A | B], !IO) :-
    chtype(Char, Chtype),
    putch2(Chtype, [A | B], !IO).

:- pred putch2(int::in, list(cattr)::in, io::di, io::uo) is det.

putch2(Chtype, [], !IO) :-
    putch3(Chtype, !IO).
putch2(Chtype0, [Attrib | Attribs], !IO) :-
    (
        Attrib = normal,
        mod_chtype(Chtype0, normal, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = standout,
        mod_chtype(Chtype0, standout, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = underline,
        mod_chtype(Chtype0, underline, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = reverse,
        mod_chtype(Chtype0, reverse, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = blink,
        mod_chtype(Chtype0, blink, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = dim,
        mod_chtype(Chtype0, dim, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = bold,
        mod_chtype(Chtype0, bold, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = protect,
        mod_chtype(Chtype0, protect, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = invis,
        mod_chtype(Chtype0, invis, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = altcharset,
        mod_chtype(Chtype0, altcharset, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = chartext,
        mod_chtype(Chtype0, chartext, Chtype),
        putch2(Chtype, Attribs, !IO)
    ;
        Attrib = colour(Colour0),
        get_colour(Colour0, Colour),
        mod_chtype(Chtype0, colour(Colour), Chtype),
        putch2(Chtype, Attribs, !IO)
    ).

:- pred putch3(int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    putch3(C::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    addch((chtype) C);
").

%----------------------------------------------------------------------------%

:- pred get_colour(colour::in, int::out) is det.

get_colour(black, black).
get_colour(green, green).
get_colour(red, red).
get_colour(cyan, cyan).
get_colour(white, white).
get_colour(magenta, magenta).
get_colour(blue, blue).
get_colour(yellow, yellow).

:- pred chtype(char::in, int::out) is det.
:- pragma foreign_proc("C",
    chtype(C::in, Ch::out),
    [promise_pure, will_not_call_mercury],
"
    Ch = (chtype) C;
").

:- pred mod_chtype(int::in, int::in, int::out) is det.
:- pragma foreign_proc("C",
    mod_chtype(Ch0::in, Attr::in, Ch::out),
    [promise_pure, will_not_call_mercury],
"
    Ch = (chtype) Ch0 | Attr;
").

%----------------------------------------------------------------------------%

hide(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(Parent, _, _, _, _, _, _),
    get_win(Parent, PWindow0, !IO),
    PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden0),
    filter((pred(Child::in) is semidet :-
        Child = child(_, _, Win)
    ), Visi0, This, Visi),
    append(This, Hidden0, Hidden),
    PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden),
    set_win(Parent, PWindow, !IO).

%----------------------------------------------------------------------------%

show(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(Parent, _, _, _, _, _, _),
    get_win(Parent, PWindow0, !IO),
    PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden0),
    filter((pred(Child::in) is semidet :-
        Child = child(_, _, Win)
    ), Hidden0, This, Hidden),
    append(Visi0, This, Visi),
    PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden),
    set_win(Parent, PWindow, !IO).

%----------------------------------------------------------------------------%

raise(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(Parent, _, _, _, _, _, _),
    get_win(Parent, PWindow0, !IO),
    PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden),
    filter((pred(Child::in) is semidet :-
        Child = child(_, _, Win)
    ), Visi0, This, Rest),
    append(Rest, This, Visi),
    PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden),
    set_win(Parent, PWindow, !IO).

%----------------------------------------------------------------------------%

lower(Win, !IO) :-
    get_win(Win, Window, !IO),
    Window = win(Parent, _, _, _, _, _, _),
    get_win(Parent, PWindow0, !IO),
    PWindow0 = win(PP, PC, PR, PO, PD, Visi0, Hidden),
    filter((pred(Child::in) is semidet :-
        Child = child(_, _, Win)
    ), Visi0, This, Rest),
    append(This, Rest, Visi),
    PWindow = win(PP, PC, PR, PO, PD, Visi, Hidden),
    set_win(Parent, PWindow, !IO).

%----------------------------------------------------------------------------%

clear(Win, !IO) :-
    get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden), !IO),
    for(0, Rows - 1, (pred(Y::in, array_di, array_uo) is det -->
        for(0, Cols - 1, (pred(X::in, D0::array_di, D::array_uo) is det :-
            set(X + Y * Cols, ' ' - [], D0, D)
        ))
    ), u(Data0), Data),
    set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden), !IO).

%----------------------------------------------------------------------------%

scroll(Win, N, !IO) :-
    get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden), !IO),
    require(((pred) is semidet :-
        N > 0,
        N < Cols
    ), "scroll: out of range"),
    for(0, Rows - N - 1, (pred(Y::in, array_di, array_uo) is det -->
        for(0, Cols - 1, (pred(X::in, D0::array_di, D::array_uo) is det :-
            lookup(D0, X + (Y + N) * Cols, C),
            set(X + Y * Cols, C, D0, D)
        ))
    ), u(Data0), Data1),
    for(Rows - N, Rows - 1, (pred(Y::in, array_di, array_uo) is det -->
        for(0, Cols - 1, (pred(X::in, D1::array_di, Q::array_uo) is det :-
            set(X + Y * Cols, ' ' - [], D1, Q)
        ))
    ), Data1, Data),
    set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden), !IO).

%----------------------------------------------------------------------------%

place_char(Win, X, Y, C - As, !IO) :-
    get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden), !IO),
    require(((pred) is semidet :-
        X >= 0, Y >= 0,
        X < Cols, Y < Cols
    ), "place_char: out of range"),
    set(X + Y * Cols, C - As, u(Data0), Data),
    set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden), !IO).

:- func u(array(T)) = array(T).
:- mode (u(in) = array_uo) is det.
:- pragma foreign_proc("C",
    u(A::in) = (B::array_uo),
    [promise_pure, will_not_call_mercury],
"
    B = A;
").

%----------------------------------------------------------------------------%

place_string(Win, X, Y, Str, !IO) :-
    get_win(Win, win(Parent, Cols, Rows, Opts, Data0, Visi, Hidden), !IO),
    require(((pred) is semidet :-
        X >= 0, Y >= 0,
        X < Cols, Y < Cols
    ), "place_string: out of range"),
    string.to_char_list(Str, Chars),
    update_data(Chars, Y * Cols, X, X + Cols, u(Data0), Data),
    set_win(Win, win(Parent, Cols, Rows, Opts, Data, Visi, Hidden), !IO).

:- pred update_data(list(char)::in, int::in, int::in, int::in,
    array(pair(char, list(cattr)))::array_di,
    array(pair(char, list(cattr)))::array_uo) is det.

update_data([], _, _, _, !Data).
update_data([C | Cs], Y, X, Xmax, !Data) :-
    ( if X < Xmax then
        array.set(X + Y, C - [], !Data),
        update_data(Cs, Y, X + 1, Xmax, !Data)
    else
        true
    ).

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    extern MR_Word  curse_root;
").

:- pragma foreign_code("C", "
    MR_Word     curse_root;
").

:- pred get_root(win::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_root(W::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    W = curse_root;
").

:- pred set_root(win::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_root(W::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    curse_root = W;
").

%----------------------------------------------------------------------------%

:- pred new_win(window::in, win::out, io::di, io::uo) is det.

new_win(Window, Win, !IO) :-
    get_curse_store(Curse0, !IO),
    store.new_mutvar(Window, Win, Curse0, Curse),
    set_curse_store(Curse, !IO).

:- pred get_win(win::in, window::out, io::di, io::uo) is det.

get_win(Win, Window, !IO) :-
    get_curse_store(Curse0, !IO),
    store.get_mutvar(Win, Window, Curse0, Curse),
    set_curse_store(Curse, !IO).

:- pred set_win(win::in, window::in, io::di, io::uo) is det.

set_win(Win, Window, !IO) :-
    get_curse_store(Curse0, !IO),
    store.set_mutvar(Win, Window, Curse0, Curse),
    set_curse_store(Curse, !IO).

%----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    extern MR_Word  curse_cursor;
").

:- pragma foreign_code("C", "
    MR_Word     curse_cursor;
").

:- pred get_cursor(cursor::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_cursor(C::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    C = curse_cursor;
").

:- pred set_cursor(cursor::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_cursor(C::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    curse_cursor = C;
").

%----------------------------------------------------------------------------%

% XXX get_curse_store is not unique-mode-correct.
% You need to be careful to ensure that get_curse_store
% and set_curse_store are only ever used in pairs.

:- pragma foreign_decl("C", "
    extern MR_Word  curse_store;
").

:- pragma foreign_code("C", "
    MR_Word     curse_store;
").

:- pred init_curse_store(curse_store::uo) is det.

:- pragma foreign_proc("C",
    init_curse_store(C::uo),
    [promise_pure, will_not_call_mercury],
"
    // Here we rely on the fact that stores have no real representation, so we
    // can fill in any dummy value for C.
    C = 0;
").

:- pred get_curse_store(curse_store::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_curse_store(C::uo, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    C = curse_store;
").

:- pred set_curse_store(curse_store::di, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_curse_store(C::di, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    curse_store = C;
").

%----------------------------------------------------------------------------%
:- end_module mcurses.user.
%----------------------------------------------------------------------------%
