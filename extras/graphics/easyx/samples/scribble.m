% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004, 2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% scribble.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Sep 17 16:19:38 EST 2004
%
% A trivial drawing program using the easyx module.  Drawing is with
% the mouse (while holding down a button).  Recognised keys:
% c     clear the window to black;
% C     clear the window to white;
% w     draw in white;
% r     draw in red;
% g     draw in green;
% b     draw in blue;
% y     draw in yellow;
% =     increase the brush size;
% -     decrease the brush size;
% q     quit the program.
%
% Compile this program with
%
% mmc --make scribble -L /usr/X11/lib -l X11
%
% ensuring that easyx.m and xlib.m are in the same directory.  You may also
% need to change /usr/X11/lib to something like /usr/X11R6/lib on some
% systems.
%
%-----------------------------------------------------------------------------%

:- module scribble.

:- interface.

:- import_module io.


:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module easyx.
:- import_module float.
:- import_module list.
:- import_module maybe.

:- type maybexy == maybe({float, float}).


main(!IO) :-
    easyx.open_display(Display, !IO),
    easyx.create_window(Display, "scribble", 300, 300, Window, !IO),
    easyx.set_line_attributes(Window, ratio(0.01), round, round, !IO),
    scribble(Window, 0.01, no, !IO).


:- pred scribble(window::in, float::in, maybe({float, float})::in,
            io::di, io::uo) is det.

scribble(Window, Size0, MaybeXY0, !IO) :-
    easyx.get_next_event(Window, Event, !IO),
    ( if sm(Event, Size0, MaybeXY0, Size, MaybeXY, Drawing) then
        easyx.draw(Window, Drawing, !IO),
        easyx.flush(Window, !IO),
        scribble(Window, Size, MaybeXY, !IO)
      else if Event \= key_press(_, _, _, "q") then
        scribble(Window, Size0, MaybeXY0, !IO)
      else
        true
    ).


    % The scribble state machine...
    %
:- pred sm(event::in, float::in, maybexy::in, float::out, maybexy::out,
                drawing::out) is semidet.

sm( key_press(_, _, _, "c"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("black"), filled_rectangle(0.0, 0.0, 1.0, 1.0)]
).
sm( key_press(_, _, _, "C"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("white"), filled_rectangle(0.0, 0.0, 1.0, 1.0)]
).
sm( key_press(_, _, _, "w"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("white")]
).
sm( key_press(_, _, _, "r"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("red")]
).
sm( key_press(_, _, _, "g"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("green")]
).
sm( key_press(_, _, _, "b"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("blue")]
).
sm( key_press(_, _, _, "y"),        Size, MaybeXY, Size, MaybeXY,
    [colour_from_name("yellow")]
).
sm( key_press(_, _, _, "equal"),    Size, MaybeXY, Size + 0.01, MaybeXY,
    [line_attributes(ratio(Size + 0.01), round, round)]
).
sm( key_press(_, _, _, "minus"),    Size, MaybeXY, Size - 0.01, MaybeXY,
    [line_attributes(ratio(Size - 0.01), round, round)]
).
sm( button_release(_, _, _, _),     Size, _MaybeXY, Size, no,
    []
).
sm( button_press(X, Y, _, _),       Size, no, Size, yes({X, Y}),
    [filled_circle(X, Y, Size)]
).
sm( pointer_motion(X, Y, _),        Size, yes({X0, Y0}), Size, yes({X, Y}),
    [line(X0, Y0, X, Y)]
).

