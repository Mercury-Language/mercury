% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% moveball.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Sep 17 16:19:38 EST 2004
%
% An orange ball follows the mouse pointer.
%
%-----------------------------------------------------------------------------%

:- module moveball.

:- interface.

:- import_module io.


:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module easyx.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module std_util.


main(!IO) :-
    easyx.open_display(Display, !IO),
    easyx.create_window(Display, "moveball", 400, 400, Window, !IO),
    easyx.set_line_attributes(Window, pixels(0), round, mitre, !IO),
    move_ball(Window, 0.5, 0.5, !IO).


:- pred move_ball(window::in, float::in, float::in, io::di, io::uo) is det.

move_ball(Window, X0, Y0, !IO) :-

    easyx.draw(Window, [
        colour_from_name("black"),  filled_rectangle(0.0, 0.0, 1.0, 1.0),
        colour_from_name("orange"), filled_circle(X0, Y0, 0.1)
    ], !IO),
    easyx.flush(Window, !IO),

    easyx.get_next_event(Window, Event, !IO),

    ( if      Event = pointer_motion(X, Y, _)
      then    move_ball(Window,  X,  Y, !IO)

      else if Event \= key_press(_, _, _, "q")
      then    move_ball(Window, X0, Y0, !IO)

      else    true
    ).

