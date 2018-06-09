% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% mclock.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Sep 17 16:19:38 EST 2004
%
% A clock.
%
%-----------------------------------------------------------------------------%

:- module mclock.

:- interface.

:- import_module io.


:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module easyx.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module string.
:- import_module time.


main(!IO) :-
    easyx.open_display(Display, !IO),
    easyx.create_window(Display, "mclock", 400, 400, Window, !IO),
    easyx.load_font(Window, "*-helvetica-*-r-*-20-*", Font, !IO),
    easyx.set_font(Window, Font, !IO),
    easyx.get_colour_from_name(Window, "navyblue", Blue, !IO),
    easyx.get_colour_from_name(Window, "white", White, !IO),
    draw_clock(Window, Blue, White, !IO).


:- pred draw_clock(window::in, colour::in, colour::in, io::di, io::uo) is det.

draw_clock(Window, Blue, White, !IO) :-
    
    time.time(Time, !IO),
    localtime(Time, TM, !IO),
    Hour = TM^tm_hour,
    Min  = TM^tm_min,

    Rot =
        ( func(A, B, D) = rotate((pi + pi) * float(A) / float(B), D) ),

    Digit =
        ( func(N) = Rot(N, 12, [text(0.0, -0.4, 0.5, 0.5, int_to_string(N))]) ),

    Hand =
        ( func(A, B, L) = Rot(A, B, [line(0.0, 0.0, 0.0, -L)]) ),

    Clock =
        [ translate(0.5, 0.5,
          [
            colour(Blue),
            line_attributes(ratio(0.1), round, round),
            circle(0.0, 0.0, 0.4),

            colour(White),
            line_attributes(ratio(0.02), round, round),
            Hand(Hour * 60 + Min, 720, 0.2),
            Hand(            Min,  60, 0.3)

          | map(Digit, 1`..`12)
          ])
        ],

    easyx.clear_window(Window, !IO),
    easyx.draw(Window, Clock, !IO),
    easyx.flush(Window, !IO),

        % This really isn't that good: resize and expose events are only
        % handled once every minute!
        %
    sleep(60, !IO),

    draw_clock(Window, Blue, White, !IO).


:- pred sleep(int::in, io::di, io::uo) is det.
:- pragma foreign_decl("C", "#include <unistd.h>").
:- pragma foreign_proc("C", sleep(N::in, IO0::di, IO::uo),
    [thread_safe, will_not_call_mercury, promise_pure],
    "sleep(N); IO = IO0;"
).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
