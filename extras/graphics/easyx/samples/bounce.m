% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004, 2006 The University of Melbourne.
% Copyright (C) 2014, 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
% bounce.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Sep 17 16:19:38 EST 2004
%
% A simple game written using the easyx module.  Balls enter the playing
% area from the top left and must be kept in play by using the mouse-
% controlled bat to prevent them from falling off the screen.  Every time
% a ball is missed, the player's bat shrinks a little.  The game is over
% when there are no more balls in play.
%
% Hit `q' to quit the game at any time.
%
% To compile this game, ensure the easyx.m and xlib.m modules are in
% the same directory and then do
%
% mmc --make bounce -L /usr/X11/lib -l X11
%
% (On some systems the -L argument may need changing, e.g. to /usr/X11R6/lib.)
%
%-----------------------------------------------------------------------------%

:- module bounce.

:- interface.

:- import_module io.


:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module easyx.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module string.
:- import_module time.


:- pragma foreign_decl("C", "#include <unistd.h>").


:- type object
    --->    ball(
                ball_x          :: float,
                ball_y          :: float,
                ball_vx         :: float,
                ball_vy         :: float,
                ball_radius     :: float,
                ball_colour     :: colour
            )
    ;       oops(
                oops_x          :: float,
                oops_y          :: float,
                oops_colour     :: colour
            ).

:- type objects == list(object).

:- type paddle
    --->    paddle(
                paddle_x        :: float,
                paddle_y        :: float,
                paddle_width    :: float,
                paddle_colour   :: colour,
                paddle_score    :: int
            ).

:- type colours == list(colour).

:- type rnd == random.supply.

:- func gravity = float.
gravity = 0.0001.

:- func odds_of_new_object = int.
odds_of_new_object = 200.

:- func initial_paddle_x = float.
initial_paddle_x = 0.5.

:- func initial_paddle_y = float.
initial_paddle_y = 0.1.

:- func initial_paddle_width = float.
initial_paddle_width = 0.2.

:- func oops_y_velocity = float.
oops_y_velocity = 0.02.

%-----------------------------------------------------------------------------%

:- pred move_objects(paddle::in, paddle::out, objects::in, objects::out)
            is det.

move_objects(!Paddle, [],              []   ).

move_objects(!Paddle, [Object | Objects0], Objects) :-
    move_objects(!Paddle, Objects0, Objects1),
    move_object(Object, !Paddle, Objects1, Objects).


:- pred move_object(object::in, paddle::in, paddle::out,
            objects::in, objects::out) is det.

move_object(ball(X0, Y0, VX0, VY0, Radius, Colour), !Paddle, !Objects) :-

    !.Paddle = paddle(PX, PY, PWidth0, PColour, PScore0),

    X = X0 + VX0,
    ( if VX0 < 0.0, X - Radius < 0.0 then
        VX = -VX0
      else if 0.0 < VX0, 1.0 < X + Radius then
        VX = -VX0
      else
        VX = VX0
    ),

    Y = Y0 + VY0,
    ( if PX < X + Radius, X - Radius < PX + PWidth0,
         Y0 - Radius > PY, PY >= Y - Radius
      then
        VY     = -VY0 - gravity,
        PScore = PScore0 + 1
      else
        VY     = VY0 - gravity,
        PScore = PScore0
    ),

    ( if Y + Radius > 0.0 then
        PWidth = PWidth0,
        Object = ball(X, Y, VX, VY, Radius, Colour)
      else
        PWidth = 0.9 * PWidth0,
        Object = oops(X, Y, Colour)
    ),
    !:Objects = [Object | !.Objects],
    !:Paddle  = paddle(PX, PY, PWidth, PColour, PScore).

move_object(oops(X, Y, Colour), !Paddle, !Objects) :-
    ( if Y > 1.1 then
        true
      else
        !:Objects = [oops(X, Y + oops_y_velocity, Colour) | !.Objects]
    ).


:- func move_paddle(paddle, float) = paddle.

move_paddle(Paddle, X) = (Paddle^paddle_x := X - (Paddle^paddle_width / 2.0)).


:- func paddle_drawing(paddle) = drawing.

paddle_drawing(paddle(X, Y, Width, Colour, Score)) =
    [
        line_attributes(pixels(10), round, round),
        colour(Colour),
        line(X, 1.0 - Y, X + Width, 1.0 - Y),
        text(1.0, 1.0, 1.0, 1.0, format("%d bounces", [i(Score)]))
    ].


:- func objects_drawing(colour, objects) = drawing.

objects_drawing(BorderColour, Objects) =
    [   line_attributes(pixels(4), round, round)
    |   foldl(add_object_drawing(BorderColour), Objects, [])
    ].


:- func add_object_drawing(colour, object, drawing) = drawing.

add_object_drawing(BorderColour, ball(X, Y, _, _, Radius, Colour), Drawing) =
    [
        colour(BorderColour),
        circle(X, 1.0 - Y, Radius),
        colour(Colour),
        filled_circle(X, 1.0 - Y, Radius)
    |   Drawing
    ].

add_object_drawing(_, oops(X, Y, Colour), Drawing) =
    [
        colour(Colour),
        text(X, 1.0 - Y, "oops")
    |   Drawing
    ].


:- func new_paddle(colour) = paddle.

new_paddle(Colour) =
    paddle(initial_paddle_x, initial_paddle_y, initial_paddle_width, Colour,
        0).


:- pred add_new_ball(colours::in, objects::in, objects::out,
            rnd::in, rnd::out) is det.

add_new_ball(Colours, Objects, [Ball | Objects], !Rnd) :-
    random.random(Y0,  !Rnd),
    random.random(VX0, !Rnd),
    random.random(R0,  !Rnd),
    random.random(N0,  !Rnd),
    X    = 0.0,
    Y    = 0.75 + float(Y0 mod 25) / 100.0,
    VX   = 0.001 + float(VX0 mod 100) / 10000.0,
    VY   = 0.0,
    R    = 0.025 + float(R0 mod 25) / 1000.0,
    C    = det_index0(Colours, 2 + (N0 / 1000) mod (length(Colours) - 2)),
    Ball = ball(X, Y, VX, VY, R, C).

%-----------------------------------------------------------------------------%

main(!IO) :-
    easyx.open_display(Display, !IO),
    easyx.create_window(Display, "bounce", 600, 600, Window, !IO),
    easyx.load_font(Window, "*-helvetica-*-r-*-34-*", Font, !IO),
    easyx.set_font(Window, Font, !IO),
    easyx.set_line_attributes(Window, pixels(0), round, mitre, !IO),
    easyx.get_colour_from_name(Window, "black",  Black,  !IO),
    easyx.get_colour_from_name(Window, "white",  White,  !IO),
    easyx.get_colour_from_name(Window, "red",    Red,    !IO),
    easyx.get_colour_from_name(Window, "green",  Green,  !IO),
    easyx.get_colour_from_name(Window, "blue",   Blue,   !IO),
    easyx.get_colour_from_name(Window, "yellow", Yellow, !IO),
    Colours = [Black, White, Red, Green, Blue, Yellow],
    time.time(Time, !IO),
    localtime(Time, TM, !IO),
    random.init(60 * TM^tm_min + TM^tm_sec, Rnd0),
    add_new_ball(Colours, [], Objects, Rnd0, Rnd),
    Paddle = new_paddle(White),
    play(Window, Colours, Paddle, _, Objects, _, Rnd, _, !IO).


:- pred play(window::in, colours::in, paddle::in, paddle::out,
    objects::in, objects::out, rnd::in, rnd::out, io::di, io::uo) is det.

play(Window, Colours, !Paddle, !Objects, !Rnd, !IO) :-

    Black = det_index0(Colours, 0),
    White = det_index0(Colours, 1),

    easyx.set_colour(Window, Black, !IO),
    easyx.clear_window(Window, !IO),

    ( if !.Objects \= [] then
        maybe_add_object(Colours, !Objects, !Rnd)
      else
        easyx.set_colour(Window, White, !IO),
        easyx.draw_text(Window, 0.5, 0.5, 0.5, 0.5, "Game Over", !IO)
    ),
    move_objects(!Paddle, !Objects),

    Drawing = paddle_drawing(!.Paddle) ++ objects_drawing(Black, !.Objects),

    easyx.draw(Window, Drawing, !IO),
    easyx.sync(Window, !IO),

    u_sleep(10000, !IO),

    process_events(Window, Quit, !Paddle, !IO),

    ( if   Quit = yes
      then true
      else play(Window, Colours, !Paddle, !Objects, !Rnd, !IO)
    ).


:- pred process_events(window::in, bool::out, paddle::in, paddle::out,
            io::di, io::uo) is det.

process_events(Window, Quit, !Paddle, !IO) :-
    easyx.get_next_event_if_any(Window, MaybeEvent, !IO),
    ( if MaybeEvent = yes(key_press(_, _, _, "q")) then
        Quit = yes
      else if MaybeEvent = yes(pointer_motion(X, _, _)) then
        !:Paddle = move_paddle(!.Paddle, X),
        process_events(Window, Quit, !Paddle, !IO)
      else if MaybeEvent = yes(_) then
        process_events(Window, Quit, !Paddle, !IO)
      else
        Quit = no
    ).


:- pred maybe_add_object(colours::in, objects::in, objects::out,
            rnd::in, rnd::out) is det.

maybe_add_object(Colours, !Objects, !Rnd) :-
    random.random(N, !Rnd),
    ( if   N mod odds_of_new_object = 0
      then add_new_ball(Colours, !Objects, !Rnd)
      else true
    ).


:- pred u_sleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", u_sleep(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure],
    "
        usleep(N);
        IO = IO0;
    ").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
