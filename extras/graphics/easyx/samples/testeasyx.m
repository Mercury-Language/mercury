%-----------------------------------------------------------------------------%
% testeasyx.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Mon Jun 28 15:21:14 EST 2004
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Example program using the easyx module.  The Union Jack is drawn with a
% rotated white square placed in the middle, along with some text.  Cursor
% keys move the image; `-' and `=' rotate the image; `q' quits the program.
%
% Compile this program with
%
% mmc --make testeasyx -L /usr/X11/lib -l X11
%
% ensuring that easyx.m and xlib.m are in the same directory.  You may also
% need to change /usr/X11/lib to something like /usr/X11R6/lib on some
% systems.
%
%-----------------------------------------------------------------------------%

:- module testeasyx.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module easyx.
:- import_module float.
:- import_module list.
:- import_module math.

%-----------------------------------------------------------------------------%

main(!IO) :-

    easyx.open_display(Display, !IO),
    easyx.create_window(Display, "testeasyx", 300, 300, Window, !IO),
    easyx.load_font(Window, "*-helvetica-*-r-*-20-*", Font, !IO),
    easyx.set_font(Window, Font, !IO),

    draw_window(Window, 0.0, 0.0, 0.0, !IO).


:- pred draw_window(window::in, float::in, float::in, float::in,
            io::di, io::uo) is det.

draw_window(Window, X, Y, R, !IO) :-

    easyx.get_colour_from_name(Window, "black", Black, !IO),
    easyx.get_colour_from_name(Window, "white", White, !IO),
    easyx.get_colour_from_name(Window, "red",   Red,   !IO),
    easyx.get_colour_from_name(Window, "gray",  Grey, !IO),
    easyx.get_colour_from_name(Window, "blue",  Blue,  !IO),

    easyx.draw(Window, [

        colour(Blue),
        filled_rectangle(0.0, 0.0, 1.0, 1.0),

        translate(X + 0.5, Y + 0.5, [rotate(R, [translate(-0.5, -0.5, [

            line_attributes(ratio(0.2), butt, mitre),
            colour(White),
            line(0.0, 0.0, 1.0, 1.0),
            line(0.0, 1.0, 1.0, 0.0),

            line_attributes(ratio(0.1), butt, mitre),
            colour(Red),
            line(0.0, 0.0, 1.0, 1.0),
            line(0.0, 1.0, 1.0, 0.0),

            line_attributes(ratio(0.2), butt, mitre),
            colour(White),
            line(0.5, 0.0, 0.5, 1.0),
            line(0.0, 0.5, 1.0, 0.5),

            line_attributes(ratio(0.1), butt, mitre),
            colour(Red),
            line(0.5, 0.0, 0.5, 1.0),
            line(0.0, 0.5, 1.0, 0.5),

            translate(0.5, 0.5, [rotate(0.2, [

                colour(Black), rectangle(-0.3, -0.3, 0.3, 0.3),
                colour(White), filled_rectangle(-0.3, -0.3, 0.3, 0.3)

            ])]),

            colour(Red),
            text(0.5, 0.5, 0.5, 0.5, "EasyX!")

        ])])]),

        colour(Grey),
        text(0.0, 1.0, 0.0, 1.0, "Q to exit")

    ], !IO),

    easyx.flush(Window, !IO),

    easyx.get_next_event(Window, Event, !IO),
    io.print(Event, !IO),
    io.nl(!IO),
    ( if      Event  = key_press(_, _, _, "Left")
      then    draw_window(Window, X - 0.1, Y, R, !IO)
      else if Event  = key_press(_, _, _, "Right")
      then    draw_window(Window, X + 0.1, Y, R, !IO)
      else if Event  = key_press(_, _, _, "Up")
      then    draw_window(Window, X, Y - 0.1, R, !IO)
      else if Event  = key_press(_, _, _, "Down")
      then    draw_window(Window, X, Y + 0.1, R, !IO)
      else if Event  = key_press(_, _, _, "equal")
      then    draw_window(Window, X, Y, R - 0.1, !IO)
      else if Event  = key_press(_, _, _, "minus")
      then    draw_window(Window, X, Y, R + 0.1, !IO)
      else if not (Event = key_press(_, _, _, Q), ( Q = "q" ; Q = "Q" ))
      then    draw_window(Window, X, Y, R, !IO)
      else    true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
