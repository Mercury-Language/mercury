%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2004, 2006, 2011 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
% easyx.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Fri Jun 25 17:49:48 EST 2004
%
% A simple, easy-to-use wrapper around some of Xlib, good for putting
% lines and boxes on the screen and writing simple interactive graphical
% applications.  This library aims for ease of use over efficiency.
%
% All drawing is done to a backing pixmap and the user must explicitly call
% easyx.flush/3 or easyx.sync/3 to make visible any changes since the last
% call to easyx.flush/3 or easyx.sync/3.  Repainting after exposure events is
% handled automatically, although resizing the window does require the user
% to redraw.
%
% An abstract coordinate space is used, but in keeping with Xlib, the
% origin of the coordinate space is at the top-left of a window, with
% coordinates increasing down and to the right.  An abstract coordinate
% value of 1.0 corresponds to the shorter of the current width or height
% of the window.
%
% Angles, where used, are measured in radians.
%
% If a deterministic predicate fails (e.g. when opening the display)
% then an exception is thrown rather than returning an error code.
%
% NOTE!  This library is currently fairly unstable: users should not be
% surprised if the interface changes.
%
%-----------------------------------------------------------------------------%

:- module easyx.
:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % One must open a display in order to create a window.
    %
:- type display.

    % open_display(Display, !IO)
    % Open a connection to the default display.
    %
:- pred open_display(display::out, io::di, io::uo) is det.

    % open_display(DisplayName, Display, !IO)
    % Open a connection to the named display.
    %
:- pred open_display(string::in, display::out, io::di, io::uo) is det.


    % A window is a target for drawing operations and a source of
    % input events (mouse and keyboard etc.)
    %
:- type window.

    % create_window(Display, Title, Width, Height, Window, !IO)
    % Create Window on Display, with the given Title, Width and Height.
    %
:- pred create_window(display::in, string::in, int::in, int::in,
            window::out, io::di, io::uo) is det.

    % flush(Window, !IO)
    % Make all rendering to Window since the last call to flush/3 visible
    % (all rendering is done to a separate drawing area; this predicate
    % updates what is on the screen.)
    %
:- pred flush(window::in, io::di, io::uo) is det.

    % sync(Window, !IO)
    % Like flush/3, but this predicate blocks until the X server has
    % finished rendering.  Also, any pending events in the input queue
    % are discarded.  This is a better choice than flush/3 for low-
    % latency applications such as real-time games.
    %
:- pred sync(window::in, io::di, io::uo) is det.

    % clear_window(Window, !IO)
    % Clear the window using the current colour.
    %
:- pred clear_window(window::in, io::di, io::uo) is det.


:- type font.

    % load_font(Window, FontName, Font, !IO)
    % Load a font from the X server.
    %
:- pred load_font(window::in, string::in, font::out, io::di, io::uo) is det.

    % Set the font for drawing text.
    %
:- pred set_font(window::in, font::in, io::di, io::uo) is det.


:- type colour.

    % get_colour_from_name(Window, ColourName, Colour, !IO)
    % Obtain the colour with the given name.
    %
:- pred get_colour_from_name(window::in, string::in,
            colour::out, io::di, io::uo) is det.

    % get_colour_from_rgb(Window, R, G, B, Colour, !IO)
    % Obtain the colour with the given RGB components (in the range
    % 0.0 to 1.0), or its nearest approximation.
    %
:- pred get_colour_from_rgb(window::in, float::in, float::in, float::in,
            colour::out, io::di, io::uo) is det.

    % set_colour(Window, Colour, !IO)
    % Set the drawing colour.
    %
:- pred set_colour(window::in, colour::in, io::di, io::uo) is det.

    % set_colour_from_name(Window, ColourName, !IO)
    % A useful shortcut.
    %
:- pred set_colour_from_name(window::in, string::in, io::di, io::uo) is det.

    % set_colour_from_rgb(Window, R, G, B, !IO)
    % A useful shortcut.
    %
:- pred set_colour_from_rgb(window::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % set_text_bg_colour(Window, Colour, !IO)
    % Sets the colour draw_image_text will use for the text background.
    %
:- pred set_text_bg_colour(window::in, colour::in, io::di, io::uo) is det.


    % Line widths can be given in pixels or abstract units.
    %
:- type line_width
    --->    pixels(int)
    ;       ratio(float).

    % Line ends may be capped in several ways.
    %
:- type cap_style
    --->    not_last
    ;       butt
    ;       round
    ;       projecting.

    % Joins between lines may be rendered in several ways.
    %
:- type join_style
    --->    mitre
    ;       round
    ;       bevel.

    % Line sequences and filled polygons are defined as sequences of
    % {X, Y} coordinates.
    %
:- type coords == list({float, float}).

    % set_line_attributes(Window, LineWidth, CapStyle, JoinStyle, !IO)
    % Set the line drawing attributes.
    %
:- pred set_line_attributes(window::in, line_width::in, cap_style::in,
    join_style::in, io::di, io::uo) is det.

    % draw_text(Window, X, Y, JX, JY, String, !IO)
    % Draw text String justified as follows: if the rendered text will
    % occupy W width and H height, then the top left corner of the rendered
    % text will appear at {X - JX * W, Y - JY * H}.
    %
    % The text is drawn without a background.
    %
    % Note that Xlib does not support rotated/scaled/distorted text.
    %
:- pred draw_text(window::in, float::in, float::in, float::in, float::in,
    string::in, io::di, io::uo) is det.

    % draw_image_text(Window, X, Y, JX, JY, String, !IO)
    % As draw_text, but the text appears on a filled rectangle
    % (see set_text_bg_colour).
    %
:- pred draw_image_text(window::in, float::in, float::in, float::in, float::in,
    string::in, io::di, io::uo) is det.

    % draw_text(Window, X, Y, String, !IO)
    % draw_image_text(Window, X, Y, String, !IO)
    % As their counterparts with justification arguments, but with
    % JX = JY = 0.0.
    %
:- pred draw_text(window::in, float::in, float::in, string::in,
    io::di, io::uo) is det.
:- pred draw_image_text(window::in, float::in, float::in, string::in,
    io::di, io::uo) is det.

    % draw_point(Window, X, Y, !IO)
    % Draw a single pixel at {X, Y}.
    %
:- pred draw_point(window::in, float::in, float::in, io::di, io::uo) is det.

    % draw_line(Window, X1, Y1, X2, Y2, !IO)
    % Draw a line from {X1, Y1} to {X2, Y2}.
    %
:- pred draw_line(window::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % draw_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO)
    % fill_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO)
    % Draw/fill an arc of an ellipse centred at {X, Y} with radii RX and RY,
    % starting at StartAngle and passing through ThroughAngle.
    %
:- pred draw_arc(window::in, float::in, float::in, float::in, float::in,
    float::in, float::in, io::di, io::uo) is det.
:- pred fill_arc(window::in, float::in, float::in, float::in, float::in,
    float::in, float::in, io::di, io::uo) is det.

    % draw_circle(Window, X, Y, R, !IO)
    % fill_circle(Window, X, Y, R, !IO)
    % Draw/fill a circle at {X, Y} with radius R.
    %
:- pred draw_circle(window::in, float::in, float::in, float::in,
    io::di, io::uo) is det.
:- pred fill_circle(window::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % draw_ellipse(Window, X, Y, RX, RY, !IO)
    % fill_ellipse(Window, X, Y, RX, RY, !IO)
    % Draw/fill an ellipse at {X, Y} with radii RX and RY.
    %
:- pred draw_ellipse(window::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.
:- pred fill_ellipse(window::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % draw_rectangle(Window, X1, Y1, X2, Y2, !IO)
    % fill_rectangle(Window, X1, Y1, X2, Y2, !IO)
    % Draw/fill a rectangle with opposite corners at {X1, Y1} and {X2, Y2}.
    %
:- pred draw_rectangle(window::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.
:- pred fill_rectangle(window::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % draw_lines(Window, Coords, !IO)
    % Draw a series of lines connecting each coordinate to its successor.
    %
:- pred draw_lines(window::in, coords::in, io::di, io::uo) is det.

    % fill_polygon(Window, Coords, !IO)
    % Fill the polygon whose vertices are given in order by Coords.
    %
:- pred fill_polygon(window::in, coords::in, io::di, io::uo) is det.


    % Support the construction of abstract drawing "programs".  Drawings
    % can be rotated, translated, scaled and mirrored. 
    %
    % XXX At present, not all operations affect all drawing primitives.
    % For instance, text neither rotates nor scales, nor do the angles
    % of arcs.
    %
:- type drawing_instruction
    --->    colour(colour)
    ;       colour_from_name(string)
    ;       colour_from_rgb(float, float, float)
    ;       line_attributes(line_width, cap_style, join_style)
    ;       font(font)
    ;       font_from_name(string)
    ;       line(float, float, float, float)
    ;       rectangle(float, float, float, float)
    ;       filled_rectangle(float, float, float, float)
    ;       arc(float, float, float, float, float, float)
    ;       filled_arc(float, float, float, float, float, float)
    ;       circle(float, float, float)
    ;       filled_circle(float, float, float)
    ;       ellipse(float, float, float, float)
    ;       filled_ellipse(float, float, float, float)
    ;       lines(coords)
    ;       filled_polygon(coords)
    ;       text(float, float, float, float, string)
    ;       image_text(float, float, float, float, string)
    ;       text(float, float, string)
    ;       image_text(float, float, string)
    ;       translate(float, float, drawing)
    ;       scale(float, drawing)
    ;       scale(float, float, drawing)
    ;       rotate(float, drawing)
    ;       mirror_x(drawing)
    ;       mirror_y(drawing).

:- type drawing == list(drawing_instruction).

    % draw(Window, Drawing, !IO)
    % Draw Drawing in Window!
    %
:- pred draw(window::in, drawing::in, io::di, io::uo) is det.


    % A restricted set of X events are recognised by this library.
    %
:- type x_event
                % Something has occurred (such as the window being resized)
                % that requires the window contents to be redrawn.
        --->    expose

                % button_press(X, Y, State, ButtonNo)
                % A mouse button has been pressed.  The mouse pointer
                % is at {X, Y}, State can be interrogated to see which
                % other mouse buttons and modifier keys (shift, control,
                % caps lock) are currently depressed.  ButtonNo is the
                % number of the mouse button that caused this event.
        ;       button_press(float, float, buttons_and_modifiers, button)

                % button_release(X, Y, State, ButtonNo)
                % As button_press, but indicates that a mouse button has
                % been released.
        ;       button_release(float, float, buttons_and_modifiers, button)

                % key_press(X, Y, State, KeyName)
                % A key has been pressed.  The mouse pointer is at {X, Y},
                % State indicates which mouse buttons and modifier keys are
                % currently depressed, and KeyName is a string indicating
                % which key caused this event.  (If the shift key is held
                % down when the key is pressed and the given key has a
                % shifted symbol, then KeyName is the name of the shifted key
                % symbol.)
                %
        ;       key_press(float, float, buttons_and_modifiers, string)

                % key_release(X, Y, State, KeyName)
                % As key_press, but indicated that a key has been released.
        ;       key_release(float, float, buttons_and_modifiers, string)

                % pointer_motion(X, Y, State)
                % The mouse pointer has moved to {X, Y}.  State indicates
                % which mouse buttons and modifier keys are currently
                % depressed.
        ;       pointer_motion(float, float, buttons_and_modifiers).

:- type buttons_and_modifiers.

    % The left hand mouse button is usually number 1.
    %
:- type button == int.

    % get_next_event(Window, Event, !IO)
    % Wait for the next input Event from the Window.
    %
:- pred get_next_event(window::in, x_event::out, io::di, io::uo) is det.

    % get_next_event_if_any(Window, MaybeEvent, !IO)
    % Get the next input event from the Window if one is available.
    %
:- pred get_next_event_if_any(window::in, maybe(x_event)::out, io::di, io::uo)
    is det.

    % Examine the State value of an event.
    %
:- pred button1(buttons_and_modifiers::in) is semidet.
:- pred button2(buttons_and_modifiers::in) is semidet.
:- pred button3(buttons_and_modifiers::in) is semidet.
:- pred button4(buttons_and_modifiers::in) is semidet.
:- pred button5(buttons_and_modifiers::in) is semidet.
:- pred shift(buttons_and_modifiers::in) is semidet.
:- pred lock(buttons_and_modifiers::in) is semidet.
:- pred control(buttons_and_modifiers::in) is semidet.
:- pred mod1(buttons_and_modifiers::in) is semidet.
:- pred mod2(buttons_and_modifiers::in) is semidet.
:- pred mod3(buttons_and_modifiers::in) is semidet.
:- pred mod4(buttons_and_modifiers::in) is semidet.
:- pred mod5(buttons_and_modifiers::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module math.
:- import_module store.
:- import_module string.
:- use_module xlib.



:- type display == xlib.display_ptr.

:- type font == xlib.font_struct_ptr.

:- type colour == xlib.color_ptr.

:- type buttons_and_modifiers == xlib.buttons_and_modifiers.

:- type window
    --->    window(
                display         :: io_mutvar(display),
                raw_window      :: io_mutvar(raw_window),
                pixmap          :: io_mutvar(pixmap),
                width           :: io_mutvar(int),
                height          :: io_mutvar(int),
                gc              :: io_mutvar(gc),
                gc_values_ptr   :: io_mutvar(gc_values_ptr),
                maybe_font      :: io_mutvar(maybe(font)),
                scaling_factor  :: io_mutvar(float)
            ).

    % Abstract types for various quantities.
    %
:- type raw_window == xlib.drawable.
:- type pixmap     == xlib.drawable.

:- type gc == xlib.gc.

:- type gc_values_ptr == xlib.gc_values_ptr.

:- type value_mask == xlib.value_mask.

:- type xpoints == xlib.xpoints.

%-----------------------------------------------------------------------------%

:- pred set_window_size(window::in, int::in, int::in, io::di, io::uo) is det.
:- pragma promise_pure(set_window_size/5).

set_window_size(Window, PW0, PH0, !IO) :-

    store.get_mutvar(Window^display,    Display, !IO),
    store.get_mutvar(Window^raw_window, Win,     !IO),
    store.get_mutvar(Window^pixmap,     OldPix,  !IO),
    store.get_mutvar(Window^gc,         GC,      !IO),
    store.get_mutvar(Window^width,      OldPW,   !IO),
    store.get_mutvar(Window^height,     OldPH,   !IO),

    PW = max(1, PW0),
    PH = max(1, PH0),

    ( if ( PW >= OldPW ; PH >= OldPH ) then

        ( if
            impure xlib.free_pixmap(Display, OldPix),
            impure NewPix = xlib.create_matching_pixmap(Display, Win)
          then
            store.set_mutvar(Window^pixmap, NewPix, !IO),
            Pix = NewPix
          else
            error("set_window_size(Window, %d, %d, !IO)", [i(PW0), i(PH0)])
        )

      else

        Pix = OldPix
    ),

    impure xlib.fill_rectangle(Display, Pix, GC, 0, 0, PW, PH),
    store.set_mutvar(Window^width,  PW, !IO),
    store.set_mutvar(Window^height, PH, !IO),
    set_window_scaling_factor(Window, !IO).


:- pred set_window_scaling_factor(window::in, io::di, io::uo) is det.

set_window_scaling_factor(Window, !IO) :-
    store.get_mutvar(Window^width, PW, !IO),
    store.get_mutvar(Window^height, PH, !IO),
    store.set_mutvar(Window^scaling_factor, float(min(PW, PH)), !IO).

%-----------------------------------------------------------------------------%

:- pred to_pixels(window::in, float::in, int::out, float::in, int::out,
            io::di, io::uo) is det.

to_pixels(Window, A, PA, B, PB, !IO) :-
    store.get_mutvar(Window^scaling_factor, ScalingFactor, !IO),
    PA = truncate_to_int(A * ScalingFactor),
    PB = truncate_to_int(B * ScalingFactor).

:- pred to_pixels(window::in,
            float::in, int::out,
            float::in, int::out,
            float::in, int::out,
            float::in, int::out,
            io::di, io::uo) is det.

to_pixels(Window, A, PA, B, PB, C, PC, D, PD, !IO) :-
    store.get_mutvar(Window^scaling_factor, ScalingFactor, !IO),
    PA = truncate_to_int(A * ScalingFactor),
    PB = truncate_to_int(B * ScalingFactor),
    PC = truncate_to_int(C * ScalingFactor),
    PD = truncate_to_int(D * ScalingFactor).


:- pred to_ratios(window::in, int::in, float::out, int::in, float::out,
            io::di, io::uo) is det.

to_ratios(Window, PA, A, PB, B, !IO) :-
    store.get_mutvar(Window^scaling_factor, ScalingFactor, !IO),
    A = float(PA) / ScalingFactor,
    B = float(PB) / ScalingFactor.


:- func a(float) = int.

a(Angle) = truncate_to_int(Angle * (180.0 * 64.0 / pi)).


:- func rgb_int(float) = int.

rgb_int(RGB) = truncate_to_int(65535.0 * RGB).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(open_display/3).

open_display(Display, !IO) :-
    ( if   impure Display0 = xlib.open_display
      then Display = Display0
      else error("open_display(Display, !IO)", [])
    ).


:- pragma promise_pure(open_display/4).

open_display(DisplayName, Display, !IO) :-
    ( if   impure Display0 = xlib.open_display(DisplayName)
      then Display = Display0
      else error("open_display(%s, Display, !IO)", [s(DisplayName)])
    ).


:- pragma promise_pure(flush/3).

flush(Window, !IO) :-
    restore_from_backing_pixmap(Window, !IO),
    store.get_mutvar(Window^display, Display, !IO),
    impure xlib.flush(Display).


:- pragma promise_pure(sync/3).

sync(Window, !IO) :-
    restore_from_backing_pixmap(Window, !IO),
    store.get_mutvar(Window^display, Display, !IO),
    impure xlib.sync(Display).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(create_window/7).

create_window(Display, WindowTitle, PW, PH, Window, !IO) :-
    ( if
        impure Win = xlib.create_simple_window(Display, PW, PH),
        impure Pix = xlib.create_matching_pixmap(Display, Win),
        impure xlib.set_window_name(Display, Win, WindowTitle),
        impure xlib.map_raised(Display, Win),
        GCValuesPtr = xlib.new_gc_values_ptr,
        impure GC = xlib.create_gc(Display, Win, 0, GCValuesPtr),
        impure xlib.sync(Display)
      then
        store.new_mutvar(Display, DisplayMutvar, !IO),
        store.new_mutvar(Win, WinMutvar, !IO),
        store.new_mutvar(Pix, PixMutvar, !IO),
        store.new_mutvar(0, WMutvar, !IO),
        store.new_mutvar(0, HMutvar, !IO),
        store.new_mutvar(GC, GCMutvar, !IO),
        store.new_mutvar(GCValuesPtr, GCValuesPtrMutvar, !IO),
        store.new_mutvar(no, MaybeFontMutvar, !IO),
        store.new_mutvar(0.0, ScalingFactorMutvar, !IO),
        Window = window(DisplayMutvar, WinMutvar, PixMutvar,
                    WMutvar, HMutvar, GCMutvar, GCValuesPtrMutvar,
                    MaybeFontMutvar, ScalingFactorMutvar),
        set_window_size(Window, PW, PH, !IO)
      else
        error("create_window(Display, \"%s\", %d, %d, Window, !IO)",
            [s(WindowTitle), i(PW), i(PH)])
    ).


:- pragma promise_pure(clear_window/3).

clear_window(Window, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^pixmap,  Pix,     !IO),
    store.get_mutvar(Window^gc,      GC,      !IO),
    store.get_mutvar(Window^width,   PW,      !IO),
    store.get_mutvar(Window^height,  PH,      !IO),
    impure xlib.fill_rectangle(Display, Pix, GC, 0, 0, PW, PH).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(load_font/5).

load_font(Window, FontName, Font, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    ( if   impure Font0 = xlib.load_query_font(Display, FontName)
      then Font = Font0
      else error("load_font(Window, \"%s\", Font, !IO)",
                [s(FontName)])
    ).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(get_colour_from_name/5).

get_colour_from_name(Window, ColourName, Colour, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    ( if   impure Colour0 = xlib.alloc_named_color(Display, ColourName)
      then Colour = Colour0
      else error("get_colour_from_name(Window, \"%s\", Colour, !IO)",
                [s(ColourName)])
    ).


:- pragma promise_pure(get_colour_from_rgb/7).

get_colour_from_rgb(Window, R, G, B, Colour, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    ( if   impure Colour0 = xlib.alloc_rgb_color(Display,
                        rgb_int(R), rgb_int(G), rgb_int(B))
      then Colour = Colour0
      else error("get_colour_from_rgb(Display, %4.2f, %4.2f, %4.2f, " ++
                "Colour, !IO)", [f(R), f(G), f(B)])
    ).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(set_colour/4).

set_colour(Window, Colour, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    impure xlib.set_fg(Display, GC, Colour).

set_colour_from_name(Window, ColourName, !IO) :-
    get_colour_from_name(Window, ColourName, Colour, !IO),
    set_colour(Window, Colour, !IO).

set_colour_from_rgb(Window, R, G, B, !IO) :-
    get_colour_from_rgb(Window, R, G, B, Colour, !IO),
    set_colour(Window, Colour, !IO).


:- pragma promise_pure(set_text_bg_colour/4).

set_text_bg_colour(Window, Colour, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    impure xlib.set_bg(Display, GC, Colour).


:- pragma promise_pure(set_line_attributes/6).

set_line_attributes(Window, LineWidth, CapStyle, JoinStyle, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    (
        LineWidth = pixels(PW0)
    ;
        LineWidth = ratio(W),
        store.get_mutvar(Window^scaling_factor, ScalingFactor, !IO),
        PW0 = truncate_to_int(W * ScalingFactor)
    ),
    PW = max(0, PW0),
    (   CapStyle = not_last,    XLibCapStyle = xlib.cap_not_last
    ;   CapStyle = butt,        XLibCapStyle = xlib.cap_butt
    ;   CapStyle = round,       XLibCapStyle = xlib.cap_round
    ;   CapStyle = projecting,  XLibCapStyle = xlib.cap_projecting
    ),
    (   JoinStyle = mitre,      XLibJoinStyle = xlib.join_mitre
    ;   JoinStyle = round,      XLibJoinStyle = xlib.join_round
    ;   JoinStyle = bevel,      XLibJoinStyle = xlib.join_bevel
    ),
    impure xlib.set_line_attributes(Display, GC, PW, XLibCapStyle,
                XLibJoinStyle).


:- pragma promise_pure(set_font/4).

set_font(Window, Font, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    impure xlib.set_font(Display, GC, Font),
    store.set_mutvar(Window^maybe_font, yes(Font), !IO).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_text/8).

draw_text(Window, X0, Y0, JX, JY, Text, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^maybe_font, MaybeFont, !IO),
    ( if MaybeFont = yes(Font) then
        Height = Font^xlib.height,
        Width  = xlib.text_width(Font, Text),
        to_pixels(Window, X0, PX0, Y0, PY0, !IO),
        PX     =                    PX0 - truncate_to_int(JX * float(Width)),
        PY     = Font^xlib.ascent + PY0 - truncate_to_int(JY * float(Height)),
        % impure xlib.draw_string(Display, Win, GC, PX, PY, Text),
        impure xlib.draw_string(Display, Pix, GC, PX, PY, Text)
      else
        error("draw_text(Window, %f, %f, %f, %f, \"%s\", !IO): " ++
            "font not set", [f(X0), f(Y0), f(JX), f(JY), s(Text)])
    ).


:- pragma promise_pure(draw_image_text/8).

draw_image_text(Window, X0, Y0, JX, JY, Text, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^maybe_font, MaybeFont, !IO),
    ( if MaybeFont = yes(Font) then
        Height = Font^xlib.height,
        Width  = xlib.text_width(Font, Text),
        to_pixels(Window, X0, PX0, Y0, PY0, !IO),
        PX     =                    PX0 - truncate_to_int(JX * float(Width)),
        PY     = Font^xlib.ascent + PY0 - truncate_to_int(JY * float(Height)),
        % impure xlib.draw_string(Display, Win, GC, PX, PY, Text),
        impure xlib.draw_image_string(Display, Pix, GC, PX, PY, Text)
      else
        error("draw_image_text(Window, %f, %f, %f, %f, \"%s\", !IO): " ++
            "font not set", [f(X0), f(Y0), f(JX), f(JY), s(Text)])
    ).


draw_text(Window, X, Y, Text, !IO) :-
    draw_text(Window, X, Y, 0.0, 0.0, Text, !IO).

draw_image_text(Window, X, Y, Text, !IO) :-
    draw_image_text(Window, X, Y, 0.0, 0.0, Text, !IO).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_arc/9).

draw_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X, PX, Y, PY, RX, PRX, RY, PRY, !IO),
    % impure xlib.draw_arc(Display, Win, GC,
                % PX - PRX/2, PY - PRY/2, PRX, PRY,
                % a(StartAngle), a(ThroughAngle)),
    impure xlib.draw_arc(Display, Pix, GC,
                PX - PRX, PY - PRY, PRX + PRX, PRY + PRX,
                a(StartAngle), a(ThroughAngle)).


:- pragma promise_pure(fill_arc/9).

fill_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X, PX, Y, PY, RX, PRX, RY, PRY, !IO),
    % impure xlib.fill_arc(Display, Win, GC,
                % PX - PRX/2, PY - PRY/2, PRX, PRY,
                % a(StartAngle), a(ThroughAngle)),
    impure xlib.fill_arc(Display, Pix, GC,
                PX - PRX, PY - PRY, PRX + PRX, PRY + PRY,
                a(StartAngle), a(ThroughAngle)).


draw_circle(Window, X, Y, R, !IO) :-
    draw_arc(Window, X, Y, R, R, 0.0, pi + pi, !IO).


fill_circle(Window, X, Y, R, !IO) :-
    fill_arc(Window, X, Y, R, R, 0.0, pi + pi, !IO).


draw_ellipse(Window, X, Y, RX, RY, !IO) :-
    draw_arc(Window, X, Y, RX, RY, 0.0, pi + pi, !IO).


fill_ellipse(Window, X, Y, RX, RY, !IO) :-
    fill_arc(Window, X, Y, RX, RY, 0.0, pi + pi, !IO).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_point/5).

draw_point(Window, X, Y, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X, PX, Y, PY, !IO),
    % impure xlib.draw_point(Display, Win, GC, PX, PY),
    impure xlib.draw_point(Display, Pix, GC, PX, PY).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_line/7).

draw_line(Window, X1, Y1, X2, Y2, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X1, PX1, Y1, PY1, X2, PX2, Y2, PY2, !IO),
    % impure xlib.draw_line(Display, Win, GC, PX1, PY1, PX2, PY2),
    impure xlib.draw_line(Display, Pix, GC, PX1, PY1, PX2, PY2).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_rectangle/7).

draw_rectangle(Window, X1, Y1, X2, Y2, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X1, PX1, Y1, PY1, X2, PX2, Y2, PY2, !IO),
    % impure xlib.draw_rectangle(Display, Win, GC, PX1, PY1, PX2, PY2),
    impure xlib.draw_rectangle(Display, Pix, GC, PX1, PY1, PX2, PY2).


:- pragma promise_pure(fill_rectangle/7).

fill_rectangle(Window, X1, Y1, X2, Y2, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    to_pixels(Window, X1, PX1, Y1, PY1, X2, PX2, Y2, PY2, !IO),
    % impure xlib.fill_rectangle(Display, Win, GC, PX1, PY1, PX2, PY2),
    impure xlib.fill_rectangle(Display, Pix, GC, PX1, PY1, PX2, PY2).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(draw_lines/4).

draw_lines(Window, Coords, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    create_xpoints(Window, Coords, XPoints, !IO),
    % impure xlib.draw_lines(Display, Win, GC, XPoints),
    impure xlib.draw_lines(Display, Pix, GC, XPoints).


:- pragma promise_pure(fill_polygon/4).

fill_polygon(Window, Coords, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    % store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    create_xpoints(Window, Coords, XPoints, !IO),
    % impure xlib.fill_polygon(Display, Win, GC, XPoints),
    impure xlib.fill_polygon(Display, Pix, GC, XPoints).


:- pred create_xpoints(window::in, coords::in, xpoints::out, io::di, io::uo)
            is det.

create_xpoints(Window, Coords, XPoints, !IO) :-
    to_xpts(Window, Coords, XPts, !IO),
    XPoints = xlib.xpoints(XPts).


:- pred to_xpts(window::in, coords::in, list(xlib.xpoint)::out,
            io::di, io::uo) is det.

to_xpts(_Window, [], [], !IO).

to_xpts( Window, [{X, Y} | XYs], [XPt | XPts], !IO) :-
    to_pixels(Window, X, PX, Y, PY, !IO),
    XPt = xlib.xpoint(PX, PY),
    to_xpts(Window, XYs, XPts, !IO).

%-----------------------------------------------------------------------------%

:- pred restore_from_backing_pixmap(window::in, io::di, io::uo) is det.

restore_from_backing_pixmap(Window, !IO) :-
    store.get_mutvar(Window^width,  PW, !IO),
    store.get_mutvar(Window^height, PH, !IO),
    restore_from_backing_pixmap(Window, 0, 0, PW, PH, !IO).


:- pred restore_from_backing_pixmap(window::in, int::in, int::in,
            int::in, int::in, io::di, io::uo) is det.

:- pragma promise_pure(restore_from_backing_pixmap/7).

restore_from_backing_pixmap(Window, PX, PY, PW, PH, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^raw_window, Win, !IO),
    store.get_mutvar(Window^gc, GC, !IO),
    store.get_mutvar(Window^pixmap, Pix, !IO),
    impure xlib.copy_area(Display, Pix, Win, GC, PX, PY, PW, PH, PX, PY).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(get_next_event/4).

get_next_event(Window, Event, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^raw_window, Win, !IO),
    impure EventPtr = xlib.window_event(Display, Win),
    process_event_ptr(Window, EventPtr, MaybeEvent, !IO),
    (
        MaybeEvent = yes(Event)
    ;
        MaybeEvent = no,
        get_next_event(Window, Event, !IO)
    ).


:- pragma promise_pure(get_next_event_if_any/4).

get_next_event_if_any(Window, MaybeEvent, !IO) :-
    store.get_mutvar(Window^display, Display, !IO),
    store.get_mutvar(Window^raw_window, Win, !IO),
    ( if
        impure EventPtr = xlib.check_window_event(Display, Win)
      then
        process_event_ptr(Window, EventPtr, MaybeEvent, !IO)
      else
        MaybeEvent = no
    ).


:- pred process_event_ptr(window::in, xlib.event_ptr::in, maybe(x_event)::out,
    io::di, io::uo) is det.

process_event_ptr(Window, EventPtr, MaybeEvent, !IO) :-
    store.get_mutvar(Window^width,  PW0, !IO),
    store.get_mutvar(Window^height, PH0, !IO),
    ( if
        xlib.resize_event(EventPtr, PW, PH)
      then
        ( if ( PW \= PW0 ; PH \= PH0 ) then
            set_window_size(Window, PW, PH, !IO),
            MaybeEvent = yes(expose)
          else
            MaybeEvent = no
        )
      else if
        xlib.expose_event(EventPtr, PX, PY, PW, PH, _Count)
      then
        restore_from_backing_pixmap(Window, PX, PY, PW, PH, !IO),
        MaybeEvent = no
      else if
        xlib.button_press_event(EventPtr, PX, PY, State, Button)
      then
        to_ratios(Window, PX, X, PY, Y, !IO),
        MaybeEvent = yes(button_press(X, Y, State, Button))
      else if
        xlib.button_release_event(EventPtr, PX, PY, State, Button)
      then
        to_ratios(Window, PX, X, PY, Y, !IO),
        MaybeEvent = yes(button_release(X, Y, State, Button))
      else if
        xlib.key_press_event(EventPtr, PX, PY, State, KeyString)
      then
        to_ratios(Window, PX, X, PY, Y, !IO),
        MaybeEvent = yes(key_press(X, Y, State, KeyString))
      else if
        xlib.key_release_event(EventPtr, PX, PY, State, KeyString)
      then
        to_ratios(Window, PX, X, PY, Y, !IO),
        MaybeEvent = yes(key_release(X, Y, State, KeyString))
      else if
        xlib.pointer_motion_event(EventPtr, PX, PY, State)
      then
        to_ratios(Window, PX, X, PY, Y, !IO),
        MaybeEvent = yes(pointer_motion(X, Y, State))
      else
        MaybeEvent = no
    ).


button1(State) :- xlib.button1(State).
button2(State) :- xlib.button2(State).
button3(State) :- xlib.button3(State).
button4(State) :- xlib.button4(State).
button5(State) :- xlib.button5(State).
shift(State)   :- xlib.shift(State).
lock(State)    :- xlib.lock(State).
control(State) :- xlib.control(State).
mod1(State)    :- xlib.mod1(State).
mod2(State)    :- xlib.mod2(State).
mod3(State)    :- xlib.mod3(State).
mod4(State)    :- xlib.mod4(State).
mod5(State)    :- xlib.mod5(State).

%-----------------------------------------------------------------------------%

draw(Window, Instrs, !IO) :-
    draw_2(Window, identity, Instrs, !IO).


:- pred draw_2(window::in, transformation::in, drawing::in, io::di, io::uo)
            is det.

draw_2(Window, Matrix, Instrs, !IO) :-
    list.foldl(draw_instr(Window, Matrix), Instrs, !IO).


:- pred draw_instr(window::in, transformation::in, drawing_instruction::in,
            io::di, io::uo) is det.

draw_instr(Window, _     , colour(Colour), !IO) :-
    set_colour(Window, Colour, !IO).

draw_instr(Window, _     , colour_from_name(ColourName), !IO) :-
    get_colour_from_name(Window, ColourName, Colour, !IO),
    set_colour(Window, Colour, !IO).

draw_instr(Window, _     , colour_from_rgb(R, G, B), !IO) :-
    get_colour_from_rgb(Window, R, G, B, Colour, !IO),
    set_colour(Window, Colour, !IO).

draw_instr(Window, _     , line_attributes(Width, CapStyle, JoinStyle), !IO) :-
    set_line_attributes(Window, Width, CapStyle, JoinStyle, !IO).

draw_instr(Window, _     , font(Font), !IO) :-
    set_font(Window, Font, !IO).

draw_instr(Window, _     , font_from_name(FontName), !IO) :-
    load_font(Window, FontName, Font, !IO),
    set_font(Window, Font, !IO).

draw_instr(Window, Matrix, line(X10, Y10, X20, Y20), !IO) :-
    apply_point_transformation(Matrix, X10, Y10, X1, Y1),
    apply_point_transformation(Matrix, X20, Y20, X2, Y2),
    draw_line(Window, X1, Y1, X2, Y2, !IO).

draw_instr(Window, Matrix, rectangle(X1, Y1, X2, Y2), !IO) :-
    ( if Matrix = identity then
        draw_rectangle(Window, X1, Y1, X2, Y2, !IO)
      else
        draw_instr(Window, Matrix, lines([{X1, Y1}, {X2, Y1}, {X2, Y2},
            {X1, Y2}, {X1, Y1}]), !IO)
    ).

draw_instr(Window, Matrix, filled_rectangle(X1, Y1, X2, Y2), !IO) :-
    ( if Matrix = identity then
        fill_rectangle(Window, X1, Y1, X2, Y2, !IO)
      else
        draw_instr(Window, Matrix, filled_polygon([{X1, Y1}, {X2, Y1},
            {X2, Y2}, {X1, Y2}, {X1, Y1}]), !IO)
    ).

    % XXX The transformation is not fully applied here.
    %
draw_instr(Window, Matrix, arc(X0, Y0, RX, RY, StartAngle, ThroughAngle),
        !IO) :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    draw_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO).

draw_instr(Window, Matrix, filled_arc(X0, Y0, RX0, RY0,
        StartAngle, ThroughAngle), !IO) :-
    apply_point_transformation(Matrix,  X0,  Y0,  X,  Y),
    apply_radii_transformation(Matrix, RX0, RY0, RX, RY),
    fill_arc(Window, X, Y, RX, RY, StartAngle, ThroughAngle, !IO).

draw_instr(Window, Matrix, circle(X0, Y0, R), !IO) :-
    draw_instr(Window, Matrix, arc(X0, Y0, R, R, 0.0, pi + pi), !IO).

draw_instr(Window, Matrix, filled_circle(X0, Y0, R), !IO) :-
    draw_instr(Window, Matrix, filled_arc(X0, Y0, R, R, 0.0, pi + pi), !IO).

draw_instr(Window, Matrix, ellipse(X0, Y0, RX, RY), !IO) :-
    draw_instr(Window, Matrix, arc(X0, Y0, RX, RY, 0.0, pi + pi), !IO).

draw_instr(Window, Matrix, filled_ellipse(X0, Y0, RX, RY), !IO) :-
    draw_instr(Window, Matrix, filled_arc(X0, Y0, RX, RY, 0.0, pi + pi), !IO).

draw_instr(Window, Matrix, lines(Coords0), !IO) :-
    Coords = apply_point_transformation_coords(Matrix, Coords0),
    draw_lines(Window, Coords, !IO).

draw_instr(Window, Matrix, filled_polygon(Coords0), !IO) :-
    Coords = apply_point_transformation_coords(Matrix, Coords0),
    fill_polygon(Window, Coords, !IO).

draw_instr(Window, Matrix, text(X0, Y0, JX, JY, Text), !IO) :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    draw_text(Window, X, Y, JX, JY, Text, !IO).

draw_instr(Window, Matrix, image_text(X0, Y0, JX, JY, Text), !IO) :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    draw_image_text(Window, X, Y, JX, JY, Text, !IO).

draw_instr(Window, Matrix, text(X0, Y0, Text), !IO) :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    draw_text(Window, X, Y, Text, !IO).

draw_instr(Window, Matrix, image_text(X0, Y0, Text), !IO) :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    draw_image_text(Window, X, Y, Text, !IO).

draw_instr(Window, Matrix, translate(DX, DY, Instrs), !IO) :-
    draw_2(Window, chg_translation(Matrix, DX, DY), Instrs, !IO).

draw_instr(Window, Matrix, scale(S, Instrs), !IO) :-
    draw_instr(Window, Matrix, scale(S, S, Instrs), !IO).

draw_instr(Window, Matrix, scale(SX, SY, Instrs), !IO) :-
    draw_2(Window, chg_scaling(Matrix, SX, SY), Instrs, !IO).

draw_instr(Window, Matrix, rotate(R, Instrs), !IO) :-
    draw_2(Window, chg_rotation(Matrix, R), Instrs, !IO).

draw_instr(Window, Matrix, mirror_x(Instrs), !IO) :-
    draw_2(Window, chg_mirror_x(Matrix), Instrs, !IO).

draw_instr(Window, Matrix, mirror_y(Instrs), !IO) :-
    draw_2(Window, chg_mirror_y(Matrix), Instrs, !IO).

%-----------------------------------------------------------------------------%
% We support a number of transformations:
%
% Translation displacing x by a and y by b:
%   ( 1 0 a )(x)     (x + a)
%   ( 0 1 b )(y)  =  (y + b)
%   ( 0 0 1 )(1)     (  1  )
%
% Scaling x by a factor of c and y by a factor of d:
%   ( c 0 0 )(x)     (cx)
%   ( 0 d 0 )(y)  =  (dy)
%   ( 0 0 1 )(1)     ( 1)
%
% Rotation through an angle theta where c = cos(theta), s = sin(theta):
%   ( c -s 0 )(x)  =  (cx - sy)
%   ( s  c 0 )(y)  =  (sx + cy)
%   ( 0  0 1 )(1)  =  (   1   )
%
% Reflection about x = 0:
%   (-1  0 0 )(x)     (-x)
%   ( 0  1 0 )(y)  =  ( y)
%   ( 0  0 1 )(1)  =  ( 1)
%
% Reflection about y = 0:
%   ( 1  0 0 )(x)     ( x)
%   ( 0 -1 0 )(y)  =  (-y)
%   ( 0  0 1 )(1)  =  ( 1)
%
% Since the bottom row of each matrix is always (0 0 1) we don't bother
% storing it.

:- type transformation == {float, float, float,
                           float, float, float}.


:- func identity = transformation.
identity = {1.0, 0.0, 0.0,
            0.0, 1.0, 0.0}.


:- func chg_translation(transformation, float, float) = transformation.
chg_translation({A, B, C, D, E, F}, DX, DY) =
    {A, B, A*DX + B*DY + C, D, E, D*DX + E*DY + F}.

:- func chg_scaling(transformation, float, float) = transformation.
chg_scaling({A, B, C, D, E, F}, SX, SY) =
    {A*SX, B*SY, C, D*SX, E*SY, F}.

:- func chg_rotation(transformation, float) = transformation.
chg_rotation({A, B, C, D, E, F}, R) = T :-
    CR = cos(R),
    SR = sin(R),
    T  = {A*CR + B*SR, -A*SR + B*CR, C,
          D*CR + E*SR, -D*SR + E*CR, F}.

:- func chg_mirror_x(transformation) = transformation.
chg_mirror_x({A, B, C, D, E, F}) =
    {-A, B, C, -D, E, F}.

:- func chg_mirror_y(transformation) = transformation.
chg_mirror_y({A, B, C, D, E, F}) =
    {A, -B, C, D, -E, F}.


:- pred apply_radii_transformation(transformation::in, float::in, float::in,
            float::out, float::out).

apply_radii_transformation({A, B, _, D, E, _},
    RX, RY, A*RX + B*RY, D*RX + E*RY).


:- pred apply_point_transformation(transformation::in, float::in, float::in,
            float::out, float::out).

apply_point_transformation({A, B, C, D, E, F},
    X, Y, A*X + B*Y + C, D*X + E*Y + F).


:- func apply_point_transformation_coords(transformation, coords) = coords.

apply_point_transformation_coords(_,      []               ) = [].
apply_point_transformation_coords(Matrix, [{X0, Y0} | XYs0]) =
        [{X, Y} | XYs] :-
    apply_point_transformation(Matrix, X0, Y0, X, Y),
    XYs = apply_point_transformation_coords(Matrix, XYs0).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred error(string::in, list(string.poly_type)::in) is erroneous.

error(Fmt, Args) :-
    throw(string.format("easyx." ++ Fmt, Args) `with_type` string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
