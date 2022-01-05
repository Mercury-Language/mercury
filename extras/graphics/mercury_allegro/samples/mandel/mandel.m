%-----------------------------------------------------------------------------%
%
% A Mandelbrot fractal viewer, by Peter Wang.
%
% Drag the left mouse button at any time to zoom in on a region of the fractal.
% Click the right mouse button to zoom back out.
% Press Escape to quit.
%
%-----------------------------------------------------------------------------%

:- module mandel.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.mouse.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.timer.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        set_gfx_mode(gfx_autodetect, 800, 800, 0, 0, GfxOk, !IO),
        (
            GfxOk = yes,
            install_keyboard(_KeyboardOk, !IO),
            install_timer(_TimerOk, !IO),
            install_mouse(_MouseOk, !IO),
            enable_hardware_cursor(!IO),
            make_custom_palette(Pal, !IO),
            set_palette(Pal,!IO),
            det_screen(_, ScreenW, ScreenH, !IO),
            det_create_bitmap(ScreenW, ScreenH, Bitmap, !IO),
            det_create_bitmap(ScreenW, 1, Slice, !IO),
            Viewport = viewport(ScreenW, ScreenH, -0.5, 0.0, 3.0, 3.0),
            select_and_show_mouse_cursor(mouse_cursor_busy, !IO),
            main_loop(Slice, Viewport, [], 0, 0, !IO),
            destroy_bitmap(Slice, !IO),
            destroy_bitmap(Bitmap, !IO)
        ;
            GfxOk = no,
            allegro_message("Could not open window", !IO)
        )
    ;
        Ok = no,
        io.print("Error initialising Allegro\n", !IO)
    ).

:- pred main_loop(bitmap::in, viewport::in, list(viewport)::in,
        int::in, int::in, io::di, io::uo) is det.

main_loop(Slice, Viewport, ViewportHistory, Y0, LinesFilled0, !IO) :-
    screen_h(ScreenH, !IO),
    draw_mandelbrot_slice(Slice, Viewport, Y0, !IO),
    LinesFilled1 = LinesFilled0+1,
    (if LinesFilled1 = ScreenH then
        select_and_show_mouse_cursor(mouse_cursor_arrow, !IO),
        wait_for_input(Input, !IO),
        select_and_show_mouse_cursor(mouse_cursor_busy, !IO),
        Y = Y0,
        LinesFilled = 0
    else 
        get_input(Input, !IO),
        % XXX: this relies on ScreenH not being a multiple of 3.
        Y = (Y0 + 3) mod ScreenH,
        LinesFilled = LinesFilled1
    ),
    (
        Input = keyboard(Ascii),
        (if Ascii = '\033\' then
            true    % quit
        else
            main_loop(Slice, Viewport, ViewportHistory, Y, LinesFilled, !IO)
        )
    ;
        Input = mouse(lmb, MouseX0, MouseY0),
        select_and_show_mouse_cursor(mouse_cursor_arrow, !IO),
        wait_until_mouse_b_zero(!IO),
        mouse_xy(MouseX1, MouseY1, !IO),
        select_and_show_mouse_cursor(mouse_cursor_busy, !IO),
        viewport_from_rect(Viewport, MouseX0, MouseY0, MouseX1, MouseY1,
            Viewport1),
        select_and_show_mouse_cursor(mouse_cursor_busy, !IO),
        darken_screen(!IO),
        main_loop(Slice, Viewport1, [Viewport|ViewportHistory], Y, 0, !IO)
    ;
        Input = mouse(rmb, _MouseX0, _MouseY0),
        wait_until_mouse_b_zero(!IO),
        (
            ViewportHistory = [],
            main_loop(Slice, Viewport, ViewportHistory, Y, LinesFilled, !IO)
        ;
            ViewportHistory = [VH | VHs],
            darken_screen(!IO),
            main_loop(Slice, VH, VHs, Y, 0, !IO)
        )
    ;
        Input = none,
        main_loop(Slice, Viewport, ViewportHistory, Y, LinesFilled, !IO)
    ).

%-----------------------------------------------------------------------------%

:- type input
    --->    mouse(mouse_button, int, int)
    ;       keyboard(char)
    ;       none.

:- type mouse_button
    --->    lmb
    ;       rmb.

:- pred wait_for_input(input::out, io::di, io::uo) is det.

wait_for_input(Input, !IO) :-
    get_input(Input0, !IO),
    (if Input0 = none then
        delay(!IO),
        wait_for_input(Input, !IO)
    else
        Input = Input0
    ).

:- pred get_input(input::out, io::di, io::uo) is det.

get_input(Input, !IO) :-
    mouse_b(B, !IO),
    mouse_xy(X, Y, !IO),
    (if button_pressed(lmb, B) then
        Input = mouse(lmb, X, Y)
    else if button_pressed(rmb, B) then
        Input = mouse(rmb, X, Y)
    else
        keypressed(KP, !IO),
        (
            KP = yes,
            readkey_decode(Ascii, _, !IO),
            Input = keyboard(Ascii)
        ;
            KP = no,
            Input = none
        )
    ).

:- pred wait_until_mouse_b_zero(io::di, io::uo) is det.

wait_until_mouse_b_zero(!IO) :-
    mouse_b(B, !IO),
    (if B = 0 then
        true
    else
        delay(!IO),
        wait_until_mouse_b_zero(!IO)
    ).

:- pred button_pressed(mouse_button::in, int::in) is semidet.

button_pressed(lmb, MouseB) :- (MouseB /\ 1) \= 0.
button_pressed(rmb, MouseB) :- (MouseB /\ 2) \= 0.

%-----------------------------------------------------------------------------%

:- type viewport
    --->    viewport(
                width       :: int,
                height      :: int,
                centre_x    :: float,
                centre_y    :: float,
                x_extent    :: float,
                y_extent    :: float
            ).

:- func x_lower(viewport) = float.
:- func y_lower(viewport) = float.
:- func x_scale(viewport) = float.
:- func y_scale(viewport) = float.

x_lower(VP) = VP^centre_x - VP^x_extent / 2.0.
y_lower(VP) = VP^centre_y - VP^y_extent / 2.0.
x_scale(VP) = VP^x_extent / float(VP^width).
y_scale(VP) = VP^y_extent / float(VP^height).

:- pred viewport_from_rect(viewport::in, int::in, int::in, int::in, int::in,
        viewport::out) is det.

viewport_from_rect(Viewport0, X0, Y0, X1, Y1, Viewport) :-
    XLower0 = x_lower(Viewport0),
    YLower0 = y_lower(Viewport0),
    XScale0 = x_scale(Viewport0),
    YScale0 = y_scale(Viewport0),

    XLower = XLower0 + float(min(X0, X1)) * XScale0,
    XUpper = XLower0 + float(max(X0, X1)) * XScale0,
    YLower = YLower0 + float(min(Y0, Y1)) * YScale0,
    YUpper = YLower0 + float(max(Y0, Y1)) * YScale0,

    CentreX = (XUpper + XLower) / 2.0,
    CentreY = (YUpper + YLower) / 2.0,
    XExtent = XUpper - XLower,
    YExtent = YUpper - YLower,
    Extent = max(XExtent, YExtent), % keep square aspect ratio

    Width = Viewport0 ^ width,
    Height = Viewport0 ^ height,
    Viewport = viewport(Width, Height, CentreX, CentreY, Extent, Extent).

:- pred draw_mandelbrot_slice(bitmap::in, viewport::in, int::in,
        io::di, io::uo) is det.

draw_mandelbrot_slice(Slice, Viewport, Y, !IO) :-
    Im = y_lower(Viewport) + float(Y) * y_scale(Viewport),
    int.fold_up2(draw_mandel_x(Slice, Im, x_scale(Viewport)),
        0, bitmap_w(Slice)-1,
        x_lower(Viewport), _Re, !IO),
    scare_mouse(!IO),
    det_screen(Screen, !IO),
    blit(Slice, Screen, 0, 0, 0, Y, bitmap_w(Slice), 1, !IO),
    unscare_mouse(!IO).

:- pred draw_mandel_x(bitmap::in, float::in, float::in, int::in,
        float::in, float::out, io::di, io::uo) is det.

draw_mandel_x(Slice, Im, XScale, X, Re, Re+XScale, !IO) :-
    mandel(cmplx(Re, Im), Insideness),
    (
        Insideness = inside,
        unsafe_putpixel(Slice, X, 0, 0, !IO)
    ;
        Insideness = outside(Iterations),
        Col = 128 + round_to_int(127.0 * sin(float(Iterations) / 32.0)),
        % Col = Iterations /\ 0xff,
        unsafe_putpixel(Slice, X, 0, Col, !IO)
    ).

:- func max_iter = int.
:- func z_max2 = float.
:- pred mandel(complex::in, insideness::out) is det.
:- pred mandel_2(complex::in, complex::in, int::in, insideness::out) is det.

:- type insideness
    --->    inside
    ;       outside(int).

max_iter = 512.
z_max2 = 4.0.

mandel(C, Insideness) :-
    mandel_2(complex(0.0), C, 0, Insideness).

mandel_2(Z, C, Iter, Insideness) :-
    (if Iter > max_iter then
        Insideness = inside
    else
        Z1 = sqr(Z) + C,
        (if abs2(Z1) > z_max2 then
            Insideness = outside(Iter+1)
        else
            mandel_2(Z1, C, Iter+1, Insideness)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred make_custom_palette(palette::out, io::di, io::uo) is det.
:- pred make_custom_palette_2(palette::in, int::in, io::di, io::uo) is det.

make_custom_palette(Pal, !IO) :-
    new_palette(Pal, !IO),
    set_palette_entry(Pal, 0, 0, 0, 0, !IO),
    int.fold_up(make_custom_palette_2(Pal), 1, pal_size-1, !IO).

make_custom_palette_2(Pal, Index, !IO) :-
    R0 = 42/4,
    G0 = 65/4,
    B0 = 95/4,
    R = R0 + (63 - R0) * Index / 256,
    G = G0 + (63 - G0) * Index / 256,
    B = B0 + (63 - B0) * Index / 256,
    set_palette_entry(Pal, Index, R, G, B, !IO).

:- pred darken_screen(io::di, io::uo) is det.

darken_screen(!IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    det_create_bitmap(ScreenW, ScreenH, Bitmap, !IO),
    blit(Screen, Bitmap, 0, 0, 0, 0, ScreenW, ScreenH, !IO),
    darken_bitmap(Bitmap, !IO),
    blit(Bitmap, Screen, 0, 0, 0, 0, ScreenW, ScreenH, !IO),
    destroy_bitmap(Bitmap, !IO).

:- pred darken_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred darken_bitmap_2(bitmap::in, int::in, io::di, io::uo) is det.
:- pred darken_bitmap_3(bitmap::in, int::in, int::in, io::di, io::uo) is det.

darken_bitmap(Bitmap, !IO) :-
    int.fold_up(darken_bitmap_2(Bitmap), 0, bitmap_h(Bitmap)-1, !IO).

darken_bitmap_2(Bitmap, Y, !IO) :-
    int.fold_up(darken_bitmap_3(Bitmap, Y), 0, bitmap_w(Bitmap)-1, !IO).

darken_bitmap_3(Bitmap, X, Y, !IO) :-
    unsafe_getpixel(Bitmap, X, Y, C, !IO),
    unsafe_putpixel(Bitmap, X, Y, C>>2, !IO).

:- pred select_and_show_mouse_cursor(mouse_cursor::in, io::di, io::uo) is det.

select_and_show_mouse_cursor(Cursor, !IO) :-
    select_mouse_cursor(Cursor, !IO),
    det_screen(Screen, !IO),
    show_mouse(Screen, !IO).

:- pred delay(io::di, io::uo) is det.

delay(!IO) :-
    rest(10, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
%   Ripped out of the `complex_numbers' library from mercury-extras.
%

:- type complex ---> cmplx(float, float).	% real part, imag part

	% convert float to complex
:- func complex(float) = complex.
complex(Real) = cmplx(Real, 0.0).

	% sqr(X) = X * X.
:- func sqr(complex) = complex.
:- mode sqr(in) = out is det.
sqr(cmplx(Re0, Im0)) = cmplx(Re, Im) :-
	Re = Re0 * Re0 - Im0 * Im0,
	Im = 2.0 * Re0 * Im0.

	% addition
:- func complex + complex = complex.
:- mode in  + in  = uo  is det.
cmplx(Xr, Xi) + cmplx(Yr, Yi) = cmplx(Xr + Yr, Xi + Yi).

	% square of absolute value
:- func abs2(complex) = float.
abs2(cmplx(R, I)) = R*R + I*I.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
