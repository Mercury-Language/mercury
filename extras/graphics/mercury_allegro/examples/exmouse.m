%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This program demonstrates how to get mouse input. The
% first part of the test retrieves the raw mouse input data
% and displays it on the screen without using any mouse
% cursor. When you press a key the standard arrow-like mouse
% cursor appears.  You are not restricted to this shape,
% and a second keypress modifies the cursor to be several
% concentric colored circles. They are not joined together,
% so you can still see bits of what's behind when you move the
% cursor over the printed text message.
%
%-----------------------------------------------------------------------------%

:- module exmouse.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.color.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.mouse.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.text.
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(_, !IO),
        install_timer(_, !IO),
        set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
        (
            GfxOk = yes,
            main_2(!IO)
        ;
            GfxOk = no,
            allegro_message("Unable to set any graphic mode\n", !IO)
        )
    ;
        Ok = no
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    font(Font, !IO),
    set_palette(desktop_palette, !IO),
    black_white(Black, White, !IO),
    clear_to_color(Screen, White, !IO),
    install_mouse(Result, !IO),
    (
        Result = ok(_),
        textout_centre_ex(Screen, Font, "exmouse",
            ScreenW/2, 8, Black, White, !IO),
        test1(0, 0, 0, !IO),
        test2(!IO)
    ;
        Result = failed,
        textout_centre_ex(Screen, Font,
            "No mouse detected, but you need one!",
            ScreenW/2, ScreenH/2, Black, White, !IO),
        readkey(_, !IO)
    ;
        Result = already_installed,
        textout_centre_ex(Screen, Font, "Eh?!",
            ScreenW/2, ScreenH/2, Black, White, !IO),
        readkey(_, !IO)
    ).

:- pred black_white(color::out, color::out, io::di, io::uo) is det.

black_white(Black, White, !IO) :-
    makecol(0, 0, 0, Black, !IO),
    makecol(255, 255, 255, White, !IO).

%-----------------------------------------------------------------------------%

:- pred test1(int::in, int::in, int::in, io::di, io::uo) is det.

test1(C0, MickeyX0, MickeyY0, !IO) :-
    det_screen(Screen, !IO),
    font(Font, !IO),
    black_white(Black, White, !IO),

    acquire_screen(!IO),

    % the mouse position is stored in the variables mouse_x and mouse_y
    mouse_xy(MouseX, MouseY, !IO),
    textout_ex(Screen, Font, format("mouse_x = %-5d", [i(MouseX)]),
        16, 48, Black, White, !IO),
    textout_ex(Screen, Font, format("mouse_y = %-5d", [i(MouseY)]),
        16, 64, Black, White, !IO),

    % or you can use this function to measure the speed of movement.
    % Note that we only call it every fourth time round the loop:
    % there's no need for that other than to slow the numbers down
    % a bit so that you will have time to read them...
    C = C0 + 1,
    (if (C /\ 3) = 0 then
        get_mouse_mickeys(MickeyX, MickeyY, !IO)
    else
        MickeyX = MickeyX0,
        MickeyY = MickeyY0
    ),
    textout_ex(Screen, Font, format("mickey_x = %-7d", [i(MickeyX)]),
        16, 88, Black, White, !IO),
    textout_ex(Screen, Font, format("mickey_y = %-7d", [i(MickeyY)]),
        16, 104, Black, White, !IO),

    % the mouse button state is stored in the variable mouse_b
    mouse_b(MouseB, !IO),
    LMB = (if (MouseB /\ 1) = 0
            then "left button not pressed"
            else "left button is pressed "),
    textout_ex(Screen, Font, LMB, 16, 128, Black, White, !IO),

    RMB = (if (MouseB /\ 2) = 0
            then "right button not pressed"
            else "right button is pressed "),
    textout_ex(Screen, Font, RMB, 16, 144, Black, White, !IO),

    MMB = (if (MouseB /\ 4) = 0
            then "middle button not pressed"
            else "middle button is pressed "),
    textout_ex(Screen, Font, MMB, 16, 160, Black, White, !IO),

    % the wheel position is stored in the variable mouse_z
    mouse_z(MouseZ, !IO),
    textout_ex(Screen, Font, format("mouse_z = %-5d", [i(MouseZ)]),
        16, 184, Black, White, !IO),

    release_screen(!IO),
    vsync(!IO),

    keypressed(KP, !IO),
    (
        KP = yes,
        clear_keybuf(!IO)
    ;
        KP = no,
        test1(C, MickeyX, MickeyY, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred test2(io::di, io::uo) is det.

test2(!IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    font(Font, !IO),
    black_white(Black, White, !IO),
    clear_to_color(Screen, White, !IO),
    textout_centre_ex(Screen, Font, "Press a key to change cursor",
        ScreenW/2, ScreenH/2, Black, White, !IO),
    show_mouse(Screen, !IO),
    readkey(_, !IO),
    hide_mouse(!IO),
    
    create_custom_cursor(CustomCursor, !IO),
    set_mouse_sprite(CustomCursor, !IO),
    set_mouse_sprite_focus(16, 16, !IO),
    clear_to_color(Screen, White, !IO),
    textout_centre_ex(Screen, Font, "Press a key to quit",
        ScreenW/2, ScreenH/2, Black, White, !IO),
    show_mouse(Screen, !IO),
    readkey(_, !IO),
    hide_mouse(!IO),
    destroy_bitmap(CustomCursor, !IO).

:- pred create_custom_cursor(bitmap::out, io::di, io::uo) is det.
:- pred create_custom_cursor_2(bitmap::in, int::in, io::di, io::uo) is det.

create_custom_cursor(CustomCursor, !IO) :-
    det_create_bitmap(32, 32, CustomCursor, !IO),
    bitmap_mask_color(CustomCursor, Mask, !IO),
    clear_to_color(CustomCursor, Mask, !IO),
    list.foldl(create_custom_cursor_2(CustomCursor), 0 `..` 7, !IO).

create_custom_cursor_2(CustomCursor, C, !IO) :-
    palette_color(C, Color, !IO),
    circle(CustomCursor, 16, 16, C*2, Color, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
