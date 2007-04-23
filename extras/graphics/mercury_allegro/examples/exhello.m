%-----------------------------------------------------------------------------%
% 
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This is a very simple program showing how to get into graphics
% mode and draw text onto the screen.
% 
%-----------------------------------------------------------------------------%

:- module exhello.
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
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.text.

:- import_module bool.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(SystemOk, !IO),
    (
        SystemOk = yes,
        install_keyboard(KeyboardOk, !IO),
        (
            KeyboardOk = yes,
            set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
            (
                GfxOk = yes,
                main_2(!IO)
            ;
                GfxOk = no,
                allegro_message("Unable to set 320x200 graphic mode.\n", !IO)
            )
        ;
            KeyboardOk = no,
            allegro_message("Error initialising keyboard.\n", !IO)
        )
    ;
        SystemOk = no,
        error("Error initialising Allegro.")
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    det_screen(Screen, W, H, !IO),
    set_palette(desktop_palette, !IO),
    makecol(255, 255, 255, White, !IO),
    makecol(0, 0, 0, Black, !IO),
    clear_to_color(Screen, White, !IO),
    font(Font, !IO),
    textout_centre_ex(Screen, Font, "Hello, world!", W/2, H/2, Black, -1, !IO),
    readkey(_K, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
