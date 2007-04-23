%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This program demonstrates how to use hardware scrolling.
% The scrolling should work on anything that supports virtual
% screens larger than the physical screen.
%
%-----------------------------------------------------------------------------%

:- module exscroll.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.palette.
:- import_module allegro.prim.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module random.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(_, !IO),
    install_keyboard(KeyboardOk, !IO),
    (
        KeyboardOk = yes,
        set_gfx_mode(gfx_autodetect, 320, 240, 640, 240, GfxOk, !IO),
        (
            GfxOk = yes,
            main_1(!IO)
        ;
            GfxOk = no,
            allegro_message(
                "Unable to set a 320x240 mode with 640x240 " ++
                "virtual dimensions\n", !IO)
        )
    ;
        KeyboardOk = no
    ),
    allegro_exit(!IO).

:- pred main_1(io::di, io::uo) is det.

main_1(!IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    create_sub_bitmap(Screen, 0, 0, ScreenW*2, ScreenH, MaybeScroller, !IO),
    (
        MaybeScroller = yes(Scroller),

        set_palette(desktop_palette, !IO),
        set_color(0, 0, 0, 0, !IO),

        rectfill(Scroller, 0, 0, ScreenW, 100, 6, !IO),
        rectfill(Scroller, 0, 100, ScreenW, ScreenH, 2, !IO),

        random.init(1000, RS0),
        main_2(Scroller, 0, _X, 100, _H, RS0, _RS, !IO),

        destroy_bitmap(Scroller, !IO)
    ;
        MaybeScroller = no
    ).

:- pred main_2(bitmap::in, int::in, int::out, int::in, int::out,
        random.supply::mdi, random.supply::muo, io::di, io::uo) is det.

main_2(Scroller, X0, X, H0, H, !RS, !IO) :-
    det_screen(_Screen, ScreenW, ScreenH, !IO),

    % advance the scroller, wrapping every 320 pixels
    X1 = (X0 + 1) rem 320,

    % draw another column of the landscape
    acquire_bitmap(Scroller, !IO),
    vline(Scroller, X1+ScreenW-1, 0, H0, 6, !IO),
    vline(Scroller, X1+ScreenW-1, H0+1, ScreenH, 2, !IO),
    release_bitmap(Scroller, !IO),

    % scroll the screen
    scroll_screen(X1, 0, !IO),

    % duplicate the landscape column so we can wrap the scroller
    (if X1 > 0 then
        acquire_bitmap(Scroller, !IO),
        vline(Scroller, X0, 0, H0, 6, !IO),
        vline(Scroller, X0, H0+1, ScreenH, 2, !IO),
        release_bitmap(Scroller, !IO)
    else
        true
    ),

    % randomly alter the landscape position
    random(0, 2, R, !RS),
    (if R = 1 then
        H1 = (if H0 > 5 then H0-1 else H0)
    else
        H1 = (if H0 < 195 then H0+1 else H0)
    ),

    keypressed(KP, !IO),
    (
        KP = no,
        main_2(Scroller, X1, X, H1, H, !RS, !IO)
    ;
        KP = yes,
        X = X1,
        H = H1
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
