%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This program demonstrates how to use the timer routines.
% The first part of the example shows a basic use of
% timing using the blocking function rest(). The second part
% shows how to use three timers with different frequencies in
% a non blocking way.
%
% Note that timers in Mercury-Allegro differ substantially to the C API.
%
%-----------------------------------------------------------------------------%

:- module extimer.
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
:- import_module allegro.text.
:- import_module allegro.timer.
:- import_module allegro.color.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(KeyboardOk, !IO),
        install_timer(TimerOk, !IO),
        set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
        (if
            KeyboardOk = yes,
            TimerOk = yes,
            GfxOk = yes
        then
            main_2(!IO)
        else
            true
        ),
        allegro_exit(!IO)
    ;
        Ok = no
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    set_palette(desktop_palette, !IO),
    det_screen(Screen, ScreenW, _, !IO),
    font(Font, !IO),
    black_white(Black, White, !IO),
    clear_to_color(Screen, White, !IO),
    textout_centre_ex(Screen, Font, "extimer",
        ScreenW/2, 8, Black, White, !IO),

    textout_centre_ex(Screen, Font, "Timing five seconds",
        ScreenW/2, 48, Black, White, !IO),
    rest_five_seconds(0, !IO),

    textout_centre_ex(Screen, Font, "Press a key to set up interrupts",
        ScreenW/2, 142, Black, White, !IO),
    readkey(_Key, !IO),
    install_int(1000, MaybeX, !IO),
    install_int_ex(bps_to_timer(10), MaybeY, !IO),
    install_int_ex(secs_to_timer(10), MaybeZ, !IO),
    (if
        MaybeX = yes(X),
        MaybeY = yes(Y),
        MaybeZ = yes(Z)
    then
        interrupt_loop(X, Y, Z, !IO)
    else
        true
    ).

:- pred black_white(int::out, int::out, io::di, io::uo) is det.

black_white(Black, White, !IO) :-
    makecol(0, 0, 0, Black, !IO),
    makecol(255, 255, 255, White, !IO).

:- pred rest_five_seconds(int::in, io::di, io::uo) is det.

rest_five_seconds(N, !IO) :-
    (if N < 5 then
        det_screen(Screen, ScreenW, _, !IO),
        font(Font, !IO),
        black_white(Black, White, !IO),
        textout_centre_ex(Screen, Font, int_to_string(N+1),
            ScreenW/2, 62+N*10, Black, White, !IO),
        rest(1000, !IO),
        rest_five_seconds(N+1, !IO)
    else
        true
    ).

:- pred interrupt_loop(ticker::in, ticker::in, ticker::in, io::di, io::uo)
            is det.

interrupt_loop(X, Y, Z, !IO) :-
    keypressed(KP, !IO),
    (if KP = no then
        det_screen(Screen, ScreenW, _, !IO),
        font(Font, !IO),
        black_white(Black, White, !IO),
        get_ticker(X, XValue, !IO),
        get_ticker(Y, YValue, !IO),
        get_ticker(Z, ZValue, !IO),
        textout_centre_ex(Screen, Font,
            format("x=%d, y=%d, z=%d", [i(XValue), i(YValue), i(ZValue)]),
            ScreenW/2, 176, Black, White, !IO),
        interrupt_loop(X, Y, Z, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
