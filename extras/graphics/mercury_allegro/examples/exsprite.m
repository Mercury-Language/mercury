%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Grzegorz Ludorowski.
% Mercury port by Peter Wang.
%
% This example demonstrates how to use datafiles, various sprite
% drawing routines and flicker-free animation.
%
% Note: this requires a copy of running.dat in which the object names have
% not been stripped.
%
%-----------------------------------------------------------------------------%

:- module exsprite.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.color.
:- import_module allegro.datafile.
:- import_module allegro.digi.
:- import_module allegro.fixed.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.sound.
:- import_module allegro.text.
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.
:- import_module float.
:- import_module math.
:- import_module maybe.
:- import_module unit.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(KeyboardOk, !IO),
        install_sound(digi_autodetect, midi_none, _SoundOk, !IO),
        install_timer(TimerOk, !IO),
        install_int_ex(bps_to_timer(30), MaybeTicker, !IO),
        set_gfx_mode(gfx_autodetect, 640, 480, 0, 0, GfxOk, !IO),
        (if
            KeyboardOk = yes,
            TimerOk = yes,
            MaybeTicker = yes(Ticker),
            GfxOk = yes
        then
            main_2(Ticker, !IO)
        else
            set_text_mode(!IO),
            allegro_message("Error initialising something\n", !IO)
        )
    ;
        Ok = no
    ).

:- pred main_2(ticker::in, io::di, io::uo) is det.

main_2(Ticker, !IO) :-
    load_datafile("running.dat", MaybeRunningData, !IO),
    (
        MaybeRunningData = yes(RunningData),
        main_3(Ticker, RunningData, !IO),
        unload_datafile(RunningData, !IO)
    ;
        MaybeRunningData = no,
        io.write_string("Could not load running.dat. You need to\n", !IO),
        io.write_string("copy it from the Allegro examples directory.\n", !IO)
    ).

:- pred main_3(ticker::in, outer_datafile::in, io::di, io::uo) is det.

main_3(Ticker, RunningData, !IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    font(Font, !IO),
    text_height(Font, TextHeight, !IO),

    % Select the palette which was loaded from the datafile.
    (
        find_datafile_object_dat(RunningData, "PALETTE_001", dat_palette,
            Palette)
    ->
        set_palette(Palette, !IO)
    ;
        true
    ),
    palette_color(1, Color1, !IO),
    palette_color(15, Color15, !IO),

    % Create and clear a bitmap for sprite buffering, big
    % enough to hold the diagonal(sqrt(2)) when rotating.
    SpriteBufferW = floor_to_int(82.0 * sqrt(2.0)) + 2, 
    SpriteBufferH = floor_to_int(82.0 * sqrt(2.0)) + 2,
    det_create_bitmap(SpriteBufferW, SpriteBufferH, SpriteBuffer, !IO),
    clear_bitmap(SpriteBuffer, !IO),

    X = (SpriteBufferW - 82) / 2,
    Y = (SpriteBufferH - 82) / 2,
    TextY = ScreenH - 10 - TextHeight,

    some [!Frame, !FrameNumber] (
        !:FrameNumber = 0,
    
        % Write current sprite drawing method.
        textout_centre_ex(Screen, Font, "Press a key for next part...",
            ScreenW/2, 10, Color1, -1, !IO),
        textout_centre_ex(Screen, Font, "Using draw_sprite",
            ScreenW/2, TextY, Color15, -1, !IO),
        get_ticker(Ticker, !:Frame, !IO),
        loop(use_draw_sprite, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, !IO),

        clear_keybuf(!IO),
        rectfill(Screen, 0, TextY, ScreenW, ScreenH, 0, !IO),
        textout_centre_ex(Screen, Font, "Using draw_sprite_h_flip",
            ScreenW/2, TextY, Color15, -1, !IO),
        loop(use_draw_sprite_h_flip, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, !IO),

        clear_keybuf(!IO),
        rectfill(Screen, 0, TextY, ScreenW, ScreenH, 0, !IO),
        textout_centre_ex(Screen, Font, "Using draw_sprite_v_flip",
            ScreenW/2, TextY, Color15, -1, !IO),
        loop(use_draw_sprite_v_flip, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, !IO),

        clear_keybuf(!IO),
        rectfill(Screen, 0, TextY, ScreenW, ScreenH, 0, !IO),
        textout_centre_ex(Screen, Font, "Using draw_sprite_vh_flip",
            ScreenW/2, TextY, Color15, -1, !IO),
        loop(use_draw_sprite_vh_flip, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, !IO),

        clear_keybuf(!IO),
        rectfill(Screen, 0, TextY, ScreenW, ScreenH, 0, !IO),
        textout_centre_ex(Screen, Font, "Now with rotating - pivot_sprite",
            ScreenW/2, TextY, Color15, -1, !IO),
        loop(use_pivot_sprite, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, 0, _Angle, !IO),

        clear_keybuf(!IO),
        rectfill(Screen, 0, TextY, ScreenW, ScreenH, 0, !IO),
        textout_centre_ex(Screen, Font,
            "Now with rotating - pivot_sprite_v_flip",
            ScreenW/2, TextY, Color15, -1, !IO),
        exsprite.loop(use_pivot_sprite_v_flip, Ticker, RunningData,
            SpriteBuffer, X, Y,
            !.Frame, _Frame, !.FrameNumber, _FrameNumber,
            0, _Angle2, !IO)
    ),

    destroy_bitmap(SpriteBuffer, !IO).

:- pred loop(pred(bitmap, int, int, bitmap, io, io)::in(
            pred(in, in, in, in, di, uo) is det),
        ticker::in, outer_datafile::in, bitmap::in, int::in, int::in,
        int::in, int::out, int::in, int::out,
        io::di, io::uo) is det.

loop(P, Ticker, RunningData, SpriteBuffer, X, Y, !Frame, !FrameNumber, !IO) :-
    loop(wrap_P(P), Ticker, RunningData, SpriteBuffer, X, Y,
        !Frame, !FrameNumber, unit, _, !IO).

:- pred wrap_P(pred(bitmap, int, int, bitmap, io, io)::in(
            pred(in, in, in, in, di, uo) is det),
        bitmap::in, int::in, int::in, bitmap::in, A::in, A::out,
        io::di, io::uo) is det.

wrap_P(P, SpriteBuffer, X, Y, FrameBitmap, A, A, !IO) :-
    P(SpriteBuffer, X, Y, FrameBitmap, !IO).

:- pred loop(pred(bitmap, int, int, bitmap, A, A, io, io)
        ::in(pred(in, in, in, in, in, out, di, uo) is det),
        ticker::in, outer_datafile::in, bitmap::in, int::in, int::in,
        int::in, int::out, int::in, int::out, A::in, A::out,
        io::di, io::uo) is det.

loop(P, Ticker, RunningData, SpriteBuffer, X, Y, !Frame, !FrameNumber, !Acc,
        !IO) :-
    get_datafile_object(RunningData, !.FrameNumber, FrameObject),
    (if
        dat_bitmap(FrameObject, FrameBitmap)
    then
        clear_bitmap(SpriteBuffer, !IO),
        P(SpriteBuffer, X, Y, FrameBitmap, !Acc, !IO),
        det_screen(Screen, ScreenW, ScreenH, !IO),
        bitmap_size(SpriteBuffer, SpriteBufferW, SpriteBufferH),
        CX = (ScreenW - SpriteBufferW)/2,
        CY = (ScreenH - SpriteBufferH)/2,
        blit(SpriteBuffer, Screen, 0, 0, CX, CY, SpriteBufferW, SpriteBufferH,
            !IO)
    else
        true
    ),
    !:Frame = !.Frame + 1,
    wait_for_animation_timer(Ticker, !.Frame, !IO),
    (if
        !.FrameNumber = 0,
        find_datafile_object_dat(RunningData, "SOUND_01", dat_sample, Sample) 
    then
        play_sample(Sample, 128, 128, 1000, no_loop, !IO)
    else
        true
    ),
    !:FrameNumber = (if !.FrameNumber = 9 then 0 else !.FrameNumber+1),
    keypressed(KP, !IO),
    (
        KP = yes
    ;
        KP = no,
        loop(P, Ticker, RunningData, SpriteBuffer, X, Y,
            !Frame, !FrameNumber, !Acc, !IO)
    ).

:- pred wait_for_animation_timer(ticker::in, int::in, io::di, io::uo) is det.

wait_for_animation_timer(Ticker, Frame, !IO) :-
    get_ticker(Ticker, Ticks, !IO),
    (if Frame > Ticks then
        rest(1, !IO),
        wait_for_animation_timer(Ticker, Frame, !IO)
    else
        true
    ).

:- pred use_draw_sprite(bitmap::in, int::in, int::in, bitmap::in,
        io::di, io::uo) is det.

use_draw_sprite(SpriteBuffer, X, Y, FrameBitmap, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    hline(SpriteBuffer, 0, Y+82, bitmap_w(SpriteBuffer)-1, Color, !IO),
    draw_sprite(SpriteBuffer, FrameBitmap, X, Y, !IO).

:- pred use_draw_sprite_h_flip(bitmap::in, int::in, int::in, bitmap::in,
        io::di, io::uo) is det.

use_draw_sprite_h_flip(SpriteBuffer, X, Y, FrameBitmap, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    hline(SpriteBuffer, 0, Y+82, bitmap_w(SpriteBuffer)-1, Color, !IO),
    draw_sprite_h_flip(SpriteBuffer, FrameBitmap, X, Y, !IO).

:- pred use_draw_sprite_v_flip(bitmap::in, int::in, int::in, bitmap::in,
        io::di, io::uo) is det.

use_draw_sprite_v_flip(SpriteBuffer, X, Y, FrameBitmap, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    hline(SpriteBuffer, 0, Y-1, bitmap_w(SpriteBuffer)-1, Color, !IO),
    draw_sprite_v_flip(SpriteBuffer, FrameBitmap, X, Y, !IO).

:- pred use_draw_sprite_vh_flip(bitmap::in, int::in, int::in, bitmap::in,
        io::di, io::uo) is det.

use_draw_sprite_vh_flip(SpriteBuffer, X, Y, FrameBitmap, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    hline(SpriteBuffer, 0, Y-1, bitmap_w(SpriteBuffer)-1, Color, !IO),
    draw_sprite_vh_flip(SpriteBuffer, FrameBitmap, X, Y, !IO).

:- pred use_pivot_sprite(bitmap::in, int::in, int::in, bitmap::in,
        int::in, int::out, io::di, io::uo) is det.

use_pivot_sprite(SpriteBuffer, X, Y, FrameBitmap, Angle, Angle-4, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    circle(SpriteBuffer, X+41, Y+41, 47, Color, !IO),
    pivot_sprite(SpriteBuffer, FrameBitmap,
        bitmap_w(SpriteBuffer)/2, bitmap_h(SpriteBuffer)/2, 41, 41,
        itofix(Angle), !IO).

:- pred use_pivot_sprite_v_flip(bitmap::in, int::in, int::in, bitmap::in,
        int::in, int::out, io::di, io::uo) is det.

use_pivot_sprite_v_flip(SpriteBuffer, X, Y, FrameBitmap, Angle, Angle+4, !IO) :-
    makecol(0, 80, 0, Color, !IO),
    circle(SpriteBuffer, X+41, Y+41, 47, Color, !IO),
    pivot_sprite_v_flip(SpriteBuffer, FrameBitmap,
        bitmap_w(SpriteBuffer)/2, bitmap_h(SpriteBuffer)/2, 41, 41,
        itofix(Angle), !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
