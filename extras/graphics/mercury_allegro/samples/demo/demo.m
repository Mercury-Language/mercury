%-----------------------------------------------------------------------------%
%
% A fairly direct Mercury port of the Allegro demo game, by Peter Wang.
%
%-----------------------------------------------------------------------------%

:- module demo.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module data.
:- import_module display.
:- import_module expl.
:- import_module game.
:- import_module title.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.datafile.
:- import_module allegro.digi.
:- import_module allegro.flic.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.load_bitmap.
:- import_module allegro.palette.
:- import_module allegro.sound.
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(KeyboardOk, !IO),
        (
            KeyboardOk = yes,
            install_timer(TimerOk, !IO),
            (
                TimerOk = yes,
                maybe_install_sound(!IO),
                main_2(!IO),
                remove_sound(!IO),
                remove_timer(!IO)
            ;
                TimerOk = no
            ),
            remove_keyboard(!IO)
        ;
            KeyboardOk = no
        ),
        allegro_exit(!IO)
    ;
        Ok = no
    ).

:- pred maybe_install_sound(io::di, io::uo) is det.

maybe_install_sound(!IO) :-
    io.command_line_arguments(Args, !IO),
    (if list.member("-nosound", Args) then
        install_sound(digi_none, midi_none, _, !IO)
    else
        install_sound(digi_autodetect, midi_autodetect, SoundOk, !IO),
        (
            SoundOk = yes
        ;
            SoundOk = no,
            install_sound(digi_none, midi_none, _, !IO)
        )
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    set_color_conversion(colorconv_none, !IO),
    load_datafile("demo.dat", MaybeDatafile, !IO),
    (
        MaybeDatafile = yes(Datafile),
        generate_explosions(MaybeExplosions, !IO),
        (
            MaybeExplosions = yes(Explosions),
            ( init_data(Datafile, Explosions, Data) ->
                main_3(Data, !IO)
            ;
                true
            ),
            destroy_explosions(Explosions, !IO)
        ;
            MaybeExplosions = no
        ),
        unload_datafile(Datafile, !IO)
    ;
        MaybeDatafile = no,
        io.print("Couldn't load demo.dat.\n", !IO),
        io.print("Did you copy it here from the Allegro distribution?\n", !IO)
    ).

:- pred main_3(data::in, io::di, io::uo) is det.

main_3(Data, !IO) :-
    io.command_line_arguments(Args, !IO),
    (if list.member("-jumpstart", Args) then
        true
    else
        intro_screen(Data, !IO)
    ),
    ScreenW = 640,
    ScreenH = 480,
    set_gfx_mode(gfx_autodetect, ScreenW, ScreenH, 0, 0, GfxOk, !IO),
    (
        GfxOk = yes,
        init_display(ScreenW, ScreenH, double_buffer, MaybeDisplay, !IO),
        (
            MaybeDisplay = yes(Display0),
            title_loop(Data, init_color_scheme, Display0, Display, !IO),
            destroy_display(Display, !IO)
        ;
            MaybeDisplay = no
        ),
        set_gfx_mode(gfx_text, 0, 0, 0, 0, _, !IO)
    ;
        GfxOk = no,
        allegro_message("Unable to set 640x480 graphic mode.\n", !IO)
    ).

:- pred intro_screen(data::in, io::di, io::uo) is det.

intro_screen(Data, !IO) :-
    set_gfx_mode(gfx_autodetect, 320, 200, 0, 0, GfxOk, !IO),
    (
        GfxOk = yes,
        det_screen(Screen, ScreenW, ScreenH, !IO),
        create_sub_bitmap(Screen, ScreenW/2-160, ScreenH/2-100, 320, 200,
            MaybeBitmap, !IO),
        (
            MaybeBitmap = yes(Bitmap),
            play_sample(Data ^ intro_spl, 255, 128, 1000, no_loop, !IO),
            play_memory_fli(Data ^ intro_anim, Bitmap, _Success, !IO),
            rest(1000, !IO),
            fade_out(1, !IO),
            destroy_bitmap(Bitmap, !IO)
        ;
            MaybeBitmap = no
        )
    ;
        GfxOk = no
    ).

:- pred title_loop(data::in, color_scheme::in, D::in, D::out, io::di, io::uo)
    is det <= display(D).

title_loop(Data, ColorScheme, !Display, !IO) :-
    title_screen(Data, ColorScheme, WhatToDo, !Display, !IO),
    (
        WhatToDo = play_game,
        play_game(Data, !Display, !IO),
        title_loop(Data, next_color_scheme(ColorScheme), !Display, !IO)
    ;
        WhatToDo = quit
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
