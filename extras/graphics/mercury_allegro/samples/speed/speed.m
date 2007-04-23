%-----------------------------------------------------------------------------%
%
%   SPEED: Simultaneous Projections Employing an Ensemble of Displays.
%
%   Or alternatively: Stupid Pointless Effort at Establishing a Dumb Acronym.
%
%   By Shawn Hargreaves, November 1999.
%
% Partial port to Mercury by Peter Wang.
% Please see http://www.talula.demon.co.uk/speed/index.html
% for the original version.  In particular it has synthesised music
% which is not yet in this port.
%
%-----------------------------------------------------------------------------%

:- module speed.
:- interface.

:- import_module io.

:- import_module badguys.
:- import_module bullet.
:- import_module explode.
:- import_module message.
:- import_module player.
:- import_module sound.

%-----------------------------------------------------------------------------%

:- type game
    --->    game(
                player      :: player,
                badguys     :: badguys,
                bullets     :: bullets,
                explosions  :: explosions,
                sounds      :: sounds,
                messages    :: messages
            ).

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lcg.
:- import_module lcg.io.
:- import_module title.
:- import_module view.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.color_format.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.palette.
:- import_module allegro.sound.
:- import_module allegro.timer.
:- import_module allegro.transparency.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module time.

%-----------------------------------------------------------------------------%

:- func width = int.
:- func height = int.
:- func bpp = int.

width = 640.
height = 480.
bpp = 8.

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        set_color_depth(bpp, !IO),
        set_gfx_mode(gfx_autodetect_windowed, width, height, 0, 0, GfxOk, !IO),
        (
            GfxOk = yes,
            install_timer(_TimerOk, !IO),
            install_keyboard(_KeyboardOk, !IO),
            install_sound(digi_autodetect, midi_none, _SoundOk, !IO),
            sound.generate_samples(!IO),
            (if
                bpp = 8
            then
                generate_332_palette(Pal),
                set_palette_entry(Pal, 0, 0,0,0, !IO),
                set_palette(Pal, !IO),
                create_rgb_table(RgbMap, Pal, !IO),
                set_rgb_map(RgbMap, !IO),
%                 create_color_table(ColorMap, Pal, add_blender8, !IO),
                set_add_blender(0, 0, 0, 255, !IO),
                create_blender_table(ColorMap, Pal, !IO),
                set_color_map(ColorMap, !IO)
            else
                error("only 8-bit colour supported at the moment")
                % set_blender_mode(add_blender15, add_blender16, add_blender14, 0, 0, 0, 0, !IO),
            ),
            title_loop(!IO),
            sound.destroy_samples(!IO)
        ;
            GfxOk = no
        )
    ;
        Ok = no
    ).

:- pred title_loop(io::di, io::uo) is det.

title_loop(!IO) :-
    title_screen(WhatToDo, !IO),
    (
        WhatToDo = play_game,
        play_game(0, !IO),
        title_loop(!IO)
    ;
        WhatToDo = quit
    ).

:- pred play_game(cycle_num::in, io::di, io::uo) is det.

:- type cycle_num == int.

play_game(CycleNum, !IO) :-
    det_screen(_, ScreenW, ScreenH, !IO),
    create_bitmap(ScreenW, ScreenH, MaybeBitmap, !IO),
    (
        MaybeBitmap = yes(Bitmap),
        TimerSpeed = bps_to_timer(30*(CycleNum+2)),
        install_int_ex(TimerSpeed, MaybeCounter, !IO),
        (
            MaybeCounter = yes(Counter),
            game_loop(Bitmap, Counter, !IO),
            remove_int(Counter, !IO)
        ;
            MaybeCounter = no
        ),
        destroy_bitmap(Bitmap, !IO)
    ;
        MaybeBitmap = no
    ).

:- pred game_loop(bitmap::in, ticker::in, io::di, io::uo) is det.

game_loop(Bitmap, Counter, !IO) :-
    time.clock(Time, !IO),
    init_badguys(0, Badguys, lcg.init(Time), _RS),
    Sounds0 = init_sounds,
    sfx_ping(2, Sounds0, Sounds1),
    play_queued_sfx(Sounds1, Sounds, !IO),
    Game0 = game(init_player, Badguys, init_bullets, init_explosions,
        Sounds, init_messages),
    game_loop_2(Bitmap, Counter, Game0, Game, init_view, _View, !IO),
    stop_sounds(Game ^ sounds, !IO).

:- pred game_loop_2(bitmap::in, ticker::in, game::in, game::out,
        view::in, view::out, io::di, io::uo) is det.

game_loop_2(Bitmap, Counter, !Game, !View, !IO) :-
    do_updates(Counter, !Game, !View, Quit, 0, N, !IO),
    (
        Quit = yes
    ;
        Quit = no,
        draw_view(Bitmap, !.Game, !.View, !IO),
        (if N = 0 then
            rest(0, !IO)
        else
            true
        ),
        game_loop_2(Bitmap, Counter, !Game, !View, !IO)
    ).

:- pred do_updates(ticker::in, game::in, game::out, view::in, view::out,
        quit::out, int::in, int::out, io::di, io::uo) is det.
:- type quit == bool.

do_updates(Counter, !Game, !View, Quit, !N, !IO) :-
    get_ticker(Counter, Count, !IO), 
    (if
        Count > 0
    then
        get_input(Input, !IO),
        % debugging
        key(key_v, V, !IO),
        key(key_b, B, !IO),
        ( V = yes ->
            advance_view(!View, _Cycled),
            wait_key_released(key_v, !IO)
        ; 
            true
        ),
        ( B = yes ->
            BG0 = !.Game ^ badguys,
            kill_all_badguys(BG0, BG),
            !:Game = !.Game ^ badguys := BG,
            wait_key_released(key_b, !IO)
        ;
            true
        ),
        % end debugging
        (
            Input ^ escape = yes,
            Quit = yes
        ;
            Input ^ escape = no,

            update_view(!View),
            % XXX icky
            some [!Player, !Badguys, !Bullets, !Explosions, !Sounds, !Messages]
            (
                !.Game = game(!:Player, !:Badguys, !:Bullets, !:Explosions,
                    !:Sounds, !:Messages),

                update_bullets(!Bullets),
                update_explosions(!Explosions),
                update_messages(!Messages, !IO),

                lcg.io.get_supply(RS0, !IO),
                update_badguys(!Badguys, !Player, !Bullets, !Explosions,
                    !Sounds, !Messages, RS0, RS1, WaveComplete),
                (
                    WaveComplete = yes,
                    advance_view(!View, Cycled),
                    (
                        Cycled = yes
                        % cyclenum++;
                        % install_int_ex(inc_counter, TIMER_SPEED);
                    ;
                        Cycled = no
                    ),
                    lay_attack_wave(Cycled, !Badguys, RS1, RS),
                    advance_player(!Player, Cycled, !Sounds, !Messages)
                ;
                    WaveComplete = no,
                    RS = RS1
                ),
                lcg.io.set_supply(RS, !IO),
                update_player(Input, !Player, Result, !Bullets, !Sounds,
                    !Messages),
                play_queued_sfx(!Sounds, !IO),

                !:Game = game(!.Player, !.Badguys, !.Bullets, !.Explosions,
                    !.Sounds, !.Messages)
            ),
            (
                Result = dead,
                Quit = yes
            ;
                Result = continue,
                get_ticker(Counter, C, !IO),
                set_ticker(Counter, C-1, !IO),
                do_updates(Counter, !Game, !View, Quit, !.N+1, !:N, !IO)
            )
        )
    else
        Quit = no
    ).

:- pred get_input(input::out, io::di, io::uo) is det.

get_input(input(Esc, Left, Right, Space), !IO) :-
    key(key_esc, Esc, !IO),
    key(key_left, Left, !IO),
    key(key_right, Right, !IO),
    key(key_space, Space, !IO).

:- pred wait_key_released(int::in, io::di, io::uo) is det.

wait_key_released(Key, !IO) :-
    key(Key, Pressed, !IO),
    (
        Pressed = yes,
        wait_key_released(Key, !IO)
    ;
        Pressed = no
    ).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
