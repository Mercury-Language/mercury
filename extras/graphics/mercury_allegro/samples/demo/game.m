%-----------------------------------------------------------------------------%

:- module game.
:- interface.

:- import_module data.
:- import_module display.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred play_game(data::in, D::in, D::out, io::di, io::uo) is det
        <= display(D).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module aster.
:- import_module bullet.
:- import_module expl.
:- import_module star2d.
:- import_module util.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.digi.
:- import_module allegro.fixed.
:- import_module allegro.keyboard.
:- import_module allegro.midi.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.rle.
:- import_module allegro.sound.
:- import_module allegro.text.
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module string.
:- import_module time.

%-----------------------------------------------------------------------------%

:- func max_speed = int.
max_speed = 32.

:- func speed_shift = int.
speed_shift = 3.

:- func bullet_delay = int.
bullet_delay = 20.

:- func pan(int, int) = int.
pan(X, ScreenW) = X * 256 / ScreenW.

%-----------------------------------------------------------------------------%

:- type game
    --->    game(
                score       :: int,
                lives       :: int,
                ship        :: ship,
                asteroids   :: asteroids,
                bullets     :: bullets,
                starfield   :: starfield_2d,
                score_pos   :: int,
                new_asteroid_time :: int
            ).

:- type ship
    --->    ship(
                x_pos       :: int,
                x_speed     :: int,
                y_speed     :: int,
                y_counter   :: int,
                ship_state  :: ship_state,
                prev_bullet_time :: int
            ).

:- type ship_state
    --->    normal(burn)
    ;       exploding(int)
    ;       dead.

:- type burn
    --->    burn_on
    ;       burn_off.

:- type game_loop
    --->    game_loop(
                prev_update_time :: int
            ).

%-----------------------------------------------------------------------------%

play_game(Data, !Display, !IO) :-
    stop_midi(!IO),
    ready_steady_go(Data, !IO),
    det_screen(_, ScreenW, ScreenH, !IO),
    clear_display(!Display, !IO),
    clear_keybuf(!IO),
    set_palette(Data ^ game_pal, !IO),
    install_int(6400/ScreenW, MaybeGameTimer, !IO),
    (
        MaybeGameTimer = yes(GameTimer),
        set_volume(-1, 90, !IO),
        some [!RS] (
            clock(Time, !IO),
            random.init(Time, !:RS),
            init_game(ScreenW, ScreenH, Game, !RS),
            get_ticker(GameTimer, StartTime, !IO),
            GameLoop = init_game_loop(StartTime),
            run_game_loop(Data, GameTimer, GameLoop, Game, !Display,
                !.RS, _RS, !IO)
        ),
        set_volume(-1, 255, !IO),
        remove_int(GameTimer, !IO),
        stop_sample(Data ^ engine_spl, !IO),
        clear_keybuf(!IO),
        fade_out(5, !IO)
    ;
        MaybeGameTimer = no
    ).

%-----------------------------------------------------------------------------%

:- pred init_game(int::in, int::in, game::out, rs::mdi, rs::muo) is det.

init_game(ScreenW, ScreenH, Game, !RS) :-
    Game = game(0, 3, Ship, Asteroids, Bullets, Starfield, ScorePos,
        NewAsteroidTime),
    Ship = init_ship(ScreenW),
    init_asteroids(ScreenW, ScreenH, Asteroids, !RS),
    Bullets = init_bullets,
    init_starfield_2d(ScreenW, ScreenH, Starfield, !RS),
    ScorePos = 0,
    NewAsteroidTime = 0.

:- func init_ship(int) = ship.

init_ship(ScreenW) = ship(XPos, 0, 0, 0, normal(burn_off), 0) :-
    XPos = (ScreenW/2) << speed_shift.

:- func init_game_loop(int) = game_loop.

init_game_loop(StartTime) = game_loop(StartTime).

%-----------------------------------------------------------------------------%

:- pred run_game_loop(data::in, ticker::in, game_loop::in, game::in,
            D::in, D::out, rs::mdi, rs::muo, io::di, io::uo) is det
            <= display(D).

run_game_loop(Data, GameTimer, GameLoop0, Game0, !Display, !RS, !IO) :-
    catchup_updates(Data, GameTimer, GameLoop0, GameLoop, Game0, Game,
        no, NeedRedraw, !RS, !IO),
    (
        NeedRedraw = yes,
        prepare_display(Bitmap, !Display, !IO),
        draw_screen(Data, Bitmap, Game0, !IO),
        flip_display(!Display, !IO)
    ;
        NeedRedraw = no,
        rest(1, !IO)
    ),
    key(key_esc, KeyEsc, !IO),
    (
        KeyEsc = yes
    ;
        KeyEsc = no,
        run_game_loop(Data, GameTimer, GameLoop, Game, !Display, !RS, !IO)
    ).

:- pred catchup_updates(data::in, ticker::in, game_loop::in, game_loop::out,
        game::in, game::out, bool::in, bool::out,
        rs::mdi, rs::muo, io::di, io::uo) is det.

catchup_updates(Data, GameTimer, GameLoop0, GameLoop, Game0, Game, 
                NeedRedraw0, NeedRedraw, !RS, !IO) :-
    GameLoop0 = game_loop(PrevUpdateTime),
    get_ticker(GameTimer, GameTime, !IO),
    (if PrevUpdateTime < GameTime then
        (if Game0 ^ lives = 0 then
            move_score(Game0, Game1, !IO)
        else
            move_everyone(Data, GameTimer, Game0, Game1, !RS, !IO)
        ),
        catchup_updates(Data, GameTimer, game_loop(PrevUpdateTime+1), GameLoop,
            Game1, Game, yes, NeedRedraw, !RS, !IO)
    else
        GameLoop = GameLoop0,
        Game = Game0,
        NeedRedraw = NeedRedraw0
    ).

:- pred move_everyone(data::in, ticker::in, game::in, game::out,
        rs::mdi, rs::muo, io::di, io::uo) is det.

move_everyone(Data, GameTimer,
        game(Score0, Lives0, Ship0, Asteroids0, Bullets0, Starfield0,
                ScorePos, NewAsteroidTime0),
        game(Score, Lives, Ship, Asteroids, Bullets, Starfield,
                ScorePos, NewAsteroidTime),
        !RS, !IO) :-
    get_ticker(GameTimer, GameTime, !IO),
    move_everyone_2(Data, GameTime, Score0, Score, Lives0, Lives,
        Ship0, Ship, Asteroids0, Asteroids, Bullets0, Bullets,
        Starfield0, Starfield, NewAsteroidTime0, NewAsteroidTime, !RS, !IO).

:- pred move_everyone_2(data::in, int::in,
        int::in, int::out, int::in, int::out,
        ship::in, ship::out, asteroids::in, asteroids::out,
        bullets::in, bullets::out, starfield_2d::in, starfield_2d::out,
        int::in, int::out, rs::mdi, rs::muo, io::di, io::uo) is det.

move_everyone_2(Data, GameTime, Score0, Score, Lives0, Lives,
        !Ship, !Asteroids, !Bullets, !Starfield,
        NewAsteroidTime0, NewAsteroidTime, !RS, !IO) :-
    det_screen(_, ScreenW, ScreenH, !IO),

    move_ship(!Ship, Score0, Score, Lives0, Lives, Data, !IO),

    % if going fast, move everyone else down to compensate
    (if !.Ship ^ y_speed > 0 then
        YCounter0 = !.Ship^y_counter + !.Ship^y_speed,
        scroll_everyone(YCounter0, YCounter, !Bullets, !Asteroids, !Starfield),
        !:Ship = (!.Ship ^ y_counter := YCounter)
    else
        true
    ),

    move_bullets(!Bullets, !Asteroids,
        play_boom(Data ^ boom_spl, ScreenW), !IO),

    move_starfield_2d(!Starfield),

    % fire bullet?
    fire_key_pressed(Fire, !IO),
    (if normal_ship(!.Ship),
        Fire = yes,
        !.Ship ^ prev_bullet_time + bullet_delay < GameTime
    then
        BulletX = (!.Ship ^ x_pos >> speed_shift) - 2,
        BulletY = ScreenH - 64,
        add_bullet(BulletX, BulletY, !Bullets),
        play_sample(Data ^ shoot_spl, 100, pan(BulletX, ScreenW), 1000,
            no_loop, !IO),
        !:Ship = (!.Ship ^ prev_bullet_time := GameTime)
    else
        true
    ),

    (if normal_ship(!.Ship) then
        X = (!.Ship ^ x_pos >> speed_shift),
        Y = ScreenH - 42,
        move_asteroids(!Asteroids, yes({X, Y}), Collision, !RS),
        (
            Collision = collision,
            !:Ship = (!.Ship ^ ship_state := exploding(0)),
            play_sample(Data ^ death_spl, 255, pan(X, ScreenW), 1000,
                no_loop, !IO)
        ;
            Collision = no_collision
        )
    else
        move_asteroids(!Asteroids, no, _Collision, !RS)
    ),

    % make a new asteroid?
    (if NewAsteroidTime0 > 600 then
        NewAsteroidTime = 0,
        add_asteroid(!Asteroids, !RS)
    else
        NewAsteroidTime = NewAsteroidTime0 + 1
    ).

:- pred move_ship(ship::in, ship::out, int::in, int::out, int::in, int::out,
        data::in, io::di, io::uo) is det.

move_ship(Ship0, Ship, Score0, Score, Lives, Lives, Data, !IO) :-
    Ship0 = ship(XPos0, XSpeed0, YSpeed0, YCounter, normal(_), PrevBulletTime),

    screen_w(ScreenW, !IO),
    key(key_left, KeyLeft, !IO),
    key(key_right, KeyRight, !IO),
    key(key_up, KeyUp, !IO),

    % moving left and right
    XSpeed1 = ( KeyLeft  = yes -> max(-max_speed, XSpeed0-2)
              ; KeyRight = yes -> min( max_speed, XSpeed0+2)
              ; XSpeed0 > 0    -> XSpeed0-2
              ; XSpeed0 < 0    -> XSpeed0+2
              ;                   XSpeed0
              ),
    XPos1 = XPos0 + XSpeed1,
    (if XPos1 < (32 << speed_shift) then
        XPos = (32 << speed_shift),
        XSpeed = 0
    else if XPos1 >= ((ScreenW - 32) << speed_shift) then
        XPos = (ScreenW - 32) << speed_shift,
        XSpeed = 0
    else
        XPos = XPos1,
        XSpeed = XSpeed1
    ),

    EngineSpl = Data ^ engine_spl,
    Pan = pan(XPos >> speed_shift, ScreenW),
    (
        % firing thrusters
        KeyUp = yes,
        YSpeed = min(YSpeed0+1, max_speed),
        State = normal(burn_on),
        Score = Score0 + 2,
        (if YSpeed0 = 0 then
            play_sample(EngineSpl, 0, Pan, 1000, loop, !IO)
        else if YSpeed0 = max_speed then
            % adjust pan while the sample is looping
            adjust_sample(EngineSpl, 64, Pan, 1000, loop, !IO)
        else
            % fade in sample while speeding up
            adjust_sample(EngineSpl, YSpeed0 * 64 / max_speed, Pan,
                1000, loop, !IO)
        )
    ;
        % not firing thrusters
        KeyUp = no,
        YSpeed = max(YSpeed0-1, 0),
        State = normal(burn_off),
        Score = Score0 + 1,
        (if YSpeed = 0 then
            stop_sample(EngineSpl, !IO)
        else
            adjust_sample(EngineSpl, YSpeed * 64 / max_speed, Pan,
                500 + YSpeed * 500 / max_speed, loop, !IO)
        )
    ),
    Ship = ship(XPos, XSpeed, YSpeed, YCounter, State, PrevBulletTime).

move_ship(Ship0, Ship, Score, Score, Lives0, Lives, _Data, !IO) :-
    Ship0 ^ ship_state = exploding(Frame),
    (if Frame+1 >= explode_frames then
        Lives = Lives0 - 1,
        State = (if Lives = 0 then dead else normal(burn_off))
    else
        State = exploding(Frame+1),
        Lives = Lives0
    ),
    Ship = (Ship0 ^ ship_state := State).

move_ship(Ship, Ship, Score, Score, Lives, Lives, _Data, !IO) :-
    Ship ^ ship_state = dead.

:- pred scroll_everyone(int::in, int::out, bullets::in, bullets::out,
        asteroids::in, asteroids::out, starfield_2d::in, starfield_2d::out)
        is det.

scroll_everyone(YCounter0, YCounter, !Bullets, !Asteroids, !Starfield) :-
    Unit = 1 << speed_shift,
    (if YCounter0 >= Unit then
        scroll_bullets(!Bullets),
        scroll_asteroids(!Asteroids),
        scroll_stars(!Starfield),
        scroll_everyone(YCounter0 - Unit, YCounter,
            !Bullets, !Asteroids, !Starfield)
    else
        YCounter = YCounter0
    ).

:- pred normal_ship(ship::in) is semidet.

normal_ship(Ship) :-
    Ship ^ ship_state = normal(_).

:- pred fire_key_pressed(bool::out, io::di, io::uo) is det.

fire_key_pressed(Fire, !IO) :-
    key(key_space, KeySpace, !IO),
    key(key_lcontrol, KeyLControl, !IO),
    key(key_rcontrol, KeyRControl, !IO),
    (if ( KeySpace = yes
        ; KeyLControl = yes
        ; KeyRControl = yes
        )
    then
        Fire = yes
    else
        Fire = no
    ).

:- pred play_boom(sample::in, int::in, int::in, io::di, io::uo) is det.

play_boom(Sample, ScreenW, X, !IO) :-
    play_sample(Sample, 255, pan(X, ScreenW), 1000, no_loop, !IO).

:- pred move_score(game::in, game::out, io::di, io::uo) is det.

move_score(Game0, Game, !IO) :-
    screen_h(ScreenH, !IO),
    ScorePos0 = (Game0 ^ score_pos) + 1,
    ScorePos = (if ScorePos0 > ScreenH then -160 else ScorePos0),
    Game = (Game0 ^ score_pos := ScorePos).

%-----------------------------------------------------------------------------%

:- pred draw_screen(data::in, bitmap::in, game::in, io::di, io::uo) is det.

draw_screen(Data, Bitmap, Game, !IO) :-
    acquire_bitmap(Bitmap, !IO),
    draw_starfield_2d(Bitmap, Game^starfield, !IO),
    draw_ship(Data, Bitmap, Game^ship, !IO),
    draw_asteroids(Data, Bitmap, Game^asteroids, !IO),
    draw_bullets(Data, Bitmap, Game^bullets, !IO),
    font(Font, !IO),
    format("Lives: %d - Score: %d", [i(Game^lives), i(Game^score)], ScoreBuf),
    textout_ex(Bitmap, Font, ScoreBuf, 0, 0, 7, -1, !IO),
    ( Game^lives = 0 ->
        draw_score(Data, Bitmap, Game^score, Game^score_pos, !IO)
    ;
        true
    ),
    release_bitmap(Bitmap, !IO).

:- pred draw_ship(data::in, bitmap::in, ship::in, io::di, io::uo) is det.

draw_ship(Data, Bitmap,
        ship(XPos, XSpeed, _YSpeed, _YCounter, State, _PrevBulletTime), !IO) :-
    X = XPos >> speed_shift,
    screen_h(ScreenH, !IO),
    (if State = normal(burn_on) then
        retrace_count(RetraceCount, !IO),
        Frame = (RetraceCount / 4) mod 7,
        Engine = list.det_index0(Data ^ engine, Frame),
        EX = X - rle_sprite_w(Engine)/2,
        EY = ScreenH - 24,
        draw_rle_sprite(Bitmap, Engine, EX, EY, !IO)
    else
        true
    ),
    get_ship_sprite(Data, State, XSpeed) = MaybeSprite,
    (
        MaybeSprite = yes(Sprite),
        SX = X - rle_sprite_w(Sprite)/2,
        SY = ScreenH - 42 - rle_sprite_h(Sprite)/2,
        draw_rle_sprite(Bitmap, Sprite, SX, SY, !IO)
    ;
        MaybeSprite = no
    ).

:- func get_ship_sprite(data, ship_state, int) = maybe(rle_sprite).

get_ship_sprite(Data, normal(_), XSpeed) =
    yes(( XSpeed  < -24 -> Data ^ ship1
        ; XSpeed  <  -2 -> Data ^ ship2
        ; XSpeed =<   2 -> Data ^ ship3
        ; XSpeed =<  24 -> Data ^ ship4
        ;                  Data ^ ship5
        )).

get_ship_sprite(Data, exploding(Frame), _XSpeed) =
    yes(list.det_index0(Data ^ explosions, Frame)).

get_ship_sprite(_Data, dead, _XSpeed) = no.

:- pred draw_score(data::in, bitmap::in, int::in, int::in, io::di, io::uo)
        is det.

draw_score(Data, Bitmap, Score, ScorePos, !IO) :-
    det_create_bitmap(160, 160, B, !IO),
    det_create_bitmap(160, 160, B2, !IO),

    blit(Bitmap, B2, ScorePos, ScorePos, 0, 0, 160, 160, !IO),
    clear_bitmap(B, !IO),
    textout_centre_ex(B, Data ^ end_font, "GAME OVER", 80, 50, 2, 0, !IO),
    string.format("Score: %d", [i(Score)], ScoreBuf),
    textout_centre_ex(B, Data ^ end_font, ScoreBuf, 80, 82, 2, 0, !IO),
    Angle = raw_int_to_fixed((ScorePos << 16) / 4),
    rotate_sprite(B2, B, 0, 0, Angle, !IO),
    blit(B2, Bitmap, 0, 0, ScorePos, ScorePos, 160, 160, !IO),

    destroy_bitmap(B2, !IO),
    destroy_bitmap(B, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred ready_steady_go(data::in, io::di, io::uo) is det.

ready_steady_go(Data, !IO) :-
    draw_intro_item(Data ^ intro_bmp_1, 5, !IO),
    play_midi(Data ^ game_music, yes, !IO),
    clear_keybuf(!IO),
    some [!Abort] (
        fade_intro_item(Data ^ game_pal, -1, 2, no_abort, !:Abort, !IO),
        draw_intro_item(Data ^ intro_bmp_2, 4, !IO),
        fade_intro_item(Data ^ game_pal, 5, 4, !Abort, !IO),
        draw_intro_item(Data ^ intro_bmp_3, 3, !IO),
        fade_intro_item(Data ^ game_pal, 9, 4, !Abort, !IO),
        draw_intro_item(Data ^ intro_bmp_4, 3, !IO),
        fade_intro_item(Data ^ game_pal, 11, 4, !Abort, !IO),
        draw_intro_item(Data ^ go_bmp, 1, !IO),
        fade_intro_item(Data ^ game_pal, 13, 16, !Abort, !IO),
        fade_intro_item(Data ^ game_pal, 14, 16, !Abort, !IO),
        fade_intro_item(Data ^ game_pal, 15, 16, !Abort, !IO),
        fade_intro_item(Data ^ game_pal, 16, 16, !.Abort, _Abort, !IO)
    ).

:- pred draw_intro_item(bitmap::in, int::in, io::di, io::uo) is det.

draw_intro_item(Item, Size, !IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    W = min(ScreenW, ScreenW*2 / Size),
    H = min(ScreenH, ScreenH / Size),
    clear_bitmap(Screen, !IO),
    stretch_blit(Item, Screen, 0, 0, bitmap_w(Item), bitmap_h(Item),
        (ScreenW - W)/2, (ScreenH - H)/2, W, H, !IO).

:- type abort
    --->    abort
    ;       no_abort.

:- pred fade_intro_item(palette::in, int::in, int::in, abort::in, abort::out,
        io::di, io::uo) is det.

fade_intro_item(_GamePal, _MusicPos, _FadeSpeed, abort, abort, !IO).
fade_intro_item(GamePal, MusicPos, FadeSpeed, no_abort, Abort, !IO) :-
    fade_intro_item_2(MusicPos, Abort, !IO),
    (
        Abort = abort
    ;
        Abort = no_abort,
        set_palette(GamePal, !IO),
        fade_out(FadeSpeed, !IO)
    ).

:- pred fade_intro_item_2(int::in, abort::out, io::di, io::uo) is det.

fade_intro_item_2(MusicPos, Abort, !IO) :-
    keypressed(KP, !IO),
    (
        KP = yes,
        Abort = abort
    ;
        KP = no,
        midi_pos(MaybeMidiPos, !IO),
        (if
            MaybeMidiPos = yes(MidiPos),
            MidiPos < MusicPos
        then
            rest(1, !IO),
            fade_intro_item_2(MusicPos, Abort, !IO)
        else
            Abort = no_abort
        )
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
