%-----------------------------------------------------------------------------%

:- module badguys.
:- interface.

:- import_module bullet.
:- import_module explode.
:- import_module lcg.
:- import_module message.
:- import_module player.
:- import_module sound.
:- import_module view.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type badguys.
:- type wavenum == int.

:- pred init_badguys(wavenum::in, badguys::out, rs::in, rs::out) is det.
:- pred lay_attack_wave(bool::in, badguys::in, badguys::out, rs::in, rs::out)
        is det.
:- pred update_badguys(badguys::in, badguys::out, player::in, player::out,
        bullets::in, bullets::out, explosions::in, explosions::out,
        sounds::in, sounds::out, messages::in, messages::out, rs::in, rs::out,
        bool::out) is det.
:- pred kill_all_badguys(badguys::in, badguys::out) is det.
:- pred draw_badguys(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, badguys::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module util.

:- import_module allegro.color.
:- import_module allegro.prim.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module require.

%-----------------------------------------------------------------------------%

    % description of an attack wave
    %
:- type waveinfo
    --->    waveinfo(
                wi_count        :: int,     % how many to create
                wi_delay        :: float,   % how long to delay them
                wi_delay_rand   :: float,   % random delay factor
                wi_speed        :: float,   % how fast they move
                wi_speed_rand   :: float,   % speed random factor
                wi_move         :: float,   % movement speed
                wi_move_rand    :: float,   % movement random factor
                wi_sin_depth    :: float,   % depth of sine wave motion
                wi_sin_depth_rand
                                :: float,   % sine depth random factor
                wi_sin_speed    :: float,   % speed of sine wave motion
                wi_sin_speed_rand
                                :: float,   % sine speed random factor
                wi_split        :: bool,    % split into multiple dudes?
                wi_aggro        :: bool,    % attack the player?
                wi_evade        :: bool     % evade the player?
            ).

:- func wave1 = list(waveinfo).
:- func wave2 = list(waveinfo).
:- func wave3 = list(waveinfo).
:- func wave4 = list(waveinfo).
:- func wave5 = list(waveinfo).
:- func wave6 = list(waveinfo).

    % attack wave #1 (straight downwards)
wave1 = [
    %         c  del  rnd  speed   r    mv   r    dp   r    sd   r    sp   ag  ev
    waveinfo( 4, 0.0, 1.0, 0.0015, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, yes, no, no ),
    waveinfo( 2, 0.7, 0.0, 0.0055, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, no,  no, no ),
    waveinfo( 2, 0.5, 0.0, 0.0045, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, no,  no, no ),
    waveinfo( 4, 0.0, 1.0, 0.0035, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, no,  no, no ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, no,  no, no )
].

    % attack wave #2 (diagonal motion)
wave2 = [
    %         c  del  rnd  speed   r       mv      r      dp   r    sd   r    sp  ag  ev
    waveinfo( 6, 2.0, 1.0, 0.0025, 0.002,  0.0000, 0.020, 0.0, 0.0, 0.0, 0.0, no, no, no ),
    waveinfo( 6, 1.2, 1.0, 0.0025, 0.001,  0.0000, 0.010, 0.0, 0.0, 0.0, 0.0, no, no, no ),
    waveinfo( 6, 0.5, 1.0, 0.0025, 0.000,  0.0000, 0.005, 0.0, 0.0, 0.0, 0.0, no, no, no ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.000, -0.0025, 0.000, 0.0, 0.0, 0.0, 0.0, no, no, no ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.000,  0.0025, 0.000, 0.0, 0.0, 0.0, 0.0, no, no, no )
].

    % attack wave #3 (sine wave motion)
wave3 = [
    %         c  del  rnd  speed   r       mv   r    dp     r      sd     r     sp   ag  ev
    waveinfo( 4, 0.0, 2.0, 0.0020, 0.0000, 0.0, 0.0, 0.005, 0.000, 0.020, 0.00, yes, no, no ),
    waveinfo( 4, 1.5, 1.0, 0.0016, 0.0024, 0.0, 0.0, 0.002, 0.006, 0.010, 0.03, no,  no, no ),
    waveinfo( 4, 1.0, 1.0, 0.0019, 0.0016, 0.0, 0.0, 0.003, 0.004, 0.015, 0.02, no,  no, no ),
    waveinfo( 4, 0.5, 1.0, 0.0022, 0.0008, 0.0, 0.0, 0.004, 0.002, 0.020, 0.01, no,  no, no ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.0000, 0.0, 0.0, 0.005, 0.000, 0.025, 0.00, no,  no, no )
].

    % attack wave #4 (evade you)
wave4 = [
    %         c  del  rnd  speed   r       mv   r      dp   r    sd   r    sp   ag  ev
    waveinfo( 4, 0.0, 2.0, 0.0020, 0.0000, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, yes, no, yes ),
    waveinfo( 3, 1.5, 1.0, 0.0016, 0.0024, 0.0, 0.010, 0.0, 0.0, 0.0, 0.0, no,  no, yes ),
    waveinfo( 3, 1.0, 1.0, 0.0019, 0.0016, 0.0, 0.001, 0.0, 0.0, 0.0, 0.0, no,  no, yes ),
    waveinfo( 4, 0.5, 1.0, 0.0022, 0.0008, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, no,  no, yes ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.0000, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, no,  no, yes )
].



    % attack wave #5 (attack you)
wave5 = [
    %         c  del  rnd  speed   r       mv   r      dp   r    sd   r    sp   ag   ev
    waveinfo( 4, 0.0, 2.0, 0.0010, 0.0000, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, yes, no,  no ),
    waveinfo( 3, 1.5, 1.0, 0.0016, 0.0024, 0.0, 0.010, 0.0, 0.0, 0.0, 0.0, no,  no,  no ),
    waveinfo( 3, 1.0, 1.0, 0.0019, 0.0016, 0.0, 0.001, 0.0, 0.0, 0.0, 0.0, no,  yes, no ),
    waveinfo( 3, 0.5, 1.0, 0.0022, 0.0008, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, no,  yes, no ),
    waveinfo( 4, 0.0, 1.0, 0.0025, 0.0000, 0.0, 0.000, 0.0, 0.0, 0.0, 0.0, no,  yes, no )
].

    % attack wave #6 (the boss wave, muahaha)
wave6 = [
    %         c  del  rnd  speed  r      mv   r     dp   r      sd   r     sp   ag   ev
    waveinfo( 8, 6.0, 2.0, 0.002, 0.001, 0.0, 0.00, 0.0, 0.0,   0.0, 0.0,  yes, yes, no  ),
    waveinfo( 8, 4.5, 2.0, 0.002, 0.001, 0.0, 0.00, 0.0, 0.0,   0.0, 0.0,  yes, no,  yes ),
    waveinfo( 8, 3.0, 2.0, 0.002, 0.001, 0.0, 0.00, 0.0, 0.006, 0.0, 0.03, yes, no,  no  ),
    waveinfo( 8, 1.5, 2.0, 0.002, 0.001, 0.0, 0.01, 0.0, 0.0,   0.0, 0.0,  yes, no,  no  ),
    waveinfo( 8, 0.0, 2.0, 0.002, 0.001, 0.0, 0.00, 0.0, 0.0,   0.0, 0.0,  yes, no,  no  )
].

    % list of available attack waves
:- func waveinfo = list(list(waveinfo)).
waveinfo = [
    drop(4, wave1), drop(4, wave2), drop(4, wave3), drop(4, wave4), drop(4, wave5),
    drop(3, wave1), drop(3, wave2), drop(3, wave3), drop(3, wave4), drop(3, wave5),
    drop(2, wave1), drop(2, wave2), drop(2, wave3), drop(2, wave4), drop(2, wave5),
    drop(1, wave1), drop(1, wave2), drop(1, wave3), drop(1, wave4), drop(1, wave5),
    drop(0, wave1), drop(0, wave2), drop(0, wave3), drop(0, wave4), drop(0, wave5),
    wave6
].

:- func drop(int, list(T)) = list(T).

drop(N, L0) = L :-
    ( list.drop(N, L0, L1) ->
        L = L1
    ;
        error("drop/2")
    ).

%-----------------------------------------------------------------------------%

:- type badguys
    --->    badguys(
                wavenum     :: wavenum,
                evildudes_or_finished_counter
                            :: evildudes_or_finished_counter
            ).

:- type evildudes_or_finished_counter
    --->    evildudes(list(badguy))
    ;       finished_counter(int).

:- type badguy
    --->    badguy(
                x           :: float,   % x position
                y           :: float,   % y position
                speed       :: float,   % vertical speed
                move        :: float,   % horizontal motion
                sin_depth   :: float,   % depth of sine motion
                sin_speed   :: float,   % speed of sine motion
                split       :: bool,    % whether to split ourselves
                aggro       :: bool,    % whether to attack the player
                evade       :: bool,    % whether to evade the player
                v           :: float,   % horizontal velocity
                t           :: int      % integer counter
            ).

%-----------------------------------------------------------------------------%

init_badguys(WaveNum, badguys(WaveNum, evildudes(EvilDudes)), !RS) :-
    list.index0_det(waveinfo, WaveNum, WaveInfos),
    make_waves(WaveInfos, [], EvilDudes0, !RS),
    list.condense(EvilDudes0, EvilDudes).

lay_attack_wave(yes, _, Badguys, !RS) :-
    init_badguys(0, Badguys, !RS).
lay_attack_wave(no, badguys(WaveNum0, _), Badguys, !RS) :-
    WaveNum1 = WaveNum0 + 1,
    (if WaveNum1 >= length(waveinfo) then
        WaveNum = 0
    else
        WaveNum = WaveNum1
    ),
    init_badguys(WaveNum, Badguys, !RS).

:- pred make_waves(list(waveinfo)::in,
        list(list(badguy))::in, list(list(badguy))::out,
        rs::in, rs::out) is det.

make_waves([], Acc, Acc, !RS).
make_waves([WI | WIs], Acc0, Acc, !RS) :-
    make_wave(WI, Badguys, !RS),
    make_waves(WIs, [Badguys | Acc0], Acc, !RS).

:- pred make_wave(waveinfo::in, list(badguy)::out, rs::in, rs::out) is det.

make_wave(WaveInfo, Badguys, !RS) :-
    iterate_map_foldl(make_badguy(WaveInfo), 0, WaveInfo ^ wi_count,
        Badguys, !RS).

:- pred make_badguy(waveinfo::in, int::in, badguy::out, rs::in, rs::out)
        is det.

make_badguy(Info, _N, Badguy, !RS) :-
    Badguy = badguy(X, Y, Speed, Move, SinDepth, SinSpeed,
        Split, Aggro, Evade, V, T),
    urand(Rx, !RS),
    urand(Ry, !RS),
    urand(Rs, !RS),
    srand(Rmv, !RS),
    urand(Rsd, !RS),
    urand(Rss, !RS),
    random(Rt, !RS),
    X = Rx,
    Y = -Info ^ wi_delay - Ry * Info ^ wi_delay_rand,
    Speed = Info ^ wi_speed + Rs * Info ^ wi_speed_rand,
    Move = Info ^ wi_move + Rmv * Info ^ wi_speed_rand,
    SinDepth = Info ^ wi_sin_depth + Rsd * Info ^ wi_sin_depth_rand,
    SinSpeed = Info ^ wi_sin_speed + Rss * Info ^ wi_sin_speed_rand,
    Split = Info ^ wi_split,
    Aggro = Info ^ wi_aggro,
    Evade = Info ^ wi_evade,
    V = 0.0,
    T = Rt /\ 255.

:- pred urand(float::out, rs::in, rs::out) is det.

urand(U, !RS) :-
    random(U0, !RS),
    U = float(U0 /\ 255) / 255.0.

:- pred srand(float::out, rs::in, rs::out) is det.

srand(U - 0.5, !RS) :- urand(U, !RS).

%-----------------------------------------------------------------------------%

update_badguys(badguys(WaveNum, evildudes(Evildudes0)), Badguys,
                !Player, !Bullets, !Explosions, !Sounds, !Messages, !RS, no) :-
    update_badguys_2(Evildudes0, Evildudes, !Player, !Bullets, !Explosions,
        !Sounds, !Messages, !RS),
    (if
        Evildudes = [],
        not player_dying(!.Player)
    then
        add_message("Wave Complete", !Messages),
        sfx_ping(0, !Sounds),
        Badguys = badguys(WaveNum, finished_counter(1))
    else
        Badguys = badguys(WaveNum, evildudes(Evildudes))
    ).

update_badguys(badguys(WaveNum, finished_counter(FC0)),
                badguys(WaveNum, finished_counter(FC)),
                !Player, !Bullets, !Explosions, !Sounds, !Messages, !RS,
                WaveComplete) :-
    FC = FC0 + 1,
    WaveComplete = (if FC > 64 then yes else no).

:- pred update_badguys_2(list(badguy)::in, list(badguy)::out,
        player::in, player::out, bullets::in, bullets::out,
        explosions::in, explosions::out, sounds::in, sounds::out, 
        messages::in, messages::out, rs::in, rs::out) is det.

update_badguys_2([], [],
        !Player, !Bullets, !Explosions, !Sounds, !Messages, !RS).
update_badguys_2([Evildude0 | Evildudes0], Evildudes,
        !Player, !Bullets, !Explosions, !Sounds, !Messages, !RS) :-
    update_badguy(Evildude0, Evildudes1, !Player, !Bullets, !Explosions,
        !Sounds, !Messages, !RS),
    update_badguys_2(Evildudes0, Evildudes2, !Player, !Bullets, !Explosions, 
        !Sounds, !Messages, !RS),
    Evildudes = Evildudes1 ++ Evildudes2.

:- pred update_badguy(badguy::in, list(badguy)::out, player::in, player::out,
        bullets::in, bullets::out, explosions::in, explosions::out,
        sounds::in, sounds::out, messages::in, messages::out, rs::in, rs::out)
        is det.

update_badguy(!.Badguy, Badguys, !Player, !Bullets, !Explosions, !Sounds,
        !Messages, !RS) :-
    (if !.Badguy ^ aggro = yes then
        % attack the player
        D0 = !.Player ^ pos - !.Badguy ^ x,
        (if D0 < -0.5 then
            D1 = D0 + 1.0
        else if D0 > 0.5 then
            D1 = D0 - 1.0
        else
            D1 = D0
        ),
        (if !.Badguy ^ y < 0.5 then
            D2 = -D1
        else
            D2 = D1
        ),
        !:Badguy = !.Badguy ^ v := !.Badguy ^ v * 0.99,
        !:Badguy = !.Badguy ^ v := !.Badguy ^ v + sgn(D2) * 0.00025
    else if !.Badguy ^ evade = yes then
        % evade the player
        (if !.Badguy ^ y < 0.75 then
            D0 = !.Player ^ pos + 0.5
        else
            D0 = !.Badguy ^ x
        ),
        (if !.Badguy ^ move \= 0.0 then
            D1 = D0 + sgn(!.Badguy ^ move) / 16.0
        else
            D1 = D0
        ),
        D2 = find_target(D1, !.Player) - !.Badguy ^ x,
        (if D2 < -0.5 then
            D3 = D2 + 1.0
        else if D2 > 0.5 then
            D3 = D2 - 1.0
        else
            D3 = D2
        ),
        !:Badguy = !.Badguy ^ v := !.Badguy ^ v * 0.96,
        !:Badguy = !.Badguy ^ v := !.Badguy ^ v + sgn(D3) * 0.0004
    else
        true
    ),

    % horizontal move
    X0 = !.Badguy ^ x + !.Badguy^move +
        sin(float(!.Badguy^t) * !.Badguy^sin_speed) * !.Badguy^sin_depth +
        !.Badguy^v,

    (if X0 < 0.0 then
        X = X0 + 1.0
    else if X0 > 1.0 then
        X = X0 - 1.0
    else
        X = X0
    ),

    !:Badguy = !.Badguy ^ x := X,

    % vertical move
    Y0 = !.Badguy ^ y,
    Y  = Y0 + !.Badguy ^ speed,
    !:Badguy = !.Badguy ^ y := Y,

    (if Y > 0.5,
        Y0 =< 0.5,
        !.Badguy ^ split = yes
    then
        % split ourselves
        Badguys0 = [NewBadguyA, NewBadguyB],
        Move = !.Badguy ^ move,
        NewBadguyA = (!.Badguy  ^ move := Move - 0.001)
                                ^ t := RandA /\ 255,
        NewBadguyB = (!.Badguy  ^ move := Move + 0.001)
                                ^ t := RandB /\ 255,
        random(RandA, !RS),
        random(RandB, !RS),
        !:Badguy = !.Badguy ^ speed := !.Badguy ^ speed + 0.001
    else
        Badguys0 = []
    ),

    T = !.Badguy ^ t + 1,
    !:Badguy = !.Badguy ^ t := T,

    (if !.Badguy ^ y > 0.0 then
        kill_player(!.Badguy ^ x, !.Badguy ^ y, !Player, HitPlayer,
            !Explosions, !Sounds, !Messages),
        (
            % did we hit someone?
            HitPlayer = yes,
            HitBullet = no
        ;
            % or did someone else hit us?
            HitPlayer = no,
            kill_bullets(!.Badguy ^ x, !.Badguy ^ y, !Bullets, !Explosions,
                !Sounds, HitBullet)
        )
    else
        HitPlayer = no,
        HitBullet = no
    ),

    Hit = HitPlayer `or` HitBullet,
    (
        Hit = yes,
        Badguys = Badguys0
    ;
        Hit = no,
        Badguys = [!.Badguy | Badguys0]
    ).

kill_all_badguys(badguys(WaveNum, _), badguys(WaveNum, finished_counter(64))).

%-----------------------------------------------------------------------------%

draw_badguys(Bitmap, R, G, B, Project, badguys(_, evildudes(Evildudes)),
        !IO) :-
    makecol(R, G, B, C, !IO),
    list.foldl(draw_badguy(Bitmap, Project, C), Evildudes, !IO).

draw_badguys(_, _, _, _, _, badguys(_, finished_counter(_)), !IO).

:- pred draw_badguy(bitmap::in, project_func::project_func, color::in, 
        badguy::in, io::di, io::uo) is det.

draw_badguy(Bitmap, Project, Col, Badguy, !IO) :-
    X = Badguy ^ x,
    Y = Badguy ^ y,
    (if
        Y > 0.0,
        Project(X - 0.02, Y + 0.01,  XA, YA),
        Project(X,        Y + 0.02,  XB, YB),
        Project(X + 0.02, Y + 0.01,  XC, YC),
        Project(X + 0.01, Y + 0.005, XD, YD),
        Project(X,        Y - 0.015, XE, YE),
        Project(X - 0.01, Y + 0.005, XF, YF)
    then
        Points = [XA, YA, XB, YB, XC, YC, XD, YD, XE, YE, XF, YF],
        polygon(Bitmap, Points, Col, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
