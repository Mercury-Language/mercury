%-----------------------------------------------------------------------------%

:- module player.
:- interface.

:- import_module bullet.
:- import_module explode.
:- import_module message.
:- import_module sound.
:- import_module view.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type player.

:- type input
    --->    input(
                escape  :: bool,
                left    :: bool,
                right   :: bool,
                fire    :: bool
            ).

:- func player ^ lives = int.
:- func player ^ pos = float.
:- func player ^ score = int.

:- func find_target(float, player) = float.
:- pred player_dying(player::in) is semidet.
:- pred kill_player(float::in, float::in, player::in, player::out, bool::out,
        explosions::in, explosions::out, sounds::in, sounds::out,
        messages::in, messages::out) is det.
:- func init_player = player.
:- pred advance_player(player::in, player::out, bool::in,
        sounds::in, sounds::out, messages::in, messages::out) is det.
:- pred update_player(input::in, player::in, player::out, player.result::out,
        bullets::in, bullets::out, sounds::in, sounds::out, 
        messages::in, messages::out) is det.
:- pred draw_player(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, player::in, io::di, io::uo) is det.

:- type player.result
    --->    dead
    ;       continue.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module util.

:- import_module allegro.color.
:- import_module allegro.prim.

:- import_module int.
:- import_module float.
:- import_module list.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- func max_segments = int.
max_segments = 16.

:- func cheat = bool.
cheat = no.

:- type player
    --->    player(
                lives       :: int,
                init_time   :: int,
                die_time    :: int,
                fire_time   :: int,
                pos         :: float,
                vel         :: float,
                segments    :: segments,
                score       :: int
            ).

:- type segments == set(segment).
:- type segment == int.

%-----------------------------------------------------------------------------%

    % tell other people where to avoid us
find_target(X, Player)
    = find_target_2(0, Seg, Player ^ pos, Player ^ segments) :-
    Seg0 = truncate_to_int(X * float(max_segments)) `mod` max_segments,
    (if Seg0 < 0 then
        Seg = Seg0 + max_segments
    else
        Seg = Seg0
    ).

:- func find_target_2(int, segment, float, set(segment)) = float.

find_target_2(I, Seg, Pos, Segments) = Out :-
    (if I >= max_segments/2 then
        (if Pos < 0.5 then
            Out = Pos + 0.5
        else
            Out = Pos - 0.5
        )
    else
        J = (Seg + I) `mod` max_segments,
        K = (Seg - I) `mod` max_segments,
        Max = float(max_segments),
        (if Segments `contains` J then
            Out = (float(J) + 0.5) / Max
        else if Segments `contains` K then
            Out = (float(K) + 0.5) / Max
        else
            Out  = find_target_2(I + 1, Seg, Pos, Segments)
        )
    ).

    % tell other people whether we are healthy
player_dying(Player) :-
    (Player ^ die_time) \= 0,
    (Player ^ lives) =< 1.

    % called by the badguys when they want to blow us up
kill_player(X, Y, !Player, HitSegment `or` HitPlayer,
        !Explosions, !Sounds, !Messages) :-
    Seg = float.floor_to_int(X * float(max_segments)) `mod` max_segments,
    (if Y >= 0.97 then
        HitSegment = yes,
        Segments0 = !.Player ^ segments,
        (if Segments0 `contains` Seg then
            Segments = Segments0 `delete` Seg,
            !:Player = !.Player ^ segments := Segments,
            explode(X, 0.98, 1, !Explosions),
            sfx_explode_block(!Sounds)
        else
            true
        )
    else
        HitSegment = no
    ),

    (if Y >= 0.95,
        !.Player ^ init_time = 0,
        !.Player ^ die_time = 0,
        cheat = no,
        D = dist(!.Player ^ pos, X),
        D < 0.06
    then
        !:Player = !.Player ^ die_time := 128,
        explode(X, 0.98, 2, !Explosions),
        explode(X, 0.98, 4, !Explosions),
        sfx_explode_player(!Sounds),
	add_message("Ship Destroyed", !Messages),
        HitPlayer = yes
    else
        HitPlayer = no
    ).

init_player =
    player(
        3,          % lives
        128, 0, 0,  % times
        0.5, 0.0,   % pos, vel
        all_segments,
        0           % score
    ).

:- func all_segments = segments.

all_segments = set.from_list(0 `..` (max_segments-1)).

advance_player(Player0, Player, Cycled, !Sounds, !Message) :-
    Bonus0 = set.count(Player0 ^ segments),
    (if Bonus0 = max_segments then
        add_message("Bonus: 100", !Message),
        Bonus1 = Bonus0 + 100
    else
        Bonus1 = Bonus0
    ),
    (if Cycled = yes then
        add_message("Bonus: 1000", !Message),
        Bonus2 = Bonus1 + 1000
    else
        Bonus2 = Bonus1
    ),
    add_message(string.format("Score: %d", [i(Bonus0)]), !Message),
    (if Bonus0 = max_segments ; Cycled = yes then
        sfx_ping(1, !Sounds)
    else
        true
    ),
    Score0 = Player0 ^ score,
    Player = (Player0 ^ score := Score0 + Bonus2)
                      ^ segments := all_segments.

update_player(Input, Player0, Player, Result, !Bullets, !Sounds, !Message) :-
    Player0 = player(Lives0, InitTime0, DieTime0, FireTime0,
        Pos0, Vel0, Segments0, Score),
    update_player_2(Input, Lives0, Lives, InitTime0, InitTime,
        DieTime0, DieTime, FireTime0, FireTime,
        Pos0, Pos, Vel0, Vel, Segments0, Segments,
        Result, !Bullets, !Sounds, !Message),
    Player = player(Lives, InitTime, DieTime, FireTime,
        Pos, Vel, Segments, Score).

:- pred update_player_2(input::in, int::in, int::out,
        int::in, int::out, int::in, int::out, int::in, int::out, 
        float::in, float::out, float::in, float::out,
        segments::in, segments::out, player.result::out,
        bullets::in, bullets::out, sounds::in, sounds::out, 
        messages::in, messages::out) is det.

update_player_2(Input, !Lives, !InitTime, !DieTime, !FireTime,
        !Pos, !Vel, !Segments, Result, !Bullets, !Sounds, !Message) :-
    % safe period while initing
    !:InitTime = max(0, !.InitTime - 1),

    % blown up?
    (if !.DieTime = 0 then
        Result = continue
    else if !.DieTime = 1 then
        !:DieTime = 0,
        !:Lives = !.Lives - 1,
        (if !.Lives = 0 then
            Result = dead
        else
            !:InitTime = 128,
            !:Pos = 0.5,
            !:Vel = 0.0,
            Result = continue
        ),
        (if !.Lives = 1 then
            add_message("This Is Your Final Life", !Message)
        else
            add_message("One Life Remaining", !Message)
        )
    else
        !:DieTime = !.DieTime - 1,
        Result = continue
    ),

    (
        Result = dead
    ;
        Result = continue,
   
        % handle user left/right input
        (if !.DieTime = 0 then
            (if Input ^ left = yes then
                !:Vel = !.Vel - 0.005
            else
                true
            ),
            (if Input ^ right = yes then
                !:Vel = !.Vel + 0.005
            else
                true
            )
        else
            true
        ),

        % move left and right
        !:Pos = !.Pos + !.Vel,

        (if !.Pos >= 1.0 then
            !:Pos = !.Pos - 1.0
        else
            true
        ),
        (if !.Pos < 0.0 then
            !:Pos = !.Pos + 1.0
        else
            true
        ),

        !:Vel = !.Vel * 0.67,

        % fire bullets
        (
            !.DieTime = 0,
            !.InitTime = 0,
            !.FireTime = 0,
            Input ^ fire = yes
        ->
            fire_bullet(!.Pos, !Bullets, !Sounds),
            !:FireTime = 24
        ;
            true
        ),
        !:FireTime = max(0, !.FireTime - 1)
    ).

draw_player(Bitmap, R, G, B, Project, Player, !IO) :-
    set.fold(draw_segment(Bitmap, R, G, B, Project), Player^segments, !IO),
    (if Player ^ init_time /\ 4 = 0, 
        Player ^ die_time = 0
    then
        draw_ship(Bitmap, R, G, B, Project, Player, !IO)
    else
        true
    ).

:- pred draw_segment(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, segment::in, io::di, io::uo) is det.

draw_segment(Bitmap, R, G, B, Project, Segment, !IO) :-
    Max = float(max_segments),
    (if
        Project(float(Segment)   / Max, 0.98, XA, YA),
        Project(float(Segment+1) / Max, 0.98, XB, YB),
        Project(float(Segment+1) / Max, 1.0,  XC, YC),
        Project(float(Segment)   / Max, 1.0,  XD, YD)
    then
        makecol(R/3, G/3, B/3, Col, !IO),
        polygon(Bitmap, [XA, YA, XB, YB, XC, YC, XD, YD], Col, !IO)
    else
        true
    ).

:- pred draw_ship(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, player::in, io::di, io::uo) is det.

draw_ship(Bitmap, R, G, B, Project, Player, !IO) :-
    Pos = Player ^ pos,
    (if
        Project(Pos - 0.04, 0.98, XA, YA),
        Project(Pos - 0.02, 0.97, XB, YB),
        Project(Pos,        0.95, XC, YC),
        Project(Pos + 0.02, 0.97, XD, YD),
        Project(Pos + 0.04, 0.98, XE, YE),
        Project(Pos,        0.98, XF, YF)
    then
        makecol(R, G, B, Col, !IO),
        polygon(Bitmap, [XA, YA, XB, YB, XC, YC, XD, YD, XE, YE, XF, YF],
            Col, !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
