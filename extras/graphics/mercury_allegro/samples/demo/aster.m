%-----------------------------------------------------------------------------%

:- module aster.
:- interface.

:- import_module data.
:- import_module util.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type asteroids.

:- type collision
    --->    collision
    ;       no_collision.

:- pred init_asteroids(int::in, int::in, asteroids::out, rs::mdi, rs::muo)
        is det.
:- pred scroll_asteroids(asteroids::in, asteroids::out) is det.
:- pred add_asteroid(asteroids::in, asteroids::out, rs::mdi, rs::muo) is det.
:- pred move_asteroids(asteroids::in, asteroids::out,
        maybe({int, int})::in, collision::out, rs::mdi, rs::muo) is det.
:- pred asteroid_collision(int::in, int::in, int::in, 
        asteroids::in, asteroids::out, collision::out) is det.
:- pred draw_asteroids(data::in, bitmap::in, asteroids::in, io::di, io::uo)
        is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module expl.

:- import_module allegro.rle.
:- import_module allegro.timer.
:- import_module int.
:- import_module list.
:- import_module random.

%-----------------------------------------------------------------------------%

:- type asteroids
    --->    asteroids(
                screen_w    :: int,
                screen_h    :: int,
                asteroids   :: list(asteroid)
            ).

:- type asteroid
    --->    asteroid(
                ast_state   :: aster.state,
                x           :: int,
                y           :: int,
                dir         :: direction,
                look        :: look
            ).

:- type state
    --->    normal
    ;       exploding(frame :: int).

:- type direction
    --->    left
    ;       right.

:- type look
    --->    look(
                asteroid_type,
                int     % integer that controls the rate at which the
                        % asteroid animation plays
            ).

:- type asteroid_type
    --->    asteroid_type_a
    ;       asteroid_type_b
    ;       asteroid_type_c.

%-----------------------------------------------------------------------------%

init_asteroids(ScreenW, ScreenH, Asteroids, !RS) :-
    make_asteroid(ScreenW, AsteroidA, !RS),
    make_asteroid(ScreenW, AsteroidB, !RS),
    Asteroids = asteroids(ScreenW, ScreenH, [AsteroidA, AsteroidB]).

:- pred make_asteroid(int::in, asteroid::out, rs::mdi, rs::muo) is det.

make_asteroid(ScreenW, asteroid(normal, X, Y, Dir, Look), !RS) :-
    random(16, ScreenW-32, X, !RS),
    random(Temp, !RS),
    Y = -60 - (Temp /\ 0x3f),
    random_direction(Dir, !RS),
    random_asteroid_type(Type, !RS),
    random(C, !RS),
    Look = look(Type, C).

%-----------------------------------------------------------------------------%

scroll_asteroids(asteroids(ScreenW, ScreenH, Asteroids0),
                 asteroids(ScreenW, ScreenH, Asteroids)) :-
    list.map(scroll_asteroid, Asteroids0, Asteroids).

:- pred scroll_asteroid(asteroid::in, asteroid::out) is det.

scroll_asteroid(Aster, Aster^y := Aster^y + 1).

%-----------------------------------------------------------------------------%

add_asteroid(asteroids(ScreenW, ScreenH, Asteroids),
             asteroids(ScreenW, ScreenH, [Aster | Asteroids]),
             !RS) :-
    make_asteroid(ScreenW, Aster, !RS).

%-----------------------------------------------------------------------------%

move_asteroids(asteroids(ScreenW, ScreenH, Asteroids0),
               asteroids(ScreenW, ScreenH, Asteroids),
               MaybeShipPos, Collision, !RS) :-
    move_asteroids_2(ScreenW, ScreenH, Asteroids0, [], Asteroids,
        MaybeShipPos, no_collision, Collision, !RS).

:- pred move_asteroids_2(int::in, int::in, list(asteroid)::in,
        list(asteroid)::in, list(asteroid)::out,
        maybe({int, int})::in, collision::in, collision::out,
        rs::mdi, rs::muo) is det.

move_asteroids_2(_, _, [], Acc, Acc, _, !Collision, !RS).
move_asteroids_2(ScreenW, ScreenH, [Asteroid0 | Asteroids], Acc0, Acc,
                 MaybeShipPos, !Collision, !RS) :-
    move_asteroid(ScreenW, ScreenH, Asteroid0, Asteroid,
        MaybeShipPos, !Collision, !RS),
    move_asteroids_2(ScreenW, ScreenH, Asteroids, [Asteroid | Acc0], Acc,
        MaybeShipPos, !Collision, !RS).

:- pred move_asteroid(int::in, int::in, asteroid::in, asteroid::out,
        maybe({int, int})::in, collision::in, collision::out,
        rs::mdi, rs::muo) is det.

move_asteroid(ScreenW, ScreenH,
              asteroid(State0, X0, Y0, Dir0, Look),
              asteroid(State,  X,  Y,  Dir,  Look),
              MaybeShipPos, !Collision, !RS) :-
    Y1 = Y0 + 1,
    (if Y1 > ScreenH + 30 then
        respawn_asteroid(ScreenW, X, Y, Dir, !RS),
        State = normal
    else
        (
            State0 = exploding(Frame),
            (if Frame+1 < explode_frames then
                State = exploding(Frame+1),
                X = X0 + (if even(Frame) then dir_to_delta(Dir0) else 0),
                Y = Y1,
                Dir = Dir0
            else
                State = normal,
                random(0, ScreenW-32, XTemp, !RS),
                random(YTemp, !RS),
                X = 16 + XTemp,
                Y = -60 + (YTemp /\ 0x3f),
                random_direction(Dir, !RS)
            )
        ;
            State0 = normal,
            X = wrap_x_coordinate(X0 + dir_to_delta(Dir0), ScreenW),
            Y = Y1,
            Dir = Dir0,
            (if MaybeShipPos = yes({ShipX, ShipY}),
                abs(X - ShipX) < 48,
                abs(Y - ShipY) < 32
            then
                !:Collision = collision,
                State = exploding(0)
            else
                State = normal
            )
        )
    ).

:- pred respawn_asteroid(int::in, int::out, int::out, direction::out,
        rs::mdi, rs::muo) is det.

respawn_asteroid(ScreenW, X, Y, Dir, !RS) :-
    random(0, ScreenW-32, X, !RS),
    random(Temp, !RS),
    Y = -32 - (Temp /\ 0x3f),
    random_direction(Dir, !RS).

:- func dir_to_delta(direction) = int.

dir_to_delta(left) = -1.
dir_to_delta(right) = 1.

:- func wrap_x_coordinate(int, int) = int.

wrap_x_coordinate(X, ScreenW) =
    ( X < -60           -> ScreenW
    ; X > ScreenW + 60  -> -60
    ; X
    ).

%-----------------------------------------------------------------------------%

asteroid_collision(X, Y, S,
        asteroids(ScreenW, ScreenH, Asteroids0),
        asteroids(ScreenW, ScreenH, Asteroids), Collision) :-
    asteroid_collision_2(X, Y, S, Asteroids0, Asteroids, Collision).

:- pred asteroid_collision_2(int::in, int::in, int::in,
        list(asteroid)::in, list(asteroid)::out, collision::out) is det.

asteroid_collision_2(_X, _Y, _S, [], [], no_collision).
asteroid_collision_2(X, Y, S, [Asteroid0 | Asteroids0], [Asteroid | Asteroids],
        Collision) :-
    asteroid_collision_3(X, Y, S, Asteroid0, Asteroid, Collision1),
    (
        Collision1 = collision,
        Asteroids = Asteroids0,
        Collision = collision
    ;
        Collision1 = no_collision,
        asteroid_collision_2(X, Y, S, Asteroids0, Asteroids, Collision)
    ).

:- pred asteroid_collision_3(int::in, int::in, int::in,
        asteroid::in, asteroid::out, collision::out) is det.

asteroid_collision_3(X, Y, S, Asteroid0, Asteroid, Collision) :-
    Asteroid0 = asteroid(normal, AX, AY, _Dir, _Look),
    (if abs(Y - AY) < S,
        abs(X - AX) < S
    then
        Asteroid = Asteroid0 ^ ast_state := exploding(0),
        Collision = collision
    else
        Asteroid = Asteroid0,
        Collision = no_collision
    ).

asteroid_collision_3(_X, _Y, _S, Asteroid, Asteroid, no_collision) :-
    Asteroid ^ ast_state = exploding(_).

%-----------------------------------------------------------------------------%

draw_asteroids(Data, Bitmap, asteroids(_, _, Asteroids), !IO) :-
    retrace_count(RetraceCount, !IO),
    list.foldl(draw_asteroid(Data, Bitmap, RetraceCount), Asteroids, !IO).

:- pred draw_asteroid(data::in, bitmap::in, int::in, asteroid::in,
        io::di, io::uo) is det.

draw_asteroid(Data, Bitmap, RetraceCount, asteroid(State, X, Y, _Dir, Look),
        !IO) :-
    get_asteroid_sprite(Data, State, Look, RetraceCount, Sprite),
    SX = X - rle_sprite_w(Sprite)/2,
    SY = Y - rle_sprite_h(Sprite)/2,
    draw_rle_sprite(Bitmap, Sprite, SX, SY, !IO).

:- pred get_asteroid_sprite(data::in, aster.state::in, look::in, int::in,
        rle_sprite::out) is det.

get_asteroid_sprite(Data, normal, look(Type, C), RetraceCount, Sprite) :-
    ( Type = asteroid_type_a, Anim = Data ^ asta
    ; Type = asteroid_type_b, Anim = Data ^ astb
    ; Type = asteroid_type_c, Anim = Data ^ astc
    ),
    J = (RetraceCount / (6 - (C /\ 3)) + C) mod 15,
    Frame = (if even(C) then 14 - J else J),
    Sprite = list.det_index0(Anim, Frame).

get_asteroid_sprite(Data, exploding(Frame), _, _, Sprite) :-
    Sprite = list.det_index0(Data ^ explosions, Frame).

%-----------------------------------------------------------------------------%

:- pred random_asteroid_type(asteroid_type::out, rs::mdi, rs::muo) is det.

random_asteroid_type(Type, !RS) :-
    Types = [asteroid_type_a, asteroid_type_b, asteroid_type_c],
    random_list_element(Types, Type, !RS).

:- pred random_direction(direction::out, rs::mdi, rs::muo) is det.

random_direction(Dir, !RS) :-
    random_list_element([left, right], Dir, !RS).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
