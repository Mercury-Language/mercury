%-----------------------------------------------------------------------------%

:- module bullet.
:- interface.

:- import_module aster.
:- import_module data.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type bullets.

:- func init_bullets = bullets.
:- pred add_bullet(int::in, int::in, bullets::in, bullets::out) is det.
:- pred move_bullets(bullets::in, bullets::out, asteroids::in, asteroids::out,
        play_boom::play_boom, io::di, io::uo) is det.
:- pred scroll_bullets(bullets::in, bullets::out) is det.
:- pred draw_bullets(data::in, bitmap::in, bullets::in, io::di, io::uo) is det.

:- type play_boom == pred(int, io, io).
:- mode play_boom == (pred(in, di, uo) is det).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.rle.

:- import_module int.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type bullets == list(bullet).

:- type bullet
        --->    bullet(
                    x   :: int,
                    y   :: int
                ).

:- func bullet_speed = int.

bullet_speed = 6.

%-----------------------------------------------------------------------------%

init_bullets = [].

add_bullet(X, Y, Bullets, [bullet(X, Y) | Bullets]).

move_bullets(!Bullets, !Asteroids, PlayBoom, !IO) :-
    move_bullets_2(!.Bullets, [], !:Bullets, !Asteroids, PlayBoom, !IO).

:- pred move_bullets_2(bullets::in, bullets::in, bullets::out,
        asteroids::in, asteroids::out,
        pred(int, io, io)::(pred(in, di, uo) is det), io::di, io::uo) is det.

move_bullets_2([],                  Acc,  Acc, !Asteroids, _PlayBoom, !IO).
move_bullets_2([Bullet0 | Bullets], Acc0, Acc, !Asteroids, PlayBoom, !IO) :-
    move_bullet(Bullet0, MaybeBullet, !Asteroids, PlayBoom, !IO),
    (
        MaybeBullet = yes(Bullet),
        move_bullets_2(Bullets, [Bullet | Acc0], Acc, !Asteroids, PlayBoom,
            !IO)
    ;
        MaybeBullet = no,
        move_bullets_2(Bullets, Acc0, Acc, !Asteroids, PlayBoom, !IO)
    ).

:- pred move_bullet(bullet::in, maybe(bullet)::out,
        asteroids::in, asteroids::out,
        pred(int, io, io)::(pred(in, di, uo) is det),
        io::di, io::uo) is det.

move_bullet(bullet(X, Y0), MaybeBullet, !Asteroids, PlayBoom, !IO) :-
    Y = Y0 - bullet_speed,
    asteroid_collision(X, Y, 20, !Asteroids, Collision),
    (
        Collision = collision,
        PlayBoom(X, !IO),
        MaybeBullet = no
    ;
        Collision = no_collision,
        (if Y >= 8 then
            MaybeBullet = yes(bullet(X, Y))
        else
            MaybeBullet = no
        )
    ).

scroll_bullets(!Bullets) :-
    list.map(scroll_bullet, !Bullets).

:- pred scroll_bullet(bullet::in, bullet::out) is det.

scroll_bullet(Bullet0, Bullet) :-
    Y = Bullet0 ^ y,
    Bullet = (Bullet0 ^ y := Y + 1).

draw_bullets(Data, Bitmap, Bullets, !IO) :-
    list.foldl(draw_bullet(Bitmap, Data ^ rocket), Bullets, !IO).

:- pred draw_bullet(bitmap::in, rle_sprite::in, bullet::in, io::di, io::uo)
        is det.

draw_bullet(Bitmap, Sprite, bullet(X, Y), !IO) :-
    SX = X - rle_sprite_w(Sprite)/2,
    SY = Y - rle_sprite_h(Sprite)/2,
    draw_rle_sprite(Bitmap, Sprite, SX, SY, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
