%-----------------------------------------------------------------------------%

:- module bullet.
:- interface.

:- import_module explode.
:- import_module sound.
:- import_module view.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type bullets.

:- pred kill_bullets(float::in, float::in, bullets::in, bullets::out,
        explosions::in, explosions::out, sounds::in, sounds::out,
        bool::out) is det.
:- func init_bullets = bullets.
:- pred fire_bullet(float::in, bullets::in, bullets::out,
        sounds::in, sounds::out) is det.
:- pred update_bullets(bullets::in, bullets::out) is det.
:- pred draw_bullets(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, bullets::in, io::di, io::uo) is det.

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

%-----------------------------------------------------------------------------%

:- type bullets == list(bullet).

:- type bullet
    --->    bullet(
                x   :: float,
                y   :: float
            ).

    % XXX: most of this routine should be in badguys.m
    %
kill_bullets(_X, _Y, [], [], !Explosions, !Sounds, no).
kill_bullets(X0, Y0, [Bullet0 | Bullets0], Bullets, !Explosions,
        !Sounds, Hit) :-
    X3 = dist(X0, Bullet0 ^ x),
    Y3 = abs(Y0 - Bullet0 ^ y),

    (if X3 < Y3 then
        D = Y3 / 2.0 + X3
    else
        D = X3 / 2.0 + Y3
    ),

    (if D < 0.025 then
        Bullets = Bullets0,
        explode(X0, Y0, 0, !Explosions),
        sfx_explode_alien(!Sounds),
        Hit = yes
    else
        Bullets = [Bullet0 | Bullets1],
        kill_bullets(X0, Y0, Bullets0, Bullets1, !Explosions, !Sounds, Hit)
    ).

init_bullets = [].

fire_bullet(X, Bullets, [Bullet | Bullets], !Sounds) :-
    Bullet = bullet(X, 0.96),
    sfx_shoot(!Sounds).

update_bullets(!Bullets) :-
    list.filter_map(update_bullet, !Bullets).

:- pred update_bullet(bullet::in, bullet::out) is semidet.

update_bullet(bullet(X, Y0), bullet(X, Y)) :-
    Y = Y0 - 0.025,
    Y >= 0.0.

draw_bullets(Bitmap, R, G, B, Project, Bullets, !IO) :-
    makecol(128+R/2, 128+G/2, 128+B/2, C1, !IO),
    (if G \= 0 then
        makecol(R/5, G/5, B/5, C2, !IO)
    else
        makecol(R/4, G/4, B/4, C2, !IO)
    ),
    list.foldl(draw_bullet(Bitmap, C1, C2, Project), Bullets, !IO).

:- pred draw_bullet(bitmap::in, color::in, color::in,
        project_func::project_func, bullet::in, io::di, io::uo) is det.

draw_bullet(Bitmap, C1, C2, Project, bullet(X, Y), !IO) :-
    (if
        Project(X - 0.005, Y + 0.01,   XA, YA),
        Project(X + 0.005, Y + 0.01,   XB, YB),
        Project(X,         Y - 0.0015, XC, YC)
    then
        polygon(Bitmap, [XA, YA, XB, YB, XC, YC], C1, !IO),

        CX = (XA + XB + XC) / 3,
        CY = (YA + YB + YC) / 3,
        Rot = (if odd(truncate_to_int(X * 256.0)) then -Y else Y),
        CosRot = cos(Rot),
        SinRot = sin(Rot),
        ViewSize = (640.0 + 480.0) / 2.0,   % XXX
        corner(CosRot, SinRot, Y, ViewSize, -1.0, -1.0, BoxXA, BoxYA),
        corner(CosRot, SinRot, Y, ViewSize, -1.0,  1.0, BoxXB, BoxYB),
        corner(CosRot, SinRot, Y, ViewSize,  1.0,  1.0, BoxXC, BoxYC),
        corner(CosRot, SinRot, Y, ViewSize,  1.0, -1.0, BoxXD, BoxYD),
        line(Bitmap, CX + BoxXA, CY + BoxYA, CX + BoxXB, CY + BoxYB, C2, !IO),
        line(Bitmap, CX + BoxXB, CY + BoxYB, CX + BoxXC, CY + BoxYC, C2, !IO),
        line(Bitmap, CX + BoxXC, CY + BoxYC, CX + BoxXD, CY + BoxYD, C2, !IO),
        line(Bitmap, CX + BoxXD, CY + BoxYD, CX + BoxXA, CY + BoxYA, C2, !IO)
    else
        true
    ).

:- pred corner(float::in, float::in, float::in, float::in,
        float::in, float::in, int::out, int::out) is det.

corner(CosRot, SinRot, Y, ViewSize, BoxX0, BoxY0, BoxX, BoxY) :-
    TX = CosRot * BoxX0 + SinRot * BoxY0,
    TY = SinRot * BoxX0 - CosRot * BoxY0,
    BoxX = truncate_to_int(TX * Y * ViewSize / 8.0),
    BoxY = truncate_to_int(TY * Y * ViewSize / 8.0).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
