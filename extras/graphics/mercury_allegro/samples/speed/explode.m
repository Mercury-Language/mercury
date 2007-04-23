%-----------------------------------------------------------------------------%

:- module explode.
:- interface.

:- import_module view.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type explosions.

:- func init_explosions = explosions.
:- pred explode(float::in, float::in, int::in, explosions::in,
        explosions::out) is det.
:- pred update_explosions(explosions::in, explosions::out) is det.
:- pred draw_explosions(bitmap::in, int::in, int::in, int::in,
        project_func::project_func, explosions::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.color.
:- import_module allegro.prim.

:- import_module float.
:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type explosions == list(explosion).

:- type explosion
    --->    explosion(
                x       :: float,
                y       :: float,
                big     :: float,
                time    :: float
            ).

init_explosions = [].

explode(X, Y, Big, Explosions, [Explosion | Explosions]) :-
    Explosion = explosion(X, Y, float(Big), 0.0).

update_explosions(!Explosions) :-
    list.filter_map(update_explosion, !Explosions).

:- pred update_explosion(explosion::in, explosion::out) is semidet.

update_explosion(explosion(X, Y, Big, Time0), explosion(X, Y, Big, Time)) :-
    Time = Time0 + (1.0 / (Big / 2.0 + 1.0)),
    Time =< 32.0.

draw_explosions(Bitmap, R, G, B, Project, Explosions, !IO) :-
    ViewSize = (640.0 + 480.0) / 2.0,   % XXX
    list.foldl(draw_explosion(Bitmap, ViewSize, R, G, B, Project),
        Explosions, !IO).

:- pred draw_explosion(bitmap::in, float::in, int::in, int::in, int::in,
        project_func::project_func, explosion::in, io::di, io::uo) is det.

draw_explosion(Bitmap, ViewSize, R, G, B, Project, explosion(X, Y, Big, Time),
        !IO) :-
    (if 
        Project(X, Y, IX, IY)
    then
        S = Time * ViewSize / (512.0 / (Big + 1.0)),
        ITime = truncate_to_int(Time),
        (if ITime < 24 then
            C1 = (24 - ITime) * 255 / 24,
            makecol(C1, C1, C1, Col1, !IO),
            circle(Bitmap, IX, IY, truncate_to_int(S*2.0), Col1, !IO),
            circle(Bitmap, IX, IY, truncate_to_int(S*S/8.0), Col1, !IO)
        else
            true
        ),
        (if ITime < 32 then
            RR0 = (32 - ITime) * R / 32,
            GG0 = (32 - ITime) * G / 32,
            BB0 = (32 - ITime) * B / 32,
            C2 = max((24 - ITime) * 255 / 24, 0),
            RR = max(RR0, C2),
            GG = max(GG0, C2),
            BB = max(BB0, C2),
            makecol(RR, GG, BB, Col2, !IO),
            circlefill(Bitmap, IX, IY, truncate_to_int(S), Col2, !IO)
        else
            true
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
