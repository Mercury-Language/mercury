%-----------------------------------------------------------------------------%

:- module star2d.
:- interface.

:- import_module util.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type starfield_2d.

:- pred init_starfield_2d(int::in, int::in, starfield_2d::out,
        rs::mdi, rs::muo) is det.
:- pred move_starfield_2d(starfield_2d::in, starfield_2d::out) is det.
:- pred scroll_stars(starfield_2d::in, starfield_2d::out) is det.
:- pred draw_starfield_2d(bitmap::in, starfield_2d::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.prim.
:- import_module int.
:- import_module list.
:- import_module random.

%-----------------------------------------------------------------------------%

:- type starfield_2d
    --->    starfield_2d(
                screen_w    :: int,
                screen_h    :: int,
                stars       :: list(star)
            ).

:- type star
    --->    star(
                x   :: int,
                y   :: int,
                z   :: int
            ).

:- func max_stars = int.

max_stars = 128.

%-----------------------------------------------------------------------------%

init_starfield_2d(ScreenW, ScreenH,
                  starfield_2d(ScreenW, ScreenH, Stars), !RS) :-
    iterate_map_foldl(make_star(ScreenW, ScreenH), 0, max_stars, Stars, !RS).

:- pred make_star(int::in, int::in, int::in, star::out, rs::mdi, rs::muo)
        is det.

make_star(ScreenW, ScreenH, _, star(X, Y, Z), !RS) :-
    random(0, ScreenW, X, !RS),
    random(0, ScreenH, Y, !RS),
    random(Z0, !RS),
    Z = Z0 /\ 7.

move_starfield_2d(starfield_2d(ScreenW, ScreenH, Stars0),
                  starfield_2d(ScreenW, ScreenH, Stars)) :-
    list.map(move_star(ScreenH), Stars0, Stars).

:- pred move_star(int::in, star::in, star::out) is det.

move_star(ScreenH, star(X, Y0, Z), star(X, Y, Z)) :-
    Y1 = Y0 + (Z >> 1) + 1,
    Y = (if Y1 >= ScreenH then 0 else Y1).

scroll_stars(starfield_2d(ScreenW, ScreenH, Stars0),
             starfield_2d(ScreenW, ScreenH, Stars)) :-
    list.map(scroll_star(ScreenH), Stars0, Stars).

:- pred scroll_star(int::in, star::in, star::out) is det.

scroll_star(ScreenH, star(X, Y0, Z), star(X, Y, Z)) :-
    Y = (if Y0+1 >= ScreenH then 0 else Y0+1).

draw_starfield_2d(Bitmap, starfield_2d(ScreenW, ScreenH, Stars), !IO) :-
    list.foldl(draw_star(Bitmap, ScreenW, ScreenH), Stars, !IO).

:- pred draw_star(bitmap::in, int::in, int::in, star::in, io::di, io::uo)
        is det.

draw_star(Bitmap, ScreenW, ScreenH, star(OX, Y, Z), !IO) :-
    X = ((OX - ScreenW/2) * (Y / (4 - Z/2) + ScreenH) / ScreenH) + ScreenW/2,
    putpixel(Bitmap, X, Y, 15 - Z, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
