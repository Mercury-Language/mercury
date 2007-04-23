%-----------------------------------------------------------------------------%

:- module expl.
:- interface.

:- import_module allegro.
:- import_module allegro.rle.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- func explode_frames = int.
:- pred generate_explosions(maybe(list(rle_sprite))::out, io::di, io::uo)
        is det.
:- pred destroy_explosions(list(rle_sprite)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module util.

:- import_module allegro.bitmap.
:- import_module allegro.color.
:- import_module allegro.fixed.
:- import_module allegro.prim.

:- import_module int.
:- import_module random.
:- import_module require.
:- import_module time.

%-----------------------------------------------------------------------------%

:- func explode_size = int.
explode_size = 80.

explode_frames = 64.

:- func hotspots = int.
hotspots = 64.

:- type hotspot
    --->    hotspot(
                x   :: fixed,
                y   :: fixed,
                xc  :: fixed,
                yc  :: fixed
            ).

%-----------------------------------------------------------------------------%

generate_explosions(MaybeExplosions, !IO) :-
    create_bitmap(explode_size, explode_size, MaybeBitmap, !IO),
    (
        MaybeBitmap = yes(Bitmap),
        clock(Time, !IO),
        random.init(Time, RS0),
        iterate_map_foldl(make_hotspot, 0, hotspots, HotSpots0, RS0, _RS),
        iterate_map_foldl2(generate_frame(Bitmap), 0, explode_frames,
            Explosions, HotSpots0, _HotSpots, !IO),
        MaybeExplosions = yes(Explosions),
        destroy_bitmap(Bitmap, !IO)
    ;
        MaybeBitmap = no,
        MaybeExplosions = no
    ).

:- pred make_hotspot(int::in, hotspot::out, rs::mdi, rs::muo) is det.

make_hotspot(_N, hotspot(X, Y, XC, YC), !RS) :-
    X = itofix(explode_size/2),
    Y = itofix(explode_size/2),
    random(XC0, !RS),
    random(YC0, !RS),
    XC = raw_int_to_fixed((XC0 /\ 0xffff) - 0x7fff),
    YC = raw_int_to_fixed((YC0 /\ 0xffff) - 0x7fff).

:- pred generate_frame(bitmap::in, int::in, rle_sprite::out,
        list(hotspot)::in, list(hotspot)::out, io::di, io::uo) is det.

generate_frame(Bitmap, Frame, RLESprite, HotSpots0, HotSpots, !IO) :-
    clear_bitmap(Bitmap, !IO),
    Color = (if Frame < 16 then Frame * 4 else (80 - Frame)) >> 2,
    list.map_foldl(draw_hotspot(Bitmap, Color), HotSpots0, HotSpots, !IO),
    postprocess(Bitmap, !IO),
    get_rle_sprite(Bitmap, MaybeRLESprite, !IO),
    (
        MaybeRLESprite = yes(RLESprite)
    ;
        MaybeRLESprite = no,
        error("generate_frame/7: get_rle_sprite failed")
    ).

:- pred draw_hotspot(bitmap::in, color::in, hotspot::in, hotspot::out,
        io::di, io::uo) is det.
:- pred draw_hotspot_2(bitmap::in, color::in, hotspot::in, int::in,
        io::di, io::uo) is det.
:- pred draw_hotspot_3(bitmap::in, color::in, hotspot::in, int::in, int::in,
        io::di, io::uo) is det.

draw_hotspot(Bitmap, Color, HotSpot0, HotSpot, !IO) :-
    int.fold_up(draw_hotspot_2(Bitmap, Color, HotSpot), -6, 7, !IO),
    HotSpot0 = hotspot(X, Y, XC, YC),
    HotSpot = hotspot(X+XC, Y+YC, XC, YC).

draw_hotspot_2(Bitmap, Color, HotSpot, X, !IO) :-
    int.fold_up(draw_hotspot_3(Bitmap, Color, HotSpot, X), -6, 7, !IO).

draw_hotspot_3(Bitmap, Color, hotspot(HX, HY, _, _), X, Y, !IO) :-
    XX = fixtoi(HX) + X,
    YY = fixtoi(HY) + Y,
    getpixel(Bitmap, XX, YY, P0, !IO),
    P = min(63, P0 + (Color >> ((abs(X) + abs(Y)) / 3))),
    putpixel(Bitmap, XX, YY, P, !IO).

:- pred postprocess(bitmap::in, io::di, io::uo) is det.
:- pred postprocess_2(bitmap::in, int::in, io::di, io::uo) is det.
:- pred postprocess_3(bitmap::in, int::in, int::in, io::di, io::uo) is det.

postprocess(Bitmap, !IO) :-
    int.fold_up(postprocess_2(Bitmap), 0, explode_size, !IO).

postprocess_2(Bitmap, X, !IO) :-
    int.fold_up(postprocess_3(Bitmap, X), 0, explode_size, !IO).

postprocess_3(Bitmap, X, Y, !IO) :-
    getpixel(Bitmap, X, Y, C, !IO),
    C1 = (if C < 8 then 0 else 16 + C/4),
    putpixel(Bitmap, X, Y, C1, !IO).

%-----------------------------------------------------------------------------%

destroy_explosions(Explosions, !IO) :-
    list.foldl(destroy_rle_sprite, Explosions, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
