%-----------------------------------------------------------------------------%

:- module title.
:- interface.

:- import_module data.
:- import_module display.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type color_scheme.

:- type what_to_do
    --->    play_game
    ;       quit.

:- func init_color_scheme = color_scheme.
:- pred title_screen(data::in, color_scheme::in, what_to_do::out,
        D::in, D::out, io::di, io::uo) is det <= display(D).
:- func next_color_scheme(color_scheme) = color_scheme.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

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
:- import_module allegro.timer.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module random.
:- import_module time.

%-----------------------------------------------------------------------------%

:- type color_scheme
    --->    color_scheme_0
    ;       color_scheme_1
    ;       color_scheme_2.

init_color_scheme = color_scheme_0.

title_screen(Data, ColorScheme, WhatToDo, !Display, !IO) :-
    play_midi(Data ^ title_music, yes, !IO),
    play_sample(Data ^ welcome_spl, 255, 127, 1000, no_loop, !IO),
    make_title_palette(Data ^ title_pal, ColorScheme, Palette, !IO),
    set_palette(Palette, !IO),
    install_int(5, MaybeZoomCounter, !IO),
    (
        MaybeZoomCounter = yes(ZoomCounter),
        set_ticker(ZoomCounter, 1, !IO),
        det_screen(Screen, !IO),
        clear_bitmap(Screen, !IO),
        title_screen_2_zoom_logo(Data ^ title_bmp, ZoomCounter, !IO),
        remove_int(ZoomCounter, !IO)
    ;
        MaybeZoomCounter = no
    ),
    clear_keybuf(!IO),
    title_screen_3_wait_to_play(Data ^ title_bmp, !Display, !IO),
    fade_out(5, !IO),
    keypressed(KP, !IO),
    (
        KP = yes,
        readkey_decode(Ascii, _, !IO),
        WhatToDo = (if Ascii = '\033\' then quit else play_game)
    ;
        KP = no,
        WhatToDo = play_game
    ).

:- pred title_screen_2_zoom_logo(bitmap::in, ticker::in, io::di, io::uo)
        is det.

title_screen_2_zoom_logo(TitleBmp, ZoomCounter, !IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    get_ticker(ZoomCounter, C, !IO),
    (if C < 160 then
        stretch_blit(TitleBmp, Screen, 0, 0, 320, 128,
            ScreenW/2 - C, ScreenH/2 - C*64/160 - 32,
            C*2, C*128/160, !IO),
        rest(1, !IO),
        title_screen_2_zoom_logo(TitleBmp, ZoomCounter, !IO)
    else
        blit(TitleBmp, Screen, 0, 0, ScreenW/2 - 160, ScreenH/2 - 96,
            320, 128, !IO)
    ).

:- pred title_screen_3_wait_to_play(bitmap::in, D::in, D::out, io::di, io::uo)
        is det <= display(D).

title_screen_3_wait_to_play(TitleBmp, !Display, !IO) :-
    time.clock(Time, !IO),
    random.init(Time, RS0),
    title_screen_3_wait_to_play_2(TitleBmp, init_starfield_3d,
        !Display, RS0, _RS, !IO).

:- pred title_screen_3_wait_to_play_2(bitmap::in, starfield_3d::in,
        D::in, D::out, rs::mdi, rs::muo, io::di, io::uo) is det <= display(D).

title_screen_3_wait_to_play_2(TitleBmp, Stars0, !Display, !RS, !IO) :-
    keypressed(KP, !IO),
    (
        KP = yes
    ;
        KP = no,
        det_screen(_, ScreenW, ScreenH, !IO),
        prepare_display(Bitmap, !Display, !IO),
        draw_starfield_3d(Bitmap, Stars0, !IO),
        draw_sprite(Bitmap, TitleBmp, ScreenW/2 - 160, ScreenH/2 - 96, !IO),
        flip_display(!Display, !IO),
        rest(1, !IO),
        update_starfield_3d(ScreenW, ScreenH, Stars0, Stars, !RS),
        title_screen_3_wait_to_play_2(TitleBmp, Stars, !Display, !RS, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred make_title_palette(palette::in, color_scheme::in, palette::out,
        io::di, io::uo) is det.

make_title_palette(TitlePalette, ColorScheme, New, !IO) :-
    new_palette(New, !IO),
    int.fold_up(copy_palette_entry(TitlePalette, New),
        0, 8, !IO),
    int.fold_up(make_title_palette_2(TitlePalette, New, ColorScheme),
        8, pal_size/2, !IO),
    int.fold_up(copy_palette_entry(TitlePalette, New),
        pal_size/2, pal_size-1, !IO).

:- pred copy_palette_entry(palette::in, palette::in, int::in, io::di, io::uo)
        is det.

copy_palette_entry(Src, Dest, C, !IO) :-
    get_palette_entry(Src,  C, R,G,B, !IO),
    set_palette_entry(Dest, C, R,G,B, !IO).

:- pred make_title_palette_2(palette::in, palette::in, color_scheme::in,
        int::in, io::di, io::uo) is det.

make_title_palette_2(TitlePalette, New, ColorScheme, C, !IO) :-
    get_palette_entry(TitlePalette, C, R,G,B, !IO),
    (
        ColorScheme = color_scheme_0,
        set_palette_entry(New, C, 0,G,R, !IO)
    ;
        ColorScheme = color_scheme_1,
        set_palette_entry(New, C, 0,R,B, !IO)
    ;
        ColorScheme = color_scheme_2,
        set_palette_entry(New, C, R,R,B, !IO)
    ).

%-----------------------------------------------------------------------------%

next_color_scheme(color_scheme_0) = color_scheme_1.
next_color_scheme(color_scheme_1) = color_scheme_2.
next_color_scheme(color_scheme_2) = color_scheme_0.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type starfield_3d
        --->    starfield_3d(
                    stars            :: list(star),
                    star_count       :: int,
                    star_count_count :: int
                ).

:- type star
        --->    dummy
        ;       star(
                    x :: fixed,
                    y :: fixed,
                    z :: fixed,
                    ox :: int,
                    oy :: int
                ).

:- func max_stars = int.

max_stars = 128.

:- func init_starfield_3d = starfield_3d.

init_starfield_3d = starfield_3d([], 0, 0).

:- pred update_starfield_3d(int::in, int::in,
        starfield_3d::in, starfield_3d::out, rs::mdi, rs::muo) is det.

update_starfield_3d(ScreenW, ScreenH,
        starfield_3d(Stars0, Count0, CountCount0),
        starfield_3d(Stars, Count, CountCount), !RS) :-
    update_stars(ScreenW, ScreenH, Stars0, Stars1, !RS),
    (if Count0 >= max_stars then
        Stars = Stars1,
        Count = Count0,
        CountCount = CountCount0
    else if CountCount0 + 1 < 32 then
        Stars = Stars1,
        Count = Count0,
        CountCount = CountCount0 + 1
    else
        Stars = [dummy | Stars1],
        Count = Count0 + 1,
        CountCount = 0
    ).

:- pred update_stars(int::in, int::in, list(star)::in, list(star)::out,
        rs::mdi, rs::muo) is det.

update_stars(_ScreenW, _ScreenH, [], [], !RS).
update_stars(ScreenW, ScreenH, [Star0|Stars0], [Star|Stars], !RS) :-
    update_star(ScreenW, ScreenH, Star0, Star, !RS),
    update_stars(ScreenW, ScreenH, Stars0, Stars, !RS).

:- pred update_star(int::in, int::in, star::in, star::out, rs::mdi, rs::muo)
        is det.

update_star(ScreenW, _ScreenH, dummy, star(X, Y, Z, -1, -1), !RS) :-
    random(0, 255, X0, !RS),
    random(Y0, !RS),
    random(Z0, !RS),
    X1 = itofix(X0),
    Y1 = itofix(((Y0 /\ 3) + 1) * ScreenW),
    X = cos(X1) * Y1,
    Y = sin(X1) * Y1,
    Z = itofix((Z0 /\ 0x1f) + 0x20).

update_star(ScreenW, ScreenH, star(X, Y, Z0, _, _), Star, !RS) :-
    IX = fixtoi(X/Z0) + ScreenW/2,
    IY = fixtoi(Y/Z0) + ScreenH/2,
    (if
        IX >= 0, IX < ScreenW,
        IY >= 0, IY < ScreenH
    then
        Z = raw_int_to_fixed(fixed_to_raw_int(Z0) - 4096),
        Star = star(X, Y, Z, IX, IY)
    else
        Star = dummy
    ).

:- pred draw_starfield_3d(bitmap::in, starfield_3d::in, io::di, io::uo)
        is det.

draw_starfield_3d(Bitmap, starfield_3d(Stars, _, _), !IO) :-
    list.foldl(draw_star(Bitmap), Stars, !IO).

:- pred draw_star(bitmap::in, star::in, io::di, io::uo) is det.

draw_star(_Bitmap, dummy, !IO).
draw_star(Bitmap, star(_, _, Z, OX, OY), !IO) :-
    Color = 7 - (fixed_to_raw_int(Z) >> 18),
    putpixel(Bitmap, OX, OY, clamp(0, Color, 7), !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
