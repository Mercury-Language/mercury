%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.blit.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.blit.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.color.
:- import_module allegro.fixed.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred blit(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred stretch_blit(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred masked_blit(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred masked_stretch_blit(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_sprite(bitmap::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.
:- pred stretch_sprite(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_sprite_v_flip(bitmap::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_sprite_h_flip(bitmap::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_sprite_vh_flip(bitmap::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_trans_sprite(bitmap::in, bitmap::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_lit_sprite(bitmap::in, bitmap::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_gouraud_sprite(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_character_ex(bitmap::in, bitmap::in, int::in, int::in, color::in, color::in, io::di, io::uo) is det.
:- pred rotate_sprite(bitmap::in, bitmap::in, int::in, int::in, fixed::in, io::di, io::uo) is det.
:- pred rotate_sprite_v_flip(bitmap::in, bitmap::in, int::in, int::in, fixed::in, io::di, io::uo) is det.
:- pred rotate_scaled_sprite(bitmap::in, bitmap::in, int::in, int::in, fixed::in, fixed::in, io::di, io::uo) is det.
:- pred rotate_scaled_sprite_v_flip(bitmap::in, bitmap::in, int::in, int::in, fixed::in, fixed::in, io::di, io::uo) is det.
:- pred pivot_sprite(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, fixed::in, io::di, io::uo) is det.
:- pred pivot_sprite_v_flip(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, fixed::in, io::di, io::uo) is det.
:- pred pivot_scaled_sprite(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, fixed::in, fixed::in, io::di, io::uo) is det.
:- pred pivot_scaled_sprite_v_flip(bitmap::in, bitmap::in, int::in, int::in, int::in, int::in, fixed::in, fixed::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    blit(Source::in, Dest::in, Sx::in, Sy::in, Dx::in, Dy::in, W::in, H::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    blit(Source, Dest, Sx, Sy, Dx, Dy, W, H);
    IO = IO0;
").

:- pragma foreign_proc("C",
    stretch_blit(Source::in, Dest::in, Sx::in, Sy::in, Sw::in, Sh::in,
        Dx::in, Dy::in, Dw::in, Dh::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    stretch_blit(Source, Dest, Sx, Sy, Sw, Sh, Dx, Dy, Dw, Dh);
    IO = IO0;
").

:- pragma foreign_proc("C",
    masked_blit(Source::in, Dest::in, Sx::in, Sy::in, Dx::in, Dy::in,
        W::in, H::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    masked_blit(Source, Dest, Sx, Sy, Dx, Dy, W, H);
    IO = IO0;
").

:- pragma foreign_proc("C",
    masked_stretch_blit(Source::in, Dest::in, Sx::in, Sy::in, Sw::in, Sh::in,
        Dx::in, Dy::in, Dw::in, Dh::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    masked_stretch_blit(Source, Dest, Sx, Sy, Sw, Sh, Dx, Dy, Dw, Dh);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_sprite(Bmp::in, Sprite::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_sprite(Bmp, Sprite, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    stretch_sprite(Bmp::in, Sprite::in, X::in, Y::in, W::in, H::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    stretch_sprite(Bmp, Sprite, X, Y, W, H);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_sprite_v_flip(Bmp::in, Sprite::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_sprite_v_flip(Bmp, Sprite, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_sprite_h_flip(Bmp::in, Sprite::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_sprite_h_flip(Bmp, Sprite, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_sprite_vh_flip(Bmp::in, Sprite::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_sprite_vh_flip(Bmp, Sprite, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_trans_sprite(Bmp::in, Sprite::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_trans_sprite(Bmp, Sprite, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_lit_sprite(Bmp::in, Sprite::in, X::in, Y::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_lit_sprite(Bmp, Sprite, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_gouraud_sprite(Bmp::in, Sprite::in, X::in, Y::in,
        C1::in, C2::in, C3::in, C4::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_gouraud_sprite(Bmp, Sprite, X, Y, C1, C2, C3, C4);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_character_ex(Bmp::in, Sprite::in, X::in, Y::in,
        Color::in, Bg::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    draw_character_ex(Bmp, Sprite, X, Y, Color, Bg);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rotate_sprite(Bmp::in, Sprite::in, X::in, Y::in, Angle::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rotate_sprite(Bmp, Sprite, X, Y, Angle);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rotate_sprite_v_flip(Bmp::in, Sprite::in, X::in, Y::in, Angle::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rotate_sprite_v_flip(Bmp, Sprite, X, Y, Angle);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rotate_scaled_sprite(Bmp::in, Sprite::in, X::in, Y::in, Angle::in,
        Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rotate_scaled_sprite(Bmp, Sprite, X, Y, Angle, Scale);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rotate_scaled_sprite_v_flip(Bmp::in, Sprite::in, X::in, Y::in, Angle::in,
        Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rotate_scaled_sprite_v_flip(Bmp, Sprite, X, Y, Angle, Scale);
    IO = IO0;
").

:- pragma foreign_proc("C",
    pivot_sprite(Bmp::in, Sprite::in, X::in, Y::in, CX::in, CY::in, Angle::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    pivot_sprite(Bmp, Sprite, X, Y, CX, CY, Angle);
    IO = IO0;
").

:- pragma foreign_proc("C",
    pivot_sprite_v_flip(Bmp::in, Sprite::in, X::in, Y::in, CX::in, CY::in,
        Angle::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    pivot_sprite_v_flip(Bmp, Sprite, X, Y, CX, CY, Angle);
    IO = IO0;
").

:- pragma foreign_proc("C",
    pivot_scaled_sprite(Bmp::in, Sprite::in, X::in, Y::in, CX::in, CY::in,
        Angle::in, Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    pivot_scaled_sprite(Bmp, Sprite, X, Y, CX, CY, Angle, Scale);
    IO = IO0;
").

:- pragma foreign_proc("C",
    pivot_scaled_sprite_v_flip(Bmp::in, Sprite::in, X::in, Y::in,
        CX::in, CY::in, Angle::in, Scale::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    pivot_scaled_sprite_v_flip(Bmp, Sprite, X, Y, CX, CY, Angle, Scale);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
