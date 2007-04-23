%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.rle.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.rle.
:- interface.

:- import_module allegro.bitmap.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type rle_sprite.

:- pred get_rle_sprite(bitmap::in, maybe(rle_sprite)::out, io::di, io::uo) is det.
:- pred destroy_rle_sprite(rle_sprite::in, io::di, io::uo) is det.
:- pred draw_rle_sprite(bitmap::in, rle_sprite::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_trans_rle_sprite(bitmap::in, rle_sprite::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_lit_rle_sprite(bitmap::in, rle_sprite::in, int::in, int::in, int::in, io::di, io::uo) is det.

    % Additions to C API.
    %
:- func rle_sprite_w(rle_sprite) = int.
:- func rle_sprite_h(rle_sprite) = int.
:- pred rle_sprite_size(rle_sprite::in, int::out, int::out) is det.
:- func rle_sprite_color_depth(rle_sprite) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", rle_sprite, "RLE_SPRITE *").

:- pragma foreign_proc("C",
    get_rle_sprite(Sprite::in, MaybeRLE::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    RLE_SPRITE *RLE = get_rle_sprite(Sprite);
    if (RLE) {
        MaybeRLE = _mal_make_maybe_rle_sprite_yes(RLE);
    } else {
        MaybeRLE = _mal_make_maybe_rle_sprite_no();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_rle_sprite(RLE::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    destroy_rle_sprite(RLE);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_rle_sprite(Bitmap::in, RLE::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    draw_rle_sprite(Bitmap, RLE, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_trans_rle_sprite(Bitmap::in, RLE::in, X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    draw_trans_rle_sprite(Bitmap, RLE, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_lit_rle_sprite(Bitmap::in, RLE::in, X::in, Y::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    draw_lit_rle_sprite(Bitmap, RLE, X, Y, Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rle_sprite_w(RLE::in) = (Get::out),
    [will_not_call_mercury, promise_pure],
"
    Get = RLE->w;
").

:- pragma foreign_proc("C",
    rle_sprite_h(RLE::in) = (Get::out),
    [will_not_call_mercury, promise_pure],
"
    Get = RLE->h;
").

:- pragma foreign_proc("C",
    rle_sprite_size(RLE::in, Width::out, Height::out),
    [will_not_call_mercury, promise_pure],
"
    Width = RLE->w;
    Height = RLE->h;
").

:- pragma foreign_proc("C",
    rle_sprite_color_depth(RLE::in) = (ColorDepth::out),
    [will_not_call_mercury, promise_pure],
"
    ColorDepth = RLE->color_depth;
").

%-----------------------------------------------------------------------------%

:- func make_maybe_rle_sprite_yes(rle_sprite) = maybe(rle_sprite).
:- func make_maybe_rle_sprite_no = maybe(rle_sprite).

:- pragma export(make_maybe_rle_sprite_yes(in) = out,
    "_mal_make_maybe_rle_sprite_yes").
:- pragma export(make_maybe_rle_sprite_no = out,
    "_mal_make_maybe_rle_sprite_no").

make_maybe_rle_sprite_yes(RLE) = yes(RLE).
make_maybe_rle_sprite_no = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
