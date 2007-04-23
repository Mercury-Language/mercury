%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.compiled.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.compiled.
:- interface.

:- import_module allegro.bitmap.
:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type compiled_sprite.

:- pred get_compiled_sprite(bitmap::in, bool::in, maybe(compiled_sprite)::out, io::di, io::uo) is det.
:- pred destroy_compiled_sprite(compiled_sprite::in, io::di, io::uo) is det.
:- pred draw_compiled_sprite(bitmap::in, compiled_sprite::in, int::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", compiled_sprite, "COMPILED_SPRITE *").

:- pragma foreign_proc("C",
    get_compiled_sprite(Sprite::in, Planar::in, MaybeCompiled::out,
        IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    COMPILED_SPRITE *Compiled = get_compiled_sprite(Sprite, Planar);
    if (Compiled) {
        MaybeCompiled = _mal_make_maybe_compiled_sprite_yes(Compiled);
    } else {
        MaybeCompiled = _mal_make_maybe_compiled_sprite_no();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_compiled_sprite(Compiled::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    destroy_compiled_sprite(Compiled);
    IO = IO0;
").

:- pragma foreign_proc("C",
    draw_compiled_sprite(Bitmap::in, Compiled::in, X::in, Y::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    draw_compiled_sprite(Bitmap, Compiled, X, Y);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func make_maybe_compiled_sprite_yes(compiled_sprite) =
    maybe(compiled_sprite).
:- pragma export(make_maybe_compiled_sprite_yes(in) = out,
    "_mal_make_maybe_compiled_sprite_yes").
make_maybe_compiled_sprite_yes(Compiled) = yes(Compiled).

:- func make_maybe_compiled_sprite_no = maybe(compiled_sprite).
:- pragma export(make_maybe_compiled_sprite_no = out,
    "_mal_make_maybe_compiled_sprite_no").
make_maybe_compiled_sprite_no = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
