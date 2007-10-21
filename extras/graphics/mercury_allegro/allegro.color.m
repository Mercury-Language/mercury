%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.color.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.color.
:- interface.

:- import_module allegro.graphics.
:- import_module allegro.palette.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type color == int.

:- pred makecol8(int::in, int::in, int::in, color::out, io::di, io::uo)  is det.
:- pred makecol15(int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol16(int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol24(int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol32(int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makeacol32(int::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol(int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol_depth(color_depth::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makeacol(int::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makeacol_depth(color_depth::in, int::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol15_dither(int::in, int::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred makecol16_dither(int::in, int::in, int::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred getr8(color::in, int::out, io::di, io::uo) is det.
:- pred getg8(color::in, int::out, io::di, io::uo) is det.
:- pred getb8(color::in, int::out, io::di, io::uo) is det.
:- pred getr15(color::in, int::out, io::di, io::uo) is det.
:- pred getg15(color::in, int::out, io::di, io::uo) is det.
:- pred getb15(color::in, int::out, io::di, io::uo) is det.
:- pred getr16(color::in, int::out, io::di, io::uo) is det.
:- pred getg16(color::in, int::out, io::di, io::uo) is det.
:- pred getb16(color::in, int::out, io::di, io::uo) is det.
:- pred getr24(color::in, int::out, io::di, io::uo) is det.
:- pred getg24(color::in, int::out, io::di, io::uo) is det.
:- pred getb24(color::in, int::out, io::di, io::uo) is det.
:- pred getr32(color::in, int::out, io::di, io::uo) is det.
:- pred getg32(color::in, int::out, io::di, io::uo) is det.
:- pred getb32(color::in, int::out, io::di, io::uo) is det.
:- pred geta32(color::in, int::out, io::di, io::uo) is det.
:- pred getr(color::in, int::out, io::di, io::uo) is det.
:- pred getg(color::in, int::out, io::di, io::uo) is det.
:- pred getb(color::in, int::out, io::di, io::uo) is det.
:- pred geta(color::in, int::out, io::di, io::uo) is det.
:- pred getr_depth(color_depth::in, color::in, int::out, io::di, io::uo) is det.
:- pred getg_depth(color_depth::in, color::in, int::out, io::di, io::uo) is det.
:- pred getb_depth(color_depth::in, color::in, int::out, io::di, io::uo) is det.
:- pred geta_depth(color_depth::in, color::in, int::out, io::di, io::uo) is det.
:- pred palette_color(index::in, color::out, io::di, io::uo) is det.
:- func mask_color_8 = int.
:- func mask_color_15 = int.
:- func mask_color_16 = int.
:- func mask_color_24 = int.
:- func mask_color_32 = int.

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
    makecol8(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol8(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol15(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol15(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol16(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol16(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol24(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol24(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol32(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol32(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makeacol32(R::in, G::in, B::in, A::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makeacol32(R, G, B, A);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    makecol(R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol(R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol_depth(Depth::in, R::in, G::in, B::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol_depth(Depth, R, G, B);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    makeacol(R::in, G::in, B::in, A::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makeacol(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makeacol_depth(Depth::in, R::in, G::in, B::in, A::in, Color::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makeacol_depth(Depth, R, G, B, A);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    makecol15_dither(R::in, G::in, B::in, X::in, Y::in, Color::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol15_dither(R, G, B, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    makecol16_dither(R::in, G::in, B::in, X::in, Y::in, Color::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = makecol16_dither(R, G, B, X, Y);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr8(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr8(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg8(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg8(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb8(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb8(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr15(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr15(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg15(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg15(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb15(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb15(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr16(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr16(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg16(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg16(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb16(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb16(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr24(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr24(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg24(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg24(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb24(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb24(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr32(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr32(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg32(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg32(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb32(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb32(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    geta32(Color::in, A::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    A = geta32(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr(Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg(Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getr(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb(Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getr(Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    geta(Color::in, A::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    A = geta(Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getr_depth(Depth::in, Color::in, R::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = getr_depth(Depth, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getg_depth(Depth::in, Color::in, G::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    G = getg_depth(Depth, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getb_depth(Depth::in, Color::in, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    B = getb_depth(Depth, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    geta_depth(Depth::in, Color::in, A::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    A = geta_depth(Depth, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    palette_color(Index::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = palette_color[Index];
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    mask_color_8 = (Mask::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Mask = MASK_COLOR_8;
").

:- pragma foreign_proc("C",
    mask_color_15 = (Mask::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Mask = MASK_COLOR_15;
").

:- pragma foreign_proc("C",
    mask_color_16 = (Mask::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Mask = MASK_COLOR_16;
").

:- pragma foreign_proc("C",
    mask_color_24 = (Mask::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Mask = MASK_COLOR_24;
").

:- pragma foreign_proc("C",
    mask_color_32 = (Mask::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Mask = MASK_COLOR_32;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
