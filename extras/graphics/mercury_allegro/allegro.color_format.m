%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.color_format.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.color_format.
:- interface.

:- import_module allegro.palette.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type rgb_map.

:- pred bestfit_color(palette::in, int::in, int::in, int::in, int::out, io::di, io::uo) is det.
:- pred rgb_map(rgb_map::out, io::di, io::uo) is det.
:- pred set_rgb_map(rgb_map::in, io::di, io::uo) is det.
:- pred create_rgb_table(rgb_map::out, palette::in, io::di, io::uo) is det.
:- pred hsv_to_rgb(float::in, float::in, float::in, int::out, int::out, int::out) is det.
:- pred rgb_to_hsv(int::in, int::in, int::in, float::out, float::out, float::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", rgb_map, "RGB_MAP *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    bestfit_color(Palette::in, R::in, G::in, B::in, Index::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Index = bestfit_color(Palette, R, G, B);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rgb_map(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Get = rgb_map;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_rgb_map(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    rgb_map = Set;
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_rgb_table(Map::out, Palette::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Map = MR_GC_NEW(RGB_MAP);
    create_rgb_table(Map, Palette, NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    hsv_to_rgb(H::in, S::in, V::in, R::out, G::out, B::out),
    [will_not_call_mercury, promise_pure],
"
    int R0, G0, B0;
    hsv_to_rgb(H, S, V, &R0, &G0, &B0);
    R = R0;
    G = G0;
    B = B0;
").

:- pragma foreign_proc("C",
    rgb_to_hsv(R::in, G::in, B::in, H::out, S::out, V::out),
    [will_not_call_mercury, promise_pure],
"
    float H0, S0, V0;
    rgb_to_hsv(R, G, B, &H0, &S0, &V0);
    H = H0, S = S0, V = V0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
