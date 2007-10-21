%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.transparency.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.transparency.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.palette.
:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred drawing_mode(drawing_mode::in, io::di, io::uo) is det.
:- pred xor_mode(bool::in, io::di, io::uo) is det.
:- pred solid_mode(io::di, io::uo) is det.
:- pred color_map(color_map::out, io::di, io::uo) is det.
:- pred set_color_map(color_map::in, io::di, io::uo) is det.
:- pred create_trans_table(color_map::out, palette::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred create_light_table(color_map::out, palette::in, int::in, int::in, int::in, io::di, io::uo) is det.
% :- pred create_color_table(color_map::out, palette::in, blend_func::in(blend_func), io::di, io::uo) is det.
:- pred create_blender_table(color_map::out, palette::in, io::di, io::uo) is det.
:- pred set_trans_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_alpha_blender(io::di, io::uo) is det.
:- pred set_write_alpha_blender(io::di, io::uo) is det.
:- pred set_add_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_burn_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_color_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_difference_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_dissolve_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_dodge_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_hue_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_invert_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_luminance_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_multiply_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_saturation_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_screen_blender(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
% set_blender_mode
% set_blender_mode_ex

:- type drawing_mode
    --->    solid
    ;       xor
    ;       copy_pattern(bitmap, int, int)
    ;       masked_pattern(bitmap, int, int)
    ;       trans.

:- type color_map.

:- type blend_func == (pred(palette, int, int, int, int, int)).
:- inst blend_func == (pred(in,      in,  in,  out, out, out) is det).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", color_map, "COLOR_MAP *",
    [can_pass_as_mercury_type]).

drawing_mode(solid, !IO) :-
    solid_mode(!IO).
drawing_mode(xor, !IO) :-
    xor_mode(yes, !IO).
drawing_mode(copy_pattern(Pattern, XAnchor, YAnchor), !IO) :-
    drawing_mode_copy_pattern(Pattern, XAnchor, YAnchor, !IO).
drawing_mode(masked_pattern(Pattern, XAnchor, YAnchor), !IO) :-
    drawing_mode_masked_pattern(Pattern, XAnchor, YAnchor, !IO).
drawing_mode(trans, !IO) :-
    drawing_mode_trans(!IO).

:- pred drawing_mode_copy_pattern(bitmap::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    drawing_mode_copy_pattern(Pattern::in, XAnchor::in, YAnchor::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    drawing_mode(DRAW_MODE_COPY_PATTERN, Pattern, XAnchor, YAnchor);
    IO = IO0;
").

:- pred drawing_mode_masked_pattern(bitmap::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    drawing_mode_masked_pattern(Pattern::in, XAnchor::in, YAnchor::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    drawing_mode(DRAW_MODE_MASKED_PATTERN, Pattern, XAnchor, YAnchor);
    IO = IO0;
").

:- pred drawing_mode_trans(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    drawing_mode_trans(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    drawing_mode(DRAW_MODE_TRANS, NULL, 0, 0);
    IO = IO0;
").

:- pragma foreign_proc("C",
    xor_mode(On::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    xor_mode(On);
    IO = IO0;
").

:- pragma foreign_proc("C",
    solid_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    solid_mode();
    IO = IO0;
").

:- pragma foreign_proc("C",
    color_map(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Get = color_map;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_color_map(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    color_map = Set;
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_trans_table(Colormap::out, Palette::in, R::in, G::in, B::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Colormap = MR_GC_NEW(COLOR_MAP);
    create_trans_table(Colormap, Palette, R, G, B, NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_light_table(Colormap::out, Palette::in, R::in, G::in, B::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Colormap = MR_GC_NEW(COLOR_MAP);
    create_light_table(Colormap, Palette, R, G, B, NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_blender_table(Colormap::out, Palette::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Colormap = MR_GC_NEW(COLOR_MAP);
    create_blender_table(Colormap, Palette, NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_trans_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_trans_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_alpha_blender(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_alpha_blender();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_write_alpha_blender(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_write_alpha_blender();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_add_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_add_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_burn_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_burn_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_color_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_color_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_difference_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_difference_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_dissolve_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_dissolve_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_dodge_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_dodge_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_hue_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_hue_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_invert_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_invert_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_luminance_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_luminance_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_multiply_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_multiply_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_saturation_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_saturation_blender(R, G, B, A);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_screen_blender(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_screen_blender(R, G, B, A);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
