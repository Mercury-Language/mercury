%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.palette.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.palette.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type palette.

:- func pal_size = int.
:- pred set_color(index::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_palette(palette::in, io::di, io::uo) is det.
:- pred set_palette_range(palette::in, int::in, int::in, wait_for_vsync::in, io::di, io::uo) is det.
:- pred get_color(index::in, int::out, int::out, int::out, io::di, io::uo) is det.
:- pred get_palette(palette::out, io::di, io::uo) is det.
:- pred get_palette_range(palette::out, int::in, int::in, io::di, io::uo) is det.
:- pred fade_interpolate(palette::in, palette::in, palette::out, int::in, int::in, int::in) is det.
:- pred fade_from_range(palette::in, palette::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred fade_in_range(palette::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred fade_out_range(int::in, int::in, int::in, io::di, io::uo) is det.
:- pred fade_from(palette::in, palette::in, int::in, io::di, io::uo) is det.
:- pred fade_in(palette::in, int::in, io::di, io::uo) is det.
:- pred fade_out(int::in, io::di, io::uo) is det.
:- pred select_palette(palette::in, io::di, io::uo) is det.
:- pred unselect_palette(io::di, io::uo) is det.
:- pred generate_332_palette(palette::out) is det.
% incomplete
% :- pred generate_optimized_palette(bitmap::in, list(optimize_palette_spec)::in, optimize_palette_result::out, io::di, io::uo) is det.
:- func default_palette = (palette::out) is det.
:- func black_palette = (palette::out) is det.
:- func desktop_palette = (palette::out) is det.

:- type index == int.

:- type wait_for_vsync
    --->    wait_for_vsync
    ;       dont_wait_for_vsync.

:- type optimize_palette_spec
    --->    free
    ;       unusable
    ;       fixed(int, int, int).

:- type optimize_palette_result
    --->    ok(palette, int)
    ;       not_truecolor_bitmap
    ;       error.

    % Additions to C API.
    %
:- pred new_palette(palette::out, io::di, io::uo) is det.
:- pred get_palette_entry(palette::in, int::in, int::out, int::out, int::out,
    io::di, io::uo) is det.
:- pred set_palette_entry(palette::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

:- pragma foreign_type("C", palette, "RGB *", [can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    pal_size = (PalSize::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PalSize = PAL_SIZE;
").

:- pragma foreign_proc("C",
    set_color(Index::in, R::in, G::in, B::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    RGB rgb = {R,G,B};
    set_color(Index, &rgb);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_palette(Palette::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_palette(Palette);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_palette_range(Palette::in, From::in, To::in, Vsync::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_palette_range(Palette, From, To, Vsync);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_color(Index::in, R::out, G::out, B::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    RGB rgb;
    get_color(Index, &rgb);
    R = rgb.r;
    G = rgb.g;
    B = rgb.b;
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_palette(Palette::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    get_palette(Palette);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_palette_range(Palette::out, From::in, To::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    get_palette_range(Palette, From, To);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_interpolate(Source::in, Dest::in, Output::out, Pos::in, From::in,
        To::in),
    [will_not_call_mercury, promise_pure],
"
    Output = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    fade_interpolate(Source, Dest, Output, Pos, From, To);
").

:- pragma foreign_proc("C",
    fade_from_range(Source::in, Dest::in, Speed::in, From::in, To::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_from_range(Source, Dest, Speed, From, To);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_in_range(P::in, Speed::in, From::in, To::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_in_range(P, Speed, From, To);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_out_range(Speed::in, From::in, To::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_out_range(Speed, From, To);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_from(Source::in, Dest::in, Speed::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_from(Source, Dest, Speed);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_in(P::in, Speed::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_in(P, Speed);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fade_out(P::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fade_out(P);
    IO = IO0;
").

:- pragma foreign_proc("C",
    select_palette(Palette::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    select_palette(Palette);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unselect_palette(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    unselect_palette();
    IO = IO0;
").

:- pragma foreign_proc("C",
    generate_332_palette(Palette::out),
    [will_not_call_mercury, promise_pure],
"
    Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    generate_332_palette(Palette);
").

%-----------------------------------------------------------------------------%

% :- pragma promise_pure(generate_optimized_palette/5).
% 
% generate_optimized_palette(Bitmap, Spec, Result, !IO) :-
%     (if
%         list.length(Spec) = 256
%     then
%         impure spec_to_rsvd(Spec, Palette0, Rsvd),
%         generate_optimized_palette_2(Bitmap, Spec, Palette0, Rsvd, Palette, N),
%         ( N < 0 ->
%             Result = error
%         ; N = 0 ->
%             Result = not_truecolor_bitmap
%         ;
%             Result = ok(Palette, N)
%         )
%     else
%         Result = error
%     ).

:- type rsvd.
:- pragma foreign_type("C", rsvd, "char *", [can_pass_as_mercury_type]).

:- impure pred spec_to_rsvd(list(optimize_palette_spec)::in, palette::out,
    rsvd::out) is det.

spec_to_rsvd(Specs, Palette, Rsvd) :-
    init_rsvd(Palette, Rsvd),
    impure spec_to_rsvd_2(Specs, 0, Palette, Rsvd).

:- impure pred spec_to_rsvd_2(list(optimize_palette_spec)::in, int::in,
    palette::in, rsvd::in) is det.

spec_to_rsvd_2([], _, _, _).
spec_to_rsvd_2([Spec|Specs], Index, P, R) :-
    (
        Spec = free,
        impure spec_to_rsvd_2(Specs, Index+1, P, R)
    ;
        Spec = unusable,
        impure set_rsvd_unusable(Index, R),
        impure spec_to_rsvd_2(Specs, Index+1, P, R)
    ;
        Spec = fixed(RR, G, B),
        impure set_rsvd_fixed(Index, RR, G, B, P, R),
        impure spec_to_rsvd_2(Specs, Index+1, P, R)
    ).

:- pred init_rsvd(palette::out, rsvd::out) is det.
:- pragma foreign_proc("C",
    init_rsvd(Palette::out, Rsvd::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    Rsvd = MR_GC_NEW_ARRAY(char, PAL_SIZE);
    memset(Palette, 0, sizeof(PALETTE));
    memset(Rsvd, 0, sizeof(char) * PAL_SIZE);
").

:- impure pred set_rsvd_unusable(int::in, rsvd::in) is det.
:- pragma foreign_proc("C",
    set_rsvd_unusable(Index::in, Rsvd::in),
    [will_not_call_mercury, thread_safe],
"
    Rsvd[Index] = -1;
").

:- impure pred set_rsvd_fixed(int::in, int::in, int::in, int::in, palette::in,
    rsvd::in) is det.
:- pragma foreign_proc("C",
    set_rsvd_fixed(Index::in, R::in, G::in, B::in, Palette::in, Rsvd::in),
    [will_not_call_mercury, thread_safe],
"
    Palette[Index].r = R;
    Palette[Index].g = G;
    Palette[Index].b = B;
    Rsvd[Index] = 1;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    default_palette = (Palette::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Palette = default_palette;
").

:- pragma foreign_proc("C",
    black_palette = (Palette::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Palette = black_palette;
").

:- pragma foreign_proc("C",
    desktop_palette = (Palette::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Palette = desktop_palette;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_palette(Palette::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_palette_entry(Palette::in, Index::in, R::out, G::out, B::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    R = Palette[Index].r;
    G = Palette[Index].g;
    B = Palette[Index].b;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_palette_entry(Palette::in, Index::in, R::in, G::in, B::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Palette[Index].r = R;
    Palette[Index].g = G;
    Palette[Index].b = B;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
