%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module contains predicates and types that deal with patterns.
%
% TODO: there are a few unimplemented predicates in this module.
% (Search for NYI.)
%
%---------------------------------------------------------------------------%

:- module cairo.pattern.
:- interface.

%---------------------------------------------------------------------------%

    % This type describes how pattern color/alpha will be determined for
    % areas "outside" the pattern's natural area, for example, outside the
    % surface bounds or outside the gradient geometry).
    %
:- type extend
    --->    extend_none
            % Pixels outside of the source pattern are fully transparent.

    ;       extend_repeat
            % The pattern is tiled by repeating.

    ;       extend_reflect
            % The pattern is tiled by reflecting at the edges.

    ;       extend_pad.
            % Pixels outside of the pattern copy the closest pixel from
            % the source.

    % The type of filtering that should be applied when reading pixel values
    % from patterns.
    %
:- type filter
    --->    filter_fast
            % A high-performance filter, with quality similar to
            % filter_nearest.

    ;       filter_good
            % A reasonable-performance filter, with quality similar to
            % filter_bilinear.

    ;       filter_best
            % The highest-quality available.  May not b suitable for
            % interactive use.

    ;       filter_nearest
            % Nearest-neighbour filtering.

    ;       filter_bilinear.
            % Linear interpolation in two dimensions.

    % Values of this type describe the type of a given pattern.
    %
:- type pattern_type
    --->    pattern_type_solid
            % The pattern is a solid (uniform) color.
            % It may be opaque or translucent.

    ;       pattern_type_surface
            % The pattern is a based on a surface (an image).

    ;       pattern_type_linear
            % The pattern is a linear gradient.

    ;       pattern_type_radial
            % The pattern is a radial gradient.

    ;       pattern_type_mesh
            % The pattern is a mesh.

    ;       pattern_type_raster_source.
            % The pattern is a user pattern providing raster data.

%---------------------------------------------------------------------------%

    % pattern.add_color_stop_rgb(Pattern, Offset, Red, Green, Blue, !IO):
    % Adds an opaque color stop to a gradient pattern.
    % Offset specifies the location along the gradient's control vector.
    % Throws a cairo.error/0 exception if Pattern is not a gradient pattern.
    %
:- pred add_color_stop_rgb(pattern::in, float::in, float::in, float::in,
    float::in, io::di, io::uo) is det.

    % pattern.add_color_stop_rgba(Pattern, Offset, Red, Green, Blue, Alpha,
    %   !IO):
    %
    % Adds a translucent color stop to a gradient pattern.
    % The offset specifies the location along the gradient's control vector.
    % Throws a cairo.error/0 exception if Pattern is not a gradient pattern.
    %
:- pred add_color_stop_rgba(pattern::in, float::in, float::in, float::in,
    float::in, float::in, io::di, io::uo) is det.

% TODO: NYI.
% :- pred get_color_stop_count
% :- pred get_color_stop_rgba

    % pattern.create_rgb(Red, Green, Blue, Pattern, !IO):
    % Pattern is a new pattern corresponding to an opaque color.
    % The color components, Red, Green, and Blue are in the range [0, 1].
    % Value outside that range will be clamped.
    %
:- pred create_rgb(float::in, float::in, float::in, pattern::out,
    io::di, io::uo) is det.

    % pattern.create_rgba(Red, Green, Blue, Alpha, Pattern, !IO):
    % Pattern is a new pattern corresponding to a translucent color.
    % The color components, Red, Green, Blue, and Alpha are in the
    % range [0, 1].  Value outside that range will be clamped.
    %
:- pred create_rgba(float::in, float::in, float::in, float::in, pattern::out,
    io::di, io::uo) is det.

% :- pred get_rgba - NYI

    % pattern.create_for_surface(Surface, Pattern, !IO):
    % Pattern is a new pattern for Surface.
    %
:- pred create_for_surface(S::in, pattern::out, io::di, io::uo)
    is det <= surface(S).

    % pattern.create_linear(X0, Y0, X1, Y1, Pattern, !IO):
    % Pattern is a new linear gradient pattern along the line defined by
    % (X0, Y0) and (X1, Y1).
    %
:- pred create_linear(float::in, float::in, float::in, float::in, pattern::out,
    io::di, io::uo) is det.

% :- pred get_linear_points - NYI.

    % pattern.create_radial(Cx0, Cy0, Radius0, Cx1, Cy1, Radius1,
    %   Pattern, !IO):
    %
    % Pattern is a new radial gradient pattern between the two circles defined
    % by (Cx0, Cy0, Radius0) and (Cx1, Cy1, Radius1).
    %
:- pred create_radial(float::in, float::in, float::in,
    float::in, float::in, float::in, pattern::out, io::di, io::uo) is det.

% :- pred get_radial_circles - NYI.

    % pattern.set_extend(Pattern, Extend, !IO):
    % Set the extend mode for Pattern to Extend.
    %
:- pred set_extend(pattern::in, extend::in, io::di, io::uo) is det.

    % pattern.get_extend(Pattern, Extend, !IO):
    % Extend is the current extend mode for Pattern.
    %
:- pred get_extend(pattern::in, extend::out, io::di, io::uo) is det.

    % pattern.set_filter(Pattern, Filter, !IO):
    % Set Filter to be the filter used when resizing Pattern.
    %
:- pred set_filter(pattern::in, filter::in, io::di, io::uo) is det.

    % pattern.get_filter(Pattern, Filter, !IO):
    % Filter is the current filter for Pattern.
    %
:- pred get_filter(pattern::in, filter::out, io::di, io::uo) is det.

    % pattern.set_matrix(Pattern, Matrix, !IO):
    % Set the transformation matrix for Pattern to Matrix.
    %
:- pred set_matrix(pattern::in, matrix::in, io::di, io::uo) is det.

    % pattern.get_matrix(Pattern, Matrix, !IO):
    % Matrix is the current transformation matrix for Pattern.
    %
:- pred get_matrix(pattern::in, matrix::out, io::di, io::uo) is det.

    % pattern.get_type(Pattern, Type, !IO):
    % Type is pattern type of Pattern.
    %
:- pred get_type(pattern::in, pattern_type::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_enum("C", extend/0, [
    extend_none    - "CAIRO_EXTEND_NONE",
    extend_repeat  - "CAIRO_EXTEND_REPEAT",
    extend_reflect - "CAIRO_EXTEND_REFLECT",
    extend_pad     - "CAIRO_EXTEND_PAD"
]).

:- pragma foreign_enum("C", filter/0, [
    filter_fast     - "CAIRO_FILTER_FAST",
    filter_good     - "CAIRO_FILTER_GOOD",
    filter_best     - "CAIRO_FILTER_BEST",
    filter_nearest  - "CAIRO_FILTER_NEAREST",
    filter_bilinear - "CAIRO_FILTER_BILINEAR"
]).

:- pragma foreign_enum("C", pattern_type/0, [
    pattern_type_solid   - "CAIRO_PATTERN_TYPE_SOLID",
    pattern_type_surface - "CAIRO_PATTERN_TYPE_SURFACE",
    pattern_type_linear  - "CAIRO_PATTERN_TYPE_LINEAR",
    pattern_type_radial  - "CAIRO_PATTERN_TYPE_RADIAL",
    pattern_type_mesh    - "CAIRO_PATTERN_TYPE_MESH",
    pattern_type_raster_source - "CAIRO_PATTERN_TYPE_RASTER_SOURCE"
]).

%---------------------------------------------------------------------------%

add_color_stop_rgb(Pattern, Offset, R, G, B, !IO) :-
    add_color_stop_rgb_2(Pattern, Offset, R, G, B, !IO),
    cairo.pattern_status(Pattern, Status, !IO),
    ( if Status = status_success then
        true
    else
        throw(cairo.error("pattern.add_color_stop_rgb/7", Status))
    ).

:- pred add_color_stop_rgb_2(pattern::in, float::in, float::in, float::in,
    float::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    add_color_stop_rgb_2(Pattern::in, Offset::in, R::in, G::in, B::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_add_color_stop_rgb(Pattern->mcairo_raw_pattern,
        Offset, R, G, B);
").

add_color_stop_rgba(Pattern, Offset, R, G, B, A, !IO) :-
    add_color_stop_rgba_2(Pattern, Offset, R, G, B, A, !IO),
    cairo.pattern_status(Pattern, Status, !IO),
    ( if Status = status_success then
        true
    else
        throw(cairo.error("pattern.add_color_stop_rgba/8", Status))
    ).

:- pred add_color_stop_rgba_2(pattern::in, float::in, float::in, float::in,
    float::in, float::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    add_color_stop_rgba_2(Pattern::in, Offset::in,
        R::in, G::in, B::in, A::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_add_color_stop_rgba(Pattern->mcairo_raw_pattern,
        Offset, R, G, B, A);
").

:- pragma foreign_proc("C",
    create_rgb(R::in, G::in, B::in, Pattern::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_t     *raw_pattern;

    raw_pattern = cairo_pattern_create_rgb(R, G, B);
    Pattern = MR_GC_NEW_ATTRIB(MCAIRO_pattern, MR_ALLOC_ID);
    Pattern->mcairo_raw_pattern = raw_pattern;
        MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    create_rgba(R::in, G::in, B::in, A::in, Pattern::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_t     *raw_pattern;

    raw_pattern = cairo_pattern_create_rgba(R, G, B, A);
    Pattern = MR_GC_NEW_ATTRIB(MCAIRO_pattern, MR_ALLOC_ID);
    Pattern->mcairo_raw_pattern = raw_pattern;
        MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    create_for_surface(Surface::in, Pattern::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_t     *raw_pattern;
    raw_pattern = cairo_pattern_create_for_surface(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
    Pattern = MR_GC_NEW_ATTRIB(MCAIRO_pattern, MR_ALLOC_ID);
    Pattern->mcairo_raw_pattern = raw_pattern;
        MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    create_linear(X0::in, Y0::in, X1::in, Y1::in, Pattern::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_t     *raw_pattern;

    raw_pattern = cairo_pattern_create_linear(X0, Y0, X1, Y1);
    Pattern = MR_GC_NEW_ATTRIB(MCAIRO_pattern, MR_ALLOC_ID);
    Pattern->mcairo_raw_pattern = raw_pattern;
        MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    create_radial(Cx0::in, Cy0::in, Radius0::in,
        Cx1::in, Cy1::in, Radius1::in,
        Pattern::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_t     *raw_pattern;

    raw_pattern = cairo_pattern_create_radial(Cx0, Cy0, Radius0,
        Cx1, Cy1, Radius1);

    Pattern = MR_GC_NEW_ATTRIB(MCAIRO_pattern, MR_ALLOC_ID);
    Pattern->mcairo_raw_pattern = raw_pattern;
        MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").


:- pragma foreign_proc("C",
    set_extend(Pattern::in, Extend::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_set_extend(Pattern->mcairo_raw_pattern, Extend);
").

:- pragma foreign_proc("C",
    get_extend(Pattern::in, Extend::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Extend = cairo_pattern_get_extend(Pattern->mcairo_raw_pattern);
").

:- pragma foreign_proc("C",
    set_filter(Pattern::in, Filter::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_set_filter(Pattern->mcairo_raw_pattern, Filter);
").

:- pragma foreign_proc("C",
    get_filter(Pattern::in, Filter::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Filter = cairo_pattern_get_filter(Pattern->mcairo_raw_pattern);
").

:- pragma foreign_proc("C",
    set_matrix(Pattern::in, Matrix::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_pattern_set_matrix(Pattern->mcairo_raw_pattern, Matrix);
").

:- pragma foreign_proc("C",
    get_matrix(Pattern::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Matrix = MR_GC_NEW_ATTRIB(cairo_matrix_t, MR_ALLOC_ID);
    cairo_pattern_get_matrix(Pattern->mcairo_raw_pattern, Matrix);
").

:- pragma foreign_proc("C",
    get_type(Pattern::in, PatternType::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    PatternType = cairo_pattern_get_type(Pattern->mcairo_raw_pattern);
").

%---------------------------------------------------------------------------%
:- end_module cairo.pattern.
%---------------------------------------------------------------------------%
