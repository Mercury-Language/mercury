%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% A Mercury binding to the cairo 2D graphics library.
%
% TODO: scaled fonts
%
%---------------------------------------------------------------------------%

:- module cairo.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- include_module font_options.
:- include_module image.
:- include_module matrix.
:- include_module path.
:- include_module pattern.
:- include_module pdf.
:- include_module png.
:- include_module ps.
:- include_module recording.
:- include_module region.
:- include_module surface.
:- include_module svg.
:- include_module text.
:- include_module transformations.

%---------------------------------------------------------------------------%
%
% Basic cairo types
%

    % Each backend is an instance of this type class.
    %
:- typeclass surface(S) where [].

    % Each font backend is an instance of this type class.
    %
:- typeclass font_face(F) where [].

    % The cairo drawing context.
    %
:- type cairo.context(S).   % <= surface(S).

:- type cairo.pattern.

:- type cairo.matrix.

:- type cairo.path.

:- type cairo.font_options.

:- type cairo.region.

    % TODO: scaled fonts are NYI.
    %
:- type cairo.scaled_font(F).   % <= font_face(F).

    % Values of this type describe that content that a surface will contain.
    %
:- type content
    --->    content_color
            % The surface will hold color content only.

    ;       content_alpha
            % The surface will hold alpha content only.

    ;       content_color_alpha.
            % The surface will hold color and alpha content.

    % Values of this type identify the memory format of image data.
    % (Please see the cairo documentation for a discussion of endianess issues
    % in relation to pixel data.)
    %
:- type format
    --->    format_argb32
            % Each pixel is a 32-bit quantity, with alpha in the upper 8 bits,
            % then red, then green, then blue.

    ;       format_rgb24
            % Each pixel is a 32-bit quantity, with the upper 8 bits unused.
            % Red, Green, and Blue are stored in the remaining 24 bits in that
            % order.

    ;       format_a8
            % Each pixel is a 8-bit quantity holding an alpha value.

    ;       format_a1
            % Each pixel is a 1-bit quantity holding an alpha value.

    ;       format_rgb16_565.
            % Each pixel is a 16-bit quantity with red in the upper 5 bits,
            % then green in the middle 6 bits, and blue in the lower 5 bits.

    % A rectangle with integer coordinates.
    %
:- type rectangle
    --->    rectangle(
                rect_x :: int,      % X coordinate of the LHS.
                rect_y :: int,      % Y coordinate of the top.
                rect_width  :: int, % Width.
                rect_height :: int  % Height
            ).

:- type rectangle_f
    --->    rectangle_f(
                rectf_x :: float,
                rectf_y :: float,
                rectf_width :: float,
                rectf_height :: float
            ).

%---------------------------------------------------------------------------%
%
% Error handling
%

    % The cairo status.
    %
    % XXX This type is liable to become out of date as status codes are added
    % to the cairo API. "Complete" switches on status values may not actually
    % cover some possibilities. Attempting to print status values which are not
    % enumerated here will result in a crash. And so on.
    %
:- type cairo.status
    --->    status_success
    ;       status_no_memory
    ;       status_invalid_restore
    ;       status_invalid_pop_group
    ;       status_no_current_point
    ;       status_invalid_matrix
    ;       status_invalid_status
    ;       status_null_pointer
    ;       status_invalid_string
    ;       status_invalid_path_data
    ;       status_read_error
    ;       status_write_error
    ;       status_surface_finished
    ;       status_surface_type_mismatch
    ;       status_pattern_type_mismatch
    ;       status_invalid_content
    ;       status_invalid_format
    ;       status_invalid_visual
    ;       status_file_not_found
    ;       status_invalid_dash
    ;       status_invalid_dsc_comment
    ;       status_invalid_index
    ;       status_clip_not_representable
    ;       status_temp_file_error
    ;       status_invalid_stride
    ;       status_font_type_mismatch
    ;       status_user_font_immutable
    ;       status_user_font_error
    ;       status_negative_count
    ;       status_invalid_clusters
    ;       status_invalid_slant
    ;       status_invalid_weight
    ;       status_invalid_size
    ;       status_user_font_not_implemented
    ;       status_device_type_mismatch
    ;       status_device_error
    ;       status_invalid_mesh_construction
    ;       status_device_finished
    ;       status_jbig2_global_missing.

    % Exceptions of this type are thrown to indicate a cairo error.
    %
:- type cairo.error
    --->    cairo.error(string, cairo.status).

    % Exceptions of this type are thrown if an attempt is made to create
    % a surface that is not supported by the implementation.
    %
:- type cairo.unsupported_surface_error
    --->    cairo.unsupported_surface_error(string).

%---------------------------------------------------------------------------%
%
% Operations on cairo contexts
%

    % cairo.create_context(TargetSurface, Context, !IO):
    % Context is a new cairo context with all graphics state parameters set to
    % default values and with TargetSurface as a target surface.
    %
:- pred create_context(S::in, context(S)::out,
    io::di, io::uo) is det <= surface(S).

    % cairo.save(Context, !IO):
    % Make a copy of the current state of Context and save it on an internal
    % stack of saved states for Context.
    %
:- pred save(context(S)::in, io::di, io::uo) is det.

    % cairo.restore(Context, !IO):
    % Restore Context to the state saved by a preceding call to cairo.save/3
    % and remove that state from the stack of saved states.
    %
:- pred restore(context(S)::in, io::di, io::uo) is det.

    % cairo.get_target(Context, Surface, !IO):
    % Surface is the target surface for Context as passed to
    % cairo.create_context/4.
    %
:- pred get_target(context(S)::in, S::out,
    io::di, io::uo) is det <= surface(S).

    % cairo.push_group(Context, !IO):
    %
:- pred push_group(context(S)::in, io::di, io::uo) is det.

    % cairo.push_group_with_content(Context, ContentType, !IO):
    %
:- pred push_group_with_content(context(S)::in, content::in,
    io::di, io::uo) is det.

:- pred pop_group(context(S)::in, pattern::out, io::di, io::uo) is det.

:- pred pop_group_to_source(context(S)::in, io::di, io::uo) is det.

:- pred get_group_target(context(S)::in, T::out, io::di, io::uo) is det
    <= surface(T).

:- pred set_source_rgb(context(S)::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

:- pred set_source_rgba(context(S)::in, float::in, float::in, float::in,
    float::in, io::di, io::uo) is det.

    % cairo.set_source(Context, Pattern, !IO):
    % Set the current source pattern for Context to Pattern.
    %
:- pred set_source(context(S)::in, pattern::in, io::di, io::uo) is det.

    % cairo.set_source_surface(Context, Surface, X, Y, !IO):
    % Create a pattern from Surface and make it the current source pattern
    % for Context.  (X, Y) is the user-space coordinate at which the surface
    % origin should appear.
    %
:- pred set_source_surface(context(S)::in, T::in, float::in, float::in,
    io::di, io::uo) is det <= surface(T).

    % cairo.get_source(Context, Pattern, !IO):
    % Pattern is the current source pattern for Context.
    %
:- pred get_source(context(S)::in, pattern::out, io::di, io::uo) is det.

:- type antialias
    --->    antialias_default
            % Use the default antialiasing for the subsystem and target
            % device.

    ;       antialias_none
            % Use a bilevel alpha mask.

    ;       antialias_gray
            % Perform single-color antialiasing.

    ;       antialias_subpixel.
            % Perform antialiasing by taking advantage of the order of
            % subpixel elements on devices such as LCD panel.

    % cairo.set_antialias(Context, AntiAlias, !IO):
    % Set the antialiasing mode of Context to AntiAlias.
    %
:- pred set_antialias(context(S)::in, antialias::in, io::di, io::uo) is det.

    % cairo.get_antialias(Context, AntiAlias, !IO):
    % AntiAlias is the current antialiasing mode for Context.
    %
:- pred get_antialias(context(S)::in, antialias::out, io::di, io::uo) is det.

    % cairo.set_dash(Context, Dashes, Offset, !IO):
    % Dashes is the dash pattern to be used by cairo.stroke/3.
    % Throws a cairo.error/0 exception if an element of Dashes is negative,
    % or all of the elements of Dashes are zero.
    %
:- pred set_dash(context(S)::in, list(float)::in, float::in,
    io::di, io::uo) is det.

    % cairo.get_dash_count(Context, Count !IO):
    % Count is the number of dashes in the current dash pattern for
    % Context.  (Count is 0, if there is no current dash pattern.)
    %
:- pred get_dash_count(context(S)::in, int::out, io::di, io::uo) is det.

%:- pred get_dash(...)  - NYI.

    % The fill rule is used to select how paths are filled.
    %
:- type fill_rule
    --->    fill_rule_winding
    ;       fill_rule_even_odd.

    % cairo.set_fill_rule(Context, FillRule, !IO):
    % Set the current fill rule for Context to FillRule.
    %
:- pred set_fill_rule(context(S)::in, fill_rule::in, io::di, io::uo) is det.

    % cairo.get_fill_rule(Context, FillRule, !IO):
    % FillRule is the current file rule for Context.
    %
:- pred get_fill_rule(context(S)::in, fill_rule::out, io::di, io::uo) is det.

    % Values of this type specify how to render the endpoints of the path when
    % stroking.
    %
:- type line_cap
    --->    line_cap_butt
            % Start(stop) the line exactly at the start(end) point.

    ;       line_cap_round
            % Use a round ending, the center of the circle is the end point.

    ;       line_cap_square.
            % Use squared ending, the center of the square is the end point.

    % cairo.set_line_cap(Context, LineCap, !IO):
    % Set the line cap style for Context to LineCap.
    %
:- pred set_line_cap(context(S)::in, line_cap::in, io::di, io::uo) is det.

    % cairo.get_line_cap(Context, LineCap, !IO):
    % LineCap is the current line cap style for Context.
    %
:- pred get_line_cap(context(S)::in, line_cap::out, io::di, io::uo) is det.

    % Values of this type specify how to render the junction of two lines
    % when stroking.
    %
:- type line_join
    --->    line_join_miter
            % Use a sharp (angled) corner.

    ;       line_join_round
            % Use a rounded join, the center of the circle is the joint point.

    ;       line_join_bevel.
            % Use a cut-off join, the join is cut off at half the line width
            % from the joint point.

    % cairo.set_line_join(Context, LineJoin, !IO):
    % Set the line join style for Context to LineJoin.
    %
:- pred set_line_join(context(S)::in, line_join::in, io::di, io::uo) is det.

    % cairo.get_line_join(Context, LineJoin, !IO):
    % LineJoin is the current line join style for Context.
    %
:- pred get_line_join(context(S)::in, line_join::out, io::di, io::uo) is det.

    % cairo.set_line_width(Context, Width, !IO):
    % Set the line Width for Context to Width.
    %
:- pred set_line_width(context(S)::in, float::in, io::di, io::uo) is det.

    % cairo.get_line_width(Context, Width, !IO):
    % Width is the current line width for Context.
    %
:- pred get_line_width(context(S)::in, float::out, io::di, io::uo) is det.

    % cairo.set_miter_limit(Context, Limit, !IO):
    % Set the miter limit for Context to Limit.
    %
:- pred set_miter_limit(context(S)::in, float::in, io::di, io::uo) is det.

    % cairo.get_miter_limit(Context, Limit, !IO):
    % Limit is the miter limit for Context.
    %
:- pred get_miter_limit(context(S)::in, float::out, io::di, io::uo) is det.

    % Values of this type specify the compositing operator used for drawing
    % operations (See: <http://cairographics.org/operators/> for details.)
    %
:- type operator
    --->    operator_clear

    ;       operator_source
    ;       operator_over
    ;       operator_in
    ;       operator_out
    ;       operator_atop

    ;       operator_dest
    ;       operator_dest_over
    ;       operator_dest_in
    ;       operator_dest_out
    ;       operator_dest_atop

    ;       operator_xor
    ;       operator_add
    ;       operator_saturate

    ;       operator_multiply
    ;       operator_screen
    ;       operator_overlay
    ;       operator_darken
    ;       operator_lighten
    ;       operator_color_dodge
    ;       operator_color_burn
    ;       operator_hard_light
    ;       operator_soft_light
    ;       operator_difference
    ;       operator_exclusion
    ;       operator_hsl_hue
    ;       operator_hsl_saturation
    ;       operator_hsl_color
    ;       operator_hsl_luminosity.

    % cairo.set_operator(Context, Operator, !IO):
    % Set the compositing operator for Context to Operator.
    %
:- pred set_operator(context(S)::in, operator::in, io::di, io::uo) is det.

    % cairo.get_operator(Context, Operator, !IO):
    % Operator is the current compositing operator for Context.
    %
:- pred get_operator(context(S)::in, operator::out, io::di, io::uo) is det.

    % cairo.set_tolerance(Context, Tolerance, !IO):
    %
:- pred set_tolerance(context(S)::in, float::in, io::di, io::uo) is det.

    % cairo.get_tolerance(Context, Tolerance, !IO):
    %
:- pred get_tolerance(context(S)::in, float::out, io::di, io::uo) is det.

    % cairo.clip(Context, !IO):
    % Establishes a new clip region by intersecting the current clip region
    % with the current path as it would be filled by cairo.fill/3 and according
    % to the current fill rule.
    % The current path will be cleared from Context.
    %
:- pred clip(context(S)::in, io::di, io::uo) is det.

    % cairo.clip_preserve(Context, !IO):
    % As above, but do not clear the current path from Context.
    %
:- pred clip_preserve(context(S)::in, io::di, io::uo) is det.

    % cairo.clip_extents(Context, Left, Top, Right, Bottom, !IO):
    % Compute a bounding box in user coordinates covering the area inside the
    % current clip for Context.
    %
:- pred clip_extents(context(S)::in, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

    % cairo.in_clip(Context, X, Y Result, !IO):
    % Result is "yes" if the point (X, Y) is inside the area that
    % would be visible through the current clip for Context.
    %
:- pred in_clip(context(S)::in, float::in, float::in, bool::out,
    io::di, io::uo) is det.

    % cairo.reset_clip(Context, !IO):
    % Reset the current clip region to its original, unrestricted state.
    %
:- pred reset_clip(context(T)::in, io::di, io::uo) is det.

    % cairo.fill(Context, !IO):
    % Fill the current path according to the current fill rule.
    % (Each sub-path is implicitly closed before being filled.)
    % The current path for Context will be cleared.
    %
:- pred fill(context(S)::in, io::di, io::uo) is det.

    % cairo.fill_preserve(Context, !IO):
    % As above, but preserve the current path for Context.
    %
:- pred fill_preserve(context(T)::in, io::di, io::uo) is det.

    % cairo.fill_extents(Context, Left, Top, Right, Bottom, !IO):
    % Compute a bounding box in user coordinates covering the area that would
    % be affected, (the "inked" area), by a cairo.fill/3 operation given the
    % current path and fill parameters. If the current path is empty, returns
    % an empty rectangle ((0,0), (0,0)). Surface dimensions and clipping are
    % not taken into account.
    %
:- pred fill_extents(context(S)::in, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

    % cairo.in_fill(Context, X, Y, Result, !IO):
    % Result is "yes" if the coordinate (X, Y) is inside the area that
    % would be affected by a cairo.fill/3 operation given the current
    % path and filling parameters.  Result is "no" otherwise.
    %
:- pred in_fill(context(S)::in, float::in, float::in, bool::out,
    io::di, io::uo) is det.

    % cairo.mask(Context, Pattern, !IO):
    % Paint the current source using the alpha channel of Pattern as a mask.
    %
:- pred mask(context(S)::in, pattern::in, io::di, io::uo) is det.

    % cairo.mask_surface(Context, Surface, X, Y, !IO):
    % Paint the current source using the alpha channel of Surface as a mask.
    % (X, Y) is coordinate at which to place the origin of Surface.
    %
:- pred mask_surface(context(S)::in, Mask::in, float::in, float::in,
    io::di, io::uo) is det <= surface(Mask).

    % cairo.paint(Context, !IO):
    % Paint the current source everywhere within the current clip region.
    %
:- pred paint(context(S)::in, io::di, io::uo) is det.

    % cairo.paint_with_alpha(Context, Alpha, !IO):
    % Paint the current source everywhere within the current clip region using
    % a mask of constant alpha value Alpha.
    %
:- pred paint_with_alpha(context(S)::in, float::in, io::di, io::uo) is det.

    % cairo.stroke(Context, !IO):
    % Stroke the current path according to the current line width, line join,
    % line cap, and dash settings for Context.
    % The current path will be cleared.
    %
:- pred stroke(context(S)::in, io::di, io::uo) is det.

    % cairo.stroke_preserve(Context, !IO):
    % As above, but preserve the current path for Context.
    %
:- pred stroke_preserve(context(S)::in, io::di, io::uo) is det.

    % cairo.stroke_extents(Context, Left, Top, Right, Bottom, !IO):
    % Compute a bounding box in user coordinates covering the area that would
    % be affected, (the "inked" area), by a cairo.stroke/3 operation given the
    % current path and stroke parameters. If the current path is empty, return
    % an empty rectangle ((0,0), (0,0)).
    % Surface dimensions and clipping are not taken into account.
    %
:- pred stroke_extents(context(S)::in, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

    % cairo.in_stroke(Context, X, Y, Result, !IO):
    % Result is "yes" if the coordinate (X, Y) is inside the area that would
    % be affected by a cairo.stroke/3 operation given the current path and
    % stroking parameters.
    %
:- pred in_stroke(context(S)::in, float::in, float::in, bool::out,
    io::di, io::uo) is det.

    % cairo.copy_page(Context, !IO):
    % Emits the current page for backends that support multiple pages, but
    % doesn't clear it, so, the contents of the current page will be retained
    % for the next page too. Use cairo.show_page/3 if you want to get an empty
    % page after the emission.
    %
:- pred copy_page(context(S)::in, io::di, io::uo) is det.

    % cairo.show_page(Context, !IO):
    % Emits and clears the current page for backends that support multiple
    % pages.
    %
:- pred show_page(context(S)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % cairo.status(Context, Status, !IO):
    % Status is the current status of Context.
    %
:- pred status(context(S)::in, status::out, io::di, io::uo) is det.

    % cairo.surface_status(Surface, Status, !IO):
    % Status is the current status of Surface.
    %
:- pred surface_status(S::in, status::out, io::di, io::uo) is det
   <= surface(S).

    % cairo.pattern_status(Pattern, Status, !IO):
    % Status is the current status of Pattern.
    %
:- pred pattern_status(pattern::in, status::out, io::di, io::uo) is det.

    % cairo.region_status(Region, Status, !IO):
    % Status is the current status of Region.
    %
:- pred region_status(region::in, status::out, io::di, io::uo) is det.

    % cairo.status_to_string(Status) = String:
    % String is a human-readable description of Status.
    %
:- func status_to_string(status) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module cairo.text.

:- pragma require_feature_set([conservative_gc, double_prec_float]).

%---------------------------------------------------------------------------%

    % All symbols defined in handwritten code should be prefixed with
    % "MCAIRO_".
    %
:- pragma foreign_decl("C", "

#include <cairo.h>

typedef struct {
    cairo_t         *mcairo_raw_context;
    MR_Word         mcairo_cached_font_face;
} MCAIRO_context;

typedef struct {
    cairo_pattern_t *mcairo_raw_pattern;
} MCAIRO_pattern;

typedef struct {
    cairo_surface_t *mcairo_raw_surface;
} MCAIRO_surface;

typedef struct {
    cairo_path_t    *mcairo_raw_path;
} MCAIRO_path;

typedef struct {
    cairo_font_face_t   *mcairo_raw_font_face;
} MCAIRO_font_face;

typedef struct {
    cairo_font_options_t  *mcairo_raw_font_options;
} MCAIRO_font_options;

typedef struct {
    cairo_scaled_font_t *mcairo_raw_scaled_font;
} MCAIRO_scaled_font;

typedef struct {
    cairo_region_t      *mcairo_raw_region;
} MCAIRO_region;

extern void
MCAIRO_finalize_context(void *context, void *client_data);

extern void
MCAIRO_finalize_pattern(void *pattern, void *client_data);

extern void
MCAIRO_finalize_surface(void *surface, void *client_data);

extern void
MCAIRO_finalize_path(void *path, void *client_data);

extern void
MCAIRO_finalize_font_face(void *font_face, void *client_data);

extern void
MCAIRO_finalize_font_options(void *font_options, void *client_data);

extern void
MCAIRO_finalize_region(void *region, void *client_data);

extern void
MCAIRO_finalize_scaled_font(void *scaled_font, void *client_data);

").

%---------------------------------------------------------------------------%

    % XXX implement equality for these.
    %
:- pragma foreign_type("C", cairo.context(T), "MCAIRO_context *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.pattern, "MCAIRO_pattern *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.matrix, "cairo_matrix_t *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.path, "MCAIRO_path *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.font_options, "MCAIRO_font_options *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.region, "MCAIRO_region *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", cairo.scaled_font(F), "MCAIRO_scaled_font *",
    [can_pass_as_mercury_type]).

%---------------------------------------------------------------------------%

:- pragma foreign_enum("C", content/0, [
    content_color       - "CAIRO_CONTENT_COLOR",
    content_alpha       - "CAIRO_CONTENT_ALPHA",
    content_color_alpha - "CAIRO_CONTENT_COLOR_ALPHA"
]).

:- pragma foreign_enum("C", operator/0, [
    operator_clear     - "CAIRO_OPERATOR_CLEAR",

    operator_source    - "CAIRO_OPERATOR_SOURCE",
    operator_over      - "CAIRO_OPERATOR_OVER",
    operator_in        - "CAIRO_OPERATOR_IN",
    operator_out       - "CAIRO_OPERATOR_OUT",
    operator_atop      - "CAIRO_OPERATOR_ATOP",

    operator_dest      - "CAIRO_OPERATOR_DEST",
    operator_dest_over - "CAIRO_OPERATOR_DEST_OVER",
    operator_dest_in   - "CAIRO_OPERATOR_DEST_IN",
    operator_dest_out  - "CAIRO_OPERATOR_DEST_OUT",
    operator_dest_atop - "CAIRO_OPERATOR_DEST_ATOP",

    operator_xor       - "CAIRO_OPERATOR_XOR",
    operator_add       - "CAIRO_OPERATOR_ADD",
    operator_saturate  - "CAIRO_OPERATOR_SATURATE",

    operator_multiply       - "CAIRO_OPERATOR_MULTIPLY",
    operator_screen         - "CAIRO_OPERATOR_SCREEN",
    operator_overlay        - "CAIRO_OPERATOR_OVERLAY",
    operator_darken         - "CAIRO_OPERATOR_DARKEN",
    operator_lighten        - "CAIRO_OPERATOR_LIGHTEN",
    operator_color_dodge    - "CAIRO_OPERATOR_COLOR_DODGE",
    operator_color_burn     - "CAIRO_OPERATOR_COLOR_BURN",
    operator_hard_light     - "CAIRO_OPERATOR_HARD_LIGHT",
    operator_soft_light     - "CAIRO_OPERATOR_SOFT_LIGHT",
    operator_difference     - "CAIRO_OPERATOR_DIFFERENCE",
    operator_exclusion      - "CAIRO_OPERATOR_EXCLUSION",
    operator_hsl_hue        - "CAIRO_OPERATOR_HSL_HUE",
    operator_hsl_saturation - "CAIRO_OPERATOR_HSL_SATURATION",
    operator_hsl_color      - "CAIRO_OPERATOR_HSL_COLOR",
    operator_hsl_luminosity - "CAIRO_OPERATOR_HSL_LUMINOSITY"
]).

:- pragma foreign_enum("C", cairo.format/0, [
    format_argb32    - "CAIRO_FORMAT_ARGB32",
    format_rgb24     - "CAIRO_FORMAT_RGB24",
    format_a8        - "CAIRO_FORMAT_A8",
    format_a1        - "CAIRO_FORMAT_A1",
    format_rgb16_565 - "CAIRO_FORMAT_RGB16_565"
]).

:- pragma foreign_enum("C", cairo.status/0, [
    status_success           - "CAIRO_STATUS_SUCCESS",
    status_no_memory         - "CAIRO_STATUS_NO_MEMORY",
    status_invalid_restore   - "CAIRO_STATUS_INVALID_RESTORE",
    status_invalid_pop_group - "CAIRO_STATUS_INVALID_POP_GROUP",
    status_no_current_point  - "CAIRO_STATUS_NO_CURRENT_POINT",
    status_invalid_matrix    - "CAIRO_STATUS_INVALID_MATRIX",
    status_invalid_status    - "CAIRO_STATUS_INVALID_STATUS",
    status_null_pointer      - "CAIRO_STATUS_NULL_POINTER",
    status_invalid_string    - "CAIRO_STATUS_INVALID_STRING",
    status_invalid_path_data - "CAIRO_STATUS_INVALID_PATH_DATA",
    status_read_error        - "CAIRO_STATUS_READ_ERROR",
    status_write_error       - "CAIRO_STATUS_WRITE_ERROR",
    status_surface_finished  - "CAIRO_STATUS_SURFACE_FINISHED",
    status_surface_type_mismatch - "CAIRO_STATUS_SURFACE_TYPE_MISMATCH",
    status_pattern_type_mismatch - "CAIRO_STATUS_PATTERN_TYPE_MISMATCH",
    status_invalid_content   - "CAIRO_STATUS_INVALID_CONTENT",
    status_invalid_format    - "CAIRO_STATUS_INVALID_FORMAT",
    status_invalid_visual    - "CAIRO_STATUS_INVALID_VISUAL",
    status_file_not_found    - "CAIRO_STATUS_FILE_NOT_FOUND",
    status_invalid_dash      - "CAIRO_STATUS_INVALID_DASH",
    status_invalid_dsc_comment - "CAIRO_STATUS_INVALID_DSC_COMMENT",
    status_invalid_index       - "CAIRO_STATUS_INVALID_INDEX",
    status_clip_not_representable - "CAIRO_STATUS_CLIP_NOT_REPRESENTABLE",
    status_temp_file_error        - "CAIRO_STATUS_TEMP_FILE_ERROR",
    status_invalid_stride         - "CAIRO_STATUS_INVALID_STRIDE",
    status_font_type_mismatch     - "CAIRO_STATUS_FONT_TYPE_MISMATCH",
    status_user_font_immutable    - "CAIRO_STATUS_USER_FONT_IMMUTABLE",
    status_user_font_error        - "CAIRO_STATUS_USER_FONT_ERROR",
    status_negative_count         - "CAIRO_STATUS_NEGATIVE_COUNT",
    status_invalid_clusters       - "CAIRO_STATUS_INVALID_CLUSTERS",
    status_invalid_slant          - "CAIRO_STATUS_INVALID_SLANT",
    status_invalid_weight         - "CAIRO_STATUS_INVALID_WEIGHT",
    status_invalid_size           - "CAIRO_STATUS_INVALID_SIZE",
    status_user_font_not_implemented- "CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED",
    status_device_type_mismatch     - "CAIRO_STATUS_DEVICE_TYPE_MISMATCH",
    status_device_error             - "CAIRO_STATUS_DEVICE_ERROR",
    status_invalid_mesh_construction- "CAIRO_STATUS_INVALID_MESH_CONSTRUCTION",
    status_device_finished          - "CAIRO_STATUS_DEVICE_FINISHED",
    status_jbig2_global_missing     - "CAIRO_STATUS_JBIG2_GLOBAL_MISSING"
]).

:- pragma foreign_code("C", "

void
MCAIRO_finalize_context(void *context, void *client_data)
{
    cairo_destroy(((MCAIRO_context *)context)->mcairo_raw_context);
}

void
MCAIRO_finalize_pattern(void *pattern, void *client_data)
{
    cairo_pattern_destroy(((MCAIRO_pattern *)pattern)->mcairo_raw_pattern);
}

void
MCAIRO_finalize_surface(void *surface, void *client_data)
{
    cairo_surface_destroy(((MCAIRO_surface *)surface)->mcairo_raw_surface);
}

void
MCAIRO_finalize_path(void *path, void *client_data)
{
    cairo_path_destroy(((MCAIRO_path *)path)->mcairo_raw_path);
}

void
MCAIRO_finalize_font_face(void *font_face, void *client_data)
{
    cairo_font_face_destroy(
        ((MCAIRO_font_face *)font_face)->mcairo_raw_font_face);
}

void
MCAIRO_finalize_font_options(void *font_options, void *client_data)
{
    cairo_font_options_destroy(
        ((MCAIRO_font_options *)font_options)->mcairo_raw_font_options);
}

void
MCAIRO_finalize_region(void *region, void *client_data)
{
    cairo_region_destroy(((MCAIRO_region *)region)->mcairo_raw_region);
}

void
MCAIRO_finalize_scaled_font(void *scaled_font, void *client_data)
{
    cairo_scaled_font_destroy(
        ((MCAIRO_scaled_font *)scaled_font)->mcairo_raw_scaled_font);
}

").

%---------------------------------------------------------------------------%

:- type font_face_container
    --->    some [F] font_face_container(F) => font_face(F).

%---------------------------------------------------------------------------%
%
% Context creation
%

create_context(Surface, Context, !IO) :-
    create_context_2(Surface, Context, !IO),
    % Make sure that the cached font face object is set to
    % a meaningful value.  (See the comments in the implementation
    % of {get,set}_font_face for details.)
    cairo.text.set_default_font_face(Context, !IO).

:- pred create_context_2(S::in, context(S)::out,
    io::di, io::uo) is det <= surface(S).

:- pragma foreign_proc("C",
    create_context_2(Surface::in, Context::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_t *raw_context;

    raw_context = cairo_create(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
    Context = MR_GC_NEW(MCAIRO_context);
    Context->mcairo_raw_context = raw_context;
    /*
    ** We fill the cached font face in later.
    */
    Context->mcairo_cached_font_face = 0;
    MR_GC_register_finalizer(Context, MCAIRO_finalize_context, 0);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    save(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_save(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    restore(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_restore(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    get_target(Ctxt::in, Target::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_surface_t     *raw_surface;
    MCAIRO_surface      *wrapped_surface;

    raw_surface = cairo_get_target(Ctxt->mcairo_raw_context);
    /*
    ** The object returned by cairo_get_target() is owned by cairo.
    ** Since we are keeping a reference to it we need to increment
    ** its reference count.
    */
    raw_surface = cairo_surface_reference(raw_surface);
    wrapped_surface = MR_GC_NEW(MCAIRO_surface);
    wrapped_surface->mcairo_raw_surface = raw_surface;
    MR_GC_register_finalizer(wrapped_surface, MCAIRO_finalize_surface, 0);
    Target = (MR_Word) wrapped_surface;
").

:- pragma foreign_proc("C",
    push_group(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_push_group(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    push_group_with_content(Ctxt::in, Content::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_push_group_with_content(Ctxt->mcairo_raw_context, Content);
").

:- pragma foreign_proc("C",
    pop_group(Ctxt::in, Pattern::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_pattern_t *new_pattern;

    new_pattern = cairo_pop_group(Ctxt->mcairo_raw_context);
    Pattern = MR_GC_NEW(MCAIRO_pattern);
    Pattern->mcairo_raw_pattern = new_pattern;
    MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    pop_group_to_source(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_pop_group_to_source(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    get_group_target(Ctxt::in, Target::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_surface_t     *raw_surface;
    MCAIRO_surface      *wrapped_surface;

    raw_surface = cairo_get_group_target(Ctxt->mcairo_raw_context);
    /*
    ** The object returned by cairo_get_group_target() is owned by cairo.
    ** Since we are keeping a reference to it we need to increment
    ** its reference count.
    */
    raw_surface = cairo_surface_reference(raw_surface);
    wrapped_surface = MR_GC_NEW(MCAIRO_surface);
    wrapped_surface->mcairo_raw_surface = raw_surface;
    MR_GC_register_finalizer(wrapped_surface, MCAIRO_finalize_surface, 0);
    Target = (MR_Word) wrapped_surface;
").

:- pragma foreign_proc("C",
    set_source_rgb(Ctxt::in, Red::in, Green::in, Blue::in,
    _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_source_rgb(Ctxt->mcairo_raw_context, Red, Green, Blue);
").

:- pragma foreign_proc("C",
    set_source_rgba(Ctxt::in, Red::in, Green::in, Blue::in, Alpha::in,
    _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_source_rgba(Ctxt->mcairo_raw_context,
    Red, Green, Blue, Alpha);
").

:- pragma foreign_proc("C",
    set_source(Ctxt::in, Pattern::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_source(Ctxt->mcairo_raw_context, Pattern->mcairo_raw_pattern);
").

:- pragma foreign_proc("C",
    set_source_surface(Ctxt::in, SrcSurface::in, X::in, Y::in,
      _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_set_source_surface(Ctxt->mcairo_raw_context,
    ((MCAIRO_surface *)SrcSurface)->mcairo_raw_surface, X, Y);
").

:- pragma foreign_proc("C",
    get_source(Ctxt::in, Pattern::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_pattern_t *raw_pattern;

    raw_pattern = cairo_get_source(Ctxt->mcairo_raw_context);

    /*
    ** The value returned by cairo_get_surface() is owned by
    ** by cairo.  Since we are going to retain a reference to
    ** it we need to increment the reference count here.
    */
    raw_pattern = cairo_pattern_reference(raw_pattern);
    Pattern = MR_GC_NEW(MCAIRO_pattern);
    Pattern->mcairo_raw_pattern = raw_pattern;
    MR_GC_register_finalizer(Pattern, MCAIRO_finalize_pattern, 0);
").

:- pragma foreign_proc("C",
    set_fill_rule(Ctxt::in, FillRule::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_fill_rule(Ctxt->mcairo_raw_context, FillRule);
").

:- pragma foreign_proc("C",
    get_fill_rule(Ctxt::in, FillRule::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    FillRule = cairo_get_fill_rule(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_line_cap(Ctxt::in, LineCap::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_line_cap(Ctxt->mcairo_raw_context, LineCap);
").

:- pragma foreign_proc("C",
    get_line_cap(Ctxt::in, LineCap::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    LineCap = cairo_get_line_cap(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    copy_page(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_copy_page(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    show_page(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_show_page(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_antialias(Ctxt::in, AA::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_antialias(Ctxt->mcairo_raw_context, (cairo_antialias_t)AA);
").

:- pragma foreign_proc("C",
    get_antialias(Ctxt::in, AA::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    AA = cairo_get_antialias(Ctxt->mcairo_raw_context);
").

set_dash(Context, Dashes, OffSet, !IO) :-
    list.length(Dashes, NumDashes),
    set_dash_2(Context, Dashes, NumDashes, OffSet, IsValid, !IO),
    (
        IsValid = yes
    ;
        IsValid = no,
        throw(cairo.error("set_dash/5", status_invalid_dash))
    ).

:- pred set_dash_2(context(S)::in, list(float)::in, int::in,
    float/*offset*/::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_dash_2(Ctxt::in, Dashes::in, NumDashes::in, OffSet::in,
        IsValid::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    double  *dashes;
    double  dash;
    size_t  i = 0;

    dashes = MR_GC_malloc(sizeof(double) * NumDashes);

    while (!MR_list_is_empty(Dashes)) {
        dash = MR_word_to_float(MR_list_head(Dashes));
        dashes[i] = dash;
        Dashes = MR_list_tail(Dashes);
        i++;
    }

    cairo_set_dash(Ctxt->mcairo_raw_context, dashes, (int)NumDashes, OffSet);

    if (cairo_status(Ctxt->mcairo_raw_context) == CAIRO_STATUS_INVALID_DASH) {
        IsValid = MR_NO;
    } else {
        IsValid = MR_YES;
    }
").

:- pragma foreign_proc("C",
    get_dash_count(Ctxt::in, Count::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Count = cairo_get_dash_count(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_line_join(Ctxt::in, LineJoin::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_line_join(Ctxt->mcairo_raw_context, LineJoin);
").

:- pragma foreign_proc("C",
    get_line_join(Ctxt::in, LineJoin::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    LineJoin = cairo_get_line_join(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_line_width(Ctxt::in, Width::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_line_width(Ctxt->mcairo_raw_context, Width);
").

:- pragma foreign_proc("C",
    get_line_width(Ctxt::in, Width::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Width = cairo_get_line_width(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_miter_limit(Ctxt::in, Limit::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_miter_limit(Ctxt->mcairo_raw_context, Limit);
").

:- pragma foreign_proc("C",
    get_miter_limit(Ctxt::in, Limit::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Limit = cairo_get_miter_limit(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_operator(Ctxt::in, Op::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_operator(Ctxt->mcairo_raw_context, Op);
").

:- pragma foreign_proc("C",
    get_operator(Ctxt::in, Op::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Op = cairo_get_operator(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    set_tolerance(Ctxt::in, Tolerance::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_tolerance(Ctxt->mcairo_raw_context, Tolerance);
").

:- pragma foreign_proc("C",
    get_tolerance(Ctxt::in, Tolerance::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Tolerance = cairo_get_tolerance(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    clip(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_clip(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    clip_preserve(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_clip_preserve(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    clip_extents(Ctxt::in, X1::out, Y1::out, X2::out, Y2::out,
       _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_clip_extents(Ctxt->mcairo_raw_context,
        &X1, &Y1, &X2, &Y2);
").

:- pragma foreign_proc("C",
    in_clip(Ctxt::in, X::in, Y::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    if (cairo_in_clip(Ctxt->mcairo_raw_context, X, Y)) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
    reset_clip(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_reset_clip(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    fill(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_fill(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    fill_preserve(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_fill_preserve(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    fill_extents(Ctxt::in, X1::out, Y1::out, X2::out, Y2::out,
            _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_fill_extents(Ctxt->mcairo_raw_context,
        &X1, &Y1, &X2, &Y2);
").

:- pragma foreign_proc("C",
    in_fill(Ctxt::in, X::in, Y::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    if (cairo_in_fill(Ctxt->mcairo_raw_context, X, Y)) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
    mask(Ctxt::in, Pattern::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_mask(Ctxt->mcairo_raw_context,
        Pattern->mcairo_raw_pattern);
").

:- pragma foreign_proc("C",
    mask_surface(Ctxt::in, Surface::in, X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_mask_surface(Ctxt->mcairo_raw_context,
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface,
        X, Y);
").

:- pragma foreign_proc("C",
    paint(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_paint(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    paint_with_alpha(Ctxt::in, Alpha::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_paint_with_alpha(Ctxt->mcairo_raw_context, Alpha);
").

:- pragma foreign_proc("C",
    stroke(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_stroke(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    stroke_preserve(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_stroke_preserve(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    stroke_extents(Ctxt::in, X1::out, Y1::out, X2::out, Y2::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_stroke_extents(Ctxt->mcairo_raw_context, &X1, &Y1, &X2, &Y2);
").

:- pragma foreign_proc("C",
    in_stroke(Ctxt::in, X::in, Y::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    if (cairo_in_stroke(Ctxt->mcairo_raw_context, X, Y)) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
   status(Ctxt::in, Status::out, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury, tabled_for_io],
"
   Status = cairo_status(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    surface_status(Surface::in, Status::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Status = cairo_surface_status(
        ((MCAIRO_surface *)Surface)->mcairo_raw_surface);
").

:- pragma foreign_proc("C",
    pattern_status(Pattern::in, Status::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Status = cairo_pattern_status(Pattern->mcairo_raw_pattern);
").

:- pragma foreign_proc("C",
    region_status(Region::in, Status::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Status = cairo_region_status(Region->mcairo_raw_region);
").

:- pragma foreign_proc("C",
   status_to_string(Status::in) = (Str::out),
   [promise_pure, will_not_call_mercury],
"
   const char *desc;

   desc = cairo_status_to_string(Status);
   MR_make_aligned_string_copy(Str, desc);
").

%---------------------------------------------------------------------------%
%
% Glyph array
%

:- type glyph_array.

:- pragma foreign_type("C", glyph_array, "cairo_glyph_t *",
    [can_pass_as_mercury_type]).

:- pred make_glyph_array(list(glyph)::in, glyph_array::uo, int::out,
    io::di, io::uo) is det.

make_glyph_array(Glyphs, Array, NumGlyphs, !IO) :-
    list.length(Glyphs, NumGlyphs),
    alloc_glyph_array(NumGlyphs, Array0),
    fill_glyph_array(Glyphs, 0, Array0, Array).

:- pred alloc_glyph_array(int::in, glyph_array::uo) is det.

:- pragma foreign_proc("C",
    alloc_glyph_array(Size::in, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_Word ptr;

    MR_incr_hp_atomic_msg(ptr, MR_bytes_to_words(Size * sizeof(cairo_glyph_t)),
        MR_ALLOC_ID, ""cairo.glyph_array/0"");
    Array = (cairo_glyph_t *) ptr;
").

:- pred fill_glyph_array(list(glyph)::in, int::in,
    glyph_array::di, glyph_array::uo) is det.

fill_glyph_array([], _Slot, !Array).
fill_glyph_array([G | Gs], Slot, !Array) :-
    G = glyph(Index, X, Y),
    set_glyph_array_slot(Slot, Index, X, Y, !Array),
    fill_glyph_array(Gs, Slot + 1, !Array).

:- pred set_glyph_array_slot(int::in, int::in, float::in, float::in,
    glyph_array::di, glyph_array::uo) is det.

:- pragma foreign_proc("C",
    set_glyph_array_slot(Slot::in, Index::in, X::in, Y::in,
        Array0::di, Array::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Array = Array0;
    Array[Slot].index = Index;
    Array[Slot].x = X;
    Array[Slot].y = Y;
").

%---------------------------------------------------------------------------%
:- end_module cairo.
%---------------------------------------------------------------------------%
