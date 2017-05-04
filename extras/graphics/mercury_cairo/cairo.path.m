%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module contains predicates for creating and manipulating path
% data.
%
% TODO: implement iteration over path components.
%
%---------------------------------------------------------------------------%

:- module cairo.path.
:- interface.

%---------------------------------------------------------------------------%

    % path.copy_path(Context, Path, !IO):
    % Path is a copy of the current path.
    %
:- pred copy_path(context(T)::in, path::out, io::di, io::uo) is det.

    % path.copy_path_flat(Context, Path, !IO):
    % Path is a flattened copy of the current path.
    % (Curves are replaced by piecewise linear approximations.)
    %
:- pred copy_path_flat(context(T)::in, path::out, io::di, io::uo) is det.

    % path.append_path(Context, Path, !IO):
    % Append Path to the current path.
    %
:- pred append_path(context(T)::in, path::in, io::di, io::uo) is det.

    % path.has_current_point(Context, Result, !IO):
    % Result is "yes" if a current point is defined on the current path
    % and "no" otherwise.
    %
:- pred has_current_point(context(T)::in, bool::out, io::di, io::uo) is det.

    % path.get_current_point(Context, X, Y, !IO):
    % (X, Y) is the current point of the current path.
    % If the current path has no current point then (X, Y) = (0.0, 0.0).
    %
:- pred get_current_point(context(T)::in, float::out, float::out,
    io::di, io::uo) is det.

    % path.new_path(Context, !IO):
    % Clears the current point.
    % After this call there will be no path and no current point.
    %
:- pred new_path(context(T)::in, io::di, io::uo) is det.

    % path.new_sub_path(Context, !IO):
    % Start a new sub-path.
    % Note that the existing path is not affected.
    % After this call there will be no current point.
    %
:- pred new_sub_path(context(T)::in, io::di, io::uo) is det.

    % path.close_path(Context, !IO):
    % Adds a line segment to the path from the current point to the beginning
    % of the current sub-path, (the most recent point passed to
    % cairo.move_to/5), and closes this sub-path.
    % After this call the current path will be at the joined endpoint of the
    % sub-path.
    %
:- pred close_path(context(T)::in, io::di, io::uo) is det.

    % path.arc(Ctxt, Xc, Yc, R, Angle1, Angle2, !IO):
    % Add a circular arc of radius R to the current path.
    % The arc is centred at (Xc, Yc), begins at Angle1 and proceeds in the
    % direction of increasing angles to end at Angle2.
    % If Angle2 is less than Angle1 it will be progressively increased by
    % 2 * pi until it is greater than Angle1.
    %
:- pred arc(context(T)::in, float::in, float::in,
    float::in, float::in, float::in, io::di, io::uo) is det.

    % path.arc_negative(Context, Xc, Yc, R, Angle1, Angle2, !IO):
    % Add a circular arc of radius R to the current path.
    % The arc is centred at (Xc, Yc), begins at Angle1 and proceeds in the
    % direction of decreasing angles to end at Angle2.
    % If Angle2 is less than Angle1 it will be progressively decreased by
    % 2 * pi until it is greater than Angle1.
    %
:- pred arc_negative(context(T)::in, float::in, float::in,
    float::in, float::in, float::in, io::di, io::uo) is det.

    % path.curve_to(Context, X1, Y1, X2, Y2, X3, Y3, !IO):
    % Adds a cubic Bezier spline to the path from the current point to position
    % (X3, Y3) in user-space coordinates, using (X1, Y1) and (X2, Y2) as the
    % control points. After this call the current point will be (x3, y3).
    %
:- pred curve_to(context(T)::in, float::in, float::in,
    float::in, float::in, float::in, float::in, io::di, io::uo) is det.

    % path.line_to(Context, X, Y, !IO):
    % Adds a line to the path from the current point to position (X, Y) in
    % user-space coordinates. After this call the current point will be (X, Y).
    % If there is no current point, then this behave like calling
    % path.move_to(Context, X, Y, !IO).
    %
:- pred line_to(context(T)::in, float::in, float::in, io::di, io::uo) is det.

    % path.move_to(Context, X, Y, !IO):
    % Begin a new sub-path. After this call the current point will be (X, Y).
    %
:- pred move_to(context(T)::in, float::in, float::in, io::di, io::uo) is det.

    % path.rectangle(Context, X, Y, Width, Height, !IO):
    % Adds a closed sub-path rectangle of the given size to the current path at
    % position (X, Y) in user-space coordinates.
    %
:- pred rectangle(context(T)::in, float::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % path.text_path(Context, Text, !IO):
    % Adds closed paths for Text to the current path.
    %
:- pred text_path(context(T)::in, string::in, io::di, io::uo) is det.

    % path.glyph_path(Context, Glyphs, !IO)
    % Adds closed paths for the glyphs to the current path.
    %
:- pred glyph_path(context(T)::in, list(glyph)::in, io::di, io::uo) is det.

    % path.rel_curve_to(Context, Dx1, Dy1, Dx2, Dy2, Dx3, Dy3, !IO):
    % Relative-coordinate version of path.curve_to/9.
    % All offsets are relative to the current point.
    % Throws a cairo.error/0 exception if there is no current point.
    %
:- pred rel_curve_to(context(T)::in, float::in, float::in,
    float::in, float::in, float::in, float::in, io::di, io::uo) is det.

    % path.rel_line_to(Context, Dx, Dy, !IO):
    % Relative-coordinate version of path.line_to/5.
    % All offsets are relative to the current point.
    % Throws a cairo.error/0 exception if there is no current point.
    %
:- pred rel_line_to(context(T)::in, float::in, float::in,
    io::di, io::uo) is det.

    % path.rel_move_to(Context, Dx, Dy, !IO):
    % Relative-coordinate version of path.move_to/5.
    % Throws a cairo.error/0 exception if there is no current point.
    %
:- pred rel_move_to(context(T)::in, float::in, float::in,
    io::di, io::uo) is det.

    % path.path_extents(Context, X1, Y1, X2, Y2, !IO):
    % Computes a bounding box in user-space coordinates covering the points
    % on the current path. If the current path is empty, returns an empty
    % rectangle ((0,0), (0,0)). Stroke parameters, fill rule, surface
    % dimensions and clipping are not taken into account.
    %
    % (X1, Y1) is the top-left of the box.
    % (X2, Y2) is the bottom-right of the box.
    %
:- pred path_extents(context(T)::in, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    copy_path(Ctxt::in, Path::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_path_t    *raw_path;

    raw_path = cairo_copy_path(Ctxt->mcairo_raw_context);
    Path = MR_GC_NEW(MCAIRO_path);
    Path->mcairo_raw_path = raw_path;
    MR_GC_register_finalizer(Path, MCAIRO_finalize_path, 0);
").

:- pragma foreign_proc("C",
    copy_path_flat(Ctxt::in, Path::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_path_t    *raw_path;

    raw_path = cairo_copy_path_flat(Ctxt->mcairo_raw_context);
    Path = MR_GC_NEW(MCAIRO_path);
    Path->mcairo_raw_path = raw_path;
    MR_GC_register_finalizer(Path, MCAIRO_finalize_path, 0);
").


:- pragma foreign_proc("C",
    append_path(Ctxt::in, Path::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_append_path(Ctxt->mcairo_raw_context,
       Path->mcairo_raw_path);
").

:- pragma foreign_proc("C",
    has_current_point(Ctxt::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    if (cairo_has_current_point(Ctxt->mcairo_raw_context)) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
    get_current_point(Ctxt::in, X::out, Y::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    double  x, y;

    cairo_get_current_point(Ctxt->mcairo_raw_context, &x, &y);
    X = x;
    Y = y;
").

:- pragma foreign_proc("C",
    new_path(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_new_path(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    new_sub_path(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_new_sub_path(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    close_path(Ctxt::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
   cairo_close_path(Ctxt->mcairo_raw_context);
").

:- pragma foreign_proc("C",
    arc(Ctxt::in, XC::in, YC::in, Radius::in, Angle1::in, Angle2::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_arc(Ctxt->mcairo_raw_context, XC, YC, Radius, Angle1, Angle2);
").

:- pragma foreign_proc("C",
    arc_negative(Ctxt::in, XC::in, YC::in, Radius::in, Angle1::in, Angle2::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_arc_negative(Ctxt->mcairo_raw_context, XC, YC, Radius, Angle1, Angle2);
").

:- pragma foreign_proc("C",
    curve_to(Ctxt::in, X1::in, Y1::in, X2::in, Y2::in, X3::in, Y3::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_curve_to(Ctxt->mcairo_raw_context, X1, Y1, X2, Y2, X3, Y3);
").

:- pragma foreign_proc("C",
   line_to(Ctxt::in, X::in, Y::in, _IO0::di, _IO::uo),
   [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_line_to(Ctxt->mcairo_raw_context, X, Y);
").

:- pragma foreign_proc("C",
    move_to(Ctxt::in, X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_move_to(Ctxt->mcairo_raw_context, X, Y);
").

:- pragma foreign_proc("C",
    rectangle(Ctxt::in, X::in, Y::in, W::in, H::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
   cairo_rectangle(Ctxt->mcairo_raw_context, X, Y, W, H);
").

:- pragma foreign_proc("C",
    text_path(Ctxt::in, Str::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
   cairo_text_path(Ctxt->mcairo_raw_context, Str);
").

glyph_path(Ctxt, Glyphs, !IO) :-
    make_glyph_array(Glyphs, Array, NumGlyphs, !IO),
    glyph_array_path(Ctxt, Array, NumGlyphs, !IO).

:- pred glyph_array_path(context(T)::in, glyph_array::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    glyph_array_path(Ctxt::in, Array::in, NumGlyphs::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_glyph_path(Ctxt->mcairo_raw_context, Array, NumGlyphs);
").

rel_curve_to(Ctxt, Dx1, Dy1, Dx2, Dy2, Dx3, Dy3, !IO) :-
    rel_curve_to_2(Ctxt, Dx1, Dy1, Dx2, Dy2, Dx3, Dy3, IsValid, !IO),
    (
        IsValid = yes
    ;
        IsValid = no,
        throw(cairo.error("rel_curve_to/7", status_no_current_point))
    ).

:- pred rel_curve_to_2(context(T)::in, float::in, float::in,
    float::in, float::in, float::in, float::in, bool::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rel_curve_to_2(Ctxt::in, Dx1::in, Dy1::in, Dx2::in, Dy2::in,
        Dx3::in, Dy3::in, IsValid::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_status_t  status;

    cairo_rel_curve_to(Ctxt->mcairo_raw_context,
       Dx1, Dy1, Dx2, Dy2, Dx3, Dy3);
    status = cairo_status(Ctxt->mcairo_raw_context);
    if (status == CAIRO_STATUS_NO_CURRENT_POINT) {
        IsValid = MR_NO;
    } else {
        IsValid = MR_YES;
    }
").

rel_line_to(Ctxt, Dx, Dy, !IO) :-
    rel_line_to_2(Ctxt, Dx, Dy, IsValid, !IO),
    (
        IsValid = yes
    ;
        IsValid = no,
        throw(cairo.error("rel_line_to/5", status_no_current_point))
    ).

:- pred rel_line_to_2(context(T)::in, float::in, float::in,
    bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rel_line_to_2(Ctxt::in, Dx::in, Dy::in, IsValid::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_status_t  status;

    cairo_rel_line_to(Ctxt->mcairo_raw_context, Dx, Dy);
    status = cairo_status(Ctxt->mcairo_raw_context);
    if (status == CAIRO_STATUS_NO_CURRENT_POINT) {
        IsValid = MR_NO;
    } else {
        IsValid = MR_YES;
    }
").

rel_move_to(Ctxt, Dx, Dy, !IO) :-
    rel_move_to_2(Ctxt, Dx, Dy, IsValid, !IO),
    (
        IsValid = yes
    ;
        IsValid = no,
        throw(cairo.error("rel_move_to/5", status_no_current_point))
    ).

:- pred rel_move_to_2(context(T)::in, float::in, float::in,
    bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rel_move_to_2(Ctxt::in, Dx::in, Dy::in, IsValid::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_status_t  status;

    cairo_rel_move_to(Ctxt->mcairo_raw_context, Dx, Dy);
    status = cairo_status(Ctxt->mcairo_raw_context);
    if (status == CAIRO_STATUS_NO_CURRENT_POINT) {
        IsValid = MR_NO;
    } else {
        IsValid = MR_YES;
    }
").

:- pragma foreign_proc("C",
    path_extents(Ctxt::in, X1::out, Y1::out, X2::out, Y2::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    double  x1, y1, x2, y2;

    cairo_path_extents(Ctxt->mcairo_raw_context, &x1, &y1, &x2, &y2);
    X1 = x1;
    Y1 = y1;
    X2 = x2;
    Y2 = y2;
").

%---------------------------------------------------------------------------%
:- end_module cairo.path.
%---------------------------------------------------------------------------%
