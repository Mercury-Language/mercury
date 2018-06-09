%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
%-----------------------------------------------------------------------------%

:- module cairo.region.
:- interface.

%-----------------------------------------------------------------------------%

    % region.create(Region, !IO):
    % Region is a new empty region.
    %
:- pred create(region::out, io::di, io::uo) is det.

    % region.create_rectangle(Rectangle, Region, !IO):
    % Region is a new region containing Rectangle.
    %
:- pred create_rectangle(rectangle::in, region::out,
    io::di, io::uo) is det.

    % region.create_rectangles(Rectangles, Region, !IO):
    % Region is a new region that containing the union of
    % Rectangles.
    %
    % XXX NYI
%:- pred create_rectangles(list(rectangle)::in, region::out,
%    io::di, io::uo) is det.

    % region.copy(Orig, Copy, !IO):
    % Copy is a copy of Orig.
    %
:- pred copy(region::in, region::out, io::di, io::uo) is det.

    % region.get_extents(Region, Rectangle, !IO):
    % Rectangle is the bounding rectangle for Region.
    %
:- pred get_extents(region::in, rectangle::out,
    io::di, io::uo) is det.

    % region.num_rectangles(Region, NumRectangles, !IO):
    % NumRectangles is the number of rectangles in Region.
    %
:- pred num_rectangles(region::in, int::out, io::di, io::uo) is det.

    % region.get_rectangle(Region, N, Rectangle, !IO):
    % Rectangle is the N'th rectangle in Region.
    % XXX the cairo manual doesn't say what happens if there
    % are less than N rectangles in Region.
    %
:- pred get_rectangle(region::in, int::in, rectangle::out,
    io::di, io::uo) is det.

    % region.is_empty(Region, IsEmpty, !IO):
    % IsEmpty is "yes" if Region is empty and "no" otherwise.
    %
:- pred is_empty(region::in, bool::out, io::di, io::uo) is det.

    % region.contains_point(Region, X, Y, Result, !IO):
    % Result is "yes" if (X, Y) is contained in Region.
    %
:- pred contains_point(region::in, int::in, int::in, bool::out,
    io::di, io::uo) is det.

:- type region.overlap
    --->    overlap_in
            % The contents are entirely inside the region.

    ;       overlap_out
            % The contents are entirely outside the region.

    ;       overlap_part.
            % The contents are partially inside and partially outside
            % the region.

    % region.contains_rectangle(Region, Rectangle, Overlap, !IO):
    % Overlap is the result of checking whether Rectangle is (partially)
    % contained within Region.
    %
:- pred contains_rectangle(region::in, rectangle::in, overlap::out,
    io::di, io::uo) is det.

    % region.equal(RegionA, RegionB, Result, !IO):
    % Result is "yes" if RegionA and RegionB contain the some coverage
    % and "no" otherwise.
    %
:- pred equal(region::in, region::in, bool::out, io::di, io::uo) is det.

    % region.translate(Region, Dx, Dy, !IO):
    % Translate Region by (Dx, Dy).
    %
:- pred translate(region::in, int::in, int::in, io::di, io::uo) is det.

    % region.intersect(Dst, Other, !IO):
    % Update Dst to be the intersection of Dst and Other.
    %
:- pred intersect(region::in, region::in, io::di, io::uo) is det.

    % region.intersect_rectangle(Region, Rectangle, !IO):
    % Update Region to be the intersection of Region and Rectangle.
    %
:- pred intersect_rectangle(region::in, rectangle::in, io::di, io::uo) is det.

    % region.subtract(Dst, Other, !IO):
    % Update Dst to be the result of subtracting Other from Dst.
    %
:- pred subtract(region::in, region::in, io::di, io::uo) is det.

    % region.subtract_rectangle(Region, Rectangle, !IO):
    % Update Region to be the result of subtracting Rectangle from Region.
    %
:- pred subtract_rectangle(region::in, rectangle::in, io::di, io::uo) is det.

    % region.union(Dst, Other, !IO):
    % Update Dst to be the union of Dst and Other.
    %
:- pred union(region::in, region::in, io::di, io::uo) is det.

    % region.union_rectangle(Region, Rectangle, !IO):
    % Update Region to be the union of Region and Rectangle.
    %
:- pred union_rectangle(region::in, rectangle::in, io::di, io::uo) is det.

    % region.xor(Dst, Other, !IO):
    % Update Dst to be the exclusive difference of Dst and Other.
    %
:- pred xor(region::in, region::in, io::di, io::uo) is det.

    % region.xor_rectangle(Region, Rectangle, !IO):
    % Update Region to be the exclusive difference of Region and Rectangle.
    %
:- pred xor_rectangle(region::in, rectangle::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_enum("C", region.overlap/0, [
    overlap_in   - "CAIRO_REGION_OVERLAP_IN",
    overlap_out  - "CAIRO_REGION_OVERLAP_OUT",
    overlap_part - "CAIRO_REGION_OVERLAP_PART"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    create(Region::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_t  *raw_region;

    raw_region = cairo_region_create();
    Region = MR_GC_NEW(MCAIRO_region);
    Region->mcairo_raw_region = raw_region;
    MR_GC_register_finalizer(Region, MCAIRO_finalize_region, 0);
").

create_rectangle(Rectangle, Region, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    create_rectangle_2(X, Y, Width, Height, Region, !IO).

:- pred create_rectangle_2(int::in, int::in, int::in, int::in,
    region::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    create_rectangle_2(X::in, Y::in, W::in, H::in, Region::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   rect;
    cairo_region_t          *raw_region;

    rect.x = X;
    rect.y = Y;
    rect.width = W;
    rect.height = H;

    raw_region = cairo_region_create_rectangle(&rect);
    Region = MR_GC_NEW(MCAIRO_region);
    Region->mcairo_raw_region = raw_region;
    MR_GC_register_finalizer(Region, MCAIRO_finalize_region, 0);
").

:- pragma foreign_proc("C",
    copy(Orig::in, Copy::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_t  *raw_copy;
    raw_copy = cairo_region_copy(Orig->mcairo_raw_region);
    Copy = MR_GC_NEW(MCAIRO_region);
    Copy->mcairo_raw_region = raw_copy;
    MR_GC_register_finalizer(Copy, MCAIRO_finalize_region, 0);
").

get_extents(Region, Rectangle, !IO) :-
    get_extents_2(Region, X, Y, Width, Height, !IO),
    Rectangle = rectangle(X, Y, Width, Height).

:- pred get_extents_2(region::in, int::out, int::out,
    int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_extents_2(Region::in, X::out, Y::out, W::out, H::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   r;
    cairo_region_get_extents(Region->mcairo_raw_region,
        &r);
    X = r.x;
    Y = r.y;
    W = r.width;
    H = r.height;
").

:- pragma foreign_proc("C",
    num_rectangles(Region::in, NR::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    NR = cairo_region_num_rectangles(Region->mcairo_raw_region);
").

get_rectangle(Region, N, Rectangle, !IO) :-
    get_rectangle_2(Region, N, X, Y, Width, Height, !IO),
    Rectangle = rectangle(X, Y, Width, Height).

:- pred get_rectangle_2(region::in, int::in, int::out, int::out,
    int::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_rectangle_2(Region::in, N::in, X::out, Y::out, W::out,
        H::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t     r;

    cairo_region_get_rectangle(Region->mcairo_raw_region,
        N, &r);
    X = r.x;
    Y = r.y;
    W = r.width;
    H = r.height;
").

:- pragma foreign_proc("C",
    is_empty(Region::in, IsEmpty::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    if (cairo_region_is_empty(Region->mcairo_raw_region)) {
        IsEmpty = MR_YES;
    } else {
        IsEmpty = MR_NO;
    }
").

:- pragma foreign_proc("C",
    contains_point(Region::in, X::in, Y::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    if (cairo_region_contains_point(Region->mcairo_raw_region, (int)X, (int)Y))
    {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

contains_rectangle(Region, Rectangle, Overlap, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    contains_rectangle_2(Region, X, Y, Width, Height, Overlap, !IO).

:- pred contains_rectangle_2(region::in, int::in, int::in, int::in, int::in,
    overlap::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    contains_rectangle_2(Region::in, X::in, Y::in, W::in, H::in,
        Overlap::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   r;

    r.x = X;
    r.y = Y;
    r.width = W;
    r.height = H;

    Overlap = cairo_region_contains_rectangle(Region->mcairo_raw_region, &r);

").

:- pragma foreign_proc("C",
    equal(A::in, B::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    if (cairo_region_equal(A->mcairo_raw_region, B->mcairo_raw_region)) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
    translate(Region::in, Dx::in, Dy::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_translate(Region->mcairo_raw_region, Dx, Dy);
").

:- pragma foreign_proc("C",
    intersect(Dst::in, Other::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_intersect(Dst->mcairo_raw_region,
        Other->mcairo_raw_region);
").

intersect_rectangle(Region, Rectangle, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    intersect_rectangle_2(Region, X, Y, Width, Height, !IO).

:- pred intersect_rectangle_2(region::in, int::in, int::in, int::in, int::in,
     io::di, io::uo) is det.

:- pragma foreign_proc("C",
    intersect_rectangle_2(Region::in, X::in, Y::in, W::in, H::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   rectangle;

    rectangle.x = X;
    rectangle.y = Y;
    rectangle.width = W;
    rectangle.height = H;
    cairo_region_intersect_rectangle(Region->mcairo_raw_region,
        &rectangle);
").

:- pragma foreign_proc("C",
    subtract(Dst::in, Other::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_subtract(Dst->mcairo_raw_region,
        Other->mcairo_raw_region);
").

subtract_rectangle(Region, Rectangle, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    subtract_rectangle_2(Region, X, Y, Width, Height, !IO).

:- pred subtract_rectangle_2(region::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    subtract_rectangle_2(Region::in, X::in, Y::in, W::in, H::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   rectangle;

    rectangle.x = X;
    rectangle.y = Y;
    rectangle.width = W;
    rectangle.height = H;
    cairo_region_subtract_rectangle(Region->mcairo_raw_region,
        &rectangle);
").

:- pragma foreign_proc("C",
    union(Dst::in, Other::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_union(Dst->mcairo_raw_region,
        Other->mcairo_raw_region);
").

union_rectangle(Region, Rectangle, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    union_rectangle_2(Region, X, Y, Width, Height, !IO).

:- pred union_rectangle_2(region::in, int::in, int::in, int::in, int::in,
     io::di, io::uo) is det.

:- pragma foreign_proc("C",
    union_rectangle_2(Region::in, X::in, Y::in, W::in, H::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   rectangle;

    rectangle.x = X;
    rectangle.y = Y;
    rectangle.width = W;
    rectangle.height = H;
    cairo_region_union_rectangle(Region->mcairo_raw_region,
        &rectangle);
").

:- pragma foreign_proc("C",
    xor(Dst::in, Other::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_region_xor(Dst->mcairo_raw_region,
        Other->mcairo_raw_region);
").

xor_rectangle(Region, Rectangle, !IO) :-
    Rectangle = rectangle(X, Y, Width, Height),
    xor_rectangle_2(Region, X, Y, Width, Height, !IO).

:- pred xor_rectangle_2(region::in, int::in, int::in, int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    xor_rectangle_2(Region::in, X::in, Y::in, W::in, H::in,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_rectangle_int_t   rectangle;

    rectangle.x = X;
    rectangle.y = Y;
    rectangle.width = W;
    rectangle.height = H;
    cairo_region_xor_rectangle(Region->mcairo_raw_region,
        &rectangle);
").

%-----------------------------------------------------------------------------%
:- end_module cairo.region.
%-----------------------------------------------------------------------------%
