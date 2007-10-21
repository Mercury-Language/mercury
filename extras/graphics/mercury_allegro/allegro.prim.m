%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.prim.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.prim.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.color.
:- import_module allegro.fixed.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred clear_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred clear_to_color(bitmap::in, color::in, io::di, io::uo) is det.
:- pred putpixel(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred unsafe_putpixel(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred unsafe_putpixel15(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred unsafe_putpixel16(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred unsafe_putpixel24(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred unsafe_putpixel32(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred getpixel(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred unsafe_getpixel(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred unsafe_getpixel15(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred unsafe_getpixel16(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred unsafe_getpixel24(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred unsafe_getpixel32(bitmap::in, int::in, int::in, color::out, io::di, io::uo) is det.
:- pred vline(bitmap::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred hline(bitmap::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
%       do_line
:- pred line(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred fastline(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred triangle(bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred polygon(bitmap::in, list(int)::in, color::in, io::di, io::uo) is det.
:- pred rect(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred rectfill(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
%       do_circle
:- pred circle(bitmap::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred circlefill(bitmap::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
%       do_ellipse
:- pred ellipse(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred ellipsefill(bitmap::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
%       do_arc
:- pred arc(bitmap::in, int::in, int::in, fixed::in, fixed::in, int::in, color::in, io::di, io::uo) is det.
:- pred calc_spline(int::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, list({int, int})::out) is det.
:- pred spline(bitmap::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, int::in, color::in, io::di, io::uo) is det.
:- pred floodfill(bitmap::in, int::in, int::in, color::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    clear_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    clear_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    clear_to_color(Bitmap::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    clear_to_color(Bitmap, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    putpixel(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    putpixel(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_putpixel(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    _putpixel(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_putpixel15(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    _putpixel15(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_putpixel16(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    _putpixel16(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_putpixel24(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    _putpixel24(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_putpixel32(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    _putpixel32(Bitmap, X, Y, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    getpixel(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = getpixel(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_getpixel(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = _getpixel(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_getpixel15(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = _getpixel15(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_getpixel16(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = _getpixel16(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_getpixel24(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = _getpixel24(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unsafe_getpixel32(Bitmap::in, X::in, Y::in, Color::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Color = _getpixel32(Bitmap, X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    vline(Bitmap::in, X::in, Y1::in, Y2::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    vline(Bitmap, X, Y1, Y2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    hline(Bitmap::in, X1::in, Y::in, X2::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    hline(Bitmap, X1, Y, X2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    line(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    line(Bitmap, X1, Y1, X2, Y2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    fastline(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    fastline(Bitmap, X1, Y1, X2, Y2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    triangle(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, X3::in, Y3::in,
        Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    triangle(Bitmap, X1, Y1, X2, Y2, X3, Y3, Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

polygon(Bitmap, Vertices, Color, !IO) :-
    allocate_vertex_array(Array0),
    polygon_fill_vertex_array(Vertices, 0, NumVerts, Array0, Array),
    polygon_2(Bitmap, NumVerts, Array, Color, !IO).

:- type vertex_array.
:- pragma foreign_type("C", vertex_array, "int *", [can_pass_as_mercury_type]).

:- pred allocate_vertex_array(vertex_array::uo) is det.
:- pragma foreign_proc("C",
    allocate_vertex_array(VertexArray::uo),
    [will_not_call_mercury, promise_pure],
"
    VertexArray = MR_NEW_ARRAY(int, 2*256); /* arbitrary limit */
").

:- pred set_vertex_array(int::in, int::in, int::in, vertex_array::di,
    vertex_array::uo) is det.
:- pragma foreign_proc("C",
    set_vertex_array(Index::in, X::in, Y::in, VertexArray0::di,
        VertexArray::uo),
    [will_not_call_mercury, promise_pure],
"
    VertexArray0[Index*2] = X;
    VertexArray0[Index*2+1] = Y;
    VertexArray = VertexArray0;
").

:- pred polygon_fill_vertex_array(list(int)::in, int::in, int::out,
    vertex_array::di, vertex_array::uo) is det.

polygon_fill_vertex_array([], NumVerts, NumVerts, !Array).
polygon_fill_vertex_array([_], NumVerts, NumVerts, !Array) :-
    error("polygon/5: vertex list not a multiple of 2").
polygon_fill_vertex_array([X,Y | Verts], Index, NumVerts, !Array) :-
    set_vertex_array(Index, X, Y, !Array),
    polygon_fill_vertex_array(Verts, Index+1, NumVerts, !Array).

:- pred polygon_2(bitmap::in, int::in, vertex_array::di, color::in,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    polygon_2(Bitmap::in, NumVerts::in, VertexArray::di, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    polygon(Bitmap, NumVerts, VertexArray, Color);
    MR_free(VertexArray);
    IO = IO0;
").
    
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    rect(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rect(Bitmap, X1, Y1, X2, Y2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    rectfill(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    rectfill(Bitmap, X1, Y1, X2, Y2, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    circle(Bitmap::in, X::in, Y::in, Radius::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    circle(Bitmap, X, Y, Radius, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    circlefill(Bitmap::in, X::in, Y::in, Radius::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    circlefill(Bitmap, X, Y, Radius, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    ellipse(Bitmap::in, X::in, Y::in, RX::in, RY::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ellipse(Bitmap, X, Y, RX, RY, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    ellipsefill(Bitmap::in, X::in, Y::in, RX::in, RY::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    ellipsefill(Bitmap, X, Y, RX, RY, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    arc(Bitmap::in, X::in, Y::in, Ang1::in, Ang2::in, R::in, Color::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    arc(Bitmap, X, Y, Ang1, Ang2, R, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    calc_spline(XA::in, YA::in, XB::in, YB::in, XC::in, YC::in, XD::in, YD::in,
        NPts::in, Points::out),
    [may_call_mercury, promise_pure],
"
    int Ctl[8] = {XA, YA, XB, YB, XC, YC, XD, YD};
    int Xs[NPts];
    int Ys[NPts];
    int I;

    calc_spline(Ctl, NPts, Xs, Ys);

    Points = _mal_nil_int_int();
    for (I = NPts - 1; I >= 0; I--) {
        Points = _mal_cons_int_int(Xs[I], Ys[I], Points);
    }
").

:- pragma foreign_proc("C",
    spline(Bitmap::in, AX::in, AY::in, BX::in, BY::in, CX::in, CY::in,
        DX::in, DY::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    const int points[8] = {AX, AY, BX, BY, CX, CY, DX, DY};
    spline(Bitmap, points, Color);
    IO = IO0;
").

:- pragma foreign_proc("C",
    floodfill(Bitmap::in, X::in, Y::in, Color::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    floodfill(Bitmap, X, Y, Color);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func mal_cons_int_int(int, int, list({int, int})) = list({int, int}).
:- func mal_nil_int_int = list({int, int}).

:- pragma foreign_export("C", mal_cons_int_int(in, in, in) = out,
    "_mal_cons_int_int").
:- pragma foreign_export("C", mal_nil_int_int = out, "_mal_nil_int_int").

mal_cons_int_int(X, Y, List) = [{X, Y} | List].
mal_nil_int_int = [].

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
