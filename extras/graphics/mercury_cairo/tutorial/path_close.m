%-----------------------------------------------------------------------------%

:- module path_close.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.path.
:- import_module cairo.png.
:- import_module cairo.transformations.

:- import_module float.
:- import_module math.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 120, 120, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 120.0, 120.0, !IO),

	cairo.set_line_width(Context, 0.1, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),

	cairo.path.move_to(Context, 0.25, 0.25, !IO),
	cairo.path.line_to(Context, 0.5, 0.375, !IO),
	cairo.path.rel_line_to(Context, 0.25, -0.125, !IO),
	cairo.path.arc(Context, 0.5, 0.5, 0.25 * sqrt(2.0), -0.25 * pi, 0.25 * pi, !IO),
	cairo.path.rel_curve_to(Context, -0.25, -0.125, -0.25, 0.125, -0.5, 0.0, !IO),
	cairo.path.close_path(Context, !IO),

	cairo.stroke(Context, !IO),

	cairo.png.write_surface_to_png(Surface, "path_close.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module path_close.
%-----------------------------------------------------------------------------%
