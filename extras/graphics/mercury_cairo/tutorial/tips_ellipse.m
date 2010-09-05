%-----------------------------------------------------------------------------%

:- module tips_ellipse.
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

	cairo.save(Context, !IO),
	cairo.transformations.scale(Context, 0.5, 1.0, !IO),
	cairo.path.arc(Context, 0.5, 0.5, 0.40, 0.0, 2.0 * pi, !IO),
	cairo.stroke(Context, !IO),

	cairo.transformations.translate(Context, 1.0, 0.0, !IO),
	cairo.path.arc(Context, 0.5, 0.5, 0.40, 0.0, 2.0 * pi, !IO),
	cairo.restore(Context, !IO),
	cairo.stroke(Context, !IO),

	cairo.png.write_surface_to_png(Surface, "tips_ellipse.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module tips_ellipse.
%-----------------------------------------------------------------------------%
