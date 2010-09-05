%-----------------------------------------------------------------------------%

:- module stroke.
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

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 120, 120, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 120.0, 120.0, !IO),

	cairo.set_line_width(Context, 0.1, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.path.rectangle(Context, 0.25, 0.25, 0.5, 0.5, !IO),
	cairo.stroke(Context, !IO),

	cairo.png.write_surface_to_png(Surface, "stroke.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module stroke.
%-----------------------------------------------------------------------------%
