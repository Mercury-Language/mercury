:- module fill_and_stroke2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.path.
:- import_module cairo.png.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	path.move_to(Context, 128.0, 25.6, !IO),
	path.line_to(Context, 230.4, 230.4, !IO),
	path.rel_line_to(Context, -102.4, 0.0, !IO),
	path.curve_to(Context, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0, !IO),
	path.close_path(Context, !IO),

	path.move_to(Context, 64.0, 25.6, !IO),	
	path.rel_line_to(Context, 51.2, 51.2, !IO),
	path.rel_line_to(Context, -51.2, 51.2, !IO),
	path.rel_line_to(Context, -51.2, -51.2, !IO),
	path.close_path(Context, !IO),

	cairo.set_line_width(Context, 10.0, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 1.0, !IO),
	cairo.fill_preserve(Context, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "fill_and_stroke2.png", !IO).

%----------------------------------------------------------------------------%
:- end_module fill_and_stroke2.
%----------------------------------------------------------------------------%
