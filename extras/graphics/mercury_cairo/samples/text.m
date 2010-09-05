:- module text.
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
:- import_module cairo.text.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	text.select_font_face(Context, "Sans", slant_normal,
		weight_bold, !IO),
	text.set_font_size(Context, 90.0, !IO),

	path.move_to(Context, 10.0, 135.0, !IO),
	text.show_text(Context, "Hello", !IO),

	path.move_to(Context, 70.0, 165.0, !IO),
	path.text_path(Context, "void", !IO),
	cairo.set_source_rgb(Context, 0.5, 0.5, 1.0, !IO),
	cairo.fill_preserve(Context, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.set_line_width(Context, 2.56, !IO),
	cairo.stroke(Context, !IO),	

	% Draw helping lines.
	cairo.set_source_rgba(Context, 1.0, 0.2, 0.2, 0.6, !IO),
	path.arc(Context, 10.0, 135.0, 5.12, 0.0, 2.0 * pi, !IO),
	path.close_path(Context, !IO),
	path.arc(Context, 70.0, 165.0, 5.12, 0.0, 2.0 * pi, !IO),
	cairo.fill(Context, !IO),

	write_surface_to_png(Surface, "text.png", !IO).

%----------------------------------------------------------------------------%
:- end_module text.
%----------------------------------------------------------------------------%
