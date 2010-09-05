:- module text_align_center.
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

	Utf8 = "cairo",
	text.select_font_face(Context, "Sans", slant_normal,
		weight_normal, !IO),
	text.set_font_size(Context, 52.0, !IO),
	text.text_extents(Context, Utf8, Extents, !IO),
	X = 128.0 - (Extents ^ te_width / 2.0 + Extents ^ te_x_bearing),
	Y = 128.0 - (Extents ^ te_height / 2.0 + Extents ^ te_y_bearing),

	path.move_to(Context, X, Y, !IO),
	text.show_text(Context, Utf8, !IO),
	
	% Draw helping lines.
	cairo.set_source_rgba(Context, 1.0, 0.2, 0.2, 0.6, !IO),
	cairo.set_line_width(Context, 6.0, !IO),
	path.arc(Context, X, Y, 10.0, 0.0, 2.0 * pi, !IO),
	cairo.fill(Context, !IO),
	path.move_to(Context, 128.0, 0.0, !IO),
	path.rel_line_to(Context, 0.0, 256.0, !IO),
	path.move_to(Context, 0.0, 128.0, !IO),
	path.rel_line_to(Context, 256.0, 0.0, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "text_align_center.png", !IO).

%----------------------------------------------------------------------------%
:- end_module text_align_center.
%----------------------------------------------------------------------------%
