%-----------------------------------------------------------------------------%

:- module showtext.
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
:- import_module cairo.text.
:- import_module cairo.transformations.

:- import_module float.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 120, 120, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 120.0, 120.0, !IO),

	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.text.select_font_face(Context, "Georgia", slant_normal,
		weight_bold, !IO),
	cairo.text.set_font_size(Context, 1.2, !IO),
	cairo.text.text_extents(Context, "a", TE, !IO),
	cairo.path.move_to(Context, 0.5 - TE ^ te_width / 2.0 - TE ^ te_x_bearing,
		0.5 - TE ^ te_height / 2.0 - TE ^ te_y_bearing, !IO),
	cairo.text.show_text(Context, "a", !IO),

	cairo.png.write_surface_to_png(Surface, "showtext.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module showtext.
%-----------------------------------------------------------------------------%
