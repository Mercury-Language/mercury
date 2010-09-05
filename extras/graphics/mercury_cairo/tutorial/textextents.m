%-----------------------------------------------------------------------------%

:- module textextents.
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
:- import_module list.
:- import_module math.

%-----------------------------------------------------------------------------%

main(!IO) :-
	Text = "joy",

	cairo.image.create_surface(format_argb32, 240, 240, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 240.0, 240.0, !IO),
	cairo.text.set_font_size(Context, 0.5, !IO),

	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.text.select_font_face(Context, "Georgia", slant_normal,
		weight_bold, !IO),

	cairo.transformations.device_to_user_distance(Context, 1.0, 1.0,
	     Ux, Uy, !IO),
	Px = ( if Ux > Uy then Ux else Uy ),
	
	cairo.text.font_extents(Context, FE, !IO),
	cairo.text.text_extents(Context, Text, TE, !IO),
	X = 0.5 - TE ^ te_x_bearing - TE ^ te_width / 2.0,
	Y = 0.5 - FE ^ fe_descent + FE ^ fe_height / 2.0,

	/* baseline, descent, ascent, height */
	cairo.set_line_width(Context, 4.0 * Px, !IO),
	cairo.set_dash(Context, [9.0 * Px], 0.0, !IO),
	cairo.set_source_rgba(Context, 0.0, 0.6, 0.0, 0.5, !IO),
	cairo.path.move_to(Context, X + TE ^ te_x_bearing, Y, !IO),
	cairo.path.rel_line_to(Context, TE ^ te_width, 0.0, !IO),
	cairo.path.move_to(Context, X + TE ^ te_x_bearing, Y + FE ^ fe_descent, !IO),
	cairo.path.rel_line_to(Context, TE ^ te_width, 0.0, !IO),
	cairo.path.move_to(Context, X + TE ^ te_x_bearing, Y - FE ^ fe_ascent, !IO),
	cairo.path.rel_line_to(Context, TE ^ te_width, 0.0, !IO),
	cairo.path.move_to(Context, X + TE ^ te_x_bearing, Y - FE ^ fe_height, !IO),
	cairo.path.rel_line_to(Context, TE ^ te_width, 0.0, !IO),
	cairo.stroke(Context, !IO),

	/* extents: width & height */
	cairo.set_source_rgba(Context, 0.0, 0.0, 0.75, 0.5, !IO),
	cairo.set_line_width(Context, Px, !IO),
	cairo.set_dash(Context, [3.0 * Px], 0.0, !IO),
	cairo.path.rectangle(Context, X + TE ^ te_x_bearing,
		Y + TE ^ te_y_bearing, TE ^ te_width, TE ^ te_height, !IO),
	cairo.stroke(Context, !IO),

	/* text */
	cairo.path.move_to(Context, X, Y, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.text.show_text(Context, Text, !IO),

	/* bearing */
	cairo.set_dash(Context, [], 0.0, !IO),
	cairo.set_line_width(Context, 2.0 * Px, !IO),
	cairo.set_source_rgba(Context, 0.0, 0.0, 0.75, 0.5, !IO),
	cairo.path.move_to(Context, X, Y, !IO),
	cairo.path.rel_line_to(Context, TE ^ te_x_bearing, TE ^ te_y_bearing, !IO),
	cairo.stroke(Context, !IO),

	/* text's advance */
	cairo.set_source_rgba(Context, 0.0, 0.0, 0.75, 0.5, !IO),
	cairo.path.arc(Context, X + TE ^ te_x_advance, Y + TE ^ te_y_advance,
		float(5) * Px, 0.0, 2.0 * pi, !IO),
	cairo.fill(Context, !IO),

	/* reference point */
	cairo.path.arc(Context, X, Y, float(5) * Px, 0.0, 2.0 * pi, !IO),
	cairo.set_source_rgba(Context, 0.75, 0.0, 0.0, 0.5, !IO),
	cairo.fill(Context, !IO),

	/* Write output and clean up */
	cairo.png.write_surface_to_png(Surface, "textextents.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module textextents.
%-----------------------------------------------------------------------------%
