:- module fill_style.
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
:- import_module cairo.transformations.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	cairo.set_line_width(Context, 6.0, !IO),
	
	path.rectangle(Context, 12.0, 12.0, 232.0, 70.0, !IO),
	path.new_sub_path(Context, !IO),
	path.arc(Context, 64.0, 64.0, 40.0, 0.0, 2.0 * pi, !IO),
	path.new_sub_path(Context, !IO),
	path.arc_negative(Context, 192.0, 64.0, 40.0, 0.0, -2.0 * pi, !IO),
	
	cairo.set_fill_rule(Context, fill_rule_even_odd, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.7, 0.0, !IO),
	cairo.fill_preserve(Context, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.stroke(Context, !IO),

	transformations.translate(Context, 0.0, 128.0, !IO),
	path.rectangle(Context, 12.0, 12.0, 232.0, 70.0, !IO),
	path.new_sub_path(Context, !IO),
	path.arc(Context, 64.0, 64.0, 40.0, 0.0, 2.0 * pi, !IO),
	path.new_sub_path(Context, !IO),
	path.arc_negative(Context, 192.0, 64.0, 40.0,  0.0, -2.0 * pi,
            !IO),
	
	cairo.set_fill_rule(Context, fill_rule_winding, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.9, !IO),
	cairo.fill_preserve(Context, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "fill_style.png", !IO).

%----------------------------------------------------------------------------%
:- end_module fill_style.
%----------------------------------------------------------------------------%
