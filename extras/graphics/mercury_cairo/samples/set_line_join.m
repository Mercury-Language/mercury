:- module set_line_join.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.path.
:- import_module cairo.image.
:- import_module cairo.png.

:- import_module float.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	
	cairo.set_line_width(Context, 40.96, !IO),
	path.move_to(Context, 76.8, 84.43, !IO),
	path.rel_line_to(Context, 51.2, -51.2, !IO),
	path.rel_line_to(Context, 51.2, 51.2, !IO),
	cairo.set_line_join(Context, line_join_miter, !IO),	% Default.
	cairo.stroke(Context, !IO),

	path.move_to(Context, 76.8, 161.28, !IO),
	path.rel_line_to(Context, 51.2, -51.2, !IO),
	path.rel_line_to(Context, 51.2, 51.2, !IO),
	cairo.set_line_join(Context, line_join_bevel, !IO),
	cairo.stroke(Context, !IO),

	path.move_to(Context, 76.8, 238.08, !IO),
	path.rel_line_to(Context, 51.2, -51.2, !IO),
	path.rel_line_to(Context, 51.2, 51.2, !IO),
	cairo.set_line_join(Context, line_join_round, !IO),
	cairo.stroke(Context, !IO),
	
	write_surface_to_png(Surface, "set_line_join.png", !IO).

%----------------------------------------------------------------------------%
:- end_module set_line_join.
%----------------------------------------------------------------------------%
