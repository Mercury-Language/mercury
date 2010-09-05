:- module set_line_cap.
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

	cairo.set_line_width(Context, 30.0, !IO),
	cairo.set_line_cap(Context, line_cap_butt, !IO),	% Default.
	path.move_to(Context, 64.0, 50.0, !IO),
	path.line_to(Context, 64.0, 200.0, !IO),
	cairo.stroke(Context, !IO),
	cairo.set_line_cap(Context, line_cap_round, !IO),
	path.move_to(Context, 128.0, 50.0, !IO),
	path.line_to(Context, 128.0, 200.0, !IO),
	cairo.stroke(Context, !IO),
	cairo.set_line_cap(Context, line_cap_square, !IO),
	path.move_to(Context, 192.0, 50.0, !IO),
	path.line_to(Context, 192.0, 200.0, !IO),
	cairo.stroke(Context, !IO),

	% Draw helping lines.
	cairo.set_source_rgb(Context, 1.0, 0.2, 0.2, !IO),
	cairo.set_line_width(Context, 2.56, !IO),
	path.move_to(Context, 64.0, 50.0, !IO),
	path.line_to(Context, 64.0, 200.0, !IO),
	path.move_to(Context, 128.0, 50.0, !IO),
	path.line_to(Context, 128.0, 200.0, !IO),
	path.move_to(Context, 192.0, 50.0, !IO),
	path.line_to(Context, 192.0, 200.0, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "set_line_cap.png", !IO).

%----------------------------------------------------------------------------%
:- end_module set_line_cap.
%----------------------------------------------------------------------------%
