:- module dash.
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
:- import_module list.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	Dashes = [50.0, 10.0, 10.0, 10.0],
	OffSet = -50.0,

	cairo.set_dash(Context, Dashes, OffSet, !IO),
	cairo.set_line_width(Context, 10.0, !IO),
	
	cairo.path.move_to(Context, 128.0, 25.6, !IO),
	cairo.path.line_to(Context, 230.4, 230.4, !IO),
	cairo.path.rel_line_to(Context, -102.4, 0.0, !IO),
	cairo.path.curve_to(Context, 51.2, 230.4, 51.2, 128.0, 128.0, 128.0,
		!IO),

	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "dash.png", !IO).

%----------------------------------------------------------------------------%
:- end_module dash.
%----------------------------------------------------------------------------%
