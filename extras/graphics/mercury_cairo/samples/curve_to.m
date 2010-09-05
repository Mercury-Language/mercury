:- module curve_to.
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

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	X = 25.6, Y = 128.0,
	X1 = 102.4, Y1 = 230.4,
	X2 = 153.6, Y2 = 25.6,
	X3 = 230.4, Y3 = 128.0,

	path.move_to(Context, X, Y, !IO),
	path.curve_to(Context, X1, Y1, X2, Y2, X3, Y3, !IO),
	
	cairo.set_line_width(Context, 10.0, !IO),
	cairo.stroke(Context, !IO),
	
	cairo.set_source_rgba(Context, 1.0, 0.2, 0.2, 0.6, !IO),
	cairo.set_line_width(Context, 6.0, !IO),
	path.move_to(Context, X, Y, !IO),
	path.line_to(Context, X1, Y1, !IO),
	path.move_to(Context, X2, Y2, !IO),
	path.line_to(Context, X3, Y3, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "curve_to.png", !IO).

%----------------------------------------------------------------------------%
:- end_module curve_to.
%----------------------------------------------------------------------------%

