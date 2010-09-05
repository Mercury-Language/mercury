:- module multi_segment_caps.
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
	
	path.move_to(Context, 50.0, 75.0, !IO),
	path.line_to(Context, 200.0, 75.0, !IO),
	
	path.move_to(Context, 50.0, 125.0, !IO),
	path.line_to(Context, 200.0, 125.0, !IO),

	path.move_to(Context, 50.0, 175.0, !IO),
	path.line_to(Context, 200.0, 175.0, !IO),

	cairo.set_line_width(Context, 30.0, !IO),
	cairo.set_line_cap(Context, line_cap_round, !IO),
	cairo.stroke(Context, !IO),
		
	write_surface_to_png(Surface, "multi_segment_caps.png", !IO).

%----------------------------------------------------------------------------%
:- end_module multi_segment_caps.
%----------------------------------------------------------------------------%
