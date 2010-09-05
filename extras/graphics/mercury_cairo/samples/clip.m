:- module clip.
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

	path.arc(Context, 128.0, 128.0, 76.8, 0.0, 2.0 * pi, !IO),
	cairo.clip(Context, !IO),

	new_path(Context, !IO),  % Current path is not consumed
	                         % by cairo.clip/3.

	path.rectangle(Context, 0.0, 0.0, 256.0, 256.0, !IO),
	cairo.fill(Context, !IO),
	cairo.set_source_rgb(Context, 0.0, 1.0, 0.0, !IO),
	path.move_to(Context, 0.0, 0.0, !IO),
	path.line_to(Context, 256.0, 256.0, !IO),
	path.move_to(Context, 256.0, 0.0, !IO),
	path.line_to(Context, 0.0, 256.0, !IO),
	cairo.set_line_width(Context, 10.0, !IO),
	cairo.stroke(Context, !IO),
	
	write_surface_to_png(Surface, "clip.png", !IO).

%----------------------------------------------------------------------------%
:- end_module clip.
%----------------------------------------------------------------------------%
