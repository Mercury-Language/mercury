:- module gradient.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.path.
:- import_module cairo.pattern.
:- import_module cairo.png.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	
	pattern.create_linear(0.0, 0.0, 0.0, 256.0, LinPat, !IO),
	pattern.add_color_stop_rgba(LinPat, 1.0, 0.0, 0.0, 0.0, 1.0, !IO),
	pattern.add_color_stop_rgba(LinPat, 0.0, 1.0, 1.0, 1.0, 1.0, !IO),
	path.rectangle(Context, 0.0, 0.0, 256.0, 256.0, !IO),
	cairo.set_source(Context, LinPat, !IO),
	cairo.fill(Context, !IO),

	pattern.create_radial(115.2, 102.4, 25.6, 102.4, 102.4, 128.0,
	    RadPat, !IO),
	pattern.add_color_stop_rgba(RadPat, 0.0, 1.0, 1.0, 1.0, 1.0, !IO),
	pattern.add_color_stop_rgba(RadPat, 1.0, 0.0, 0.0, 0.0, 1.0, !IO),
	cairo.set_source(Context, RadPat, !IO),
	path.arc(Context, 128.0, 128.0, 76.8, 0.0, 2.0 * pi, !IO),
	cairo.fill(Context, !IO),

	write_surface_to_png(Surface, "gradient.png", !IO).

%----------------------------------------------------------------------------%
:- end_module gradient.
%----------------------------------------------------------------------------%
