%-----------------------------------------------------------------------------%

:- module mask.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.pattern.
:- import_module cairo.path.
:- import_module cairo.png.
:- import_module cairo.transformations.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 120, 120, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 120.0, 120.0, !IO),

	cairo.pattern.create_linear(0.0, 0.0, 1.0, 1.0, LinPat, !IO),
	cairo.pattern.add_color_stop_rgb(LinPat, 0.0, 0.0, 0.3, 0.8, !IO),
	cairo.pattern.add_color_stop_rgb(LinPat, 1.0, 0.0, 0.8, 0.3, !IO),

	cairo.pattern.create_radial(0.5, 0.5, 0.25, 0.5, 0.5, 0.75, RadPat, !IO),
	cairo.pattern.add_color_stop_rgba(RadPat, 0.0, 0.0, 0.0, 0.0, 1.0, !IO),
	cairo.pattern.add_color_stop_rgba(RadPat, 0.5, 0.0, 0.0, 0.0, 0.0, !IO),

	cairo.set_source(Context, LinPat, !IO),
	cairo.mask(Context, RadPat, !IO),

	cairo.png.write_surface_to_png(Surface, "mask.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module mask.
%-----------------------------------------------------------------------------%
