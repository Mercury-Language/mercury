%-----------------------------------------------------------------------------%

:- module setsourcegradient.
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
:- import_module cairo.pattern.
:- import_module cairo.png.
:- import_module cairo.transformations.

:- import_module float.
:- import_module int.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 120, 120, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 120.0, 120.0, !IO),

	cairo.pattern.create_radial(0.25, 0.25, 0.1, 0.5, 0.5, 0.5, RadPat, !IO),
	cairo.pattern.add_color_stop_rgb(RadPat, 0.0, 1.0, 0.8, 0.8, !IO),
	cairo.pattern.add_color_stop_rgb(RadPat, 1.0, 0.9, 0.0, 0.0, !IO),

	int.fold_up(draw_rectangle(Context), 1, 9, !IO),

	cairo.set_source(Context, RadPat, !IO),
	cairo.fill(Context, !IO),
	
	cairo.pattern.create_linear(0.25, 0.35, 0.75, 0.65, LinPat, !IO),
	cairo.pattern.add_color_stop_rgba(LinPat, 0.00, 1.0, 1.0, 1.0, 0.0, !IO),
	cairo.pattern.add_color_stop_rgba(LinPat, 0.25, 0.0, 1.0, 0.0, 0.5, !IO),
	cairo.pattern.add_color_stop_rgba(LinPat, 0.50, 1.0, 1.0, 1.0, 0.0, !IO),
	cairo.pattern.add_color_stop_rgba(LinPat, 0.75, 0.0, 0.0, 1.0, 0.5, !IO),
	cairo.pattern.add_color_stop_rgba(LinPat, 1.00, 1.0, 1.0, 1.0, 0.0, !IO),

	cairo.path.rectangle(Context, 0.0, 0.0, 1.0, 1.0, !IO),
	cairo.set_source(Context, LinPat, !IO),
	cairo.fill(Context, !IO),

	cairo.png.write_surface_to_png(Surface, "setsourcegradient.png", !IO).

:- pred draw_rectangle(context(S)::in, int::in, io::di, io::uo)
    is det <= surface(S).

draw_rectangle(Context, I, !IO) :-
	int.fold_up(draw_rectangle_2(Context, I), 1, 9, !IO).

:- pred draw_rectangle_2(context(S)::in, int::in, int::in, io::di, io::uo)
    is det <= surface(S).

draw_rectangle_2(Context, I, J, !IO) :-
	cairo.path.rectangle(Context, float(I) / 10.0 - 0.04,
	     float(J) / 10.0 - 0.04, 0.08, 0.08, !IO).

%-----------------------------------------------------------------------------%
:- end_module setsourcegradient.
%-----------------------------------------------------------------------------%
