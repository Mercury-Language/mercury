:- module arc_negative.
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
	
	XC = 128.0,
	YC = 128.0,
	Radius = 100.0,
	Angle1 = 45.0 * (pi / 180.0),	% Angles are specified
	Angle2 = 180.0 * (pi / 180.0),	% in radians.
	
	cairo.set_line_width(Context, 10.0, !IO),
	cairo.path.arc_negative(Context, XC, YC, Radius, Angle1, Angle2, !IO),
	cairo.stroke(Context, !IO),
	
	% Draw helping lines.
	cairo.set_source_rgba(Context, 1.0, 0.2, 0.2, 0.6, !IO),
	cairo.set_line_width(Context, 6.0, !IO),

	cairo.path.arc(Context, XC, YC, 10.0, 0.0, 2.0 * pi, !IO),
	cairo.fill(Context, !IO),
	
	cairo.path.arc(Context, XC, YC, Radius, Angle1, Angle1, !IO),
	cairo.path.line_to(Context, XC, YC, !IO),

	cairo.path.arc(Context, XC, YC, Radius, Angle2, Angle2, !IO),
	cairo.path.line_to(Context, XC, YC, !IO),
	cairo.stroke(Context, !IO),

	write_surface_to_png(Surface, "arc_negative.png", !IO).

%----------------------------------------------------------------------------%
:- end_module arc_negative.
%----------------------------------------------------------------------------%
