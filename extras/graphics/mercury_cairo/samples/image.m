:- module image.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.png.
:- import_module cairo.transformations.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	image_surface_create_from_png("data/romedalen.png", Image, !IO),
	image.get_width(Image, W, !IO),
	image.get_height(Image, H, !IO),

	cairo.transformations.translate(Context, 128.0, 128.0, !IO),
	cairo.transformations.rotate(Context, 45.0 * pi / 180.0, !IO),
	cairo.transformations.scale(Context, 256.0 / float(W), 256.0 / float(H), !IO),
	cairo.transformations.translate(Context, -0.5 * float(W), -0.5 * float(H), !IO),

	cairo.set_source_surface(Context, Image, 0.0, 0.0, !IO),
	cairo.paint(Context, !IO),
	
	write_surface_to_png(Surface, "image.png", !IO).

%----------------------------------------------------------------------------%
:- end_module image.
%----------------------------------------------------------------------------%
