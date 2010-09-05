:- module imagepattern.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cairo.
:- import_module cairo.image.
:- import_module cairo.matrix.
:- import_module cairo.path.
:- import_module cairo.pattern.
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

	pattern.create_for_surface(Image, Pattern, !IO),
	pattern.set_extend(Pattern, extend_repeat, !IO),

	transformations.translate(Context, 128.0, 128.0, !IO),
	transformations.rotate(Context, pi / 4.0, !IO),
	transformations.scale(Context, 1.0 / sqrt(2.0), 1.0 / sqrt(2.0), !IO),
	transformations.translate(Context, -128.0, -128.0, !IO),
	
	matrix.init_scale(float(W) / 256.0 * 5.0, float(H) / 256.0 * 5.0, Matrix, !IO),
	pattern.set_matrix(Pattern, Matrix, !IO),
	
	cairo.set_source(Context, Pattern, !IO),

	path.rectangle(Context, 0.0, 0.0, 256.0, 256.0, !IO),
	cairo.fill(Context, !IO),
	
	write_surface_to_png(Surface, "imagepattern.png", !IO).

%----------------------------------------------------------------------------%
:- end_module imagepattern.
%----------------------------------------------------------------------------%
