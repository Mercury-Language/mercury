:- module clip_image.
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
:- import_module cairo.transformations.

:- import_module float.
:- import_module math.

%----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 256, 256, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),

	path.arc(Context, 128.0, 128.0, 76.8, 0.0, 2.0 * pi, !IO),
	cairo.clip(Context, !IO),
	new_path(Context, !IO),	% Path not consumed by clip/3.

	image_surface_create_from_png("data/romedalen.png", Image, !IO),
	image.get_width(Image, W, !IO),
	image.get_height(Image, H, !IO),

	cairo.transformations.scale(Context, 256.0 / float(W), 256.0 / float(H), !IO),
	cairo.set_source_surface(Context, Image, 0.0, 0.0, !IO),
	cairo.paint(Context, !IO),

	write_surface_to_png(Surface, "clip_image.png", !IO).

%----------------------------------------------------------------------------%
:- end_module clip_image.
%----------------------------------------------------------------------------%
