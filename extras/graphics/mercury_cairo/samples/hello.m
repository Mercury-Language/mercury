%-----------------------------------------------------------------------------%

:- module hello.
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
:- import_module cairo.png.
:- import_module cairo.text.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 240, 80, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.text.select_font_face(Context, "serif", slant_normal, weight_bold,
		!IO),
	cairo.text.set_font_size(Context, 32.0, !IO),
	cairo.set_source_rgb(Context, 0.0, 0.0, 1.0, !IO),
	cairo.path.move_to(Context, 10.0, 50.0, !IO),
	cairo.text.show_text(Context, "Hello World", !IO),
	write_surface_to_png(Surface, "hello.png", !IO).

%-----------------------------------------------------------------------------%
:- end_module hello.
%-----------------------------------------------------------------------------%
