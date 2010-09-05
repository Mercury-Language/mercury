%-----------------------------------------------------------------------------%

:- module tips_letter.
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
:- import_module cairo.transformations.

:- import_module char.
:- import_module int.
:- import_module float.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
	cairo.image.create_surface(format_argb32, 780, 30, Surface, !IO),
	cairo.create_context(Surface, Context, !IO),
	cairo.transformations.scale(Context, 30.0, 30.0, !IO),
	cairo.text.set_font_size(Context, 0.8, !IO),

	Alphabet = "AbCdEfGhIjKlMnOpQrStUvWxYz",

	cairo.set_source_rgb(Context, 0.0, 0.0, 0.0, !IO),
	cairo.text.select_font_face(Context, "Georgia", slant_normal,
		weight_bold, !IO),

	string.foldl2(draw_letter(Context), Alphabet, 0, _, !IO),

	cairo.png.write_surface_to_png(Surface, "tips_letter.png", !IO).


:- pred draw_letter(context(S)::in, char::in, int::in, int::out,
    io::di, io::uo) is det <= surface(S).

draw_letter(Context, LetterChar, !I, !IO) :-
	Letter = string.from_char(LetterChar),
	cairo.text.text_extents(Context, Letter, TE, !IO),
	cairo.path.move_to(Context,
		float(!.I) + 0.5 - TE ^ te_x_bearing - TE ^ te_width / 2.0,
		0.5 - TE ^ te_y_bearing - TE ^ te_height / 2.0, !IO),
	cairo.text.show_text(Context, Letter, !IO),
	!:I = !.I + 1.
	
%-----------------------------------------------------------------------------%
:- end_module tips_letter.
%-----------------------------------------------------------------------------%
