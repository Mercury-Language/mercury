%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% This sub-module provides support for rendering text.
%
%----------------------------------------------------------------------------%

:- module cairo.text.
:- interface.

%----------------------------------------------------------------------------%

    % Specifies variants of a font face based upon their slant.
    %
:- type font_slant
    --->    slant_normal
    ;       slant_italic
    ;       slant_oblique.

    % Specifies variants of a font face based on their weight.
    %
:- type font_weight
    --->    weight_normal
    ;       weight_bold.

:- type font_family == string.

:- type glyph
    --->    glyph(
                glyph_index :: int,
                glyph_x     :: float,
                glyph_y     :: float
            ).

    % The extents of a text string in user-space.
    %
:- type text_extents
    --->    text_extents(
                te_x_bearing  :: float,
                te_y_bearing  :: float,
                te_width      :: float,
                te_height     :: float,
                te_x_advance  :: float,
                te_y_advance  :: float
            ).

:- type font_extents
    --->    font_extents(
                fe_ascent        :: float,
                fe_descent       :: float,
                fe_height        :: float,
                fe_max_x_advance :: float,
                fe_max_y_advance :: float
            ).

%---------------------------------------------------------------------------%

    % text.select_font_face(Context, Family, Slant, Weight, !IO):
    % Selects a family and style of font from a simplified description as a
    % Family name, Slant and Weight.
    %
:- pred select_font_face(context(T)::in, font_family::in, font_slant::in,
    font_weight::in, io::di, io::uo) is det.

    % text.set_font_size(Context, Size, !IO):
    % Sets the current font matrix to a scale by a factor of Size.
    %
:- pred set_font_size(context(S)::in, float::in, io::di, io::uo) is det.

    % text.set_font_matrix(Context, Matrix, !IO):
    % Set the current font matrix for Context to Matrix.
    %
:- pred set_font_matrix(context(S)::in, matrix::in, io::di, io::uo) is det.

    % text.get_font_matrix(Context, Matrix, !IO):
    % Matrix is the current font matrix for Context.
    %
:- pred get_font_matrix(context(S)::in, matrix::out, io::di, io::uo) is det.

    % text.set_font_options(Context, FontOptions, !IO):
    % Set the custom font rendering options for Context to FontOptions.
    %
:- pred set_font_options(context(S)::in, font_options::in,
    io::di, io::uo) is det.

    % text.get_font_options(Context, FontOptions, !IO):
    % FontOptions are the custom font rendering options for Context.
    %
:- pred get_font_options(context(S)::in, font_options::out,
    io::di, io::uo) is det.

    % text.set_font_face(Context, FontFace, !IO):
    % Replace the current font face for Context with FontFace.
    %
:- pred set_font_face(context(S)::in, F::in, io::di, io::uo) is det
    <= font_face(F).

    % text.set_default_font_face(Context, !IO):
    % Replace the current font face for Context with the default font face.
    %
:- pred set_default_font_face(context(S)::in, io::di, io::uo) is det.

    % text.get_font_face(Context, FontFace, !IO):
    % FontFace is the current font face for Context.
    %
:- some [F] pred get_font_face(context(S)::in, F::out, io::di, io::uo) is det
    => font_face(F).

    % text.show_text(Context, Text, !IO):
    %
:- pred show_text(context(S)::in, string::in, io::di, io::uo) is det.

    % text.show_glyphs(Context, Glyphs, !IO):
    %
:- pred show_glyphs(context(S)::in, list(glyph)::in, io::di, io::uo) is det.

:- pred font_extents(context(S)::in, font_extents::out, io::di, io::uo)
    is det.

    % text.extents(Context, String, Extents, !IO):
    % Extents is the text extents of String.
    %
:- pred text_extents(context(S)::in, string::in, text_extents::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% The "toy" font face
%

:- type toy_font_face.

:- instance font_face(toy_font_face).

    % toy_font_face_create(Family::in, Slant::in, FontFace::out, !IO):
    %
:- pred toy_font_face_create(string::in, font_slant::in, font_weight::in,
    toy_font_face::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_enum("C", font_slant/0, [
    slant_normal  - "CAIRO_FONT_SLANT_NORMAL",
    slant_italic  - "CAIRO_FONT_SLANT_ITALIC",
    slant_oblique - "CAIRO_FONT_SLANT_OBLIQUE"
]).

:- pragma foreign_enum("C", font_weight/0, [
    weight_normal - "CAIRO_FONT_WEIGHT_NORMAL",
    weight_bold   - "CAIRO_FONT_WEIGHT_BOLD"
]).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    set_font_size(Context::in, Size::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_font_size(Context->mcairo_raw_context, Size);
").

select_font_face(Context, Family, Slant, Weight, !IO) :-
    % Calling the cairo_set_font_face() directly won't keep the cached
    % copy of the font face up-to-date.
    cairo.text.toy_font_face_create(Family, Slant, Weight, FF, !IO),
    cairo.text.set_font_face(Context, FF, !IO).

:- pragma foreign_proc("C",
    set_font_matrix(Ctxt::in, Matrix::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_set_font_matrix(Ctxt->mcairo_raw_context, Matrix);
").

:- pragma foreign_proc("C",
    get_font_matrix(Ctxt::in, Matrix::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_matrix_t  *font_matrix;

    font_matrix = MR_GC_NEW(cairo_matrix_t);
    cairo_get_font_matrix(Ctxt->mcairo_raw_context, font_matrix);
    Matrix = font_matrix;
").

:- pragma foreign_proc("C",
    show_text(Ctxt::in, Text::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_show_text(Ctxt->mcairo_raw_context, Text);
").

show_glyphs(Ctxt, Glyphs, !IO) :-
    make_glyph_array(Glyphs, Array, NumGlyphs, !IO),
    show_glyphs_array(Ctxt, Array, NumGlyphs, !IO).

:- pred show_glyphs_array(context(T)::in, glyph_array::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    show_glyphs_array(Ctxt::in, Array::in, NumGlyphs::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_show_glyphs(Ctxt->mcairo_raw_context, Array, NumGlyphs);
").

:- pragma foreign_proc("C",
    set_font_options(Ctxt::in, FntOpts::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_set_font_options(Ctxt->mcairo_raw_context,
        FntOpts->mcairo_raw_font_options);
").

:- pragma foreign_proc("C",
    get_font_options(Ctxt::in, FntOpts::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_options_t    *raw_font_options;
    raw_font_options = cairo_font_options_create();
    cairo_get_font_options(Ctxt->mcairo_raw_context, raw_font_options);
    FntOpts = MR_GC_NEW(MCAIRO_font_options);
    FntOpts->mcairo_raw_font_options = raw_font_options;
    MR_GC_register_finalizer(FntOpts, MCAIRO_finalize_font_options, 0);
").

set_font_face(Context, FF, !IO) :-
    CachedFF = 'new font_face_container'(FF),
    set_font_face_2(Context, FF, CachedFF, !IO).

:- pred set_font_face_2(context(S)::in, F::in,
    font_face_container::in, _IO0::di, _IO::uo) is det.

:- pragma foreign_proc("C",
    set_font_face_2(Ctxt::in, FF::in, CachedFF::in, _IO::di, _IO0::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_set_font_face(Ctxt->mcairo_raw_context,
        ((MCAIRO_font_face *)FF)->mcairo_raw_font_face);
    Ctxt->mcairo_cached_font_face = CachedFF;
").

set_default_font_face(Context, !IO) :-
    cairo.text.toy_font_face_create("",
        slant_normal, weight_normal, ToyFF, !IO),
    cairo.text.set_font_face(Context, ToyFF, !IO).

get_font_face(Context, FF, !IO) :-
    get_cached_font_face(Context, CachedFF, !IO),
    % TODO: we should have a sanity check that cairo's current
    % font face is the same as the one we cached.
    CachedFF = font_face_container(FF).

:- pred get_cached_font_face(context(S)::in, font_face_container::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_cached_font_face(Ctxt::in, CachedFF::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    CachedFF = Ctxt->mcairo_cached_font_face;
").

font_extents(Context, Extents, !IO) :-
    font_extents_2(Context, Ascent, Descent, Height, MaxXAdv, MaxYAdv, !IO),
    Extents = font_extents(
        Ascent,
        Descent,
        Height,
        MaxXAdv,
        MaxYAdv).

:- pred font_extents_2(context(S)::in, float::out, float::out, float::out,
    float::out, float::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    font_extents_2(Ctxt::in, Ascent::out, Descent::out, Height::out,
        MaxXAdv::out, MaxYAdv::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_font_extents_t    extents;
    cairo_font_extents(Ctxt->mcairo_raw_context, &extents);

    Ascent = extents.ascent;
    Descent = extents.descent;
    Height = extents.height;
    MaxXAdv = extents.max_x_advance;
    MaxYAdv = extents.max_y_advance;
").

text_extents(Context, String, Extents, !IO) :-
    text_extents_2(Context, String, X_Bearing, Y_Bearing, Width, Height,
        X_Advance, Y_Advance, !IO),
    Extents = text_extents(
        X_Bearing,
        Y_Bearing,
        Width,
        Height,
        X_Advance,
        Y_Advance).

:- pred text_extents_2(context(T)::in, string::in, float::out, float::out,
    float::out, float::out, float::out, float::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    text_extents_2(Ctxt::in, Str::in, X_Bearing::out, Y_Bearing::out,
        Width::out, Height::out, X_Advance::out, Y_Advance::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    cairo_text_extents_t    extents;
    cairo_text_extents(Ctxt->mcairo_raw_context, Str, &extents);

    X_Bearing = extents.x_bearing;
    Y_Bearing = extents.y_bearing;
    Width = extents.width;
    Height = extents.height;
    X_Advance = extents.x_advance;
    Y_Advance = extents.y_advance;
").

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", toy_font_face, "MCAIRO_font_face *").

:- instance font_face(toy_font_face) where [].

:- pragma foreign_proc("C",
    toy_font_face_create(Family::in, Slant::in, Weight::in, FontFace::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    cairo_font_face_t   *raw_font_face;

    raw_font_face = cairo_toy_font_face_create(Family, Slant, Weight);
    FontFace = MR_GC_NEW(MCAIRO_font_face);
    FontFace->mcairo_raw_font_face = raw_font_face;
    MR_GC_register_finalizer(FontFace, MCAIRO_finalize_font_face, 0);
").

%---------------------------------------------------------------------------%
:- end_module cairo.text.
%---------------------------------------------------------------------------%
