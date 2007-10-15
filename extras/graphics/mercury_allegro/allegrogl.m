%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegrogl.m.
% Author: wangp.
%
% A Mercury binding for AllegroGL.
%
%-----------------------------------------------------------------------------%

:- module allegrogl.
:- interface.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.text.
:- import_module mogl.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

%
% Core routines
%

:- pred install_allegro_gl(bool::out, io::di, io::uo) is det.
:- pred remove_allegro_gl(io::di, io::uo) is det.
:- pred flip(io::di, io::uo) is det.
:- pred opengl_version(float::out, io::di, io::uo) is det.

%
% Texture routines
%

:- pred check_texture_ex(list(texture_flag)::in, bitmap::in,
    mogl.texture_format::in, bool::out, io::di, io::uo) is det.

:- pred make_texture_ex(list(texture_flag)::in, bitmap::in,
    mogl.texture_format::in, maybe(mogl.texture_name)::out, io::di, io::uo)
    is det.

:- type texture_flag
    --->    mipmap
    ;       has_alpha
    ;       flip
    ;       masked
    ;       rescale
    ;       alpha_only.

%
% Matrix conversion routines
%

    % (unlikely to bind these)
    %
    % void allegro_gl_MATRIX_to_GLfloat (MATRIX *m, GLfloat gl[16]);
    % void allegro_gl_MATRIX_to_GLdouble (MATRIX *m, GLdouble gl[16]);
    % void allegro_gl_MATRIX_f_to_GLfloat (MATRIX_f *m, GLfloat gl[16]);
    % void allegro_gl_MATRIX_f_to_GLdouble (MATRIX_f *m, GLdouble gl[16]);
    %
    % void allegro_gl_GLfloat_to_MATRIX (GLfloat gl[16], MATRIX *m);
    % void allegro_gl_GLdouble_to_MATRIX (GLdouble gl[16], MATRIX *m);
    % void allegro_gl_GLfloat_to_MATRIX_f (GLfloat gl[16], MATRIX_f *m);
    % void allegro_gl_GLdouble_to_MATRIX_f (GLdouble gl[16], MATRIX_f *m);

% 
% Quaternion conversion routines
% 

    % (unlikely to bind these)
    %
    % void allegro_gl_apply_quat(QUAT *q);
    % void allegro_gl_quat_to_glrotatef(QUAT *q, float *angle,
    %                                   float *x, float *y, float *z);
    % void allegro_gl_quat_to_glrotated(QUAT *q, double *angle,
    %                                   double *x, double *y, double *z);

%
% Text drawing and font conversion
%

:- type agl_font.

:- type font_type
    --->    dont_care
    ;       bitmap
    ;       textured.

:- pred printf(agl_font::in, float::in, float::in, float::in, int::in, string::in,
    int::out, io::di, io::uo) is det.

:- pred printf_ex(agl_font::in, float::in, float::in, float::in, string::in,
    int::out, io::di, io::uo) is det.

:- pred convert_allegro_font_ex(font::in, font_type::in, float::in,
    mogl.texture_format::in, maybe(agl_font)::out, io::di, io::uo) is det.

:- pred destroy_font(agl_font::in, io::di, io::uo) is det.

:- pred list_font_textures(agl_font::in, list(texture_name)::out,
    io::di, io::uo) is det.

%
% Allegro-compatible GUI routines
%

    % void algl_set_mouse_drawer (void(*user_draw_mouse)(void))

:- pred draw_mouse(io::di, io::uo) is det.

:- pred alert(string::in, string::in, string::in, string::in, string::in,
    int::in, int::in, int::out, io::di, io::uo) is det.

:- pred alert3(string::in, string::in, string::in, string::in, string::in,
    string::in, int::in, int::in, int::in, int::out, io::di, io::uo) is det.

%
% Option settings
%

:- type option
    --->    red_depth
    ;       green_depth
    ;       blue_depth
    ;       alpha_depth
    ;       color_depth
    ;       acc_red_depth
    ;       acc_green_depth
    ;       acc_blue_depth
    ;       acc_alpha_depth
    ;       doublebuffer
    ;       stereo
    ;       aux_buffers
    ;       z_depth
    ;       stencil_depth
    ;       window_x
    ;       window_y
    ;       rendermethod
    ;       fullscreen
    ;       windowed
    ;       video_memory_policy
    ;       sample_buffers
    ;       samples
    ;       float_color
    ;       float_z.

    % video_memory_policy
:- func release = int.
:- func keep = int.

:- type priority
    --->    dont_care
    ;       suggest
    ;       require.

:- pred clear_settings(io::di, io::uo) is det.
:- pred set(option::in, int::in, io::di, io::uo) is det.
:- pred get(option::in, int::out, io::di, io::uo) is det.
:- pred set_priority(priority::in, option::in, io::di, io::uo) is det.
% :- pred get_priority(priority::in, list(option)::out,
%     io::di, io::uo) is det.
:- pred save_settings(io::di, io::uo) is det.
:- pred load_settings(io::di, io::uo) is det.

%
% Allegro interfacing
%

:- pred set_allegro_mode(io::di, io::uo) is det.
:- pred unset_allegro_mode(io::di, io::uo) is det.
:- pred set_projection(io::di, io::uo) is det.
:- pred unset_projection(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

:- pragma foreign_decl("C", "
    #include <alleggl.h>
    #include <allegro.mh>

    extern int __magl_font_type_flags[];
    extern int __magl_option_flags[];
    extern int __magl_priority_flags[];
").

%-----------------------------------------------------------------------------%
%
% Core routines
%

:- pragma foreign_proc("C",
    install_allegro_gl(Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    __mal_GFX_OPENGL = GFX_OPENGL;
    __mal_GFX_OPENGL_WINDOWED = GFX_OPENGL_WINDOWED;
    __mal_GFX_OPENGL_FULLSCREEN = GFX_OPENGL_FULLSCREEN;

    Result = (install_allegro_gl() == 0) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_allegro_gl(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    remove_allegro_gl();
    IO = IO0;
").

:- pragma foreign_proc("C",
    flip(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_flip();
    IO = IO0;
").

:- pragma foreign_proc("C",
    opengl_version(Version::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Version = allegro_gl_opengl_version();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Texture routines
%

:- func texture_flags_to_bits(list(texture_flag)) = int.

texture_flags_to_bits([])       = 0.
texture_flags_to_bits([F | Fs]) =
    texture_flag_to_bit(F) \/ texture_flags_to_bits(Fs).

:- func texture_flag_to_bit(texture_flag) = int.

texture_flag_to_bit(TF) = lookup_texture_flag_bit(texture_flag_to_int(TF)).

:- func texture_flag_to_int(texture_flag) = int.

texture_flag_to_int(mipmap)     = 0.
texture_flag_to_int(has_alpha)  = 1.
texture_flag_to_int(flip)       = 2.
texture_flag_to_int(masked)     = 3.
texture_flag_to_int(rescale)    = 4.
texture_flag_to_int(alpha_only) = 5.

:- func lookup_texture_flag_bit(int) = int.
:- pragma foreign_proc("C",
    lookup_texture_flag_bit(TextureFlagInt::in) = (Bit::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    static const int __magl_texture_flag_bits[] = {
        AGL_TEXTURE_MIPMAP,
        AGL_TEXTURE_HAS_ALPHA,
        AGL_TEXTURE_FLIP,
        AGL_TEXTURE_MASKED,
        AGL_TEXTURE_RESCALE,
        AGL_TEXTURE_ALPHA_ONLY
    };
    Bit = __magl_texture_flag_bits[TextureFlagInt];
").

check_texture_ex(TextureFlags, Bitmap, TextureFormat, CanBeTexture, !IO) :-
    TextureFlagsBits = texture_flags_to_bits(TextureFlags),
    check_texture_ex_2(TextureFlagsBits, Bitmap, TextureFormat,
        CanBeTexture, !IO).

:- pred check_texture_ex_2(int::in, bitmap::in, mogl.texture_format::in,
    bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    check_texture_ex_2(TextureFlagsBits::in, Bitmap::in, TextureFormatInt::in,
        CanBeTexture::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CanBeTexture = allegro_gl_check_texture_ex(TextureFlagsBits, Bitmap,
        (GLenum) TextureFormatInt) ? MR_YES : MR_NO;
    IO = IO0;
").

make_texture_ex(TextureFlags, Bitmap, TextureFormat, MaybeTextureName, !IO) :-
    TextureFlagsBits = texture_flags_to_bits(TextureFlags),
    make_texture_ex_2(TextureFlagsBits, Bitmap, TextureFormat,
        TextureName, !IO),
    MaybeTextureName = (if TextureName = 0 then no else yes(TextureName)).

:- pred make_texture_ex_2(int::in, bitmap::in, mogl.texture_format::in,
    mogl.texture_name::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    make_texture_ex_2(TextureFlagsBits::in, Bitmap::in, TextureFormat::in,
        TextureName::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    TextureName = allegro_gl_make_texture_ex(TextureFlagsBits, Bitmap,
        (GLenum) TextureFormat);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Text drawing and font conversion
%

:- pragma foreign_type("C", agl_font, "FONT *",
        [can_pass_as_mercury_type]).

:- func font_type_to_int(font_type) = int.

font_type_to_int(dont_care) = 0.
font_type_to_int(bitmap)    = 1.
font_type_to_int(textured)  = 2.

:- pragma foreign_code("C",
"
    int __magl_font_type_flags[] = {
        AGL_FONT_TYPE_DONT_CARE,
        AGL_FONT_TYPE_BITMAP,
        AGL_FONT_TYPE_TEXTURED
    };
").

:- pragma foreign_proc("C",
    printf(Fnt::in, X::in, Y::in, Z::in, Color::in, Str::in, Result::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = allegro_gl_printf(Fnt, X, Y, Z, Color, ""%s"", Str);
    IO = IO0;
").

:- pragma foreign_proc("C",
    printf_ex(Fnt::in, X::in, Y::in, Z::in, Str::in, Result::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = allegro_gl_printf_ex(Fnt, X, Y, Z, ""%s"", Str);
    IO = IO0;
").

convert_allegro_font_ex(Fnt, FontType, Scale, Format, MaybeAGLFont, !IO) :-
    convert_allegro_font_ex_2(Fnt, font_type_to_int(FontType), Scale,
        Format, MaybeAGLFont, !IO).

:- pred convert_allegro_font_ex_2(font::in, int::in, float::in,
    mogl.texture_format::in, maybe(agl_font)::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    convert_allegro_font_ex_2(Fnt::in, FontType::in, Scale::in, Format::in,
        MaybeAGLFont::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FONT *AGLFont = allegro_gl_convert_allegro_font_ex(Fnt,
        __magl_font_type_flags[FontType], Scale,
        (GLenum) Format);
    if (AGLFont) {
        MaybeAGLFont = __magl_yes_agl_font(AGLFont);
    } else {
        MaybeAGLFont = __magl_no_agl_font();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_font(AGLFont::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_destroy_font(AGLFont);
    IO = IO0;
").

:- pragma foreign_proc("C",
    list_font_textures(AGLFont::in, Textures::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    size_t NumIds = allegro_gl_list_font_textures(AGLFont, NULL, 0);
    {
        GLuint Ids[NumIds];
        int i;

        allegro_gl_list_font_textures(AGLFont, Ids, NumIds);
        Textures = MR_list_empty();
        for (i = NumIds-1; i >= 0; i--) {
            Textures = MR_list_cons(Ids[i], Textures);
        }
    }
    IO = IO0;
").

:- func yes_agl_font(agl_font) = maybe(agl_font).
:- pragma foreign_export("C", yes_agl_font(in) = out, "__magl_yes_agl_font").
yes_agl_font(F) = yes(F).

:- func no_agl_font = maybe(agl_font).
:- pragma foreign_export("C", no_agl_font = out, "__magl_no_agl_font").
no_agl_font = no.

%-----------------------------------------------------------------------------%
%
% Allegro-compatible GUI routines
%

:- pragma foreign_proc("C",
    draw_mouse(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    algl_draw_mouse();
    IO = IO0;
").

:- pragma foreign_proc("C",
    alert(S1::in, S2::in, S3::in, B1::in, B2::in, C1::in, C2::in, Ret::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ret = algl_alert(S1, S2, S3, B1, B2, C1, C2);
    IO = IO0;
").

:- pragma foreign_proc("C",
    alert3(S1::in, S2::in, S3::in, B1::in, B2::in, B3::in, C1::in, C2::in,
        C3::in, Ret::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ret = algl_alert3(S1, S2, S3, B1, B2, B3, C1, C2, C3);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Option settings
%

:- func option_to_int(option) = int.

option_to_int(red_depth)           = 0.
option_to_int(green_depth)         = 1.
option_to_int(blue_depth)          = 2.
option_to_int(alpha_depth)         = 3.
option_to_int(color_depth)         = 4.
option_to_int(acc_red_depth)       = 5.
option_to_int(acc_green_depth)     = 6.
option_to_int(acc_blue_depth)      = 7.
option_to_int(acc_alpha_depth)     = 8.
option_to_int(doublebuffer)        = 9.
option_to_int(stereo)              = 10.
option_to_int(aux_buffers)         = 11.
option_to_int(z_depth)             = 12.
option_to_int(stencil_depth)       = 13.
option_to_int(window_x)            = 14.
option_to_int(window_y)            = 15.
option_to_int(rendermethod)        = 16.
option_to_int(fullscreen)          = 17.
option_to_int(windowed)            = 18.
option_to_int(video_memory_policy) = 19.
option_to_int(sample_buffers)      = 20.
option_to_int(samples)             = 21.
option_to_int(float_color)         = 22.
option_to_int(float_z)             = 23.

:- pragma foreign_code("C",
"
    int __magl_option_flags[] = {
        AGL_RED_DEPTH,
        AGL_GREEN_DEPTH,
        AGL_BLUE_DEPTH,
        AGL_ALPHA_DEPTH,
        AGL_COLOR_DEPTH,
        AGL_ACC_RED_DEPTH,
        AGL_ACC_GREEN_DEPTH,
        AGL_ACC_BLUE_DEPTH,
        AGL_ACC_ALPHA_DEPTH,
        AGL_DOUBLEBUFFER,
        AGL_STEREO,
        AGL_AUX_BUFFERS,
        AGL_Z_DEPTH,
        AGL_STENCIL_DEPTH,
        AGL_WINDOW_X,
        AGL_WINDOW_Y,
        AGL_RENDERMETHOD,
        AGL_FULLSCREEN,
        AGL_WINDOWED,
        AGL_VIDEO_MEMORY_POLICY,
        AGL_SAMPLE_BUFFERS,
        AGL_SAMPLES,
        AGL_FLOAT_COLOR,
        AGL_FLOAT_Z
    };
").

:- func priority_to_int(priority) = int.

priority_to_int(dont_care) = 0.
priority_to_int(suggest)   = 1.
priority_to_int(require)   = 2.

:- pragma foreign_code("C",
"
    int __magl_priority_flags[] = {
        AGL_DONTCARE,
        AGL_SUGGEST,
        AGL_REQUIRE
    };
").

:- pragma foreign_proc("C",
    release = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = AGL_RELEASE;
").

:- pragma foreign_proc("C",
    keep = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = AGL_KEEP;
").

:- pragma foreign_proc("C",
    clear_settings(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_clear_settings();
    IO = IO0;
").

set(Option, Value, !IO) :-
    set_2(option_to_int(Option), Value, !IO).

:- pred set_2(int::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_2(Option::in, Value::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_set(__magl_option_flags[Option], Value);
    IO = IO0;
").

get(Option, Value, !IO) :-
    get_2(option_to_int(Option), Value, !IO).

:- pred get_2(int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_2(Option::in, Value::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = allegro_gl_get(__magl_option_flags[Option]);
    IO = IO0;
").

set_priority(Priority, Option, !IO) :-
    set_priority_2(priority_to_int(Priority), option_to_int(Option), !IO).

:- pred set_priority_2(int::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    set_priority_2(Priority::in, Option::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_set(__magl_priority_flags[Priority],
                    __magl_option_flags[Option]);
    IO = IO0;
").

:- pragma foreign_proc("C",
    save_settings(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_save_settings();
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_settings(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_load_settings();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Allegro interfacing
%

:- pragma foreign_proc("C",
    set_allegro_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_set_allegro_mode();
    IO = IO0;
").


:- pragma foreign_proc("C",
    unset_allegro_mode(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_unset_allegro_mode();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_projection(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_set_projection();
    IO = IO0;
").

:- pragma foreign_proc("C",
    unset_projection(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    allegro_gl_unset_projection();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module allegrogl.
%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 et wm=0 tw=0
