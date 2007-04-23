%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.text.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.text.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.color.

:- import_module char.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type font.

:- pred font(font::out, io::di, io::uo) is det.
:- pred allegro_404_char(char::out, io::di, io::uo) is det.
:- pred set_allegro_404_char(char::in, io::di, io::uo) is det.
:- pred text_length(font::in, string::in, int::out, io::di, io::uo) is det.
:- pred text_height(font::in, int::out, io::di, io::uo) is det.
:- pred textout_ex(bitmap::in, font::in, string::in, int::in, int::in, color::in, color::in, io::di, io::uo) is det.
:- pred textout_centre_ex(bitmap::in, font::in, string::in, int::in, int::in, color::in, color::in, io::di, io::uo) is det.
:- pred textout_right_ex(bitmap::in, font::in, string::in, int::in, int::in, color::in, color::in, io::di, io::uo) is det.
:- pred textout_justify_ex(bitmap::in, font::in, string::in, int::in, int::in, int::in, int::in, color::in, color::in, io::di, io::uo) is det.
% textprintf_ex
% textprintf_centre_ex
% textprintf_right_ex
% textprintf_justify_ex

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", font, "FONT *").

:- pragma foreign_proc("C",
    font(Fnt::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Fnt = font;
    IO = IO0;
").

:- pragma foreign_proc("C",
    allegro_404_char(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Get = allegro_404_char;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_allegro_404_char(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    allegro_404_char = Set;
    IO = IO0;
").

:- pragma foreign_proc("C",
    text_length(Font::in, Str::in, Len::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Len = text_length(Font, Str);
    IO = IO0;
").

:- pragma foreign_proc("C",
    text_height(Font::in, Height::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Height = text_height(Font);
    IO = IO0;
").

:- pragma foreign_proc("C",
    textout_ex(Bitmap::in, Font::in, Str::in, X::in, Y::in, Color::in, Bg::in,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    textout_ex(Bitmap, Font, Str, X, Y, Color, Bg);
    IO = IO0;
").

:- pragma foreign_proc("C",
    textout_centre_ex(Bitmap::in, Font::in, Str::in, X::in, Y::in, Color::in,
        Bg::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    textout_centre_ex(Bitmap, Font, Str, X, Y, Color, Bg);
    IO = IO0;
").

:- pragma foreign_proc("C",
    textout_right_ex(Bitmap::in, Font::in, Str::in, X::in, Y::in, Color::in,
        Bg::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    textout_right_ex(Bitmap, Font, Str, X, Y, Color, Bg);
    IO = IO0;
").

:- pragma foreign_proc("C",
    textout_justify_ex(Bitmap::in, Font::in, Str::in, X1::in, X2::in, Y::in,
        Diff::in, Color::in, Bg::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    textout_justify_ex(Bitmap, Font, Str, X1, X2, Y, Diff, Color, Bg);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
