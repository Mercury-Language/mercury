%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.save_bitmap.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.save_bitmap.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.palette.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred save_bitmap(string::in, bitmap::in, palette::in, bool::out,
    io::di, io::uo) is det.
:- pred save_bmp(string::in, bitmap::in, palette::in, bool::out,
    io::di, io::uo) is det.
:- pred save_pcx(string::in, bitmap::in, palette::in, bool::out,
    io::di, io::uo) is det.
:- pred save_tga(string::in, bitmap::in, palette::in, bool::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    save_bitmap(Filename::in, Bitmap::in, Palette::in, Success::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == save_bitmap(Filename, Bitmap, Palette)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    save_bmp(Filename::in, Bitmap::in, Palette::in, Success::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == save_bmp(Filename, Bitmap, Palette)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    save_pcx(Filename::in, Bitmap::in, Palette::in, Success::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == save_pcx(Filename, Bitmap, Palette)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    save_tga(Filename::in, Bitmap::in, Palette::in, Success::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Success = (0 == save_tga(Filename, Bitmap, Palette)) ? MR_YES : MR_NO;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
