%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.load_bitmap.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.load_bitmap.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.palette.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type load_bitmap_result
    --->    ok(bitmap, palette)
    ;       error.

:- type load_bitmap_proc == pred(string, load_bitmap_result, io, io).
:- type save_bitmap_proc == pred(string, bitmap, palette, bool, io, io).

:- type colorconv == int.

:- pred load_bitmap(string::in, load_bitmap_result::out, io::di, io::uo) is det.
:- pred load_bmp(string::in, load_bitmap_result::out, io::di, io::uo) is det.
:- pred load_lbm(string::in, load_bitmap_result::out, io::di, io::uo) is det.
:- pred load_pcx(string::in, load_bitmap_result::out, io::di, io::uo) is det.
:- pred load_tga(string::in, load_bitmap_result::out, io::di, io::uo) is det.
% :- pred register_bitmap_file_type(string::in, load_bitmap_proc::in, save_bitmap_proc::in, io::di, io::uo) is det.
:- pred set_color_conversion(int::in, io::di, io::uo) is det.
:- pred get_color_conversion(int::out, io::di, io::uo) is det.

:- func colorconv_none = int.
:- func colorconv_8_to_15 = int.
:- func colorconv_8_to_16 = int.
:- func colorconv_8_to_24 = int.
:- func colorconv_8_to_32 = int.
:- func colorconv_15_to_8 = int.
:- func colorconv_15_to_16 = int.
:- func colorconv_15_to_24 = int.
:- func colorconv_15_to_32 = int.
:- func colorconv_16_to_8 = int.
:- func colorconv_16_to_15 = int.
:- func colorconv_16_to_24 = int.
:- func colorconv_16_to_32 = int.
:- func colorconv_24_to_8 = int.
:- func colorconv_24_to_15 = int.
:- func colorconv_24_to_16 = int.
:- func colorconv_24_to_32 = int.
:- func colorconv_32_to_8 = int.
:- func colorconv_32_to_15 = int.
:- func colorconv_32_to_16 = int.
:- func colorconv_32_to_24 = int.
:- func colorconv_32A_to_8 = int.
:- func colorconv_32A_to_15 = int.
:- func colorconv_32A_to_16 = int.
:- func colorconv_32A_to_24 = int.
:- func colorconv_dither_pal = int.
:- func colorconv_dither_hi = int.
:- func colorconv_keep_trans = int.

:- func colorconv_expand_256 = int.
:- func colorconv_reduce_to_256 = int.
:- func colorconv_expand_15_to_16 = int.
:- func colorconv_reduce_16_to_15 = int.
:- func colorconv_expand_hi_to_true = int.
:- func colorconv_reduce_true_to_hi = int.
:- func colorconv_24_equals_32 = int.
:- func colorconv_total = int.
:- func colorconv_partial = int.
:- func colorconv_most = int.
:- func colorconv_dither = int.
:- func colorconv_keep_alpha = int.

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
    load_bitmap(Filename::in, Result::out, IO0::di, IO::uo),
    [promise_pure],
"
    RGB *Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    BITMAP *Bitmap = load_bitmap(Filename, Palette);
    if (Bitmap) {
        Result = _mal_make_load_bitmap_result_ok(Bitmap, Palette);
    } else {
        Result = _mal_make_load_bitmap_result_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_bmp(Filename::in, Result::out, IO0::di, IO::uo),
    [promise_pure],
"
    RGB *Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    BITMAP *Bitmap = load_bmp(Filename, Palette);
    if (Bitmap) {
        Result = _mal_make_load_bitmap_result_ok(Bitmap, Palette);
    } else {
        Result = _mal_make_load_bitmap_result_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_lbm(Filename::in, Result::out, IO0::di, IO::uo),
    [promise_pure],
"
    RGB *Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    BITMAP *Bitmap = load_lbm(Filename, Palette);
    if (Bitmap) {
        Result = _mal_make_load_bitmap_result_ok(Bitmap, Palette);
    } else {
        Result = _mal_make_load_bitmap_result_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_pcx(Filename::in, Result::out, IO0::di, IO::uo),
    [promise_pure],
"
    RGB *Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    BITMAP *Bitmap = load_pcx(Filename, Palette);
    if (Bitmap) {
        Result = _mal_make_load_bitmap_result_ok(Bitmap, Palette);
    } else {
        Result = _mal_make_load_bitmap_result_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    load_tga(Filename::in, Result::out, IO0::di, IO::uo),
    [promise_pure],
"
    RGB *Palette = MR_GC_NEW_ARRAY(RGB, PAL_SIZE);
    BITMAP *Bitmap = load_tga(Filename, Palette);
    if (Bitmap) {
        Result = _mal_make_load_bitmap_result_ok(Bitmap, Palette);
    } else {
        Result = _mal_make_load_bitmap_result_error();
    }
    IO = IO0;
").

% :- pragma foreign_proc("C",
%     register_bitmap_file_type(Ext::in, Loader::in, Saver::in,
%       IO0::di, IO::uo),
%     [will_not_call_mercury, promise_pure],
% "
%     register_bitmap_file_type(Ext, Loader, Saver);
%     IO = IO0;
% ").

:- pragma foreign_proc("C",
    set_color_conversion(Mode::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_color_conversion(Mode);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_color_conversion(Mode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Mode = get_color_conversion();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", colorconv_none = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_NONE;").
:- pragma foreign_proc("C", colorconv_8_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_8_TO_15;").
:- pragma foreign_proc("C", colorconv_8_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_8_TO_16;").
:- pragma foreign_proc("C", colorconv_8_to_24 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_8_TO_24;").
:- pragma foreign_proc("C", colorconv_8_to_32 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_8_TO_32;").
:- pragma foreign_proc("C", colorconv_15_to_8 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_15_TO_8;").
:- pragma foreign_proc("C", colorconv_15_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_15_TO_16;").
:- pragma foreign_proc("C", colorconv_15_to_24 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_15_TO_24;").
:- pragma foreign_proc("C", colorconv_15_to_32 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_15_TO_32;").
:- pragma foreign_proc("C", colorconv_16_to_8 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_16_TO_8;").
:- pragma foreign_proc("C", colorconv_16_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_16_TO_15;").
:- pragma foreign_proc("C", colorconv_16_to_24 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_16_TO_24;").
:- pragma foreign_proc("C", colorconv_16_to_32 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_16_TO_32;").
:- pragma foreign_proc("C", colorconv_24_to_8 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_24_TO_8;").
:- pragma foreign_proc("C", colorconv_24_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_24_TO_15;").
:- pragma foreign_proc("C", colorconv_24_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_24_TO_16;").
:- pragma foreign_proc("C", colorconv_24_to_32 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_24_TO_32;").
:- pragma foreign_proc("C", colorconv_32_to_8 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32_TO_8;").
:- pragma foreign_proc("C", colorconv_32_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32_TO_15;").
:- pragma foreign_proc("C", colorconv_32_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32_TO_16;").
:- pragma foreign_proc("C", colorconv_32_to_24 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32_TO_24;").
:- pragma foreign_proc("C", colorconv_32A_to_8 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32A_TO_8;").
:- pragma foreign_proc("C", colorconv_32A_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32A_TO_15;").
:- pragma foreign_proc("C", colorconv_32A_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32A_TO_16;").
:- pragma foreign_proc("C", colorconv_32A_to_24 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_32A_TO_24;").
:- pragma foreign_proc("C", colorconv_dither_pal = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_DITHER_PAL;").
:- pragma foreign_proc("C", colorconv_dither_hi = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_DITHER_HI;").
:- pragma foreign_proc("C", colorconv_keep_trans = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_KEEP_TRANS;").
:- pragma foreign_proc("C", colorconv_expand_256 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_EXPAND_256;").
:- pragma foreign_proc("C", colorconv_reduce_to_256 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_REDUCE_TO_256;").
:- pragma foreign_proc("C", colorconv_expand_15_to_16 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_EXPAND_15_TO_16;").
:- pragma foreign_proc("C", colorconv_reduce_16_to_15 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_REDUCE_16_TO_15;").
:- pragma foreign_proc("C", colorconv_expand_hi_to_true = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_EXPAND_HI_TO_TRUE;").
:- pragma foreign_proc("C", colorconv_reduce_true_to_hi = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_REDUCE_TRUE_TO_HI;").
:- pragma foreign_proc("C", colorconv_24_equals_32 = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_24_EQUALS_32;").
:- pragma foreign_proc("C", colorconv_total = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_TOTAL;").
:- pragma foreign_proc("C", colorconv_partial = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_PARTIAL;").
:- pragma foreign_proc("C", colorconv_most = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_MOST;").
:- pragma foreign_proc("C", colorconv_dither = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_DITHER;").
:- pragma foreign_proc("C", colorconv_keep_alpha = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = COLORCONV_KEEP_ALPHA;").

%-----------------------------------------------------------------------------%

:- func make_load_bitmap_result_ok(bitmap, palette) = load_bitmap_result.
:- pragma export(make_load_bitmap_result_ok(in, in) = out,
    "_mal_make_load_bitmap_result_ok").

make_load_bitmap_result_ok(Bmp, Pal) = ok(Bmp, Pal).

:- func make_load_bitmap_result_error = load_bitmap_result.
:- pragma export(make_load_bitmap_result_error = out,
    "_mal_make_load_bitmap_result_error").

make_load_bitmap_result_error = error.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
