%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.flic.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.flic.
:- interface.

:- import_module allegro.bitmap.
:- import_module allegro.palette.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type memory_fli.

:- pred play_fli(string::in, bitmap::in, fli_status::out, io::di, io::uo)
    is det.
:- pred play_memory_fli(memory_fli::in, bitmap::in, fli_status::out,
    io::di, io::uo) is det.
:- pred open_fli(string::in, bool::out, io::di, io::uo) is det.
:- pred open_memory_fli(memory_fli::in, bool::out, io::di, io::uo) is det.
:- pred close_fli(io::di, io::uo) is det.
:- pred next_fli_frame(bool::in, fli_status::out, io::di, io::uo) is det.
:- pred fli_bitmap(maybe(bitmap)::out, io::di, io::uo) is det.
:- pred fli_palette(palette::out, io::di, io::uo) is det.
:- pred fli_bmp_dirty(int::out, int::out, io::di, io::uo) is det.
:- pred fli_pal_dirty(int::out, int::out, io::di, io::uo) is det.
:- pred reset_fli_variables(io::di, io::uo) is det.
:- pred fli_frame(int::out, io::di, io::uo) is det.
:- pred fli_timer(int::out, io::di, io::uo) is det.

:- type fli_status
    --->    ok
    ;       eof
    ;       not_open
    ;       error.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", memory_fli, "void *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    play_fli(Filename::in, Bitmap::in, Status::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int StatusInt = play_fli(Filename, Bitmap, FALSE, NULL);
    if (StatusInt == FLI_OK) {
        Status = _mal_make_fli_status_ok();
    } else {
        Status = _mal_make_fli_status_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    play_memory_fli(MemoryFLI::in, Bitmap::in, Status::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int StatusInt = play_memory_fli(MemoryFLI, Bitmap, FALSE, NULL);
    if (StatusInt == FLI_OK) {
        Status = _mal_make_fli_status_ok();
    } else {
        Status = _mal_make_fli_status_error();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    open_fli(Filename::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (FLI_OK == open_fli(Filename)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    open_memory_fli(MemoryFLI::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (FLI_OK == open_memory_fli(MemoryFLI)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    close_fli(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    close_fli();
    IO = IO0;
").

:- pragma foreign_proc("C",
    next_fli_frame(Loop::in, Status::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int StatusInt = next_fli_frame(Loop);
    switch (StatusInt) {
        case FLI_OK:
            Status = _mal_make_fli_status_ok();
            break;
        case FLI_EOF:
            Status = _mal_make_fli_status_eof();
            break;
        case FLI_NOT_OPEN:
            Status = _mal_make_fli_status_not_open();
            break;
        case FLI_ERROR:
        default:
            Status = _mal_make_fli_status_error();
            break;
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_bitmap(MaybeBitmap::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    BITMAP *Bitmap = fli_bitmap;
    if (Bitmap) {
        MaybeBitmap = _mal_make_yes_bitmap(Bitmap);
    } else {
        MaybeBitmap = _mal_make_no_bitmap();
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_palette(Palette::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Palette = fli_palette;
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_bmp_dirty(From::out, To::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    From = fli_bmp_dirty_from;
    To = fli_bmp_dirty_to;
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_pal_dirty(From::out, To::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    From = fli_pal_dirty_from;
    To = fli_pal_dirty_to;
    IO = IO0;
").

:- pragma foreign_proc("C",
    reset_fli_variables(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    reset_fli_variables();
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_frame(Frame::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Frame = fli_frame;
    IO = IO0;
").

:- pragma foreign_proc("C",
    fli_timer(Timer::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Timer = fli_timer;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func make_fli_status_ok = fli_status.
:- func make_fli_status_eof = fli_status.
:- func make_fli_status_not_open = fli_status.
:- func make_fli_status_error = fli_status.

:- pragma foreign_export("C", make_fli_status_ok = out,
    "_mal_make_fli_status_ok").
:- pragma foreign_export("C", make_fli_status_eof = out,
    "_mal_make_fli_status_eof").
:- pragma foreign_export("C", make_fli_status_not_open = out,
    "_mal_make_fli_status_not_open").
:- pragma foreign_export("C", make_fli_status_error = out,
    "_mal_make_fli_status_error").

make_fli_status_ok = ok.
make_fli_status_eof = eof.
make_fli_status_not_open = not_open.
make_fli_status_error = error.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
