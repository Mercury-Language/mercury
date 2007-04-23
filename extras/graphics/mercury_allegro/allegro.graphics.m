%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.graphics.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.graphics.
:- interface.

:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred set_color_depth(color_depth::in, io::di, io::uo) is det.
:- pred get_color_depth(color_depth::out, io::di, io::uo) is det.
:- pred request_refresh_rate(int::in, io::di, io::uo) is det.
:- pred get_refresh_rate(maybe(int)::out, io::di, io::uo) is det.
% :- pred get_gfx_mode_list
% :- pred destroy_gfx_mode_list
:- pred set_gfx_mode(gfx_card::in, int::in, int::in, int::in, int::in, bool::out, io::di, io::uo) is det.
:- pred set_display_switch_mode(switch_mode::in, bool::out, io::di, io::uo) is det.
% :- pred set_display_switch_callback(switch_dir::in, (pred(io, io))::(pred(di, uo) is det)) is det.
% :- pred remove_display_switch_callback((pred(io, io))::(pred(di, uo) is det)) is det.
:- pred get_display_switch_mode(switch_mode::out, io::di, io::uo) is det.
:- pred is_windowed_mode(bool::out, io::di, io::uo) is det.
:- pred gfx_capabilities(int::out, io::di, io::uo) is det.
:- pred enable_triple_buffer(bool::out, io::di, io::uo) is det.
:- pred scroll_screen(int::in, int::in, io::di, io::uo) is det.
:- pred request_scroll(int::in, int::in, io::di, io::uo) is det.
:- pred poll_scroll(poll_scroll_status::out, io::di, io::uo) is det.
:- pred show_video_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred request_video_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred vsync(io::di, io::uo) is det.

:- type color_depth == int.

:- type gfx_card
    --->    gfx_autodetect
    ;       gfx_autodetect_windowed
    ;       gfx_autodetect_fullscreen
    ;       gfx_safe
    ;       gfx_text

    ;       gfx_opengl
    ;       gfx_opengl_windowed
    ;       gfx_opengl_fullscreen.

:- type switch_mode
    --->    switch_none
    ;       switch_pause
    ;       switch_amnesia
    ;       switch_background
    ;       switch_backamnesia.

:- type poll_scroll_status
    --->    still_waiting
    ;       done.

    % Addition to C API.
    %
:- pred set_text_mode(io::di, io::uo) is det.

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
    set_color_depth(Depth::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_color_depth(Depth);
    IO0 = IO;
").

:- pragma foreign_proc("C",
    get_color_depth(Depth::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Depth = get_color_depth();
    IO0 = IO;
").

:- pragma foreign_proc("C",
    request_refresh_rate(Rate::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    request_refresh_rate(Rate);
    IO = IO0;
").

get_refresh_rate(MaybeRate, !IO) :-
    get_refresh_rate_2(Rate, !IO),
    MaybeRate = (if Rate = 0 then no else yes(Rate)).

:- pred get_refresh_rate_2(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    get_refresh_rate_2(Rate::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Rate = get_refresh_rate();
    IO = IO0;
").

set_gfx_mode(Card, W, H, VW, VH, Success, !IO) :-
    set_gfx_mode_2(gfx_card_to_int(Card), W, H, VW, VH, Success, !IO).

:- pred set_gfx_mode_2(int::in, int::in, int::in, int::in, int::in, bool::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_gfx_mode_2(Card::in, W::in, H::in, VW::in, VH::in, Success::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == set_gfx_mode(Card, W, H, VW, VH)) ? MR_YES : MR_NO;
    IO = IO0;
").

set_text_mode(!IO) :-
    set_gfx_mode(gfx_text, 0, 0, 0, 0, _Result, !IO).

:- pragma foreign_proc("C",
    set_display_switch_mode(SwitchMode::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == set_display_switch_mode(SwitchMode)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_display_switch_mode(SwitchMode::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    SwitchMode = get_display_switch_mode();
    IO = IO0;
").

:- pragma foreign_proc("C",
    is_windowed_mode(Is::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Is = is_windowed_mode() ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    gfx_capabilities(Caps::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Caps = gfx_capabilities;
    IO = IO0;
").

:- pragma foreign_proc("C",
    enable_triple_buffer(Enabled::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Enabled = enable_triple_buffer() ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    scroll_screen(X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    scroll_screen(X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    request_scroll(X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    request_scroll(X, Y);
    IO = IO0;
").

poll_scroll(Status, !IO) :-
    poll_scroll_2(StatusInt, !IO),
    Status = (if StatusInt = 0 then done else still_waiting).

:- pred poll_scroll_2(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    poll_scroll_2(StatusInt::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    StatusInt = poll_scroll();
    IO = IO0;
").

:- pragma foreign_proc("C",
    show_video_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    show_video_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    request_video_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    request_video_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    vsync(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    vsync();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func gfx_card_to_int(gfx_card) = int.

gfx_card_to_int(gfx_autodetect) = gfx_autodetect_int.
gfx_card_to_int(gfx_autodetect_windowed) = gfx_autodetect_windowed_int.
gfx_card_to_int(gfx_autodetect_fullscreen) = gfx_autodetect_fullscreen_int.
gfx_card_to_int(gfx_safe) = gfx_safe_int.
gfx_card_to_int(gfx_text) = gfx_text_int.
gfx_card_to_int(gfx_opengl) = gfx_opengl_int.
gfx_card_to_int(gfx_opengl_windowed) = gfx_opengl_windowed_int.
gfx_card_to_int(gfx_opengl_fullscreen) = gfx_opengl_fullscreen_int.

:- func gfx_autodetect_int = int.
:- func gfx_autodetect_windowed_int = int.
:- func gfx_autodetect_fullscreen_int = int.
:- func gfx_safe_int = int.
:- func gfx_text_int = int.
:- func gfx_opengl_int = int.
:- func gfx_opengl_windowed_int = int.
:- func gfx_opengl_fullscreen_int = int.

:- pragma foreign_proc("C", gfx_autodetect_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = GFX_AUTODETECT;").
:- pragma foreign_proc("C", gfx_autodetect_windowed_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = GFX_AUTODETECT_WINDOWED;").
:- pragma foreign_proc("C", gfx_autodetect_fullscreen_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = GFX_AUTODETECT_FULLSCREEN;").
:- pragma foreign_proc("C", gfx_safe_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = GFX_SAFE;").
:- pragma foreign_proc("C", gfx_text_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = GFX_TEXT;").

    % AllegroGL
:- pragma foreign_proc("C", gfx_opengl_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = __mal_GFX_OPENGL;").
:- pragma foreign_proc("C", gfx_opengl_windowed_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = __mal_GFX_OPENGL_WINDOWED;").
:- pragma foreign_proc("C", gfx_opengl_fullscreen_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = __mal_GFX_OPENGL_FULLSCREEN;").

%-----------------------------------------------------------------------------%

:- func switch_mode_to_int(switch_mode) = int.

switch_mode_to_int(switch_none) = switch_none_int.
switch_mode_to_int(switch_pause) = switch_pause_int.
switch_mode_to_int(switch_amnesia) = switch_amnesia_int.
switch_mode_to_int(switch_background) = switch_background_int.
switch_mode_to_int(switch_backamnesia) = switch_backamnesia_int.

:- func switch_none_int = int.
:- func switch_pause_int = int.
:- func switch_amnesia_int = int.
:- func switch_background_int = int.
:- func switch_backamnesia_int = int.

:- pragma foreign_proc("C", switch_none_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = SWITCH_NONE;").
:- pragma foreign_proc("C", switch_pause_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = SWITCH_PAUSE;").
:- pragma foreign_proc("C", switch_amnesia_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = SWITCH_AMNESIA;").
:- pragma foreign_proc("C", switch_background_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = SWITCH_BACKGROUND;").
:- pragma foreign_proc("C", switch_backamnesia_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = SWITCH_BACKAMNESIA;").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
