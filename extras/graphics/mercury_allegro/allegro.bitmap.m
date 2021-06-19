%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.bitmap.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.bitmap.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type bitmap.

:- pred screen(maybe(bitmap)::out, io::di, io::uo) is det.
:- pred screen_w(int::out, io::di, io::uo) is det.
:- pred screen_h(int::out, io::di, io::uo) is det.
:- pred virtual_w(int::out, io::di, io::uo) is det.
:- pred virtual_h(int::out, io::di, io::uo) is det.
:- pred create_bitmap(int::in, int::in, maybe(bitmap)::out, io::di, io::uo) is det.
:- pred create_bitmap_ex(int::in, int::in, int::in, maybe(bitmap)::out, io::di, io::uo) is det.
:- pred create_sub_bitmap(bitmap::in, int::in, int::in, int::in, int::in, maybe(bitmap)::out, io::di, io::uo) is det.
:- pred create_video_bitmap(int::in, int::in, maybe(bitmap)::out, io::di, io::uo) is det.
:- pred create_system_bitmap(int::in, int::in, maybe(bitmap)::out, io::di, io::uo) is det.
:- pred destroy_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred bitmap_color_depth(bitmap::in, int::out, io::di, io::uo) is det.
:- pred bitmap_mask_color(bitmap::in, int::out, io::di, io::uo) is det.
:- pred is_same_bitmap(bitmap::in, bitmap::in, bool::out) is det.
:- pred is_planar_bitmap(bitmap::in, bool::out) is det.
:- pred is_linear_bitmap(bitmap::in, bool::out) is det.
:- pred is_memory_bitmap(bitmap::in, bool::out) is det.
:- pred is_screen_bitmap(bitmap::in, bool::out) is det.
:- pred is_video_bitmap(bitmap::in, bool::out) is det.
:- pred is_system_bitmap(bitmap::in, bool::out) is det.
:- pred is_sub_bitmap(bitmap::in, bool::out) is det.
:- pred acquire_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred release_bitmap(bitmap::in, io::di, io::uo) is det.
:- pred acquire_screen(io::di, io::uo) is det.
:- pred release_screen(io::di, io::uo) is det.
:- pred set_clip_rect(bitmap::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred get_clip_rect(bitmap::in, int::out, int::out, int::out, int::out, io::di, io::uo) is det.
:- pred add_clip_rect(bitmap::in, int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_clip_state(bitmap::in, bool::in, io::di, io::uo) is det.
:- pred get_clip_state(bitmap::in, bool::out, io::di, io::uo) is det.
:- pred is_inside_bitmap(bitmap::in, int::in, int::in, bool::in, bool::out, io::di, io::uo) is det.

    % Additions to C API.
    %
:- func bitmap_w(bitmap) = int.
:- func bitmap_h(bitmap) = int.
:- pred bitmap_size(bitmap::in, int::out, int::out) is det.

    % Convenience wrappers.
    %
:- pred det_screen(bitmap::out, io::di, io::uo) is det.
:- pred det_screen(bitmap::out, int::out, int::out, io::di, io::uo) is det.
:- pred det_create_bitmap(int::in, int::in, bitmap::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- pragma foreign_decl("C", "
    #define key allegro_mercury_key
    #include <allegro.h>
    #undef key
").

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", bitmap, "BITMAP *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    screen(Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Result = screen ? _mal_make_yes_bitmap(screen) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    screen_w(Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Result = SCREEN_W;
    IO = IO0;
").

:- pragma foreign_proc("C",
    screen_h(Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Result = SCREEN_H;
    IO = IO0;
").

:- pragma foreign_proc("C",
    virtual_w(Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Result = VIRTUAL_W;
    IO = IO0;
").

:- pragma foreign_proc("C",
    virtual_h(Result::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Result = VIRTUAL_H;
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_bitmap(Width::in, Height::in, Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    BITMAP *bmp = create_bitmap(Width, Height);
    Result = bmp ? _mal_make_yes_bitmap(bmp) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_bitmap_ex(Depth::in, Width::in, Height::in, Result::out,
        IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    BITMAP *bmp = create_bitmap_ex(Depth, Width, Height);
    Result = bmp ? _mal_make_yes_bitmap(bmp) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_sub_bitmap(Parent::in, X::in, Y::in, Width::in, Height::in,
        Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    BITMAP *bmp = create_sub_bitmap(Parent, X, Y, Width, Height);
    Result = bmp ? _mal_make_yes_bitmap(bmp) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_video_bitmap(Width::in, Height::in, Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    BITMAP *bmp = create_video_bitmap(Width, Height);
    Result = bmp ? _mal_make_yes_bitmap(bmp) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    create_system_bitmap(Width::in, Height::in, Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
    BITMAP *bmp = create_system_bitmap(Width, Height);
    Result = bmp ? _mal_make_yes_bitmap(bmp) : _mal_make_no_bitmap();
    IO = IO0;
").

:- pragma foreign_proc("C",
    destroy_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    destroy_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    bitmap_color_depth(Bitmap::in, Depth::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Depth = bitmap_color_depth(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    bitmap_mask_color(Bitmap::in, Mask::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Mask = bitmap_mask_color(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    is_same_bitmap(BitmapA::in, BitmapB::in, IsSame::out),
    [will_not_call_mercury, promise_pure],
"
    IsSame = is_same_bitmap(BitmapA, BitmapB);
").

:- pragma foreign_proc("C",
    is_planar_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_planar_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_linear_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_linear_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_memory_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_memory_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_screen_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_screen_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_video_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_video_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_system_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_system_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    is_sub_bitmap(Bitmap::in, Is::out),
    [will_not_call_mercury, promise_pure],
"
    Is = is_sub_bitmap(Bitmap);
").

:- pragma foreign_proc("C",
    acquire_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    acquire_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    release_bitmap(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    release_bitmap(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    acquire_screen(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    acquire_screen();
    IO = IO0;
").

:- pragma foreign_proc("C",
    release_screen(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    release_screen();
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_clip_rect(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_clip_rect(Bitmap, X1, Y1, X2, Y2);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_clip_rect(Bitmap::in, X1::out, Y1::out, X2::out, Y2::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    int _X1, _Y1, _X2, _Y2;
    get_clip_rect(Bitmap, &_X1, &_Y1, &_X2, &_Y2);
    X1 = _X1;
    Y1 = _Y1;
    X2 = _X2;
    Y2 = _Y2;
    IO = IO0;
").

:- pragma foreign_proc("C",
    add_clip_rect(Bitmap::in, X1::in, Y1::in, X2::in, Y2::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    add_clip_rect(Bitmap, X1, Y1, X2, Y2);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_clip_state(Bitmap::in, State::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    set_clip_state(Bitmap, State);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_clip_state(Bitmap::in, State::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    State = get_clip_state(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    is_inside_bitmap(Bitmap::in, X::in, Y::in, Clip::in, IsInside::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    IsInside = is_inside_bitmap(Bitmap, X, Y, Clip);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    bitmap_w(Bitmap::in) = (Get::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = Bitmap->w;
").

:- pragma foreign_proc("C",
    bitmap_h(Bitmap::in) = (Get::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = Bitmap->h;
").

:- pragma foreign_proc("C",
    bitmap_size(Bitmap::in, Width::out, Height::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Width = Bitmap->w;
    Height = Bitmap->h;
").

%-----------------------------------------------------------------------------%

:- pragma inline(pred(det_screen/3)).

det_screen(Screen, !IO) :-
    screen(MaybeScreen, !IO),
    (
        MaybeScreen = yes(Screen)
    ;
        MaybeScreen = no,
        error("screen_det/3")
    ).

:- pragma inline(pred(det_screen/5)).

det_screen(Screen, Width, Height, !IO) :-
    det_screen(Screen, !IO),
    screen_w(Width, !IO),
    screen_h(Height, !IO).

det_create_bitmap(W, H, Bitmap, !IO) :-
    create_bitmap(W, H, MaybeBitmap, !IO),
    (
        MaybeBitmap = yes(Bitmap)
    ;
        MaybeBitmap = no,
        error("create_bitmap_det/5")
    ).

%-----------------------------------------------------------------------------%

:- func c_pointer_to_bitmap(c_pointer) = bitmap.
:- pragma foreign_export("C", c_pointer_to_bitmap(in) = out,
    "_mal_c_pointer_to_bitmap").

:- pragma foreign_proc("C",
    c_pointer_to_bitmap(CPtr::in) = (Bitmap::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Bitmap = (BITMAP *)CPtr;
").

%-----------------------------------------------------------------------------%

:- func make_yes_bitmap(bitmap) = maybe(bitmap).
:- func make_no_bitmap = maybe(bitmap).

:- pragma foreign_export("C", make_yes_bitmap(in) = out,
    "_mal_make_yes_bitmap").
:- pragma foreign_export("C", make_no_bitmap = out, "_mal_make_no_bitmap").

make_yes_bitmap(X) = yes(X).
make_no_bitmap = no.

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
