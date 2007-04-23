%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.mouse.m.
% Author: wangp.
%
%-----------------------------------------------------------------------------%

:- module allegro.mouse.
:- interface.

:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred install_mouse(install_mouse_result::out, io::di, io::uo) is det.
:- pred remove_mouse(io::di, io::uo) is det.
% poll_mouse
% mouse_needs_poll
:- pred enable_hardware_cursor(io::di, io::uo) is det.
:- pred disable_hardware_cursor(io::di, io::uo) is det.
:- pred select_mouse_cursor(mouse_cursor::in, io::di, io::uo) is det.
:- pred set_mouse_cursor_bitmap(mouse_cursor::in, bitmap::in, io::di, io::uo) is det.
:- pred set_default_mouse_cursor_bitmap(mouse_cursor::in, io::di, io::uo) is det.
:- pred mouse_x(int::out, io::di, io::uo) is det.
:- pred mouse_y(int::out, io::di, io::uo) is det.
:- pred mouse_z(int::out, io::di, io::uo) is det.
:- pred mouse_b(int::out, io::di, io::uo) is det.
:- pred mouse_pos(int::out, io::di, io::uo) is det.
:- pred mouse_sprite(bitmap::out, io::di, io::uo) is det.
:- pred mouse_xy_focus(int::out, int::out, io::di, io::uo) is det.
:- pred show_mouse(bitmap::in, io::di, io::uo) is det.
:- pred hide_mouse(io::di, io::uo) is det.
:- pred scare_mouse(io::di, io::uo) is det.
:- pred scare_mouse_area(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred unscare_mouse(io::di, io::uo) is det.
:- pred show_os_cursor(mouse_cursor::in, bool::out, io::di, io::uo) is det.
:- pred freeze_mouse_flag(bool::out, io::di, io::uo) is det.
:- pred set_freeze_mouse_flag(bool::in, io::di, io::uo) is det.
:- pred position_mouse(int::in, int::in, io::di, io::uo) is det.
:- pred position_mouse_z(int::in, io::di, io::uo) is det.
:- pred set_mouse_range(int::in, int::in, int::in, int::in, io::di, io::uo) is det.
:- pred set_mouse_speed(int::in, int::in, io::di, io::uo) is det.
:- pred set_mouse_sprite(bitmap::in, io::di, io::uo) is det.
:- pred set_default_mouse_sprite(io::di, io::uo) is det.
:- pred set_mouse_sprite_focus(int::in, int::in, io::di, io::uo) is det.
:- pred get_mouse_mickeys(int::out, int::out, io::di, io::uo) is det.
% mouse_callback

:- type install_mouse_result
    --->    ok(int)
    ;       already_installed
    ;       failed.

:- type mouse_cursor
    --->    mouse_cursor_none
    ;       mouse_cursor_allegro
    ;       mouse_cursor_arrow
    ;       mouse_cursor_busy
    ;       mouse_cursor_question
    ;       mouse_cursor_edit.

    % Additions to C API.
    %
:- pred mouse_xy(int::out, int::out, io::di, io::uo) is det.

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
    install_mouse(Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    int N = install_mouse();
    if (N < 0) {
        Result = _mal_make_failed_install_mouse_result();
    } else if (N == 0) {
        Result = _mal_make_already_installed_install_mouse_result();
    } else {
        Result = _mal_make_ok_install_mouse_result(N);
    }
    IO = IO0;
").

:- pragma foreign_proc("C",
    remove_mouse(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    remove_mouse();
    IO = IO0;
").

:- pragma foreign_proc("C",
    enable_hardware_cursor(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    enable_hardware_cursor();
    IO = IO0;
").

:- pragma foreign_proc("C",
    disable_hardware_cursor(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    disable_hardware_cursor();
    IO = IO0;
").

select_mouse_cursor(Cursor, !IO) :-
    select_mouse_cursor_2(mouse_cursor_to_int(Cursor), !IO).

:- pred select_mouse_cursor_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    select_mouse_cursor_2(Cursor::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    select_mouse_cursor(Cursor);
    IO = IO0;
").

set_mouse_cursor_bitmap(Cursor, Bitmap, !IO) :-
    set_mouse_cursor_bitmap_2(mouse_cursor_to_int(Cursor), Bitmap, !IO).

:- pred set_mouse_cursor_bitmap_2(int::in, bitmap::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_mouse_cursor_bitmap_2(Cursor::in, Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_cursor_bitmap(Cursor, Bitmap);
    IO = IO0;
").

set_default_mouse_cursor_bitmap(Cursor, !IO) :-
    set_default_mouse_cursor_bitmap_2(mouse_cursor_to_int(Cursor), !IO).

:- pred set_default_mouse_cursor_bitmap_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_default_mouse_cursor_bitmap_2(Cursor::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_cursor_bitmap(Cursor, NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_x(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = mouse_x;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_y(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = mouse_y;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_z(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = mouse_z;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_b(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = mouse_b;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_pos(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Get = mouse_pos;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_sprite(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Get = mouse_sprite;
    IO = IO0;
").

:- pragma foreign_proc("C",
    mouse_xy_focus(X::out, Y::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = mouse_x_focus;
    Y = mouse_y_focus;
    IO = IO0;
").

:- pragma foreign_proc("C",
    show_mouse(Bitmap::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    show_mouse(Bitmap);
    IO = IO0;
").

:- pragma foreign_proc("C",
    hide_mouse(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    show_mouse(NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    scare_mouse(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    scare_mouse();
    IO = IO0;
").

:- pragma foreign_proc("C",
    scare_mouse_area(X::in, Y::in, W::in, H::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    scare_mouse_area(X, Y, W, H);
    IO = IO0;
").

:- pragma foreign_proc("C",
    unscare_mouse(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    unscare_mouse();
    IO = IO0;
").

show_os_cursor(Cursor, Success, !IO) :-
    show_os_cursor_2(mouse_cursor_to_int(Cursor), Success, !IO).

:- pred show_os_cursor_2(int::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    show_os_cursor_2(Cursor::in, Success::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Success = (0 == show_os_cursor(Cursor)) ? MR_YES : MR_NO;
    IO = IO0;
").

:- pragma foreign_proc("C",
    freeze_mouse_flag(Get::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Get = freeze_mouse_flag;
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_freeze_mouse_flag(Set::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    freeze_mouse_flag = Set ? TRUE : FALSE;
    IO = IO0;
").

:- pragma foreign_proc("C",
    position_mouse(X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    position_mouse(X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    position_mouse_z(Z::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    position_mouse_z(Z);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_mouse_range(X1::in, Y1::in, X2::in, Y2::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_range(X1, Y1, X2, Y2);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_mouse_speed(XSpeed::in, YSpeed::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_speed(XSpeed, YSpeed);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_mouse_sprite(Sprite::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_sprite(Sprite);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_default_mouse_sprite(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_sprite(NULL);
    IO = IO0;
").

:- pragma foreign_proc("C",
    set_mouse_sprite_focus(X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    set_mouse_sprite_focus(X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    get_mouse_mickeys(X::out, Y::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int X0, Y0;
    get_mouse_mickeys(&X0, &Y0);
    X = X0;
    Y = Y0;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    mouse_xy(X::out, Y::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = mouse_x;
    Y = mouse_y;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func make_ok_install_mouse_result(int) = install_mouse_result.
:- func make_already_installed_install_mouse_result = install_mouse_result.
:- func make_failed_install_mouse_result = install_mouse_result.

:- pragma foreign_export("C",
    make_ok_install_mouse_result(in) = out,
    "_mal_make_ok_install_mouse_result").
:- pragma foreign_export("C",
    make_already_installed_install_mouse_result = out,
    "_mal_make_already_installed_install_mouse_result").
:- pragma foreign_export("C",
    make_failed_install_mouse_result = out,
    "_mal_make_failed_install_mouse_result").

make_ok_install_mouse_result(X) = ok(X).
make_already_installed_install_mouse_result = already_installed.
make_failed_install_mouse_result = failed.

%-----------------------------------------------------------------------------%

:- func mouse_cursor_to_int(mouse_cursor) = int.

mouse_cursor_to_int(mouse_cursor_none) = mouse_cursor_none_int.
mouse_cursor_to_int(mouse_cursor_allegro) = mouse_cursor_allegro_int.
mouse_cursor_to_int(mouse_cursor_arrow) = mouse_cursor_arrow_int.
mouse_cursor_to_int(mouse_cursor_busy) = mouse_cursor_busy_int.
mouse_cursor_to_int(mouse_cursor_question) = mouse_cursor_question_int.
mouse_cursor_to_int(mouse_cursor_edit) = mouse_cursor_edit_int.

:- func mouse_cursor_none_int = int.
:- func mouse_cursor_allegro_int = int.
:- func mouse_cursor_arrow_int = int.
:- func mouse_cursor_busy_int = int.
:- func mouse_cursor_question_int = int.
:- func mouse_cursor_edit_int = int.

:- pragma foreign_proc("C",
    mouse_cursor_none_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_NONE;").
:- pragma foreign_proc("C",
    mouse_cursor_allegro_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_ALLEGRO;").
:- pragma foreign_proc("C",
    mouse_cursor_arrow_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_ARROW;").
:- pragma foreign_proc("C",
    mouse_cursor_busy_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_BUSY;").
:- pragma foreign_proc("C",
    mouse_cursor_question_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_QUESTION;").
:- pragma foreign_proc("C",
    mouse_cursor_edit_int = (K::out),
    [will_not_call_mercury, promise_pure, thread_safe],
    "K = MOUSE_CURSOR_EDIT;").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
