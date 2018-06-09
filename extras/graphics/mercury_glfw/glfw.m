%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2012 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% A Mercury binding to GLFW version 2.7.
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
%-----------------------------------------------------------------------------%

:- module glfw.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%
%
% Initialization and Termination.
%
    % Initialize GLFW.
    % This predicate must be called before any other predicates in this
    % module are called.
    % Throws a software_error/1 exception if initialisation fails.
    %
:- pred glfw.init(io::di, io::uo) is det.

    % Terminate GLFW.
    %
:- pred glfw.terminate(io::di, io::uo) is det.

    % glfw.get_version(Major, Minor, Revision, !IO):
    % Return the GLFW library version.
    %
:- pred glfw.get_version(int::out, int::out, int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Window Handling.
%

:- type window_mode
    --->    window
    ;       fullscreen.

:- pred glfw.open_window(int::in, int::in, int::in, int::in, int::in, int::in,
    int::in, int::in, window_mode::in, io::di, io::uo) is det.

:- type window_hint
    --->    refresh_rate(int)
    ;       accum_red_bits(int)
    ;       accum_green_bits(int)
    ;       accum_blue_bits(int)
    ;       accum_alpha_bits(int)
    ;       aux_buffers(int)
    ;       stereo(bool)
    ;       window_no_resize(bool)
    ;       fsaa_samples(int)
    ;       opengl_version_major(int)
    ;       opengl_version_minor(int)
    ;       opengl_forward_compat(bool)
    ;       opengl_debug_context(bool)
    ;       opengl_profile(int).

:- pred glfw.open_window_hint(window_hint::in, io::di, io::uo) is det.

:- pred glfw.close_window(io::di, io::uo) is det.

:- type window_close_callback == pred(bool, io, io).
:- inst window_close_callback == (pred(out, di, uo) is det).

:- pred glfw.set_window_close_callback(
    window_close_callback::in(window_close_callback), io::di, io::uo) is det.
:- pred glfw.unset_window_close_callback(io::di, io::uo) is det.

    % NOTE: the title string needs to be Latin-1.
    %
:- pred glfw.set_window_title(string::in, io::di, io::uo) is det.

:- pred glfw.set_window_size(int::in, int::in, io::di, io::uo) is det.

:- pred glfw.set_window_pos(int::in, int::in, io::di, io::uo) is det.

:- pred glfw.get_window_size(int::out, int::out, io::di, io::uo) is det.

:- type window_size_callback == pred(int, int, io, io).
:- inst window_size_callback == (pred(in, in, di, uo) is det).

:- pred glfw.set_window_size_callback(
    window_size_callback::in(window_size_callback), io::di, io::uo) is det.
:- pred glfw.unset_window_size_callback(io::di, io::uo) is det.

:- pred glfw.iconify_window(io::di, io::uo) is det.

:- pred glfw.restore_window(io::di, io::uo) is det.

:- type bool_window_param
    --->    opened
    ;       active
    ;       iconified
    ;       accelerated
    ;       stereo
    ;       window_no_resize
    ;       opengl_forward_compat
    ;       opengl_debug_context.

:- pred glfw.get_bool_window_param(bool_window_param::in, bool::out,
    io::di, io::uo) is det.

:- type int_window_param
    --->    red_bits
    ;       green_bits
    ;       blue_bits
    ;       alpha_bits
    ;       depth_bits
    ;       stencil_bits
    ;       refresh_rate
    ;       accum_red_bits
    ;       accum_green_bits
    ;       accum_blue_bits
    ;       accum_alpha_bits
    ;       aux_buffers
    ;       opengl_version_major
    ;       opengl_version_minor
    ;       opengl_profile.

:- pred glfw.get_int_window_param(int_window_param::in, int::out,
    io::di, io::uo) is det.

:- pred glfw.swap_buffers(io::di, io::uo) is det.

:- pred glfw.swap_interval(int::in, io::di, io::uo) is det.

:- type window_refresh_callback == pred(io, io).
:- inst window_refresh_callback == (pred(di, uo) is det).

:- pred glfw.set_window_refresh_callback(
    window_refresh_callback::in(window_refresh_callback),
    io::di, io::uo) is det.
:- pred glfw.unset_window_refresh_callback(io::di, io::uo) is det.

:- type video_mode
    --->    video_mode(
                width      :: int,
                height     :: int,
                red_bits   :: int,
                green_bits :: int,
                blue_bits  :: int
            ).

:- type video_modes == list(video_mode).

    % glfw.get_video_modes(VideoModes, !IO):
    % VidoeModes is the list of detected video modes on this system.
    % This predicate will return at most 1024 video modes.
    %
:- pred glfw.get_video_modes(video_modes::out, io::di, io::uo) is det.

:- pred glfw.get_desktop_mode(video_mode::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Input Handling.
%

:- pred glfw.poll_events(io::di, io::uo) is det.

:- pred glfw.wait_events(io::di, io::uo) is det.

:- type key
    --->    key_A
    ;       key_B
    ;       key_C
    ;       key_D
    ;       key_E
    ;       key_F
    ;       key_G
    ;       key_H
    ;       key_I
    ;       key_J
    ;       key_K
    ;       key_L
    ;       key_M
    ;       key_N
    ;       key_O
    ;       key_P
    ;       key_Q
    ;       key_R
    ;       key_S
    ;       key_T
    ;       key_U
    ;       key_V
    ;       key_W
    ;       key_X
    ;       key_Y
    ;       key_Z
    ;       key_0
    ;       key_1
    ;       key_2
    ;       key_3
    ;       key_4
    ;       key_5
    ;       key_6
    ;       key_7
    ;       key_8
    ;       key_9
    ;       key_period
    ;       key_comma
    ;       key_forward_slash
    ;       key_apostrophe
    ;       key_semicolon
    ;       key_open_square_bracket
    ;       key_close_square_bracket
    ;       key_backslash
    ;       key_grave_accent
    ;       key_space
    ;       key_escape
    ;       key_minus
    ;       key_equals
    ;       key_f1
    ;       key_f2
    ;       key_f3
    ;       key_f4
    ;       key_f5
    ;       key_f6
    ;       key_f7
    ;       key_f8
    ;       key_f9
    ;       key_f10
    ;       key_f11
    ;       key_f12
    ;       key_f13
    ;       key_f14
    ;       key_f15
    ;       key_f16
    ;       key_f17
    ;       key_f18
    ;       key_f19
    ;       key_f20
    ;       key_f21
    ;       key_f22
    ;       key_f23
    ;       key_f24
    ;       key_f25
    ;       key_up
    ;       key_down
    ;       key_left
    ;       key_right
    ;       key_lshift
    ;       key_rshift
    ;       key_lctrl
    ;       key_rctrl
    ;       key_lalt
    ;       key_ralt
    ;       key_lsuper
    ;       key_rsuper
    ;       key_tab
    ;       key_enter
    ;       key_backspace
    ;       key_insert
    ;       key_del
    ;       key_pageup
    ;       key_pagedown
    ;       key_home
    ;       key_end
    ;       key_kp_0
    ;       key_kp_1
    ;       key_kp_2
    ;       key_kp_3
    ;       key_kp_4
    ;       key_kp_5
    ;       key_kp_6
    ;       key_kp_7
    ;       key_kp_8
    ;       key_kp_9
    ;       key_kp_divide
    ;       key_kp_multiply
    ;       key_kp_subtract
    ;       key_kp_add
    ;       key_kp_decimal
    ;       key_kp_equal
    ;       key_kp_enter
    ;       key_kp_num_lock
    ;       key_caps_lock
    ;       key_scroll_lock
    ;       key_pause
    ;       key_menu.

:- type key_or_button_state
    --->    press
    ;       release.

:- type key_state == key_or_button_state.

:- pred glfw.get_key(key::in, key_state::out, io::di, io::uo) is det.

:- type mouse_button
    --->    mouse_button_left       % Button 1.
    ;       mouse_button_right      % Button 2.
    ;       mouse_button_middle     % Button 3.
    ;       mouse_button_4
    ;       mouse_button_5
    ;       mouse_button_6
    ;       mouse_button_7
    ;       mouse_button_8.

:- type mouse_button_state == key_or_button_state.

:- pred glfw.get_mouse_button(mouse_button::in, mouse_button_state::out,
    io::di, io::uo) is det.

:- pred glfw.get_mouse_pos(int::out, int::out, io::di, io::uo) is det.

:- pred glfw.set_mouse_pos(int::in, int::in, io::di, io::uo) is det.

:- pred glfw.get_mouse_wheel(int::out, io::di, io::uo) is det.

:- pred glfw.set_mouse_wheel(int::in, io::di, io::uo) is det.

:- type key_callback == pred(key, key_state, io, io).
:- inst key_callback == (pred(in, in, di, uo) is det).

:- pred glfw.set_key_callback(key_callback::in(key_callback),
    io::di, io::uo) is det.
:- pred glfw.unset_key_callback(io::di, io::uo) is det.

:- type char_callback == pred(char, key_state, io, io).
:- inst char_callback == (pred(in, in, di, uo) is det).

:- pred glfw.set_char_callback(char_callback::in(char_callback),
    io::di, io::uo) is det.
:- pred glfw.unset_char_callback(io::di, io::uo) is det.

:- type mouse_button_callback == pred(mouse_button, mouse_button_state, io, io).
:- inst mouse_button_callback == (pred(in, in, di, uo) is det).

:- pred glfw.set_mouse_button_callback(
    mouse_button_callback::in(mouse_button_callback), io::di, io::uo) is det.
:- pred glfw.unset_mouse_button_callback(io::di, io::uo) is det.

:- type mouse_pos_callback == pred(int, int, io, io).
:- inst mouse_pos_callback == (pred(in, in, di, uo) is det).

:- pred glfw.set_mouse_pos_callback(mouse_pos_callback::in(mouse_pos_callback),
    io::di, io::uo) is det.
:- pred glfw.unset_mouse_pos_callback(io::di, io::uo) is det.

:- type mouse_wheel_callback == pred(int, io, io).
:- inst mouse_wheel_callback == (pred(in, di, uo) is det).

:- pred glfw.set_mouse_wheel_callback(
    mouse_wheel_callback::in(mouse_wheel_callback), io::di, io::uo) is det.
:- pred glfw.unset_mouse_wheel_callback(io::di, io::uo) is det.

:- type joystick_id
    --->    joystick_1
    ;       joystick_2
    ;       joystick_3
    ;       joystick_4
    ;       joystick_5
    ;       joystick_6
    ;       joystick_7
    ;       joystick_8
    ;       joystick_9
    ;       joystick_10
    ;       joystick_11
    ;       joystick_12
    ;       joystick_13
    ;       joystick_14
    ;       joystick_15
    ;       joystick_16.

:- type bool_joystick_param
    --->    present.

:- type int_joystick_param
    --->    axes
    ;       buttons.

:- type joystick_button_state == key_or_button_state.

:- pred glfw.get_bool_joystick_param(joystick_id::in, bool_joystick_param::in,
    bool::out, io::di, io::uo) is det.

:- pred glfw.get_int_joystick_param(joystick_id::in, int_joystick_param::in,
    int::out, io::di, io::uo) is det.

:- pred glfw.get_joystick_pos(joystick_id::in, int::in, list(float)::out,
    io::di, io::uo) is det.

:- pred glfw.get_joystick_buttons(joystick_id::in, int::in,
    list(joystick_button_state)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timing.
%

:- pred glfw.get_time(float::out, io::di, io::uo) is det.

:- pred glfw.set_time(float::in, io::di, io::uo) is det.

:- pred glfw.sleep(float::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Miscellaneous.
%

:- type feature
    --->    auto_poll_events
    ;       key_repeat
    ;       mouse_cursor
    ;       sticky_keys
    ;       sticky_mouse_buttons
    ;       system_keys.

:- pred glfw.enable(feature::in, io::di, io::uo) is det.

:- pred glfw.disable(feature::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module require.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

#include <GL/glfw.h>

extern int GLFWCALL
MGLFW_window_close_callback_func(void);

extern void GLFWCALL
MGLFW_window_size_callback_func(int, int);

extern void GLFWCALL
MGLFW_window_refresh_callback_func(void);

extern void GLFWCALL
MGLFW_key_callback_func(int, int);

extern void GLFWCALL
MGLFW_char_callback_func(int, int);

extern void GLFWCALL
MGLFW_mouse_button_callback_func(int, int);

extern void GLFWCALL
MGLFW_mouse_pos_callback_func(int, int);

extern void GLFWCALL
MGLFW_mouse_wheel_callback_func(int);

extern MR_Word MGLFW_window_close_callback;
extern MR_Word MGLFW_window_size_callback;
extern MR_Word MGLFW_window_refresh_callback;
extern MR_Word MGLFW_key_callback;
extern MR_Word MGLFW_char_callback;
extern MR_Word MGLFW_mouse_button_callback;
extern MR_Word MGLFW_mouse_pos_callback;
extern MR_Word MGLFW_mouse_wheel_callback;

").

:- pragma foreign_code("C", "

MR_Word MGLFW_window_close_callback;
MR_Word MGLFW_window_size_callback;
MR_Word MGLFW_window_refresh_callback;
MR_Word MGLFW_key_callback;
MR_Word MGLFW_char_callback;
MR_Word MGLFW_mouse_button_callback;
MR_Word MGLFW_mouse_pos_callback;
MR_Word MGLFW_mouse_wheel_callback;

int GLFWCALL
MGLFW_window_close_callback_func(void)
{
    MR_Bool result;

    MGLFW_do_window_close_callback(MGLFW_window_close_callback,
        &result);

    return (result == MR_YES) ? GL_TRUE : GL_FALSE;
}

void GLFWCALL
MGLFW_window_size_callback_func(int width, int height)
{
    MGLFW_do_window_size_callback(MGLFW_window_size_callback,
        width, height);
}

void GLFWCALL
MGLFW_window_refresh_callback_func(void)
{
    MGLFW_do_window_refresh_callback(MGLFW_window_refresh_callback);
}

void GLFWCALL
MGLFW_key_callback_func(int key, int action)
{
    MGLFW_do_key_callback(MGLFW_key_callback, key, action);
}

void GLFWCALL
MGLFW_char_callback_func(int character, int action)
{
    MGLFW_do_char_callback(MGLFW_char_callback, character, action);
}

void GLFWCALL
MGLFW_mouse_button_callback_func(int button, int action)
{
    MGLFW_do_mouse_button_callback(MGLFW_mouse_button_callback,
        button, action);
}

void GLFWCALL
MGLFW_mouse_pos_callback_func(int x, int y)
{
    MGLFW_do_mouse_pos_callback(MGLFW_mouse_pos_callback, x, y);
}

void GLFWCALL
MGLFW_mouse_wheel_callback_func(int pos)
{
    MGLFW_do_mouse_wheel_callback(MGLFW_mouse_wheel_callback, pos);
}
").

%-----------------------------------------------------------------------------%

init(!IO) :-
    init_2(InitSucceeded, !IO),
    (
        InitSucceeded = yes
    ;
        InitSucceeded = no,
        error("glfw.init/2: initialisation failed")
    ).

:- pred init_2(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    init_2(Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Result = (glfwInit() == GL_TRUE) ? MR_YES : MR_NO;
").

:- pragma foreign_proc("C",
    terminate(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
   glfwTerminate();
").


:- pragma foreign_proc("C",
    get_version(Major::out, Minor::out, Rev::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int     major;
    int     minor;
    int     rev;
    glfwGetVersion(&major, &minor, &rev);

    Major = major;
    Minor = minor;
    Rev = rev;
").

%-----------------------------------------------------------------------------%
%
% Window Handling.
%

:- pragma foreign_enum("C", window_mode/0, [
    window     - "GLFW_WINDOW",
    fullscreen - "GLFW_FULLSCREEN"
]).

open_window(Width, Height, RedBits, GreenBits, BlueBits, AlphaBits,
        DepthBits, StencilBits, WindowMode, !IO) :-
    open_window_2(Width, Height, RedBits, GreenBits, BlueBits, AlphaBits,
        DepthBits, StencilBits, WindowMode, Succeeded, !IO),
    (
        Succeeded = yes
    ;
        Succeeded = no,
        error("glfw.open_window/11: could not open window")
    ).

:- pred open_window_2(int::in, int::in, int::in, int::in, int::in,
    int::in, int::in, int::in, window_mode::in, bool::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    open_window_2(Height::in, Width::in, RedBits::in, GreenBits::in,
        BlueBits::in, AlphaBits::in, DepthBits::in, StencilBits::in,
        WindowMode::in, Succeeded::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int result;

    result = glfwOpenWindow((int)Width, (int)Height,
        (int)RedBits, (int)GreenBits, (int)BlueBits,
        (int)AlphaBits, (int)DepthBits, (int)StencilBits,
        WindowMode);

    Succeeded = (result == GL_TRUE) ? MR_YES : MR_NO;
").

:- type hint_target
    --->    hint_target_refresh_rate
    ;       hint_target_accum_red_bits
    ;       hint_target_accum_green_bits
    ;       hint_target_accum_blue_bits
    ;       hint_target_accum_alpha_bits
    ;       hint_target_aux_buffers
    ;       hint_target_stereo
    ;       hint_target_window_no_resize
    ;       hint_target_fsaa_samples
    ;       hint_target_opengl_version_major
    ;       hint_target_opengl_version_minor
    ;       hint_target_opengl_forward_compat
    ;       hint_target_opengl_debug_context
    ;       hint_target_opengl_profile.

:- pragma foreign_enum("C",  hint_target/0, [
    hint_target_refresh_rate     - "GLFW_REFRESH_RATE",
    hint_target_accum_red_bits   - "GLFW_ACCUM_RED_BITS",
    hint_target_accum_green_bits - "GLFW_ACCUM_GREEN_BITS",
    hint_target_accum_blue_bits  - "GLFW_ACCUM_BLUE_BITS",
    hint_target_accum_alpha_bits - "GLFW_ACCUM_ALPHA_BITS",
    hint_target_aux_buffers      - "GLFW_AUX_BUFFERS",
    hint_target_stereo           - "GLFW_STEREO",
    hint_target_window_no_resize - "GLFW_WINDOW_NO_RESIZE",
    hint_target_fsaa_samples     - "GLFW_FSAA_SAMPLES",
    hint_target_opengl_version_major  - "GLFW_OPENGL_VERSION_MAJOR",
    hint_target_opengl_version_minor  - "GLFW_OPENGL_VERSION_MINOR",
    hint_target_opengl_forward_compat - "GLFW_OPENGL_FORWARD_COMPAT",
    hint_target_opengl_debug_context  - "GLFW_OPENGL_DEBUG_CONTEXT",
    hint_target_opengl_profile        - "GLFW_OPENGL_PROFILE"
]).

open_window_hint(Hint, !IO) :-
    (
        Hint = refresh_rate(Int),
        Target = hint_target_refresh_rate
    ;
        Hint = accum_red_bits(Int),
        Target = hint_target_accum_red_bits
    ;
        Hint = accum_green_bits(Int),
        Target = hint_target_accum_green_bits
    ;
        Hint = accum_blue_bits(Int),
        Target = hint_target_accum_blue_bits
    ;
        Hint = accum_alpha_bits(Int),
        Target = hint_target_accum_alpha_bits
    ;
        Hint = aux_buffers(Int),
        Target = hint_target_aux_buffers
    ;
        Hint = stereo(Bool),
        Int = bool_to_gl_bool(Bool),
        Target = hint_target_stereo
    ;
        Hint = window_no_resize(Bool),
        Int = bool_to_gl_bool(Bool),
        Target = hint_target_window_no_resize
    ;
        Hint = fsaa_samples(Int),
        Target = hint_target_fsaa_samples
    ;
        Hint = opengl_version_major(Int),
        Target = hint_target_opengl_version_major
    ;
        Hint = opengl_version_minor(Int),
        Target = hint_target_opengl_version_minor
    ;
        Hint = opengl_forward_compat(Bool),
        Int = bool_to_gl_bool(Bool),
        Target = hint_target_opengl_forward_compat
    ;
        Hint = opengl_debug_context(Bool),
        Int = bool_to_gl_bool(Bool),
        Target = hint_target_opengl_debug_context
    ;
        Hint = opengl_profile(Int),
        Target = hint_target_opengl_profile
    ),
    open_window_hint_2(Target, Int, !IO).

:- func bool_to_gl_bool(bool) = int.

:- pragma foreign_proc("C",
    bool_to_gl_bool(B::in) = (GB::out),
    [promise_pure, will_not_call_mercury],
"
    GB = (B == MR_YES) ? GL_TRUE : GL_FALSE;
").

:- pred open_window_hint_2(hint_target::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    open_window_hint_2(Target::in, Value::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwOpenWindowHint(Target, (int)Value);
").

:- pragma foreign_proc("C",
    close_window(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwCloseWindow();
").

:- pragma foreign_proc("C",
    set_window_close_callback(Pred::in(window_close_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_window_close_callback = Pred;
    glfwSetWindowCloseCallback(MGLFW_window_close_callback_func);
").

:- pragma foreign_proc("C",
    unset_window_close_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowCloseCallback(NULL);
    MGLFW_window_close_callback = 0;
").

:- pred do_window_close_callback(
    window_close_callback::in(window_close_callback), bool::out,
    io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_window_close_callback(in(window_close_callback), out, di, uo),
    "MGLFW_do_window_close_callback").

do_window_close_callback(Pred, Result, !IO) :-
    Pred(Result, !IO).

:- pragma foreign_proc("C",
    set_window_title(Title::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowTitle(Title);
").

:- pragma foreign_proc("C",
    set_window_size(Width::in, Height::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowSize((int)Width, (int)Height);
").

:- pragma foreign_proc("C",
    set_window_pos(X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowPos((int)X, (int)Y);
").

:- pragma foreign_proc("C",
    get_window_size(Width::out, Height::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int width;
    int height;

    glfwGetWindowSize(&width, &height);
    Width = width;
    Height = height;
").

:- pragma foreign_proc("C",
    set_window_size_callback(Pred::in(window_size_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_window_size_callback = Pred;
    glfwSetWindowSizeCallback(MGLFW_window_size_callback_func);
").

:- pragma foreign_proc("C",
    unset_window_size_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowSizeCallback(NULL);
    MGLFW_window_size_callback = 0;
").

:- pred do_window_size_callback(
    window_size_callback::in(window_size_callback), int::in, int::in,
    io::di, io::uo) is det.

:- pragma foreign_export("C",
    do_window_size_callback(in(window_size_callback), in, in, di, uo),
    "MGLFW_do_window_size_callback").

do_window_size_callback(Pred, Width, Height, !IO) :-
    Pred(Width, Height, !IO).

:- pragma foreign_proc("C",
    iconify_window(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwIconifyWindow();
").

:- pragma foreign_proc("C",
    restore_window(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwRestoreWindow();
").

:- pragma foreign_enum("C", bool_window_param/0, [
    opened                - "GLFW_OPENED",
    active                - "GLFW_ACTIVE",
    iconified             - "GLFW_ICONIFIED",
    accelerated           - "GLFW_ACCELERATED",
    stereo                - "GLFW_STEREO",
    window_no_resize      - "GLFW_WINDOW_NO_RESIZE",
    opengl_forward_compat - "GLFW_OPENGL_FORWARD_COMPAT",
    opengl_debug_context  - "GLFW_OPENGL_DEBUG_CONTEXT"
]).

:- pragma foreign_proc("C",
    get_bool_window_param(Param::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Result = (glfwGetWindowParam((int)Param)) ? MR_YES  : MR_NO;
").

:- pragma foreign_enum("C", int_window_param/0, [
    red_bits         - "GLFW_RED_BITS",
    green_bits       - "GLFW_GREEN_BITS",
    blue_bits        - "GLFW_BLUE_BITS",
    alpha_bits       - "GLFW_ALPHA_BITS",
    depth_bits       - "GLFW_DEPTH_BITS",
    stencil_bits     - "GLFW_STENCIL_BITS",
    refresh_rate     - "GLFW_REFRESH_RATE",
    accum_red_bits   - "GLFW_ACCUM_RED_BITS",
    accum_green_bits - "GLFW_ACCUM_GREEN_BITS",
    accum_blue_bits  - "GLFW_ACCUM_BLUE_BITS",
    accum_alpha_bits - "GLFW_ACCUM_ALPHA_BITS",
    aux_buffers      - "GLFW_AUX_BUFFERS",
    opengl_version_major - "GLFW_OPENGL_VERSION_MAJOR",
    opengl_version_minor - "GLFW_OPENGL_VERSION_MINOR",
    opengl_profile   - "GLFW_OPENGL_PROFILE"
]).

:- pragma foreign_proc("C",
    get_int_window_param(Param::in, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Result = glfwGetWindowParam((int)Param);
").

:- pragma foreign_proc("C",
    swap_buffers(_IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    glfwSwapBuffers();
").

:- pragma foreign_proc("C",
    swap_interval(Interval::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSwapInterval((int)Interval);
").

:- pragma foreign_proc("C",
    set_window_refresh_callback(Pred::in(window_refresh_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_window_refresh_callback = Pred;
    glfwSetWindowRefreshCallback(MGLFW_window_refresh_callback_func);
").

:- pragma foreign_proc("C",
    unset_window_refresh_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetWindowCloseCallback(NULL);
    MGLFW_window_refresh_callback = 0;
").

:- pred do_window_refresh_callback(
    window_refresh_callback::in(window_refresh_callback),
    io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_window_refresh_callback(in(window_refresh_callback), di, uo),
    "MGLFW_do_window_refresh_callback").

do_window_refresh_callback(Pred, !IO) :-
    Pred(!IO).

:- func make_video_mode(int, int, int, int, int) = video_mode.
:- pragma foreign_export("C", make_video_mode(in, in, in, in, in) = out,
    "MGLFW_make_video_mode").
make_video_mode(W, H, R, G, B) = video_mode(W, H, R, G, B).

get_video_modes(VideoModes, !IO) :-
    get_video_modes_2(RevVideoModes, !IO),
    VideoModes = list.reverse(RevVideoModes).

:- pred get_video_modes_2(video_modes::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_video_modes_2(VideoModes::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    GLFWvidmode *list;
    MR_Word     mer_vid_mode;
    int         num_video_modes;
    int         i;

    list = GC_malloc(1024 * sizeof(GLFWvidmode));

    num_video_modes = glfwGetVideoModes(list, 1024);

    VideoModes = MR_list_empty();

    for (i = 0; i < num_video_modes; i++) {
        mer_vid_mode = MGLFW_make_video_mode(
            list[i].Width,
            list[i].Height,
            list[i].RedBits,
            list[i].GreenBits,
            list[i].BlueBits
        );
        VideoModes = MR_list_cons(mer_vid_mode, VideoModes);
    }
    GC_free(list);
").

:- pragma foreign_proc("C",
    get_desktop_mode(VideoMode::out, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    GLFWvidmode     vm;
    glfwGetDesktopMode(&vm);
    VideoMode = MGLFW_make_video_mode(
        vm.Width,
        vm.Height,
        vm.RedBits,
        vm.GreenBits,
        vm.BlueBits
    );
").

%-----------------------------------------------------------------------------%
%
% Input Handling.
%

:- pragma foreign_proc("C",
    poll_events(_IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    glfwPollEvents();
").

:- pragma foreign_proc("C",
    wait_events(_IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, tabled_for_io],
"
    glfwWaitEvents();
").

:- pragma foreign_enum("C", key/0, [
   key_A - "'A'",
   key_B - "'B'",
   key_C - "'C'",
   key_D - "'D'",
   key_E - "'E'",
   key_F - "'F'",
   key_G - "'G'",
   key_H - "'H'",
   key_I - "'I'",
   key_J - "'J'",
   key_K - "'K'",
   key_L - "'L'",
   key_M - "'M'",
   key_N - "'N'",
   key_O - "'O'",
   key_P - "'P'",
   key_Q - "'Q'",
   key_R - "'R'",
   key_S - "'S'",
   key_T - "'T'",
   key_U - "'U'",
   key_V - "'V'",
   key_W - "'W'",
   key_X - "'X'",
   key_Y - "'Y'",
   key_Z - "'Z'",
   key_0 - "'0'",
   key_1 - "'1'",
   key_2 - "'2'",
   key_3 - "'3'",
   key_4 - "'4'",
   key_5 - "'5'",
   key_6 - "'6'",
   key_7 - "'7'",
   key_8 - "'8'",
   key_9 - "'9'",
   key_period - "'.'",
   key_comma - "','",
   key_forward_slash - "'/'",
   key_apostrophe - "'\\''",
   key_semicolon  - "';'",
   key_open_square_bracket  - "'['",
   key_close_square_bracket - "']'",
   key_backslash    - "'\\\\'",
   key_grave_accent - "'`'",
   key_minus  - "'-'",
   key_equals - "'='",
   key_space  - "GLFW_KEY_SPACE",
   key_escape - "GLFW_KEY_ESC",
   key_f1     - "GLFW_KEY_F1",
   key_f2     - "GLFW_KEY_F2",
   key_f3     - "GLFW_KEY_F3",
   key_f4     - "GLFW_KEY_F4",
   key_f5     - "GLFW_KEY_F5",
   key_f6     - "GLFW_KEY_F6",
   key_f7     - "GLFW_KEY_F7",
   key_f8     - "GLFW_KEY_F8",
   key_f9     - "GLFW_KEY_F9",
   key_f10    - "GLFW_KEY_F10",
   key_f11    - "GLFW_KEY_F11",
   key_f12    - "GLFW_KEY_F12",
   key_f13    - "GLFW_KEY_F13",
   key_f14    - "GLFW_KEY_F14",
   key_f15    - "GLFW_KEY_F15",
   key_f16    - "GLFW_KEY_F16",
   key_f17    - "GLFW_KEY_F17",
   key_f18    - "GLFW_KEY_F18",
   key_f19    - "GLFW_KEY_F19",
   key_f20    - "GLFW_KEY_F20",
   key_f21    - "GLFW_KEY_F21",
   key_f22    - "GLFW_KEY_F22",
   key_f23    - "GLFW_KEY_F23",
   key_f24    - "GLFW_KEY_F24",
   key_f25    - "GLFW_KEY_F25",
   key_up     - "GLFW_KEY_UP",
   key_down   - "GLFW_KEY_DOWN",
   key_left   - "GLFW_KEY_LEFT",
   key_right  - "GLFW_KEY_RIGHT",
   key_lshift - "GLFW_KEY_LSHIFT",
   key_rshift - "GLFW_KEY_RSHIFT",
   key_lctrl  - "GLFW_KEY_LCTRL",
   key_rctrl  - "GLFW_KEY_RCTRL",
   key_lalt   - "GLFW_KEY_LALT",
   key_ralt   - "GLFW_KEY_RALT",
   key_lsuper - "GLFW_KEY_LSUPER",
   key_rsuper - "GLFW_KEY_RSUPER",
   key_tab    - "GLFW_KEY_TAB",
   key_enter  - "GLFW_KEY_ENTER",
   key_backspace - "GLFW_KEY_BACKSPACE",
   key_insert    - "GLFW_KEY_INSERT",
   key_del       - "GLFW_KEY_DEL",
   key_pageup    - "GLFW_KEY_PAGEUP",
   key_pagedown  - "GLFW_KEY_PAGEDOWN",
   key_home      - "GLFW_KEY_HOME",
   key_end       - "GLFW_KEY_END",
   key_kp_0 - "GLFW_KEY_KP_0",
   key_kp_1 - "GLFW_KEY_KP_1",
   key_kp_2 - "GLFW_KEY_KP_2",
   key_kp_3 - "GLFW_KEY_KP_3",
   key_kp_4 - "GLFW_KEY_KP_4",
   key_kp_5 - "GLFW_KEY_KP_5",
   key_kp_6 - "GLFW_KEY_KP_6",
   key_kp_7 - "GLFW_KEY_KP_7",
   key_kp_8 - "GLFW_KEY_KP_8",
   key_kp_9 - "GLFW_KEY_KP_9",
   key_kp_divide   - "GLFW_KEY_KP_DIVIDE",
   key_kp_multiply - "GLFW_KEY_KP_MULTIPLY",
   key_kp_subtract - "GLFW_KEY_KP_SUBTRACT",
   key_kp_add      - "GLFW_KEY_KP_ADD",
   key_kp_decimal  - "GLFW_KEY_KP_DECIMAL",
   key_kp_equal    - "GLFW_KEY_KP_EQUAL",
   key_kp_enter    - "GLFW_KEY_KP_ENTER",
   key_kp_num_lock - "GLFW_KEY_KP_NUM_LOCK",
   key_caps_lock   - "GLFW_KEY_CAPS_LOCK",
   key_scroll_lock - "GLFW_KEY_SCROLL_LOCK",
   key_pause       - "GLFW_KEY_PAUSE",
   key_menu        - "GLFW_KEY_MENU"
]).

:- pragma foreign_enum("C", key_or_button_state/0, [
    press   - "GLFW_PRESS",
    release - "GLFW_RELEASE"
]).

:- pragma foreign_proc("C",
    get_key(Key::in, State::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    State = glfwGetKey((int)Key);
").

:- pragma foreign_proc("C",
    set_key_callback(Pred::in(key_callback), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_key_callback = Pred;
    glfwSetKeyCallback(MGLFW_key_callback_func);
").

:- pragma foreign_proc("C",
    unset_key_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetKeyCallback(NULL);
    MGLFW_key_callback = 0;
").

:- pred do_key_callback(key_callback::in(key_callback), key::in,
    key_state::in, io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_key_callback(in(key_callback), in, in, di, uo),
    "MGLFW_do_key_callback").

do_key_callback(Pred, Key, KeyState, !IO) :-
    Pred(Key, KeyState, !IO).

:- pragma foreign_proc("C",
    set_char_callback(Pred::in(char_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_char_callback = Pred;
    glfwSetCharCallback(MGLFW_char_callback_func);
").

:- pragma foreign_proc("C",
    unset_char_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetCharCallback(NULL);
    MGLFW_char_callback = 0;
").

:- pred do_char_callback(char_callback::in(char_callback), char::in,
    key_state::in, io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_char_callback(in(char_callback), in, in, di, uo),
    "MGLFW_do_char_callback").

do_char_callback(Pred, Key, KeyState, !IO) :-
    Pred(Key, KeyState, !IO).

:- pragma foreign_enum("C", mouse_button/0, [
    mouse_button_left   - "GLFW_MOUSE_BUTTON_LEFT",
    mouse_button_right  - "GLFW_MOUSE_BUTTON_RIGHT",
    mouse_button_middle - "GLFW_MOUSE_BUTTON_MIDDLE",
    mouse_button_4      - "GLFW_MOUSE_BUTTON_4",
    mouse_button_5      - "GLFW_MOUSE_BUTTON_5",
    mouse_button_6      - "GLFW_MOUSE_BUTTON_6",
    mouse_button_7      - "GLFW_MOUSE_BUTTON_7",
    mouse_button_8      - "GLFW_MOUSE_BUTTON_8"
]).

:- pragma foreign_proc("C",
    get_mouse_button(Button::in, State::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    State = glfwGetMouseButton((int)Button);
").

:- pragma foreign_proc("C",
    get_mouse_pos(X::out, Y::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int x;
    int y;

    glfwGetMousePos(&x, &y);
    X = x;
    Y = y;
").

:- pragma foreign_proc("C",
    set_mouse_pos(X::in, Y::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetMousePos((int)X, (int)Y);
").

:- pragma foreign_proc("C",
    get_mouse_wheel(WheelPos::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    WheelPos = glfwGetMouseWheel();
").

:- pragma foreign_proc("C",
    set_mouse_wheel(WheelPos::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetMouseWheel((int)WheelPos);
").

:- pragma foreign_proc("C",
    set_mouse_button_callback(Pred::in(mouse_button_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_mouse_button_callback = Pred;
    glfwSetMouseButtonCallback(MGLFW_mouse_button_callback_func);
").

:- pragma foreign_proc("C",
    unset_mouse_button_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetMouseButtonCallback(NULL);
    MGLFW_mouse_button_callback = 0;
").

:- pred do_mouse_button_callback(
    mouse_button_callback::in(mouse_button_callback), mouse_button::in,
    mouse_button_state::in, io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_mouse_button_callback(in(mouse_button_callback), in, in, di, uo),
    "MGLFW_do_mouse_button_callback").

do_mouse_button_callback(Pred, Button, Action, !IO) :-
    Pred(Button, Action, !IO).

:- pragma foreign_proc("C",
    set_mouse_pos_callback(Pred::in(mouse_pos_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_mouse_pos_callback = Pred;
    glfwSetMousePosCallback(MGLFW_mouse_pos_callback_func);
").

:- pragma foreign_proc("C",
    unset_mouse_pos_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetMousePosCallback(NULL);
    MGLFW_mouse_pos_callback = 0;
").

:- pred do_mouse_pos_callback(mouse_pos_callback::in(mouse_pos_callback),
    int::in, int::in, io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_mouse_pos_callback(in(mouse_pos_callback), in, in, di, uo),
    "MGLFW_do_mouse_pos_callback").

do_mouse_pos_callback(Pred, X, Y, !IO) :-
    Pred(X, Y, !IO).

:- pragma foreign_proc("C",
    set_mouse_wheel_callback(Pred::in(mouse_wheel_callback),
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    MGLFW_mouse_wheel_callback = Pred;
    glfwSetMouseWheelCallback(MGLFW_mouse_wheel_callback_func);
").

:- pragma foreign_proc("C",
    unset_mouse_wheel_callback(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetMouseWheelCallback(NULL);
    MGLFW_mouse_wheel_callback = 0;
").

:- pred do_mouse_wheel_callback(mouse_wheel_callback::in(mouse_wheel_callback),
    int::in, io::di, io::uo) is det.
:- pragma foreign_export("C",
    do_mouse_wheel_callback(in(mouse_wheel_callback), in, di, uo),
    "MGLFW_do_mouse_wheel_callback").

do_mouse_wheel_callback(Pred, Pos, !IO) :-
    Pred(Pos, !IO).

:- pragma foreign_enum("C", joystick_id/0, [
    joystick_1  - "GLFW_JOYSTICK_1",
    joystick_2  - "GLFW_JOYSTICK_2",
    joystick_3  - "GLFW_JOYSTICK_3",
    joystick_4  - "GLFW_JOYSTICK_4",
    joystick_5  - "GLFW_JOYSTICK_5",
    joystick_6  - "GLFW_JOYSTICK_6",
    joystick_7  - "GLFW_JOYSTICK_7",
    joystick_8  - "GLFW_JOYSTICK_8",
    joystick_9  - "GLFW_JOYSTICK_9",
    joystick_10 - "GLFW_JOYSTICK_10",
    joystick_11 - "GLFW_JOYSTICK_11",
    joystick_12 - "GLFW_JOYSTICK_12",
    joystick_13 - "GLFW_JOYSTICK_13",
    joystick_14 - "GLFW_JOYSTICK_14",
    joystick_15 - "GLFW_JOYSTICK_15",
    joystick_16 - "GLFW_JOYSTICK_16"
]).

:- pragma foreign_enum("C", bool_joystick_param/0, [
    present - "GLFW_PRESENT"
]).

:- pragma foreign_enum("C", int_joystick_param/0, [
    axes    - "GLFW_AXES",
    buttons - "GLFW_BUTTONS"
]).

:- pragma foreign_proc("C",
    get_bool_joystick_param(Id::in, Param::in, Result::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
   if (glfwGetJoystickParam((int)Id, (int)Param) == GL_TRUE) {
        Result = MR_YES;
    } else {
        Result = MR_NO;
    }
").

:- pragma foreign_proc("C",
    get_int_joystick_param(Id::in, Param::in, Result::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Result = glfwGetJoystickParam((int)Id, (int)Param);
").

:- pragma foreign_proc("C",
    get_joystick_pos(Id::in, NumAxes::in, AxesPos::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int     axes_read;
    float   *pos_array;
    int     i;

    AxesPos = MR_list_empty();
    if (NumAxes > 0 ) {
        pos_array = GC_malloc(NumAxes * sizeof(float));
        axes_read = glfwGetJoystickPos((int)Id, pos_array, (int)NumAxes);
        for (i = axes_read - 1; i >= 0; i--) {
            AxesPos = MR_list_cons(MR_float_to_word(pos_array[i]), AxesPos);
        }
        GC_free(pos_array);
    }
").

:- pragma foreign_proc("C",
    get_joystick_buttons(Id::in, NumButtons::in, ButtonState::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int             buttons_read;
    unsigned char   *buttons_array;
    int             i;

    ButtonState = MR_list_empty();
    if (NumButtons > 0) {
        buttons_array = GC_malloc(NumButtons * sizeof(unsigned char));
        buttons_read = glfwGetJoystickButtons((int)Id, buttons_array,
            (int)NumButtons);
        for (i = buttons_read - 1; i >=0; i--) {
            ButtonState = MR_list_cons(buttons_array[i], ButtonState);
        }
        GC_free(buttons_array);
    }
").

%-----------------------------------------------------------------------------%
%
% Timing.
%

:- pragma foreign_proc("C",
    get_time(Time::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Time = (MR_Float) glfwGetTime();
").

:- pragma foreign_proc("C",
    set_time(Time::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSetTime((double)Time);
").

:- pragma foreign_proc("C",
    sleep(Time::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwSleep((double)Time);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", feature/0, [
    auto_poll_events      - "GLFW_AUTO_POLL_EVENTS",
    key_repeat            - "GLFW_KEY_REPEAT",
    mouse_cursor          - "GLFW_MOUSE_CURSOR",
    sticky_keys           - "GLFW_STICKY_KEYS",
    sticky_mouse_buttons  - "GLFW_STICKY_MOUSE_BUTTONS",
    system_keys           - "GLFW_SYSTEM_KEYS"
]).

:- pragma foreign_proc("C",
    enable(Feature::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwEnable(Feature);
").

:- pragma foreign_proc("C",
    disable(Feature::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    glfwDisable(Feature);
").

%-----------------------------------------------------------------------------%
:- end_module glfw.
%-----------------------------------------------------------------------------%
