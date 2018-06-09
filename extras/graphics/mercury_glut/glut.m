%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2012 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: glut.m.
% Author: juliensf.
%
% This is partial Mercury binding to the GL Utility Library (GLUT).
%
% If used with freeglut then some of the extensions available in freeglut will
% also be available.  Calling the freeglut extensions when freeglut is *not*
% available will result in a software_error1/ exception being thrown.
%
% You can use the predicate glut.have_freeglut/0 to test for the presence
% of freeglut.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module glut.
:- interface.

:- include_module callback.
:- include_module color_map.
:- include_module font.
:- include_module model.
:- include_module overlay.
:- include_module window.

:- import_module glut.window.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Initialisation.
%

    % Succeeds if this binding was compiled against freeglut.
    %
:- pred have_freeglut is semidet.

:- type display_mode
    --->    rgba
    ;       index
    ;       single
    ;       double
    ;       accum
    ;       alpha
    ;       depth
    ;       stencil
    ;       multisample
    ;       stereo
    ;       luminance.

    % Set the initial display mode.
    % (See the glutInit() man page for the way that this works)
    %
:- pred glut.init_display_mode(list(display_mode)::in, io::di, io::uo) is det.

    % Set the initial display mode via a string.
    % (See man glutInitDisplayString for details).
    %
:- pred glut.init_display_string(string::in, io::di, io::uo) is det.

    % glut.init_window_position(X, Y, !IO).
    % Set the initial window position.  `X' and `Y' are the window
    % location in pixels.
    %
:- pred glut.init_window_position(int::in, int::in, io::di, io::uo) is det.

    % glut.init_window_size(Width, Height, !IO).
    % Set the initial window size.  `Width' and `Height' are the window
    % dimensions in pixels.
    %
:- pred glut.init_window_size(int::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Event processing.
%

    % Enter the GLUT event processing loop.
    %
:- pred glut.main_loop(io::di, io::uo) is det.

    % Notify GLUT that you want quit the event processing loop.
    % This predicate will cause the Mercury runtime to terminate and then cause
    % the process to exit.
    %
:- pred glut.quit(io::di, io::uo) is det.


% Freeglut extensions.
% ====================

    % Perform a single iteration of the GLUT event processing loop.
    %
:- pred glut.main_loop_event(io::di, io::uo) is det.

    % Halt the GLUT event processing loop.
    %
:- pred glut.leave_main_loop(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% State retrieval.
%

    % Return the number of milliseconds since GLUT was initialised (or
    % since this predicate was last called).
    %
:- pred glut.elapsed_time(int::out, io::di, io::uo) is det.

    % Returns `yes' if the current display mode is supported;
    % `no' otherwise.
    %
:- pred glut.display_mode_possible(bool::out, io::di, io::uo) is det.

:- type glut.state
    --->    screen_width
            % Width of the screen in pixels.
            % Zero indicates the width is unknown or unavailable.

    ;       screen_height
            % Height of the screen in pixels.
            % Zero indicates the height is unknown or unavailable.

    ;       screen_width_mm
            % Width of the screen in millimetres.
            % Zero indicates the width is unknown or unavailable.

    ;       screen_height_mm
            % Height of the screen in millimetres.
            % Zero indicates the height is unknown or unavailable.

    ;       init_window_x
            % The X value of the initial window position.

    ;       init_window_y.
            % The Y value of the initial window position.

    % Retrieves the specified GLUT state.
    %
:- pred glut.get(glut.state::in, int::out, io::di, io::uo) is det.

:- type device
    --->    keyboard
    ;       mouse
    ;       spaceball
    ;       dial_and_button_box
    ;       tablet
    ;       joystick.

    % Returns `yes' if the we are running on a machine that has
    % the specified device; `no' otherwise.
    %
:- pred glut.has_device(device::in, bool::out, io::di, io::uo) is det.


% Freeglut extensions.
% ====================

    % Returns `yes' if we are currently in fullscreen mode and `no'
    % otherwise.
    %
:- pred glut.full_screen(bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module require.

:- pragma foreign_decl("C",
"
    #if defined(__APPLE__) && defined(__MACH__)
        #include <GLUT/glut.h>
    #else
        #include <GL/glut.h>
    #endif

    #if defined(FREEGLUT)
        #include <GL/freeglut_ext.h>
    #endif
").

:- initialise glut.init/2.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_freeglut,
    [will_not_call_mercury, promise_pure],
"
#if defined(FREEGLUT)
   SUCCESS_INDICATOR = MR_TRUE;
#else
   SUCCESS_INDICATOR = MR_FALSE;
#endif
").

%-----------------------------------------------------------------------------%

:- pred glut.init(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    glut.init(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    int argc;

    argc = mercury_argc + 1;

    glutInit(&argc, (char **) (mercury_argv - 1));
").

%-----------------------------------------------------------------------------%

glut.init_display_mode(Flags0, !IO) :-
    Flags = list.foldr((\/), list.map(display_mode_to_int, Flags0), 0x0),
    init_display_mode_2(Flags, !IO).

:- pred glut.init_display_mode_2(int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    glut.init_display_mode_2(Flags::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutInitDisplayMode((unsigned) Flags);
").

:- func display_mode_to_int(display_mode) = int.

display_mode_to_int(rgba)        = glut_rgba.
display_mode_to_int(index)       = glut_index.
display_mode_to_int(single)      = glut_single.
display_mode_to_int(double)      = glut_double.
display_mode_to_int(accum)       = glut_accum.
display_mode_to_int(alpha)       = glut_alpha.
display_mode_to_int(depth)       = glut_depth.
display_mode_to_int(stencil)     = glut_stencil.
display_mode_to_int(multisample) = glut_multisample.
display_mode_to_int(stereo)      = glut_stereo.
display_mode_to_int(luminance)   = glut_luminance.

:- func glut_rgba = int.
:- pragma foreign_proc("C",
    glut_rgba = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_RGBA;
").

:- func glut_index = int.
:- pragma foreign_proc("C",
    glut_index = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_INDEX;
").

:- func glut_single = int.
:- pragma foreign_proc("C",
    glut_single = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_SINGLE;
").

:- func glut_double = int.
:- pragma foreign_proc("C",
    glut_double = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_DOUBLE;
").

:- func glut_accum = int.
:- pragma foreign_proc("C", glut_accum = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_ACCUM;
").

:- func glut_alpha = int.
:- pragma foreign_proc("C", glut_alpha = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_ACCUM;
").

:- func glut_depth = int.
:- pragma foreign_proc("C", glut_depth = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_DEPTH;
").

:- func glut_stencil = int.
:- pragma foreign_proc("C", glut_stencil = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_STENCIL;
").

:- func glut_multisample = int.
:- pragma foreign_proc("C", glut_multisample = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_MULTISAMPLE;
").

:- func glut_stereo = int.
:- pragma foreign_proc("C", glut_stereo = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_STEREO;
").

:- func glut_luminance = int.
:- pragma foreign_proc("C", glut_luminance = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_LUMINANCE;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.init_display_string(CtrlStr::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitDisplayString((char *) CtrlStr);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.init_window_position(X::in, Y::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitWindowPosition(X, Y);
").

:- pragma foreign_proc("C",
    glut.init_window_size(W::in, S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitWindowSize(W, S);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.main_loop(_IO0::di, _IO::uo),
    [may_call_mercury, tabled_for_io, promise_pure],
"
    glutMainLoop();
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.quit(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    exit(mercury_runtime_terminate());
").

%-----------------------------------------------------------------------------%

glut.main_loop_event(!IO) :-
    ( if have_freeglut then
        main_loop_event_2(!IO)
    else
        error("glut.main_loop_event/2: freeglut is required")
    ).

:- pred main_loop_event_2(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    main_loop_event_2(_IO0::di, _IO::uo),
    [promise_pure, may_call_mercury],
"
#if defined(FREEGLUT)
    glutMainLoopEvent();
#endif
").

glut.leave_main_loop(!IO) :-
    ( if have_freeglut then
        leave_main_loop_2(!IO)
    else
        error("glut.leave_main_loop/2: freeglut is required")
    ).

:- pred leave_main_loop_2(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    leave_main_loop_2(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
#if defined(FREEGLUT)
    glutLeaveMainLoop();
#endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", glut.state/0, [
    screen_width      - "GLUT_SCREEN_WIDTH",
    screen_height     - "GLUT_SCREEN_HEIGHT",
    screen_width_mm   - "GLUT_SCREEN_WIDTH_MM",
    screen_height_mm  - "GLUT_SCREEN_HEIGHT_MM",
    init_window_x     - "GLUT_INIT_WINDOW_X",
    init_window_y     - "GLUT_INIT_WINDOW_Y"
]).

:- pragma foreign_proc("C",
    glut.get(State::in, Value::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    Value = (MR_Integer) glutGet((GLenum) State);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", glut.device/0, [
    keyboard            - "GLUT_HAS_KEYBOARD",
    mouse               - "GLUT_HAS_MOUSE",
    spaceball           - "GLUT_HAS_SPACEBALL",
    dial_and_button_box - "GLUT_HAS_DIAL_AND_BUTTON_BOX",
    tablet              - "GLUT_HAS_TABLET",
    joystick            - "GLUT_HAS_JOYSTICK"
]).

:- pragma foreign_proc("C",
    glut.has_device(Device::in, Res::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    if(glutDeviceGet((GLenum) Device)) {
        Res = MR_YES;
    } else {
        Res = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.elapsed_time(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    Time = (MR_Integer) glutGet(GLUT_ELAPSED_TIME);
").

:- pragma foreign_proc("C",
    glut.display_mode_possible(IsPossible::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    if(glutGet(GLUT_DISPLAY_MODE_POSSIBLE)) {
        IsPossible = MR_YES;
    } else {
        IsPossible = MR_NO;
    }
").

%-----------------------------------------------------------------------------%

glut.full_screen(FullScreen, !IO) :-
    ( if have_freeglut then
        full_screen_2(FullScreen, !IO)
    else
        error("glut.full_screen/3: freeglut required")
    ).

:- pred full_screen_2(bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    full_screen_2(FullScreen::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
#if defined(FREEGLUT)
   if(glutGet(GLUT_FULL_SCREEN)) {
        FullScreen = MR_YES;
    } else {
        FullScreen = MR_NO;
    }
#else
    FullScreen = MR_NO;
#endif

").

%-----------------------------------------------------------------------------%
:- end_module glut.
%-----------------------------------------------------------------------------%
