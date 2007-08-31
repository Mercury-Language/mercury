%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: glut.m.
% Author: juliensf.
%
% This is partial Mercury binding to the GL Utility Library (GLUT).
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
% Initialisation
%

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

    % Enter the GLUT event processing loop.  
    % You need to use glut.quit/2 to get out of this. 
    %
:- pred glut.main_loop(io::di, io::uo) is det.

    % Notify GLUT that you want quit the event processing loop 
    % and abort execution.
    %
:- pred glut.quit(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% State retrieval
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
").

:- initialise glut.init/2.

%-----------------------------------------------------------------------------%
    
:- pred glut.init(io::di, io::uo) is det.

:- pragma foreign_proc("C", 
    glut.init(IO0::di, IO::uo), 
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    int argc;

    argc = mercury_argc + 1;
    
    glutInit(&argc, (char **) (mercury_argv - 1));
    IO = IO0;
").

%-----------------------------------------------------------------------------%

glut.init_display_mode(Flags0, !IO) :-
    Flags = list.foldr((\/), list.map(display_mode_to_int, Flags0), 0x0),
    init_display_mode_2(Flags, !IO).

:- pred glut.init_display_mode_2(int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C", 
    glut.init_display_mode_2(Flags::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutInitDisplayMode((unsigned) Flags);
    IO = IO0;
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
    glut.init_display_string(CtrlStr::in, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitDisplayString((char *) CtrlStr);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.init_window_position(X::in, Y::in, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitWindowPosition(X, Y);
    IO = IO0;
").

:- pragma foreign_proc("C",
    glut.init_window_size(W::in, S::in, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutInitWindowSize(W, S);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.main_loop(IO0::di, IO::uo),
    [may_call_mercury, tabled_for_io, promise_pure],
"
    glutMainLoop();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", 
    glut.quit(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    exit(mercury_runtime_terminate());
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", glut.state/0,
[
    screen_width      - "GLUT_SCREEN_WIDTH",
    screen_height     - "GLUT_SCREEN_HEIGHT",
    screen_width_mm   - "GLUT_SCREEN_WIDTH_MM",
    screen_height_mm  - "GLUT_SCREEN_HEIGHT_MM",
    init_window_x     - "GLUT_INIT_WINDOW_X",
    init_window_y     - "GLUT_INIT_WINDOW_Y"
]).

:- pragma foreign_proc("C",
    glut.get(State::in, Value::out, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    Value = (MR_Integer) glutGet((GLenum) State);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
    
:- pragma foreign_enum("C", glut.device/0, 
[
    keyboard            - "GLUT_HAS_KEYBOARD",
    mouse               - "GLUT_HAS_MOUSE",
    spaceball           - "GLUT_HAS_SPACEBALL",
    dial_and_button_box - "GLUT_HAS_DIAL_AND_BUTTON_BOX",
    tablet              - "GLUT_HAS_TABLET",
    joystick            - "GLUT_HAS_JOYSTICK"
]).

:- pragma foreign_proc("C",
    glut.has_device(Device::in, Res::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    if(glutDeviceGet((GLenum) Device)) {
        Res = MR_YES; 
    } else {
        Res = MR_NO;
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.elapsed_time(Time::out, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    Time = (MR_Integer) glutGet(GLUT_ELAPSED_TIME);
    IO = IO0;   
").

:- pragma foreign_proc("C",
    glut.display_mode_possible(IsPossible::out, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    if(glutGet(GLUT_DISPLAY_MODE_POSSIBLE)) {
        IsPossible = MR_YES;
    } else {
        IsPossible = MR_NO;
    }
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module glut.
%-----------------------------------------------------------------------------%
