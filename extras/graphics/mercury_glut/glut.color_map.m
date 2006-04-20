%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: glut.color_map.m.
% Author: juliensf.
%
% This module contains routines for manipulating colormaps when using
% color index mode.  This is necessary since OpenGL does not include any
% means of doing this.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module glut.color_map.
:- interface.

%-----------------------------------------------------------------------------%

:- type component
    --->    red
    ;       blue
    ;       green.

    % color_map.set_color(Index, Red, Blue, Green, !IO).
    % Set the colormap entry for the given index.
    %
:- pred color_map.set_color(int::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

    % color_map.get_color(Index, Component, Value, !IO).
    % Retrieve the value for the given component for the given
    % color index colormap entry.
    %
:- pred color_map.get_color(int::in, component::in, float::out,
    io::di, io::uo) is det.

    % Copy the logical colormap for the layer in use from the specified
    % window to the current window.
    % XXX Not useful until we support multiple windows.
    %
:- pred color_map.copy(window::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
    #if defined(__APPLE__) && defined(__MACH__)
        #include <GLUT/glut.h>
    #else
        #include <GL/glut.h>

    #endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    color_map.set_color(I::in, R::in, G::in, B::in, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSetColor((int) I, (GLfloat) R, (GLfloat) G, (GLfloat) B);
    IO = IO0;
").

color_map.get_color(Index, Component, Value, !IO) :-
    get_color_2(Index, component_to_int(Component), Value, !IO).

:- pred get_color_2(int::in, int::in, float::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_color_2(I::in, C::in, V::out, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    V = (MR_Float) glutGetColor((int) I, (int) C);
    IO = IO0;
"). 

:- func component_to_int(component) = int.

component_to_int(red)   = glut_red.
component_to_int(green) = glut_green.
component_to_int(blue)  = glut_blue.

:- func glut_red = int.
:- pragma foreign_proc("C", glut_red = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_RED;
").

:- func glut_green = int.
:- pragma foreign_proc("C", glut_green = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_GREEN;
").

:- func glut_blue = int.
:- pragma foreign_proc("C", glut_blue = (Value::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = (MR_Integer) GLUT_BLUE;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    color_map.copy(WinId::in, IO0::di, IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutCopyColormap((int) WinId);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module glut.color_map.
%-----------------------------------------------------------------------------%
