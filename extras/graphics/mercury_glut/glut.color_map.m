%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2012 The University of Melbourne.
% Copyright (C) 2017-2018, 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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

:- import_module glut.window.

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
    color_map.set_color(I::in, R::in, G::in, B::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSetColor((int) I, (GLfloat) R, (GLfloat) G, (GLfloat) B);
").

:- pragma foreign_proc("C",
    get_color(I::in, C::in, V::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    V = (MR_Float) glutGetColor((int) I, (int) C);
").

:- pragma foreign_enum("C", component/0,
[
    red     - "GLUT_RED",
    green   - "GLUT_GREEN",
    blue    - "GLUT_BLUE"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    color_map.copy(WinId::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutCopyColormap((int) WinId);
").

%-----------------------------------------------------------------------------%
:- end_module glut.color_map.
%-----------------------------------------------------------------------------%
