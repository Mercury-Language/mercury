%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This regression test for Mantis bug #486 is cut down from code that was
% originally in extras/graphics/mercury_glut/glut.window.m.
%
%---------------------------------------------------------------------------%

:- module bug486.
:- interface.

:- include_module bug486.bug486_helper_1.
:- import_module bug486.bug486_helper_1.

:- func special_window_state = bug486.bug486_helper_1.state.

:- implementation.

special_window_state = window_height.
