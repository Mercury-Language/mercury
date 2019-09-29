%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This regression test for Mantis bug #486 is cut down from code that was
% originally in extras/graphics/mercury_glut/glut.window.m.
%
%-----------------------------------------------------------------------------%

:- module bug486.window.
:- interface.

:- type window.state
    --->    x
    ;       y
    ;       window_width
    ;       window_height.

:- implementation.

:- pragma foreign_decl("C",
"
#define GLUT_WINDOW_X           41
#define GLUT_WINDOW_Y           42
#define GLUT_WINDOW_WIDTH       43
#define GLUT_WINDOW_HEIGHT      44
").

:- pragma foreign_enum("C", window.state/0,
[
    x                   - "GLUT_WINDOW_X",
    y                   - "GLUT_WINDOW_Y",
    window_width        - "GLUT_WINDOW_WIDTH",
    window_height       - "GLUT_WINDOW_HEIGHT"
]).
