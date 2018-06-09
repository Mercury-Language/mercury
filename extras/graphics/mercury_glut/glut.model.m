%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2012 The University of Melbourne.
% Copyright (C) 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: glut.model.m.
% Author: juliensf.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module glut.model.
:- interface.

%-----------------------------------------------------------------------------%

% Most of the predicates in this module come in two flavours.
% The model.wire_* ones draw a wireframe object.  The corresponding
% model.solid_* predicate draws a solid version of the same model.
% None of these predicates make use of display lists.

    % model.*_cube(Size, !IO).
    % `Size' is the length of each edge.
    %
:- pred model.wire_cube(float::in, io::di, io::uo) is det.
:- pred model.solid_cube(float::in, io::di, io::uo) is det.

    % model.*_sphere(Radius, Slices, Stacks, !IO).
    % `Slices' is the number of subdivisions around the z-axis.
    % `Stacks' is the number of subdivisions along the z-axis.
    %
:- pred model.wire_sphere(float::in, int::in, int::in, io::di, io::uo) is det.
:- pred model.solid_sphere(float::in, int::in, int::in, io::di, io::uo) is det.

    % model.*_torus(Inner, Outer, Sides, Rings, !IO).
    % `Inner' is the inner radius of the torus.
    % `Outer' is the outer radius of the torus.
    % `Sides' is the number of sides for each radial section.
    % `Rings' is the number of radial divisions for the torus.
    %
:- pred model.wire_torus(float::in, float::in, int::in, int::in,
    io::di, io::uo) is det.
:- pred model.solid_torus(float::in, float::in, int::in, int::in,
    io::di, io::uo) is det.

    % model.*_cone(Base, Height, Slices, Stacks, !IO).
    % `Base' is the radius of the base of the cone.
    % `Height' is the height of the cone.
    % `Slices' is the number of subdivisions around the z-axis.
    % `Stacks' is the number of subdivisions along the z-axis.
    %
:- pred model.wire_cone(float::in, float::in, int::in, int::in,
    io::di, io::uo) is det.
:- pred model.solid_cone(float::in, float::in, int::in, int::in,
    io::di, io::uo) is det.

    % model.*_teapot(Size).
    % `Size' is the relative size of the teapot.
    %
:- pred model.wire_teapot(float::in, io::di, io::uo) is det.
:- pred model.solid_teapot(float::in, io::di, io::uo) is det.

%
% The rest of these are fairly self-explanatory.
%

:- pred model.wire_icosahedron(io::di, io::uo) is det.
:- pred model.solid_icosahedron(io::di, io::uo) is det.

:- pred model.wire_octahedron(io::di, io::uo) is det.
:- pred model.solid_octahedron(io::di, io::uo) is det.

:- pred model.wire_tetrahedron(io::di, io::uo) is det.
:- pred model.solid_tetrahedron(io::di, io::uo) is det.

:- pred model.wire_dodecahedron(io::di, io::uo) is det.
:- pred model.solid_dodecahedron(io::di, io::uo) is det.


% Freeglut extensions.
% ====================

:- pred model.wire_rhombic_dodecahedron(io::di, io::uo) is det.
:- pred model.solid_rhombic_dodecahedron(io::di, io::uo) is det.

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

    #if defined(FREEGLUT)
        #include <GL/freeglut_ext.h>
    #endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    glut.model.wire_cube(Size::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireCube((GLdouble) Size);
").

:- pragma foreign_proc("C",
    glut.model.solid_cube(Size::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidCube((GLdouble) Size);
").

:- pragma foreign_proc("C",
    glut.model.wire_sphere(Radius::in, Slices::in, Stacks::in, _IO0::di,
        _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireSphere((GLdouble) Radius, (GLint) Slices, (GLint) Stacks);
").

:- pragma foreign_proc("C",
    glut.model.solid_sphere(Radius::in, Slices::in, Stacks::in, _IO0::di,
        _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidSphere((GLdouble) Radius, (GLint) Slices, (GLint) Stacks);
").

:- pragma foreign_proc("C",
    glut.model.wire_torus(InRad::in, OutRad::in, Sides::in, Rings::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireTorus((GLdouble) InRad, (GLdouble) OutRad, (GLint) Sides,
        (GLint) Rings);
").

:- pragma foreign_proc("C",
    glut.model.solid_torus(InRad::in, OutRad::in, Sides::in, Rings::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidTorus((GLdouble) InRad, (GLdouble) OutRad, (GLint) Sides,
        (GLint) Rings);
").

:- pragma foreign_proc("C",
    glut.model.wire_icosahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireIcosahedron();
").

:- pragma foreign_proc("C",
    glut.model.solid_icosahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidIcosahedron();
").

:- pragma foreign_proc("C",
    glut.model.wire_octahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireOctahedron();
").

:- pragma foreign_proc("C",
    glut.model.solid_octahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidOctahedron();
").

:- pragma foreign_proc("C",
    glut.model.wire_tetrahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireTetrahedron();
").

:- pragma foreign_proc("C",
    glut.model.solid_tetrahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidTetrahedron();
").

:- pragma foreign_proc("C",
    glut.model.wire_dodecahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireDodecahedron();
").

:- pragma foreign_proc("C",
    glut.model.solid_dodecahedron(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidDodecahedron();
").

:- pragma foreign_proc("C",
    glut.model.wire_cone(Base::in, Height::in, Slices::in, Stacks::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireCone((GLdouble) Base, (GLdouble) Height, (GLint) Slices,
        (GLint) Stacks);
").

:- pragma foreign_proc("C",
    glut.model.solid_cone(Base::in, Height::in, Slices::in, Stacks::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidCone((GLdouble) Base, (GLdouble) Height, (GLint) Slices,
        (GLint) Stacks);
").

:- pragma foreign_proc("C",
    glut.model.wire_teapot(Size::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutWireTeapot((GLdouble) Size);
").

:- pragma foreign_proc("C",
    glut.model.solid_teapot(Size::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutSolidTeapot((GLdouble) Size);
").

%-----------------------------------------------------------------------------%

model.wire_rhombic_dodecahedron(!IO) :-
    ( if have_freeglut then
        wire_rhombic_dodecahedron_2(!IO)
    else
        error("glut.model.wire_rhombic_dodecahedron/2: freeglut required")
    ).

:- pred wire_rhombic_dodecahedron_2(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    wire_rhombic_dodecahedron_2(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
#if defined(FREEGLUT)
    glutWireRhombicDodecahedron();
#endif
").

model.solid_rhombic_dodecahedron(!IO) :-
    ( if have_freeglut then
        solid_rhombic_dodecahedron_2(!IO)
    else
        error("glut.model.solid_rhombic_dodecahedron/2: freeglut required")
    ).

:- pred solid_rhombic_dodecahedron_2(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    solid_rhombic_dodecahedron_2(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
#if defined(FREEGLUT)
    glutSolidRhombicDodecahedron();
#endif
").

%-----------------------------------------------------------------------------%
:- end_module glut.model.
%-----------------------------------------------------------------------------%
