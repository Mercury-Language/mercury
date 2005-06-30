%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: glut.model.m
% author: juliensf
%
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
	glut.model.wire_cube(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWireCube((GLdouble) Size);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_cube(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSolidCube((GLdouble) Size);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_sphere(Radius::in, Slices::in, Stacks::in, IO0::di,
		IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWireSphere((GLdouble) Radius, (GLint) Slices, (GLint) Stacks);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_sphere(Radius::in, Slices::in, Stacks::in, IO0::di,
		IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSolidSphere((GLdouble) Radius, (GLint) Slices, (GLint) Stacks);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_torus(InRad::in, OutRad::in, Sides::in, Rings::in,
		IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutWireTorus((GLdouble) InRad, (GLdouble) OutRad, (GLint) Sides, 
		(GLint) Rings);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_torus(InRad::in, OutRad::in, Sides::in, Rings::in,
		IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutSolidTorus((GLdouble) InRad, (GLdouble) OutRad, (GLint) Sides, 
		(GLint) Rings);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_icosahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutWireIcosahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_icosahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutSolidIcosahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_octahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutWireOctahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_octahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutSolidOctahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_tetrahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutWireTetrahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_tetrahedron(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutSolidTetrahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_dodecahedron(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWireDodecahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_dodecahedron(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSolidDodecahedron();
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_cone(Base::in, Height::in, Slices::in, Stacks::in,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWireCone((GLdouble) Base, (GLdouble) Height, (GLint) Slices, 
		(GLint) Stacks);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_cone(Base::in, Height::in, Slices::in, Stacks::in,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSolidCone((GLdouble) Base, (GLdouble) Height, (GLint) Slices, 
		(GLint) Stacks);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.wire_teapot(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWireTeapot((GLdouble) Size);
	IO = IO0;
").

:- pragma foreign_proc("C",
	glut.model.solid_teapot(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSolidTeapot((GLdouble) Size);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module glut.model.
%-----------------------------------------------------------------------------%
