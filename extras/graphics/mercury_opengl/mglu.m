%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: mglu.m
% main autors: conway, ohutch.
%
% This file provides bindings to the GLU library.
%
%-----------------------------------------------------------------------------%

:- module mglu.

:- interface.

:- import_module float, int, io, bool.

%-------------------------
%
% Viewing transformations
%
%-------------------------

:- pred look_at(float, float, float, float, float, float, float, float, float,
		io__state, io__state).
:- mode look_at(in, in, in, in, in, in, in, in, in, di, uo) is det.

:- pred perspective(float, float, float, float, io__state, io__state).
:- mode perspective(in, in, in, in, di, uo) is det.

%-------------------------
%
% Quadric functions
%
%-------------------------

:- type quadric.

:- type quadric_normals
			--->	smooth 
			;	flat 
			;	none.

:- type quadric_draw_style
			--->	point
			;	line
			;	fill
			;	silhouette.

:- type quadric_orientation
			--->	outside
			;	inside.


:- pred new_quadric(quadric, io__state, io__state).
:- mode new_quadric(out, di, uo) is det.

:- pred delete_quadric(quadric, io__state, io__state).
:- mode delete_quadric(in, di, uo) is det.

:- pred quadric_draw_style(quadric, quadric_draw_style, 
		io__state, io__state).
:- mode quadric_draw_style(in, in, di, uo) is det.

:- pred quadric_orientation(quadric, quadric_orientation, 
		io__state, io__state).
:- mode quadric_orientation(in, in, di, uo) is det.

:- pred quadric_normals(quadric, quadric_normals, io__state, io__state).
:- mode quadric_normals(in, in, di, uo) is det.

:- pred quadric_texture(quadric, bool, io__state, io__state).
:- mode quadric_texture(in, in, di, uo) is det.

%%%:- pred quadric_callback(quadric, ???, ???).
%%%:- mode quadric_callback(in, in, in) is det.

:- pred cylinder(quadric, float, float, float, int, int, 
		io__state, io__state).
:- mode cylinder(in, in, in, in, in, in, di, uo) is det.

:- pred sphere(quadric, float, int, int, io__state, io__state).
:- mode sphere(in, in, in, in, di, uo) is det.

:- pred disk(quadric, float, float, int, int, io__state, io__state).
:- mode disk(in, in, in, in, in, di, uo) is det.

:- pred partial_disk(quadric, float, float, int, int, float, 
		float, io__state, io__state).
:- mode partial_disk(in, in, in, in, in, in, in, di, uo) is det.


%------------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

:- pragma c_header_code("
	#include <math.h>
	#include <GL/glu.h>
").

:- pragma c_code(look_at(Ex::in, Ey::in, Ez::in, Cx::in, Cy::in, Cz::in,
		Ux::in, Uy::in, Uz::in, IO0::di, IO::uo), "
	gluLookAt((GLdouble) Ex, (GLdouble) Ey, (GLdouble) Ez,
		(GLdouble) Cx, (GLdouble) Cy, (GLdouble) Cz,
		(GLdouble) Ux, (GLdouble) Uy, (GLdouble) Uz);
	IO = IO0;
").

:- pragma c_code(perspective(Fovy::in, Asp::in, N::in, F::in,
		IO0::di, IO::uo), "
	gluPerspective((GLdouble) Fovy, (GLdouble) Asp,
		(GLdouble) N, (GLdouble) F);
	IO = IO0;
").


%------------------------------------------------------------------------------%

:- pragma foreign_type("C", quadric, "GLUquadric *").

:- func quadric_normals_to_int(quadric_normals) = int.

quadric_normals_to_int(smooth) 	= 0.
quadric_normals_to_int(flat)	= 1.
quadric_normals_to_int(none) 	= 2.

:- pragma c_header_code("
	extern const GLenum quadric_normals_flags[];
").

:- pragma c_code("
const GLenum quadric_normals_flags[] = {
	GLU_SMOOTH,
	GLU_FLAT,
	GLU_NONE
};
").

:- func quadric_draw_style_to_int(quadric_draw_style) = int.

quadric_draw_style_to_int(point) 	= 0.
quadric_draw_style_to_int(line)		= 1.
quadric_draw_style_to_int(fill) 	= 2.
quadric_draw_style_to_int(silhouette)	= 3.

:- pragma c_header_code("
	extern const GLenum quadric_draw_style_flags[];
").

:- pragma c_code("
const GLenum quadric_draw_style_flags[] = {
	GLU_POINT,
	GLU_LINE,
	GLU_FILL,
	GLU_SILHOUETTE
};
").


:- func quadric_orientation_to_int(quadric_orientation) = int.

quadric_orientation_to_int(outside) 	= 0.
quadric_orientation_to_int(inside) 	= 1.

:- pragma c_header_code("
	extern const GLenum quadric_orientation_flags[];
").

:- pragma c_code("
const GLenum quadric_orientation_flags[] = {
	GLU_OUTSIDE,
	GLU_INSIDE
};
").

:- func bool_to_int(bool) = int.

bool_to_int(yes) = 1.
bool_to_int(no) = 0.

:- pragma c_code(new_quadric(Q::out, IO0::di, IO::uo), "
	Q = gluNewQuadric();
	IO = IO0;
").


:- pragma c_code(delete_quadric(Q::in, IO0::di, IO::uo), "
	gluDeleteQuadric(Q);
	IO = IO0;
").
	

quadric_draw_style(Q, S) -->
	quadric_draw_style2(Q, quadric_draw_style_to_int(S)).


:- pred quadric_draw_style2(quadric, int, io__state, io__state).
:- mode quadric_draw_style2(in, in, di, uo) is det.

:- pragma c_code(quadric_draw_style2(Q::in, S::in, IO0::di, IO::uo), "
	gluQuadricDrawStyle(Q, quadric_draw_style_flags[S]);
	IO = IO0;
").

quadric_orientation(Q, O) -->
	quadric_orientation2(Q, quadric_orientation_to_int(O)).

:- pred quadric_orientation2(quadric, int, io__state, io__state).
:- mode quadric_orientation2(in, in, di, uo) is det.

:- pragma c_code(quadric_orientation2(Q::in, O::in, IO0::di, IO::uo), "
	gluQuadricOrientation(Q, quadric_orientation_flags[O]);
	IO = IO0;
").
	

quadric_normals(Q, N) -->
	quadric_normals2(Q, quadric_normals_to_int(N)).
	
:- pred quadric_normals2(quadric, int, io__state, io__state).
:- mode quadric_normals2(in, in, di, uo) is det.

:- pragma c_code(quadric_normals2(Q::in, N::in, IO0::di, IO::uo), "
	gluQuadricNormals(Q, quadric_normals_flags[N]);
	IO = IO0;
").

quadric_texture(Q, B) -->
	quadric_texture2(Q, bool_to_int(B)).

:- pred quadric_texture2(quadric, int, io__state, io__state).
:- mode quadric_texture2(in, in, di, uo) is det.

:- pragma c_code(quadric_texture2(Q::in, B::in, IO0::di, IO::uo), "
	gluQuadricTexture(Q, B);
	IO = IO0;
").

:- pragma c_code(cylinder(Q::in, BR::in, TR::in, H::in, SL::in, ST::in, 
			IO0::di, IO::uo), "
	gluCylinder(Q, BR, TR, H, SL, ST);
	IO = IO0;
").

:- pragma c_code(sphere(Q::in, R::in, SL::in, ST::in, IO0::di, IO::uo), "
	gluSphere(Q, R, SL, ST);
	IO = IO0;
").

:- pragma c_code(disk(Q::in, IR::in, OR::in, S::in, L::in, IO0::di, IO::uo), "
	gluDisk(Q, IR, OR, S, L);
	IO = IO0;
").

:- pragma c_code(partial_disk(Q::in, IR::in, OR::in, S::in, L::in, STA::in,
			SWA::in, IO0::di, IO::uo), "
	gluPartialDisk(Q, IR, OR, S, L, STA, SWA);
	IO = IO0;
").
