%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% file: mogl.m
% main author: conway.
%
% This file provides a binding to OpenGL 1.1. (It won't work with OpenGL 1.0.)
%
% It will work with OpenGL 1.2 - 1.4 but it doesn't (currently)
% implement any of the extended functionality in those versions.
%
%------------------------------------------------------------------------------%

:- module mogl.

:- interface.

:- import_module io, int, float, list, bool.

%------------------------------------------------------------------------------%
%
% GL Errors.
%

:- type mogl__error	
	--->	no_error
	;	invalid_enum
	;	invalid_value
	;	invalid_operation
	;	stack_overflow
	;	stack_underflow
	;	out_of_memory.

:- pred get_error(mogl__error, io, io).
:- mode get_error(out, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Begin/End objects.
%

:- type block_mode	
	--->	points
	;	line_strip
	;	line_loop
	;	lines
	;	polygon
	;	triangle_strip
	;	triangle_fan
	;	triangles
	;	quad_strip
	;	quads.

:- pred begin(block_mode, io, io).
:- mode begin(in, di, uo) is det.

:- pred end(io, io).
:- mode end(di, uo) is det.

:- pred edge_flag(bool, io, io).
:- mode edge_flag(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Vertex specification
%

:- pred vertex2(float, float, io, io).
:- mode vertex2(in, in, di, uo) is det.

:- pred vertex3(float, float, float, io, io).
:- mode vertex3(in, in, in, di, uo) is det.

:- pred vertex4(float, float, float, float, io, io).
:- mode vertex4(in, in, in, in, di, uo) is det.

:- pred tex_coord1(float, io, io).
:- mode tex_coord1(in, di, uo) is det.

:- pred tex_coord2(float, float, io, io).
:- mode tex_coord2(in, in, di, uo) is det.

:- pred tex_coord3(float, float, float, io, io).
:- mode tex_coord3(in, in, in, di, uo) is det.

:- pred tex_coord4(float, float, float, float, io, io).
:- mode tex_coord4(in, in, in, in, di, uo) is det.

:- pred normal3(float, float, float, io, io).
:- mode normal3(in, in, in, di, uo) is det.

:- pred color3(float, float, float, io, io).
:- mode color3(in, in, in, di, uo) is det.

:- pred color4(float, float, float, float, io, io).
:- mode color4(in, in, in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Coordinate transformations.
%

:- pred depth_range(float, float, io, io).
:- mode depth_range(in, in, di, uo) is det.

:- pred viewport(int, int, int, int, io, io).
:- mode viewport(in, in, in, in, di, uo) is det.

:- type matrix_mode
	--->	texture
	;	modelview
	;	projection.

:- type	matrix
	--->	m(float, float, float, float,	% a[11], a[12], ...
		  float, float, float, float,	% a[21], a[22], ...
		  float, float, float, float,	% a[31], a[32], ...
		  float, float, float, float).	% a[41], a[42], ...

:- pred matrix_mode(matrix_mode, io, io).
:- mode matrix_mode(in, di, uo) is det.

:- pred load_matrix(matrix, io, io).
:- mode load_matrix(in, di, uo) is det.

:- pred mult_matrix(matrix, io, io).
:- mode mult_matrix(in, di, uo) is det.

:- pred load_identity(io, io).
:- mode load_identity(di, uo) is det.

:- pred rotate(float, float, float, float, io, io).
:- mode rotate(in, in, in, in, di, uo) is det.

:- pred translate(float, float, float, io, io).
:- mode translate(in, in, in, di, uo) is det.

:- pred scale(float, float, float, io, io).
:- mode scale(in, in, in, di, uo) is det.

:- pred frustum(float, float, float, float, float, float, io, io).
:- mode frustum(in, in, in, in, in, in, di, uo) is det.

:- pred ortho(float, float, float, float, float, float, io, io).
:- mode ortho(in, in, in, in, in, in, di, uo) is det.

:- pred push_matrix(io, io).
:- mode push_matrix(di, uo) is det.

:- pred pop_matrix(io, io).
:- mode pop_matrix(di, uo) is det.

:- type texture_coord --->	s ; t ; r ; q.

:- type texture_parameter(T) --->
		texture_gen_mode(texture_gen_parameter)
	;	object_plane(T)
	;	eye_plane(T).

:- type texture_gen_parameter 
	---> 	object_linear
	;	eye_linear
	;	sphere_map.

:- pred tex_gen(texture_coord, texture_parameter(float), io, io).
:- mode tex_gen(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Clipping.
%

:- type	clip_plane --->
		clip(float, float, float, float).

:- pred clip_plane(int, clip_plane, io, io).
:- mode clip_plane(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Current raster position.
%

:- pred raster_pos2(float, float, io, io).
:- mode raster_pos2(in, in, di, uo) is det.

:- pred raster_pos3(float, float, float, io, io).
:- mode raster_pos3(in, in, in, di, uo) is det.

:- pred raster_pos4(float, float, float, float, io, io).
:- mode raster_pos4(in, in, in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Colors and coloring.
%

:- type face_direction	--->	cw ; ccw .

:- type face_side	--->	front ; back ; front_and_back .

:- type material
	--->	ambient(float, float, float, float)
	;	diffuse(float, float, float, float)
	;	ambient_and_diffuse(float, float, float, float)
	;	specular(float, float, float, float)
	;	emission(float, float, float, float)
	;	shininess(float)
	;	color_indexes(float, float, float).

:- type light_no == int.

:- type light
	--->	ambient(float, float, float, float)
	;	diffuse(float, float, float, float)
	;	specular(float, float, float, float)
	;	position(float, float, float, float)
	;	spot_direction(float, float, float)
	;	spot_exponent(float)
	;	spot_cutoff(float)
	;	constant_attenuation(float)
	;	linear_attenuation(float)
	;	quadratic_attenuation(float).

:- type lighting_model
		--->	light_model_ambient(float, float, float, float)
		;	light_model_local_viewer(bool)
		;	light_model_two_side(bool).

:- type color_material_mode
		--->	ambient
		;	diffuse
		;	ambient_and_diffuse
		;	specular
		;	emission.

:- type shade_model ---> smooth ; flat.

:- pred front_face(face_direction, io, io).
:- mode front_face(in, di, uo) is det.

:- pred material(face_side, material, io, io).
:- mode material(in, in, di, uo) is det.

:- pred light(light_no, light, io, io).
:- mode light(in, in, di, uo) is det.

:- pred light_model(lighting_model, io, io).
:- mode light_model(in, di, uo) is det.

:- pred color_material(face_side, color_material_mode, io, io).
:- mode color_material(in, in, di, uo) is det.

:- pred shade_model(shade_model, io, io).
:- mode shade_model(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Points.
%

:- pred point_size(float, io, io).
:- mode point_size(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Line segments.
%

:- pred line_width(float, io, io).
:- mode line_width(in, di, uo) is det.

:- pred line_stipple(int, int, io, io).
:- mode line_stipple(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Polygons.
%

:- type polygon_stipple == int.		% use bottom 32 bits of each int.

:- type polygon_mode
		--->	point
		;	line
		;	fill.

:- pred cull_face(face_side, io, io).
:- mode cull_face(in, di, uo) is det.

:- pred polygon_stipple(polygon_stipple, io, io).
:- mode polygon_stipple(in, di, uo) is det.

:- pred polygon_mode(face_side, polygon_mode, io, io).
:- mode polygon_mode(in, in, di, uo) is det.

:- pred polygon_offset(float, float, io, io).
:- mode polygon_offset(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Pixel Rectangles.
%

/*

:- type pixel_store_parameter
		--->	unpack_swap_bytes(bool)
		;	unpack_lsb_first(bool)
		;	unpack_row_length(int)
		;	unpack_skip_rows(int)
		;	unpack_skip_pixels(int)
		;	unpack_alignment(int)
		.

:- pred pixel_store(pixel_store_parameter, io__state, io__state).
:- mode pixel_store(in, di, uo) is det.

:- type pixel_transfer_parameter
		--->	map_color(bool)
		;	map_stencil(bool)
		;	index_shift(int)
		;	index_offset(int)
		;	red_scale(float)
		;	green_scale(float)
		;	blue_scale(float)
		;	alpha_scale(float)
		;	red_bias(float)
		;	green_bias(float)
		;	blue_bias(float)
		;	alpha_bias(float)
		;	depth_bias(float)
		.

:- pred pixel_transfer(pixel_transfer_parameter, io__state, io__state).
:- mode pixel_transfer(in, di, uo) is det.

% pixel_map not implemented

:- type draw_mode
		--->	color_index
		;	stencil_index
		;	depth_component
		;	red
		;	green
		;	blue
		;	alpha
		;	rgb
		;	rgba
		;	luminance
		;	luminance_alpha
		.

:- type draw_data
		--->	bitmap(list(int))	% 32 bits
%		;	ubyte(list(int))	% 4 x 8 bits -> 4 x ubyte
%		;	byte(list(int))		% 4 x 8 bits -> 4 x byte
%		;	ushort(list(int))	% 2 x 16 bits -> 2 x ushort
%		;	short(list(int))	% 2 x 16 bits -> 2 x short
%		;	int(list(int))		% 1 x 32 bits -> 1 x int
		;	float(list(float))
		.

:- pred draw_pixels(int, int, draw_mode, draw_data, io__state, io__state).
:- mode draw_pixels(in, in, in, in, di, uo) is det.

*/

%------------------------------------------------------------------------------%
%
% Bitmaps.
%

/*
:- pred bitmap(int, int, float, float, float, float, list(int),
		io__state, io__state).
:- mode bitmap(in, in, in, in, in, in, in, di, uo) is det.
*/

%------------------------------------------------------------------------------%
%
% Texturing.
%

/*

:- type texture_target
		--->	texture_2d
		;	proxy_texture_2d
		.

:- type texture_format
		--->	alpha
		;	luminance
		;	luminance_alpha
		;	intensity
		;	rgb
		;	rgba
		.

:- pred tex_image_2d(texture_target, int, int, int, int, int,
		texture_format, texture_data, io__state, io__state).
:- mode tex_image_2d(in, in, in, in, in, in, in, in, di, uo) is det.

*/

%------------------------------------------------------------------------------%
%
% Fog.
%

:- type fog_parameter
		--->	fog_mode(fog_mode)
		;	fog_density(float)
		;	fog_start(float)
		;	fog_end(float).

:- type fog_mode
		--->	linear
		;	exp
		;	exp2.

:- pred fog(fog_parameter, io, io).
:- mode fog(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Per-fragment operations.
%

/*
:- pred scissor(int, int, int, int, io__state, io__state).
:- mode scissor(in, in, in, in, di, uo) is det.

:- type test_func
		--->	never
		;	always
		;	less
		;	lequal
		;	equal
		;	gequal
		;	greater
		;	notequal
		.

:- pred alpha_func(test_func, float, io__state, io__state).
:- mode alpha_func(in, in, di, uo) is det.

:- pred stencil_func(test_func, float, int, io__state, io__state).
:- mode stencil_func(in, in, in, di, uo) is det.

:- type stencil_op
		--->	keep
		;	zero
		;	replace
		;	incr
		;	decr
		;	invert
		.

:- pred stencil_op(stencil_op, stencil_op, stencil_op, io__state, io__state).
:- mode stencil_op(in, in, in, di, uo) is det.

:- pred depth_func(test_func, io__state, io__state).
:- mode depth_func(in, di, uo) is det.

:- type	blend_src
		--->	zero
		;	one
		;	dst_color
		;	one_minus_dst_color
		;	src_alpha
		;	one_minus_src_alpha
		;	dst_alpha
		;	one_minus_dst_alpha
		;	src_alpha_saturate
		.

:- type blend_dst
		--->	zero
		;	one
		;	src_color
		;	one_minus_src_color
		;	src_alpha
		;	one_minus_src_alpha
		;	dst_alpha
		;	one_minus_dst_alpha
		.

:- pred blend_func(blend_src, blend_dst, io__state, io__state).
:- mode blend_func(in, in, di, uo) is det.

:- type logical_operation
		--->	clear
		;	(and)
		;	and_reverse
		;	copy
		;	and_inverted
		;	noop
		;	xor
		;	(or)
		;	nor
		;	equiv
		;	invert
		;	or_reverse
		;	copy_inverted
		;	or_inverted
		;	nand
		;	set
		.
*/

%------------------------------------------------------------------------------%
%
% Whole framebuffer operations.
%

:- type buffer
	--->	none
	;	front_left
	;	front_right
	;	back_left
	;	back_right
	;	front
	;	back
	;	left
	;	right
	;	front_and_back
	;	aux(int).

:- pred draw_buffer(buffer, io, io).
:- mode draw_buffer(in, di, uo) is det.

:- pred index_mask(int, io, io).
:- mode index_mask(in, di, uo) is det.

:- pred color_mask(bool, bool, bool, bool, io, io).
:- mode color_mask(in, in, in, in, di, uo) is det.

:- pred depth_mask(bool, io, io).
:- mode depth_mask(in, di, uo) is det.

:- pred stencil_mask(int, io, io).
:- mode stencil_mask(in, di, uo) is det.

:- type buffer_bit
	--->	color
	;	depth
	;	stencil
	;	accum.

:- pred clear(list(buffer_bit), io, io).
:- mode clear(in, di, uo) is det.

:- pred clear_color(float, float, float, float, io, io).
:- mode clear_color(in, in, in, in, di, uo) is det.

:- pred clear_index(float, io, io).
:- mode clear_index(in, di, uo) is det.

:- pred clear_depth(float, io, io).
:- mode clear_depth(in, di, uo) is det.

:- pred clear_stencil(int, io, io).
:- mode clear_stencil(in, di, uo) is det.

:- pred clear_accum(float, float, float, float, io, io).
:- mode clear_accum(in, in, in, in, di, uo) is det.

:- type accum_op
		--->	accum
		;	load
		;	return
		;	mult
		;	add.

:- pred accum(accum_op, float, io, io).
:- mode accum(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Evaluators.
%

% Evalutators not implemented

%------------------------------------------------------------------------------%
%
% Selection.
%

/*

:- pred init_names(io__state, io__state).
:- mode init_names(di, uo) is det.

:- pred pop_name(io__state, io__state).
:- mode pop_name(di, uo) is det.

:- pred push_name(int, io__state, io__state).
:- mode push_name(in, di, uo) is det.

:- pred load_name(int, io__state, io__state).
:- mode load_name(in, di, uo) is det.

:- type render_mode
		--->	render
		;	select
		;	feedback
		.

:- pred render_mode(render_mode, int, io__state, io__state).
:- mode render_mode(in, out, di, uo) is det.

*/

%------------------------------------------------------------------------------%
%
% Display lists.
%

:- type display_list_mode 
	--->	compile
	;	compile_and_execute.

:- pred new_list(int, display_list_mode, io, io).
:- mode new_list(in, in, di, uo) is det.

:- pred end_list(io, io).
:- mode end_list(di, uo) is det.

:- pred call_list(int, io, io).
:- mode call_list(in, di, uo) is det.

:- pred gen_lists(int, int, io, io).
:- mode gen_lists(in, out, di, uo) is det.

:- pred delete_lists(int, int, io, io).
:- mode delete_lists(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% Flush and Finish.
%

:- pred flush(io, io).
:- mode flush(di, uo) is det.

:- pred finish(io, io).
:- mode finish(di, uo) is det.

%------------------------------------------------------------------------------%
%
% Enable/Disable.
%

:- type	control_flag
		--->	normalize			
		;	clip_plane(int)		
		;	lighting			
		;	light(int)			
		;	color_material
		;	line_stipple		
		;	cull_face		
		;	polygon_stipple		
		;	polygon_offset_point
		;	polygon_offset_line	
		;	polygon_offset_fill	
		;	fog					
		;	scissor_test		
		;	alpha_test			
		;	stencil_test		
		;	depth_test			
		;	blend				
		;	dither				
		;	index_logic_op		
		;	color_logic_op.		

:- pred enable(control_flag, io, io).
:- mode enable(in, di, uo) is det.

:- pred disable(control_flag, io, io).
:- mode disable(in, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, int, float, require, std_util.

:- pragma foreign_decl("C", "
	#include <stdio.h>
	#include <math.h>
	#include <GL/gl.h>
").

%------------------------------------------------------------------------------%
%
% GL Errors.
%

:- func error_to_int(int::in) = (mogl__error::out) is semidet.

error_to_int(0) = no_error.
error_to_int(1) = invalid_enum.
error_to_int(2) = invalid_value.
error_to_int(3) = invalid_operation.
error_to_int(4) = stack_overflow.
error_to_int(5) = stack_underflow.
error_to_int(6) = out_of_memory.

get_error(Err, !IO) :-
	get_error2(ErrNo, !IO),
	( if	Err0 = error_to_int(ErrNo)
	  then	Err = Err0
	  else	error("GetError returned an unexpected value.")
	).

:- pred get_error2(int, io, io).
:- mode get_error2(out, di, uo) is det.

:- pragma foreign_proc("C", 
	get_error2(Err::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	static GLenum errcodes[] = {
		GL_NO_ERROR,
		GL_INVALID_ENUM,
		GL_INVALID_VALUE,
		GL_INVALID_OPERATION,
		GL_STACK_OVERFLOW,
		GL_STACK_UNDERFLOW,
		GL_OUT_OF_MEMORY
	};
	GLenum	err;
	Integer i;

	Err = 0;

	err = glGetError();

	for (i=0; i < 7; i++)
		if (errcodes[i] == err)
		{
			Err =  i;
			break;
		}
	
	IO = IO0;
}").

%------------------------------------------------------------------------------%
%
% Begin/End objects.
%

:- func block_mode_to_int(block_mode) = int.

block_mode_to_int(points)	= 0.
block_mode_to_int(line_strip)	= 1.
block_mode_to_int(line_loop)	= 2.
block_mode_to_int(lines)	= 3.
block_mode_to_int(polygon)	= 4.
block_mode_to_int(triangle_strip) = 5.
block_mode_to_int(triangle_fan) = 6.
block_mode_to_int(triangles) 	= 7.
block_mode_to_int(quad_strip)	= 8.
block_mode_to_int(quads)	= 9.

:- pragma foreign_decl("C", "
	extern const GLenum block_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum block_mode_flags[] = {
		GL_POINTS,
		GL_LINE_STRIP,
		GL_LINE_LOOP,
		GL_LINES,
		GL_POLYGON,
		GL_TRIANGLE_STRIP,
		GL_TRIANGLE_FAN,
		GL_TRIANGLES,
		GL_QUAD_STRIP,
		GL_QUADS
	};
").

begin(Blk, !IO) :-
	begin2(block_mode_to_int(Blk), !IO).

:- pred begin2(int, io, io).
:- mode begin2(in, di, uo) is det.

:- pragma foreign_proc("C", 
	begin2(Mode::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glBegin(block_mode_flags[Mode]);
	IO = IO0;
").

:- pragma foreign_proc("C", 
	end(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glEnd();
	IO = IO0;
").

edge_flag(no, !IO) :-
	edge_flag2(0, !IO).
edge_flag(yes, !IO) :- 
	edge_flag2(1, !IO).

:- pred edge_flag2(int, io, io).
:- mode edge_flag2(in, di, uo) is det.

:- pragma foreign_proc("C", 
	edge_flag2(F::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glEdgeFlag((GLboolean) F);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Vertex specification
%

:- pragma foreign_proc("C", 
	vertex2(X::in, Y::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glVertex2f((GLfloat) X, (GLfloat) Y);
	}
	else
	{
		glVertex2d((GLdouble) X, (GLdouble) Y);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	vertex3(X::in, Y::in, Z::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glVertex3f((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	}
	else
	{
		glVertex3d((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	vertex4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glVertex4f((GLfloat) X, (GLfloat) Y, (GLfloat) Z, (GLfloat) W);
	}
	else
	{
		glVertex4d((GLdouble) X, (GLdouble) Y, (GLdouble) Z,
			(GLdouble) W);
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C", 
	tex_coord1(X::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTexCoord1f((GLfloat) X);
	}
	else
	{
		glTexCoord1d((GLdouble) X);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	tex_coord2(X::in, Y::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTexCoord2f((GLfloat) X, (GLfloat) Y);
	}
	else
	{
		glTexCoord2d((GLdouble) X, (GLdouble) Y);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	tex_coord3(X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTexCoord3f((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	}
	else
	{
		glTexCoord3d((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	tex_coord4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTexCoord4f((GLfloat) X, (GLfloat) Y, (GLfloat) Z,
			(GLfloat) W);
	}
	else
	{
		glTexCoord4d((GLdouble) X, (GLdouble) Y, (GLdouble) Z,
			(GLdouble) W);
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C", 
	normal3(X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glNormal3f((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	}
	else
	{
		glNormal3d((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C", 
	color3(R::in, G::in, B::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glColor3f((GLfloat) R, (GLfloat) G, (GLfloat) B);
	}
	else
	{
		glColor3d((GLdouble) R, (GLdouble) G, (GLdouble) B);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	color4(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		glColor4f((GLfloat) R, (GLfloat) G, (GLfloat) B, (GLfloat) A);
	}
	else
	{
		glColor4d((GLdouble) R, (GLdouble) G, (GLdouble) B,
			(GLdouble) A);
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Coordinate transformations.
%

:- pragma foreign_proc("C", 
	depth_range(Near::in, Far::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDepthRange((GLclampd) Near, (GLclampd) Far);
	IO = IO0;
").

:- pragma foreign_proc("C", 
	viewport(X::in, Y::in, Wdth::in, Hght::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glViewport((GLint) X, (GLint) Y, (GLsizei) Wdth, (GLsizei) Hght);
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- func matrix_mode_to_int(matrix_mode) = int.

matrix_mode_to_int(texture)	= 0.
matrix_mode_to_int(modelview)	= 1.
matrix_mode_to_int(projection)	= 2.

:- pragma foreign_decl("C", "
	extern const GLenum matrix_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum matrix_mode_flags[] = {
		GL_TEXTURE,
		GL_MODELVIEW,
		GL_PROJECTION
	};
").

matrix_mode(Mode, !IO) :-
	matrix_mode2(matrix_mode_to_int(Mode), !IO).

:- pred matrix_mode2(int, io, io).
:- mode matrix_mode2(in, di, uo) is det.

:- pragma foreign_proc("C", 
	matrix_mode2(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glMatrixMode(matrix_mode_flags[I]);
	IO = IO0;
").

load_matrix(Matrix, !IO) :-
	Matrix = m(
		A1, A5, A9,  A13,
		A2, A6, A10, A14,
		A3, A7, A11, A15,
		A4, A8, A12, A16
	),
	load_matrix2(A1, A2, A3, A4, A5, A6, A7, A8,
		A9, A10, A11, A12, A13, A14, A15, A16, !IO).

:- pred load_matrix2(
		float, float, float, float,
		float, float, float, float,
		float, float, float, float,
		float, float, float, float, io, io).
:- mode load_matrix2(
		in, in, in, in,
		in, in, in, in,
		in, in, in, in,
		in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	load_matrix2(A1::in, A2::in, A3::in, A4::in,
		A5::in, A6::in, A7::in, A8::in,
		A9::in, A10::in, A11::in, A12::in,
		A13::in, A14::in, A15::in, A16::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		GLfloat	a[16];
		a[0] = (GLfloat) A1; a[1] = (GLfloat) A2;
		a[2] = (GLfloat) A3; a[3] = (GLfloat) A4;
		a[4] = (GLfloat) A5; a[5] = (GLfloat) A6;
		a[6] = (GLfloat) A7; a[7] = (GLfloat) A8;
		a[8] = (GLfloat) A9; a[9] = (GLfloat) A10;
		a[10] = (GLfloat) A11; a[11] = (GLfloat) A12;
		a[12] = (GLfloat) A13; a[13] = (GLfloat) A14;
		a[14] = (GLfloat) A15; a[15] = (GLfloat) A16;
		glLoadMatrixf(a);
	} else {
		GLdouble	a[16];
		a[0] = (GLdouble) A1; a[1] = (GLdouble) A2;
		a[2] = (GLdouble) A3; a[3] = (GLdouble) A4;
		a[4] = (GLdouble) A5; a[5] = (GLdouble) A6;
		a[6] = (GLdouble) A7; a[7] = (GLdouble) A8;
		a[8] = (GLdouble) A9; a[9] = (GLdouble) A10;
		a[10] = (GLdouble) A11; a[11] = (GLdouble) A12;
		a[12] = (GLdouble) A13; a[13] = (GLdouble) A14;
		a[14] = (GLdouble) A15; a[15] = (GLdouble) A16;
		glLoadMatrixd(a);
	}
	IO = IO0;
").

mult_matrix(Matrix, !IO) :-
	Matrix = m(
		A1, A5, A9, A13,
		A2, A6, A10, A14,
		A3, A7, A11, A15,
		A4, A8, A12, A16
	),
	mult_matrix2(A1, A2, A3, A4, A5, A6, A7, A8,
		A9, A10, A11, A12, A13, A14, A15, A16, !IO).

:- pred mult_matrix2(
		float, float, float, float,
		float, float, float, float,
		float, float, float, float,
		float, float, float, float, io, io).
:- mode mult_matrix2(
		in, in, in, in,
		in, in, in, in,
		in, in, in, in,
		in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C",
	mult_matrix2(A1::in, A2::in, A3::in, A4::in,
		A5::in, A6::in, A7::in, A8::in,
		A9::in, A10::in, A11::in, A12::in,
		A13::in, A14::in, A15::in, A16::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if (sizeof(MR_Float) == sizeof(GLfloat))
	{
		GLfloat	a[16];
		a[0] = (GLfloat) A1; a[1] = (GLfloat) A2;
		a[2] = (GLfloat) A3; a[3] = (GLfloat) A4;
		a[4] = (GLfloat) A5; a[5] = (GLfloat) A6;
		a[6] = (GLfloat) A7; a[7] = (GLfloat) A8;
		a[8] = (GLfloat) A9; a[9] = (GLfloat) A10;
		a[10] = (GLfloat) A11; a[11] = (GLfloat) A12;
		a[12] = (GLfloat) A13; a[13] = (GLfloat) A14;
		a[14] = (GLfloat) A15; a[15] = (GLfloat) A16;
		glMultMatrixf(a);
	} else {
		GLdouble	a[16];
		a[0] = (GLdouble) A1; a[1] = (GLdouble) A2;
		a[2] = (GLdouble) A3; a[3] = (GLdouble) A4;
		a[4] = (GLdouble) A5; a[5] = (GLdouble) A6;
		a[6] = (GLdouble) A7; a[7] = (GLdouble) A8;
		a[8] = (GLdouble) A9; a[9] = (GLdouble) A10;
		a[10] = (GLdouble) A11; a[11] = (GLdouble) A12;
		a[12] = (GLdouble) A13; a[13] = (GLdouble) A14;
		a[14] = (GLdouble) A15; a[15] = (GLdouble) A16;
		glMultMatrixd(a);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	load_identity(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glLoadIdentity();
	IO = IO0;
").

:- pragma foreign_proc("C", 
	rotate(Theta::in, X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRotatef((GLfloat) Theta,
			(GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glRotated((GLdouble) Theta,
			(GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	translate(X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTranslatef((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glTranslated((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	scale(X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glScalef((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glScaled((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	frustum(L::in, R::in, B::in, T::in, N::in, F::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFrustum((GLdouble) L, (GLdouble) R, (GLdouble) B,
		(GLdouble) T, (GLdouble) N, (GLdouble) F);
	IO = IO0;
").

:- pragma foreign_proc("C", 
	ortho(L::in, R::in, B::in, T::in, N::in, F::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glOrtho((GLdouble) L, (GLdouble) R, (GLdouble) B,
		(GLdouble) T, (GLdouble) N, (GLdouble) F);
	IO = IO0;
").

:- pragma foreign_proc("C", 
	push_matrix(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glPushMatrix();
	IO = IO0;
").

:- pragma foreign_proc("C", 
	pop_matrix(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glPopMatrix();
	IO = IO0;
").

:- func texture_coord_to_int(texture_coord) = int.

texture_coord_to_int(s)	= 0.
texture_coord_to_int(t)	= 1.
texture_coord_to_int(r)	= 2.
texture_coord_to_int(q)	= 3.

:- pragma foreign_decl("C", "
	extern const GLenum texture_coord_flags[];
").

:- pragma foreign_code("C", "
	const GLenum texture_coord_flags[] = {
		GL_S,
		GL_T,
		GL_R,
		GL_Q
	};
").

:- func texture_gen_parameter_to_int(texture_gen_parameter) = int.

texture_gen_parameter_to_int(object_linear) = 0.
texture_gen_parameter_to_int(eye_linear)    = 1.
texture_gen_parameter_to_int(sphere_map)    = 2.

:- pragma foreign_decl("C", "
	extern const GLenum texture_gen_flags[];
").

:- pragma foreign_code("C", "
	const GLenum texture_gen_flags[] = {
		GL_OBJECT_LINEAR,
		GL_EYE_LINEAR,
		GL_SPHERE_MAP
	};
").

tex_gen(Coord, texture_gen_mode(Param), !IO) :-
	tex_genf2a(texture_coord_to_int(Coord),
		texture_gen_parameter_to_int(Param), !IO).
tex_gen(Coord, object_plane(Param), !IO) :-
	tex_genf2b(texture_coord_to_int(Coord), Param, !IO).
tex_gen(Coord, eye_plane(Param), !IO) :-
	tex_genf2c(texture_coord_to_int(Coord), Param, !IO).

:- pred tex_genf2a(int, int, io, io).
:- mode tex_genf2a(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	tex_genf2a(Coord::in, Param::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glTexGeni(texture_coord_flags[Coord], GL_TEXTURE_GEN_MODE,
		texture_gen_flags[Param]);
	IO = IO0;
").

:- pred tex_genf2b(int, float, io, io).
:- mode tex_genf2b(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	tex_genf2b(Coord::in, Param::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
" 
	glTexGend(texture_coord_flags[Coord], GL_OBJECT_PLANE, (GLdouble)Param);
	IO = IO0;
").

:- pred tex_genf2c(int, float, io, io).
:- mode tex_genf2c(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	tex_genf2c(Coord::in, Param::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
" 
	glTexGend(texture_coord_flags[Coord], GL_EYE_PLANE, (GLdouble) Param);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Clipping.
%

clip_plane(Num, clip(X, Y, Z, W), !IO) :-
	clip_plane2(Num, X, Y, Z, W, !IO).

:- pred clip_plane2(int, float, float, float, float, io, io).
:- mode clip_plane2(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	clip_plane2(I::in, X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLdouble p[4];

	p[0] = (GLdouble) X;
	p[1] = (GLdouble) Y;
	p[2] = (GLdouble) Z;
	p[3] = (GLdouble) W;
	glClipPlane(GL_CLIP_PLANE0+I, p);
	IO = IO0;
}").

%------------------------------------------------------------------------------%
%
% Current raster position.
%

:- pragma foreign_proc("C", 
	raster_pos2(X::in, Y::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
" 
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRasterPos2f((GLfloat) X, (GLfloat) Y);
	} else {
		glRasterPos2d((GLdouble) X, (GLdouble) Y);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	raster_pos3(X::in, Y::in, Z::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
" 
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRasterPos3f((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glRasterPos3d((GLdouble) X, (GLdouble) Y, (GLfloat) Z);
	}
	IO = IO0;
").

:- pragma foreign_proc("C", 
	raster_pos4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
" 
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRasterPos4f((GLfloat) X, (GLfloat) Y, (GLfloat) Z,
			(GLfloat) W);
	} else {
		glRasterPos4d((GLdouble) X, (GLdouble) Y, (GLfloat) Z,
			(GLdouble) W);
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Colors and coloring.
%

:- func face_direction_to_int(face_direction) = int.

face_direction_to_int(cw)  = 0.
face_direction_to_int(ccw) = 1.

:- pragma foreign_decl("C", "
	extern const GLenum face_direction_flags[];
").

:- pragma foreign_code("C", "
	const GLenum face_direction_flags[] = {
		GL_CW,
		GL_CCW
	};
").

:- func face_side_to_int(face_side) = int.

face_side_to_int(front) = 0.
face_side_to_int(back)  = 1.
face_side_to_int(front_and_back) = 2.

:- pragma foreign_decl("C", "
	extern const GLenum face_side_flags[];
").

:- pragma foreign_code("C", "
	const GLenum face_side_flags[] = {
		GL_FRONT,
		GL_BACK,
		GL_FRONT_AND_BACK
	};
").

:- func color_material_mode_to_int(color_material_mode)	= int.

color_material_mode_to_int(ambient)		= 0.
color_material_mode_to_int(diffuse)		= 1.
color_material_mode_to_int(ambient_and_diffuse)	= 2.
color_material_mode_to_int(specular)		= 3.
color_material_mode_to_int(emission)		= 4.

:- pragma foreign_decl("C", "
	extern const GLenum color_material_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum color_material_mode_flags[] = {
		GL_AMBIENT,
		GL_DIFFUSE,
		GL_AMBIENT_AND_DIFFUSE,
		GL_SPECULAR,
		GL_EMISSION
	};
").

:- func shade_model_to_int(shade_model) = int.

shade_model_to_int(smooth) = 0.
shade_model_to_int(flat)   = 1.

:- pragma foreign_decl("C", "
	extern const GLenum shade_model_flags[];
").

:- pragma foreign_code("C", "
	const GLenum shade_model_flags[] = {
		GL_SMOOTH,
		GL_FLAT
	};
").

front_face(Face, !IO) :-
	front_face2(face_direction_to_int(Face), !IO).

:- pred front_face2(int, io, io).
:- mode front_face2(in, di, uo) is det.

:- pragma foreign_proc("C", 
	front_face2(F::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFrontFace(face_direction_flags[F]);
	IO = IO0;
").

material(Face, ambient(R, G, B, A), !IO) :-
	material_ambient(face_side_to_int(Face), R, G, B, A, !IO).
material(Face, diffuse(R, G, B, A), !IO)  :-
	material_diffuse(face_side_to_int(Face), R, G, B, A, !IO).
material(Face, ambient_and_diffuse(R, G, B, A), !IO) :- 
	material_ambient_and_diffuse(face_side_to_int(Face), R, G, B, A, !IO).
material(Face, specular(R, G, B, A), !IO) :-
	material_specular(face_side_to_int(Face), R, G, B, A, !IO).
material(Face, emission(R, G, B, A), !IO) :-
	material_emission(face_side_to_int(Face), R, G, B, A, !IO).
material(Face, shininess(S), !IO) :-
	material_shininess(face_side_to_int(Face), S, !IO).
material(Face, color_indexes(R, G, B), !IO) :-
	material_color_indexes(face_side_to_int(Face), R, G, B, !IO).

:- pred material_ambient(int, float, float, float, float, io, io).
:- mode material_ambient(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_ambient(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_AMBIENT, params);
	IO = IO0;
}").

:- pred material_diffuse(int, float, float, float, float, io, io).
:- mode material_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_diffuse(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_DIFFUSE, params);
	IO = IO0;
}").

:- pred material_ambient_and_diffuse(int, float, float, float, float, io, io).
:- mode material_ambient_and_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_ambient_and_diffuse(F::in, R::in, G::in, B::in, A::in, IO0::di,
		IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_AMBIENT_AND_DIFFUSE, params);
	IO = IO0;
}").

:- pred material_specular(int, float, float, float, float, io, io).
:- mode material_specular(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_specular(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_SPECULAR, params);
	IO = IO0;
}").

:- pred material_emission(int, float, float, float, float, io, io).
:- mode material_emission(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_emission(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_EMISSION, params);
	IO = IO0;
}").

:- pred material_shininess(int, float, io, io).
:- mode material_shininess(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_shininess(F::in, S::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	glMaterialf(face_side_flags[F], GL_SHININESS, (GLfloat) S);
	IO = IO0;
}").

:- pred material_color_indexes(int, float, float, float, io, io).
:- mode material_color_indexes(in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	material_color_indexes(F::in, R::in, G::in, B::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[3];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	glMaterialfv(face_side_flags[F], GL_COLOR_INDEXES, params);
	IO = IO0;
}").

light(Num, ambient(R, G, B, A), !IO) :-
	light_ambient(Num, R, G, B, A, !IO).
light(Num, diffuse(R, G, B, A), !IO) :-
	light_diffuse(Num, R, G, B, A, !IO).
light(Num, specular(R, G, B, A), !IO) :-
	light_specular(Num, R, G, B, A, !IO).
light(Num, position(X, Y, Z, W), !IO) :- 
	light_position(Num, X, Y, Z, W, !IO).
light(Num, spot_direction(I, J, K), !IO) :-
	light_spot_direction(Num, I, J, K, !IO).
light(Num, spot_exponent(K), !IO) :-
	light_spot_exponent(Num, K, !IO).
light(Num, spot_cutoff(K), !IO)  :-
	light_spot_cutoff(Num, K, !IO).
light(Num, constant_attenuation(K), !IO) :-
	light_constant_attenuation(Num, K, !IO).
light(Num, linear_attenuation(K), !IO) :-
	light_linear_attenuation(Num, K, !IO).
light(Num, quadratic_attenuation(K), !IO) :-
	light_quadratic_attenuation(Num, K, !IO).

:- pred light_ambient(int, float, float, float, float, io, io).
:- mode light_ambient(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_ambient(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_AMBIENT, params);
	IO = IO0;
}").

:- pred light_diffuse(int, float, float, float, float, io, io).
:- mode light_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_diffuse(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_DIFFUSE, params);
	IO = IO0;
}").

:- pred light_specular(int, float, float, float, float, io, io).
:- mode light_specular(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_specular(F::in, R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_SPECULAR, params);
	IO = IO0;
}").

:- pred light_position(int, float, float, float, float, io, io).
:- mode light_position(in, in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_position(F::in, X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[4];

	params[0] = (GLfloat) X;
	params[1] = (GLfloat) Y;
	params[2] = (GLfloat) Z;
	params[3] = (GLfloat) W;
	glLightfv(F + GL_LIGHT0, GL_POSITION, params);
	IO = IO0;
}").

:- pred light_spot_direction(int, float, float, float, io, io).
:- mode light_spot_direction(in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_spot_direction(F::in, I::in, J::in, K::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	GLfloat	params[3];

	params[0] = (GLfloat) I;
	params[1] = (GLfloat) J;
	params[2] = (GLfloat) K;
	glLightfv(F + GL_LIGHT0, GL_SPOT_DIRECTION, params);
	IO = IO0;
}").

:- pred light_spot_exponent(int, float, io, io).
:- mode light_spot_exponent(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_spot_exponent(F::in, E::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	glLightf(F + GL_LIGHT0, GL_SPOT_EXPONENT, (GLfloat) E);
	IO = IO0;
}").

:- pred light_spot_cutoff(int, float, io, io).
:- mode light_spot_cutoff(in, in, di, uo) is det.

:- pragma foreign_proc("C", 
	light_spot_cutoff(F::in, E::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	glLightf(F + GL_LIGHT0, GL_SPOT_CUTOFF, (GLfloat) E);
	IO = IO0;
}").

:- pred light_constant_attenuation(int, float, io, io).
:- mode light_constant_attenuation(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	light_constant_attenuation(F::in, E::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	glLightf(F + GL_LIGHT0, GL_CONSTANT_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- pred light_linear_attenuation(int, float, io, io).
:- mode light_linear_attenuation(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	light_linear_attenuation(F::in, E::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	glLightf(F + GL_LIGHT0, GL_LINEAR_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- pred light_quadratic_attenuation(int, float, io, io).
:- mode light_quadratic_attenuation(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	light_quadratic_attenuation(F::in, E::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	glLightf(F + GL_LIGHT0, GL_QUADRATIC_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- func bool_to_int(bool) = int.

bool_to_int(no)  = 0.
bool_to_int(yes) = 1.

light_model(light_model_ambient(R, G, B, A), !IO) :-
	light_model_ambient(R, G, B, A, !IO).
light_model(light_model_local_viewer(Bool), !IO) :-
	light_model_local_viewer(bool_to_int(Bool), !IO).
light_model(light_model_two_side(Bool), !IO) :-
	light_model_two_side(bool_to_int(Bool), !IO).

:- pred light_model_ambient(float, float, float, float, io, io).
:- mode light_model_ambient(in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C",
	light_model_ambient(R::in, G::in, B::in, A::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	GLfloat params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, params);
	IO = IO0;
}").

:- pred light_model_local_viewer(int, io, io).
:- mode light_model_local_viewer(in, di, uo) is det.

:- pragma foreign_proc("C",
	light_model_local_viewer(F::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"{
	glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, (GLint) F);
	IO = IO0;
}").

:- pred light_model_two_side(int, io, io).
:- mode light_model_two_side(in, di, uo) is det.

:- pragma foreign_proc("C",
	light_model_two_side(F::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"{
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, (GLint) F);
	IO = IO0;
}").

color_material(Face, Mode, !IO) :-
	color_material2(face_side_to_int(Face),
		color_material_mode_to_int(Mode), !IO).

:- pred color_material2(int, int, io, io).
:- mode color_material2(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	color_material2(Face::in, Mode::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glColorMaterial(face_side_flags[Face], color_material_mode_flags[Mode]);
	IO = IO0;
").

shade_model(Model, !IO) :-
	shade_model2(shade_model_to_int(Model), !IO).

:- pred shade_model2(int, io, io).
:- mode shade_model2(in, di, uo) is det.

:- pragma foreign_proc("C",
	shade_model2(Model::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glShadeModel(shade_model_flags[Model]);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Points.
%

:- pragma foreign_proc("C",
	point_size(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glPointSize((GLfloat) Size);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Line segments.
%

:- pragma foreign_proc("C",
	line_width(Size::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glLineWidth((GLfloat) Size);
	IO = IO0;
").

:- pragma foreign_proc("C",
	line_stipple(Fac::in, Pat::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glLineStipple((GLint) Fac, (GLushort) Pat);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Polygons.
%

:- func polygon_mode_to_int(polygon_mode) = int.

polygon_mode_to_int(point) = 0.
polygon_mode_to_int(line)  = 1.
polygon_mode_to_int(fill)  = 2.

:- pragma foreign_decl("C", "
	extern const GLenum polygon_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum polygon_mode_flags[] = {
		GL_POINT,
		GL_LINE,
		GL_FILL
	};
").

cull_face(Face, !IO) :-
	cull_face2(face_side_to_int(Face), !IO).

:- pred cull_face2(int, io, io).
:- mode cull_face2(in, di, uo) is det.

:- pragma foreign_proc("C",
	cull_face2(F::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glCullFace(face_side_flags[F]);
	IO = IO0;
").

%:- pred polygon_stipple(polygon_stipple, io__state, io__state).
%:- mode polygon_stipple(in, di, uo) is det.

polygon_stipple(_, _, _) :-
	error("sorry, polygon_stipple unimplemented").	

polygon_mode(Face, Mode, !IO) :-
	polygon_mode2(face_side_to_int(Face), polygon_mode_to_int(Mode), !IO).

:- pred polygon_mode2(int, int, io, io).
:- mode polygon_mode2(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	polygon_mode2(Face::in, Mode::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glPolygonMode(face_side_flags[Face], polygon_mode_flags[Mode]);
	IO = IO0;
").

:- pragma foreign_proc("C",
	polygon_offset(Fac::in, Units::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glPolygonOffset((GLfloat) Fac, (GLfloat) Units);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Pixel Rectangles.
%

/*

:- type pixel_store_parameter
		--->	unpack_swap_bytes(bool)
		;	unpack_lsb_first(bool)
		;	unpack_row_length(int)
		;	unpack_skip_rows(int)
		;	unpack_skip_pixels(int)
		;	unpack_alignment(int)
		.

:- pred pixel_store(pixel_store_parameter, io__state, io__state).
:- mode pixel_store(in, di, uo) is det.

:- type pixel_transfer_parameter
		--->	map_color(bool)
		;	map_stencil(bool)
		;	index_shift(int)
		;	index_offset(int)
		;	red_scale(float)
		;	green_scale(float)
		;	blue_scale(float)
		;	alpha_scale(float)
		;	red_bias(float)
		;	green_bias(float)
		;	blue_bias(float)
		;	alpha_bias(float)
		;	depth_bias(float)
		.

:- pred pixel_transfer(pixel_transfer_parameter, io__state, io__state).
:- mode pixel_transfer(in, di, uo) is det.

% pixel_map not implemented

:- type draw_mode
		--->	color_index
		;	stencil_index
		;	depth_component
		;	red
		;	green
		;	blue
		;	alpha
		;	rgb
		;	rgba
		;	luminance
		;	luminance_alpha
		.

:- type draw_data
		--->	bitmap(list(int))	% 32 bits
%		;	ubyte(list(int))	% 4 x 8 bits -> 4 x ubyte
%		;	byte(list(int))		% 4 x 8 bits -> 4 x byte
%		;	ushort(list(int))	% 2 x 16 bits -> 2 x ushort
%		;	short(list(int))	% 2 x 16 bits -> 2 x short
%		;	int(list(int))		% 1 x 32 bits -> 1 x int
		;	float(list(float))
		.

:- pred draw_pixels(int, int, draw_mode, draw_data, io__state, io__state).
:- mode draw_pixels(in, in, in, in, di, uo) is det.

*/

%------------------------------------------------------------------------------%
%
% Bitmaps
%

/*
:- pred bitmap(int, int, float, float, float, float, list(int),
		io__state, io__state).
:- mode bitmap(in, in, in, in, in, in, in, di, uo) is det.
*/

%------------------------------------------------------------------------------%
%
% Texturing
%

/*
:- type texture_target
		--->	texture_2d
		;	proxy_texture_2d
		.

:- type texture_format
		--->	alpha
		;	luminance
		;	luminance_alpha
		;	intensity
		;	rgb
		;	rgba
		.

:- pred tex_image_2d(texture_target, int, int, int, int, int,
		texture_format, texture_data, io__state, io__state).
:- mode tex_image_2d(in, in, in, in, in, in, in, in, di, uo) is det.
*/

%------------------------------------------------------------------------------%
%
% Fog.
%

:- func fog_mode_to_int(fog_mode) = int.

fog_mode_to_int(linear)	= 0.
fog_mode_to_int(exp)	= 1.
fog_mode_to_int(exp2)	= 2.

:- pragma foreign_decl("C", "
	extern const GLenum fog_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum fog_mode_flags[] = {
		GL_LINEAR,
		GL_EXP,
		GL_EXP2
	};
").

fog(fog_mode(Mode), !IO) :-
	fog_mode(fog_mode_to_int(Mode), !IO).
fog(fog_density(Density), !IO) :-
	fog_density(Density, !IO).
fog(fog_start(Start), !IO) :-
	fog_start(Start, !IO).
fog(fog_end(End), !IO)  :-
	fog_end(End, !IO).

:- pred fog_mode(int, io, io).
:- mode fog_mode(in, di, uo) is det.

:- pragma foreign_proc("C",
	fog_mode(M::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFogi(GL_FOG_MODE, (GLint) fog_mode_flags[M]);
	IO = IO0;
").

:- pred fog_density(float, io, io).
:- mode fog_density(in, di, uo) is det.

:- pragma foreign_proc("C",
	fog_density(P::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFogf(GL_FOG_DENSITY, (GLfloat) P);
	IO = IO0;
").

:- pred fog_start(float, io, io).
:- mode fog_start(in, di, uo) is det.

:- pragma foreign_proc("C", 
	fog_start(P::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFogf(GL_FOG_START, (GLfloat) P);
	IO = IO0;
").

:- pred fog_end(float, io, io).
:- mode fog_end(in, di, uo) is det.

:- pragma foreign_proc("C",
	fog_end(P::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFogf(GL_FOG_END, (GLfloat) P);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% Per-fragment operations.
%

/*

:- pred scissor(int, int, int, int, io__state, io__state).
:- mode scissor(in, in, in, in, di, uo) is det.

:- type test_func
		--->	never
		;	always
		;	less
		;	lequal
		;	equal
		;	gequal
		;	greater
		;	notequal
		.

:- pred alpha_func(test_func, float, io__state, io__state).
:- mode alpha_func(in, in, di, uo) is det.

:- pred stencil_func(test_func, float, int, io__state, io__state).
:- mode stencil_func(in, in, in, di, uo) is det.

:- type stencil_op
		--->	keep
		;	zero
		;	replace
		;	incr
		;	decr
		;	invert
		.

:- pred stencil_op(stencil_op, stencil_op, stencil_op, io__state, io__state).
:- mode stencil_op(in, in, in, di, uo) is det.

:- pred depth_func(test_func, io__state, io__state).
:- mode depth_func(in, di, uo) is det.

:- type	blend_src
		--->	zero
		;	one
		;	dst_color
		;	one_minus_dst_color
		;	src_alpha
		;	one_minus_src_alpha
		;	dst_alpha
		;	one_minus_dst_alpha
		;	src_alpha_saturate
		.

:- type blend_dst
		--->	zero
		;	one
		;	src_color
		;	one_minus_src_color
		;	src_alpha
		;	one_minus_src_alpha
		;	dst_alpha
		;	one_minus_dst_alpha
		.

:- pred blend_func(blend_src, blend_dst, io__state, io__state).
:- mode blend_func(in, in, di, uo) is det.

:- type logical_operation
		--->	clear
		;	(and)
		;	and_reverse
		;	copy
		;	and_inverted
		;	noop
		;	xor
		;	(or)
		;	nor
		;	equiv
		;	invert
		;	or_reverse
		;	copy_inverted
		;	or_inverted
		;	nand
		;	set
		.

*/

%------------------------------------------------------------------------------%
%
% Whole framebuffer operations.
%

:- func buffer_to_int(buffer) = int.

buffer_to_int(none)		= 0.
buffer_to_int(front_left)	= 1.
buffer_to_int(front_right)	= 2.
buffer_to_int(back_left)	= 3.
buffer_to_int(back_right)	= 4.
buffer_to_int(front)		= 5.
buffer_to_int(back)		= 6.
buffer_to_int(left)		= 7.
buffer_to_int(right)		= 8.
buffer_to_int(front_and_back) 	= 9.
buffer_to_int(aux(I))		= 10 + I.

:- pragma foreign_decl("C", "
	extern const GLenum buffer_flags[];
").

:- pragma foreign_code("C", "
	const GLenum buffer_flags[] = {
		GL_NONE,
		GL_FRONT_LEFT,
		GL_FRONT_RIGHT,
		GL_BACK_LEFT,
		GL_BACK_RIGHT,
		GL_FRONT,
		GL_BACK,
		GL_LEFT,
		GL_RIGHT,
		GL_FRONT_AND_BACK,
		GL_AUX0,
		GL_AUX1,
		GL_AUX2,
		GL_AUX3
	};
").

draw_buffer(Buffer, !IO) :-
	draw_buffer2(buffer_to_int(Buffer), !IO).

:- pred draw_buffer2(int, io, io).
:- mode draw_buffer2(in, di, uo) is det.

:- pragma foreign_proc("C",
	draw_buffer2(B::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDrawBuffer(buffer_flags[B]);
	IO = IO0;
").

:- pragma foreign_proc("C",
	index_mask(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glIndexMask((GLuint) I);
	IO = IO0;
").

color_mask(A, B, C, D, !IO) :-
	color_mask2(bool_to_int(A), bool_to_int(B),
		bool_to_int(C), bool_to_int(D), !IO).

:- pred color_mask2(int, int, int, int, io, io).
:- mode color_mask2(in, in, in, in, di, uo) is det.

:- pragma foreign_proc("C",
	color_mask2(A::in, B::in, C::in, D::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glColorMask((GLboolean) A, (GLboolean) B, (GLboolean) C, (GLboolean) D);
	IO = IO0;
").

depth_mask(Bool, !IO) :-
	depth_mask2(bool_to_int(Bool), !IO).

:- pred depth_mask2(int, io, io).
:- mode depth_mask2(in, di, uo) is det.

:- pragma foreign_proc("C",
	depth_mask2(M::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDepthMask((GLboolean) M);
	IO = IO0;
").

:- pragma foreign_proc("C",
	stencil_mask(M::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glStencilMask((GLuint) M);
	IO = IO0;
").

clear(BitList, !IO) :-
	Mask = list.foldr((\/), list.map(buffer_bit_to_bit, BitList), 0),
	clear2(Mask, !IO).

:- func buffer_bit_to_bit(buffer_bit) = int.

buffer_bit_to_bit(Flag) = lookup_buffer_bit(buffer_bit_to_int(Flag)).

:- func buffer_bit_to_int(buffer_bit) = int.

buffer_bit_to_int(color)   = 0.
buffer_bit_to_int(depth)   = 1.
buffer_bit_to_int(stencil) = 2.
buffer_bit_to_int(accum)   = 3.

:- func lookup_buffer_bit(int)	= int.

:- pragma foreign_proc("C",
	lookup_buffer_bit(F::in) = (B::out),
	[will_not_call_mercury, promise_pure],
"{
	static GLbitfield a[] = {
		GL_COLOR_BUFFER_BIT,
		GL_DEPTH_BUFFER_BIT,
		GL_STENCIL_BUFFER_BIT,
		GL_ACCUM_BUFFER_BIT
	};

	B = a[F];
}").

:- pred clear2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
	clear2(Mask::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glClear(Mask);
	IO = IO0;
").

:- pragma foreign_proc("C",
	clear_color(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glClearColor((GLclampf) R, (GLclampf) G, (GLclampf) B, (GLclampf) A);
	IO = IO0;
").

:- pragma foreign_proc("C",
	clear_index(I::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glClearIndex((GLfloat) I);
	IO = IO0;
").

:- pragma foreign_proc("C",
	clear_depth(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glClearDepth((GLfloat) I);
	IO = IO0;
").

:- pragma foreign_proc("C",
	clear_stencil(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glClearStencil((GLint) I);
	IO = IO0;
").

:- pragma foreign_proc("C", 
	clear_accum(R::in, G::in, B::in, A::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glClearAccum((GLfloat) R, (GLfloat) G, (GLfloat) B, (GLfloat) A);
	IO = IO0;
").

:- func accum_op_to_int(accum_op) = int.

accum_op_to_int(accum)	= 0.
accum_op_to_int(load)	= 1.
accum_op_to_int(return)	= 2.
accum_op_to_int(mult)	= 3.
accum_op_to_int(add)	= 4.

:- pragma foreign_decl("C", "
	extern const GLenum accum_op_flags[];
").

:- pragma foreign_code("C", "
	const GLenum accum_op_flags[] = {
		GL_ACCUM,
		GL_LOAD,
		GL_RETURN,
		GL_MULT,
		GL_ADD
	};
").

accum(Op, Param, !IO) :-
	accum2(accum_op_to_int(Op), Param, !IO).

:- pred accum2(int, float, io, io).
:- mode accum2(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	accum2(Op::in, Param::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glAccum(accum_op_flags[Op], Param);
	IO = IO0;
").

%------------------------------------------------------------------------------%

% Evalutators not implemented

%------------------------------------------------------------------------------%
%
% Selection.
%

/*

:- pred init_names(io__state, io__state).
:- mode init_names(di, uo) is det.

:- pred pop_name(io__state, io__state).
:- mode pop_name(di, uo) is det.

:- pred push_name(int, io__state, io__state).
:- mode push_name(in, di, uo) is det.

:- pred load_name(int, io__state, io__state).
:- mode load_name(in, di, uo) is det.

:- type render_mode
		--->	render
		;	select
		;	feedback
		.

:- pred render_mode(render_mode, int, io__state, io__state).
:- mode render_mode(in, out, di, uo) is det.

*/

%------------------------------------------------------------------------------%
%
% Display lists.
% 

:- func display_list_mode_to_int(display_list_mode) = int.

display_list_mode_to_int(compile) = 0.
display_list_mode_to_int(compile_and_execute) = 1.

:- pragma foreign_decl("C", "
	extern const GLenum display_list_mode_flags[];
").

:- pragma foreign_code("C", "
	const GLenum display_list_mode_flags[] ={
		GL_COMPILE,
		GL_COMPILE_AND_EXECUTE
	};
").

new_list(Num, Mode, !IO) :-
	new_list2(Num, display_list_mode_to_int(Mode), !IO).

:- pred new_list2(int, int, io, io).
:- mode new_list2(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	new_list2(N::in, M::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glNewList((GLuint) N, display_list_mode_flags[M]);
	IO = IO0;
").

:- pragma foreign_proc("C",
	end_list(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glEndList();
	IO = IO0;
").

:- pragma foreign_proc("C",
	call_list(N::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glCallList((GLuint) N);
	IO = IO0;
").

:- pragma foreign_proc("C",
	gen_lists(N::in, M::out, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	M = (MR_Integer) glGenLists((GLsizei) N);
	IO = IO0;
").

:- pragma foreign_proc("C",
	delete_lists(N::in, M::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDeleteLists((GLuint) N, (GLsizei) M);
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	flush(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFlush();
	IO = IO0;
	assert(glGetError() == GL_NO_ERROR);
").

:- pragma foreign_proc("C",
	finish(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glFinish();
	IO = IO0;
	assert(glGetError() == GL_NO_ERROR);
").

%------------------------------------------------------------------------------%

:- func control_flag_to_int(control_flag) = int.

control_flag_to_int(normalize)       = 0.
control_flag_to_int(clip_plane(_))   = 1.
control_flag_to_int(lighting)        = 2.
control_flag_to_int(light(_))        = 3.
control_flag_to_int(color_material)  = 4.
control_flag_to_int(line_stipple)    = 5.
control_flag_to_int(cull_face)       = 6.
control_flag_to_int(polygon_stipple) = 7.
control_flag_to_int(polygon_offset_point) = 8.
control_flag_to_int(polygon_offset_line)  = 9.
control_flag_to_int(polygon_offset_fill)  = 10.
control_flag_to_int(fog)            = 11.
control_flag_to_int(scissor_test)   = 12.
control_flag_to_int(alpha_test)     = 13.
control_flag_to_int(stencil_test)   = 14.
control_flag_to_int(depth_test)     = 15.
control_flag_to_int(blend)          = 16.
control_flag_to_int(dither)         = 17.
control_flag_to_int(index_logic_op) = 18.
control_flag_to_int(color_logic_op) = 19.

:- pragma foreign_decl("C", "
	extern const GLenum control_flag_flags[];
").

:- pragma foreign_code("C", "
	const GLenum control_flag_flags[] = {
		GL_NORMALIZE,
		GL_CLIP_PLANE0,
		GL_LIGHTING,
		GL_LIGHT0,
		GL_COLOR_MATERIAL,
		GL_LINE_STIPPLE,
		GL_CULL_FACE,
		GL_POLYGON_STIPPLE,
		GL_POLYGON_OFFSET_POINT,
		GL_POLYGON_OFFSET_LINE,
		GL_POLYGON_OFFSET_FILL,
		GL_FOG,
		GL_SCISSOR_TEST,
		GL_ALPHA_TEST,
		GL_STENCIL_TEST,
		GL_DEPTH_TEST,
		GL_BLEND,
		GL_DITHER,
		GL_INDEX_LOGIC_OP,
		GL_COLOR_LOGIC_OP
	};
").

enable(Flag, !IO) :-
	( Flag = clip_plane(I) ->
	  	enable3(control_flag_to_int(Flag), I, !IO)
	; Flag = light(I) ->
	  	enable3(control_flag_to_int(Flag), I, !IO)
	;	
		enable2(control_flag_to_int(Flag), !IO)
	).

:- pred enable2(int, io, io).
:- mode enable2(in, di, uo) is det.

:- pragma foreign_proc("C",
	enable2(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glEnable(control_flag_flags[I]);
	IO = IO0;
").

:- pred enable3(int, int, io, io).
:- mode enable3(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	enable3(I::in, J::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glEnable(control_flag_flags[I]+J);
	IO = IO0;
").

disable(Flag, !IO) :-
	( Flag = clip_plane(I)  ->
		disable3(control_flag_to_int(Flag), I, !IO)
	; Flag = light(I) ->
		disable3(control_flag_to_int(Flag), I, !IO)
	;
		disable2(control_flag_to_int(Flag), !IO)
	).

:- pred disable2(int, io, io).
:- mode disable2(in, di, uo) is det.

:- pragma foreign_proc("C",
	disable2(I::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDisable(control_flag_flags[I]);
	IO = IO0;
").

:- pred disable3(int, int, io, io).
:- mode disable3(in, in, di, uo) is det.

:- pragma foreign_proc("C",
	disable3(I::in, J::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glDisable(control_flag_flags[I]+J);
	IO = IO0;
").

%------------------------------------------------------------------------------%
:- end_module mogl.
%------------------------------------------------------------------------------%
