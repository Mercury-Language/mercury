%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%
% file: mogl.m
% main author: conway.
%
% This file provides a binding to OpenGL 1.1. (It won't work with OpenGL 1.0.)
%
%------------------------------------------------------------------------------%

:- module mogl.

%------------------------------------------------------------------------------%

:- interface.

:- import_module io, int, float, list, bool.

%------------------------------------------------------------------------------%
%
% 2.5	GL Errors
%
%------------------------------------------------------------------------------%

:- type error	--->
		no_error
	;	invalid_enum
	;	invalid_value
	;	invalid_operation
	;	stack_overflow
	;	stack_underflow
	;	out_of_memory
	.

:- pred get_error(mogl__error, io__state, io__state).
:- mode get_error(out, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.6	Begin/End Objects
%
%------------------------------------------------------------------------------%

:- type block_mode	--->
		points
	;	line_strip
	;	line_loop
	;	lines
	;	polygon
	;	triangle_strip
	;	triangle_fan
	;	triangles
	;	quad_strip
	;	quads
	.

:- pred begin(block_mode, io__state, io__state).
:- mode begin(in, di, uo) is det.

:- pred end(io__state, io__state).
:- mode end(di, uo) is det.

	% 2.6.2	Polygon Edges

:- pred edge_flag(bool, io__state, io__state).
:- mode edge_flag(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.7	Vertex Specification
%
%------------------------------------------------------------------------------%

:- pred vertex2(float, float, io__state, io__state).
:- mode vertex2(in, in, di, uo) is det.

:- pred vertex3(float, float, float, io__state, io__state).
:- mode vertex3(in, in, in, di, uo) is det.

:- pred vertex4(float, float, float, float, io__state, io__state).
:- mode vertex4(in, in, in, in, di, uo) is det.

:- pred tex_coord1(float, io__state, io__state).
:- mode tex_coord1(in, di, uo) is det.

:- pred tex_coord2(float, float, io__state, io__state).
:- mode tex_coord2(in, in, di, uo) is det.

:- pred tex_coord3(float, float, float, io__state, io__state).
:- mode tex_coord3(in, in, in, di, uo) is det.

:- pred tex_coord4(float, float, float, float, io__state, io__state).
:- mode tex_coord4(in, in, in, in, di, uo) is det.

:- pred normal3(float, float, float, io__state, io__state).
:- mode normal3(in, in, in, di, uo) is det.

:- pred color3(float, float, float, io__state, io__state).
:- mode color3(in, in, in, di, uo) is det.

:- pred color4(float, float, float, float, io__state, io__state).
:- mode color4(in, in, in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.10	Coordinate Transformations
%
%------------------------------------------------------------------------------%

:- pred depth_range(float, float, io__state, io__state).
:- mode depth_range(in, in, di, uo) is det.

:- pred viewport(int, int, int, int, io__state, io__state).
:- mode viewport(in, in, in, in, di, uo) is det.

:- type matrix_mode
	--->	texture
	;	modelview
	;	projection
	.

:- type	matrix
	--->	m(float, float, float, float,	% a[11], a[12], ...
		  float, float, float, float,	% a[21], a[22], ...
		  float, float, float, float,	% a[31], a[32], ...
		  float, float, float, float).	% a[41], a[42], ...

:- pred matrix_mode(matrix_mode, io__state, io__state).
:- mode matrix_mode(in, di, uo) is det.

:- pred load_matrix(matrix, io__state, io__state).
:- mode load_matrix(in, di, uo) is det.

:- pred mult_matrix(matrix, io__state, io__state).
:- mode mult_matrix(in, di, uo) is det.

:- pred load_identity(io__state, io__state).
:- mode load_identity(di, uo) is det.

:- pred rotate(float, float, float, float, io__state, io__state).
:- mode rotate(in, in, in, in, di, uo) is det.

:- pred translate(float, float, float, io__state, io__state).
:- mode translate(in, in, in, di, uo) is det.

:- pred scale(float, float, float, io__state, io__state).
:- mode scale(in, in, in, di, uo) is det.

:- pred frustum(float, float, float, float, float, float,
		io__state, io__state).
:- mode frustum(in, in, in, in, in, in, di, uo) is det.

:- pred ortho(float, float, float, float, float, float,
		io__state, io__state).
:- mode ortho(in, in, in, in, in, in, di, uo) is det.

:- pred push_matrix(io__state, io__state).
:- mode push_matrix(di, uo) is det.

:- pred pop_matrix(io__state, io__state).
:- mode pop_matrix(di, uo) is det.

:- type texture_coord --->	s ; t ; r ; q.

:- type texture_parameter(T) --->
		texture_gen_mode(texture_gen_parameter)
	;	object_plane(T)
	;	eye_plane(T)
	.

:- type texture_gen_parameter --->
		object_linear
	;	eye_linear
	;	sphere_map
	.

:- pred tex_gen(texture_coord, texture_parameter(float), io__state, io__state).
:- mode tex_gen(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.11	Clipping
%
%------------------------------------------------------------------------------%

:- type	clip_plane	--->
		clip(float, float, float, float).

:- pred clip_plane(int, clip_plane, io__state, io__state).
:- mode clip_plane(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.12	Current Raster Position
%
%------------------------------------------------------------------------------%

:- pred raster_pos2(float, float, io__state, io__state).
:- mode raster_pos2(in, in, di, uo) is det.

:- pred raster_pos3(float, float, float, io__state, io__state).
:- mode raster_pos3(in, in, in, di, uo) is det.

:- pred raster_pos4(float, float, float, float, io__state, io__state).
:- mode raster_pos4(in, in, in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 2.13	Colors and Coloring
%
%------------------------------------------------------------------------------%

:- type face_direction	--->	cw ; ccw .

:- type face_side	--->	front ; back ; front_and_back .

:- type material
		--->	ambient(float, float, float, float)
		;	diffuse(float, float, float, float)
		;	ambient_and_diffuse(float, float, float, float)
		;	specular(float, float, float, float)
		;	emission(float, float, float, float)
		;	shininess(float)
		;	color_indexes(float, float, float)
		.

:- type light_no ==	int.

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
		;	quadratic_attenuation(float)
		.

:- type lighting_model
		--->	light_model_ambient(float, float, float, float)
		;	light_model_local_viewer(bool)
		;	light_model_two_side(bool)
		.

:- type color_material_mode
		--->	ambient
		;	diffuse
		;	ambient_and_diffuse
		;	specular
		;	emission
		.

:- type shade_model --->	smooth ; flat .

:- pred front_face(face_direction, io__state, io__state).
:- mode front_face(in, di, uo) is det.

:- pred material(face_side, material, io__state, io__state).
:- mode material(in, in, di, uo) is det.

:- pred light(light_no, light, io__state, io__state).
:- mode light(in, in, di, uo) is det.

:- pred light_model(lighting_model, io__state, io__state).
:- mode light_model(in, di, uo) is det.

:- pred color_material(face_side, color_material_mode, io__state, io__state).
:- mode color_material(in, in, di, uo) is det.

:- pred shade_model(shade_model, io__state, io__state).
:- mode shade_model(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 3.3	Points
%
%------------------------------------------------------------------------------%

:- pred point_size(float, io__state, io__state).
:- mode point_size(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 3.4	Lines Segments
%
%------------------------------------------------------------------------------%

:- pred line_width(float, io__state, io__state).
:- mode line_width(in, di, uo) is det.

:- pred line_stipple(int, int, io__state, io__state).
:- mode line_stipple(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 3.5	Polygons
%
%------------------------------------------------------------------------------%

:- type polygon_stipple	==	int.	% use bottom 32 bits of each int.

:- type polygon_mode
		--->	point
		;	line
		;	fill
		.

:- pred cull_face(face_side, io__state, io__state).
:- mode cull_face(in, di, uo) is det.

:- pred polygon_stipple(polygon_stipple, io__state, io__state).
:- mode polygon_stipple(in, di, uo) is det.

:- pred polygon_mode(face_side, polygon_mode, io__state, io__state).
:- mode polygon_mode(in, in, di, uo) is det.

:- pred polygon_offset(float, float, io__state, io__state).
:- mode polygon_offset(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 3.6	Pixel Rectangles
%
%------------------------------------------------------------------------------%

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
% 3.7	Bitmaps
%
%------------------------------------------------------------------------------%

/*
:- pred bitmap(int, int, float, float, float, float, list(int),
		io__state, io__state).
:- mode bitmap(in, in, in, in, in, in, in, di, uo) is det.
*/

%------------------------------------------------------------------------------%
%
% 3.8	Texturing
%
%------------------------------------------------------------------------------%

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
% 3.9	Fog
%
%------------------------------------------------------------------------------%

:- type fog_parameter
		--->	fog_mode(fog_mode)
		;	fog_density(float)
		;	fog_start(float)
		;	fog_end(float)
		.

:- type fog_mode
		--->	linear
		;	exp
		;	exp2
		.

:- pred fog(fog_parameter, io__state, io__state).
:- mode fog(in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 4.1	Per-Fragment Operations
%
%------------------------------------------------------------------------------%

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
% 4.2	Whole Framebuffer Operations
%
%------------------------------------------------------------------------------%

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
		;	aux(int)
		.

:- pred draw_buffer(buffer, io__state, io__state).
:- mode draw_buffer(in, di, uo) is det.

:- pred index_mask(int, io__state, io__state).
:- mode index_mask(in, di, uo) is det.

:- pred color_mask(bool, bool, bool, bool, io__state, io__state).
:- mode color_mask(in, in, in, in, di, uo) is det.

:- pred depth_mask(bool, io__state, io__state).
:- mode depth_mask(in, di, uo) is det.

:- pred stencil_mask(int, io__state, io__state).
:- mode stencil_mask(in, di, uo) is det.

:- type buffer_bit
		--->	color
		;	depth
		;	stencil
		;	accum
		.

:- pred clear(list(buffer_bit), io__state, io__state).
:- mode clear(in, di, uo) is det.

:- pred clear_color(float, float, float, float, io__state, io__state).
:- mode clear_color(in, in, in, in, di, uo) is det.

:- pred clear_index(float, io__state, io__state).
:- mode clear_index(in, di, uo) is det.

:- pred clear_depth(float, io__state, io__state).
:- mode clear_depth(in, di, uo) is det.

:- pred clear_stencil(int, io__state, io__state).
:- mode clear_stencil(in, di, uo) is det.

:- pred clear_accum(float, float, float, float, io__state, io__state).
:- mode clear_accum(in, in, in, in, di, uo) is det.

:- type accum_op
		--->	accum
		;	load
		;	return
		;	mult
		;	add
		.

:- pred accum(accum_op, float, io__state, io__state).
:- mode accum(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 5.1 Evaluators
%
%------------------------------------------------------------------------------%

% Evalutators not implemented

%------------------------------------------------------------------------------%
%
% 5.2 Selection
%
%------------------------------------------------------------------------------%

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
% 5.4 Display Lists
%
%------------------------------------------------------------------------------%

:- type display_list_mode --->
		compile
	;	compile_and_execute
	.

:- pred new_list(int, display_list_mode, io__state, io__state).
:- mode new_list(in, in, di, uo) is det.

:- pred end_list(io__state, io__state).
:- mode end_list(di, uo) is det.

:- pred call_list(int, io__state, io__state).
:- mode call_list(in, di, uo) is det.

:- pred gen_lists(int, int, io__state, io__state).
:- mode gen_lists(in, out, di, uo) is det.

:- pred delete_lists(int, int, io__state, io__state).
:- mode delete_lists(in, in, di, uo) is det.

%------------------------------------------------------------------------------%
%
% 5.5 Flush and Finish
%
%------------------------------------------------------------------------------%

:- pred flush(io__state, io__state).
:- mode flush(di, uo) is det.

:- pred finish(io__state, io__state).
:- mode finish(di, uo) is det.

%------------------------------------------------------------------------------%
%
% Enable/Disable
%
%------------------------------------------------------------------------------%

:- type	control_flag
		--->	normalize		% 2.10.3
		;	clip_plane(int)		% 2.11
		;	lighting		% 2.13.1
		;	light(int)		% 2.13.2
		;	color_material		% 2.13.3
		;	line_stipple		% 3.4.2
		;	cull_face		% 3.5.1
		;	polygon_stipple		% 3.5.2
		;	polygon_offset_point	% 3.5.5
		;	polygon_offset_line	% 3.5.5
		;	polygon_offset_fill	% 3.5.5
		;	fog			% 3.9
		;	scissor_test		% 4.1.2
		;	alpha_test		% 4.1.3
		;	stencil_test		% 4.1.4
		;	depth_test		% 4.1.5
		;	blend			% 4.1.6
		;	dither			% 4.1.7
		;	index_logic_op		% 4.1.8
		;	color_logic_op		% 4.1.8
		.

:- pred enable(control_flag, io__state, io__state).
:- mode enable(in, di, uo) is det.

:- pred disable(control_flag, io__state, io__state).
:- mode disable(in, di, uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, int, float, require, std_util.

:- pragma c_header_code("
	#include <stdio.h>
	#include <math.h>
	#include <GL/gl.h>
").

%------------------------------------------------------------------------------%
%
% 2.5	GL Errors
%
%------------------------------------------------------------------------------%


:- func error_to_int(int::in) = (mogl__error::out) is semidet.

error_to_int(0)	= no_error.
error_to_int(1) = invalid_enum.
error_to_int(2) = invalid_value.
error_to_int(3) = invalid_operation.
error_to_int(4) = stack_overflow.
error_to_int(5) = stack_underflow.
error_to_int(6) = out_of_memory.

get_error(Err) -->
	get_error2(ErrNo),
	(
		{ Err0 = error_to_int(ErrNo) }
	->
		{ Err = Err0 }
	;
		{ error("GetError returned an unexpected value") }
	).

:- pred get_error2(int, io__state, io__state).
:- mode get_error2(out, di, uo) is det.

:- pragma c_code(get_error2(Err::out, IO0::di, IO::uo), "
{
	static GLenum	errcodes[] = {
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
% 2.6	Begin/End Objects
%
%------------------------------------------------------------------------------%

:- func block_mode_to_int(block_mode) = int.

block_mode_to_int(points)	  = 0.
block_mode_to_int(line_strip)	  = 1.
block_mode_to_int(line_loop)	  = 2.
block_mode_to_int(lines)	  = 3.
block_mode_to_int(polygon)	  = 4.
block_mode_to_int(triangle_strip) = 5.
block_mode_to_int(triangle_fan)	  = 6.
block_mode_to_int(triangles)	  = 7.
block_mode_to_int(quad_strip)	  = 8.
block_mode_to_int(quads)	  = 9.

:- pragma c_header_code("
	extern const GLenum block_mode_flags[];
").

:- pragma c_code("
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

begin(Blk) -->
	begin2(block_mode_to_int(Blk)).

:- pred begin2(int, io__state, io__state).
:- mode begin2(in, di, uo) is det.

:- pragma c_code(begin2(Mode::in, IO0::di, IO::uo), "
	glBegin(block_mode_flags[Mode]);
	IO = IO0;
").

:- pragma c_code(end(IO0::di, IO::uo), "
	glEnd();
	IO = IO0;
").

	% 2.6.2	Polygon Edges

edge_flag(no) -->
	edge_flag2(0).
edge_flag(yes) -->
	edge_flag2(1).

:- pred edge_flag2(int, io__state, io__state).
:- mode edge_flag2(in, di, uo) is det.

:- pragma c_code(edge_flag2(F::in, IO0::di, IO::uo), "
	glEdgeFlag((GLboolean) F);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 2.7	Vertex Specification
%
%------------------------------------------------------------------------------%

:- pragma c_code(vertex2(X::in, Y::in, IO0::di, IO::uo), "
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

:- pragma c_code(vertex3(X::in, Y::in, Z::in, IO0::di, IO::uo), "
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

:- pragma c_code(vertex4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), "
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

:- pragma c_code(tex_coord1(X::in, IO0::di, IO::uo), "
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

:- pragma c_code(tex_coord2(X::in, Y::in, IO0::di, IO::uo), "
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

:- pragma c_code(tex_coord3(X::in, Y::in, Z::in, IO0::di, IO::uo), "
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

:- pragma c_code(tex_coord4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), "
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

:- pragma c_code(normal3(X::in, Y::in, Z::in, IO0::di, IO::uo), "
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

:- pragma c_code(color3(R::in, G::in, B::in, IO0::di, IO::uo), "
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

:- pragma c_code(color4(R::in, G::in, B::in, A::in, IO0::di, IO::uo), "
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
% 2.10	Coordinate Transformations
%
%------------------------------------------------------------------------------%

:- pragma c_code(depth_range(Near::in, Far::in, IO0::di, IO::uo), "
	glDepthRange((GLclampd) Near, (GLclampd) Far);
	IO = IO0;
").

:- pragma c_code(viewport(X::in, Y::in, Wdth::in, Hght::in, IO0::di, IO::uo), "
	glViewport((GLint) X, (GLint) Y, (GLsizei) Wdth, (GLsizei) Hght);
	IO = IO0;
").

%------------------------------------------------------------------------------%

:- func matrix_mode_to_int(matrix_mode) = int.

matrix_mode_to_int(texture)	= 0.
matrix_mode_to_int(modelview)	= 1.
matrix_mode_to_int(projection)	= 2.

:- pragma c_header_code("
	extern const GLenum	matrix_mode_flags[];
").

:- pragma c_code("
	const GLenum	matrix_mode_flags[] = {
		GL_TEXTURE,
		GL_MODELVIEW,
		GL_PROJECTION
	};
").

matrix_mode(Mode) -->
	matrix_mode2(matrix_mode_to_int(Mode)).

:- pred matrix_mode2(int, io__state, io__state).
:- mode matrix_mode2(in, di, uo) is det.

:- pragma c_code(matrix_mode2(I::in, IO0::di, IO::uo), "
	glMatrixMode(matrix_mode_flags[I]);
	IO = IO0;
").

load_matrix(Matrix) -->
	{ Matrix = m(
		A1, A5, A9, A13,
		A2, A6, A10, A14,
		A3, A7, A11, A15,
		A4, A8, A12, A16
	) },
	load_matrix2(A1, A2, A3, A4, A5, A6, A7, A8,
		A9, A10, A11, A12, A13, A14, A15, A16).

:- pred load_matrix2(
		float, float, float, float,
		float, float, float, float,
		float, float, float, float,
		float, float, float, float, io__state, io__state).
:- mode load_matrix2(
		in, in, in, in,
		in, in, in, in,
		in, in, in, in,
		in, in, in, in, di, uo) is det.

:- pragma c_code(
	load_matrix2(A1::in, A2::in, A3::in, A4::in,
		A5::in, A6::in, A7::in, A8::in,
		A9::in, A10::in, A11::in, A12::in,
		A13::in, A14::in, A15::in, A16::in, IO0::di, IO::uo), "
	if (sizeof(Float) == sizeof(GLfloat))
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

mult_matrix(Matrix) -->
	{ Matrix = m(
		A1, A5, A9, A13,
		A2, A6, A10, A14,
		A3, A7, A11, A15,
		A4, A8, A12, A16
	) },
	mult_matrix2(A1, A2, A3, A4, A5, A6, A7, A8,
		A9, A10, A11, A12, A13, A14, A15, A16).

:- pred mult_matrix2(
		float, float, float, float,
		float, float, float, float,
		float, float, float, float,
		float, float, float, float, io__state, io__state).
:- mode mult_matrix2(
		in, in, in, in,
		in, in, in, in,
		in, in, in, in,
		in, in, in, in, di, uo) is det.

:- pragma c_code(
	mult_matrix2(A1::in, A2::in, A3::in, A4::in,
		A5::in, A6::in, A7::in, A8::in,
		A9::in, A10::in, A11::in, A12::in,
		A13::in, A14::in, A15::in, A16::in, IO0::di, IO::uo), "
	if (sizeof(Float) == sizeof(GLfloat))
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

:- pragma c_code(load_identity(IO0::di, IO::uo), "
	glLoadIdentity();
	IO = IO0;
").

:- pragma c_code(rotate(Theta::in, X::in, Y::in, Z::in, IO0::di, IO::uo), "
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

:- pragma c_code(translate(X::in, Y::in, Z::in, IO0::di, IO::uo), "
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glTranslatef((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glTranslated((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma c_code(scale(X::in, Y::in, Z::in, IO0::di, IO::uo), "
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glScalef((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glScaled((GLdouble) X, (GLdouble) Y, (GLdouble) Z);
	}
	IO = IO0;
").

:- pragma c_code(frustum(L::in, R::in, B::in, T::in, N::in, F::in,
		IO0::di, IO::uo), "
	glFrustum((GLdouble) L, (GLdouble) R, (GLdouble) B,
		(GLdouble) T, (GLdouble) N, (GLdouble) F);
	IO = IO0;
").

:- pragma c_code(ortho(L::in, R::in, B::in, T::in, N::in, F::in,
		IO0::di, IO::uo), "
	glOrtho((GLdouble) L, (GLdouble) R, (GLdouble) B,
		(GLdouble) T, (GLdouble) N, (GLdouble) F);
	IO = IO0;
").

:- pragma c_code(push_matrix(IO0::di, IO::uo), "
	glPushMatrix();
	IO = IO0;
").

:- pragma c_code(pop_matrix(IO0::di, IO::uo), "
	glPopMatrix();
	IO = IO0;
").

:- func texture_coord_to_int(texture_coord) = int.

texture_coord_to_int(s)	= 0.
texture_coord_to_int(t)	= 1.
texture_coord_to_int(r)	= 2.
texture_coord_to_int(q)	= 3.

:- pragma c_header_code("
	extern const GLenum	texture_coord_flags[];
").

:- pragma c_code("
	const GLenum	texture_coord_flags[] = {
		GL_S,
		GL_T,
		GL_R,
		GL_Q
	};
").

:- func texture_gen_parameter_to_int(texture_gen_parameter) = int.

texture_gen_parameter_to_int(object_linear)	= 0.
texture_gen_parameter_to_int(eye_linear)	= 1.
texture_gen_parameter_to_int(sphere_map)	= 2.

:- pragma c_header_code("
	extern const GLenum	texture_gen_flags[];
").

:- pragma c_code("
	const GLenum	texture_gen_flags[] = {
		GL_OBJECT_LINEAR,
		GL_EYE_LINEAR,
		GL_SPHERE_MAP
	};
").

tex_gen(Coord, texture_gen_mode(Param)) -->
	tex_genf2a(texture_coord_to_int(Coord),
		texture_gen_parameter_to_int(Param)).
tex_gen(Coord, object_plane(Param)) -->
	tex_genf2b(texture_coord_to_int(Coord), Param).
tex_gen(Coord, eye_plane(Param)) -->
	tex_genf2c(texture_coord_to_int(Coord), Param).

:- pred tex_genf2a(int, int, io__state, io__state).
:- mode tex_genf2a(in, in, di, uo) is det.

:- pragma c_code(tex_genf2a(Coord::in, Param::in, IO0::di, IO::uo), "
	glTexGeni(texture_coord_flags[Coord], GL_TEXTURE_GEN_MODE,
		texture_gen_flags[Param]);
	IO = IO0;
").

:- pred tex_genf2b(int, float, io__state, io__state).
:- mode tex_genf2b(in, in, di, uo) is det.

:- pragma c_code(tex_genf2b(Coord::in, Param::in, IO0::di, IO::uo), "
	glTexGend(texture_coord_flags[Coord], GL_OBJECT_PLANE, (GLdouble) Param);
	IO = IO0;
").

:- pred tex_genf2c(int, float, io__state, io__state).
:- mode tex_genf2c(in, in, di, uo) is det.

:- pragma c_code(tex_genf2c(Coord::in, Param::in, IO0::di, IO::uo), "
	glTexGend(texture_coord_flags[Coord], GL_EYE_PLANE, (GLdouble) Param);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 2.11	Clipping
%
%------------------------------------------------------------------------------%

clip_plane(Num, clip(X, Y, Z, W)) -->
	clip_plane2(Num, X, Y, Z, W).

:- pred clip_plane2(int, float, float, float, float, io__state, io__state).
:- mode clip_plane2(in, in, in, in, in, di, uo) is det.

:- pragma c_code(clip_plane2(I::in, X::in, Y::in, Z::in, W::in,
		IO0::di, IO::uo), "
{
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
% 2.12	Current Raster Position
%
%------------------------------------------------------------------------------%

:- pragma c_code(raster_pos2(X::in, Y::in, IO0::di, IO::uo), "
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRasterPos2f((GLfloat) X, (GLfloat) Y);
	} else {
		glRasterPos2d((GLdouble) X, (GLdouble) Y);
	}
	IO = IO0;
").

:- pragma c_code(raster_pos3(X::in, Y::in, Z::in, IO0::di, IO::uo), "
	if(sizeof(MR_Float) == sizeof(GLfloat))
	{
		glRasterPos3f((GLfloat) X, (GLfloat) Y, (GLfloat) Z);
	} else {
		glRasterPos3d((GLdouble) X, (GLdouble) Y, (GLfloat) Z);
	}
	IO = IO0;
").

:- pragma c_code(raster_pos4(X::in, Y::in, Z::in, W::in, IO0::di, IO::uo), "
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
% 2.13	Colors and Coloring
%
%------------------------------------------------------------------------------%

:- func face_direction_to_int(face_direction) = int.

face_direction_to_int(cw)	= 0.
face_direction_to_int(ccw)	= 1.

:- pragma c_header_code("
	extern	const GLenum	face_direction_flags[];
").

:- pragma c_code("
	const GLenum	face_direction_flags[] = {
		GL_CW,
		GL_CCW
	};
").

:- func face_side_to_int(face_side)	= int.

face_side_to_int(front)		 = 0.
face_side_to_int(back)		 = 1.
face_side_to_int(front_and_back) = 2.

:- pragma c_header_code("
	extern	const GLenum	face_side_flags[];
").

:- pragma c_code("
	const GLenum	face_side_flags[] = {
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

:- pragma c_header_code("
	extern	const GLenum	color_material_mode_flags[];
").

:- pragma c_code("
	const GLenum	color_material_mode_flags[] = {
		GL_AMBIENT,
		GL_DIFFUSE,
		GL_AMBIENT_AND_DIFFUSE,
		GL_SPECULAR,
		GL_EMISSION
	};
").

:- func shade_model_to_int(shade_model)	= int.

shade_model_to_int(smooth)	= 0.
shade_model_to_int(flat)	= 1.

:- pragma c_header_code("
	extern GLenum	shade_model_flags[];
").

:- pragma c_code("
	GLenum	shade_model_flags[] = {
		GL_SMOOTH,
		GL_FLAT
	};
").

front_face(Face) -->
	front_face2(face_direction_to_int(Face)).

:- pred front_face2(int, io__state, io__state).
:- mode front_face2(in, di, uo) is det.

:- pragma c_code(front_face2(F::in, IO0::di, IO::uo), "
	glFrontFace(face_direction_flags[F]);
	IO = IO0;
").

material(Face, ambient(R, G, B, A)) -->
	material_ambient(face_side_to_int(Face), R, G, B, A).
material(Face, diffuse(R, G, B, A)) -->
	material_diffuse(face_side_to_int(Face), R, G, B, A).
material(Face, ambient_and_diffuse(R, G, B, A)) -->
	material_ambient_and_diffuse(face_side_to_int(Face), R, G, B, A).
material(Face, specular(R, G, B, A)) -->
	material_specular(face_side_to_int(Face), R, G, B, A).
material(Face, emission(R, G, B, A)) -->
	material_emission(face_side_to_int(Face), R, G, B, A).
material(Face, shininess(S)) -->
	material_shininess(face_side_to_int(Face), S).
material(Face, color_indexes(R, G, B)) -->
	material_color_indexes(face_side_to_int(Face), R, G, B).

:- pred material_ambient(int, float, float, float, float, io__state, io__state).
:- mode material_ambient(in, in, in, in, in, di, uo) is det.

:- pragma c_code(material_ambient(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_AMBIENT, params);
	IO = IO0;
}").

:- pred material_diffuse(int, float, float, float, float, io__state, io__state).
:- mode material_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma c_code(material_diffuse(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_DIFFUSE, params);
	IO = IO0;
}").

:- pred material_ambient_and_diffuse(int, float, float, float, float,
		io__state, io__state).
:- mode material_ambient_and_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma c_code(material_ambient_and_diffuse(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_AMBIENT_AND_DIFFUSE, params);
	IO = IO0;
}").

:- pred material_specular(int, float, float, float, float,
		io__state, io__state).
:- mode material_specular(in, in, in, in, in, di, uo) is det.

:- pragma c_code(material_specular(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_SPECULAR, params);
	IO = IO0;
}").

:- pred material_emission(int, float, float, float, float,
		io__state, io__state).
:- mode material_emission(in, in, in, in, in, di, uo) is det.

:- pragma c_code(material_emission(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glMaterialfv(face_side_flags[F], GL_EMISSION, params);
	IO = IO0;
}").

:- pred material_shininess(int, float, io__state, io__state).
:- mode material_shininess(in, in, di, uo) is det.

:- pragma c_code(material_shininess(F::in, S::in, IO0::di, IO::uo), "
{
	glMaterialf(face_side_flags[F], GL_SHININESS, (GLfloat) S);
	IO = IO0;
}").

:- pred material_color_indexes(int, float, float, float,
		io__state, io__state).
:- mode material_color_indexes(in, in, in, in, di, uo) is det.

:- pragma c_code(material_color_indexes(F::in, R::in, G::in, B::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[3];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	glMaterialfv(face_side_flags[F], GL_COLOR_INDEXES, params);
	IO = IO0;
}").

light(Num, ambient(R, G, B, A)) -->
	light_ambient(Num, R, G, B, A).
light(Num, diffuse(R, G, B, A)) -->
	light_diffuse(Num, R, G, B, A).
light(Num, specular(R, G, B, A)) -->
	light_specular(Num, R, G, B, A).
light(Num, position(X, Y, Z, W)) -->
	light_position(Num, X, Y, Z, W).
light(Num, spot_direction(I, J, K)) -->
	light_spot_direction(Num, I, J, K).
light(Num, spot_exponent(K)) -->
	light_spot_exponent(Num, K).
light(Num, spot_cutoff(K)) -->
	light_spot_cutoff(Num, K).
light(Num, constant_attenuation(K)) -->
	light_constant_attenuation(Num, K).
light(Num, linear_attenuation(K)) -->
	light_linear_attenuation(Num, K).
light(Num, quadratic_attenuation(K)) -->
	light_quadratic_attenuation(Num, K).

:- pred light_ambient(int, float, float, float, float, io__state, io__state).
:- mode light_ambient(in, in, in, in, in, di, uo) is det.

:- pragma c_code(light_ambient(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_AMBIENT, params);
	IO = IO0;
}").

:- pred light_diffuse(int, float, float, float, float, io__state, io__state).
:- mode light_diffuse(in, in, in, in, in, di, uo) is det.

:- pragma c_code(light_diffuse(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_DIFFUSE, params);
	IO = IO0;
}").

:- pred light_specular(int, float, float, float, float, io__state, io__state).
:- mode light_specular(in, in, in, in, in, di, uo) is det.

:- pragma c_code(light_specular(F::in, R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightfv(F + GL_LIGHT0, GL_SPECULAR, params);
	IO = IO0;
}").

:- pred light_position(int, float, float, float, float, io__state, io__state).
:- mode light_position(in, in, in, in, in, di, uo) is det.

:- pragma c_code(light_position(F::in, X::in, Y::in, Z::in, W::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[4];

	params[0] = (GLfloat) X;
	params[1] = (GLfloat) Y;
	params[2] = (GLfloat) Z;
	params[3] = (GLfloat) W;
	glLightfv(F + GL_LIGHT0, GL_POSITION, params);
	IO = IO0;
}").

:- pred light_spot_direction(int, float, float, float, io__state, io__state).
:- mode light_spot_direction(in, in, in, in, di, uo) is det.

:- pragma c_code(light_spot_direction(F::in, I::in, J::in, K::in,
		IO0::di, IO::uo), "
{
	GLfloat	params[3];

	params[0] = (GLfloat) I;
	params[1] = (GLfloat) J;
	params[2] = (GLfloat) K;
	glLightfv(F + GL_LIGHT0, GL_SPOT_DIRECTION, params);
	IO = IO0;
}").

:- pred light_spot_exponent(int, float, io__state, io__state).
:- mode light_spot_exponent(in, in, di, uo) is det.

:- pragma c_code(light_spot_exponent(F::in, E::in, IO0::di, IO::uo), "
{
	glLightf(F + GL_LIGHT0, GL_SPOT_EXPONENT, (GLfloat) E);
	IO = IO0;
}").

:- pred light_spot_cutoff(int, float, io__state, io__state).
:- mode light_spot_cutoff(in, in, di, uo) is det.

:- pragma c_code(light_spot_cutoff(F::in, E::in, IO0::di, IO::uo), "
{
	glLightf(F + GL_LIGHT0, GL_SPOT_CUTOFF, (GLfloat) E);
	IO = IO0;
}").

:- pred light_constant_attenuation(int, float, io__state, io__state).
:- mode light_constant_attenuation(in, in, di, uo) is det.

:- pragma c_code(light_constant_attenuation(F::in, E::in, IO0::di, IO::uo), "
{
	glLightf(F + GL_LIGHT0, GL_CONSTANT_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- pred light_linear_attenuation(int, float, io__state, io__state).
:- mode light_linear_attenuation(in, in, di, uo) is det.

:- pragma c_code(light_linear_attenuation(F::in, E::in, IO0::di, IO::uo), "
{
	glLightf(F + GL_LIGHT0, GL_LINEAR_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- pred light_quadratic_attenuation(int, float, io__state, io__state).
:- mode light_quadratic_attenuation(in, in, di, uo) is det.

:- pragma c_code(light_quadratic_attenuation(F::in, E::in, IO0::di, IO::uo), "
{
	glLightf(F + GL_LIGHT0, GL_QUADRATIC_ATTENUATION, (GLfloat) E);
	IO = IO0;
}").

:- func bool_to_int(bool) = int.

bool_to_int(no) = 0.
bool_to_int(yes) = 1.

light_model(light_model_ambient(R, G, B, A)) -->
	light_model_ambient(R, G, B, A).
light_model(light_model_local_viewer(Bool)) -->
	light_model_local_viewer(bool_to_int(Bool)).
light_model(light_model_two_side(Bool)) -->
	light_model_two_side(bool_to_int(Bool)).

:- pred light_model_ambient(float, float, float, float, io__state, io__state).
:- mode light_model_ambient(in, in, in, in, di, uo) is det.

:- pragma c_code(light_model_ambient(R::in, G::in, B::in, A::in,
		IO0::di, IO::uo), "
{
	GLfloat params[4];

	params[0] = (GLfloat) R;
	params[1] = (GLfloat) G;
	params[2] = (GLfloat) B;
	params[3] = (GLfloat) A;
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, params);
	IO = IO0;
}").

:- pred light_model_local_viewer(int, io__state, io__state).
:- mode light_model_local_viewer(in, di, uo) is det.

:- pragma c_code(light_model_local_viewer(F::in, IO0::di, IO::uo), "
{
	glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, (GLint) F);
	IO = IO0;
}").

:- pred light_model_two_side(int, io__state, io__state).
:- mode light_model_two_side(in, di, uo) is det.

:- pragma c_code(light_model_two_side(F::in, IO0::di, IO::uo), "
{
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, (GLint) F);
	IO = IO0;
}").

color_material(Face, Mode) -->
	color_material2(face_side_to_int(Face),
		color_material_mode_to_int(Mode)).

:- pred color_material2(int, int, io__state, io__state).
:- mode color_material2(in, in, di, uo) is det.

:- pragma c_code(color_material2(Face::in, Mode::in, IO0::di, IO::uo), "
	glColorMaterial(face_side_flags[Face], color_material_mode_flags[Mode]);
	IO = IO0;
").

shade_model(Model) -->
	shade_model2(shade_model_to_int(Model)).

:- pred shade_model2(int, io__state, io__state).
:- mode shade_model2(in, di, uo) is det.

:- pragma c_code(shade_model2(Model::in, IO0::di, IO::uo), "
	glShadeModel(shade_model_flags[Model]);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 3.3	Points
%
%------------------------------------------------------------------------------%

:- pragma c_code(point_size(Size::in, IO0::di, IO::uo), "
	glPointSize((GLfloat) Size);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 3.4	Lines Segments
%
%------------------------------------------------------------------------------%

:- pragma c_code(line_width(Size::in, IO0::di, IO::uo), "
	glLineWidth((GLfloat) Size);
	IO = IO0;
").

:- pragma c_code(line_stipple(Fac::in, Pat::in, IO0::di, IO::uo), "
	glLineStipple((GLint) Fac, (GLushort) Pat);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 3.5	Polygons
%
%------------------------------------------------------------------------------%

:- func polygon_mode_to_int(polygon_mode) = int.

polygon_mode_to_int(point)	= 0.
polygon_mode_to_int(line)	= 1.
polygon_mode_to_int(fill)	= 2.

:- pragma c_header_code("
	extern const GLenum	polygon_mode_flags[];
").

:- pragma c_code("
	const GLenum	polygon_mode_flags[] = {
		GL_POINT,
		GL_LINE,
		GL_FILL
	};
").

cull_face(Face) -->
	cull_face2(face_side_to_int(Face)).

:- pred cull_face2(int, io__state, io__state).
:- mode cull_face2(in, di, uo) is det.

:- pragma c_code(cull_face2(F::in, IO0::di, IO::uo), "
	glCullFace(face_side_flags[F]);
	IO = IO0;
").

%:- pred polygon_stipple(polygon_stipple, io__state, io__state).
%:- mode polygon_stipple(in, di, uo) is det.

polygon_stipple(_) -->
		% Avoid a determinism warning
	( { semidet_succeed } ->
		{ error("sorry, polygon_stipple uniplemented") }
	;
		[]
	).

polygon_mode(Face, Mode) -->
	polygon_mode2(face_side_to_int(Face), polygon_mode_to_int(Mode)).

:- pred polygon_mode2(int, int, io__state, io__state).
:- mode polygon_mode2(in, in, di, uo) is det.

:- pragma c_code(polygon_mode2(Face::in, Mode::in, IO0::di, IO::uo), "
	glPolygonMode(face_side_flags[Face], polygon_mode_flags[Mode]);
	IO = IO0;
").

:- pragma c_code(polygon_offset(Fac::in, Units::in, IO0::di, IO::uo), "
	glPolygonOffset((GLfloat) Fac, (GLfloat) Units);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 3.6	Pixel Rectangles
%
%------------------------------------------------------------------------------%

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
% 3.7	Bitmaps
%
%------------------------------------------------------------------------------%

/*
:- pred bitmap(int, int, float, float, float, float, list(int),
		io__state, io__state).
:- mode bitmap(in, in, in, in, in, in, in, di, uo) is det.
*/

%------------------------------------------------------------------------------%
%
% 3.8	Texturing
%
%------------------------------------------------------------------------------%

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
% 3.9	Fog
%
%------------------------------------------------------------------------------%

:- func fog_mode_to_int(fog_mode) = int.

fog_mode_to_int(linear)	= 0.
fog_mode_to_int(exp)	= 1.
fog_mode_to_int(exp2)	= 2.

:- pragma c_header_code("
	extern const GLenum fog_mode_flags[];
").

:- pragma c_code("
	const GLenum fog_mode_flags[] = {
		GL_LINEAR,
		GL_EXP,
		GL_EXP2
	};
").

fog(fog_mode(Mode)) -->
	fog_mode(fog_mode_to_int(Mode)).
fog(fog_density(Density)) -->
	fog_density(Density).
fog(fog_start(Start)) -->
	fog_start(Start).
fog(fog_end(End)) -->
	fog_end(End).

:- pred fog_mode(int, io__state, io__state).
:- mode fog_mode(in, di, uo) is det.

:- pragma c_code(fog_mode(M::in, IO0::di, IO::uo), "
	glFogi(GL_FOG_MODE, (GLint) fog_mode_flags[M]);
	IO = IO0;
").

:- pred fog_density(float, io__state, io__state).
:- mode fog_density(in, di, uo) is det.

:- pragma c_code(fog_density(P::in, IO0::di, IO::uo), "
	glFogf(GL_FOG_DENSITY, (GLfloat) P);
	IO = IO0;
").

:- pred fog_start(float, io__state, io__state).
:- mode fog_start(in, di, uo) is det.

:- pragma c_code(fog_start(P::in, IO0::di, IO::uo), "
	glFogf(GL_FOG_START, (GLfloat) P);
	IO = IO0;
").

:- pred fog_end(float, io__state, io__state).
:- mode fog_end(in, di, uo) is det.

:- pragma c_code(fog_end(P::in, IO0::di, IO::uo), "
	glFogf(GL_FOG_END, (GLfloat) P);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 4.1	Per-Fragment Operations
%
%------------------------------------------------------------------------------%

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
% 4.2	Whole Framebuffer Operations
%
%------------------------------------------------------------------------------%

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
buffer_to_int(front_and_back)	= 9.
buffer_to_int(aux(I))		= 10 + I.

:- pragma c_header_code("
	extern const GLenum buffer_flags[];
").

:- pragma c_code("
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

draw_buffer(Buffer) -->
	draw_buffer2(buffer_to_int(Buffer)).

:- pred draw_buffer2(int, io__state, io__state).
:- mode draw_buffer2(in, di, uo) is det.

:- pragma c_code(draw_buffer2(B::in, IO0::di, IO::uo), "
	glDrawBuffer(buffer_flags[B]);
	IO = IO0;
").

:- pragma c_code(index_mask(I::in, IO0::di, IO::uo), "
	glIndexMask((GLuint) I);
	IO = IO0;
").

color_mask(A, B, C, D) -->
	color_mask2(bool_to_int(A), bool_to_int(B),
		bool_to_int(C), bool_to_int(D)).

:- pred color_mask2(int, int, int, int, io__state, io__state).
:- mode color_mask2(in, in, in, in, di, uo) is det.

:- pragma c_code(color_mask2(A::in, B::in, C::in, D::in, IO0::di, IO::uo), "
	glColorMask((GLboolean) A, (GLboolean) B, (GLboolean) C, (GLboolean) D);
	IO = IO0;
").

depth_mask(Bool) -->
	depth_mask2(bool_to_int(Bool)).

:- pred depth_mask2(int, io__state, io__state).
:- mode depth_mask2(in, di, uo) is det.

:- pragma c_code(depth_mask2(M::in, IO0::di, IO::uo), "
	glDepthMask((GLboolean) M);
	IO = IO0;
").

:- pragma c_code(stencil_mask(M::in, IO0::di, IO::uo), "
	glStencilMask((GLuint) M);
	IO = IO0;
").


clear(BitList) -->
	{ make_mask(BitList, 0, Mask) },
	clear2(Mask).

:- pred make_mask(list(buffer_bit), int, int).
:- mode make_mask(in, in, out) is det.

make_mask([], Acc, Acc).
make_mask([Flag|Flags], Acc0, Acc) :-
	make_mask(Flags, Acc0 \/ buffer_bit_to_bit(Flag), Acc).

:- func buffer_bit_to_bit(buffer_bit) = int.

buffer_bit_to_bit(Flag) = lookup_buffer_bit(buffer_bit_to_int(Flag)).

:- func buffer_bit_to_int(buffer_bit) = int.

buffer_bit_to_int(color)	= 0.
buffer_bit_to_int(depth)	= 1.
buffer_bit_to_int(stencil)	= 2.
buffer_bit_to_int(accum)	= 3.

:- func lookup_buffer_bit(int)	= int.

:- pragma c_code(lookup_buffer_bit(F::in) = (B::out),"
{
	static GLbitfield a[] = {
		GL_COLOR_BUFFER_BIT,
		GL_DEPTH_BUFFER_BIT,
		GL_STENCIL_BUFFER_BIT,
		GL_ACCUM_BUFFER_BIT
	};

	B = a[F];
}").

:- pred clear2(int::in, io__state::di, io__state::uo) is det.

:- pragma c_code(clear2(Mask::in, IO0::di, IO::uo), "
	glClear(Mask);
	IO = IO0;
").

:- pragma c_code(clear_color(R::in, G::in, B::in, A::in, IO0::di, IO::uo), "
	glClearColor((GLclampf) R, (GLclampf) G, (GLclampf) B, (GLclampf) A);
	IO = IO0;
").

:- pragma c_code(clear_index(I::in, IO0::di, IO::uo), "
	glClearIndex((GLfloat) I);
	IO = IO0;
").

:- pragma c_code(clear_depth(I::in, IO0::di, IO::uo), "
	glClearDepth((GLfloat) I);
	IO = IO0;
").

:- pragma c_code(clear_stencil(I::in, IO0::di, IO::uo), "
	glClearStencil((GLint) I);
	IO = IO0;
").

:- pragma c_code(clear_accum(R::in, G::in, B::in, A::in, IO0::di, IO::uo), "
	glClearAccum((GLfloat) R, (GLfloat) G, (GLfloat) B, (GLfloat) A);
	IO = IO0;
").

:- func accum_op_to_int(accum_op) = int.

accum_op_to_int(accum)	= 0.
accum_op_to_int(load)	= 1.
accum_op_to_int(return)	= 2.
accum_op_to_int(mult)	= 3.
accum_op_to_int(add)	= 4.

:- pragma c_header_code("
	extern const GLenum accum_op_flags[];
").

:- pragma c_code("
	const GLenum accum_op_flags[] = {
		GL_ACCUM,
		GL_LOAD,
		GL_RETURN,
		GL_MULT,
		GL_ADD
	};
").

accum(Op, Param) -->
	accum2(accum_op_to_int(Op), Param).

:- pred accum2(int, float, io__state, io__state).
:- mode accum2(in, in, di, uo) is det.

:- pragma c_code(accum2(Op::in, Param::in, IO0::di, IO::uo), "
	glAccum(accum_op_flags[Op], Param);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 5.1 Evaluators
%
%------------------------------------------------------------------------------%

% Evalutators not implemented

%------------------------------------------------------------------------------%
%
% 5.2 Selection
%
%------------------------------------------------------------------------------%

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
% 5.4 Display Lists
%
%------------------------------------------------------------------------------%

:- func display_list_mode_to_int(display_list_mode) = int.

display_list_mode_to_int(compile)		= 0.
display_list_mode_to_int(compile_and_execute)	= 1.

:- pragma c_header_code("
	extern const GLenum	display_list_mode_flags[];
").

:- pragma c_code("
	const GLenum	display_list_mode_flags[] ={
		GL_COMPILE,
		GL_COMPILE_AND_EXECUTE
	};
").

new_list(Num, Mode) -->
	new_list2(Num, display_list_mode_to_int(Mode)).

:- pred new_list2(int, int, io__state, io__state).
:- mode new_list2(in, in, di, uo) is det.

:- pragma c_code(new_list2(N::in, M::in, IO0::di, IO::uo), "
	glNewList((GLuint) N, display_list_mode_flags[M]);
	IO = IO0;
").

:- pragma c_code(end_list(IO0::di, IO::uo), "
	glEndList();
	IO = IO0;
").

:- pragma c_code(call_list(N::in, IO0::di, IO::uo), "
	glCallList((GLuint) N);
	IO = IO0;
").

:- pragma c_code(gen_lists(N::in, M::out, IO0::di, IO::uo), "
	M = (Integer) glGenLists((GLsizei) N);
	IO = IO0;
").

:- pragma c_code(delete_lists(N::in, M::in, IO0::di, IO::uo), "
	glDeleteLists((GLuint) N, (GLsizei) M);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%
% 5.5 Flush and Finish
%
%------------------------------------------------------------------------------%

:- pragma c_code(flush(IO0::di, IO::uo), "
	glFlush();
	IO = IO0;
	assert(glGetError() == GL_NO_ERROR);
").

:- pragma c_code(finish(IO0::di, IO::uo), "
	glFinish();
	IO = IO0;
	assert(glGetError() == GL_NO_ERROR);
").

%------------------------------------------------------------------------------%
%
% Enable/Disable
%
%------------------------------------------------------------------------------%

:- func control_flag_to_int(control_flag) = int.
control_flag_to_int(normalize)		= 0.
control_flag_to_int(clip_plane(_))	= 1.
control_flag_to_int(lighting)		= 2.
control_flag_to_int(light(_))		= 3.
control_flag_to_int(color_material)	= 4.
control_flag_to_int(line_stipple)	= 5.
control_flag_to_int(cull_face)		= 6.
control_flag_to_int(polygon_stipple)	= 7.
control_flag_to_int(polygon_offset_point) = 8.
control_flag_to_int(polygon_offset_line)= 9.
control_flag_to_int(polygon_offset_fill)= 10.
control_flag_to_int(fog)		= 11.
control_flag_to_int(scissor_test)	= 12.
control_flag_to_int(alpha_test)		= 13.
control_flag_to_int(stencil_test)	= 14.
control_flag_to_int(depth_test)		= 15.
control_flag_to_int(blend)		= 16.
control_flag_to_int(dither)		= 17.
control_flag_to_int(index_logic_op)	= 18.
control_flag_to_int(color_logic_op)	= 19.

:- pragma c_header_code("
	extern const GLenum control_flag_flags[];
").

:- pragma c_code("
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

enable(Flag) -->
	( { Flag = clip_plane(I) } ->
		enable3(control_flag_to_int(Flag), I)
	; { Flag = light(I) } ->
		enable3(control_flag_to_int(Flag), I)
	;
		enable2(control_flag_to_int(Flag))
	).

:- pred enable2(int, io__state, io__state).
:- mode enable2(in, di, uo) is det.

:- pragma c_code(enable2(I::in, IO0::di, IO::uo), "
	glEnable(control_flag_flags[I]);
	IO = IO0;
").

:- pred enable3(int, int, io__state, io__state).
:- mode enable3(in, in, di, uo) is det.

:- pragma c_code(enable3(I::in, J::in, IO0::di, IO::uo), "
	glEnable(control_flag_flags[I]+J);
	IO = IO0;
").

disable(Flag) -->
	( { Flag = clip_plane(I) } ->
		disable3(control_flag_to_int(Flag), I)
	; { Flag = light(I) } ->
		disable3(control_flag_to_int(Flag), I)
	;
		disable2(control_flag_to_int(Flag))
	).

:- pred disable2(int, io__state, io__state).
:- mode disable2(in, di, uo) is det.

:- pragma c_code(disable2(I::in, IO0::di, IO::uo), "
	glDisable(control_flag_flags[I]);
	IO = IO0;
").

:- pred disable3(int, int, io__state, io__state).
:- mode disable3(in, in, di, uo) is det.

:- pragma c_code(disable3(I::in, J::in, IO0::di, IO::uo), "
	glDisable(control_flag_flags[I]+J);
	IO = IO0;
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
