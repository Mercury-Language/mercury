%-----------------------------------------------------------------------------%
% file: gears.m
% author: juliensf
%
% This program is public domain.
%
% This is a Mercury version of the of the gears demo that is supplied
% with Mesa.
%
% You should be able to find the original C versions (there are several)
% at <http://www.mesa3d.org>
%
%-----------------------------------------------------------------------------%

:- module gears.

:- interface.

:- import_module io.

:- pred gears.main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mogl, mglu.
:- import_module glut, glut.window, glut.callback.
:- import_module globals.

:- import_module char, float, int, list, math, string.

%-----------------------------------------------------------------------------%

main(!IO) :-
	io.command_line_arguments(Args, !IO),
	(
		(
			Args  = [],
			Limit = 0
		;
			Args  = [Limit0],
			string.to_int(Limit0, Limit)
		)
	->
		gears.main_2(Limit, !IO)
	;
		io.stderr_stream(StdErr, !IO),
		io.write_string(StdErr, "Usage: gears [<limit>]\n", !IO),
		io.set_exit_status(1, !IO)
	).

:- pred gears.main_2(int::in, io::di, io::uo) is det.

gears.main_2(Limit, !IO) :-
	glut.init(!IO),
	glut.init_display_mode([rgba, depth, double], !IO),
	glut.window.create("Gears", !IO),
	
	gears.init(Limit, !IO),

	glut.callback.display_func(gears.draw, !IO),
	glut.callback.reshape_func(gears.reshape, !IO),
	glut.callback.keyboard_func(gears.key, !IO),
	glut.callback.special_func(gears.special, !IO),
	glut.callback.visibility_func(gears.visible, !IO),
	glut.main_loop(!IO).

:- pred gears.gear(float::in, float::in, float::in, int::in, float::in,
	io::di, io::uo) is det.

gears.gear(InnerRadius, OuterRadius, Width, Teeth, ToothDepth, !IO) :-
	R0 = InnerRadius,
	R1 = OuterRadius - ToothDepth / 2.0,
	R2 = OuterRadius + ToothDepth / 2.0,

	Da = 2.0 * pi / float(Teeth) / 4.0, 

	mogl.shade_model(flat, !IO),
	mogl.normal3(0.0, 0.0, 1.0, !IO),

	gears.draw_front_face(R0, R1, Da, Width, Teeth, !IO),
	gears.draw_front_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO),
  	
	mogl.normal3(0.0, 0.0, -1.0, !IO),

	gears.draw_back_face(R0, R1, Da, Width, Teeth, !IO),
	gears.draw_back_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO),
	gears.draw_outward_faces_of_teeth(R1, R2, Da, Width, Teeth, !IO),
  	
	mogl.shade_model(smooth, !IO),
	
	gears.draw_inside_radius_cylinder(R0, Width, Teeth, !IO).

:- pred gears.draw_front_face(float::in, float::in, float::in, float::in, 
	int::in, io::di, io::uo) is det.

gears.draw_front_face(R0, R1, Da, Width, Teeth, !IO) :-
	mogl.begin(quad_strip, !IO),
	DrawFrontFace = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
			Width * 0.5, !IO),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle),
			Width * 0.5, !IO),
		( I < Teeth ->
			mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
				Width * 0.5, !IO),
    			mogl.vertex3(R1 * cos(Angle + 3.0 * Da), 
				R1 * sin(Angle + 3.0 * Da),
				Width * 0.5, !IO)
		;
			true
		)
	),
	int.fold_up(DrawFrontFace, 0, Teeth, !IO),
	mogl.end(!IO).
	
:- pred gears.draw_front_sides_of_teeth(float::in, float::in, float::in,
	float::in, int::in, io::di, io::uo) is det.
  
gears.draw_front_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
	mogl.begin(quads, !IO),
	DrawSides = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle), Width * 0.5, 
			!IO),
		mogl.vertex3(R2 * cos(Angle + Da), R2 * sin(Angle + Da),
			Width * 0.5, !IO),
		mogl.vertex3(R2 * cos(Angle + 2.0 * Da),
			R2 * sin(Angle + 2.0 * Da), Width * 0.5, !IO),
		mogl.vertex3(R1 * cos(Angle + 3.0 * Da), 
			R1 * sin(Angle + 3.0 * Da), Width * 0.5, !IO)
	),
	int.fold_up(DrawSides, 0, Teeth, !IO),
	mogl.end(!IO).

:- pred gears.draw_back_face(float::in, float::in, float::in, float::in,
	int::in, io::di, io::uo) is det.

gears.draw_back_face(R0, R1, Da, Width, Teeth, !IO) :-
 	mogl.begin(quad_strip, !IO),
	DrawBackFace = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle),
			-Width * 0.5, !IO),
		mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
			-Width * 0.5, !IO),
		mogl.vertex3(R1 * cos(Angle + 3.0 * Da), 
			R1 * sin(Angle + 3.0 * Da), -Width * 0.5, !IO),
		mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
			-Width * 0.5, !IO)
	),
	int.fold_up(DrawBackFace, 0, Teeth, !IO),
	mogl.end(!IO).

:- pred gears.draw_back_sides_of_teeth(float::in, float::in, float::in,
	float::in, int::in, io::di, io::uo) is det.

gears.draw_back_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
	mogl.begin(quads, !IO),
	DrawBackSidesOfTeeth = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.vertex3(R1 * cos(Angle + 3.0 * Da),
			R1 * sin(Angle + 3.0 * Da), -Width * 0.5, !IO),
		mogl.vertex3(R2 * cos(Angle + 2.0 * Da),
			R2 * sin(Angle + 2.0 * Da), -Width * 0.5, !IO),
		mogl.vertex3(R2 * cos(Angle + Da),
			R2 * sin(Angle + Da), -Width * 0.5, !IO),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle), 
			-Width * 0.5, !IO)
	),
	int.fold_up(DrawBackSidesOfTeeth, 0, Teeth, !IO),
	mogl.end(!IO).

:- pred gears.draw_outward_faces_of_teeth(float::in, float::in, float::in,
	float::in, int::in, io::di, io::uo) is det.

gears.draw_outward_faces_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
	mogl.begin(quad_strip, !IO),
	DrawOutwardFacesOfTeeth = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle), Width * 0.5,
			!IO),
		mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle), -Width * 0.5,
			!IO),
		U0 = R2 * cos(Angle + Da) - R1 * cos(Angle),
		V0 = R2 * sin(Angle + Da) - R1 * sin(Angle),
		Len = sqrt(U0 * U0 + V0 * V0),
		U1 = U0 / Len,
		V1 = V0 / Len,
		mogl.normal3(V1, -U1, 0.0, !IO),
		mogl.vertex3(R2 * cos(Angle + Da), R2 * sin(Angle + Da),
			Width * 0.5, !IO),
		mogl.vertex3(R2 * cos(Angle + Da), R2 * sin(Angle + Da),
			-Width * 0.5, !IO),
		mogl.normal3(cos(Angle), sin(Angle), 0.0, !IO),
		mogl.vertex3(R2 * cos(Angle + 2.0 * Da), 
			R2 * sin(Angle + 2.0 * Da), Width * 0.5, !IO),
		mogl.vertex3(R2 * cos(Angle + 2.0 * Da),
			R2 * sin(Angle + 2.0 * Da), -Width * 0.5, !IO),

		U = R1 * cos(Angle + 3.0 * Da) - R2 * cos(Angle + 2.0 * Da),
		V = R1 * sin(Angle + 3.0 * Da) - R2 * sin(Angle + 2.0 * Da),

		mogl.normal3(V, -U, 0.0, !IO),
		mogl.vertex3(R1 * cos(Angle + 3.0 * Da),
			R1 * sin(Angle + 3.0 * Da), Width * 0.5, !IO),
		mogl.vertex3(R1 * cos(Angle + 3.0 * Da),
			R1 * sin(Angle + 3.0 * Da), -Width * 0.5, !IO),

		mogl.normal3(cos(Angle), sin(Angle), 0.0, !IO)
	),
	int.fold_up(DrawOutwardFacesOfTeeth, 0, Teeth, !IO),
	mogl.vertex3(R1 * cos(0.0), R1 * sin(0.0),  Width * 0.5, !IO),
	mogl.vertex3(R1 * cos(0.0), R1 * sin(0.0), -Width * 0.5, !IO),
	mogl.end(!IO).


:- pred gears.draw_inside_radius_cylinder(float::in, float::in, int::in,
	io::di, io::uo) is det.

gears.draw_inside_radius_cylinder(R0, Width, Teeth, !IO) :-
	mogl.begin(quad_strip, !IO),
	DrawInside = (pred(I::in, !.IO::di, !:IO::uo) is det :-
		Angle = float(I) * 2.0 * pi / float(Teeth),
		mogl.normal3(-cos(Angle), -sin(Angle), 0.0, !IO),
		mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
			-Width * 0.5, !IO),
		mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
			Width * 0.5, !IO)
	),
	int.fold_up(DrawInside, 0, Teeth, !IO),
	mogl.end(!IO).

:- pred gears.draw(io::di, io::uo) is det.

gears.draw(!IO) :-
	globals.get("ViewRotX", ViewRotX, !IO),
	globals.get("ViewRotY", ViewRotY, !IO),
	globals.get("ViewRotZ", ViewRotZ, !IO),
	
	globals.get("Angle", Angle, !IO),
	globals.get("Count", Count0, !IO),
	globals.get("Limit", Limit, !IO),

	globals.get("GearOne", GearOne, !IO),
	globals.get("GearTwo", GearTwo, !IO),
	globals.get("GearThree", GearThree, !IO),
	
	globals.get("Frames", Frames0, !IO),
	globals.get("T0", T0, !IO),

	mogl.clear([color, depth], !IO),

	mogl.push_matrix(!IO),
	mogl.rotate(ViewRotX, 1.0, 0.0, 0.0, !IO),
	mogl.rotate(ViewRotY, 0.0, 1.0, 0.0, !IO),
	mogl.rotate(ViewRotZ, 0.0, 0.0, 1.0, !IO),

	mogl.push_matrix(!IO),
	mogl.translate(-3.0, -2.0, 0.0, !IO),
	mogl.rotate(Angle, 0.0, 0.0, 1.0, !IO),
	mogl.call_list(GearOne, !IO),
	mogl.pop_matrix(!IO),

	mogl.push_matrix(!IO),
	mogl.translate(3.1, -2.0, 0.0, !IO),
	mogl.rotate(-2.0 * Angle - 9.0, 0.0, 0.0, 1.0, !IO),
	mogl.call_list(GearTwo, !IO),
	mogl.pop_matrix(!IO),

	mogl.push_matrix(!IO),
	mogl.translate(-3.1, 4.2, 0.0, !IO),
	mogl.rotate(-2.0 * Angle - 25.0, 0.0, 0.0, 1.0, !IO),
	mogl.call_list(GearThree, !IO),
	mogl.pop_matrix(!IO),

	mogl.pop_matrix(!IO),
	
	Count = Count0 + 1,
	( if Count = Limit then	glut.quit(!IO) else true ),
	globals.set("Count", Count, !IO),
	
	glut.window.swap_buffers(!IO),
	%
	% Calculate the frame rate.
	%
	Frames = Frames0 + 1,
	glut.elapsed_time(T, !IO),
	( T - T0 >= 5000 ->
		Seconds = float((T - T0)) / 1000.0,
		FPS = float(Frames) / Seconds,
		io.format("%d frames in %f seconds = %6.3f FPS\n",
			[i(Frames), f(Seconds), f(FPS)], !IO),
		globals.set("T0", T, !IO),
		globals.set("Frames", 0, !IO)
	;
		globals.set("Frames", Frames, !IO)
	).

:- pred gears.idle(io::di, io::uo) is det.

gears.idle(!IO) :-
	globals.get("Angle", Angle, !IO),
	globals.set("Angle", Angle + 2.0, !IO),
	glut.window.post_redisplay(!IO).

:- pred gears.key(char::in, int::in, int::in, io::di, io::uo) is det.

gears.key(Key, _X, _Y, !IO) :-
	( char.to_int(Key, 27) ->
		glut.quit(!IO)
	;
		globals.get("ViewRotZ", ViewRotZ0, !IO),
		( Key = 'z' -> ViewRotZ = ViewRotZ0 + 5.0
		; Key = 'Z' -> ViewRotZ = ViewRotZ0 - 5.0
		; ViewRotZ = ViewRotZ0
		),
		globals.set("ViewRotZ", ViewRotZ, !IO),
		glut.window.post_redisplay(!IO)
	).

:- pred gears.special(special_key::in, int::in, int::in, io::di, io::uo) is det.

gears.special(Key, _, _, !IO) :-
	globals.get("ViewRotX", ViewRotX0, !IO),
	globals.get("ViewRotY", ViewRotY0, !IO),
	( gears.special_2(Key, ViewRotX0, ViewRotX1, ViewRotY0, ViewRotY1) ->
		ViewRotX = ViewRotX1, ViewRotY = ViewRotY1
	;
		ViewRotX = ViewRotX0, ViewRotY = ViewRotY0
	),
	globals.set("ViewRotX", ViewRotX, !IO),
	globals.set("ViewRotY", ViewRotY, !IO),
	glut.window.post_redisplay(!IO).

:- pred gears.special_2(special_key::in, float::in, float::out, float::in,
	float::out) is semidet.

gears.special_2(up,    ViewRotX, ViewRotX + 5.0, ViewRotY, ViewRotY).
gears.special_2(down,  ViewRotX, ViewRotX - 5.0, ViewRotY, ViewRotY).
gears.special_2(left,  ViewRotX, ViewRotX,       ViewRotY, ViewRotY + 5.0).
gears.special_2(right, ViewRotX, ViewRotX,       ViewRotY, ViewRotY - 5.0).

:- pred gears.reshape(int::in, int::in, io::di, io::uo) is det.

gears.reshape(Width, Height, !IO) :-
	H = float(Height) / float(Width),
	mogl.viewport(0, 0, Width, Height, !IO),
	mogl.matrix_mode(projection, !IO),
	mogl.load_identity(!IO),
	mogl.frustum(-1.0, 1.0, -H, H, 5.0, 60.0, !IO),
	mogl.matrix_mode(modelview, !IO),
	mogl.load_identity(!IO),
	mogl.translate(0.0, 0.0, -40.0, !IO).

:- pred gears.init(int::in, io::di, io::uo) is det.

gears.init(Limit, !IO) :-
	mogl.light(0, position(5.0, 5.0, 10.0, 0.0), !IO),
	mogl.enable(cull_face, !IO),
	mogl.enable(lighting, !IO),
	mogl.enable(light(0), !IO),
	mogl.enable(depth_test, !IO),
	
  	mogl.gen_lists(1, GearOne, !IO),
	mogl.new_list(GearOne, compile, !IO),
		mogl.material(front, ambient_and_diffuse(0.8, 0.1, 0.0, 1.0),
			!IO),
		gears.gear(1.0, 4.0, 1.0, 20, 0.7, !IO),
	mogl.end_list(!IO),
  
  	mogl.gen_lists(1, GearTwo, !IO),
	mogl.new_list(GearTwo, compile, !IO),
		mogl.material(front, ambient_and_diffuse(0.0, 0.8, 0.2, 1.0),
			!IO),
		gears.gear(0.5, 2.0, 2.0, 10, 0.7, !IO),
	mogl.end_list(!IO),
  	
	mogl.gen_lists(1, GearThree, !IO),
	mogl.new_list(GearThree, compile, !IO),
		mogl.material(front, ambient_and_diffuse(0.2, 0.2, 1.0, 1.0),
			!IO),
		gears.gear(1.3, 2.0, 0.5, 10, 0.7, !IO),
	mogl.end_list(!IO),

	mogl.enable(normalize, !IO),
	%
	% Set the initial value of the global state.
	%
	globals.init(!IO),
	globals.set("GearOne", GearOne, !IO),
	globals.set("GearTwo", GearTwo, !IO),
	globals.set("GearThree", GearThree, !IO),
	globals.set("Angle", 0.0, !IO),
	globals.set("Count", 1, !IO),
	globals.set("Limit", Limit, !IO),
	globals.set("Frames", 0, !IO),
	globals.set("T0", 0, !IO),
	globals.set("ViewRotX", 20.0, !IO),
	globals.set("ViewRotY", 30.0, !IO),
	globals.set("ViewRotZ", 0.0, !IO).

:- pred gears.visible(visibility::in, io::di, io::uo) is det.

gears.visible(visible, !IO) :- glut.callback.idle_func(gears.idle, !IO).
gears.visible(not_visible, !IO) :- glut.callback.disable_idle_func(!IO).

%-----------------------------------------------------------------------------%
:- end_module gears.
%-----------------------------------------------------------------------------%
