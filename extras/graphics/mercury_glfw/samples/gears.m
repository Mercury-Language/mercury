%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
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

:- import_module glfw.
:- import_module mglu.
:- import_module mogl.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Global state
%
    % The initial values of these four are dummy values.  We won't 
    % know the real value until after we've setup the display
    % and processed the command line arguments.
    %
:- mutable(gear_one,   int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(gear_two,   int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(gear_three, int,   0,    ground, [untrailed, attach_to_io_state]).

:- mutable(angle,      float, 0.0,  ground, [untrailed, attach_to_io_state]).
:- mutable(time,       int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_x, float, 20.0, ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_y, float, 30.0, ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_z, float, 0.0,  ground, [untrailed, attach_to_io_state]).

:- mutable(running, bool, yes, ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

main(!IO) :-
    glfw.init(!IO),
    glfw.open_window(300, 300, 0, 0, 0, 0, 0, 0, window, !IO),
    glfw.set_window_title("Gears", !IO),
    glfw.enable(key_repeat, !IO),
    glfw.swap_interval(1, !IO),    
    gears.init(!IO),
    glfw.set_window_size_callback(gears.reshape, !IO),
    glfw.set_key_callback(gears.key, !IO),
    do_main_loop(!IO),
    glfw.terminate(!IO).

:- pred gears.init(io::di, io::uo) is det.

gears.init(!IO) :-
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
    % Set the remainder of the global state.
    %
    set_gear_one(GearOne, !IO),
    set_gear_two(GearTwo, !IO),
    set_gear_three(GearThree, !IO).

:- pred do_main_loop(io::di, io::uo) is det.

do_main_loop(!IO) :-
    gears.draw(!IO),
    gears.animate(!IO),
    glfw.swap_buffers(!IO),
    glfw.get_bool_window_param(opened, IsWinOpen, !IO),
    get_running(Running, !IO),
    ( if IsWinOpen = yes, Running = yes then
        do_main_loop(!IO)
    else 
        true    
    ).

:- pred gears.draw(io::di, io::uo) is det.

gears.draw(!IO) :-
    get_view_rot_x(ViewRotX, !IO),
    get_view_rot_y(ViewRotY, !IO),
    get_view_rot_z(ViewRotZ, !IO),

    get_angle(Angle, !IO),

    get_gear_one(GearOne, !IO),
    get_gear_two(GearTwo, !IO),
    get_gear_three(GearThree, !IO),

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

    mogl.pop_matrix(!IO).


:- pred gears.animate(io::di, io::uo) is det.

gears.animate(!IO) :-
    glfw.get_time(Time, !IO),
    set_angle(100.0 * Time, !IO).

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

:- pred gears.key(key::in, key_state::in, io::di, io::uo) is det.

gears.key(_, release, !IO).
gears.key(Key, press, !IO) :-
    ( if Key = key_Z then
        get_view_rot_z(ViewRotZ, !IO),
        glfw.get_key(key_lshift, LShiftAction, !IO),
        ( if LShiftAction = press then
            set_view_rot_z(ViewRotZ - 5.0, !IO)
        else
            set_view_rot_z(ViewRotZ+ 5.0, !IO)  
        )
    else if Key = key_escape then
        set_running(no, !IO)
    else if Key = key_up then
        get_view_rot_x(ViewRotX, !IO),
        set_view_rot_x(ViewRotX + 5.0, !IO)
    else if Key = key_down then
        get_view_rot_x(ViewRotX, !IO),
        set_view_rot_x(ViewRotX - 5.0, !IO)
    else if Key = key_left then
        get_view_rot_y(ViewRotY, !IO),
        set_view_rot_y(ViewRotY + 5.0, !IO)
    else if Key = key_right then
        get_view_rot_y(ViewRotY, !IO),
        set_view_rot_y(ViewRotY - 5.0, !IO)
    else
        true
    ).

:- pred gears.reshape(int::in, int::in, io::di, io::uo) is det.

gears.reshape(Width, Height, !IO) :-
    H = float(Height) / float(Width),
    ZNear = 5.0,
    ZFar = 30.0,
    XMax = ZNear * 0.5,
    mogl.viewport(0, 0, Width, Height, !IO),
    mogl.matrix_mode(projection, !IO),
    mogl.load_identity(!IO),
    mogl.frustum(-XMax, XMax, -XMax * H, XMax * H, ZNear, ZFar, !IO),
    mogl.matrix_mode(modelview, !IO),
    mogl.load_identity(!IO),
    mogl.translate(0.0, 0.0, -20.0, !IO).

%-----------------------------------------------------------------------------%
:- end_module gears.
%-----------------------------------------------------------------------------%
