%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% File: gears.m
% Author: juliensf
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

:- import_module glut.
:- import_module glut.callback.
:- import_module glut.window.
:- import_module mglu.
:- import_module mogl.

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
    % The initial values of these four are dummy values.
    % We will not know the real values until after we have setup the display
    % and processed the command line arguments.
    %
:- mutable(gear_one,   int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(gear_two,   int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(gear_three, int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(limit,      int,   0,    ground, [untrailed, attach_to_io_state]).

:- mutable(angle,      float, 0.0,  ground, [untrailed, attach_to_io_state]).
:- mutable(count,      int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(frames,     int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(time,       int,   0,    ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_x, float, 20.0, ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_y, float, 30.0, ground, [untrailed, attach_to_io_state]).
:- mutable(view_rot_z, float, 0.0,  ground, [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        (
            Args  = [],
            Limit = 0
        ;
            Args  = [Limit0],
            string.to_int(Limit0, Limit)
        )
    then
        set_limit(Limit, !IO),
        main_2(!IO)
    else
        io.stderr_stream(StdErr, !IO),
        io.write_string(StdErr, "Usage: gears [<limit>]\n", !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    glut.init_display_mode([rgba, depth, double], !IO),
    glut.window.create("Gears", !IO),

    mogl.get_string(version,    VersionResult, !IO),
    mogl.get_string(vendor,     VendorResult, !IO),
    mogl.get_string(renderer,   RendererResult, !IO),
    mogl.get_string(extensions, ExtensionsResult, !IO),

    io.write_string("GL Version: ", !IO),
    write_maybe(VersionResult, !IO),

    io.write_string("GL Vendor: ", !IO),
    write_maybe(VendorResult, !IO),

    io.write_string("Renderer: ", !IO),
    write_maybe(RendererResult, !IO),

    % XXX We could format the extensions list a bit
    % better than this.
    io.write_string("Available Extensions: ", !IO),
    write_maybe(ExtensionsResult, !IO),

    gears.init(!IO),

    glut.callback.display_func(gears.draw, !IO),
    glut.callback.reshape_func(gears.reshape, !IO),
    glut.callback.keyboard_func(gears.key, !IO),
    glut.callback.special_func(gears.special, !IO),
    glut.callback.visibility_func(gears.visible, !IO),
    glut.main_loop(!IO).

:- pred write_maybe(maybe(string)::in, io::di, io::uo) is det.

write_maybe(no, !IO) :- io.write_string("unknown.\n", !IO).
write_maybe(yes(Str), !IO) :- io.write_string(Str ++ ".\n", !IO).

:- pred gear(float::in, float::in, float::in, int::in, float::in,
    io::di, io::uo) is det.

gear(InnerRadius, OuterRadius, Width, Teeth, ToothDepth, !IO) :-
    R0 = InnerRadius,
    R1 = OuterRadius - ToothDepth / 2.0,
    R2 = OuterRadius + ToothDepth / 2.0,

    Da = 2.0 * pi / float(Teeth) / 4.0,

    mogl.shade_model(flat, !IO),
    mogl.normal3(0.0, 0.0, 1.0, !IO),

    draw_front_face(R0, R1, Da, Width, Teeth, !IO),
    draw_front_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO),

    mogl.normal3(0.0, 0.0, -1.0, !IO),

    draw_back_face(R0, R1, Da, Width, Teeth, !IO),
    draw_back_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO),
    draw_outward_faces_of_teeth(R1, R2, Da, Width, Teeth, !IO),

    mogl.shade_model(smooth, !IO),

    draw_inside_radius_cylinder(R0, Width, Teeth, !IO).

:- pred draw_front_face(float::in, float::in, float::in, float::in,
    int::in, io::di, io::uo) is det.

draw_front_face(R0, R1, Da, Width, Teeth, !IO) :-
    mogl.begin(quad_strip, !IO),
    DrawFrontFace = (pred(I::in, !.IO::di, !:IO::uo) is det :-
        Angle = float(I) * 2.0 * pi / float(Teeth),
        mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
            Width * 0.5, !IO),
        mogl.vertex3(R1 * cos(Angle), R1 * sin(Angle),
            Width * 0.5, !IO),
        ( if I < Teeth then
            mogl.vertex3(R0 * cos(Angle), R0 * sin(Angle),
                Width * 0.5, !IO),
                mogl.vertex3(R1 * cos(Angle + 3.0 * Da),
                R1 * sin(Angle + 3.0 * Da),
                Width * 0.5, !IO)
        else
            true
        )
    ),
    int.fold_up(DrawFrontFace, 0, Teeth, !IO),
    mogl.end(!IO).

:- pred draw_front_sides_of_teeth(float::in, float::in, float::in,
    float::in, int::in, io::di, io::uo) is det.

draw_front_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
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

:- pred draw_back_face(float::in, float::in, float::in, float::in,
    int::in, io::di, io::uo) is det.

draw_back_face(R0, R1, Da, Width, Teeth, !IO) :-
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

:- pred draw_back_sides_of_teeth(float::in, float::in, float::in,
    float::in, int::in, io::di, io::uo) is det.

draw_back_sides_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
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

:- pred draw_outward_faces_of_teeth(float::in, float::in, float::in,
    float::in, int::in, io::di, io::uo) is det.

draw_outward_faces_of_teeth(R1, R2, Da, Width, Teeth, !IO) :-
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

:- pred draw_inside_radius_cylinder(float::in, float::in, int::in,
    io::di, io::uo) is det.

draw_inside_radius_cylinder(R0, Width, Teeth, !IO) :-
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

:- pred draw(io::di, io::uo) is det.

draw(!IO) :-
    get_view_rot_x(ViewRotX, !IO),
    get_view_rot_y(ViewRotY, !IO),
    get_view_rot_z(ViewRotZ, !IO),

    get_angle(Angle, !IO),
    get_count(Count0, !IO),
    get_limit(Limit, !IO),

    get_gear_one(GearOne, !IO),
    get_gear_two(GearTwo, !IO),
    get_gear_three(GearThree, !IO),

    get_frames(Frames0, !IO),
    get_time(T0, !IO),

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
    ( if Count = Limit then
        glut.quit(!IO)
    else
         true
    ),
    set_count(Count, !IO),

    glut.window.swap_buffers(!IO),

    % Calculate the frame rate.
    %
    Frames = Frames0 + 1,
    glut.elapsed_time(T, !IO),
    ( if T - T0 >= 5000 then
        Seconds = float((T - T0)) / 1000.0,
        FPS = float(Frames) / Seconds,
        io.format("%d frames in %f seconds = %6.3f FPS\n",
            [i(Frames), f(Seconds), f(FPS)], !IO),
        set_time(T, !IO),
        set_frames(0, !IO)
    else
        set_frames(Frames, !IO)
    ).

:- pred idle(io::di, io::uo) is det.

idle(!IO) :-
    get_angle(Angle, !IO),
    set_angle(Angle + 2.0, !IO),
    glut.window.post_redisplay(!IO).

:- pred key(char::in, int::in, int::in, io::di, io::uo) is det.

key(Key, _X, _Y, !IO) :-
    ( if char.to_int(Key, 27) then
        glut.quit(!IO)
    else
        get_view_rot_z(ViewRotZ0, !IO),
        ( if Key = 'z' then
            ViewRotZ = ViewRotZ0 + 5.0
        else if Key = 'Z' then
            ViewRotZ = ViewRotZ0 - 5.0
        else
            ViewRotZ = ViewRotZ0
        ),
        set_view_rot_z(ViewRotZ, !IO),
        glut.window.post_redisplay(!IO)
    ).

:- pred special(special_key::in, int::in, int::in, io::di, io::uo) is det.

special(Key, _, _, !IO) :-
    get_view_rot_x(ViewRotX0, !IO),
    get_view_rot_y(ViewRotY0, !IO),
    ( if special_2(Key, ViewRotX0, ViewRotX1, ViewRotY0, ViewRotY1) then
        ViewRotX = ViewRotX1, ViewRotY = ViewRotY1
    else
        ViewRotX = ViewRotX0, ViewRotY = ViewRotY0
    ),
    set_view_rot_x(ViewRotX, !IO),
    set_view_rot_y(ViewRotY, !IO),
    glut.window.post_redisplay(!IO).

:- pred special_2(special_key::in, float::in, float::out, float::in,
    float::out) is semidet.

special_2(up,    ViewRotX, ViewRotX + 5.0, ViewRotY, ViewRotY).
special_2(down,  ViewRotX, ViewRotX - 5.0, ViewRotY, ViewRotY).
special_2(left,  ViewRotX, ViewRotX,       ViewRotY, ViewRotY + 5.0).
special_2(right, ViewRotX, ViewRotX,       ViewRotY, ViewRotY - 5.0).

:- pred reshape(int::in, int::in, io::di, io::uo) is det.

reshape(Width, Height, !IO) :-
    H = float(Height) / float(Width),
    mogl.viewport(0, 0, Width, Height, !IO),
    mogl.matrix_mode(projection, !IO),
    mogl.load_identity(!IO),
    mogl.frustum(-1.0, 1.0, -H, H, 5.0, 60.0, !IO),
    mogl.matrix_mode(modelview, !IO),
    mogl.load_identity(!IO),
    mogl.translate(0.0, 0.0, -40.0, !IO).

:- pred init(io::di, io::uo) is det.

init(!IO) :-
    mogl.light(0, position(5.0, 5.0, 10.0, 0.0), !IO),
    mogl.enable(cull_face, !IO),
    mogl.enable(lighting, !IO),
    mogl.enable(light(0), !IO),
    mogl.enable(depth_test, !IO),

    mogl.gen_lists(1, GearOne, !IO),
    mogl.new_list(GearOne, compile, !IO),
        mogl.material(front, ambient_and_diffuse(0.8, 0.1, 0.0, 1.0),
            !IO),
        gear(1.0, 4.0, 1.0, 20, 0.7, !IO),
    mogl.end_list(!IO),

    mogl.gen_lists(1, GearTwo, !IO),
    mogl.new_list(GearTwo, compile, !IO),
        mogl.material(front, ambient_and_diffuse(0.0, 0.8, 0.2, 1.0),
            !IO),
        gear(0.5, 2.0, 2.0, 10, 0.7, !IO),
    mogl.end_list(!IO),

    mogl.gen_lists(1, GearThree, !IO),
    mogl.new_list(GearThree, compile, !IO),
        mogl.material(front, ambient_and_diffuse(0.2, 0.2, 1.0, 1.0),
            !IO),
        gear(1.3, 2.0, 0.5, 10, 0.7, !IO),
    mogl.end_list(!IO),

    mogl.enable(normalize, !IO),

    % Set the remainder of the global state.
    %
    set_gear_one(GearOne, !IO),
    set_gear_two(GearTwo, !IO),
    set_gear_three(GearThree, !IO).

:- pred visible(visibility::in, io::di, io::uo) is det.

visible(Visibility, !IO) :-
    (
        Visibility = visible,
        glut.callback.idle_func(gears.idle, !IO)
    ;
        Visibility = not_visible,
        glut.callback.disable_idle_func(!IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module gears.
%-----------------------------------------------------------------------------%
