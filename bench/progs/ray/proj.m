:- module proj.
% the main module for the 380 project

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.

:- import_module list, int, float, char, string, require, gfx, scene, vec3, ray, std_util, ppm, debug.

% processed program arguments
:- type argtable --->
        argtable(
                float,              % ambient illumination
                list(light),        % list of light sources
                float,              % coefficient of specular reflection
                float,              % exponent of specular reflection
                float,              % ``focal length'' of the pin-hole camera
                shade_type,         % full or phong
                string,             % scene description file
                background          % background of image
        ).

main -->
    io__command_line_arguments(Args),
    { ArgTableDefault = argtable(0.0, [], 0.0, 1.0, 1.0, full, "-", solid(100, 100, colour(0.0, 0.0, 0.0))) },
    { ArgTable0 = process_args(Args, ArgTableDefault) },
    { argtable(Amb, Light, SpecC, SpecE, Foc, Shad, SceneFile, Background) = ArgTable0 },
    { (
        % if no light source given, use default -- unit power light at origin
        Light = []
    ->
        Light1 = [light(1.0, vec(0.0, 0.0, 0.0))]
    ;
        Light1 = Light
    ) },
    { ArgTable = argtable(Amb, Light1, SpecC, SpecE, Foc, Shad, SceneFile, Background) },
    get_scene(SceneFile, Scene),
    % { dump("got scene\n", []) },
    % { dump_scene(Scene) },
    get_image_size(Background, X, Y),
    % { dump("got size\n", []) },
    io__tell("OUT.ppm", _),
    ppm_write_header(X, Y),
    % { dump("written header\n", []) },
    generate_image(0, X, 0, Y, ArgTable, Scene).

% process_args(ArgList, Default) = ArgTable
% process the argument list
:- func process_args(list(string), argtable) = argtable.
:- mode process_args(in, in) = out is det.

process_args(L, T0) = T :-
    (
        L = [],
        T = T0
    ; 
        L = [S | L0],
        T0 = argtable(Amb, LL, SC, SE, Foc, Shad, Scene, Back),
            (
                S = "-a"    % ambient illumination
            ->
                (
                    L0 = [S1 | L1],
                    string__to_float(S1, Amb1)
                ->
                    T = process_args(L1, argtable(Amb1, LL, SC, SE, Foc, Shad, Scene, Back))
                ;
                    error("Wrong Arguments to -a option")
                )
            ;
                S = "-l"    % light source
            ->
                (
                    L0 = [S1, S2, S3, S4 | L1],
                    string__to_float(S1, LightPower),
                    string__to_float(S2, LightX),
                    string__to_float(S3, LightY),
                    string__to_float(S4, LightZ)
                ->
                    LL1 = [light(LightPower, vec(LightX, LightY, LightZ)) | LL],
                    T = process_args(L1, argtable(Amb, LL1, SC, SE, Foc, Shad, Scene, Back))
                ;
                    error("Wrong Arguments to -l option")
                )
            ;
                S = "-s"    % specular reflection
            ->
                (
                    L0 = [S1, S2 | L1],
                    string__to_float(S1, SC1),
                    string__to_float(S2, SE1)
                ->
                    T = process_args(L1, argtable(Amb, LL, SC1, SE1, Foc, Shad, Scene, Back))
                ;
                    error("Wrong Arguments to -s option")
                )
            ;
                S = "-f"    % focal length
            ->
                (
                    L0 = [S1 | L1],
                    string__to_float(S1, Foc1)
                ->
                    T = process_args(L1, argtable(Amb, LL, SC, SE, Foc1,Shad, Scene, Back))
                ;
                    error("Wrong Arguments to -f option")
                )
            ;
                S = "-F"    % polyhedral shading
            ->
                T = process_args(L0, argtable(Amb, LL, SC, SE, Foc, full, Scene, Back))
            ;
                S = "-S"    % smooth shading
            ->
                T = process_args(L0, argtable(Amb, LL, SC, SE, Foc, phong, Scene, Back))
            ;
                Scene1 = S,
                    (
                        L0 = [S1, S2, S3, S4, S5 | _L1],
                        string__to_int(S1, Width),
                        string__to_int(S2, Height),
                        string__to_float(S3, BackRed),
                        string__to_float(S4, BackGreen),
                        string__to_float(S5, BackBlue)
                    ->
                        Back1 = solid(Width, Height, colour(BackRed, BackGreen, BackBlue))
                    ;
                        L0 = [S1 | _L1]
                    ->
                        Back1 = filename(S1)
                    ;
                        Back1 = Back
                    ),
                T = argtable(Amb, LL, SC, SE, Foc, Shad, Scene1, Back1)
            )
        ).

% get the dimensions of the image pixmap
:- pred get_image_size(background, int, int, io__state, io__state).
:- mode get_image_size(in, out, out, di, uo) is det.

get_image_size(Background, X, Y) -->
    (
        { Background = filename(_) },
        { X = 100 }, % eventuallly will read from ppm file
        { Y = 100 }
    ;
        { Background = solid(X, Y, _) }
    ).

% return background colour of next pixel
:- pred get_background_colour(background, colour, io__state, io__state).
:- mode get_background_colour(in, out, di, uo) is det.

get_background_colour(Background, Colour) -->
    (
        { Background = filename(_) },
        { Colour = colour(0.0, 0.0, 0.0) } % note: eventually will read from ppm file
    ;
        { Background = solid(_, _, Colour) }
    ).

% generate the image, a pixel at a time
:- pred generate_image(int, int, int, int, argtable, scene, io__state, io__state).
:- mode generate_image(in, in, in, in, in, in, di, uo) is det.

generate_image(X, MaxX, Y, MaxY, ArgTable, Scene) -->
    (
        { Y >= MaxY }
    ->
        []
    ;
        { X >= MaxX }
    ->
        % { dump("Finished Row %d\n", [i(Y)]) },   %% dmo
	% io__report_stats,
        generate_image(0, MaxX, Y+1, MaxY, ArgTable, Scene)
    ;
        { argtable(Amb, Lights, SpecC, SpecE, Foc, Shad, _SceneFile, _Background) = ArgTable },
        { gen_dir(Foc, X, MaxX, Y, MaxY, Dir) },
        { ray_trace(100, vec(0.0, 0.0, 0.0), Dir, Scene, Amb, Lights,
		SpecC, SpecE, Shad, Colour) },
%        get_background_colour(Background, BGCol),
            (
%                { Pixel = yes(Colour) },
                 ppm_write_pixel(Colour)
%            ;
%                { Pixel = no },
%                ppm_write_pixel(BGCol)
            ),
        generate_image(X+1, MaxX, Y, MaxY, ArgTable, Scene)
    ).

% work out the direction of the ray from the origin to the image plane at pixel
% co-ordinates (X, Y).  Image plane is at Z = -Foc in world co-ords.
:- pred gen_dir(float, int, int, int, int, vec3).
:- mode gen_dir(in, in, in, in, in, out) is det.

gen_dir(Foc, X, MaxX, Y, MaxY, Dir) :-
    (
        DZ = -Foc,
        int__to_float(X, FX),
        int__to_float(MaxX, FMaxX),
        int__to_float(Y, FY),
        int__to_float(MaxY, FMaxY),
        DX = FX - FMaxX / 2.0,
        DY = -(FY - FMaxY / 2.0),
        %dump("gen_dir: direction=(%f, %f, %f)\n", [f(DX), f(DY), f(DZ)]), %%dmo
        Dir = unit(vec(DX, DY, DZ))
    ).

% the following predicates are for debugging.

:- pred dump_scene(scene).
:- mode dump_scene(in) is det.

dump_scene(Scene) :-
    (
        Scene = []
    ;
        Scene = [polytri(Triangles, vec(X, Y, Z), colour(R, G, B)) | Scene1],
        dump("Polygon colour (%f, %f, %f)\n", [f(R), f(G), f(B)]),
        dump("        normal (%f, %f, %f)\n", [f(X), f(Y), f(Z)]),
        dump_triangles(Triangles),
        dump_scene(Scene1)
    ).

% debugging stuff.
% print out info about a triangle.
:- pred dump_triangles(list(triangle)).
:- mode dump_triangles(in) is det.

dump_triangles(Ts) :-
    (
        Ts = []
    ;
        Ts = [tri(vec(X1, Y1, Z1), vec(X2, Y2, Z2), vec(X3, Y3, Z3), _, _, _) | Ts1],
        dump("    (%f, %f, %f)\n    (%f, %f, %f)\n    (%f, %f, %f)\n --------\n", 
            [f(X1), f(Y1), f(Z1), f(X2), f(Y2), f(Z2), f(X3), f(Y3), f(Z3)]),
        dump_triangles(Ts1)
    ).
