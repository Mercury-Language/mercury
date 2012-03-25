%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% A Mercury version of the GLFW spinning triangle example.
%
%-----------------------------------------------------------------------------%

:- module triangle.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module glfw.
:- import_module mogl.
:- import_module mglu.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

main(!IO) :-
    glfw.init(!IO),
    glfw.open_window(640, 480, 0, 0, 0, 0, 0, 0, window, !IO),
    glfw.set_window_title("Spinning Triangle", !IO),
    glfw.enable(sticky_keys, !IO),
    glfw.swap_interval(1, !IO),
    main_2(!IO),
    glfw.terminate(!IO).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    glfw.get_time(Time, !IO),
    glfw.get_mouse_pos(X, _Y, !IO),
    glfw.get_window_size(Width, Height0, !IO),
    
    Height = ( if Height0 > 0 then Height0 else 1 ),
    mogl.viewport(0, 0, Width, Height, !IO),
   
    % Clear the color buffer to black.
    mogl.clear_color(0.0, 0.0, 0.0, 0.0, !IO),
    mogl.clear([color], !IO), 

    % Select and setup the projection matrix.
    mogl.matrix_mode(projection, !IO),
    mogl.load_identity(!IO),
    mglu.perspective(65.0, float(Width) / float(Height), 1.0, 100.0, !IO),
    
    % Select and setup the modelview matrix.
    mogl.matrix_mode(modelview, !IO),
    mogl.load_identity(!IO),
    mglu.look_at(
        0.0, 1.0, 0.0,  % Eye-position.
        0.0, 20.0, 0.0, % View-point.
        0.0, 0.0, 1.0,  % Up-vector.
        !IO),
    
    % Draw a rotating colorful triangle.
    mogl.translate(0.0, 14.0, 0.0, !IO),
    mogl.rotate(0.3 * float(X) + Time * 100.0, 0.0, 0.0, 1.0, !IO),
    mogl.begin(triangles, !IO),
        mogl.color3(1.0, 0.0, 0.0, !IO),
        mogl.vertex3(-5.0, 0.0, -4.0, !IO),
        mogl.color3(0.0, 1.0, 0.0, !IO),
        mogl.vertex3(5.0, 0.0, -4.0, !IO),
        mogl.color3(0.0, 0.0, 1.0, !IO),
        mogl.vertex3(0.0, 0.0, 6.0, !IO),
    mogl.end(!IO),
   
    % Swap buffers.
    glfw.swap_buffers(!IO),

    glfw.get_key(key_escape, EscKeyState, !IO),
    glfw.get_bool_window_param(opened, IsWindowOpen, !IO),
    
    ( if EscKeyState \= press, IsWindowOpen = yes then
        main_2(!IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module triangle.
%-----------------------------------------------------------------------------%
