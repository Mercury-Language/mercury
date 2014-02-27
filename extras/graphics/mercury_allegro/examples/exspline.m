%-----------------------------------------------------------------------------%
%
% Example program for the Allegro library, by Shawn Hargreaves.
% Mercury port by Peter Wang.
%
% This program demonstrates the use of spline curves to create smooth 
% paths connecting a number of node points. This can be useful for 
% constructing realistic motion and animations.
%
% The technique is to connect the series of guide points p1..p(n) with
% spline curves from p1-p2, p2-p3, etc. Each spline must pass though
% both of its guide points, so they must be used as the first and fourth
% of the spline control points. The fun bit is coming up with sensible
% values for the second and third spline control points, such that the
% spline segments will have equal gradients where they meet. I came
% up with the following solution:
%
% For each guide point p(n), calculate the desired tangent to the curve
% at that point. I took this to be the vector p(n-1) -> p(n+1), which 
% can easily be calculated with the inverse tangent function, and gives 
% decent looking results. One implication of this is that two dummy 
% guide points are needed at each end of the curve, which are used in 
% the tangent calculations but not connected to the set of splines.
%
% Having got these tangents, it becomes fairly easy to calculate the
% spline control points. For a spline between guide points p(a) and
% p(b), the second control point should lie along the positive tangent
% from p(a), and the third control point should lie along the negative
% tangent from p(b). How far they are placed along these tangents 
% controls the shape of the curve: I found that applying a 'curviness'
% scaling factor to the distance between p(a) and p(b) works well.
%
% One thing to note about splines is that the generated points are
% not all equidistant. Instead they tend to bunch up nearer to the
% ends of the spline, which means you will need to apply some fudges
% to get an object to move at a constant speed. On the other hand,
% in situations where the curve has a noticable change of direction 
% at each guide point, the effect can be quite nice because it makes
% the object slow down for the curve.
%
%-----------------------------------------------------------------------------%

:- module exspline.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.color.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.mouse.
:- import_module allegro.palette.
:- import_module allegro.prim.
:- import_module allegro.text.
:- import_module allegro.timer.
:- import_module allegro.transparency.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type node == {int, int}.
:- type tangent == float.

main(!IO) :-
    allegro_init(Ok, !IO),
    (
        Ok = yes,
        install_keyboard(_, !IO),
        install_mouse(_, !IO),
        install_timer(_, !IO),
        set_gfx_mode(gfx_autodetect, 640, 480, 0, 0, GfxOk, !IO),
        (
            GfxOk = yes,
            set_palette(desktop_palette, !IO),
            main_2(!IO)
        ;
            GfxOk = no,
	    set_gfx_mode(gfx_text, 0, 0, 0, 0, _, !IO),
            allegro_error(Message, !IO),
            allegro_message("Unable to set any graphic mode\n" ++
                Message ++ "\n", !IO)
        )
    ;
        Ok = no
    ).

:- pred main_2(io::di, io::uo) is det.

main_2(!IO) :-
    input_nodes(Nodes, !IO),
    calc_tangents(Nodes, Tangents),
    Curviness = 0.25,
    ShowTangents = no,
    ShowControlPoints = no,
    main_3(Nodes, Tangents, Curviness, ShowTangents, ShowControlPoints, !IO).

:- pred main_3(list(node)::in, list(tangent)::in, float::in, bool::in,
        bool::in, io::di, io::uo) is det.

main_3(Nodes, Tangents, Curviness, ShowTangents, ShowControlPoints, !IO) :-
    draw_splines(Nodes, Tangents, Curviness, ShowTangents,  
        ShowControlPoints, !IO),
    readkey(Key, !IO),
    C = Key >> 8,
    (if C = key_esc then
        true
    else if C = key_up then
        main_3(Nodes, Tangents, Curviness+0.05, ShowTangents,
            ShowControlPoints, !IO)
    else if C = key_down then
        main_3(Nodes, Tangents, Curviness-0.05, ShowTangents,
            ShowControlPoints, !IO)
    else if C = key_space then
        walk(Curviness, Nodes, Tangents, !IO),
        main_3(Nodes, Tangents, Curviness, ShowTangents,
            ShowControlPoints, !IO)
    else if C = key_t then
        main_3(Nodes, Tangents, Curviness, not(ShowTangents),
            ShowControlPoints, !IO)
    else if C = key_c then
        main_3(Nodes, Tangents, Curviness, ShowTangents,
            not(ShowControlPoints), !IO)
    else
        main_3(Nodes, Tangents, Curviness, ShowTangents,
            ShowControlPoints, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred input_nodes(list(node)::out, io::di, io::uo) is det.

input_nodes(Nodes, !IO) :-
    det_screen(Screen, ScreenW, _ScreenH, !IO),
    font(Font, !IO),
    makecol(255, 255, 255, White, !IO),
    makecol(0, 0, 0, Black, !IO),
    clear_to_color(Screen, White, !IO),
    textout_centre_ex(Screen, Font,
        "Click the left mouse button to add path nodes",
        ScreenW/2, 8, Black, White, !IO),
    textout_centre_ex(Screen, Font,
        "Right mouse button or any key to finish",
        ScreenW/2, 24, Black, White, !IO),
    show_mouse(Screen, !IO),
    wait_release_mouse(!IO),
    clear_keybuf(!IO),
    input_nodes_2([], RevNodes, !IO),
    Nodes = list.reverse(RevNodes).

:- pred input_nodes_2(list(node)::in, list(node)::out, io::di, io::uo) is det.

input_nodes_2(Nodes0, Nodes, !IO) :-
    mouse_b(MouseB, !IO),
    keypressed(KeyPressed, !IO),
    (if MouseB /\ 1 \= 0 then
        mouse_xy(MouseX, MouseY, !IO),
        hide_mouse(!IO),
        draw_node(length(Nodes0), MouseX, MouseY, !IO),
        det_screen(Screen, !IO),
        show_mouse(Screen, !IO),
        Nodes1 = [{MouseX, MouseY} | Nodes0],
        wait_release_mouse(!IO),
        input_nodes_2(Nodes1, Nodes, !IO)
    else if (MouseB /\ 2 \= 0 ; KeyPressed = yes) then
        (if length(Nodes0) < 3 then
            % XXX alert("You must enter at least two nodes",
            %   NULL, NULL, "OK", NULL, 13, 0);
            input_nodes_2(Nodes0, Nodes, !IO)
        else
            Nodes = Nodes0
        )
    else
        rest(10, !IO),
        input_nodes_2(Nodes0, Nodes, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred calc_tangents(list(node)::in, list(tangent)::out) is det.

calc_tangents(Nodes0, Tangents) :-
    Len = length(Nodes0),
    Head = dummy_node(det_index0(Nodes0, 0), det_index0(Nodes0, 1)),
    Tail = dummy_node(det_index0(Nodes0, Len-1), det_index0(Nodes0, Len-2)),
    ( list.take(Len-1, Nodes0, ButLast) ->
        Tangents = list.map_corresponding(calc_tangent,
            [Head | ButLast], det_tail(Nodes0) ++ [Tail])
    ;
        error("calc_tangents")
    ).

:- func calc_tangent(node, node) = tangent.

calc_tangent({XA, YA}, {XB, YB}) = atan2(float(YB - YA), float(XB - XA)).

:- func dummy_node(node, node) = node.

dummy_node({NodeX, NodeY}, {PrevX, PrevY}) = {X, Y} :-
    X = NodeX - (PrevX - NodeX) / 8,
    Y = NodeY - (PrevY - NodeY) / 8.

%-----------------------------------------------------------------------------%

:- pred draw_splines(list(node)::in, list(tangent)::in, float::in, bool::in,
        bool::in, io::di, io::uo) is det.

draw_splines(Nodes, Tangents, Curviness, ShowTangents, ShowControlPoints,
        !IO) :-
    det_screen(Screen, ScreenW, _ScreenH, !IO),
    font(Font, !IO),
    makecol(0, 0, 0, Black, !IO),
    makecol(255, 255, 255, White, !IO),
    acquire_screen(!IO),
    clear_to_color(Screen, White, !IO),
    textout_centre_ex(Screen, Font, "Spline curve path", ScreenW/2, 8,
        Black, White, !IO),
    textout_centre_ex(Screen, Font,
        string.format("Curviness = %.2f", [f(Curviness)]),
        ScreenW/2, 32, Black, White, !IO),
    textout_centre_ex(Screen, Font, "Up/down keys to alter",
        ScreenW/2, 44, Black, White, !IO),
    textout_centre_ex(Screen, Font, "Space to walk",
        ScreenW/2, 68, Black, White, !IO),
    textout_centre_ex(Screen, Font, "C to display control points",
        ScreenW/2, 92, Black, White, !IO),
    textout_centre_ex(Screen, Font, "T to display tangents",
        ScreenW/2, 104, Black, White, !IO),
    release_screen(!IO),
    draw_splines_2(Curviness, ShowControlPoints, Nodes, Tangents, !IO),
    draw_nodes(1, Nodes, !IO),
    (
        ShowTangents = yes,
        list.foldl_corresponding(draw_tangent, Nodes, Tangents, !IO)
    ;
        ShowTangents = no
    ).

:- pred draw_splines_2(float::in, bool::in,
        list(node)::in, list(tangent)::in, io::di, io::uo) is det.

draw_splines_2(_, _, [], _, !IO) :-
    error("draw_splines_2").
draw_splines_2(_, _, [_], _, !IO).
draw_splines_2(Curviness, ShowControlPoints,
        [NodeA, NodeB | Nodes], Tan, !IO) :-
    (if
        Tan = [TangentA, TangentB | Tangents]
    then
        det_screen(Screen, !IO),
        makecol(0, 0, 0, Black, !IO),
        get_control_points(Curviness, NodeA - TangentA, NodeB - TangentB,
            XA, YA, XB, YB, XC, YC, XD, YD),
        spline(Screen, XA, YA, XB, YB, XC, YC, XD, YD, Black, !IO),
        (
            ShowControlPoints = yes,
            circlefill(Screen, XB, YB, 2, 2, !IO),
            circlefill(Screen, XC, YC, 2, 2, !IO)
        ;
            ShowControlPoints = no
        ),
        draw_splines_2(Curviness, ShowControlPoints,
            [NodeB | Nodes], [TangentB | Tangents], !IO)
    else
        error("draw_splines_2")
    ).

:- pred draw_nodes(int::in, list(node)::in, io::di, io::uo) is det.

draw_nodes(_, [], !IO).
draw_nodes(Num, [{X, Y} | Nodes], !IO) :-
    draw_node(Num, X, Y, !IO),
    draw_nodes(Num+1, Nodes, !IO).

:- pred draw_node(int::in, int::in, int::in, io::di, io::uo) is det.

draw_node(Num, X, Y, !IO) :-
    det_screen(Screen, !IO),
    font(Font, !IO),
    Red = 1,
    White = 255,
    circlefill(Screen, X, Y, 2, Red, !IO),
    textout_ex(Screen, Font, string.from_int(Num), X-7, Y-7, White, -1, !IO).

:- pred draw_tangent(node::in, tangent::in, io::di, io::uo) is det.

draw_tangent({X0, Y0}, Tangent, !IO) :-
    X = float(X0),
    Y = float(Y0),
    det_screen(Screen, !IO),
    line(Screen,
        truncate_to_int(X - cos(Tangent) * 24.0),
        truncate_to_int(Y - sin(Tangent) * 24.0),
        truncate_to_int(X + cos(Tangent) * 24.0),
        truncate_to_int(Y + sin(Tangent) * 24.0),
        1, !IO).

:- pred get_control_points(float::in,
        pair(node, float)::in, pair(node, float)::in,
        int::out, int::out, int::out, int::out,
        int::out, int::out, int::out, int::out) is det.

get_control_points(Curviness, N1, N2, XA, YA, XB, YB, XC, YC, XD, YD) :-
    N1 = Node1 @ {N1X, N1Y} - Tangent1,
    N2 = Node2 @ {N2X, N2Y} - Tangent2,
    Dist = node_dist(Node1, Node2) * Curviness,
    XA = N1X,
    YA = N1Y,
    XB = N1X + truncate_to_int(Dist * cos(Tangent1)),
    YB = N1Y + truncate_to_int(Dist * sin(Tangent1)),
    XC = N2X - truncate_to_int(Dist * cos(Tangent2)),
    YC = N2Y - truncate_to_int(Dist * sin(Tangent2)),
    XD = N2X,
    YD = N2Y.

:- func node_dist(node, node) = float.

node_dist({N1X, N1Y}, {N2X, N2Y}) = Dist :-
    Scale = 64.0,
    DX = float(N1X - N2X) / Scale,
    DY = float(N1Y - N2Y) / Scale,
    Dist = sqrt(DX*DX + DY*DY) * Scale.

%-----------------------------------------------------------------------------%

:- pred walk(float::in, list(node)::in, list(tangent)::in, io::di, io::uo)
        is det.

walk(Curviness, Nodes, Tangents, !IO) :-
    det_screen(Screen, !IO),
    makecol(255, 255, 255, White, !IO),
    acquire_screen(!IO),
    clear_to_color(Screen, White, !IO),
    draw_nodes(1, Nodes, !IO),
    release_screen(!IO),
    wait_release_mouse(!IO),
    clear_keybuf(!IO),
    xor_mode(yes, !IO),
    walk_2(Curviness, Nodes, Tangents, {-16, -16}, _, !IO),
    xor_mode(no, !IO),
    wait_release_mouse(!IO),
    clear_keybuf(!IO).

:- pred walk_2(float::in, list(node)::in, list(tangent)::in,
        node::in, node::out, io::di, io::uo) is det.

walk_2(_Curviness, [], _Tangents, Node, Node, !IO) :-
    error("walk_2").
walk_2(_Curviness, [_], _Tangents, Node, Node, !IO).
walk_2(Curviness, [N1, N2 | Nodes], Tangents, OldNode0, OldNode, !IO) :-
    (if
        Tangents = [Tan1, Tan2 | Tans]
    then
        NPoints = (truncate_to_int(node_dist(N1, N2)) + 3) / 4,
        get_control_points(Curviness, N1 - Tan1, N2 - Tan2,
            XA, YA, XB, YB, XC, YC, XD, YD),
        calc_spline(XA, YA, XB, YB, XC, YC, XD, YD, NPoints, SplinePoints),
        list.foldl2(walk_3, SplinePoints, OldNode0, OldNode1, !IO),
        walk_2(Curviness, [N2 | Nodes], [Tan2 | Tans], OldNode1, OldNode, !IO)
    else
        error("walk_2")
    ).

:- pred walk_3({int, int}::in, {int, int}::in, {int, int}::out, io::di, io::uo)
        is det.

walk_3(Pos@{X, Y}, {OX, OY}, Pos, !IO) :-
    vsync(!IO),
    acquire_screen(!IO),
    det_screen(Screen, !IO),
    circlefill(Screen, OX, OY, 6, 2, !IO),
    circlefill(Screen, X, Y, 6, 2, !IO),
    release_screen(!IO).

%-----------------------------------------------------------------------------%

:- pred wait_release_mouse(io::di, io::uo) is det.

wait_release_mouse(!IO) :-
    mouse_b(B, !IO),
    (if B = 0 then
        true
    else
        wait_release_mouse(!IO)
    ).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
