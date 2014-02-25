%-----------------------------------------------------------------------------%

:- module view.
:- interface.

:- import_module speed.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type view.

:- type project_func == (pred(float, float, int, int)).
:- mode project_func == in(pred(in, in, out, out) is semidet).

:- func init_view = view.
:- pred advance_view(view::in, view::out, bool::out) is det.
:- pred update_view(view::in, view::out) is det.
:- pred draw_view(bitmap::in, game::in, view::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module badguys.
:- import_module bullet.
:- import_module explode.
:- import_module message.
:- import_module player.
:- import_module util.

:- import_module allegro.blit.
:- import_module allegro.color.
:- import_module allegro.math3d.
:- import_module allegro.prim.
:- import_module allegro.text.
:- import_module allegro.transparency.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- import_module string.

%-----------------------------------------------------------------------------%

    % desired position of a viewport window
:- type desired_pos
    --->    desired_pos(float, float, float, float).
                % left, top, right, bottom



    % viewport positioning macros
    %
:- func off_tl = desired_pos.
:- func off_tr = desired_pos.
:- func off_bl = desired_pos.
:- func off_br = desired_pos.

off_tl = desired_pos(-0.1, -0.1, -0.1, -0.1).
off_tr = desired_pos( 1.1, -0.1,  1.1, -0.1).
off_bl = desired_pos(-0.1,  1.1, -0.1,  1.1).
off_br = desired_pos( 1.1,  1.1,  1.1,  1.1).

:- func qtr_tl = desired_pos.
:- func qtr_tr = desired_pos.
:- func qtr_bl = desired_pos.
:- func qtr_br = desired_pos.

qtr_tl = desired_pos(0.0, 0.0, 0.5, 0.5).
qtr_tr = desired_pos(0.5, 0.0, 1.0, 0.5).
qtr_bl = desired_pos(0.0, 0.5, 0.5, 1.0).
qtr_br = desired_pos(0.5, 0.5, 1.0, 1.0).

:- func big_tl = desired_pos.
:- func big_tr = desired_pos.
:- func big_bl = desired_pos.
:- func big_br = desired_pos.

big_tl = desired_pos(0.0, 0.0, 0.7, 0.7).
big_tr = desired_pos(0.3, 0.0, 1.0, 0.7).
big_bl = desired_pos(0.0, 0.3, 0.7, 1.0).
big_br = desired_pos(0.3, 0.3, 1.0, 1.0).

:- func full = desired_pos.

full = desired_pos(0.0, 0.0, 1.0, 1.0).



    % list of viewport window positions
    %
:- type view_configuration ==
    {desired_pos, desired_pos, desired_pos, desired_pos}.

:- func view_configurations = list(view_configuration).

view_configurations = [
   { full,   off_tr, off_bl, off_br },    /* 1    single */
   { off_tl, full,   off_bl, off_br },    /* 2    single */
   { big_tl, big_br, off_bl, off_br },    /* 12   multiple */
   { off_tl, off_tr, full,   off_br },    /* 3    single */
   { big_tl, off_tr, big_br, off_br },    /* 13   multiple */
   { off_tl, big_tr, big_bl, off_br },    /* 23   multiple */
   { full,   full,   off_bl, off_br },    /* 12   superimpose */
   { off_tl, off_tr, off_bl, full   },    /* 4    single */
   { big_tl, off_tr, off_bl, big_br },    /* 14   multiple */
   { off_tl, full,   full,   off_br },    /* 23   superimpose */
   { off_tl, big_tl, off_bl, big_br },    /* 24   multiple */
   { off_tl, full,   off_bl, full   },    /* 24   superimpose */
   { qtr_tl, qtr_tr, qtr_bl, off_br },    /* 123  multiple */
   { big_tl, off_tr, off_bl, big_br },    /* 14   superimpose */
   { qtr_tl, off_tr, qtr_bl, qtr_br },    /* 134  multiple */
   { full,   off_tr, full,   off_br },    /* 13   superimpose */
   { off_tl, off_tr, big_tl, big_br },    /* 34   multiple */
   { off_tl, qtr_tr, big_bl, qtr_br },    /* 234  multiple */
   { off_tl, off_tr, full,   full   },    /* 34   superimpose */
   { full,   qtr_tr, off_bl, qtr_br },    /* 124  multiple */
   { full,   full,   off_bl, full   },    /* 124  superimpose */
   { qtr_tl, qtr_tr, qtr_bl, qtr_br },    /* 1234 multiple */
   { full,   full,   full,   off_br },    /* 123  superimpose */
   { full,   off_tr, full,   full   },    /* 134  superimpose */
   { off_tl, full,   full,   full   },    /* 234  superimpose */
   { full,   full,   full,   full   }     /* 1234 superimpose */
].

:- func num_view_configurations = int.
num_view_configurations = length(view_configurations).



:- type view
    --->    view(
                viewnum     :: int,
                viewinfos   :: {viewinfo, viewinfo, viewinfo, viewinfo}
            ).



    % current viewport state
    %
:- type viewinfo
    --->    viewinfo(
                left            :: float,
                top             :: float,
                right           :: float,
                bottom          :: float,
                vel_left        :: float,
                vel_top         :: float,
                vel_right       :: float,
                vel_bottom      :: float
            ).

:- type viewport_type
    --->    type_1
    ;       type_2
    ;       type_3
    ;       type_4.



init_view = init_view(0).

:- func init_view(int) = view.

init_view(ViewNum) = view(ViewNum, {Zero, Zero, Zero, Zero}) :-
    Zero = viewinfo(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0).

advance_view(view(ViewNum0, ViewInfos), view(ViewNum, ViewInfos), Cycled) :-
    ViewNum1 = ViewNum0 + 1,
    (if ViewNum1 >= num_view_configurations then
        ViewNum = 0,
        Cycled = yes
    else
        ViewNum = ViewNum1,
        Cycled = no
    ).

update_view(view(ViewNum, {A0, B0, C0, D0}), view(ViewNum, {A, B, C, D})) :-
    list.det_index0(view_configurations, ViewNum, {PosA, PosB, PosC, PosD}),
    update_viewinfo(PosA, A0, A),
    update_viewinfo(PosB, B0, B),
    update_viewinfo(PosC, C0, C),
    update_viewinfo(PosD, D0, D).

:- pred update_viewinfo(desired_pos::in, viewinfo::in, viewinfo::out) is det.

update_viewinfo(DesiredPos, ViewInfo0, ViewInfo) :-
    ViewInfo0 = viewinfo(Left0, Top0, Right0, Bottom0,
                         VelLeft0, VelTop0, VelRight0, VelBottom0),
    DesiredPos = desired_pos(DesiredLeft, DesiredTop,
                             DesiredRight, DesiredBottom),
    update_pos(DesiredLeft, Left0, Left, VelLeft0, VelLeft),
    update_pos(DesiredTop, Top0, Top, VelTop0, VelTop),
    update_pos(DesiredRight, Right0, Right, VelRight0, VelRight),
    update_pos(DesiredBottom, Bottom0, Bottom, VelBottom0, VelBottom),
    ViewInfo = viewinfo(Left, Top, Right, Bottom,
                        VelLeft, VelTop, VelRight, VelBottom).

:- pred update_pos(float::in, float::in, float::out, float::in, float::out)
        is det.

update_pos(DesiredPos, Pos0, Pos, Vel0, Vel) :-
    Delta0 = DesiredPos - Pos0,
    Delta = ln(abs(Delta0) + 1.0) * sgn(Delta0) / 64.0,
    Vel1 = (Vel0 * 0.9) + Delta,

    (if abs(Delta) < 0.00001,
        abs(Vel1) < 0.00001
    then
        Pos = DesiredPos,
        Vel = 0.0
    else
        Pos = Pos0 + Vel,
        Vel = Vel1
    ).



draw_view(Bitmap, Game, View, !IO) :-
    clear_bitmap(Bitmap, !IO),

    drawing_mode(trans, !IO),

    View = view(_, {VIA, VIB, VIC, VID}),
    draw_viewinfo(Bitmap, Game, type_1, VIA, !IO),
    draw_viewinfo(Bitmap, Game, type_2, VIB, !IO),
    draw_viewinfo(Bitmap, Game, type_3, VIC, !IO),
    draw_viewinfo(Bitmap, Game, type_4, VID, !IO),

    solid_mode(!IO),

    draw_messages(Bitmap, Game ^ messages, !IO),

    font(Font, !IO),
    makecol(128, 128, 128, Gray, !IO),
    textout_ex(Bitmap, Font, Lives, 4, 4, Gray, -1, !IO),
    textout_ex(Bitmap, Font, Score, 4, 16, Gray, -1, !IO),
    textout_ex(Bitmap, Font, "Hiscore: XXX", 4, 28, Gray, -1, !IO),
    Lives = string.format("Lives: %d", [i(Game ^ player ^ lives)]),
    Score = string.format("Score: %d", [i(Game ^ player ^ score)]),

    det_screen(Screen, ScreenW, ScreenH, !IO),
    blit(Bitmap, Screen, 0, 0, 0, 0, ScreenW, ScreenH, !IO).

:- pred draw_viewinfo(bitmap::in, game::in, viewport_type::in, viewinfo::in,
        io::di, io::uo) is det.

draw_viewinfo(Bitmap, Game, ViewportType, ViewInfo, !IO) :-
    Width  = float(bitmap_w(Bitmap)),
    Height = float(bitmap_h(Bitmap)),
    Left   = ViewInfo ^ left   * Width,
    Top    = ViewInfo ^ top    * Height,
    Right  = ViewInfo ^ right  * Width,
    Bottom = ViewInfo ^ bottom * Height,
    (
        ViewportType = type_1,
        Project = project_flat(Left, Top, Right, Bottom),
        R = 0,
        G = 255,
        B = 0
    ;
        ViewportType = type_2,
        Project = project_spherical(Left, Top, Right, Bottom),
        R = 255,
        G = 255,
        B = 0
    ;
        ViewportType = type_3,
        Project = project_tube(Left, Top, Right, Bottom),
        R = 0,
        G = 0,
        B = 255
    ;
        ViewportType = type_4,
        Project = project_cylinder(Left, Top, Right, Bottom, PlayerPos),
        R = 255,
        G = 0,
        B = 0,
        PlayerPos = Game ^ player ^ pos
    ),
    (if 
        Right > Left,
        Bottom > Top,
        Right > 0.0,
        Bottom > 0.0,
        Left < 640.0,   % XXX
        Top < 480.0     % XXX
    then
        draw_grid(Bitmap, ViewportType, Project, R, G, B, !IO),
        draw_player(Bitmap, R, G, B, Project, Game ^ player, !IO),
        draw_badguys(Bitmap, R, G, B, Project, Game ^ badguys, !IO),
        draw_bullets(Bitmap, R, G, B, Project, Game ^ bullets, !IO),
        draw_explosions(Bitmap, R, G, B, Project, Game ^ explosions, !IO)
    else
        true
    ).

:- pred draw_grid(bitmap::in, viewport_type::in, project_func::project_func,
        int::in, int::in, int::in, io::di, io::uo) is det.
:- pred draw_grid_2(bitmap::in, viewport_type::in, project_func::project_func,
        color::in, int::in, io::di, io::uo) is det.
:- pred draw_grid_3(bitmap::in, viewport_type::in, project_func::project_func,
        color::in, int::in, int::in, io::di, io::uo) is det.

:- func grid_n = int.
grid_n = 16.

draw_grid(Bitmap, ViewportType, Project, R, G, B, !IO) :-
    makecol(R/5, G/5, B/5, C, !IO),
    int.fold_up(draw_grid_2(Bitmap, ViewportType, Project, C),
        0, grid_n, !IO).

draw_grid_2(Bitmap, ViewportType, Project, C, X, !IO) :-
    int.fold_up(draw_grid_3(Bitmap, ViewportType, Project, C, X),
        0, grid_n, !IO).

draw_grid_3(Bitmap, ViewportType, Project, C, X, Y, !IO) :-
    N = float(grid_n),
    (if
        Project(float(X) / N, float(Y) / N, XA, YA),
        Project(float(X+1)/N, float(Y) / N, XB, YB),
        Project(float(X) / N, float(Y+1)/N, XC, YC)
    then
        (if X < grid_n then
            line(Bitmap, XA, YA, XB, YB, C, !IO)
        else
            true
        ),
        (if Y < grid_n, (X < grid_n ; ViewportType = type_1) then
            line(Bitmap, XA, YA, XC, YC, C, !IO)
        else
            true
        )
    else
        true
    ).



:- pred project_flat(float::in, float::in, float::in, float::in,
        float::in, float::in, int::out, int::out) is semidet.

project_flat(Left, Top, Right, Bottom, X0,Y0, X,Y) :-
    semidet_succeed,
    X = truncate_to_int(Left + X0 * (Right - Left)),
    Y = truncate_to_int(Top  + Y0 * (Bottom - Top)).



:- pred project_spherical(float::in, float::in, float::in, float::in,
        float::in, float::in, int::out, int::out) is semidet.

project_spherical(Left, Top, Right, Bottom, X0,Y0, X,Y) :-
    semidet_succeed,
    Ang = X0 * pi * 2.0,
    XSize = Right - Left,
    YSize = Bottom - Top,
    Size = min(XSize, YSize) / 2.0,
    FF = (if Y0 > 0.99 then 0.0 else 1.0 - Y0 * 0.9),
    DX = cos(Ang) * FF * Size,
    DY = sin(Ang) * FF * Size,
    X = truncate_to_int(DX + (Left + Right) / 2.0),
    Y = truncate_to_int(DY + (Top + Bottom) / 2.0).



:- pred project_tube(float::in, float::in, float::in, float::in,
        float::in, float::in, int::out, int::out) is semidet.

project_tube(Left, Top, Right, Bottom, X0,Y0, X,Y) :-
    semidet_succeed,
    Ang = X0 * pi * 2.0 + pi / 2.0,
    XSize = Right - Left,
    YSize = Bottom - Top,
    Size = min(XSize, YSize) / 2.0,
    X1 = cos(Ang),
    Y1 = sin(Ang),
    Z0 = 1.0 + (1.0 - Y0) * 8.0,
    (if Z0 = 0.0 then Z = 0.000001 else Z = Z0),
    X = truncate_to_int( X1/Z * Size + (Left + Right) / 2.0 ),
    Y = truncate_to_int( Y1/Z * Size + (Top + Bottom) / 2.0 ).



:- mutable(cylinder_matrix, maybe(matrix_f), no, ground, [untrailed]).

:- pred project_cylinder(float::in, float::in, float::in, float::in, float::in,
        float::in, float::in, int::out, int::out) is semidet.

project_cylinder(Left, Top, Right, Bottom, PlayerPos, X0,Y0, X,Y) :-
    promise_pure (
        semipure get_cylinder_matrix(MaybeCylinderMatrix),
        (
            MaybeCylinderMatrix = yes(CylinderMatrix)
        ;
            MaybeCylinderMatrix = no,
            get_z_rotate_matrix_f(M1, -64.0),
            qtranslate_matrix_f(M1, 0.0, 1.75, 0.0, M2),
            get_scaling_matrix_f(M3, 2.0, 1.0, 1.0),
            matrix_mul_f(M2, M3, CylinderMatrix),
            impure set_cylinder_matrix(yes(CylinderMatrix))
        )
    ),

    Ang = (X0 - PlayerPos) * pi * 2.0,
    XSize = Right - Left,
    YSize = Bottom - Top,
    Size = min(XSize, YSize) / 2.0,
    X1 = cos(Ang),
    Y1 = sin(Ang),
    Z1 = 1.0 + (1.0 - Y0) * 4.0,
    apply_matrix_f(CylinderMatrix, X1, Y1, Z1, X2, Y2, Z2),
    Y2 =< 1.5,
    X = truncate_to_int( X2/Z2 * Size + (Left + Right) / 2.0 ),
    Y = truncate_to_int( (Y2/Z2 * 2.0 - 1.0) * Size + (Top + Bottom) / 2.0 ).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
