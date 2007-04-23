%-----------------------------------------------------------------------------%
%
% A double pendulum simulator using Lagrangian Mechanics.
% Original Scheme code by David Wang, 2006-07-14.
% Mercury port by Peter Wang.
%
%-----------------------------------------------------------------------------%

:- module pendulum2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.
:- import_module allegro.bitmap.
:- import_module allegro.blit.
:- import_module allegro.color.
:- import_module allegro.graphics.
:- import_module allegro.init.
:- import_module allegro.keyboard.
:- import_module allegro.prim.
:- import_module allegro.timer.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module math.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type world
    --->    world(
                theta   :: float,
                dtheta  :: float,
                phi     :: float,
                dphi    :: float
            ).

:- func initial_world = world.
initial_world = world(2.0, 0.0, 2.0, 0.0).

:- func g = float.
g = 1.0.

:- func m1 = float.
m1 = 5.5.

:- func m2 = float.
m2 = 4.0.

:- func mm = float.
mm = m1 + m2.

:- func r1 = float.
r1 = 120.0.

:- func r2 = float.
r2 = 80.0.

:- func t = float.
t = 0.005.

:- func steps_per_frame = int.
steps_per_frame = 100.

:- func rest_msecs = int.
rest_msecs = 10.

:- func screen_w = int.
:- func screen_h = int.
screen_w = 640.
screen_h = 480.

:- func centre_x = int.
:- func centre_y = int.

centre_x = screen_w / 2.
centre_y = screen_h / 2.

%-----------------------------------------------------------------------------%

main(!IO) :-
    allegro_init(_, !IO),
    set_gfx_mode(gfx_autodetect, screen_w, screen_h, 0, 0, _, !IO),
    install_keyboard(_, !IO),
    install_timer(_, !IO),
    create_bitmap(screen_w, screen_h, MaybeDBuf, !IO),
    (
        MaybeDBuf = yes(DBuf),
        main_2(DBuf, initial_world, !IO),
        destroy_bitmap(DBuf, !IO)
    ;
        MaybeDBuf = no
    ).

:- pred main_2(bitmap::in, world::in, io::di, io::uo) is det.

main_2(DBuf, World0, !IO) :-
    World = simulate_steps(steps_per_frame, World0),

    clear_bitmap(DBuf, !IO),
    draw_world(DBuf, World, !IO),
    det_screen(Screen, !IO),
    blit(DBuf, Screen, 0, 0, 0, 0, screen_w, screen_h, !IO),

    rest(rest_msecs, !IO),

    keypressed(KP, !IO),
    (
        KP = yes
    ;
        KP = no,
        main_2(DBuf, World, !IO)
    ).

:- func simulate_steps(int, world) = world.

simulate_steps(N, World) =
    (if N = 0 then
        World
    else
        simulate_steps(N-1, simulate_step(World))
    ).

:- func simulate_step(world) = world.

simulate_step(World0) = World :-
    World0 = world(Theta0, DTheta0, Phi0, DPhi0),

    Psi = Theta0 - Phi0,
    A   =  mm * r1,
    B   =  m2 * r2 * cos(Psi),
    C   = (m2 * r2 * DPhi0 * DPhi0 * sin(Psi))
        + (mm *  g * sin(Theta0)),
    D   =  m2 * r1 * cos(Psi),
    E   =  m2 * r2,
    F   = (m2 *  g * sin(Phi0))
        - (m2 * r1 * DTheta0 * DTheta0 * sin(Psi)),

    DDTheta = ((B*F) - (E*C)) / ((E*A) - (B*D)),
    DDPhi   = ((A*F) - (D*C)) / ((D*B) - (A*E)),

    DTheta  = DTheta0 + t * DDTheta,
    Theta   = Theta0  + t * DTheta,
    DPhi    = DPhi0   + t * DDPhi,
    Phi     = Phi0    + t * DPhi,

    World = world(Theta, DTheta, Phi, DPhi).

:- pred draw_world(bitmap::in, world::in, io::di, io::uo) is det.

draw_world(DBuf, World, !IO) :-
    Theta = World ^ theta,
    Phi   = World ^ phi,

    X0 = centre_x,
    Y0 = centre_y,
    X1 = X0 + round_to_int(r1 * sin(Theta)),
    Y1 = Y0 + round_to_int(r1 * cos(Theta)),
    X2 = X1 + round_to_int(r2 * sin(Phi)),
    Y2 = Y1 + round_to_int(r2 * cos(Phi)),

    draw_pendulum(DBuf, X0, Y0, X1, Y1, m1, !IO),
    draw_pendulum(DBuf, X1, Y1, X2, Y2, m2, !IO).

:- pred draw_pendulum(bitmap::in, int::in, int::in, int::in, int::in,
    float::in, io::di, io::uo) is det.

draw_pendulum(Bitmap, X0, Y0, X1, Y1, Radius, !IO) :-
    makecol(255, 255, 255, White, !IO),
    makecol(255, 0, 0, Red, !IO),
    line(Bitmap, X0, Y0, X1, Y1, White, !IO),
    circlefill(Bitmap, X1, Y1, round_to_int(Radius), Red, !IO).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
