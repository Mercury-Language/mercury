%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% n-body benchmark from The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% A variant of the nbody benchmark in nbody.m that uses stores and references.
%
% Compile with: -O5 --intermodule-optimization --ctgc
%
%-----------------------------------------------------------------------------%

:- module nbody_ref.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module string.
:- import_module store.

:- pragma require_feature_set([double_prec_float]).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [NStr], string.to_int(NStr, N) then
        new_n_body_system(NBodySystem, !IO),
        energy(NBodySystem, InitialEnergy, !IO),
        io.format("%.9f\n", [f(InitialEnergy)], !IO),
        int.fold_up(advance(0.01, NBodySystem), 1, N, !IO),
        energy(NBodySystem, FinalEnergy, !IO),
        io.format("%.9f\n", [f(FinalEnergy)], !IO)
      else
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred new_n_body_system(bodies::out, io::di, io::uo) is det.

new_n_body_system(NBodySystem, !IO) :-
    store.new_ref(sun, Sun, !IO),
    store.new_ref(jupiter, Jupiter, !IO),
    store.new_ref(saturn, Saturn, !IO),
    store.new_ref(uranus, Uranus, !IO),
    store.new_ref(neptune, Neptune, !IO),
    NBodySystem = array([Sun, Jupiter, Saturn, Uranus, Neptune]),
    offset_momentum(NBodySystem, !IO).

:- pred offset_momentum(bodies::array_ui, io::di, io::uo)
    is det.

offset_momentum(Bodies, !IO) :-
    array.foldl4(offset_momentum_2, Bodies, 0.0, Px, 0.0, Py, 0.0, Pz, !IO),
    Sun = Bodies ^ unsafe_elem(0),
    set_velocity(Sun, -Px / solar_mass, -Py / solar_mass,
        -Pz / solar_mass, !IO).

:- pred offset_momentum_2(body_ref::in, float::in, float::out,
    float::in, float::out, float::in, float::out, io::di, io::uo) is det.

offset_momentum_2(Body, !Px, !Py, !Pz, !IO) :-
    get_velocity(Body, Vx, Vy, Vz, !IO),
    get_mass(Body, Mass, !IO),
    !:Px = !.Px + Vx * Mass,
    !:Py = !.Py + Vy * Mass,
    !:Pz = !.Pz + Vz * Mass.

%-----------------------------------------------------------------------------%

:- pred energy(bodies::array_ui, float::out, io::di, io::uo) is det.

energy(Bodies, Energy, !IO) :-
    int.fold_up2(energy_2(Bodies), 0, Bodies ^ max, 0.0, Energy, !IO).

:- pred energy_2(bodies::array_ui, int::in, float::in, float::out,
    io::di, io::uo) is det.

energy_2(Bodies, I, !E, !IO) :-
    B = Bodies ^ unsafe_elem(I),
    get_mass(B, Mass, !IO),
    get_velocity(B, Vx, Vy, Vz, !IO),
    !:E = !.E + 0.5 * Mass * (Vx * Vx + Vy * Vy + Vz * Vz),
    int.fold_up2(energy_3(Bodies, B), I + 1, Bodies ^ max, !E, !IO).

:- pred energy_3(bodies::array_ui, body_ref::in, int::in,
    float::in, float::out, io::di, io::uo) is det.
    
energy_3(Bodies, B, J, !E, !IO) :-
    B2 = Bodies ^ unsafe_elem(J),
    get_position(B, Bx, By, Bz, !IO),
    get_position(B2, B2x, B2y, B2z, !IO),
    Dx = Bx - B2x,
    Dy = By - B2y,
    Dz = Bz - B2z,
    Distance = math.unchecked_sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
    get_mass(B, Bmass, !IO),
    get_mass(B2, B2mass, !IO),
    !:E = !.E - (Bmass * B2mass) `unchecked_quotient` Distance.

%-----------------------------------------------------------------------------%

:- pred advance(float::in, bodies::array_ui, int::in, io::di, io::uo) is det.

advance(Dt, Bodies, _, !IO) :-
    NumBodies = Bodies ^ max,
    int.fold_up(advance_2(Dt, Bodies), 0, NumBodies, !IO),
    int.fold_up(update_pos(Dt, Bodies), 0, NumBodies, !IO).

:- pred advance_2(float::in, bodies::array_ui, int::in, io::di, io::uo) is det.

advance_2(Dt, Bodies, I, !IO) :-
    B = Bodies ^ unsafe_elem(I),
    int.fold_up(advance_3(Dt, Bodies, B), I + 1, Bodies ^ max, !IO).

:- pred advance_3(float::in, bodies::array_ui, body_ref::in, int::in,
    io::di, io::uo) is det.

advance_3(Dt, Bodies, B, J, !IO) :-
    B2 = Bodies ^ unsafe_elem(J),
    get_position(B, Bx, By, Bz, !IO),
    get_position(B2, B2x, B2y, B2z, !IO),
    Dx = Bx - B2x,
    Dy = By - B2y,
    Dz = Bz - B2z,
    Distance = math.unchecked_sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
    Mag = Dt `unchecked_quotient` (Distance * Distance * Distance),
    get_mass(B, Bmass, !IO),
    get_mass(B2, B2mass, !IO),
    some [!Bvx, !Bvy, !Bvz, !B2vx, !B2vy, !B2vz] (
        get_velocity(B, !:Bvx, !:Bvy, !:Bvz, !IO),
        get_velocity(B2, !:B2vx, !:B2vy, !:B2vz, !IO),
        !:Bvx = !.Bvx - Dx * B2mass * Mag,
        !:Bvy = !.Bvy - Dy * B2mass * Mag,
        !:Bvz = !.Bvz - Dz * B2mass * Mag,
        !:B2vx = !.B2vx + Dx * Bmass * Mag,
        !:B2vy = !.B2vy + Dy * Bmass * Mag,
        !:B2vz = !.B2vz + Dz * Bmass * Mag,
        set_velocity(B, !.Bvx, !.Bvy, !.Bvz, !IO),
        set_velocity(B2, !.B2vx, !.B2vy, !.B2vz, !IO)
    ).

:- pred update_pos(float::in, bodies::array_ui, int::in, io::di, io::uo) is det.

update_pos(Dt, Bodies, I, !IO) :-
    Body = Bodies ^ unsafe_elem(I),
    get_position(Body, X0, Y0, Z0, !IO),
    get_velocity(Body, Vx, Vy, Vz, !IO),
    X = X0 + Dt * Vx,
    Y = Y0 + Dt * Vy,
    Z = Z0 + Dt * Vz,
    set_position(Body, X, Y, Z, !IO).

%-----------------------------------------------------------------------------%

:- type bodies == array(body_ref).

:- type body_ref == generic_ref(body, io).

:- pragma inline(pred(get_position/6)).
:- pred get_position(body_ref::in, float::out, float::out, float::out,
    io::di, io::uo) is det.

get_position(BodyRef, X, Y, Z, !IO) :-
    store.arg_ref(BodyRef, 0, XRef, !IO),
    store.copy_ref_value(XRef, X, !IO),
    store.arg_ref(BodyRef, 1, YRef, !IO),
    store.copy_ref_value(YRef, Y, !IO),
    store.arg_ref(BodyRef, 2, ZRef, !IO),
    store.copy_ref_value(ZRef, Z, !IO).

:- pragma inline(pred(set_position/6)).
:- pred set_position(body_ref::in, float::di, float::di, float::di,
    io::di, io::uo) is det.

set_position(BodyRef, X, Y, Z, !IO) :-
    store.arg_ref(BodyRef, 0, XRef, !IO),
    store.set_ref_value(XRef, X, !IO),
    store.arg_ref(BodyRef, 1, YRef, !IO),
    store.set_ref_value(YRef, Y, !IO),
    store.arg_ref(BodyRef, 2, ZRef, !IO),
    store.set_ref_value(ZRef, Z, !IO).

:- pragma inline(pred(get_velocity/6)).
:- pred get_velocity(body_ref::in, float::out, float::out, float::out,
    io::di, io::uo) is det.

get_velocity(BodyRef, Vx, Vy, Vz, !IO) :-
    store.arg_ref(BodyRef, 3, VxRef, !IO),
    store.copy_ref_value(VxRef, Vx, !IO),
    store.arg_ref(BodyRef, 4, VyRef, !IO),
    store.copy_ref_value(VyRef, Vy, !IO),
    store.arg_ref(BodyRef, 5, VzRef, !IO),
    store.copy_ref_value(VzRef, Vz, !IO).

:- pragma inline(pred(set_velocity/6)).
:- pred set_velocity(body_ref::in, float::di, float::di, float::di,
    io::di, io::uo) is det.

set_velocity(BodyRef, Vx, Vy, Vz, !IO) :-
    store.arg_ref(BodyRef, 3, VxRef, !IO),
    store.set_ref_value(VxRef, Vx, !IO),
    store.arg_ref(BodyRef, 4, VyRef, !IO),
    store.set_ref_value(VyRef, Vy, !IO),
    store.arg_ref(BodyRef, 5, VzRef, !IO),
    store.set_ref_value(VzRef, Vz, !IO).

:- pragma inline(pred(get_mass/4)).
:- pred get_mass(body_ref::in, float::out, io::di, io::uo) is det.

get_mass(BodyRef, Mass, !IO) :-
    store.arg_ref(BodyRef, 6, MassRef, !IO),
    store.copy_ref_value(MassRef, Mass, !IO).

:- type body
    --->    body(
                x    :: float,
                y    :: float,
                z    :: float,
                vx   :: float,
                vy   :: float,
                vz   :: float,
                mass :: float
            ).

:- func nbody_ref.pi = float.

nbody_ref.pi = 3.141592653589793.

:- func solar_mass = (float::uo).

solar_mass = float(4) * nbody_ref.pi * nbody_ref.pi.

:- func days_per_year = float.

days_per_year = 365.24.

:- func jupiter = (body::uo).

jupiter = body(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03 * days_per_year,
    7.69901118419740425e-03 * days_per_year,
    -6.90460016972063023e-05 * days_per_year,
    9.54791938424326609e-04 * solar_mass
).

:- func saturn = (body::uo).

saturn = body(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03 * days_per_year,
    4.99852801234917238e-03 * days_per_year,
    2.30417297573763929e-05 * days_per_year,
    2.85885980666130812e-04 * solar_mass
).

:- func uranus = (body::uo).

uranus = body(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03 * days_per_year,
    2.37847173959480950e-03 * days_per_year,
    -2.96589568540237556e-05 * days_per_year,
    4.36624404335156298e-05 * solar_mass
).

:- func neptune = (body::uo).

neptune = body(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03 * days_per_year,
    1.62824170038242295e-03 * days_per_year,
    -9.51592254519715870e-05 * days_per_year,
     5.15138902046611451e-05 * solar_mass
).

:- func sun = (body::uo).

sun = body(
    0.0,
    0.0,
    0.0,
    0.0,
    0.0,
    0.0,
    solar_mass
).

%-----------------------------------------------------------------------------%
:- end_module nbody_ref.
%-----------------------------------------------------------------------------%
