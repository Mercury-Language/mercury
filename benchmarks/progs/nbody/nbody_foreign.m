%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% n-body benchmark from The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Author: Julien Fischer <juliensf@csse.unimelb.edu.au>
%
% A variant of the nbody benchmark in nbody.m that uses mutable data
% structures defined in whatever the target language is.
%
% Compile with: -O5 --intermodule-optimization
%
% For C (gcc) add: --cflags="-O3 -mfpmath=sse -msse3"
%
%-----------------------------------------------------------------------------%

:- module nbody_foreign.
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
    new_body_ref(sun, Sun, !IO),
    new_body_ref(jupiter, Jupiter, !IO),
    new_body_ref(saturn, Saturn, !IO),
    new_body_ref(uranus, Uranus, !IO),
    new_body_ref(neptune, Neptune, !IO),
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

:- type body_ref.

:- pred new_body_ref(body::in, body_ref::out, io::di, io::uo) is det.

new_body_ref(Body, Ref, !IO) :-
    Body = body(X, Y, Z, Vx, Vy, Vz, Mass),
    new_body_ref_2(Ref, !IO),
    set_body_ref_field(Ref, 0, X, !IO),
    set_body_ref_field(Ref, 1, Y, !IO),
    set_body_ref_field(Ref, 2, Z, !IO),
    set_body_ref_field(Ref, 3, Vx, !IO),
    set_body_ref_field(Ref, 4, Vy, !IO),
    set_body_ref_field(Ref, 5, Vz, !IO),
    set_body_ref_field(Ref, 6, Mass, !IO).

:- pred new_body_ref_2(body_ref::out, io::di, io::uo) is det.

:- pred set_body_ref_field(body_ref::in, int::in, float::in,
    io::di, io::uo) is det.

:- pred get_body_ref_field(body_ref::in, int::in, float::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% C implementation.
%

:- pragma foreign_type("C", body_ref, "MR_Float *", [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    new_body_ref_2(Ref::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref = MR_GC_NEW_ARRAY(MR_Float, 7);
").

:- pragma foreign_proc("C",
    set_body_ref_field(Ref::in, I::in, Value::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref[I] = Value;
").

:- pragma foreign_proc("C",
    get_body_ref_field(Ref::in, I::in, Value::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = Ref[I];
").

%-----------------------------------------------------------------------------%
%
% Java implementation.
%

:- pragma foreign_type("Java", body_ref, "double []").

:- pragma foreign_proc("Java",
    new_body_ref_2(Ref::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref = new double[7];
").

:- pragma foreign_proc("Java",
    set_body_ref_field(Ref::in, I::in, Value::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref[I] = Value;
").

:- pragma foreign_proc("Java",
    get_body_ref_field(Ref::in, I::in, Value::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = Ref[I];
").

%-----------------------------------------------------------------------------%
%
% C# implementation.
%

:- pragma foreign_type("C#", body_ref, "double []").

:- pragma foreign_proc("C#",
    new_body_ref_2(Ref::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref = new double[7];
").

:- pragma foreign_proc("C#",
    set_body_ref_field(Ref::in, I::in, Value::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Ref[I] = Value;
").

:- pragma foreign_proc("C#",
    get_body_ref_field(Ref::in, I::in, Value::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Value = Ref[I];
").

%-----------------------------------------------------------------------------%

:- pragma inline(get_position/6).
:- pred get_position(body_ref::in, float::out, float::out, float::out,
    io::di, io::uo) is det.

get_position(BodyRef, X, Y, Z, !IO) :-
    get_body_ref_field(BodyRef, 0, X, !IO),
    get_body_ref_field(BodyRef, 1, Y, !IO),
    get_body_ref_field(BodyRef, 2, Z, !IO).

:- pragma inline(set_position/6).
:- pred set_position(body_ref::in, float::in, float::in, float::in,
    io::di, io::uo) is det.

set_position(BodyRef, X, Y, Z, !IO) :-
    set_body_ref_field(BodyRef, 0, X, !IO),
    set_body_ref_field(BodyRef, 1, Y, !IO),
    set_body_ref_field(BodyRef, 2, Z, !IO).

:- pragma inline(get_velocity/6).
:- pred get_velocity(body_ref::in, float::out, float::out, float::out,
    io::di, io::uo) is det.

get_velocity(BodyRef, Vx, Vy, Vz, !IO) :-
    get_body_ref_field(BodyRef, 3, Vx, !IO),
    get_body_ref_field(BodyRef, 4, Vy, !IO),
    get_body_ref_field(BodyRef, 5, Vz, !IO).

:- pragma inline(set_velocity/6).
:- pred set_velocity(body_ref::in, float::di, float::di, float::di,
    io::di, io::uo) is det.

set_velocity(BodyRef, Vx, Vy, Vz, !IO) :-
    set_body_ref_field(BodyRef, 3, Vx, !IO),
    set_body_ref_field(BodyRef, 4, Vy, !IO),
    set_body_ref_field(BodyRef, 5, Vz, !IO).

:- pragma inline(get_mass/4).
:- pred get_mass(body_ref::in, float::out, io::di, io::uo) is det.

get_mass(BodyRef, Mass, !IO) :-
    get_body_ref_field(BodyRef, 6, Mass, !IO).

%-----------------------------------------------------------------------------%

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

:- func nbody_foreign.pi = float.

nbody_foreign.pi = 3.141592653589793.

:- func solar_mass = (float::uo).

solar_mass = float(4) * nbody_foreign.pi * nbody_foreign.pi.

:- func days_per_year = float.

days_per_year = 365.24.

:- func jupiter = body.

jupiter = body(
    4.84143144246472090e+00,
    -1.16032004402742839e+00,
    -1.03622044471123109e-01,
    1.66007664274403694e-03 * days_per_year,
    7.69901118419740425e-03 * days_per_year,
    -6.90460016972063023e-05 * days_per_year,
    9.54791938424326609e-04 * solar_mass
).

:- func saturn = body.

saturn = body(
    8.34336671824457987e+00,
    4.12479856412430479e+00,
    -4.03523417114321381e-01,
    -2.76742510726862411e-03 * days_per_year,
    4.99852801234917238e-03 * days_per_year,
    2.30417297573763929e-05 * days_per_year,
    2.85885980666130812e-04 * solar_mass
).

:- func uranus = body.

uranus = body(
    1.28943695621391310e+01,
    -1.51111514016986312e+01,
    -2.23307578892655734e-01,
    2.96460137564761618e-03 * days_per_year,
    2.37847173959480950e-03 * days_per_year,
    -2.96589568540237556e-05 * days_per_year,
    4.36624404335156298e-05 * solar_mass
).

:- func neptune = body.

neptune = body(
    1.53796971148509165e+01,
    -2.59193146099879641e+01,
    1.79258772950371181e-01,
    2.68067772490389322e-03 * days_per_year,
    1.62824170038242295e-03 * days_per_year,
    -9.51592254519715870e-05 * days_per_year,
     5.15138902046611451e-05 * solar_mass
).

:- func sun = body.

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
:- end_module nbody_foreign.
%-----------------------------------------------------------------------------%
