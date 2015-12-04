%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% n-body benchmark from The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
%
% Mercury version by Julien Fischer.
%
% Compile with: -O5 --intermodule-optimization --ctgc
%
%-----------------------------------------------------------------------------%

:- module nbody.
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
        NBodySystem0 = new_n_body_system,
        io.format("%.9f\n", [f(energy(NBodySystem0))], !IO),
        int.fold_up(advance(0.01), 1, N, NBodySystem0, NBodySystem),
        io.format("%.9f\n", [f(energy(NBodySystem))], !IO)
    else
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%

:- func new_n_body_system = (bodies::array_uo) is det.

new_n_body_system =
    offset_momentum(array([sun, jupiter, saturn, uranus, neptune])).

:- func offset_momentum(bodies::array_di) = (bodies::array_uo) is det.

offset_momentum(!.Bodies) = !:Bodies :-
    array.foldl3(offset_momentum_2, !.Bodies, 0.0, Px, 0.0, Py, 0.0, Pz),
    some [!Sun] (
        !:Sun = !.Bodies ^ unsafe_elem(0),
        !Sun ^ vx := -Px / solar_mass,
        !Sun ^ vy := -Py / solar_mass,
        !Sun ^ vz := -Pz / solar_mass,
        !Bodies ^ unsafe_elem(0) := !.Sun
    ).

:- pred offset_momentum_2(body::in, float::in, float::out,
    float::in, float::out, float::in, float::out) is det.

offset_momentum_2(Body, !Px, !Py, !Pz) :-
    !:Px = !.Px + Body ^ vx * Body ^ mass,
    !:Py = !.Py + Body ^ vy * Body ^ mass,
    !:Pz = !.Pz + Body ^ vz * Body ^ mass.

%-----------------------------------------------------------------------------%

:- func energy(bodies::array_ui) = (float::out) is det.

energy(Bodies) = Energy :-
    int.fold_up(energy_2(Bodies), 0, Bodies ^ max, 0.0, Energy).

:- pred energy_2(bodies::array_ui, int::in, float::in, float::out) is det.

energy_2(Bodies, I, !E) :-
    B = Bodies ^ unsafe_elem(I),
    !:E = !.E + 0.5 * B ^ mass *
        (B ^ vx * B ^ vx + B ^ vy * B ^ vy + B ^ vz * B ^ vz),
    int.fold_up(energy_3(Bodies, B), I + 1, Bodies ^ max, !E).

:- pred energy_3(bodies::array_ui, body::in, int::in,
    float::in, float::out) is det.

energy_3(Bodies, B, J, !E) :-
    B2 = Bodies ^ unsafe_elem(J),
    Dx = B ^ x - B2 ^ x,
    Dy = B ^ y - B2 ^ y,
    Dz = B ^ z - B2 ^ z,
    Distance = math.unchecked_sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
    !:E = !.E - (B ^ mass * B2 ^ mass) `unchecked_quotient` Distance.

%-----------------------------------------------------------------------------%

:- pred advance(float::in, int::in, bodies::array_di, bodies::array_uo) is det.

advance(Dt, _, !Bodies) :-
    NumBodies = !.Bodies ^ max,
    int.fold_up(advance_2(Dt), 0, NumBodies, !Bodies),
    int.fold_up(update_pos(Dt), 0, NumBodies, !Bodies).

:- pred advance_2(float::in, int::in,
    bodies::array_di, bodies::array_uo) is det.

advance_2(Dt, I, !Bodies) :-
    some [!B] (
        !:B = !.Bodies ^ unsafe_elem(I),
        int.fold_up2(advance_3(Dt), I + 1, !.Bodies ^ max, !B, !Bodies),
        !Bodies ^ unsafe_elem(I) := !.B
    ).

:- pred advance_3(float::in, int::in, body::in, body::out,
    bodies::array_di, bodies::array_uo) is det.

advance_3(Dt, J, !B, !Bodies) :-
    some [!B2] (
        !:B2 = !.Bodies ^ unsafe_elem(J),
        Dx = !.B ^ x - !.B2 ^ x,
        Dy = !.B ^ y - !.B2 ^ y,
        Dz = !.B ^ z - !.B2 ^ z,
        Distance = math.unchecked_sqrt(Dx * Dx + Dy * Dy + Dz * Dz),
        Mag = Dt `unchecked_quotient` (Distance * Distance * Distance),
        !B ^ vx := !.B ^ vx - Dx * !.B2 ^ mass * Mag,
        !B ^ vy := !.B ^ vy - Dy * !.B2 ^ mass * Mag,
        !B ^ vz := !.B ^ vz - Dz * !.B2 ^ mass * Mag,
        !B2 ^ vx := !.B2 ^ vx + Dx * !.B ^ mass * Mag,
        !B2 ^ vy := !.B2 ^ vy + Dy * !.B ^ mass * Mag,
        !B2 ^ vz := !.B2 ^ vz + Dz * !.B ^ mass * Mag,
        !Bodies ^ unsafe_elem(J) := !.B2
    ).

:- pred update_pos(float::in, int::in, bodies::array_di, bodies::array_uo)
    is det.

update_pos(Dt, I, !Bodies) :-
    some [!Body] (
        !:Body = !.Bodies ^ unsafe_elem(I),
        !Body ^ x := !.Body ^ x + Dt * !.Body ^ vx,
        !Body ^ y := !.Body ^ y + Dt * !.Body ^ vy,
        !Body ^ z := !.Body ^ z + Dt * !.Body ^ vz,
        !Bodies ^ unsafe_elem(I) := !.Body
    ).

%-----------------------------------------------------------------------------%

:- type bodies == array(body).

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

:- func nbody.pi = float.

nbody.pi = 3.141592653589793.

:- func solar_mass = float.

solar_mass = float(4) * nbody.pi * nbody.pi.

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
:- end_module nbody.
%-----------------------------------------------------------------------------%
