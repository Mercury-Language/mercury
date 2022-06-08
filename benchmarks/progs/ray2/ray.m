% A port of one of the programs from
% http://www.ffconsultancy.com/languages/ray_tracer/index.html
%-----------------------------------------------------------------------------%

:- module ray.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module char.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module math.
:- import_module string.
:- import_module unit.

:- func delta = float.

% delta = sqrt(float.epsilon).
delta = 1.4901161193847656e-08.

:- type vec ---> vec(float, float, float).

:- func zero = vec.
zero = vec(0.0, 0.0, 0.0).

:- func float * vec = vec.
S * vec(X, Y, Z) = vec(S*X, S*Y, S*Z).

:- func vec + vec = vec.
vec(A,B,C) + vec(X,Y,Z) = vec(A+X, B+Y, C+Z).

:- func vec - vec = vec.
vec(A,B,C) - vec(X,Y,Z) = vec(A-X, B-Y, C-Z).

:- func vec `dot` vec = float.
vec(A,B,C) `dot` vec(X,Y,Z) = A*X + B*Y + C*Z.

:- func unitise(vec) = vec.
unitise(R) = (1.0 / sqrt(R `dot` R)) * R.

:- type scene
    --->    sphere(vec, float)
    ;       group(vec, float, list(scene)).

:- func ray_sphere(vec, vec, vec, float) = float.

ray_sphere(Orig, Dir, Centre, Radius) = Intersect :-
    V = Centre - Orig,
    B = V `dot` Dir,
    Disc0 = B * B - (V `dot` V) + Radius * Radius,
    (if Disc0 < 0.0 then
        Intersect = float.max
    else
        Disc = sqrt(Disc0),
        T2 = B + Disc,
        (if T2 < 0.0 then
            Intersect = float.max
        else
            T1 = B - Disc,
            Intersect = (if T1 > 0.0 then T1 else T2)
        )
    ).

:- pred intersect(vec::in, vec::in, scene::in, float::out, vec::out) is det.
:- pred intersect_2(vec::in, vec::in, scene::in, float::in, float::out,
    vec::in, vec::out) is det.

intersect(Orig, Dir, Scene, FirstDist, FirstVec) :-
    intersect_2(Orig, Dir, Scene, float.max, FirstDist, zero, FirstVec).

intersect_2(Orig, Dir, sphere(Centre, Radius), L0, L, V0, V) :-
    L1 = ray_sphere(Orig, Dir, Centre, Radius),
    (if L1 >= L0 then
        L = L0,
        V = V0
    else
        L = L1,
        V = unitise(Orig + L * Dir - Centre)
    ).
intersect_2(Orig, Dir, group(Centre, Radius, Scenes), L0, L, V0, V) :-
    (if ray_sphere(Orig, Dir, Centre, Radius) >= L0 then
        L = L0,
        V = V0
    else
        list.foldl2(intersect_2(Orig, Dir), Scenes, L0, L, V0, V)
    ).

:- func neg_light = vec.

neg_light = unitise(vec(1.0, 3.0, -2.0)).

:- func ray_trace(vec, scene) = float.

ray_trace(Dir, Scene) = V :-
    intersect(zero, Dir, Scene, Lambda, N),
    (if Lambda >= float.max then
        V = 0.0
    else
        G = N `dot` neg_light,
        (if G =< 0.0 then
            V = 0.0
        else
            P = Lambda * Dir + delta * N,
            intersect(P, neg_light, Scene, L, _),
            V = (if L >= float.max then G else 0.0)
        )
    ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    Level = 4,
    N = 512,
    aux(Level, N, !IO).

:- pred aux(int::in, int::in, io::di, io::uo) is det.

aux(Level, N, !IO) :-
    SS = 4,
    Scene = create(Level, 1.0, vec(0.0, -1.0, 4.0)),
    io.format("P5\n%d %d\n255\n", [i(N), i(N)], !IO),
    io.flush_output(!IO),
    loop(pixel(Scene, float(N), SS), N, !IO),
    io.flush_binary_output(!IO).

:- func create(int, float, vec) = scene.

create(Level, R, C @ vec(X, Y, Z)) = Scene :- 
    Obj = sphere(C, R),
    (if Level = 1 then
        Scene = Obj
    else
        Scene = group(C, 3.0 * R, Objs),
        R1 = 3.0 * R / sqrt(12.0),
        Objs = [
            Obj,
            Aux(-R1, -R1),
            Aux( R1, -R1),
            Aux(-R1,  R1),
            Aux( R1,  R1)
        ],
        Aux = (func(X1, Z1) = create(Level-1, 0.5*R, vec(X-X1, Y+R1, Z+Z1)))
    ).

%-----------------------------------------------------------------------------%

:- type row == bitmap.
:- mode row_in == bitmap_di.
:- mode row_out == bitmap_uo.

:- pred loop(pixel_pred::in(pixel_pred), int::in, io::di, io::uo) is det.

loop(F, N, !IO) :-
    loop_1(F, N, 0, N-1, !IO).

:- pred loop_1(pixel_pred::in(pixel_pred), int::in, int::in, int::in,
    io::di, io::uo) is det.

loop_1(F, N, Y, YEnd, !IO) :-
    (if Y >= YEnd then
        true
     else
        (
            Row0 = bitmap.init(8 * N),
            loop_2(F, Y, 0, N, Row0, Row),
            write_row(Row, !IO)
        &
            loop_1(F, N, Y + 1, YEnd, !IO)
        )
    ).

% unrolled
/*
loop_1(F, N, Y, YEnd, !IO) :-
    (if Y >= YEnd then
        true
     else
     if Y + 4 < YEnd then
        ( loop_2(F, Y + 0, 0, N, [], Left0), write_row(Left0, !IO)
        & loop_2(F, Y + 1, 0, N, [], Left1), write_row(Left1, !IO)
        & loop_2(F, Y + 2, 0, N, [], Left2), write_row(Left2, !IO)
        & loop_2(F, Y + 3, 0, N, [], Left3), write_row(Left3, !IO)
        & loop_1(F, N, Y + 4, YEnd, !IO)
        )
     else
     if Y + 3 < YEnd then
        ( loop_2(F, Y + 0, 0, N, [], Left0), write_row(Left0, !IO)
        & loop_2(F, Y + 1, 0, N, [], Left1), write_row(Left1, !IO)
        & loop_2(F, Y + 2, 0, N, [], Left2), write_row(Left2, !IO)
        & loop_1(F, N, Y + 3, YEnd, !IO)
        )
     else
     if Y + 2 < YEnd then
        ( loop_2(F, Y + 0, 0, N, [], Left0), write_row(Left0, !IO)
        & loop_2(F, Y + 1, 0, N, [], Left1), write_row(Left1, !IO)
        & loop_1(F, N, Y + 2, YEnd, !IO)
        )
     else
        ( loop_2(F, Y + 0, 0, N, [], Left), write_row(Left, !IO)
        & loop_1(F, N, Y + 1, YEnd, !IO)
        )
    ).
    */

:- pred write_row(row::in, io::di, io::uo) is det.

write_row(Row, !IO) :-
    bitmap.write_bitmap(Row, !IO).

/* list(int)
write_row([], !IO).
write_row([B | Bs], !IO) :-
    write_row(Bs, !IO),
    io.write_byte(B, !IO).
    */

:- pred loop_2(pixel_pred::in(pixel_pred), int::in, int::in, int::in,
    row::row_in, row::row_out) is det.

loop_2(F, Y, X, XEnd, !A) :-
    (if X >= XEnd then
        true
    else
        loop_3(F, Y, X, !A),
        loop_2(F, Y, X + 1, XEnd, !A)
    ).

:- pred loop_3(pixel_pred::in(pixel_pred), int::in, int::in,
    row::row_in, row::row_out) is det.

loop_3(F, Y, X, !A) :-
    F(float(X), float(Y), I),
    !A ^ unsafe_byte(X) := I.

:- type pixel_pred == (pred(float, float, int)).
:- inst pixel_pred == (pred(in, in, out) is det).

:- pred pixel(scene::in, float::in, int::in, float::in, float::in,
    int::out) is det.

pixel(Scene, N, SS, X0, Y0, I) :-
    X = X0 - N / 2.0,
    Y = (N - 1.0) / 2.0 - Y0,
    seq_loop(eye_ray(Scene, N, float(SS), X, Y), SS, 0.0, G),
    I = truncate_to_int(255.0 * G / float(SS * SS)).

%-----------------------------------------------------------------------------%

:- type loop_pred(A) == (pred(float, float, A, A)).
:- mode loop_pred1 == in(pred(in, in, in, out) is det).

:- pred seq_loop(loop_pred(A)::loop_pred1, int::in, A::in, A::out) is det.

seq_loop(F, N, !A) :-
    int.fold_up(seq_loop_2(F, N), 0, N-1, !A).

:- pred seq_loop_2(loop_pred(A)::loop_pred1, int::in, int::in,
    A::in, A::out) is det.

seq_loop_2(F, N, Y, !A) :-
    int.fold_up(seq_loop_3(F, Y), 0, N-1, !A).

:- pred seq_loop_3(loop_pred(A)::loop_pred1, int::in, int::in,
    A::in, A::out) is det.

seq_loop_3(F, Y, X, !A) :-
    F(float(X), float(Y), !A).

:- pred eye_ray(scene::in, float::in, float::in, float::in, float::in,
    float::in, float::in, float::in, float::out) is det.

eye_ray(Scene, N, SS, X, Y, DX, DY, G0, G) :-
    Dir = unitise(vec(X + DX/SS, Y + DY/SS, N)),
    G = G0 + ray_trace(Dir, Scene).

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
