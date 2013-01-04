:- module monte.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module tausworthe3.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type shape == pred(float, float, float).
:- inst shape == (pred(in, in, in) is semidet).

:- type box
    --->    box(
		xmin	:: float,
		ymin	:: float,
		zmin	:: float,
		xmax	:: float,
		ymax	:: float,
		zmax	:: float
	    ).

:- type volume == float.

:- type rnd == tausworthe3.

%-----------------------------------------------------------------------------%

main(!IO) :-
	monte(box(-1.0, -1.0, -1.0, 1.0, 1.0, 1.0), 
        sphere, 5000000, Vol),
	io.print(Vol, !IO),
	io.nl(!IO).

:- pred monte(box, shape, int, volume).
:- mode monte(in, in(shape), in, out) is det.

monte(Box, Shape, NumPoints, Volume) :-
	init_tausworthe3 = Rnd0,
	monte2(Box, Shape, NumPoints, Rnd0, 0, Hits),
	Prop = float(Hits) / float(NumPoints),
	BoxVolume = (Box^xmax - Box^xmin) * (Box^ymax - Box^ymin) * (Box^zmax - Box^zmin),
	Volume = BoxVolume * Prop,
    trace [io(!IO)]
        io.format("Hits: %d, NumPoints: %d\n", [i(Hits), i(NumPoints)], !IO).

%:- pred monte(box, shape, int, int, volume).
%:- mode monte(in, in(shape), in, in, out) is det.
%
%monte(Box, Shape, NumProcs, NumPoints, Volume) :-
%	init_tausworthe3 = Rnd0,
%	parMonte(NumProcs, NumProcs, Box, Shape, NumPoints, Rnd0, HitsList),
%	% add up the ints in HitsList
%	sumHits(HitsList, 0, Hits),
%	Prop = float(Hits) / float(NumPoints),
%	BoxVolume = (Box^xmax - Box^xmin) * (Box^ymax - Box^ymin) * (Box^zmax - Box^zmin),
%	Volume = BoxVolume * Prop.
%
%:- pred sumHits(list(int)::in, int::in, int::out) is det.
%
%sumHits([], A, A).
%sumHits([H|T], A0, A) :-
%    sumHits(T, A0+H, A).
%
%:- pred parMonte(int, int, box, shape, int, tausworthe3, list(int)).
%:- mode parMonte(in, in, in, in(shape), in, in, out) is det.
%
%parMonte(N, Np, Box, Shape, Prec, Rnd0, HitsList) :-
%	( N > 1 ->
%	    rand_tausworthe3(Seed, Rnd0, Rnd1),
%	    seed_tausworthe3(Seed, Seed, Seed) = NewRnd,
%	    (
%		monte2(Box, Shape, Prec // Np, NewRnd, 0, Hits)
%	    & 
%		parMonte(N - 1, Np, Box, Shape, Prec, Rnd1, HitsList0)
%	    ),
%	    HitsList = [Hits|HitsList0]
%	; N = 1 ->
%	    % make sure we catch any extras
%	    Rest = Prec // Np + (Prec mod Np),
%	    monte2(Box, Shape, Rest, Rnd0, 0, Hits),
%	    HitsList = [Hits]
%	;
%	    error("parMonte: N < 1")
%	).

:- pred monte2(box, shape, int, rnd, int, int).
:- mode monte2(in, in(shape), in, in, in, out) is det.

monte2(Box, Shape, N, Rnd0, Hits0, Hits) :-
	( N > 0 ->
	    frange(Box^xmin, Box^xmax, X, Rnd0, Rnd1),
	    frange(Box^ymin, Box^ymax, Y, Rnd1, Rnd2),
	    frange(Box^zmin, Box^zmax, Z, Rnd2, Rnd3),
	    ( call(Shape, X, Y, Z) ->
		Hits1 = Hits0 + 1
	    ;
		Hits1 = Hits0
	    ),
	    monte2(Box, Shape, N - 1, Rnd3, Hits1, Hits)
	;
	    Hits = Hits0
	).

:- pred frange(float::in, float::in, float::out, rnd::in, rnd::out) is det.

frange(Min, Max, Num, !RS) :-
    rand_tausworthe3(Next, !RS),
    RandMax = int.max_int,
    Range = Max - Min,
    Num = Min + (Range * float(Next) / float(RandMax)).

:- pred sphere(float, float, float).
:- mode sphere(in, in, in) is semidet.

sphere(X, Y, Z) :-
	X * X + Y * Y + Z * Z =< 1.0.

:- pred torus(float, float, float, float).
:- mode torus(in, in, in, in) is semidet.

torus(R, X, Y, Z) :-
	sqr(sqrt(sqr(X) + sqr(Z)) - 1.0) + sqr(Y) =< sqr(R).

:- func sqr(float) = float.

sqr(X) = X*X.

:- pred cylinder(float, float, float).
:- mode cylinder(in, in, in) is semidet.

cylinder(X, Y, Z) :-
	sqr(X) + sqr(Y) =< 1.0,
	0.0 =< Z, Z =< 1.0.

:- pred planeZ(float, float, float).
:- mode planeZ(in, in, in) is semidet.

planeZ(_, _, Z) :-
	Z >= 0.0.

:- pred translate(shape, float, float, float, float, float, float).
:- mode translate(in(shape), in, in, in, in, in, in) is semidet.

translate(Shape, Dx, Dy, Dz, X, Y, Z) :-
	call(Shape, X - Dx, Y - Dy, Z - Dz).

:- pred scale(shape, float, float, float, float, float, float).
:- mode scale(in(shape), in, in, in, in, in, in) is semidet.

scale(Shape, Sx, Sy, Sz, X, Y, Z) :-
	call(Shape, X / Sx, Y / Sy, Z / Sz).

:- pred rotateY(shape, float, float, float, float).
:- mode rotateY(in(shape), in, in, in, in) is semidet.

rotateY(Shape, Theta, X0, Y0, Z0) :-
	Sin = sin(-Theta),
	Cos = cos(-Theta),
	X = Cos * X0 + Sin * Z0,
	Y = Y0,
	Z = -Sin * X0 + Cos * Z0,
	call(Shape, X, Y, Z).

:- pred union(shape, shape, float, float, float).
:- mode union(in(shape), in(shape), in, in, in) is semidet.

union(A, B, X, Y, Z) :-
	(
	  call(A, X, Y, Z)
	;
	  call(B, X, Y, Z)
	).

:- pred intersection(shape, shape, float, float, float).
:- mode intersection(in(shape), in(shape), in, in, in) is semidet.

intersection(A, B, X, Y, Z) :-
	call(A, X, Y, Z),
	call(B, X, Y, Z).

:- pred difference(shape, shape, float, float, float).
:- mode difference(in(shape), in(shape), in, in, in) is semidet.

difference(A, B, X, Y, Z) :-
	call(A, X, Y, Z),
	not call(B, X, Y, Z).

