%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: myers.m.
% Main author: bromage.
% 
% TO DO: We should implement the big-snake heuristic (a.k.a.
%	--speed-large-files).
%
% ALSO TO DO: Gene Myers et al have since produced another algorithm
%	which takes O(NP) time where P is the number of deletions in
%	the edit script.  If the `too expensive' heuristic can be
%	retro-fitted onto that algorithm easily enough, we should try
%	out this algorithm and see how fast it runs.  In theory, we
%	should be looking at about a 2x speedup.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module myers.
:- interface.

:- import_module difftype.

:- import_module array.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred diff_by_myers(array(int), array(int), diff, io__state, io__state).
:- mode diff_by_myers(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

% The basic algorithm is described in:
%	"An O(ND) Difference Algorithm and its Variations", Eugene Myers,
%	Algorithmica Vol. 1 No. 2, 1986, pp. 251-266.
%
% This uses the variation in section 4b.

diff_by_myers(FileX, FileY, Diff) -->
	globals__io_lookup_bool_option(minimal, Minimal),
	{
		array__size(FileX, SizeX),
		array__size(FileY, SizeY),
		SizeMax = SizeX + SizeY + 3,
		DOffset = SizeY + 1,

		% If we don't insist on --minimal, calculate the
		% approximate square root of the input size for
		% the "too expensive" heuristic.  The effect of
		% this is to limit the amount of work to about
		% O(n ** (1.5 log n)) at the expense of finding a
		% possibly non-minimal diff.

		( Minimal = yes,
			Heur = none
		; Minimal = no,
			int__log2(SizeMax, SizeLog2),
			int__max(minimum_too_expensive, 1 << (SizeLog2 // 2),
					SizeHeuristic),
			Heur = too_expensive(SizeHeuristic)
		),

			% Fill the arrays with nondescript numbers which
			% the algorithm shouldn't produce.  (For debugging
			% purposes.)
		array__init(SizeMax, -65537, Fwd),
		array__init(SizeMax, -65537, Bwd),
		myers__bsearch(DOffset, FileX, FileY, 0, SizeX, 0, SizeY,
			Heur, Fwd, _, Bwd, _, [], Diff)
	}.

	% XXX This lower bound is a guess.  Need to do some measurements
	%     to see if it's good or not.
:- func minimum_too_expensive = int.
minimum_too_expensive = 256.

:- pred myers__bsearch(int, array(int), array(int), int, int, int, int, heur,
		array(int), array(int), array(int), array(int),
		diff, diff).
:- mode myers__bsearch(in, in, in, in, in, in, in, in,
		array_di, array_uo, array_di, array_uo,
		in, out) is det.

myers__bsearch(DOffset, FileX, FileY, Xlow0, Xhigh0, Ylow0, Yhigh0, Heur,
			Fwd0, Fwd, Bwd0, Bwd, Diff0, Diff) :-
	myers__scan_forward(FileX, FileY, Xhigh0, Yhigh0,
		Xlow0, Xlow, Ylow0, Ylow),
	myers__scan_backward(FileX, FileY, Xlow, Ylow,
		Xhigh0, Xhigh, Yhigh0, Yhigh),

	(
		( Xlow >= Xhigh
		; Ylow >= Yhigh
		)
	->
		Fwd = Fwd0, Bwd = Bwd0,
		difftype__add_edit(Xlow - Xhigh, Ylow - Yhigh, Diff0, Diff)
	;
		myers__find_middle(DOffset, FileX, FileY,
			Xlow, Xhigh, Ylow, Yhigh, Heur,
			Fwd0, Fwd1, Bwd0, Bwd1, Xmid, Ymid, Cost,
			LeftHeur - RightHeur),
		(
			Cost > 0
		->
			myers__bsearch(DOffset, FileX, FileY,
				Xmid, Xhigh, Ymid, Yhigh, LeftHeur,
				Fwd1, Fwd2, Bwd1, Bwd2, Diff0, Diff1),
			myers__bsearch(DOffset, FileX, FileY,
				Xlow, Xmid, Ylow, Ymid, RightHeur,
				Fwd2, Fwd, Bwd2, Bwd, Diff1, Diff)
		;
			error("myers__bsearch")
		)
	).

:- type myers_constants
	--->	constants(
			int,		% DOffset
			array(int),	% X
			array(int),	% Y
			int,		% Xlow
			int,		% Xhigh
			int,		% Ylow
			int,		% Yhigh
			int,		% Dmin
			int,		% Dmax
			bool,		% DeltaOdd
			heur		% "Too expensive" heuristic.
		).

:- type heur
	--->	too_expensive(int)
	;	none.

	% The best part about this algorithm is: We don't actually
	% need to find the middle of the diff.  We only have to find
	% an estimate to it.  If we don't find the exact middle,
	% we will have a correct diff, but it won't necessarily be
	% minimal.
:- pred myers__find_middle(int, array(int), array(int), pos, pos, pos, pos,
		heur,
		array(int), array(int), array(int), array(int),
		pos, pos, int, pair(heur)).
:- mode myers__find_middle(in, in, in, in, in, in, in, in,
		array_di, array_uo, array_di, array_uo,
		out, out, out, out) is det.

myers__find_middle(DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh, Heur,
		Fwd0, Fwd, Bwd0, Bwd, Xmid, Ymid, Cost, HeurReq) :-

	Dmin = Xlow - Yhigh,
	Dmax = Xhigh - Ylow,

	Fmid = Xlow - Ylow,
	array__set(Fwd0, Fmid + DOffset, Xlow, Fwd1),
	Bmid = Xhigh - Yhigh,
	array__set(Bwd0, Bmid + DOffset, Xhigh, Bwd1),

	( 1 = (Fmid - Bmid) /\ 1 ->
		DeltaOdd = yes
	;
		DeltaOdd = no
	),

	Constants = constants(
		DOffset, FileX, FileY, Xlow, Xhigh, Ylow, Yhigh,
		Dmin, Dmax, DeltaOdd, Heur
	),

	myers__find_middle_2(Constants, Fwd1, Fwd, Bwd1, Bwd,
		Fmid, Fmid, Bmid, Bmid, 1, Cost, Xmid - Ymid, HeurReq).


:- pred myers__find_middle_2(myers_constants,
		array(int), array(int), array(int), array(int),
		int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers__find_middle_2(in, array_di, array_uo, array_di, array_uo,
		in, in, in, in, in, out, out, out) is det.

myers__find_middle_2(Constants, Fwd0, Fwd, Bwd0, Bwd,
		Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
	Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _, _),
	( Fmin > Dmin ->
		Fmin1 = Fmin - 1,
		array__set(Fwd0, Fmin1 + DOffset - 1, -1, Fwd1)
	;
		Fmin1 = Fmin + 1,
		Fwd1 = Fwd0
	),
	( Fmax < Dmax ->
		Fmax1 = Fmax + 1,
		array__set(Fwd1, Fmax1 + DOffset + 1, -1, Fwd2)
	;
		Fmax1 = Fmax - 1,
		Fwd2 = Fwd1
	),
	myers__find_forward_reaching_path(Constants, Fwd2, Fwd, Bwd0, Bwd,
		Fmin1, Fmax1, Bmin, Bmax, Fmax1, Cost0, Cost, Mid, HeurReq).


:- pred myers__find_forward_reaching_path(myers_constants,
		array(int), array(int), array(int), array(int),
		int, int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers__find_forward_reaching_path(in, array_di, array_uo,
		array_di, array_uo, in, in, in, in, in, in, out, out, out)
				is det.

myers__find_forward_reaching_path(Constants, Fwd0, Fwd, Bwd0, Bwd,
		Fmin, Fmax, Bmin, Bmax, SearchCost, Cost0, Cost, Mid,
		HeurReq) :-
	( SearchCost < Fmin ->
		Constants = constants(DOffset, _, _, _, _, _, _, Dmin, Dmax, _,
					_),
		int__max_int(MaxInt),
		( Bmin > Dmin ->
			Bmin1 = Bmin - 1,
			array__set(Bwd0, Bmin1 + DOffset - 1, MaxInt, Bwd1)
		;
			Bmin1 = Bmin + 1,
			Bwd1 = Bwd0
		),
		( Bmax < Dmax ->
			Bmax1 = Bmax + 1,
			array__set(Bwd1, Bmax1 + DOffset + 1, MaxInt, Bwd2)
		;
			Bmax1 = Bmax - 1,
			Bwd2 = Bwd1
		),
		myers__find_backward_reaching_path(Constants,
			Fwd0, Fwd, Bwd2, Bwd, Fmin, Fmax, Bmin1, Bmax1,
			Bmax1, Cost0, Cost, Mid, HeurReq)
	;
		Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
		array__lookup(Fwd0, SearchCost + DOffset - 1, Tlo),
		array__lookup(Fwd0, SearchCost + DOffset + 1, Thi),
		( Tlo >= Thi ->
			X0 = Tlo + 1
		;
			X0 = Thi
		),
		Y0 = X0 - SearchCost,
		Constants = constants(_, FileX, FileY, _, Xhigh, _, Yhigh,
			_, _, _, _),
		myers__scan_forward(FileX, FileY, Xhigh, Yhigh, X0, X, Y0, Y),
		array__set(Fwd0, SearchCost + DOffset, X, Fwd1),

		Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
		(
			DeltaOdd = yes,
			Bmin =< SearchCost,
			SearchCost =< Bmax,
			array__lookup(Bwd0, SearchCost + DOffset, BB),
			BB =< X
		->
			Mid = X - Y,
			Cost = 2 * Cost0 + 1,
			Fwd = Fwd1,
			Bwd = Bwd0,
			HeurReq = none - none
		;
			myers__find_forward_reaching_path(Constants,
				Fwd1, Fwd, Bwd0, Bwd, Fmin, Fmax, Bmin, Bmax,
				SearchCost - 2, Cost0, Cost, Mid, HeurReq)
		)
	).


:- pred myers__find_backward_reaching_path(myers_constants,
		array(int), array(int), array(int), array(int),
		int, int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers__find_backward_reaching_path(in, array_di, array_uo,
		array_di, array_uo, in, in, in, in, in, in,
		out, out, out) is det.

myers__find_backward_reaching_path(Constants, Fwd0, Fwd, Bwd0, Bwd,
		Fmin, Fmax, Bmin, Bmax, SearchCost, Cost0, Cost, Mid,
		HeurReq) :-
	( SearchCost < Bmin ->
		myers__try_heuristics(Constants, Fwd0, Fwd, Bwd0, Bwd,
			Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq)
	;
		Constants = constants(DOffset, _, _, _, _, _, _, _, _, _, _),
		array__lookup(Bwd0, SearchCost + DOffset - 1, Tlo),
		array__lookup(Bwd0, SearchCost + DOffset + 1, Thi),
		( Tlo < Thi ->
			X0 = Tlo
		;
			X0 = Thi - 1
		),
		Y0 = X0 - SearchCost,
		Constants = constants(_, FileX, FileY, Xlow, _, Ylow, _,
			_, _, _, _),
		myers__scan_backward(FileX, FileY, Xlow, Ylow, X0, X, Y0, Y),
		array__set(Bwd0, SearchCost + DOffset, X, Bwd1),

		Constants = constants(_, _, _, _, _, _, _, _, _, DeltaOdd, _),
		(
			DeltaOdd = no,
			Fmin =< SearchCost,
			SearchCost =< Fmax,
			array__lookup(Fwd0, SearchCost + DOffset, FF),
			X =< FF
		->
			Mid = X - Y,
			Cost = 2 * Cost0,
			Fwd = Fwd0,
			Bwd = Bwd1,
			HeurReq = none - none
		;
			myers__find_backward_reaching_path(Constants,
				Fwd0, Fwd, Bwd1, Bwd, Fmin, Fmax, Bmin, Bmax,
				SearchCost - 2, Cost0, Cost, Mid, HeurReq)
		)
	).


	% Try applying some heuristics to see if we can avoid some work.
:- pred myers__try_heuristics(myers_constants,
		array(int), array(int), array(int), array(int),
		int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers__try_heuristics(in, array_di, array_uo,
		array_di, array_uo, in, in, in, in, in, out, out, out) is det.

myers__try_heuristics(Constants, Fwd0, Fwd, Bwd0, Bwd,
		Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
	Constants = constants(_, _, _, _, _, _, _, _, _, _, Heur),
	(
		Heur = too_expensive(Cutoff),
		Cost0 >= Cutoff
	->
			% If we've done too much work, stop here.
		Fwd = Fwd0, Bwd = Bwd0,
		myers__too_expensive_heuristic(Constants, Fwd, Bwd,
			Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq)
	;
		% Can't apply heuristic, so try looking for a diff of size
		% Cost0 + 1.

		myers__find_middle_2(Constants, Fwd0, Fwd, Bwd0, Bwd,
			Fmin, Fmax, Bmin, Bmax, Cost0 + 1, Cost, Mid, HeurReq)
	).

%-----------------------------------------------------------------------------%

	% We've done too much work, so make our best guess.
:- pred myers__too_expensive_heuristic(myers_constants, array(int), array(int),
		int, int, int, int, int, int, pair(pos), pair(heur)).
:- mode myers__too_expensive_heuristic(in, array_ui, array_ui,
		in, in, in, in, in, out, out, out) is det.

myers__too_expensive_heuristic(Constants, Fwd, Bwd,
		Fmin, Fmax, Bmin, Bmax, Cost0, Cost, Mid, HeurReq) :-
	% Find the best diagonal that we can, take the end of
	% that diagonal as the "middle".  Do not apply the
	% heuristic recursively to that best diagonal.

	Constants = constants(DOffset, _, _, Xlow, Xhigh, Ylow, Yhigh,
			_, _, _, Heur),

		% Find the best forward diagonal.
	myers__find_best_forward_diagonal(Fmax, Fmin, Fwd,
			Xhigh, Yhigh, DOffset, -1, FXYBest, 0, FXBest),

		% Find the best backward diagonal.
	int__max_int(MaxInt),
	myers__find_best_backward_diagonal(Bmax, Bmin, Bwd,
			Xlow, Ylow, DOffset, MaxInt, BXYBest, 0, BXBest),

		% Choose which of these diagonals is the better one
		% and return that as the "middle" point.
	(
		FXYBest - (Xhigh + Yhigh) < (Xlow + Ylow) - BXYBest
	->
		Xmid = FXBest,
		Ymid = FXYBest - FXBest,
		HeurReq = none - Heur
	;
		Xmid = BXBest,
		Ymid = BXYBest - BXBest,
		HeurReq = Heur - none
	),
	Mid = Xmid - Ymid,
	Cost = 2 * Cost0 - 1.

:- pred myers__find_best_forward_diagonal(int, int, array(int), int, int, int,
			int, int, int, int).
:- mode myers__find_best_forward_diagonal(in, in, array_ui, in, in, in,
			in, out, in, out) is det.

myers__find_best_forward_diagonal(D, Fmin, Fwd, Xhigh, Yhigh, DOffset,
			FXYBest0, FXYBest, FXBest0, FXBest) :-
	( D < Fmin ->
		FXYBest = FXYBest0,
		FXBest = FXBest0
	;
		array__lookup(Fwd, D + DOffset, X0),
		int__min(Xhigh, X0, X1),
		Y0 = X1 - D,

		( Yhigh < Y0 ->
			X = Yhigh + D,
			Y = Yhigh
		;
			X = X1,
			Y = Y0
		),

		NewFXY = X + Y,
		( FXYBest0 < NewFXY ->
			myers__find_best_forward_diagonal(D - 2, Fmin, Fwd,
				Xhigh, Yhigh, DOffset, NewFXY, FXYBest,
				X, FXBest)
		;
			myers__find_best_forward_diagonal(D - 2, Fmin, Fwd,
				Xhigh, Yhigh, DOffset, FXYBest0, FXYBest,
				FXBest0, FXBest)
		)
	).

:- pred myers__find_best_backward_diagonal(int, int, array(int), int, int, int,
			int, int, int, int).
:- mode myers__find_best_backward_diagonal(in, in, array_ui, in, in, in,
			in, out, in, out) is det.

myers__find_best_backward_diagonal(D, Bmin, Bwd, Xlow, Ylow, DOffset,
			BXYBest0, BXYBest, BXBest0, BXBest) :-
	( D < Bmin ->
		BXYBest = BXYBest0,
		BXBest = BXBest0
	;
		array__lookup(Bwd, D + DOffset, X0),
		int__max(Xlow, X0, X1),
		Y0 = X1 - D,

		( Y0 < Ylow ->
			X = Ylow + D,
			Y = Ylow
		;
			X = X1,
			Y = Y0
		),

		NewBXY = X + Y,
		( NewBXY < BXYBest0 ->
			myers__find_best_backward_diagonal(D - 2, Bmin, Bwd,
				Xlow, Ylow, DOffset, NewBXY, BXYBest,
				X, BXBest)
		;
			myers__find_best_backward_diagonal(D - 2, Bmin, Bwd,
				Xlow, Ylow, DOffset, BXYBest0, BXYBest,
				BXBest0, BXBest)
		)
	).

%-----------------------------------------------------------------------------%

	% Travel forwards along a snake.
:- pred myers__scan_forward(array(int), array(int), int, int,
		int, int, int, int).
:- mode myers__scan_forward(in, in, in, in, in, out, in, out) is det.

myers__scan_forward(FileX, FileY, Xhigh, Yhigh, Xlow0, Xlow, Ylow0, Ylow) :-
	(
		Xlow0 < Xhigh,
		Ylow0 < Yhigh,
		array__lookup(FileX, Xlow0, Line),
		array__lookup(FileY, Ylow0, Line)
	->
		myers__scan_forward(FileX, FileY, Xhigh, Yhigh,
			Xlow0 + 1, Xlow, Ylow0 + 1, Ylow)
	;
		Xlow = Xlow0, Ylow = Ylow0
	).


	% Travel backwards along a snake.
:- pred myers__scan_backward(array(int), array(int), int, int,
		int, int, int, int).
:- mode myers__scan_backward(in, in, in, in, in, out, in, out) is det.

myers__scan_backward(FileX, FileY, Xlow, Ylow, Xhigh0, Xhigh, Yhigh0, Yhigh) :-
	(
		Xhigh0 > Xlow,
		Yhigh0 > Ylow,
		array__lookup(FileX, Xhigh0 - 1, Line),
		array__lookup(FileY, Yhigh0 - 1, Line)
	->
		myers__scan_backward(FileX, FileY, Xlow, Ylow,
			Xhigh0 - 1, Xhigh, Yhigh0 - 1, Yhigh)
	;
		Xhigh = Xhigh0, Yhigh = Yhigh0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
