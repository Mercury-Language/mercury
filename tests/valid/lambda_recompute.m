% Regression test.
% Date: Feb 27: 1998.
% Symptom: Software error: variable XX not found.
% Cause: mode_util:recompute_instmap_delta was not recomputing the instmap
% delta of the inner lambda goal after some duplicate call elimination was
% performed.
:- module lambda_recompute.
:- interface.

:- import_module io.

:- pred compile_display_list(display_list, io__state, io__state).
:- mode compile_display_list(out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, float, int, list, map, require, std_util, pair, string.

%------------------------------------------------------------------------------%

compile_display_list([mon(PPos, 1)|DList]) -->
	{ PPos = 1 - 1 },
	{ map__init(MapIKnow) },
	{ for(0, 1, (pred(X::in, in, out) is det -->
		for(0, 1, (pred(Y::in, in, out) is det -->
			( { search(MapIKnow, pos(X, Y), Place) } ->
				{ Place = place(Kind, _Flags, Obj) },
				(   (
					{ Kind \= 1 }
				    ;
				    	{ PPos = pos(X, Y) }
				    )
				->
					element(place(pos(X, Y), Kind)),
					( { Obj = [_N - T|_] } ->
						element(thing(pos(X, Y), T))
					;
						[]
					)
				;
					[]
				)
			;
				[]
			)
		))
	), [], DList) }.

:- pred element(element, display_list, display_list).
:- mode element(in, in, out) is det.

element(E, DL, [E|DL]).

:- type map	== map(pos, place).

:- type pos == pair(int).

:- type place
	--->	place(int, int, list(pair(int))).


:- func pos(int, int) = pos.
:- external(pos/2).


:- type map_i_know 
	--->	map(
			list(map),
			map,
			list(map)
		).

:- type element
	--->	place(pos, int)
	;	thing(pos, int)
	;	mon(pos, int)
	.

:- type display_list	==	list(element).

%------------------------------------------------------------------------------%


:- pred get_monst(int, int, io__state, io__state).
:- mode get_monst(in, out, di, uo) is det.

:- external(get_monst/4).


:- pred for(int, int, pred(int, T, T), T, T).
:- mode for(in, in, pred(in, in, out) is det, in, out) is det.
:- mode for(in, in, pred(in, in, out) is semidet, in, out) is semidet.
:- mode for(in, in, pred(in, di, uo) is det, di, uo) is det.


for(Min, Max, Pred, Acc0, Acc) :-
	( Min =< Max ->
		call(Pred, Min, Acc0, Acc1),
		for(Min+1, Max, Pred, Acc1, Acc)
	;
		Acc = Acc0
	).

