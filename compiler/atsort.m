%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Atsort.m - approximate topological sort. The sort is approximate because
% it must work even on data that has cycles.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module libs__atsort.

:- interface.
:- import_module map, list.

:- type relmap(T) == map(T, list(T)).

:- pred atsort(relmap(T), relmap(T), relmap(T), relmap(T), list(T),
	list(list(T))).
:- mode atsort(in, in, in, in, in, out) is det.

:- pred atsort__closure(list(T), relmap(T), list(T)).
:- mode atsort__closure(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

% :- pred main1(list(list(int))).
% :- mode main1(out) is det.
% 
% main1(L) :-
% 	test1(S, P),
% 	atsort(S, P, L).
% 
% :- pred test1(relmap(int), relmap(int)).
% :- mode test1(out, out) is det.
% 
% test1(S, P) :-
% 	map__init(S0),
% 	map__set(S0, 1, [2, 3], S1),
% 	map__set(S1, 2, [], S2),
% 	map__set(S2, 3, [4, 5], S3),
% 	map__set(S3, 4, [], S4),
% 	map__set(S4, 5, [3], S),
% 	map__init(P0),
% 	map__set(P0, 1, [], P1),
% 	map__set(P1, 2, [1], P2),
% 	map__set(P2, 3, [1, 5], P3),
% 	map__set(P3, 4, [3], P4),
% 	map__set(P4, 5, [3], P).

atsort(Succmap, Predmap, MustSuccmap, MustPredmap, PrefOrder, Sortlist) :-
	map__keys(Succmap, Snodelist),
	map__keys(Predmap, Pnodelist),
	( Snodelist = Pnodelist ->
		Nodelist = Snodelist
	;
		error("succ and pred nodelists differ in atsort")
	),
	atsort__main(Nodelist, Succmap, Predmap, MustSuccmap, MustPredmap,
		PrefOrder, Sortlist).

:- pred atsort__main(list(T), relmap(T), relmap(T), relmap(T), relmap(T),
	list(T), list(list(T))).
:- mode atsort__main(in, in, in, in, in, in, out) is det.

atsort__main(Nodes0, Succmap0, Predmap0, MustSuccmap, MustPredmap, PrefOrder,
		Sorted) :-
	atsort__repeat_source_sink(Nodes0,
		Succmap0, Succmap1, Predmap0, Predmap1,
		[], Source1, Mid1, [], Sink1),
	( Mid1 = [] ->
		list__reverse(Source1, Source1rev),
		list__append(Source1rev, Sink1, Sorted)
	;
		atsort__choose(Mid1, Succmap1, Succmap2, Predmap1, Predmap2,
			MustSuccmap, MustPredmap, PrefOrder, Chosen, Mid2),
		% write('Chosen: '),
		% write(Chosen),
		% nl,
		% write('Not chosen: '),
		% write(Mid2),
		% nl,
		atsort__main(Mid2, Succmap2, Predmap2, MustSuccmap, MustPredmap,
			PrefOrder, MidSorted),
		list__reverse(Source1, Source1rev),
		list__condense([Source1rev, [[Chosen]], MidSorted, Sink1],
			Sorted)
	).

%-----------------------------------------------------------------------------%

:- pred atsort__choose(list(T), relmap(T), relmap(T), relmap(T), relmap(T),
	relmap(T), relmap(T), list(T), T, list(T)).
:- mode atsort__choose(in, in, out, in, out, in, in, in, out, out) is det.

atsort__choose(Nodes, Succmap0, Succmap, Predmap0, Predmap,
		_MustSuccmap, MustPredmap, PrefOrder, Chosen, NotChosen) :-
	atsort__can_choose(Nodes, Nodes, MustPredmap, [], CanChoose),
	atsort__choose_pref(PrefOrder, CanChoose, Chosen),
	list__delete_all(Nodes, Chosen, NotChosen),
	atsort__map_delete_all_source_links([Chosen],
		Succmap0, Predmap0, Predmap1),
	atsort__map_delete_all_sink_links([Chosen],
		Predmap0, Succmap0, Succmap1),
	atsort__map_delete_all_nodes([Chosen], Succmap1, Succmap),
	atsort__map_delete_all_nodes([Chosen], Predmap1, Predmap).

	% See whether this node can be chosen ahead of the given list of nodes.
	% Do not give preference to nodes that occur in MustPredmap.

:- pred atsort__can_choose(list(T), list(T), relmap(T), list(T), list(T)).
% :- mode atsort__can_choose(in, in, in, di, uo) is det.
:- mode atsort__can_choose(in, in, in, in, out) is det.

atsort__can_choose([], _All, _MustPredmap, CanChoose, CanChoose).
atsort__can_choose([Node | Nodes], All, MustPredmap, CanChoose0, CanChoose) :-
	( map__search(MustPredmap, Node, MustPrednodes) ->
		( atsort__must_avoid(All, MustPrednodes) ->
			CanChoose1 = [Node | CanChoose0]
		;
			CanChoose1 = CanChoose0
		)
	;
		CanChoose1 = [Node | CanChoose0]
	),
	atsort__can_choose(Nodes, All, MustPredmap, CanChoose1, CanChoose).

	% None of the members of the first list occur in the second.

:- pred atsort__must_avoid(list(T), list(T)).
:- mode atsort__must_avoid(in, in) is semidet.

atsort__must_avoid([], _).
atsort__must_avoid([Head | Tail], Avoidlist) :-
	\+ list__member(Head, Avoidlist),
	atsort__must_avoid(Tail, Avoidlist).

:- pred atsort__choose_pref(list(T), list(T), T).
:- mode atsort__choose_pref(in, in, out) is det.

atsort__choose_pref([], _CanChoose, _Chosen) :-
	error("cannot choose any node in atsort").
atsort__choose_pref([Pref | Prefs], CanChoose, Chosen) :-
	( list__member(Pref, CanChoose) ->
		Chosen = Pref
	;
		atsort__choose_pref(Prefs, CanChoose, Chosen)
	).

%-----------------------------------------------------------------------------%

:- pred atsort__repeat_source_sink(list(T),
	relmap(T), relmap(T), relmap(T), relmap(T),
	list(list(T)), list(list(T)), list(T), list(list(T)), list(list(T))).
% :- mode atsort__repeat_source_sink(in, di, uo, di, uo,
% 	di, uo, out, di, uo) is det.
:- mode atsort__repeat_source_sink(in, in, out, in, out,
	in, out, out, in, out) is det.

atsort__repeat_source_sink(Nodes0, Succmap0, Succmap, Predmap0, Predmap,
		Source0, Source, Mid, Sink0, Sink) :-
	atsort__source_sink(Nodes0, Succmap0, Predmap0,
		[], Source1, [], Mid1, [], Sink1),
	( Source1 = [], Sink1 = [] ->
		Succmap = Succmap0,
		Predmap = Predmap0,
		Source = Source0,
		Sink = Sink0,
		Mid = Mid1
	;
		list__delete_elems(Nodes0, Source1, Nodes1),
		list__delete_elems(Nodes1, Sink1, Nodes2),
		atsort__map_delete_all_source_links(Source1,
			Succmap0, Predmap0, Predmap1),
		atsort__map_delete_all_sink_links(Sink1,
			Predmap0, Succmap0, Succmap1),
		atsort__map_delete_all_nodes(Source1, Succmap1, Succmap1_5),
		atsort__map_delete_all_nodes(Source1, Predmap1, Predmap1_5),
		atsort__map_delete_all_nodes(Sink1, Succmap1_5, Succmap2),
		atsort__map_delete_all_nodes(Sink1, Predmap1_5, Predmap2),
		( Source1 = [] ->
			Source2 = Source0
		;
			Source2 = [Source1 | Source0]
		),
		( Sink1 = [] ->
			Sink2 = Sink0
		;
			Sink2 = [Sink1 | Sink0]
		),
		atsort__repeat_source_sink(Nodes2,
			Succmap2, Succmap, Predmap2, Predmap,
			Source2, Source, Mid, Sink2, Sink)
	).

%-----------------------------------------------------------------------------%

:- pred atsort__source_sink(list(T), relmap(T), relmap(T),
	list(T), list(T), list(T), list(T), list(T), list(T)).
% :- mode atsort__source_sink(in, in, in, di, uo, di, uo, di, uo) is det.
:- mode atsort__source_sink(in, in, in, in, out, in, out, in, out) is det.

atsort__source_sink([], _, _, Source, Source, Mid, Mid, Sink, Sink).
atsort__source_sink([Node | Nodes], Succmap, Predmap,
		Source0, Source, Mid0, Mid, Sink0, Sink) :-
	(
		map__search(Succmap, Node, Succnodes),
		Succnodes = []
	->
		Source1 = Source0,
		Mid1 = Mid0,
		Sink1 = [Node | Sink0]
	;
		map__search(Predmap, Node, Prednodes),
		Prednodes = []
	->
		Source1 = [Node | Source0],
		Mid1 = Mid0,
		Sink1 = Sink0
	;
		Source1 = Source0,
		Mid1 = [Node | Mid0],
		Sink1 = Sink0
	),
	atsort__source_sink(Nodes, Succmap, Predmap,
		Source1, Source, Mid1, Mid, Sink1, Sink).

%-----------------------------------------------------------------------------%

:- pred atsort__map_delete_all_source_links(list(T),
	relmap(T), relmap(T), relmap(T)).
% :- mode atsort__map_delete_all_source_links(in, in, di, uo) is det.
:- mode atsort__map_delete_all_source_links(in, in, in, out) is det.

atsort__map_delete_all_source_links([], _Succmap, Predmap, Predmap).
atsort__map_delete_all_source_links([Source | Sources],
		Succmap, Predmap0, Predmap) :-
	map__lookup(Succmap, Source, Succnodes),
	atsort__map_delete_this_element(Succnodes, Source, Predmap0, Predmap1),
	atsort__map_delete_all_source_links(Sources,
		Succmap, Predmap1, Predmap).

:- pred atsort__map_delete_all_sink_links(list(T),
	relmap(T), relmap(T), relmap(T)).
% :- mode atsort__map_delete_all_sink_links(in, in, di, uo) is det.
:- mode atsort__map_delete_all_sink_links(in, in, in, out) is det.

atsort__map_delete_all_sink_links([], _Predmap, Succmap, Succmap).
atsort__map_delete_all_sink_links([Sink | Sinks],
		Predmap, Succmap0, Succmap) :-
	map__lookup(Predmap, Sink, Prednodes),
	atsort__map_delete_this_element(Prednodes, Sink, Succmap0, Succmap1),
	atsort__map_delete_all_sink_links(Sinks,
		Predmap, Succmap1, Succmap).

:- pred atsort__map_delete_this_element(list(T), T, relmap(T), relmap(T)).
:- mode atsort__map_delete_this_element(in, in, in, out) is det.

atsort__map_delete_this_element([], _, Map, Map).
atsort__map_delete_this_element([Node | Nodes], Elt, Map0, Map) :-
	( map__search(Map0, Node, List0) ->
		list__delete_all(List0, Elt, List1),
		map__det_update(Map0, Node, List1, Map1)
	;
		Map1 = Map0
	),
	atsort__map_delete_this_element(Nodes, Elt, Map1, Map).

:- pred atsort__map_delete_all_nodes(list(T), relmap(T), relmap(T)).
% :- mode atsort__map_delete_all_nodes(in, di, uo) is det.
:- mode atsort__map_delete_all_nodes(in, in, out) is det.

atsort__map_delete_all_nodes([], Map, Map).
atsort__map_delete_all_nodes([Node | Nodes], Map0, Map) :-
	map__delete(Map0, Node, Map1),
	atsort__map_delete_all_nodes(Nodes, Map1, Map).

%-----------------------------------------------------------------------------%

atsort__closure(Nodes, Map, Reachable) :-
	atsort__closure_2(Nodes, Map, [], Reachable).

	% The first argument is a list of nodes to look at. If they have
	% not been seen before, we insert them into the reachable list,
	% and schedule their neighbours to be looked at too.

	% XXX Should think about making Reachable be a bintree set.

:- pred atsort__closure_2(list(T), relmap(T), list(T), list(T)).
% :- mode atsort__closure_2(in, in, di, uo) is det.
:- mode atsort__closure_2(in, in, in, out) is det.

atsort__closure_2([], _, Reachable, Reachable).
atsort__closure_2([Node | Nodes0], Map, Reachable0, Reachable) :-
	( list__member(Node, Reachable0) ->
		Nodes1 = Nodes0,
		Reachable1 = Reachable0
	;
		map__lookup(Map, Node, Neighbours),
		atsort__maybe_insert(Neighbours, Nodes0, Nodes1),
		Reachable1 = [Node | Reachable0]
	),
	atsort__closure_2(Nodes1, Map, Reachable1, Reachable).

:- pred atsort__maybe_insert(list(T), list(T), list(T)).
% :- mode atsort__maybe_insert(in, di, uo) is det.
:- mode atsort__maybe_insert(in, in, out) is det.

atsort__maybe_insert([], List, List).
atsort__maybe_insert([Node | Nodes], List0, List) :-
	( list__member(Node, List0) ->
		List1 = List0
	;
		List1 = [Node | List0]
	),
	atsort__maybe_insert(Nodes, List1, List).

%-----------------------------------------------------------------------------%
