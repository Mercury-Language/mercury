
%-----------------------------------------------------------------------------%

% Atsort.nl - approximate topological sort. The sort is approximate because
% it must work even on data that has cycles.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module atsort.

:- interface.
:- import_module map, list.

:- type relmap(T) == map(T, list(T)).

:- pred atsort(relmap(T), relmap(T), list(list(T))).
:- mode atsort(in, in, out) is det.

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

atsort(Succmap, Predmap, Sortlist) :-
	map__keys(Succmap, Snodelist),
	map__keys(Predmap, Pnodelist),
	( Snodelist = Pnodelist ->
		Nodelist = Snodelist
	;
		error("succ and pred nodelists differ in atsort")
	),
	atsort__main(Nodelist, Succmap, Predmap, Sortlist).

:- pred atsort__main(list(T), relmap(T), relmap(T), list(list(T))).
:- mode atsort__main(in, in, in, out) is det.

atsort__main(Nodes0, Succmap0, Predmap0, Sorted) :-
	atsort__repeat_source_sink(Nodes0,
		Succmap0, Succmap1, Predmap0, Predmap1,
		[], Source1, Mid1, [], Sink1),
	( Mid1 = [] ->
		list__reverse(Source1, Source1rev),
		list__append(Source1rev, Sink1, Sorted)
	;
		atsort__choose(Mid1, Succmap1, Succmap2, Predmap1, Predmap2,
			Chosen, Mid2),
		atsort__main(Mid2, Succmap2, Predmap2, MidSorted),
		list__reverse(Source1, Source1rev),
		list__condense([Source1rev, [[Chosen]], MidSorted, Sink1],
			Sorted)
	).

:- pred atsort__choose(list(T), relmap(T), relmap(T), relmap(T), relmap(T),
	T, list(T)).
:- mode atsort__choose(in, in, out, in, out, out, out) is det.

atsort__choose([], _, _, _, _, _, _) :-
	error("atsort__choose called with empty list").
atsort__choose([Chosen | Rest], Succmap0, Succmap, Predmap0, Predmap,
		Chosen, Rest) :-
	atsort__map_delete_all_source_links([Chosen],
		Succmap0, Predmap0, Predmap1),
	atsort__map_delete_all_sink_links([Chosen],
		Predmap0, Succmap0, Succmap1),
	atsort__map_delete_all_nodes([Chosen], Succmap1, Succmap),
	atsort__map_delete_all_nodes([Chosen], Predmap1, Predmap).

:- pred atsort__repeat_source_sink(list(T),
	relmap(T), relmap(T), relmap(T), relmap(T),
	list(list(T)), list(list(T)), list(T), list(list(T)), list(list(T))).
:- mode atsort__repeat_source_sink(in, di, uo, di, uo,
	di, uo, out, di, uo) is det.

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

:- pred atsort__source_sink(list(T), relmap(T), relmap(T),
	list(T), list(T), list(T), list(T), list(T), list(T)).
:- mode atsort__source_sink(in, in, in, di, uo, di, uo, di, uo) is det.

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

:- pred atsort__map_delete_all_source_links(list(T),
	relmap(T), relmap(T), relmap(T)).
:- mode atsort__map_delete_all_source_links(in, in, di, uo) is det.

atsort__map_delete_all_source_links([], _Succmap, Predmap, Predmap).
atsort__map_delete_all_source_links([Source | Sources],
		Succmap, Predmap0, Predmap) :-
	map__lookup(Succmap, Source, Succnodes),
	atsort__map_delete_this_element(Succnodes, Source, Predmap0, Predmap1),
	atsort__map_delete_all_source_links(Sources,
		Succmap, Predmap1, Predmap).

:- pred atsort__map_delete_all_sink_links(list(T),
	relmap(T), relmap(T), relmap(T)).
:- mode atsort__map_delete_all_sink_links(in, in, di, uo) is det.

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
		map__set(Map0, Node, List1, Map1)
	;
		Map1 = Map0
	),
	atsort__map_delete_this_element(Nodes, Elt, Map1, Map).

:- pred atsort__map_delete_all_nodes(list(T), relmap(T), relmap(T)).
:- mode atsort__map_delete_all_nodes(in, di, uo) is det.

atsort__map_delete_all_nodes([], Map, Map).
atsort__map_delete_all_nodes([Node | Nodes], Map0, Map) :-
	map__delete(Map0, Node, Map1),
	atsort__map_delete_all_nodes(Nodes, Map1, Map).
