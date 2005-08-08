%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Atsort.m - approximate topological sort. The sort is approximate because
% it must work even on data that has cycles.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module libs__atsort.

:- interface.

:- import_module list.
:- import_module map.

:- type relmap(T) == map(T, list(T)).

    % atsort(Succmap, Predmap, MustSuccmap, MustPredmap, PrefOrder, Sortlist):
    %
    % Succmap and Predmap describe the graph to sort: they map nodes to
    % the list of their successors and predecessors respectively. This
    % graph may have cycles. MustSuccmap and MustPredmap describe an
    % acyclic subset of this graph. The final node order, Sortlist, must
    % obey the order described by MustSuccmap and MustPredmap. PrefOrder
    % gives a preference for the order of the nodes.
    %
:- pred atsort(relmap(T)::in, relmap(T)::in, relmap(T)::in, relmap(T)::in,
    list(T)::in, list(list(T))::out) is det.

    % atsort__closure(Nodes, Map, Reachable):
    %
    % Set Reachable to the set of nodes reachable from Nodes via Map.
    % Reachable is in no particular order, and won't include members of
    % Nodes unless they are reachable from other members.
    %
:- pred atsort__closure(list(T)::in, relmap(T)::in, list(T)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

% :- pred main1(list(list(int))).
% :- mode main1(out) is det.
% 
% main1(L) :-
%   test1(S, P),
%   atsort(S, P, L).
% 
% :- pred test1(relmap(int), relmap(int)).
% :- mode test1(out, out) is det.
% 
% test1(S, P) :-
%   map__init(S0),
%   map__set(S0, 1, [2, 3], S1),
%   map__set(S1, 2, [], S2),
%   map__set(S2, 3, [4, 5], S3),
%   map__set(S3, 4, [], S4),
%   map__set(S4, 5, [3], S),
%   map__init(P0),
%   map__set(P0, 1, [], P1),
%   map__set(P1, 2, [1], P2),
%   map__set(P2, 3, [1, 5], P3),
%   map__set(P3, 4, [3], P4),
%   map__set(P4, 5, [3], P).

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

:- pred atsort__main(list(T)::in, relmap(T)::in, relmap(T)::in,
    relmap(T)::in, relmap(T)::in, list(T)::in, list(list(T))::out) is det.

atsort__main(Nodes0, !.Succmap, !.Predmap, MustSuccmap, MustPredmap, PrefOrder,
        Sorted) :-
    atsort__repeat_source_sink(Nodes0, !Succmap, !Predmap,
        [], Source1, Mid1, [], Sink1),
    ( Mid1 = [] ->
        list__reverse(Source1, Source1rev),
        list__append(Source1rev, Sink1, Sorted)
    ;
        atsort__choose(Mid1, !Succmap, !Predmap, MustSuccmap, MustPredmap,
            PrefOrder, Chosen, Mid2),
        % write('Chosen: '),
        % write(Chosen),
        % nl,
        % write('Not chosen: '),
        % write(Mid2),
        % nl,
        atsort__main(Mid2, !.Succmap, !.Predmap, MustSuccmap, MustPredmap,
            PrefOrder, MidSorted),
        list__reverse(Source1, Source1rev),
        list__condense([Source1rev, [[Chosen]], MidSorted, Sink1], Sorted)
    ).

%-----------------------------------------------------------------------------%

:- pred atsort__choose(list(T)::in,
    relmap(T)::in, relmap(T)::out, relmap(T)::in, relmap(T)::out,
    relmap(T)::in, relmap(T)::in, list(T)::in, T::out, list(T)::out) is det.

atsort__choose(Nodes, !Succmap, !Predmap, _MustSuccmap, MustPredmap, PrefOrder,
        Chosen, NotChosen) :-
    atsort__can_choose(Nodes, Nodes, MustPredmap, [], CanChoose),
    atsort__choose_pref(PrefOrder, CanChoose, Chosen),
    list__delete_all(Nodes, Chosen, NotChosen),
    Succmap0 = !.Succmap,
    Predmap0 = !.Predmap,
    atsort__map_delete_all_source_links([Chosen], Succmap0, !Predmap),
    atsort__map_delete_all_sink_links([Chosen], Predmap0, !Succmap),
    atsort__map_delete_all_nodes([Chosen], !Succmap),
    atsort__map_delete_all_nodes([Chosen], !Predmap).

    % See whether this node can be chosen ahead of the given list of nodes.
    % Do not give preference to nodes that occur in MustPredmap.
    %
:- pred atsort__can_choose(list(T)::in, list(T)::in, relmap(T)::in,
    list(T)::in, list(T)::out) is det.

atsort__can_choose([], _All, _MustPredmap, !CanChoose).
atsort__can_choose([Node | Nodes], All, MustPredmap, !CanChoose) :-
    ( map__search(MustPredmap, Node, MustPrednodes) ->
        ( atsort__must_avoid(All, MustPrednodes) ->
            !:CanChoose = [Node | !.CanChoose]
        ;
            true
        )
    ;
        !:CanChoose = [Node | !.CanChoose]
    ),
    atsort__can_choose(Nodes, All, MustPredmap, !CanChoose).

    % None of the members of the first list occur in the second.
    %
:- pred atsort__must_avoid(list(T)::in, list(T)::in) is semidet.

atsort__must_avoid([], _).
atsort__must_avoid([Head | Tail], Avoidlist) :-
    \+ list__member(Head, Avoidlist),
    atsort__must_avoid(Tail, Avoidlist).

:- pred atsort__choose_pref(list(T)::in, list(T)::in, T::out) is det.

atsort__choose_pref([], _CanChoose, _Chosen) :-
    error("cannot choose any node in atsort").
atsort__choose_pref([Pref | Prefs], CanChoose, Chosen) :-
    ( list__member(Pref, CanChoose) ->
        Chosen = Pref
    ;
        atsort__choose_pref(Prefs, CanChoose, Chosen)
    ).

%-----------------------------------------------------------------------------%

:- pred atsort__repeat_source_sink(list(T)::in,
    relmap(T)::in, relmap(T)::out, relmap(T)::in, relmap(T)::out,
    list(list(T))::in, list(list(T))::out, list(T)::out,
    list(list(T))::in, list(list(T))::out) is det.

atsort__repeat_source_sink(Nodes0, !Succmap, !Predmap, Source0, Source, Mid,
        Sink0, Sink) :-
    atsort__source_sink(Nodes0, !.Succmap, !.Predmap,
        [], Source1, [], Mid1, [], Sink1),
    (
        Source1 = [],
        Sink1 = []
    ->
        Source = Source0,
        Sink = Sink0,
        Mid = Mid1
    ;
        list__delete_elems(Nodes0, Source1, Nodes1),
        list__delete_elems(Nodes1, Sink1, Nodes2),
        Succmap0 = !.Succmap,
        Predmap0 = !.Predmap,
        atsort__map_delete_all_source_links(Source1, Succmap0, !Predmap),
        atsort__map_delete_all_sink_links(Sink1, Predmap0, !Succmap),
        atsort__map_delete_all_nodes(Source1, !Succmap),
        atsort__map_delete_all_nodes(Source1, !Predmap),
        atsort__map_delete_all_nodes(Sink1, !Succmap),
        atsort__map_delete_all_nodes(Sink1, !Predmap),
        (
            Source1 = [],
            Source2 = Source0
        ;
            Source1 = [_ | _],
            Source2 = [Source1 | Source0]
        ),
        (
            Sink1 = [],
            Sink2 = Sink0
        ;
            Sink1 = [_ | _],
            Sink2 = [Sink1 | Sink0]
        ),
        atsort__repeat_source_sink(Nodes2, !Succmap, !Predmap,
            Source2, Source, Mid, Sink2, Sink)
    ).

%-----------------------------------------------------------------------------%

:- pred atsort__source_sink(list(T)::in, relmap(T)::in, relmap(T)::in,
    list(T)::in, list(T)::out, list(T)::in, list(T)::out,
    list(T)::in, list(T)::out) is det.

atsort__source_sink([], _, _, !Source, !Mid, !Sink).
atsort__source_sink([Node | Nodes], Succmap, Predmap, !Source, !Mid, !Sink) :-
    (
        map__search(Succmap, Node, Succnodes),
        Succnodes = []
    ->
        !:Sink = [Node | !.Sink]
    ;
        map__search(Predmap, Node, Prednodes),
        Prednodes = []
    ->
        !:Source = [Node | !.Source]
    ;
        !:Mid = [Node | !.Mid]
    ),
    atsort__source_sink(Nodes, Succmap, Predmap, !Source, !Mid, !Sink).

%-----------------------------------------------------------------------------%

:- pred atsort__map_delete_all_source_links(list(T)::in,
    relmap(T)::in, relmap(T)::in, relmap(T)::out) is det.

atsort__map_delete_all_source_links([], _Succmap, !Predmap).
atsort__map_delete_all_source_links([Source | Sources], Succmap, !Predmap) :-
    map__lookup(Succmap, Source, Succnodes),
    atsort__map_delete_this_element(Succnodes, Source, !Predmap),
    atsort__map_delete_all_source_links(Sources, Succmap, !Predmap).

:- pred atsort__map_delete_all_sink_links(list(T)::in,
    relmap(T)::in, relmap(T)::in, relmap(T)::out) is det.

atsort__map_delete_all_sink_links([], _Predmap, !Succmap).
atsort__map_delete_all_sink_links([Sink | Sinks], Predmap, !Succmap) :-
    map__lookup(Predmap, Sink, Prednodes),
    atsort__map_delete_this_element(Prednodes, Sink, !Succmap),
    atsort__map_delete_all_sink_links(Sinks, Predmap, !Succmap).

:- pred atsort__map_delete_this_element(list(T)::in, T::in,
    relmap(T)::in, relmap(T)::out) is det.

atsort__map_delete_this_element([], _, !Map).
atsort__map_delete_this_element([Node | Nodes], Elt, !Map) :-
    ( map__search(!.Map, Node, List0) ->
        list__delete_all(List0, Elt, List1),
        map__det_update(!.Map, Node, List1, !:Map)
    ;
        true
    ),
    atsort__map_delete_this_element(Nodes, Elt, !Map).

:- pred atsort__map_delete_all_nodes(list(T)::in,
    relmap(T)::in, relmap(T)::out) is det.

atsort__map_delete_all_nodes([], !Map).
atsort__map_delete_all_nodes([Node | Nodes], !Map) :-
    map__delete(!.Map, Node, !:Map),
    atsort__map_delete_all_nodes(Nodes, !Map).

%-----------------------------------------------------------------------------%

atsort__closure(Nodes, Map, Reachable) :-
    atsort__closure_2(Nodes, Map, [], Reachable).

    % The first argument is a list of nodes to look at. If they have
    % not been seen before, we insert them into the reachable list,
    % and schedule their neighbours to be looked at too.
    %
    % XXX Should think about making Reachable be a bintree set.
    %
:- pred atsort__closure_2(list(T)::in, relmap(T)::in,
    list(T)::in, list(T)::out) is det.

atsort__closure_2([], _, !Reachable).
atsort__closure_2([Node | Nodes0], Map, !Reachable) :-
    ( list__member(Node, !.Reachable) ->
        Nodes1 = Nodes0
    ;
        map__lookup(Map, Node, Neighbours),
        atsort__maybe_insert(Neighbours, Nodes0, Nodes1),
        !:Reachable = [Node | !.Reachable]
    ),
    atsort__closure_2(Nodes1, Map, !Reachable).

:- pred atsort__maybe_insert(list(T)::in, list(T)::in, list(T)::out) is det.

atsort__maybe_insert([], !List).
atsort__maybe_insert([Node | Nodes], !List) :-
    ( list__member(Node, !.List) ->
        !:List = !.List
    ;
        !:List = [Node | !.List]
    ),
    atsort__maybe_insert(Nodes, !List).

%-----------------------------------------------------------------------------%
