%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% file: relation.m.
% main author: bromage, petdr.
% stability: low.
%
% This module defines a data type for binary relations over reflexive
% domains.
%
% In fact, this is exactly equivalent to a graph/1 type.
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module relation.

:- interface.
:- import_module list, set, set_bbbtree, std_util, assoc_list.

:- type relation(T).

	% relation__init creates a new relation.
:- pred relation__init(relation(T)).
:- mode relation__init(out) is det.

	% relation__add adds an element to the relation.
:- pred relation__add(relation(T), T, T, relation(T)).
:- mode relation__add(in, in, in, out) is det.

	% relation__add_assoc_list adds a list of elements to a
	% relation.
:- pred relation__add_assoc_list(relation(T), assoc_list(T, T), relation(T)).
:- mode relation__add_assoc_list(in, in, out) is det.

	% relation__remove removes an element from the relation.
:- pred relation__remove(relation(T), T, T, relation(T)).
:- mode relation__remove(in, in, in, out) is det.

	% relation__remove_assoc_list removes a list of elements
	% from a relation.
:- pred relation__remove_assoc_list(relation(T), assoc_list(T, T), relation(T)).
:- mode relation__remove_assoc_list(in, in, out) is det.

	% relation__lookup checks to see if an element is
	% in the relation.
:- pred relation__lookup(relation(T), T, T).
:- mode relation__lookup(in, in, out) is nondet.
:- mode relation__lookup(in, in, in) is semidet.

	% relation__reverse_lookup checks to see if an element is
	% in the relation.
:- pred relation__reverse_lookup(relation(T), T, T).
:- mode relation__reverse_lookup(in, out, in) is nondet.
:- mode relation__reverse_lookup(in, in, in) is semidet.

	% relation__lookup_from returns the set of elements
	% y such that xRy, given an x.
:- pred relation__lookup_from(relation(T), T, set(T)).
:- mode relation__lookup_from(in, in, out) is det.

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
:- pred relation__lookup_to(relation(T), T, set(T)).
:- mode relation__lookup_to(in, in, out) is det.

	% relation__to_assoc_list turns a relation into a list of
	% pairs of elements.
:- pred relation__to_assoc_list(relation(T), assoc_list(T, T)).
:- mode relation__to_assoc_list(in, out) is det.

	% relation__from_assoc_list turns a list of pairs of
	% elements into a relation.
:- pred relation__from_assoc_list(assoc_list(T, T), relation(T)).
:- mode relation__from_assoc_list(in, out) is det.

	% relation__effective domain finds the set of all
	% elements actually used in a relation.
:- pred relation__effective_domain(relation(T), set(T)).
:- mode relation__effective_domain(in, out) is det.

	% relation__inverse(R, R') is true iff for all x, y
	% in the domain of R, xRy if yR'x.
:- pred relation__inverse(relation(T), relation(T)).
:- mode relation__inverse(in, out) is det.

	% relation__compose(R1, R2, R) is true if R is the
	% composition of the relations R1 and R2.
:- pred relation__compose(relation(T), relation(T), relation(T)).
:- mode relation__compose(in, in, out) is det.

	% relation__dfs(Rel, X, Dfs) is true if Dfs is a
	% depth-first sorting of Rel starting at X.  The
	% set of elements in the list Dfs is exactly equal
	% to the set of elements y such that xR*y, where
	% R* is the reflexive transitive closure of R.
	% Returns the elements in visited order.
:- pred relation__dfs(relation(T), T, list(T)).
:- mode relation__dfs(in, in, out) is det.

	% relation__dfsrev(Rel, X, Dfs) is true if Dfs is a
	% depth-first sorting of Rel starting at X.  The
	% set of elements in the list Dfs is exactly equal
	% to the set of elements y such that xR*y, where
	% R* is the reflexive transitive closure of R.
	% Returns the elements in reverse visited order.
:- pred relation__dfsrev(relation(T), T, list(T)).
:- mode relation__dfsrev(in, in, out) is det.

	% relation__components(R, Comp) is true if Comp
	% is the set of the connected components of R.
:- pred relation__components(relation(T), set(set(T))).
:- mode relation__components(in, out) is det.

	% relation__cliques(R, Cliques) is true if
	% Cliques is the set of the strongly connected
	% components (cliques) of R.
:- pred relation__cliques(relation(T), set(set(T))).
:- mode relation__cliques(in, out) is det.

	% relation__reduced(R, Red) is true if Red is
	% the reduced relation (relation of cliques)
	% obtained from R.
:- pred relation__reduced(relation(T), relation(set(T))).
:- mode relation__reduced(in, out) is det.

	% relation__tsort(R, TS) is true if TS is a
	% topological sorting of R.  It fails if R
	% is cyclic.
:- pred relation__tsort(relation(T), list(T)).
:- mode relation__tsort(in, out) is semidet.

	% relation__atsort(R, ATS) is true if ATS is
	% a topological sorting of the cliques in R.
:- pred relation__atsort(relation(T), list(set(T))).
:- mode relation__atsort(in, out) is det.

	% relation__sc(R, SC) is true if SC is the
	% symmetric closure of R.  In graph terms,
	% symmetric closure % is the same as turning
	% a directed graph into an undirected graph.
:- pred relation__sc(relation(T), relation(T)).
:- mode relation__sc(in, out) is det.

	% relation__tc(R, TC) is true if TC is the
	% transitive closure of R.
:- pred relation__tc(relation(T), relation(T)).
:- mode relation__tc(in, out) is det.

	% relation__rtc(R, RTC) is true if RTC is the
	% reflexive transitive closure of R.
:- pred relation__rtc(relation(T), relation(T)).
:- mode relation__rtc(in, out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, std_util, list, queue, stack.
:- import_module require.

:- type relation(T) --->
	relation(
		map(T, set(T)),		% The mapping U -> V
		map(T, set(T))		% The reverse mapping V -> U
		).

%------------------------------------------------------------------------------%

	% relation__init creates a new relation.
relation__init(relation(FwdMap, BwdMap)) :-
	map__init(FwdMap),
	map__init(BwdMap).

%------------------------------------------------------------------------------%

	% relation__add adds an element to the relation.
relation__add(relation(FwdIn, BwdIn), U, V, relation(FwdOut, BwdOut)) :-
	( map__search(FwdIn, U, VSet0) ->
	    set__insert(VSet0, V, VSet1),
	    map__det_update(FwdIn, U, VSet1, FwdOut)
	;
	    set__init(VSet0),
	    set__insert(VSet0, V, VSet1),
	    map__det_insert(FwdIn, U, VSet1, FwdOut)
	),
	( map__search(BwdIn, V, USet0) ->
	    set__insert(USet0, U, USet1),
	    map__det_update(BwdIn, V, USet1, BwdOut)
	;
	    set__init(USet0),
	    set__insert(USet0, U, USet1),
	    map__det_insert(BwdIn, V, USet1, BwdOut)
	).

%------------------------------------------------------------------------------%

	% relation__add_assoc_list adds a list of elements to
	% a relation.
relation__add_assoc_list(Rel, [], Rel).
relation__add_assoc_list(Rel0, [U - V | Elems], Rel1) :-
	relation__add(Rel0, U, V, Rel2),
	relation__add_assoc_list(Rel2, Elems, Rel1).

%------------------------------------------------------------------------------%

	% relation__remove removes an element from the relation.
relation__remove(relation(FwdIn, BwdIn), U, V, relation(FwdOut, BwdOut)) :-
	( map__search(FwdIn, U, VSet0) ->
	    set__delete(VSet0, V, VSet1),
	    map__det_update(FwdIn, U, VSet1, FwdOut)
	;
	    FwdIn = FwdOut
	),
	( map__search(BwdIn, V, USet0) ->
	    set__delete(USet0, U, USet1),
	    map__det_update(BwdIn, V, USet1, BwdOut)
	;
	    BwdIn = BwdOut
	).

%------------------------------------------------------------------------------%

	% relation__remove_assoc_list removes a list of elements
	% from a relation.
relation__remove_assoc_list(Rel, [], Rel).
relation__remove_assoc_list(Rel0, [U - V | Elems], Rel1) :-
	relation__remove(Rel0, U, V, Rel2),
	relation__remove_assoc_list(Rel2, Elems, Rel1).

%------------------------------------------------------------------------------%

	% relation__lookup checks to see if an element is
	% in the relation.
relation__lookup(relation(Fwd, _Bwd), U, V) :-
	map__search(Fwd, U, VSet),
	set__member(V, VSet).

%------------------------------------------------------------------------------%

	% relation__reverse_lookup checks to see if an element is
	% in the relation.
relation__reverse_lookup(relation(_Fwd, Bwd), U, V) :-
	map__search(Bwd, V, USet),
	set__member(U, USet).

%------------------------------------------------------------------------------%

	% relation__lookup_from returns the set of elements
	% y such that xRy, given an x.
relation__lookup_from(relation(Fwd, _Bwd), U, Vs) :-
	( map__search(Fwd, U, Vs0) ->
	    Vs = Vs0
	;
	    set__init(Vs)
	).

%------------------------------------------------------------------------------%

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
relation__lookup_to(relation(_Fwd, Bwd), V, Us) :-
	( map__search(Bwd, V, Us0) ->
	    Us = Us0
	;
	    set__init(Us)
	).

%------------------------------------------------------------------------------%

	% relation__to_assoc_list turns a relation into a list of
	% pairs of elements.
relation__to_assoc_list(relation(Fwd, _Bwd), List) :-
	map__keys(Fwd, FwdKeys),
	relation__to_assoc_list_2(Fwd, FwdKeys, List).

:- pred relation__to_assoc_list_2(map(T,set(T)), list(T), assoc_list(T, T)).
:- mode relation__to_assoc_list_2(in, in, out) is det.
relation__to_assoc_list_2(_Fwd, [], []).
relation__to_assoc_list_2(Fwd, [Key | Keys], List) :-
	relation__to_assoc_list_2(Fwd, Keys, List1),
	map__lookup(Fwd, Key, Set),
	set__to_sorted_list(Set, List2),
	relation__append_to(Key, List2, List3),
	list__append(List1, List3, List).

:- pred relation__append_to(T, list(T), assoc_list(T,T)).
:- mode relation__append_to(in, in, out) is det.
relation__append_to(_U, [], []).
relation__append_to(U, [V | Vs], [U - V | UVs]) :-
	relation__append_to(U, Vs, UVs).

%------------------------------------------------------------------------------%

	% relation__from_assoc_list turns a list of pairs of
	% elements into a relation.
relation__from_assoc_list([], Rel) :-
	relation__init(Rel).
relation__from_assoc_list([U - V | List], Rel) :-
	relation__from_assoc_list(List, Rel1),
	relation__add(Rel1, U, V, Rel).

%------------------------------------------------------------------------------%

	% relation__effective domain finds the set of all
	% elements actually used in a relation.
relation__effective_domain(relation(Fwd, Bwd), EffDom) :-
	map__keys(Fwd, FKeyList),
	set__list_to_set(FKeyList, FKeySet),
	map__keys(Bwd, BKeyList),
	set__list_to_set(BKeyList, BKeySet),
	set__union(FKeySet, BKeySet, EffDom).

%------------------------------------------------------------------------------%

	% relation__inverse(R, R') is true iff R is the
	% inverse of R'.  Given our representation, this
	% is incredibly easy to achieve.
relation__inverse(relation(Fwd, Bwd), relation(Bwd, Fwd)).

%------------------------------------------------------------------------------%

	% relation__compose(R1, R2, R) is true iff R is the
	% composition of the relations R1 and R2.
relation__compose(R1, R2, Compose) :-
	relation__effective_domain(R1, Dom),
	set__to_sorted_list(Dom, Dom1),
	relation__init(Comp0),
	relation__compose_2(Dom1, R1, R2, Comp0, Compose).

:- pred relation__compose_2(list(T), relation(T), 
		relation(T), relation(T), relation(T)).
:- mode relation__compose_2(in, in, in, in, out) is det.
relation__compose_2([], _R1, _R2, Comp, Comp).
relation__compose_2([X | Xs], R1, R2, Comp0, Comp) :-
	relation__lookup_from(R1, X, R1XSet),
	set__to_sorted_list(R1XSet, R1XList),
	set__init(CXSet0),
	relation__compose_3(R1XList, R2, CXSet0, CXSet),
	set__to_sorted_list(CXSet, CXList),
	relation__append_to(X, CXList, XCX),
	relation__add_assoc_list(Comp0, XCX, Comp1),
	relation__compose_2(Xs, R1, R2, Comp1, Comp).

:- pred relation__compose_3(list(T), relation(T), set(T), set(T)).
:- mode relation__compose_3(in, in, in, out) is det.
relation__compose_3([], _R2, CX, CX).
relation__compose_3([Y | Ys], R2, CX0, CX) :-
	relation__lookup_from(R2, Y, ZsSet),
	set__union(ZsSet, CX0, CX1),
	relation__compose_3(Ys, R2, CX1, CX).

%------------------------------------------------------------------------------%

	% relation__dfs/3 performs a depth-first search of
	% a relation.  It returns the elements in visited
	% order.
relation__dfs(Rel, X, Dfs) :-
	relation__dfsrev(Rel, X, DfsRev),
	list__reverse(DfsRev, Dfs).

	% relation__dfsrev/3 performs a depth-first search of
	% a relation.  It returns the elements in reverse visited
	% order.
relation__dfsrev(Rel, X, DfsRev) :-
	set_bbbtree__init(Vis0),
	stack__init(S0),
	stack__push(S0, X, S1),
	relation__dfs_2(Rel, S1, Vis0, [], _, DfsRev).

	% relation__dfs(Rel, Dfs) is true if Dfs is a depth-
	% first sorting of Rel.  Where the nodes are in the
	% order visited.
:- pred relation__dfs(relation(T), list(T)).
:- mode relation__dfs(in, out) is det.
relation__dfs(Rel, Dfs) :-
	relation__dfsrev(Rel, DfsRev),
	list__reverse(DfsRev, Dfs).

	% relation__dfsrev(Rel, Dfs) is true if Dfs is a depth-
	% first sorting of Rel.  Where the nodes are in the reverse
	% order visited.
:- pred relation__dfsrev(relation(T), list(T)).
:- mode relation__dfsrev(in, out) is det.
relation__dfsrev(Rel, DfsRev) :-
	relation__effective_domain(Rel, DomSet),
	set__to_sorted_list(DomSet, DomList),
	stack__init(S0),
	stack__push_list(S0, DomList, S1),
	set_bbbtree__init(Vis),
	relation__dfs_2(Rel, S1, Vis, [], _, DfsRev).


:- pred relation__dfs_2(relation(T), stack(T), set_bbbtree(T), list(T), 
							set_bbbtree(T),list(T)).
:- mode relation__dfs_2(in, in, in, in, out, out) is det.
relation__dfs_2(Rel, S0, VisIn, DfsIn, VisOut, DfsOut) :-
	( stack__pop(S0, X, S1) ->
	    ( set_bbbtree__member(X, VisIn) ->
		relation__dfs_2(Rel, S1, VisIn, DfsIn, VisOut, DfsOut)
	    ;
		Dfs1 = [ X | DfsIn ],
		relation__lookup_from(Rel, X, AdjSet),
		set__to_sorted_list(AdjSet, AdjList),
		set_bbbtree__insert(VisIn, X, Vis1),
		stack__push_list(S1, AdjList, S2),
		relation__dfs_2(Rel, S2, Vis1, Dfs1, VisOut, DfsOut)
	    )
	;
	    DfsOut = DfsIn,
	    VisOut = VisIn
	).

%------------------------------------------------------------------------------%

	% relation__components takes a relation and returns
	% a set of the connected components.
relation__components(Rel, Set) :-
	relation__effective_domain(Rel, DomSet),
	set__to_sorted_list(DomSet, DomList),
	set__init(Comp0),
	relation__components_2(Rel, DomList, Comp0, Set).

:- pred relation__components_2(relation(T), list(T), set(set(T)), set(set(T))).
:- mode relation__components_2(in, in, in, out) is det.
relation__components_2(_Rel, [], Comp, Comp).
relation__components_2(Rel, [ X | Xs ], Comp0, Comp) :-
	set__init(Set0),
	queue__list_to_queue([X], Q0),
	relation__reachable_from(Rel, Set0, Q0, Component),
	set__insert(Comp0, Component, Comp1),
	set__list_to_set(Xs, XsSet),
	set__difference(XsSet, Component, Xs1Set),
	set__to_sorted_list(Xs1Set, Xs1),
	relation__components_2(Rel, Xs1, Comp1, Comp).

:- pred relation__reachable_from(relation(T), set(T), queue(T), set(T)).
:- mode relation__reachable_from(in, in, in, out) is det.
relation__reachable_from(Rel, Set0, Q0, Set) :-
	( queue__get(Q0, X, Q1) ->
	    ( set__member(X, Set0) ->
		relation__reachable_from(Rel, Set0, Q1, Set)
	    ;
	    	relation__lookup_from(Rel, X, FwdSet),
	    	relation__lookup_to(Rel, X, BwdSet),
	    	set__union(FwdSet, BwdSet, NextSet0),
		set__difference(NextSet0, Set0, NextSet1),
		set__to_sorted_list(NextSet1, NextList),
		queue__put_list(Q0, NextList, Q2),
		set__insert(Set0, X, Set1),
	    	relation__reachable_from(Rel, Set1, Q2, Set)
	    )
	;
	    Set = Set0
	).

%------------------------------------------------------------------------------%

	% relation cliques
	%	take a relation and return the set of strongly connected
	%	components
	%
	%	Works using the following algorith
	%		1. Using a DFS number all nodes in the order visited.
	%		2. Reverse the relation ie R'
	%		3. Starting from the highest numbered node do a DFS on
	%		   R'.  All the nodes visited are a member of the cycle.
	%		4. From the next highest non-visited node do a DFS on
	%		   R' (not including visited nodes).  This is the next
	%		   cycle.
	%		5. Repeat step 4 until all nodes visited.
relation__cliques(Rel, Cliques) :-
		% Effectively assigns a numbering to the nodes.
	relation__dfsrev(Rel, DfsRev),
	relation__inverse(Rel, RelInv),
	set__init(Cliques0),
	set_bbbtree__init(Visit),
	relation__cliques_2(DfsRev, RelInv, Visit, Cliques0, Cliques).

:- pred relation__cliques_2(list(T), relation(T), set_bbbtree(T), 
						set(set(T)), set(set(T))).
:- mode relation__cliques_2(in, in, in, in, out) is det.

relation__cliques_2([], _, _, Cliques, Cliques).
relation__cliques_2([H | T0], RelInv, Visit0, Cliques0, Cliques) :-
		% Do a DFS on R'
	stack__init(S0),
	stack__push(S0, H, S),
	relation__dfs_2(RelInv, S, Visit0, [], Visit, StrongComponent),

		% Insert the cycle into the clique set.
	set__list_to_set(StrongComponent, StrongComponentSet),
	set__insert(Cliques0, StrongComponentSet, Cliques1),

		% Delete all the visited elements, so first element of the
		% list is the next highest number node.
	list__delete_elems(T0, StrongComponent, T),
	relation__cliques_2(T, RelInv, Visit, Cliques1, Cliques).

%------------------------------------------------------------------------------%

	% relation__reduced(R, Red) is true if Red is
	% the reduced relation (relation of cliques)
	% obtained from R.
relation__reduced(Rel, Red) :-
	relation__cliques(Rel, Cliques),
	set__to_sorted_list(Cliques, CliqList),
	relation__make_clique_map(CliqList, CliqMap),
	relation__to_assoc_list(Rel, RelAL),
	relation__make_reduced_graph(CliqMap, RelAL, Red).

:- pred relation__make_clique_map(list(set(T)), map(T, set(T))).
:- mode relation__make_clique_map(in, out) is det.
relation__make_clique_map([], Map) :-
	map__init(Map).
relation__make_clique_map([S | Ss], Map) :-
	relation__make_clique_map(Ss, Map0),
	set__to_sorted_list(S, SList),
	relation__make_clique_map_2(Map0, S, SList, Map).

:- pred relation__make_clique_map_2(map(T, set(T)), set(T), list(T), 
		map(T, set(T))).
:- mode relation__make_clique_map_2(in, in, in, out) is det.
relation__make_clique_map_2(Map, _Set, [], Map).
relation__make_clique_map_2(MapIn, Set, [X | Xs], MapOut) :-
	map__set(MapIn, X, Set, Map1),
	relation__make_clique_map_2(Map1, Set, Xs, MapOut).

:- pred relation__make_reduced_graph(map(T, set(T)), 
		assoc_list(T,T), relation(set(T))).
:- mode relation__make_reduced_graph(in, in, out) is det.
relation__make_reduced_graph(_Map, [], Rel) :-
	relation__init(Rel).
relation__make_reduced_graph(Map, [U - V | Rest], Rel) :-
	relation__make_reduced_graph(Map, Rest, Rel0),
	map__lookup(Map, U, USet),
	map__lookup(Map, V, VSet),
	( USet = VSet ->
	    Rel = Rel0
	;
	    relation__add(Rel0, USet, VSet, Rel)
	).

%------------------------------------------------------------------------------%

	% relation__tsort returns a topological sorting
	% of a relation.  It fails if the relation is cyclic.
relation__tsort(Rel, Tsort) :-
	relation__effective_domain(Rel, DomSet),
	set__to_sorted_list(DomSet, DomList),
	set__init(Vis0),
	relation__c_dfs(Rel, DomList, Vis0, _Vis, [], Tsort),
	relation__check_tsort(Rel, Vis0, Tsort).

:- pred relation__tsort_2(relation(T), list(T), set(T), set(T),
				list(T), list(T)).
:- mode relation__tsort_2(in, in, in, out, in, out) is det.
relation__tsort_2(_Rel, [], Vis, Vis, Tsort, Tsort).
relation__tsort_2(Rel, [X | Xs], Vis0, Vis, Tsort0, Tsort) :-
	stack__init(S0),
	stack__push(S0, X, S1),
	relation__tsort_3(Rel, S1, Vis0, Vis1, Tsort0, Tsort1),
	list__delete_elems(Xs, Tsort1, XsRed),
	relation__tsort_2(Rel, XsRed, Vis1, Vis, Tsort1, Tsort).

:- pred relation__tsort_3(relation(T), stack(T), set(T), set(T),
				list(T), list(T)).
:- mode relation__tsort_3(in, in, in, out, in, out) is det.
relation__tsort_3(Rel, S0, Vis0, Vis, Tsort0, Tsort) :-
	( stack__pop(S0, X, S1) ->
	    ( set__member(X, Vis0) ->
		relation__tsort_3(Rel, S1, Vis0, Vis, Tsort0, Tsort)
	    ;
	    	relation__lookup_from(Rel, X, AdjSet),
	    	set__to_sorted_list(AdjSet, AdjList),
	    	set__insert(Vis0, X, Vis1),
	    	stack__push_list(S1, AdjList, S2),
	    	relation__tsort_3(Rel, S2, Vis1, Vis, Tsort0, Tsort1),
	    	Tsort = [ X | Tsort1 ]
	    )
	;
	    Tsort = Tsort0, Vis = Vis0
	).

:- pred relation__check_tsort(relation(T), set(T), list(T)).
:- mode relation__check_tsort(in, in, in) is semidet.
relation__check_tsort(_Rel, _Vis, []).
relation__check_tsort(Rel, Vis, [X | Xs]) :-
	set__insert(Vis, X, Vis1),
	relation__lookup_from(Rel, X, RX),
	set__intersect(Vis1, RX, BackPointers),
	set__empty(BackPointers),
	relation__check_tsort(Rel, Vis1, Xs).

:- pred relation__c_dfs(relation(T), list(T), set(T), set(T), list(T), list(T)).
:- mode relation__c_dfs(in, in, in, out, in, out) is det.
relation__c_dfs(_Rel, [], Vis, Vis, Dfs, Dfs).
relation__c_dfs(Rel, [X | Xs], VisIn, VisOut, DfsIn, DfsOut) :-
        ( set__member(X, VisIn) ->
            VisIn = Vis1, DfsIn = Dfs1
        ;
            relation__c_dfs_2(Rel, X, VisIn, Vis1, DfsIn, Dfs1)
        ),
        relation__c_dfs(Rel, Xs, Vis1, VisOut, Dfs1, DfsOut).

:- pred relation__c_dfs_2(relation(T), T, set(T), set(T), list(T), list(T)).
:- mode relation__c_dfs_2(in, in, in, out, in, out) is det.
relation__c_dfs_2(Rel, X, VisIn, VisOut, DfsIn, DfsOut) :-
        set__insert(VisIn, X, Vis1),
        relation__lookup_from(Rel, X, RelX),
        set__to_sorted_list(RelX, RelXList),
        relation__c_dfs(Rel, RelXList, Vis1, VisOut, DfsIn, Dfs1),
        DfsOut = [X | Dfs1].

%------------------------------------------------------------------------------%

	% relation__atsort returns a topological sorting
	% of the cliques in a relation.
relation__atsort(Rel, ATsort) :-
	relation__reduced(Rel, Red),
	( relation__tsort(Red, ATsort0) ->
	    ATsort = ATsort0
	;
	    error("relation__atsort")
	).

%------------------------------------------------------------------------------%

	% relation__sc returns the symmetric closure of
	% a relation.
relation__sc(Rel, Sc) :-
	relation__inverse(Rel, Inv),
	relation__to_assoc_list(Inv, InvList),
	relation__add_assoc_list(Rel, InvList, Sc).

%------------------------------------------------------------------------------%

	% relation__tc returns the transitive closure of
	% a relation.  We use this procedure:
	%
	%	- Compute the reflexive transitive closure.
	%	- Find the "fake reflexives", that is, the
	%	  set of elements x for which xR+x should
	%	  not be true.  This is done by noting that
	%	  R+ = R . R* (where '.' denotes composition).
	%	  Therefore x is a fake reflexive iff the set
	%	  { x | yRx and xR*y } is empty.
	%	- Remove those elements from the reflexive
	%	  transitive closure computed above.
relation__tc(Rel, Tc) :-
	relation__rtc(Rel, Rtc),

	% Find the fake reflexives
	relation__effective_domain(Rel, Dom),
	set__to_sorted_list(Dom, DomList),
	relation__detect_fake_reflexives(Rel, Rtc, DomList, FakeRefl),

	% Remove them from the RTC, giving us the TC.
	assoc_list__from_corresponding_lists(FakeRefl, FakeRefl, FakeReflComp),
	relation__remove_assoc_list(Rtc, FakeReflComp, Tc).

:- pred relation__detect_fake_reflexives(relation(T), relation(T),
		list(T), list(T)).
:- mode relation__detect_fake_reflexives(in, in, in, out) is det.
relation__detect_fake_reflexives(_Rel, _Rtc, [], []).
relation__detect_fake_reflexives(Rel, Rtc, [X | Xs], FakeRefl) :-
	relation__detect_fake_reflexives(Rel, Rtc, Xs, Fake1),
	relation__lookup_from(Rel, X, RelX),
	relation__lookup_to(Rtc, X, RtcX),
	set__intersect(RelX, RtcX, Between),
	( set__empty(Between) ->
	    FakeRefl = [X | Fake1]
	;
	    FakeRefl = Fake1
	).

%------------------------------------------------------------------------------%

	% relation__rtc returns the reflexive transitive
	% closure of a relation.  This uses the algorithm
	% of Eve and Kurki-Suonio.  See:
	%
	%	Eve and Kurki-Suonio, "On computing the
	%	transitive closure of a relation", Acta
	%	Informatica, 8, pp 303--314, 1977
relation__rtc(Rel, Rtc) :-
	relation__effective_domain(Rel, Dom),
	set__to_sorted_list(Dom, DomList),
	list__length(DomList, DomLen),
	map__init(Map0),
	rtc__init_map(Map0, DomList, Map),
	relation__init(RtcIn),
	Inf is (DomLen + 2) * 2,	% This is close enough to infinity.
	relation__rtc_2(Inf, Rel, DomList, Map, RtcIn, Rtc).

	% rtc__init_map takes a domain Xs and creates a map
	% Map such that:
	%
	%	all [X]
	%	    (
	%		list__member(X, Xs) =>
	%		map__lookup(Map, X, 0)
	%	    ).
	%
	% Question: Would an "array" type which allows an
	% arbitrary domain (not just integers) be a better
	% solution?  About the only differences between this
	% and a "map" would be the ease of initialisation,
	% and the semi-static nature of the domain.  (Arrays
	% can be resized.)
:- pred rtc__init_map(map(T, int), list(T), map(T, int)).
:- mode rtc__init_map(in, in, out) is det.
rtc__init_map(Map, [], Map).
rtc__init_map(MapIn, [ X | Xs ], MapOut) :-
	map__det_insert(MapIn, X, 0, Map1),
	rtc__init_map(Map1, Xs, MapOut).

:- pred relation__rtc_2(int, relation(T), list(T), map(T, int), 
		relation(T), relation(T)).
:- mode relation__rtc_2(in, in, in, in, in, out) is det.
relation__rtc_2(_, _, [], _, Rtc, Rtc).
relation__rtc_2(Inf, Rel, [ X | Xs ], Map, RtcIn, RtcOut) :-
	( map__lookup(Map, X, 0) ->
	    stack__init(S0),
	    rtc(Inf, Rel, X, 1, S0, _S1, Map, Map1, RtcIn, Rtc1)
	;
	    Map = Map1, RtcIn = Rtc1
	),
	relation__rtc_2(Inf, Rel, Xs, Map1, Rtc1, RtcOut).

	% When the arity gets this big, it's probably worth
	% considering making a state type...
:- pred rtc(int, relation(T), T, int, stack(T), stack(T), 
		map(T, int), map(T, int), relation(T), relation(T)).
:- mode rtc(in, in, in, in, in, out, in, out, in, out) is det.
rtc(Inf, Rel, A, K, S0, S1, MapIn, MapOut, RtcIn, RtcOut) :-
	K1 is K + 1,
	stack__push(S0, A, S2),
	map__set(MapIn, A, K, Map1),
	relation__add(RtcIn, A, A, Rtc1),
	relation__lookup_from(Rel, A, BsSet),
	set__to_sorted_list(BsSet, BsList),
	rtc_2(Inf, Rel, A, K1, BsList, S2, S3, Map1, Map2, Rtc1, Rtc2),
	( map__lookup(Map2, A, K) ->
	    rtc_3(Inf, A, Map2, Map3, S3, S4, Rtc2, Rtc3)
	;
	    Map2=Map3, S3=S4, Rtc2=Rtc3
	),
	Map3=MapOut, Rtc3=RtcOut, S1=S4.

:- pred rtc_2(int, relation(T), T, int, list(T), stack(T), stack(T), 
		map(T, int), map(T, int), relation(T), relation(T)).
:- mode rtc_2(in, in, in, in, in, in, out, in, out, in, out) is det.
rtc_2(_Inf, _Rel, _A, _K1, [], Stack, Stack, Map, Map, Rtc, Rtc).
rtc_2(Inf, Rel, A, K1, [B | Bs], S0, S1, Map0, Map1, Rtc0, Rtc1) :-
	( map__lookup(Map0, B, 0) ->
	    rtc(Inf, Rel, B, K1, S0, S2, Map0, Map2, Rtc0, Rtc2)
	;
	    S0=S2, Map0=Map2, Rtc0=Rtc2
	),
	map__lookup(Map2, A, Na),
	map__lookup(Map2, B, Nb),
	( Na =< Nb ->
	    Map2 = Map3
	;
	    map__set(Map2, A, Nb, Map3)
	),
	relation__lookup_from(Rtc2, B, RtcBSet),
	set__to_sorted_list(RtcBSet, RtcB),
	relation__append_to(A, RtcB, RtcA),
	relation__add_assoc_list(Rtc2, RtcA, Rtc3),
	rtc_2(Inf, Rel, A, K1, Bs, S2, S1, Map3, Map1, Rtc3, Rtc1).

:- pred rtc_3(int, T, map(T, int), map(T, int),
		stack(T), stack(T), relation(T), relation(T)).
:- mode rtc_3(in, in, in, out, in, out, in, out) is det.
rtc_3(Inf, A, MapIn, MapOut, S0, S1, RtcIn, RtcOut) :-
	stack__pop_det(S0, B, S2),
	( A = B ->
	    MapIn=MapOut, S1=S2, RtcIn=RtcOut
	;
	    relation__lookup_from(RtcIn, A, RtcASet),
	    set__to_sorted_list(RtcASet, RtcA),
	    relation__append_to(B, RtcA, RtcB),
	    relation__add_assoc_list(RtcIn, RtcB, Rtc1),
	    map__set(MapIn, B, Inf, Map1),
	    rtc_3(Inf, A, Map1, MapOut, S2, S1, Rtc1, RtcOut)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
