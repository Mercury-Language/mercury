%---------------------------------------------------------------------------%
% Copyright (C) 1995-1999,2002 The University of Melbourne.
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
:- import_module list, set, set_bbbtree, assoc_list.

:- type relation(T).

:- type relation_key.

	% relation__init creates a new relation.
:- pred relation__init(relation(T)).
:- mode relation__init(out) is det.

:- func relation__init = relation(T).

	% relation__add_element adds an element to the domain of a
	% relation.  Return the old relation_key if one already
	% exists.
:- pred relation__add_element(relation(T), T, relation_key, relation(T)).
:- mode relation__add_element(in, in, out, out) is det.

	% relation__search_element returns the relation_key associated
        % with a domain element.  Fail if the relation_key is not valid.
:- pred relation__search_element(relation(T), T, relation_key).
:- mode relation__search_element(in, in, out) is semidet.

	% relation__lookup_element returns the relation_key associated
        % with a domain element.  Abort if the relation_key is not valid.
:- pred relation__lookup_element(relation(T), T, relation_key).
:- mode relation__lookup_element(in, in, out) is det.

:- func relation__lookup_element(relation(T), T) = relation_key.

	% relation__search_key returns the domain element associated
	% with a relation_key.  Fail if the relation_key is not valid.
:- pred relation__search_key(relation(T), relation_key, T).
:- mode relation__search_key(in, in, out) is semidet.

	% relation__lookup_key returns the domain element associated
	% with a relation_key.  Abort if the relation_key is not valid.
:- pred relation__lookup_key(relation(T), relation_key, T).
:- mode relation__lookup_key(in, in, out) is det.

:- func relation__lookup_key(relation(T), relation_key) = T.

	% relation__add adds an element to the relation.
:- pred relation__add(relation(T), relation_key, relation_key, relation(T)).
:- mode relation__add(in, in, in, out) is det.

:- func relation__add(relation(T), relation_key, relation_key) = relation(T).

	% relation__add_values adds an pair of values to the relation's
	% domain and adds an element to the relation.
	%
	% relation__add_values(R0, X, Y, R) :-
	%	 relation__add_element(R0, X, XKey, R1),
	%	 relation__add_element(R1, Y, YKey, R2),
	%	 relation__add(R1, XKey, YKey, R).
	%
:- pred relation__add_values(relation(T), T, T, relation(T)).
:- mode relation__add_values(in, in, in, out) is det.

:- func relation__add_values(relation(T), T, T) = relation(T).

	% relation__add_assoc_list adds a list of elements to a
	% relation.
:- pred relation__add_assoc_list(relation(T),
		assoc_list(relation_key, relation_key), relation(T)).
:- mode relation__add_assoc_list(in, in, out) is det.

:- func relation__add_assoc_list(relation(T),
		assoc_list(relation_key, relation_key)) = relation(T).

	% relation__remove removes an element from the relation.
:- pred relation__remove(relation(T), relation_key, relation_key,
		relation(T)).
:- mode relation__remove(in, in, in, out) is det.

:- func relation__remove(relation(T), relation_key, relation_key)
		= relation(T).

	% relation__remove_assoc_list removes a list of elements
	% from a relation.
:- pred relation__remove_assoc_list(relation(T),
		assoc_list(relation_key, relation_key), relation(T)).
:- mode relation__remove_assoc_list(in, in, out) is det.

:- func relation__remove_assoc_list(relation(T),
		assoc_list(relation_key, relation_key)) = relation(T).

	% relation__lookup checks to see if an element is
	% in the relation.
:- pred relation__lookup(relation(T), relation_key, relation_key).
:- mode relation__lookup(in, in, out) is nondet.
:- mode relation__lookup(in, in, in) is semidet.

	% relation__reverse_lookup checks to see if an element is
	% in the relation.
:- pred relation__reverse_lookup(relation(T), relation_key, relation_key).
:- mode relation__reverse_lookup(in, out, in) is nondet.
:- mode relation__reverse_lookup(in, in, in) is semidet.

	% relation__lookup_from returns the set of elements
	% y such that xRy, given an x.
:- pred relation__lookup_from(relation(T), relation_key, set(relation_key)).
:- mode relation__lookup_from(in, in, out) is det.

:- func relation__lookup_from(relation(T), relation_key) = set(relation_key).

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
:- pred relation__lookup_to(relation(T), relation_key, set(relation_key)).
:- mode relation__lookup_to(in, in, out) is det.

:- func relation__lookup_to(relation(T), relation_key) = set(relation_key).

	% relation__to_assoc_list turns a relation into a list of
	% pairs of elements.
:- pred relation__to_assoc_list(relation(T), assoc_list(T, T)).
:- mode relation__to_assoc_list(in, out) is det.

:- func relation__to_assoc_list(relation(T)) = assoc_list(T, T).

	% relation__to_key_assoc_list turns a relation into a list of
	% pairs of relation keys.
:- pred relation__to_key_assoc_list(relation(T),
	assoc_list(relation_key, relation_key)).
:- mode relation__to_key_assoc_list(in, out) is det.

:- func relation__to_key_assoc_list(relation(T))
		= assoc_list(relation_key, relation_key).

	% relation__from_assoc_list turns a list of pairs of
	% elements into a relation.
:- pred relation__from_assoc_list(assoc_list(T, T), relation(T)).
:- mode relation__from_assoc_list(in, out) is det.

:- func relation__from_assoc_list(assoc_list(T, T)) = relation(T).

	% relation__domain finds the set of all elements in the
	% domain of a relation.
:- pred relation__domain(relation(T), set(T)).
:- mode relation__domain(in, out) is det.

:- func relation__domain(relation(T)) = set(T).

	% relation__inverse(R, R') is true iff for all x, y
	% in the domain of R, xRy if yR'x.
:- pred relation__inverse(relation(T), relation(T)).
:- mode relation__inverse(in, out) is det.

:- func relation__inverse(relation(T)) = relation(T).

	% relation__compose(R1, R2, R) is true if R is the
	% composition of the relations R1 and R2.
:- pred relation__compose(relation(T), relation(T), relation(T)).
:- mode relation__compose(in, in, out) is det.

:- func relation__compose(relation(T), relation(T)) = relation(T).

	% relation__dfs(Rel, X, Dfs) is true if Dfs is a
	% depth-first sorting of Rel starting at X.  The
	% set of elements in the list Dfs is exactly equal
	% to the set of elements y such that xR*y, where
	% R* is the reflexive transitive closure of R.
:- pred relation__dfs(relation(T), relation_key, list(relation_key)).
:- mode relation__dfs(in, in, out) is det.

:- func relation__dfs(relation(T), relation_key) = list(relation_key).

	% relation__dfsrev(Rel, X, DfsRev) is true if DfsRev is a
	% reverse depth-first sorting of Rel starting at X.  The
	% set of elements in the list Dfs is exactly equal
	% to the set of elements y such that xR*y, where
	% R* is the reflexive transitive closure of R.
:- pred relation__dfsrev(relation(T), relation_key, list(relation_key)).
:- mode relation__dfsrev(in, in, out) is det.

:- func relation__dfsrev(relation(T), relation_key) = list(relation_key).

	% relation__dfs(Rel, Dfs) is true if Dfs is a depth-
	% first sorting of Rel, i.e. a list of the nodes in Rel
	% such that it contains all elements in the relation and all 
	% the children of a node are placed in the list before 
	% the parent.
:- pred relation__dfs(relation(T), list(relation_key)).
:- mode relation__dfs(in, out) is det.

:- func relation__dfs(relation(T)) = list(relation_key).

	% relation__dfsrev(Rel, DfsRev) is true if DfsRev is a reverse 
	% depth-first sorting of Rel.  ie DfsRev is the reverse of Dfs
	% from relation__dfs/2.
:- pred relation__dfsrev(relation(T), list(relation_key)).
:- mode relation__dfsrev(in, out) is det.

:- func relation__dfsrev(relation(T)) = list(relation_key).

	% relation__dfs(Rel, X, Visit0, Visit, Dfs) is true 
	% if Dfs is a depth-first sorting of Rel starting at 
	% X providing we have already visited Visit0 nodes, 
	% i.e.  a list of nodes such that all the unvisited 
	% children of a node are placed in the list before the 
	% parent.  Visit0 allows us to initialise a set of
	% previously visited nodes.  Visit is Dfs + Visit0.
:- pred relation__dfs(relation(T), relation_key, set_bbbtree(relation_key),
		set_bbbtree(relation_key), list(relation_key)).
:- mode relation__dfs(in, in, in, out, out) is det.

	% relation__dfsrev(Rel, X, Visit0, Visit, DfsRev) is true if 
	% DfsRev is a reverse depth-first sorting of Rel starting at X 
	% providing we have already visited Visit0 nodes, 
	% ie the reverse of Dfs from relation__dfs/5.
	% Visit is Visit0 + DfsRev.
:- pred relation__dfsrev(relation(T), relation_key,
		set_bbbtree(relation_key), set_bbbtree(relation_key),
		list(relation_key)).
:- mode relation__dfsrev(in, in, in, out, out) is det.

	% relation__is_dag(R) is true iff R is a directed acyclic graph.
:- pred relation__is_dag(relation(T)).
:- mode relation__is_dag(in) is semidet.

	% relation__components(R, Comp) is true if Comp
	% is the set of the connected components of R.
:- pred relation__components(relation(T), set(set(relation_key))).
:- mode relation__components(in, out) is det.

:- func relation__components(relation(T)) = set(set(relation_key)).

	% relation__cliques(R, Cliques) is true if
	% Cliques is the set of the strongly connected
	% components (cliques) of R.
:- pred relation__cliques(relation(T), set(set(relation_key))).
:- mode relation__cliques(in, out) is det.

:- func relation__cliques(relation(T)) = set(set(relation_key)).

	% relation__reduced(R, Red) is true if Red is
	% the reduced relation (relation of cliques)
	% obtained from R.
:- pred relation__reduced(relation(T), relation(set(T))).
:- mode relation__reduced(in, out) is det.

:- func relation__reduced(relation(T)) = relation(set(T)).

	% relation__tsort(R, TS) is true if TS is a
	% topological sorting of R.  It fails if R
	% is cyclic.
:- pred relation__tsort(relation(T), list(T)).
:- mode relation__tsort(in, out) is semidet.

	% relation__atsort(R, ATS) is true if ATS is
	% a topological sorting of the cliques in R.
:- pred relation__atsort(relation(T), list(set(T))).
:- mode relation__atsort(in, out) is det.

:- func relation__atsort(relation(T)) = list(set(T)).

	% relation__sc(R, SC) is true if SC is the
	% symmetric closure of R.  In graph terms,
	% symmetric closure is the same as turning
	% a directed graph into an undirected graph.
:- pred relation__sc(relation(T), relation(T)).
:- mode relation__sc(in, out) is det.

:- func relation__sc(relation(T)) = relation(T).

	% relation__tc(R, TC) is true if TC is the
	% transitive closure of R.
:- pred relation__tc(relation(T), relation(T)).
:- mode relation__tc(in, out) is det.

:- func relation__tc(relation(T)) = relation(T).

	% relation__rtc(R, RTC) is true if RTC is the
	% reflexive transitive closure of R.
:- pred relation__rtc(relation(T), relation(T)).
:- mode relation__rtc(in, out) is det.

:- func relation__rtc(relation(T)) = relation(T).

	% relation__traverse(R, ProcessNode, ProcessEdge) will
	% traverse a relation calling ProcessNode for each node in the
	% relation and ProcessEdge for each edge in the relation.
	% Each node is processed followed by all the edges originating
	% at that node, until all nodes have been processed.
:- pred relation__traverse(relation(K), pred(K, T, T), pred(K, K, T, T), T, T).
:- mode relation__traverse(in, pred(in, di, uo) is det,
		pred(in, in, di, uo) is det, di, uo) is det.
:- mode relation__traverse(in, pred(in, in, out) is det,
		pred(in, in, in, out) is det, in, out) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module map, bimap, int, std_util, list, queue, stack.
:- import_module require.

:- type relation_key == int.

:- type relation(T) --->
	relation(
		relation_key,				% Next key
		bimap(T, relation_key),			% Elements <-> keys
		map(relation_key, set(relation_key)),	% The mapping U -> V
		map(relation_key, set(relation_key))	% The reverse mapping
							% V -> U
		).

%------------------------------------------------------------------------------%

	% relation__init creates a new relation.
relation__init(relation(0, ElMap, FwdMap, BwdMap)) :-
	bimap__init(ElMap),
	map__init(FwdMap),
	map__init(BwdMap).

%------------------------------------------------------------------------------%

	% relation__add_element adds an element to the domain of a
	% relation.  Return the old relation_key if one already
	% exists.
relation__add_element(relation(Key0, ElMap0, Fwd, Rev),
		Elem, NewKey, relation(Key, ElMap, Fwd, Rev)) :-
	( bimap__search(ElMap0, Elem, NewKey0) ->
		Key = Key0, NewKey = NewKey0, ElMap = ElMap0
	;
		NewKey = Key0,
		Key is Key0 + 1,
		bimap__set(ElMap0, Elem, NewKey, ElMap)
	).

%------------------------------------------------------------------------------%

	% relation__search_element returns the relation_key associated
        % with a domain element.  Fail if the relation_key is not valid.
relation__search_element(relation(_Key, ElMap, _Fwd, _Rev), Elem, Key) :-
        bimap__search(ElMap, Elem, Key).

	% relation__lookup_element returns the relation_key associated
        % with a domain element.  Abort if the relation_key is not valid.
relation__lookup_element(Rel, Elem, Key) :-
	( relation__search_element(Rel, Elem, Key0) ->
		Key = Key0
	;
		error("relation__lookup_element")
	).

%------------------------------------------------------------------------------%

	% relation__search_key returns the domain element associated
	% with a relation_key.  Fail if the relation_key is not valid.
relation__search_key(relation(_Key, ElMap, _Fwd, _Rev), Key, Elem) :-
	bimap__search(ElMap, Elem, Key).

	% relation__lookup_key returns the domain element associated
	% with a relation_key.  Abort if the relation_key is not valid.
relation__lookup_key(Rel, Key, Elem) :-
	( relation__search_key(Rel, Key, Elem0) ->
		Elem = Elem0
	;
		error("relation__lookup_key")
	).

%------------------------------------------------------------------------------%

relation__add_values(R0, X, Y, R) :-
	relation__add_element(R0, X, XKey, R1),
	relation__add_element(R1, Y, YKey, R2),
	relation__add(R2, XKey, YKey, R).

	% relation__add adds an element to the relation.
relation__add(relation(Key, ElMap, FwdIn, BwdIn), U, V,
		relation(Key, ElMap, FwdOut, BwdOut)) :-
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
relation__remove(relation(Key, ElMap, FwdIn, BwdIn), U, V,
		relation(Key, ElMap, FwdOut, BwdOut)) :-
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
relation__lookup(relation(_Key, _ElMap, Fwd, _Bwd), U, V) :-
	map__search(Fwd, U, VSet),
	set__member(V, VSet).

%------------------------------------------------------------------------------%

	% relation__reverse_lookup checks to see if an element is
	% in the relation.
relation__reverse_lookup(relation(_Key, _ElMap, _Fwd, Bwd), U, V) :-
	map__search(Bwd, V, USet),
	set__member(U, USet).

%------------------------------------------------------------------------------%

	% relation__lookup_from returns the set of elements
	% y such that xRy, given an x.
relation__lookup_from(relation(_Key, _ElMap, Fwd, _Bwd), U, Vs) :-
	( map__search(Fwd, U, Vs0) ->
		Vs = Vs0
	;
		set__init(Vs)
	).

%------------------------------------------------------------------------------%

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
relation__lookup_to(relation(_Key, _ElMap, _Fwd, Bwd), V, Us) :-
	( map__search(Bwd, V, Us0) ->
		Us = Us0
	;
		set__init(Us)
	).

%------------------------------------------------------------------------------%

	% relation__to_assoc_list turns a relation into a list of
	% pairs of elements.
relation__to_assoc_list(relation(_Key, ElMap, Fwd, _Bwd), List) :-
	map__keys(Fwd, FwdKeys),
	relation__to_assoc_list_2(Fwd, FwdKeys, ElMap, List).

:- pred relation__to_assoc_list_2(map(relation_key, set(relation_key)),
	list(relation_key), bimap(T, relation_key), assoc_list(T, T)).
:- mode relation__to_assoc_list_2(in, in, in, out) is det.
relation__to_assoc_list_2(_Fwd, [], _, []).
relation__to_assoc_list_2(Fwd, [Key | Keys], ElementMap, AssocList) :-
	relation__to_assoc_list_2(Fwd, Keys, ElementMap, AssocList1),
	map__lookup(Fwd, Key, Set),
	set__to_sorted_list(Set, List),
	bimap__reverse_lookup(ElementMap, KeyEl, Key),
	Lookup = lambda([U::in, V::out] is det,
			bimap__reverse_lookup(ElementMap, V, U)),
	list__map(Lookup, List, ListEls),
	relation__append_to(KeyEl, ListEls, AssocList2),
	list__append(AssocList1, AssocList2, AssocList).

	% relation__to_key_assoc_list turns a relation into a list of
	% pairs of elements.
relation__to_key_assoc_list(relation(_Key, _ElMap, Fwd, _Bwd), List) :-
	map__keys(Fwd, FwdKeys),
	relation__to_key_assoc_list_2(Fwd, FwdKeys, List).

:- pred relation__to_key_assoc_list_2(map(relation_key, set(relation_key)),
	list(relation_key), assoc_list(relation_key, relation_key)).
:- mode relation__to_key_assoc_list_2(in, in, out) is det.
relation__to_key_assoc_list_2(_Fwd, [], []).
relation__to_key_assoc_list_2(Fwd, [Key | Keys], AssocList) :-
	relation__to_key_assoc_list_2(Fwd, Keys, AssocList1),
	map__lookup(Fwd, Key, Set),
	set__to_sorted_list(Set, List),
	relation__append_to(Key, List, AssocList2),
	list__append(AssocList1, AssocList2, AssocList).

:- pred relation__append_to(T1, list(T2), assoc_list(T1, T2)).
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
	relation__add_values(Rel1, U, V, Rel).

%------------------------------------------------------------------------------%

	% relation__domain finds the set of all elements in the domain
	% of a relation.
relation__domain(relation(_Key, ElMap, _Fwd, _Bwd), Dom) :-
	bimap__ordinates(ElMap, DomList),
	set__sorted_list_to_set(DomList, Dom).

:- pred relation__domain_sorted_list(relation(T), list(relation_key)).
:- mode relation__domain_sorted_list(in, out) is det.

relation__domain_sorted_list(relation(_Key, ElMap, _Fwd, _Bwd), Dom) :-
	bimap__coordinates(ElMap, Dom).

%------------------------------------------------------------------------------%

	% relation__inverse(R, R') is true iff R is the
	% inverse of R'.  Given our representation, this
	% is incredibly easy to achieve.
relation__inverse(relation(Key, ElMap, Fwd, Bwd),
		relation(Key, ElMap, Bwd, Fwd)).

%------------------------------------------------------------------------------%

	% relation__compose(R1, R2, R) is true iff R is the
	% composition of the relations R1 and R2.
relation__compose(R1, R2, Compose) :-
	relation__domain(R1, DomainSet),
	set__to_sorted_list(DomainSet, DomainList),
	relation__init(Compose0),
	list__foldl(relation__compose_2(R1, R2), DomainList,
		Compose0, Compose).

	% relation__compose_2(R1, R2, X, Comp0, Comp):
	%	Comp is the union of Comp0 and CompX,
	%	where CompX = { (X, Z) : some [Y] (X R1 Y, Y R2 Z) }.
:- pred relation__compose_2(relation(T), relation(T), T,
			relation(T), relation(T)).
:- mode relation__compose_2(in, in, in, in, out) is det.
relation__compose_2(R1, R2, X, Comp0, Comp) :-
	relation__lookup_element(R1, X, X_R1Key),
	relation__lookup_from(R1, X_R1Key, Ys_R1KeysSet),
	set__to_sorted_list(Ys_R1KeysSet, Ys_R1Keys),
	list__map(relation__lookup_key(R1), Ys_R1Keys, Ys),
	list__foldl(relation__compose_3(R2, X), Ys, Comp0, Comp).

	% relation__compose_3(R2, X, Y, Comp0, Comp):
	%	Comp is the union of Comp0 and CompXY,
	%	where CompXY = { (X, Z) : Y R2 Z }.
:- pred relation__compose_3(relation(T), T, T, relation(T), relation(T)).
:- mode relation__compose_3(in, in, in, in, out) is det.
relation__compose_3(R2, X, Y, Comp0, Comp) :-
	( relation__search_element(R2, Y, Y_R2Key) ->
		relation__lookup_from(R2, Y_R2Key, Zs_R2Keys_Set),
		set__to_sorted_list(Zs_R2Keys_Set, Zs_R2Keys),
		list__map(relation__lookup_key(R2), Zs_R2Keys, Zs),
		AddValue = lambda([Z::in, Rel0::in, Rel::out] is det,
				relation__add_values(Rel0, X, Z, Rel)),
		list__foldl(AddValue, Zs, Comp0, Comp)
	;
		Comp = Comp0
	).

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
	relation__dfs_3([X], Rel, Vis0, [], _, DfsRev).

	% relation__dfs/5 performs a depth-first search of
	% a relation.  It returns the elements in visited
	% order.  Providing the nodes Visited0 have already been 
	% visited.
relation__dfs(Rel, X, Visited0, Visited, Dfs) :-
	relation__dfsrev(Rel, X, Visited0, Visited, DfsRev),
	list__reverse(DfsRev, Dfs).

	% relation__dfsrev/5 performs a depth-first search of
	% a relation.  It returns the elements in reverse visited
	% order.  Providing the nodes Visited0 have already been
	% visited.
relation__dfsrev(Rel, X, Visited0, Visited, DfsRev) :-
	relation__dfs_3([X], Rel, Visited0, [], Visited, DfsRev).


	% relation__dfs(Rel, Dfs) is true if Dfs is a depth-
	% first sorting of Rel.  Where the nodes are in the
	% order visited.
relation__dfs(Rel, Dfs) :-
	relation__dfsrev(Rel, DfsRev),
	list__reverse(DfsRev, Dfs).

	% relation__dfsrev(Rel, Dfs) is true if Dfs is a depth-
	% first sorting of Rel.  Where the nodes are in the reverse
	% order visited.
relation__dfsrev(Rel, DfsRev) :-
	relation__domain_sorted_list(Rel, DomList),
	set_bbbtree__init(Visit),
	relation__dfs_2(DomList, Rel, Visit, [], DfsRev).


:- pred relation__dfs_2(list(relation_key), relation(T),
	set_bbbtree(relation_key), list(relation_key), list(relation_key)).
:- mode relation__dfs_2(in, in, in, in, out) is det.

relation__dfs_2([], _, _, DfsRev, DfsRev).
relation__dfs_2([Node | Nodes], Rel, Visit0, DfsRev0, DfsRev) :-
	relation__dfs_3([Node], Rel, Visit0, DfsRev0, Visit, DfsRev1),
	relation__dfs_2(Nodes, Rel, Visit, DfsRev1, DfsRev).
	

:- pred relation__dfs_3(list(relation_key), relation(T),
	set_bbbtree(relation_key), list(relation_key),
	set_bbbtree(relation_key), list(relation_key)).
:- mode relation__dfs_3(in, in, in, in, out, out) is det.

relation__dfs_3([], _Rel, Visit, Dfs, Visit, Dfs).
relation__dfs_3([Node | Nodes], Rel, Visit0, Dfs0, Visit, Dfs) :-
	(
		set_bbbtree__member(Node, Visit0)
	->
		relation__dfs_3(Nodes, Rel, Visit0, Dfs0, Visit, Dfs)
	;
		relation__lookup_from(Rel, Node, AdjSet),
		set__to_sorted_list(AdjSet, AdjList),
		set_bbbtree__insert(Visit0, Node, Visit1),

			% Go and visit all a nodes children first
		relation__dfs_3(AdjList, Rel, Visit1, Dfs0, Visit2, Dfs1),

		Dfs2 = [ Node | Dfs1 ],

			% Go and visit the rest
		relation__dfs_3(Nodes, Rel, Visit2, Dfs2, Visit, Dfs)
	).


%------------------------------------------------------------------------------%


	% relation__is_dag
	%	Does a DFS on the relation.  It is a directed acylic graph
	%	if at each node we never visit and already visited node.
relation__is_dag(R) :-
	relation__domain_sorted_list(R, DomList),
	set_bbbtree__init(Visit),
	set_bbbtree__init(AllVisit),
	relation__is_dag_2(DomList, R, Visit, AllVisit).

:- pred relation__is_dag_2(list(relation_key), relation(T),
	set_bbbtree(relation_key), set_bbbtree(relation_key)).
:- mode relation__is_dag_2(in, in, in, in) is semidet.

	% If a node hasn't already been visited check if the DFS from that node
	% has any cycles in it.
relation__is_dag_2([], _, _, _).
relation__is_dag_2([Node | Nodes], Rel, Visit0, AllVisited0) :-
	(
		set_bbbtree__member(Node, AllVisited0)
	->
		AllVisited = AllVisited0
	;
		relation__is_dag_3([Node],Rel, Visit0, AllVisited0, AllVisited)
	),
	relation__is_dag_2(Nodes, Rel, Visit0, AllVisited).
	

:- pred relation__is_dag_3(list(relation_key), relation(T),
	set_bbbtree(relation_key), set_bbbtree(relation_key),
	set_bbbtree(relation_key)).
:- mode relation__is_dag_3(in, in, in, in, out) is semidet.

	% Provided that we never encounter a node that we haven't visited before
	% during the current DFS, the graph isn't cyclic.
	% NB It is possible that we have visited a node before while doing a
	% DFS from another node. 
	%
	% ie		2     3
	%		 \   /
	%		  \ /
	%		   1
	%
	% 1 will be visited by a DFS from both 2 and 3.
	%
relation__is_dag_3([Node | Nodes], Rel, Visited0, AllVisited0, AllVisited) :-
	not set_bbbtree__member(Node, Visited0),
	relation__lookup_from(Rel, Node, AdjSet),
	set__to_sorted_list(AdjSet, AdjList),
	set_bbbtree__insert(Visited0, Node, Visited),
	set_bbbtree__insert(AllVisited0, Node, AllVisited1),

		% Go and visit all a nodes children first
	relation__is_dag_3(AdjList, Rel, Visited, AllVisited1, AllVisited1),

		% Go and visit the rest 
	relation__is_dag_3(Nodes, Rel, Visited0, AllVisited1, AllVisited).

	
%------------------------------------------------------------------------------%

	% relation__components takes a relation and returns
	% a set of the connected components.
relation__components(Rel, Set) :-
	relation__domain_sorted_list(Rel, DomList),
	set__init(Comp0),
	relation__components_2(Rel, DomList, Comp0, Set).

:- pred relation__components_2(relation(T), list(relation_key),
	set(set(relation_key)), set(set(relation_key))).
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

:- pred relation__reachable_from(relation(T), set(relation_key),
	queue(relation_key), set(relation_key)).
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
	%		1. Topologically sort the nodes.  Then number the nodes
	%		   so the highest num is the first node in the 
	%		   topological sort.
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

:- pred relation__cliques_2(list(relation_key), relation(T),
	set_bbbtree(relation_key), set(set(relation_key)),
	set(set(relation_key))).
:- mode relation__cliques_2(in, in, in, in, out) is det.

relation__cliques_2([], _, _, Cliques, Cliques).
relation__cliques_2([H | T0], RelInv, Visit0, Cliques0, Cliques) :-
		% Do a DFS on R'
	relation__dfs_3([H], RelInv, Visit0, [], Visit, StrongComponent),

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
	relation__init(Red0),
	map__init(CliqMap0),
	relation__make_clique_map(Rel, CliqList, CliqMap0, CliqMap, Red0, Red1),
	relation__to_key_assoc_list(Rel, RelAL),
	relation__make_reduced_graph(CliqMap, RelAL, Red1, Red).

:- pred relation__make_clique_map(relation(T), list(set(relation_key)),
		map(relation_key, relation_key),
		map(relation_key, relation_key),
		relation(set(T)), relation(set(T))).
:- mode relation__make_clique_map(in, in, in, out, in, out) is det.
relation__make_clique_map(_Rel, [], Map, Map, Red, Red).
relation__make_clique_map(Rel, [S | Ss], Map0, Map, Red0, Red) :-
	set__to_sorted_list(S, SList),
	list__map(relation__lookup_key(Rel), SList, EList),
	set__list_to_set(EList, ESet),
	relation__add_element(Red0, ESet, SKey, Red1),
	relation__make_clique_map_2(Map0, SKey, SList, Map1),
	relation__make_clique_map(Rel, Ss, Map1, Map, Red1, Red).

:- pred relation__make_clique_map_2(map(relation_key, relation_key),
		relation_key, list(relation_key), 
		map(relation_key, relation_key)).
:- mode relation__make_clique_map_2(in, in, in, out) is det.
relation__make_clique_map_2(Map, _Key, [], Map).
relation__make_clique_map_2(MapIn, Key, [X | Xs], MapOut) :-
	map__set(MapIn, X, Key, Map1),
	relation__make_clique_map_2(Map1, Key, Xs, MapOut).

:- pred relation__make_reduced_graph(map(relation_key, relation_key), 
		assoc_list(relation_key, relation_key),
		relation(set(T)), relation(set(T))).
:- mode relation__make_reduced_graph(in, in, in, out) is det.
relation__make_reduced_graph(_Map, [], Rel, Rel).
relation__make_reduced_graph(Map, [U - V | Rest], Rel0, Rel) :-
	map__lookup(Map, U, USet),
	map__lookup(Map, V, VSet),
	( USet = VSet ->
		Rel1 = Rel0
	;
		relation__add(Rel0, USet, VSet, Rel1)
	),
	relation__make_reduced_graph(Map, Rest, Rel1, Rel).

%------------------------------------------------------------------------------%

	% relation__tsort returns a topological sorting
	% of a relation.  It fails if the relation is cyclic.
relation__tsort(Rel, Tsort) :-
	relation__tsort_2(Rel, Tsort0),
	list__map(relation__lookup_key(Rel), Tsort0, Tsort).

:- pred relation__tsort_2(relation(T), list(relation_key)).
:- mode relation__tsort_2(in, out) is semidet.

relation__tsort_2(Rel, Tsort) :-
	relation__domain_sorted_list(Rel, DomList),
	set__init(Vis0),
	relation__c_dfs(Rel, DomList, Vis0, _Vis, [], Tsort),
	relation__check_tsort(Rel, Vis0, Tsort).

:- pred relation__tsort_2(relation(T), list(relation_key), set(relation_key),
	set(relation_key), list(relation_key), list(relation_key)).
:- mode relation__tsort_2(in, in, in, out, in, out) is det.
relation__tsort_2(_Rel, [], Vis, Vis, Tsort, Tsort).
relation__tsort_2(Rel, [X | Xs], Vis0, Vis, Tsort0, Tsort) :-
	stack__init(S0),
	stack__push(S0, X, S1),
	relation__tsort_3(Rel, S1, Vis0, Vis1, Tsort0, Tsort1),
	list__delete_elems(Xs, Tsort1, XsRed),
	relation__tsort_2(Rel, XsRed, Vis1, Vis, Tsort1, Tsort).

:- pred relation__tsort_3(relation(T), stack(relation_key), set(relation_key),
	set(relation_key), list(relation_key), list(relation_key)).
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

:- pred relation__check_tsort(relation(T), set(relation_key),
		list(relation_key)).
:- mode relation__check_tsort(in, in, in) is semidet.
relation__check_tsort(_Rel, _Vis, []).
relation__check_tsort(Rel, Vis, [X | Xs]) :-
	set__insert(Vis, X, Vis1),
	relation__lookup_from(Rel, X, RX),
	set__intersect(Vis1, RX, BackPointers),
	set__empty(BackPointers),
	relation__check_tsort(Rel, Vis1, Xs).

:- pred relation__c_dfs(relation(T), list(relation_key), set(relation_key),
	set(relation_key), list(relation_key), list(relation_key)).
:- mode relation__c_dfs(in, in, in, out, in, out) is det.
relation__c_dfs(_Rel, [], Vis, Vis, Dfs, Dfs).
relation__c_dfs(Rel, [X | Xs], VisIn, VisOut, DfsIn, DfsOut) :-
        ( set__member(X, VisIn) ->
            VisIn = Vis1, DfsIn = Dfs1
        ;
            relation__c_dfs_2(Rel, X, VisIn, Vis1, DfsIn, Dfs1)
        ),
        relation__c_dfs(Rel, Xs, Vis1, VisOut, Dfs1, DfsOut).

:- pred relation__c_dfs_2(relation(T), relation_key, set(relation_key),
	set(relation_key), list(relation_key), list(relation_key)).
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
	%
	% The algorithm used is described in:
	%
	%	R. E. Tarjan, "Depth-first search and
	%	linear graph algorithms,"  SIAM Journal
	%	on Computing, 1, 2 (1972).
relation__atsort(Rel, ATsort) :-
	relation__dfsrev(Rel, DfsRev),
	relation__inverse(Rel, RelInv),
	set_bbbtree__init(Visit),
	relation__atsort_2(DfsRev, RelInv, Visit, [], ATsort0),
	list__reverse(ATsort0, ATsort).

:- pred relation__atsort_2(list(relation_key), relation(T),
	set_bbbtree(relation_key), list(set(T)),
	list(set(T))).
:- mode relation__atsort_2(in, in, in, in, out) is det.

relation__atsort_2([], _, _, ATsort, ATsort).
relation__atsort_2([H | T], RelInv, Visit0, ATsort0, ATsort) :-
	( set_bbbtree__member(H, Visit0) ->
		relation__atsort_2(T, RelInv, Visit0, ATsort0, ATsort)
	;
		relation__dfs_3([H], RelInv, Visit0, [], Visit, CliqueL),
		list__map(relation__lookup_key(RelInv), CliqueL, Clique),
		set__list_to_set(Clique, CliqueSet),
		relation__atsort_2(T, RelInv, Visit, [CliqueSet | ATsort0],
				ATsort)
	).

%------------------------------------------------------------------------------%

	% relation__sc returns the symmetric closure of
	% a relation.
relation__sc(Rel, Sc) :-
	relation__inverse(Rel, Inv),
	relation__to_key_assoc_list(Inv, InvList),
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
	relation__domain_sorted_list(Rel, DomList),
	relation__detect_fake_reflexives(Rel, Rtc, DomList, FakeRefl),

	% Remove them from the RTC, giving us the TC.
	assoc_list__from_corresponding_lists(FakeRefl, FakeRefl, FakeReflComp),
	relation__remove_assoc_list(Rtc, FakeReflComp, Tc).

:- pred relation__detect_fake_reflexives(relation(T), relation(T),
		list(relation_key), list(relation_key)).
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

	% relation__rtc returns the reflexive transitive closure
	% of a relation.
	%
	% Note: This is not the most efficient algorithm (in the sense
	% of minimal number of arc insertions) possible.  However it
	% "reasonably" efficient and, more importantly, is much easier
	% to debug than some others.
	%
	% The algorithm is very simple, and is based on the
	% observation that the RTC of any element in a clique is the
	% same as the RTC of any other element in that clique.  So
	% we visit each clique in reverse topological sorted order,
	% compute the RTC for each element in the clique and then
	% add the appropriate arcs.
	%
relation__rtc(Rel, RTC) :-
	relation__dfs(Rel, Dfs),
	set_bbbtree__init(Visit),

	Rel   = relation(NextElement, ElMap, _, _),
	map__init(FwdMap),
	map__init(BwdMap),
	RTC0 = relation(NextElement, ElMap, FwdMap, BwdMap),

	relation__rtc_2(Dfs, Rel, Visit, RTC0, RTC).

:- pred relation__rtc_2(list(relation_key), relation(T),
	set_bbbtree(relation_key), relation(T), relation(T)).
:- mode relation__rtc_2(in, in, in, in, out) is det.

relation__rtc_2([], _, _, RTC, RTC).
relation__rtc_2([H | T], Rel, Visit0, RTC0, RTC) :-
	( set_bbbtree__member(H, Visit0) ->
		relation__rtc_2(T, Rel, Visit0, RTC0, RTC)
	;
		relation__dfs_3([H], Rel, Visit0, [], Visit, CliqueL0),
		list__sort_and_remove_dups(CliqueL0, CliqueL),
		list__foldl(lambda([K :: in, L0 :: in, L :: out] is det,
			( relation__lookup_from(Rel, K, Followers0),
			  set__to_sorted_list(Followers0, Followers),
			  list__merge_and_remove_dups(Followers, L0, L)
			)),
			CliqueL, CliqueL, CliqueFollowers),
		list__foldl(lambda([K :: in, L0 :: in, L :: out] is det,
			( relation__lookup_from(RTC0, K, Followers0),
			  set__to_sorted_list(Followers0, Followers),
			  list__merge_and_remove_dups(Followers, L0, L)
			)),
			CliqueFollowers, CliqueL, NewFollowers),
		relation__add_cartesian_product(CliqueL, NewFollowers,
			RTC0, RTC1),
		relation__rtc_2(T, Rel, Visit, RTC1, RTC)
	).

:- pred relation__add_cartesian_product(list(relation_key), list(relation_key),
		relation(T), relation(T)).
:- mode relation__add_cartesian_product(in, in, in, out) is det.

relation__add_cartesian_product([], _, RTC, RTC).
relation__add_cartesian_product([K1 | Ks1], Ks2, RTC0, RTC) :-
	relation__add_cartesian_product_2(K1, Ks2, RTC0, RTC1),
	relation__add_cartesian_product(Ks1, Ks2, RTC1, RTC).

:- pred relation__add_cartesian_product_2(relation_key, list(relation_key),
		relation(T), relation(T)).
:- mode relation__add_cartesian_product_2(in, in, in, out) is det.

relation__add_cartesian_product_2(_, [], RTC, RTC).
relation__add_cartesian_product_2(K1, [K2 | Ks2], RTC0, RTC) :-
	relation__add(RTC0, K1, K2, RTC1),
	relation__add_cartesian_product_2(K1, Ks2, RTC1, RTC).

%------------------------------------------------------------------------------%

relation__traverse(Relation, ProcessNode, ProcessEdge) -->
	{ Domain = to_sorted_list(relation__domain(Relation)) },
	relation__traverse_nodes(Domain, Relation, ProcessNode, ProcessEdge).

:- pred relation__traverse_nodes(list(K), relation(K), pred(K, T, T),
		pred(K, K, T, T), T, T).
:- mode relation__traverse_nodes(in, in, pred(in, di, uo) is det,
		pred(in, in, di, uo) is det, di, uo) is det.
:- mode relation__traverse_nodes(in, in, pred(in, in, out) is det,
		pred(in, in, in, out) is det, in, out) is det.

relation__traverse_nodes([], _, _, _) --> [].
relation__traverse_nodes([Node | Nodes], Relation, ProcessNode, ProcessEdge) -->
	{ Children = to_sorted_list(lookup_from(Relation,
			lookup_element(Relation, Node))) },
	ProcessNode(Node),
	relation__traverse_children(Children, Node, Relation, ProcessEdge),
	relation__traverse_nodes(Nodes, Relation, ProcessNode, ProcessEdge).

:- pred relation__traverse_children(list(relation_key), K, relation(K),
		pred(K, K, T, T), T, T).
:- mode relation__traverse_children(in, in, in, pred(in, in, di, uo) is det,
		di, uo) is det.
:- mode relation__traverse_children(in, in, in, pred(in, in, in, out) is det,
		in, out) is det.

relation__traverse_children([], _, _, _) --> [].
relation__traverse_children([ChildKey | Children], Parent,
		Relation, ProcessEdge) -->
	{ Child = lookup_key(Relation, ChildKey) },
	ProcessEdge(Parent, Child),
	relation__traverse_children(Children, Parent, Relation, ProcessEdge).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
% 	Function forms added.

relation__init = R :-
	relation__init(R).

relation__lookup_element(R, X) = K :-
	relation__lookup_element(R, X, K).

relation__lookup_key(R, K) = X :-
	relation__lookup_key(R, K, X).

relation__add(R1, K1, K2) = R2 :-
	relation__add(R1, K1, K2, R2).

relation__add_values(R1, X, Y) = R2 :-
	relation__add_values(R1, X, Y, R2).

relation__add_assoc_list(R1, AL) = R2 :-
	relation__add_assoc_list(R1, AL, R2).

relation__remove(R1, K1, K2) = R2 :-
	relation__remove(R1, K1, K2, R2).

relation__remove_assoc_list(R1, AL) = R2 :-
	relation__remove_assoc_list(R1, AL, R2).

relation__lookup_from(R, K) = S :-
	relation__lookup_from(R, K, S).

relation__lookup_to(R, K) = S :-
	relation__lookup_to(R, K, S).

relation__to_assoc_list(R) = AL :-
	relation__to_assoc_list(R, AL).

relation__to_key_assoc_list(R) = AL :-
	relation__to_key_assoc_list(R, AL).

relation__from_assoc_list(AL) = R :-
	relation__from_assoc_list(AL, R).

relation__domain(R) = S :-
	relation__domain(R, S).

relation__inverse(R1) = R2 :-
	relation__inverse(R1, R2).

relation__compose(R1, R2) = R3 :-
	relation__compose(R1, R2, R3).

relation__dfs(R, K) = Ks :-
	relation__dfs(R, K, Ks).

relation__dfsrev(R, K) = Ks :-
	relation__dfsrev(R, K, Ks).

relation__dfs(R) = Ks :-
	relation__dfs(R, Ks).

relation__dfsrev(R) = Ks :-
	relation__dfsrev(R, Ks).

relation__components(R) = KSS :-
	relation__components(R, KSS).

relation__cliques(R) = KSS :-
	relation__cliques(R, KSS).

relation__reduced(R1) = R2 :-
	relation__reduced(R1, R2).

relation__atsort(R) = Ss :-
	relation__atsort(R, Ss).

relation__sc(R1) = R2 :-
	relation__sc(R1, R2).

relation__tc(R1) = R2 :-
	relation__tc(R1, R2).

relation__rtc(R1) = R2 :-
	relation__rtc(R1, R2).

