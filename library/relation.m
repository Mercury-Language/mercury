%---------------------------------------------------------------------------%
% Copyright (C) 1995-1999,2002-2004 The University of Melbourne.
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
:- import_module enum, list, set, assoc_list, sparse_bitset.

:- type relation(T).

:- type relation_key.

:- instance enum(relation_key).

:- type relation_key_set == sparse_bitset(relation_key).

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

:- pred relation__lookup_key_set_from(relation(T),
		relation_key, relation_key_set).
:- mode relation__lookup_key_set_from(in, in, out) is det.

:- func relation__lookup_key_set_from(relation(T),
		relation_key) = relation_key_set.

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
:- pred relation__lookup_to(relation(T), relation_key, set(relation_key)).
:- mode relation__lookup_to(in, in, out) is det.

:- func relation__lookup_to(relation(T), relation_key) = set(relation_key).

:- pred relation__lookup_key_set_to(relation(T),
		relation_key, relation_key_set).
:- mode relation__lookup_key_set_to(in, in, out) is det.

:- func relation__lookup_key_set_to(relation(T),
		relation_key) = relation_key_set.

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
:- pred relation__dfs(relation(T), relation_key, relation_key_set,
		relation_key_set, list(relation_key)).
:- mode relation__dfs(in, in, in, out, out) is det.

	% relation__dfsrev(Rel, X, Visit0, Visit, DfsRev) is true if 
	% DfsRev is a reverse depth-first sorting of Rel starting at X 
	% providing we have already visited Visit0 nodes, 
	% ie the reverse of Dfs from relation__dfs/5.
	% Visit is Visit0 + DfsRev.
:- pred relation__dfsrev(relation(T), relation_key,
		relation_key_set, relation_key_set,
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
:- import_module require, sparse_bitset.

:- type relation_key ---> relation_key(int).
:- instance enum(relation_key) where [
	to_int(relation_key(Int)) = Int,
	from_int(Int) = relation_key(Int)
].

	% Note that the integer keys in the maps below are
	% actually relation keys.  We use the raw integers as
	% keys to allow type specialization.
:- type relation(T) --->
	relation(
		relation_key,				% Next key
		bimap(T, relation_key),			% Elements <-> keys
		map(int, relation_key_set),		% The mapping U -> V
		map(int, relation_key_set)		% The reverse mapping
							% V -> U
	).

%------------------------------------------------------------------------------%

	% relation__init creates a new relation.
relation__init(relation(relation_key(0), ElMap, FwdMap, BwdMap)) :-
	bimap__init(ElMap),
	map__init(FwdMap),
	map__init(BwdMap).

%------------------------------------------------------------------------------%

	% relation__add_element adds an element to the domain of a
	% relation.  Return the old relation_key if one already
	% exists.
relation__add_element(relation(relation_key(Key0), ElMap0, Fwd, Rev),
		Elem, NewKey, relation(relation_key(Key), ElMap, Fwd, Rev)) :-
	( bimap__search(ElMap0, Elem, NewKey0) ->
		Key = Key0, NewKey = NewKey0, ElMap = ElMap0
	;
		NewKey = relation_key(Key0),
		Key = Key0 + 1,
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
relation__add(relation(Key, ElMap, FwdIn, BwdIn),
		UKey @ relation_key(U), VKey @ relation_key(V),
		relation(Key, ElMap, FwdOut, BwdOut)) :-
	( map__search(FwdIn, U, VSet0) ->
		( contains(VSet0, VKey) ->
			FwdOut = FwdIn
		;
			insert(VSet0, VKey, VSet1),
			map__det_update(FwdIn, U, VSet1, FwdOut)
		)
	;
		init(VSet0),
		insert(VSet0, VKey, VSet1),
		map__det_insert(FwdIn, U, VSet1, FwdOut)
	),
	( map__search(BwdIn, V, USet0) ->
		( contains(USet0, UKey) ->
			BwdOut = BwdIn
		;
			insert(USet0, UKey, USet1),
			map__det_update(BwdIn, V, USet1, BwdOut)
		)
	;
		init(USet0),
		insert(USet0, UKey, USet1),
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
relation__remove(relation(Key, ElMap, FwdIn, BwdIn),
		UKey @ relation_key(U), VKey @ relation_key(V),
		relation(Key, ElMap, FwdOut, BwdOut)) :-
	( map__search(FwdIn, U, VSet0) ->
		delete(VSet0, VKey, VSet1),
		map__det_update(FwdIn, U, VSet1, FwdOut)
	;
		FwdIn = FwdOut
	),
	( map__search(BwdIn, V, USet0) ->
		delete(USet0, UKey, USet1),
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
relation__lookup(relation(_Key, _ElMap, Fwd, _Bwd), relation_key(U), V) :-
	map__search(Fwd, U, VSet),
	member(V, VSet).

%------------------------------------------------------------------------------%

	% relation__reverse_lookup checks to see if an element is
	% in the relation.
relation__reverse_lookup(relation(_Key, _ElMap, _Fwd, Bwd),
		U, relation_key(V)) :-
	map__search(Bwd, V, USet),
	member(U, USet).

%------------------------------------------------------------------------------%

relation__lookup_from(R, U, to_set(Vs)) :-
	relation__lookup_key_set_from(R, U, Vs).

	% relation__lookup_from returns the set of elements
	% y such that xRy, given an x.
relation__lookup_key_set_from(relation(_Key, _ElMap, Fwd, _Bwd),
		relation_key(U), Vs) :-
	( map__search(Fwd, U, Vs0) ->
		Vs = Vs0
	;
		init(Vs)
	).

relation__lookup_key_set_from(R, U) = Vs :-
	relation__lookup_key_set_from(R, U, Vs).

%------------------------------------------------------------------------------%

relation__lookup_to(R, U, to_set(Vs)) :-
	relation__lookup_key_set_to(R, U, Vs).

	% relation__lookup_to returns the set of elements
	% x such that xRy, given some y.
relation__lookup_key_set_to(relation(_Key, _ElMap, _Fwd, Bwd),
		relation_key(V), Us) :-
	( map__search(Bwd, V, Us0) ->
		Us = Us0
	;
		init(Us)
	).

relation__lookup_key_set_to(R, U) = Vs :-
	relation__lookup_key_set_to(R, U, Vs).

%------------------------------------------------------------------------------%

	% relation__to_assoc_list turns a relation into a list of
	% pairs of elements.
relation__to_assoc_list(relation(_Key, ElMap, Fwd, _Bwd), List) :-
	map__keys(Fwd, FwdKeys),
	relation__to_assoc_list_2(Fwd, FwdKeys, ElMap, [], List).

:- pred relation__to_assoc_list_2(map(int, relation_key_set),
	list(int), bimap(T, relation_key),
	assoc_list(T, T), assoc_list(T, T)).
:- mode relation__to_assoc_list_2(in, in, in, in, out) is det.
relation__to_assoc_list_2(_Fwd, [], _, !AssocList).
relation__to_assoc_list_2(Fwd, [Key | Keys], ElementMap, !AssocList) :-
	relation__to_assoc_list_2(Fwd, Keys, ElementMap, !AssocList),
	bimap__reverse_lookup(ElementMap, KeyEl, relation_key(Key)),
	map__lookup(Fwd, Key, Set),
	!:AssocList =
		foldr(
			(func(U, AL) = [KeyEl - V | AL] :-
				bimap__reverse_lookup(ElementMap, V, U)
			), Set, !.AssocList).

	% relation__to_key_assoc_list turns a relation into a list of
	% pairs of elements.
relation__to_key_assoc_list(relation(_Key, _ElMap, Fwd, _Bwd), List) :-
	map__keys(Fwd, FwdKeys),
	relation__to_key_assoc_list_2(Fwd, FwdKeys, [], List).

:- pred relation__to_key_assoc_list_2(map(int, relation_key_set),
	list(int), assoc_list(relation_key, relation_key),
	assoc_list(relation_key, relation_key)).
:- mode relation__to_key_assoc_list_2(in, in, in, out) is det.
relation__to_key_assoc_list_2(_Fwd, [], !AssocList).
relation__to_key_assoc_list_2(Fwd, [Key | Keys], !AssocList) :-
	relation__to_key_assoc_list_2(Fwd, Keys, !AssocList),
	map__lookup(Fwd, Key, Set),
	!:AssocList =
		foldr(
			(func(U, AL) = [relation_key(Key) - U | AL]),
			Set, !.AssocList).

%------------------------------------------------------------------------------%

	% relation__from_assoc_list turns a list of pairs of
	% elements into a relation.
relation__from_assoc_list(AL, Rel) :-
	Rel = list__foldl(
		(func(U - V, Rel0) = Rel1 :-
			relation__add_values(Rel0, U, V, Rel1)
		), AL, relation__init).

%------------------------------------------------------------------------------%

	% relation__domain finds the set of all elements in the domain
	% of a relation.
relation__domain(relation(_Key, ElMap, _Fwd, _Bwd), Dom) :-
	bimap__ordinates(ElMap, DomList),
	sorted_list_to_set(DomList, Dom).

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
relation__compose(R1, R2, !:Compose) :-
	!:Compose = relation__init,

	% Find the set of elements which occur in both the
	% range of R1 and the domain of R2.
	relation__domain(relation__inverse(R1), R1Range),
	relation__domain(R2, R2Domain),
	MatchElements = set__intersect(R1Range, R2Domain),

	% Find the sets of keys to be matched in each relation.
	KeyAL = list__map(
		(func(MatchElem) = R1Keys - R2Keys :-
			relation__lookup_element(R1, MatchElem, R1Key),
			relation__lookup_key_set_to(R1, R1Key, R1Keys),
			relation__lookup_element(R2, MatchElem, R2Key),
			relation__lookup_key_set_from(R2, R2Key, R2Keys)
		),
		to_sorted_list(MatchElements)),
	
	% Find the sets of keys in each relation which will occur in
	% the new relation.
	list__foldl2(
	    (pred((R1Keys - R2Keys)::in, R1NeededKeys0::in, R1NeededKeys1::out,
			R2NeededKeys0::in, R2NeededKeys1::out) is det :-
		R1NeededKeys1 = sparse_bitset__union(R1NeededKeys0, R1Keys),
		R2NeededKeys1 = sparse_bitset__union(R2NeededKeys0, R2Keys)
	    ), KeyAL, sparse_bitset__init, R1NeededKeys,
	    sparse_bitset__init, R2NeededKeys),

	% Add the elements to the composition.
	{!:Compose, KeyMap1} = foldl(copy_element(R1), R1NeededKeys,
				{!.Compose, map__init}),
	{!:Compose, KeyMap2} = foldl(copy_element(R2), R2NeededKeys,
				{!.Compose, map__init}),

	% Add the arcs to the composition.
	list__foldl(
	    (pred((R1Keys - R2Keys)::in, !.Compose::in,
	    		!:Compose::out) is det :-	
		relation__add_cartesian_product(
			map_key_set(KeyMap1, R1Keys),
			map_key_set(KeyMap2, R2Keys),
			!Compose)
	    ), KeyAL, !Compose).

:- func copy_element(relation(T), relation_key,
		{relation(T), map(int, relation_key)}) =
		{relation(T), map(int, relation_key)}.

copy_element(R0, Key, {Compose0, KeyMap0}) = {Compose, KeyMap} :-
	relation__lookup_key(R0, Key, Elem),
	relation__add_element(Compose0, Elem, ComposeKey, Compose),
	Key = relation_key(KeyInt),
	map__det_insert(KeyMap0, KeyInt, ComposeKey, KeyMap).

:- func map_key_set(map(int, relation_key),
		relation_key_set) = relation_key_set.

map_key_set(KeyMap, Set0) = Set :-
	Set = foldl(
		(func(Key0, Set1) = Set2 :-
			Key0 = relation_key(KeyInt),
			map__lookup(KeyMap, KeyInt, Key),
			Set2 = insert(Set1, Key)
		), Set0, init).

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
	init(Vis0),
	relation__dfs_2(Rel, X, Vis0, _, [], DfsRev).

	% relation__dfs/5 performs a depth-first search of
	% a relation.  It returns the elements in visited
	% order.  Providing the nodes Visited0 have already been 
	% visited.
relation__dfs(Rel, X, Visited0, Visited, Dfs) :-
	relation__dfs_2(Rel, X, Visited0, Visited, [], DfsRev),
	list__reverse(DfsRev, Dfs).

	% relation__dfsrev/5 performs a depth-first search of
	% a relation.  It returns the elements in reverse visited
	% order.  Providing the nodes Visited0 have already been
	% visited.
relation__dfsrev(Rel, X, Visited0, Visited, DfsRev) :-
	relation__dfs_2(Rel, X, Visited0, Visited, [], DfsRev).


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
	list__foldl2(relation__dfs_2(Rel), DomList, init, _, [], DfsRev).

:- pred relation__dfs_2(relation(T), relation_key,
		relation_key_set, relation_key_set,
		list(relation_key), list(relation_key)).
:- mode relation__dfs_2(in, in, in, out, in, out) is det.

relation__dfs_2(Rel, Node, !Visit, !DfsRev) :-
	(
		contains(!.Visit, Node)
	->
		true
	;
		relation__lookup_key_set_from(Rel, Node, AdjSet),
		insert(!.Visit, Node, !:Visit),

		% Go and visit all of the node's children first
		{!:Visit, !:DfsRev} = foldl(
			(func(Adj, {!.Visit, !.DfsRev}) =
					{!:Visit, !:DfsRev} :-
				relation__dfs_2(Rel, Adj, !Visit, !DfsRev)
			), AdjSet, {!.Visit, !.DfsRev}),

		!:DfsRev = [Node | !.DfsRev]
	).


%------------------------------------------------------------------------------%


	% relation__is_dag
	%	Does a DFS on the relation.  It is a directed acylic graph
	%	if at each node we never visit an already visited node.
relation__is_dag(R) :-
	relation__domain_sorted_list(R, DomList),
	init(Visit),
	init(AllVisit),
	foldl(relation__is_dag_2(R, Visit), DomList, AllVisit, _).

:- pred relation__is_dag_2(relation(T), relation_key_set,
	relation_key, relation_key_set, relation_key_set).
:- mode relation__is_dag_2(in, in, in, in, out) is semidet.

	% Provided that we never encounter a node that we've visited before
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
relation__is_dag_2(Rel, Visit, Node, !AllVisited) :-
	( contains(Visit, Node) ->
		fail
	; contains(!.AllVisited, Node) ->
		true
	;
		relation__lookup_key_set_from(Rel, Node, AdjSet),
		!:AllVisited = insert(!.AllVisited, Node),
		foldl(relation__is_dag_2(Rel, insert(Visit, Node)),
			AdjSet, !AllVisited)
	).

%------------------------------------------------------------------------------%

	% relation__components takes a relation and returns
	% a set of the connected components.
relation__components(Rel, Set) :-
	relation__domain_sorted_list(Rel, DomList),
	relation__components_2(Rel, DomList, set__init, SetofBitsets),
	Set = set__map(to_set, SetofBitsets).

:- pred relation__components_2(relation(T), list(relation_key),
	set(relation_key_set), set(relation_key_set)).
:- mode relation__components_2(in, in, in, out) is det.
relation__components_2(_Rel, [], Comp, Comp).
relation__components_2(Rel, [X | Xs], Comp0, Comp) :-
	init(Set0),
	queue__list_to_queue([X], Q0),
	relation__reachable_from(Rel, Set0, Q0, Component),
	set__insert(Comp0, Component, Comp1),
	list_to_set(Xs, XsSet `with_type` relation_key_set),
	difference(XsSet, Component, Xs1Set),
	to_sorted_list(Xs1Set, Xs1),
	relation__components_2(Rel, Xs1, Comp1, Comp).

:- pred relation__reachable_from(relation(T), relation_key_set,
	queue(relation_key), relation_key_set).
:- mode relation__reachable_from(in, in, in, out) is det.
relation__reachable_from(Rel, Set0, Q0, Set) :-
	( queue__get(Q0, X, Q1) ->
	    ( contains(Set0, X) ->
		relation__reachable_from(Rel, Set0, Q1, Set)
	    ;
	    	relation__lookup_key_set_from(Rel, X, FwdSet),
	    	relation__lookup_key_set_to(Rel, X, BwdSet),
	    	union(FwdSet, BwdSet, NextSet0),
		difference(NextSet0, Set0, NextSet1),
		to_sorted_list(NextSet1, NextList),
		queue__put_list(Q0, NextList, Q2),
		insert(Set0, X, Set1),
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
	init(Visit),
	relation__cliques_2(DfsRev, RelInv, Visit, Cliques0, Cliques1),
	Cliques = set__map(to_set, Cliques1).

:- pred relation__cliques_2(list(relation_key), relation(T),
	relation_key_set, set(relation_key_set),
	set(relation_key_set)).
:- mode relation__cliques_2(in, in, in, in, out) is det.

relation__cliques_2([], _, _, Cliques, Cliques).
relation__cliques_2([H | T0], RelInv, Visit0, Cliques0, Cliques) :-
		% Do a DFS on R'
	relation__dfs_2(RelInv, H, Visit0, Visit, [], StrongComponent),

		% Insert the cycle into the clique set.
	list_to_set(StrongComponent, StrongComponentSet),
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
	to_sorted_list(S, SList),
	list__map(relation__lookup_key(Rel), SList, EList),
	list_to_set(EList, ESet),
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
	relation__dfsrev(Rel, Tsort0),
	relation__check_tsort(Rel, init, Tsort0),
	Tsort = list__map(relation__lookup_key(Rel), Tsort0).

:- pred relation__check_tsort(relation(T), relation_key_set,
		list(relation_key)).
:- mode relation__check_tsort(in, in, in) is semidet.
relation__check_tsort(_Rel, _Vis, []).
relation__check_tsort(Rel, Vis, [X | Xs]) :-
	insert(Vis, X, Vis1),
	relation__lookup_key_set_from(Rel, X, RX),
	intersect(Vis1, RX, BackPointers),
	empty(BackPointers),
	relation__check_tsort(Rel, Vis1, Xs).

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
	init(Visit),
	relation__atsort_2(DfsRev, RelInv, Visit, [], ATsort0),
	list__reverse(ATsort0, ATsort).

:- pred relation__atsort_2(list(relation_key), relation(T),
	relation_key_set, list(set(T)), list(set(T))).
:- mode relation__atsort_2(in, in, in, in, out) is det.

relation__atsort_2([], _, _, ATsort, ATsort).
relation__atsort_2([H | T], RelInv, Visit0, ATsort0, ATsort) :-
	( contains(Visit0, H) ->
		relation__atsort_2(T, RelInv, Visit0, ATsort0, ATsort)
	;
		relation__dfs_2(RelInv, H, Visit0, Visit, [], CliqueL),
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
	relation__lookup_key_set_from(Rel, X, RelX),
	relation__lookup_key_set_to(Rtc, X, RtcX),
	intersect(RelX, RtcX, Between),
	( empty(Between) ->
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
	init(Visit),

	Rel   = relation(NextElement, ElMap, _, _),
	map__init(FwdMap),
	map__init(BwdMap),
	RTC0 = relation(NextElement, ElMap, FwdMap, BwdMap),

	relation__rtc_2(Dfs, Rel, Visit, RTC0, RTC).

:- pred relation__rtc_2(list(relation_key), relation(T),
	relation_key_set, relation(T), relation(T)).
:- mode relation__rtc_2(in, in, in, in, out) is det.

relation__rtc_2([], _, _, !RTC).
relation__rtc_2([H | T], Rel, Visit0, !RTC) :-
	( contains(Visit0, H) ->
		relation__rtc_2(T, Rel, Visit0, !RTC)
	;
		relation__dfs_2(Rel, H, Visit0, Visit, [], CliqueL0),
		list_to_set(CliqueL0, CliqueL),
		foldl(find_followers(Rel), CliqueL, CliqueL, CliqueFollowers),
		foldl(find_followers(!.RTC), CliqueFollowers,
			CliqueL, NewFollowers),
		relation__add_cartesian_product(CliqueL, NewFollowers, !RTC),
		relation__rtc_2(T, Rel, Visit, !RTC)
	).

:- pred find_followers(relation(T), relation_key,
		relation_key_set, relation_key_set).
:- mode find_followers(in, in, in, out) is det.

find_followers(Rel, K, L0, L) :-
	relation__lookup_key_set_from(Rel, K, Followers),
	union(Followers, L0, L).

:- pred relation__add_cartesian_product(relation_key_set, relation_key_set, 
		relation(T), relation(T)).
:- mode relation__add_cartesian_product(in, in, in, out) is det.

relation__add_cartesian_product(KeySet1, KeySet2, !RTC) :-
    foldl(
	(pred(Key1::in, !.RTC::in, !:RTC::out) is det :-
	    foldl(
		(pred(Key2::in, !.RTC::in, !:RTC::out) is det :-
		    relation__add(!.RTC, Key1, Key2, !:RTC)
		), KeySet2, !RTC)
	), KeySet1, !RTC).

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
	% XXX avoid the sparse_bitset.to_sorted_list here
	% (difficult to do using sparse_bitset.foldl because
	% traverse_children has multiple modes).
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

