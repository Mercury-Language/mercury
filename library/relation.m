%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1999,2002-2006 The University of Melbourne.
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

:- import_module assoc_list.
:- import_module enum.
:- import_module list.
:- import_module set.
:- import_module sparse_bitset.

:- type relation(T).

:- type relation_key.

:- instance enum(relation_key).

:- type relation_key_set == sparse_bitset(relation_key).

    % relation.init creates a new relation.
    %
:- func relation.init = relation(T).
:- pred relation.init(relation(T)::out) is det.

    % relation.add_element adds an element to the domain of a relation.
    % Return the old relation_key if one already exists.
    %
:- pred relation.add_element(relation(T)::in, T::in, relation_key::out,
    relation(T)::out) is det.

    % relation.search_element returns the relation_key associated with a
    % domain element. Fail if the relation_key is not valid.
    %
:- pred relation.search_element(relation(T)::in, T::in, relation_key::out)
    is semidet.

    % relation.lookup_element returns the relation_key associated with a
    % domain element. Abort if the relation_key is not valid.
    %
:- func relation.lookup_element(relation(T), T) = relation_key.
:- pred relation.lookup_element(relation(T)::in, T::in, relation_key::out)
    is det.

    % relation.search_key returns the domain element associated with a
    % relation_key. Fail if the relation_key is not valid.
    %
:- pred relation.search_key(relation(T)::in, relation_key::in, T::out)
    is semidet.

    % relation.lookup_key returns the domain element associated with a
    % relation_key. Abort if the relation_key is not valid.
    %
:- func relation.lookup_key(relation(T), relation_key) = T.
:- pred relation.lookup_key(relation(T)::in, relation_key::in, T::out) is det.

    % relation.add adds an element to the relation.
    %
:- func relation.add(relation(T), relation_key, relation_key) = relation(T).
:- pred relation.add(relation(T)::in, relation_key::in, relation_key::in,
    relation(T)::out) is det.

    % relation.add_values adds an pair of values to the relation's
    % domain and adds an element to the relation.
    %
    % relation.add_values(R0, X, Y, R) :-
    %    relation.add_element(R0, X, XKey, R1),
    %    relation.add_element(R1, Y, YKey, R2),
    %    relation.add(R1, XKey, YKey, R).
    %
:- func relation.add_values(relation(T), T, T) = relation(T).
:- pred relation.add_values(relation(T)::in, T::in, T::in, relation(T)::out)
    is det.

    % relation.add_assoc_list adds a list of elements to a relation.
    %
:- func relation.add_assoc_list(relation(T),
    assoc_list(relation_key, relation_key)) = relation(T).
:- pred relation.add_assoc_list(relation(T)::in,
    assoc_list(relation_key, relation_key)::in, relation(T)::out) is det.

    % relation.remove removes an element from the relation.
    %
:- func relation.remove(relation(T), relation_key, relation_key)
    = relation(T).
:- pred relation.remove(relation(T)::in, relation_key::in, relation_key::in,
    relation(T)::out) is det.

    % relation.remove_assoc_list removes a list of elements from a relation.
    %
:- func relation.remove_assoc_list(relation(T),
    assoc_list(relation_key, relation_key)) = relation(T).
:- pred relation.remove_assoc_list(relation(T)::in,
    assoc_list(relation_key, relation_key)::in, relation(T)::out) is det.

    % relation.lookup checks to see if an element is in the relation.
    %
:- pred relation.lookup(relation(T), relation_key, relation_key).
:- mode relation.lookup(in, in, out) is nondet.
:- mode relation.lookup(in, in, in) is semidet.

    % relation.reverse_lookup checks to see if an element is in the relation.
    %
:- pred relation.reverse_lookup(relation(T), relation_key, relation_key).
:- mode relation.reverse_lookup(in, out, in) is nondet.
:- mode relation.reverse_lookup(in, in, in) is semidet.

    % Given an x, relation.lookup_from returns the set of elements y
    % such that xRy.
    %
:- func relation.lookup_from(relation(T), relation_key) = set(relation_key).
:- pred relation.lookup_from(relation(T)::in, relation_key::in,
    set(relation_key)::out) is det.

:- func relation.lookup_key_set_from(relation(T), relation_key)
    = relation_key_set.
:- pred relation.lookup_key_set_from(relation(T)::in,
    relation_key::in, relation_key_set::out) is det.

    % Given some y, relation.lookup_to returns the set of elements x
    % such that xRy.
    %
:- func relation.lookup_to(relation(T), relation_key) = set(relation_key).
:- pred relation.lookup_to(relation(T)::in, relation_key::in,
    set(relation_key)::out) is det.

:- func relation.lookup_key_set_to(relation(T), relation_key)
    = relation_key_set.
:- pred relation.lookup_key_set_to(relation(T)::in,
    relation_key::in, relation_key_set::out) is det.

    % relation.to_assoc_list turns a relation into a list of pairs of
    % elements.
    %
:- func relation.to_assoc_list(relation(T)) = assoc_list(T, T).
:- pred relation.to_assoc_list(relation(T)::in, assoc_list(T, T)::out) is det.

    % relation.to_key_assoc_list turns a relation into a list of pairs of
    % relation keys.
    %
:- func relation.to_key_assoc_list(relation(T))
    = assoc_list(relation_key, relation_key).
:- pred relation.to_key_assoc_list(relation(T)::in,
    assoc_list(relation_key, relation_key)::out) is det.

    % relation.from_assoc_list turns a list of pairs of elements into
    % a relation.
    %
:- func relation.from_assoc_list(assoc_list(T, T)) = relation(T).
:- pred relation.from_assoc_list(assoc_list(T, T)::in, relation(T)::out)
    is det.

    % relation.domain finds the set of all elements in the domain of a
    % relation.
    %
:- func relation.domain(relation(T)) = set(T).
:- pred relation.domain(relation(T)::in, set(T)::out) is det.

    % relation.inverse(R, R') is true iff for all x, y in the domain of R,
    % xRy if yR'x.
    %
:- func relation.inverse(relation(T)) = relation(T).
:- pred relation.inverse(relation(T)::in, relation(T)::out) is det.

    % relation.compose(R1, R2, R) is true if R is the composition
    % of the relations R1 and R2.
    %
:- func relation.compose(relation(T), relation(T)) = relation(T).
:- pred relation.compose(relation(T)::in, relation(T)::in, relation(T)::out)
    is det.

    % relation.dfs(Rel, X, Dfs) is true if Dfs is a depth-first sorting of Rel
    % starting at X. The set of elements in the list Dfs is exactly equal to
    % the set of elements y such that xR*y, where R* is the reflexive
    % transitive closure of R.
    %
:- func relation.dfs(relation(T), relation_key) = list(relation_key).
:- pred relation.dfs(relation(T)::in, relation_key::in,
    list(relation_key)::out) is det.

    % relation.dfsrev(Rel, X, DfsRev) is true if DfsRev is a reverse
    % depth-first sorting of Rel starting at X. The R* is the reflexive
    % transitive closure of R.
    %
:- func relation.dfsrev(relation(T), relation_key) = list(relation_key).
:- pred relation.dfsrev(relation(T)::in, relation_key::in,
    list(relation_key)::out) is det.

    % relation.dfs(Rel, Dfs) is true if Dfs is a depth-first sorting of Rel,
    % i.e. a list of the nodes in Rel such that it contains all elements
    % in the relation and all the children of a node are placed in the list
    % before the parent.
    %
:- func relation.dfs(relation(T)) = list(relation_key).
:- pred relation.dfs(relation(T)::in, list(relation_key)::out) is det.

    % relation.dfsrev(Rel, DfsRev) is true if DfsRev is a reverse
    % depth-first sorting of Rel.  ie DfsRev is the reverse of Dfs
    % from relation.dfs/2.
    %
:- func relation.dfsrev(relation(T)) = list(relation_key).
:- pred relation.dfsrev(relation(T)::in, list(relation_key)::out) is det.

    % relation.dfs(Rel, X, Visit0, Visit, Dfs) is true if Dfs is a depth-first
    % sorting of Rel starting at X providing we have already visited Visit0
    % nodes, i.e.  a list of nodes such that all the unvisited children of a
    % node are placed in the list before the parent. Visit0 allows us to
    % initialise a set of previously visited nodes. Visit is Dfs + Visit0.
    %
:- pred relation.dfs(relation(T)::in, relation_key::in, relation_key_set::in,
    relation_key_set::out, list(relation_key)::out) is det.

    % relation.dfsrev(Rel, X, Visit0, Visit, DfsRev) is true if DfsRev is a
    % reverse depth-first sorting of Rel starting at X providing we have
    % already visited Visit0 nodes, ie the reverse of Dfs from relation.dfs/5.
    % Visit is Visit0 + DfsRev.
    %
:- pred relation.dfsrev(relation(T)::in, relation_key::in,
    relation_key_set::in, relation_key_set::out, list(relation_key)::out)
    is det.

    % relation.is_dag(R) is true iff R is a directed acyclic graph.
    %
:- pred relation.is_dag(relation(T)::in) is semidet.

    % relation.components(R, Comp) is true if Comp is the set of the
    % connected components of R.
    %
:- func relation.components(relation(T)) = set(set(relation_key)).
:- pred relation.components(relation(T)::in, set(set(relation_key))::out)
    is det.

    % relation.cliques(R, Cliques) is true if Cliques is the set of the
    % strongly connected components (cliques) of R.
    %
:- func relation.cliques(relation(T)) = set(set(relation_key)).
:- pred relation.cliques(relation(T)::in, set(set(relation_key))::out) is det.

    % relation.reduced(R, Red) is true if Red is the reduced relation
    % (relation of cliques) obtained from R.
    %
:- func relation.reduced(relation(T)) = relation(set(T)).
:- pred relation.reduced(relation(T)::in, relation(set(T))::out) is det.

    % relation.tsort(R, TS) is true if TS is a topological sorting of R.
    % It fails if R is cyclic.
    %
:- pred relation.tsort(relation(T)::in, list(T)::out) is semidet.

    % relation.atsort(R, ATS) is true if ATS is a topological sorting
    % of the cliques in R.
    %
:- func relation.atsort(relation(T)) = list(set(T)).
:- pred relation.atsort(relation(T)::in, list(set(T))::out) is det.

    % relation.sc(R, SC) is true if SC is the symmetric closure of R.
    % In graph terms, symmetric closure is the same as turning a directed graph
    % into an undirected graph.
    %
:- func relation.sc(relation(T)) = relation(T).
:- pred relation.sc(relation(T)::in, relation(T)::out) is det.

    % relation.tc(R, TC) is true if TC is the transitive closure of R.
    %
:- func relation.tc(relation(T)) = relation(T).
:- pred relation.tc(relation(T)::in, relation(T)::out) is det.

    % relation.rtc(R, RTC) is true if RTC is the reflexive transitive closure
    % of R.
    %
:- func relation.rtc(relation(T)) = relation(T).
:- pred relation.rtc(relation(T)::in, relation(T)::out) is det.

    % relation.traverse(R, ProcessNode, ProcessEdge) will traverse a relation
    % calling ProcessNode for each node in the relation and ProcessEdge for
    % each edge in the relation. Each node is processed followed by all the
    % edges originating at that node, until all nodes have been processed.
    %
:- pred relation.traverse(relation(K), pred(K, T, T), pred(K, K, T, T), T, T).
:- mode relation.traverse(in, pred(in, di, uo) is det,
    pred(in, in, di, uo) is det, di, uo) is det.
:- mode relation.traverse(in, pred(in, in, out) is det,
    pred(in, in, in, out) is det, in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module queue.
:- import_module require.
:- import_module sparse_bitset.
:- import_module stack.
:- import_module std_util.

:- type relation_key
    --->    relation_key(int).

:- type key_map     == map(int, relation_key).
:- type key_set_map == map(int, relation_key_set).

:- instance enum(relation_key) where [
    to_int(relation_key(Int)) = Int,
    from_int(Int) = relation_key(Int)
].

    % Note that the integer keys in the maps below are actually relation keys.
    % We use the raw integers as keys to allow type specialization.
:- type relation(T)
    --->    relation(
                relation_key,                   % Next key
                bimap(T, relation_key),         % Elements <-> keys
                key_set_map,                    % The mapping U -> V
                key_set_map                     % The reverse mapping V -> U
            ).

%------------------------------------------------------------------------------%

relation.init(relation(relation_key(0), ElMap, FwdMap, BwdMap)) :-
    bimap.init(ElMap),
    map.init(FwdMap),
    map.init(BwdMap).

%------------------------------------------------------------------------------%

relation.add_element(Rel0, Elem, NewKey, Rel) :-
    Rel0 = relation(relation_key(Key0), ElMap0, Fwd, Rev),
    ( bimap.search(ElMap0, Elem, NewKey0) ->
        NewKey = NewKey0,
        Rel = Rel0
    ;
        NewKey = relation_key(Key0),
        Key = Key0 + 1,
        bimap.set(ElMap0, Elem, NewKey, ElMap),
        Rel = relation(relation_key(Key), ElMap, Fwd, Rev)
    ).

%------------------------------------------------------------------------------%

relation.search_element(relation(_Key, ElMap, _Fwd, _Rev), Elem, Key) :-
    bimap.search(ElMap, Elem, Key).

relation.lookup_element(Rel, Elem, Key) :-
    ( relation.search_element(Rel, Elem, Key0) ->
        Key = Key0
    ;
        error("relation.lookup_element")
    ).

%------------------------------------------------------------------------------%

relation.search_key(relation(_Key, ElMap, _Fwd, _Rev), Key, Elem) :-
    bimap.search(ElMap, Elem, Key).

relation.lookup_key(Rel, Key, Elem) :-
    ( relation.search_key(Rel, Key, Elem0) ->
        Elem = Elem0
    ;
        error("relation.lookup_key")
    ).

%------------------------------------------------------------------------------%

relation.add_values(R0, X, Y, R) :-
    relation.add_element(R0, X, XKey, R1),
    relation.add_element(R1, Y, YKey, R2),
    relation.add(R2, XKey, YKey, R).

relation.add(Rel0, UKey @ relation_key(U), VKey @ relation_key(V), Rel) :-
    Rel0 = relation(Key, ElMap, FwdIn, BwdIn),
    ( map.search(FwdIn, U, VSet0) ->
        ( contains(VSet0, VKey) ->
            FwdOut = FwdIn
        ;
            insert(VSet0, VKey, VSet1),
            map.det_update(FwdIn, U, VSet1, FwdOut)
        )
    ;
        init(VSet0),
        insert(VSet0, VKey, VSet1),
        map.det_insert(FwdIn, U, VSet1, FwdOut)
    ),
    ( map.search(BwdIn, V, USet0) ->
        ( contains(USet0, UKey) ->
            BwdOut = BwdIn
        ;
            insert(USet0, UKey, USet1),
            map.det_update(BwdIn, V, USet1, BwdOut)
        )
    ;
        init(USet0),
        insert(USet0, UKey, USet1),
        map.det_insert(BwdIn, V, USet1, BwdOut)
    ),
    Rel = relation(Key, ElMap, FwdOut, BwdOut).

:- pred relation.sv_add(relation_key::in, relation_key::in,
    relation(T)::in, relation(T)::out) is det.

relation.sv_add(UKey, VKey, Rel0, Rel) :-
    relation.add(Rel0, UKey, VKey, Rel).

%------------------------------------------------------------------------------%

relation.add_assoc_list(Rel, [], Rel).
relation.add_assoc_list(Rel0, [U - V | Elems], Rel) :-
    relation.add(Rel0, U, V, Rel1),
    relation.add_assoc_list(Rel1, Elems, Rel).

%------------------------------------------------------------------------------%

relation.remove(Rel0, UKey @ relation_key(U), VKey @ relation_key(V), Rel) :-
    Rel0 = relation(Key, ElMap, FwdIn, BwdIn),
    ( map.search(FwdIn, U, VSet0) ->
        delete(VSet0, VKey, VSet1),
        map.det_update(FwdIn, U, VSet1, FwdOut)
    ;
        FwdIn = FwdOut
    ),
    ( map.search(BwdIn, V, USet0) ->
        delete(USet0, UKey, USet1),
        map.det_update(BwdIn, V, USet1, BwdOut)
    ;
        BwdIn = BwdOut
    ),
    Rel = relation(Key, ElMap, FwdOut, BwdOut).

%------------------------------------------------------------------------------%

relation.remove_assoc_list(Rel, [], Rel).
relation.remove_assoc_list(Rel0, [U - V | Elems], Rel) :-
    relation.remove(Rel0, U, V, Rel1),
    relation.remove_assoc_list(Rel1, Elems, Rel).

%------------------------------------------------------------------------------%

relation.lookup(relation(_Key, _ElMap, Fwd, _Bwd), relation_key(U), V) :-
    map.search(Fwd, U, VSet),
    member(V, VSet).

%------------------------------------------------------------------------------%

relation.reverse_lookup(Rel, U, relation_key(V)) :-
    Rel = relation(_Key, _ElMap, _Fwd, Bwd),
    map.search(Bwd, V, USet),
    member(U, USet).

%------------------------------------------------------------------------------%

relation.lookup_from(R, U, to_set(Vs)) :-
    relation.lookup_key_set_from(R, U, Vs).

relation.lookup_key_set_from(Rel, relation_key(U), Vs) :-
    Rel = relation(_Key, _ElMap, Fwd, _Bwd),
    ( map.search(Fwd, U, Vs0) ->
        Vs = Vs0
    ;
        init(Vs)
    ).

relation.lookup_key_set_from(R, U) = Vs :-
    relation.lookup_key_set_from(R, U, Vs).

%------------------------------------------------------------------------------%

relation.lookup_to(R, U, to_set(Vs)) :-
    relation.lookup_key_set_to(R, U, Vs).

    % relation.lookup_to returns the set of elements
    % x such that xRy, given some y.
relation.lookup_key_set_to(Rel, relation_key(V), Us) :-
    Rel = relation(_Key, _ElMap, _Fwd, Bwd),
    ( map.search(Bwd, V, Us0) ->
        Us = Us0
    ;
        init(Us)
    ).

relation.lookup_key_set_to(R, U) = Vs :-
    relation.lookup_key_set_to(R, U, Vs).

%------------------------------------------------------------------------------%

relation.to_assoc_list(relation(_Key, ElMap, Fwd, _Bwd), List) :-
    map.keys(Fwd, FwdKeys),
    relation.to_assoc_list_2(Fwd, FwdKeys, ElMap, [], List).

:- pred relation.to_assoc_list_2(key_set_map::in,
    list(int)::in, bimap(T, relation_key)::in,
    assoc_list(T, T)::in, assoc_list(T, T)::out) is det.

relation.to_assoc_list_2(_Fwd, [], _, !AssocList).
relation.to_assoc_list_2(Fwd, [Key | Keys], ElementMap, !AssocList) :-
    relation.to_assoc_list_2(Fwd, Keys, ElementMap, !AssocList),
    bimap.reverse_lookup(ElementMap, KeyEl, relation_key(Key)),
    map.lookup(Fwd, Key, Set),
    sparse_bitset.foldr(accumulate_rev_lookup(ElementMap, KeyEl), Set,
        !AssocList).

:- pred accumulate_rev_lookup(bimap(T, relation_key)::in, T::in,
    relation_key::in, assoc_list(T, T)::in, assoc_list(T, T)::out) is det.

accumulate_rev_lookup(ElementMap, KeyEl, U, !AL) :-
    bimap.reverse_lookup(ElementMap, V, U),
    !:AL = [KeyEl - V | !.AL].

relation.to_key_assoc_list(relation(_Key, _ElMap, Fwd, _Bwd), List) :-
    map.keys(Fwd, FwdKeys),
    relation.to_key_assoc_list_2(Fwd, FwdKeys, [], List).

:- pred relation.to_key_assoc_list_2(key_set_map::in, list(int)::in,
    assoc_list(relation_key, relation_key)::in,
    assoc_list(relation_key, relation_key)::out) is det.

relation.to_key_assoc_list_2(_Fwd, [], !AssocList).
relation.to_key_assoc_list_2(Fwd, [Key | Keys], !AssocList) :-
    relation.to_key_assoc_list_2(Fwd, Keys, !AssocList),
    map.lookup(Fwd, Key, Set),
    sparse_bitset.foldr(accumulate_with_key(relation_key(Key)), Set,
        !AssocList).

:- pred accumulate_with_key(relation_key::in, relation_key::in,
    assoc_list(relation_key, relation_key)::in,
    assoc_list(relation_key, relation_key)::out) is det.

accumulate_with_key(RelKey, U, !AL) :-
    !:AL = [RelKey - U | !.AL].

%------------------------------------------------------------------------------%

    % relation.from_assoc_list turns a list of pairs of
    % elements into a relation.
relation.from_assoc_list(AL, Rel) :-
    Rel = list.foldl(
        (func(U - V, Rel0) = Rel1 :-
            relation.add_values(Rel0, U, V, Rel1)
        ), AL, relation.init).

%------------------------------------------------------------------------------%

relation.domain(relation(_Key, ElMap, _Fwd, _Bwd), Dom) :-
    bimap.ordinates(ElMap, DomList),
    sorted_list_to_set(DomList, Dom).

:- pred relation.domain_sorted_list(relation(T)::in, list(relation_key)::out)
    is det.

relation.domain_sorted_list(relation(_Key, ElMap, _Fwd, _Bwd), Dom) :-
    bimap.coordinates(ElMap, Dom).

%------------------------------------------------------------------------------%

relation.inverse(Rel, InvRel) :-
    Rel = relation(Key, ElMap, Fwd, Bwd),
    InvRel = relation(Key, ElMap, Bwd, Fwd).

%------------------------------------------------------------------------------%

relation.compose(R1, R2, !:Compose) :-
    !:Compose = relation.init,

    % Find the set of elements which occur in both the
    % range of R1 and the domain of R2.
    relation.domain(relation.inverse(R1), R1Range),
    relation.domain(R2, R2Domain),
    MatchElements = set.intersect(R1Range, R2Domain),

    % Find the sets of keys to be matched in each relation.
    KeyAL = list.map(
        (func(MatchElem) = R1Keys - R2Keys :-
            relation.lookup_element(R1, MatchElem, R1Key),
            relation.lookup_key_set_to(R1, R1Key, R1Keys),
            relation.lookup_element(R2, MatchElem, R2Key),
            relation.lookup_key_set_from(R2, R2Key, R2Keys)
        ),
        to_sorted_list(MatchElements)),

    % Find the sets of keys in each relation which will occur in
    % the new relation.
    list.foldl2(find_new_rel_keys, KeyAL,
        sparse_bitset.init, R1NeededKeys,
        sparse_bitset.init, R2NeededKeys),

    % Add the elements to the composition.
    sparse_bitset.foldl2(copy_element(R1), R1NeededKeys, !Compose,
        map.init, KeyMap1),
    sparse_bitset.foldl2(copy_element(R2), R2NeededKeys, !Compose,
        map.init, KeyMap2),

    % Add the arcs to the composition.
    list.foldl(add_compose_arcs(KeyMap1, KeyMap2), KeyAL, !Compose).

:- pred find_new_rel_keys(pair(relation_key_set)::in,
    relation_key_set::in, relation_key_set::out,
    relation_key_set::in, relation_key_set::out) is det.

find_new_rel_keys(R1Keys - R2Keys,
        R1NeededKeys0, R1NeededKeys1, R2NeededKeys0, R2NeededKeys1) :-
    R1NeededKeys1 = sparse_bitset.union(R1NeededKeys0, R1Keys),
    R2NeededKeys1 = sparse_bitset.union(R2NeededKeys0, R2Keys).

:- pred add_compose_arcs(key_map::in, key_map::in, pair(relation_key_set)::in,
    relation(T)::in, relation(T)::out) is det.

add_compose_arcs(KeyMap1, KeyMap2, R1Keys - R2Keys, !Compose) :-
    relation.add_cartesian_product(
        map_key_set(KeyMap1, R1Keys),
        map_key_set(KeyMap2, R2Keys),
        !Compose).

:- pred copy_element(relation(T)::in, relation_key::in,
    relation(T)::in, relation(T)::out, key_map::in, key_map::out) is det.

copy_element(R0, Key, !Compose, !KeyMap) :-
    relation.lookup_key(R0, Key, Elem),
    relation.add_element(!.Compose, Elem, ComposeKey, !:Compose),
    Key = relation_key(KeyInt),
    map.det_insert(!.KeyMap, KeyInt, ComposeKey, !:KeyMap).

:- func map_key_set(key_map, relation_key_set) = relation_key_set.

map_key_set(KeyMap, Set0) = Set :-
    sparse_bitset.foldl(accumulate_key_set(KeyMap), Set0, init, Set).

:- pred accumulate_key_set(key_map::in, relation_key::in,
    relation_key_set::in, relation_key_set::out) is det.

accumulate_key_set(KeyMap, Key0, !Set) :-
    Key0 = relation_key(KeyInt),
    map.lookup(KeyMap, KeyInt, Key),
    !:Set = insert(!.Set, Key).

%------------------------------------------------------------------------------%

relation.dfs(Rel, X, Dfs) :-
    relation.dfsrev(Rel, X, DfsRev),
    list.reverse(DfsRev, Dfs).

relation.dfsrev(Rel, X, DfsRev) :-
    init(Vis0),
    relation.dfs_2(Rel, X, Vis0, _, [], DfsRev).

relation.dfs(Rel, X, Visited0, Visited, Dfs) :-
    relation.dfs_2(Rel, X, Visited0, Visited, [], DfsRev),
    list.reverse(DfsRev, Dfs).

relation.dfsrev(Rel, X, Visited0, Visited, DfsRev) :-
    relation.dfs_2(Rel, X, Visited0, Visited, [], DfsRev).

relation.dfs(Rel, Dfs) :-
    relation.dfsrev(Rel, DfsRev),
    list.reverse(DfsRev, Dfs).

relation.dfsrev(Rel, DfsRev) :-
    relation.domain_sorted_list(Rel, DomList),
    list.foldl2(relation.dfs_2(Rel), DomList, init, _, [], DfsRev).

:- pred relation.dfs_2(relation(T)::in, relation_key::in,
    relation_key_set::in, relation_key_set::out,
    list(relation_key)::in, list(relation_key)::out) is det.

relation.dfs_2(Rel, Node, !Visit, !DfsRev) :-
    ( contains(!.Visit, Node) ->
        true
    ;
        relation.lookup_key_set_from(Rel, Node, AdjSet),
        insert(!.Visit, Node, !:Visit),

        % Go and visit all of the node's children first.
        sparse_bitset.foldl2(relation.dfs_2(Rel), AdjSet, !Visit, !DfsRev),
        !:DfsRev = [Node | !.DfsRev]
    ).

%------------------------------------------------------------------------------%

relation.is_dag(R) :-
    % Does a DFS on the relation. It is a directed acylic graph
    % if at each node we never visit an already visited node.
    relation.domain_sorted_list(R, DomList),
    init(Visit),
    init(AllVisit),
    foldl(relation.is_dag_2(R, Visit), DomList, AllVisit, _).

:- pred relation.is_dag_2(relation(T)::in, relation_key_set::in,
    relation_key::in, relation_key_set::in, relation_key_set::out)
    is semidet.

    % Provided that we never encounter a node that we've visited before
    % during the current DFS, the graph isn't cyclic.
    % NB It is possible that we have visited a node before while doing a
    % DFS from another node.
    %
    % ie        2     3
    %        \   /
    %         \ /
    %          1
    %
    % 1 will be visited by a DFS from both 2 and 3.
    %
relation.is_dag_2(Rel, Visit, Node, !AllVisited) :-
    ( contains(Visit, Node) ->
        fail
    ; contains(!.AllVisited, Node) ->
        true
    ;
        relation.lookup_key_set_from(Rel, Node, AdjSet),
        !:AllVisited = insert(!.AllVisited, Node),
        foldl(relation.is_dag_2(Rel, insert(Visit, Node)), AdjSet,
            !AllVisited)
    ).

%------------------------------------------------------------------------------%

relation.components(Rel, Set) :-
    relation.domain_sorted_list(Rel, DomList),
    relation.components_2(Rel, DomList, set.init, SetofBitsets),
    Set = set.map(to_set, SetofBitsets).

:- pred relation.components_2(relation(T)::in, list(relation_key)::in,
    set(relation_key_set)::in, set(relation_key_set)::out) is det.

relation.components_2(_Rel, [], !Comp).
relation.components_2(Rel, [X | Xs], !Comp) :-
    init(Set0),
    queue.list_to_queue([X], Q0),
    relation.reachable_from(Rel, Q0, Set0, Component),
    set.insert(!.Comp, Component, !:Comp),
    list_to_set(Xs, XsSet `with_type` relation_key_set),
    difference(XsSet, Component, Xs1Set),
    to_sorted_list(Xs1Set, Xs1),
    relation.components_2(Rel, Xs1, !Comp).

:- pred relation.reachable_from(relation(T)::in, queue(relation_key)::in,
    relation_key_set::in, relation_key_set::out) is det.

relation.reachable_from(Rel, Q0, !Set) :-
    ( queue.get(Q0, X, Q1) ->
        ( contains(!.Set, X) ->
            relation.reachable_from(Rel, Q1, !Set)
        ;
            relation.lookup_key_set_from(Rel, X, FwdSet),
            relation.lookup_key_set_to(Rel, X, BwdSet),
            union(FwdSet, BwdSet, NextSet0),
            difference(NextSet0, !.Set, NextSet1),
            to_sorted_list(NextSet1, NextList),
            queue.put_list(Q0, NextList, Q2),
            insert(!.Set, X, !:Set),
            relation.reachable_from(Rel, Q2, !Set)
        )
    ;
        true
    ).

%------------------------------------------------------------------------------%

    % relation cliques
    %   take a relation and return the set of strongly connected
    %   components
    %
    %   Works using the following algorith
    %       1. Topologically sort the nodes.  Then number the nodes
    %          so the highest num is the first node in the
    %          topological sort.
    %       2. Reverse the relation ie R'
    %       3. Starting from the highest numbered node do a DFS on
    %          R'.  All the nodes visited are a member of the cycle.
    %       4. From the next highest non-visited node do a DFS on
    %          R' (not including visited nodes).  This is the next
    %          cycle.
    %       5. Repeat step 4 until all nodes visited.
relation.cliques(Rel, Cliques) :-
    % Effectively assigns a numbering to the nodes.
    relation.dfsrev(Rel, DfsRev),
    relation.inverse(Rel, RelInv),
    set.init(Cliques0),
    init(Visit),
    relation.cliques_2(DfsRev, RelInv, Visit, Cliques0, Cliques1),
    Cliques = set.map(to_set, Cliques1).

:- pred relation.cliques_2(list(relation_key), relation(T),
    relation_key_set, set(relation_key_set),
    set(relation_key_set)).
:- mode relation.cliques_2(in, in, in, in, out) is det.

relation.cliques_2([], _, _, Cliques, Cliques).
relation.cliques_2([H | T0], RelInv, Visit0, Cliques0, Cliques) :-
    % Do a DFS on R'
    relation.dfs_2(RelInv, H, Visit0, Visit, [], StrongComponent),

    % Insert the cycle into the clique set.
    list_to_set(StrongComponent, StrongComponentSet),
    set.insert(Cliques0, StrongComponentSet, Cliques1),

    % Delete all the visited elements, so first element of the list
    % is the next highest number node.
    list.delete_elems(T0, StrongComponent, T),
    relation.cliques_2(T, RelInv, Visit, Cliques1, Cliques).

%------------------------------------------------------------------------------%

relation.reduced(Rel, Red) :-
    relation.cliques(Rel, Cliques),
    set.to_sorted_list(Cliques, CliqList),
    relation.init(Red0),
    map.init(CliqMap0),
    relation.make_clique_map(Rel, CliqList, CliqMap0, CliqMap, Red0, Red1),
    relation.to_key_assoc_list(Rel, RelAL),
    relation.make_reduced_graph(CliqMap, RelAL, Red1, Red).

:- pred relation.make_clique_map(relation(T)::in, list(set(relation_key))::in,
    map(relation_key, relation_key)::in,
    map(relation_key, relation_key)::out,
    relation(set(T))::in, relation(set(T))::out) is det.

relation.make_clique_map(_Rel, [], !Map, !Red).
relation.make_clique_map(Rel, [S | Ss], !Map, !Red) :-
    to_sorted_list(S, SList),
    list.map(relation.lookup_key(Rel), SList, EList),
    list_to_set(EList, ESet),
    relation.add_element(!.Red, ESet, SKey, !:Red),
    relation.make_clique_map_2(SKey, SList, !Map),
    relation.make_clique_map(Rel, Ss, !Map, !Red).

:- pred relation.make_clique_map_2(relation_key::in, list(relation_key)::in,
    map(relation_key, relation_key)::in, map(relation_key, relation_key)::out)
    is det.

relation.make_clique_map_2(_Key, [], !Map).
relation.make_clique_map_2(Key, [X | Xs], !Map) :-
    map.set(!.Map, X, Key, !:Map),
    relation.make_clique_map_2(Key, Xs, !Map).

:- pred relation.make_reduced_graph(map(relation_key, relation_key)::in,
    assoc_list(relation_key, relation_key)::in,
    relation(set(T))::in, relation(set(T))::out) is det.

relation.make_reduced_graph(_Map, [], !Rel).
relation.make_reduced_graph(Map, [U - V | Rest], !Rel) :-
    map.lookup(Map, U, USet),
    map.lookup(Map, V, VSet),
    ( USet = VSet ->
        true
    ;
        relation.add(!.Rel, USet, VSet, !:Rel)
    ),
    relation.make_reduced_graph(Map, Rest, !Rel).

%------------------------------------------------------------------------------%

relation.tsort(Rel, Tsort) :-
    relation.dfsrev(Rel, Tsort0),
    relation.check_tsort(Rel, init, Tsort0),
    Tsort = list.map(relation.lookup_key(Rel), Tsort0).

:- pred relation.check_tsort(relation(T)::in, relation_key_set::in,
    list(relation_key)::in) is semidet.

relation.check_tsort(_Rel, _Vis, []).
relation.check_tsort(Rel, Vis, [X | Xs]) :-
    insert(Vis, X, Vis1),
    relation.lookup_key_set_from(Rel, X, RX),
    intersect(Vis1, RX, BackPointers),
    empty(BackPointers),
    relation.check_tsort(Rel, Vis1, Xs).

%------------------------------------------------------------------------------%

relation.atsort(Rel, ATsort) :-
    % relation.atsort returns a topological sorting
    % of the cliques in a relation.
    %
    % The algorithm used is described in:
    %
    %   R. E. Tarjan, "Depth-first search and
    %   linear graph algorithms,"  SIAM Journal
    %   on Computing, 1, 2 (1972).
    relation.dfsrev(Rel, DfsRev),
    relation.inverse(Rel, RelInv),
    init(Visit),
    relation.atsort_2(DfsRev, RelInv, Visit, [], ATsort0),
    list.reverse(ATsort0, ATsort).

:- pred relation.atsort_2(list(relation_key)::in, relation(T)::in,
    relation_key_set::in, list(set(T))::in, list(set(T))::out) is det.

relation.atsort_2([], _, _, !ATsort).
relation.atsort_2([H | T], RelInv, Visit0, !ATsort) :-
    ( contains(Visit0, H) ->
        relation.atsort_2(T, RelInv, Visit0, !ATsort)
    ;
        relation.dfs_2(RelInv, H, Visit0, Visit, [], CliqueL),
        list.map(relation.lookup_key(RelInv), CliqueL, Clique),
        set.list_to_set(Clique, CliqueSet),
        relation.atsort_2(T, RelInv, Visit, [CliqueSet | !.ATsort], !:ATsort)
    ).

%------------------------------------------------------------------------------%

relation.sc(Rel, Sc) :-
    relation.inverse(Rel, Inv),
    relation.to_key_assoc_list(Inv, InvList),
    relation.add_assoc_list(Rel, InvList, Sc).

%------------------------------------------------------------------------------%

relation.tc(Rel, Tc) :-
    % relation.tc returns the transitive closure of a relation.
    % We use this procedure:
    %
    %   - Compute the reflexive transitive closure.
    %   - Find the "fake reflexives", that is, the
    %     set of elements x for which xR+x should
    %     not be true.  This is done by noting that
    %     R+ = R . R* (where '.' denotes composition).
    %     Therefore x is a fake reflexive iff the set
    %     { x | yRx and xR*y } is empty.
    %   - Remove those elements from the reflexive
    %     transitive closure computed above.
    relation.rtc(Rel, Rtc),

    % Find the fake reflexives.
    relation.domain_sorted_list(Rel, DomList),
    relation.detect_fake_reflexives(Rel, Rtc, DomList, FakeRefl),

    % Remove them from the RTC, giving us the TC.
    assoc_list.from_corresponding_lists(FakeRefl, FakeRefl, FakeReflComp),
    relation.remove_assoc_list(Rtc, FakeReflComp, Tc).

:- pred relation.detect_fake_reflexives(relation(T)::in, relation(T)::in,
    list(relation_key)::in, list(relation_key)::out) is det.

relation.detect_fake_reflexives(_Rel, _Rtc, [], []).
relation.detect_fake_reflexives(Rel, Rtc, [X | Xs], FakeRefl) :-
    relation.detect_fake_reflexives(Rel, Rtc, Xs, Fake1),
    relation.lookup_key_set_from(Rel, X, RelX),
    relation.lookup_key_set_to(Rtc, X, RtcX),
    intersect(RelX, RtcX, Between),
    ( empty(Between) ->
        FakeRefl = [X | Fake1]
    ;
        FakeRefl = Fake1
    ).

%------------------------------------------------------------------------------%

relation.rtc(Rel, RTC) :-
    % relation.rtc returns the reflexive transitive closure of a relation.
    %
    % Note: This is not the most efficient algorithm (in the sense of minimal
    % number of arc insertions) possible. However it "reasonably" efficient
    % and, more importantly, is much easier to debug than some others.
    %
    % The algorithm is very simple, and is based on the observation that the
    % RTC of any element in a clique is the same as the RTC of any other
    % element in that clique. So we visit each clique in reverse topological
    % sorted order, compute the RTC for each element in the clique and then
    % add the appropriate arcs.
    %
    relation.dfs(Rel, Dfs),
    init(Visit),

    Rel   = relation(NextElement, ElMap, _, _),
    map.init(FwdMap),
    map.init(BwdMap),
    RTC0 = relation(NextElement, ElMap, FwdMap, BwdMap),

    relation.rtc_2(Dfs, Rel, Visit, RTC0, RTC).

:- pred relation.rtc_2(list(relation_key)::in, relation(T)::in,
    relation_key_set::in, relation(T)::in, relation(T)::out) is det.

relation.rtc_2([], _, _, !RTC).
relation.rtc_2([H | T], Rel, Visit0, !RTC) :-
    ( contains(Visit0, H) ->
        relation.rtc_2(T, Rel, Visit0, !RTC)
    ;
        relation.dfs_2(Rel, H, Visit0, Visit, [], CliqueL0),
        list_to_set(CliqueL0, CliqueL),
        foldl(find_followers(Rel), CliqueL, CliqueL, CliqueFollowers),
        foldl(find_followers(!.RTC), CliqueFollowers, CliqueL, NewFollowers),
        relation.add_cartesian_product(CliqueL, NewFollowers, !RTC),
        relation.rtc_2(T, Rel, Visit, !RTC)
    ).

:- pred find_followers(relation(T)::in, relation_key::in,
    relation_key_set::in, relation_key_set::out) is det.

find_followers(Rel, K, L0, L) :-
    relation.lookup_key_set_from(Rel, K, Followers),
    union(Followers, L0, L).

:- pred relation.add_cartesian_product(relation_key_set::in,
    relation_key_set::in, relation(T)::in, relation(T)::out) is det.

relation.add_cartesian_product(KeySet1, KeySet2, !RTC) :-
    foldl((pred(Key1::in, !.RTC::in, !:RTC::out) is det :-
        foldl(relation.sv_add(Key1), KeySet2, !RTC)
    ), KeySet1, !RTC).

%------------------------------------------------------------------------------%

relation.traverse(Relation, ProcessNode, ProcessEdge, !Acc) :-
    Domain = to_sorted_list(relation.domain(Relation)),
    relation.traverse_nodes(Domain, Relation, ProcessNode, ProcessEdge, !Acc).

:- pred relation.traverse_nodes(list(K), relation(K), pred(K, T, T),
    pred(K, K, T, T), T, T).
:- mode relation.traverse_nodes(in, in, pred(in, di, uo) is det,
    pred(in, in, di, uo) is det, di, uo) is det.
:- mode relation.traverse_nodes(in, in, pred(in, in, out) is det,
    pred(in, in, in, out) is det, in, out) is det.

relation.traverse_nodes([], _, _, _, !Acc).
relation.traverse_nodes([Node | Nodes], Relation, ProcessNode, ProcessEdge,
        !Acc) :-
    % XXX avoid the sparse_bitset.to_sorted_list here
    % (difficult to do using sparse_bitset.foldl because
    % traverse_children has multiple modes).
    Children = to_sorted_list(lookup_from(Relation,
        lookup_element(Relation, Node))),
    ProcessNode(Node, !Acc),
    relation.traverse_children(Children, Node, Relation, ProcessEdge, !Acc),
    relation.traverse_nodes(Nodes, Relation, ProcessNode, ProcessEdge, !Acc).

:- pred relation.traverse_children(list(relation_key), K, relation(K),
    pred(K, K, T, T), T, T).
:- mode relation.traverse_children(in, in, in, pred(in, in, di, uo) is det,
    di, uo) is det.
:- mode relation.traverse_children(in, in, in, pred(in, in, in, out) is det,
    in, out) is det.

relation.traverse_children([], _, _, _, !Acc).
relation.traverse_children([ChildKey | Children], Parent,
        Relation, ProcessEdge, !Acc) :-
    Child = lookup_key(Relation, ChildKey),
    ProcessEdge(Parent, Child, !Acc),
    relation.traverse_children(Children, Parent, Relation, ProcessEdge, !Acc).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%   Function forms added.

relation.init = R :-
    relation.init(R).

relation.lookup_element(R, X) = K :-
    relation.lookup_element(R, X, K).

relation.lookup_key(R, K) = X :-
    relation.lookup_key(R, K, X).

relation.add(R1, K1, K2) = R2 :-
    relation.add(R1, K1, K2, R2).

relation.add_values(R1, X, Y) = R2 :-
    relation.add_values(R1, X, Y, R2).

relation.add_assoc_list(R1, AL) = R2 :-
    relation.add_assoc_list(R1, AL, R2).

relation.remove(R1, K1, K2) = R2 :-
    relation.remove(R1, K1, K2, R2).

relation.remove_assoc_list(R1, AL) = R2 :-
    relation.remove_assoc_list(R1, AL, R2).

relation.lookup_from(R, K) = S :-
    relation.lookup_from(R, K, S).

relation.lookup_to(R, K) = S :-
    relation.lookup_to(R, K, S).

relation.to_assoc_list(R) = AL :-
    relation.to_assoc_list(R, AL).

relation.to_key_assoc_list(R) = AL :-
    relation.to_key_assoc_list(R, AL).

relation.from_assoc_list(AL) = R :-
    relation.from_assoc_list(AL, R).

relation.domain(R) = S :-
    relation.domain(R, S).

relation.inverse(R1) = R2 :-
    relation.inverse(R1, R2).

relation.compose(R1, R2) = R3 :-
    relation.compose(R1, R2, R3).

relation.dfs(R, K) = Ks :-
    relation.dfs(R, K, Ks).

relation.dfsrev(R, K) = Ks :-
    relation.dfsrev(R, K, Ks).

relation.dfs(R) = Ks :-
    relation.dfs(R, Ks).

relation.dfsrev(R) = Ks :-
    relation.dfsrev(R, Ks).

relation.components(R) = KSS :-
    relation.components(R, KSS).

relation.cliques(R) = KSS :-
    relation.cliques(R, KSS).

relation.reduced(R1) = R2 :-
    relation.reduced(R1, R2).

relation.atsort(R) = Ss :-
    relation.atsort(R, Ss).

relation.sc(R1) = R2 :-
    relation.sc(R1, R2).

relation.tc(R1) = R2 :-
    relation.tc(R1, R2).

relation.rtc(R1) = R2 :-
    relation.rtc(R1, R2).

%------------------------------------------------------------------------------%
:- end_module relation.
%------------------------------------------------------------------------------%
