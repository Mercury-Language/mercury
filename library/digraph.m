%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1999,2002-2007,2010-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: digraph.m
% Main author: bromage, petdr
% Stability: medium
%
% This module defines a data type representing directed graphs. A directed
% graph of type digraph(T) is logically equivalent to a set of vertices of
% type T, and a set of edges of type pair(T). The endpoints of each edge
% must be included in the set of vertices; cycles and loops are allowed.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module digraph.
:- interface.

:- import_module assoc_list.
:- import_module enum.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module sparse_bitset.

%---------------------------------------------------------------------------%

    % The type of directed graphs with vertices in T.
    %
:- type digraph(T).

    % The abstract type that indexes vertices in a digraph.
    % Each key is valid only with the digraph it was created from, and
    % the predicates and functions in this module may throw an exception
    % if their caller passes them an invalid key.
    %
:- type digraph_key(T).

:- instance enum(digraph_key(T)).

:- type digraph_key_set(T) == sparse_bitset(digraph_key(T)).

    % init creates an empty digraph.
    %
:- func init = digraph(T).
:- pred init(digraph(T)::out) is det.

    % add_vertex adds a vertex to the domain of a digraph.
    % Returns the old key if one already exists for this vertex,
    % otherwise it allocates a new key.
    %
:- pred add_vertex(T::in, digraph_key(T)::out,
    digraph(T)::in, digraph(T)::out) is det.

    % search_key returns the key associated with a vertex.
    % Fails if the vertex is not in the graph.
    %
:- pred search_key(digraph(T)::in, T::in, digraph_key(T)::out) is semidet.

    % lookup_key returns the key associated with a vertex.
    % Throws an exception if the vertex is not in the graph.
    %
:- func lookup_key(digraph(T), T) = digraph_key(T).
:- pred lookup_key(digraph(T)::in, T::in, digraph_key(T)::out) is det.

    % lookup_vertex returns the vertex associated with a key.
    %
:- func lookup_vertex(digraph(T), digraph_key(T)) = T.
:- pred lookup_vertex(digraph(T)::in, digraph_key(T)::in, T::out) is det.

    % add_edge adds an edge to the digraph if it doesn't already exist,
    % and leaves the digraph unchanged otherwise.
    %
:- func add_edge(digraph_key(T), digraph_key(T), digraph(T)) = digraph(T).
:- pred add_edge(digraph_key(T)::in, digraph_key(T)::in,
    digraph(T)::in, digraph(T)::out) is det.

    % add_vertices_and_edge adds a pair of vertices and an edge
    % between them to the digraph.
    %
    % add_vertices_and_edge(X, Y, !G) :-
    %    add_vertex(X, XKey, !G),
    %    add_vertex(Y, YKey, !G),
    %    add_edge(XKey, YKey, !G).
    %
:- func add_vertices_and_edge(T, T, digraph(T)) = digraph(T).
:- pred add_vertices_and_edge(T::in, T::in,
    digraph(T)::in, digraph(T)::out) is det.

    % As above, but takes a pair of vertices in a single argument.
    %
:- func add_vertex_pair(pair(T), digraph(T)) = digraph(T).
:- pred add_vertex_pair(pair(T)::in, digraph(T)::in, digraph(T)::out) is det.

    % add_assoc_list adds a list of edges to a digraph.
    %
:- func add_assoc_list(assoc_list(digraph_key(T), digraph_key(T)),
    digraph(T)) = digraph(T).
:- pred add_assoc_list(assoc_list(digraph_key(T), digraph_key(T))::in,
    digraph(T)::in, digraph(T)::out) is det.

    % delete_edge deletes an edge from the digraph if it exists,
    % and leaves the digraph unchanged otherwise.
    %
:- func delete_edge(digraph_key(T), digraph_key(T), digraph(T)) = digraph(T).
:- pred delete_edge(digraph_key(T)::in, digraph_key(T)::in,
    digraph(T)::in, digraph(T)::out) is det.

    % delete_assoc_list deletes a list of edges from a digraph.
    %
:- func delete_assoc_list(assoc_list(digraph_key(T), digraph_key(T)),
    digraph(T)) = digraph(T).
:- pred delete_assoc_list(
    assoc_list(digraph_key(T), digraph_key(T))::in,
    digraph(T)::in, digraph(T)::out) is det.

    % is_edge checks to see if an edge is in the digraph.
    %
:- pred is_edge(digraph(T), digraph_key(T), digraph_key(T)).
:- mode is_edge(in, in, out) is nondet.
:- mode is_edge(in, in, in) is semidet.

    % is_edge_rev is equivalent to is_edge, except that
    % the nondet mode works in the reverse direction.
    %
:- pred is_edge_rev(digraph(T), digraph_key(T), digraph_key(T)).
:- mode is_edge_rev(in, out, in) is nondet.
:- mode is_edge_rev(in, in, in) is semidet.

    % Given key x, lookup_from returns the set of keys y such that
    % there is an edge (x,y) in the digraph.
    %
:- func lookup_from(digraph(T), digraph_key(T)) = set(digraph_key(T)).
:- pred lookup_from(digraph(T)::in, digraph_key(T)::in,
    set(digraph_key(T))::out) is det.

    % As above, but returns a digraph_key_set.
    %
:- func lookup_key_set_from(digraph(T), digraph_key(T)) = digraph_key_set(T).
:- pred lookup_key_set_from(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::out) is det.

    % Given a key y, lookup_to returns the set of keys x such that
    % there is an edge (x,y) in the digraph.
    %
:- func lookup_to(digraph(T), digraph_key(T)) = set(digraph_key(T)).
:- pred lookup_to(digraph(T)::in, digraph_key(T)::in,
    set(digraph_key(T))::out) is det.

    % As above, but returns a digraph_key_set.
    %
:- func lookup_key_set_to(digraph(T), digraph_key(T)) = digraph_key_set(T).
:- pred lookup_key_set_to(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::out) is det.

%---------------------------------------------------------------------------%

    % to_assoc_list turns a digraph into a list of pairs of vertices,
    % one for each edge.
    %
:- func to_assoc_list(digraph(T)) = assoc_list(T, T).
:- pred to_assoc_list(digraph(T)::in, assoc_list(T, T)::out) is det.

    % to_key_assoc_list turns a digraph into a list of pairs of keys,
    % one for each edge.
    %
:- func to_key_assoc_list(digraph(T)) =
    assoc_list(digraph_key(T), digraph_key(T)).
:- pred to_key_assoc_list(digraph(T)::in,
    assoc_list(digraph_key(T), digraph_key(T))::out) is det.

    % from_assoc_list turns a list of pairs of vertices into a digraph.
    %
:- func from_assoc_list(assoc_list(T, T)) = digraph(T).
:- pred from_assoc_list(assoc_list(T, T)::in, digraph(T)::out) is det.

%---------------------------------------------------------------------------%

    % dfs(G, Key, Dfs) is true if Dfs is a depth-first sorting of G
    % starting at Key. The set of keys in the list Dfs is equal to the
    % set of keys reachable from Key.
    %
:- func dfs(digraph(T), digraph_key(T)) = list(digraph_key(T)).
:- pred dfs(digraph(T)::in, digraph_key(T)::in,
    list(digraph_key(T))::out) is det.

    % dfsrev(G, Key, DfsRev) is true if DfsRev is a reverse
    % depth-first sorting of G starting at Key. The set of keys in the
    % list DfsRev is equal to the set of keys reachable from Key.
    %
:- func dfsrev(digraph(T), digraph_key(T)) = list(digraph_key(T)).
:- pred dfsrev(digraph(T)::in, digraph_key(T)::in,
    list(digraph_key(T))::out) is det.

    % dfs(G, Dfs) is true if Dfs is a depth-first sorting of G.
    % If one considers each edge to point from a parent node to a child node,
    % then Dfs will be a list of all the keys in G such that all keys for
    % the children of a vertex are placed in the list before the parent key.
    %
    % If the digraph is cyclic, the position in which cycles are broken
    % (that is, in which a child is placed *after* its parent) is undefined.
    %
:- func dfs(digraph(T)) = list(digraph_key(T)).
:- pred dfs(digraph(T)::in, list(digraph_key(T))::out) is det.

    % dfsrev(G, DfsRev) is true if DfsRev is a reverse depth-first
    % sorting of G. That is, DfsRev is the reverse of Dfs from dfs/2.
    %
:- func dfsrev(digraph(T)) = list(digraph_key(T)).
:- pred dfsrev(digraph(T)::in, list(digraph_key(T))::out) is det.

    % dfs(G, Key, !Visit, Dfs) is true if Dfs is a depth-first
    % sorting of G starting at Key, assuming we have already visited !.Visit
    % vertices. That is, Dfs is a list of vertices such that all the
    % unvisited children of a vertex are placed in the list before the
    % parent. !.Visit allows us to initialise a set of previously visited
    % vertices. !:Visit is Dfs + !.Visit.
    %
:- pred dfs(digraph(T)::in, digraph_key(T)::in, digraph_key_set(T)::in,
    digraph_key_set(T)::out, list(digraph_key(T))::out) is det.

    % dfsrev(G, Key, !Visit, DfsRev) is true if DfsRev is a
    % reverse depth-first sorting of G starting at Key providing we have
    % already visited !.Visit nodes, i.e. the reverse of Dfs from dfs/5.
    % !:Visit is !.Visit + DfsRev.
    %
:- pred dfsrev(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out,
    list(digraph_key(T))::out) is det.

%---------------------------------------------------------------------------%

    % vertices returns the set of vertices in a digraph.
    %
:- func vertices(digraph(T)) = set(T).
:- pred vertices(digraph(T)::in, set(T)::out) is det.

    % inverse(G, G') is true iff the domains of G and G' are equal,
    % and for all x, y in this domain, (x,y) is an edge in G iff (y,x) is
    % an edge in G'.
    %
:- func inverse(digraph(T)) = digraph(T).
:- pred inverse(digraph(T)::in, digraph(T)::out) is det.

    % compose(G1, G2, G) is true if G is the composition
    % of the digraphs G1 and G2. This means that there is an edge (x,y) in G
    % iff there exists vertex m such that (x,m) is in G1 and (m,y) is in G2.
    %
:- func compose(digraph(T), digraph(T)) = digraph(T).
:- pred compose(digraph(T)::in, digraph(T)::in, digraph(T)::out)
    is det.

    % is_dag(G) is true iff G is a directed acyclic graph.
    %
:- pred is_dag(digraph(T)::in) is semidet.

    % components(G, Comp) is true if Comp is the set of the
    % connected components of G.
    %
:- func components(digraph(T)) = set(set(digraph_key(T))).
:- pred components(digraph(T)::in, set(set(digraph_key(T)))::out)
    is det.

    % cliques(G, Cliques) is true if Cliques is the set of the
    % cliques (strongly connected components) of G.
    %
:- func cliques(digraph(T)) = set(set(digraph_key(T))).
:- pred cliques(digraph(T)::in, set(set(digraph_key(T)))::out) is det.

    % reduced(G, R) is true if R is the reduced digraph (digraph of cliques)
    % obtained from G.
    %
:- func reduced(digraph(T)) = digraph(set(T)).
:- pred reduced(digraph(T)::in, digraph(set(T))::out) is det.

    % As above, but also return a map from each key in the original digraph
    % to the key for its clique in the reduced digraph.
    %
:- pred reduced(digraph(T)::in, digraph(set(T))::out,
    map(digraph_key(T), digraph_key(set(T)))::out) is det.

    % tsort(G, TS) is true if TS is a topological sorting of G.
    %
    % If we view each edge in the digraph as representing a <from, to>
    % relationship, then TS will contain a vertex "from" *before*
    % all the other vertices "to" for which a <from, to> edge exists
    % in the graph. In other words, TS will be in from-to order.
    %
    % tsort fails if G is cyclic.
    %
:- pred tsort(digraph(T)::in, list(T)::out) is semidet.

    % Both these predicates do a topological sort of G.
    %
    % return_vertices_in_from_to_order(G, TS) is a synonym for tsort(G, TS).
    % return_vertices_in_to_from_order(G, TS) is identical to both
    % except for the fact that it returns the vertices in the opposite order.
    %
:- pred return_vertices_in_from_to_order(digraph(T)::in, list(T)::out)
    is semidet.
:- pred return_vertices_in_to_from_order(digraph(T)::in, list(T)::out)
    is semidet.

    % atsort(G, ATS) is true if ATS is a topological sorting
    % of the strongly connected components (SCCs) in G.
    %
    % If we view each edge in the digraph as representing a <from, to>
    % relationship, then ATS will contain SCC A before all SCCs B
    % for which there is an edge <from, to> with "from" being in SCC A
    % and "to" being in SCC B. In other words, ATS will be in from-to order.
    %
:- func atsort(digraph(T)) = list(set(T)).
:- pred atsort(digraph(T)::in, list(set(T))::out) is det.

    % Both these predicates do a topological sort of the strongly connected
    % components (SCCs) of G.
    %
    % return_sccs_in_from_to_order(G) = ATS is a synonym for atsort(G) = ATS.
    % return_sccs_in_to_from_order(G) = ATS is identical to both
    % except for the fact that it returns the SCCs in the opposite order.
    %
:- func return_sccs_in_from_to_order(digraph(T)) = list(set(T)).
:- func return_sccs_in_to_from_order(digraph(T)) = list(set(T)).

    % sc(G, SC) is true if SC is the symmetric closure of G.
    % That is, (x,y) is in SC iff either (x,y) or (y,x) is in G.
    %
:- func sc(digraph(T)) = digraph(T).
:- pred sc(digraph(T)::in, digraph(T)::out) is det.

    % tc(G, TC) is true if TC is the transitive closure of G.
    %
:- func tc(digraph(T)) = digraph(T).
:- pred tc(digraph(T)::in, digraph(T)::out) is det.

    % rtc(G, RTC) is true if RTC is the reflexive transitive closure of G.
    %
:- func rtc(digraph(T)) = digraph(T).
:- pred rtc(digraph(T)::in, digraph(T)::out) is det.

    % traverse(G, ProcessVertex, ProcessEdge, !Acc) will traverse the digraph G
    % - calling ProcessVertex for each vertex in the digraph, and
    % - calling ProcessEdge for each edge in the digraph.
    % The processing of each vertex is followed by the processing of
    % all the edges originating at that vertex, until all vertices
    % have been processed.
    %
:- pred traverse(digraph(T), pred(T, A, A), pred(T, T, A, A), A, A).
:- mode traverse(in, pred(in, di, uo) is det,
    pred(in, in, di, uo) is det, di, uo) is det.
:- mode traverse(in, pred(in, in, out) is det,
    pred(in, in, in, out) is det, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- type digraph_key(T)
    --->    digraph_key(int).

:- instance enum(digraph_key(T)) where [
    to_int(digraph_key(Int)) = Int,
    from_int(Int) = digraph_key(Int)
].

:- type digraph(T)
    --->    digraph(
                % Next unallocated key number.
                next_key            :: int,

                % Maps vertices to their keys.
                vertex_map          :: bimap(T, digraph_key(T)),

                % Maps each vertex to its direct successors.
                fwd_map             :: key_set_map(T),

                % Maps each vertex to its direct predecessors.
                bwd_map             :: key_set_map(T)
            ).

%---------------------------------------------------------------------------%

    % Note that the integer keys in these maps are actually digraph keys.
    % We use the raw integers as keys to allow type specialization.
    %
:- type key_map(T)     == map(int, digraph_key(T)).
:- type key_set_map(T) == map(int, digraph_key_set(T)).

:- pred key_set_map_add(int::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

key_set_map_add(XI, Y, Map0, Map) :-
    ( if map.search(Map0, XI, SuccXs0) then
        ( if sparse_bitset.contains(SuccXs0, Y) then
            Map = Map0
        else
            sparse_bitset.insert(Y, SuccXs0, SuccXs),
            map.det_update(XI, SuccXs, Map0, Map)
        )
    else
        SuccXs = sparse_bitset.make_singleton_set(Y),
        map.det_insert(XI, SuccXs, Map0, Map)
    ).

:- pred key_set_map_delete(int::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

key_set_map_delete(XI, Y, Map0, Map) :-
    ( if map.search(Map0, XI, SuccXs0) then
        sparse_bitset.delete(Y, SuccXs0, SuccXs),
        map.det_update(XI, SuccXs, Map0, Map)
    else
        Map = Map0
    ).

%---------------------------------------------------------------------------%

init = G :-
    digraph.init(G).

init(digraph(0, VMap, FwdMap, BwdMap)) :-
    bimap.init(VMap),
    map.init(FwdMap),
    map.init(BwdMap).

%---------------------------------------------------------------------------%

add_vertex(Vertex, Key, !G) :-
    VertexMap0 = !.G ^ vertex_map,
    ( if bimap.search(VertexMap0, Vertex, Key0) then
        Key = Key0
    else
        allocate_key(Key, !G),
        bimap.set(Vertex, Key, VertexMap0, VertexMap),
        !G ^ vertex_map := VertexMap
    ).

:- pred allocate_key(digraph_key(T)::out, digraph(T)::in, digraph(T)::out)
    is det.

allocate_key(digraph_key(I), !G) :-
    I = !.G ^ next_key,
    !G ^ next_key := I + 1.

%---------------------------------------------------------------------------%

search_key(G, Vertex, Key) :-
    bimap.search(G ^ vertex_map, Vertex, Key).

lookup_key(G, Vertex) = Key :-
    digraph.lookup_key(G, Vertex, Key).

lookup_key(G, Vertex, Key) :-
    ( if digraph.search_key(G, Vertex, Key0) then
        Key = Key0
    else
        unexpected($module, $pred, "search for key failed")
    ).

lookup_vertex(G, Key) = Vertex :-
    digraph.lookup_vertex(G, Key, Vertex).

lookup_vertex(G, Key, Vertex) :-
    ( if bimap.search(G ^ vertex_map, Vertex0, Key) then
        Vertex = Vertex0
    else
        unexpected($module, $pred, "search for vertex failed")
    ).

%---------------------------------------------------------------------------%

add_edge(X, Y, !.G) = !:G :-
    digraph.add_edge(X, Y, !G).

add_edge(X, Y, !G) :-
    X = digraph_key(XI),
    Y = digraph_key(YI),
    FwdMap0 = !.G ^ fwd_map,
    BwdMap0 = !.G ^ bwd_map,
    key_set_map_add(XI, Y, FwdMap0, FwdMap),
    key_set_map_add(YI, X, BwdMap0, BwdMap),
    !G ^ fwd_map := FwdMap,
    !G ^ bwd_map := BwdMap.

add_vertices_and_edge(VX, VY, !.G) = !:G :-
    digraph.add_vertices_and_edge(VX, VY, !G).

add_vertices_and_edge(VX, VY, !G) :-
    digraph.add_vertex(VX, X, !G),
    digraph.add_vertex(VY, Y, !G),
    digraph.add_edge(X, Y, !G).

add_vertex_pair(Edge, !.G) = !:G :-
    digraph.add_vertex_pair(Edge, !G).

add_vertex_pair(VX - VY, !G) :-
    digraph.add_vertices_and_edge(VX, VY, !G).

add_assoc_list(Edges, !.G) = !:G :-
    digraph.add_assoc_list(Edges, !G).

add_assoc_list([], !G).
add_assoc_list([X - Y | Edges], !G) :-
    digraph.add_edge(X, Y, !G),
    digraph.add_assoc_list(Edges, !G).

%---------------------------------------------------------------------------%

delete_edge(X, Y, !.G) = !:G :-
    digraph.delete_edge(X, Y, !G).

delete_edge(X, Y, !G) :-
    X = digraph_key(XI),
    Y = digraph_key(YI),
    FwdMap0 = !.G ^ fwd_map,
    BwdMap0 = !.G ^ bwd_map,
    key_set_map_delete(XI, Y, FwdMap0, FwdMap),
    key_set_map_delete(YI, X, BwdMap0, BwdMap),
    !G ^ fwd_map := FwdMap,
    !G ^ bwd_map := BwdMap.

delete_assoc_list(Edges, !.G) = !:G :-
    digraph.delete_assoc_list(Edges, !G).

delete_assoc_list([], !G).
delete_assoc_list([X - Y | Edges], !G) :-
    digraph.delete_edge(X, Y, !G),
    digraph.delete_assoc_list(Edges, !G).

%---------------------------------------------------------------------------%

is_edge(G, digraph_key(XI), Y) :-
    map.search(G ^ fwd_map, XI, YSet),
    sparse_bitset.member(Y, YSet).

is_edge_rev(G, X, digraph_key(YI)) :-
    map.search(G ^ bwd_map, YI, XSet),
    sparse_bitset.member(X, XSet).

%---------------------------------------------------------------------------%

lookup_from(G, X) = Ys :-
    digraph.lookup_from(G, X, Ys).

lookup_from(G, X, to_set(Ys)) :-
    digraph.lookup_key_set_from(G, X, Ys).

lookup_key_set_from(G, X) = Ys :-
    digraph.lookup_key_set_from(G, X, Ys).

lookup_key_set_from(G, digraph_key(XI), Ys) :-
    ( if map.search(G ^ fwd_map, XI, Ys0) then
        Ys = Ys0
    else
        sparse_bitset.init(Ys)
    ).

lookup_to(G, Y) = Xs :-
    digraph.lookup_to(G, Y, Xs).

lookup_to(G, Y, to_set(Xs)) :-
    digraph.lookup_key_set_to(G, Y, Xs).

lookup_key_set_to(G, Y) = Xs :-
    digraph.lookup_key_set_to(G, Y, Xs).

lookup_key_set_to(G, digraph_key(YI), Xs) :-
    ( if map.search(G ^ bwd_map, YI, Xs0) then
        Xs = Xs0
    else
        sparse_bitset.init(Xs)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

to_assoc_list(G) = List :-
    digraph.to_assoc_list(G, List).

to_assoc_list(G, List) :-
    Fwd = G ^ fwd_map,
    map.keys(Fwd, FwdKeys),
    digraph.to_assoc_list_2(Fwd, FwdKeys, G ^ vertex_map, [], List).

:- pred digraph.to_assoc_list_2(key_set_map(T)::in, list(int)::in,
    bimap(T, digraph_key(T))::in, assoc_list(T, T)::in, assoc_list(T, T)::out)
    is det.

to_assoc_list_2(_Fwd, [], _, !AL).
to_assoc_list_2(Fwd, [XI | XIs], VMap, !AL) :-
    digraph.to_assoc_list_2(Fwd, XIs, VMap, !AL),
    bimap.reverse_lookup(VMap, VX, digraph_key(XI)),
    map.lookup(Fwd, XI, SuccXs),
    sparse_bitset.foldr(accumulate_rev_lookup(VMap, VX), SuccXs, !AL).

:- pred accumulate_rev_lookup(bimap(T, digraph_key(T))::in, T::in,
    digraph_key(T)::in, assoc_list(T, T)::in, assoc_list(T, T)::out) is det.

accumulate_rev_lookup(VMap, VX, Y, !AL) :-
    bimap.reverse_lookup(VMap, VY, Y),
    !:AL = [VX - VY | !.AL].

to_key_assoc_list(G) = List :-
    digraph.to_key_assoc_list(G, List).

to_key_assoc_list(G, List) :-
    Fwd = G ^ fwd_map,
    map.keys(Fwd, FwdKeys),
    digraph.to_key_assoc_list_2(Fwd, FwdKeys, [], List).

:- pred digraph.to_key_assoc_list_2(key_set_map(T)::in, list(int)::in,
    assoc_list(digraph_key(T), digraph_key(T))::in,
    assoc_list(digraph_key(T), digraph_key(T))::out) is det.

to_key_assoc_list_2(_Fwd, [], !AL).
to_key_assoc_list_2(Fwd, [XI | XIs], !AL) :-
    digraph.to_key_assoc_list_2(Fwd, XIs, !AL),
    map.lookup(Fwd, XI, SuccXs),
    sparse_bitset.foldr(accumulate_with_key(digraph_key(XI)), SuccXs, !AL).

:- pred accumulate_with_key(digraph_key(T)::in, digraph_key(T)::in,
    assoc_list(digraph_key(T), digraph_key(T))::in,
    assoc_list(digraph_key(T), digraph_key(T))::out) is det.

accumulate_with_key(X, Y, !AL) :-
    !:AL = [X - Y | !.AL].

from_assoc_list(AL) = G :-
    digraph.from_assoc_list(AL, G).

from_assoc_list(AL, G) :-
    list.foldl(add_vertex_pair, AL, digraph.init, G).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

dfs(G, X) = Dfs :-
    digraph.dfs(G, X, Dfs).

dfs(G, X, Dfs) :-
    digraph.dfsrev(G, X, DfsRev),
    list.reverse(DfsRev, Dfs).

dfsrev(G, X) = DfsRev :-
    digraph.dfsrev(G, X, DfsRev).

dfsrev(G, X, DfsRev) :-
    init(Vis0),
    digraph.dfs_2(G, X, Vis0, _, [], DfsRev).

dfs(G) = Dfs :-
    digraph.dfs(G, Dfs).

dfs(G, Dfs) :-
    digraph.dfsrev(G, DfsRev),
    list.reverse(DfsRev, Dfs).

dfsrev(G) = DfsRev :-
    digraph.dfsrev(G, DfsRev).

dfsrev(G, DfsRev) :-
    digraph.keys(G, Keys),
    list.foldl2(digraph.dfs_2(G), Keys, init, _, [], DfsRev).

dfs(G, X, !Visited, Dfs) :-
    digraph.dfs_2(G, X, !Visited, [], DfsRev),
    list.reverse(DfsRev, Dfs).

dfsrev(G, X, !Visited, DfsRev) :-
    digraph.dfs_2(G, X, !Visited, [], DfsRev).

:- pred digraph.dfs_2(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out,
    list(digraph_key(T))::in, list(digraph_key(T))::out) is det.

dfs_2(G, X, !Visited, !DfsRev) :-
    ( if sparse_bitset.contains(!.Visited, X) then
        true
    else
        digraph.lookup_key_set_from(G, X, SuccXs),
        sparse_bitset.insert(X, !Visited),

        % Go and visit all of the node's children first.
        sparse_bitset.foldl2(digraph.dfs_2(G), SuccXs, !Visited, !DfsRev),
        !:DfsRev = [X | !.DfsRev]
    ).

%---------------------------------------------------------------------------%

vertices(G) = Vs :-
    digraph.vertices(G, Vs).

vertices(G, Vs) :-
    bimap.ordinates(G ^ vertex_map, VsList),
    set.sorted_list_to_set(VsList, Vs).

:- pred digraph.keys(digraph(T)::in, list(digraph_key(T))::out) is det.

keys(G, Keys) :-
    bimap.coordinates(G ^ vertex_map, Keys).

%---------------------------------------------------------------------------%

inverse(G) = InvG :-
    digraph.inverse(G, InvG).

inverse(G, InvG) :-
    G = digraph(Next, VMap, Fwd, Bwd),
    InvG = digraph(Next, VMap, Bwd, Fwd).

%---------------------------------------------------------------------------%

compose(G1, G2) = Comp :-
    digraph.compose(G1, G2, Comp).

compose(G1, G2, !:Comp) :-
    !:Comp = digraph.init,

    % Find the set of vertices which occur in both G1 and G2.
    digraph.vertices(G1, G1Vs),
    digraph.vertices(G2, G2Vs),
    Matches = set.intersect(G1Vs, G2Vs),

    % Find the sets of keys to be matched in each digraph.
    AL = list.map(
        ( func(Match) = Xs - Ys :-
            digraph.lookup_key(G1, Match, M1),
            digraph.lookup_key_set_to(G1, M1, Xs),
            digraph.lookup_key(G2, Match, M2),
            digraph.lookup_key_set_from(G2, M2, Ys)
        ),
        set.to_sorted_list(Matches)),

    % Find the sets of keys in each digraph which will occur in
    % the new digraph.
    list.foldl2(find_necessary_keys, AL, sparse_bitset.init, Needed1,
        sparse_bitset.init, Needed2),

    % Add the elements to the composition.
    sparse_bitset.foldl2(copy_vertex(G1), Needed1, !Comp, map.init, KMap1),
    sparse_bitset.foldl2(copy_vertex(G2), Needed2, !Comp, map.init, KMap2),

    % Add the edges to the composition.
    list.foldl(add_composition_edges(KMap1, KMap2), AL, !Comp).

:- pred find_necessary_keys(pair(digraph_key_set(T))::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

find_necessary_keys(Xs - Ys, !Needed1, !Needed2) :-
    sparse_bitset.union(Xs, !Needed1),
    sparse_bitset.union(Ys, !Needed2).

:- pred copy_vertex(digraph(T)::in, digraph_key(T)::in,
    digraph(T)::in, digraph(T)::out, key_map(T)::in, key_map(T)::out) is det.

copy_vertex(G, X, !Comp, !KMap) :-
    digraph.lookup_vertex(G, X, VX),
    digraph.add_vertex(VX, CompX, !Comp),
    X = digraph_key(XI),
    map.det_insert(XI, CompX, !KMap).

:- pred add_composition_edges(key_map(T)::in, key_map(T)::in,
    pair(digraph_key_set(T))::in, digraph(T)::in, digraph(T)::out) is det.

add_composition_edges(KMap1, KMap2, Xs - Ys, !Comp) :-
    digraph.add_cartesian_product(map_digraph_key_set(KMap1, Xs),
        map_digraph_key_set(KMap2, Ys), !Comp).

:- func map_digraph_key_set(key_map(T), digraph_key_set(T)) =
    digraph_key_set(T).

map_digraph_key_set(KMap, Set0) = Set :-
    sparse_bitset.foldl(accumulate_digraph_key_set(KMap), Set0,
        sparse_bitset.init, Set).

:- pred accumulate_digraph_key_set(key_map(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

accumulate_digraph_key_set(KMap, X, !Set) :-
    X = digraph_key(XI),
    map.lookup(KMap, XI, Y),
    sparse_bitset.insert(Y, !Set).

%---------------------------------------------------------------------------%

is_dag(G) :-
    % Traverses the digraph depth-first, keeping track of all ancestors.
    % Fails if we encounter an ancestor during the traversal, otherwise
    % succeeds.
    %
    % not is_dag(G) <=> we encounter an ancestor at some stage:
    %
    % (=>) By assumption there exists a cycle. Since all vertices are reached
    % in the traversal, we reach all vertices in the cycle at some stage.
    % Let x be the vertex in the cycle that is reached first, and let y be
    % the vertex preceding x in the cycle. Since x was first, y has not
    % been visited and must therefore be reached at some stage in the depth-
    % first traversal beneath x. At this stage we encounter x as both a
    % child and an ancestor.
    %
    % (<=) If we encounter an ancestor in any traversal, then we have a cycle.
    %
    digraph.keys(G, Keys),
    list.foldl(digraph.is_dag_2(G, []), Keys, sparse_bitset.init, _).

:- pred digraph.is_dag_2(digraph(T)::in, list(digraph_key(T))::in,
    digraph_key(T)::in, digraph_key_set(T)::in, digraph_key_set(T)::out)
    is semidet.

is_dag_2(G, Ancestors, X, !Visited) :-
    ( if list.member(X, Ancestors) then
        fail
    else if sparse_bitset.contains(!.Visited, X) then
        true
    else
        digraph.lookup_key_set_from(G, X, SuccXs),
        sparse_bitset.insert(X, !Visited),
        foldl(digraph.is_dag_2(G, [X | Ancestors]), SuccXs, !Visited)
    ).

%---------------------------------------------------------------------------%

components(G) = Components :-
    digraph.components(G, Components).

components(G, Components) :-
    digraph.keys(G, Keys),
    sparse_bitset.list_to_set(Keys, KeySet : digraph_key_set(T)),
    digraph.components_loop(G, KeySet, set.init, Components).

:- pred digraph.components_loop(digraph(T)::in, digraph_key_set(T)::in,
    set(set(digraph_key(T)))::in, set(set(digraph_key(T)))::out) is det.

components_loop(G, Xs0, !Components) :-
    ( if sparse_bitset.remove_least(X, Xs0, Xs1) then
        sparse_bitset.init(Comp0),
        Keys0 = make_singleton_set(X),
        digraph.reachable_from(G, Keys0, Comp0, Comp),
        set.insert(sparse_bitset.to_set(Comp), !Components),
        sparse_bitset.difference(Xs1, Comp, Xs2),
        digraph.components_loop(G, Xs2, !Components)
    else
        true
    ).

:- pred digraph.reachable_from(digraph(T)::in, digraph_key_set(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

reachable_from(G, Keys0, !Comp) :-
    % Invariant: Keys0 and !.Comp are disjoint.
    ( if sparse_bitset.remove_least(X, Keys0, Keys1) then
        sparse_bitset.insert(X, !Comp),
        digraph.lookup_key_set_from(G, X, FwdSet),
        digraph.lookup_key_set_to(G, X, BwdSet),
        sparse_bitset.union(FwdSet, BwdSet, NextSet0),
        sparse_bitset.difference(NextSet0, !.Comp, NextSet),
        sparse_bitset.union(Keys1, NextSet, Keys),
        digraph.reachable_from(G, Keys, !Comp)
    else
        true
    ).

%---------------------------------------------------------------------------%

cliques(G) = Cliques :-
    digraph.cliques(G, Cliques).

cliques(G, Cliques) :-
    % Take a digraph and return the set of strongly connected components.
    %
    % Works using the following algorithm:
    % 1. Reverse the digraph.
    % 2. Traverse G in reverse depth-first order. From the first vertex
    %    do a DFS on the reversed G; all vertices visited are a member
    %    of the clique.
    % 3. From the next non-visited vertex do a DFS on the reversed G,
    %    not including visited vertices. This is the next clique.
    % 4. Repeat step 3 until all vertices visited.

    digraph.dfsrev(G, DfsRev),
    digraph.inverse(G, GInv),
    set.init(Cliques0),
    sparse_bitset.init(Visit),
    digraph.cliques_2(DfsRev, GInv, Visit, Cliques0, Cliques).

:- pred digraph.cliques_2(list(digraph_key(T))::in, digraph(T)::in,
    digraph_key_set(T)::in, set(set(digraph_key(T)))::in,
    set(set(digraph_key(T)))::out) is det.

cliques_2([], _, _, !Cliques).
cliques_2([X | Xs0], GInv, !.Visited, !Cliques) :-
    % Do a DFS on GInv, starting from X, but not including visited vertices.
    digraph.dfs_2(GInv, X, !Visited, [], CliqueList),

    % Insert the cycle into the clique set.
    set.list_to_set(CliqueList, Clique),
    set.insert(Clique, !Cliques),

    % Delete all the visited vertices, so head of the list is the next
    % highest non-visited vertex.
    list.delete_elems(Xs0, CliqueList, Xs),
    digraph.cliques_2(Xs, GInv, !.Visited, !Cliques).

%---------------------------------------------------------------------------%

reduced(G) = R :-
    digraph.reduced(G, R).

reduced(G, R) :-
    digraph.reduced(G, R, _).

reduced(G, !:R, !:CliqMap) :-
    digraph.cliques(G, Cliques),
    set.to_sorted_list(Cliques, CliqList),
    digraph.init(!:R),
    map.init(!:CliqMap),
    digraph.make_clique_map(G, CliqList, !CliqMap, !R),
    digraph.to_key_assoc_list(G, AL),
    digraph.make_reduced_graph(!.CliqMap, AL, !R).

:- type clique_map(T) == map(digraph_key(T), digraph_key(set(T))).

    % Add a vertex to the reduced graph for each clique, and build a map
    % from each key in the clique to this new key.
    %
:- pred digraph.make_clique_map(digraph(T)::in,
    list(set(digraph_key(T)))::in, clique_map(T)::in, clique_map(T)::out,
    digraph(set(T))::in, digraph(set(T))::out) is det.

make_clique_map(_, [], !CliqMap, !R).
make_clique_map(G, [Clique | Cliques], !CliqMap, !R) :-
    Vertices = set.map(digraph.lookup_vertex(G), Clique),
    digraph.add_vertex(Vertices, CliqKey, !R),
    set.fold(digraph.make_clique_map_2(CliqKey), Clique, !CliqMap),
    digraph.make_clique_map(G, Cliques, !CliqMap, !R).

:- pred digraph.make_clique_map_2(digraph_key(set(T))::in, digraph_key(T)::in,
    clique_map(T)::in, clique_map(T)::out) is det.

make_clique_map_2(CliqKey, X, !CliqMap) :-
    map.set(X, CliqKey, !CliqMap).

:- pred digraph.make_reduced_graph(clique_map(T)::in,
    assoc_list(digraph_key(T), digraph_key(T))::in,
    digraph(set(T))::in, digraph(set(T))::out) is det.

make_reduced_graph(_, [], !R).
make_reduced_graph(CliqMap, [X - Y | Edges], !R) :-
    map.lookup(CliqMap, X, CliqX),
    map.lookup(CliqMap, Y, CliqY),
    ( if CliqX = CliqY then
        true
    else
        digraph.add_edge(CliqX, CliqY, !R)
    ),
    digraph.make_reduced_graph(CliqMap, Edges, !R).

%---------------------------------------------------------------------------%

tsort(G, FromToTsort) :-
    return_vertices_in_from_to_order(G, FromToTsort).

return_vertices_in_from_to_order(G, FromToTsort) :-
    digraph.dfsrev(G, Tsort0),
    digraph.check_tsort(G, init, Tsort0),
    FromToTsort = list.map(digraph.lookup_vertex(G), Tsort0).

return_vertices_in_to_from_order(G, ToFromTsort) :-
    return_vertices_in_from_to_order(G, FromToTsort),
    list.reverse(FromToTsort, ToFromTsort).

:- pred digraph.check_tsort(digraph(T)::in, digraph_key_set(T)::in,
    list(digraph_key(T))::in) is semidet.

check_tsort(_, _, []).
check_tsort(G, Vis0, [X | Xs]) :-
    sparse_bitset.insert(X, Vis0, Vis),
    digraph.lookup_key_set_from(G, X, SuccXs),
    sparse_bitset.intersect(Vis, SuccXs, BackPointers),
    sparse_bitset.is_empty(BackPointers),
    digraph.check_tsort(G, Vis, Xs).

%---------------------------------------------------------------------------%

atsort(G) = ATsort :-
    ATsort = digraph.return_sccs_in_from_to_order(G).

atsort(G, ATsort) :-
    ATsort = digraph.return_sccs_in_from_to_order(G).

digraph.return_sccs_in_from_to_order(G) = ATsort :-
    ATsort0 = digraph.return_sccs_in_to_from_order(G),
    list.reverse(ATsort0, ATsort).

digraph.return_sccs_in_to_from_order(G) = ATsort :-
    % The algorithm used is described in R.E. Tarjan, "Depth-first search
    % and linear graph algorithms", SIAM Journal on Computing, 1, 2 (1972).
    digraph.dfsrev(G, DfsRev),
    digraph.inverse(G, GInv),
    sparse_bitset.init(Vis),
    digraph.atsort_loop(DfsRev, GInv, Vis, [], ATsort).

:- pred digraph.atsort_loop(list(digraph_key(T))::in, digraph(T)::in,
    digraph_key_set(T)::in, list(set(T))::in, list(set(T))::out) is det.

atsort_loop([], _, _, !ATsort).
atsort_loop([X | Xs], GInv, !.Vis, !ATsort) :-
    ( if sparse_bitset.contains(!.Vis, X) then
        true
    else
        digraph.dfs_2(GInv, X, !Vis, [], CliqKeys),
        list.map(digraph.lookup_vertex(GInv), CliqKeys, CliqList),
        set.list_to_set(CliqList, Cliq),
        !:ATsort = [Cliq | !.ATsort]
    ),
    digraph.atsort_loop(Xs, GInv, !.Vis, !ATsort).

%---------------------------------------------------------------------------%

sc(G) = Sc :-
    digraph.sc(G, Sc).

sc(G, Sc) :-
    digraph.inverse(G, GInv),
    digraph.to_key_assoc_list(GInv, GInvList),
    digraph.add_assoc_list(GInvList, G, Sc).

%---------------------------------------------------------------------------%

tc(G) = Tc :-
    digraph.tc(G, Tc).

tc(G, Tc) :-
    % digraph.tc returns the transitive closure of a digraph.
    % We use this procedure:
    %
    % - Compute the reflexive transitive closure.
    % - Find the "fake reflexives", that is, the set of vertices x for which
    %   (x,x) is not an edge in G+. This is done by noting that G+ = G . G*
    %   (where '.' denotes composition). Therefore x is a fake reflexive
    %   iff there is no y such that (x,y) is an edge in G and (y,x) is an edge
    %   in G*.
    % - Remove those edges from the reflexive transitive closure
    %   computed above.
    digraph.rtc(G, Rtc),

    % Find the fake reflexives.
    digraph.keys(G, Keys),
    digraph.detect_fake_reflexives(G, Rtc, Keys, [], Fakes),

    % Remove them from the RTC, giving us the TC.
    digraph.delete_assoc_list(Fakes, Rtc, Tc).

:- pred digraph.detect_fake_reflexives(digraph(T)::in, digraph(T)::in,
    list(digraph_key(T))::in, assoc_list(digraph_key(T), digraph_key(T))::in,
    assoc_list(digraph_key(T), digraph_key(T))::out) is det.

detect_fake_reflexives(_, _, [], !Fakes).
detect_fake_reflexives(G, Rtc, [X | Xs], !Fakes) :-
    digraph.lookup_key_set_from(G, X, SuccXs),
    digraph.lookup_key_set_to(Rtc, X, PreXs),
    sparse_bitset.intersect(SuccXs, PreXs, Ys),
    ( if sparse_bitset.is_empty(Ys) then
        !:Fakes = [X - X | !.Fakes]
    else
        true
    ),
    digraph.detect_fake_reflexives(G, Rtc, Xs, !Fakes).

%---------------------------------------------------------------------------%

rtc(G) = Rtc :-
    digraph.rtc(G, Rtc).

rtc(G, !:Rtc) :-
    % digraph.rtc returns the reflexive transitive closure of a digraph.
    %
    % Note: This is not the most efficient algorithm (in the sense of minimal
    % number of arc insertions) possible. However it "reasonably" efficient
    % and, more importantly, is much easier to debug than some others.
    %
    % The algorithm is very simple, and is based on the observation that the
    % RTC of any element in a clique is the same as the RTC of any other
    % element in that clique. So we visit each clique in reverse topological
    % sorted order, compute the RTC for each element in the clique and then
    % add the appropriate edges.

    digraph.dfs(G, Dfs),
    sparse_bitset.init(Vis),

    % First start with all the vertices in G, but no edges.
    G = digraph(NextKey, VMap, _, _),
    map.init(FwdMap),
    map.init(BwdMap),
    !:Rtc = digraph(NextKey, VMap, FwdMap, BwdMap),

    digraph.rtc_2(Dfs, G, Vis, !Rtc).

:- pred digraph.rtc_2(list(digraph_key(T))::in, digraph(T)::in,
    digraph_key_set(T)::in, digraph(T)::in, digraph(T)::out) is det.

rtc_2([], _, _, !Rtc).
rtc_2([X | Xs], G, !.Vis, !Rtc) :-
    ( if sparse_bitset.contains(!.Vis, X) then
        true
    else
        digraph.dfs_2(G, X, !Vis, [], CliqList),
        sparse_bitset.list_to_set(CliqList, Cliq),
        sparse_bitset.foldl(find_followers(G), Cliq,
            Cliq, Followers0),
        sparse_bitset.foldl(find_followers(!.Rtc), Followers0,
            Cliq, Followers),
        digraph.add_cartesian_product(Cliq, Followers, !Rtc)
    ),
    digraph.rtc_2(Xs, G, !.Vis, !Rtc).

:- pred find_followers(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

find_followers(G, X, !Followers) :-
    digraph.lookup_key_set_from(G, X, SuccXs),
    sparse_bitset.union(SuccXs, !Followers).

:- pred digraph.add_cartesian_product(digraph_key_set(T)::in,
    digraph_key_set(T)::in, digraph(T)::in, digraph(T)::out) is det.

add_cartesian_product(KeySet1, KeySet2, !Rtc) :-
    sparse_bitset.foldl(
        ( pred(Key1::in, !.Rtc::in, !:Rtc::out) is det :-
            sparse_bitset.foldl(digraph.add_edge(Key1), KeySet2, !Rtc)
        ), KeySet1, !Rtc).

%---------------------------------------------------------------------------%

traverse(Graph, ProcessVertex, ProcessEdge, !Acc) :-
    VertexMap = Graph ^ vertex_map,
    bimap.foldl(traverse_vertex(Graph, ProcessVertex, ProcessEdge),
        VertexMap, !Acc).

:- pred traverse_vertex(digraph(T),
    pred(T, A, A), pred(T, T, A, A), T, digraph_key(T), A, A).
:- mode traverse_vertex(in, pred(in, di, uo) is det,
    pred(in, in, di, uo) is det, in, in, di, uo) is det.
:- mode traverse_vertex(in, pred(in, in, out) is det,
    pred(in, in, in, out) is det, in, in, in, out) is det.

traverse_vertex(Graph, ProcessVertex, ProcessEdge, Vertex, VertexKey, !Acc) :-
    ProcessVertex(Vertex, !Acc),
    digraph.lookup_key_set_from(Graph, VertexKey, ChildrenKeys),
    sparse_bitset.foldl(traverse_child(Graph, ProcessEdge, Vertex),
        ChildrenKeys, !Acc).

:- pred traverse_child(digraph(T), pred(T, T, A, A),
    T, digraph_key(T), A, A).
:- mode traverse_child(in, pred(in, in, di, uo) is det,
    in, in, di, uo) is det.
:- mode traverse_child(in, pred(in, in, in, out) is det,
    in, in, in, out) is det.

traverse_child(Graph, ProcessEdge, Parent, ChildKey, !Acc) :-
    Child = digraph.lookup_vertex(Graph, ChildKey),
    ProcessEdge(Parent, Child, !Acc).

%---------------------------------------------------------------------------%
:- end_module digraph.
%---------------------------------------------------------------------------%
