%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1999,2002-2007,2010-2012 The University of Melbourne.
% Copyright (C) 2014-2018, 2022-2023 The Mercury team.
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

:- instance uenum(digraph_key(T)).

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

    % A synonym for sc/1.
    %
:- func symmetric_closure(digraph(T)) = digraph(T).

    % tc(G, TC) is true if TC is the transitive closure of G.
    %
:- func tc(digraph(T)) = digraph(T).
:- pred tc(digraph(T)::in, digraph(T)::out) is det.

    % A synonym for tc/1.
    %
:- func transitive_closure(digraph(T)) = digraph(T).

    % rtc(G, RTC) is true if RTC is the reflexive transitive closure of G.
    %
    % RTC is the reflexive closure of the transitive closure of G,
    % or, equivalently, the transitive closure of the reflexive closure of G.
    %
:- func rtc(digraph(T)) = digraph(T).
:- pred rtc(digraph(T)::in, digraph(T)::out) is det.

    % A synonym for rtc/1.
    %
:- func reflexive_transitive_closure(digraph(T)) = digraph(T).

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

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

    % Straightforward implementation of tc for debugging.
    %
:- pred slow_tc(digraph(T)::in, digraph(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bimap.
:- import_module require.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type digraph_key(T)
    --->    digraph_key(uint).

:- instance uenum(digraph_key(T)) where [
    to_uint(digraph_key(UInt)) = UInt,
    from_uint(UInt, digraph_key(UInt))
].

:- type digraph(T)
    --->    digraph(
                % Next unallocated key number.
                next_key            :: uint,

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
:- type key_map(T)     == map(uint, digraph_key(T)).
:- type key_set_map(T) == map(uint, digraph_key_set(T)).

:- pred key_set_map_add(uint::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

key_set_map_add(XI, Y, Map0, Map) :-
    ( if map.search(Map0, XI, SuccXs0) then
        ( if sparse_bitset.insert_new(Y, SuccXs0, SuccXs) then
            map.det_update(XI, SuccXs, Map0, Map)
        else
            Map = Map0
        )
    else
        SuccXs = sparse_bitset.make_singleton_set(Y),
        map.det_insert(XI, SuccXs, Map0, Map)
    ).

:- pred key_set_map_union(uint::in, digraph_key_set(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

key_set_map_union(XI, Ys, Map0, Map) :-
    ( if map.transform_value(sparse_bitset.union(Ys), XI, Map0, Map1) then
        Map = Map1
    else
        map.det_insert(XI, Ys, Map0, Map)
    ).

:- pred key_set_map_delete(uint::in, digraph_key(T)::in,
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

init(digraph(0u, VMap, FwdMap, BwdMap)) :-
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
    !G ^ next_key := I + 1u.

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

:- pred to_assoc_list_2(key_set_map(T)::in, list(uint)::in,
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

:- pred to_key_assoc_list_2(key_set_map(T)::in, list(uint)::in,
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

:- pred dfs_2(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out,
    list(digraph_key(T))::in, list(digraph_key(T))::out) is det.

dfs_2(G, X, !Visited, !DfsRev) :-
    ( if sparse_bitset.insert_new(X, !Visited) then
        digraph.lookup_key_set_from(G, X, SuccXs),

        % Go and visit all of the node's children.
        sparse_bitset.foldl2(digraph.dfs_2(G), SuccXs, !Visited, !DfsRev),
        !:DfsRev = [X | !.DfsRev]
    else
        % We have already visited X.
        true
    ).

%---------------------------------------------------------------------------%

vertices(G) = Vs :-
    digraph.vertices(G, Vs).

vertices(G, Vs) :-
    bimap.ordinates(G ^ vertex_map, VsList),
    set.sorted_list_to_set(VsList, Vs).

:- pred keys(digraph(T)::in, list(digraph_key(T))::out) is det.

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

:- pred add_cartesian_product(digraph_key_set(T)::in,
    digraph_key_set(T)::in, digraph(T)::in, digraph(T)::out) is det.

add_cartesian_product(KeySet1, KeySet2, !G) :-
    sparse_bitset.foldl(
        ( pred(Key1::in, G0::in, G::out) is det :-
            sparse_bitset.foldl(digraph.add_edge(Key1), KeySet2, G0, G)
        ), KeySet1, !G).

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

:- pred is_dag_2(digraph(T)::in, list(digraph_key(T))::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is semidet.

is_dag_2(G, Ancestors, X, !Visited) :-
    ( if list.member(X, Ancestors) then
        fail
    else if sparse_bitset.insert_new(X, !Visited) then
        digraph.lookup_key_set_from(G, X, SuccXs),
        foldl(digraph.is_dag_2(G, [X | Ancestors]), SuccXs, !Visited)
    else
        % We have already visited X.
        true
    ).

%---------------------------------------------------------------------------%

components(G) = Components :-
    digraph.components(G, Components).

components(G, Components) :-
    digraph.keys(G, Keys),
    sparse_bitset.list_to_set(Keys, KeySet : digraph_key_set(T)),
    digraph.components_loop(G, KeySet, set.init, Components).

:- pred components_loop(digraph(T)::in, digraph_key_set(T)::in,
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

:- pred reachable_from(digraph(T)::in, digraph_key_set(T)::in,
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

:- pred cliques_2(list(digraph_key(T))::in, digraph(T)::in,
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
:- pred make_clique_map(digraph(T)::in, list(set(digraph_key(T)))::in,
    clique_map(T)::in, clique_map(T)::out,
    digraph(set(T))::in, digraph(set(T))::out) is det.

make_clique_map(_, [], !CliqMap, !R).
make_clique_map(G, [Clique | Cliques], !CliqMap, !R) :-
    Vertices = set.map(digraph.lookup_vertex(G), Clique),
    digraph.add_vertex(Vertices, CliqKey, !R),
    set.fold(digraph.make_clique_map_2(CliqKey), Clique, !CliqMap),
    digraph.make_clique_map(G, Cliques, !CliqMap, !R).

:- pred make_clique_map_2(digraph_key(set(T))::in, digraph_key(T)::in,
    clique_map(T)::in, clique_map(T)::out) is det.

make_clique_map_2(CliqKey, X, !CliqMap) :-
    map.set(X, CliqKey, !CliqMap).

:- pred make_reduced_graph(clique_map(T)::in,
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

:- pred check_tsort(digraph(T)::in, digraph_key_set(T)::in,
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

return_sccs_in_from_to_order(G) = ATsort :-
    ATsort0 = digraph.return_sccs_in_to_from_order(G),
    list.reverse(ATsort0, ATsort).

return_sccs_in_to_from_order(G) = ATsort :-
    % The algorithm used is described in R.E. Tarjan, "Depth-first search
    % and linear graph algorithms", SIAM Journal on Computing, 1, 2 (1972).
    %
    % Strictly speaking, this is Kosaraju's algorithm. Tarjan's algorithm
    % improves upon it by performing one traversal of the input graph
    % instead of two.
    digraph.dfsrev(G, DfsRev),
    digraph.inverse(G, GInv),
    sparse_bitset.init(Vis),
    digraph.atsort_loop(DfsRev, GInv, Vis, [], ATsort).

:- pred atsort_loop(list(digraph_key(T))::in, digraph(T)::in,
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

sc(G) = symmetric_closure(G).

sc(G, Sc) :-
    Sc = symmetric_closure(G).

symmetric_closure(G) = Sc :-
    digraph.inverse(G, GInv),
    digraph.to_key_assoc_list(GInv, GInvList),
    digraph.add_assoc_list(GInvList, G, Sc).

%---------------------------------------------------------------------------%

tc(G) = transitive_closure(G).

tc(G, Tc) :-
    Tc = transitive_closure(G).

transitive_closure(G) = Tc :-
    basic_tc(G, Tc).

%---------------------------------------------------------------------------%

% This implements the Basic_TC (BTC) algorithm described by Yannis Ioannidis
% et al. in "Transitive Closure Algorithms Based on Graph Traversal"
% ACM Transactions on Database Systems, Vol. 18, No. 3, Sept. 1993, pp. 512-576
% <https://www.madgik.di.uoa.gr/publications/transitive-closure-algorithms-based-graph-traversal>
%
% It is also helpful to read Esko Nuutila's doctoral thesis
% "Efficient Transitive Closure Computation in Large Digraphs"
% <http://www.cs.hut.fi/~enu/thesis.html>
%
% Note: Nuutila's STACK_TC algorithm should be faster than Basic_TC in general,
% as it computes edges between components rather than vertices. The algorithm
% outputs Comp and Succ, such that to find the successors of a vertex v,
% you would look up Succ(Comp(v)). That representation saves a lot of time and
% memory since the successors of every vertex in a component are always the
% same.
%
% However, the advantage is eroded given that our digraph representation stores
% the successors and predecessors of each vertex individually. Then Basic_TC
% tends to be faster, likely due to its relative simplicity.

:- type modified_tarjan_visit(T)
    --->    modified_tarjan_visit(
                visit_counter   :: uint,
                visit_map       :: map(digraph_key(T), uint)
            ).

:- type modified_tarjan_state(T)
    --->    modified_tarjan_state(
                % A map from a vertex to the candidate root of the component
                % that will include the vertex.
                root_map    :: map(digraph_key(T), digraph_key(T)),

                % Stack of vertices being visited.
                stack       :: list(digraph_key(T)),

                % A vertex is included in popped once the component containing
                % the vertex has been determined, i.e. it has been popped off
                % the stack.
                popped      :: digraph_key_set(T),

                % The detected components in topological order
                % (parent before descendants).
                comps       :: list(component(T))
            ).

:- type component(T)
    --->    component(
                component_root      :: digraph_key(T),
                component_nonroots  :: list(digraph_key(T))
            ).

:- pred basic_tc(digraph(T)::in, digraph(T)::out) is det.

basic_tc(G, Tc) :-
    % First identify strong components.
    modified_tarjan(G, Comps),
    list.reverse(Comps, RevComps),

    % Loop over components in reverse topological order
    % (descendants before parent).
    G = digraph(NextKey, VMap, FwdMap0, _BwdMap0),
    list.foldl2(btc_process_component(FwdMap0), RevComps,
        map.init, SuccMap, map.init, PredMap),
    Tc = digraph(NextKey, VMap, SuccMap, PredMap).

%---------------------%

    % NOTE: modified_tarjan could be used elsewhere in this module.
    %
:- pred modified_tarjan(digraph(T)::in, list(component(T))::out) is det.

modified_tarjan(G, Comps) :-
    G = digraph(_NextKey, VMap, FwdMap, _BwdMap),
    Visit0 = modified_tarjan_visit(0u, map.init),
    State0 = modified_tarjan_state(map.init, [], sparse_bitset.init, []),
    bimap.foldl2(modified_tarjan_main_loop(FwdMap), VMap,
        Visit0, _Visit, State0, State),
    State = modified_tarjan_state(_RootMap, _Stack, _Popped, Comps).

:- pred modified_tarjan_main_loop(key_set_map(T)::in,
    T::in, digraph_key(T)::in,
    modified_tarjan_visit(T)::in, modified_tarjan_visit(T)::out,
    modified_tarjan_state(T)::in, modified_tarjan_state(T)::out) is det.

modified_tarjan_main_loop(OrigEdges, _V, KeyV, !Visit, !State) :-
    ( if modified_tarjan_new_visit(KeyV, !Visit) then
        modified_tarjan_visit(OrigEdges, KeyV, !Visit, !State)
    else
        true
    ).

:- pred modified_tarjan_new_visit(digraph_key(T)::in,
    modified_tarjan_visit(T)::in, modified_tarjan_visit(T)::out) is semidet.

modified_tarjan_new_visit(V, !Visit) :-
    Counter0 = !.Visit ^ visit_counter,
    Map0 = !.Visit ^ visit_map,
    map.insert(V, Counter0, Map0, Map),
    Counter = Counter0 + 1u,
    !Visit ^ visit_counter := Counter,
    !Visit ^ visit_map := Map.

:- pred modified_tarjan_visit(key_set_map(T)::in, digraph_key(T)::in,
    modified_tarjan_visit(T)::in, modified_tarjan_visit(T)::out,
    modified_tarjan_state(T)::in, modified_tarjan_state(T)::out) is det.

modified_tarjan_visit(OrigEdges, V, !Visit, !State) :-
    some [!RootMap, !Stack] (
        !:RootMap = !.State ^ root_map,
        !:Stack = !.State ^ stack,

        map.det_insert(V, V, !RootMap),
        !:Stack = [V | !.Stack],

        !State ^ root_map := !.RootMap,
        !State ^ stack := !.Stack
    ),

    get_successors(OrigEdges, V, SuccVs),
    sparse_bitset.foldl2(modified_tarjan_visit_v_w(OrigEdges, V),
        SuccVs, !Visit, !State),

    RootMap = !.State ^ root_map,
    ( if map.search(RootMap, V, V) then
        % V is the root of a component that also contains Ws.
        some [!Stack, !Popped, !Comps] (
            !:Stack = !.State ^ stack,
            !:Popped = !.State ^ popped,
            !:Comps = !.State ^ comps,

            pop_component(V, Ws, !Stack),
            sparse_bitset.insert(V, !Popped),
            sparse_bitset.insert_list(Ws, !Popped),
            !:Comps = [component(V, Ws) | !.Comps],

            !State ^ stack := !.Stack,
            !State ^ popped := !.Popped,
            !State ^ comps := !.Comps
        )
    else
        true
    ).

:- pred modified_tarjan_visit_v_w(key_set_map(T)::in,
    digraph_key(T)::in, digraph_key(T)::in,
    modified_tarjan_visit(T)::in, modified_tarjan_visit(T)::out,
    modified_tarjan_state(T)::in, modified_tarjan_state(T)::out) is det.

modified_tarjan_visit_v_w(OrigEdges, V, W, !Visit, !State) :-
    ( if modified_tarjan_new_visit(W, !Visit) then
        modified_tarjan_visit(OrigEdges, W, !Visit, !State)
    else
        true
    ),

    Popped = !.State ^ popped,
    ( if sparse_bitset.contains(Popped, W) then
        % We already determined the component that contains W.
        true
    else
        % Otherwise, update the candidate that will become the root of the
        % component that contains W.
        RootMap0 = !.State ^ root_map,
        map.lookup(RootMap0, V, RootV),
        map.lookup(RootMap0, W, RootW),
        ( if visited_earlier(!.Visit, RootV, RootW) then
            map.det_update(V, RootW, RootMap0, RootMap),
            !State ^ root_map := RootMap
        else
            true
        )
    ).

:- pred visited_earlier(modified_tarjan_visit(T)::in,
    digraph_key(T)::in, digraph_key(T)::in) is semidet.

visited_earlier(Visit, X, Y) :-
    VisitMap = Visit ^ visit_map,
    map.lookup(VisitMap, X, OrderX),
    map.lookup(VisitMap, Y, OrderY),
    OrderY < OrderX.

:- pred pop_component(digraph_key(T)::in, list(digraph_key(T))::out,
    list(digraph_key(T))::in, list(digraph_key(T))::out) is det.

pop_component(Root, NonRoots, !Stack) :-
    (
        !.Stack = [V | !:Stack],
        ( if V = Root then
            NonRoots = []
        else
            pop_component(Root, TailNonRoots, !Stack),
            NonRoots = [V | TailNonRoots]
        )
    ;
        !.Stack = [],
        unexpected($pred, "empty stack")
    ).

%---------------------%

:- pred btc_process_component(key_set_map(T)::in, component(T)::in,
    key_set_map(T)::in, key_set_map(T)::out,
    key_set_map(T)::in, key_set_map(T)::out) is det.

btc_process_component(OrigEdges, Comp, !SuccMap, !PredMap) :-
    % V is the root of a component that also contains Ws.
    Comp = component(V, Ws),

    % Build the set of successors for the root vertex V.
    get_successors(!.SuccMap, V, SuccV0),
    list.foldl(build_successor_set(OrigEdges, !.SuccMap), [V | Ws],
        SuccV0, SuccV),

    V = digraph_key(VI),
    map.det_insert(VI, SuccV, !SuccMap),

    % Distribute successors to other vertices in the component.
    list.foldl(add_successors(SuccV), Ws, !SuccMap),

    % Maintain the predecessor map from the (new) successors back to each
    % vertex in the component. This ends up dominating the time spent computing
    % the transitive closure, even though the user may not make use of the
    % predecessor map at all.
    (
        Ws = [],
        sparse_bitset.foldl(add_predecessor(V), SuccV, !PredMap)
    ;
        Ws = [_ | _],
        sparse_bitset.list_to_set([V | Ws], VWs),
        sparse_bitset.foldl(add_predecessors(VWs), SuccV, !PredMap)
    ).

:- pred build_successor_set(key_set_map(T)::in, key_set_map(T)::in,
    digraph_key(T)::in, digraph_key_set(T)::in, digraph_key_set(T)::out)
    is det.

build_successor_set(OrigEdges, SuccMap0, W, !SuccV) :-
    get_successors(OrigEdges, W, SuccW),
    sparse_bitset.difference(SuccW, !.SuccV, NewSuccessors),
    sparse_bitset.foldl(build_successor_set_2(SuccMap0), NewSuccessors,
        !SuccV).

:- pred build_successor_set_2(key_set_map(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

build_successor_set_2(SuccMap0, W, !SuccV) :-
    get_successors(SuccMap0, W, SuccW),
    sparse_bitset.insert(W, !SuccV),
    sparse_bitset.union(SuccW, !SuccV).

:- pred get_successors(key_set_map(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::out) is det.

get_successors(SuccMap, V, SuccV) :-
    V = digraph_key(VI),
    ( if map.search(SuccMap, VI, SuccV0) then
        SuccV = SuccV0
    else
        SuccV = sparse_bitset.init
    ).

:- pred add_successors(digraph_key_set(T)::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

add_successors(Ys, X, !Map) :-
    X = digraph_key(XI),
    key_set_map_union(XI, Ys, !Map).

:- pred add_predecessors(digraph_key_set(T)::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

add_predecessors(Ys, X, !Map) :-
    X = digraph_key(XI),
    key_set_map_union(XI, Ys, !Map).

:- pred add_predecessor(digraph_key(T)::in, digraph_key(T)::in,
    key_set_map(T)::in, key_set_map(T)::out) is det.

add_predecessor(Y, X, !Map) :-
    X = digraph_key(XI),
    key_set_map_add(XI, Y, !Map).

%---------------------------------------------------------------------------%

rtc(G) = reflexive_transitive_closure(G).

rtc(G, Rtc) :-
    Rtc = reflexive_transitive_closure(G).

reflexive_transitive_closure(G) =
    reflexive_closure(transitive_closure(G)).

:- func reflexive_closure(digraph(T)) = digraph(T).

reflexive_closure(G) = Rc :-
    digraph.keys(G, Keys),
    list.foldl(add_reflexive, Keys, G, Rc).

:- pred add_reflexive(digraph_key(T)::in,
    digraph(T)::in, digraph(T)::out) is det.

add_reflexive(X, !G) :-
    add_edge(X, X, !G).

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

slow_tc(G, TC) :-
    % First start with all the vertices in G, but no edges.
    G = digraph(NextKey, VMap, FwdMap, _BwdMap),
    TC0 = digraph(NextKey, VMap, map.init, map.init),

    map.keys(FwdMap, FwdKeys),
    list.foldl(add_edges_to_reachable(G), FwdKeys, TC0, TC).

:- pred add_edges_to_reachable(digraph(T)::in, uint::in,
    digraph(T)::in, digraph(T)::out) is det.

add_edges_to_reachable(G, XI, !TC) :-
    X = digraph_key(XI),
    find_descendants(G, X,
        sparse_bitset.init, _Visited,
        sparse_bitset.init, Reachable),
    sparse_bitset.foldl(add_edge(X), Reachable, !TC).

:- pred find_descendants(digraph(T)::in, digraph_key(T)::in,
    digraph_key_set(T)::in, digraph_key_set(T)::out,
    digraph_key_set(T)::in, digraph_key_set(T)::out) is det.

find_descendants(G, X, !Visited, !Reachable) :-
    ( if sparse_bitset.contains(!.Visited, X) then
        true
    else
        digraph.lookup_key_set_from(G, X, SuccXs),
        sparse_bitset.insert(X, !Visited),
        sparse_bitset.union(SuccXs, !Reachable),
        sparse_bitset.foldl2(find_descendants(G), SuccXs, !Visited, !Reachable)
    ).

%---------------------------------------------------------------------------%
:- end_module digraph.
%---------------------------------------------------------------------------%
