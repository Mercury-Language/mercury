%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% This code was taken from library/graph.m and simplified.
%
% Description of bug:
%   The liveness of polymorphic type variables for accurate gc
%   wasn't being computed correctly.
%   It was being removed from the initial_liveness for not being
%   part of the non-locals (like an unused variable).
%
% Symptom(s) of bug:
%   % Allocating storage locations for live vars in predicate
%       `agc_graph:insert_node/4' mode 0
%   Software error: variable TypeInfo_for_A (18) not found
%
% Date bug existed: 1-July-1997
% Author: trd

:- module agc_graph.

:- interface.
:- import_module unit.

:- type graph(N, A).
:- type node(N).
:- type arc(A).

:- type graph(N) == graph(N, unit).
:- type arc == arc(unit).

:- pred insert_node(graph(N, A)::in, N::in, node(N)::out, graph(N, A)::out)
    is semidet.

%---------------------------------------------------------------------------%

:- type node(N) ==  int.
:- type arc(A)  ==  int.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.

:- type graph(N, A)
    --->    graph(
                node_supply,
                arc_supply,
                map(node(N), N),
                map(arc(A), arc_info(N, A)),
                map(node(N), map(arc(A), node(N)))
            ).

:- type node_supply ==  int.
:- type arc_supply  ==  int.

:- type arc_info(N, A)
    --->    arc_info(node(N), node(N), A).

%---------------------------------------------------------------------------%

insert_node(G, NInfo, N, G) :-
    G = graph(_, _, Nodes0, _, _),

    get_node_supply(G, N),
    get_edges(G, Edges0),

    not map.member(Nodes0, _, NInfo),
    map.init(Edges0).

%---------------------------------------------------------------------------%

:- pred get_node_supply(graph(N, A)::in, node_supply::out) is det.

get_node_supply(G, NS) :-
    G = graph(NS, _AS, _N, _A, _E).

:- pred get_edges(graph(N, A)::in, map(node(N), map(arc(A), node(N)))::out)
    is det.

get_edges(G, E) :-
    G = graph(_NS, _AS, _N, _A, E).

%---------------------------------------------------------------------------%
