%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. The transformation HHF was leaving inaccurate
% nonlocals sets for some conjuncts in the insert_edge predicate, which
% lead to the propagation solver approach to constraints based mode analysis
% failing. The transformation still has the bug as this test is written,
% it's just no longer used when the propagation solver is used.

:- module mc_hhf_nonlocals_bug.

:- interface.

:- import_module unit.

:- type graph(N, A).
:- type node(N).
:- type arc(A).

:- type graph(N) == graph(N, unit).
:- type arc == arc(unit).

:- pred mc_hhf_nonlocals_bug.insert_edge(graph(N, A)::in, node(N)::in,
    node(N)::in, A::in, arc(A)::out, graph(N, A)::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.

:- type graph(N, A)
    --->    graph(
                arc_supply      :: counter,
                arc_map         :: map(arc(A), arc_info(N, A)),
                edge_map        :: map(node(N), map(arc(A), node(N)))
            ).

:- type node(N)
    --->    node(int).

:- type arc(A)
    --->    arc(int).

:- type arc_info(N, A)
    --->    arc_info(node(N), node(N), A).

mc_hhf_nonlocals_bug.insert_edge(!.G, Start, End, Info, Arc, !:G) :-
    AS0 = !.G ^ arc_supply,
    counter.allocate(A, AS0, AS),
    Arc = arc(A),
    !:G = !.G ^ arc_supply := AS,

    Arcs0 = !.G ^ arc_map,
    map.insert(Arc, arc_info(Start, End, Info), Arcs0, Arcs),
    !:G = !.G ^ arc_map := Arcs.
