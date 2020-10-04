%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test: Mercury 0.6 generated bad code for this
% example.  (The generated code went into an infinite loop resulting
% in a nondet stack overflow.)

:- module cycles.

%---------------------------------------------------------------------------
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module solutions.

%--------------------------------------------------

:- type node
    --->    a
    ;       b
    ;       c
    ;       d
    ;       e
    ;       f
    ;       g
    ;       h
    ;       i
    ;       j
    ;       k
    ;       l.

:- type status
    --->    traverse
    ;       cycle
    ;       no_cycle.

%--------------------------------------------------

main(!IO) :-
    cycles(a, Cycles),
    write_cycles(Cycles, !IO).

%--------------------------------------------------

:- pred arrow(node, node).
:- mode arrow(in, in) is semidet.
:- mode arrow(in, out) is nondet.
:- mode arrow(out, out) is multi.

arrow(a, b).
arrow(b, c).
arrow(c, c).
arrow(a, d).
arrow(d, a).

%--------------------------------------------------

:- pred cycles(node::in, list(list(node))::out) is det.

cycles(N, Nodes) :-
    solutions((pred(C::out) is nondet :- cycle(N, C)), Nodes).

%--------------------------------------------------

:- pred cycle(node::in, list(node)::out) is nondet.

cycle(StartNode, NodeLs) :-
    cycle1(StartNode, StartNode, [StartNode], NodeLs, traverse).

:- pred cycle1(node::in, node::in, list(node)::in, list(node)::out, status::in)
    is nondet.

cycle1(StartNode, CurrNode, NodeLs0, NodeLs, traverse) :-
    ( if arrow(CurrNode, AdjNode) then
        ( if not list.member(AdjNode, NodeLs0) then
            Status1 = traverse
        else
            Status1 = cycle
        ),
        NodeLs1 = [AdjNode | NodeLs0],
        cycle1(StartNode, AdjNode, NodeLs1, NodeLs, Status1)
    else
        cycle1(StartNode, StartNode, NodeLs0, NodeLs, no_cycle)
    ).
cycle1(StartNode, StartNode, NodeLs, NodeLs, cycle).

%--------------------------------------------------

:- pred node_to_char(node::in, char::out) is det.

node_to_char(a, 'a').
node_to_char(b, 'b').
node_to_char(c, 'c').
node_to_char(d, 'd').
node_to_char(e, 'e').
node_to_char(f, 'f').
node_to_char(g, 'g').
node_to_char(h, 'h').
node_to_char(i, 'i').
node_to_char(j, 'j').
node_to_char(k, 'k').
node_to_char(l, 'l').

%--------------------------------------------------

:- pred write_node(node::in, io::di, io::uo) is det.

write_node(N, !IO) :-
    node_to_char(N, C),
    io.write_char(C, !IO).

%--------------------------------------------------

:- pred write_nodes(list(node)::in, io::di, io::uo) is det.

write_nodes(Nodes, !IO) :-
    write_nodes1(Nodes, yes, !IO).

:- pred write_nodes1(list(node)::in, bool::in, io::di, io::uo) is det.

write_nodes1([], yes, !IO) :-
    io.write_string("[]", !IO).
write_nodes1([], no, !IO) :-
    io.write_string("]", !IO).
write_nodes1([N | Ns], Start, !IO) :-
    (
        Start = yes,
        io.write_string("[", !IO)
    ;
        Start = no
    ),
    write_node(N, !IO),
    (
        Ns = [_ | _],
        io.write_string(", ", !IO)
    ;
        Ns = []
    ),
    write_nodes1(Ns, no, !IO).

%--------------------------------------------------

:- pred write_cycles(list(list(node))::in, io::di, io::uo) is det.

write_cycles(Nodes, !IO) :-
    write_cycles1(Nodes, yes, !IO),
    io.write_string("\n", !IO).

:- pred write_cycles1(list(list(node))::in, bool::in, io::di, io::uo) is det.

write_cycles1([], yes, !IO) :-
    io.write_string("[]", !IO).
write_cycles1([], no, !IO) :-
    io.write_string("]", !IO).
write_cycles1([N | Ns], Start, !IO) :-
    (
        Start = yes,
        io.write_string("[", !IO)
    ;
        Start = no
    ),
    write_nodes(N, !IO),
    (
        Ns = [_ | _],
        io.write_string(", ", !IO)
    ;
        Ns = []
    ),
    write_cycles1(Ns, no, !IO).
