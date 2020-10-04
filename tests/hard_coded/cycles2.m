%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test: the compiler of May 4, 1998 generated
% incorrect code for the disjunction within the first branch of the
% switch on StartNode in cycle/2, resulting in an infinite loop.
%
% The problem is that in some cases if a variable is cached on entry
% to a disjunction, it is evaluated in between the mark_hp at the
% start of the disjunction and the restore_hp at the start of the first
% disjunct. It should be either be flushed before the mark_hp, or in each
% branch after the restore_hp.

:- module cycles2.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module solutions.

%---------------------------------------------------------------------------%

main(!IO) :-
    cycles(a, Cycles),
    write_cycles(Cycles, !IO).

%---------------------------------------------------------------------------%

:- type node ---> a ; b ; c ; d ; e ; f ; g ; h ; i ; j ; k ; l.

:- type status
    --->    traverse
    ;       cycle
    ;       no_cycle.

:- pred cycle(node::in, list(node)::out) is nondet.

cycle(StartNode, NodeLs) :-
    ( if
        NodeLs0 = [StartNode],
        (
            StartNode = a,
            %**************************************************
            % Incorrect code is generated for this disjunction.
            %**************************************************
            (
                AdjNode = b
            ;
                AdjNode = d
            )
        ;
            StartNode = b,
            AdjNode = c
        ;
            StartNode = c,
            AdjNode = c
        ;
            StartNode = d,
            AdjNode = d
        )
    then
       ( if not list__member(AdjNode, NodeLs0) then
          Status1 = traverse
       else
          Status1 = cycle
       ),
       NodeLs1 = [AdjNode | NodeLs0],
       cycle1(StartNode, AdjNode, NodeLs1, NodeLs, Status1)
    else
       NodeLs2 = [StartNode],
       cycle1(StartNode, StartNode, NodeLs2, NodeLs, no_cycle)
    ).

%---------------------------------------------------------------------------%

:- pred arrow(node, node).
:- mode arrow(in, in) is semidet.
:- mode arrow(in, out) is nondet.
:- mode arrow(out, out) is multi.

:- pragma inline(arrow/2).

arrow(a, b).
arrow(b, c).
arrow(c, c).
arrow(a, d).
arrow(d, a).

%---------------------------------------------------------------------------%

:- pred cycles(node::in, list(list(node))::out) is det.

cycles(N, Nodes) :-
    solutions((pred(C::out) is nondet :- cycle(N, C)), Nodes).

%---------------------------------------------------------------------------%

:- pred cycle1(node, node, list(node), list(node), status).
:- mode cycle1(in, in, in,  out, in) is nondet.

cycle1(StartNode, CurrNode, NodeLs0, NodeLs, traverse) :-
    ( if arrow(CurrNode, AdjNode) then
       ( if not list__member(AdjNode, NodeLs0) then
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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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
        Ns = []
    ;
        Ns = [_ | _],
        io.write_string(", ", !IO)
    ),
    write_cycles1(Ns, no, !IO).

%---------------------------------------------------------------------------%

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
        Ns = []
    ;
        Ns = [_ | _],
        io.write_string(", ", !IO)
    ),
    write_nodes1(Ns, no, !IO).

%---------------------------------------------------------------------------%

:- pred write_node(node::in, io::di, io::uo) is det.

write_node(N, !IO) :-
    node_to_char(N, C),
    io.write_char(C, !IO).

%---------------------------------------------------------------------------%
