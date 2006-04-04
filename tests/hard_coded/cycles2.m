% This is a regression test: the compiler of May 4, 1998 generated
% incorrect code for the disjunction within the first branch of the
% switch on StartNode in cycle/2, resulting in an infinite loop.
%
% The problem is that in some cases if a variable is cached on entry
% to a disjunction, it is evaluated in between the mark_hp at the
% start of the disjunction and the restore_hp at the start of the first
% disjunct. It should be either be flushed before the mark_hp, or in each
% branch after the restore_hp.

% cycles2.m
%===========================================================================
:- module cycles2.

%---------------------------------------------------------------------------
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------
:- implementation.
:- import_module list, bool, solutions, char.

%--------------------------------------------------
:- type node ---> a ; b ; c ; d ; e ; f ; g ; h ; i ; j ; k ; l.

:- type status ---> traverse ; cycle ; no_cycle.

%--------------------------------------------------
:- pred cycle(node::in, list(node)::out) is nondet.

cycle(StartNode, NodeLs) :- 
	(
		NodeLs0 = [StartNode],
		(
			StartNode = a,
			%**************************************************
			%**************************************************
			% Incorrect code is generated for this disjunction.
			%**************************************************
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
	->
	   ((\+ list__member(AdjNode, NodeLs0)) ->
	      Status1 = traverse
	   ; 
	      Status1 = cycle
	   ),
	   NodeLs1 = [AdjNode | NodeLs0],
	   cycle1(StartNode, AdjNode, NodeLs1, NodeLs, Status1)
	;
	   NodeLs2 = [StartNode],
	   cycle1(StartNode, StartNode, NodeLs2, NodeLs, no_cycle)
	).

%--------------------------------------------------
main --> 
	{ cycles(a, Cycles) },
	write_cycles(Cycles).

%--------------------------------------------------
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

%--------------------------------------------------
:- pred cycles(node::in, list(list(node))::out) is det.   

cycles(N, Nodes) :- 
	solutions((pred(C::out) is nondet :- cycle(N, C)), Nodes).

%--------------------------------------------------

:- pred cycle1(node, node, list(node), list(node), status).
:- mode cycle1(in, in, in,  out, in) is nondet.

cycle1(StartNode, CurrNode, NodeLs0, NodeLs, traverse) :-
	(arrow(CurrNode, AdjNode) ->
	   ((\+ list__member(AdjNode, NodeLs0)) ->
	      Status1 = traverse
	   ; 
	      Status1 = cycle
	   ),
	   NodeLs1 = [AdjNode | NodeLs0],
	   cycle1(StartNode, AdjNode, NodeLs1, NodeLs, Status1)
	;
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
:- pred write_node(node::in, io__state::di, io__state::uo) is det.

write_node(N) --> { node_to_char(N, C) }, io__write_char(C).

%--------------------------------------------------
:- pred write_nodes(list(node)::in, io__state::di, io__state::uo) is det.

write_nodes(Nodes) --> write_nodes1(Nodes, yes).


:- pred write_nodes1(list(node), bool, io__state, io__state).
:- mode write_nodes1(in, in, di, uo) is det.

write_nodes1([], yes) --> io__write_string("[]").
write_nodes1([], no) --> io__write_string("]").
write_nodes1([N|Ns], Start) --> 
	( { Start = yes } -> io__write_string("[") ; { true }), 
	write_node(N),
	( { Ns \= [] } -> io__write_string(", ") ; { true }),
	write_nodes1(Ns, no).

%--------------------------------------------------
:- pred write_cycles(list(list(node)), io__state, io__state).
:- mode write_cycles(in, di, uo) is det.

write_cycles(Nodes) --> write_cycles1(Nodes, yes), io__write_string("\n").


:- pred write_cycles1(list(list(node)), bool, io__state, io__state).
:- mode write_cycles1(in, in, di, uo) is det.

write_cycles1([], yes) --> io__write_string("[]").
write_cycles1([], no) --> io__write_string("]").
write_cycles1([N|Ns], Start) --> 
	( { Start = yes } -> io__write_string("[") ; { true }), 
	write_nodes(N),
	( { Ns \= [] } -> io__write_string(", ") ; { true }),
	write_cycles1(Ns, no).

%===========================================================================
:- end_module cycles2.

