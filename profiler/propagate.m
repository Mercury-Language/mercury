%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% propagate.m
%
% Main author: petdr.
%
% Propagates the counts around the call_graph.
% To do this it first identifies all the cycles in the call graph.
% Each cycle is treated as a new super-predicate.  ie All time propagated into
% a cycle is shared between all it's members.
% Then using a topological sorting of the call graph, the time is propagated
% from the leaves of all the call graph to the head.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module propagate.

:- interface.

:- import_module io, relation.
:- import_module prof_info.

:- pred propagate__counts(relation(string), prof, prof, io__state, io__state).
:- mode propagate__counts(in, in, out, di, uo) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list, float, int, list, map, multi_map, require.
:- import_module string, set_bbbtree, std_util.
% :- import_module writeln.

:- type cycle_info ==	pair(
				map(string, int),	% predicate - cycle
				multi_map(int, string)  % cycle - list preds
			).

propagate__counts(CallGraph, Prof0, Prof) -->
	{ prof_get_addrdeclmap(Prof0, AddrDeclMap) },
	{ prof_get_profnodemap(Prof0, ProfNodeMap0) },
	
	{ propagate__identify_cycles(CallGraph, ATSort, CycleInfo) },
	{ propagate__update_cycles(CycleInfo, AddrDeclMap, ProfNodeMap0, 
								ProfNodeMap1) },
	

	{ propagate__counts_2(ATSort, CycleInfo, AddrDeclMap, ProfNodeMap1, 
								ProfNodeMap) },

	{ CycleInfo = M - _MM },
	{ prof_set_cyclemap(M, Prof0, Prof1) },
	{ prof_set_profnodemap(ProfNodeMap, Prof1, Prof) }.


%-----------------------------------------------------------------------------%


% propagate__identify_cycles:
%	Identifies the cycles in the callgraph and places the members of each
% 	cycle into a map which associates a unique int with each cycle and a
%	multimap which associates with each cycle number a list of preds.  Also
%	Approximate topologically sorts the call graph.
%
:- pred propagate__identify_cycles(relation(string), list(string),
							cycle_info).
:- mode propagate__identify_cycles(in, out, out) is det.

propagate__identify_cycles(Rel, ATSort, CycleInfo) :-
	relation__dfsrev(Rel, DfsRev),
	relation__inverse(Rel, RelInv),
	cycle_info_init(CycleInfo0),
	set_bbbtree__init(Visit0),
	propagate__identify_cycles_2(DfsRev, 1, RelInv, Visit0, [], 
						CycleInfo0, ATSort, CycleInfo).


:- pred propagate__identify_cycles_2(list(relation_key), int, relation(string), 
			set_bbbtree(relation_key), list(string),
			cycle_info, list(string), cycle_info).
:- mode propagate__identify_cycles_2(in, in, in, in, in, in, out, out) is det.

propagate__identify_cycles_2([], _, _, _, ATSort, CycleInfo, ATSort, CycleInfo).
propagate__identify_cycles_2([X | Xs0], CycleNum0, RelInv, Visit0, ATSort0, 
					CycleInfo0, ATSort, CycleInfo) :- 

		% Do a DFS on R'.  The nodes we can get to and have not 
		% already visited before are one cycle in the call graph.
	relation__dfsrev(RelInv, X, Visit0, Visit, DfsRev0),
	list__map(relation__lookup_key(RelInv), DfsRev0, DfsRev),

	% writeln("*******************"),
	% writeln_list(DfsRev),
	% writeln("*******************"),

	(
		(
			DfsRev = [_]
		;
			DfsRev = [],	% This case should never happen
			error("propagate__identify_cycles_2: empty list\n")

		)
	->
		CycleNum = CycleNum0,
		CycleInfo1 = CycleInfo0
	;
		CycleNum is CycleNum0 + 1,
		propagate__add_to_cycle_map(CycleInfo0, DfsRev, CycleNum, 
								CycleInfo1)
	),

	list__append(DfsRev, ATSort0, ATSort1),

		% Delete all visited elements from Xs0 as they have already
		% been identified as part of a cycle.
	list__delete_elems(Xs0, DfsRev0, Xs),
	propagate__identify_cycles_2(Xs, CycleNum, RelInv, Visit, ATSort1, 
						CycleInfo1, ATSort, CycleInfo).


% cycle_info_init:
%	Initialise the cycle_info structure.
%
:- pred cycle_info_init(cycle_info).
:- mode cycle_info_init(out) is det.

cycle_info_init(M - MM) :-
	map__init(M),
	multi_map__init(MM).


% propagate__add_to_cycle_map:
%	Add all the predicates in a cycle into the cycle map
%
:- pred propagate__add_to_cycle_map(cycle_info, list(string), int,
		cycle_info).
:- mode propagate__add_to_cycle_map(in, in, in, out) is det.

propagate__add_to_cycle_map(CycleInfo, [], _, CycleInfo).
propagate__add_to_cycle_map(M0 - MM0, [X | Xs], V, M - MM) :-
	map__det_insert(M0, X, V, M1),
	multi_map__set(MM0, V, X, MM1),
	propagate__add_to_cycle_map(M1 - MM1, Xs, V, M - MM).


%-----------------------------------------------------------------------------%


:- pred propagate__update_cycles(cycle_info, addrdecl, prof_node_map, 
							prof_node_map).
:- mode propagate__update_cycles(in, in, in, out) is det.

propagate__update_cycles(_M - MM, AddrDecl, ProfNodeMap0, ProfNodeMap) :-
	multi_map__to_assoc_list(MM, AssocList),
	propagate__update_cycles_2(AssocList, AddrDecl, ProfNodeMap0, 
								ProfNodeMap).

:- pred propagate__update_cycles_2(assoc_list(int, list(string)), addrdecl,
						prof_node_map, prof_node_map).
:- mode propagate__update_cycles_2(in, in, in, out) is det.

propagate__update_cycles_2([], _, ProfNodeMap, ProfNodeMap).
propagate__update_cycles_2([ Num - Preds | Rest], AddrDecl, ProfNodeMap0, 
								ProfNodeMap) :-
	propagate__update_cycles_3(Preds, Num, AddrDecl, ProfNodeMap0, 
								ProfNodeMap1),
	propagate__update_cycles_2(Rest, AddrDecl, ProfNodeMap1, ProfNodeMap).

:- pred propagate__update_cycles_3(list(string), int, addrdecl, prof_node_map,
								prof_node_map).
:- mode propagate__update_cycles_3(in, in, in, in, out) is det.

propagate__update_cycles_3([], _, _, ProfNodeMap, ProfNodeMap).
propagate__update_cycles_3([P | Ps], CycleNum, AddrDecl, ProfNodeMap0, 
								ProfNodeMap) :-
	get_prof_node(P, AddrDecl, ProfNodeMap0, ProfNode0),
	prof_node_set_cycle_num(CycleNum, ProfNode0, ProfNode),
	update_prof_node(P, ProfNode, AddrDecl, ProfNodeMap0, ProfNodeMap1),
	propagate__update_cycles_3(Ps, CycleNum, AddrDecl, ProfNodeMap1,
								ProfNodeMap).
	

%-----------------------------------------------------------------------------%


% propagate__counts_2
%	XXX
%
:- pred propagate__counts_2(list(string), cycle_info, addrdecl, 
			prof_node_map, prof_node_map).
:- mode propagate__counts_2(in, in, in, in, out) is det.

propagate__counts_2([], _, _, ProfNodeMap, ProfNodeMap).
propagate__counts_2([Pred | Preds], M - MM, AddrDeclMap, ProfNodeMap0, 
								ProfNodeMap) :-
	(
		% writeln("********************************"),
			% Determine if predicate is in a cycle
		map__search(M, Pred, Cycle)
	->
		% writeln("Cycle:"),
		% writeln_list(CyclePreds),

		multi_map__lookup(MM, Cycle, CyclePreds),
		list__length(CyclePreds, Length),

		(
				% Throw away the rest of the predicates to
				% be processed by the profiler as we are about
				% to make them into one cycle.
			list__drop((Length - 1), Preds, NewPreds)
		->
			propagate__process_cycle(CyclePreds, Cycle, AddrDeclMap,
						    ProfNodeMap0, ProfNodeMap1),

			propagate__counts_2(NewPreds, M-MM, AddrDeclMap, 
						ProfNodeMap1, ProfNodeMap)
		;
			error("propagate__counts_2: list_drop failed\n")
		)
	;
		get_prof_node(Pred, AddrDeclMap, ProfNodeMap0, ProfNode),
		prof_node_get_initial_counts(ProfNode, InitCounts),
		prof_node_get_propagated_counts(ProfNode, PropCounts),
		prof_node_get_parent_list(ProfNode, ParentList),
		prof_node_get_total_calls(ProfNode, TotalCalls),
		int__to_float(InitCounts, InitCountsFloat),

		% writeln("Predicate:"),
		% writeln(Pred),
		% writeln("Initial counts:"),
		% writeln_int(InitCounts),
		% writeln("Propagated Counts:"),
		% writeln_float(PropCounts),

		TotalCounts is InitCountsFloat + PropCounts,

		int__to_float(TotalCalls, FltTotalCalls),

		propagate__counts_3(ParentList, TotalCounts, FltTotalCalls,
				AddrDeclMap, ProfNodeMap0, ProfNodeMap1),
		propagate__counts_2(Preds, M-MM, AddrDeclMap, ProfNodeMap1, 
								ProfNodeMap)
	).


% propagate__process_cycle:
%	Takes the list of cycle preds and treats them as one single unit called
%	<cycle X>.
%
:- pred propagate__process_cycle(list(string), int, addrdecl, prof_node_map,
								prof_node_map).
:- mode propagate__process_cycle(in, in, in, in, out) is det.

propagate__process_cycle(Preds, Cycle, AddrMap, ProfNodeMap0, ProfNodeMap) :-
		% Determine the parents of a cycle
	propagate__cycle_parents(Preds, AddrMap, ProfNodeMap0, Total, 
							Recursive, ParentList),
		% Build the cycle name
	string__int_to_string(Cycle, CycleStr),
	string__append("< cycle ", CycleStr, NameStr0),
	string__append(NameStr0, " as a whole >", NameStr),

		% Work out number of selfcounts
	propagate__sum_self_counts(Preds, AddrMap, ProfNodeMap0, SelfCounts),

		% Work out number of propagated counts
	propagate__sum_propagated_counts(Preds, AddrMap, ProfNodeMap0, 
								PropCounts),

	% writeln("Self Counts :"),
	% writeln_int(SelfCounts),
	% writeln("Propagated Counts :"),
	% writeln_float(PropCounts),

	propagate__build_cycle_list(Preds, AddrMap, ProfNodeMap0, CycleList),

	prof_node_init_cycle(NameStr, 0, SelfCounts, PropCounts, CycleList,
				Total, Recursive, ProfNode),
				
		% NB we give the address of a cycle as being the negative of
		% the cycle number as this will be unique.
	Address is -Cycle,
	map__det_insert(ProfNodeMap0, Address, ProfNode, ProfNodeMap1),
	
		
		% Propagate the counts XXX
	int__to_float(SelfCounts, FltSelfCounts),
	int__to_float(Total, TotalCalls),
	TotalCounts is FltSelfCounts + PropCounts,
	propagate__counts_3(ParentList, TotalCounts, TotalCalls, AddrMap,
						ProfNodeMap1, ProfNodeMap).


% propagate__sum_self_counts:
%	Sums the self counts fields for all the predicates.
%
:- pred propagate__sum_self_counts(list(string), addrdecl,
			prof_node_map, int).
:- mode propagate__sum_self_counts(in, in, in, out) is det.

propagate__sum_self_counts([], _, _, 0).
propagate__sum_self_counts([P | Ps], ADMap, PNMap, X) :-
	propagate__sum_self_counts(Ps, ADMap, PNMap, X0),

	get_prof_node(P, ADMap, PNMap, ProfNode),
	prof_node_get_initial_counts(ProfNode, InitCount),
	X is X0 + InitCount. 


% propagate__sum_propagated_counts:
%	Sums the propagated counts fields for all the predicates.
%
:- pred propagate__sum_propagated_counts(list(string), addrdecl, prof_node_map,
									float).
:- mode propagate__sum_propagated_counts(in, in, in, out) is det.

propagate__sum_propagated_counts([], _, _, 0.0).
propagate__sum_propagated_counts([P | Ps], ADMap, PNMap, X) :-
	propagate__sum_propagated_counts(Ps, ADMap, PNMap, X0),

	get_prof_node(P, ADMap, PNMap, ProfNode),
	prof_node_get_propagated_counts(ProfNode, PropCount),
	X is X0 + PropCount. 


% propagate__build_cycle_list
%	Takes the list of predicates and works out how many times each predicate
%	is called by a fellow predicate
%	XXX Not fully implemented yet.
%
:- pred propagate__build_cycle_list(list(string), addrdecl, prof_node_map,
							list(pred_info)).
:- mode propagate__build_cycle_list(in, in, in, out) is det.

propagate__build_cycle_list([], _, _, []).
propagate__build_cycle_list([P | Ps], ADM, PNM, CycleList) :-
	propagate__build_cycle_list(Ps, ADM, PNM, CycleList0),
	pred_info_init(P, 0, PredInfo),
	CycleList = [ PredInfo | CycleList0].


:- pred propagate__counts_3(list(pred_info), float, float, addrdecl, 
						prof_node_map, prof_node_map).
:- mode propagate__counts_3(in, in, in, in, in, out) is det.

propagate__counts_3([], _, _, _, ProfNodeMap, ProfNodeMap).
propagate__counts_3([ P | Ps], TotalCounts, TotalCalls, AddrMap, 
						ProfNodeMap0, ProfNodeMap) :-
	pred_info_get_entire(P, Pred, Calls),

		% Work out the number of counts to propagate.
		% XXX Probably need to do a 0.0 check
	int__to_float(Calls, FloatCalls),
	ToPropagateCounts is FloatCalls / TotalCalls * TotalCounts,

		% Add new counts to current propagated counts
	get_prof_node(Pred, AddrMap, ProfNodeMap0, ProfNode0),
	prof_node_get_propagated_counts(ProfNode0, PropCount0),
	PropCount is PropCount0 + ToPropagateCounts,
	prof_node_set_propagated_counts(PropCount, ProfNode0, ProfNode),
	update_prof_node(Pred, ProfNode, AddrMap, ProfNodeMap0, ProfNodeMap1),

	% writeln("Propagating to "),
	% writeln(Pred),
	% writeln_float(ToPropagateCounts),
	propagate__counts_3(Ps, TotalCounts, TotalCalls, AddrMap, ProfNodeMap1,
								ProfNodeMap).



% propagate__cycle_parents
%	Returns a list(pred_info) which is the list of parents of the cycle
%	Also returns how may times the cycle is called and how may times
%	predicates in a cycle call each other.
%
:- pred propagate__cycle_parents(list(string), addrdecl, prof_node_map, 
						int, int, list(pred_info)).
:- mode propagate__cycle_parents(in, in, in, out, out, out) is det.

propagate__cycle_parents(Preds, AddrMap, ProfNodeMap, 
					TotalCalls, SelfCalls, ParentList) :-
	propagate__build_parent_map(Preds, AddrMap, ProfNodeMap, TotalCalls, 
							SelfCalls, ParentMap),
	map__to_assoc_list(ParentMap, ParentAssocList),
	assoc_list_to_pred_info_list(ParentAssocList, ParentList).


% propagate__build_parent_map:
%	Builds a map which contains all the parents of a cycle, and the 
%	total number of times that parent is called.  Doesn't include the 
%	cycle members, and callers which never call any of the members of
%	the cycle.  At the same time also sums the total calls into the 
%	cycle and the calls internal to the cycle.
%
:- pred propagate__build_parent_map(list(string), addrdecl, prof_node_map, 
						int, int, map(string, int)).
:- mode propagate__build_parent_map(in, in, in, out, out, out) is det.

propagate__build_parent_map([], _AddrMap, _ProfNodeMap, _, _, _ParentMap) :-
	error("build_parent_map: empty cycle list\n").
propagate__build_parent_map([C | Cs], AddrMap, ProfNodeMap, 
					TotalCalls, SelfCalls, ParentMap) :-
	map__init(ParentMap0),
	build_parent_map_2([C | Cs], [C | Cs], AddrMap, ProfNodeMap, 0, 0,
				ParentMap0, TotalCalls, SelfCalls, ParentMap).


:- pred build_parent_map_2(list(string), list(string), addrdecl, prof_node_map, 
			int, int, map(string, int), int, int, map(string, int)).
:- mode build_parent_map_2(in, in, in, in, in, in, in, out, out, out) is det.

build_parent_map_2([], _, _, _, T, S, ParentMap, T, S, ParentMap).
build_parent_map_2([C | Cs], CliqueList, AddrMap, ProfNodeMap, 
			TotalCalls0, SelfCalls0, ParentMap0, TotalCalls, 
							SelfCalls, ParentMap) :-
	get_prof_node(C, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_parent_list(ProfNode, ParentList),
	add_to_parent_map(ParentList, CliqueList, 0, 0, ParentMap0, 
				TotalCalls1, SelfCalls1, ParentMap1),
	
	TotalCalls2 is TotalCalls0 + TotalCalls1,
	SelfCalls2 is SelfCalls0 + SelfCalls1,
	build_parent_map_2(Cs, CliqueList, AddrMap, ProfNodeMap, TotalCalls2,
					SelfCalls2, ParentMap1, TotalCalls, 
					SelfCalls, ParentMap).


% add_to_parent_map:
% 	Adds list of parents to parent map.  Ignores clique members and
%	repeats and callers which never call current predicate.
%	Also returns the total number of times predicate is called.
%
:- pred add_to_parent_map(list(pred_info), list(string), int, int, 
				map(string, int), int, int, map(string, int)).
:- mode add_to_parent_map(in, in, in, in, in, out, out, out) is det.

add_to_parent_map([], _CliqueList, T, S, ParentMap, T, S, ParentMap).
add_to_parent_map([P | Ps], CliqueList, TotalCalls0, SelfCalls0, ParentMap0, 
					TotalCalls, SelfCalls, ParentMap) :-
	pred_info_get_pred_name(P, PredName),
	pred_info_get_counts(P, Counts),
	(
		(
			list__member(PredName, CliqueList)
		;
			Counts = 0
		)
	->
		SelfCalls1 is SelfCalls0 + Counts, 
		add_to_parent_map(Ps, CliqueList, TotalCalls0, SelfCalls1, 
				ParentMap0, TotalCalls, SelfCalls, ParentMap)
	;	
		(
			map__search(ParentMap0, PredName, CurrCount0)
		->
			CurrCount is CurrCount0 + Counts,
			map__det_update(ParentMap0, PredName, CurrCount, 
								ParentMap1)
		;
			map__det_insert(ParentMap0, PredName, Counts, 
								ParentMap1)
		),
		TotalCalls1 is TotalCalls0 + Counts,
		add_to_parent_map(Ps, CliqueList, TotalCalls1, SelfCalls0,
				ParentMap1, TotalCalls, SelfCalls, ParentMap)
	).


:- pred assoc_list_to_pred_info_list(assoc_list(string, int), list(pred_info)).
:- mode assoc_list_to_pred_info_list(in, out) is det.

assoc_list_to_pred_info_list([], []).
assoc_list_to_pred_info_list([S - I | Xs], List) :-
	assoc_list_to_pred_info_list(Xs, List0),
	pred_info_init(S, I, PredInfo),
	List = [ PredInfo | List0 ].
