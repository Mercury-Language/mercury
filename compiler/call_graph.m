%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module call_graph.
% Main author: conway.

% Computes the `call_graph', i.e. which variables are either live across a
% call or live at the start of a disjunction, allocates a stack slot for
% each of these variables, and stores this information in the call_info
% structure in the proc_info.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds, graph, io.

:- type	pred_proc_id	==	pair(pred_id, proc_id).

:- type call_graph	==	graph(pred_proc_id, unit).

:- pred call_graph__build_call_graph(module_info, call_graph).
:- mode call_graph__build_call_graph(in, out) is det.

:- pred call_graph__write_call_graph(call_graph, module_info,
						io__state, io__state).
:- mode call_graph__write_call_graph(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, prog_io, std_util.
:- import_module mode_util, int, term, require, string.
:- import_module varset, mercury_to_mercury.

%-----------------------------------------------------------------------------%

	% Traverse the module structure, calling `call_graph__add_arcs'
	% for each procedure body.

call_graph__build_call_graph(ModuleInfo, CallGraph) :-
	module_info_predids(ModuleInfo, PredIds),
	graph__init(CallGraph0),
	call_graph__build_pred_nodes(PredIds, ModuleInfo, CallGraph0, CallGraph1),
	call_graph__add_pred_arcs(PredIds, ModuleInfo, CallGraph1, CallGraph).

:- pred call_graph__build_pred_nodes(list(pred_id), module_info,
			call_graph, call_graph).
:- mode call_graph__build_pred_nodes(in, in, in, out) is det.

call_graph__build_pred_nodes([], _ModuleInfo, CallGraph, CallGraph).
call_graph__build_pred_nodes([PredId | PredIds], ModuleInfo,
					CallGraph0, CallGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	call_graph__build_proc_nodes(ProcIds, PredId, ModuleInfo,
			CallGraph0, CallGraph1),
	call_graph__build_pred_nodes(PredIds, ModuleInfo, CallGraph1, CallGraph).

:- pred call_graph__build_proc_nodes(list(proc_id), pred_id, module_info,
			call_graph, call_graph).
:- mode call_graph__build_proc_nodes(in, in, in, in, out) is det.

call_graph__build_proc_nodes([], _PredId, _ModuleInfo, CallGraph, CallGraph).
call_graph__build_proc_nodes([ProcId | ProcIds], PredId, ModuleInfo,
						CallGraph0, CallGraph) :-

	graph__insert_node(CallGraph0, PredId - ProcId, _Node, CallGraph1),

	call_graph__build_proc_nodes(ProcIds, PredId, ModuleInfo,
						CallGraph1, CallGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred call_graph__add_pred_arcs(list(pred_id), module_info,
			call_graph, call_graph).
:- mode call_graph__add_pred_arcs(in, in, in, out) is det.

call_graph__add_pred_arcs([], _ModuleInfo, CallGraph, CallGraph).
call_graph__add_pred_arcs([PredId | PredIds], ModuleInfo,
					CallGraph0, CallGraph) :-
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	(
		pred_info_is_imported(PredInfo)
	->
		CallGraph1 = CallGraph0
	;
		pred_info_procids(PredInfo, ProcIds),
		call_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo,
			CallGraph0, CallGraph1)
	),
	call_graph__add_pred_arcs(PredIds, ModuleInfo, CallGraph1, CallGraph).

:- pred call_graph__add_proc_arcs(list(proc_id), pred_id, module_info,
			call_graph, call_graph).
:- mode call_graph__add_proc_arcs(in, in, in, in, out) is det.

call_graph__add_proc_arcs([], _PredId, _ModuleInfo, CallGraph, CallGraph).
call_graph__add_proc_arcs([ProcId | ProcIds], PredId, ModuleInfo,
						CallGraph0, CallGraph) :-
	module_info_preds(ModuleInfo, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_goal(ProcInfo0, Goal),

	call_graph__add_arcs_in_goal(Goal, PredId - ProcId,
					CallGraph0, CallGraph1),

	call_graph__add_proc_arcs(ProcIds, PredId, ModuleInfo,
						CallGraph1, CallGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The call_info structure (map(var, lval)) is threaded through the traversal
% of the goal. The liveness information is computed from the liveness
% delta annotations.

:- pred call_graph__add_arcs_in_goal(hlds__goal, pred_proc_id,
						call_graph, call_graph).
:- mode call_graph__add_arcs_in_goal(in, in, in, out) is det.

call_graph__add_arcs_in_goal(Goal - _GoalInfo, PPId, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal_2(Goal, PPId, CallGraph0, CallGraph).

%-----------------------------------------------------------------------------%
	% Here we process each of the different sorts of goals.
	% `Liveness' is the set of live variables, i.e. vars which
	% have been referenced and will be referenced again.

:- pred call_graph__add_arcs_in_goal_2(hlds__goal_expr, pred_proc_id,
		call_graph, call_graph).
:- mode call_graph__add_arcs_in_goal_2(in, in, in, out) is det.

call_graph__add_arcs_in_goal_2(conj(Goals), Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_list(Goals, Caller, CallGraph0, CallGraph).

call_graph__add_arcs_in_goal_2(disj(Goals), Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_list(Goals, Caller, CallGraph0, CallGraph).

call_graph__add_arcs_in_goal_2(switch(_Var, _Det, Cases),
					Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_cases(Cases, Caller, CallGraph0, CallGraph).

call_graph__add_arcs_in_goal_2(if_then_else(_Vars, Cond, Then, Else),
			Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal(Cond, Caller, CallGraph0, CallGraph1),
	call_graph__add_arcs_in_goal(Then, Caller, CallGraph1, CallGraph2),
	call_graph__add_arcs_in_goal(Else, Caller, CallGraph2, CallGraph).

call_graph__add_arcs_in_goal_2(not(Goal), Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal(Goal, Caller, CallGraph0, CallGraph).

call_graph__add_arcs_in_goal_2(some(_Vars, Goal), Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal(Goal, Caller, CallGraph0, CallGraph).

call_graph__add_arcs_in_goal_2(
		call(PredId, ProcId, _ArgTerms, Builtin, _SymName, _Follow),
			Caller, CallGraph0, CallGraph) :-
	(
		is_builtin__is_inline(Builtin)
	->
		CallGraph = CallGraph0
	;
		graph__find_node(CallGraph0, Caller, PNode),
		graph__find_node(CallGraph0, PredId - ProcId, CNode),
		graph__insert_edge(CallGraph0, PNode, CNode, unit,
							_Arc, CallGraph)
	).

call_graph__add_arcs_in_goal_2(unify(_,_,_,_,_), _Caller, CallGraph, CallGraph).

%-----------------------------------------------------------------------------%

:- pred call_graph__add_arcs_in_list(list(hlds__goal), pred_proc_id,
			call_graph, call_graph).
:- mode call_graph__add_arcs_in_list(in, in, in, out) is det.

call_graph__add_arcs_in_list([], _Caller, CallGraph, CallGraph).
call_graph__add_arcs_in_list([Goal|Goals], Caller, CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal(Goal, Caller, CallGraph0, CallGraph1),
	call_graph__add_arcs_in_list(Goals, Caller, CallGraph1, CallGraph).

%-----------------------------------------------------------------------------%

:- pred call_graph__add_arcs_in_cases(list(case), pred_proc_id,
			call_graph, call_graph).
:- mode call_graph__add_arcs_in_cases(in, in, in, out) is det.

call_graph__add_arcs_in_cases([], _Caller, CallGraph, CallGraph).
call_graph__add_arcs_in_cases([case(_Cons, Goal)|Goals], Caller,
						CallGraph0, CallGraph) :-
	call_graph__add_arcs_in_goal(Goal, Caller, CallGraph0, CallGraph1),
	call_graph__add_arcs_in_cases(Goals, Caller, CallGraph1, CallGraph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

call_graph__write_call_graph(CallGraph, ModuleInfo) -->
	{ graph__nodes(CallGraph, NodeSet) },
	{ set__to_sorted_list(NodeSet, NodeList) },
	call_graph__write_call_graph_2(NodeList, CallGraph, ModuleInfo).

:- pred call_graph__write_call_graph_2(list(node(pred_proc_id)), call_graph,
				module_info, io__state, io__state).
:- mode call_graph__write_call_graph_2(in, in, in, di, uo) is det.

call_graph__write_call_graph_2([], _CallGraph, _ModuleInfo) --> [].
call_graph__write_call_graph_2([Node|Nodes], CallGraph, ModuleInfo) -->
	{ graph__successors(CallGraph, Node, SuccSet) },
	{ set__to_sorted_list(SuccSet, SuccList) },
	call_graph__write_call_graph_3(SuccList, Node, CallGraph, ModuleInfo),
	call_graph__write_call_graph_2(Nodes, CallGraph, ModuleInfo).

:- pred call_graph__write_call_graph_3(list(node(pred_proc_id)),
		node(pred_proc_id), call_graph,
				module_info, io__state, io__state).
:- mode call_graph__write_call_graph_3(in, in, in, in, di, uo) is det.

call_graph__write_call_graph_3([], _Node, _CallGraph, _ModuleInfo) -->
	[].
call_graph__write_call_graph_3([S|Ss], Node, CallGraph, ModuleInfo) -->
	{ graph__lookup_node(CallGraph, Node, PPPId) },
	{ graph__lookup_node(CallGraph, S, CPPId) },
	{ PPPId = PPredId - PProcId },
	{ CPPId = CPredId - CProcId },
	{ module_info_pred_proc_info(ModuleInfo, PPredId, PProcId,
						PPredInfo, PProcInfo) },
	{ module_info_pred_proc_info(ModuleInfo, CPredId, CProcId,
						CPredInfo, CProcInfo) },
	{ pred_info_name(PPredInfo, PName) },
	{ proc_info_declared_determinism(PProcInfo, PDet) },
	{ proc_info_argmodes(PProcInfo, PModes) },
	{ proc_info_context(PProcInfo, PContext) },

	{ pred_info_name(CPredInfo, CName) },
	{ proc_info_declared_determinism(CProcInfo, CDet) },
	{ proc_info_argmodes(CProcInfo, CModes) },
	{ proc_info_context(CProcInfo, CContext) },

	{ varset__init(ModeVarSet) },

	mercury_output_mode_subdecl(ModeVarSet, unqualified(PName),
						PModes, PDet, PContext),
	io__write_string(" -> "),
	mercury_output_mode_subdecl(ModeVarSet, unqualified(CName),
						CModes, CDet, CContext),
	io__write_string(".\n"),

	call_graph__write_call_graph_3(Ss, Node, CallGraph, ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
