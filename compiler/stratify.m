%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% stratify.m - the stratification analysis pass.

% Main authors: ohutch, conway.

% This module performs stratification analysis. 
% It works by processing the call graph 1 scc at a time. It traverses 
% the goal for each procedure in the scc and reports an error or 
% warning (depending on the context) for any negated call to another member 
% of the scc. If it encounters a higher order call or a call to an
% outside module it will also emit a message.
% 
% 
% It has a second pass which is not currently enabled
%
% The second pass looks for possible non stratified code by looking at
% higher order calls. This second pass works by rebuilding the call 
% graph with any possible arcs that can arise though higher order calls
% and then traversing the new sccs looking for negative loops
%
% The second pass is necessary because the rebuilt call graph does not
% allow the detection of definite non-stratification. 
% 

%-----------------------------------------------------------------------------%

:- module check_hlds__stratify.

:- interface.

:- import_module hlds__hlds_module, io.

	% Perform stratification analysis, for the given module.
	% If the "warn-non-stratification" option is set this 
	% pred will check the entire module for stratification
	% otherwise it will only check preds in the stratified_preds 
	% set of the module_info structure. 
:- pred stratify__check_stratification(module_info, module_info,
		io__state, io__state).
:- mode stratify__check_stratification(in, out, di, uo) is det.

:- implementation.

:- import_module transform_hlds__dependency_graph, hlds__hlds_pred.
:- import_module hlds__hlds_goal, hlds__hlds_data.
:- import_module hlds__hlds_module, check_hlds__type_util.
:- import_module check_hlds__mode_util, parse_tree__prog_data.
:- import_module hlds__passes_aux.
:- import_module parse_tree__prog_out, libs__globals, libs__options.

:- import_module assoc_list, map, list, set, bool, std_util, relation, require.
:- import_module string.

stratify__check_stratification(Module0, Module) -->
	{ module_info_ensure_dependency_info(Module0, Module1) },
	{ module_info_dependency_info(Module1, DepInfo) },
	
	{ hlds_dependency_info_get_dependency_graph(DepInfo, DepGraph0) },
	{ relation__atsort(DepGraph0, FOSCCs1) },	
	{ dep_sets_to_lists_and_sets(FOSCCs1, [], FOSCCs) },
	globals__io_lookup_bool_option(warn_non_stratification, Warn),
	{ module_info_stratified_preds(Module1, StratifiedPreds) },
	first_order_check_sccs(FOSCCs, StratifiedPreds, Warn, Module1, Module).

	% The following code was used for the second pass of this module but
	% as that pass is disabled so is this code. The higher order code
	% is disabled because it is currently unable to detect cases where a 
	% higher order proc is hidden in some complex data structure
	%	
	%{ gen_conservative_graph(Module2, DepGraph0, DepGraph, HOInfo) },
	%{ relation__atsort(DepGraph, HOSCCs1) },	
	%{ dep_sets_to_lists_and_sets(HOSCCs1, [], HOSCCs) },
	%higher_order_check_sccs(HOSCCs, HOInfo, Module2, Module).

%-----------------------------------------------------------------------------%

:- pred dep_sets_to_lists_and_sets(list(set(pred_proc_id)), 
	list(pair(list(pred_proc_id), set(pred_id))), 
	list(pair(list(pred_proc_id), set(pred_id)))).
:- mode dep_sets_to_lists_and_sets(in, in, out) is det.

dep_sets_to_lists_and_sets([], Xs, Xs).
dep_sets_to_lists_and_sets([X | Xs], Ys, Zs) :-
        set__to_sorted_list(X, Y),
	list__map(get_proc_id, Y, ProcList),
	set__list_to_set(ProcList, ProcSet),
        dep_sets_to_lists_and_sets(Xs, [Y - ProcSet|Ys], Zs).

:- pred get_proc_id(pred_proc_id::in, pred_id::out) is det.
get_proc_id(proc(PredId, _), PredId).

	% check the first order SCCs for stratification
:- pred first_order_check_sccs(list(pair(list(pred_proc_id), 
	set(pred_id))), set(pred_id), bool, module_info, module_info, 
	io__state, io__state).
:- mode first_order_check_sccs(in, in, in, in, out, di, uo) is det.

first_order_check_sccs([], _, _, Module, Module) --> [].
first_order_check_sccs([SCCl - SCCs|Rest], StratifiedPreds, Warn0, 
		Module0, Module) -->
	(
		{ set__intersect(SCCs, StratifiedPreds, I) },
		{ set__empty(I) }
	->
		{ Warn = Warn0 }
	;
		{ Warn = yes }
	),
	(
		{ Warn = yes }
	->
		first_order_check_scc(SCCl, no, Module0, Module1)
	;
		{ Module1 = Module0 }
	),
	first_order_check_sccs(Rest, StratifiedPreds, Warn0, Module1, Module).

:- pred first_order_check_scc(list(pred_proc_id), bool, module_info, 
	module_info, io__state, io__state).
:- mode first_order_check_scc(in, in, in, out, di, uo) is det.

first_order_check_scc(Scc, Error, Module0, Module) -->
	first_order_check_scc_2(Scc, Scc, Error, Module0, Module).

:- pred first_order_check_scc_2(list(pred_proc_id), list(pred_proc_id), 
		bool, module_info, module_info, io__state, io__state).
:- mode first_order_check_scc_2(in, in, in, in, out, di, uo) is det.

first_order_check_scc_2([], _Scc, _, Module, Module) --> [].
first_order_check_scc_2([PredProcId|Remaining], WholeScc, Error, 
		Module0, Module) -->
	{ PredProcId = proc(PredId, ProcId) },
	{ module_info_pred_info(Module0, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, Proc) },
	{ proc_info_goal(Proc, Goal - GoalInfo) },
	first_order_check_goal(Goal, GoalInfo, no, WholeScc, 
		PredProcId, Error, Module0, Module1),
	first_order_check_scc_2(Remaining, WholeScc, Error, Module1, Module).

:- pred first_order_check_goal(hlds_goal_expr, hlds_goal_info, bool,
		list(pred_proc_id), pred_proc_id, bool,
		module_info, module_info, io__state, io__state).
:- mode first_order_check_goal(in, in, in, in, in, in, in, out, di, uo) is det.

first_order_check_goal(conj(Goals), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module).
first_order_check_goal(par_conj(Goals), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module).
first_order_check_goal(disj(Goals), _GoalInfo, Negated, 
		WholeScc, ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module).
first_order_check_goal(switch(_Var, _Fail, Cases), _GoalInfo,
		Negated, WholeScc, ThisPredProcId, Error, Module0, Module) -->
	first_order_check_case_list(Cases, Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module).
first_order_check_goal(if_then_else(_Vars, Cond - CInfo, Then - TInfo, 
		Else - EInfo), _GoalInfo, Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module) -->
	first_order_check_goal(Cond, CInfo, yes, WholeScc, ThisPredProcId,
		Error, Module0, Module1),
	first_order_check_goal(Then, TInfo, Negated, WholeScc, ThisPredProcId,
		Error, Module1, Module2),
	first_order_check_goal(Else, EInfo, Negated, WholeScc, ThisPredProcId,
		Error, Module2, Module).
first_order_check_goal(some(_Vars, _, Goal - GoalInfo), _GoalInfo,
		Negated, WholeScc, ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module).
first_order_check_goal(not(Goal - GoalInfo), _GoalInfo, _Negated, 
		WholeScc, ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal(Goal, GoalInfo, yes, WholeScc, ThisPredProcId,
		Error, Module0, Module).
first_order_check_goal(foreign_proc(_Attributes, CPred,
			CProc, _, _, _, _), 
		GoalInfo, Negated, WholeScc, ThisPredProcId, 
		Error, Module0, Module) -->
	(
		{ Negated = yes },
		{ list__member(proc(CPred, CProc),  WholeScc) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		emit_message(ThisPredProcId, Context, 
			"call introduces a non-stratified loop", 
			Error, Module0, Module)	
	;
		{ Module = Module0 }
	).
first_order_check_goal(unify(_Var, _RHS, _Mode, _Uni, _Context), _GoalInfo,
	_Negated, _WholeScc, _ThisPredProcId, _, Module, Module) --> [].
first_order_check_goal(call(CPred, CProc, _Args, _BuiltinState, _Contex, _Sym), 
		GInfo, Negated, WholeScc, ThisPredProcId, 
		Error, Module0, Module) -->
	{ Callee = proc(CPred, CProc) },
	(
		{ Negated = yes },
		{ list__member(Callee, WholeScc) }
	->
		{ goal_info_get_context(GInfo, Context) },
		emit_message(ThisPredProcId, Context, 
			"call introduces a non-stratified loop", 
			Error, Module0, Module)	
	;
		{ Module = Module0 }
	).
first_order_check_goal(generic_call(_Var, _Vars, _Modes, _Det),
	_GInfo, _Negated, _WholeScc, _ThisPredProcId, 
	_Error,  Module, Module) --> []. 
first_order_check_goal(shorthand(_), _, _, _, _, _, _, _) -->
	% these should have been expanded out by now
	{ error("first_order_check_goal: unexpected shorthand") }.

:- pred first_order_check_goal_list(list(hlds_goal), bool, 
	list(pred_proc_id), pred_proc_id, bool, module_info, 
	module_info, io__state, io__state).
:- mode first_order_check_goal_list(in, in, in, in, in, in, out, di, uo) is det.

first_order_check_goal_list([], _, _, _, _, Module, Module) --> [].
first_order_check_goal_list([Goal - GoalInfo|Goals], Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module) -->
	first_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module1),
	first_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		Error, Module1, Module).

:- pred first_order_check_case_list(list(case), bool, list(pred_proc_id),
		pred_proc_id, bool, module_info, module_info, 
		io__state, io__state).
:- mode first_order_check_case_list(in, in, in, in, in, in, out, 
		di, uo) is det.

first_order_check_case_list([], _, _, _, _, Module, Module) --> [].
first_order_check_case_list([Case|Goals], Negated, WholeScc, ThisPredProcId,
		Error, Module0, Module) -->
	{ Case = case(_ConsId, Goal - GoalInfo) },
	first_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, Error, Module0, Module1),
	first_order_check_case_list(Goals, Negated, WholeScc, ThisPredProcId,
		Error, Module1, Module).

%-----------------------------------------------------------------------------%

 % XXX : Currently we don't allow the higher order case so this code
 % is disabled.

	% check the higher order SCCs for stratification		
:- pred higher_order_check_sccs(list(pair(list(pred_proc_id), 
	set(pred_proc_id))), ho_map, module_info, module_info, 
	io__state, io__state).
:- mode higher_order_check_sccs(in, in, in, out, di, uo) is det.

higher_order_check_sccs([], _HOInfo, Module, Module) --> []. 
higher_order_check_sccs([SCCl - SCCs|Rest], HOInfo, Module0, Module) -->
	higher_order_check_scc(SCCl, SCCs, HOInfo, Module0, Module1),
	higher_order_check_sccs(Rest, HOInfo, Module1, Module).

:- pred higher_order_check_scc(list(pred_proc_id), set(pred_proc_id), ho_map,
		module_info, module_info, io__state, io__state).
:- mode higher_order_check_scc(in, in, in, in, out, di, uo) is det.

higher_order_check_scc([], _WholeScc, _HOInfo, Module, Module) --> [].
higher_order_check_scc([PredProcId|Remaining], WholeScc, HOInfo, Module0, 
		Module) -->
	{ PredProcId = proc(PredId, ProcId) },
	{ module_info_pred_info(Module0, PredId, PredInfo) },
	globals__io_lookup_bool_option(warn_non_stratification, Warn),
	{ Error = no },
	(	( { Error = yes ; Warn = yes } ),
		{ map__search(HOInfo, PredProcId, HigherOrderInfo) }
	->
		{ HigherOrderInfo = info(HOCalls, _) },
		{ set__intersect(HOCalls, WholeScc, HOLoops) },
		(
			{ set__empty(HOLoops) }
		->
			{ HighOrderLoops = no }
		;
			{ HighOrderLoops = yes }
		),
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ map__lookup(ProcTable, ProcId, Proc) },
		{ proc_info_goal(Proc, Goal - GoalInfo) },
		higher_order_check_goal(Goal, GoalInfo, no, WholeScc, 
			PredProcId, HighOrderLoops, Error, Module0, Module1)
	;
		{ Module1 = Module0 }
	),
	higher_order_check_scc(Remaining, WholeScc, HOInfo, Module1, Module).

:- pred higher_order_check_goal(hlds_goal_expr, hlds_goal_info, bool,
		set(pred_proc_id), pred_proc_id, bool, bool,
		module_info, module_info, io__state, io__state).
:- mode higher_order_check_goal(in, in, in, in, in, in, in, in, 
	out, di, uo) is det.

higher_order_check_goal(conj(Goals), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(par_conj(Goals), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(disj(Goals), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(switch(_Var, _Fail, Cases), _GoalInfo,
		Negated, WholeScc, ThisPredProcId, HighOrderLoops, 
		Error, Module0, Module) -->
	higher_order_check_case_list(Cases, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(if_then_else(_Vars, Cond - CInfo, Then - TInfo, 
		Else - EInfo), _GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal(Cond, CInfo, yes, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module1),
	higher_order_check_goal(Then, TInfo, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module1, Module2),
	higher_order_check_goal(Else, EInfo, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module2, Module).
higher_order_check_goal(some(_Vars, _, Goal - GoalInfo), _GoalInfo, Negated, 
		WholeScc, ThisPredProcId, HighOrderLoops, 
		Error, Module0, Module) -->
	higher_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(not(Goal - GoalInfo), _GoalInfo, _Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal(Goal, GoalInfo, yes, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module).
higher_order_check_goal(foreign_proc(_IsRec, _, _, _, _, _, _),
	_GoalInfo, _Negated, _WholeScc, _ThisPredProcId, _HighOrderLoops, 
	_, Module, Module) --> [].
higher_order_check_goal(unify(_Var, _RHS, _Mode, _Uni, _Context), _GoalInfo,
	_Negated, _WholeScc, _ThisPredProcId, _HighOrderLoops, 
	_Error, Module, Module) --> [].
higher_order_check_goal((call(_CPred, _CProc, _Args, _Builtin, _Contex, Sym)), 
		GoalInfo, _Negated, _WholeScc, ThisPredProcId, HighOrderLoops, 
		Error, Module0, Module) --> 
	(
		% XXX : is this good enough to detect all calls to solutions ?
		{ HighOrderLoops = yes },
		(	{ Sym = unqualified(Name) }
		;
			{ Sym = qualified(_, Name) }
		),
		{ Name = "solutions" }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		emit_message(ThisPredProcId, Context, 
			"call to solutions/2 introduces a non-stratified loop", 
			Error, Module0, Module)
					
	;
		{ Module = Module0 }
	).
	
higher_order_check_goal(generic_call(GenericCall, _Vars, _Modes, _Det), 
		GoalInfo, Negated, _WholeScc, ThisPredProcId, HighOrderLoops, 
		Error, Module0, Module) -->
	(
		{ Negated = yes },
		{ HighOrderLoops = yes },
		{ GenericCall = higher_order(_, _, _), Msg = "higher order"
		; GenericCall = class_method(_, _, _, _), Msg = "class method"
		}
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ string__append(Msg, 
			" call may introduce a non-stratified loop",
			ErrorMsg) },
		emit_message(ThisPredProcId, Context, ErrorMsg,
			Error, Module0, Module)		
	;
		{ Module = Module0 }
	).
higher_order_check_goal(shorthand(_), _, _, _, _, _, _, _, _) -->
	% these should have been expanded out by now
	{ error("higher_order_check_goal: unexpected shorthand") }.

:- pred higher_order_check_goal_list(list(hlds_goal), bool, set(pred_proc_id),
	pred_proc_id, bool, bool, module_info, module_info, 
	io__state, io__state).
:- mode higher_order_check_goal_list(in, in, in, in, in, in, in, out, 
	di, uo) is det.

higher_order_check_goal_list([], _, _, _, _, _, Module, Module) --> [].
higher_order_check_goal_list([Goal - GoalInfo|Goals], Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module) -->
	higher_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module1),
	higher_order_check_goal_list(Goals, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module1, Module).

:- pred higher_order_check_case_list(list(case), bool, set(pred_proc_id),
	pred_proc_id, bool, bool, module_info, module_info, 
	io__state, io__state).
:- mode higher_order_check_case_list(in, in, in, in, in, in, in, out, 
	di, uo) is det.

higher_order_check_case_list([], _, _, _, _, _, Module, Module) --> [].
higher_order_check_case_list([Case|Goals], Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module0, Module) -->
	{ Case = case(_ConsId, Goal - GoalInfo) },
	higher_order_check_goal(Goal, GoalInfo, Negated, WholeScc, 
		ThisPredProcId, HighOrderLoops, Error, Module0, Module1),
	higher_order_check_case_list(Goals, Negated, WholeScc, ThisPredProcId,
		HighOrderLoops, Error, Module1, Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% direction higher order params can flow in a proc
:- type ho_in_out 
	--->
		ho_in ;
		ho_out ;
		ho_in_out ;
		ho_none.

	% this structure is used to hold the higher order
	% characteristics of a proc
:- type higher_order_info 
	--->
		info(
			set(pred_proc_id),  % possible higher order
					    % addrs than can reach the
					    % proc
			ho_in_out	    % possible paths the addrs can 
					    % take in and out of the proc
		).

	% a map from all non imported procs to there higher order
	% info
:- type ho_map   == map(pred_proc_id, higher_order_info).	

	% a map from all non imported procs to all the procs they can
	% call
:- type call_map == map(pred_proc_id, set(pred_proc_id)).

	% given a module and a dependency graph this pred
	% builds a new dependency graph with all possible
	% higher order calls added, it also returns a map of all the
	% higher order info collected by this pred
:- pred gen_conservative_graph(module_info, dependency_graph, 
	dependency_graph, ho_map).
:- mode gen_conservative_graph(in, in, out, out) is det.

gen_conservative_graph(Module, DepGraph0, DepGraph, HOInfo) :-
	get_call_info(Module, ProcCalls, HOInfo0, CallsHO),
	map__keys(ProcCalls, Callers),
	iterate_solution(Callers, ProcCalls, CallsHO, HOInfo0, HOInfo),
	map__to_assoc_list(HOInfo, HOInfoL),
	add_new_arcs(HOInfoL, CallsHO, DepGraph0, DepGraph).

	% For a given module collects for each non imported proc a set 
	% of called procs and a higher order info structure. This pred 
	% also returns a set of all non imported procs that make a 
	% higher order call
:- pred get_call_info(module_info, call_map, ho_map, set(pred_proc_id)).
:- mode get_call_info(in, out, out, out) is det.

get_call_info(Module, ProcCalls, HOInfo, CallsHO) :-
	map__init(ProcCalls0),
	map__init(HOInfo0),
	set__init(CallsHO0),
	module_info_predids(Module, PredIds),
	expand_predids(PredIds, Module, ProcCalls0, ProcCalls, HOInfo0, 
		HOInfo, CallsHO0, CallsHO). 

	% find the transitive closure of a given list of procs
	% this pred is used to see how face a higher order address can
	% reach though proc calls
:- pred iterate_solution(list(pred_proc_id), call_map, set(pred_proc_id), 
	ho_map, ho_map).
:- mode iterate_solution(in, in, in, in, out) is det.

iterate_solution(PredProcs, ProcCalls, CallsHO, HOInfo0, HOInfo) :-
        tc(PredProcs, ProcCalls, CallsHO, HOInfo0, HOInfo1, no, Changed),
        (
                Changed = no,
		HOInfo = HOInfo1
        ;
                Changed = yes,
                iterate_solution(PredProcs, ProcCalls, CallsHO, 
			HOInfo1, HOInfo)
        ).

	% for each caller merge any higher order addresses it takes with all of
	% its callees and return if any change has occurred
:- pred tc(list(pred_proc_id), call_map, set(pred_proc_id), ho_map, ho_map, 
	bool, bool).
:- mode tc(in, in, in, in, out, in, out) is det.

tc([], _, _, HOInfo, HOInfo, Changed, Changed).
tc([P|Ps], ProcCalls, CallsHO, HOInfo0, HOInfo, Changed0, Changed) :-
	map__lookup(ProcCalls, P, PCalls),
	set__to_sorted_list(PCalls, PCallsL),
	merge_calls(PCallsL, P, CallsHO, yes, HOInfo0, HOInfo1, 
		Changed0, Changed1),
	tc(Ps, ProcCalls, CallsHO, HOInfo1, HOInfo, Changed1, Changed).

	% merge any higher order addresses that can pass between the
	% given caller and callees. This code also merges any possible
	% addresses that can pass in and out of higher order calls
:- pred merge_calls(list(pred_proc_id), pred_proc_id, set(pred_proc_id), bool, 
	ho_map, ho_map, bool, bool).
:- mode merge_calls(in, in, in, in, in, out, in, out) is det.

merge_calls([], _, _, _, HOInfo, HOInfo, Changed, Changed).
merge_calls([C|Cs], P, CallsHO, DoingFirstOrder, HOInfo0, HOInfo, Changed0, 
		Changed) :-
	(
		map__search(HOInfo0, C, CInfo)
	->
		map__lookup(HOInfo0, P, PInfo),
		CInfo = info(CHaveAT0, CHOInOut),
		PInfo = info(PHaveAT0, PHOInOut),
		% first merge the first order info, if we need to
		(
			CHOInOut = ho_none
		->
			Changed1 = Changed0,
			HOInfo2 = HOInfo0
		;
			(
				CHOInOut = ho_in, 
				(
					set__subset(PHaveAT0, CHaveAT0) 
				->
					Changed1 = Changed0,
					CHaveAT = CHaveAT0
				;
					set__union(PHaveAT0, CHaveAT0, 
						CHaveAT),
					Changed1 = yes
				),
				PHaveAT = PHaveAT0
			;
				CHOInOut = ho_out,
				(
					set__subset(CHaveAT0, PHaveAT0)
				->
					Changed1 = Changed0,
					PHaveAT = PHaveAT0
				;
					set__union(CHaveAT0, PHaveAT0, 
						PHaveAT),
					Changed1 = yes
				),
				CHaveAT = CHaveAT0
			;
				CHOInOut = ho_in_out,
				(
					CHaveAT0 = PHaveAT0
				->
					CHaveAT = CHaveAT0,
					PHaveAT = PHaveAT0,
					Changed1 = Changed0
				;
					set__union(CHaveAT0, PHaveAT0, 
						NewHaveAT),
					CHaveAT = NewHaveAT,
					PHaveAT = NewHaveAT,
					Changed1 = yes
				)
			;
				CHOInOut = ho_none,
				% XXX : what is a good message for this?
				error("merge_calls : this cant happen!")
			),
			NewCInfo = info(CHaveAT, CHOInOut),
			NewPInfo = info(PHaveAT, PHOInOut),
			map__det_update(HOInfo0, C, NewCInfo, HOInfo1),
			map__det_update(HOInfo1, P, NewPInfo, HOInfo2)
		),
		% then, if we need to, merge the higher order info
		(
			DoingFirstOrder = yes,
			set__member(P, CallsHO)
		->
			map__lookup(HOInfo2, P, PHOInfo),
			PHOInfo = info(PossibleCalls, _),
			set__to_sorted_list(PossibleCalls, PossibleCallsL),
			merge_calls(PossibleCallsL, P, CallsHO, no, HOInfo2,
				HOInfo3, Changed1, Changed2)
		;
			Changed2 = Changed1,
			HOInfo3 = HOInfo2
		),
		merge_calls(Cs, P, CallsHO, DoingFirstOrder, HOInfo3, 
			HOInfo, Changed2, Changed)
	;
		merge_calls(Cs, P, CallsHO, DoingFirstOrder, HOInfo0, HOInfo,
			Changed0, Changed)
	).
 
	% given the set of procs that make higher order calls and a
	% list of procs and higher order call info this pred rebuilds
	% the given call graph with new arcs for every possible higher
	% order call
:- pred add_new_arcs(assoc_list(pred_proc_id, higher_order_info), 
	set(pred_proc_id), dependency_graph, dependency_graph).
:- mode add_new_arcs(in, in, in, out) is det.

add_new_arcs([], _, DepGraph, DepGraph).
add_new_arcs([Caller - CallerInfo|Cs], CallsHO, DepGraph0, DepGraph) :-
	(
		% only add arcs for callers who call higher order procs
		set__member(Caller, CallsHO)
	->
		CallerInfo = info(PossibleCallees0, _),
		set__to_sorted_list(PossibleCallees0, PossibleCallees),
		relation__lookup_element(DepGraph0, Caller, CallerKey),
		add_new_arcs2(PossibleCallees, CallerKey, DepGraph0, 
			DepGraph1)
	;
		DepGraph1 = DepGraph0
	),
	add_new_arcs(Cs, CallsHO, DepGraph1, DepGraph).

:- pred add_new_arcs2(list(pred_proc_id), relation_key, dependency_graph, 
	dependency_graph).
:- mode add_new_arcs2(in, in, in, out) is det.

add_new_arcs2([], _, DepGraph, DepGraph).
add_new_arcs2([Callee|Cs], CallerKey, DepGraph0, DepGraph) :-
	relation__lookup_element(DepGraph0, Callee, CalleeKey),
	relation__add(DepGraph0, CallerKey, CalleeKey, DepGraph1),
	add_new_arcs2(Cs, CallerKey, DepGraph1, DepGraph).

	% for each given pred id pass all non imported procs onto the
	% process_procs pred
:- pred expand_predids(list(pred_id), module_info, call_map, call_map, 
	ho_map, ho_map, set(pred_proc_id), set(pred_proc_id)). 
:- mode expand_predids(in, in, in, out, in, out, in, out) is det.

expand_predids([], _, ProcCalls, ProcCalls, HOInfo, HOInfo, CallsHO, CallsHO). 
expand_predids([PredId|PredIds], Module, ProcCalls0, ProcCalls, HOInfo0, 
		HOInfo, CallsHO0, CallsHO) :- 
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_non_imported_procids(PredInfo, Procs),
	pred_info_procedures(PredInfo, ProcTable),
	pred_info_arg_types(PredInfo, ArgTypes),
	process_procs(Procs, Module, PredId, ArgTypes, ProcTable, ProcCalls0, 
		ProcCalls1, HOInfo0, HOInfo1, CallsHO0, CallsHO1), 
	expand_predids(PredIds, Module, ProcCalls1, ProcCalls, HOInfo1, 
		HOInfo, CallsHO1, CallsHO).

	% for each given proc id generate the set of procs it calls and
	% its higher order info structure
:- pred process_procs(list(proc_id), module_info, pred_id, list(type), 
	proc_table, call_map, call_map, ho_map, ho_map, set(pred_proc_id), 
	set(pred_proc_id)).
:- mode process_procs(in, in, in, in, in, in, out, in, out, in, out) is det.

process_procs([], _, _, _, _, ProcCalls, ProcCalls, HOInfo, HOInfo, 
	CallsHO, CallsHO). 
process_procs([ProcId|Procs], Module, PredId, ArgTypes, ProcTable, ProcCalls0,
		ProcCalls, HOInfo0, HOInfo, CallsHO0, CallsHO) :- 
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_goal(ProcInfo, Goal - _GoalInfo),
	PredProcId = proc(PredId, ProcId),
	check_goal(Goal, Calls, HaveAT, CallsHigherOrder),
	map__det_insert(ProcCalls0, PredProcId, Calls, ProcCalls1),
	higherorder_in_out(ArgTypes, ArgModes, Module, HOInOut),
	map__det_insert(HOInfo0, PredProcId, info(HaveAT, HOInOut), 
		HOInfo1),
	(
		CallsHigherOrder = yes,
		set__insert(CallsHO0, PredProcId, CallsHO1)
	;
		CallsHigherOrder = no,
		CallsHO1 = CallsHO0
	),
	process_procs(Procs, Module, PredId, ArgTypes, ProcTable, ProcCalls1,
		ProcCalls, HOInfo1, HOInfo, CallsHO1, CallsHO).
	
	% determine if a given set of modes and types indicates that
	% higher order values can be passed into and/or out of a proc
:- pred higherorder_in_out(list(type), list(mode), module_info, ho_in_out). 
:- mode higherorder_in_out(in, in, in, out) is det.

higherorder_in_out(Types, Modes, Module, HOInOut) :-
	higherorder_in_out1(Types, Modes, Module, no, HOIn, no, HOOut),
	bool_2_ho_in_out(HOIn, HOOut, HOInOut).

:- pred bool_2_ho_in_out(bool, bool, ho_in_out).
:- mode bool_2_ho_in_out(in, in, out) is det.

bool_2_ho_in_out(yes, no, ho_in).
bool_2_ho_in_out(no, yes, ho_out).
bool_2_ho_in_out(yes, yes, ho_in_out).
bool_2_ho_in_out(no, no, ho_none).
	
:- pred higherorder_in_out1(list(type), list(mode), module_info, bool, bool,
	bool, bool).
:- mode higherorder_in_out1(in, in, in, in, out, in, out) is det.

higherorder_in_out1([], [], _Module, HOIn, HOIn, HOOut, HOOut).
higherorder_in_out1([], [_|_], _, _, _, _, _) :-
	error("higherorder_in_out1: lists were different lengths").
higherorder_in_out1([_|_], [], _, _, _, _, _) :-
	error("higherorder_in_out1: lists were different lengths").
higherorder_in_out1([Type|Types], [Mode|Modes], Module, HOIn0, HOIn, 	
		HOOut0, HOOut) :-
	(
		% XXX : will have to use a more general check for higher
		% order constants in parameters user could hide higher
		% order consts in a data structure etc..
		type_is_higher_order(Type, _, _, _)
	->	
		(
			mode_is_input(Module, Mode) 
		->	
			HOIn1 = yes,
			HOOut1 = HOOut0
		;	
			mode_is_output(Module, Mode)
		->
			HOOut1 = yes,
			HOIn1 = HOIn0
		;
			HOIn1 = HOIn0,
			HOOut1 = HOOut0
		)
	;
		HOIn1 = HOIn0,
		HOOut1 = HOOut0
	),
	higherorder_in_out1(Types, Modes, Module, HOIn1, HOIn, HOOut1, HOOut).
	
	% return the set of all procs called in and all addresses
	% taken, in a given goal
:- pred check_goal(hlds_goal_expr, set(pred_proc_id), set(pred_proc_id), 
	bool). 
:- mode check_goal(in, out, out, out) is det.

check_goal(Goal, Calls, TakenAddrs, CallsHO) :-
	set__init(Calls0), 
	set__init(TakenAddrs0),
	check_goal1(Goal, Calls0, Calls, TakenAddrs0, TakenAddrs, no, CallsHO).

:- pred check_goal1(hlds_goal_expr, set(pred_proc_id), set(pred_proc_id), 
	set(pred_proc_id), set(pred_proc_id), bool, bool).
:- mode check_goal1(in, in, out, in, out, in, out) is det.

	% see if a goal has its address taken
check_goal1(unify(_Var, RHS, _Mode, Unification, _Context), Calls, 
		Calls, HasAT0, HasAT, CallsHO, CallsHO) :- 
	(
		% currently this code assumes that all procs called in a
		% lambda goal have addresses taken. this is not
		% always to case, but should be a suitable approximation for
		% the stratification analysis
		RHS = lambda_goal(_PredOrFunc, _EvalMethod, _Fix, _NonLocals,
				_Vars, _Modes, _Determinism, Goal - _GoalInfo)
	->
		get_called_procs(Goal, [], CalledProcs),
		set__insert_list(HasAT0, CalledProcs, HasAT)

	;
		% currently when this pass is run the construct/4
		% case will not happen as higher order constants have
		% been transformed to lambda goals. see above
		Unification = construct(_Var2, ConsId, _, _, _, _, _)
	->
		(
			(
				ConsId = pred_const(PredId, ProcId, _)
			;
				ConsId = code_addr_const(PredId, ProcId)
			)
		->
			set__insert(HasAT0, proc(PredId, ProcId), HasAT)
		;
			HasAT = HasAT0
		)
	;
		HasAT = HasAT0
	).
	
	% add this call to the call list
check_goal1(call(CPred, CProc, _Args, _Builtin, _Contex, _Sym), Calls0, Calls, 
		HasAT, HasAT, CallsHO, CallsHO) :-
	set__insert(Calls0, proc(CPred, CProc), Calls).

	% record that the higher order call was made
check_goal1(generic_call(_Var, _Vars, _Modes, _Det),
		Calls, Calls, HasAT, HasAT, _, yes).

check_goal1(conj(Goals), Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO) :-
	check_goal_list(Goals, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO).
check_goal1(par_conj(Goals), Calls0, Calls, HasAT0, HasAT,
		CallsHO0, CallsHO) :-
	check_goal_list(Goals, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO).
check_goal1(disj(Goals), Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO) :-
	check_goal_list(Goals, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO).
check_goal1(switch(_Var, _Fail, Cases), Calls0, Calls, HasAT0, 
		HasAT, CallsHO0, CallsH0) :- 
	check_case_list(Cases, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsH0).
check_goal1(if_then_else(_Vars, Cond - _CInfo, Then - _TInfo, Else - _EInfo),
		Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO) :-
	check_goal1(Cond, Calls0, Calls1, HasAT0, HasAT1, CallsHO0, CallsHO1),
	check_goal1(Then, Calls1, Calls2, HasAT1, HasAT2, CallsHO1, CallsHO2),
	check_goal1(Else, Calls2, Calls, HasAT2, HasAT, CallsHO2, CallsHO).
	
check_goal1(some(_Vars, _, Goal - _GoalInfo), Calls0, Calls, HasAT0, HasAT, 
		CallsHO0, CallsHO) :- 
	check_goal1(Goal, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO).

check_goal1(not(Goal - _GoalInfo), Calls0, Calls, HasAT0, HasAT, CallsHO0,
		CallsHO) :- 
	check_goal1(Goal, Calls0, Calls, HasAT0, HasAT, CallsHO0, CallsHO).

check_goal1(foreign_proc(_Attrib, _CPred, _CProc, _, _, _, _),
		Calls, Calls, HasAT, HasAT, CallsHO, CallsHO).

check_goal1(shorthand(_), _, _, _, _, _, _) :-
	% these should have been expanded out by now
	error("check_goal1: unexpected shorthand").
	
:- pred check_goal_list(list(hlds_goal), set(pred_proc_id), set(pred_proc_id), 
	set(pred_proc_id), set(pred_proc_id), bool, bool). 
:- mode check_goal_list(in, in, out, in, out, in, out) is det.

check_goal_list([], Calls, Calls, HasAT, HasAT, CallsHO, CallsHO).
check_goal_list([Goal - _GoalInfo|Goals], Calls0, Calls, HasAT0, HasAT, 
		CallsHO0, CallsHO) :-  
	check_goal1(Goal, Calls0, Calls1, HasAT0, HasAT1, CallsHO0, CallsHO1),
	check_goal_list(Goals, Calls1, Calls, HasAT1, HasAT, CallsHO1, CallsHO).

:- pred check_case_list(list(case), set(pred_proc_id), set(pred_proc_id),
	set(pred_proc_id), set(pred_proc_id), bool, bool). 
:- mode check_case_list(in, in, out, in, out, in, out) is det.

check_case_list([], Calls, Calls, HasAT, HasAT, CallsHO, CallsHO). 
check_case_list([Case|Goals], Calls0, Calls, HasAT0, HasAT, CallsHO0, 
		CallsHO) :-
	Case = case(_ConsId, Goal - _GoalInfo),
	check_goal1(Goal, Calls0, Calls1, HasAT0, HasAT1, CallsHO0, CallsHO1),
	check_case_list(Goals, Calls1, Calls, HasAT1, HasAT, CallsHO1, CallsHO).

	% This pred returns a list of all the calls in a given set of
	% goals including calls in unification lambda functions and
	% pred_proc_id's in constructs 
:- pred get_called_procs(hlds_goal_expr, list(pred_proc_id), 
	list(pred_proc_id)). 
:- mode get_called_procs(in, in, out) is det.

get_called_procs(unify(_Var, RHS, _Mode, Unification, _Context), Calls0, 
		Calls) :- 
	(
		% currently this code assumes that all procs called in a
		% lambda goal have addresses taken. this is not
		% always to case, but should be a suitable approximation for
		% the stratification analysis
		RHS = lambda_goal(_PredOrFunc, _EvalMethod, _Fix, _NonLocals,
				_Vars, _Modes, _Determinism, Goal - _GoalInfo)
	->
		get_called_procs(Goal, Calls0, Calls)
	;
		% currently when this pass is run the construct/4
		% case will not happen as higher order constants have
		% been transformed to lambda goals see above
		Unification = construct(_Var2, ConsId, _, _, _, _, _)
	->
		(
			(
				ConsId = pred_const(PredId, ProcId, _)
			;
				ConsId = code_addr_const(PredId, ProcId)
			)
		->
			Calls =  [proc(PredId, ProcId) | Calls0]
		;
			Calls = Calls0
		)
	;
		Calls = Calls0	
	).
	
	% add this call to the call list
get_called_procs(call(CPred, CProc, _Args, _Builtin, _Contex, _Sym), Calls0, 
		Calls) :- 
	Calls = [proc(CPred, CProc) | Calls0].

get_called_procs(generic_call(_Var, _Vars, _Modes, _Det), Calls, Calls).

get_called_procs(conj(Goals), Calls0, Calls) :-
	check_goal_list(Goals, Calls0, Calls).
get_called_procs(par_conj(Goals), Calls0, Calls) :-
	check_goal_list(Goals, Calls0, Calls).
get_called_procs(disj(Goals), Calls0, Calls) :-
	check_goal_list(Goals, Calls0, Calls).
get_called_procs(switch(_Var, _Fail, Cases), Calls0, Calls) :-
	check_case_list(Cases, Calls0, Calls).
get_called_procs(if_then_else(_Vars, Cond - _CInfo, Then - _TInfo, 
		Else - _EInfo), Calls0, Calls) :-
	get_called_procs(Cond, Calls0, Calls1),
	get_called_procs(Then, Calls1, Calls2),
	get_called_procs(Else, Calls2, Calls). 
get_called_procs(some(_Vars, _, Goal - _GoalInfo), Calls0, Calls) :-
	get_called_procs(Goal, Calls0, Calls).
get_called_procs(not(Goal - _GoalInfo), Calls0, Calls) :-
	get_called_procs(Goal, Calls0, Calls).
get_called_procs(foreign_proc(_Attrib, _CPred, _CProc,
		_, _, _, _), Calls, Calls).
get_called_procs(shorthand(_), _, _) :-
	% these should have been expanded out by now
	error("get_called_procs: unexpected shorthand").

:- pred check_goal_list(list(hlds_goal), list(pred_proc_id), 
	list(pred_proc_id)).
:- mode check_goal_list(in, in, out) is det.

check_goal_list([], Calls, Calls).
check_goal_list([Goal - _GoalInfo|Goals], Calls0, Calls) :-
	get_called_procs(Goal, Calls0, Calls1), 
	check_goal_list(Goals, Calls1, Calls). 

:- pred check_case_list(list(case), list(pred_proc_id), list(pred_proc_id)).
:- mode check_case_list(in, in, out) is det.

check_case_list([], Calls, Calls). 
check_case_list([Case|Goals], Calls0, Calls) :-
	Case = case(_ConsId, Goal - _GoalInfo),
	get_called_procs(Goal, Calls0, Calls1), 
	check_case_list(Goals, Calls1, Calls).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred emit_message(pred_proc_id, prog_context, string, bool, 
		module_info, module_info, io__state, io__state).
:- mode emit_message(in, in, in, in, in, out, di, uo) is det.

emit_message(ThisPredProc, Context, Message, Error, Module0, Module) -->
	{ ThisPredProc = proc(TPred, TProc) },
	report_pred_proc_id(Module0, TPred, TProc, yes(Context), _Context),
	prog_out__write_context(Context),
	(
		{ Error = no }
	->
		{ Module = Module0 },
		io__write_string("  warning: ")
	;
		{ module_info_incr_errors(Module0, Module) },
		io__set_exit_status(1),
		io__write_string("  error: ")
	),
	io__write_string(Message),
	io__write_char('\n'),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string("\tA non-stratified loop is a loop in the call graph of the given\n"),
		io__write_string("\tpredicate/function that allows it to call itself negatively. This\n"),
		io__write_string("\tcan cause problems for bottom up evaluation of the predicate/function.\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%
