%-----------------------------------------------------------------------------
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module higher_order.
% Main author: stayl
%
% Specializes calls to higher order predicates where the value of one or more
% higher order arguments are known. Since this creates a new copy of the 
% called procedure I have limited the specialization to cases where the called
% procedure's goal contains less than 20 calls and unifications. For predicates
% above this size the overhead of the higher order call becomes less 
% significant while the increase in code size becomes significant.
% If a specialization creates new opportunities for specialization, the
% specialization process will be iterated until no further opportunities arise.
% The specialized version for predicate 'foo' is named 'foo__ho<n>', where n
% is a number that uniquely identifies this specialized version.
%-------------------------------------------------------------------------------

:- interface.

:- import_module hlds_module, io.

:- pred specialize_higher_order(module_info::in, module_info::out,
			io__state::di, io__state::uo) is det.

%-------------------------------------------------------------------------------

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, instmap.
:- import_module code_util, globals, make_hlds, mode_util, goal_util.
:- import_module type_util, options, prog_data, quantification.
:- import_module mercury_to_mercury.

:- import_module assoc_list, bool, char, int, list, map, require, set.
:- import_module std_util, string, varset, term.

	% Iterate collecting requests and processing them until there
	% are no more requests remaining.
specialize_higher_order(ModuleInfo0, ModuleInfo) -->
	{ get_specialization_requests(Requests, GoalSizes,
						ModuleInfo0, ModuleInfo1) },
	{ map__init(NewPreds0) },
	process_requests(Requests, GoalSizes, 1, _NextHOid,
				NewPreds0, _NewPreds, ModuleInfo1, ModuleInfo).

:- pred process_requests(set(request)::in, goal_sizes::in, int::in,
		int::out, new_preds::in, new_preds::out, module_info::in,
		module_info::out, io__state::di, io__state::uo) is det.

process_requests(Requests0, GoalSizes0, NextHOid0, NextHOid,
			NewPreds0, NewPreds, ModuleInfo1, ModuleInfo) -->
	{ filter_requests(ModuleInfo1, Requests0, GoalSizes0, Requests) },
	(
		{ Requests = [] }
	->
		{ ModuleInfo = ModuleInfo1 },
		{ NextHOid = NextHOid0 },
		{ NewPreds = NewPreds0 }
	;
		{ set__init(PredProcsToFix0) },
		{ map__init(NewPredsForThisPass0) },
		create_new_preds(Requests, NewPredsForThisPass0,
			NewPredsForThisPass, PredProcsToFix0, PredProcsToFix,
			NextHOid0, NextHOid1, ModuleInfo1, ModuleInfo2),
		{ map__keys(NewPredsForThisPass, SpecializedPreds) },
		{ map__merge(NewPreds0, NewPredsForThisPass, NewPreds1) },
		{ set__to_sorted_list(PredProcsToFix, PredProcs) },
		{ set__init(NewRequests0) },
		{ create_specialized_versions(SpecializedPreds, NewPreds1,
				NewRequests0, NewRequests, GoalSizes0,
				GoalSizes, ModuleInfo2, ModuleInfo3) },
			% The dependencies have changed, so the
			% dependency graph needs to rebuilt for
			% inlining to work properly.
			% XXX Is it worth rebuilding the dependency graph
			% here completely rebuilding it later? (it's only
			% necessary in profiling grades, since otherwise
			% the dependency graph isn't built before here). 
		{ fixup_preds(PredProcs, NewPreds1, ModuleInfo3, ModuleInfo4) },
		{ SpecializedPreds = [] ->
			module_info_clobber_dependency_info(ModuleInfo4,
				ModuleInfo5)
		;
			ModuleInfo5 = ModuleInfo4
		},
		process_requests(NewRequests, GoalSizes, NextHOid1,
			NextHOid, NewPreds1, NewPreds, ModuleInfo5, ModuleInfo)
	).



%-------------------------------------------------------------------------------

	% The largest goal that will be specialized. Goal size is measured
	% by the number of calls and unifications the goal contains. This is
	% used to stop specialization of large predicates, for which the 
	% call overhead will be less noticeable and the increase in code size
	% will be significant.
:- pred max_specialized_goal_size(int::out) is det.

max_specialized_goal_size(20).


:- type request --->
	request(
		pred_proc_id,			% calling pred
		pred_proc_id,			% called pred 
		list(higher_order_arg)
	).

		% Stores pred_id, proc_id, index in argument vector, number of 
		% curried arguments of a higher order argument, higher-order
		% curried arguments with known values.
:- type higher_order_arg --->
		higher_order_arg(
			pred_id,
			proc_id,
		 	int,			% index in argument vector
			int,			% number of curried args
			list(higher_order_arg)	% higher-order curried arguments
						% with known values
		).

:- type goal_sizes == map(pred_id, int). 	%stores the size of each
				% predicate's goal used in the heuristic
				% to decide which preds are specialized
		
	% Used to hold the value of known higher order variables.
	% If a variable is not in the map, it does not have a value yet
	% If it is in the map as a yes, it has been seen previously, and has
	% a unique possible value, and calls involving it can be specialized.
	% If it is in the map as a no, it has more than one possible value,
	% and higher order calls involving it cannot be specialized.
:- type pred_vars == map(var, maybe_pred_and_args). 

	% The list of vars is a list of the curried arguments, which must
	% be explicitly passed to the specialized predicate. The list of 
	% uni_modes is their modes in the unification.
:- type maybe_pred_and_args --->
		yes(pred_id, proc_id, list(var))
	;	no.

	% used while traversing goals
:- type higher_order_info --->
		info(pred_vars, set(request), new_preds, module_info).

:- type new_preds == map(pred_proc_id, set(new_pred)).

:- type new_pred --->
		new_pred(
			pred_id,
			proc_id,
			sym_name,		% name 
			list(higher_order_arg)	% specialized args
		).


	% Returned by traverse_goal. 
:- type changed --->
		changed		% Need to requantify goal + check other procs
	;	request		% Need to check other procs
	;	unchanged.	% Do nothing more for this predicate

%-------------------------------------------------------------------------------
:- pred get_specialization_requests(set(request)::out, goal_sizes::out,
			module_info::in, module_info::out) is det.

get_specialization_requests(Requests, GoalSizes, ModuleInfo0, ModuleInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	map__init(GoalSizes0),
	set__init(Requests0),
	get_specialization_requests_2(PredIds, Requests0, Requests,
			GoalSizes0, GoalSizes, ModuleInfo0, ModuleInfo).


:- pred get_specialization_requests_2(list(pred_id)::in, set(request)::in, 
		set(request)::out, goal_sizes::in, goal_sizes::out, 
		module_info::in, module_info::out) is det.

get_specialization_requests_2([], Requests, Requests, Sizes, Sizes, 
					ModuleInfo, ModuleInfo).
get_specialization_requests_2([PredId | PredIds], Requests0, Requests,
			GoalSizes0, GoalSizes, ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds0), 
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_non_imported_procids(PredInfo0, NonImportedProcs),
	(
		NonImportedProcs = [],
		Requests2 = Requests0,
		GoalSizes1 = GoalSizes0,
		ModuleInfo1 = ModuleInfo0
	;
		NonImportedProcs = [ProcId | ProcIds],
		pred_info_procedures(PredInfo0, Procs0),
		map__lookup(Procs0, ProcId, ProcInfo0),
		proc_info_goal(ProcInfo0, Goal0),
		map__init(PredVars0),
			% first time through we can only specialize call/N
		map__init(NewPreds0),
		PredProcId = proc(PredId, ProcId),
		Info0 = info(PredVars0, Requests0, NewPreds0, ModuleInfo0),
		traverse_goal(Goal0, Goal1, PredProcId, Changed,
				GoalSize, Info0, info(_, Requests1,_,_)),
		map__set(GoalSizes0, PredId, GoalSize, GoalSizes1),
		proc_info_set_goal(ProcInfo0, Goal1, ProcInfo1),
		(
			Changed = changed
		->
			requantify_proc(ProcInfo1, ProcInfo),
			map__det_update(Procs0, ProcId, ProcInfo, Procs1)
		;
			Procs1 = Procs0
		),
		(
			(Changed = request ; Changed = changed)
		->
			traverse_other_procs(PredId, ProcIds, ModuleInfo0,
					Requests1, Requests2, Procs1, Procs),
			pred_info_set_procedures(PredInfo0, Procs, PredInfo),
			map__det_update(Preds0, PredId, PredInfo, Preds),
			module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1)
		;
			ModuleInfo1 = ModuleInfo0,
			Requests2 = Requests1
		)
	),
	get_specialization_requests_2(PredIds, Requests2, Requests,
			GoalSizes1, GoalSizes, ModuleInfo1, ModuleInfo).

		% This is called when the first procedure of a pred was 
		% changed. It fixes up all the other procs, ignoring the
		% goal_size and requests that come out, since that information
		% has already been collected. 
:- pred traverse_other_procs(pred_id::in, list(proc_id)::in, module_info::in,
			set(request)::in, set(request)::out, proc_table::in,
			proc_table::out) is det. 

traverse_other_procs(_PredId, [], _Module, Requests, Requests, Procs, Procs).
traverse_other_procs(PredId, [ProcId | ProcIds], ModuleInfo, Requests0,
						Requests, Procs0, Procs) :-
	map__init(PredVars0),
	map__init(NewPreds0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	Info0 = info(PredVars0, Requests0, NewPreds0, ModuleInfo),
	traverse_goal(Goal0, Goal1, proc(PredId, ProcId), _, _,
					Info0, info(_, Requests1,_,_)),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_variables(ProcInfo0, Varset0),
	implicitly_quantify_clause_body(HeadVars, Goal1, Varset0, VarTypes0,
						Goal, Varset, VarTypes, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_variables(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs1),
	traverse_other_procs(PredId, ProcIds, ModuleInfo, Requests1,
						Requests, Procs1, Procs).
	

%-------------------------------------------------------------------------------
	% Goal traversal

	% Traverses the goal collecting higher order variables for which 
	% the value is known, and specializing calls and adding
	% specialization requests to the request_info structure. 
	% The first time through the only predicate we can specialize
	% is call/N. The pred_proc_id is that of the current procedure,
	% used to find out which procedures need fixing up later.
:- pred traverse_goal(hlds_goal::in, hlds_goal::out, pred_proc_id::in,
	changed::out, int::out, higher_order_info::in,
	higher_order_info::out) is det.

traverse_goal(conj(Goals0) - Info, conj(Goals) - Info,
			PredProcId, Changed, GoalSize) -->
	traverse_conj(Goals0, Goals, PredProcId, unchanged, Changed,
					0, GoalSize).

traverse_goal(disj(Goals0, SM) - Info, disj(Goals, SM) - Info,
				PredProcId, Changed, GoalSize) -->
	traverse_disj(Goals0, Goals, PredProcId, Changed, GoalSize).

		% a switch is treated as a disjunction
traverse_goal(switch(Var, CanFail, Cases0, SM) - Info,
		switch(Var, CanFail, Cases, SM) - Info,
		PredProcId, Changed, GoalSize) -->
	traverse_cases(Cases0, Cases, PredProcId, Changed, GoalSize).

		% check whether this call could be specialized
traverse_goal(Goal0, Goal, PredProcId, Changed, 1) -->
	{ Goal0 = higher_order_call(_,_,_,_,_) - _ }, 
	maybe_specialize_higher_order_call(Goal0, Goal, PredProcId, Changed).

		% check whether this call could be specialized
traverse_goal(Goal0, Goal, PredProcId, Changed, 1) -->
	{ Goal0 = call(_,_,_,_,_,_) - _ }, 
	maybe_specialize_call(Goal0, Goal, PredProcId, Changed).

		% if-then-elses are handled as disjunctions
traverse_goal(Goal0, Goal, PredProcId, Changed, GoalSize, Info0, Info) :- 
	Goal0 = if_then_else(Vars, Cond0, Then0, Else0, SM) - GoalInfo,
	traverse_goal(Cond0, Cond, PredProcId, Changed1,
						GoalSize1, Info0, Info1),
	traverse_goal(Then0, Then, PredProcId, Changed2,
						GoalSize2, Info1, Info2),
	traverse_goal(Else0, Else, PredProcId, Changed3,
						GoalSize3, Info0, Info3),
	Goal = if_then_else(Vars, Cond, Then, Else, SM) - GoalInfo,
	GoalSize is GoalSize1 + GoalSize2 + GoalSize3,
	update_changed_status(Changed1, Changed2, Changed4),
	update_changed_status(Changed4, Changed3, Changed),
	merge_higher_order_infos(Info2, Info3, Info).

traverse_goal(not(NegGoal0) - Info, not(NegGoal) - Info,
				PredProcId, Changed, GoalSize) -->
	traverse_goal(NegGoal0, NegGoal, PredProcId, Changed, GoalSize).

traverse_goal(some(Vars, Goal0) - Info, some(Vars, Goal) - Info,
				PredProcId, Changed, GoalSize) -->
	traverse_goal(Goal0, Goal, PredProcId, Changed, GoalSize).

traverse_goal(Goal, Goal, _, unchanged, 1) -->
	{ Goal = pragma_c_code(_, _, _, _, _, _, _, _) - _ }.

traverse_goal(Goal, Goal, _, unchanged, 1) -->
	{ Goal = unify(_, _, _, Unify, _) - _ }, 
	check_unify(Unify).


:- pred traverse_conj(hlds_goals::in, hlds_goals::out, pred_proc_id::in,
	changed::in, changed::out, int::in, int::out, higher_order_info::in,
	higher_order_info::out) is det.

traverse_conj([], [], _, Changed, Changed, Size, Size) --> [].
traverse_conj([Goal0 | Goals0], [Goal | Goals],
		PredProcId, Changed0, Changed, GoalSize0, GoalSize) --> 
	traverse_goal(Goal0, Goal, PredProcId, LocalChanged, ThisGoalSize),
	{ GoalSize1 is GoalSize0 + ThisGoalSize },
	{ update_changed_status(Changed0, LocalChanged, Changed1) },
	traverse_conj(Goals0, Goals, PredProcId, Changed1, Changed,
							GoalSize1, GoalSize).

		% to process a disjunction, we process each disjunct with the
		% specialization information before the goal, then merge the
		% results to give the specialization information after the
		% disjunction.
:- pred traverse_disj(hlds_goals::in, hlds_goals::out, pred_proc_id::in,
		changed::out, int::out, higher_order_info::in,
		higher_order_info::out) is det.

traverse_disj([], [], _, unchanged, 0) --> [].
traverse_disj([Goal0 | Goals0], [Goal | Goals], PredProcId,
						Changed, GoalSize) -->
	=(Info0),
	traverse_goal(Goal0, Goal, PredProcId, Changed0, GoalSize0),
	traverse_disj_2(Goals0, Goals, PredProcId,
			Changed0, Changed, GoalSize0, GoalSize, Info0).


:- pred traverse_disj_2(hlds_goals::in, hlds_goals::out, pred_proc_id::in,
	changed::in, changed::out, int::in, int::out, higher_order_info::in,
	higher_order_info::in, higher_order_info::out) is det.

traverse_disj_2([], [], _, Changed, Changed, Size, Size, _, Info, Info).
traverse_disj_2([Goal0 | Goals0], [Goal | Goals], PredProcId, Changed0, Changed,
			GoalSize0, GoalSize, InitialInfo, Info0, Info) :-
	traverse_goal(Goal0, Goal, PredProcId, LocalChanged, ThisGoalSize,
						InitialInfo, ThisGoalInfo),
	update_changed_status(Changed0, LocalChanged, Changed1),
	GoalSize1 is GoalSize0 + ThisGoalSize,
	merge_higher_order_infos(Info0, ThisGoalInfo, Info1),
	traverse_disj_2(Goals0, Goals, PredProcId, Changed1, Changed,
				GoalSize1, GoalSize, InitialInfo, Info1, Info).


				% The dependencies have changed, so the
				% dependency graph needs to rebuilt for
				% inlining to work properly.
				% XXX Fix the graph here instead of 
				% completely rebuilding it later (only
				% a problem in profiling grades).
		% Switches are treated in exactly the same way as disjunctions.
:- pred traverse_cases(list(case)::in, list(case)::out, pred_proc_id::in,
		changed::out, int::out, higher_order_info::in,
		higher_order_info::out) is det.

traverse_cases([], [], _, unchanged, 0) --> [].
traverse_cases([case(ConsId, Goal0) | Cases0], [case(ConsId, Goal) | Cases], 
				PredProcId, Changed, GoalSize) -->
	=(Info0),
	traverse_goal(Goal0, Goal, PredProcId, Changed0, ThisGoalSize),
	traverse_cases_2(Cases0, Cases, PredProcId, Changed0,
					Changed, ThisGoalSize, GoalSize, Info0).

:- pred traverse_cases_2(list(case)::in, list(case)::out, pred_proc_id::in,
	changed::in, changed::out, int::in, int::out, higher_order_info::in,
	higher_order_info::in, higher_order_info::out) is det.

traverse_cases_2([], [], _, Changed, Changed, Size, Size, _, Info, Info).
traverse_cases_2([Case0 | Cases0], [Case | Cases], PredProcId, Changed0,
		Changed, GoalSize0, GoalSize, InitialInfo, Info0, Info) :-
	Case0 = case(ConsId, Goal0),
	traverse_goal(Goal0, Goal, PredProcId, LocalChanged,
				ThisGoalSize, InitialInfo, ThisGoalInfo),
	Case = case(ConsId, Goal),
	update_changed_status(Changed0, LocalChanged, Changed1),
	GoalSize1 is GoalSize0 + ThisGoalSize,
	merge_higher_order_infos(Info0, ThisGoalInfo, Info1),
	traverse_cases_2(Cases0, Cases, PredProcId, Changed1, Changed,
				GoalSize1, GoalSize, InitialInfo, Info1, Info).


	% This is used in traversing disjunctions. We save the initial
	% accumulator, then traverse each disjunct starting with the initial
	% info. We then merge the resulting infos.
:- pred merge_higher_order_infos(higher_order_info::in, higher_order_info::in,
					higher_order_info::out) is det.

merge_higher_order_infos(Info1, Info2, Info) :-
	Info1 = info(PredVars1, Requests1, NewPreds, ModuleInfo),
	Info2 = info(PredVars2, Requests2,_,_),
	merge_pred_vars(PredVars1, PredVars2, PredVars),
	set__union(Requests1, Requests2, Requests12),
	set__to_sorted_list(Requests12, List12),
	set__sorted_list_to_set(List12, Requests),
	Info = info(PredVars, Requests, NewPreds, ModuleInfo).


:- pred merge_pred_vars(pred_vars::in, pred_vars::in, pred_vars::out) is det.

merge_pred_vars(PredVars1, PredVars2, PredVars) :-
	map__to_assoc_list(PredVars1, PredVarList1),
	map__to_assoc_list(PredVars2, PredVarList2),
	merge_pred_var_lists(PredVarList1, PredVarList2, PredVarList),
	map__from_assoc_list(PredVarList, PredVars). 
	
	
		% find out which variables after a disjunction cannot
		% be specialized
:- pred merge_pred_var_lists(assoc_list(var, maybe_pred_and_args)::in,  	
			assoc_list(var, maybe_pred_and_args)::in,
			assoc_list(var, maybe_pred_and_args)::out) is det.

merge_pred_var_lists([], List, List).
merge_pred_var_lists([PredVar | PredVars], List2, MergedList) :-
	merge_pred_var_with_list(PredVar, List2, MergedList1),
	merge_pred_var_lists(PredVars, MergedList1, MergedList).


:- pred merge_pred_var_with_list(pair(var, maybe_pred_and_args)::in,
			assoc_list(var, maybe_pred_and_args)::in,
			assoc_list(var, maybe_pred_and_args)::out) is det.

merge_pred_var_with_list(VarValue, [], [VarValue]).
merge_pred_var_with_list(Var1 - Value1, [Var2 - Value2 | Vars], MergedList) :-
	(
		Var1 = Var2
	->
		(	(
				Value1 \= Value2
			;	Value1 = no
			;	Value2 = no
			)
		->
			MergedList = [Var1 - no | Vars]
		;
			MergedList = [Var2 - Value2 | Vars]
		)
			% each var occurs at most once most in each list
			% so if we have seen it we don't need to go on
	;
		MergedList = [Var2 - Value2 | MergedList1],
		merge_pred_var_with_list(Var1 - Value1, Vars, MergedList1)
	).	
			

:- pred check_unify(unification::in, higher_order_info::in,
				higher_order_info::out) is det.

	% testing two higher order terms for equality is not allowed
check_unify(simple_test(_, _)) --> [].

check_unify(assign(Var1, Var2)) -->
	maybe_add_alias(Var1, Var2).

	% deconstructing a higher order term is not allowed
check_unify(deconstruct(_, _, _, _, _)) --> [].
	
check_unify(construct(LVar, ConsId, Args, _Modes), Info0, Info) :- 
	Info0 = info(PredVars0, Requests, NewPreds, ModuleInfo),
	(
		ConsId = pred_const(PredId, ProcId)
	->
		(
			map__search(PredVars0, LVar, Specializable)
		->
			(
				% we can't specialize calls involving
				% a variable with more than one
				% possible value
				Specializable = yes(_, _, _),
				map__det_update(PredVars0, LVar, no, PredVars)
			;
				% if a variable is already
				% non-specializable, it can't become
				% specializable
				Specializable = no,
				PredVars = PredVars0
			)
		;
			map__det_insert(PredVars0, LVar,
				yes(PredId, ProcId, Args), PredVars)
		)
	;
		PredVars = PredVars0	
	),
	Info = info(PredVars, Requests, NewPreds, ModuleInfo).
	
check_unify(complicated_unify(_, _)) -->
	{ error("higher_order:check_unify - complicated unification") }.

		% Process a higher-order call to see if it could possibly
		% be specialized.
:- pred maybe_specialize_higher_order_call( hlds_goal::in, hlds_goal::out,
		pred_proc_id::in, changed::out, higher_order_info::in,
		higher_order_info::out) is det.

maybe_specialize_higher_order_call(Goal0 - GoalInfo, Goal - GoalInfo,
		PredProcId, Changed, Info0, Info) :-
	Info0 = info(PredVars, Requests0, NewPreds, Module),
	( Goal0 = higher_order_call(PredVar0, Args0, _Types, _Modes, _Det) ->
		PredVar = PredVar0,
		Args = Args0
	;
		error("higher_order.m: higher_order_call expected")
	),
		
	% We can trivially specialize calls to call/N.
	(
		map__search(PredVars, PredVar,
			yes(PredId, ProcId, CurriedArgs))
	->
		module_info_pred_info(Module, PredId, PredInfo),
		pred_info_module(PredInfo, ModuleName),
		pred_info_name(PredInfo, PredName),
		code_util__builtin_state(Module, PredId, ProcId, Builtin),
		list__append(CurriedArgs, Args, AllArgs),
		MaybeContext = no,
		Goal1 = call(PredId, ProcId, AllArgs,
			Builtin, MaybeContext,
			qualified(ModuleName, PredName)),
		maybe_specialize_call(Goal1 - GoalInfo,
			Goal - _, PredProcId, _, Info0,
			info(_, Requests, _, _)),
		Changed = changed 
	;
		% non-specializable call to call/N
		Goal = Goal0,
		Changed = unchanged,
		Requests = Requests0
	),
	Info = info(PredVars, Requests, NewPreds, Module).

		% Process a call to see if it could possibly be specialized.
:- pred maybe_specialize_call( hlds_goal::in, hlds_goal::out,
		pred_proc_id::in, changed::out, higher_order_info::in,
		higher_order_info::out) is det.

maybe_specialize_call(Goal0 - GoalInfo, Goal - GoalInfo, PredProcId,
		Changed, Info0, Info) :-
	Info0 = info(PredVars, Requests0, NewPreds, Module),
	(
		Goal0 = call(_, _, _, _, _, _)
	->
		Goal0 = call(CalledPred, CalledProc, Args0, IsBuiltin,
					MaybeContext, _SymName0)
	;
		error("higher_order.m: call expected")
	),
	module_info_pred_info(Module, CalledPred, PredInfo),
	(
		pred_info_is_imported(PredInfo)
	->
		Requests = Requests0,
		Changed = unchanged,
		Goal = Goal0
	;	
		pred_info_arg_types(PredInfo, _, ArgTypes),
		find_higher_order_args(Module, Args0, ArgTypes, PredVars, 1,
				[], HigherOrderArgs, Args0, Args1),
		(
			HigherOrderArgs = [] 
		->
			Requests = Requests0,
			Changed = unchanged,
			Goal = Goal0
		;
			% Check to see if any of the specialized
			% versions of the called pred apply here.
			map__search(NewPreds,
				proc(CalledPred, CalledProc),
				NewPredSet),
			set__to_sorted_list(NewPredSet, NewPredList),	% NOP
			list__filter(lambda([X::in] is semidet, (
				X = new_pred(_,_,_, HigherOrderArgs)
				)), NewPredList, Matches),
			(
				Matches = [Match],
				Match = new_pred(NewCalledPred,
						NewCalledProc, NewName,
						_HOArgs)
			;
				Matches = [_,_|_],
				error("multiple specializations")
			)
		->
			remove_listof_higher_order_args(Args1, 1,
					HigherOrderArgs, Args2),
			Goal = call(NewCalledPred, NewCalledProc,
				Args2, IsBuiltin, MaybeContext, NewName),
			Changed = changed,
			Requests = Requests0
		;
			% There is a known higher order variable in the
			% call, so we put in a request for a specialized
			% version of the pred.
			Goal = Goal0,
			Request = request(PredProcId,
					proc(CalledPred, CalledProc),
					HigherOrderArgs), 
			set__insert(Requests0, Request, Requests),
			Changed = request
		)
	),
	Info = info(PredVars, Requests, NewPreds, Module).

	% Returns a list of the higher-order arguments in a call that have
	% a known value. Also update the argument list to now include
	% curried arguments that need to be explicitly passed.
	% The order of the argument list must match that generated
	% by construct_higher_order terms.
:- pred find_higher_order_args(module_info::in, list(var)::in, list(type)::in,
		pred_vars::in, int::in, list(higher_order_arg)::in,
		list(higher_order_arg)::out,
		list(var)::in, list(var)::out) is det.

find_higher_order_args(_, [], _, _, _, HOArgs, HOArgs, NewArgs, NewArgs).
find_higher_order_args(_, [_|_], [], _, _, _, _, _, _) :-
	error("find_higher_order_args: length mismatch").
find_higher_order_args(ModuleInfo, [Arg | Args], [ArgType | ArgTypes],
		PredVars, ArgNo, HOArgs0, HOArgs, NewArgs0, NewArgs) :-
	NextArg is ArgNo + 1,
	(
		% We don't specialize arguments whose declared type is
		% polymorphic. The closure they pass cannot possibly
		% be called within the called predicate, since that predicate 
		% doesn't know it's a closure (without some dodgy use of
		% type_to_univ and univ_to_type).
		type_is_higher_order(ArgType, _, _),
		map__search(PredVars, Arg, yes(PredId, ProcId, CurriedArgs))
	->
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_arg_types(PredInfo, _, CurriedArgTypes),
		find_higher_order_args(ModuleInfo, CurriedArgs,
			CurriedArgTypes, PredVars, 1, [], HOCurriedArgs,
			CurriedArgs, NewExtraArgs0),
		list__length(CurriedArgs, NumArgs),
		remove_listof_higher_order_args(NewExtraArgs0, 1, HOCurriedArgs,
								NewExtraArgs),
		HOArgs1 = [higher_order_arg(PredId, ProcId, ArgNo,
					 NumArgs, HOCurriedArgs) | HOArgs0],
		list__append(NewArgs0, NewExtraArgs, NewArgs1)
	;
		HOArgs1 = HOArgs0,
		NewArgs1 = NewArgs0
	),
	find_higher_order_args(ModuleInfo, Args, ArgTypes, PredVars, NextArg,
			HOArgs1, HOArgs, NewArgs1, NewArgs).
		
		% if the right argument of an assignment is a higher order
		% term with a known value, we need to add an entry for
		% the left argument
:- pred maybe_add_alias(var::in, var::in, higher_order_info::in,
				higher_order_info::out) is det.

maybe_add_alias(LVar, RVar,
		info(PredVars0, Requests, NewPreds, ModuleInfo),
		info(PredVars, Requests, NewPreds, ModuleInfo)) :-
	(
		map__search(PredVars0, RVar, yes(A, B, C))
	->
		map__set(PredVars0, LVar, yes(A, B, C), PredVars)
	;
		PredVars = PredVars0
	).
		

:- pred update_changed_status(changed::in, changed::in, changed::out) is det.

update_changed_status(changed, _, changed).
update_changed_status(request, changed, changed).
update_changed_status(request, request, request).
update_changed_status(request, unchanged, request).
update_changed_status(unchanged, Changed, Changed).

%-------------------------------------------------------------------------------
% Predicates to process requests for specialization, and create any  
% new predicates that are required.	

		% Filter out requests for higher-order specialization 
		% for preds which are too large. Maybe we could allow
		% programmers to declare which predicates they want
		% specialized, as with inlining?
		% Nonlocal predicates are filtered out here, since they
		% will not have an entry in the goal_sizes.
		% Don't create specialized versions of specialized
		% versions, since for some fairly contrived examples 
		% involving recursively building up lambda expressions
		% this can create ridiculous numbers of versions.
:- pred filter_requests(module_info::in, set(request)::in, goal_sizes::in,
						list(request)::out) is det.

filter_requests(ModuleInfo, Requests0, GoalSizes, Requests) :-
	set__to_sorted_list(Requests0, Requests1),
	list__filter(lambda([X::in] is semidet, (
			X = request(_, CalledPredProcId, _),
			CalledPredProcId = proc(CalledPredId, _),
			map__search(GoalSizes, CalledPredId, GoalSize),
			max_specialized_goal_size(MaxSize),
			GoalSize =< MaxSize,
			predicate_name(ModuleInfo, CalledPredId, PredName),
			\+ (
				% There are probably cleaner ways to check 
				% if this is a specialised version.
				string__sub_string_search(PredName, 
					"__ho", Index),
				NumIndex is Index + 4,
				string__index(PredName, NumIndex, Digit),
				char__is_digit(Digit)
			)
		)),
		Requests1, Requests).

:- pred create_new_preds(list(request)::in, new_preds::in, new_preds::out,
		set(pred_proc_id)::in, set(pred_proc_id)::out, int::in,
		int::out, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

create_new_preds([], NewPreds, NewPreds, ToFix, ToFix, NextId, NextId,
							Mod, Mod, IO, IO). 
create_new_preds([Request | Requests], NewPreds0, NewPreds, PredsToFix0,
		PredsToFix, NextHOid0, NextHOid, Module0, Module, IO0, IO)  :-
	Request = request(CallingPredProcId, CalledPredProcId, HOArgs),
	set__insert(PredsToFix0, CallingPredProcId, PredsToFix1),
	(
		map__search(NewPreds0, CalledPredProcId, SpecVersions0)
	->
		(
			% check that we aren't redoing the same pred
			% SpecVersions are pred_proc_ids of the specialized
			% versions of the current pred.
			\+ (
				set__member(X, SpecVersions0),
				X = new_pred(_,_,_, DoneHOArgs),
				DoneHOArgs = HOArgs
			)
		->
			create_new_pred(Request, NewPred, NextHOid0,
				NextHOid1, Module0, Module1, IO0, IO1), 
			set__insert(SpecVersions0, NewPred, SpecVersions),
			map__det_update(NewPreds0, CalledPredProcId,
						SpecVersions, NewPreds1)
		;
			Module1 = Module0,
			NewPreds1 = NewPreds0,
			IO1 = IO0,
			NextHOid1 = NextHOid0
		)
	;
		create_new_pred(Request, NewPred, NextHOid0, NextHOid1,
						Module0, Module1, IO0, IO1),
		set__singleton_set(SpecVersions0, NewPred),
		map__det_insert(NewPreds0, CalledPredProcId, SpecVersions0,
			NewPreds1)
	),
	create_new_preds(Requests, NewPreds1, NewPreds, PredsToFix1, PredsToFix,
			NextHOid1, NextHOid, Module1, Module, IO1, IO).


		% Here we create the pred_info for the new predicate.
:- pred create_new_pred(request::in, new_pred::out, int::in, int::out,
	module_info::in, module_info::out, io__state::di, io__state::uo) is det.

create_new_pred(request(_CallingPredProc, CalledPredProc, HOArgs),
		new_pred(NewPredId, NewProcId, Name, HOArgs), NextHOid0,
		NextHOid, ModuleInfo0, ModuleInfo, IOState0, IOState) :- 
	CalledPredProc = proc(CalledPred, _),
	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_get_preds(PredTable0, Preds0),
	map__lookup(Preds0, CalledPred, PredInfo0),
	pred_info_name(PredInfo0, Name0),
	pred_info_arity(PredInfo0, Arity),
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	pred_info_module(PredInfo0, PredModule),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose,
							IOState0, IOState1),
        pred_info_arg_types(PredInfo0, Tvars, Types0),
	string__int_to_string(Arity, ArStr),
	(
 		VeryVerbose = yes
	->
		io__write_strings(["% Specializing calls to `", PredModule, ":",
			Name0, "'/", ArStr, " with higher-order arguments:\n"],
			IOState1, IOState2),
		list__length(Types0, ActualArity),
		NumToDrop is ActualArity - Arity,
		output_higher_order_args(ModuleInfo0, NumToDrop,
					HOArgs, IOState2, IOState)
	;
       		IOState = IOState1
       	),
	string__int_to_string(NextHOid0, IdStr),
	NextHOid is NextHOid0 + 1,
	string__append_list([Name0, "__ho", IdStr], PredName),
	pred_info_typevarset(PredInfo0, TypeVars),
	remove_listof_higher_order_args(Types0, 1, HOArgs, Types),
	pred_info_context(PredInfo0, Context),
	pred_info_get_marker_list(PredInfo0, MarkerList),
	pred_info_get_goal_type(PredInfo0, GoalType),
	Name = qualified(PredModule, PredName),
	varset__init(EmptyVarSet),
	map__init(EmptyVarTypes),
	
	% This isn't looked at after here, and just clutters up
	% hlds dumps if it's filled in.
	ClausesInfo = clauses_info(EmptyVarSet, EmptyVarTypes,
		EmptyVarTypes, [], []),
	pred_info_init(PredModule, Name, Arity, Tvars,
		Types, true, Context, ClausesInfo, local, MarkerList, GoalType,
		PredOrFunc, PredInfo1),
	pred_info_set_typevarset(PredInfo1, TypeVars, PredInfo2),
	pred_info_procedures(PredInfo2, Procs0),
	next_mode_id(Procs0, no, NewProcId),
	predicate_table_insert(PredTable0, PredInfo2, NewPredId, PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable, ModuleInfo).
	

:- pred output_higher_order_args(module_info::in, int::in,
	list(higher_order_arg)::in, io__state::di, io__state::uo) is det.

output_higher_order_args(_, _, []) --> [].
output_higher_order_args(ModuleInfo, NumToDrop, [HOArg | HOArgs]) -->
	{ HOArg = higher_order_arg(PredId, _ProcId, ArgNo, NumArgs, _) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
		% adjust message for type_infos
	{ DeclaredArgNo is ArgNo - NumToDrop },
	io__write_string("\tHeadVar__"),
	io__write_int(DeclaredArgNo),
	io__write_string(" = `"),
	io__write_string(Name),
	io__write_string("'/"),
	io__write_int(Arity),
	io__write_string(" with "),
	io__write_int(NumArgs),
	io__write_string(" curried arguments\n"),
	output_higher_order_args(ModuleInfo, NumToDrop, HOArgs).
	

:- pred remove_listof_higher_order_args(list(T)::in, int::in,
			list(higher_order_arg)::in, list(T)::out) is det.

remove_listof_higher_order_args(List0, ArgNo, ArgsToRemove, List) :-
        (
                ArgsToRemove = []
        ->
                List = List0
        ;
                (
                        List0 = [Head | Tail],
                        NextArg is ArgNo + 1,
                        (
				list__member(HOArg, ArgsToRemove),
				HOArg = higher_order_arg(_, _, ArgNo, _, _)
                        ->
                                List = List1
                        ;
                                List = [Head | List1]
                        ),
                        remove_listof_higher_order_args(Tail, NextArg,
							ArgsToRemove, List1)
                ;
                        List0 = [],
                        List = List0
                )
        ).


	% Fixup calls to specialized predicates.
:- pred fixup_preds(list(pred_proc_id)::in, new_preds::in,
				module_info::in, module_info::out) is det.

fixup_preds([], _, ModuleInfo, ModuleInfo).
fixup_preds([PredProcId | PredProcIds], NewPreds, ModuleInfo0, ModuleInfo) :-
	PredProcId = proc(PredId, ProcId), 
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, Procs0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	map__init(PredVars0),
	set__init(Requests0),
	traverse_goal(Goal0, Goal1, PredProcId, _, _,
		info(PredVars0, Requests0, NewPreds, ModuleInfo0), _),
	proc_info_variables(ProcInfo0, Varset0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	implicitly_quantify_clause_body(HeadVars, Goal1, Varset0, VarTypes0,
					Goal, Varset, VarTypes, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_variables(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1),
	fixup_preds(PredProcIds, NewPreds, ModuleInfo1, ModuleInfo).


:- pred create_specialized_versions(list(pred_proc_id)::in, new_preds::in, 
		set(request)::in, set(request)::out, goal_sizes::in,
		goal_sizes::out, module_info::in, module_info::out) is det.

create_specialized_versions([], _, Requests, Requests, Sizes, Sizes, Mod, Mod).
create_specialized_versions([PredProc | PredProcs], NewPreds, Requests0,
		Requests, GoalSizes0, GoalSizes, ModuleInfo0, ModuleInfo) :-
	map__lookup(NewPreds, PredProc, SpecVersions0),
	set__to_sorted_list(SpecVersions0, SpecVersions),
	PredProc = proc(PredId, ProcId),
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, _, ProcInfo),
	create_specialized_versions_2(SpecVersions, NewPreds, ProcInfo, 
			Requests0, Requests1, GoalSizes0, GoalSizes1,
			ModuleInfo0, ModuleInfo1),
	create_specialized_versions(PredProcs, NewPreds, Requests1, Requests, 
			GoalSizes1, GoalSizes, ModuleInfo1, ModuleInfo).


	% Create specialized versions of a single procedure.
:- pred create_specialized_versions_2(list(new_pred)::in, new_preds::in, 
		proc_info::in, set(request)::in, set(request)::out,  
		goal_sizes::in, goal_sizes::out, module_info::in,
		module_info::out) is det.

create_specialized_versions_2([], _, _, Requests, Requests, Sizes, Sizes,
					ModuleInfo, ModuleInfo).
create_specialized_versions_2([NewPred | NewPreds], NewPredMap, NewProcInfo0,
	Requests0, Requests, GoalSizes0, GoalSizes, ModuleInfo0, ModuleInfo)
		:-
	NewPred = new_pred(NewPredId, NewProcId, _Name, HOArgs),
	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_get_preds(PredTable0, Preds0),
	map__lookup(Preds0, NewPredId, NewPredInfo0),
	pred_info_procedures(NewPredInfo0, NewProcs0),
	map__init(Substitution0),
	proc_info_headvars(NewProcInfo0, HeadVars0),
	proc_info_argmodes(NewProcInfo0, ArgModes0),
	construct_higher_order_terms(ModuleInfo0, HeadVars0, HeadVars1,
		ArgModes0, ArgModes1, HOArgs, NewProcInfo0, NewProcInfo1,
		NewPredInfo0, NewPredInfo1, Substitution0,
		Substitution, Constructions),
	proc_info_goal(NewProcInfo1, Goal0),
	Goal0 = GoalExpr0 - Info0,
		% put in constructions to bind the headvars to
		% their specialized values
	(
		GoalExpr0 = conj(Goals0)
	->
		list__append(Constructions, Goals0, Goals)
	;
		list__append(Constructions, [Goal0], Goals)
	),
	Goal1 = conj(Goals) - Info0,
	remove_listof_higher_order_args(HeadVars1, 1, HOArgs, HeadVars),
	remove_listof_higher_order_args(ArgModes1, 1, HOArgs, ArgModes),

	% specialize types
	proc_info_vartypes(NewProcInfo1, VarTypes0),
	apply_substitution_to_type_map(VarTypes0, Substitution, VarTypes1),
	map__apply_to_list(HeadVars, VarTypes1, ArgTypes0),
	term__vars_list(ArgTypes0, TypeVars),
	varset__init(VarSet0),
	map__init(DummyVarTypes), % type vars don't have a type
	map__init(Renaming0),
	goal_util__create_variables(TypeVars, VarSet0, DummyVarTypes,
		Renaming0, DummyVarTypes, VarSet0, ArgTVarset, _, Renaming),
	term__apply_variable_renaming_to_list(ArgTypes0, Renaming, ArgTypes),
	pred_info_set_arg_types(NewPredInfo1, ArgTVarset, ArgTypes,
							 NewPredInfo2),
	map__init(PredVars0),
        traverse_goal(Goal1, Goal2, proc(NewPredId, NewProcId), _, GoalSize,
			info(PredVars0, Requests0, NewPredMap, ModuleInfo0),
			info(_, Requests1,_,_)),
	map__set(GoalSizes0, NewPredId, GoalSize, GoalSizes1),
	proc_info_variables(NewProcInfo1, Varset0),
					
	implicitly_quantify_clause_body(HeadVars, Goal2, Varset0, VarTypes1,
					Goal3, Varset, VarTypes, _),
	proc_info_get_initial_instmap(NewProcInfo1, ModuleInfo0, InstMap0),
	recompute_instmap_delta(no, Goal3, Goal4, InstMap0,
		ModuleInfo0, ModuleInfo1),
	proc_info_set_goal(NewProcInfo1, Goal4, NewProcInfo1a),
	proc_info_set_variables(NewProcInfo1a, Varset, NewProcInfo2),
	proc_info_set_vartypes(NewProcInfo2, VarTypes, NewProcInfo3),
	proc_info_set_argmodes(NewProcInfo3, ArgModes, NewProcInfo4),
	proc_info_set_headvars(NewProcInfo4, HeadVars, NewProcInfo),
	map__det_insert(NewProcs0, NewProcId, NewProcInfo, NewProcs),
	pred_info_set_procedures(NewPredInfo2, NewProcs, NewPredInfo),
	map__det_update(Preds0, NewPredId, NewPredInfo, Preds),
	predicate_table_set_preds(PredTable0, Preds, PredTable),
	module_info_set_predicate_table(ModuleInfo1, PredTable, ModuleInfo2),
	create_specialized_versions_2(NewPreds, NewPredMap, NewProcInfo0,
			Requests1, Requests, GoalSizes1, GoalSizes,
			ModuleInfo2, ModuleInfo).

	
		% Returns a list of hlds_goals which construct the list of
		% higher order arguments which have been specialized. Traverse
		% goal will then recognize these as having a unique possible
		% value and will specialize any calls involving them.
		% Substitution* is a substitution for all variables in the
		% specialized predicates. The caller should apply this
		% to the type map for this procedure.
		% This predicate fixes the tvarset in the pred_info
		% containing the type vars in the goal. 
		% The caller should fix the argument types and the
		% corresponding tvarset.
		% Takes an original list of headvars and arg_modes and
		% returns these with curried arguments added. The 
		% caller should remove the higher-order arguments from
		% the argument list.
		% The predicate is recursively applied to all curried
		% higher order arguments of higher order arguments.
:- pred construct_higher_order_terms(module_info::in, list(var)::in, 
		list(var)::out, list(mode)::in, list(mode)::out,
		list(higher_order_arg)::in, proc_info::in, proc_info::out,
		pred_info::in, pred_info::out, tsubst::in,
		tsubst::out, list(hlds_goal)::out) is det.

construct_higher_order_terms(_, HeadVars, HeadVars, ArgModes, ArgModes,
	[], ProcInfo, ProcInfo, PredInfo, PredInfo, Subst, Subst, []).
construct_higher_order_terms(ModuleInfo, HeadVars0, HeadVars, ArgModes0,
		ArgModes, [HOArg | HOArgs], ProcInfo0, ProcInfo, PredInfo0,
		PredInfo, Substitution0, Substitution, Goals) :-
	HOArg = higher_order_arg(PredId, ProcId, Index, NumArgs, CurriedHOArgs),
	list__index1_det(HeadVars0, Index, LVar),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
					CalledPredInfo, CalledProcInfo),
	pred_info_arg_types(CalledPredInfo, CalledTVarset, CalledArgTypes0),
					
	% Add the curried arguments to the procedure's argument list.
	proc_info_argmodes(CalledProcInfo, CalledArgModes),
	(
		list__split_list(NumArgs, CalledArgModes,
				CurriedArgModes0a, UnCurriedArgModes0)
	->
		CurriedArgModes0 = CurriedArgModes0a,
		UnCurriedArgModes = UnCurriedArgModes0
	;
		error("list__split_list_failed")
	),
	proc_info_variables(ProcInfo0, Varset0),
	varset__new_vars(Varset0, NumArgs, NewHeadVars0, Varset1),
	proc_info_set_variables(ProcInfo0, Varset1, ProcInfo1),

	% Find the type substitution and work out the types
	% of the new variables.
	proc_info_vartypes(ProcInfo1, VarTypes0),
	map__lookup(VarTypes0, LVar, LVarType),
	pred_info_typevarset(PredInfo0, TypeVarset0),
	varset__merge(TypeVarset0, CalledTVarset, CalledArgTypes0,
						TypeVarset1, CalledArgTypes),
	pred_info_set_typevarset(PredInfo0, TypeVarset1, PredInfo1),
	(
		list__split_list(NumArgs, CalledArgTypes,
				CurriedArgTypes0, UnCurriedArgTypes0)
	->
		CurriedArgTypes = CurriedArgTypes0,
		UnCurriedArgTypes = UnCurriedArgTypes0
	;
		error("list__split failed")
	),
	(
		type_is_higher_order(LVarType, _PredOrFunc, LVarArgTypes)
	->
		(
			type_list_subsumes(LVarArgTypes, UnCurriedArgTypes,
							NewSubstitution)
		->
			% Add the substitution found for this higher-order
			% term to the substitution to be applied to the type
			% map.
			map__overlay(Substitution0, NewSubstitution,
							Substitution1)
		;
			Substitution1 = Substitution0
		)
	;
		error("specialized argument not of higher-order type")
	),
	map__det_insert_from_corresponding_lists(VarTypes0, NewHeadVars0,
					CurriedArgTypes, VarTypes1),
	proc_info_set_vartypes(ProcInfo1, VarTypes1, ProcInfo2),

	% Recursively construct the curried higher-order arguments.
	construct_higher_order_terms(ModuleInfo, NewHeadVars0, NewHeadVars1,
		CurriedArgModes0, CurriedArgModes1, CurriedHOArgs,
		ProcInfo2, ProcInfo3, PredInfo1, PredInfo2, Substitution1,
		Substitution2, CurriedGoals),

	% Fix up the argument lists.
	remove_listof_higher_order_args(CurriedArgModes1, 1,
					CurriedHOArgs, CurriedArgModes),
	remove_listof_higher_order_args(NewHeadVars1, 1,
					 CurriedHOArgs, NewHeadVars),
	list__append(ArgModes0, CurriedArgModes, ArgModes1),
	list__append(HeadVars0, NewHeadVars, HeadVars1),

	% Build the higher-order constant.
	pred_info_module(CalledPredInfo, Module),
	pred_info_name(CalledPredInfo, Name),
	Rhs = functor(cons(qualified(Module, Name), NumArgs), NewHeadVars0),
	Context = unify_context(head(Index), []),
	mode_util__modes_to_uni_modes(CurriedArgModes1, CurriedArgModes1,
					ModuleInfo, UniModes),
	Unify = construct(LVar, pred_const(PredId, ProcId),
					NewHeadVars0, UniModes),
	proc_info_inferred_determinism(ProcInfo3, Detism),
	pred_info_get_is_pred_or_func(PredInfo2, PredOrFunc),
	PredInstInfo = pred_inst_info(PredOrFunc, UnCurriedArgModes, Detism),
	Inst = ground(shared, yes(PredInstInfo)),
	Unimode = (free -> Inst) - (Inst -> Inst),
	Goal = unify(LVar, Rhs, Unimode, Unify, Context),

	goal_info_init(Info0),
	goal_info_set_determinism(Info0, Detism, Info1),
	instmap_delta_init_reachable(InstmapDelta0),
	instmap_delta_insert(InstmapDelta0, LVar, Inst, InstmapDelta),
	goal_info_set_instmap_delta(Info1, InstmapDelta, Info),
	construct_higher_order_terms(ModuleInfo, HeadVars1, HeadVars, ArgModes1,
		ArgModes, HOArgs, ProcInfo3, ProcInfo, PredInfo2, PredInfo,
		Substitution2, Substitution, Goals1),
	list__condense([CurriedGoals, [Goal - Info], Goals1], Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
