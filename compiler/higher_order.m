%-----------------------------------------------------------------------------
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module higher_order.
% Main author: stayl
%
% Specializes calls to higher order or polymorphic predicates where the value
% of one or more higher order, type_info or typeclass_info arguments are known.
%
% Since this creates a new copy of the called procedure I have limited the
% specialization to cases where the called procedure's goal contains less than
% 20 calls and unifications. For predicates above this size the overhead of
% the higher order call becomes less significant while the increase in code
% size becomes significant. The limit can be changed using
% `--higher-order-size-limit'.
%
% If a specialization creates new opportunities for specialization, the
% specialization process will be iterated until no further opportunities arise.
% The specialized version for predicate 'foo' is named 'foo__ho<n>', where n
% is a number that uniquely identifies this specialized version.
%-------------------------------------------------------------------------------

:- interface.

:- import_module hlds_module.
:- import_module bool, io.

	% specialize_higher_order(DoHigherOrder, DoTypeInfos, Module0, Module).
	% DoHigherOrder is the value of `--optimize-higher-order'.
	% DoTypeInfos is the value of `--type-specialization'
:- pred specialize_higher_order(bool::in, bool::in,
		module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-------------------------------------------------------------------------------

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, instmap, (inst).
:- import_module code_util, globals, make_hlds, mode_util, goal_util.
:- import_module type_util, options, prog_data, prog_out, quantification.
:- import_module mercury_to_mercury, inlining, polymorphism, prog_util.
:- import_module special_pred.

:- import_module assoc_list, char, int, list, map, require, set.
:- import_module std_util, string, varset, term.

	% Iterate collecting requests and processing them until there
	% are no more requests remaining.
specialize_higher_order(DoHigherOrder, DoTypeInfos,
		ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_int_option(higher_order_size_limit, SizeLimit),
	{ Params = ho_params(DoHigherOrder, DoTypeInfos, SizeLimit) },
	{ get_specialization_requests(Params, Requests, GoalSizes,
		ModuleInfo0, ModuleInfo1) },
	{ map__init(NewPredMap) },
	{ map__init(PredVarMap) },
	{ NewPreds0 = new_preds(NewPredMap, PredVarMap) },
	process_requests(Params, Requests, GoalSizes, 1, _NextHOid,
		NewPreds0, _NewPreds, ModuleInfo1, ModuleInfo).

:- pred process_requests(ho_params::in, set(request)::in, goal_sizes::in,
	int::in, int::out, new_preds::in, new_preds::out, module_info::in,
	module_info::out, io__state::di, io__state::uo) is det.

process_requests(Params, Requests0, GoalSizes0, NextHOid0, NextHOid,
			NewPreds0, NewPreds, ModuleInfo1, ModuleInfo) -->
	{ filter_requests(Params, ModuleInfo1,
		Requests0, GoalSizes0, Requests) },
	(
		{ Requests = [] }
	->
		{ ModuleInfo = ModuleInfo1 },
		{ NextHOid = NextHOid0 },
		{ NewPreds = NewPreds0 }
	;
		{ set__init(PredProcsToFix0) },
		create_new_preds(Requests, NewPreds0, NewPreds1,
			[], NewPredList, PredProcsToFix0, PredProcsToFix,
			NextHOid0, NextHOid1, ModuleInfo1, ModuleInfo2),
		{ set__to_sorted_list(PredProcsToFix, PredProcs) },
		{ set__init(NewRequests0) },
		{ create_specialized_versions(Params, NewPredList,
			NewPreds1, NewPreds2, NewRequests0, NewRequests,
			GoalSizes0, GoalSizes, ModuleInfo2, ModuleInfo3) },

		{ fixup_preds(Params, PredProcs, NewPreds2,
			ModuleInfo3, ModuleInfo4) },
		{ NewPredList \= [] ->
			% The dependencies have changed, so the
			% dependency graph needs to rebuilt for
			% inlining to work properly.
			module_info_clobber_dependency_info(ModuleInfo4,
				ModuleInfo5)
		;
			ModuleInfo5 = ModuleInfo4
		},
		process_requests(Params, NewRequests, GoalSizes, NextHOid1,
			NextHOid, NewPreds2, NewPreds, ModuleInfo5, ModuleInfo)
	).

%-------------------------------------------------------------------------------

:- type request
	---> request(
		pred_proc_id,			% calling pred
		pred_proc_id,			% called pred 
		list(var),			% call args
		list(var),			% call extra typeinfo vars
		list(higher_order_arg),
		list(type),			% argument types in caller
		list(type),			% Extra typeinfo argument
						% types required by
						% --typeinfo-liveness.
		tvarset				% caller's typevarset.
	).

		% Stores cons_id, index in argument vector, number of 
		% curried arguments of a higher order argument, higher-order
		% curried arguments with known values.
		% For cons_ids other than pred_const and `type_info',
		% the arguments must be constants
:- type higher_order_arg
	---> higher_order_arg(
		cons_id,
	 	int,			% index in argument vector
		int,			% number of curried args
		list(var),		% curried arguments in caller
		list(type),		% curried argument types in caller
		list(higher_order_arg)	% higher-order curried arguments
					% with known values
	).

:- type goal_sizes == map(pred_id, int). 	%stores the size of each
				% predicate's goal used in the heuristic
				% to decide which preds are specialized
		
	% Used to hold the value of known higher order variables.
	% If a variable is not in the map, it does not have a value yet.
:- type pred_vars == map(var, maybe_const). 

	% The list of vars is a list of the curried arguments, which must
	% be explicitly passed to the specialized predicate.
	% For cons_ids other than pred_const and `type_info', the arguments
	% must be constants. For pred_consts and type_infos, non-constant
	% arguments are passed through to any specialised version.
:- type maybe_const --->
		constant(cons_id, list(var))	% unique possible value
	;	multiple_values			% multiple possible values,
						% cannot specialise.
	.

	% used while traversing goals
:- type higher_order_info 
	---> info(
		pred_vars,	% higher_order variables
		set(request),	% requested versions
		new_preds,	% versions created in
				% previous iterations
				% not changed by traverse_goal
		pred_proc_id,	% pred_proc_id of goal being traversed
		pred_info,	% not changed by traverse_goal
		proc_info,	% not changed by traverse_goal
		module_info,	% not changed by traverse_goal
		ho_params,
		changed
	).

:- type ho_params
	---> ho_params(
		bool,		% propagate higher-order constants.
		bool,		% propagate type-info constants.
		int		% size limit on requested version.
	).

:- type new_preds
	---> new_preds(
		map(pred_proc_id, set(new_pred)),
				% versions for each predicate
		map(pred_proc_id, pred_vars)
				% higher-order or constant input variables
				% for a specialised version.
	).

:- type new_pred
	---> new_pred(
		pred_proc_id,		% version pred_proc_id
		pred_proc_id,		% old pred_proc_id
		pred_proc_id,		% requesting caller
		sym_name,		% name 
		list(higher_order_arg),	% specialized args
		list(var),		% unspecialised argument vars in caller
		list(var),		% extra typeinfo vars in caller
		list(type),		% unspecialised argument types
					% in requesting caller
		list(type),		% extra typeinfo argument
					% types in requesting caller
		tvarset			% caller's typevarset
	).

	% Returned by traverse_goal. 
:- type changed
	--->	changed		% Need to requantify goal + check other procs
	;	request		% Need to check other procs
	;	unchanged.	% Do nothing more for this predicate

%-------------------------------------------------------------------------------
:- pred get_specialization_requests(ho_params::in, set(request)::out,
	goal_sizes::out, module_info::in, module_info::out) is det.

get_specialization_requests(Params, Requests, GoalSizes,
		ModuleInfo0, ModuleInfo) :-
	module_info_predids(ModuleInfo0, PredIds),
	map__init(GoalSizes0),
	set__init(Requests0),
	get_specialization_requests_2(Params, PredIds, Requests0, Requests,
			GoalSizes0, GoalSizes, ModuleInfo0, ModuleInfo).

:- pred get_specialization_requests_2(ho_params::in, list(pred_id)::in,
	set(request)::in, set(request)::out, goal_sizes::in, goal_sizes::out, 
	module_info::in, module_info::out) is det.

get_specialization_requests_2(_Params, [], Requests, Requests, Sizes, Sizes, 
					ModuleInfo, ModuleInfo).
get_specialization_requests_2(Params, [PredId | PredIds], Requests0, Requests,
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
		map__init(NewPredMap),
		map__init(PredVarMap),
		NewPreds0 = new_preds(NewPredMap, PredVarMap),
		PredProcId = proc(PredId, ProcId),
		Info0 = info(PredVars0, Requests0, NewPreds0, PredProcId,
			PredInfo0, ProcInfo0, ModuleInfo0, Params, unchanged),
		traverse_goal_0(Goal0, Goal1, Info0,
			info(_, Requests1,_,_,_,_,_,_, Changed)),
		goal_size(Goal1, GoalSize),
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
			traverse_other_procs(Params, PredId, ProcIds,
				ModuleInfo0, PredInfo0, Requests1, Requests2,
				Procs1, Procs),
			pred_info_set_procedures(PredInfo0, Procs, PredInfo),
			map__det_update(Preds0, PredId, PredInfo, Preds),
			module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1)
		;
			ModuleInfo1 = ModuleInfo0,
			Requests2 = Requests1
		)
	),
	get_specialization_requests_2(Params, PredIds, Requests2, Requests,
			GoalSizes1, GoalSizes, ModuleInfo1, ModuleInfo).

		% This is called when the first procedure of a pred was 
		% changed. It fixes up all the other procs, ignoring the
		% goal_size and requests that come out, since that information
		% has already been collected. 
:- pred traverse_other_procs(ho_params::in, pred_id::in, list(proc_id)::in,
		module_info::in, pred_info::in, set(request)::in,
		set(request)::out, proc_table::in, proc_table::out) is det. 

traverse_other_procs(_Params, _PredId, [], _Module, _PredInfo,
		Requests, Requests, Procs, Procs).
traverse_other_procs(Params, PredId, [ProcId | ProcIds], ModuleInfo,
		PredInfo0, Requests0, Requests, Procs0, Procs) :-
	map__init(PredVars0),
	map__init(NewPredMap),
	map__init(PredVarMap),
	NewPreds0 = new_preds(NewPredMap, PredVarMap),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	Info0 = info(PredVars0, Requests0, NewPreds0, proc(PredId, ProcId),
			PredInfo0, ProcInfo0, ModuleInfo, Params, unchanged),
	traverse_goal_0(Goal0, Goal1, Info0,
			info(_, Requests1, _,_,_,_,_,_,_)),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	implicitly_quantify_clause_body(HeadVars, Goal1, Varset0, VarTypes0,
						Goal, Varset, VarTypes, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs1),
	traverse_other_procs(Params, PredId, ProcIds, ModuleInfo, PredInfo0,
		Requests1, Requests, Procs1, Procs).
	
%-------------------------------------------------------------------------------
	% Goal traversal

:- pred traverse_goal_0(hlds_goal::in, hlds_goal::out, 
	higher_order_info::in, higher_order_info::out) is det.

traverse_goal_0(Goal0, Goal, Info0, Info) :-
	Info0 = info(_, B, NewPreds0, PredProcId, E, F, G, H, I),
	NewPreds0 = new_preds(_, PredVarMap),
	% Lookup the initial known bindings of the variables if this
	% procedure is a specialised version.
	( map__search(PredVarMap, PredProcId, PredVars) ->
		Info1 = info(PredVars, B, NewPreds0, PredProcId, E, F, G, H, I)
	;
		Info1 = Info0
	),
	traverse_goal(Goal0, Goal, Info1, Info).

	% Traverses the goal collecting higher order variables for which 
	% the value is known, and specializing calls and adding
	% specialization requests to the request_info structure. 
	% The first time through the only predicate we can specialize
	% is call/N. The pred_proc_id is that of the current procedure,
	% used to find out which procedures need fixing up later.
:- pred traverse_goal(hlds_goal::in, hlds_goal::out, 
	higher_order_info::in, higher_order_info::out) is det.

traverse_goal(conj(Goals0) - Info, conj(Goals) - Info) -->
	list__map_foldl(traverse_goal, Goals0, Goals).

traverse_goal(par_conj(Goals0, SM) - Info, par_conj(Goals, SM) - Info) -->
		% traverse_disj treats its list of goals as independent
		% rather than specifically disjoint, so we can use it
		% to process a list of independent parallel conjuncts.
	traverse_disj(Goals0, Goals).

traverse_goal(disj(Goals0, SM) - Info, disj(Goals, SM) - Info) -->
	traverse_disj(Goals0, Goals).

		% a switch is treated as a disjunction
traverse_goal(switch(Var, CanFail, Cases0, SM) - Info,
		switch(Var, CanFail, Cases, SM) - Info) -->
	traverse_cases(Cases0, Cases).

		% check whether this call could be specialized
traverse_goal(Goal0, Goal) -->
	{ Goal0 = higher_order_call(Var, Args, _,_,_,_) - _ }, 
	maybe_specialize_higher_order_call(Var, no, Args, Goal0, Goal).

		% class_method_calls are treated similarly to
		% higher_order_calls.
traverse_goal(Goal0, Goal) -->
	{ Goal0 = class_method_call(Var, Method, Args,_,_,_) - _ },
	maybe_specialize_higher_order_call(Var, yes(Method), Args,
		Goal0, Goal).

		% check whether this call could be specialized
traverse_goal(Goal0, Goal) -->
	{ Goal0 = call(_,_,_,_,_,_) - _ }, 
	maybe_specialize_call(Goal0, Goal).

		% if-then-elses are handled as disjunctions
traverse_goal(Goal0, Goal, Info0, Info) :- 
	Goal0 = if_then_else(Vars, Cond0, Then0, Else0, SM) - GoalInfo,
	traverse_goal(Cond0, Cond, Info0, Info1),
	traverse_goal(Then0, Then, Info1, Info2),
	traverse_goal(Else0, Else, Info0, Info3),
	Goal = if_then_else(Vars, Cond, Then, Else, SM) - GoalInfo,
	merge_higher_order_infos(Info2, Info3, Info).

traverse_goal(not(NegGoal0) - Info, not(NegGoal) - Info) -->
	traverse_goal(NegGoal0, NegGoal).

traverse_goal(some(Vars, Goal0) - Info, some(Vars, Goal) - Info) -->
	traverse_goal(Goal0, Goal).

traverse_goal(Goal, Goal) -->
	{ Goal = pragma_c_code(_, _, _, _, _, _, _) - _ }.

traverse_goal(Goal, Goal) -->
	{ Goal = unify(_, _, _, Unify, _) - _ }, 
	check_unify(Unify).

		% To process a disjunction, we process each disjunct with the
		% specialization information before the goal, then merge the
		% results to give the specialization information after the
		% disjunction.
		%
		% This code is used both for disjunction and parallel
		% conjunction.

:- pred traverse_disj(hlds_goals::in, hlds_goals::out,
	higher_order_info::in, higher_order_info::out) is det.

traverse_disj([], []) --> [].
traverse_disj([Goal0 | Goals0], [Goal | Goals]) -->
	=(Info0),
	traverse_goal(Goal0, Goal),
	traverse_disj_2(Goals0, Goals, Info0).

:- pred traverse_disj_2(hlds_goals::in, hlds_goals::out, higher_order_info::in,
	higher_order_info::in, higher_order_info::out) is det.

traverse_disj_2([], [], _, Info, Info).
traverse_disj_2([Goal0 | Goals0], [Goal | Goals], InitialInfo, Info0, Info) :-
	traverse_goal(Goal0, Goal, InitialInfo, ThisGoalInfo),
	merge_higher_order_infos(Info0, ThisGoalInfo, Info1),
	traverse_disj_2(Goals0, Goals, InitialInfo, Info1, Info).

		% Switches are treated in exactly the same way as disjunctions.
:- pred traverse_cases(list(case)::in, list(case)::out, 
	higher_order_info::in, higher_order_info::out) is det.

traverse_cases([], []) --> [].
traverse_cases([case(ConsId, Goal0) | Cases0],
		[case(ConsId, Goal) | Cases]) -->
	=(Info0),
	traverse_goal(Goal0, Goal),
	traverse_cases_2(Cases0, Cases, Info0).

:- pred traverse_cases_2(list(case)::in, list(case)::out, higher_order_info::in,
	higher_order_info::in, higher_order_info::out) is det.

traverse_cases_2([], [], _, Info, Info).
traverse_cases_2([Case0 | Cases0], [Case | Cases], InitialInfo, Info0, Info) :-
	Case0 = case(ConsId, Goal0),
	traverse_goal(Goal0, Goal, InitialInfo, ThisGoalInfo),
	Case = case(ConsId, Goal),
	merge_higher_order_infos(Info0, ThisGoalInfo, Info1),
	traverse_cases_2(Cases0, Cases, InitialInfo, Info1, Info).

	% This is used in traversing disjunctions. We save the initial
	% accumulator, then traverse each disjunct starting with the initial
	% info. We then merge the resulting infos.
:- pred merge_higher_order_infos(higher_order_info::in, higher_order_info::in,
					higher_order_info::out) is det.

merge_higher_order_infos(Info1, Info2, Info) :-
	Info1 = info(PredVars1, Requests1, NewPreds, PredProcId,
			PredInfo, ProcInfo, ModuleInfo, Params, Changed1),
	Info2 = info(PredVars2, Requests2,_,_,_,_,_,_,Changed2),
	merge_pred_vars(PredVars1, PredVars2, PredVars),
	set__union(Requests1, Requests2, Requests12),
	set__to_sorted_list(Requests12, List12),
	set__sorted_list_to_set(List12, Requests),
	update_changed_status(Changed1, Changed2, Changed),
	Info = info(PredVars, Requests, NewPreds, PredProcId,
		PredInfo, ProcInfo, ModuleInfo, Params, Changed).

:- pred merge_pred_vars(pred_vars::in, pred_vars::in, pred_vars::out) is det.

merge_pred_vars(PredVars1, PredVars2, PredVars) :-
	map__to_assoc_list(PredVars1, PredVarList1),
	map__to_assoc_list(PredVars2, PredVarList2),
	merge_pred_var_lists(PredVarList1, PredVarList2, PredVarList),
	map__from_assoc_list(PredVarList, PredVars). 
	
		% find out which variables after a disjunction cannot
		% be specialized
:- pred merge_pred_var_lists(assoc_list(var, maybe_const)::in,  	
			assoc_list(var, maybe_const)::in,
			assoc_list(var, maybe_const)::out) is det.

merge_pred_var_lists([], List, List).
merge_pred_var_lists([PredVar | PredVars], List2, MergedList) :-
	merge_pred_var_with_list(PredVar, List2, MergedList1),
	merge_pred_var_lists(PredVars, MergedList1, MergedList).

:- pred merge_pred_var_with_list(pair(var, maybe_const)::in,
			assoc_list(var, maybe_const)::in,
			assoc_list(var, maybe_const)::out) is det.

merge_pred_var_with_list(VarValue, [], [VarValue]).
merge_pred_var_with_list(Var1 - Value1, [Var2 - Value2 | Vars], MergedList) :-
	(
		Var1 = Var2
	->
		(	(
				Value1 \= Value2
			;	Value1 = multiple_values
			;	Value2 = multiple_values
			)
		->
			MergedList = [Var1 - multiple_values | Vars]
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
	Info0 = info(PredVars0, Requests, NewPreds, PredProcId,
		PredInfo, ProcInfo, ModuleInfo, Params, Changed),
	( is_interesting_cons_id(Params, ConsId) ->
		( map__search(PredVars0, LVar, Specializable) ->
			(
				% we can't specialize calls involving
				% a variable with more than one
				% possible value
				Specializable = constant(_, _),
				map__det_update(PredVars0, LVar,
					multiple_values, PredVars)
			;
				% if a variable is already
				% non-specializable, it can't become
				% specializable
				Specializable = multiple_values,
				PredVars = PredVars0
			)
		;
			map__det_insert(PredVars0, LVar,
				constant(ConsId, Args), PredVars)
		)
	;
		PredVars = PredVars0	
	),
	Info = info(PredVars, Requests, NewPreds, PredProcId, 
		PredInfo, ProcInfo, ModuleInfo, Params, Changed).
	
check_unify(complicated_unify(_, _)) -->
	{ error("higher_order:check_unify - complicated unification") }.

:- pred is_interesting_cons_id(ho_params::in, cons_id::in) is semidet.

is_interesting_cons_id(ho_params(_, yes, _),
		cons(qualified(Module, Name), _)) :-
	mercury_private_builtin_module(Module),
	( Name = "type_info"
	; Name = "typeclass_info"
	).
is_interesting_cons_id(ho_params(yes, _, _), pred_const(_, _)).
is_interesting_cons_id(ho_params(_, yes, _), base_type_info_const(_, _, _)).
is_interesting_cons_id(ho_params(_, yes, _),
		base_typeclass_info_const(_, _, _, _)).
	% We need to keep track of int_consts so we can interpret
	% superclass_info_from_typeclass_info and typeinfo_from_typeclass_info.
	% We don't specialize based on them.
is_interesting_cons_id(ho_params(_, yes, _), int_const(_)).

	% Process a higher-order call or class_method_call to see if it
	% could possibly be specialized.
:- pred maybe_specialize_higher_order_call(var::in, maybe(int)::in,
	list(var)::in, hlds_goal::in, hlds_goal::out, 
	higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_higher_order_call(PredVar, MaybeMethod, Args,
		Goal0 - GoalInfo, Goal - GoalInfo, Info0, Info) :-
	Info0 = info(PredVars, _Requests0, _NewPreds, _PredProcId,
		_CallerPredInfo, _CallerProcInfo, ModuleInfo, _, _),
		
	%proc_info_vartypes(CallerProcInfo, VarTypes),
	%map__lookup(VarTypes, PredVar, PredVarType),

	% We can specialize calls to call/N and class_method_call/N if
	% the closure or typeclass_info has a known value.
	(
		map__search(PredVars, PredVar, constant(ConsId, CurriedArgs)),
		(
			ConsId = pred_const(PredId0, ProcId0),
			MaybeMethod = no
		->
			PredId = PredId0,
			ProcId = ProcId0,
			list__append(CurriedArgs, Args, AllArgs)
		;
			% A typeclass_info variable should consist of
			% a known base_typeclass_info and some argument
			% typeclass_infos.
			ConsId = cons(TypeClassInfo, _),
			mercury_private_builtin_module(Module),
			TypeClassInfo = qualified(Module, "typeclass_info"),
			CurriedArgs = [BaseTypeClassInfo | OtherTypeClassArgs],
			map__search(PredVars, BaseTypeClassInfo,
				constant(BaseConsId, _)),
			BaseConsId = base_typeclass_info_const(_,
				ClassId, Instance, _),
			MaybeMethod = yes(Method),
			module_info_instances(ModuleInfo, Instances),
			map__lookup(Instances, ClassId, InstanceList),
			list__index1_det(InstanceList, Instance, InstanceDefn),
			InstanceDefn = hlds_instance_defn(_,
				InstanceConstraints, _, _,
				yes(ClassInterface), _, _),
			list__length(InstanceConstraints, InstanceArity),
			list__take(InstanceArity, OtherTypeClassArgs,
				InstanceConstraintArgs)
		->	
			list__index1_det(ClassInterface, Method,
				hlds_class_proc(PredId, ProcId)),
			list__append(InstanceConstraintArgs, Args, AllArgs)
		;
			fail	
		)
	->
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_module(PredInfo, ModuleName),
		pred_info_name(PredInfo, PredName),
		code_util__builtin_state(ModuleInfo, PredId, ProcId, Builtin),

		MaybeContext = no,
		Goal1 = call(PredId, ProcId, AllArgs,
			Builtin, MaybeContext,
			qualified(ModuleName, PredName)),
		higher_order_info_update_changed_status(changed, Info0, Info1),
		maybe_specialize_call(Goal1 - GoalInfo,
			Goal - _, Info1, Info)
	;
		% non-specializable call/N or class_method_call/N
		Goal = Goal0,
		Info = Info0
	).

		% Process a call to see if it could possibly be specialized.
:- pred maybe_specialize_call(hlds_goal::in, hlds_goal::out,
		higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_call(Goal0 - GoalInfo, Goal - GoalInfo, Info0, Info) :-
	Info0 = info(PredVars, Requests0, NewPreds, PredProcId,
			PredInfo, ProcInfo, Module, Params, Changed0),
	(
		Goal0 = call(_, _, _, _, _, _)
	->
		Goal0 = call(CalledPred, CalledProc, Args0, IsBuiltin,
					MaybeContext, _SymName0)
	;
		error("higher_order.m: call expected")
	),
	module_info_pred_info(Module, CalledPred, CalleePredInfo),
	(
		% Look for calls to unify/2 and compare/3 which can
		% be specialized.
		specialize_special_pred(Info0, CalledPred, CalledProc,
			Args0, MaybeContext, Goal1) 
	->
		Goal = Goal1,
		higher_order_info_update_changed_status(changed, Info0, Info)
	;
		polymorphism__is_typeclass_info_manipulator(Module,
			CalledPred, Manipulator)
	->
		interpret_typeclass_info_manipulator(Manipulator, Args0,
			Goal0, Goal, Info0, Info)
	;
		( pred_info_is_imported(CalleePredInfo)
		; pred_info_get_goal_type(CalleePredInfo, pragmas)
		)
	->
		Info = Info0,
		Goal = Goal0
	;
		pred_info_arg_types(CalleePredInfo, CalleeArgTypes),
		proc_info_vartypes(ProcInfo, VarTypes),
		find_higher_order_args(Module, Args0, CalleeArgTypes,
			VarTypes, PredVars, 1, [], HigherOrderArgs0,
			Args0, Args1),
		( HigherOrderArgs0 = [] ->
			Info = Info0,
			Goal = Goal0
		;
			list__reverse(HigherOrderArgs0, HigherOrderArgs),
			find_matching_version(Info0, CalledPred, CalledProc,
				Args0, Args1, HigherOrderArgs, FindResult),
			(
				FindResult = match(Match, ExtraTypeInfos),
				Match = new_pred(NewPredProcId, _, _,
					NewName, _HOArgs, _, _, _, _, _),
				list__append(ExtraTypeInfos, Args1, Args),
				NewPredProcId = proc(NewCalledPred,
					NewCalledProc),
				Goal = call(NewCalledPred, NewCalledProc,
					Args, IsBuiltin, MaybeContext, NewName),
				update_changed_status(Changed0,
					changed, Changed),
				Requests = Requests0
			;
				% There is a known higher order variable in
				% the call, so we put in a request for a
				% specialized version of the pred.
				Goal = Goal0,
				FindResult = request(Request),
				set__insert(Requests0, Request, Requests),
				update_changed_status(Changed0,
					request, Changed)
			),
			Info = info(PredVars, Requests, NewPreds, PredProcId,
				PredInfo, ProcInfo, Module, Params, Changed)
		)
	).

	% Returns a list of the higher-order arguments in a call that have
	% a known value. Also update the argument list to now include
	% curried arguments that need to be explicitly passed.
	% The order of the argument list must match that generated
	% by construct_higher_order_terms.
:- pred find_higher_order_args(module_info::in, list(var)::in, list(type)::in,
		map(var, type)::in, pred_vars::in, int::in,
		list(higher_order_arg)::in, list(higher_order_arg)::out,
		list(var)::in, list(var)::out) is det.

find_higher_order_args(_, [], _, _, _, _,
		HOArgs, HOArgs, NewArgs, NewArgs).
find_higher_order_args(_, [_|_], [], _, _, _, _, _, _, _) :-
	error("find_higher_order_args: length mismatch").
find_higher_order_args(ModuleInfo, [Arg | Args],
		[CalleeArgType | CalleeArgTypes], VarTypes, PredVars, ArgNo,
		HOArgs0, HOArgs, NewArgs0, NewArgs) :-
	NextArg is ArgNo + 1,
	(
		% We don't specialize arguments whose declared type is
		% polymorphic. The closure they pass cannot possibly
		% be called within the called predicate, since that predicate 
		% doesn't know it's a closure (without some dodgy use of
		% type_to_univ and univ_to_type).
		map__search(PredVars, Arg, constant(ConsId, CurriedArgs)),

		% We don't specialize based on int_consts (we only keep track
		% of them to interpret calls to the procedures which
		% extract fields from typeclass_infos).
		ConsId \= int_const(_),

		( ConsId = pred_const(_, _) ->
			type_is_higher_order(CalleeArgType, _, _)
		;
			true
		)
	->
		% Find any known higher-order arguments
		% in the list of curried arguments.
		map__apply_to_list(CurriedArgs, VarTypes, CurriedArgTypes),
		( ConsId = pred_const(PredId, _) ->
			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			pred_info_arg_types(PredInfo, CurriedCalleeArgTypes)
		;
			CurriedCalleeArgTypes = CurriedArgTypes
		),
		find_higher_order_args(ModuleInfo, CurriedArgs,
			CurriedCalleeArgTypes, VarTypes,
			PredVars, 1, [], HOCurriedArgs0,
			CurriedArgs, NewExtraArgs),
		list__reverse(HOCurriedArgs0, HOCurriedArgs),
		list__length(CurriedArgs, NumArgs),
		HOArg = higher_order_arg(ConsId, ArgNo, NumArgs,
			CurriedArgs, CurriedArgTypes, HOCurriedArgs),
		HOArgs1 = [HOArg | HOArgs0],
		list__append(NewArgs0, NewExtraArgs, NewArgs1)
	;
		HOArgs1 = HOArgs0,
		NewArgs1 = NewArgs0
	),
	find_higher_order_args(ModuleInfo, Args, CalleeArgTypes,
		VarTypes, PredVars, NextArg, HOArgs1, HOArgs,
		NewArgs1, NewArgs).

:- type find_result
	--->	match(
			new_pred,	% Specialised version to use.
			list(var)	% Ordered list of extra type-info
					% variables to add to the front of
					% the argument list, empty if
					% --typeinfo-liveness is not set.
		)
	;	request(request)
	.

:- pred find_matching_version(higher_order_info::in, 
	pred_id::in, proc_id::in, list(var)::in, list(var)::in,
	list(higher_order_arg)::in, find_result::out) is det.

	% Args0 is the original list of arguments.
	% Args1 is the original list of arguments with the curried arguments
	% of known higher-order arguments added.
find_matching_version(Info, CalledPred, CalledProc, Args0, Args1,
		HigherOrderArgs, Result) :-
	Info = info(_, _, NewPreds, Caller,
		PredInfo, ProcInfo, ModuleInfo, _, _),
	proc_info_vartypes(ProcInfo, VarTypes),
	pred_info_arg_types(PredInfo, _, ExistQVars, _),
	pred_info_typevarset(PredInfo, TVarSet),

	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals,
		typeinfo_liveness, TypeInfoLiveness),
	( TypeInfoLiveness = yes ->
		set__list_to_set(Args1, NonLocals0),
		proc_info_typeinfo_varmap(ProcInfo, TVarMap),
		proc_info_typeclass_info_varmap(ProcInfo, TCVarMap),
		goal_util__extra_nonlocal_typeinfos(TVarMap, TCVarMap,
			VarTypes, ExistQVars, NonLocals0, TypeInfos0),
		set__delete_list(TypeInfos0, Args1, ExtraTypeInfos0),
		set__to_sorted_list(ExtraTypeInfos0, ExtraTypeInfos),
		map__apply_to_list(ExtraTypeInfos,
			VarTypes, ExtraTypeInfoTypes)
	;
		ExtraTypeInfos = [],
		ExtraTypeInfoTypes = []
	),

	map__apply_to_list(Args0, VarTypes, CallArgTypes),
	Request = request(Caller, proc(CalledPred, CalledProc), Args0,
		ExtraTypeInfos, HigherOrderArgs, CallArgTypes,
		ExtraTypeInfoTypes, TVarSet), 

	% Check to see if any of the specialized
	% versions of the called pred apply here.
	( 
		NewPreds = new_preds(NewPredMap, _),
		map__search(NewPredMap, proc(CalledPred, CalledProc),
			NewPredSet),
		set__to_sorted_list(NewPredSet, NewPredList),
		search_for_version(TypeInfoLiveness, ModuleInfo, Request,
			ExtraTypeInfos, NewPredList,
			Match, OrderedExtraTypeInfos)
	->
		Result = match(Match, OrderedExtraTypeInfos)
	;
		Result = request(Request)
	).

:- pred search_for_version(bool::in, module_info::in, request::in, 
		list(var)::in, list(new_pred)::in, new_pred::out,
		list(var)::out) is semidet.

search_for_version(TypeInfoLiveness, ModuleInfo, Request, ExtraTypeInfos,
		[Version | Versions], Match, OrderedExtraTypeInfos) :-
	(
		version_matches(TypeInfoLiveness, ModuleInfo, Request,
			Version, yes(ExtraTypeInfos), OrderedExtraTypeInfos0)
	->
		Match = Version,
		OrderedExtraTypeInfos = OrderedExtraTypeInfos0
	;
		search_for_version(TypeInfoLiveness, ModuleInfo, Request,
			ExtraTypeInfos, Versions, Match, OrderedExtraTypeInfos)
	).

	% Check whether the request has already been implemented by 
	% the new_pred, maybe ordering the list of extra type_infos
	% in the caller predicate to match up with those in the caller.
:- pred version_matches(bool::in, module_info::in, request::in, 
	new_pred::in, maybe(list(var))::in, list(var)::out) is semidet.

version_matches(TypeInfoLiveness, _ModuleInfo, Request, Version,
		MaybeExtraTypeInfos, OrderedExtraTypeInfos) :-

	Request = request(_, _, _, _, RequestHigherOrderArgs, CallArgTypes,
		ExtraTypeInfoTypes, RequestTVarSet), 
	Version = new_pred(_, _, _, _, VersionHigherOrderArgs, _, _,
		VersionArgTypes0, VersionExtraTypeInfoTypes0, VersionTVarSet),

	higher_order_args_match(RequestHigherOrderArgs,
		VersionHigherOrderArgs),

	% Rename apart type variables.
	varset__merge_subst(RequestTVarSet, VersionTVarSet, _, TVarSubn),
	term__apply_substitution_to_list(VersionArgTypes0, TVarSubn,
		VersionArgTypes),
	term__apply_substitution_to_list(VersionExtraTypeInfoTypes0,
		TVarSubn, VersionExtraTypeInfoTypes),
	type_list_subsumes(VersionArgTypes, CallArgTypes, Subn),
	( TypeInfoLiveness = yes ->
		% If typeinfo_liveness is set, the subsumption
		% must go in both directions, since otherwise
		% the set of type_infos which need to be passed
		% might not be the same.
		type_list_subsumes(CallArgTypes, VersionArgTypes, _)
	;
		true
	),
	( TypeInfoLiveness = yes, MaybeExtraTypeInfos = yes(ExtraTypeInfos) ->
		term__apply_rec_substitution_to_list(
			VersionExtraTypeInfoTypes,
			Subn, RenamedVersionTypeInfos),
		assoc_list__from_corresponding_lists(ExtraTypeInfos,
			ExtraTypeInfoTypes, ExtraTypeInfoAL),
		order_typeinfos(Subn, ExtraTypeInfoAL, RenamedVersionTypeInfos,
			[], OrderedExtraTypeInfos)
	;
		OrderedExtraTypeInfos = []
	).

	% Put the extra typeinfos for --typeinfo-liveness in the correct
	% order by looking at their types.
:- pred order_typeinfos(tsubst::in, assoc_list(var, type)::in,
		list(type)::in, list(var)::in, list(var)::out) is semidet.

order_typeinfos(_, [], [], RevOrderedVars, OrderedVars) :-
	list__reverse(RevOrderedVars, OrderedVars).
order_typeinfos(Subn, VarsAndTypes0, [VersionType | VersionTypes],
		RevOrderedVars0, OrderedVars) :-
	term__apply_rec_substitution(VersionType, Subn, VersionType1),
	strip_term_context(VersionType1, VersionType2),
	order_typeinfos_2(VersionType2, Var, VarsAndTypes0, VarsAndTypes),
	order_typeinfos(Subn, VarsAndTypes, VersionTypes,
		[Var | RevOrderedVars0], OrderedVars).	

	% Find the variable in the requesting predicate which corresponds
	% to the current extra typeinfo argument.
:- pred order_typeinfos_2((type)::in, var::out, assoc_list(var, type)::in,
		assoc_list(var, type)::out) is semidet.

order_typeinfos_2(VersionType, Var, [Var1 - VarType | VarsAndTypes0],
		VarsAndTypes) :-
	( strip_term_context(VarType, VersionType) ->
		Var = Var1,
		VarsAndTypes = VarsAndTypes0
	;
		order_typeinfos_2(VersionType, Var,
			VarsAndTypes0, VarsAndTypes1),
		VarsAndTypes = [Var1 - VarType | VarsAndTypes1]
	).

:- pred higher_order_args_match(list(higher_order_arg)::in,
		list(higher_order_arg)::in) is semidet.

higher_order_args_match([], []).
higher_order_args_match([Arg1 | Args1], [Arg2 | Args2]) :-
	Arg1 = higher_order_arg(ConsId, ArgNo, NumArgs,
			_, _, HOCurriedArgs1),
	Arg2 = higher_order_arg(ConsId, ArgNo, NumArgs,
			_, _, HOCurriedArgs2),
	higher_order_args_match(HOCurriedArgs1, HOCurriedArgs2),
	higher_order_args_match(Args1, Args2).

		% if the right argument of an assignment is a higher order
		% term with a known value, we need to add an entry for
		% the left argument
:- pred maybe_add_alias(var::in, var::in, higher_order_info::in,
				higher_order_info::out) is det.

maybe_add_alias(LVar, RVar,
		info(PredVars0, Requests, NewPreds, PredProcId, 
			PredInfo, ProcInfo, ModuleInfo, Params, Changed),
		info(PredVars, Requests, NewPreds, PredProcId,
			PredInfo, ProcInfo, ModuleInfo, Params, Changed)) :-
	(
		map__search(PredVars0, RVar, constant(A, B))
	->
		map__set(PredVars0, LVar, constant(A, B), PredVars)
	;
		PredVars = PredVars0
	).
		
:- pred update_changed_status(changed::in, changed::in, changed::out) is det.

update_changed_status(changed, _, changed).
update_changed_status(request, changed, changed).
update_changed_status(request, request, request).
update_changed_status(request, unchanged, request).
update_changed_status(unchanged, Changed, Changed).

:- pred higher_order_info_update_changed_status(changed::in,
		higher_order_info::in, higher_order_info::out) is det.

higher_order_info_update_changed_status(Changed1, Info0, Info) :-
	Info0 = info(A,B,C,D,E,F,G,H, Changed0),
	update_changed_status(Changed0, Changed1, Changed),
	Info = info(A,B,C,D,E,F,G,H, Changed).

%-------------------------------------------------------------------------------

	% Interpret a call to `type_info_from_typeclass_info' or
	% `superclass_from_typeclass_info'. Currently they both have
	% the same definition. This should be kept in sync with
	% compiler/polymorphism.m, library/private_builtin.m and
	% runtime/mercury_type_info.h.
:- pred interpret_typeclass_info_manipulator(typeclass_info_manipulator::in,
	list(var)::in, hlds_goal_expr::in, hlds_goal_expr::out,
	higher_order_info::in, higher_order_info::out) is det.

interpret_typeclass_info_manipulator(_, Args, Goal0, Goal, Info0, Info) :-
	Info0 = info(PredVars0, _, _, _, _, _, ModuleInfo, _, _),
	(
		Args = [TypeClassInfoVar, IndexVar, TypeInfoVar],
		map__search(PredVars0, TypeClassInfoVar,
			constant(_TypeClassInfoConsId, TypeClassInfoArgs)),

		map__search(PredVars0, IndexVar,
			constant(int_const(Index), [])),

		% Extract the number of class constraints on the instance
		% from the base_typeclass_info.
		TypeClassInfoArgs = [BaseTypeClassInfoVar | OtherVars],

		map__search(PredVars0, BaseTypeClassInfoVar,
		    	constant(base_typeclass_info_const(_,
				ClassId, InstanceNum, _), _))
	->
		module_info_instances(ModuleInfo, Instances),
		map__lookup(Instances, ClassId, InstanceDefns),
		list__index1_det(InstanceDefns, InstanceNum, InstanceDefn),
		InstanceDefn = hlds_instance_defn(_, Constraints, _,_,_,_,_),
		list__length(Constraints, NumConstraints),	
		TypeInfoIndex is Index + NumConstraints,	
		list__index1_det(OtherVars, TypeInfoIndex, TypeInfoArg),
		maybe_add_alias(TypeInfoVar, TypeInfoArg, Info0, Info),
		Uni = assign(TypeInfoVar, TypeInfoArg),
		in_mode(In),
		out_mode(Out),
		Goal = unify(TypeInfoVar, var(TypeInfoArg), Out - In,
			Uni, unify_context(explicit, []))
	;
		Goal = Goal0,
		Info = Info0
	).

%-------------------------------------------------------------------------------

	% Succeed if the called pred is "unify", "compare" or "index" and
	% is specializable, returning a specialized goal.
:- pred specialize_special_pred(higher_order_info::in, pred_id::in,
		proc_id::in, list(var)::in, maybe(call_unify_context)::in,
		hlds_goal_expr::out) is semidet.
		
specialize_special_pred(Info0, CalledPred, _CalledProc, Args,
		MaybeContext, Goal) :-
	Info0 = info(PredVars, _, _, _, _, ProcInfo, ModuleInfo, _, _),
	proc_info_vartypes(ProcInfo, VarTypes),
	module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
	mercury_public_builtin_module(PublicBuiltin),
	pred_info_module(CalledPredInfo, PublicBuiltin),
	pred_info_name(CalledPredInfo, PredName),
	pred_info_arity(CalledPredInfo, PredArity),
	special_pred_name_arity(SpecialId, PredName, _, PredArity),
	special_pred_get_type(PredName, Args, Var),
	map__lookup(VarTypes, Var, SpecialPredType),
	SpecialPredType \= term__variable(_),
	Args = [TypeInfoVar | SpecialPredArgs],
	map__search(PredVars, TypeInfoVar,
		constant(_TypeInfoConsId, TypeInfoVarArgs)),
	type_to_type_id(SpecialPredType, _ - TypeArity, _),
	( TypeArity = 0 ->
		TypeInfoArgs = []
	;
		TypeInfoVarArgs = [_BaseTypeInfo | TypeInfoArgs]
	),

	( SpecialId = unify, type_is_atomic(SpecialPredType, ModuleInfo) ->
		% Unifications of atomic types can be specialized
		% to simple_tests.
		list__reverse(Args, [Arg2, Arg1 | _]),
		in_mode(In),
		Goal = unify(Arg1, var(Arg2), (In - In),
			simple_test(Arg1, Arg2), unify_context(explicit, [])) 
	;
		polymorphism__get_special_proc(SpecialPredType, SpecialId,
			ModuleInfo, SymName, SpecialPredId, SpecialProcId),
		list__append(TypeInfoArgs, SpecialPredArgs, CallArgs),
		Goal = call(SpecialPredId, SpecialProcId, CallArgs,
			not_builtin, MaybeContext, SymName)
	).
		
%-------------------------------------------------------------------------------
% Predicates to process requests for specialization, and create any  
% new predicates that are required.	

		% Filter out requests for higher-order specialization 
		% for preds which are too large. Maybe we could allow
		% programmers to declare which predicates they want
		% specialized, as with inlining?
		% Don't create specialized versions of specialized
		% versions, since for some fairly contrived examples 
		% involving recursively building up lambda expressions
		% this can create ridiculous numbers of versions.
:- pred filter_requests(ho_params::in, module_info::in,
	set(request)::in, goal_sizes::in, list(request)::out) is det.

filter_requests(Params, ModuleInfo, Requests0, GoalSizes, Requests) :-
	Params = ho_params(_, _, MaxSize),
	set__to_sorted_list(Requests0, Requests1),
	list__filter(lambda([X::in] is semidet, (
			X = request(_, CalledPredProcId, _, _, _, _, _, _),
			CalledPredProcId = proc(CalledPredId,
				CalledProcId),
			module_info_pred_info(ModuleInfo,
				CalledPredId, PredInfo),
			\+ pred_info_is_imported(PredInfo),
			\+ (
				pred_info_is_pseudo_imported(PredInfo),
				hlds_pred__in_in_unification_proc_id(
					CalledProcId)
			),
			map__search(GoalSizes, CalledPredId, GoalSize),
			GoalSize =< MaxSize,
			pred_info_name(PredInfo, PredName),
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
		list(new_pred)::in, list(new_pred)::out,
		set(pred_proc_id)::in, set(pred_proc_id)::out, int::in,
		int::out, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

create_new_preds([], NewPreds, NewPreds, NewPredList, NewPredList,
		ToFix, ToFix, NextId, NextId, Mod, Mod, IO, IO). 
create_new_preds([Request | Requests], NewPreds0, NewPreds,
		NewPredList0, NewPredList, PredsToFix0, PredsToFix,
		NextHOid0, NextHOid, Module0, Module, IO0, IO)  :-
	Request = request(CallingPredProcId, CalledPredProcId, _HOArgs,
			_CallArgs, _, _CallerArgTypes, _ExtraTypeInfoTypes, _),
	set__insert(PredsToFix0, CallingPredProcId, PredsToFix1),
	(
		NewPreds0 = new_preds(NewPredMap0, _),
		map__search(NewPredMap0, CalledPredProcId, SpecVersions0)
	->
		globals__io_lookup_bool_option(typeinfo_liveness,
			TypeInfoLiveness, IO0, IO1),
		(
			% check that we aren't redoing the same pred
			% SpecVersions are pred_proc_ids of the specialized
			% versions of the current pred.
			\+ (
				set__member(Version, SpecVersions0),
				version_matches(TypeInfoLiveness, Module0,
					Request, Version, no, _)
			)
		->
			create_new_pred(Request, NewPred, NextHOid0,
				NextHOid1, Module0, Module1, IO1, IO2), 
			add_new_pred(CalledPredProcId, NewPred,
				NewPreds0, NewPreds1),
			NewPredList1 = [NewPred | NewPredList0]
		;
			Module1 = Module0,
			NewPredList1 = NewPredList0,
			NewPreds1 = NewPreds0,
			IO2 = IO1,
			NextHOid1 = NextHOid0
		)
	;
		create_new_pred(Request, NewPred, NextHOid0, NextHOid1,
			Module0, Module1, IO0, IO2),
		add_new_pred(CalledPredProcId, NewPred, NewPreds0, NewPreds1),
		NewPredList1 = [NewPred | NewPredList0]
	),
	create_new_preds(Requests, NewPreds1, NewPreds, NewPredList1,
		NewPredList, PredsToFix1, PredsToFix, NextHOid1, NextHOid,
		Module1, Module, IO2, IO).

:- pred add_new_pred(pred_proc_id::in, new_pred::in,
		new_preds::in, new_preds::out) is det.

add_new_pred(CalledPredProcId, NewPred, new_preds(NewPreds0, PredVars),
		new_preds(NewPreds, PredVars)) :-
	( map__search(NewPreds0, CalledPredProcId, SpecVersions0) ->
		set__insert(SpecVersions0, NewPred, SpecVersions)
	;
		set__singleton_set(SpecVersions, NewPred)
	),
	map__set(NewPreds0, CalledPredProcId, SpecVersions, NewPreds).

		% Here we create the pred_info for the new predicate.
:- pred create_new_pred(request::in, new_pred::out, int::in, int::out,
	module_info::in, module_info::out, io__state::di, io__state::uo) is det.

create_new_pred(Request, NewPred, NextHOid0, NextHOid,
		ModuleInfo0, ModuleInfo, IOState0, IOState) :- 
	Request = request(Caller, CalledPredProc, CallArgs, ExtraTypeInfoArgs,
			HOArgs, ArgTypes, ExtraTypeInfoTypes, CallerTVarSet),
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
        pred_info_arg_types(PredInfo0, ArgTVarSet, ExistQVars, Types),
	string__int_to_string(Arity, ArStr),
	(
 		VeryVerbose = yes
	->
		prog_out__sym_name_to_string(PredModule, PredModuleString),
		io__write_strings(["% Specializing calls to `",
			PredModuleString, ":", Name0, "'/", ArStr,
			" with higher-order arguments:\n"],
			IOState1, IOState2),
		list__length(Types, ActualArity),
		NumToDrop is ActualArity - Arity,
		output_higher_order_args(ModuleInfo0, NumToDrop,
					HOArgs, IOState2, IOState)
	;
       		IOState = IOState1
       	),
	string__int_to_string(NextHOid0, IdStr),
	NextHOid is NextHOid0 + 1,
	string__append_list([Name0, "__ho", IdStr], PredName),
	pred_info_typevarset(PredInfo0, TypeVarSet),
	pred_info_context(PredInfo0, Context),
	pred_info_get_markers(PredInfo0, MarkerList),
	pred_info_get_goal_type(PredInfo0, GoalType),
	pred_info_get_class_context(PredInfo0, ClassContext),
	Name = qualified(PredModule, PredName),
	varset__init(EmptyVarSet),
	map__init(EmptyVarTypes),
	map__init(EmptyProofs),
	
	% This isn't looked at after here, and just clutters up
	% hlds dumps if it's filled in.
	ClausesInfo = clauses_info(EmptyVarSet, EmptyVarTypes,
		EmptyVarTypes, [], []),
	pred_info_init(PredModule, Name, Arity, ArgTVarSet, ExistQVars,
		Types, true, Context, ClausesInfo, local, MarkerList, GoalType,
		PredOrFunc, ClassContext, EmptyProofs, PredInfo1),
	pred_info_set_typevarset(PredInfo1, TypeVarSet, PredInfo2),
	pred_info_procedures(PredInfo2, Procs0),
	next_mode_id(Procs0, no, NewProcId),
	predicate_table_insert(PredTable0, PredInfo2, NewPredId, PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable, ModuleInfo),
	NewPred = new_pred(proc(NewPredId, NewProcId), CalledPredProc, Caller,
		Name, HOArgs, CallArgs, ExtraTypeInfoArgs, ArgTypes,
		ExtraTypeInfoTypes, CallerTVarSet).
	
:- pred output_higher_order_args(module_info::in, int::in,
	list(higher_order_arg)::in, io__state::di, io__state::uo) is det.

output_higher_order_args(_, _, []) --> [].
output_higher_order_args(ModuleInfo, NumToDrop, [HOArg | HOArgs]) -->
	{ HOArg = higher_order_arg(ConsId, ArgNo, NumArgs, _, _, _) },
	( { ConsId = pred_const(PredId, _ProcId) } ->
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
		io__write_int(Arity)
	;
		% XXX output the type.
		io__write_string(" type_info ")
	),
	io__write_string(" with "),
	io__write_int(NumArgs),
	io__write_string(" curried arguments\n"),
	output_higher_order_args(ModuleInfo, NumToDrop, HOArgs).

	% Fixup calls to specialized predicates.
:- pred fixup_preds(ho_params::in, list(pred_proc_id)::in, new_preds::in,
		module_info::in, module_info::out) is det.

fixup_preds(_Params, [], _, ModuleInfo, ModuleInfo).
fixup_preds(Params, [PredProcId | PredProcIds], NewPreds,
		ModuleInfo0, ModuleInfo) :-
	PredProcId = proc(PredId, ProcId), 
	module_info_preds(ModuleInfo0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, Procs0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	map__init(PredVars0),
	set__init(Requests0),
	Info0 = info(PredVars0, Requests0, NewPreds, PredProcId,
			PredInfo0, ProcInfo0, ModuleInfo0, Params, unchanged),
	traverse_goal_0(Goal0, Goal1, Info0, _),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	implicitly_quantify_clause_body(HeadVars, Goal1, Varset0, VarTypes0,
					Goal, Varset, VarTypes, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1),
	fixup_preds(Params, PredProcIds, NewPreds, ModuleInfo1, ModuleInfo).

	% Create specialized versions of a single procedure.
:- pred create_specialized_versions(ho_params::in, list(new_pred)::in,
		new_preds::in, new_preds::out, set(request)::in,
		set(request)::out, goal_sizes::in, goal_sizes::out,
		module_info::in, module_info::out) is det.

create_specialized_versions(_Params, [], NewPreds, NewPreds,
		Requests, Requests, Sizes, Sizes, ModuleInfo, ModuleInfo).
create_specialized_versions(Params, [NewPred | NewPreds], NewPredMap0,
		NewPredMap, Requests0, Requests, GoalSizes0, GoalSizes,
		ModuleInfo0, ModuleInfo) :-
	NewPred = new_pred(NewPredProcId, OldPredProcId, Caller, _Name,
		HOArgs0, CallArgs, ExtraTypeInfoArgs, CallerArgTypes0,
		ExtraTypeInfoTypes0, _),

	OldPredProcId = proc(OldPredId, OldProcId),
	module_info_pred_proc_info(ModuleInfo0, OldPredId, OldProcId,
		_, NewProcInfo0),

	NewPredProcId = proc(NewPredId, NewProcId),
	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_get_preds(PredTable0, Preds0),
	map__lookup(Preds0, NewPredId, NewPredInfo0),
	pred_info_procedures(NewPredInfo0, NewProcs0),
	proc_info_headvars(NewProcInfo0, HeadVars0),
	proc_info_argmodes(NewProcInfo0, ArgModes0),
	pred_info_arg_types(NewPredInfo0, _, ExistQVars0, _),
	pred_info_typevarset(NewPredInfo0, TypeVarSet0),

	Caller = proc(CallerPredId, CallerProcId),
	module_info_pred_proc_info(ModuleInfo0, CallerPredId, CallerProcId,
		CallerPredInfo, CallerProcInfo),
	pred_info_arg_types(CallerPredInfo, CallerTypeVarSet, _, _),
	pred_info_get_head_type_params(CallerPredInfo, CallerHeadParams),
	proc_info_typeinfo_varmap(CallerProcInfo, CallerTypeInfoVarMap0),

	%
	% Specialize the types of the called procedure as for inlining.
	%
	proc_info_vartypes(NewProcInfo0, VarTypes0),
	varset__merge_subst(CallerTypeVarSet, TypeVarSet0,
		TypeVarSet, TypeRenaming), 
        apply_substitution_to_type_map(VarTypes0, TypeRenaming, VarTypes1),

	% the real set of existentially quantified variables may be
	% smaller, but this is OK
	map__apply_to_list(ExistQVars0, TypeRenaming, ExistQTerms),
	term__term_list_to_var_list(ExistQTerms, ExistQVars),

        map__apply_to_list(HeadVars0, VarTypes1, HeadTypes0),
	inlining__get_type_substitution(HeadTypes0, CallerArgTypes0,
		CallerHeadParams, ExistQVars, TypeSubn),
	
	apply_rec_substitution_to_type_map(VarTypes1, TypeSubn, VarTypes2),
	( ( ExistQVars = [] ; map__is_empty(TypeSubn) ) ->
		HOArgs = HOArgs0,
		ExtraTypeInfoTypes = ExtraTypeInfoTypes0
	;	
		% If there are existentially quantified variables in the
		% callee we may need to bind type variables in the caller.
		list__map(substitute_higher_order_arg(TypeSubn),
			HOArgs0, HOArgs),
		term__apply_rec_substitution_to_list(ExtraTypeInfoTypes0,
			TypeSubn, ExtraTypeInfoTypes)
	),
	proc_info_set_vartypes(NewProcInfo0, VarTypes2, NewProcInfo1),

	% Add in the extra typeinfo vars.
	proc_info_create_vars_from_types(NewProcInfo1, ExtraTypeInfoTypes,
		ExtraTypeInfoVars, NewProcInfo2),

	map__from_corresponding_lists(CallArgs, HeadVars0, VarRenaming0),
	map__det_insert_from_corresponding_lists(VarRenaming0,
		ExtraTypeInfoArgs, ExtraTypeInfoVars, VarRenaming1),

	% Construct the constant input closures within the goal
	% for the called procedure.
	map__init(PredVars0),
	construct_higher_order_terms(ModuleInfo0, HeadVars0, HeadVars1,
		ArgModes0, ArgModes1, HOArgs, NewProcInfo2, NewProcInfo3,
		VarRenaming1, VarRenaming, PredVars0, PredVars),

	% Let traverse_goal know about the constant input arguments.
	NewPredMap0 = new_preds(A, PredVarMap0),
	map__det_insert(PredVarMap0, NewPredProcId, PredVars, PredVarMap),
	NewPredMap1 = new_preds(A, PredVarMap),	

	%
	% Fix up the typeinfo_varmap. 
	%
	proc_info_typeinfo_varmap(NewProcInfo3, TypeInfoVarMap0),

	% Restrict the caller's typeinfo_varmap
	% down onto the arguments of the call.
	map__to_assoc_list(CallerTypeInfoVarMap0, TypeInfoAL0),
	list__filter(lambda([TVarAndLocn::in] is semidet, (
			TVarAndLocn = _ - Locn,
			type_info_locn_var(Locn, LocnVar),
			map__contains(VarRenaming, LocnVar)
		)), TypeInfoAL0, TypeInfoAL),
	map__from_assoc_list(TypeInfoAL, CallerTypeInfoVarMap1),

	% The type renaming doesn't rename type variables in the caller.
	map__init(EmptyTypeRenaming),
	apply_substitutions_to_var_map(CallerTypeInfoVarMap1,
		EmptyTypeRenaming, TypeSubn, VarRenaming,
		CallerTypeInfoVarMap),
	% The variable renaming doesn't rename variables in the callee.
	map__init(EmptyVarRenaming),
	apply_substitutions_to_var_map(TypeInfoVarMap0, TypeRenaming,
		TypeSubn, EmptyVarRenaming, TypeInfoVarMap1),
	map__merge(TypeInfoVarMap1, CallerTypeInfoVarMap,
		TypeInfoVarMap),

	proc_info_set_typeinfo_varmap(NewProcInfo3,
		TypeInfoVarMap, NewProcInfo4),

	%
	% Fix up the argument vars, types and modes.
	%

	in_mode(InMode),
	list__length(ExtraTypeInfoVars, NumTypeInfos),
	list__duplicate(NumTypeInfos, InMode, ExtraTypeInfoModes),
	list__append(ExtraTypeInfoVars, HeadVars1, HeadVars),
	list__append(ExtraTypeInfoModes, ArgModes1, ArgModes),
	proc_info_set_headvars(NewProcInfo4, HeadVars, NewProcInfo5),
	proc_info_set_argmodes(NewProcInfo5, ArgModes, NewProcInfo6),

	proc_info_vartypes(NewProcInfo6, VarTypes6),
	map__apply_to_list(HeadVars, VarTypes6, ArgTypes),
	pred_info_set_arg_types(NewPredInfo0, TypeVarSet,
		ExistQVars, ArgTypes, NewPredInfo1),
	pred_info_set_typevarset(NewPredInfo1, TypeVarSet, NewPredInfo2),

	%
	% Fix up the typeclass_info_varmap. Apply the substitutions
	% to the types in the original typeclass_info_varmap, then add in
	% the extra typeclass_info variables required by --typeinfo-liveness.
	%
	proc_info_typeclass_info_varmap(NewProcInfo6, TCVarMap0),
	apply_substitutions_to_typeclass_var_map(TCVarMap0, TypeRenaming,
		TypeSubn, EmptyVarRenaming, TCVarMap1),
	add_extra_typeclass_infos(HeadVars, ArgTypes, TCVarMap1, TCVarMap),
	proc_info_set_typeclass_info_varmap(NewProcInfo6,
		TCVarMap, NewProcInfo7),

	%
	% Find the new class context by searching the argument types
	% for typeclass_infos (the corresponding constraint is encoded
	% in the type of a typeclass_info).
	%
	find_class_context(ModuleInfo0, ArgTypes, ArgModes,
		[], [], ClassContext),
	pred_info_set_class_context(NewPredInfo2, ClassContext, NewPredInfo3),

	%
	% Run traverse_goal to specialize based on the new information.
	%
	proc_info_goal(NewProcInfo7, Goal1),
	HOInfo0 = info(PredVars, Requests0, NewPredMap1, NewPredProcId,
		NewPredInfo2, NewProcInfo6, ModuleInfo0, Params, unchanged),
        traverse_goal_0(Goal1, Goal2, HOInfo0,
		info(_, Requests1,_,_,_,_,_,_,_)),
	goal_size(Goal2, GoalSize),
	map__set(GoalSizes0, NewPredId, GoalSize, GoalSizes1),

	%
	% Requantify and recompute instmap deltas.
	%
	proc_info_varset(NewProcInfo7, Varset7),
	proc_info_vartypes(NewProcInfo7, VarTypes7),
	implicitly_quantify_clause_body(HeadVars, Goal2, Varset7, VarTypes7,
					Goal3, Varset, VarTypes, _),
	proc_info_get_initial_instmap(NewProcInfo3, ModuleInfo0, InstMap0),
	recompute_instmap_delta(no, Goal3, Goal4, InstMap0,
		ModuleInfo0, ModuleInfo1),

	proc_info_set_goal(NewProcInfo7, Goal4, NewProcInfo8),
	proc_info_set_varset(NewProcInfo8, Varset, NewProcInfo9),
	proc_info_set_vartypes(NewProcInfo9, VarTypes, NewProcInfo),
	map__det_insert(NewProcs0, NewProcId, NewProcInfo, NewProcs),
	pred_info_set_procedures(NewPredInfo3, NewProcs, NewPredInfo),
	map__det_update(Preds0, NewPredId, NewPredInfo, Preds),
	predicate_table_set_preds(PredTable0, Preds, PredTable),
	module_info_set_predicate_table(ModuleInfo1, PredTable, ModuleInfo2),
	create_specialized_versions(Params, NewPreds, NewPredMap1,
		NewPredMap, Requests1, Requests, GoalSizes1, GoalSizes,
		ModuleInfo2, ModuleInfo).

		% Returns a list of hlds_goals which construct the list of
		% higher order arguments which have been specialized. Traverse
		% goal will then recognize these as having a unique possible
		% value and will specialize any calls involving them.
		% Takes an original list of headvars and arg_modes and
		% returns these with curried arguments added.
		% The old higher-order arguments are left in. They may be
		% needed in calls which could not be specialised. If not,
		% unused_args.m can clean them up.
		% The predicate is recursively applied to all curried
		% higher order arguments of higher order arguments.
		% This also builds the initial pred_vars map which records
		% higher-order and type_info constants for a call to
		% traverse_goal, and a var-var renaming from the requesting
		% call's arguments to the headvars of this predicate.
:- pred construct_higher_order_terms(module_info::in, list(var)::in, 
		list(var)::out, list(mode)::in, list(mode)::out,
		list(higher_order_arg)::in, proc_info::in, proc_info::out,
		map(var, var)::in, map(var, var)::out,
		pred_vars::in, pred_vars::out) is det.

construct_higher_order_terms(_, HeadVars, HeadVars, ArgModes, ArgModes,
		[], ProcInfo, ProcInfo, Renaming, Renaming,
		PredVars, PredVars).
construct_higher_order_terms(ModuleInfo, HeadVars0, HeadVars, ArgModes0,
		ArgModes, [HOArg | HOArgs], ProcInfo0, ProcInfo,
		Renaming0, Renaming, PredVars0, PredVars) :-
	HOArg = higher_order_arg(ConsId, Index, NumArgs,
		CurriedArgs, CurriedArgTypes, CurriedHOArgs),

	list__index1_det(HeadVars0, Index, LVar),
	(
		( ConsId = pred_const(PredId, ProcId)
		; ConsId = code_addr_const(PredId, ProcId)
		)
	->
		% Add the curried arguments to the procedure's argument list.
		module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_CalledPredInfo, CalledProcInfo),
		proc_info_argmodes(CalledProcInfo, CalledArgModes),
		( list__take(NumArgs, CalledArgModes, CurriedArgModes0) ->
			CurriedArgModes1 = CurriedArgModes0
		;
			error("list__split_list_failed")
		)
	;
		in_mode(InMode),
		list__duplicate(NumArgs, InMode, CurriedArgModes1)
	),

	proc_info_create_vars_from_types(ProcInfo0, CurriedArgTypes,
		NewHeadVars0, ProcInfo1),

	% Make traverse_goal pretend that the input higher-order argument is
	% built using the new arguments as its curried arguments.
	map__det_insert(PredVars0, LVar,
		constant(ConsId, NewHeadVars0), PredVars1),

	assoc_list__from_corresponding_lists(CurriedArgs,
		NewHeadVars0, CurriedRenaming),
	list__foldl(lambda([VarPair::in, Map0::in, Map::out] is det, (
			VarPair = Var1 - Var2,
			map__set(Map0, Var1, Var2, Map)
		)), CurriedRenaming, Renaming0, Renaming1),

	% Recursively construct the curried higher-order arguments.
	construct_higher_order_terms(ModuleInfo, NewHeadVars0, NewHeadVars,
		CurriedArgModes1, CurriedArgModes, CurriedHOArgs,
		ProcInfo1, ProcInfo2, Renaming1, Renaming2,
		PredVars1, PredVars2),

	% Fix up the argument lists.
	list__append(ArgModes0, CurriedArgModes, ArgModes1),
	list__append(HeadVars0, NewHeadVars, HeadVars1),

	construct_higher_order_terms(ModuleInfo, HeadVars1, HeadVars, ArgModes1,
		ArgModes, HOArgs, ProcInfo2, ProcInfo,
		Renaming2, Renaming, PredVars2, PredVars).

%-----------------------------------------------------------------------------%

	% Substitute the types in a higher_order_arg.
:- pred substitute_higher_order_arg(tsubst::in, higher_order_arg::in, 
		higher_order_arg::out) is det.

substitute_higher_order_arg(Subn, HOArg0, HOArg) :-
	HOArg0 = higher_order_arg(A, B, C, D,
		CurriedArgTypes0, CurriedHOArgs0),
	term__apply_rec_substitution_to_list(CurriedArgTypes0,
		Subn, CurriedArgTypes),
	list__map(substitute_higher_order_arg(Subn),
		CurriedHOArgs0, CurriedHOArgs),
	HOArg = higher_order_arg(A, B, C, D,
		CurriedArgTypes, CurriedHOArgs).

%-----------------------------------------------------------------------------%

	% Collect the list of class_constraints from the list of argument
	% types. The typeclass_info for universal constraints is input,
	% output for existential constraints.
:- pred find_class_context(module_info::in, list(type)::in, list(mode)::in,
	list(class_constraint)::in, list(class_constraint)::in,
	class_constraints::out) is det.

find_class_context(_, [], [], Univ0, Exist0, Constraints) :-
	list__reverse(Univ0, Univ),
	list__reverse(Exist0, Exist),
	Constraints = constraints(Univ, Exist).
find_class_context(_, [], [_|_], _, _, _) :-
	error("higher_order:find_class_context").
find_class_context(_, [_|_], [], _, _, _) :-
	error("higher_order:find_class_context").
find_class_context(ModuleInfo, [Type | Types], [Mode | Modes],
		Univ0, Exist0, Constraints) :-
	( polymorphism__typeclass_info_class_constraint(Type, Constraint) ->
		( mode_is_input(ModuleInfo, Mode) ->
			maybe_add_constraint(Univ0, Constraint, Univ),
			Exist = Exist0
		;
			maybe_add_constraint(Exist0, Constraint, Exist),
			Univ = Univ0
		)
	;
		Univ = Univ0,
		Exist = Exist0
	),
	find_class_context(ModuleInfo, Types, Modes, Univ, Exist, Constraints).

:- pred maybe_add_constraint(list(class_constraint)::in,
		class_constraint::in, list(class_constraint)::out) is det.

maybe_add_constraint(Constraints0, Constraint0, Constraints) :-
	Constraint0 = constraint(ClassName, Types0),
	strip_term_contexts(Types0, Types),
	Constraint = constraint(ClassName, Types),
	(
		% Remove duplicates.
		\+ list__member(Constraint, Constraints0)
	->
		Constraints = [Constraint | Constraints0]	
	;
		Constraints = Constraints0		
	).

%-----------------------------------------------------------------------------%

	% Make sure that the typeclass_infos required by `--typeinfo-liveness'
	% are in the typeclass_info_varmap.
:- pred add_extra_typeclass_infos(list(var)::in, list(type)::in,
		map(class_constraint, var)::in,
		map(class_constraint, var)::out) is det.

add_extra_typeclass_infos(Vars, Types, TCVarMap0, TCVarMap) :-
	( add_extra_typeclass_infos_2(Vars, Types, TCVarMap0, TCVarMap1) ->
		TCVarMap = TCVarMap1
	;
		error("higher_order:add_extra_typeclass_infos")
	).
		
:- pred add_extra_typeclass_infos_2(list(var)::in, list(type)::in,
		map(class_constraint, var)::in,
		map(class_constraint, var)::out) is semidet.

add_extra_typeclass_infos_2([], [], TCVarMap, TCVarMap).
add_extra_typeclass_infos_2([Var | Vars], [Type0 | Types],
		TCVarMap0, TCVarMap) :-
	strip_term_context(Type0, Type),
	( polymorphism__typeclass_info_class_constraint(Type, Constraint) ->
		map__set(TCVarMap0, Constraint, Var, TCVarMap1)
	;
		TCVarMap1 = TCVarMap0
	),
	add_extra_typeclass_infos(Vars, Types, TCVarMap1, TCVarMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
