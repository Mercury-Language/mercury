%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
:- module transform_hlds__higher_order.
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

:- import_module hlds__hlds_module.
:- import_module io.

:- pred specialize_higher_order(module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-------------------------------------------------------------------------------

:- implementation.

:- import_module hlds__hlds_pred, hlds__hlds_goal, hlds__hlds_data.
:- import_module hlds__instmap, (parse_tree__inst).
:- import_module ll_backend__code_util, libs__globals, check_hlds__mode_util.
:- import_module hlds__goal_util.
:- import_module check_hlds__type_util, libs__options, parse_tree__prog_data.
:- import_module parse_tree__prog_out, hlds__quantification.
:- import_module parse_tree__mercury_to_mercury, transform_hlds__inlining.
:- import_module check_hlds__polymorphism, parse_tree__prog_util.
:- import_module hlds__special_pred, check_hlds__unify_proc, hlds__passes_aux.

:- import_module assoc_list, bool, char, int, list, map, require, set.
:- import_module std_util, string, varset, term.

	% Iterate collecting requests and processing them until there
	% are no more requests remaining.
specialize_higher_order(ModuleInfo0, ModuleInfo) -->
	globals__io_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, optimize_higher_order,
		HigherOrder) },
	{ globals__lookup_bool_option(Globals, type_specialization,
		TypeSpec) },
	{ globals__lookup_bool_option(Globals, user_guided_type_specialization,
		UserTypeSpec) },
	{ globals__lookup_int_option(Globals, higher_order_size_limit,
		SizeLimit) },
	{ globals__lookup_int_option(Globals, higher_order_arg_limit,
		ArgLimit) },
	{ Params = ho_params(HigherOrder, TypeSpec,
		UserTypeSpec, SizeLimit, ArgLimit) },
	{ map__init(NewPreds0) },
	{ NextHOid0 = 1 },
	{ map__init(GoalSizes0) },
	{ set__init(Requests0) },
	{ map__init(VersionInfo0) },
	{ Info0 = higher_order_global_info(Requests0, NewPreds0, VersionInfo0,
			ModuleInfo0, GoalSizes0, Params, NextHOid0) },

	{ module_info_predids(ModuleInfo0, PredIds0) },
	{ module_info_type_spec_info(ModuleInfo0,
		type_spec_info(_, UserSpecPreds, _, _)) },

	%
	% Make sure the user requested specializations are processed first,
	% since we don't want to create more versions if one of these
	% matches. We need to process these even if specialization is
	% not being performed in case any of the specialized versions
	% are called from other modules.
	%
	( { set__empty(UserSpecPreds) } ->
		{ PredIds = PredIds0 },
		{ UserSpecPredList = [] },
		{ Info3 = Info0 }
	;
		{ set__list_to_set(PredIds0, PredIdSet0) },
		{ set__difference(PredIdSet0, UserSpecPreds, PredIdSet) },
		{ set__to_sorted_list(PredIdSet, PredIds) },

		{ set__to_sorted_list(UserSpecPreds, UserSpecPredList) },
		{ Info1 = Info0 ^ ho_params ^ user_type_spec := yes },
		{ list__foldl(get_specialization_requests, UserSpecPredList,
			Info1, Info2) },
		process_requests(Info2, Info3)
	),

	( { bool__or_list([HigherOrder, TypeSpec, UserTypeSpec], yes) } ->

		%
		% Process all other specializations until no more requests
		% are generated.
		%
		{ list__foldl(get_specialization_requests, PredIds,
			Info3, Info4) },
		recursively_process_requests(Info4, Info)
	;
		{ Info = Info3 }
	),

	% Remove the predicates which were used to force the production of
	% user-requested type specializations, since they are not called
	% from anywhere and are no longer needed.
	{ list__foldl(module_info_remove_predicate,
		UserSpecPredList, Info ^ module_info, ModuleInfo) }.

	% Process one lot of requests, returning requests for any
	% new specializations made possible by the first lot.
:- pred process_requests(higher_order_global_info::in,
		higher_order_global_info::out,
		io__state::di, io__state::uo) is det.

process_requests(Info0, Info) -->
	filter_requests(Requests, LoopRequests, Info0, Info1),
	( { Requests = [] } ->
		{ Info = Info1 }
	;
		{ set__init(PredProcsToFix0) },
		create_new_preds(Requests, [], NewPredList,
			PredProcsToFix0, PredProcsToFix1, Info1, Info2),
		{ list__foldl(check_loop_request(Info2), LoopRequests,
			PredProcsToFix1, PredProcsToFix) },
		{ set__to_sorted_list(PredProcsToFix, PredProcs) },
		{ fixup_specialized_versions(NewPredList, Info2, Info3) },
		{ fixup_preds(PredProcs, Info3, Info4) },
		{ NewPredList \= [] ->
			% The dependencies have changed, so the
			% dependency graph needs to rebuilt for
			% inlining to work properly.
			module_info_clobber_dependency_info(
				Info4 ^ module_info,
				ModuleInfo),
			Info = Info4 ^ module_info := ModuleInfo
		;
			Info = Info4
		}
	).

	% Process requests until there are no new requests to process.
:- pred recursively_process_requests(higher_order_global_info::in, 
	higher_order_global_info::out, io__state::di, io__state::uo) is det.

recursively_process_requests(Info0, Info) -->
	( { set__empty(Info0 ^ requests) } ->
		{ Info = Info0 }
	;
		process_requests(Info0, Info1),
		recursively_process_requests(Info1, Info)
	).

%-------------------------------------------------------------------------------

:- type higher_order_global_info
	---> higher_order_global_info(
		requests :: set(request),	% Requested versions.
		new_preds :: new_preds,
						% Specialized versions for
						% each predicate not changed
						% by traverse_goal
		version_info :: map(pred_proc_id, version_info),
						% Extra information about
						% each specialized version.
		module_info :: module_info,
		goal_sizes :: goal_sizes,
		ho_params :: ho_params,
		next_higher_order_id :: int	% Number identifying
						% a specialized version.
	).

	% used while traversing goals
:- type higher_order_info
	---> higher_order_info(
		global_info :: higher_order_global_info,
		pred_vars :: pred_vars,		% higher_order variables
		pred_proc_id :: pred_proc_id,
				% pred_proc_id of goal being traversed
		pred_info :: pred_info,
				% pred_info of goal being traversed
		proc_info :: proc_info,
				% proc_info of goal being traversed
		changed :: changed
	).

:- type request
	---> request(
		pred_proc_id,			% calling pred
		pred_proc_id,			% called pred
		list(prog_var),			% call args
		list(tvar),			% type variables for which
						% extra type-infos must be
						% passed from the caller if
						% --typeinfo-liveness is set.
		list(higher_order_arg),
		list(type),			% argument types in caller
		bool,				% should the interface of
						% the specialized procedure
						% use typeinfo liveness.
		tvarset,			% caller's typevarset.
		bool,				% is this a user-requested
						% specialization
		context				% context of the call which
						% caused the request to be
						% generated
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
		list(prog_var),		% curried arguments in caller
		list(type),		% curried argument types in caller
		list(higher_order_arg),	% higher-order curried arguments
					% with known values
		bool			% is this higher_order_arg a constant
	).

:- type goal_sizes == map(pred_id, int). 	%stores the size of each
				% predicate's goal used in the heuristic
				% to decide which preds are specialized

	% Used to hold the value of known higher order variables.
	% If a variable is not in the map, it does not have a value yet.
:- type pred_vars == map(prog_var, maybe_const).

:- type new_preds == map(pred_proc_id, set(new_pred)).

	% The list of vars is a list of the curried arguments, which must
	% be explicitly passed to the specialized predicate.
	% For cons_ids other than pred_const and `type_info', the arguments
	% must be constants. For pred_consts and type_infos, non-constant
	% arguments are passed through to any specialised version.
:- type maybe_const --->
		constant(cons_id, list(prog_var))
						% unique possible value
	;	multiple_values			% multiple possible values,
						% cannot specialise.
	.

:- type ho_params
	---> ho_params(
		optimize_higher_order :: bool,	
				% Propagate higher-order constants.
		type_spec :: bool,
				% Propagate type-info constants.
		user_type_spec :: bool,
				% User-guided type specialization.
		size_limit :: int,
				% Size limit on requested version.
		arg_limit :: int
				% The maximum size of the
				% higher-order arguments of
				% a specialized version.
	).

:- type version_info
	---> version_info(
		pred_proc_id,
				% The procedure from the original program
				% from which this version was created.

		int,		% Depth of the higher_order_args for
				% this version.
		pred_vars,
				% Higher-order or constant input variables
				% for a specialised version.
		list(parent_version_info)
				% The chain of specialized versions which
				% caused this version to be created.
				% For each element in the list with the
				% same pred_proc_id, the depth must decrease.
				% This ensures that the specialization
				% process must terminate.
	).

:- type parent_version_info
	---> parent_version_info(
		pred_proc_id,	% The procedure from the original program
				% from which this parent was created.
		int		% Depth of the higher_order_args for
				% this version.
	).

:- type new_pred
	---> new_pred(
		pred_proc_id,		% version pred_proc_id
		pred_proc_id,		% old pred_proc_id
		pred_proc_id,		% requesting caller
		sym_name,		% name
		list(higher_order_arg),	% specialized args
		list(prog_var),		% unspecialised argument vars in caller
		list(tvar),		% extra typeinfo tvars in caller
		list(type),		% unspecialised argument types
					% in requesting caller
		bool,			% does the interface of the specialized
					% version use type-info liveness
		tvarset,		% caller's typevarset
		bool			% is this a user-specified type
					% specialization
	).

	% Returned by traverse_goal.
:- type changed
	--->	changed		% Need to requantify goal + check other procs
	;	request		% Need to check other procs
	;	unchanged.	% Do nothing more for this predicate

%-----------------------------------------------------------------------------%

:- pred get_specialization_requests(pred_id::in,
	higher_order_global_info::in, higher_order_global_info::out) is det.

get_specialization_requests(PredId, GlobalInfo0, GlobalInfo) :-
	module_info_pred_info(GlobalInfo0 ^ module_info, PredId, PredInfo0),
	pred_info_non_imported_procids(PredInfo0, NonImportedProcs),
	(
		NonImportedProcs = [],
		GlobalInfo = GlobalInfo0
	;
		NonImportedProcs = [ProcId | _],
		MustRecompute = no,
		list__foldl(traverse_proc(MustRecompute, PredId),
			NonImportedProcs, GlobalInfo0, GlobalInfo1),
		module_info_pred_proc_info(GlobalInfo1 ^ module_info,
			PredId, ProcId, _, ProcInfo),
		proc_info_goal(ProcInfo, Goal),
		goal_size(Goal, GoalSize),
		map__set(GlobalInfo1 ^ goal_sizes, PredId,
			GoalSize, GoalSizes),
		GlobalInfo = GlobalInfo1 ^ goal_sizes := GoalSizes
	).

		% This is called when the first procedure of a pred was
		% changed. It fixes up all the other procs, ignoring the
		% goal_size and requests that come out, since that information
		% has already been collected.
:- pred traverse_proc(bool::in, pred_id::in, proc_id::in,
	higher_order_global_info::in, higher_order_global_info::out) is det.

traverse_proc(MustRecompute, PredId, ProcId, GlobalInfo0, GlobalInfo) :-
	map__init(PredVars0),
	module_info_pred_proc_info(GlobalInfo0 ^ module_info,
		PredId, ProcId, PredInfo0, ProcInfo0),
	Info0 = higher_order_info(GlobalInfo0, PredVars0,
			proc(PredId, ProcId), PredInfo0, ProcInfo0, unchanged),
	traverse_goal(MustRecompute, Info0, Info),
	Info = higher_order_info(GlobalInfo1, _, _, PredInfo, ProcInfo, _),
	module_info_set_pred_proc_info(GlobalInfo1 ^ module_info,
		PredId, ProcId, PredInfo, ProcInfo, ModuleInfo),
	GlobalInfo = GlobalInfo1 ^ module_info := ModuleInfo.

%-------------------------------------------------------------------------------
	% Goal traversal

:- pred traverse_goal(bool::in, higher_order_info::in,
		higher_order_info::out) is det.

traverse_goal(MustRecompute, Info0, Info) :-
	VersionInfoMap = Info0 ^ global_info ^ version_info,

	% Lookup the initial known bindings of the variables if this
	% procedure is a specialised version.
	(
		map__search(VersionInfoMap, Info0 ^ pred_proc_id,
			version_info(_, _, PredVars, _))
	->
		Info1 = Info0 ^ pred_vars := PredVars
	;
		Info1 = Info0
	),
	proc_info_goal(Info0 ^ proc_info, Goal0),
	traverse_goal_2(Goal0, Goal, Info1, Info2),
	fixup_proc_info(MustRecompute, Goal, Info2, Info).

:- pred fixup_proc_info(bool::in, hlds_goal::in,
		higher_order_info::in, higher_order_info::out) is det.

fixup_proc_info(MustRecompute, Goal0, Info0, Info) :-
	( (Info0 ^ changed = changed ; MustRecompute = yes) ->
		ModuleInfo0 = Info0 ^ global_info ^ module_info,
		ProcInfo0 = Info0 ^ proc_info,
		proc_info_set_goal(ProcInfo0, Goal0, ProcInfo1),
		requantify_proc(ProcInfo1, ProcInfo2),
		proc_info_goal(ProcInfo2, Goal2),
		RecomputeAtomic = no,
		proc_info_get_initial_instmap(ProcInfo2, ModuleInfo0, InstMap),
		proc_info_vartypes(ProcInfo2, VarTypes),
		proc_info_inst_varset(ProcInfo2, InstVarSet),
		recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3,
			VarTypes, InstVarSet, InstMap, ModuleInfo0, ModuleInfo),
		proc_info_set_goal(ProcInfo2, Goal3, ProcInfo),
		Info = (Info0 ^ proc_info := ProcInfo)
				^ global_info ^ module_info := ModuleInfo
	;
		Info = Info0
	).

	% Traverses the goal collecting higher order variables for which
	% the value is known, and specializing calls and adding
	% specialization requests to the request_info structure.
	% The first time through the only predicate we can specialize
	% is call/N. The pred_proc_id is that of the current procedure,
	% used to find out which procedures need fixing up later.
:- pred traverse_goal_2(hlds_goal::in, hlds_goal::out,
	higher_order_info::in, higher_order_info::out) is det.

traverse_goal_2(conj(Goals0) - Info, conj(Goals) - Info) -->
	list__map_foldl(traverse_goal_2, Goals0, Goals).

traverse_goal_2(par_conj(Goals0) - Info, par_conj(Goals) - Info) -->
		% traverse_disj treats its list of goals as independent
		% rather than specifically disjoint, so we can use it
		% to process a list of independent parallel conjuncts.
	traverse_disj(Goals0, Goals).

traverse_goal_2(disj(Goals0) - Info, disj(Goals) - Info) -->
	traverse_disj(Goals0, Goals).

		% a switch is treated as a disjunction
traverse_goal_2(switch(Var, CanFail, Cases0) - Info,
		switch(Var, CanFail, Cases) - Info) -->
	traverse_cases(Cases0, Cases).

		% check whether this call could be specialized
traverse_goal_2(Goal0, Goal) -->
	{ Goal0 = generic_call(GenericCall, Args, _, _) - GoalInfo },
	(
		{
			GenericCall = higher_order(Var, _, _),
			MaybeMethod = no
		;
			GenericCall = class_method(Var, Method, _, _),
			MaybeMethod = yes(Method)
		}
	->
		maybe_specialize_higher_order_call(Var, MaybeMethod,
			Args, Goal0, Goals),
		{ conj_list_to_goal(Goals, GoalInfo, Goal) }
	;
		{ Goal = Goal0 }
	).

		% check whether this call could be specialized
traverse_goal_2(Goal0, Goal) -->
	{ Goal0 = call(_,_,_,_,_,_) - _ },
	maybe_specialize_call(Goal0, Goal).

		% if-then-elses are handled as disjunctions
traverse_goal_2(Goal0, Goal) -->
	{ Goal0 = if_then_else(Vars, Cond0, Then0, Else0) - GoalInfo },
	get_pre_branch_info(PreInfo),
	traverse_goal_2(Cond0, Cond),
	traverse_goal_2(Then0, Then),
	get_post_branch_info(PostThenInfo),
	set_pre_branch_info(PreInfo),
	traverse_goal_2(Else0, Else),
	get_post_branch_info(PostElseInfo),
	{ Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo },
	{ merge_post_branch_infos(PostThenInfo, PostElseInfo, PostInfo) },
	set_post_branch_info(PostInfo).

traverse_goal_2(not(NegGoal0) - Info, not(NegGoal) - Info) -->
	traverse_goal_2(NegGoal0, NegGoal).

traverse_goal_2(some(Vars, CanRemove, Goal0) - Info,
		some(Vars, CanRemove, Goal) - Info) -->
	traverse_goal_2(Goal0, Goal).

traverse_goal_2(Goal, Goal) -->
	{ Goal = foreign_proc(_, _, _, _, _, _, _) - _ }.

traverse_goal_2(Goal0, Goal) -->
	{ Goal0 = GoalExpr0 - _ },
	{ GoalExpr0 = unify(_, _, _, Unify0, _) },
	(
		{ Unify0 = construct(_, pred_const(_, _, _), _, _, _, _, _) }
	->
		maybe_specialize_pred_const(Goal0, Goal)
	;
		{ Goal = Goal0 }
	),
	( { Goal = unify(_, _, _, Unify, _) - _ } ->
		check_unify(Unify)
	;
		[]
	).

traverse_goal_2(shorthand(_) - _, _) -->
	% these should have been expanded out by now
	{ error("traverse_goal_2: unexpected shorthand") }.

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
	get_pre_branch_info(PreInfo),
	traverse_goal_2(Goal0, Goal),
	get_post_branch_info(PostInfo0),
	traverse_disj_2(PreInfo, Goals0, Goals, PostInfo0, PostInfo),
	set_post_branch_info(PostInfo).

:- pred traverse_disj_2(pre_branch_info::in, hlds_goals::in, hlds_goals::out,
	post_branch_info::in, post_branch_info::out,
	higher_order_info::in, higher_order_info::out) is det.

traverse_disj_2(_, [], [], PostInfo, PostInfo) --> [].
traverse_disj_2(PreInfo, [Goal0 | Goals0], [Goal | Goals],
		PostInfo0, PostInfo) -->
	set_pre_branch_info(PreInfo),
	traverse_goal_2(Goal0, Goal),
	get_post_branch_info(PostInfo1),
	{ merge_post_branch_infos(PostInfo0, PostInfo1, PostInfo2) },
	traverse_disj_2(PreInfo, Goals0, Goals,
		PostInfo2, PostInfo).

		% Switches are treated in exactly the same way as disjunctions.
:- pred traverse_cases(list(case)::in, list(case)::out,
	higher_order_info::in, higher_order_info::out) is det.

traverse_cases([], []) --> [].
traverse_cases([case(ConsId, Goal0) | Cases0],
		[case(ConsId, Goal) | Cases]) -->
	get_pre_branch_info(PreInfo),
	traverse_goal_2(Goal0, Goal),
	get_post_branch_info(PostInfo0),
	traverse_cases_2(PreInfo, Cases0, Cases, PostInfo0, PostInfo),
	set_post_branch_info(PostInfo).

:- pred traverse_cases_2(pre_branch_info::in, list(case)::in, list(case)::out,
	post_branch_info::in, post_branch_info::out,
	higher_order_info::in, higher_order_info::out) is det.

traverse_cases_2(_, [], [], PostInfo, PostInfo) --> [].
traverse_cases_2(PreInfo, [Case0 | Cases0], [Case | Cases],
		PostInfo0, PostInfo) -->
	set_pre_branch_info(PreInfo),
	{ Case0 = case(ConsId, Goal0) },
	traverse_goal_2(Goal0, Goal),
	{ Case = case(ConsId, Goal) },
	get_post_branch_info(PostInfo1),
	{ merge_post_branch_infos(PostInfo0, PostInfo1, PostInfo2) },
	traverse_cases_2(PreInfo, Cases0, Cases, PostInfo2, PostInfo).

:- type pre_branch_info == pred_vars.
:- type post_branch_info == pred_vars.

:- pred get_pre_branch_info(pre_branch_info::out,
		higher_order_info::in, higher_order_info::out) is det.

get_pre_branch_info(Info ^ pred_vars, Info, Info).

:- pred set_pre_branch_info(pre_branch_info::in,
		higher_order_info::in, higher_order_info::out) is det.

set_pre_branch_info(PreInfo, Info, Info ^ pred_vars := PreInfo).

:- pred get_post_branch_info(pre_branch_info::out,
		higher_order_info::in, higher_order_info::out) is det.

get_post_branch_info(Info ^ pred_vars, Info, Info).

:- pred set_post_branch_info(post_branch_info::in,
		higher_order_info::in, higher_order_info::out) is det.

set_post_branch_info(PostInfo, Info, Info ^ pred_vars := PostInfo).

	% This is used in traversing disjunctions. We save the initial
	% accumulator, then traverse each disjunct starting with the initial
	% info. We then merge the resulting infos.
:- pred merge_post_branch_infos(post_branch_info::in, post_branch_info::in,
					post_branch_info::out) is det.

merge_post_branch_infos(PredVars1, PredVars2, PredVars) :-
	map__to_assoc_list(PredVars1, PredVarList1),
	map__to_assoc_list(PredVars2, PredVarList2),
	merge_pred_var_lists(PredVarList1, PredVarList2, PredVarList),
	map__from_assoc_list(PredVarList, PredVars).

		% find out which variables after a disjunction cannot
		% be specialized
:- pred merge_pred_var_lists(assoc_list(prog_var, maybe_const)::in,
			assoc_list(prog_var, maybe_const)::in,
			assoc_list(prog_var, maybe_const)::out) is det.

merge_pred_var_lists([], List, List).
merge_pred_var_lists([PredVar | PredVars], List2, MergedList) :-
	merge_pred_var_with_list(PredVar, List2, MergedList1),
	merge_pred_var_lists(PredVars, MergedList1, MergedList).

:- pred merge_pred_var_with_list(pair(prog_var, maybe_const)::in,
			assoc_list(prog_var, maybe_const)::in,
			assoc_list(prog_var, maybe_const)::out) is det.

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
check_unify(deconstruct(_, _, _, _, _, _)) --> [].

check_unify(construct(LVar, ConsId, Args, _Modes, _, _, _), Info0, Info) :-
	( is_interesting_cons_id(Info0 ^ global_info ^ ho_params, ConsId) ->
		( map__search(Info0 ^ pred_vars, LVar, Specializable) ->
			(
				% we can't specialize calls involving
				% a variable with more than one
				% possible value
				Specializable = constant(_, _),
				map__det_update(Info0 ^ pred_vars, LVar,
					multiple_values, PredVars),
				Info = Info0 ^ pred_vars := PredVars
			;
				% if a variable is already
				% non-specializable, it can't become
				% specializable
				Specializable = multiple_values,
				Info = Info0
			)
		;
			map__det_insert(Info0 ^ pred_vars, LVar,
				constant(ConsId, Args), PredVars),
			Info = Info0 ^ pred_vars := PredVars
		)
	;
		Info = Info0
	).

check_unify(complicated_unify(_, _, _)) -->
	{ error("higher_order:check_unify - complicated unification") }.

:- pred is_interesting_cons_id(ho_params::in, cons_id::in) is semidet.

is_interesting_cons_id(Params, cons(qualified(Module, Name), _)) :-
	yes = Params ^ user_type_spec,
	mercury_private_builtin_module(Module),
	( Name = "type_info"
	; Name = "typeclass_info"
	).
is_interesting_cons_id(Params, pred_const(_, _, _)) :-
	yes = Params ^ optimize_higher_order.
is_interesting_cons_id(Params,
		type_ctor_info_const(_, _, _)) :-
	yes = Params ^ user_type_spec.
is_interesting_cons_id(Params,
		base_typeclass_info_const(_, _, _, _)) :-
	yes = Params ^ user_type_spec.

	% We need to keep track of int_consts so we can interpret
	% superclass_info_from_typeclass_info and typeinfo_from_typeclass_info.
	% We don't specialize based on them.
is_interesting_cons_id(Params, int_const(_)) :-
	yes = Params ^ user_type_spec.

	% Process a higher-order call or class_method_call to see if it
	% could possibly be specialized.
:- pred maybe_specialize_higher_order_call(prog_var::in, maybe(int)::in,
	list(prog_var)::in, hlds_goal::in, list(hlds_goal)::out,
	higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_higher_order_call(PredVar, MaybeMethod, Args,
		Goal0 - GoalInfo, Goals, Info0, Info) :-

	ModuleInfo = Info0 ^ global_info ^ module_info,

	% We can specialize calls to call/N and class_method_call/N if
	% the closure or typeclass_info has a known value.
	(
		map__search(Info0 ^ pred_vars, PredVar,
			constant(ConsId, CurriedArgs)),
		(
			ConsId = pred_const(PredId0, ProcId0, _),
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
			map__search(Info0 ^ pred_vars, BaseTypeClassInfo,
				constant(BaseConsId, _)),
			BaseConsId = base_typeclass_info_const(_,
				ClassId, Instance, _),
			MaybeMethod = yes(Method),
			module_info_instances(ModuleInfo, Instances),
			map__lookup(Instances, ClassId, InstanceList),
			list__index1_det(InstanceList, Instance, InstanceDefn),
			InstanceDefn = hlds_instance_defn(_, _, _,
				InstanceConstraints, InstanceTypes0, _,
				yes(ClassInterface), _, _),
			term__vars_list(InstanceTypes0, InstanceTvars),
			get_unconstrained_tvars(InstanceTvars,
				InstanceConstraints, UnconstrainedTVars),
			NumArgsToExtract = list__length(InstanceConstraints)
					+ list__length(UnconstrainedTVars),
			list__take(NumArgsToExtract, OtherTypeClassArgs,
				InstanceConstraintArgs)
		->
			list__index1_det(ClassInterface, Method,
				hlds_class_proc(PredId, ProcId)),
			list__append(InstanceConstraintArgs, Args, AllArgs)
		;
			fail
		)
	->
		construct_specialized_higher_order_call(PredId, ProcId,
			AllArgs, GoalInfo, Goal, Info0, Info),
		Goals = [Goal]
	;
		% Handle a class method call where we know which instance
		% is being used, but we haven't seen a construction for
		% the typeclass_info. This can happen for user-guided
		% typeclass specialization, because the type-specialized class
		% constraint is still in the constraint list, so a
		% typeclass_info is passed in by the caller rather than
		% being constructed locally.
		%
		% The problem is that in importing modules we don't know
		% which instance declarations are visible in the imported
		% module, so we don't know which class constraints are
		% redundant after type specialization.
		MaybeMethod = yes(Method),

		CallerProcInfo0 = Info0 ^ proc_info,
		CallerPredInfo0 = Info0 ^ pred_info,
		proc_info_vartypes(CallerProcInfo0, VarTypes),
		map__lookup(VarTypes, PredVar, TypeClassInfoType),
		polymorphism__typeclass_info_class_constraint(
			TypeClassInfoType, ClassConstraint),
		ClassConstraint = constraint(ClassName, ClassArgs),
		list__length(ClassArgs, ClassArity),
		module_info_instances(ModuleInfo, InstanceTable),
        	map__lookup(InstanceTable, class_id(ClassName, ClassArity),
			Instances),
		pred_info_typevarset(CallerPredInfo0, TVarSet0),
		find_matching_instance_method(Instances, Method,
			ClassArgs, PredId, ProcId, InstanceConstraints,
			UnconstrainedTVarTypes, TVarSet0, TVarSet)
	->
		pred_info_set_typevarset(CallerPredInfo0,
			TVarSet, CallerPredInfo),
		% Pull out the argument typeclass_infos.
		( InstanceConstraints = [], UnconstrainedTVarTypes = [] ->
			ExtraGoals = [],
			CallerProcInfo = CallerProcInfo0,
			AllArgs = Args
		;
			get_unconstrained_instance_type_infos(ModuleInfo,
				PredVar, UnconstrainedTVarTypes, 1,
				ArgTypeInfoGoals, ArgTypeInfoVars,
				CallerProcInfo0, CallerProcInfo1),
			FirstArgTypeclassInfo =
				list__length(UnconstrainedTVarTypes) + 1,
			get_arg_typeclass_infos(ModuleInfo, PredVar, 
				InstanceConstraints, FirstArgTypeclassInfo,
				ArgTypeClassInfoGoals, ArgTypeClassInfoVars,
				CallerProcInfo1, CallerProcInfo),
			list__condense(
				[ArgTypeInfoVars, ArgTypeClassInfoVars, Args],
				AllArgs),
			list__append(ArgTypeInfoGoals,
				ArgTypeClassInfoGoals, ExtraGoals)
		),
		Info1 = (Info0 ^ pred_info := CallerPredInfo)
				^ proc_info := CallerProcInfo,
		construct_specialized_higher_order_call(PredId, ProcId,
			AllArgs, GoalInfo, Goal, Info1, Info),
		list__append(ExtraGoals, [Goal], Goals)
	;
		% non-specializable call/N or class_method_call/N
		Goals = [Goal0 - GoalInfo],
		Info = Info0
	).

:- pred find_matching_instance_method(list(hlds_instance_defn)::in, int::in,
	list(type)::in, pred_id::out, proc_id::out,
	list(class_constraint)::out, list(type)::out,
	tvarset::in, tvarset::out) is semidet.

find_matching_instance_method([Instance | Instances], MethodNum,
		ClassTypes, PredId, ProcId, Constraints,
		UnconstrainedTVarTypes, TVarSet0, TVarSet) :-
        (
		instance_matches(ClassTypes, Instance,
			Constraints0, UnconstrainedTVarTypes0,
			TVarSet0, TVarSet1)
	->
		TVarSet = TVarSet1,
		Constraints = Constraints0,
		UnconstrainedTVarTypes = UnconstrainedTVarTypes0,
		Instance = hlds_instance_defn(_, _, _, _,
			_, _, yes(ClassInterface), _, _),
		list__index1_det(ClassInterface, MethodNum,
			hlds_class_proc(PredId, ProcId))
	;
		find_matching_instance_method(Instances, MethodNum,
			ClassTypes, PredId, ProcId, Constraints,
			UnconstrainedTVarTypes, TVarSet0, TVarSet)
	).

:- pred instance_matches(list(type)::in, hlds_instance_defn::in,
	list(class_constraint)::out, list(type)::out,
	tvarset::in, tvarset::out) is semidet.

instance_matches(ClassTypes, Instance, Constraints, UnconstrainedTVarTypes,
		TVarSet0, TVarSet) :-
	Instance = hlds_instance_defn(_, _, _, Constraints0,
		InstanceTypes0, _, _, InstanceTVarSet, _),
	varset__merge_subst(TVarSet0, InstanceTVarSet, TVarSet,
		RenameSubst),
	term__apply_substitution_to_list(InstanceTypes0,
		RenameSubst, InstanceTypes),
	apply_subst_to_constraint_list(RenameSubst,
		Constraints0, Constraints1),
	term__vars_list(InstanceTypes, InstanceTVars),
	get_unconstrained_tvars(InstanceTVars, Constraints1,
		UnconstrainedTVars0),

	type_list_subsumes(InstanceTypes, ClassTypes, Subst),
	apply_rec_subst_to_constraint_list(Subst,
		Constraints1, Constraints),

	term__var_list_to_term_list(UnconstrainedTVars0,
		UnconstrainedTVarTypes0),
	term__apply_rec_substitution_to_list(UnconstrainedTVarTypes0,
		Subst, UnconstrainedTVarTypes).

	% Build calls to
	% `private_builtin:instance_constraint_from_typeclass_info/3'
	% to extract the typeclass_infos for the constraints on an instance.
	% This simulates the action of `do_call_class_method' in
	% runtime/mercury_ho_call.c.
:- pred get_arg_typeclass_infos(module_info::in, prog_var::in,
		list(class_constraint)::in, int::in, list(hlds_goal)::out,
		list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_arg_typeclass_infos(ModuleInfo, TypeClassInfoVar,
		InstanceConstraints, Index, Goals, Vars,
		ProcInfo0, ProcInfo) :-

	MakeResultType = polymorphism__build_typeclass_info_type,
	get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
		"instance_constraint_from_typeclass_info", MakeResultType,
		InstanceConstraints, Index, Goals, Vars, ProcInfo0, ProcInfo).

	% Build calls to
	% `private_builtin:unconstrained_type_info_from_typeclass_info/3'
	% to extract the type-infos for the unconstrained type variables
	% of an instance declaration.
	% This simulates the action of `do_call_class_method' in
	% runtime/mercury_ho_call.c.
:- pred get_unconstrained_instance_type_infos(module_info::in,
		prog_var::in, list(type)::in, int::in, list(hlds_goal)::out,
		list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_unconstrained_instance_type_infos(ModuleInfo, TypeClassInfoVar,
		UnconstrainedTVarTypes, Index, Goals, Vars,
		ProcInfo0, ProcInfo) :-
	MakeResultType = polymorphism__build_type_info_type,
	get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
		"unconstrained_type_info_from_typeclass_info",
		MakeResultType, UnconstrainedTVarTypes,
		Index, Goals, Vars, ProcInfo0, ProcInfo).

:- pred get_typeclass_info_args(module_info::in, prog_var::in, string::in,
		pred(T, type)::(pred(in, out) is det),
		list(T)::in, int::in, list(hlds_goal)::out,
		list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args(ModuleInfo, TypeClassInfoVar, PredName, MakeResultType,
		Args, Index, Goals, Vars, ProcInfo0, ProcInfo) :-
	mercury_private_builtin_module(PrivateBuiltin),
	SymName = qualified(PrivateBuiltin, PredName),
	module_info_get_predicate_table(ModuleInfo, PredTable),
	(
		predicate_table_search_pred_sym_arity(PredTable,
			SymName, 3, [ExtractArgPredId0])
	->
		ExtractArgPredId = ExtractArgPredId0
	;
		string__append("higher_order.m: can't find private_builtin__",
			PredName, Msg),
		error(Msg)
	),
	hlds_pred__initial_proc_id(ExtractArgProcId),
	get_typeclass_info_args_2(TypeClassInfoVar, ExtractArgPredId,
		ExtractArgProcId, SymName, MakeResultType,
		Args, Index, Goals, Vars, ProcInfo0, ProcInfo).

:- pred get_typeclass_info_args_2(prog_var::in, pred_id::in, proc_id::in,
		sym_name::in, pred(T, type)::(pred(in, out) is det),
		list(T)::in, int::in, list(hlds_goal)::out,
		list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args_2(_, _, _, _, _, [], _, [], [], ProcInfo, ProcInfo).
get_typeclass_info_args_2(TypeClassInfoVar, PredId, ProcId, SymName,
		MakeResultType, [Arg | Args], Index,
		[IndexGoal, CallGoal | Goals],
		[ResultVar | Vars], ProcInfo0, ProcInfo) :-
	MakeResultType(Arg, ResultType),
	proc_info_create_var_from_type(ProcInfo0, ResultType, no,
		ResultVar, ProcInfo1),
	MaybeContext = no,
	make_int_const_construction(Index, no, IndexGoal,
		IndexVar, ProcInfo1, ProcInfo2),
	CallArgs = [TypeClassInfoVar, IndexVar, ResultVar],

	set__list_to_set(CallArgs, NonLocals),
	instmap_delta_init_reachable(InstMapDelta0),
	instmap_delta_insert(InstMapDelta0, ResultVar,
		ground(shared, none), InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, GoalInfo),
	CallGoal = call(PredId, ProcId, CallArgs, not_builtin,
		MaybeContext, SymName) - GoalInfo,
	get_typeclass_info_args_2(TypeClassInfoVar, PredId, ProcId, SymName,
		MakeResultType, Args, Index + 1, Goals, Vars,
		ProcInfo2, ProcInfo).

%-----------------------------------------------------------------------------%

:- pred construct_specialized_higher_order_call(pred_id::in, proc_id::in,
	list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
	higher_order_info::in, higher_order_info::out) is det.

construct_specialized_higher_order_call(PredId, ProcId,
		AllArgs, GoalInfo, Goal - GoalInfo, Info0, Info) :-
	ModuleInfo = Info0 ^ global_info ^ module_info,
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
	SymName = qualified(ModuleName, PredName),
	code_util__builtin_state(ModuleInfo, PredId, ProcId, Builtin),

	MaybeContext = no,
	Goal1 = call(PredId, ProcId, AllArgs, Builtin, MaybeContext, SymName),
	Info1 = Info0 ^ changed := changed,
	maybe_specialize_call(Goal1 - GoalInfo, Goal - _, Info1, Info).

:- pred maybe_specialize_call(hlds_goal::in, hlds_goal::out,
		higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_call(Goal0 - GoalInfo, Goal - GoalInfo, Info0, Info) :-
	ModuleInfo0 = Info0 ^ global_info ^ module_info,
	(
		Goal0 = call(_, _, _, _, _, _)
	->
		Goal0 = call(CalledPred, CalledProc, Args0, IsBuiltin,
			MaybeContext, _SymName0)
	;
		error("higher_order.m: call expected")
	),
	module_info_pred_proc_info(ModuleInfo0, CalledPred, CalledProc,
		CalleePredInfo, CalleeProcInfo),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, special_preds, HaveSpecialPreds),
	(
		% Look for calls to unify/2 and compare/3 which can
		% be specialized.
		specialize_special_pred(CalledPred, CalledProc, Args0,
			MaybeContext, GoalInfo, HaveSpecialPreds, Goal1,
			Info0, Info1)
	->
		Goal = Goal1,
		Info = Info1 ^ changed := changed
	;
		polymorphism__is_typeclass_info_manipulator(ModuleInfo0,
			CalledPred, Manipulator)
	->
		interpret_typeclass_info_manipulator(Manipulator, Args0,
			Goal0, Goal, Info0, Info)
	;
		(
			pred_info_is_imported(CalleePredInfo),
			module_info_type_spec_info(ModuleInfo0,
				type_spec_info(TypeSpecProcs, _, _, _)),
			\+ set__member(proc(CalledPred, CalledProc),
				TypeSpecProcs)
		;
			pred_info_is_pseudo_imported(CalleePredInfo),
			hlds_pred__in_in_unification_proc_id(CalledProc)
		;
			pred_info_pragma_goal_type(CalleePredInfo)
		)
	->
		Info = Info0,
		Goal = Goal0
	;
		CanRequest = yes,
		maybe_specialize_ordinary_call(CanRequest, CalledPred,
			CalledProc, CalleePredInfo, CalleeProcInfo, Args0,
			IsBuiltin, MaybeContext, GoalInfo, Result,
			Info0, Info),
		(
			Result = specialized(ExtraTypeInfoGoals, Goal1),
			goal_to_conj_list(Goal1 - GoalInfo, GoalList1),
			list__append(ExtraTypeInfoGoals, GoalList1, GoalList),
			Goal = conj(GoalList)
		;
			Result = not_specialized,
			Goal = Goal0
		)
	).

	%
	% Try to specialize constructions of higher-order terms.
	% This is useful if we don't have the code for predicates
	% to which this higher-order term is passed.
	%
	% The specialization is done by treating
	% 	Pred = foo(A, B, ...)
	% as
	%	pred(X::<mode1>, Y::<mode2>, ...) is <det> :-
	%		foo(A, B, ..., X, Y, ...)
	% and specializing the call. 
	%
:- pred maybe_specialize_pred_const(hlds_goal::in, hlds_goal::out,
		higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_pred_const(Goal0 - GoalInfo, Goal - GoalInfo) -->
	NewPreds =^ global_info ^ new_preds,
	ModuleInfo =^ global_info ^ module_info,
	ProcInfo0 =^ proc_info,
	( 
		{ Goal0 = unify(_, _, UniMode, Unify0, Context) },
		{ Unify0 = construct(LVar, ConsId0, Args0, _,
				HowToConstruct, CellIsUnique, MaybeExprn) },
		{ ConsId0 = pred_const(PredId, ProcId, EvalMethod) },
		{ map__contains(NewPreds, proc(PredId, ProcId)) },
		{ proc_info_vartypes(ProcInfo0, VarTypes0) },
		{ map__lookup(VarTypes0, LVar, LVarType) },
		{ type_is_higher_order(LVarType, _, _, ArgTypes) }
	->
		% Create variables to represent
		{ proc_info_create_vars_from_types(ProcInfo0,
			ArgTypes, UncurriedArgs, ProcInfo1) },
		{ list__append(Args0, UncurriedArgs, Args1) },
		^ proc_info := ProcInfo1,
			
		{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			CalleePredInfo, CalleeProcInfo) },

		% We don't create requests for higher-order terms 
		% because that would result in duplication of effort
		% if all uses of the constant end up being specialized.
		% For parser combinator programs it would also
		% result in huge numbers of requests with no easy
		% way to control which ones should be created.
		{ CanRequest = no },
		{ IsBuiltin = not_builtin },
		{ MaybeContext = no },
		maybe_specialize_ordinary_call(CanRequest, PredId,
			ProcId, CalleePredInfo, CalleeProcInfo, Args1,
			IsBuiltin, MaybeContext, GoalInfo, Result),
		( 
			{ Result = specialized(ExtraTypeInfoGoals0, Goal1) },
			{
				Goal1 = call(NewPredId0, NewProcId0,
						NewArgs0, _, _, _),
				list__remove_suffix(NewArgs0,
					UncurriedArgs, NewArgs1)
			->
				NewPredId = NewPredId0,
				NewProcId = NewProcId0,
				NewArgs = NewArgs1
			;
				error("maybe_specialize_pred_const")	
			},

			{ module_info_pred_proc_info(ModuleInfo,
				NewPredId, NewProcId, _, NewCalleeProcInfo) },
			{ proc_info_argmodes(NewCalleeProcInfo,
				NewCalleeArgModes) },
			{	list__take(list__length(NewArgs),
					NewCalleeArgModes, CurriedArgModes0)
			->
				CurriedArgModes = CurriedArgModes0
			;
				error("maybe_specialize_pred_const")
			},
			{ mode_util__modes_to_uni_modes(CurriedArgModes,
				CurriedArgModes, ModuleInfo, UniModes) },
				
			% The dummy arguments can't be used anywhere.
			ProcInfo2 =^ proc_info,
			{ proc_info_vartypes(ProcInfo2, VarTypes2) },
			{ map__delete_list(VarTypes2,
				UncurriedArgs, VarTypes) },
			{ proc_info_set_vartypes(ProcInfo2,
				VarTypes, ProcInfo) },
			^ proc_info := ProcInfo,

			{ NewConsId = pred_const(NewPredId, NewProcId,
					EvalMethod) },
			{ Unify = construct(LVar, NewConsId,
				NewArgs, UniModes, HowToConstruct,
				CellIsUnique, MaybeExprn) },
			{ Goal2 = unify(LVar, functor(NewConsId, NewArgs),
				UniMode, Unify, Context) },

			% Make sure any constants in the
			% ExtraTypeInfoGoals are recorded.
			list__map_foldl(traverse_goal_2, ExtraTypeInfoGoals0,
				ExtraTypeInfoGoals),
			{ ExtraTypeInfoGoals = [] ->
				Goal = Goal2
			;
				Goal = conj(ExtraTypeInfoGoals
						++ [Goal2 - GoalInfo])
			}
		;
			{ Result = not_specialized },
			% The dummy arguments can't be used anywhere.
			^ proc_info := ProcInfo0,
			{ Goal = Goal0 }
		)
	;
		{ Goal = Goal0 }
	).

:- type specialization_result
	--->	specialized(
			list(hlds_goal),	% Goals to construct extra
						% type-infos.
			hlds_goal_expr		% The specialized call.
		)
	;	not_specialized.

:- pred maybe_specialize_ordinary_call(bool::in, pred_id::in, proc_id::in,
	pred_info::in, proc_info::in, list(prog_var)::in, builtin_state::in,
	maybe(call_unify_context)::in, hlds_goal_info::in,
	specialization_result::out,
	higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_ordinary_call(CanRequest, CalledPred, CalledProc,
		CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin,
		MaybeContext, GoalInfo, Result, Info0, Info) :-
	ModuleInfo0 = Info0 ^ global_info ^ module_info,
	pred_info_import_status(CalleePredInfo, CalleeStatus),
	proc_info_vartypes(CalleeProcInfo, CalleeVarTypes),
	proc_info_headvars(CalleeProcInfo, CalleeHeadVars),
	map__apply_to_list(CalleeHeadVars, CalleeVarTypes, CalleeArgTypes),

	CallerProcInfo0 = Info0 ^ proc_info,
	proc_info_vartypes(CallerProcInfo0, VarTypes),
	find_higher_order_args(ModuleInfo0, CalleeStatus, Args0,
		CalleeArgTypes, VarTypes, Info0 ^ pred_vars, 1, [],
		HigherOrderArgs0),

	proc(CallerPredId, _) = Info0 ^ pred_proc_id,
	module_info_type_spec_info(ModuleInfo0,
		type_spec_info(_, ForceVersions, _, _)),
	( set__member(CallerPredId, ForceVersions) ->
		IsUserSpecProc = yes
	;
		IsUserSpecProc = no
	),

	(
		(
			HigherOrderArgs0 = [_ | _]
		;
			% We should create these
			% even if there is no specialization
			% to avoid link errors.
			IsUserSpecProc = yes
		;
			yes = Info0 ^ global_info ^ ho_params ^ user_type_spec,
			map__apply_to_list(Args0, VarTypes, ArgTypes),

			% Check whether any typeclass constraints
			% now match an instance.
			pred_info_get_class_context(CalleePredInfo,
				CalleeClassContext),
			CalleeClassContext =
				constraints(CalleeUnivConstraints0, _),
			pred_info_typevarset(CalleePredInfo, CalleeTVarSet),
			pred_info_get_exist_quant_tvars(CalleePredInfo,
				CalleeExistQTVars),
			CallerPredInfo0 = Info0 ^ pred_info,
			pred_info_typevarset(CallerPredInfo0, TVarSet),
			pred_info_get_univ_quant_tvars(CallerPredInfo0,
				CallerUnivQTVars),
			type_subst_makes_instance_known(ModuleInfo0,
				CalleeUnivConstraints0, TVarSet,
				CallerUnivQTVars, ArgTypes, CalleeTVarSet,
				CalleeExistQTVars, CalleeArgTypes)
		)
	->
		list__reverse(HigherOrderArgs0, HigherOrderArgs),
		goal_info_get_context(GoalInfo, Context),
		find_matching_version(Info0, CalledPred, CalledProc, Args0,
			Context, HigherOrderArgs, IsUserSpecProc, FindResult),
		(
			FindResult = match(match(Match, _, Args1,
					ExtraTypeInfoTypes)),
			Match = new_pred(NewPredProcId, _, _,
				NewName, _HOArgs, _, _, _, _, _, _),
			NewPredProcId = proc(NewCalledPred, NewCalledProc),

			construct_extra_type_infos(ExtraTypeInfoTypes,
				ExtraTypeInfoVars, ExtraTypeInfoGoals,
				Info0, Info1),

			list__append(ExtraTypeInfoVars, Args1, Args),
			CallGoal = call(NewCalledPred, NewCalledProc,
				Args, IsBuiltin, MaybeContext, NewName),
			Result = specialized(ExtraTypeInfoGoals, CallGoal),
			Info = Info1 ^ changed := changed
		;
			% There is a known higher order variable in
			% the call, so we put in a request for a
			% specialized version of the pred.
			FindResult = request(Request),
			Result = not_specialized,
			( CanRequest = yes ->
				set__insert(Info0 ^ global_info ^ requests,
					Request, Requests),
				update_changed_status(Info0 ^ changed,
					request, Changed),
				Info = (Info0 ^ global_info
						^ requests := Requests)
						^ changed := Changed
			;
				Info = Info0		
			)
		;
			FindResult = no_request,
			Result = not_specialized,
			Info = Info0
		)
	;
		Result = not_specialized,
		Info = Info0
	).

	% Returns a list of the higher-order arguments in a call that have
	% a known value.
:- pred find_higher_order_args(module_info::in, import_status::in,
	list(prog_var)::in, list(type)::in, map(prog_var, type)::in,
	pred_vars::in, int::in, list(higher_order_arg)::in,
	list(higher_order_arg)::out) is det.

find_higher_order_args(_, _, [], _, _, _, _, HOArgs, HOArgs).
find_higher_order_args(_, _, [_|_], [], _, _, _, _, _) :-
	error("find_higher_order_args: length mismatch").
find_higher_order_args(ModuleInfo, CalleeStatus, [Arg | Args],
		[CalleeArgType | CalleeArgTypes], VarTypes,
		PredVars, ArgNo, HOArgs0, HOArgs) :-
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

		( ConsId = pred_const(_, _, _) ->
			% If we don't have clauses for the callee, we can't
			% specialize any higher-order arguments. We may be
			% able to do user guided type specialization.
			CalleeStatus \= imported(_),
			CalleeStatus \= external(_),
			type_is_higher_order(CalleeArgType, _, _, _)
		;
			true
		)
	->
		% Find any known higher-order arguments
		% in the list of curried arguments.
		map__apply_to_list(CurriedArgs, VarTypes, CurriedArgTypes),
		( ConsId = pred_const(PredId, _, _) ->
			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			pred_info_arg_types(PredInfo, CurriedCalleeArgTypes)
		;
			CurriedCalleeArgTypes = CurriedArgTypes
		),
		find_higher_order_args(ModuleInfo, CalleeStatus, CurriedArgs,
			CurriedCalleeArgTypes, VarTypes,
			PredVars, 1, [], HOCurriedArgs0),
		list__reverse(HOCurriedArgs0, HOCurriedArgs),
		list__length(CurriedArgs, NumArgs),
		(
			NumArgs = list__length(HOCurriedArgs),
			\+ (
				list__member(HOCurriedArg, HOCurriedArgs),
				HOCurriedArg = higher_order_arg(_, _, _,
					_, _, _, no)
			)
		->
			IsConst = yes
		;
			IsConst = no
		),
		HOArg = higher_order_arg(ConsId, ArgNo, NumArgs,
			CurriedArgs, CurriedArgTypes, HOCurriedArgs, IsConst),
		HOArgs1 = [HOArg | HOArgs0]
	;
		HOArgs1 = HOArgs0
	),
	find_higher_order_args(ModuleInfo, CalleeStatus, Args, CalleeArgTypes,
		VarTypes, PredVars, NextArg, HOArgs1, HOArgs).

	% Succeeds if the type substitution for a call makes any of
	% the class constraints match an instance which was not matched
	% before.
:- pred type_subst_makes_instance_known(module_info::in,
		list(class_constraint)::in, tvarset::in, list(tvar)::in,
		list(type)::in, tvarset::in, existq_tvars::in,
		list(type)::in) is semidet.

type_subst_makes_instance_known(ModuleInfo, CalleeUnivConstraints0, TVarSet0,
		CallerHeadTypeParams, ArgTypes, CalleeTVarSet,
		CalleeExistQVars, CalleeArgTypes0) :-
	CalleeUnivConstraints0 \= [],
	varset__merge_subst(TVarSet0, CalleeTVarSet,
		TVarSet, TypeRenaming),
	term__apply_substitution_to_list(CalleeArgTypes0, TypeRenaming,
		CalleeArgTypes1),

	% Substitute the types in the callee's class constraints.
	inlining__get_type_substitution(CalleeArgTypes1, ArgTypes,
		CallerHeadTypeParams, CalleeExistQVars, TypeSubn),
	apply_subst_to_constraint_list(TypeRenaming,
		CalleeUnivConstraints0, CalleeUnivConstraints1),
	apply_rec_subst_to_constraint_list(TypeSubn,
		CalleeUnivConstraints1, CalleeUnivConstraints),
	assoc_list__from_corresponding_lists(CalleeUnivConstraints0,
		CalleeUnivConstraints, CalleeUnivConstraintAL),

	% Go through each constraint in turn, checking whether any instances
	% match which didn't before the substitution was applied.
	list__member(CalleeUnivConstraint0 - CalleeUnivConstraint,
		CalleeUnivConstraintAL),
	CalleeUnivConstraint0 = constraint(ClassName, ConstraintArgs0),
	list__length(ConstraintArgs0, ClassArity),
	CalleeUnivConstraint = constraint(_, ConstraintArgs),
	module_info_instances(ModuleInfo, InstanceTable),
	map__search(InstanceTable, class_id(ClassName, ClassArity), Instances),
	list__member(Instance, Instances),
	instance_matches(ConstraintArgs, Instance, _, _, TVarSet, _),
	\+ instance_matches(ConstraintArgs0, Instance, _, _, TVarSet, _).

:- type find_result
	--->	match(match)
	; 	request(request)
	;	no_request
	.

:- type match
	---> match(
		new_pred,
		maybe(int),	% was the match partial, if so,
				% how many higher_order arguments
				% matched.
		list(prog_var),	% the arguments to the specialised call.
		list(type)	% type variables for which extra type-infos
				% must be added to the start of the argument
				% list.
	).

	% WARNING - do not filter out higher-order arguments from the
	% request returned by find_matching_version, otherwise some
	% type-infos that the call specialization code is expecting to
	% come from the curried arguments of the higher-order arguments
	% will not be present in the specialized argument list.
:- pred find_matching_version(higher_order_info::in,
	pred_id::in, proc_id::in, list(prog_var)::in, prog_context::in,
	list(higher_order_arg)::in, bool::in, find_result::out) is det.

	% Args0 is the original list of arguments.
	% Args is the original list of arguments with the curried arguments
	% of known higher-order arguments added.
find_matching_version(Info, CalledPred, CalledProc, Args0, Context,
		HigherOrderArgs, IsUserSpecProc, Result) :-
	ModuleInfo = Info ^ global_info ^ module_info,
	NewPreds = Info ^ global_info ^ new_preds,
	Caller = Info ^ pred_proc_id,
	PredInfo = Info ^ pred_info,
	ProcInfo = Info ^ proc_info,
	Params = Info ^ global_info ^ ho_params,

	% WARNING - do not filter out higher-order arguments after this step,
	% except when partially matching against a previously produced
	% specialization, otherwise some type-infos that the call
	% specialization code is expecting to come from the curried
	% arguments of the higher-order arguments will not be present
	% in the specialized argument list.
	module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
	module_info_globals(ModuleInfo, Globals),
	proc_interface_should_use_typeinfo_liveness(CalledPredInfo,
		CalledProc, Globals, TypeInfoLiveness),
	get_extra_arguments(HigherOrderArgs, Args0, Args),
	compute_extra_typeinfos(TypeInfoLiveness,
		Info, Args, ExtraTypeInfoTVars),

	proc_info_vartypes(ProcInfo, VarTypes),
	map__apply_to_list(Args0, VarTypes, CallArgTypes),
	pred_info_typevarset(PredInfo, TVarSet),

	Request = request(Caller, proc(CalledPred, CalledProc), Args0,
		ExtraTypeInfoTVars, HigherOrderArgs, CallArgTypes,
		TypeInfoLiveness, TVarSet, IsUserSpecProc, Context),

	% Check to see if any of the specialized
	% versions of the called pred apply here.
	(
		map__search(NewPreds, proc(CalledPred, CalledProc),
			Versions0),
		set__to_sorted_list(Versions0, Versions),
		search_for_version(Info, Params, ModuleInfo, Request,
			Versions, no, Match)
	->
		Result = match(Match)
	;
		HigherOrder = Params ^ optimize_higher_order,
		TypeSpec = Params ^ type_spec,
		UserTypeSpec = Params ^ user_type_spec,
		(
			UserTypeSpec = yes,
			IsUserSpecProc = yes
		;
			module_info_pred_info(ModuleInfo,
				CalledPred, CalledPredInfo),
			\+ pred_info_is_imported(CalledPredInfo),
			(
				% This handles the predicates introduced
				% by check_typeclass.m to call the class
				% methods for a specific instance.
				% Without this, user-specified specialized
				% versions of class methods won't be called.
				UserTypeSpec = yes,
				pred_info_get_markers(CalledPredInfo,
					Markers),
				(
					check_marker(Markers, class_method)
				;
					check_marker(Markers,
						class_instance_method)
				)
			;
				HigherOrder = yes,
				list__member(HOArg, HigherOrderArgs),
				HOArg = higher_order_arg(pred_const(_, _, _),
					_, _, _, _, _, _)
			;
				TypeSpec = yes
			)
		)
	->
		Result = request(Request)
	;
		Result = no_request
	).

	% If `--typeinfo-liveness' is set, specializing type `T' to `list(U)'
	% requires passing in the type-info for `U'. This predicate
	% works out which extra variables to pass in given the argument
	% list for the call.
:- pred compute_extra_typeinfos(bool::in, higher_order_info::in,
		list(prog_var)::in, list(tvar)::out) is det.

compute_extra_typeinfos(TypeInfoLiveness, Info, Args1, ExtraTypeInfoTVars) :-
	( TypeInfoLiveness = yes ->
		% Work out which type variables don't already have type-infos
		% in the list of argument types.
		% The list is in the order which the type variables occur
		% in the list of argument types so that the extra type-info
		% arguments for calls to imported user-guided type
		% specialization procedures can be matched against the
		% specialized version (`goal_util__extra_nonlocal_typeinfos'
		% is not used here because the type variables are returned
		% sorted by variable number, which will vary between calls).
		ProcInfo = Info ^ proc_info,
		proc_info_vartypes(ProcInfo, VarTypes),
		map__apply_to_list(Args1, VarTypes, ArgTypes),
		term__vars_list(ArgTypes, AllTVars),
		( AllTVars = [] ->
			ExtraTypeInfoTVars = []
		;
			list__foldl(arg_type_contains_type_info_for_tvar,
				ArgTypes, [], TypeInfoTVars),
			list__delete_elems(AllTVars, TypeInfoTVars,
				ExtraTypeInfoTVars0),
			list__remove_dups(ExtraTypeInfoTVars0,
				ExtraTypeInfoTVars)
		)
	;
		ExtraTypeInfoTVars = []
	).

:- pred arg_type_contains_type_info_for_tvar((type)::in, list(tvar)::in,
		list(tvar)::out) is det.

arg_type_contains_type_info_for_tvar(TypeInfoType, TVars0, TVars) :-
	(
		polymorphism__type_info_type(TypeInfoType, Type),
		Type = term__variable(TVar)
	->
		TVars = [TVar | TVars0]
	;
		polymorphism__typeclass_info_class_constraint(TypeInfoType,
			Constraint),
		Constraint = constraint(_ClassName, ClassArgTypes)
	->
		% Find out which tvars the typeclass-info contains
		% the type-infos for.
		list__filter_map(
			(pred(ClassArgType::in, ClassTVar::out) is semidet :-
				ClassArgType = term__variable(ClassTVar)
			), ClassArgTypes, ClassTVars),
		list__append(ClassTVars, TVars0, TVars)
	;
		TVars = TVars0
	).

:- pred construct_extra_type_infos(list(type)::in,
		list(prog_var)::out, list(hlds_goal)::out,
		higher_order_info::in, higher_order_info::out) is det.

construct_extra_type_infos(Types, TypeInfoVars, TypeInfoGoals, Info0, Info) :-
	create_poly_info(Info0 ^ global_info ^ module_info, Info0 ^ pred_info,
		Info0 ^ proc_info, PolyInfo0),
	term__context_init(Context),
	polymorphism__make_type_info_vars(Types, Context,
		TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
	poly_info_extract(PolyInfo, Info0 ^ pred_info, PredInfo,
		Info0 ^ proc_info, ProcInfo, ModuleInfo),
	Info = ((Info0 ^ pred_info := PredInfo)
			^ proc_info := ProcInfo)
			^ global_info ^ module_info := ModuleInfo.

:- pred search_for_version(higher_order_info::in, ho_params::in,
		module_info::in, request::in, list(new_pred)::in,
		maybe(match)::in, match::out) is semidet.

search_for_version(_Info, _Params, _ModuleInfo, _Request,
		[], yes(Match), Match).
search_for_version(Info, Params, ModuleInfo, Request,
		[Version | Versions], Match0, Match) :-
	(
		version_matches(Params, ModuleInfo, Request, Version, Match1)
	->
		(
			Match1 = match(_, MatchIsPartial, _, _),
			MatchIsPartial = no
		->
			Match = Match1
		;
			(
				Match0 = no
			->
				Match2 = yes(Match1)
			;
				% pick the best match
				Match0 = yes(match(_, yes(NumMatches0), _, _)),
				Match1 = match(_, yes(NumMatches1), _, _)
			->
				( NumMatches0 > NumMatches1 ->
					Match2 = Match0
				;
					Match2 = yes(Match1)
				)
			;
				error("higher_order: search_for_version")
			),
			search_for_version(Info, Params, ModuleInfo, Request,
				Versions, Match2, Match)
		)
	;
		search_for_version(Info, Params, ModuleInfo, Request,
			Versions, Match0, Match)
	).

	% Check whether the request has already been implemented by
	% the new_pred, maybe ordering the list of extra type_infos
	% in the caller predicate to match up with those in the caller.
:- pred version_matches(ho_params::in, module_info::in, request::in,
		new_pred::in, match::out) is semidet.

version_matches(Params, ModuleInfo, Request, Version,
		match(Version, PartialMatch, Args, ExtraTypeInfoTypes)) :-

	Request = request(_, Callee, Args0, _, RequestHigherOrderArgs,
		CallArgTypes, _, RequestTVarSet, _, _),
	Version = new_pred(_, _, _, _, VersionHigherOrderArgs,
		_, VersionExtraTypeInfoTVars, VersionArgTypes0,
		_, VersionTVarSet, _),

	higher_order_args_match(RequestHigherOrderArgs,
		VersionHigherOrderArgs, HigherOrderArgs, MatchIsPartial),

	( MatchIsPartial = yes ->
		list__length(HigherOrderArgs, NumHOArgs),
		PartialMatch = yes(NumHOArgs)
	;
		PartialMatch = no
	),

	Callee = proc(CalleePredId, _),
	module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
	(
		% Don't accept partial matches unless the predicate is
		% imported or we are only doing user-guided type
		% specialization.
		MatchIsPartial = no
	;
		Params ^ type_spec = no
	;
		pred_info_is_imported(CalleePredInfo)
	),

	% Rename apart type variables.
	varset__merge_subst(RequestTVarSet, VersionTVarSet, _, TVarSubn),
	term__apply_substitution_to_list(VersionArgTypes0, TVarSubn,
		VersionArgTypes),
	type_list_subsumes(VersionArgTypes, CallArgTypes, TypeSubn),

	% Work out the types of the extra type-info variables that
	% need to be passed to the specialized version.
	term__var_list_to_term_list(VersionExtraTypeInfoTVars,
		VersionExtraTypeInfoTypes),
	term__apply_substitution_to_list(VersionExtraTypeInfoTypes,
		TVarSubn, ExtraTypeInfoTypes0),
	term__apply_rec_substitution_to_list(ExtraTypeInfoTypes0, TypeSubn,
		ExtraTypeInfoTypes),

	get_extra_arguments(HigherOrderArgs, Args0, Args).

:- pred higher_order_args_match(list(higher_order_arg)::in,
		list(higher_order_arg)::in, list(higher_order_arg)::out,
		bool::out) is semidet.

higher_order_args_match([], [], [], no).
higher_order_args_match(RequestArgs, [], [], yes) :-
	RequestArgs = [_ | _],
	\+ (
		list__member(RequestArg, RequestArgs),
		RequestArg = higher_order_arg(RequestConsId, _, _, _, _, _, _),
		RequestConsId = pred_const(_, _, _)
	).
higher_order_args_match([RequestArg | Args1], [VersionArg | Args2],
		Args, PartialMatch) :-
	RequestArg = higher_order_arg(ConsId1, ArgNo1, _, _, _, _,
			RequestIsConst),
	VersionArg = higher_order_arg(ConsId2, ArgNo2, _, _, _, _,
			VersionIsConst),

	( ArgNo1 = ArgNo2 ->
		ConsId1 = ConsId2,
		RequestArg = higher_order_arg(_, _, NumArgs,
			CurriedArgs, CurriedArgTypes, HOCurriedArgs1, _),
		VersionArg = higher_order_arg(_, _, NumArgs,
			_, _, HOCurriedArgs2, _),
		higher_order_args_match(HOCurriedArgs1, HOCurriedArgs2,
			NewHOCurriedArgs, PartialMatch),
		higher_order_args_match(Args1, Args2, Args3, _),
		NewRequestArg = higher_order_arg(ConsId1, ArgNo1, NumArgs,
			CurriedArgs, CurriedArgTypes, NewHOCurriedArgs,
			RequestIsConst `and` VersionIsConst),
		Args = [NewRequestArg | Args3]
	;
		% type-info arguments present in the request may be missing
		% from the version if we are doing user-guided type
		% specialization.
		% All of the arguments in the version must be
		% present in the request for a match.
		ArgNo1 < ArgNo2,

		% All the higher-order arguments must be present in the
		% version otherwise we should create a new one.
		ConsId1 \= pred_const(_, _, _),
		PartialMatch = yes,
		higher_order_args_match(Args1, [VersionArg | Args2], Args, _)
	).

	% Add the curried arguments of the higher-order terms to the
	% argument list. The order here must match that generated by
	% construct_higher_order_terms.
:- pred get_extra_arguments(list(higher_order_arg)::in,
		list(prog_var)::in, list(prog_var)::out) is det.

get_extra_arguments(HOArgs, Args0, ExtraArgs ++ Args) :-
	get_extra_arguments_2(HOArgs, ExtraArgs),
	remove_const_higher_order_args(1, Args0, HOArgs, Args).
	
:- pred get_extra_arguments_2(list(higher_order_arg)::in,
		list(prog_var)::out) is det.

get_extra_arguments_2([], []).
get_extra_arguments_2([HOArg | HOArgs], Args) :-
	HOArg = higher_order_arg(_, _, _,
		CurriedArgs0, _, HOCurriedArgs, IsConst),
	( IsConst = yes ->
		% If this argument is constant, all its sub-terms must be
		% constant, so there won't be anything more to add.
		get_extra_arguments_2(HOArgs, Args)
	;
		remove_const_higher_order_args(1, CurriedArgs0,
			HOCurriedArgs, CurriedArgs),
		get_extra_arguments_2(HOCurriedArgs, ExtraCurriedArgs),
		get_extra_arguments_2(HOArgs, Args1),
		list__condense([CurriedArgs, ExtraCurriedArgs, Args1],
			Args)
	).

		% if the right argument of an assignment is a higher order
		% term with a known value, we need to add an entry for
		% the left argument
:- pred maybe_add_alias(prog_var::in, prog_var::in, higher_order_info::in,
				higher_order_info::out) is det.

maybe_add_alias(LVar, RVar, Info0, Info) :-
	( map__search(Info0 ^ pred_vars, RVar, constant(A, B)) ->
		map__set(Info0 ^ pred_vars, LVar, constant(A, B), PredVars),
		Info = Info0 ^ pred_vars := PredVars
	;
		Info = Info0
	).

:- pred update_changed_status(changed::in, changed::in, changed::out) is det.

update_changed_status(changed, _, changed).
update_changed_status(request, changed, changed).
update_changed_status(request, request, request).
update_changed_status(request, unchanged, request).
update_changed_status(unchanged, Changed, Changed).

%-------------------------------------------------------------------------------

	% Interpret a call to `type_info_from_typeclass_info',
	% `superclass_from_typeclass_info' or
	% `instance_constraint_from_typeclass_info'.
	% This should be kept in sync with compiler/polymorphism.m,
	% library/private_builtin.m and runtime/mercury_type_info.h.
:- pred interpret_typeclass_info_manipulator(typeclass_info_manipulator::in,
	list(prog_var)::in, hlds_goal_expr::in, hlds_goal_expr::out,
	higher_order_info::in, higher_order_info::out) is det.

interpret_typeclass_info_manipulator(Manipulator, Args,
		Goal0, Goal, Info0, Info) :-
	ModuleInfo = Info0 ^ global_info ^ module_info,
	PredVars = Info0 ^ pred_vars,
	(
		Args = [TypeClassInfoVar, IndexVar, TypeInfoVar],
		map__search(PredVars, TypeClassInfoVar,
			constant(_TypeClassInfoConsId, TypeClassInfoArgs)),

		map__search(PredVars, IndexVar,
			constant(int_const(Index0), [])),

		% Extract the number of class constraints on the instance
		% from the base_typeclass_info.
		TypeClassInfoArgs = [BaseTypeClassInfoVar | OtherVars],

		map__search(PredVars, BaseTypeClassInfoVar,
		    	constant(base_typeclass_info_const(_,
				ClassId, InstanceNum, _), _))
	->
		module_info_instances(ModuleInfo, Instances),
		map__lookup(Instances, ClassId, InstanceDefns),
		list__index1_det(InstanceDefns, InstanceNum, InstanceDefn),
		InstanceDefn = hlds_instance_defn(_,_,_,Constraints,_,_,_,_,_),
		(
			Manipulator = type_info_from_typeclass_info,
			list__length(Constraints, NumConstraints),
			Index = Index0 + NumConstraints
		;
			Manipulator = superclass_from_typeclass_info,
			list__length(Constraints, NumConstraints),
			% polymorphism.m adds the number of
			% type_infos to the index.
			Index = Index0 + NumConstraints
		;
			Manipulator = instance_constraint_from_typeclass_info,
			Index = Index0
		),
		list__index1_det(OtherVars, Index, TypeInfoArg),
		maybe_add_alias(TypeInfoVar, TypeInfoArg, Info0, Info1),
		Uni = assign(TypeInfoVar, TypeInfoArg),
		in_mode(In),
		out_mode(Out),
		Goal = unify(TypeInfoVar, var(TypeInfoArg), Out - In,
			Uni, unify_context(explicit, [])),
		Info = Info1 ^ changed := changed
	;
		Goal = Goal0,
		Info = Info0
	).

%-------------------------------------------------------------------------------

	% Succeed if the called pred is "unify" or "compare" and
	% is specializable, returning a specialized goal.
:- pred specialize_special_pred(pred_id::in, proc_id::in, list(prog_var)::in,
	maybe(call_unify_context)::in, hlds_goal_info::in, bool::in,
	hlds_goal_expr::out, higher_order_info::in, higher_order_info::out)
	is semidet.

specialize_special_pred(CalledPred, CalledProc, Args, MaybeContext,
		OrigGoalInfo, HaveSpecialPreds, Goal, Info0, Info) :-
	ModuleInfo = Info0 ^ global_info ^ module_info,
	ProcInfo0 = Info0 ^ proc_info,
	PredVars = Info0 ^ pred_vars,
	proc_info_vartypes(ProcInfo0, VarTypes),
	module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
	mercury_public_builtin_module(PublicBuiltin),
	pred_info_module(CalledPredInfo, PublicBuiltin),
	pred_info_name(CalledPredInfo, PredName),
	pred_info_arity(CalledPredInfo, PredArity),
	special_pred_name_arity(SpecialId, PredName, _, PredArity),
	special_pred_get_type(PredName, Args, Var),
	map__lookup(VarTypes, Var, SpecialPredType),
	SpecialPredType \= term__variable(_),

	% Don't specialize tuple types -- the code to unify
	% them only exists in the generic unification routine
	% in the runtime. `private_builtin__builtin_unify_tuple/2'
	% and `private_builtin__builtin_compare_tuple/3' always abort.
	% It might be worth inlining complicated unifications of
	% small tuples (or any other small type).
	\+ type_is_tuple(SpecialPredType, _),

	Args = [TypeInfoVar | SpecialPredArgs],
	map__search(PredVars, TypeInfoVar,
		constant(_TypeInfoConsId, TypeInfoVarArgs)),
	type_to_ctor_and_args(SpecialPredType, _ - TypeArity, _),
	( TypeArity = 0 ->
		TypeInfoArgs = []
	;
		TypeInfoVarArgs = [_TypeCtorInfo | TypeInfoArgs]
	),

	(
		% Look for unification or comparison applied directly to
		% a builtin or atomic type. This needs to be done separately
		% from the case for user-defined types, for two reasons.
		% First, because we want to specialize such calls even if
		% we are not generating any special preds. Second, because
		% the specialized code is different in the two cases:
		% here it is a call to a builtin predicate, perhaps preceded
		% by casts; there it is a call to a compiler-generated
		% predicate.

		specializeable_special_call(SpecialId, CalledProc),
		type_is_atomic(SpecialPredType, ModuleInfo),
		\+ type_has_user_defined_equality_pred(ModuleInfo,
			SpecialPredType, _)
	->
		(
			SpecialId = unify,
			SpecialPredArgs = [Arg1, Arg2]
		;
			SpecialId = compare,
			SpecialPredArgs = [_, Arg1, Arg2]
		),
		(
			SpecialId = unify,
			in_mode(In),
			Goal = unify(Arg1, var(Arg2), (In - In),
				simple_test(Arg1, Arg2),
				unify_context(explicit, [])),
			Info = Info0
		;
			SpecialId = compare,
			SpecialPredArgs = [ComparisonResult, _, _],
			find_builtin_type_with_equivalent_compare(
				ModuleInfo, SpecialPredType, CompareType,
				NeedIntCast),
			polymorphism__get_special_proc(CompareType,
				SpecialId, ModuleInfo, SymName,
				SpecialPredId, SpecialProcId),
			(
				NeedIntCast = no,
				NewCallArgs = [ComparisonResult, Arg1, Arg2],
				Goal = call(SpecialPredId, SpecialProcId,
					NewCallArgs, not_builtin,
					MaybeContext, SymName),
				Info = Info0
			;
				NeedIntCast = yes,
				goal_info_get_context(OrigGoalInfo, Context),
				generate_unsafe_type_cast(ModuleInfo, Context,
					CompareType, Arg1, CastArg1, CastGoal1,
					ProcInfo0, ProcInfo1),
				generate_unsafe_type_cast(ModuleInfo, Context,
					CompareType, Arg2, CastArg2, CastGoal2,
					ProcInfo1, ProcInfo),
				NewCallArgs = [ComparisonResult,
					CastArg1, CastArg2],
				Call = call(SpecialPredId, SpecialProcId,
					NewCallArgs, not_builtin,
					MaybeContext, SymName),
				set__list_to_set([ComparisonResult,
					Arg1, Arg2], NonLocals),
				instmap_delta_from_assoc_list(
					[ComparisonResult - 
						ground(shared,none)],
					InstMapDelta),
				Detism = det,
				goal_info_init(NonLocals, InstMapDelta,
					Detism, Context, GoalInfo),
				Goal = conj([CastGoal1, CastGoal2,
						Call - GoalInfo]),
				Info = Info0 ^ proc_info := ProcInfo
			)
		)
	;
		% Look for unification or comparison applied to a no-tag type
		% wrapping a builtin or atomic type.
		% This needs to be done to optimize all the map_lookups
		% with keys of type `term__var/1' in the compiler.
		% (:- type var(T) ---> var(int).)
		% This could possibly be better handled by just inlining
		% the unification code, but the compiler doesn't have the
		% code for the comparison or in-in unification procedures
		% for imported types, and unification and comparison may
		% be implemented in C code in the runtime system.

		specializeable_special_call(SpecialId, CalledProc),
		type_is_no_tag_type(ModuleInfo, SpecialPredType, 
			Constructor, WrappedType),
		\+ type_has_user_defined_equality_pred(ModuleInfo,
			SpecialPredType, _),
		\+ type_has_user_defined_equality_pred(ModuleInfo,
			WrappedType, _),

		% This could be done for non-atomic types, but it would
		% be a bit more complicated because the type-info for
		% the wrapped type would need to be extracted first.
		type_is_atomic(WrappedType, ModuleInfo)
	->
		(
			SpecialId = unify,
			SpecialPredArgs = [Arg1, Arg2]
		;
			SpecialId = compare,
			SpecialPredArgs = [_, Arg1, Arg2]
		),
		goal_info_get_context(OrigGoalInfo, Context),
		unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg1,
			UnwrappedArg1, ExtractGoal1, ProcInfo0, ProcInfo1),
		unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg2,
			UnwrappedArg2, ExtractGoal2, ProcInfo1, ProcInfo2),
		set__list_to_set([UnwrappedArg1, UnwrappedArg2], NonLocals0),
		(
			SpecialId = unify,
			in_mode(In),
			NonLocals = NonLocals0,
			instmap_delta_init_reachable(InstMapDelta),
			Detism = semidet,
			SpecialGoal = unify(UnwrappedArg1, var(UnwrappedArg2),
				(In - In),
				simple_test(UnwrappedArg1, UnwrappedArg2),
				unify_context(explicit, [])),
			goal_info_init(NonLocals, InstMapDelta, Detism,
				Context, GoalInfo),
			Goal = conj([ExtractGoal1, ExtractGoal2,
					SpecialGoal - GoalInfo]),
			Info = Info0 ^ proc_info := ProcInfo2
		;
			SpecialId = compare,
			SpecialPredArgs = [ComparisonResult, _, _],
			set__insert(NonLocals0, ComparisonResult, NonLocals),
			instmap_delta_from_assoc_list(
				[ComparisonResult - ground(shared, none)],
				InstMapDelta),
			Detism = det,
			% Build a new call with the unwrapped arguments.
			find_builtin_type_with_equivalent_compare(
				ModuleInfo, WrappedType, CompareType,
				NeedIntCast),
			polymorphism__get_special_proc(CompareType,
				SpecialId, ModuleInfo, SymName,
				SpecialPredId, SpecialProcId),
			(
				NeedIntCast = no,
				NewCallArgs = [ComparisonResult,
					UnwrappedArg1, UnwrappedArg2],
				SpecialGoal = call(SpecialPredId,
					SpecialProcId, NewCallArgs,
					not_builtin, MaybeContext, SymName),
				goal_info_init(NonLocals, InstMapDelta, Detism,
					Context, GoalInfo),
				Goal = conj([ExtractGoal1, ExtractGoal2,
						SpecialGoal - GoalInfo]),
				Info = Info0 ^ proc_info := ProcInfo2
			;
				NeedIntCast = yes,
				generate_unsafe_type_cast(ModuleInfo, Context,
					CompareType, UnwrappedArg1, CastArg1,
					CastGoal1, ProcInfo2, ProcInfo3),
				generate_unsafe_type_cast(ModuleInfo, Context,
					CompareType, UnwrappedArg2, CastArg2,
					CastGoal2, ProcInfo3, ProcInfo4),
				NewCallArgs = [ComparisonResult,
					CastArg1, CastArg2],
				SpecialGoal = call(SpecialPredId,
					SpecialProcId, NewCallArgs,
					not_builtin, MaybeContext, SymName),
				goal_info_init(NonLocals, InstMapDelta, Detism,
					Context, GoalInfo),
				Goal = conj([ExtractGoal1, CastGoal1,
						ExtractGoal2, CastGoal2,
						SpecialGoal - GoalInfo]),
				Info = Info0 ^ proc_info := ProcInfo4
			)
		)
	;
			% We can only specialize unifications and comparisons
			% to call the type-specific unify or compare predicate
			% if we are generating such predicates.
		HaveSpecialPreds = yes,
		find_special_proc(SpecialPredType, SpecialId,
			SymName, SpecialPredId, SpecialProcId, Info0, Info),
		( type_is_higher_order(SpecialPredType, _, _, _) ->
			% builtin_*_pred are special cases which
			% doesn't need the type-info arguments.
			CallArgs = SpecialPredArgs
		;
			list__append(TypeInfoArgs, SpecialPredArgs, CallArgs)
		),
		Goal = call(SpecialPredId, SpecialProcId, CallArgs,
			not_builtin, MaybeContext, SymName)
	).

:- pred find_special_proc((type)::in, special_pred_id::in, sym_name::out,
		pred_id::out, proc_id::out, higher_order_info::in,
		higher_order_info::out) is semidet.

find_special_proc(Type, SpecialId, SymName, PredId, ProcId, Info0, Info) :-
	ModuleInfo0 = Info0 ^ global_info ^ module_info,
	(
	    polymorphism__get_special_proc(Type, SpecialId,
		ModuleInfo0, SymName0, PredId0, ProcId0)
	->
	    SymName = SymName0,
	    PredId = PredId0,
	    ProcId = ProcId0,
	    Info = Info0
	;
	    type_to_ctor_and_args(Type, TypeCtor, _),
	    special_pred_is_generated_lazily(ModuleInfo, TypeCtor),
	    (
		SpecialId = compare,
		unify_proc__add_lazily_generated_compare_pred_decl(TypeCtor,
			PredId, ModuleInfo0, ModuleInfo),
		hlds_pred__initial_proc_id(ProcId)
	    ;
		SpecialId = index,
		% This shouldn't happen. The index predicate should
		% only be called from the compare predicate. If it
		% is called, it shouldn't be generated lazily.
	    	fail
	    ;
		SpecialId = unify,

		%
		% XXX We should only add the declaration, not the body,
		% for the unify pred, but that complicates things
		% if mode analysis is rerun after higher_order.m and
		% requests more unification procedures. In particular,
		% it's difficult to run polymorphism on the new clauses
		% if the predicate's arguments have already had type-infos
		% added. This case shouldn't come up unless an optimization
		% does reordering which requires rescheduling a conjunction.
		%
		unify_proc__add_lazily_generated_unify_pred(TypeCtor,
			PredId, ModuleInfo0, ModuleInfo),
		hlds_pred__in_in_unification_proc_id(ProcId)
	    ),
	    module_info_pred_info(ModuleInfo, PredId, PredInfo),
	    pred_info_module(PredInfo, ModuleName),
	    pred_info_name(PredInfo, Name),
	    SymName = qualified(ModuleName, Name),
	    Info = Info0 ^ global_info ^ module_info := ModuleInfo
	).

:- pred find_builtin_type_with_equivalent_compare(module_info::in,
	(type)::in, (type)::out, bool::out) is det.

find_builtin_type_with_equivalent_compare(ModuleInfo, Type, EqvType,
		NeedIntCast) :-
	classify_type(Type, ModuleInfo, TypeCategory),
	(
		TypeCategory = int_type,
		EqvType = Type,
		NeedIntCast = no
	;
		TypeCategory = char_type,
		EqvType = Type,
		NeedIntCast = no
	;
		TypeCategory = str_type,
		EqvType = Type,
		NeedIntCast = no
	;
		TypeCategory = float_type,
		EqvType = Type,
		NeedIntCast = no
	;
		TypeCategory = pred_type,
		error("pred type in find_builtin_type_with_equivalent_compare")
	;
		TypeCategory = tuple_type,
		error("tuple type in find_builtin_type_with_equivalent_compare")
	;
		TypeCategory = enum_type,
		construct_type(unqualified("int") - 0, [], EqvType),
		NeedIntCast = yes
	;
		TypeCategory = polymorphic_type,
		error("poly type in find_builtin_type_with_equivalent_compare")
	;
		TypeCategory = user_type,
		error("user type in find_builtin_type_with_equivalent_compare")
	).

:- pred specializeable_special_call(special_pred_id::in, proc_id::in)
	is semidet.

specializeable_special_call(SpecialId, CalledProc) :-
	proc_id_to_int(CalledProc, CalledProcInt),
	CalledProcInt = 0,
	(
		SpecialId = unify
	;
		SpecialId = compare
	).

:- pred generate_unsafe_type_cast(module_info::in, prog_context::in,
	(type)::in, prog_var::in, prog_var::out, hlds_goal::out,
	proc_info::in, proc_info::out) is det.

generate_unsafe_type_cast(ModuleInfo, Context, ToType, Arg, CastArg, Goal,
		ProcInfo0, ProcInfo) :-
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	mercury_private_builtin_module(MercuryBuiltin),
	(
		predicate_table_search_pred_m_n_a(PredicateTable,
			MercuryBuiltin, "unsafe_type_cast", 2, [PredIdPrime])
	->
		PredId = PredIdPrime
	;
		error("generate_unsafe_type_cast: pred table lookup failed")
	),
	hlds_pred__initial_proc_id(ProcId),
	proc_info_create_var_from_type(ProcInfo0, ToType, no,
		CastArg, ProcInfo),
	set__list_to_set([Arg, CastArg], NonLocals),
	instmap_delta_from_assoc_list([CastArg - ground(shared, none)],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, Context, GoalInfo),
	Goal = call(PredId, ProcId, [Arg, CastArg], inline_builtin,
		no, qualified(MercuryBuiltin, "unsafe_type_cast")) - GoalInfo.

:- pred unwrap_no_tag_arg((type)::in, prog_context::in, sym_name::in,
	prog_var::in, prog_var::out, hlds_goal::out,
	proc_info::in, proc_info::out) is det.

unwrap_no_tag_arg(WrappedType, Context, Constructor, Arg, UnwrappedArg,
		Goal, ProcInfo0, ProcInfo) :-
	proc_info_create_var_from_type(ProcInfo0, WrappedType, no,
		UnwrappedArg, ProcInfo),
	ConsId = cons(Constructor, 1),
	UniModes = [(ground(shared, none) - free) ->
			(ground(shared, none) - ground(shared, none))],
	in_mode(In),
	out_mode(Out),
	set__list_to_set([Arg, UnwrappedArg], NonLocals),
	% This will be recomputed later.
	instmap_delta_from_assoc_list([UnwrappedArg - ground(shared, none)],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, Context, GoalInfo),
	Goal = unify(Arg, functor(ConsId, [UnwrappedArg]), In - Out,
		deconstruct(Arg, ConsId, [UnwrappedArg], UniModes,
			cannot_fail, no),
		unify_context(explicit, [])) - GoalInfo.

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
:- pred filter_requests(list(request)::out, list(request)::out,
	higher_order_global_info::in, higher_order_global_info::out,
	io__state::di, io__state::uo) is det.

filter_requests(FilteredRequests, LoopRequests, Info0, Info) -->
	{ Requests0 = set__to_sorted_list(Info0 ^ requests) },
	{ Info = Info0 ^ requests := set__init },
	list__foldl2(filter_requests_2(Info), Requests0,
		[] - [], FilteredRequests - LoopRequests).

:- pred filter_requests_2(higher_order_global_info::in, request::in,
	pair(list(request))::in, pair(list(request))::out,
	io__state::di, io__state::uo) is det.

filter_requests_2(Info, Request, AcceptedRequests0 - LoopRequests0,
		AcceptedRequests - LoopRequests) -->
	{ ModuleInfo = Info ^ module_info },
	{ Request = request(CallingPredProcId, CalledPredProcId, _, _, HOArgs,
		_, _, _, IsUserTypeSpec, Context) },
	{ CalledPredProcId = proc(CalledPredId, _) },
	{ module_info_pred_info(ModuleInfo, CalledPredId, PredInfo) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	{ pred_info_module(PredInfo, PredModule) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_arg_types(PredInfo, Types) },
	{ list__length(Types, ActualArity) },
	maybe_write_request(VeryVerbose, ModuleInfo, "Request for",
		qualified(PredModule, PredName), Arity, ActualArity,
		no, HOArgs, Context),
	(
		% Ignore the size limit for user specified specializations.
		{ IsUserTypeSpec = yes }
	->
		maybe_write_string(VeryVerbose,
		"%    request specialized (user-requested specialization)\n"),
		{ AcceptedRequests = [Request | AcceptedRequests0] },
		{ LoopRequests = LoopRequests0 }
	;
		{ map__search(Info ^ goal_sizes, CalledPredId, GoalSize0) ->
			GoalSize = GoalSize0	
		;
			% This can happen for a specialized version.
			GoalSize = 0
		},

		(
			{ GoalSize > Info ^ ho_params ^ size_limit }
		->
			{ AcceptedRequests = AcceptedRequests0 },
			{ LoopRequests = LoopRequests0 },
			maybe_write_string(VeryVerbose,
			"%    not specializing (goal too large).\n")
		;
			{ higher_order_args_size(HOArgs) >
				Info ^ ho_params ^ arg_limit }
		->
			% If the arguments are too large, we can
			% end up producing a specialized version
			% with massive numbers of arguments, because
			% all of the curried arguments are passed as
			% separate arguments.
			% Without this extras/xml/xml.parse.chars.m
			% takes forever to compile.
			{ AcceptedRequests = AcceptedRequests0 },
			{ LoopRequests = LoopRequests0 },
			maybe_write_string(VeryVerbose,
			"%    not specializing (args too large).\n")
		;
			%
			% To ensure termination of the specialization
			% process, the depth of the higher-order arguments
			% must strictly decrease compared to parents with
			% the same original pred_proc_id.
			%
			{ VersionInfoMap = Info ^ version_info },
			{
				map__search(VersionInfoMap, CalledPredProcId,
					CalledVersionInfo)
			->
				CalledVersionInfo = version_info(
					OrigPredProcId, _, _, _)
			;
				OrigPredProcId = CalledPredProcId
			},
			{ map__search(VersionInfoMap, CallingPredProcId,
				CallingVersionInfo) },		
			{ CallingVersionInfo = version_info(_,
				_, _, ParentVersions) },
			{ ArgDepth = higher_order_args_depth(HOArgs) },
			{ some [ParentVersion] (
				list__member(ParentVersion, ParentVersions),
				ParentVersion = parent_version_info(
					OrigPredProcId, OldArgDepth),
				ArgDepth >= OldArgDepth
			) }
		->
			{ AcceptedRequests = AcceptedRequests0 },
			{ LoopRequests = [Request | LoopRequests0] },
			maybe_write_string(VeryVerbose,
			"%    not specializing (recursive specialization).\n")
		;
			maybe_write_string(VeryVerbose,
			"%    request specialized.\n"),
			{ AcceptedRequests = [Request | AcceptedRequests0] },
			{ LoopRequests = LoopRequests0 }
		)
	).

:- pred create_new_preds(list(request)::in, list(new_pred)::in,
	list(new_pred)::out, set(pred_proc_id)::in, set(pred_proc_id)::out,
	higher_order_global_info::in, higher_order_global_info::out,
	io__state::di, io__state::uo) is det.

create_new_preds([], NewPredList, NewPredList, ToFix, ToFix,
		Info, Info, IO, IO).
create_new_preds([Request | Requests], NewPredList0, NewPredList,
		PredsToFix0, PredsToFix, Info0, Info, IO0, IO)  :-
	Request = request(CallingPredProcId, CalledPredProcId, _HOArgs,
		_CallArgs, _, _CallerArgTypes, _, _, _, _),
	set__insert(PredsToFix0, CallingPredProcId, PredsToFix1),
	(
		map__search(Info0 ^ new_preds, CalledPredProcId, SpecVersions0)
	->
		(
			% check that we aren't redoing the same pred
			% SpecVersions are pred_proc_ids of the specialized
			% versions of the current pred.
			\+ (
				set__member(Version, SpecVersions0),
				version_matches(Info0 ^ ho_params,
					Info0 ^ module_info,
					Request, Version, _)
			)
		->
			create_new_pred(Request, NewPred, Info0, Info1,
				IO0, IO1),
			NewPredList1 = [NewPred | NewPredList0]
		;
			NewPredList1 = NewPredList0,
			Info1 = Info0,
			IO1 = IO0
		)
	;
		create_new_pred(Request, NewPred, 
			Info0, Info1, IO0, IO1),
		NewPredList1 = [NewPred | NewPredList0]
	),
	create_new_preds(Requests, NewPredList1, NewPredList,
		PredsToFix1, PredsToFix, Info1, Info, IO1, IO).

	% If we weren't allowed to create a specialized version because the
	% loop check failed, check whether the version was created for another
	% request for which the loop check succeeded.
:- pred check_loop_request(higher_order_global_info::in, request::in,
	set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

check_loop_request(Info, Request, PredsToFix0, PredsToFix) :-
	Request = request(CallingPredProcId, CalledPredProcId,
			_, _, _, _, _, _, _, _),
	(
		map__search(Info ^ new_preds, CalledPredProcId, SpecVersions0),
		some [Version] (
			set__member(Version, SpecVersions0),
			version_matches(Info ^ ho_params, Info ^ module_info,
				Request, Version, _)
		)
	->
		set__insert(PredsToFix0, CallingPredProcId, PredsToFix)
	;
		PredsToFix = PredsToFix0
	).

		% Here we create the pred_info for the new predicate.
:- pred create_new_pred(request::in, new_pred::out,
	higher_order_global_info::in, higher_order_global_info::out,
	io__state::di, io__state::uo) is det.

create_new_pred(Request, NewPred, Info0, Info, IOState0, IOState) :-
	Request = request(Caller, CalledPredProc, CallArgs, ExtraTypeInfoTVars,
			HOArgs, ArgTypes, TypeInfoLiveness,
			CallerTVarSet, IsUserTypeSpec, Context),
	ModuleInfo0 = Info0 ^ module_info,
	module_info_pred_proc_info(ModuleInfo0, CalledPredProc,
		PredInfo0, ProcInfo0),

	pred_info_name(PredInfo0, Name0),
	pred_info_arity(PredInfo0, Arity),
	pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
	pred_info_module(PredInfo0, PredModule),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose,
							IOState0, IOState1),
        pred_info_arg_types(PredInfo0, ArgTVarSet, ExistQVars, Types),

	( IsUserTypeSpec = yes ->
		% If this is a user-guided type specialisation, the new name
		% comes from the name and mode number of the requesting
		% predicate. The mode number is included because we want to
		% avoid the creation of more than one predicate with the same
		% name if more than one mode of a predicate is specialized.
		% Since the names of e.g. deep profiling proc_static structures
		% are derived from the names of predicates, duplicate predicate
		% names lead to duplicate global variable names and hence to
		% link errors.
		Caller = proc(CallerPredId, CallerProcId),
		predicate_name(ModuleInfo0, CallerPredId, PredName0),
		proc_id_to_int(CallerProcId, CallerProcInt),

		% The higher_order_arg_order_version part is to avoid
		% segmentation faults or other errors when the order
		% or number of extra arguments changes.
		% If the user does not recompile all affected code,
		% the program will not link.
		PredName = string__append_list(
			[PredName0, "_", int_to_string(CallerProcInt), "_",
			int_to_string(higher_order_arg_order_version)]),
		SymName = qualified(PredModule, PredName),
		Info1 = Info0,
		NewProcId = CallerProcId,
		% For exported predicates the type specialization must
		% be exported.
		% For opt_imported predicates we only want to keep this
		% version if we do some other useful specialization on it.
		pred_info_import_status(PredInfo0, Status)
	;
		hlds_pred__initial_proc_id(NewProcId),
		NextHOid = Info0 ^ next_higher_order_id,
		Info1 = Info0 ^ next_higher_order_id := NextHOid + 1,
		string__int_to_string(NextHOid, IdStr),
		string__append_list([Name0, "__ho", IdStr], PredName),
		SymName = qualified(PredModule, PredName),
		Status = local
	),

	list__length(Types, ActualArity),
	maybe_write_request(VeryVerbose, ModuleInfo0, "Specializing",
		qualified(PredModule, Name0), Arity, ActualArity,
		yes(PredName), HOArgs, Context, IOState1, IOState),

	pred_info_typevarset(PredInfo0, TypeVarSet),
	pred_info_get_markers(PredInfo0, MarkerList),
	pred_info_get_goal_type(PredInfo0, GoalType),
	pred_info_get_class_context(PredInfo0, ClassContext),
	pred_info_get_aditi_owner(PredInfo0, Owner),
	varset__init(EmptyVarSet),
	map__init(EmptyVarTypes),
	map__init(EmptyTVarNameMap),
	map__init(EmptyProofs),
	map__init(EmptyTIMap),
	map__init(EmptyTCIMap),

	% This isn't looked at after here, and just clutters up
	% hlds dumps if it's filled in.
	ClausesInfo = clauses_info(EmptyVarSet, EmptyVarTypes,
		EmptyTVarNameMap, EmptyVarTypes, [], [],
		EmptyTIMap, EmptyTCIMap, no),
	pred_info_init(PredModule, SymName, Arity, ArgTVarSet, ExistQVars,
		Types, true, Context, ClausesInfo, Status, MarkerList, GoalType,
		PredOrFunc, ClassContext, EmptyProofs, Owner, NewPredInfo0),
	pred_info_set_typevarset(NewPredInfo0, TypeVarSet, NewPredInfo1),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, NewPredInfo1, NewPredId, PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable, ModuleInfo1),

	Info2 = Info1 ^ module_info := ModuleInfo1,

	NewPred = new_pred(proc(NewPredId, NewProcId), CalledPredProc, Caller,
		SymName, HOArgs, CallArgs, ExtraTypeInfoTVars, ArgTypes,
		TypeInfoLiveness, CallerTVarSet, IsUserTypeSpec),

	add_new_pred(CalledPredProc, NewPred, Info2, Info3),

	create_new_proc(NewPred, ProcInfo0,
		NewPredInfo1, NewPredInfo, Info3, Info4),
	module_info_set_pred_info(Info4 ^ module_info, NewPredId, NewPredInfo,
		ModuleInfo),
	Info = Info4 ^ module_info := ModuleInfo.

:- pred add_new_pred(pred_proc_id::in, new_pred::in,
	higher_order_global_info::in, higher_order_global_info::out) is det.

add_new_pred(CalledPredProcId, NewPred, Info0, Info) :-
	( map__search(Info0 ^ new_preds, CalledPredProcId, SpecVersions0) ->
		set__insert(SpecVersions0, NewPred, SpecVersions)
	;
		set__singleton_set(SpecVersions, NewPred)
	),
	map__set(Info0 ^ new_preds, CalledPredProcId, SpecVersions, NewPreds),
	Info = Info0 ^ new_preds := NewPreds.

:- pred maybe_write_request(bool::in, module_info::in, string::in,
	sym_name::in, arity::in, arity::in, maybe(string)::in,
	list(higher_order_arg)::in, prog_context::in,
	io__state::di, io__state::uo) is det.

maybe_write_request(no, _, _, _, _, _, _, _, _) --> [].
maybe_write_request(yes, ModuleInfo, Msg, SymName,
		Arity, ActualArity, MaybeNewName, HOArgs, Context) -->
	{ prog_out__sym_name_to_string(SymName, OldName) },
	{ string__int_to_string(Arity, ArStr) },
	io__write_string("% "),
	prog_out__write_context(Context),
	io__write_strings([Msg, " `", OldName, "'/", ArStr]),

	( { MaybeNewName = yes(NewName) } ->
		io__write_string(" into "),
		io__write_string(NewName)
	;
		[]
	),
	io__write_string(" with higher-order arguments:\n"),
	{ NumToDrop is ActualArity - Arity },
	output_higher_order_args(ModuleInfo, NumToDrop, 0, HOArgs).

:- pred output_higher_order_args(module_info::in, int::in, int::in,
	list(higher_order_arg)::in, io__state::di, io__state::uo) is det.

output_higher_order_args(_, _, _, []) --> [].
output_higher_order_args(ModuleInfo, NumToDrop, Indent, [HOArg | HOArgs]) -->
	{ HOArg = higher_order_arg(ConsId, ArgNo, NumArgs,
			_, _, CurriedHOArgs, IsConst) },
	io__write_string("% "),
	{ list__duplicate(Indent + 1, "  ", Spaces) }, 
	list__foldl(io__write_string, Spaces),
	( { IsConst = yes } ->
		io__write_string("const ")
	;
		[]
	),
	( { ConsId = pred_const(PredId, _ProcId, _) } ->
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_name(PredInfo, Name) },
		{ pred_info_arity(PredInfo, Arity) },
			% adjust message for type_infos
		{ DeclaredArgNo is ArgNo - NumToDrop },
		io__write_string("HeadVar__"),
		io__write_int(DeclaredArgNo),
		io__write_string(" = `"),
		io__write_string(Name),
		io__write_string("'/"),
		io__write_int(Arity)
	; { ConsId = type_ctor_info_const(TypeModule, TypeName, TypeArity) } ->
		io__write_string("type_ctor_info for `"),
		prog_out__write_sym_name(qualified(TypeModule, TypeName)),
		io__write_string("'/"),
		io__write_int(TypeArity)
	; { ConsId = base_typeclass_info_const(_, ClassId, _, _) } ->
		io__write_string("base_typeclass_info for `"),
		{ ClassId = class_id(ClassName, ClassArity) },
		prog_out__write_sym_name(ClassName),
		io__write_string("'/"),
		io__write_int(ClassArity)
	;
		% XXX output the type.
		io__write_string("type_info/typeclass_info ")
	),
	io__write_string(" with "),
	io__write_int(NumArgs),
	io__write_string(" curried arguments"),
	( { CurriedHOArgs = [] } ->
		io__nl
	;
		io__write_string(":\n"),
		output_higher_order_args(ModuleInfo, 0,
			Indent + 1, CurriedHOArgs)
	),	
	output_higher_order_args(ModuleInfo, NumToDrop, Indent, HOArgs).

%-----------------------------------------------------------------------------%

:- pred fixup_preds(list(pred_proc_id)::in, higher_order_global_info::in,
		higher_order_global_info::out) is det.

fixup_preds(PredProcIds, Info0, Info) :-
	MustRecompute = no,
	Requests0 = Info0 ^ requests,
	list__foldl(fixup_pred(MustRecompute), PredProcIds, Info0, Info1),

	% Any additional requests must have already been denied.
	Info = Info1 ^ requests := Requests0.

:- pred fixup_specialized_versions(list(new_pred)::in,
	higher_order_global_info::in, higher_order_global_info::out) is det.

fixup_specialized_versions(NewPredList, Info0, Info) :-
	list__map(
		(pred(NewPred::in, PredProcId::out) is det :-
			NewPred = new_pred(PredProcId, _, _,
					_, _, _, _, _, _, _, _)
		),
		NewPredList, NewPredProcIds),

	%
	% Reprocess the goals to find any new specializations made
	% possible by the specializations performed in this pass.
	%
	MustRecompute = yes,
	list__foldl(fixup_pred(MustRecompute), NewPredProcIds,
		Info0, Info).

	% Fixup calls to specialized predicates.
:- pred fixup_pred(bool::in, pred_proc_id::in,
	higher_order_global_info::in, higher_order_global_info::out) is det.

fixup_pred(MustRecompute, proc(PredId, ProcId), GlobalInfo0, GlobalInfo) :-
	traverse_proc(MustRecompute, PredId, ProcId, GlobalInfo0, GlobalInfo).

%-----------------------------------------------------------------------------%

	% Build a proc_info for a specialized version.
:- pred create_new_proc(new_pred::in, proc_info::in, pred_info::in,
		pred_info::out, higher_order_global_info::in,
		higher_order_global_info::out) is det.

create_new_proc(NewPred, NewProcInfo0, NewPredInfo0,
		NewPredInfo, Info0, Info) :-
	ModuleInfo = Info0 ^ module_info,

	NewPred = new_pred(NewPredProcId, OldPredProcId, CallerPredProcId,
		_Name, HOArgs0, CallArgs, ExtraTypeInfoTVars0, CallerArgTypes0,
		_, _, _),

	proc_info_headvars(NewProcInfo0, HeadVars0),
	proc_info_argmodes(NewProcInfo0, ArgModes0),
	pred_info_get_exist_quant_tvars(NewPredInfo0, ExistQVars0),
	pred_info_typevarset(NewPredInfo0, TypeVarSet0),
	pred_info_arg_types(NewPredInfo0, OriginalArgTypes0),

	CallerPredProcId = proc(CallerPredId, CallerProcId),
	module_info_pred_proc_info(ModuleInfo, CallerPredId, CallerProcId,
		CallerPredInfo, CallerProcInfo),
	pred_info_typevarset(CallerPredInfo, CallerTypeVarSet),
	pred_info_get_univ_quant_tvars(CallerPredInfo, CallerHeadParams),
	proc_info_typeinfo_varmap(CallerProcInfo, CallerTypeInfoVarMap0),

	%
	% Specialize the types of the called procedure as for inlining.
	%
	proc_info_vartypes(NewProcInfo0, VarTypes0),
	varset__merge_subst(CallerTypeVarSet, TypeVarSet0,
		TypeVarSet, TypeRenaming),
	apply_substitution_to_type_map(VarTypes0, TypeRenaming, VarTypes1),
	term__apply_substitution_to_list(OriginalArgTypes0,
		TypeRenaming, OriginalArgTypes1),

	% the real set of existentially quantified variables may be
	% smaller, but this is OK
	term__var_list_to_term_list(ExistQVars0, ExistQTypes0),
	term__apply_substitution_to_list(ExistQTypes0, TypeRenaming,
		ExistQTypes1),
	term__term_list_to_var_list(ExistQTypes1, ExistQVars1),

	inlining__get_type_substitution(OriginalArgTypes1, CallerArgTypes0,
		CallerHeadParams, ExistQVars1, TypeSubn),

	term__apply_rec_substitution_to_list(ExistQTypes1, TypeSubn,
		ExistQTypes),
	ExistQVars = list__filter_map(
			(func(ExistQType) = ExistQVar is semidet :-
				ExistQType = term__variable(ExistQVar)
			), ExistQTypes),

	apply_rec_substitution_to_type_map(VarTypes1, TypeSubn, VarTypes2),
	term__apply_rec_substitution_to_list(OriginalArgTypes1, TypeSubn,
		OriginalArgTypes),
	proc_info_set_vartypes(NewProcInfo0, VarTypes2, NewProcInfo1),

	term__var_list_to_term_list(ExtraTypeInfoTVars0,
		ExtraTypeInfoTVarTypes0),
	( (map__is_empty(TypeSubn) ; ExistQVars = []) ->
		HOArgs = HOArgs0,
		ExtraTypeInfoTVarTypes = ExtraTypeInfoTVarTypes0,
		ExtraTypeInfoTVars = ExtraTypeInfoTVars0
	;
		% If there are existentially quantified variables in the
		% callee we may need to bind type variables in the caller.
		list__map(substitute_higher_order_arg(TypeSubn),
			HOArgs0, HOArgs),

		term__apply_rec_substitution_to_list(ExtraTypeInfoTVarTypes0,
			TypeSubn, ExtraTypeInfoTVarTypes),
		% The substitution should never bind any of the type variables
		% for which extra type-infos are needed, otherwise it
		% wouldn't be necessary to add them.
		term__term_list_to_var_list(ExtraTypeInfoTVarTypes,
			ExtraTypeInfoTVars)
	),

	% Add in the extra typeinfo vars.
	list__map(polymorphism__build_type_info_type,
		ExtraTypeInfoTVarTypes, ExtraTypeInfoTypes),
	proc_info_create_vars_from_types(NewProcInfo1, ExtraTypeInfoTypes,
		ExtraTypeInfoVars, NewProcInfo2),

	map__from_corresponding_lists(CallArgs, HeadVars0, VarRenaming0),

	% Construct the constant input closures within the goal
	% for the called procedure.
	map__init(PredVars0),
	construct_higher_order_terms(ModuleInfo, HeadVars0, ExtraHeadVars,
		ArgModes0, ExtraArgModes, HOArgs, NewProcInfo2, NewProcInfo3,
		VarRenaming0, VarRenaming, PredVars0, PredVars, ConstGoals),

	%
	% Record extra information about this version.
	%
	VersionInfoMap0 = Info0 ^ version_info,
	ArgsDepth = higher_order_args_depth(HOArgs),

	( map__search(VersionInfoMap0, OldPredProcId, OldProcVersionInfo) ->
		OldProcVersionInfo = version_info(OrigPredProcId, _, _, _)
	;
		OrigPredProcId = OldPredProcId
	),

	( map__search(VersionInfoMap0, CallerPredProcId, CallerVersionInfo) ->
		CallerVersionInfo = version_info(_, _, _, CallerParentVersions)
	;
		CallerParentVersions = []
	),
	ParentVersions =
		[parent_version_info(OrigPredProcId, ArgsDepth)
		| CallerParentVersions],

	VersionInfo = version_info(OrigPredProcId, ArgsDepth,
			PredVars, ParentVersions),
	map__det_insert(VersionInfoMap0, NewPredProcId, VersionInfo,
		VersionInfoMap),
	Info = Info0 ^ version_info := VersionInfoMap,

	%
	% Fix up the typeinfo_varmap.
	%
	proc_info_typeinfo_varmap(NewProcInfo3, TypeInfoVarMap0),

	% Restrict the caller's typeinfo_varmap
	% down onto the arguments of the call.
	map__to_assoc_list(CallerTypeInfoVarMap0, TypeInfoAL0),
	list__filter(
		(pred(TVarAndLocn::in) is semidet :-
			TVarAndLocn = _ - Locn,
			type_info_locn_var(Locn, LocnVar),
			map__contains(VarRenaming, LocnVar)
		), TypeInfoAL0, TypeInfoAL),
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
		TypeInfoVarMap2),

	% Add entries in the typeinfo_varmap for the extra type-infos.
	list__map(
		(pred(TypeInfoVar::in, type_info(TypeInfoVar)::out) is det),
		ExtraTypeInfoVars, ExtraTypeInfoLocns),
	map__from_corresponding_lists(ExtraTypeInfoTVars, ExtraTypeInfoLocns,
		ExtraTypeInfoMap),
	map__overlay(TypeInfoVarMap2, ExtraTypeInfoMap, TypeInfoVarMap),

	proc_info_set_typeinfo_varmap(NewProcInfo3,
		TypeInfoVarMap, NewProcInfo4),

	%
	% Fix up the argument vars, types and modes.
	%

	in_mode(InMode),
	list__length(ExtraTypeInfoVars, NumTypeInfos),
	list__duplicate(NumTypeInfos, InMode, ExtraTypeInfoModes),

	remove_const_higher_order_args(1, HeadVars0, HOArgs, HeadVars1),
	remove_const_higher_order_args(1, ArgModes0, HOArgs, ArgModes1),
	list__condense([ExtraTypeInfoVars, ExtraHeadVars, HeadVars1],
		HeadVars),
	list__condense([ExtraTypeInfoModes, ExtraArgModes, ArgModes1],
		ArgModes),
	proc_info_set_headvars(NewProcInfo4, HeadVars, NewProcInfo5),
	proc_info_set_argmodes(NewProcInfo5, ArgModes, NewProcInfo6),

	proc_info_goal(NewProcInfo6, Goal6),
	Goal6 = _ - GoalInfo6,
	goal_to_conj_list(Goal6, GoalList6),
	conj_list_to_goal(list__append(ConstGoals, GoalList6),
		GoalInfo6, Goal),
	proc_info_set_goal(NewProcInfo6, Goal, NewProcInfo7),

	proc_info_vartypes(NewProcInfo7, VarTypes7),
	map__apply_to_list(ExtraHeadVars, VarTypes7, ExtraHeadVarTypes0),
	remove_const_higher_order_args(1, OriginalArgTypes,
		HOArgs, ModifiedOriginalArgTypes),
	list__condense(
		[ExtraTypeInfoTypes, ExtraHeadVarTypes0,
			ModifiedOriginalArgTypes],
		ArgTypes),	
	pred_info_set_arg_types(NewPredInfo0, TypeVarSet,
		ExistQVars, ArgTypes, NewPredInfo1),
	pred_info_set_typevarset(NewPredInfo1, TypeVarSet, NewPredInfo2),

	%
	% The types of the headvars in the vartypes map in the
	% proc_info may be more specific than the argument types
	% returned by pred_info_argtypes if the procedure body
	% binds some existentially quantified type variables.
	% The types of the extra arguments added by
	% construct_higher_order_terms use the substitution
	% computed based on the result pred_info_arg_types.
	% We may need to apply a substitution to the types of the
	% new variables in the vartypes in the proc_info.
	%
	% XXX We should apply this substitution to the variable
	% types in any callers of this predicate, which may
	% introduce other opportunities for specialization.
	%
	(
		ExistQVars = []
	->
		NewProcInfo8 = NewProcInfo7
	;
		map__apply_to_list(HeadVars0, VarTypes7, OriginalHeadTypes),
		(
			type_list_subsumes(OriginalArgTypes,
				OriginalHeadTypes, ExistentialSubn)
		->
			term__apply_rec_substitution_to_list(ExtraHeadVarTypes0,
				ExistentialSubn, ExtraHeadVarTypes),
			assoc_list__from_corresponding_lists(ExtraHeadVars,
				ExtraHeadVarTypes, ExtraHeadVarsAndTypes),
			list__foldl(
			    (pred(VarAndType::in, Map0::in, Map::out) is det :-
				VarAndType = Var - Type,
				map__det_update(Map0, Var, Type, Map)
			    ),
			    ExtraHeadVarsAndTypes, VarTypes7, VarTypes8),
			proc_info_set_vartypes(NewProcInfo7,
				VarTypes8, NewProcInfo8)
		;
			error(
		"higher_order__create_new_proc: type_list_subsumes failed")
		)
	),

	%
	% Apply the substitutions to the types in the original
	% typeclass_info_varmap.
	%
	proc_info_typeclass_info_varmap(NewProcInfo8, TCVarMap0),
	apply_substitutions_to_typeclass_var_map(TCVarMap0, TypeRenaming,
		TypeSubn, EmptyVarRenaming, TCVarMap),
	proc_info_set_typeclass_info_varmap(NewProcInfo8,
		TCVarMap, NewProcInfo9),

	%
	% Find the new class context by searching the argument types
	% for typeclass_infos (the corresponding constraint is encoded
	% in the type of a typeclass_info).
	%
	find_class_context(ModuleInfo, ArgTypes, ArgModes,
		[], [], ClassContext),
	pred_info_set_class_context(NewPredInfo2, ClassContext, NewPredInfo3),

	map__init(NewProcs0),
	NewPredProcId = proc(_, NewProcId),
	map__det_insert(NewProcs0, NewProcId, NewProcInfo9, NewProcs),
	pred_info_set_procedures(NewPredInfo3, NewProcs, NewPredInfo).

		% Take an original list of headvars and arg_modes and
		% return these with curried arguments added.
		% The old higher-order arguments are left in. They may be
		% needed in calls which could not be specialised. If not,
		% unused_args.m can clean them up.
		%
		% Build the initial pred_vars map which records
		% higher-order and type_info constants for a call to
		% traverse_goal.
		%
		% Build a var-var renaming from the requesting
		% call's arguments to the headvars of the specialized
		% version.
		%
		% This predicate is recursively applied to all curried
		% higher order arguments of higher order arguments.
		%
		% Update higher_order_arg_order_version if the order or
		% number of the arguments for specialized versions changes.
:- pred construct_higher_order_terms(module_info::in, list(prog_var)::in,
		list(prog_var)::out, list(mode)::in, list(mode)::out,
		list(higher_order_arg)::in, proc_info::in, proc_info::out,
		map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
		pred_vars::in, pred_vars::out, list(hlds_goal)::out) is det.

construct_higher_order_terms(_, _, [], _, [], [], ProcInfo, ProcInfo,
		Renaming, Renaming, PredVars, PredVars, []).
construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars, ArgModes0,
		NewArgModes, [HOArg | HOArgs], ProcInfo0, ProcInfo,
		Renaming0, Renaming, PredVars0, PredVars, ConstGoals) :-
	HOArg = higher_order_arg(ConsId, Index, NumArgs,
		CurriedArgs, CurriedArgTypes, CurriedHOArgs, IsConst),

	list__index1_det(HeadVars0, Index, LVar),
	( ConsId = pred_const(PredId, ProcId, _) ->
		% Add the curried arguments to the procedure's argument list.
		module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			CalledPredInfo, CalledProcInfo),
		pred_info_get_is_pred_or_func(CalledPredInfo, PredOrFunc),
		proc_info_argmodes(CalledProcInfo, CalledArgModes),
		(
			list__split_list(NumArgs, CalledArgModes,
				CurriedArgModes0, NonCurriedArgModes0)
		->
			NonCurriedArgModes = NonCurriedArgModes0,
			CurriedArgModes1 = CurriedArgModes0
		;
			error("list__split_list_failed")
		),
		proc_info_interface_determinism(CalledProcInfo,
			ProcDetism),
		GroundInstInfo = higher_order(pred_inst_info(PredOrFunc,
				NonCurriedArgModes, ProcDetism))
	;
		in_mode(InMode),
		GroundInstInfo = none,
		list__duplicate(NumArgs, InMode, CurriedArgModes1)
	),

	proc_info_create_vars_from_types(ProcInfo0, CurriedArgTypes,
		CurriedHeadVars1, ProcInfo1),

	( IsConst = no ->
		% Make traverse_goal pretend that the input higher-order
		% argument is built using the new arguments as its curried
		% arguments.
		map__det_insert(PredVars0, LVar,
			constant(ConsId, CurriedHeadVars1), PredVars1)
	;
		PredVars1 = PredVars0
	),

	assoc_list__from_corresponding_lists(CurriedArgs,
		CurriedHeadVars1, CurriedRenaming),
	list__foldl(
		(pred(VarPair::in, Map0::in, Map::out) is det :-
			VarPair = Var1 - Var2,
			map__set(Map0, Var1, Var2, Map)
		), CurriedRenaming, Renaming0, Renaming1),

	% Recursively construct the curried higher-order arguments.
	construct_higher_order_terms(ModuleInfo, CurriedHeadVars1,
		ExtraCurriedHeadVars, CurriedArgModes1, ExtraCurriedArgModes,
		CurriedHOArgs, ProcInfo1, ProcInfo2, Renaming1, Renaming2,
		PredVars1, PredVars2, CurriedConstGoals),

	% Construct the rest of the higher-order arguments.
	construct_higher_order_terms(ModuleInfo, HeadVars0, NewHeadVars1,
		ArgModes0, NewArgModes1, HOArgs, ProcInfo2, ProcInfo,
		Renaming2, Renaming, PredVars2, PredVars, ConstGoals1),
	
	( IsConst = yes ->
		%
		% Build the constant inside the specialized version,
		% so that other constants which include it will
		% be recognized as constant.
		%
		mode_util__modes_to_uni_modes(CurriedArgModes1,
			CurriedArgModes1, ModuleInfo, UniModes),
		set__list_to_set(CurriedHeadVars1, ConstNonLocals),
		ConstInst = ground(shared, GroundInstInfo),
		instmap_delta_from_assoc_list([LVar - ConstInst],
			ConstInstMapDelta),
		goal_info_init(ConstNonLocals, ConstInstMapDelta,
			det, ConstGoalInfo),
		RHS = functor(ConsId, CurriedHeadVars1),
		UniMode = (free -> ConstInst) - (ConstInst -> ConstInst),
		ConstGoal = unify(LVar, RHS, UniMode,
			construct(LVar, ConsId, CurriedHeadVars1, UniModes,
				construct_dynamically, cell_is_unique, no),	
			unify_context(explicit, [])) - ConstGoalInfo,
		ConstGoals0 = CurriedConstGoals ++ [ConstGoal]
	;
		ConstGoals0 = CurriedConstGoals
	),	

	% Fix up the argument lists.
	remove_const_higher_order_args(1, CurriedHeadVars1, CurriedHOArgs,
		CurriedHeadVars),
	remove_const_higher_order_args(1, CurriedArgModes1, CurriedHOArgs,
		CurriedArgModes),
	list__condense([CurriedHeadVars, ExtraCurriedHeadVars, NewHeadVars1],
		NewHeadVars),
	list__condense([CurriedArgModes, ExtraCurriedArgModes, NewArgModes1],
		NewArgModes),
	list__append(ConstGoals0, ConstGoals1, ConstGoals).

:- pred remove_const_higher_order_args(int::in, list(T)::in,
		list(higher_order_arg)::in, list(T)::out) is det.

remove_const_higher_order_args(_, [], _, []).
remove_const_higher_order_args(Index, [Arg | Args0], HOArgs0, Args) :-
	( HOArgs0 = [HOArg | HOArgs] ->
		HOArg = higher_order_arg(_, HOIndex, _, _, _, _, IsConst),
		( HOIndex = Index ->
			remove_const_higher_order_args(Index + 1, Args0,
				HOArgs, Args1),
			( IsConst = yes ->
				Args = Args1
			;
				Args = [Arg | Args1]	
			)
		; HOIndex > Index ->
			remove_const_higher_order_args(Index + 1, Args0,
				HOArgs0, Args1),
			Args = [Arg | Args1]
		;
			error("remove_const_higher_order_args")
		)
			
	;
		Args = [Arg | Args0]
	).

:- func higher_order_arg_order_version = int.

higher_order_arg_order_version = 1.

%-----------------------------------------------------------------------------%

	% Substitute the types in a higher_order_arg.
:- pred substitute_higher_order_arg(tsubst::in, higher_order_arg::in,
		higher_order_arg::out) is det.

substitute_higher_order_arg(Subn, HOArg0, HOArg) :-
	HOArg0 = higher_order_arg(A, B, C, D,
		CurriedArgTypes0, CurriedHOArgs0, G),
	term__apply_rec_substitution_to_list(CurriedArgTypes0,
		Subn, CurriedArgTypes),
	list__map(substitute_higher_order_arg(Subn),
		CurriedHOArgs0, CurriedHOArgs),
	HOArg = higher_order_arg(A, B, C, D,
		CurriedArgTypes, CurriedHOArgs, G).

%-----------------------------------------------------------------------------%

:- func higher_order_args_size(list(higher_order_arg)) = int.

higher_order_args_size(Args) =
		list__foldl(int__max,
			list__map(higher_order_arg_size, Args), 0).

:- func higher_order_arg_size(higher_order_arg) = int.

higher_order_arg_size(higher_order_arg(_, _, _, _, _, CurriedArgs, _)) =
		1 + higher_order_args_size(CurriedArgs).

:- func higher_order_args_depth(list(higher_order_arg)) = int.

higher_order_args_depth(Args) =
		list__foldl(int__max,
			list__map(higher_order_arg_depth, Args), 0).

:- func higher_order_arg_depth(higher_order_arg) = int.

higher_order_arg_depth(higher_order_arg(_, _, _, _, _, CurriedArgs, _)) =
		1 + higher_order_args_size(CurriedArgs).

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

maybe_add_constraint(Constraints0, Constraint, Constraints) :-
	(
		% Remove duplicates.
		\+ list__member(Constraint, Constraints0)
	->
		Constraints = [Constraint | Constraints0]
	;
		Constraints = Constraints0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
