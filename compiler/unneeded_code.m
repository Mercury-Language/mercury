%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module implements two related source-to-source transforms,
% both of which focus on goals that produce some variables, where these
% variables are not always required by the following computation.
%
% If there are no computation paths on which the variables produced by a goal
% may be needed, then the first transform deletes that goal.
%
% If the variables produced by a goal may be needed on some but not all
% computation paths, then the second transform moves that goal to the starts
% of those computation paths, thus avoiding the cost of executing the goal
% on all other computation paths. (This is related to the concept of partial
% redundancy elimination (PRE) for imperative languages.)
%
% Mercury has two constructs that make it possible for a variable to be needed
% on some computation paths but not others: switches and if-then-elses.
%
% In the case of switches, the alternative computation paths are those
% corresponding to the possible values of the switched-on variable, and
% not just the switch arms. Even if all switch arms need a variable, it
% is an optimization to copy the code generating that variable to the starts of
% all the switch arms if the switch is can_fail, i.e. there are some function
% symbols that the switched-on variable can be bound to that do not have arms.
%
% In the case of if-then-elses, the alternatives are the then part and
% the else part. Any variable needed by the condition is needed in both those
% computation paths.
%
% From the point of view of this transform, disjunctions are not branched
% control structures, because entering a disjunct does not preclude later
% entering another disjunct. Any variable needed by any disjunct must therefore
% be produced before control enters the disjunction. (In theory, a disjunct
% that cannot fail in a model_semi disjunction prevents entry to the following
% disjuncts, but any such following disjuncts will have been removed long ago
% by simplification.)
%
% Note that by avoiding the execution of a goal that appears in the original
% source code of the program, both these transforms can in general change the
% operational semantics of the program. Therefore a goal can only be eliminated
% or moved if the goal is has no observable effect except the result it
% generates (i.e is pure, cannot fail, cannot loop, cannot raise an exception),
% which is usually true only of goals composed entirely of builtins, or if
% the semantics options explicitly permit the change in the operational
% semantics, which will usually be an improvement (e.g. avoiding an infinite
% loop or an unnecessary exception).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__unneeded_code.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module io.

:- pred unneeded_code__process_proc_msg(pred_id::in, proc_id::in,
	proc_info::in, proc_info::out, module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__prog_data.
:- import_module hlds__hlds_goal, hlds__instmap, hlds__quantification.
:- import_module hlds__goal_form, hlds__passes_aux, hlds__hlds_out.
:- import_module check_hlds__inst_match, check_hlds__mode_util.
:- import_module check_hlds__goal_path.
:- import_module ll_backend__code_aux.
:- import_module libs__globals, libs__options.

:- import_module bool, int, list, assoc_list, map, set, std_util, require.

	% The branch_alts and branch_point types record the information the
	% transform needs to know about a particular branched control
	% structure: where it is, what kind it is, and how many alternatives
	% it has.

:- type branch_point
	--->	branch_point(
			goal_path,	% The position of the branch point.
			branch_alts	% What kind of goal the branch point
					% is, and many branches it has.
					% Note that the second argument is a
					% function of the first.
		).

:- type branch_alts
	--->	ite			% If-then-elses always have two
					% alternatives: the then branch
					% (numbered 1) and the else branch
					% (numbered 2).

	;	switch(int).		% The number of alternatives in a
					% switch is equal to the number of
					% function symbols in the type of
					% the switched-on variable; this number
					% is given by the argument. If the
					% switch cannot_fail, then this will be
					% equal to the number of cases;
					% if the switch can_fail, there will be
					% strictly fewer cases than this.

	% The location type identifies one arm of a branched control structure.
	% The branched control structure id is a branch_point instead of a
	% simple goal_path because without the branch_alts info, the
	% transformation cannot tell if a given set of branches of a branched
	% control structure covers all possible execution paths or not.

:- type location
	--->	location(
			branch_point,	% To which branched control structure
					% does the location belong.
			int		% The branch within that control
					% structure.
		).

	% The where_needed_map type maps each variable to the set of
	% computation branches where it is needed. If a variable is needed
	% everywhere, then the computation producing it cannot be eliminated
	% or moved. If it is not needed at all, its producer can be eliminated.
	% If it is needed on some but not all branches, then the producer
	% can be moved to the starts of those branches.
	%
	% The set of branches to whose starts the producer can be moved
	% is represented as a map from the id of the branched control
	% structure to the set of branch numbers within that branched control
	% structure. If the branched control structure at goal path gp is
	% mapped to a set including N, then the producer of that variable
	% may be moved to the start of the goal with goal path <gp>;sN;
	% (if the control structure is a switch) or <gp>;t; or <gp>;e;
	% (if the control structure is an if-then-else). 
	%
	% Since <gp>;sN; is conjoined with e.g. <gp>;sN;<gp2>;sM;
	% it would be a mode error (variable having two conjoined producers)
	% for the transformed code to have the producer of some variable
	% inserted at the start of both those goals. It is therefore an
	% invariant that a where_needed structure mapping gp to N
	% will not contain any keys whose goal_path includes <gp>;sN;
	% or its if-then-else equivalent.
	%
	% An example:
	%
	%	% switch on X at goal path gp
	%	( % s1
	%		X = a,
	%		... code that needs Y and Z ...
	%	; % s2
	%		X = b,
	%		( Y = f ->
	%			... code that needs only Z ...
	%		;
	%			... code that does not need Y or Z ...
	%		)
	%	)
	%
	% X is needed everywhere, since even if X is bound to c, its value must
	% be tested.
	%
	% Y is needed everywhere iff the type of X contains only a and b,
	% otherwise it is needed only in the <gp>;s1; and <gp>;s2; switch arms.
	%
	% Z is needed in <gp>;s1; and <gp>;s2;t; but is not needed in the
	% <gp>;s2;e; else arm. Therefore the where_needed_branches map for Z
	% will map gp to 1 and <gp>;s2; to 1.

:- type where_needed_map 	==	map(prog_var, where_needed).

:- type where_needed
	--->	everywhere
	;	branches(where_needed_branches).

:- type where_needed_branches	==	map(branch_point, set(int)).

	% The refined_goal_map structure maps branch goals to the list of
	% producers that should be moved to the start of that branch.
	% The order is important, since some of the producers in such a list
	% may depend on variables produced by other goals that precede them
	% in the list.

:- type refined_goal_map == map(pair(goal_path, int), list(hlds_goal)).

%-----------------------------------------------------------------------------%

% The transformation considers every nonlocal variable of a goal
% that is bound on entry to be consumed by that goal. If the nonlocal set
% contains any such variables that are not actually needed by the goal,
% then the transformation will not be as effective as it could be.
% Therefore we preprocess the procedure body to ensure that the nonlocals
% sets are accurate reflections of the true needs of goals.

unneeded_code__process_proc_msg(PredId, ProcId, ProcInfo0, ProcInfo,
		ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Removing dead code in "),
		hlds_out__write_pred_proc_id(ModuleInfo0, PredId, ProcId),
		io__write_string(": "),
		{ unneeded_code__pre_process_proc(ProcInfo0, ProcInfo1) },
		{ unneeded_code__process_proc(ProcInfo1, ProcInfo,
			ModuleInfo0, ModuleInfo, Successful) },
		(
			{ Successful = yes },
			io__write_string("done.\n")
		;
			{ Successful = no },
			io__write_string("none found.\n")
		)
	;
		{ unneeded_code__pre_process_proc(ProcInfo0, ProcInfo1) },
		{ unneeded_code__process_proc(ProcInfo1, ProcInfo,
			ModuleInfo0, ModuleInfo, _) }
	).

:- pred unneeded_code__pre_process_proc(proc_info::in, proc_info::out) is det.

unneeded_code__pre_process_proc(ProcInfo0, ProcInfo) :-
	proc_info_headvars(ProcInfo0, HeadVars),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_varset(ProcInfo0, Varset0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	implicitly_quantify_clause_body(HeadVars, Goal0, Varset0, VarTypes0,
		Goal, Varset, VarTypes, _Warnings),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_varset(ProcInfo1, Varset, ProcInfo2),
	proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo).

% The source-to-source transform operates in two phases.
%
% The first phase traverses the procedure body, keeping track of which
% variables are needed where. When it finds a goal that can be deleted,
% it deletes it by replacing it with the goal `true' (i.e. conj([])).
% When it finds a goal that can be moved, it does the same, but also
% records in the RefinedGoalsMap that the deleted goal must later be
% inserted at the starts of the branches where its outputs may be needed,
% and accordingly notes that its own inputs are needed in those branches.
%
% The second phase traverses the modified problem body, and inserts the
% goals in the RefinedGoalsMap at the starts of the indicated branches.
% This phase identified the indicated branches by the goal_path annotations
% on their parents. These may be out of date since the first phase will have
% deleted some goals, but since neither phase modifies the goal_path annotation
% on a goal once that goal has been inserted into the RefinedGoalsMap,
% this does not matter.
%
% Neither phase traverses the internals of a goal that has been moved.
% To make sure that such goals are optimized whenever possible, the algorithm
% invokes itself recursively whenever it was able to successfully (delete or)
% move a goal. This cannot lead to infinite recursion, since each iteration
% will strictly reduce the number of computation paths on which a subgoal
% of the procedure body is executed. Since both the number of subgoals and
% computation paths are finite, the recursion must end.

:- type option_values
	--->	option_values(
			fully_strict	::	bool,
			reorder_conj	::	bool,	
			copy_limit	::	int
		).

:- pred unneeded_code__process_proc(proc_info::in, proc_info::out,
	module_info::in, module_info::out, bool::out) is det.

unneeded_code__process_proc(ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo,
		Successful) :-
	goal_path__fill_slots(ProcInfo0, ModuleInfo0, ProcInfo1),
	proc_info_goal(ProcInfo1, Goal0),
	proc_info_varset(ProcInfo1, Varset0),
	proc_info_vartypes(ProcInfo1, VarTypes0),
	proc_info_get_initial_instmap(ProcInfo1, ModuleInfo0, InstMap0),
	Goal0 = _ - GoalInfo0,
	goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	proc_info_instantiated_head_vars(ModuleInfo0, ProcInfo1,
		NeededVarsList),
	map__init(WhereNeededMap0),
	NeededEverywhere =
		lambda([Var::in, NeededMap0::in, NeededMap::out] is det, (
			map__det_insert(NeededMap0, Var, everywhere, NeededMap)
		)),
	list__foldl(NeededEverywhere, NeededVarsList,
		WhereNeededMap0, WhereNeededMap1),
	map__init(RefinedGoals0),
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, reorder_conj, ReorderConj),
	globals__lookup_bool_option(Globals, fully_strict, FullyStrict),
	globals__lookup_int_option(Globals, unneeded_code_copy_limit,
		Limit),
	Options = option_values(FullyStrict, ReorderConj, Limit),
	unneeded_code__process_goal(Goal0, Goal1, InstMap0, InstMap,
		VarTypes0, ModuleInfo0, Options, WhereNeededMap1, _,
		RefinedGoals0, RefinedGoals1, no, Changed),
	unneeded_code__refine_goal(Goal1, RefinedGoals1, Goal2, RefinedGoals),
	require(map__is_empty(RefinedGoals),
		"unneeded_code__process_proc: goal reattachment unsuccessful"),
	( Changed = yes ->
			% We need to fix up the goal_info by recalculating
			% the nonlocal vars and the non-atomic instmap deltas.
		proc_info_headvars(ProcInfo0, HeadVars),
		proc_info_inst_varset(ProcInfo0, InstVarSet),
		implicitly_quantify_clause_body(HeadVars,
			Goal2, Varset0, VarTypes0,
			Goal3, Varset, VarTypes, _Warnings),
		recompute_instmap_delta(no, Goal3, Goal, VarTypes, InstVarSet,
			InstMap0, ModuleInfo0, ModuleInfo1),
		proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
		proc_info_set_varset(ProcInfo2, Varset, ProcInfo3),
		proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo4),
		unneeded_code__process_proc(ProcInfo4, ProcInfo,
			ModuleInfo1, ModuleInfo, _),
		Successful = yes
	;
		ProcInfo = ProcInfo0,
		ModuleInfo = ModuleInfo0,
		Successful = no
	).

:- pred unneeded_code__process_goal(hlds_goal::in, hlds_goal::out, instmap::in,
	instmap::in, vartypes::in, module_info::in, option_values::in,
	where_needed_map::in, where_needed_map::out,
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_goal(Goal0, Goal, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	unneeded_code__can_eliminate_or_move(Goal0, InstMap0, InstMap,
		VarTypes, ModuleInfo, Options, WhereNeededMap0, WhereInfo),
	(
		WhereInfo = everywhere,
		unneeded_code__process_goal_internal(Goal0, Goal,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			WhereNeededMap0, WhereNeededMap1,
			RefinedGoals0, RefinedGoals, Changed0, Changed)
	;
		WhereInfo = branches(Branches),
		unneeded_code__demand_inputs(Goal0, ModuleInfo, InstMap0,
			WhereInfo, WhereNeededMap0, WhereNeededMap1),
		map__to_assoc_list(Branches, BranchList),
		list__foldl(unneeded_code__insert_branch_into_refined_goals(
			Goal0), BranchList, RefinedGoals0, RefinedGoals),
		true_goal(Goal),
		Changed = yes
	),
	unneeded_code__undemand_virgin_outputs(Goal0, ModuleInfo, InstMap0,
		WhereNeededMap1, WhereNeededMap2),
	(
		Goal = _ - GoalInfo,
		goal_info_get_features(GoalInfo, Features),
		set__member((impure), Features)
	->
			% By saying that all vars that are live before
			% the impure goal are needed everywhere, we prevent
			% the movement of the goals producing those vars
			% across the impure goal.
			%
			% This code requires compound goals containing impure
			% code to also be marked impure.
		map__map_values(unneeded_code__demand_var_everywhere,
			WhereNeededMap2, WhereNeededMap)
	;
		WhereNeededMap = WhereNeededMap2
	).

:- pred unneeded_code__insert_branch_into_refined_goals(hlds_goal::in,
	pair(branch_point, set(int))::in,
	refined_goal_map::in, refined_goal_map::out) is det.

unneeded_code__insert_branch_into_refined_goals(Goal,
		BranchPoint - BranchNumSet, RefinedGoals0, RefinedGoals) :-
	BranchPoint = branch_point(GoalPath, _),
	set__to_sorted_list(BranchNumSet, BranchNums),
	list__foldl(unneeded_code__insert_branch_arm_into_refined_goals(
		Goal, GoalPath), BranchNums, RefinedGoals0, RefinedGoals).

:- pred unneeded_code__insert_branch_arm_into_refined_goals(hlds_goal::in,
	goal_path::in, int::in,
	refined_goal_map::in, refined_goal_map::out) is det.

unneeded_code__insert_branch_arm_into_refined_goals(Goal, GoalPath, BranchNum,
		RefinedGoals0, RefinedGoals) :-
	Key = GoalPath - BranchNum,
	( map__search(RefinedGoals0, Key, Goals0) ->
		Goals = [Goal | Goals0],
		map__det_update(RefinedGoals0, Key, Goals, RefinedGoals)
	;
		map__det_insert(RefinedGoals0, Key, [Goal], RefinedGoals)
	).

%-----------------------------------------------------------------------------%

:- pred unneeded_code__can_eliminate_or_move(hlds_goal::in, instmap::in,
	instmap::in, vartypes::in, module_info::in, option_values::in,
	where_needed_map::in, where_needed::out) is det.

unneeded_code__can_eliminate_or_move(Goal, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, WhereNeededMap, WhereInfo) :-
	instmap_changed_vars(InstMap0, InstMap, VarTypes, ModuleInfo,
		ChangedVarSet),
	set__to_sorted_list(ChangedVarSet, ChangedVars),
	map__init(Empty),
	WhereInfo0 = branches(Empty),
	Goal = _ - GoalInfo,
	goal_info_get_goal_path(GoalInfo, CurrentPath),
	list__foldl(
		unneeded_code__collect_where_needed(
			CurrentPath, WhereNeededMap),
		ChangedVars, WhereInfo0, WhereInfo1),
	unneeded_code__adjust_where_needed(Goal, Options,
		WhereInfo1, WhereInfo).

:- pred unneeded_code__collect_where_needed(goal_path::in,
	where_needed_map::in, prog_var::in,
	where_needed::in, where_needed::out) is det.

unneeded_code__collect_where_needed(CurrentPath, WhereNeededMap, ChangedVar,
		WhereInfo0, WhereInfo) :-
	( map__search(WhereNeededMap, ChangedVar, Where) ->
		unneeded_code__where_needed_upper_bound(CurrentPath,
			Where, WhereInfo0, WhereInfo)
	;
		WhereInfo = WhereInfo0
	).

% This is the predicate responsible for ensuring that the act of optimizing
% away the execution of a goal on some or all computation paths changes the
% operational semantics only in ways that are explicitly permitted by the
% programmer.

:- pred unneeded_code__adjust_where_needed(hlds_goal::in, option_values::in,
	where_needed::in, where_needed::out) is det.

unneeded_code__adjust_where_needed(Goal, Options, WhereInfo0, WhereInfo) :-
	(
		Goal = GoalExpr - GoalInfo,
		(
				% Do not move goals that can fail, since
				% doing so can cause execution to reach goals
				% it shouldn't, and those goals may have
				% undesirable behavior (e.g. infinite loops).
			goal_info_get_determinism(GoalInfo, Detism),
			unneeded_code__detism_is_moveable(Detism, no)
		;
				% Do not move impure or semipure goals,
				% since their ordering wrt other such goals
				% must be preserved.
			goal_info_get_features(GoalInfo, Features),
			set__member((impure), Features)
		;
				% With --fully-strict, we cannot optimize away
				% infinite loops or exceptions.
			Options^fully_strict = yes,
			goal_can_loop_or_throw(Goal)
		;
				% With --no-reorder-conj, we cannot move
				% infinite loops or exceptions, but we can
				% delete them.
			Options^reorder_conj = no,
			goal_can_loop_or_throw(Goal),
			WhereInfo0 = branches(BranchMap),
			\+ map__is_empty(BranchMap)
		;
				% Do not delete the `true' goal, since
				% deleting it is a no-op, and thus does *not*
				% strictly reduce the number of computation
				% paths on which a subgoal of the procedure
				% body is executed.
			GoalExpr = conj([])
		;
			WhereInfo0 = branches(BranchMap),
			map__values(BranchMap, BranchArms),
			list__map(set__count, BranchArms, BranchArmCounts),
			BranchArmCount = list__foldl(int__plus,
				BranchArmCounts, 0),
			BranchArmCount > Options^copy_limit

			% We may also want to add ither space time tradeoffs.
			% E.g. if profiling shows that Goal is required in
			% 10 branches that account for 99% of all executions
			% and is not required in 5 branches that account for
			% the remaining 1%, and Goal itself is sufficiently
			% cheap to execute, then not moving Goal may cost
			% a small slowdown in 1% of cases but avoid 9 extra
			% copies of Goal. Due to better instruction cache
			% behavior, not moving Goal may in fact yield faster
			% code after all.
		)
	->
		WhereInfo = everywhere
	;
		WhereInfo = WhereInfo0
	).

:- pred unneeded_code__detism_is_moveable(determinism::in, bool::out) is det.

unneeded_code__detism_is_moveable(det, yes).
unneeded_code__detism_is_moveable(semidet, no).
unneeded_code__detism_is_moveable(nondet, no).
unneeded_code__detism_is_moveable(multidet, yes).
unneeded_code__detism_is_moveable(erroneous, no).
unneeded_code__detism_is_moveable(failure, no).
unneeded_code__detism_is_moveable(cc_nondet, no).
unneeded_code__detism_is_moveable(cc_multidet, yes).

%---------------------------------------------------------------------------%

:- pred unneeded_code__demand_inputs(hlds_goal::in, module_info::in,
	instmap::in, where_needed::in,
	where_needed_map::in, where_needed_map::out) is det.

unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0, WhereNeeded,
		WhereNeededMap0, WhereNeededMap) :-
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocalSet),
	goal_info_get_goal_path(GoalInfo, GoalPath),
	set__to_sorted_list(NonLocalSet, NonLocals),
	list__filter(unneeded_code__nonlocal_may_be_input(ModuleInfo, InstMap0),
		NonLocals, Inputs),
	list__foldl(unneeded_code__demand_var(GoalPath, WhereNeeded), Inputs,
		WhereNeededMap0, WhereNeededMap).

:- pred unneeded_code__nonlocal_may_be_input(module_info::in, instmap::in,
	prog_var::in) is semidet.

unneeded_code__nonlocal_may_be_input(ModuleInfo, InstMap0, Var) :-
	instmap__lookup_var(InstMap0, Var, Inst),
	inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred unneeded_code__undemand_virgin_outputs(hlds_goal::in, module_info::in,
	instmap::in, where_needed_map::in, where_needed_map::out) is det.

unneeded_code__undemand_virgin_outputs(Goal, ModuleInfo, InstMap0,
		WhereNeededMap0, WhereNeededMap) :-
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocalSet),
	set__to_sorted_list(NonLocalSet, NonLocals),
	list__filter(unneeded_code__nonlocal_is_virgin_output(
		ModuleInfo, InstMap0), NonLocals, VirginOutputs),
	list__foldl(unneeded_code__undemand_var, VirginOutputs,
		WhereNeededMap0, WhereNeededMap).

:- pred unneeded_code__nonlocal_is_virgin_output(module_info::in, instmap::in,
	prog_var::in) is semidet.

unneeded_code__nonlocal_is_virgin_output(ModuleInfo, InstMap0, Var) :-
	instmap__lookup_var(InstMap0, Var, Inst),
	\+ inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred unneeded_code__demand_var(goal_path::in, where_needed::in,
	prog_var::in, where_needed_map::in, where_needed_map::out) is det.

unneeded_code__demand_var(CurrentPath, WhereNeeded, Var,
		WhereNeededMap0, WhereNeededMap) :-
	( map__search(WhereNeededMap0, Var, Where0) ->
		unneeded_code__where_needed_upper_bound(CurrentPath,
			WhereNeeded, Where0, Where),
		map__det_update(WhereNeededMap0, Var, Where, WhereNeededMap)
	;
		map__det_insert(WhereNeededMap0, Var, WhereNeeded,
			WhereNeededMap)
	).

:- pred unneeded_code__undemand_var(prog_var::in,
	where_needed_map::in, where_needed_map::out) is det.

unneeded_code__undemand_var(Var, WhereNeededMap0, WhereNeededMap) :-
	map__delete(WhereNeededMap0, Var, WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred unneeded_code__demand_var_everywhere(prog_var::in, where_needed::in,
	where_needed::out) is det.

unneeded_code__demand_var_everywhere(_Var, _WhereNeeded0, everywhere).

%---------------------------------------------------------------------------%

:- pred unneeded_code__process_goal_internal(hlds_goal::in, hlds_goal::out,
	instmap::in, instmap::in, vartypes::in, module_info::in,
	option_values::in, where_needed_map::in, where_needed_map::out,
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_goal_internal(Goal0, Goal, InstMap0, InstMap,
		VarTypes, ModuleInfo, Options, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	% Goal = GoalExpr - GoalInfo,
	(
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = Goal0,
		unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0,
			everywhere, WhereNeededMap0, WhereNeededMap),
		RefinedGoals = RefinedGoals0,
		Changed = Changed0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		Goal = Goal0,
		unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0,
			everywhere, WhereNeededMap0, WhereNeededMap),
		RefinedGoals = RefinedGoals0,
		Changed = Changed0
	;
		GoalExpr0 = generic_call(_, _, _, _),
		Goal = Goal0,
		unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0,
			everywhere, WhereNeededMap0, WhereNeededMap),
		RefinedGoals = RefinedGoals0,
		Changed = Changed0
	;
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		Goal = Goal0,
		unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0,
			everywhere, WhereNeededMap0, WhereNeededMap),
		RefinedGoals = RefinedGoals0,
		Changed = Changed0
	;
		GoalExpr0 = par_conj(_, _),
		Goal = Goal0,
		unneeded_code__demand_inputs(Goal, ModuleInfo, InstMap0,
			everywhere, WhereNeededMap0, WhereNeededMap),
		RefinedGoals = RefinedGoals0,
		Changed = Changed0
	;
		GoalExpr0 = conj(Conjuncts0),
		unneeded_code__process_conj(Conjuncts0, Conjuncts,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			WhereNeededMap0, WhereNeededMap,
			RefinedGoals0, RefinedGoals, Changed0, Changed),
		GoalExpr = conj(Conjuncts),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = switch(SwitchVar, CanFail, Cases0, StoreMap),
		(
			Cases0 = [case(_, _ - FirstCaseGoalInfo) | _],
			goal_info_get_goal_path(FirstCaseGoalInfo,
				FirstCaseGoalPath),
			FirstCaseGoalPath = [SwitchStep | _],
			SwitchStep = switch(_, NumCases)
		->
			NumAlt = NumCases
		;
			error("unneeded_code__process_goal_internal: switch count")
		),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		BranchPoint = branch_point(GoalPath, switch(NumAlt)), 
		map__map_values(unneeded_code__demand_var_everywhere,
			WhereNeededMap0, WhereNeededMap1),
		map__init(BranchNeededMap0),
		unneeded_code__process_cases(Cases0, BranchPoint, 1,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			GoalPath, Cases, WhereNeededMap1,
			BranchNeededMap0, BranchNeededMap,
			RefinedGoals0, RefinedGoals, Changed0, Changed),
		unneeded_code__merge_where_needed_maps(GoalPath,
			WhereNeededMap1, BranchNeededMap, WhereNeededMap2),
		unneeded_code__demand_var(GoalPath, everywhere, SwitchVar,
			WhereNeededMap2, WhereNeededMap),
		GoalExpr = switch(SwitchVar, CanFail, Cases, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = disj(Disjuncts0, StoreMap),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		map__map_values(unneeded_code__demand_var_everywhere,
			WhereNeededMap0, WhereNeededMap1),
		unneeded_code__process_disj(Disjuncts0, InstMap0, InstMap,
			VarTypes, ModuleInfo, Options, GoalPath, Disjuncts,
			WhereNeededMap1, WhereNeededMap1, WhereNeededMap,
			RefinedGoals0, RefinedGoals, Changed0, Changed),
		GoalExpr = disj(Disjuncts, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0, StoreMap),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		BranchPoint = branch_point(GoalPath, ite), 
		map__map_values(unneeded_code__demand_var_everywhere,
			WhereNeededMap0, WhereNeededMap1),
		unneeded_code__process_ite(Cond0, Then0, Else0, BranchPoint,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			GoalPath, Cond, Then, Else, WhereNeededMap1,
			WhereNeededMap, RefinedGoals0, RefinedGoals, Changed0,
			Changed),
		GoalExpr = if_then_else(Quant, Cond, Then, Else, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		unneeded_code__process_goal(NegGoal0, NegGoal,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			WhereNeededMap0, WhereNeededMap,
			RefinedGoals0, RefinedGoals, Changed0, Changed),
		GoalExpr = not(NegGoal),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = some(Vars, CanRemove, SomeGoal0),
		unneeded_code__process_goal(SomeGoal0, SomeGoal,
			InstMap0, InstMap, VarTypes, ModuleInfo, Options,
			WhereNeededMap0, WhereNeededMap,
			RefinedGoals0, RefinedGoals, Changed0, Changed),
		GoalExpr = some(Vars, CanRemove, SomeGoal),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = shorthand(_),
		error("shorthand in unneeded_code__process_goal_internal")
	).

%---------------------------------------------------------------------------%

:- type bracketed_goal
	--->	bracketed_goal(hlds_goal, instmap, instmap).

:- pred unneeded_code__process_conj(list(hlds_goal)::in, list(hlds_goal)::out, 
	instmap::in, instmap::in, vartypes::in, module_info::in,
	option_values::in, where_needed_map::in, where_needed_map::out,
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_conj(Goals0, Goals, InstMap0, _InstMap, VarTypes,
		ModuleInfo, Options, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	unneeded_code__build_bracketed_conj(Goals0, InstMap0, BracketedGoals),
	list__reverse(BracketedGoals, RevBracketedGoals),
	unneeded_code__process_rev_bracketed_conj(RevBracketedGoals, RevGoals,
		VarTypes, ModuleInfo, Options, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed),
	list__reverse(RevGoals, Goals).

:- pred unneeded_code__build_bracketed_conj(list(hlds_goal)::in, instmap::in,
	list(bracketed_goal)::out) is det.

unneeded_code__build_bracketed_conj([], _, []).
unneeded_code__build_bracketed_conj([Goal | Goals], InstMap0, BracketedGoals)
		:-
	( instmap__is_unreachable(InstMap0) ->
		BracketedGoals = []
	;
		Goal = _ - GoalInfo,
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
		unneeded_code__build_bracketed_conj(Goals, InstMap1,
			BracketedTail),
		BracketedGoal = bracketed_goal(Goal, InstMap0, InstMap1),
		BracketedGoals = [BracketedGoal | BracketedTail]
	).

:- pred unneeded_code__process_rev_bracketed_conj(list(bracketed_goal)::in,
	list(hlds_goal)::out, vartypes::in, module_info::in, option_values::in,
	where_needed_map::in, where_needed_map::out,
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_rev_bracketed_conj([], [], _, _, _,
		WhereNeededMap, WhereNeededMap,
		RefinedGoals, RefinedGoals, Changed, Changed).
unneeded_code__process_rev_bracketed_conj([BracketedGoal | BracketedGoals],
		Goals, VarTypes, ModuleInfo, Options, WhereNeededMap0,
		WhereNeededMap, RefinedGoals0, RefinedGoals, Changed0,
		Changed) :-
	BracketedGoal = bracketed_goal(Goal0, InstMap0, InstMap),
	unneeded_code__process_goal(Goal0, Goal1, InstMap0, InstMap,
		VarTypes, ModuleInfo, Options, WhereNeededMap0, WhereNeededMap1,
		RefinedGoals0, RefinedGoals1, Changed0, Changed1),
	unneeded_code__process_rev_bracketed_conj(BracketedGoals, Goals1,
		VarTypes, ModuleInfo, Options, WhereNeededMap1, WhereNeededMap,
		RefinedGoals1, RefinedGoals, Changed1, Changed),
	( true_goal(Goal1) ->
		Goals = Goals1
	;
		Goals = [Goal1 | Goals1]
	).

%---------------------------------------------------------------------------%

:- pred unneeded_code__process_disj(list(hlds_goal)::in, instmap::in,
	instmap::in, vartypes::in, module_info::in, option_values::in,
	goal_path::in, list(hlds_goal)::out,
	where_needed_map::in, where_needed_map::in, where_needed_map::out, 
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_disj([], _, _, _, _, _, _, [],
		_, WhereNeededMap, WhereNeededMap,
		RefinedGoals, RefinedGoals, Changed, Changed).
unneeded_code__process_disj([Goal0 | Goals0], InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, CurrentPath, [Goal | Goals],
		StartWhereNeededMap, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	unneeded_code__process_goal(Goal0, Goal, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, StartWhereNeededMap, WhereNeededMapFirst,
		RefinedGoals0, RefinedGoals1, Changed0, Changed1),
	map__to_assoc_list(WhereNeededMapFirst, WhereNeededList),
	unneeded_code__add_where_needed_list(WhereNeededList, CurrentPath,
		WhereNeededMap0, WhereNeededMap1),
	unneeded_code__process_disj(Goals0, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, CurrentPath, Goals,
		StartWhereNeededMap, WhereNeededMap1, WhereNeededMap,
		RefinedGoals1, RefinedGoals, Changed1, Changed).

%---------------------------------------------------------------------------%

:- pred unneeded_code__process_cases(list(case)::in, branch_point::in, int::in,
	instmap::in, instmap::in, vartypes::in, module_info::in,
	option_values::in, goal_path::in, list(case)::out, where_needed_map::in,
	where_needed_map::in, where_needed_map::out, 
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_cases([], _, _, _, _, _, _, _, _, [],
		_, WhereNeededMap, WhereNeededMap,
		RefinedGoals, RefinedGoals, Changed, Changed).
unneeded_code__process_cases([case(Var, Goal0) | Cases0],
		BranchPoint, BranchNum, InstMap0, InstMap,
		VarTypes, ModuleInfo, Options, CurrentPath,
		[case(Var, Goal) | Cases], StartWhereNeededMap,
		WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	unneeded_code__process_goal(Goal0, Goal, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, StartWhereNeededMap, WhereNeededMapFirst,
		RefinedGoals0, RefinedGoals1, Changed0, Changed1),
	map__to_assoc_list(WhereNeededMapFirst, WhereNeededList),
	unneeded_code__add_alt_start(WhereNeededList, BranchPoint, BranchNum,
		CurrentPath, WhereNeededMap0, WhereNeededMap1),
	unneeded_code__process_cases(Cases0, BranchPoint, BranchNum + 1,
		InstMap0, InstMap, VarTypes, ModuleInfo, Options, CurrentPath,
		Cases, StartWhereNeededMap, WhereNeededMap1, WhereNeededMap,
		RefinedGoals1, RefinedGoals, Changed1, Changed).

%---------------------------------------------------------------------------%

:- pred unneeded_code__process_ite(hlds_goal::in, hlds_goal::in, hlds_goal::in,
	branch_point::in, instmap::in, instmap::in, vartypes::in,
	module_info::in, option_values::in, goal_path::in,
	hlds_goal::out, hlds_goal::out, hlds_goal::out,
	where_needed_map::in, where_needed_map::out, 
	refined_goal_map::in, refined_goal_map::out,
	bool::in, bool::out) is det.

unneeded_code__process_ite(Cond0, Then0, Else0, BranchPoint,
		InstMap0, InstMap, VarTypes, ModuleInfo, Options, CurrentPath,
		Cond, Then, Else, WhereNeededMap0, WhereNeededMap,
		RefinedGoals0, RefinedGoals, Changed0, Changed) :-
	Cond0 = _ - CondInfo0,
	goal_info_get_instmap_delta(CondInfo0, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMapCond),

	unneeded_code__process_goal(Else0, Else, InstMap0, InstMap, VarTypes,
		ModuleInfo, Options, WhereNeededMap0, WhereNeededMapElse,
		RefinedGoals0, RefinedGoals1, Changed0, Changed1),
	unneeded_code__process_goal(Then0, Then, InstMapCond, InstMap,
		VarTypes, ModuleInfo, Options, WhereNeededMap0,
		WhereNeededMapThen, RefinedGoals1, RefinedGoals2, Changed1,
		Changed2),

	map__init(BranchNeededMap0),
	map__to_assoc_list(WhereNeededMapElse, WhereNeededListElse),
	unneeded_code__add_alt_start(WhereNeededListElse, BranchPoint, 2,
		CurrentPath, BranchNeededMap0, BranchNeededMap1),
	map__to_assoc_list(WhereNeededMapThen, WhereNeededListThen),
	unneeded_code__add_alt_start(WhereNeededListThen, BranchPoint, 1,
		CurrentPath, BranchNeededMap1, BranchNeededMap),
	unneeded_code__merge_where_needed_maps(CurrentPath,
		WhereNeededMap0, BranchNeededMap, WhereNeededMapCond),

	unneeded_code__process_goal(Cond0, Cond, InstMap0, InstMapCond,
		VarTypes, ModuleInfo, Options, WhereNeededMapCond,
		WhereNeededMap, RefinedGoals2, RefinedGoals, Changed2, Changed).

%---------------------------------------------------------------------------%

% Merge two where_needed_maps, so that if var V is needed at branch B
% in the resulting where_needed_map iff it is needed there in one of the input
% maps.

:- pred unneeded_code__merge_where_needed_maps(goal_path::in,
	where_needed_map::in, where_needed_map::in, where_needed_map::out)
	is det.

unneeded_code__merge_where_needed_maps(CurrentPath,
		WhereNeededMap1, WhereNeededMap2, WhereNeededMap) :-
	map__to_assoc_list(WhereNeededMap1, WhereNeededList1),
	unneeded_code__add_where_needed_list(WhereNeededList1, CurrentPath,
		WhereNeededMap2, WhereNeededMap).

:- pred unneeded_code__add_where_needed_list(
	assoc_list(prog_var, where_needed)::in, goal_path::in,
	where_needed_map::in, where_needed_map::out) is det.

unneeded_code__add_where_needed_list([], _, WhereNeededMap, WhereNeededMap).
unneeded_code__add_where_needed_list([Var - BranchWhere | WhereNeededList],
		CurrentPath, WhereNeededMap0, WhereNeededMap) :-
	( map__search(WhereNeededMap0, Var, OldWhere) ->
		unneeded_code__where_needed_upper_bound(CurrentPath,
			BranchWhere, OldWhere, CombinedWhere),
		map__det_update(WhereNeededMap0, Var, CombinedWhere,
			WhereNeededMap1)
	;
		map__det_insert(WhereNeededMap0, Var, BranchWhere,
			WhereNeededMap1)
	),
	unneeded_code__add_where_needed_list(WhereNeededList, CurrentPath,
		WhereNeededMap1, WhereNeededMap).

% Given a where_needed_map, add to it the where_needed information for the
% start of an alternative in a branched goal. This source is important,
% because if the analysis *at the start of an alternative* says that the
% variable is needed everywhere, the scope of this "everywhere" is only
% that alternative.

:- pred unneeded_code__add_alt_start(assoc_list(prog_var, where_needed)::in,
	branch_point::in, int::in, goal_path::in,
	where_needed_map::in, where_needed_map::out) is det.

unneeded_code__add_alt_start([], _, _, _, WhereNeededMap, WhereNeededMap).
unneeded_code__add_alt_start([Var - BranchWhere0 | WhereNeededList],
		BranchPoint, BranchNum, CurrentPath,
		WhereNeededMap0, WhereNeededMap) :-
	(
		BranchWhere0 = everywhere,
		map__init(Empty),
		set__singleton_set(BranchNumSet, BranchNum),
		map__det_insert(Empty, BranchPoint, BranchNumSet, BranchMap),
		BranchWhere = branches(BranchMap)
	;
		BranchWhere0 = branches(_),
		BranchWhere = BranchWhere0
	),
	( map__search(WhereNeededMap0, Var, OldWhere) ->
		unneeded_code__where_needed_upper_bound(CurrentPath,
			BranchWhere, OldWhere, CombinedWhere),
		map__det_update(WhereNeededMap0, Var, CombinedWhere,
			WhereNeededMap1)
	;
		map__det_insert(WhereNeededMap0, Var, BranchWhere,
			WhereNeededMap1)
	),
	unneeded_code__add_alt_start(WhereNeededList, BranchPoint, BranchNum,
		CurrentPath, WhereNeededMap1, WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred unneeded_code__refine_goal(hlds_goal::in, refined_goal_map::in,
	hlds_goal::out, refined_goal_map::out) is det.

unneeded_code__refine_goal(Goal0, RefinedGoals0, Goal, RefinedGoals) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	(
		GoalExpr0 = unify(_, _, _, _, _),
		Goal = Goal0,
		RefinedGoals = RefinedGoals0
	;
		GoalExpr0 = call(_, _, _, _, _, _),
		Goal = Goal0,
		RefinedGoals = RefinedGoals0
	;
		GoalExpr0 = generic_call(_, _, _, _),
		Goal = Goal0,
		RefinedGoals = RefinedGoals0
	;
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		Goal = Goal0,
		RefinedGoals = RefinedGoals0
	;
		GoalExpr0 = par_conj(_, _),
		Goal = Goal0,
		RefinedGoals = RefinedGoals0
	;
		GoalExpr0 = conj(Conjuncts0),
		unneeded_code__refine_conj(Conjuncts0, RefinedGoals0,
			Conjuncts, RefinedGoals),
		GoalExpr = conj(Conjuncts),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = switch(SwitchVar, CanFail, Cases0, StoreMap),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		unneeded_code__refine_cases(Cases0, RefinedGoals0,
			GoalPath, 1, Cases, RefinedGoals),
		GoalExpr = switch(SwitchVar, CanFail, Cases, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = disj(Disjuncts0, StoreMap),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		unneeded_code__refine_disj(Disjuncts0, RefinedGoals0,
			GoalPath, 1, Disjuncts, RefinedGoals),
		GoalExpr = disj(Disjuncts, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0, StoreMap),
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		unneeded_code__refine_ite(Cond0, Then0, Else0, RefinedGoals0,
			GoalPath, Cond, Then, Else, RefinedGoals),
		GoalExpr = if_then_else(Quant, Cond, Then, Else, StoreMap),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = not(NegGoal0),
		unneeded_code__refine_goal(NegGoal0, RefinedGoals0,
			NegGoal, RefinedGoals),
		GoalExpr = not(NegGoal),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = some(Vars, CanFail, SomeGoal0),
		unneeded_code__refine_goal(SomeGoal0, RefinedGoals0,
			SomeGoal, RefinedGoals),
		GoalExpr = some(Vars, CanFail, SomeGoal),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = shorthand(_),
		error("shorthand in unneeded_code__refine_goal")
	).

:- pred unneeded_code__refine_conj(list(hlds_goal)::in, refined_goal_map::in,
	list(hlds_goal)::out, refined_goal_map::out) is det.

unneeded_code__refine_conj([], RefinedGoals, [], RefinedGoals).
unneeded_code__refine_conj([Goal0 | Goals0], RefinedGoals0, Goals,
		RefinedGoals) :-
	unneeded_code__refine_goal(Goal0, RefinedGoals0, HeadGoal,
		RefinedGoals1),
	unneeded_code__refine_conj(Goals0, RefinedGoals1, TailGoals,
		RefinedGoals),
	( HeadGoal = conj(HeadGoals) - _ ->
		list__append(HeadGoals, TailGoals, Goals)
	;
		Goals = [HeadGoal | TailGoals]
	).

:- pred unneeded_code__refine_cases(list(case)::in, refined_goal_map::in,
	goal_path::in, int::in, list(case)::out, refined_goal_map::out) is det.

unneeded_code__refine_cases([], RefinedGoals, _, _, [], RefinedGoals).
unneeded_code__refine_cases([case(Var, Goal0) | Cases0], RefinedGoals0,
		GoalPath, BranchNum,
		[case(Var, Goal) | Cases], RefinedGoals) :-
	unneeded_code__refine_goal(Goal0, RefinedGoals0, Goal1, RefinedGoals1),
	( map__search(RefinedGoals1, GoalPath - BranchNum, ToInsertGoals) ->
		unneeded_code__insert_refine_goals(ToInsertGoals, Goal1, Goal),
		map__delete(RefinedGoals1, GoalPath - BranchNum, RefinedGoals2)
	;
		Goal = Goal1,
		RefinedGoals2 = RefinedGoals1
	),
	unneeded_code__refine_cases(Cases0, RefinedGoals2,
		GoalPath, BranchNum + 1, Cases, RefinedGoals).

:- pred unneeded_code__refine_disj(list(hlds_goal)::in, refined_goal_map::in,
	goal_path::in, int::in, list(hlds_goal)::out, refined_goal_map::out)
	is det.

unneeded_code__refine_disj([], RefinedGoals, _, _, [], RefinedGoals).
unneeded_code__refine_disj([Goal0 | Goals0], RefinedGoals0,
		GoalPath, BranchNum, [Goal | Goals], RefinedGoals) :-
	unneeded_code__refine_goal(Goal0, RefinedGoals0, Goal1, RefinedGoals1),
	( map__search(RefinedGoals1, GoalPath - BranchNum, ToInsertGoals) ->
		unneeded_code__insert_refine_goals(ToInsertGoals, Goal1, Goal),
		map__delete(RefinedGoals1, GoalPath - BranchNum, RefinedGoals2)
	;
		Goal = Goal1,
		RefinedGoals2 = RefinedGoals1
	),
	unneeded_code__refine_disj(Goals0, RefinedGoals2,
		GoalPath, BranchNum + 1, Goals, RefinedGoals).

:- pred unneeded_code__refine_ite(hlds_goal::in, hlds_goal::in, hlds_goal::in,
	refined_goal_map::in, goal_path::in,
	hlds_goal::out, hlds_goal::out, hlds_goal::out, refined_goal_map::out)
	is det.

unneeded_code__refine_ite(Cond0, Then0, Else0, RefinedGoals0, GoalPath,
		Cond, Then, Else, RefinedGoals) :-
	unneeded_code__refine_goal(Cond0, RefinedGoals0, Cond, RefinedGoals1),
	unneeded_code__refine_goal(Then0, RefinedGoals1, Then1, RefinedGoals2),
	unneeded_code__refine_goal(Else0, RefinedGoals2, Else1, RefinedGoals3),

	( map__search(RefinedGoals3, GoalPath - 1, ToInsertGoalsThen) ->
		unneeded_code__insert_refine_goals(ToInsertGoalsThen, Then1,
			Then),
		map__delete(RefinedGoals3, GoalPath - 1, RefinedGoals4)
	;
		Then = Then1,
		RefinedGoals4 = RefinedGoals3
	),
	( map__search(RefinedGoals4, GoalPath - 2, ToInsertGoalsElse) ->
		unneeded_code__insert_refine_goals(ToInsertGoalsElse, Else1,
			Else),
		map__delete(RefinedGoals4, GoalPath - 2, RefinedGoals)
	;
		Else = Else1,
		RefinedGoals = RefinedGoals4
	).

:- pred unneeded_code__insert_refine_goals(list(hlds_goal)::in, hlds_goal::in,
	hlds_goal::out) is det.

unneeded_code__insert_refine_goals(ToInsertGoals, Goal0, Goal) :-
	list__append(ToInsertGoals, [Goal0], Conj),
	% XXX GoalInfo0
	Goal0 = _ - GoalInfo0,
	conj_list_to_goal(Conj, GoalInfo0, Goal).

%-----------------------------------------------------------------------------%

% Given two sets of requirements about where a goal is needed, return a single
% requirement that contains all the demands. The main purpose of this predicate
% is to discover when the union of two sets of requirements (e.g. branch sets
% {b1,b2} and {b3} covers all computation paths.

:- pred unneeded_code__where_needed_upper_bound(goal_path::in,
	where_needed::in, where_needed::in, where_needed::out) is det.

unneeded_code__where_needed_upper_bound(CurrentPath,
		WhereNeededA, WhereNeededB, WhereNeeded) :-
	(
		WhereNeededA = everywhere,
		WhereNeeded = everywhere
	;
		WhereNeededA = branches(BranchesA),
		(
			WhereNeededB = everywhere,
			WhereNeeded = everywhere
		;
			WhereNeededB = branches(BranchesB),
			unneeded_code__where_needed_branches_upper_bound(
				CurrentPath, BranchesA, BranchesB, WhereNeeded)
		)
	).

:- pred unneeded_code__where_needed_branches_upper_bound(goal_path::in,
	where_needed_branches::in, where_needed_branches::in,
	where_needed::out) is det.

unneeded_code__where_needed_branches_upper_bound(CurrentPath,
		BranchesA, BranchesB, WhereNeeded) :-
	% should select smaller map to convert to list
	map__to_assoc_list(BranchesA, BranchesList),
	unneeded_code__where_needed_branches_upper_bound_2(CurrentPath,
		BranchesList, BranchesB, WhereNeeded).

:- pred unneeded_code__where_needed_branches_upper_bound_2(goal_path::in,
	assoc_list(branch_point, set(int))::in, where_needed_branches::in,
	where_needed::out) is det.

unneeded_code__where_needed_branches_upper_bound_2(_, [],
		Branches, branches(Branches)).
unneeded_code__where_needed_branches_upper_bound_2(CurrentPath, [First | Rest],
		Branches0, WhereNeeded) :-
	First = BranchPoint - NewAlts,
	( map__search(Branches0, BranchPoint, OldAlts) ->
		set__union(OldAlts, NewAlts, Alts),
		BranchPoint = branch_point(GoalPath, BranchAlts),
		( unneeded_code__branch_point_is_complete(BranchAlts, Alts) ->
			(
				unneeded_code__get_parent_branch_point(GoalPath,
					ParentGoalPath, ParentGoalPathStep,
					ParentBranchAlt, ParentBranchNum),
				\+ list__remove_suffix(CurrentPath,
					[ParentGoalPathStep | ParentGoalPath],
					_)
			->
				map__delete(Branches0, BranchPoint, Branches1),
				ParentBranchPoint = branch_point(
					ParentGoalPath, ParentBranchAlt),
				set__singleton_set(ParentAlts,
					ParentBranchNum),
				unneeded_code__where_needed_branches_upper_bound_2(
					CurrentPath,
					[ParentBranchPoint - ParentAlts
						| Rest],
					Branches1, WhereNeeded)
			;
				WhereNeeded = everywhere
			)
		;
			map__det_update(Branches0, BranchPoint, Alts,
				Branches1),
			unneeded_code__where_needed_branches_upper_bound_2(
				CurrentPath, Rest, Branches1, WhereNeeded)
		)
	;
		map__det_insert(Branches0, BranchPoint, NewAlts, Branches1),
		unneeded_code__where_needed_branches_upper_bound_2(CurrentPath,
			Rest, Branches1, WhereNeeded)
	).
	
:- pred unneeded_code__get_parent_branch_point(goal_path::in, goal_path::out,
	goal_path_step::out, branch_alts::out, int::out) is semidet.

unneeded_code__get_parent_branch_point([First | Rest], Parent, ParentStep,
		BranchAlt, BranchNum) :-
	( First = switch(Arm, NumAlts) ->
		Parent = Rest,
		ParentStep = First,
		BranchAlt = switch(NumAlts),
		BranchNum = Arm
	; First = ite_then ->
		Parent = Rest,
		ParentStep = First,
		BranchAlt = ite,
		BranchNum = 1
	; First = ite_else ->
		Parent = Rest,
		ParentStep = First,
		BranchAlt = ite,
		BranchNum = 2
	;
		unneeded_code__get_parent_branch_point(Rest, Parent, ParentStep,
			BranchAlt, BranchNum)
	).

:- pred unneeded_code__branch_point_is_complete(branch_alts::in, set(int)::in)
	is semidet.

unneeded_code__branch_point_is_complete(ite, Alts) :-
	set__count(Alts, NumAlts),
	NumAlts = 2.
unneeded_code__branch_point_is_complete(switch(NumFunctors), Alts) :-
	set__count(Alts, NumAlts),
	NumAlts = NumFunctors.

%---------------------------------------------------------------------------%
