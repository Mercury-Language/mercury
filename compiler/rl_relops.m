%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_relops
% Main author: stayl
%
% Generate intermediate code for relational operations.
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_relops.

:- interface.

:- import_module aditi_backend.rl.
:- import_module aditi_backend.rl_info.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module std_util.

/*
	% Perform projections and selections before joins. In some cases
	% this may worsen performance (reducing tuple size by removing
	% a single integer from each tuple is not likely to improve the
	% perforance of a join by much, but requires an extra pass over
	% the data). There's not really any way to decide at compile time
	% what the performance difference will be, so we do it anyway.
	%
	% XXX this is a bad idea, since the relation being projected
	% may have an index built for it, which will have to be rebuilt
	% after the projection. This can be done later as an optimization.
:- pred rl_relops__pre_select_and_project(relation_id::in, relation_id::out,
	list(prog_var)::in, list(prog_var)::in, instmap::in,
	list(hlds_goal)::in, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.
*/

	% Compute a projection for a single call rule. If the
	% projection condition is semidet, generate a select first.
:- pred rl_relops__select_and_project(relation_id::in, relation_id::out,
	list(prog_var)::in, list(prog_var)::in, instmap::in,
	list(hlds_goal)::in, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% rl_relops__join(InputRel1, InputRel2, InputArgs1, InputArgs2,
	% 	InstMap, JoinCondGoals, OutputArgs, OutputSchema, OutputRel,
	% 	Code).
:- pred rl_relops__join(relation_id::in, relation_id::in, list(prog_var)::in,
	list(prog_var)::in, instmap::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, relation_id::out,
	rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% rl_relops__join(FullRel1, DiffRel1, FullRel2, DiffRel2,
	%	InputArgs1, InputArgs2, InstMap, JoinCondGoals,
	%	OutputArgs, OutputSchema, OutputRel, Code).
	%
	% Generate (DiffRel1 |X| FullRel2) U (FullRel1 |X| DiffRel2) for a
	% rule with two recursive calls.
:- pred rl_relops__diff_diff_join(relation_id::in, relation_id::in,
	relation_id::in, relation_id::in, list(prog_var)::in,
	list(prog_var)::in, instmap::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, relation_id::out,
	rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% rl_relops__subtract(InputRel1, InputRel2, InputArgs1, InputArgs2,
	%	InstMap, SubtractCond, ProjectCond, OutputArgs, OutputSchema,
	%	OutputRelation, Code).
:- pred rl_relops__subtract(relation_id::in, relation_id::in,
	list(prog_var)::in, list(prog_var)::in, instmap::in,
	list(hlds_goal)::in, list(hlds_goal)::in, list(prog_var)::in,
	relation_schema::in, relation_id::out, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% rl_relops__difference(InputRel1, InputRel2, OutputRel, Code).
	%
	% A difference is just a special case of subtract
	% (InputRel1 - InputRel2 = OutputRel).
	% The inputs to the difference must be sorted.
:- pred rl_relops__difference(relation_id::in, relation_id::in,
	relation_id::in, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% rl_relops__union(MustSortOutput, Schema, InputRels, MaybeOutputRel,
	% 	OutputRel, Code).
:- pred rl_relops__union(bool::in, relation_schema::in, list(relation_id)::in,
	maybe(relation_id)::in, relation_id::out, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% Generate code to sort a relation on all its arguments.
:- pred rl_relops__sort(relation_id::in, relation_id::out,
	rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

	% Package up a list of top-down goals to execute as a condition
	% or projection for a relational operation.
:- pred rl_relops__goal(instmap::in, rl_goal_inputs::in, rl_goal_outputs::in,
	list(hlds_goal)::in, rl_goal::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

:- implementation.

:- import_module aditi_backend.rl_key.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*
rl_relops__pre_select_and_project(Relation, ProjectRel, OutputArgs,
		NonLocalOutputArgs, InstMap0, CondGoals, Code) -->
	rl_info_write_message("Generating pre-project\n", []),
	{ set__list_to_set(NonLocalOutputArgs, NonLocalVars) },
	rl_relops__get_dependent_goals(NonLocalVars, InstMap0,
		CondGoals, ThisCallGoals),

	rl_relops__maybe_select(Relation, SelectRel, OutputArgs,
		InstMap0, ThisCallGoals, SelectCode),

	( { OutputArgs = NonLocalOutputArgs } ->
		{ ProjectRel = SelectRel },
		{ ProjectCode = empty } ,
		rl_info_write_message("No project\n", [])
	;
		rl_relops__project(SelectRel, ProjectRel, OutputArgs,
			NonLocalOutputArgs, InstMap0, [], ProjectCode),
		rl_info_write_message("Generated project\n", [])
	),
	{ Code = tree(SelectCode, ProjectCode) }.
*/

	% Get the goals from a list of goals that depend only on the
	% given variables. This is used to select out the goals that
	% can be evaluated with only one of the calls to be joined.
:- pred rl_relops__get_dependent_goals(set(prog_var)::in, instmap::in,
	list(hlds_goal)::in, list(hlds_goal)::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_relops__get_dependent_goals(_, _, [], []) --> [].
rl_relops__get_dependent_goals(Vars, InstMap0,
		[Goal | Goals], DependentGoals) -->

	% Get the input vars for this goal.
	{ Goal = _ - GoalInfo },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ set__to_sorted_list(NonLocals, NonLocalsList) },
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	{ instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap) },
	rl_info_get_module_info(ModuleInfo),
	{ IsInput =
		(pred(Var::in) is semidet :-
			instmap__lookup_var(InstMap0, Var, Inst0),
			instmap__lookup_var(InstMap, Var, Inst),
			mode_is_input(ModuleInfo, (Inst0 -> Inst))
		) },
	{ list__filter(IsInput, NonLocalsList, InputNonLocalsList) },
	{ set__sorted_list_to_set(InputNonLocalsList, InputNonLocals) },
	{ set__difference(InputNonLocals, Vars, OtherCallInputs) },
	( { set__empty(OtherCallInputs) } ->
		{ set__union(Vars, NonLocals, Vars1) },
		rl_relops__get_dependent_goals(Vars1, InstMap,
			Goals, DependentGoals1),
		{ DependentGoals = [Goal | DependentGoals1] }
	;
		rl_relops__get_dependent_goals(Vars, InstMap,
			Goals, DependentGoals)
	).

%-----------------------------------------------------------------------------%

rl_relops__select_and_project(InputRel, OutputRel, InputArgs,
		OutputArgs, InstMap0, CondGoals, Code) -->
	rl_info_write_message("Generating single call select+project\n", []),

	{ goal_list_determinism(CondGoals, Detism) },
	{ determinism_components(Detism, CanFail, _) },
	(
		{ InputArgs = OutputArgs ->
			% If the outputs are the same as the inputs, we
			% only need to do the operation if it can fail.
			CanFail = can_fail,
			MaybeOutputs = no
		;
			MaybeOutputs = yes(OutputArgs)
		}
	->
		rl_relops__goal(InstMap0, one_input(InputArgs),
			MaybeOutputs, CondGoals, Cond),
		( { MaybeOutputs = yes(_) } ->
			rl_info_get_proc_info(ProcInfo),
			{ proc_info_vartypes(ProcInfo, VarTypes) },
			{ map__apply_to_list(OutputArgs, VarTypes,
				OutputTypes) },
			{ OutputSchema = schema(OutputTypes) }
		;
			{ OutputSchema = same_as_relation(InputRel) }
		),
		rl_info_get_new_temporary(OutputSchema, OutputRel),
		rl_info__comment(Comment),
		{ ProjectInstr = project(output_rel(OutputRel, []), InputRel,
			Cond, [], filter) - Comment },
		{ Code = node([ProjectInstr]) }
	;
		{ OutputRel = InputRel },
		{ Code = empty }
	).

%-----------------------------------------------------------------------------%

rl_relops__join(InputRel1, InputRel2, Args1, Args2, InstMap,
		JoinCondGoals, OutputVars, OutputSchema, OutputRel, Code) -->
	rl_relops__join_2(InputRel1, InputRel2, Args1, Args2, InstMap,
		JoinCondGoals, OutputVars, OutputSchema, OutputRel,
		_JoinType, _JoinOutputs, Code).

:- pred rl_relops__join_2(relation_id::in, relation_id::in, list(prog_var)::in,
	list(prog_var)::in, instmap::in, list(hlds_goal)::in,
	list(prog_var)::in, relation_schema::in, relation_id::out,
	join_type::out, list(prog_var)::out, rl_tree::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_relops__join_2(InputRel1, InputRel2, Args1, Args2, InstMap,
		JoinCondGoals, OutputVars, OutputSchema, OutputRel,
		JoinType, JoinOutputs, Code) -->
	rl_relops__classify_join_condition(Args1, Args2, OutputVars,
		JoinCondGoals, JoinType, JoinOutputs, ReverseInputs),
	{ rl_relops__maybe_reverse_inputs(ReverseInputs,
		InputRel1, InputRel2, ReorderedInput1, ReorderedInput2) },
	{ rl_relops__maybe_reverse_inputs(ReverseInputs,
		Args1, Args2, ReorderedArgs1, ReorderedArgs2) },

	rl_relops__goal(InstMap, two_inputs(ReorderedArgs1, ReorderedArgs2),
		yes(OutputVars), JoinCondGoals, JoinCond),
	rl_info__comment(Comment),

	rl_info_get_new_temporary(OutputSchema, OutputRel),
	{ rl__is_semi_join(JoinType, JoinCond, SemiJoin) },

	rl_info_get_module_info(ModuleInfo),
	{ rl__is_trivial_join(ModuleInfo, JoinType, JoinCond,
		SemiJoin, TrivialJoin) },

	{ Code = node([join(output_rel(OutputRel, []), ReorderedInput1,
		ReorderedInput2, JoinType, JoinCond,
		SemiJoin, TrivialJoin) - Comment]) }.

rl_relops__diff_diff_join(DiffRel1, FullRel1, DiffRel2, FullRel2,
		Args1, Args2, InstMap, JoinCondGoals, RuleOutputs,
		RuleSchema, RuleResult, JoinCode) -->
	rl_relops__join_2(DiffRel1, FullRel2, Args1, Args2, InstMap,
		JoinCondGoals, RuleOutputs, RuleSchema, OutputRel1,
		JoinType, JoinOutputs, JoinCode1),
	rl_relops__join_2(FullRel1, DiffRel2, Args1, Args2, InstMap,
		JoinCondGoals, RuleOutputs, RuleSchema, OutputRel2,
		JoinType2, JoinOutputs2, JoinCode2),

	% We should get the same join type back from both joins.
	{ JoinType = JoinType2, JoinOutputs = JoinOutputs2 ->
		true
	;
		error("rl_relops__diff_diff_join: different join types")
	},

	rl_relops__union(yes, RuleSchema, [OutputRel1, OutputRel2],
		no, RuleResult, UnionCode),

	{ JoinCode =
		tree(JoinCode1,
		tree(JoinCode2,
		UnionCode
	)) }.

	% All joins start out as nested loop joins, and are specialized later.
:- pred rl_relops__classify_join_condition(list(prog_var)::in,
	list(prog_var)::in, list(prog_var)::in, list(hlds_goal)::in,
	join_type::out, list(prog_var)::out, bool::out,
	rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_relops__classify_join_condition(_, _, Outputs, _,
		Type, JoinOutputs, ReverseInputRels) -->
	{ Type = nested_loop },
	{ ReverseInputRels = no },
	{ JoinOutputs = Outputs }.

:- pred rl_relops__maybe_reverse_inputs(bool::in,
		T::in, T::in, T::out, T::out) is det.

rl_relops__maybe_reverse_inputs(yes, A, B, B, A).
rl_relops__maybe_reverse_inputs(no, A, B, A, B).

%-----------------------------------------------------------------------------%

rl_relops__subtract(Rel1, Rel2, OutputArgs1, OutputArgs2, InstMap,
		SubtractGoals, ProjectGoals, OutputVars, _Schema,
		Result, SubtractCode) -->
	rl_relops__classify_subtract_condition(SubtractGoals, SubtractType),
	rl_relops__goal(InstMap, two_inputs(OutputArgs1, OutputArgs2), no,
		SubtractGoals, SubtractCond),
	rl_info_get_new_temporary(same_as_relation(Rel1), TempRel),
	rl_info_get_module_info(ModuleInfo),

	{ rl__is_trivial_subtract(ModuleInfo, SubtractType, SubtractCond,
		TrivialSubtract) },

	{ Subtract = subtract(output_rel(TempRel, []),
		Rel1, Rel2, SubtractType, SubtractCond,
		TrivialSubtract) - Comment },
	rl_info__comment(Comment),

	% The output projection for subtracts must be done as a separate
	% operation, unlike for nested loop joins.
	rl_relops__select_and_project(TempRel, Result, OutputArgs1,
		OutputVars, InstMap, ProjectGoals, ProjectCode),
	{ SubtractCode =
		tree(node([Subtract]),
		ProjectCode
	) }.

	% All subtracts start out as semi_nested_loop subtracts,
	% and are specialized later.
:- pred rl_relops__classify_subtract_condition(list(hlds_goal)::in,
	subtract_type::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_relops__classify_subtract_condition(_, semi_nested_loop) --> [].

rl_relops__difference(OldRel, NewRel, DiffRel, Code) -->
	rl_info_get_relation_schema(OldRel, Schema),
	{ rl__ascending_sort_spec(Schema, SortSpec) },
	{ Code = node([difference(output_rel(DiffRel, []), NewRel, OldRel,
			sort_merge(attributes(SortSpec))) - ""]) }.

%-----------------------------------------------------------------------------%

rl_relops__union(SortNeeded, Schema, RelsToUnion,
		MaybeUnionRel, UnionRel, UnionCode) -->
    (
	{ RelsToUnion = [] },
	{ error("rl_relops__union: no relations to union") }
    ;
	{ RelsToUnion = [RelToUnion | RelsToUnion1] },
	(
		{ RelsToUnion1 = [] },
		( { SortNeeded = yes } ->
			rl_relops__sort(RelToUnion, UnionRel, UnionCode)
		;
			{ UnionRel = RelToUnion },
			{ UnionCode = empty }
		)
	;
		{ RelsToUnion1 = [_|_] },
		(
			{ MaybeUnionRel = yes(UnionRel) }
		;
			{ MaybeUnionRel = no },
			rl_info_get_new_temporary(Schema, UnionRel)
		),
		( { SortNeeded = yes } ->
			rl_relops__sort_rels(RelsToUnion,
				SortedRelsToUnion, SortCode)
		;
			{ SortCode = empty },
			{ SortedRelsToUnion = RelsToUnion }
		),
		rl_info_relation_schema_to_type_list(Schema, Types),
		{ rl__ascending_sort_spec(Types, SortAttrs) },
		{ Union = union(output_rel(UnionRel, []), SortedRelsToUnion,
			sort_merge(attributes(SortAttrs))) - "" },
		{ UnionCode = tree(SortCode, node([Union])) }
	)
    ).

%-----------------------------------------------------------------------------%

rl_relops__sort(InputRel, SortedRel, Code) -->
	rl_info_get_relation_schema(InputRel, Schema),
	( { Schema = [] } ->
		% Optimize for a zero arity relation, which occurs
		% for procedures with no input arguments.
		{ SortedRel = InputRel },
		{ Code = empty }
	;
		rl_info_get_new_temporary(schema(Schema), SortedRel),
		{ rl__ascending_sort_spec(Schema, Spec) },
		{ Code = node([
			sort(output_rel(SortedRel, []), InputRel, Spec) - ""
		]) }
	).

:- pred rl_relops__sort_rels(list(relation_id)::in, list(relation_id)::out,
		rl_tree::out, rl_info::rl_info_di, rl_info::rl_info_uo) is det.

rl_relops__sort_rels([], [], empty) --> [].
rl_relops__sort_rels([InputRel | InputRels],
		[OutputRel | OutputRels], Code) -->
	rl_relops__sort(InputRel, OutputRel, SortCode),
	rl_relops__sort_rels(InputRels, OutputRels, Code1),
	{ Code = tree(SortCode, Code1) }.

%-----------------------------------------------------------------------------%

rl_relops__goal(InstMap, Inputs, Outputs, Goals, RLGoal) -->
	rl_info_get_module_info(ModuleInfo),
	rl_info_get_pred_proc_id(PredProcId),
	rl_info_get_proc_info(ProcInfo),
	{ proc_info_varset(ProcInfo, VarSet) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ rl_key__extract_indexing(Inputs, Goals,
		ModuleInfo, VarTypes, Bounds) },
	{ RLGoal = rl_goal(yes(PredProcId), VarSet, VarTypes,
		InstMap, Inputs, Outputs, Goals, Bounds) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
