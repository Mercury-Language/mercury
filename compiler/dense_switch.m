%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% dense_switch.m

% For switches on atomic types, generate code using a dense jump table.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module ll_backend__dense_switch.

:- interface.

:- import_module backend_libs__switch_util.
:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

	% Should this switch be implemented as a dense jump table?
	% If so, we return the starting and ending values for the table,
	% and whether the switch is not covers all cases or not
	% (we may convert locally semidet switches into locally det
	% switches by adding extra cases whose body is just `fail').

:- pred dense_switch__is_dense_switch(code_info::in, prog_var::in,
	cases_list::in, can_fail::in, int::in, int::out, int::out,
	can_fail::out) is semidet.

	% Generate code for a switch using a dense jump table.

:- pred dense_switch__generate(cases_list::in, int::in, int::in, prog_var::in,
	code_model::in, can_fail::in, hlds_goal_info::in, label::in,
	branch_end::in, branch_end::out, code_tree::out,
	code_info::in, code_info::out) is det.

	% also used by lookup_switch
:- pred dense_switch__calc_density(int::in, int::in, int::out) is det.

	% also used by lookup_switch
:- pred dense_switch__type_range(code_info::in, type_category::in, (type)::in,
	int::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_module.
:- import_module libs__tree.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__trace.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module std_util.

dense_switch__is_dense_switch(CI, CaseVar, TaggedCases, CanFail0, ReqDensity,
		FirstVal, LastVal, CanFail) :-
	list__length(TaggedCases, NumCases),
	NumCases > 2,
	TaggedCases = [FirstCase | _],
	FirstCase = case(_, int_constant(FirstCaseVal), _, _),
	list__index1_det(TaggedCases, NumCases, LastCase),
	LastCase = case(_, int_constant(LastCaseVal), _, _),
	Span = LastCaseVal - FirstCaseVal,
	Range = Span + 1,
	dense_switch__calc_density(NumCases, Range, Density),
	Density > ReqDensity,
	( CanFail0 = can_fail ->
		% For semidet switches, we normally need to check that
		% the variable is in range before we index into the jump table.
		% However, if the range of the type is sufficiently small,
		% we can make the jump table large enough to hold all
		% of the values for the type.
		Type = code_info__variable_type(CI, CaseVar),
		code_info__get_module_info(CI, ModuleInfo),
		classify_type(ModuleInfo, Type) = TypeCategory,
		(
			dense_switch__type_range(CI, TypeCategory, Type,
				TypeRange),
			dense_switch__calc_density(NumCases, TypeRange,
				DetDensity),
			DetDensity > ReqDensity
		->
			CanFail = cannot_fail,
			FirstVal = 0,
			LastVal = TypeRange - 1
		;
			CanFail = CanFail0,
			FirstVal = FirstCaseVal,
			LastVal = LastCaseVal
		)
	;
		CanFail = CanFail0,
		FirstVal = FirstCaseVal,
		LastVal = LastCaseVal
	).

%---------------------------------------------------------------------------%

	% Calculate the percentage density given the range
	% and the number of cases.

dense_switch__calc_density(NumCases, Range, Density) :-
	N1 = NumCases * 100,
	Density = N1 // Range.

%---------------------------------------------------------------------------%

	% Determine the range of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is to big to switch on (e.g. int).

dense_switch__type_range(CI, TypeCategory, Type, Range) :-
	code_info__get_module_info(CI, ModuleInfo),
	switch_util__type_range(TypeCategory, Type, ModuleInfo, Min, Max),
	Range = Max - Min + 1.

%---------------------------------------------------------------------------%

dense_switch__generate(Cases, StartVal, EndVal, Var, CodeModel, CanFail,
		SwitchGoalInfo, EndLabel, MaybeEnd0, MaybeEnd, Code, !CI) :-
		% Evaluate the variable which we are going to be switching on
	code_info__produce_variable(Var, VarCode, Rval, !CI),
		% If the case values start at some number other than 0,
		% then subtract that number to give us a zero-based index
	( StartVal = 0 ->
		Index = Rval
	;
		Index = binop(-, Rval, const(int_const(StartVal)))
	),
		% If the switch is not locally deterministic, we need to
		% check that the value of the variable lies within the
		% appropriate range
	(
		CanFail = can_fail,
		Difference = EndVal - StartVal,
		code_info__fail_if_rval_is_false(
			binop(unsigned_le, Index,
				const(int_const(Difference))), RangeCheck, !CI)
	;
		CanFail = cannot_fail,
		RangeCheck = empty
	),
		% Now generate the jump table and the cases
	dense_switch__generate_cases(Cases, StartVal, EndVal, CodeModel,
		SwitchGoalInfo, EndLabel, MaybeEnd0, MaybeEnd, Labels,
		CasesCode, !CI),

		% XXX
		% We keep track of the code_info at the end of one of
		% the non-fail cases.  We have to do this because
		% generating a `fail' slot last would yield the
		% wrong liveness and would not unset the failure cont
		% for a nondet switch.
	DoJump = node([
		computed_goto(Index, Labels)
			- "switch (using dense jump table)"
	]),
		% Assemble the code together
	Code = tree(VarCode, tree(RangeCheck, tree(DoJump, CasesCode))).

:- pred dense_switch__generate_cases(cases_list::in, int::in, int::in,
	code_model::in, hlds_goal_info::in, label::in, branch_end::in,
	branch_end::out, list(label)::out, code_tree::out,
	code_info::in, code_info::out) is det.

dense_switch__generate_cases(Cases0, NextVal, EndVal, CodeModel,
		SwitchGoalInfo, EndLabel, !MaybeEnd, Labels, Code, !CI) :-
	( NextVal > EndVal ->
		Labels = [],
		Code = node([
			label(EndLabel)
				- "End of dense switch"
		])
	;
		code_info__get_next_label(ThisLabel, !CI),
		dense_switch__generate_case(Cases0, Cases1, NextVal, CodeModel,
			SwitchGoalInfo, !MaybeEnd, ThisCode, Comment, !CI),
		LabelCode = node([
			label(ThisLabel)
				- Comment
		]),
		JumpCode = node([
			goto(label(EndLabel))
				- "branch to end of dense switch"
		]),
			% generate the rest of the cases.
		NextVal1 = NextVal + 1,
		dense_switch__generate_cases(Cases1, NextVal1, EndVal,
			CodeModel, SwitchGoalInfo, EndLabel,
			!MaybeEnd, Labels1, OtherCasesCode, !CI),
		Labels = [ThisLabel | Labels1],
		Code =
			tree(LabelCode,
			tree(ThisCode,
			tree(JumpCode,
			     OtherCasesCode)))
	).

%---------------------------------------------------------------------------%

:- pred dense_switch__generate_case(cases_list::in, cases_list::out, int::in,
	code_model::in, hlds_goal_info::in, branch_end::in, branch_end::out,
	code_tree::out, string::out, code_info::in, code_info::out) is det.

dense_switch__generate_case(!Cases, NextVal, CodeModel, SwitchGoalInfo,
		!MaybeEnd, Code, Comment, !CI) :-
	(
		!.Cases = [Case | !:Cases],
		Case = case(_, int_constant(NextVal), _, Goal)
	->
		Comment = "case of dense switch",
		% We need to save the expression cache, etc.,
		% and restore them when we've finished.
		code_info__remember_position(!.CI, BranchStart),
		trace__maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
			TraceCode, !CI),
		code_gen__generate_goal(CodeModel, Goal, GoalCode, !CI),
		goal_info_get_store_map(SwitchGoalInfo, StoreMap),
		code_info__generate_branch_end(StoreMap, !MaybeEnd, SaveCode,
			!CI),
		Code =
			tree(TraceCode,
			tree(GoalCode,
			     SaveCode)),
		code_info__reset_to_position(BranchStart, !CI)
	;
		% This case didn't occur in the original case list
		% - just generate a `fail' for it.
		Comment = "compiler-introduced `fail' case of dense switch",
		code_info__generate_failure(Code, !CI)
	).
