%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% dense_switch.m

% For switches on atomic types, generate code using a dense jump table.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module ll_backend__dense_switch.

:- interface.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_data, hlds__hlds_llds.
:- import_module ll_backend__llds, ll_backend__code_info.
:- import_module backend_libs__code_model.
:- import_module backend_libs__switch_util, check_hlds__type_util.

	% Should this switch be implemented as a dense jump table?
	% If so, we return the starting and ending values for the table,
	% and whether the switch is not covers all cases or not
	% (we may convert locally semidet switches into locally det
	% switches by adding extra cases whose body is just `fail').

:- pred dense_switch__is_dense_switch(prog_var, cases_list, can_fail, int,
	int, int, can_fail, code_info, code_info).
:- mode dense_switch__is_dense_switch(in, in, in, in, out, out, out, in, out)
	is semidet.

	% Generate code for a switch using a dense jump table.

:- pred dense_switch__generate(cases_list, int, int, prog_var, code_model,
	can_fail, store_map, label, branch_end, branch_end, code_tree,
	code_info, code_info).
:- mode dense_switch__generate(in, in, in, in, in, in, in, in,
	in, out, out, in, out) is det.

	% also used by lookup_switch
:- pred dense_switch__calc_density(int, int, int).
:- mode dense_switch__calc_density(in, in, out) is det.

	% also used by lookup_switch
:- pred dense_switch__type_range(builtin_type, type, int, code_info, code_info).
:- mode dense_switch__type_range(in, in, out, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_goal, hlds__hlds_module.
:- import_module ll_backend__code_gen, ll_backend__trace.
:- import_module backend_libs__builtin_ops.

:- import_module char, map, libs__tree, int, std_util, require, list.

dense_switch__is_dense_switch(CaseVar, TaggedCases, CanFail0, ReqDensity,
		FirstVal, LastVal, CanFail) -->
	{
		list__length(TaggedCases, NumCases),
		NumCases > 2,
		TaggedCases = [FirstCase | _],
		FirstCase = case(_, int_constant(FirstCaseVal), _, _),
		list__index1_det(TaggedCases, NumCases, LastCase),
		LastCase = case(_, int_constant(LastCaseVal), _, _),
		Span is LastCaseVal - FirstCaseVal,
		Range is Span + 1,
		dense_switch__calc_density(NumCases, Range, Density),
		Density > ReqDensity
	},
	( { CanFail0 = can_fail } ->
		% For semidet switches, we normally need to check that
		% the variable is in range before we index into the jump table.
		% However, if the range of the type is sufficiently small,
		% we can make the jump table large enough to hold all
		% of the values for the type.
		code_info__variable_type(CaseVar, Type),
		code_info__get_module_info(ModuleInfo),
		{ classify_type(Type, ModuleInfo, TypeCategory) },
		(
			dense_switch__type_range(TypeCategory, Type, TypeRange),
			{ dense_switch__calc_density(NumCases, TypeRange,
				DetDensity) },
			{ DetDensity > ReqDensity }
		->
			{ CanFail = cannot_fail },
			{ FirstVal = 0 },
			{ LastVal is TypeRange - 1 }
		;
			{ CanFail = CanFail0 },
			{ FirstVal = FirstCaseVal },
			{ LastVal = LastCaseVal }
		)
	;
		{ CanFail = CanFail0 },
		{ FirstVal = FirstCaseVal },
		{ LastVal = LastCaseVal }
	).

%---------------------------------------------------------------------------%

	% Calculate the percentage density given the range
	% and the number of cases.

dense_switch__calc_density(NumCases, Range, Density) :-
	N1 is NumCases * 100,
	Density is N1 // Range.

%---------------------------------------------------------------------------%

	% Determine the range of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is to big to switch on (e.g. int).

dense_switch__type_range(TypeCategory, Type, Range) -->
	code_info__get_module_info(ModuleInfo),
	{ switch_util__type_range(TypeCategory, Type, ModuleInfo,
		Min, Max) },
	{ Range = Max - Min + 1 }.

%---------------------------------------------------------------------------%

dense_switch__generate(Cases, StartVal, EndVal, Var, CodeModel, CanFail,
		StoreMap, EndLabel, MaybeEnd0, MaybeEnd, Code) -->
		% Evaluate the variable which we are going to be switching on
	code_info__produce_variable(Var, VarCode, Rval),
		% If the case values start at some number other than 0,
		% then subtract that number to give us a zero-based index
	{ StartVal = 0 ->
		Index = Rval
	;
		Index = binop(-, Rval, const(int_const(StartVal)))
	},
		% If the switch is not locally deterministic, we need to
		% check that the value of the variable lies within the
		% appropriate range
	(
		{ CanFail = can_fail },
		{ Difference is EndVal - StartVal },
		code_info__fail_if_rval_is_false(
			binop(unsigned_le, Index,
				const(int_const(Difference))), RangeCheck)
	;
		{ CanFail = cannot_fail },
		{ RangeCheck = empty }
	),
		% Now generate the jump table and the cases
	dense_switch__generate_cases(Cases, StartVal, EndVal, CodeModel,
			StoreMap, EndLabel, MaybeEnd0, MaybeEnd,
			Labels, CasesCode),
		% XXX
		% We keep track of the code_info at the end of one of
		% the non-fail cases.  We have to do this because 
		% generating a `fail' slot last would yield the
		% wrong liveness and would not unset the failure cont
		% for a nondet switch.
	{ DoJump = node([
		computed_goto(Index, Labels)
			- "switch (using dense jump table)"
	]) },
		% Assemble the code together
	{ Code = tree(VarCode, tree(RangeCheck, tree(DoJump, CasesCode))) }.

:- pred dense_switch__generate_cases(cases_list, int, int,
	code_model, store_map, label, branch_end, branch_end,
	list(label), code_tree, code_info, code_info).
:- mode dense_switch__generate_cases(in, in, in, in, in, in, in, out,
	out, out, in, out) is det.

dense_switch__generate_cases(Cases0, NextVal, EndVal, CodeModel, StoreMap,
		EndLabel, MaybeEnd0, MaybeEnd, Labels, Code) -->
	(
		{ NextVal > EndVal }
	->
		{ MaybeEnd = MaybeEnd0 },
		{ Labels = [] },
		{ Code = node([
			label(EndLabel)
				- "End of dense switch"
		]) }
	;
		code_info__get_next_label(ThisLabel),
		dense_switch__generate_case(Cases0, NextVal, CodeModel,
			StoreMap, Cases1, MaybeEnd0, MaybeEnd1,
			ThisCode, Comment),
		{ LabelCode = node([
			label(ThisLabel)
				- Comment
		]) },
		{ JumpCode = node([
			goto(label(EndLabel))
				- "branch to end of dense switch"
		]) },
			% generate the rest of the cases.
		{ NextVal1 is NextVal + 1 },
		dense_switch__generate_cases(Cases1, NextVal1, EndVal,
			CodeModel, StoreMap, EndLabel, MaybeEnd1, MaybeEnd,
			Labels1, OtherCasesCode),
		{ Labels = [ThisLabel | Labels1] },
		{ Code =
			tree(LabelCode,
			tree(ThisCode,
			tree(JumpCode,
			     OtherCasesCode)))
		}
	).

%---------------------------------------------------------------------------%

:- pred dense_switch__generate_case(cases_list, int, code_model, store_map,
		cases_list, branch_end, branch_end, code_tree, string,
		code_info, code_info).
:- mode dense_switch__generate_case(in, in, in, in, out, in, out, out, out,
		in, out) is det.

dense_switch__generate_case(Cases0, NextVal, CodeModel, StoreMap, Cases,
		MaybeEnd0, MaybeEnd, Code, Comment) -->
	(
		{ Cases0 = [Case | Cases1] },
		{ Case = case(_, int_constant(NextVal), _, Goal) }
	->
		{ Comment = "case of dense switch" },
		% We need to save the expression cache, etc.,
		% and restore them when we've finished.
		code_info__remember_position(BranchStart),
		trace__maybe_generate_internal_event_code(Goal, TraceCode),
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
			SaveCode),
		{ Code =
			tree(TraceCode,
			tree(GoalCode,
			     SaveCode))
		},
		code_info__reset_to_position(BranchStart),
		{ Cases = Cases1 }
	;
		% This case didn't occur in the original case list
		% - just generate a `fail' for it.
		{ Comment = "compiler-introduced `fail' case of dense switch" },
		code_info__generate_failure(Code),
		{ MaybeEnd = MaybeEnd0 },
		{ Cases = Cases0 }
	).
