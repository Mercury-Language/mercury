%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: switch_gen.m
% Authors: conway, fjh, zs
%
% This module handles the generation of code for switches, which are
% disjunctions that do not require backtracking.  Switches are detected
% in switch_detection.m.  This is the module that determines what
% sort of indexing to use for each switch and then actually generates the
% code.
%
% Currently the following forms of indexing are used:
%
%	For switches on atomic data types (int, char, enums),
%	if the cases are not sparse, we use the value of the switch variable
%	to index into a jump table.
%
%	If all the alternative goals for a switch on an atomic data type
%	contain only construction unifications of constants, then we generate
%	a dense lookup table (an array) for each output variable of the switch,
%	rather than a dense jump table, so that executing the switch becomes
%	a matter of doing an array index for each output variable - avoiding
%	the branch overhead of the jump-table.
%
%	For switches on discriminated union types, we generate code that does
%	indexing first on the primary tag, and then on the secondary tag (if
%	the primary tag is shared between several function symbols). The
%	indexing code for switches on both primary and secondary tags can be
%	in the form of a try-me-else chain, a try chain, a dense jump table
%	or a binary search.
%
%	For switches on strings, we lookup the address to jump to in a
%	hash table, using open addressing to resolve hash collisions.
%
%	For all other cases (or if the --smart-indexing option was
%	disabled), we just generate a chain of if-then-elses.
%
%---------------------------------------------------------------------------%

:- module switch_gen.

:- interface.

:- import_module prog_data, hlds_goal, hlds_data, code_model, code_info, llds.
:- import_module list.

:- pred switch_gen__generate_switch(code_model, prog_var, can_fail, list(case),
	store_map, hlds_goal_info, code_tree, code_info, code_info).
:- mode switch_gen__generate_switch(in, in, in, in, in, in, out, in, out)
	is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module dense_switch, string_switch, tag_switch, lookup_switch.
:- import_module code_gen, unify_gen, code_aux, code_util.
:- import_module switch_util, type_util.
:- import_module trace, globals, options.

:- import_module bool, int, string, map, tree, std_util, require.

%---------------------------------------------------------------------------%

	% Choose which method to use to generate the switch.
	% CanFail says whether the switch covers all cases.

switch_gen__generate_switch(CodeModel, CaseVar, CanFail, Cases, StoreMap,
		GoalInfo, Code) -->
	switch_gen__determine_category(CaseVar, SwitchCategory),
	code_info__get_next_label(EndLabel),
	switch_gen__lookup_tags(Cases, CaseVar, TaggedCases0),
	{ list__sort_and_remove_dups(TaggedCases0, TaggedCases) },
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, smart_indexing,
		Indexing) },
	(
		% Check for a switch on a type whose representation
		% uses reserved addresses 
		{ list__member(Case, TaggedCases) },    
		{ Case = case(_Priority, Tag, _ConsId, _Goal) },
		{
			Tag = reserved_address(_)
		;
			Tag = shared_with_reserved_addresses(_, _)
		}
	->
		% XXX This may be be inefficient in some cases.
		switch_gen__generate_all_cases(TaggedCases, CaseVar,
			CodeModel, CanFail, StoreMap, EndLabel, no, MaybeEnd,
			Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		code_info__get_maybe_trace_info(MaybeTraceInfo),
		{ MaybeTraceInfo = no },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, lookup_switch_size,
			LookupSize) },
		{ NumCases >= LookupSize },
		{ globals__lookup_int_option(Globals, lookup_switch_req_density,
			ReqDensity) },
		lookup_switch__is_lookup_switch(CaseVar, TaggedCases, GoalInfo,
			CanFail, ReqDensity, StoreMap, no, MaybeEndPrime,
			CodeModel, FirstVal, LastVal, NeedRangeCheck,
			NeedBitVecCheck, OutVars, CaseVals, MLiveness)
	->
		{ MaybeEnd = MaybeEndPrime },
		lookup_switch__generate(CaseVar, OutVars, CaseVals,
			FirstVal, LastVal, NeedRangeCheck, NeedBitVecCheck,
			MLiveness, StoreMap, no, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSize) },
		{ NumCases >= DenseSize },
		{ globals__lookup_int_option(Globals, dense_switch_req_density,
			ReqDensity) },
		dense_switch__is_dense_switch(CaseVar, TaggedCases, CanFail,
			ReqDensity, FirstVal, LastVal, CanFail1)
	->
		dense_switch__generate(TaggedCases,
			FirstVal, LastVal, CaseVar, CodeModel, CanFail1,
			StoreMap, EndLabel, no, MaybeEnd, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = string_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, string_switch_size,
			StringSize) },
		{ NumCases >= StringSize }
	->
		string_switch__generate(TaggedCases, CaseVar, CodeModel,
			CanFail, StoreMap, EndLabel, no, MaybeEnd, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = tag_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, tag_switch_size,
			TagSize) },
		{ NumCases >= TagSize }
	->
		tag_switch__generate(TaggedCases, CaseVar, CodeModel, CanFail,
			StoreMap, EndLabel, no, MaybeEnd, Code)
	;
		% To generate a switch, first we flush the
		% variable on whose tag we are going to switch, then we
		% generate the cases for the switch.

		switch_gen__generate_all_cases(TaggedCases, CaseVar,
			CodeModel, CanFail, StoreMap, EndLabel, no, MaybeEnd,
			Code)
	),
	code_info__after_all_branches(StoreMap, MaybeEnd).

%---------------------------------------------------------------------------%

	% We categorize switches according to whether the value
	% being switched on is an atomic type, a string, or
	% something more complicated.

:- pred switch_gen__determine_category(prog_var, switch_category,
	code_info, code_info).
:- mode switch_gen__determine_category(in, out, in, out) is det.

switch_gen__determine_category(CaseVar, SwitchCategory) -->
	code_info__variable_type(CaseVar, Type),
	code_info__get_module_info(ModuleInfo),
	{ classify_type(Type, ModuleInfo, TypeCategory) },
	{ switch_util__type_cat_to_switch_cat(TypeCategory, SwitchCategory) }.

%---------------------------------------------------------------------------%

:- pred switch_gen__lookup_tags(list(case), prog_var, cases_list,
				code_info, code_info).
:- mode switch_gen__lookup_tags(in, in, out, in, out) is det.

switch_gen__lookup_tags([], _, []) --> [].
switch_gen__lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, Goal) },
	code_info__cons_id_to_tag(Var, ConsId, Tag),
	{ switch_util__switch_priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
	switch_gen__lookup_tags(Cases, Var, TaggedCases).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Generate a switch as a chain of if-then-elses.
	%
	% To generate a case for a switch we generate
	% code to do a tag-test and fall through to the next case in
	% the event of failure.
	%
	% Each case except the last consists of
	%
	%	a tag test, jumping to the next case if it fails
	%	the goal for that case
	%	code to move variables to where the store map says they
	%		ought to be
	%	a branch to the end of the switch.
	%
	% For the last case, if the switch covers all cases that can occur,
	% we don't need to generate the tag test, and we never need to
	% generate the branch to the end of the switch.
	%
	% After the last case, we put the end-of-switch label which other
	% cases branch to after their case goals.
	%
	% In the important special case of a det switch with two cases,
	% we try to find out which case will be executed more frequently,
	% and put that one first. This minimizes the number of pipeline
	% breaks caused by taken branches.

:- pred switch_gen__generate_all_cases(list(extended_case), prog_var,
	code_model, can_fail, store_map, label, branch_end, branch_end,
	code_tree, code_info, code_info).
:- mode switch_gen__generate_all_cases(in, in, in, in, in, in, in, out, out,
	in, out) is det.

switch_gen__generate_all_cases(Cases0, Var, CodeModel, CanFail, StoreMap,
		EndLabel, MaybeEnd0, MaybeEnd, Code) -->
	code_info__produce_variable(Var, VarCode, _Rval),
	(
		{ CodeModel = model_det },
		{ CanFail = cannot_fail },
		{ Cases0 = [Case1, Case2] },
		{ Case1 = case(_, _, _, Goal1) },
		{ Case2 = case(_, _, _, Goal2) }
	->
		code_info__get_pred_id(PredId),
		code_info__get_proc_id(ProcId),
		{ code_util__count_recursive_calls(Goal1, PredId, ProcId,
			Min1, Max1) },
		{ code_util__count_recursive_calls(Goal2, PredId, ProcId,
			Min2, Max2) },
		{
			Max1 = 0,	% Goal1 is a base case
			Min2 = 1	% Goal2 is probably singly recursive
		->
			Cases = [Case2, Case1]
		;
			Max2 = 0,	% Goal2 is a base case
			Min1 > 1	% Goal1 is at least doubly recursive
		->
			Cases = [Case2, Case1]
		;
			Cases = Cases0
		}
	;
		{ Cases = Cases0 }
	),
	switch_gen__generate_cases(Cases, Var, CodeModel, CanFail,
		StoreMap, EndLabel, MaybeEnd0, MaybeEnd, CasesCode),
	{ Code = tree(VarCode, CasesCode) }.

:- pred switch_gen__generate_cases(list(extended_case), prog_var, code_model,
	can_fail, store_map, label, branch_end, branch_end, code_tree,
	code_info, code_info).
:- mode switch_gen__generate_cases(in, in, in, in, in, in, in, out, out,
	in, out) is det.

	% At the end of a locally semidet switch, we fail because we
	% came across a tag which was not covered by one of the cases.
	% It is followed by the end of switch label to which the cases
	% branch.
switch_gen__generate_cases([], _Var, _CodeModel, CanFail, _StoreMap,
		EndLabel, MaybeEnd, MaybeEnd, Code) -->
	( { CanFail = can_fail } ->
		code_info__generate_failure(FailCode)
	;
		{ FailCode = empty }
	),
	{ EndCode = node([
		label(EndLabel) -
			"end of switch"
	]) },
	{ Code = tree(FailCode, EndCode) }.

switch_gen__generate_cases([case(_, _, Cons, Goal) | Cases], Var, CodeModel,
		CanFail, StoreMap, EndLabel, MaybeEnd0, MaybeEnd, CasesCode) -->
	code_info__remember_position(BranchStart),
	(
		{ Cases = [_|_] ; CanFail = can_fail }
	->
		unify_gen__generate_tag_test(Var, Cons, branch_on_failure,
			NextLabel, TestCode),
		trace__maybe_generate_internal_event_code(Goal, TraceCode),
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1,
			SaveCode),
		{ ElseCode = node([
			goto(label(EndLabel)) -
				"skip to the end of the switch",
			label(NextLabel) -
				"next case"
		]) },
		{ ThisCaseCode =
			tree(TestCode,
			tree(TraceCode,
			tree(GoalCode,
			tree(SaveCode,
			     ElseCode))))
		}
	;
		trace__maybe_generate_internal_event_code(Goal, TraceCode),
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1,
			SaveCode),
		{ ThisCaseCode =
			tree(TraceCode,
			tree(GoalCode,
			     SaveCode))
		}
	),
	code_info__reset_to_position(BranchStart),
		% generate the rest of the cases.
	switch_gen__generate_cases(Cases, Var, CodeModel, CanFail, StoreMap,
		EndLabel, MaybeEnd1, MaybeEnd, OtherCasesCode),
	{ CasesCode = tree(ThisCaseCode, OtherCasesCode) }.

%------------------------------------------------------------------------------%
