%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% tag_switch.m - generate switches based on primary and secondary tags.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module tag_switch.

:- interface.

:- import_module hlds_goal, llds, switch_gen, code_info.
:- import_module list.

	% Generate intelligent indexing code for tag based switches.

:- pred tag_switch__generate(list(extended_case), var, code_model, can_fail,
	store_map, label, code_tree, code_info, code_info).
:- mode tag_switch__generate(in, in, in, in, in, in, out, in, out) is det.

:- implementation.

:- import_module hlds_module, hlds_pred, hlds_data, code_gen.
:- import_module options, globals, type_util, prog_data.
:- import_module assoc_list, bool, map, tree, int, require, std_util, term.

% where is the secondary tag (if any) for this primary tag value
:- type stag_loc	--->	none ; local ; remote.

% map secondary tag values (-1 stands for none) to their goal
:- type stag_goal_map	==	map(int, hlds_goal).
:- type stag_goal_list	==	assoc_list(int, hlds_goal).

% map primary tag values to the set of their goals
:- type ptag_case_map	==	map(tag_bits, pair(stag_loc, stag_goal_map)).
:- type ptag_case_list	==	assoc_list(tag_bits,
					pair(stag_loc, stag_goal_map)).

% map primary tag values to the number of constructors sharing them
:- type ptag_count_map	==	map(tag_bits, pair(stag_loc, int)).
:- type ptag_count_list ==	assoc_list(tag_bits, pair(stag_loc, int)).

%-----------------------------------------------------------------------------%

	% The idea is to generate two-level switches, first on the primary
	% tag and then on the secondary tag. Since more than one function
	% symbol can be eliminated by a failed primary tag test, this reduces
	% the expected the number of comparisons required before finding the
	% code corresponding to the actual value of the switch variable.
	% We also get a speedup compared to non-tag tests by extracting
	% the primary and secondary tags once instead of repeatedly for
	% each functor test.
	%
	% We have three methods we can use for generating the code for the
	% switches on both primary and secondary tags.
	%
	% 1. try-me-else chains have the form
	%
	%		if (tag(var) != tag1) goto L1
	%		code for tag1
	%		goto end
	%	L1:	if (tag(var) != tag2) goto L2
	%		code for tag2
	%		goto end
	%	L2:	...
	%	Ln:	code for last possible tag value (or failure)
	%		goto end
	%
	% 2. try chains have the form
	%
	%		if (tag(var) == tag1) goto L1
	%		if (tag(var) == tag2) goto L2
	%		...
	%		code for last possible tag value (or failure)
	%		goto end
	%	L1:	code for tag1
	%		goto end
	%	L2:	code for tag2
	%		goto end
	%		...
	%
	% 3. jump tables have the form
	%
	%		goto tag(var) of L1, L2, ...
	%	L1:	code for tag1
	%		goto end
	%	L2:	code for tag2
	%		goto end
	%		...
	%
	% Note that for a det switch with two tag values, try-me-else chains
	% and and try chains are equivalent.
	%
	% Which method is best depends on the number of possible tag values,
	% the costs of taken jumps and table lookups on the given architecture
	% and on the frequency with which the various alternatives are taken.
	%
	% While the first two are in principle known at compile time,
	% the third is not. Nevertheless, for switches on primary tags
	% we can use the heuristic that the more secondary tags assigned to
	% a primary tag, the more likely that switch variable will have that
	% primary tag at runtime.
	%
	% For try-me-else chains, we want tag1 to be the most frequent case,
	% tag 2 the next most frequent case, etc.
	%
	% For det try chains, we want the last tag value to be the most
	% frequent case, since it can be reached without taken jumps.
	% We want tag1 to be the next most frequent, tag2 the next most
	% frequent after that, etc.
	%
	% For semidet try chains, there is no last possible tag value (the
	% code for failure occupies its position), so we want tag1 to be
	% the most frequent case, tag 2 the next most frequent case, etc.
	%
	% For jump tables, the position of the labels in the computed goto
	% must conform to their numerical value. The order of the code
	% fragments does not really matter, although the last has a slight
	% edge in that no goto is needed to reach the code following the
	% switch. If there is no code following the switch (which happens
	% very frequently), then even this advantage is nullified.

:- type switch_method	--->	try_me_else_chain ; try_chain ; jump_table.

tag_switch__generate(Cases, Var, CodeModel, CanFail, StoreMap, EndLabel, Code)
		-->
	% group the cases based on primary tag value
	% and find out how many constructors share each primary tag value

	code_info__get_module_info(ModuleInfo),
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) },
	{ tag_switch__get_ptag_counts(Type, ModuleInfo,
		MaxPrimary, PtagCountMap) },
	{ map__to_assoc_list(PtagCountMap, PtagCountList) },
	{ map__init(PtagCaseMap0) },
	{ tag_switch__group_cases_by_ptag(Cases, PtagCaseMap0, PtagCaseMap) },

	{ map__count(PtagCaseMap, PtagsUsed) },
	code_info__get_globals(Globals),
	{ globals__lookup_int_option(Globals, dense_switch_size,
		DenseSwitchSize) },
	{ globals__lookup_int_option(Globals, try_switch_size,
		TrySwitchSize) },
	( { PtagsUsed > DenseSwitchSize } ->
		{ PrimaryMethod = jump_table }
	; { PtagsUsed > TrySwitchSize } ->
		{ PrimaryMethod = try_chain }
	;
		{ PrimaryMethod = try_me_else_chain }
	),

	% We get a register for holding the tag. The tag is needed only
	% by the switch, and no other code gets control between producing
	% the tag value and all uses of it, so we can release the register
	% for use by the code of the various cases.

	% We forgo using the register if the primary tag is needed only once,
	% or if the "register" we get is likely to be slower than
	% recomputing the tag from scratch.

	code_info__produce_variable_in_reg(Var, VarCode, VarRval),
	code_info__acquire_reg(r, PtagReg),
	code_info__release_reg(PtagReg),
	{
		PrimaryMethod \= jump_table,
		PtagsUsed >= 2,
		globals__lookup_int_option(Globals, num_real_r_regs,
			NumRealRegs),
		(
			NumRealRegs = 0
		;
			( PtagReg = reg(r, PtagRegNo) ->
				PtagRegNo =< NumRealRegs
			;
				error("improper reg in tag switch")
			)
		)
	->
		PtagCode = node([
			assign(PtagReg, unop(tag, VarRval))
				- "compute tag to switch on"
		]),
		PtagRval = lval(PtagReg)
	;
		PtagCode = empty,
		PtagRval = unop(tag, VarRval)
	},

	% we generate FailCode and EndCode here because the last case within
	% a primary tag may not be the last case overall

	code_info__get_next_label(FailLabel),
	{ FailLabelCode = node([
		label(FailLabel) -
			"switch has failed"
	]) },
	(
		{ CanFail = cannot_fail },
		{ FailCode = node([
			goto(do_not_reached) - "oh-oh, det switch failed"
		]) }
	;
		{ CanFail = can_fail },
		code_info__generate_failure(FailCode)
	),
	{ LabelledFailCode = tree(FailLabelCode, FailCode) },

	{ EndCode = node([label(EndLabel) - "end of tag switch"]) },

	(
		{ PrimaryMethod = jump_table },
		{ tag_switch__order_ptags_by_value(0, MaxPrimary, PtagCaseMap,
			PtagCaseList) },
		tag_switch__generate_primary_jump_table(PtagCaseList,
			0, MaxPrimary, VarRval, CodeModel, StoreMap,
			EndLabel, FailLabel, PtagCountMap, Labels, TableCode),
		{ SwitchCode = node([
			computed_goto(PtagRval, Labels) -
				"switch on primary tag"
		]) },
		{ CasesCode = tree(SwitchCode, TableCode) }
	;
		{ PrimaryMethod = try_chain },
		{ tag_switch__order_ptags_by_count(PtagCountList, PtagCaseMap,
			PtagCaseList0) },
		{
			CanFail = cannot_fail,
			PtagCaseList0 = [MostFreqCase | OtherCases]
		->
			list__append(OtherCases, [MostFreqCase], PtagCaseList)
		;
			PtagCaseList = PtagCaseList0
		},
		tag_switch__generate_primary_try_chain(PtagCaseList,
			PtagRval, VarRval, CodeModel, CanFail, StoreMap,
			EndLabel, FailLabel, PtagCountMap, empty, empty,
			CasesCode)
	;
		{ PrimaryMethod = try_me_else_chain },
		{ tag_switch__order_ptags_by_count(PtagCountList, PtagCaseMap,
			PtagCaseList) },
		tag_switch__generate_primary_try_me_else_chain(PtagCaseList,
			PtagRval, VarRval, CodeModel, CanFail, StoreMap,
			EndLabel, FailLabel, PtagCountMap, CasesCode)
	),

	{ Code =
		tree(VarCode,
		tree(PtagCode,
		tree(CasesCode,
		tree(LabelledFailCode,
		     EndCode)))) }.

%-----------------------------------------------------------------------------%

	% Generate a switch on a primary tag value using a try-me-else chain.

:- pred tag_switch__generate_primary_try_me_else_chain(ptag_case_list,
	rval, rval, code_model, can_fail, store_map, label, label,
	ptag_count_map, code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_try_me_else_chain(in, in, in, in, in, in,
	in, in, in, out, in, out) is det.

tag_switch__generate_primary_try_me_else_chain([], _, _, _, _, _, _, _, _, _)
		-->
	{ error("generate_primary_try_me_else_chain: empty switch") }.
tag_switch__generate_primary_try_me_else_chain([PtagGroup | PtagGroups],
		TagRval, VarRval, CodeModel, CanFail, StoreMap,
		EndLabel, FailLabel, PtagCountMap, Code) -->
	{ PtagGroup = Primary - (StagLoc - StagGoalMap) },
	{ map__lookup(PtagCountMap, Primary, CountInfo) },
	{ CountInfo = StagLoc1 - MaxSecondary },
	{ StagLoc = StagLoc1 ->
		true
	;
		error("secondary tag locations differ in generate_primary_try_me_else_chain")
	},
	(
		{ PtagGroups = [_|_] ; CanFail = can_fail }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		{ TestRval = binop(ne, TagRval,
			unop(mktag, const(int_const(Primary)))) },
		{ TestCode = node([
			if_val(TestRval, label(ElseLabel)) -
				"test primary tag only"
		]) },
		tag_switch__generate_primary_tag_code(StagGoalMap,
			Primary, MaxSecondary, StagLoc, VarRval, CodeModel,
			StoreMap, EndLabel, FailLabel, TagCode),
		{ ElseCode = node([
			label(ElseLabel) -
				"handle next primary tag"
		]) },
		{ ThisTagCode =
			tree(TestCode,
			tree(TagCode,
			     ElseCode))
		},
		( { PtagGroups = [_|_] } ->
			code_info__slap_code_info(CodeInfo),
			tag_switch__generate_primary_try_me_else_chain(
				PtagGroups, TagRval, VarRval, CodeModel,
				CanFail, StoreMap, EndLabel, FailLabel,
				PtagCountMap, OtherTagsCode),
			{ Code = tree(ThisTagCode, OtherTagsCode) }
		;
			% FailLabel ought to be the next label anyway,
			% so this goto will be optimized away (unless the
			% layout of the failcode in the caller changes).
			{ FailCode = node([
				goto(label(FailLabel)) -
					"primary tag with no code to handle it"
			]) },
			{ Code = tree(ThisTagCode, FailCode) }
		)
	;
		tag_switch__generate_primary_tag_code(StagGoalMap,
			Primary, MaxSecondary, StagLoc, VarRval, CodeModel,
			StoreMap, EndLabel, FailLabel, ThisTagCode),
		{ Code = ThisTagCode }
	).

%-----------------------------------------------------------------------------%

	% Generate a switch on a primary tag value using a try chain.

:- pred tag_switch__generate_primary_try_chain(ptag_case_list,
	rval, rval, code_model, can_fail, store_map, label, label,
	ptag_count_map, code_tree, code_tree, code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_try_chain(in, in, in, in, in, in,
	in, in, in, in, in, out, in, out) is det.

tag_switch__generate_primary_try_chain([], _, _, _, _, _, _, _, _, _, _, _) -->
	 { error("empty list in generate_primary_try_chain") }.
tag_switch__generate_primary_try_chain([PtagGroup | PtagGroups],
		TagRval, VarRval, CodeModel, CanFail, StoreMap, EndLabel,
		FailLabel, PtagCountMap, PrevTests0, PrevCases0, Code) -->
	{ PtagGroup = Primary - (StagLoc - StagGoalMap) },
	{ map__lookup(PtagCountMap, Primary, CountInfo) },
	{ CountInfo = StagLoc1 - MaxSecondary },
	{ StagLoc = StagLoc1 ->
		true
	;
		error("secondary tag locations differ in generate_primary_try_chain")
	},
	(
		{ PtagGroups = [_|_] ; CanFail = can_fail }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ThisPtagLabel),
		{ TestRval = binop(eq, TagRval,
			unop(mktag, const(int_const(Primary)))) },
		{ TestCode = node([
			if_val(TestRval, label(ThisPtagLabel)) -
				"test primary tag only"
		]) },
		{ LabelCode = node([
			label(ThisPtagLabel) -
				"this primary tag"
		]) },
		tag_switch__generate_primary_tag_code(StagGoalMap,
			Primary, MaxSecondary, StagLoc, VarRval, CodeModel,
			StoreMap, EndLabel, FailLabel, TagCode),
		{ PrevTests = tree(PrevTests0, TestCode) },
		{ PrevCases = tree(tree(LabelCode, TagCode), PrevCases0) },
		( { PtagGroups = [_|_] } ->
			code_info__slap_code_info(CodeInfo),
			tag_switch__generate_primary_try_chain(PtagGroups,
				TagRval, VarRval, CodeModel, CanFail, StoreMap,
				EndLabel, FailLabel, PtagCountMap,
				PrevTests, PrevCases, Code)
		;
			{ FailCode = node([
				goto(label(FailLabel)) -
					"primary tag with no code to handle it"
			]) },
			{ Code = tree(PrevTests, tree(FailCode, PrevCases)) }
		)
	;
		{ Comment = node([
			comment("fallthrough to last tag value") - ""
		]) },
		tag_switch__generate_primary_tag_code(StagGoalMap,
			Primary, MaxSecondary, StagLoc, VarRval,
			CodeModel, StoreMap, EndLabel, FailLabel,
			TagCode),
		{ Code =
			tree(PrevTests0,
			tree(Comment,
			tree(TagCode,
			     PrevCases0)))
		}
	).

%-----------------------------------------------------------------------------%

	% Generate the cases for a primary tag using a dense jump table
	% that has an entry for all possible primary tag values.

:- pred tag_switch__generate_primary_jump_table(ptag_case_list, int, int,
	rval, code_model, store_map, label, label, ptag_count_map,
	list(label), code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_jump_table(in, in, in, in,
	in, in, in, in, in, out, out, in, out) is det.

tag_switch__generate_primary_jump_table(PtagGroups, CurPrimary, MaxPrimary,
		VarRval, CodeModel, StoreMap, EndLabel, FailLabel, PtagCountMap,
		Labels, Code) -->
	( { CurPrimary > MaxPrimary } ->
		{ PtagGroups = [] ->
			true
		;
			error("caselist not empty when reaching limiting primary tag")
		},
		{ Labels = [] },
		{ Code = empty }
	;
		{ NextPrimary is CurPrimary + 1 },
		( { PtagGroups = [CurPrimary - PrimaryInfo | PtagGroups1] } ->
			{ PrimaryInfo = StagLoc - StagGoalMap },
			{ map__lookup(PtagCountMap, CurPrimary, CountInfo) },
			{ CountInfo = StagLoc1 - MaxSecondary },
			{ StagLoc = StagLoc1 ->
				true
			;
				error("secondary tag locations differ in generate_primary_jump_table")
			},
			code_info__get_next_label(NewLabel),
			{ LabelCode = node([
				label(NewLabel) -
					"start of a case in primary tag switch"
			]) },
			( { PtagGroups1 = [] } ->
				tag_switch__generate_primary_tag_code(
					StagGoalMap, CurPrimary, MaxSecondary,
					StagLoc, VarRval, CodeModel,
					StoreMap, EndLabel, FailLabel,
					ThisTagCode)
			;
				code_info__grab_code_info(CodeInfo),
				tag_switch__generate_primary_tag_code(
					StagGoalMap, CurPrimary, MaxSecondary,
					StagLoc, VarRval, CodeModel,
					StoreMap, EndLabel, FailLabel,
					ThisTagCode),
				code_info__slap_code_info(CodeInfo)
			),
			tag_switch__generate_primary_jump_table(PtagGroups1,
				NextPrimary, MaxPrimary, VarRval, CodeModel,
				StoreMap, EndLabel, FailLabel, PtagCountMap,
				OtherLabels, OtherCode),
			{ Labels = [NewLabel | OtherLabels] },
			{ Code =
				tree(LabelCode,
				tree(ThisTagCode,
				     OtherCode))
			}
		;
			tag_switch__generate_primary_jump_table(PtagGroups,
				NextPrimary, MaxPrimary, VarRval, CodeModel,
				StoreMap, EndLabel, FailLabel, PtagCountMap,
				OtherLabels, Code),
			{ Labels = [FailLabel | OtherLabels] }
		)
	).

%-----------------------------------------------------------------------------%

	% Generate the code corresponding to a primary tag.
	% If this primary tag has secondary tags, decide whether we should
	% use a jump table to implement the secondary switch.

:- pred tag_switch__generate_primary_tag_code(stag_goal_map, tag_bits, int,
	stag_loc, rval, code_model, store_map,
	label, label, code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_tag_code(in, in, in, in, in, in, in,
	in, in, out, in, out) is det.

tag_switch__generate_primary_tag_code(GoalMap, Primary, MaxSecondary, StagLoc,
		Rval, CodeModel, StoreMap, EndLabel, FailLabel, Code) -->
	{ map__to_assoc_list(GoalMap, GoalList) },
	(
		{ StagLoc = none }
	->
		% There is no secondary tag, so there is no switch on it
		( { GoalList = [-1 - Goal] } ->
			code_gen__generate_goal(CodeModel, Goal, GoalCode),
			code_info__generate_branch_end(CodeModel, StoreMap,
				SaveCode),
			{ GotoCode = node([
				goto(label(EndLabel)) -
					"skip to end of primary tag switch"
			]) },
			{ Code =
				tree(GoalCode,
				tree(SaveCode,
				     GotoCode))
			}
		; { GoalList = [] } ->
			{ error("no goal for non-shared tag") }
		;
			{ error("more than one goal for non-shared tag") }
		)
	;
		% There is a secondary tag, so figure out how to switch on it
		code_info__get_globals(Globals),
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSwitchSize) },
		{ globals__lookup_int_option(Globals, try_switch_size,
			TrySwitchSize) },
		{ MaxSecondary > DenseSwitchSize ->
			SecondaryMethod = jump_table
		; MaxSecondary > TrySwitchSize ->
			SecondaryMethod = try_chain
		;
			SecondaryMethod = try_me_else_chain
		},

		{ StagLoc = remote ->
			OrigStagRval = lval(field(Primary, Rval,
				const(int_const(0)))),
			Comment = "compute remote sec tag to switch on"
		;
			OrigStagRval = unop(unmkbody, Rval),
			Comment = "compute local sec tag to switch on"
		},

		code_info__acquire_reg(r, StagReg),
		code_info__release_reg(StagReg),
		{
			SecondaryMethod \= jump_table,
			MaxSecondary >= 2,
			globals__lookup_int_option(Globals, num_real_r_regs,
				NumRealRegs),
			(
				NumRealRegs = 0
			;
				( StagReg = reg(r, StagRegNo) ->
					StagRegNo =< NumRealRegs
				;
					error("improper reg in tag switch")
				)
			)
		->
			StagCode = node([
				assign(StagReg, OrigStagRval) -
					Comment
			]),
			StagRval = lval(StagReg)
		;
			StagCode = empty,
			StagRval = OrigStagRval
		},


		(
			{ list__length(GoalList, GoalCount) },
			{ FullGoalCount is MaxSecondary + 1 },
			{ FullGoalCount = GoalCount }
		->
			{ CanFail = cannot_fail }
		;
			{ CanFail = can_fail }
		),

		(
			{ SecondaryMethod = jump_table },
			tag_switch__generate_secondary_jump_table(GoalList,
				0, MaxSecondary, CodeModel, StoreMap,
				EndLabel, FailLabel, Labels, CasesCode),
			{ SwitchCode = node([
				computed_goto(StagRval, Labels) -
					"switch on secondary tag"
			]) },
			{ Code = tree(SwitchCode, CasesCode) }
		;
			{ SecondaryMethod = try_chain },
			tag_switch__generate_secondary_try_chain(GoalList,
				StagRval, CodeModel, CanFail, StoreMap,
				EndLabel, FailLabel, empty, empty, Codes),
			{ Code = tree(StagCode, Codes) }
		;
			{ SecondaryMethod = try_me_else_chain },
			tag_switch__generate_secondary_try_me_else_chain(
				GoalList, StagRval, CodeModel, CanFail,
				StoreMap, EndLabel, FailLabel, Codes),
			{ Code = tree(StagCode, Codes) }
		)
	).

%-----------------------------------------------------------------------------%

	% Generate a switch on a secondary tag value using a try-me-else chain.

:- pred tag_switch__generate_secondary_try_me_else_chain(stag_goal_list, rval,
	code_model, can_fail, store_map, label, label,
	code_tree, code_info, code_info).
:- mode tag_switch__generate_secondary_try_me_else_chain(in, in, in, in, in,
	in, in, out, in, out) is det.

tag_switch__generate_secondary_try_me_else_chain([], _, _, _, _, _, _, _) -->
	{ error("generate_secondary_try_me_else_chain: empty switch") }.
tag_switch__generate_secondary_try_me_else_chain([Case0 | Cases0], StagRval,
		CodeModel, CanFail, StoreMap, EndLabel, FailLabel, Code) -->
	{ Case0 = Secondary - Goal },
	( { Cases0 = [_|_] ; CanFail = can_fail } ->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		{ TestCode = node([
			if_val(binop(ne, StagRval,
					const(int_const(Secondary))),
				label(ElseLabel))
				- "test remote sec tag only"
		]) },
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ GotoLabelCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch",
			label(ElseLabel) -
				"handle next secondary tag"
		]) },
		{ ThisCode =
			tree(TestCode,
			tree(GoalCode,
			tree(SaveCode,
			     GotoLabelCode)))
		},
		( { Cases0 = [_|_] } ->
			code_info__slap_code_info(CodeInfo),
			tag_switch__generate_secondary_try_me_else_chain(Cases0,
				StagRval, CodeModel, CanFail, StoreMap,
				EndLabel, FailLabel, OtherCode),
			{ Code = tree(ThisCode, OtherCode) }
		;
			{ FailCode = node([
				goto(label(FailLabel)) -
					"secondary tag does not match"
			]) },
			{ Code = tree(ThisCode, FailCode) }
		)
	;
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ GotoCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch"
		]) },
		{ Code =
			tree(GoalCode,
			tree(SaveCode,
			     GotoCode))
		}
	).

%-----------------------------------------------------------------------------%

	% Generate a switch on a secondary tag value using a try chain.

:- pred tag_switch__generate_secondary_try_chain(stag_goal_list, rval,
	code_model, can_fail, store_map, label, label,
	code_tree, code_tree, code_tree, code_info, code_info).
:- mode tag_switch__generate_secondary_try_chain(in, in, in, in, in,
	in, in, in, in, out, in, out) is det.

tag_switch__generate_secondary_try_chain([], _, _, _, _, _, _, _, _, _) -->
	{ error("generate_secondary_try_chain: empty switch") }.
tag_switch__generate_secondary_try_chain([Case0 | Cases0], StagRval,
		CodeModel, CanFail, StoreMap, EndLabel, FailLabel,
		PrevTests0, PrevCases0, Code) -->
	{ Case0 = Secondary - Goal },
	( { Cases0 = [_|_] ; CanFail = can_fail } ->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ThisStagLabel),
		{ TestCode = node([
			if_val(binop(eq, StagRval,
					const(int_const(Secondary))),
				label(ThisStagLabel))
				- "test remote sec tag only"
		]) },
		{ LabelCode = node([
			label(ThisStagLabel) -
				"handle next secondary tag"
		]) },
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ GotoCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch"
		]) },
		{ ThisCode =
			tree(LabelCode,
			tree(GoalCode,
			tree(SaveCode,
			     GotoCode)))
		},
		{ PrevTests = tree(PrevTests0, TestCode) },
		{ PrevCases = tree(ThisCode, PrevCases0) },
		( { Cases0 = [_|_] } ->
			code_info__slap_code_info(CodeInfo),
			tag_switch__generate_secondary_try_chain(Cases0,
				StagRval, CodeModel, CanFail, StoreMap,
				EndLabel, FailLabel,
				PrevTests, PrevCases, Code)
		;
			{ FailCode = node([
				goto(label(FailLabel)) -
					"secondary tag with no code to handle it"
			]) },
			{ Code = tree(PrevTests, tree(FailCode, PrevCases)) }
		)
	;
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ GotoCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch"
		]) },
		{ Code =
			tree(PrevTests0,
			tree(GoalCode,
			tree(SaveCode,
			tree(GotoCode,
			     PrevCases0))))
		}
	).

%-----------------------------------------------------------------------------%

	% Generate the cases for a primary tag using a dense jump table
	% that has an entry for all possible secondary tag values.

:- pred tag_switch__generate_secondary_jump_table(stag_goal_list, int, int,
	code_model, store_map, label, label, list(label),
	code_tree, code_info, code_info).
:- mode tag_switch__generate_secondary_jump_table(in, in, in, in,
	in, in, in, out, out, in, out) is det.

tag_switch__generate_secondary_jump_table(CaseList, CurSecondary, MaxSecondary,
		CodeModel, StoreMap, EndLabel, FailLabel, Labels, Code) -->
	( { CurSecondary > MaxSecondary } ->
		{ CaseList = [] ->
			true
		;
			error("caselist not empty when reaching limiting secondary tag")
		},
		{ Labels = [] },
		{ Code = empty }
	;
		{ NextSecondary is CurSecondary + 1 },
		( { CaseList = [CurSecondary - Goal | CaseList1] } ->
			code_info__get_next_label(NewLabel),
			{ LabelCode = node([
				label(NewLabel) -
					"start of case in secondary tag switch"
			]) },
			( { CaseList1 = [] } ->
				code_gen__generate_goal(CodeModel, Goal,
					GoalCode),
				code_info__generate_branch_end(CodeModel,
					StoreMap, SaveCode)
			;
				code_info__grab_code_info(CodeInfo),
				code_gen__generate_goal(CodeModel, Goal,
					GoalCode),
				code_info__generate_branch_end(CodeModel,
					StoreMap, SaveCode),
				code_info__slap_code_info(CodeInfo)
			),
			{ GotoCode = node([
				goto(label(EndLabel)) -
					"branch to end of tag switch"
			]) },
			tag_switch__generate_secondary_jump_table(CaseList1,
				NextSecondary, MaxSecondary, CodeModel,
				StoreMap, EndLabel, FailLabel, OtherLabels,
				OtherCode),
			{ Labels = [NewLabel | OtherLabels] },
			{ Code =
				tree(LabelCode,
				tree(GoalCode,
				tree(SaveCode,
				tree(GotoCode,
				     OtherCode))))
			}
		;
			tag_switch__generate_secondary_jump_table(CaseList,
				NextSecondary, MaxSecondary, CodeModel,
				StoreMap, EndLabel, FailLabel, OtherLabels,
				Code),
			{ Labels = [FailLabel | OtherLabels] }
		)
	).

%-----------------------------------------------------------------------------%

	% Find out how many secondary tags share each primary tag
	% of the given variable.

:- pred tag_switch__get_ptag_counts(type, module_info, int, ptag_count_map).
:- mode tag_switch__get_ptag_counts(in, in, out, out) is det.

tag_switch__get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
	( type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in tag_switch__get_ptag_counts")
	),
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	( Body = du_type(_, ConsTable, _) ->
		map__to_assoc_list(ConsTable, ConsList),
		tag_switch__cons_list_to_tag_list(ConsList, TagList)
	;
		error("non-du type in tag_switch__get_ptag_counts")
	),
	map__init(PtagCountMap0),
	tag_switch__get_ptag_counts_2(TagList, -1, MaxPrimary,
		PtagCountMap0, PtagCountMap).

:- pred tag_switch__get_ptag_counts_2(list(cons_tag), int, int,
	ptag_count_map, ptag_count_map).
:- mode tag_switch__get_ptag_counts_2(in, in, out, in, out) is det.

tag_switch__get_ptag_counts_2([], Max, Max, PtagCountMap, PtagCountMap).
tag_switch__get_ptag_counts_2([ConsTag | TagList], MaxPrimary0, MaxPrimary,
		PtagCountMap0, PtagCountMap) :-
	( ConsTag = simple_tag(Primary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, _) ->
			error("simple tag is shared")
		;
			map__det_insert(PtagCountMap0, Primary, none - (-1),
				PtagCountMap1)
		)
	; ConsTag = complicated_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__det_update(PtagCountMap0, Primary, remote - Max,
				PtagCountMap1)
		;
			map__det_insert(PtagCountMap0, Primary,
				remote - Secondary, PtagCountMap1)
		)
	; ConsTag = complicated_constant_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__det_update(PtagCountMap0, Primary, local - Max,
				PtagCountMap1)
		;
			map__det_insert(PtagCountMap0, Primary,
				local - Secondary, PtagCountMap1)
		)
	;
		error("non-du tag in tag_switch__get_ptag_counts_2")
	),
	tag_switch__get_ptag_counts_2(TagList, MaxPrimary1, MaxPrimary,
		PtagCountMap1, PtagCountMap).

%-----------------------------------------------------------------------------%

	% Group together all the cases that depend on the given variable
	% having the same primary tag value.

:- pred tag_switch__group_cases_by_ptag(cases_list,
	ptag_case_map, ptag_case_map).
:- mode tag_switch__group_cases_by_ptag(in, in, out) is det.

tag_switch__group_cases_by_ptag([], PtagCaseMap, PtagCaseMap).
tag_switch__group_cases_by_ptag([Case0 | Cases0], PtagCaseMap0, PtagCaseMap) :-
	Case0 = case(_Priority, Tag, _ConsId, Goal),
	( Tag = simple_tag(Primary) ->
		( map__search(PtagCaseMap0, Primary, _Group) ->
			error("simple tag is shared")
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, -1, Goal, StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				none - StagGoalMap, PtagCaseMap1)
		)
	; Tag = complicated_tag(Primary, Secondary) ->
		( map__search(PtagCaseMap0, Primary, Group) ->
			Group = StagLoc - StagGoalMap0,
			( StagLoc = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_update(PtagCaseMap0, Primary,
				remote - StagGoalMap, PtagCaseMap1)
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				remote - StagGoalMap, PtagCaseMap1)
		)
	; Tag = complicated_constant_tag(Primary, Secondary) ->
		( map__search(PtagCaseMap0, Primary, Group) ->
			Group = StagLoc - StagGoalMap0,
			( StagLoc = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_update(PtagCaseMap0, Primary,
				local - StagGoalMap, PtagCaseMap1)
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, Secondary, Goal,
				StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				local - StagGoalMap, PtagCaseMap1)
		)
	;
		error("non-du tag in tag_switch__group_cases_by_ptag")
	),
	tag_switch__group_cases_by_ptag(Cases0, PtagCaseMap1, PtagCaseMap).

%-----------------------------------------------------------------------------%

	% Order the primary tags based on the number of secondary tags
	% associated with them, putting the ones with the most secondary tags
	% first. We use selection sort.
	% Note that it is not an error for a primary tag to have no case list;
	% this can happen in semideterministic switches, or in det switches
	% where the initial inst of the switch variable is a bound(...) inst
	% representing a subtype.

:- pred tag_switch__order_ptags_by_count(ptag_count_list, ptag_case_map,
	ptag_case_list).
:- mode tag_switch__order_ptags_by_count(in, in, out) is det.

tag_switch__order_ptags_by_count(PtagCountList0, PtagCaseMap0, PtagCaseList) :-
	(
		tag_switch__select_frequent_ptag(PtagCountList0,
			Primary, _, PtagCountList1)
	->
		( map__search(PtagCaseMap0, Primary, PtagCase) ->
			map__delete(PtagCaseMap0, Primary, PtagCaseMap1),
			tag_switch__order_ptags_by_count(PtagCountList1,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Primary - PtagCase | PtagCaseList1]
		;
			tag_switch__order_ptags_by_count(PtagCountList1,
				PtagCaseMap0, PtagCaseList)
		)
	;
		( map__is_empty(PtagCaseMap0) ->
			PtagCaseList = []
		;
			error("PtagCaseMap0 is not empty in tag_switch__order_ptags_by_count")
		)
	).

	% Select the most frequently used primary tag based on the number of
	% secondary tags associated with it.

:- pred tag_switch__select_frequent_ptag(ptag_count_list, tag_bits, int,
	ptag_count_list).
:- mode tag_switch__select_frequent_ptag(in, out, out, out) is semidet.

tag_switch__select_frequent_ptag([PtagCount0 | PtagCountList1], Primary, Count,
		PtagCountList) :-
	PtagCount0 = Primary0 - (_ - Count0),
	(
		tag_switch__select_frequent_ptag(PtagCountList1,
			Primary1, Count1, PtagCountList2),
		Count1 > Count0
	->
		Primary = Primary1,
		Count = Count1,
		PtagCountList = [PtagCount0 | PtagCountList2]
	;
		Primary = Primary0,
		Count = Count0,
		PtagCountList = PtagCountList1
	).

%-----------------------------------------------------------------------------%

	% Order the primary tags based on their value, lowest value first.
	% We scan through the primary tags values from zero to maximum.
	% Note that it is not an error for a primary tag to have no case list,
	% since this can happen in semideterministic switches.

:- pred tag_switch__order_ptags_by_value(int, int,
	ptag_case_map, ptag_case_list).
:- mode tag_switch__order_ptags_by_value(in, in, in, out) is det.

tag_switch__order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
	( MaxPtag >= Ptag ->
		NextPtag is Ptag + 1,
		( map__search(PtagCaseMap0, Ptag, PtagCase) ->
			map__delete(PtagCaseMap0, Ptag, PtagCaseMap1),
			tag_switch__order_ptags_by_value(NextPtag, MaxPtag,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Ptag - PtagCase | PtagCaseList1]
		;
			tag_switch__order_ptags_by_value(NextPtag, MaxPtag,
				PtagCaseMap0, PtagCaseList)
		)
	;
		( map__is_empty(PtagCaseMap0) ->
			PtagCaseList = []
		;
			error("PtagCaseMap0 is not empty in order_ptags_by_value")
		)
	).

%-----------------------------------------------------------------------------%

:- pred tag_switch__cons_list_to_tag_list(assoc_list(cons_id, cons_tag),
	list(cons_tag)).
:- mode tag_switch__cons_list_to_tag_list(in, out) is det.

tag_switch__cons_list_to_tag_list([], []).
tag_switch__cons_list_to_tag_list([_ConsId - ConsTag | ConsList],
		[ConsTag | Tagslist]) :-
	tag_switch__cons_list_to_tag_list(ConsList, Tagslist).
