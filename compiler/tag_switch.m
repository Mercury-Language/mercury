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
:- import_module list, assoc_list.

	% Generate intelligent indexing code for tag based switches.

:- pred tag_switch__generate(list(extended_case), var, code_model, can_fail,
	store_map, label, code_tree, code_info, code_info).
:- mode tag_switch__generate(in, in, in, in, in, in, out, in, out) is det.

:- implementation.

:- import_module hlds_module, hlds_pred, hlds_data, code_gen.
:- import_module options, globals, type_util, std_util.
:- import_module bool, map, tree, int, require.

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
	% tag and then on the secondary tag. The first-level switch is always
	% a chain of if-then-elses, since jump tables are not worthwhile
	% unless they have more than about four entries. Second-level switches
	% will be either chains of if-then-elses or jump tables depending on
	% the number of alternatives sharing the primary tag. Note that we
	% should gain performance even if we can generate no jump tables,
	% since the two levels reduce the expected number of comparisons,
	% and the extraction of the primary and secondary tags is done once
	% instead of being repeated for each test.

	% We put both the possible tag values and the cases into groups
	% depending on their primary tag. We sort the primary tags based
	% on the number of alternatives they have. We test the primary tag
	% of the switch variable against the possible primary tags starting
	% with the most shared primary tag. This arrangement minimizes the
	% expected number of primary tag comparisons.

tag_switch__generate(Cases, Var, CodeModel, CanFail, StoreMap, EndLabel, Code)
		-->
	% group the cases based on primary tag value
	% and find out how many constructors share each primary tag value

	code_info__get_module_info(ModuleInfo),
	code_info__get_proc_info(ProcInfo),
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) },
	{ tag_switch__get_ptag_counts(Type, ModuleInfo,
		MaxPrimary, TagCountMap) },
	{ map__to_assoc_list(TagCountMap, TagCountList) },
	{ map__init(TagCaseMap0) },
	{ tag_switch__group_tags(Cases, TagCaseMap0, TagCaseMap) },

	{ map__count(TagCaseMap, PtagsUsed) },
	code_info__get_globals(Globals),
	{ globals__lookup_int_option(Globals, dense_switch_size,
		DenseSwitchSize) },
	( { PtagsUsed < DenseSwitchSize } ->
		{ PrimaryTable = no }
	;
		{ PrimaryTable = yes }
	),

	% We get a register for holding the tag. The tag is needed only
	% by the switch, and no other code gets control between producing
	% the tag value and all uses of it, so we can release the register
	% for use by the code of the various cases.

	% We forgo using the register if only one primary tag is used,
	% or if the "register" we get is likely to be slower than
	% recomputing the tag from scratch.

	code_info__produce_variable_in_reg(Var, VarCode, VarRval),
	code_info__acquire_reg(r, TagReg),
	code_info__release_reg(TagReg),
	{
		PrimaryTable = no,
		PtagsUsed >= 2,
		globals__lookup_int_option(Globals, num_real_r_regs,
			NumRealRegs),
		(
			NumRealRegs = 0
		;
			( TagReg = reg(r, TagRegNo) ->
				TagRegNo =< NumRealRegs
			;
				error("improper reg in tag switch")
			)
		)
	->
		TagCode = node([assign(TagReg, unop(tag, VarRval))
				- "compute tag to switch on"]),
		TagRval = lval(TagReg)
	;
		TagCode = empty,
		TagRval = unop(tag, VarRval)
	},

	% we generate FailCode and EndCode here because the last case within
	% a primary tag may not be the last case overall

	code_info__get_next_label(FailLabel),
	(
		{ CanFail = cannot_fail },
		{ FailCode = empty }
	;
		{ CanFail = can_fail },
		code_info__generate_failure(FailCode1),
		{ FailCode = tree(
			node([label(FailLabel) - "switch has failed"]),
			FailCode1) }
	),
	{ EndCode = node([label(EndLabel) - "end of tag switch"]) },

	(
		{ PrimaryTable = yes },
		{ tag_switch__order_tags_by_value(0, MaxPrimary, TagCaseMap,
			TagCaseList) },
		tag_switch__generate_primary_tag_table(TagCaseList,
			0, MaxPrimary, VarRval, CodeModel, StoreMap,
			EndLabel, FailLabel, TagCountMap, Labels, TagsCode),
		{ SwitchCode = node([
			computed_goto(TagRval, Labels) -
				"switch on primary tag"
		]) },
		{ CasesCode = tree(SwitchCode, TagsCode) }
	;
		{ PrimaryTable = no },
		{ tag_switch__order_tags_by_count(TagCountList, TagCaseMap,
			TagCaseList) },
		tag_switch__generate_primary_tag_chain(TagCaseList,
			TagRval, VarRval, CodeModel, CanFail, StoreMap,
			EndLabel, FailLabel, TagCountMap, CasesCode)
	),

	{ Code =
		tree(VarCode,
		tree(TagCode,
		tree(CasesCode,
		tree(FailCode,
		     EndCode)))) }.

%-----------------------------------------------------------------------------%

	% Generate a series of if-then-elses, one for each primary tag value.
	% Jump tables are used only on secondary tags.

:- pred tag_switch__generate_primary_tag_chain(ptag_case_list,
	rval, rval, code_model, can_fail, store_map, label, label,
	ptag_count_map, code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_tag_chain(in, in, in, in, in, in, in, in,
	in, out, in, out) is det.

tag_switch__generate_primary_tag_chain([], _, _, _, _, _, _, _, _, empty) -->
	[].
tag_switch__generate_primary_tag_chain([TagGroup | TagGroups],
		TagRval, VarRval, CodeModel, CanFail, StoreMap,
		EndLabel, FailLabel, TagCountMap, Code) -->
	{ TagGroup = Primary - (StagLoc - TagGoalMap) },
	{ map__lookup(TagCountMap, Primary, CountInfo) },
	{ CountInfo = StagLoc1 - MaxSecondary },
	{ StagLoc = StagLoc1 ->
		true
	;
		error("secondary tag locations differ in tag_switch__generate_primary_tag_chain")
	},
	(
		{ TagGroups = [_|_] ; CanFail = can_fail }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		{ TestRval = binop(ne, TagRval,
			unop(mktag, const(int_const(Primary)))) },
		{ TestCode = node([
			if_val(TestRval, label(ElseLabel)) -
				"test primary tag only"
		]) },
		tag_switch__generate_primary_tag_code(TagGoalMap,
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
		( { TagGroups = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		tag_switch__generate_primary_tag_code(TagGoalMap,
			Primary, MaxSecondary, StagLoc, VarRval, CodeModel,
			StoreMap, EndLabel, FailLabel, ThisTagCode)
	),
	tag_switch__generate_primary_tag_chain(TagGroups,
		TagRval, VarRval, CodeModel, CanFail, StoreMap,
		EndLabel, FailLabel, TagCountMap, OtherTagsCode),
	{ Code = tree(ThisTagCode, OtherTagsCode) }.

%-----------------------------------------------------------------------------%

	% Generate the cases for a primary tag using a dense jump table
	% that has an entry for all possible primary tag values.

:- pred tag_switch__generate_primary_tag_table(ptag_case_list, int, int,
	rval, code_model, store_map, label, label, ptag_count_map,
	list(label), code_tree, code_info, code_info).
:- mode tag_switch__generate_primary_tag_table(in, in, in, in,
	in, in, in, in, in, out, out, in, out) is det.

tag_switch__generate_primary_tag_table(TagGroups, CurPrimary, MaxPrimary,
		VarRval, CodeModel, StoreMap, EndLabel, FailLabel, TagCountMap,
		Labels, Code) -->
	( { CurPrimary > MaxPrimary } ->
		{ TagGroups = [] ->
			true
		;
			error("caselist not empty when reaching limiting primary tag")
		},
		{ Labels = [] },
		{ Code = empty }
	;
		{ NextPrimary is CurPrimary + 1 },
		( { TagGroups = [CurPrimary - PrimaryInfo | TagGroups1] } ->
			{ PrimaryInfo = StagLoc - TagGoalMap },
			{ map__lookup(TagCountMap, CurPrimary, CountInfo) },
			{ CountInfo = StagLoc1 - MaxSecondary },
			{ StagLoc = StagLoc1 ->
				true
			;
				error("secondary tag locations differ in tag_switch__generate_primary_tag_table")
			},
			code_info__get_next_label(NewLabel),
			{ LabelCode = node([
				label(NewLabel) -
					"start of a case in primary tag switch"
			]) },
			( { TagGroups1 = [] } ->
				tag_switch__generate_primary_tag_code(
					TagGoalMap, CurPrimary, MaxSecondary,
					StagLoc, VarRval, CodeModel,
					StoreMap, EndLabel, FailLabel,
					ThisTagCode)
			;
				code_info__grab_code_info(CodeInfo),
				tag_switch__generate_primary_tag_code(
					TagGoalMap, CurPrimary, MaxSecondary,
					StagLoc, VarRval, CodeModel,
					StoreMap, EndLabel, FailLabel,
					ThisTagCode),
				code_info__slap_code_info(CodeInfo)
			),
			tag_switch__generate_primary_tag_table(TagGroups1,
				NextPrimary, MaxPrimary, VarRval, CodeModel,
				StoreMap, EndLabel, FailLabel, TagCountMap,
				OtherLabels, OtherCode),
			{ Labels = [NewLabel | OtherLabels] },
			{ Code =
				tree(LabelCode,
				tree(ThisTagCode,
				     OtherCode))
			}
		;
			tag_switch__generate_primary_tag_table(TagGroups,
				NextPrimary, MaxPrimary, VarRval, CodeModel,
				StoreMap, EndLabel, FailLabel, TagCountMap,
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
		;
			{ error("more than one goal for non-shared tag") }
		)
	;
		code_info__get_globals(Globals),
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSwitchSize) },
		{ MaxSecondary < DenseSwitchSize }
	->
		code_info__acquire_reg(r, SecTagReg),
		code_info__release_reg(SecTagReg),
		{ StagLoc = remote ->
			SecTagCode = node([assign(SecTagReg,
				lval(field(Primary, Rval, const(int_const(0)))))
				- "compute remote sec tag to switch on"])
		;
			SecTagCode = node([assign(SecTagReg,
				unop(unmkbody, Rval))
				- "compute local sec tag to switch on"])
		},
		{ SecTagRval = lval(SecTagReg) },
		(
			{ list__length(GoalList, GoalCount) },
			{ FullGoalCount is MaxSecondary + 1 },
			{ FullGoalCount = GoalCount }
		->
			{ CanFail = cannot_fail }
		;
			{ CanFail = can_fail }
		),
		tag_switch__generate_secondary_tag_chain(GoalList, SecTagRval,
			CodeModel, CanFail, StoreMap, EndLabel, FailLabel,
			Codes),
		{ Code = tree(SecTagCode, Codes) }
	;
		tag_switch__generate_secondary_tag_table(GoalList,
			0, MaxSecondary, CodeModel, StoreMap,
			EndLabel, FailLabel, Labels, CasesCode),
		{ StagLoc = remote ->
			Index = lval(field(Primary, Rval, const(int_const(0))))
		;
			Index = unop(unmkbody, Rval)
		},
		{ SwitchCode = node([computed_goto(Index, Labels) -
			"switch on secondary tag"]) },
		{ Code = tree(SwitchCode, CasesCode) }
	).

%-----------------------------------------------------------------------------%

	% Generate the cases for a primary tag by emitting a chain of
	% if-then-elses, the conditions of which check only the secondary tag.

:- pred tag_switch__generate_secondary_tag_chain(stag_goal_list, rval,
	code_model, can_fail, store_map, label, label,
	code_tree, code_info, code_info).
:- mode tag_switch__generate_secondary_tag_chain(in, in, in, in, in, in, in,
	out, in, out) is det.

tag_switch__generate_secondary_tag_chain([], _SecTagRval, _CodeModel, CanFail,
		_StoreMap, _EndLabel, FailLabel, Code) -->
	( { CanFail = can_fail } ->
		{ Code = node([
			goto(label(FailLabel)) -
				"secondary tag does not match"
		]) }
	;
		{ Code = empty }
	).
tag_switch__generate_secondary_tag_chain([Case0 | Cases0], SecTagRval,
		CodeModel, CanFail, StoreMap, EndLabel, FailLabel, Code) -->
	{ Case0 = Secondary - Goal },
	( { Cases0 = [_|_] ; CanFail = can_fail } ->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		{ TestCode = node([if_val(binop(ne, SecTagRval,
			const(int_const(Secondary))),
			label(ElseLabel)) - "test remote sec tag only"]) },
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ ElseCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch",
			label(ElseLabel) -
				"handle next secondary tag"
		]) },
		{ ThisCode =
			tree(TestCode,
			tree(GoalCode,
			tree(SaveCode,
			     ElseCode)))
		},
		( { Cases0 = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		code_info__generate_branch_end(CodeModel, StoreMap, SaveCode),
		{ GotoCode = node([
			goto(label(EndLabel)) -
				"skip to end of secondary tag switch"
		]) },
		{ ThisCode =
			tree(GoalCode,
			tree(SaveCode,
			     GotoCode))
		}
	),
	tag_switch__generate_secondary_tag_chain(Cases0, SecTagRval,
		CodeModel, CanFail, StoreMap, EndLabel, FailLabel, OtherCode),
	{ Code = tree(ThisCode, OtherCode) }.

%-----------------------------------------------------------------------------%

	% Generate the cases for a primary tag using a dense jump table
	% that has an entry for all possible secondary tag values.

:- pred tag_switch__generate_secondary_tag_table(stag_goal_list, int, int,
	code_model, store_map, label, label, list(label),
	code_tree, code_info, code_info).
:- mode tag_switch__generate_secondary_tag_table(in, in, in, in,
	in, in, in, out, out, in, out) is det.

tag_switch__generate_secondary_tag_table(CaseList, CurSecondary, MaxSecondary,
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
					"start of a case in secondary tag switch"
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
			tag_switch__generate_secondary_tag_table(CaseList1,
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
			tag_switch__generate_secondary_tag_table(CaseList,
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

tag_switch__get_ptag_counts(Type, ModuleInfo, MaxPrimary, TagCountMap) :-
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
	map__init(TagCountMap0),
	tag_switch__get_ptag_counts_2(TagList, -1, MaxPrimary,
		TagCountMap0, TagCountMap).

:- pred tag_switch__get_ptag_counts_2(list(cons_tag), int, int,
	ptag_count_map, ptag_count_map).
:- mode tag_switch__get_ptag_counts_2(in, in, out, in, out) is det.

tag_switch__get_ptag_counts_2([], Max, Max, TagCountMap, TagCountMap).
tag_switch__get_ptag_counts_2([ConsTag | TagList], MaxPrimary0, MaxPrimary,
		TagCountMap0, TagCountMap) :-
	( ConsTag = simple_tag(Primary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(TagCountMap0, Primary, _) ->
			error("simple tag is shared")
		;
			map__set(TagCountMap0, Primary, none - (-1),
				TagCountMap1)
		)
	; ConsTag = complicated_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(TagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__set(TagCountMap0, Primary, remote - Max,
				TagCountMap1)
		;
			map__set(TagCountMap0, Primary, remote - Secondary,
				TagCountMap1)
		)
	; ConsTag = complicated_constant_tag(Primary, Secondary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(TagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			int__max(Secondary, MaxSoFar, Max),
			map__set(TagCountMap0, Primary, local - Max,
				TagCountMap1)
		;
			map__set(TagCountMap0, Primary, local - Secondary,
				TagCountMap1)
		)
	;
		error("non-du tag in tag_switch__get_ptag_counts_2")
	),
	tag_switch__get_ptag_counts_2(TagList, MaxPrimary1, MaxPrimary,
		TagCountMap1, TagCountMap).

%-----------------------------------------------------------------------------%

	% Group together all the cases that depend on the given variable
	% having the same primary tag value.

:- pred tag_switch__group_tags(cases_list, ptag_case_map, ptag_case_map).
:- mode tag_switch__group_tags(in, in, out) is det.

tag_switch__group_tags([], TagCaseMap, TagCaseMap).
tag_switch__group_tags([Case0 | Cases0], TagCaseMap0, TagCaseMap) :-
	Case0 = case(_Priority, Tag, _ConsId, Goal),
	( Tag = simple_tag(Primary) ->
		( map__search(TagCaseMap0, Primary, _Group) ->
			error("simple tag is shared")
		;
			true
		),
		map__init(TagGoalMap0),
		map__set(TagGoalMap0, -1, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, none - TagGoalMap, TagCaseMap1)
	; Tag = complicated_tag(Primary, Secondary) ->
		( map__search(TagCaseMap0, Primary, Group) ->
			Group = StagLoc - TagGoalMap0,
			( StagLoc = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			)
		;
			map__init(TagGoalMap0)
		),
		map__set(TagGoalMap0, Secondary, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, remote - TagGoalMap, TagCaseMap1)
	; Tag = complicated_constant_tag(Primary, Secondary) ->
		( map__search(TagCaseMap0, Primary, Group) ->
			Group = StagLoc - TagGoalMap0,
			( StagLoc = local ->
				true
			;
				error("local tag is shared with non-local")
			)
		;
			map__init(TagGoalMap0)
		),
		map__set(TagGoalMap0, Secondary, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, local - TagGoalMap, TagCaseMap1)
	;
		error("non-du tag in tag_switch__group_tags")
	),
	tag_switch__group_tags(Cases0, TagCaseMap1, TagCaseMap).

%-----------------------------------------------------------------------------%

	% Order the primary tags based on the number of secondary tags
	% associated with them, putting the ones with the most secondary tags
	% first. We use selection sort.
	% Note that it is not an error for a primary tag to have no case list,
	% since this can happen in semideterministic switches.

:- pred tag_switch__order_tags_by_count(ptag_count_list, ptag_case_map,
	ptag_case_list).
:- mode tag_switch__order_tags_by_count(in, in, out) is det.

tag_switch__order_tags_by_count(TagCountList0, TagCaseMap0, TagCaseList) :-
	(
		tag_switch__select_frequent_tag(TagCountList0,
			Primary, _, TagCountList1)
	->
		( map__search(TagCaseMap0, Primary, TagCase) ->
			map__delete(TagCaseMap0, Primary, TagCaseMap1),
			tag_switch__order_tags_by_count(TagCountList1,
				TagCaseMap1, TagCaseList1),
			TagCaseList = [Primary - TagCase | TagCaseList1]
		;
			tag_switch__order_tags_by_count(TagCountList1,
				TagCaseMap0, TagCaseList)
		)
	;
		( map__is_empty(TagCaseMap0) ->
			TagCaseList = []
		;
			error("TagCaseMap0 is not empty in tag_switch__order_tags_by_count")
		)
	).

	% Select the most frequently used primary tag based on the number of
	% secondary tags associated with it.

:- pred tag_switch__select_frequent_tag(ptag_count_list, tag_bits, int,
	ptag_count_list).
:- mode tag_switch__select_frequent_tag(in, out, out, out) is semidet.

tag_switch__select_frequent_tag([TagCount0 | TagCountList1], Primary, Count,
		TagCountList) :-
	TagCount0 = Primary0 - (_ - Count0),
	(
		tag_switch__select_frequent_tag(TagCountList1,
			Primary1, Count1, TagCountList2),
		Count1 > Count0
	->
		Primary = Primary1,
		Count = Count1,
		TagCountList = [TagCount0 | TagCountList2]
	;
		Primary = Primary0,
		Count = Count0,
		TagCountList = TagCountList1
	).

%-----------------------------------------------------------------------------%

	% Order the primary tags based on their value, lowest value first.
	% We scan through the primary tags values from zero to maximum.
	% Note that it is not an error for a primary tag to have no case list,
	% since this can happen in semideterministic switches.

:- pred tag_switch__order_tags_by_value(int, int,
	ptag_case_map, ptag_case_list).
:- mode tag_switch__order_tags_by_value(in, in, in, out) is det.

tag_switch__order_tags_by_value(Ptag, MaxPtag, TagCaseMap0, TagCaseList) :-
	( MaxPtag >= Ptag ->
		NextPtag is Ptag + 1,
		( map__search(TagCaseMap0, Ptag, TagCase) ->
			map__delete(TagCaseMap0, Ptag, TagCaseMap1),
			tag_switch__order_tags_by_value(NextPtag, MaxPtag,
				TagCaseMap1, TagCaseList1),
			TagCaseList = [Ptag - TagCase | TagCaseList1]
		;
			tag_switch__order_tags_by_value(NextPtag, MaxPtag,
				TagCaseMap0, TagCaseList)
		)
	;
		( map__is_empty(TagCaseMap0) ->
			TagCaseList = []
		;
			error("TagCaseMap0 is not empty in tag_switch__order_tags_by_value")
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
