%-----------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% ml_tag_switch.m - generate switches based on primary and secondary tags,
% for the MLDS back-end.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module ml_tag_switch.

:- interface.

:- import_module prog_data, hlds_data, mlds, ml_switch_gen, ml_code_util.
:- import_module llds. % XXX for code_model
:- import_module list.

	% Generate efficient indexing code for tag based switches.

:- pred ml_tag_switch__generate(list(ml_extended_case)::in, prog_var::in,
		code_model::in, can_fail::in, prog_context::in,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

:- implementation.

:- import_module hlds_goal, hlds_module.
:- import_module ml_code_gen, ml_unify_gen.
:- import_module builtin_ops, type_util.

:- import_module assoc_list, map, int, string, require, std_util.

%-----------------------------------------------------------------------------%

% XXX the stuff below here is duplicated from switch_gen.m;
% it should go in a new module switch_util.m.

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

ml_tag_switch__generate(Cases, Var, CodeModel, CanFail, Context,
		MLDS_Decls, MLDS_Statements) -->
	% generate the rval for the primary tag

	ml_gen_var(Var, VarLval),
	{ VarRval = lval(VarLval) },
	{ PTagRval = unop(std_unop(tag), VarRval) },

	% group the cases based on primary tag value,
	% find out how many constructors share each primary tag value,
	% and sort the cases so that the most frequently occurring
	% primary tag values come first.

	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	ml_variable_type(Var, Type),
	{ ml_tag_switch__get_ptag_counts(Type, ModuleInfo,
		_MaxPrimary, PtagCountMap) },
	{ map__to_assoc_list(PtagCountMap, PtagCountList) },
	{ map__init(PtagCaseMap0) },
	{ ml_tag_switch__group_cases_by_ptag(Cases, PtagCaseMap0,
		PtagCaseMap) },
	{ ml_tag_switch__order_ptags_by_count(PtagCountList, PtagCaseMap,
		PtagCaseList) },

	% generate the switch on the primary tag

	ml_tag_switch__gen_ptag_cases(PtagCaseList, Var, CanFail, CodeModel,
		PtagCountMap, Context, MLDS_Cases),
	ml_switch_generate_default(CanFail, CodeModel, Context, Default),

	% package up the results into a switch statement

	{ SwitchStmt = switch(mlds__native_int_type, PTagRval, MLDS_Cases,
		Default) },
	{ SwitchStatement = mlds__statement(SwitchStmt,
		mlds__make_context(Context)) },
	{ MLDS_Decls = [] },
	{ MLDS_Statements = [SwitchStatement] }.

:- pred ml_tag_switch__gen_ptag_cases(ptag_case_list::in, prog_var::in,
		can_fail::in, code_model::in, ptag_count_map::in,
		prog_context::in, list(mlds__switch_case)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_tag_switch__gen_ptag_cases([], _, _, _, _, _, []) --> [].
ml_tag_switch__gen_ptag_cases([Case | Cases], Var, CanFail, CodeModel,
		PtagCountMap, Context, [MLDS_Case | MLDS_Cases]) -->
	ml_tag_switch__gen_ptag_case(Case, Var, CanFail, CodeModel,
		PtagCountMap, Context, MLDS_Case),
	ml_tag_switch__gen_ptag_cases(Cases, Var, CanFail, CodeModel,
		PtagCountMap, Context, MLDS_Cases).

:- pred ml_tag_switch__gen_ptag_case(
		pair(tag_bits, pair(stag_loc, stag_goal_map))::in,
		prog_var::in, can_fail::in, code_model::in, ptag_count_map::in,
		prog_context::in, mlds__switch_case::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_tag_switch__gen_ptag_case(Case, Var, CanFail, CodeModel, PtagCountMap,
		Context, MLDS_Case) -->
	{ Case = PrimaryTag - (SecTagLocn - GoalMap) },
	{ map__lookup(PtagCountMap, PrimaryTag, CountInfo) },
	{ CountInfo = SecTagLocn1 - MaxSecondary },
	{ SecTagLocn = SecTagLocn1 ->
		true
	;
		error("ml_tag_switch.m: secondary tag locations differ")
	},
	{ map__to_assoc_list(GoalMap, GoalList) },
	( { SecTagLocn = none } ->
		% There is no secondary tag, so there is no switch on it
		( { GoalList = [_ - Goal] } ->
			ml_gen_goal(CodeModel, Goal, MLDS_Statement)
		; { GoalList = [] } ->
			{ error("no goal for non-shared tag") }
		;
			{ error("more than one goal for non-shared tag") }
		)
	;
		(
			{ CanFail = cannot_fail }
		->
			{ CaseCanFail = cannot_fail }
		;
			{ list__length(GoalList, GoalCount) },
			{ FullGoalCount is MaxSecondary + 1 },
			{ FullGoalCount = GoalCount }
		->
			{ CaseCanFail = cannot_fail }
		;
			{ CaseCanFail = can_fail }
		),
		( { GoalList = [_ - Goal], CaseCanFail = cannot_fail } ->
			% There is only one possible matching goal,
			% so we don't need to switch on it
			ml_gen_goal(CodeModel, Goal, MLDS_Statement)
		;
			ml_tag_switch__gen_stag_switch(GoalList, PrimaryTag,
				SecTagLocn, Var, CodeModel, CaseCanFail,
				Context, MLDS_Statement)
		)
	),
	{ PrimaryTagRval = const(int_const(PrimaryTag)) },
	{ MLDS_Case = [match_value(PrimaryTagRval)] - MLDS_Statement }.

:- pred ml_tag_switch__gen_stag_switch(stag_goal_list, int, stag_loc,
		prog_var, code_model, can_fail, prog_context,
		mlds__statement, ml_gen_info, ml_gen_info).
:- mode ml_tag_switch__gen_stag_switch(in, in, in, in, in, in, in, out,
		in, out) is det.

ml_tag_switch__gen_stag_switch(Cases, PrimaryTag, StagLocn, Var,
		CodeModel, CanFail, Context, MLDS_Statement) -->

	% generate the rval for the secondary tag

	=(Info),
	{ ml_gen_info_get_module_info(Info, ModuleInfo) },
	ml_variable_type(Var, VarType),
	ml_gen_var(Var, VarLval),
	{ VarRval = lval(VarLval) },
	(
		{ StagLocn = local },
		{ STagRval = unop(std_unop(unmkbody), VarRval) }
	;
		{ StagLocn = remote },
		{ STagRval = ml_gen_secondary_tag_rval(PrimaryTag,
			VarType, ModuleInfo, VarRval) }
	;
		{ StagLocn = none },
		{ error("ml_tag_switch__gen_stag_switch: no stag") }
	),

	% generate the switch on the secondary tag

	ml_tag_switch__gen_stag_cases(Cases, CodeModel, MLDS_Cases),
	ml_switch_generate_default(CanFail, CodeModel, Context, Default),

	% package up the results into a switch statement

	{ SwitchStmt = switch(mlds__native_int_type, STagRval, MLDS_Cases,
		Default) },
	{ SwitchStatement = mlds__statement(SwitchStmt,
		mlds__make_context(Context)) },
	{ MLDS_Statement = SwitchStatement }.

:- pred ml_tag_switch__gen_stag_cases(stag_goal_list, code_model,
		list(mlds__switch_case), ml_gen_info, ml_gen_info).
:- mode ml_tag_switch__gen_stag_cases(in, in, out, in, out) is det.

ml_tag_switch__gen_stag_cases([], _, []) --> [].
ml_tag_switch__gen_stag_cases([Case | Cases], CodeModel,
		[MLDS_Case | MLDS_Cases]) -->
	ml_tag_switch__gen_stag_case(Case, CodeModel, MLDS_Case),
	ml_tag_switch__gen_stag_cases(Cases, CodeModel, MLDS_Cases).

:- pred ml_tag_switch__gen_stag_case(pair(tag_bits, hlds_goal), code_model,
		mlds__switch_case, ml_gen_info, ml_gen_info).
:- mode ml_tag_switch__gen_stag_case(in, in, out, in, out) is det.

ml_tag_switch__gen_stag_case(Case, CodeModel, MLDS_Case) -->
	{ Case = Stag - Goal },
	{ StagRval = const(int_const(Stag)) },
	ml_gen_goal(CodeModel, Goal, MLDS_Statement),
	{ MLDS_Case = [match_value(StagRval)] - MLDS_Statement }.

%-----------------------------------------------------------------------------%

% XXX everything from here to the end is duplicated from switch_gen.m;
% it should go in a new module switch_util.m.

%-----------------------------------------------------------------------------%

	% Find out how many secondary tags share each primary tag
	% of the given variable.

:- pred ml_tag_switch__get_ptag_counts(prog_type, module_info, int, ptag_count_map).
:- mode ml_tag_switch__get_ptag_counts(in, in, out, out) is det.

ml_tag_switch__get_ptag_counts(Type, ModuleInfo, MaxPrimary, PtagCountMap) :-
	( type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in ml_tag_switch__get_ptag_counts")
	),
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	( Body = du_type(_, ConsTable, _, _) ->
		map__to_assoc_list(ConsTable, ConsList),
		ml_tag_switch__cons_list_to_tag_list(ConsList, TagList)
	;
		error("non-du type in ml_tag_switch__get_ptag_counts")
	),
	map__init(PtagCountMap0),
	ml_tag_switch__get_ptag_counts_2(TagList, -1, MaxPrimary,
		PtagCountMap0, PtagCountMap).

:- pred ml_tag_switch__get_ptag_counts_2(list(cons_tag), int, int,
	ptag_count_map, ptag_count_map).
:- mode ml_tag_switch__get_ptag_counts_2(in, in, out, in, out) is det.

ml_tag_switch__get_ptag_counts_2([], Max, Max, PtagCountMap, PtagCountMap).
ml_tag_switch__get_ptag_counts_2([ConsTag | TagList], MaxPrimary0, MaxPrimary,
		PtagCountMap0, PtagCountMap) :-
	( ConsTag = unshared_tag(Primary) ->
		int__max(MaxPrimary0, Primary, MaxPrimary1),
		( map__search(PtagCountMap0, Primary, _) ->
			error("unshared tag is shared")
		;
			map__det_insert(PtagCountMap0, Primary, none - (-1),
				PtagCountMap1)
		)
	; ConsTag = shared_remote_tag(Primary, Secondary) ->
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
	; ConsTag = shared_local_tag(Primary, Secondary) ->
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
		error("non-du tag in ml_tag_switch__get_ptag_counts_2")
	),
	ml_tag_switch__get_ptag_counts_2(TagList, MaxPrimary1, MaxPrimary,
		PtagCountMap1, PtagCountMap).

%-----------------------------------------------------------------------------%

	% Group together all the cases that depend on the given variable
	% having the same primary tag value.

:- pred ml_tag_switch__group_cases_by_ptag(ml_cases_list,
	ptag_case_map, ptag_case_map).
:- mode ml_tag_switch__group_cases_by_ptag(in, in, out) is det.

ml_tag_switch__group_cases_by_ptag([], PtagCaseMap, PtagCaseMap).
ml_tag_switch__group_cases_by_ptag([Case0 | Cases0], PtagCaseMap0, PtagCaseMap) :-
	Case0 = case(_Priority, Tag, _ConsId, Goal),
	( Tag = unshared_tag(Primary) ->
		( map__search(PtagCaseMap0, Primary, _Group) ->
			error("unshared tag is shared")
		;
			map__init(StagGoalMap0),
			map__det_insert(StagGoalMap0, -1, Goal, StagGoalMap),
			map__det_insert(PtagCaseMap0, Primary,
				none - StagGoalMap, PtagCaseMap1)
		)
	; Tag = shared_remote_tag(Primary, Secondary) ->
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
	; Tag = shared_local_tag(Primary, Secondary) ->
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
		error("non-du tag in ml_tag_switch__group_cases_by_ptag")
	),
	ml_tag_switch__group_cases_by_ptag(Cases0, PtagCaseMap1, PtagCaseMap).

%-----------------------------------------------------------------------------%

	% Order the primary tags based on the number of secondary tags
	% associated with them, putting the ones with the most secondary tags
	% first. We use selection sort.
	% Note that it is not an error for a primary tag to have no case list;
	% this can happen in semideterministic switches, or in det switches
	% where the initial inst of the switch variable is a bound(...) inst
	% representing a subtype.

:- pred ml_tag_switch__order_ptags_by_count(ptag_count_list, ptag_case_map,
	ptag_case_list).
:- mode ml_tag_switch__order_ptags_by_count(in, in, out) is det.

ml_tag_switch__order_ptags_by_count(PtagCountList0, PtagCaseMap0, PtagCaseList) :-
	(
		ml_tag_switch__select_frequent_ptag(PtagCountList0,
			Primary, _, PtagCountList1)
	->
		( map__search(PtagCaseMap0, Primary, PtagCase) ->
			map__delete(PtagCaseMap0, Primary, PtagCaseMap1),
			ml_tag_switch__order_ptags_by_count(PtagCountList1,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Primary - PtagCase | PtagCaseList1]
		;
			ml_tag_switch__order_ptags_by_count(PtagCountList1,
				PtagCaseMap0, PtagCaseList)
		)
	;
		( map__is_empty(PtagCaseMap0) ->
			PtagCaseList = []
		;
			error("PtagCaseMap0 is not empty in ml_tag_switch__order_ptags_by_count")
		)
	).

	% Select the most frequently used primary tag based on the number of
	% secondary tags associated with it.

:- pred ml_tag_switch__select_frequent_ptag(ptag_count_list, tag_bits, int,
	ptag_count_list).
:- mode ml_tag_switch__select_frequent_ptag(in, out, out, out) is semidet.

ml_tag_switch__select_frequent_ptag([PtagCount0 | PtagCountList1], Primary, Count,
		PtagCountList) :-
	PtagCount0 = Primary0 - (_ - Count0),
	(
		ml_tag_switch__select_frequent_ptag(PtagCountList1,
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

:- pred ml_tag_switch__order_ptags_by_value(int, int,
	ptag_case_map, ptag_case_list).
:- mode ml_tag_switch__order_ptags_by_value(in, in, in, out) is det.

ml_tag_switch__order_ptags_by_value(Ptag, MaxPtag, PtagCaseMap0, PtagCaseList) :-
	( MaxPtag >= Ptag ->
		NextPtag is Ptag + 1,
		( map__search(PtagCaseMap0, Ptag, PtagCase) ->
			map__delete(PtagCaseMap0, Ptag, PtagCaseMap1),
			ml_tag_switch__order_ptags_by_value(NextPtag, MaxPtag,
				PtagCaseMap1, PtagCaseList1),
			PtagCaseList = [Ptag - PtagCase | PtagCaseList1]
		;
			ml_tag_switch__order_ptags_by_value(NextPtag, MaxPtag,
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

:- pred ml_tag_switch__cons_list_to_tag_list(assoc_list(cons_id, cons_tag),
	list(cons_tag)).
:- mode ml_tag_switch__cons_list_to_tag_list(in, out) is det.

ml_tag_switch__cons_list_to_tag_list([], []).
ml_tag_switch__cons_list_to_tag_list([_ConsId - ConsTag | ConsList],
		[ConsTag | Tagslist]) :-
	ml_tag_switch__cons_list_to_tag_list(ConsList, Tagslist).

%-----------------------------------------------------------------------------%
