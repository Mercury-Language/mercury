%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_simplify_switch.m
% Main author: fjh

% This module, which is invoked by the various parts of the MLDS code generator
% that generate switches, converts MLDS switches into computed gotos
% or if-then-else chains.

% We should eventually also handle lookup switches and binary search switches
% here too.

% The choice of which exactly which simplifications will get
% performed depends on the target (e.g. whether it understands
% switches) and the --prefer-switch option.

%-----------------------------------------------------------------------------%

:- module ml_simplify_switch.
:- interface.

:- import_module mlds, ml_code_util.

:- pred ml_simplify_switch(mlds__stmt::in, mlds__context::in,
		mlds__statement::out,
		ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_switch_gen, builtin_ops, type_util.
:- import_module globals, options.

:- import_module bool, int, list, map, require, std_util.

%-----------------------------------------------------------------------------%

ml_simplify_switch(Stmt0, MLDS_Context, Statement) -->
	ml_gen_info_get_globals(Globals),
	(
	%
	% Convert dense int switches into computed gotos,
	% unless the target prefers switches.
	%
		% is this an int switch?
		{ Stmt0 = switch(Type, Rval, Range, Cases, Default) },
		{ is_integral_type(Type) },

		% does the target want us to convert dense int
		% switches into computed gotos?
		{ target_supports_computed_goto(Globals) },
		\+ {
			target_supports_int_switch(Globals),
			globals__lookup_bool_option(Globals, prefer_switch, yes)
		},

		% is the switch big enough?
		{ list__length(Cases, NumCases) },
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSize) },
		{ NumCases >= DenseSize },

		% ... and dense enough?
		{ globals__lookup_int_option(Globals, dense_switch_req_density,
			ReqDensity) },
		{ is_dense_switch(Cases, ReqDensity) }
	->
		{ maybe_eliminate_default(Range, Cases, Default, ReqDensity,
			FirstVal, LastVal, NeedRangeCheck) },
		generate_dense_switch(Cases, Default,
			FirstVal, LastVal, NeedRangeCheck,
			Type, Rval, MLDS_Context,
			MLDS_Decls, MLDS_Statements),
		{ Stmt = block(MLDS_Decls, MLDS_Statements) },
		{ Statement = mlds__statement(Stmt, MLDS_Context) }
	;
	%
	% Convert the remaining (sparse) int switches into if-then-else chains,
	% unless the target prefers switches.
	%
		{ Stmt0 = switch(Type, Rval, _Range, Cases, Default) },
		{ is_integral_type(Type) },
		\+ {
			target_supports_int_switch(Globals),
			globals__lookup_bool_option(Globals, prefer_switch, yes)
		}
	->
		{ Statement = ml_switch_to_if_else_chain(Cases, Default, Rval,
			MLDS_Context) }
	;
	%
	% Optimize away trivial switches
	% (these can occur e.g. with --tags none, where the
	% primary tag test always has only one reachable case)
	%
		{ Stmt0 = switch(_Type, _Rval, _Range, Cases, Default) },
		{ Cases = [SingleCase] },
		{ Default = default_is_unreachable }
	->
		{ SingleCase = _MatchCondition - CaseStatement },
		{ Statement = CaseStatement }
	;
		{ Stmt = Stmt0 },
		{ Statement = mlds__statement(Stmt, MLDS_Context) }
	).

:- pred is_integral_type(mlds__type::in) is semidet.
is_integral_type(mlds__native_int_type).
is_integral_type(mlds__native_char_type).
is_integral_type(mlds__mercury_type(_, int_type, _)).
is_integral_type(mlds__mercury_type(_, char_type, _)).
is_integral_type(mlds__mercury_type(_, enum_type, _)).

:- pred is_dense_switch(list(mlds__switch_case)::in, int::in) is semidet.
is_dense_switch(Cases, ReqDensity) :-
	% Need at least two cases
	NumCases = list__length(Cases),
	NumCases > 2,

	% The switch needs to be dense enough
	find_first_and_last_case(Cases, FirstCaseVal, LastCaseVal),
	CasesRange = LastCaseVal - FirstCaseVal + 1,
	Density = calc_density(NumCases, CasesRange),
	Density > ReqDensity.

	% For switches with a default, we normally need to check that
	% the variable is in range before we index into the jump table.
	% However, if the range of the type is sufficiently small,
	% we can make the jump table large enough to hold all
	% of the values for the type.
:- pred maybe_eliminate_default(mlds__switch_range::in,
		list(mlds__switch_case)::in, mlds__switch_default::in, int::in,
		int::out, int::out, bool::out) is det.

maybe_eliminate_default(Range, Cases, Default, ReqDensity,
		FirstVal, LastVal, NeedRangeCheck) :-
	(
		Default \= default_is_unreachable,
		Range = range(Min, Max),
		TypeRange = Max - Min + 1,
		NumCases = list__length(Cases),
		NoDefaultDensity = calc_density(NumCases, TypeRange),
		NoDefaultDensity > ReqDensity
	->
		NeedRangeCheck = no,
		FirstVal = Min,
		LastVal = Max
	;
		( Default = default_is_unreachable ->
			NeedRangeCheck = no
		;
			NeedRangeCheck = yes
		),
		find_first_and_last_case(Cases, FirstCaseVal, LastCaseVal),
		FirstVal = FirstCaseVal,
		LastVal = LastCaseVal
	).

	% Calculate the percentage density given the range
	% and the number of cases.

:- func calc_density(int, int) = int.
calc_density(NumCases, Range) = Density :-
	Density is (NumCases * 100) // Range.

%-----------------------------------------------------------------------------%

% Find the highest and lowest case values in a list of cases.

:- pred find_first_and_last_case(list(mlds__switch_case)::in,
		int::out, int::out) is det.

find_first_and_last_case(Cases, Min, Max) :-
	list__foldl2(find_first_and_last_case_2, Cases, 0, Min, 0, Max).

:- pred find_first_and_last_case_2(mlds__switch_case::in,
		int::in, int::out, int::in, int::out) is det.

find_first_and_last_case_2(Case, Min0, Min, Max0, Max) :-
	Case = CaseConds - _CaseStatement,
	list__foldl2(find_first_and_last_case_3, CaseConds, 
		Min0, Min, Max0, Max).

:- pred find_first_and_last_case_3(mlds__case_match_cond::in,
		int::in, int::out, int::in, int::out) is det.

find_first_and_last_case_3(match_value(Rval), Min0, Min, Max0, Max) :-
	(
		Rval = const(int_const(Val))
	->
		int__min(Min0, Val, Min),
		int__max(Max0, Val, Max)
	;
		error("find_first_and_last_case_3: non-int case")
	).
find_first_and_last_case_3(match_range(MinRval, MaxRval),
		Min0, Min, Max0, Max) :-
	(
		MinRval = const(int_const(Min1)),
		MaxRval = const(int_const(Max1))
	->
		int__min(Min0, Min1, Min),
		int__max(Max0, Max1, Max)
	;
		error("find_first_and_last_case_3: non-int case")
	).

%-----------------------------------------------------------------------------%

	% Generate code for a switch using a dense jump table.

:- pred generate_dense_switch(list(mlds__switch_case)::in, 
		mlds__switch_default::in, int::in, int::in, bool::in,
		mlds__type::in, mlds__rval::in, mlds__context::in,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

generate_dense_switch(Cases, Default, FirstVal, LastVal, NeedRangeCheck,
		_Type, Rval, MLDS_Context, MLDS_Decls, MLDS_Statements) -->
	%
	% If the case values start at some number other than 0,
	% then subtract that number to give us a zero-based index
	%
	{ FirstVal = 0 ->
		Index = Rval
	;
		Index = binop(-, Rval, const(int_const(FirstVal)))
	},

	%
	% Now generate the jump table
	%
	ml_gen_new_label(EndLabel),
	{ map__init(CaseLabelsMap0) },
	generate_cases(Cases, EndLabel, CaseLabelsMap0,
		CaseLabelsMap, CasesDecls, CasesCode),
	ml_gen_new_label(DefaultLabel),
	{ CaseLabels = get_case_labels(FirstVal, LastVal,
		CaseLabelsMap, DefaultLabel) },
	{ DefaultLabelStatement = mlds__statement(
		label(DefaultLabel),
		MLDS_Context) },
	(
		{ Default = default_is_unreachable },
		% we still need the label, in case we inserted
		% references to it into (unreachable) slots in the
		% jump table
		{ DefaultStatements = [DefaultLabelStatement] }
	;
		{ Default = default_do_nothing },
		{ DefaultStatements = [DefaultLabelStatement] }
	;
		{ Default = default_case(DefaultCase) },
		{ DefaultStatements = [DefaultLabelStatement, DefaultCase] }
	),

	{ StartComment = mlds__statement(
		atomic(comment("switch (using dense jump table)")),
		MLDS_Context) },
	{ DoJump = mlds__statement(
		computed_goto(Index, CaseLabels),
		MLDS_Context) },
	{ EndLabelStatement = mlds__statement(
		label(EndLabel),
		MLDS_Context) },
	{ EndComment = mlds__statement(
		atomic(comment("End of dense switch")),
		MLDS_Context) },

	% We may need to check that the value of the variable lies within the
	% appropriate range
	(
		{ NeedRangeCheck = yes }
	->
		{ Difference is LastVal - FirstVal },
		{ InRange = binop(unsigned_le, Index,
				const(int_const(Difference))) },
		{ Else = yes(mlds__statement(
			block([], DefaultStatements),
			MLDS_Context)) },
		{ SwitchBody = mlds__statement(
			block([], [DoJump | CasesCode]),
			MLDS_Context) },
		{ DoSwitch = mlds__statement(
			if_then_else(InRange, SwitchBody, Else),
			MLDS_Context) },
		{ MLDS_Statements = [StartComment, DoSwitch] ++
			[EndLabelStatement, EndComment] }
	;
		{ MLDS_Statements = [StartComment, DoJump | CasesCode] ++
			DefaultStatements ++
			[EndLabelStatement, EndComment] }
	),
	{ MLDS_Decls = CasesDecls }.

:- pred generate_cases(list(mlds__switch_case)::in, mlds__label::in,
		case_labels_map::in, case_labels_map::out,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

generate_cases([], _EndLabel, CaseLabelsMap, CaseLabelsMap, [], []) --> [].
generate_cases([Case | Cases], EndLabel, CaseLabelsMap0,
		CaseLabelsMap, MLDS_Decls, MLDS_Statements) -->
	generate_case(Case, EndLabel, CaseLabelsMap0, CaseLabelsMap1,
		CaseDecls, CaseStatements),
	generate_cases(Cases, EndLabel,
		CaseLabelsMap1, CaseLabelsMap,
		MLDS_Decls1, MLDS_Statements1),
	{ MLDS_Decls = CaseDecls ++ MLDS_Decls1 },
	{ MLDS_Statements = CaseStatements ++ MLDS_Statements1 }.

:- pred generate_case(mlds__switch_case::in, mlds__label::in,
		case_labels_map::in, case_labels_map::out,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

% This converts an MLDS switch case into code for a dense switch case,
% by adding a label at the front and a `goto <EndLabel>' at the end.
% It also inserts the label for this case into the CaseLabelsMap.

generate_case(Case, EndLabel, CaseLabelsMap0, CaseLabelsMap,
		MLDS_Decls, MLDS_Statements) -->
	{ Case = MatchCondition - CaseStatement },
	ml_gen_new_label(ThisLabel),
	{ insert_cases_into_map(MatchCondition, ThisLabel,
		CaseLabelsMap0, CaseLabelsMap) },
	{ CaseStatement = mlds__statement(_, MLDS_Context) },
	{ LabelComment = mlds__statement(
		atomic(comment("case of dense switch")),
		MLDS_Context) },
	{ LabelCode = mlds__statement(
		label(ThisLabel),
		MLDS_Context) },
	{ JumpComment = mlds__statement(
		atomic(comment("branch to end of dense switch")),
		MLDS_Context) },
	{ JumpCode = mlds__statement(
		goto(EndLabel),
		MLDS_Context) },
	{ MLDS_Decls = [] },
	{ MLDS_Statements = [LabelComment, LabelCode, CaseStatement,
		JumpComment, JumpCode] }.

%-----------------------------------------------------------------------------%

%
% We build up a map which records which label should be used for
% each case value.
%
:- type case_labels_map == map(int, mlds__label).

:- pred insert_cases_into_map(mlds__case_match_conds::in, mlds__label::in,
		case_labels_map::in, case_labels_map::out) is det.

insert_cases_into_map([], _ThisLabel, CaseLabelsMap, CaseLabelsMap).
insert_cases_into_map([Cond|Conds], ThisLabel, CaseLabelsMap0, CaseLabelsMap) :-
	insert_case_into_map(Cond, ThisLabel, CaseLabelsMap0, CaseLabelsMap1),
	insert_cases_into_map(Conds, ThisLabel, CaseLabelsMap1, CaseLabelsMap).

:- pred insert_case_into_map(mlds__case_match_cond::in, mlds__label::in,
		case_labels_map::in, case_labels_map::out) is det.

insert_case_into_map(match_value(Rval), ThisLabel,
		CaseLabelsMap0, CaseLabelsMap) :-
	( Rval = const(int_const(Val)) ->
		map__det_insert(CaseLabelsMap0, Val, ThisLabel, CaseLabelsMap)
	;
		error("insert_case_into_map: non-int case")
	).
insert_case_into_map(match_range(MinRval, MaxRval), ThisLabel,
		CaseLabelsMap0, CaseLabelsMap) :-
	(
		MinRval = const(int_const(Min)),
		MaxRval = const(int_const(Max))
	->
		insert_range_into_map(Min, Max, ThisLabel,
			CaseLabelsMap0, CaseLabelsMap)
	;
		error("insert_case_into_map: non-int case")
	).

:- pred insert_range_into_map(int::in, int::in, mlds__label::in,
		case_labels_map::in, case_labels_map::out) is det.

insert_range_into_map(Min, Max, ThisLabel, CaseLabelsMap0, CaseLabelsMap) :-
	( Min > Max ->
		CaseLabelsMap = CaseLabelsMap0
	;
		map__det_insert(CaseLabelsMap0, Min, ThisLabel,
			CaseLabelsMap1),
		insert_range_into_map(Min + 1, Max, ThisLabel,
			CaseLabelsMap1, CaseLabelsMap)
	).

%-----------------------------------------------------------------------------%

% Given the starting and ending case values, the mapping from case values
% to labels, and the default label to use for case values which aren't in
% the map, this function returns the list of labels to use for the case
% values.

:- func get_case_labels(int, int, map(int, mlds__label), mlds__label) =
		list(mlds__label).

get_case_labels(ThisVal, LastVal, CaseLabelsMap, DefaultLabel) = CaseLabels :-
	( ThisVal > LastVal ->
		CaseLabels = []
	;
		( map__search(CaseLabelsMap, ThisVal, CaseLabel0) ->
			CaseLabel = CaseLabel0
		;
			CaseLabel = DefaultLabel
		),
		CaseLabels1 = get_case_labels(ThisVal + 1, LastVal,
			CaseLabelsMap, DefaultLabel),
		CaseLabels = [CaseLabel | CaseLabels1]
	).

%-----------------------------------------------------------------------------%

	% Convert an int switch to a chain of if-then-elses
	% that test each case in turn.
	%
:- func ml_switch_to_if_else_chain(mlds__switch_cases, mlds__switch_default,
		mlds__rval, mlds__context) = mlds__statement.
ml_switch_to_if_else_chain([], Default, _Rval, MLDS_Context) =
		MLDS_Statement :-
	(
		Default = default_do_nothing,
		MLDS_Statement = mlds__statement(block([],[]), MLDS_Context)
	;
		Default = default_is_unreachable,
		MLDS_Statement = mlds__statement(block([],[]), MLDS_Context)
	;
		Default = default_case(MLDS_Statement)
	).
ml_switch_to_if_else_chain([Case | Cases], Default, SwitchRval, MLDS_Context) =
		MLDS_Statement :-
	Case = MatchConditions - CaseStatement,
	(
		Cases = [], Default = default_is_unreachable
	->
		MLDS_Statement = CaseStatement
	;
		CaseMatchedRval = ml_gen_case_match_conds(MatchConditions,
			SwitchRval),
		RestStatement = ml_switch_to_if_else_chain(Cases, Default,
			SwitchRval, MLDS_Context),
		IfStmt = if_then_else(CaseMatchedRval,
			CaseStatement, yes(RestStatement)),
		MLDS_Statement = mlds__statement(IfStmt, MLDS_Context)
	).

	% Generate an rval which will be true iff any of the specified
	% list of case conditions matches the specified rval
	% (which must have integral type).
:- func ml_gen_case_match_conds(mlds__case_match_conds, rval) = rval.
ml_gen_case_match_conds([], _) = const(false).
ml_gen_case_match_conds([Cond], SwitchRval) =
	ml_gen_case_match_cond(Cond, SwitchRval).
ml_gen_case_match_conds([Cond1, Cond2 | Conds], SwitchRval) =
	binop(or,
		ml_gen_case_match_cond(Cond1, SwitchRval),
		ml_gen_case_match_conds([Cond2 | Conds], SwitchRval)).

	% Generate an rval which will be true iff the specified
	% case condition matches the specified rval
	% (which must have integral type).
:- func ml_gen_case_match_cond(mlds__case_match_cond, rval) = rval.
ml_gen_case_match_cond(match_value(CaseRval), SwitchRval) =
	binop(eq, CaseRval, SwitchRval).
ml_gen_case_match_cond(match_range(MinRval, MaxRval), SwitchRval) =
	binop(and, binop(>=, SwitchRval, MinRval),
		   binop(<=, SwitchRval, MaxRval)).

%-----------------------------------------------------------------------------%
