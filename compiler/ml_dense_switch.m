%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: ml_dense_switch.m
% author: fjh

% For switches on atomic types, generate code using a computed_goto
% dense jump table.

% The code here is quite similar to the code in dense_switch.m.

%-----------------------------------------------------------------------------%

:- module ml_dense_switch.

:- interface.

:- import_module prog_data.
:- import_module hlds_data, type_util.
:- import_module mlds, ml_switch_gen, ml_code_util.
:- import_module llds. % XXX for code_model

	% Should this switch be implemented as a dense jump table?
	% If so, we return the starting and ending values for the table,
	% and whether the switch is not covers all cases or not
	% (we may convert locally semidet switches into locally det
	% switches by adding extra cases whose body is just `fail').

:- pred ml_dense_switch__is_dense_switch(prog_var::in, ml_cases_list::in,
		can_fail::in, int::in, int::out, int::out, can_fail::out,
		ml_gen_info::in, ml_gen_info::out) is semidet.

	% Generate code for a switch using a dense jump table.

:- pred ml_dense_switch__generate(ml_cases_list::in, int::in, int::in,
		prog_var::in, code_model::in, can_fail::in,
		prog_context::in, mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

	% also used by ml_lookup_switch
:- pred ml_dense_switch__calc_density(int, int, int).
:- mode ml_dense_switch__calc_density(in, in, out) is det.

	% also used by ml_lookup_switch
:- pred ml_dense_switch__type_range(builtin_type, prog_type, int,
		ml_gen_info, ml_gen_info).
:- mode ml_dense_switch__type_range(in, in, out, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, ml_code_gen, builtin_ops.
:- import_module char, map, int, std_util, require, list.

ml_dense_switch__is_dense_switch(CaseVar, TaggedCases, CanFail0, ReqDensity,
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
		ml_dense_switch__calc_density(NumCases, Range, Density),
		Density > ReqDensity
	},
	( { CanFail0 = can_fail } ->
		% For semidet switches, we normally need to check that
		% the variable is in range before we index into the jump table.
		% However, if the range of the type is sufficiently small,
		% we can make the jump table large enough to hold all
		% of the values for the type.
		ml_variable_type(CaseVar, Type),
		=(MLGenInfo),
		{ ml_gen_info_get_module_info(MLGenInfo, ModuleInfo) },
		{ type_util__classify_type(Type, ModuleInfo, TypeCategory) },
		(
			ml_dense_switch__type_range(TypeCategory, Type,
				TypeRange),
			{ ml_dense_switch__calc_density(NumCases, TypeRange,
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

%-----------------------------------------------------------------------------%

	% Calculate the percentage density given the range
	% and the number of cases.

ml_dense_switch__calc_density(NumCases, Range, Density) :-
	N1 is NumCases * 100,
	Density is N1 // Range.

%-----------------------------------------------------------------------------%

	% Determine the range of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is to big to switch on (e.g. int).

ml_dense_switch__type_range(char_type, _, CharRange) -->
	% XXX the following code uses the host's character size,
	% not the target's, so it won't work if cross-compiling
	% to a machine with a different character size.
	% Note also that the code above in dense_switch.m and the code
	% in lookup_switch.m assume that char__min_char_value is 0.
	{ char__max_char_value(MaxChar) },
	{ char__min_char_value(MinChar) },
	{ CharRange is MaxChar - MinChar + 1 }.
ml_dense_switch__type_range(enum_type, Type, TypeRange) -->
	{ type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("dense_switch__type_range: invalid enum type?")
	},
	=(MLGenInfo),
	{ ml_gen_info_get_module_info(MLGenInfo, ModuleInfo) },
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
	{ TypeBody = du_type(_, ConsTable, _, _) ->
		map__count(ConsTable, TypeRange)
	;
		error("dense_switch__type_range: enum type is not d.u. type?")
	}.

%-----------------------------------------------------------------------------%

ml_dense_switch__generate(Cases, StartVal, EndVal, Var, CodeModel, CanFail,
		Context, MLDS_Decls, MLDS_Statements) -->
		% Evaluate the variable which we are going to be switching on
	ml_gen_var(Var, Lval),
	{ Rval = lval(Lval) },
		% If the case values start at some number other than 0,
		% then subtract that number to give us a zero-based index
	{ StartVal = 0 ->
		Index = Rval
	;
		Index = binop(-, Rval, const(int_const(StartVal)))
	},
		% Now generate the jump table and the cases
	ml_gen_new_label(EndLabel),
	ml_dense_switch__generate_cases(Cases, StartVal, EndVal, CodeModel,
			Context, EndLabel, CaseLabels, CasesDecls, CasesCode),

	{ Comment = mlds__statement(
		atomic(comment("switch (using dense jump table)")),
		mlds__make_context(Context)) },
	{ DoJump = mlds__statement(
		computed_goto(Index, CaseLabels),
		mlds__make_context(Context)) },
	{ EndLabelStatement = mlds__statement(label(EndLabel),
		mlds__make_context(Context)) },

		% If the switch is not locally deterministic, we need to
		% check that the value of the variable lies within the
		% appropriate range
	(
		{ CanFail = can_fail },
		{ Difference is EndVal - StartVal },
		{ InRange = binop(<=, unop(std_unop(cast_to_unsigned), Index),
				const(int_const(Difference))) },
		ml_gen_failure(CodeModel, Context, DoFailStatements),
		{ DoFailStatements = [] ->
			Else = no
		;
			Else = yes(ml_gen_block([], DoFailStatements, Context))
		},
		{ SwitchBody = ml_gen_block([], [DoJump | CasesCode],
			Context) },
		{ DoSwitch = mlds__statement(
			if_then_else(InRange, SwitchBody, Else),
			mlds__make_context(Context)) },
		{ MLDS_Statements = [Comment, DoSwitch] ++
			[EndLabelStatement] }
	;
		{ CanFail = cannot_fail },
		{ MLDS_Statements = [Comment, DoJump | CasesCode] ++
			[EndLabelStatement] }
	),
	{ MLDS_Decls = CasesDecls }.

:- pred ml_dense_switch__generate_cases(ml_cases_list::in, int::in, int::in,
		code_model::in, prog_context::in,
		mlds__label::in, list(mlds__label)::out,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_dense_switch__generate_cases(Cases0, NextVal, EndVal, CodeModel, Context,
		EndLabel, CaseLabels, MLDS_Decls, MLDS_Statements) -->
	(
		{ NextVal > EndVal }
	->
		{ EndComment = mlds__statement(
			atomic(comment("End of dense switch")),
			mlds__make_context(Context)) },
		{ CaseLabels = [] },
		{ MLDS_Decls = [] },
		{ MLDS_Statements = [EndComment] }
	;
		% generate the next case
		ml_dense_switch__generate_case(Cases0, NextVal,
			CodeModel, Context, EndLabel,
			Cases1, ThisLabel, CaseStatements),
		% generate the rest of the cases.
		ml_dense_switch__generate_cases(Cases1, NextVal + 1, EndVal,
			CodeModel, Context, EndLabel,
			CaseLabels1, MLDS_Decls, MLDS_Statements1),
		% package it all up together
		{ CaseLabels = [ThisLabel | CaseLabels1] },
		{ MLDS_Statements = list__append(CaseStatements,
			MLDS_Statements1) }
	).

%-----------------------------------------------------------------------------%

:- pred ml_dense_switch__generate_case(ml_cases_list::in, int::in,
		code_model::in, prog_context::in, mlds__label::in,
		ml_cases_list::out, mlds__label::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_dense_switch__generate_case(Cases0, NextVal, CodeModel, Context,
		EndLabel, Cases1, ThisLabel, MLDS_Statements) -->
	ml_gen_new_label(ThisLabel),
	{ LabelComment = mlds__statement(
		atomic(comment(Comment)),
		mlds__make_context(Context)) },
	{ LabelCode = mlds__statement(
		label(ThisLabel),
		mlds__make_context(Context)) },
	ml_dense_switch__generate_case_body(Cases0, NextVal, CodeModel,
		Context, Cases1, Comment, CaseStatement),
	{ JumpComment = mlds__statement(
		atomic(comment("branch to end of dense switch")),
		mlds__make_context(Context)) },
	{ JumpCode = mlds__statement(
		goto(EndLabel),
		mlds__make_context(Context)) },
	{ MLDS_Statements = [LabelComment, LabelCode, CaseStatement,
		JumpComment, JumpCode] }.
		
:- pred ml_dense_switch__generate_case_body(ml_cases_list::in, int::in,
		code_model::in, prog_context::in, ml_cases_list::out,
		string::out, mlds__statement::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_dense_switch__generate_case_body(Cases0, NextVal, CodeModel, Context,
		Cases, Comment, MLDS_Statement) -->
	(
		{ Cases0 = [Case | Cases1] },
		{ Case = case(_, int_constant(NextVal), _, Goal) }
	->
		{ Comment = "case of dense switch" },
		ml_gen_goal(CodeModel, Goal, MLDS_Statement),
		{ Cases = Cases1 }
	;
		% This case didn't occur in the original case list
		% - just generate a `fail' for it.
		{ Comment = "compiler-introduced `fail' case of dense switch" },
		ml_gen_failure(CodeModel, Context, FailStatements),
		{ MLDS_Statement = ml_gen_block([], FailStatements, Context) },
		{ Cases = Cases0 }
	).

%-----------------------------------------------------------------------------%
