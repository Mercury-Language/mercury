%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% dense_switch.m

% For switches on atomic types, generate code using a dense jump table.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module dense_switch.

:- interface.

:- import_module llds, switch_gen, code_info, type_util.
:- import_module list.

	% Should this switch be implemented as a dense jump table?
	% If so, we return the starting and ending values for the table,
	% and whether the switch is not covers all cases or not
	% (we may convert locally semidet switches into locally det
	% switches by adding extra cases whose body is just `fail').

:- pred dense_switch__is_dense_switch(var, cases_list, can_fail, int,
	int, int, can_fail, code_info, code_info).
:- mode dense_switch__is_dense_switch(in, in, in, in, out, out, out, in, out)
	is semidet.

	% Generate code for a switch using a dense jump table.

:- pred dense_switch__generate(cases_list, int, int,
	var, code_model, can_fail, label, code_tree, code_info, code_info).
:- mode dense_switch__generate(in, in, in, in, in, in, in,
	out, in, out) is det.

	% also used by lookup_switch
:- pred dense_switch__calc_density(int, int, int).
:- mode dense_switch__calc_density(in, in, out) is det.

	% also used by lookup_switch
:- pred dense_switch__type_range(builtin_type, type, int, code_info, code_info).
:- mode dense_switch__type_range(in, in, out, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_goal, hlds_data, code_gen.
:- import_module map, tree, int, std_util, require.

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
			{ dense_switch__calc_density(NumCases, TypeRange, DetDensity) },
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

	% XXX the size of `character' is hard-coded here.

dense_switch__type_range(char_type, _, 128) --> [].
dense_switch__type_range(enum_type, Type, TypeRange) -->
	{ type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("dense_switch__type_range: invalid enum type?")
	},
	code_info__get_module_info(ModuleInfo),
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{
		TypeDefn = hlds__type_defn(_, _,
			du_type(_, ConsTable, _), _, _)
	->
		map__count(ConsTable, TypeRange)
	;
		error("dense_switch__type_range: enum type is not d.u. type?")
	}.

%---------------------------------------------------------------------------%

dense_switch__generate(Cases, StartVal, EndVal, Var, CodeModel, CanFail,
		EndLabel, Code) -->
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
		code_info__generate_test_and_fail(
			binop(<=, unop(cast_to_unsigned, Index),
				const(int_const(Difference))), RangeCheck)
	;
		{ CanFail = cannot_fail },
		{ RangeCheck = empty }
	),
		% Now generate the jump table and the cases
	dense_switch__generate_cases(Cases, StartVal, EndVal, CodeModel, EndLabel,
			Labels, CasesCode, no, MLiveness),
		% We keep track of what variables are supposed to be
		% live at the end of cases. We have to do this explicitly
		% because generating a `fail' slot last would yield the
		% wrong liveness.
	(
		{ MLiveness = yes(Liveness) }
	->
		code_info__set_liveness_info(Liveness)
	;
		{ error("dense_switch__generate: no liveness!") }
	),
	{ DoJump = node([
		computed_goto(Index, Labels)
			- "switch (using dense jump table)"
	]) },
		% Assemble to code together
	{ Code = tree(tree(VarCode, RangeCheck), tree(DoJump, CasesCode)) },
	code_info__remake_with_store_map.

:- pred dense_switch__generate_cases(cases_list, int, int,
	code_model, label, list(label), code_tree, 
	maybe(liveness_info), maybe(liveness_info), code_info, code_info).
:- mode dense_switch__generate_cases(in, in, in, in, in, out, out,
	in, out, in, out) is det.

dense_switch__generate_cases(Cases0, NextVal, EndVal, CodeModel, EndLabel,
		Labels, Code, Liveness0, Liveness) -->
	(
		{ NextVal > EndVal }
	->
		{ Code = node([ label(EndLabel) - "End of dense switch" ]) },
		{ Labels = [] },
		{ Liveness = Liveness0 }
	;
		code_info__get_next_label(ThisLabel),
		dense_switch__generate_case(Cases0, NextVal, CodeModel,
					Cases1, ThisCode, Comment, NewLiveness),
		{ ThisCaseCode = tree(
			node([ label(ThisLabel) - Comment ]),
			tree(	ThisCode,
				node([ goto(label(EndLabel))
					- "branch to end of dense switch" ])
			)
		) },
		{ dense_switch__merge_maybe_liveness(Liveness0, NewLiveness,
				Liveness1) },
			% generate the rest of the cases.
		{ NextVal1 is NextVal + 1 },
		dense_switch__generate_cases(Cases1, NextVal1, EndVal,
			CodeModel, EndLabel, Labels1, OtherCasesCode,
			Liveness1, Liveness),
		{ Labels = [ThisLabel | Labels1] },
		{ Code = tree(ThisCaseCode, OtherCasesCode) }
	).

%---------------------------------------------------------------------------%

:- pred dense_switch__generate_case(cases_list, int, code_model,
		cases_list, code_tree, string,
			maybe(liveness_info), code_info, code_info).
:- mode dense_switch__generate_case(in, in, in, out, out, out, out, in, out)
	is det.

dense_switch__generate_case(Cases0, NextVal, CodeModel, Cases, Code, Comment,
		ML) -->
	(
		{ Cases0 = [Case | Cases1] },
		{ Case = case(_, int_constant(NextVal), _, Goal) }
	->
		% We need to save the expression cache, etc.,
		% and restore them when we've finished
		{ Comment = "case of dense switch" },
		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_goal(CodeModel, Goal, Code),
		code_info__get_liveness_info(L),
		code_info__slap_code_info(CodeInfo),
		{ ML = yes(L) },
		{ Cases = Cases1 }
	;
		% This case didn't occur in the original case list - just
		% generate a `fail' for it.
		{ Comment = "compiler-introduced `fail' case of dense switch" },
		code_info__generate_failure(Code),
		{ Cases = Cases0 },
		{ ML = no }
	).

:- pred dense_switch__merge_maybe_liveness(maybe(liveness_info),
				maybe(liveness_info), maybe(liveness_info)).
:- mode dense_switch__merge_maybe_liveness(in, in, out) is det.

dense_switch__merge_maybe_liveness(L0, L1, L) :-
	(
		L0 = no
	->
		L = L1
	;
		L = L0
	).

