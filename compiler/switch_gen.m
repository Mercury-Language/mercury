%---------------------------------------------------------------------------%
%
% File: switch_gen.nl
% Authors: conway, fjh
%
% This module handles the generation of code for switches, which are
% disjunctions that do not require backtracking.  Switches are detected
% in switch_detection.nl.  This is the module that determines what
% sort of indexing to use for each switch and then actually generates the
% code.
%
% Currently the following forms of indexing are used:
%
%	For switches on atomic data types (int, char, enums),
%	if the cases are not sparse, we use the value of the switch variable
%	to index into a jump table.
%
%	For switches on discriminated union types, we generate a chain of
%	if-then-elses on the primary tags but then use the secondary tag
%	to index into a jump table if the table is big enough.
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

:- import_module hlds, code_info.

:- pred switch_gen__generate_switch(category, var, category, list(case),
	code_tree, code_info, code_info).
:- mode switch_gen__generate_switch(in, in, in, in, out, in, out) is det.

% These types are exported to dense_switch, string_switch and tag_switch.

:- type extended_case ---> case(int, cons_tag, cons_id, hlds__goal).
:- type cases_list == list(extended_case).

%	switch_gen__generate_switch(Det, Var, LocalDet, Cases, Code):
%		Generate code for a switch statement.
%		Det is the determinism of the context.
%		LocalDet is the determinism of the switch itself,
%		ignoring the determism of the goals in the cases.
%		I.e. LocalDet is `det' if the switch covers all cases and
%		`semidet' otherwise.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module dense_switch, string_switch, tag_switch.
:- import_module int, string, list, map, tree, std_util, require.
:- import_module llds, code_gen, unify_gen, type_util, code_util.
:- import_module globals, options.

:- type switch_category
	--->	atomic_switch
	;	string_switch
	;	tag_switch
	;	other_switch.

%---------------------------------------------------------------------------%

	% Choose which method to use to generate the switch.

switch_gen__generate_switch(Det, CaseVar, LocalDet, Cases, Code) -->
	switch_gen__determine_category(CaseVar, SwitchCategory),
	code_info__get_next_label(EndLabel),
	switch_gen__lookup_tags(Cases, CaseVar, TaggedCases0),
	{ list__sort(TaggedCases0, TaggedCases) },
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, smart_indexing,
		Indexing) },
	(
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, dense_switch_size,
			DenseSize) },
		{ NumCases > DenseSize },
		{ globals__lookup_int_option(Globals, req_density,
			ReqDensity) },
		dense_switch__is_dense_switch(CaseVar, TaggedCases, LocalDet,
			ReqDensity, FirstVal, LastVal, LocalDet1)
	->
		dense_switch__generate(TaggedCases,
			FirstVal, LastVal, CaseVar, Det, LocalDet1,
			EndLabel, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = string_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, string_switch_size,
			StringSize) },
		{ NumCases > StringSize }
	->
		string_switch__generate(TaggedCases, CaseVar, Det,
			LocalDet, EndLabel, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = tag_switch },
		{ list__length(TaggedCases, NumCases) },
		{ globals__lookup_int_option(Globals, tag_switch_size,
			TagSize) },
		{ NumCases > TagSize }
	->
		tag_switch__generate(TaggedCases,
			CaseVar, Det, LocalDet, EndLabel, Code)
	;
		% To generate a switch, first we flush the
		% variable on whose tag we are going to switch, then we
		% generate the cases for the switch.

		switch_gen__generate_all_cases(TaggedCases,
			CaseVar, Det, LocalDet, EndLabel, Code)
	),
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

	% We categorize switches according to whether the value
	% being switched on is an atomic type, a string, or
	% something more complicated.

:- pred switch_gen__determine_category(var, switch_category,
	code_info, code_info).
:- mode switch_gen__determine_category(in, out, in, out) is det.

switch_gen__determine_category(CaseVar, SwitchCategory) -->
	code_info__variable_type(CaseVar, Type),
	code_info__get_module_info(ModuleInfo),
	{ classify_type(Type, ModuleInfo, TypeCategory) },
	{ switch_gen__type_cat_to_switch_cat(TypeCategory, SwitchCategory) }.

:- pred switch_gen__type_cat_to_switch_cat(builtin_type, switch_category).
:- mode switch_gen__type_cat_to_switch_cat(in, out) is det.

switch_gen__type_cat_to_switch_cat(enum_type, atomic_switch).
switch_gen__type_cat_to_switch_cat(int_type,  atomic_switch).
switch_gen__type_cat_to_switch_cat(char_type, atomic_switch).
switch_gen__type_cat_to_switch_cat(str_type,  string_switch).
switch_gen__type_cat_to_switch_cat(pred_type, other_switch).
switch_gen__type_cat_to_switch_cat(user_type(_), tag_switch).
switch_gen__type_cat_to_switch_cat(polymorphic_type, other_switch).

%---------------------------------------------------------------------------%

:- pred switch_gen__lookup_tags(list(case), var, cases_list,
				code_info, code_info).
:- mode switch_gen__lookup_tags(in, in, out, in, out) is det.

switch_gen__lookup_tags([], _, []) --> [].
switch_gen__lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, Goal) },
	code_info__cons_id_to_tag(Var, ConsId, Tag),
	{ switch_gen__priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
	switch_gen__lookup_tags(Cases, Var, TaggedCases).

%---------------------------------------------------------------------------%

:- pred switch_gen__priority(cons_tag, int).
:- mode switch_gen__priority(in, out) is det.

	% prioritize tag tests - the most efficient ones first.

switch_gen__priority(int_constant(_), 1).
switch_gen__priority(complicated_constant_tag(_, _), 1).
switch_gen__priority(simple_tag(_), 2).
switch_gen__priority(float_constant(_), 3).
switch_gen__priority(complicated_tag(_, _), 4).
switch_gen__priority(string_constant(_), 5).
switch_gen__priority(pred_closure_tag(_, _), 6).
switch_gen__priority(address_constant(_, _), 6).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Generate a switch as a chain of if-then-elses.
	%
	% To generate a case for a switch we generate
	% code to do a tag-test and fall through to the next case in
	% the event of failure.
	%
	% Each case except the last consists of a tag test, followed by
	% the goal for that case, followed by a branch to the end of
	% the switch. The goal is generated as a "forced" goal which
	% ensures that all variables which are live at the end of the
	% case get stored in their stack slot.  For the last case, if
	% the switch is locally deterministic, then we don't need to
	% generate the tag test, and we never need to generate the
	% branch to the end of the switch.  After the last case, we put
	% the end-of-switch label which other cases branch to after
	% their case goals.

:- pred switch_gen__generate_all_cases(list(extended_case), var,
	category, category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_all_cases(in, in, in, in, in, out, in, out) is det.

switch_gen__generate_all_cases(Cases, Var, Det, LocalDet, EndLabel, Code) -->
	code_info__produce_variable(Var, VarCode, _Rval),
	(
		{ Det = deterministic },
		{ LocalDet = deterministic },
		{ Cases = [Case1, Case2] }
	->
		{ Case1 = case(_, _, Cons1, Goal1) },
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
		unify_gen__generate_tag_test(Var, Cons1, TestCode),
		code_info__pop_failure_cont,
		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_goal(Det, Goal1, Case1Code),

		{ Case2 = case(_, _, _Cons2, Goal2) },
		code_info__slap_code_info(CodeInfo),
		code_gen__generate_forced_goal(Det, Goal2, Case2Code),

		{ tree__flatten(TestCode, TestListList) },
		{ list__condense(TestListList, TestList) },
		{ code_util__negate_the_test(TestList, RealTestList) },
		{ Code  = tree(
			tree(
				VarCode,
				tree(
					node(RealTestList),
					Case2Code)),
			tree(
				node([
					goto(label(EndLabel)) -
						"skip to the end of the switch",
					label(ElseLab) - "next case" ]),
				tree(
					Case1Code,
					node([ label(EndLabel) -
						"End of switch" ]))))
		}
	;
		switch_gen__generate_cases(Cases, Var, Det, LocalDet,
			EndLabel, CasesCode),
		{ Code = tree(VarCode, CasesCode) }
	).

:- pred switch_gen__generate_cases(list(extended_case), var,
	category, category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_cases(in, in, in, in, in, out, in, out) is det.

	% At the end of a locally semidet switch, we fail because we
	% came across a tag which was not covered by one of the cases.
	% It is followed by the end of switch label to which the cases
	% branch.
switch_gen__generate_cases([], _Var, _Det, LocalDet, EndLabel, Code) -->
	( { LocalDet = semideterministic } ->
		code_info__generate_failure(FailCode)
	;
		{ FailCode = empty }
	),
	{ Code = tree(FailCode, node([ label(EndLabel) -
		"End of switch" ])) }.

	% A case consists of a tag-test followed by a 
	% goal and a label for the start of the next case.
switch_gen__generate_cases([case(_, _, Cons, Goal)|Cases], Var, Det, LocalDet, 
					EndLabel, CasesCode) -->
	(
		{ Cases = [_|_] ; LocalDet = semideterministic }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		code_info__push_failure_cont(known(ElseLabel)),
		unify_gen__generate_tag_test(Var, Cons, TestCode),
		code_info__pop_failure_cont,
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ElseCode = node([
			goto(label(EndLabel)) -
				"skip to the end of the switch",
			label(ElseLabel) - "next case"
		]) },
		{ ThisCaseCode = tree(tree(TestCode, ThisCode), ElseCode) },
		% If there are more cases, then we need to restore
		% the expression cache, etc.
		( { Cases = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		code_gen__generate_forced_goal(Det, Goal, ThisCaseCode)
	),
		% generate the rest of the cases.
	switch_gen__generate_cases(Cases, Var, Det, LocalDet, EndLabel,
		CasesCode0),
	{ CasesCode = tree(ThisCaseCode, CasesCode0) }.
