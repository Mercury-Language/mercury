%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module switch_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred switch_gen__generate_switch(category, var, category, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_switch(in, in, in, in, out, in, out) is det.

%	switch_gen__generate_switch(Det, Var, LocalDet, Cases, Code):
%		Generate code for a switch statement.
%		Det is the determinism of the context.
%		LocalDet is the determinism of the switch itself,
%		ignoring the determism of the goals in the cases.
%		I.e. LocalDet is `det' if the switch covers all cases and
%		`semidet' otherwise.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, unify_gen.

:- import_module tree, list, map, std_util, require.

:- type extended_case ---> case(int, cons_tag, cons_id, hlds__goal).

%---------------------------------------------------------------------------%

	% To generate a switch, first we flush the
	% variable on whose tag we are going to switch, then we
	% generate the cases for the switch.

switch_gen__generate_switch(Det, CaseVar, LocalDet, Cases, Code) -->
	code_info__get_next_label(EndLabel),
	switch_gen__lookup_tags(Cases, CaseVar, TaggedCases0),
	{ list__sort(TaggedCases0, TaggedCases) },
	(
		{ switch_gen__is_dense_switch(TaggedCases, FirstVal) },
		{ list__length(TaggedCases, NumCases) },
		{ NumCases > 2 }	% heuristic
	->
		switch_gen__generate_dense_switch(TaggedCases, FirstVal,
				CaseVar, Det, LocalDet, EndLabel, Code)
	;
		code_info__produce_variable(CaseVar, VarCode, _Lval),
		switch_gen__generate_cases(TaggedCases,
				CaseVar, Det, LocalDet, EndLabel, CasesCode),
		{ Code = tree(VarCode, CasesCode) }
	),
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

:- pred switch_gen__lookup_tags(list(case), var, list(extended_case),
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

%---------------------------------------------------------------------------%

	% Can this switch be implemented as a dense jump table?

:- pred switch_gen__is_dense_switch(list(extended_case), int).
:- mode switch_gen__is_dense_switch(in, out) is semidet.

switch_gen__is_dense_switch([Case | Cases], StartingVal) :-
	Case = case(_, int_constant(StartingVal), _, _),
	switch_gen__is_dense_switch_2(Cases, StartingVal).

:- pred switch_gen__is_dense_switch_2(list(extended_case), int).
:- mode switch_gen__is_dense_switch_2(in, in) is semidet.

switch_gen__is_dense_switch_2([], _).
switch_gen__is_dense_switch_2([Case | Cases], PrevVal) :-
	Val is PrevVal + 1,
	Case = case(_, int_constant(Val), _, _),
	switch_gen__is_dense_switch_2(Cases, Val).

%---------------------------------------------------------------------------%

	% Generate code for a switch using a dense jump table 

:- pred switch_gen__generate_dense_switch(list(extended_case), int, var,
		category, category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_dense_switch(in, in, in, in, in, in,
		out, in, out) is det.

switch_gen__generate_dense_switch(Cases, StartVal, Var, Det, LocalDet, EndLabel,
		Code) -->
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
	( { LocalDet = semideterministic } ->
		{ list__length(Cases, NumCases) },
		code_info__generate_test_and_fail(
			binop(<, unop(cast_to_unsigned, Index),
				const(int_const(NumCases))), RangeCheck)
	;
		{ RangeCheck = empty }
	),
		% Now generate the jump table and the cases
	switch_gen__generate_dense_cases(Cases, Det, EndLabel,
			Labels, CasesCode),
	{ DoJump = node([
		computed_goto(Index, Labels)
			- "switch (using dense jump table)"
	]) },
		% Assemble to code together
	{ Code = tree(tree(VarCode, RangeCheck), tree(DoJump, CasesCode)) }.

:- pred switch_gen__generate_dense_cases(list(extended_case), category, label,
			list(label), code_tree, code_info, code_info).
:- mode switch_gen__generate_dense_cases(in, in, in, out, out, in, out) is det.

switch_gen__generate_dense_cases([], _Det, EndLabel, [], Code) -->
	{ Code = node([ label(EndLabel) - "End of dense switch" ]) }.

switch_gen__generate_dense_cases([case(_, _, _, Goal)|Cases], Det, EndLabel,
			[ThisLabel | Labels], Code) -->
	% If there are more cases, the we need to save
	% the expression cache, etc., and restore them when we've finished
	(
		{ Cases = [_|_] }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ThisLabel),
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ThisCase = tree(
			node([ label(ThisLabel)
				- "case of dense switch" ]),
			tree(	ThisCode,
				node([ goto(label(EndLabel))
					- "branch to end of dense switch" ])
			)
		) },
		code_info__slap_code_info(CodeInfo)
	;
		code_info__get_next_label(ThisLabel),
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ThisCase = tree(
			node([ label(ThisLabel)
				- "last case of dense switch" ]),
			ThisCode
		) }
	),
		% generate the rest of the cases.
	switch_gen__generate_dense_cases(Cases, Det, EndLabel,
			Labels, CasesCode),
	{ Code = tree(ThisCase, CasesCode) }.


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

:- pred switch_gen__generate_cases(list(extended_case), var, category,
			category, label, code_tree, code_info, code_info).
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
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
		unify_gen__generate_tag_test(Var, Cons, TestCode),
		code_info__pop_failure_cont,
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ElseLabel = node([
			goto(label(EndLabel)) -
				"skip to the end of the switch",
			label(ElseLab) - "next case"
		]) },
		{ ThisCaseCode = tree(tree(TestCode, ThisCode), ElseLabel) },
		% If there are more cases, the we need to restore
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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
