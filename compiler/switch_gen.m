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
:- implementation.

:- import_module unify_gen.

:- import_module tree, list, map, std_util, require.

	% To generate a switch, first we flush the
	% variable on whose tag we are going to switch, then we
	% generate the cases for the switch.

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

switch_gen__generate_switch(Det, CaseVar, LocalDet, Cases, Instr) -->
	code_info__produce_variable(CaseVar, VarCode, _Lval),
	code_info__get_next_label(EndLabel),
	switch_gen__generate_cases(Cases, CaseVar, Det, LocalDet, 
						EndLabel, CasesCode),
	{ Instr = tree(VarCode, CasesCode) },
	code_info__remake_with_store_map.

:- pred switch_gen__generate_cases(list(case), var, category, category, label,
					code_tree, code_info, code_info).
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
switch_gen__generate_cases([case(Cons, Goal)|Cases], Var, Det, LocalDet, 
					EndLabel, CasesCode) -->
	(
		{ Cases = [_|_] ; LocalDet = semideterministic }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
		unify_gen__generate_tag_rval(Var, Cons, Rval, FlushCode),
		code_info__generate_test_and_fail(Rval, BranchCode),
		code_info__pop_failure_cont,
		{ TestCode = tree(FlushCode, BranchCode) },
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ElseLabel = node([
			goto(label(EndLabel)) -
				"skip to the end of the nondet switch",
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
