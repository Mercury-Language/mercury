%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module switch_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred switch_gen__generate_det_switch(var, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_det_switch(in, in, out, in, out) is det.

:- pred switch_gen__generate_semi_switch(var, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_semi_switch(in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module unify_gen.

:- import_module tree, list, map, std_util, require.

	% To generate a deterministic switch, first we flush the
	% variable, on whoes tag we are going to switch, then we
	% generate the cases for the switch.

switch_gen__generate_det_switch(CaseVar, Cases, Instr) -->
	code_info__flush_variable(CaseVar, VarCode),
	code_info__get_variable_register(CaseVar, Lval),
	code_info__get_next_label(EndLabel),
	switch_gen__generate_det_cases(Cases, CaseVar, 
					Lval, EndLabel, CasesCode),
	{ Instr = tree(VarCode, CasesCode) },
	code_info__remake_code_info.

	% To generate a case for a deterministic switch we generate
	% code to do a tag-test and fall through to the next case in
	% the event of failure. The bodies of the cases are deterministic
	% so we generate them as such.

:- pred switch_gen__generate_det_cases(list(case), var, lval, label,
					code_tree, code_info, code_info).
:- mode switch_gen__generate_det_cases(in, in, in, in, out, in, out) is det.

	% At the end of the list of cases we put the end-of-switch
	% label which each case branches to after its case goal.
switch_gen__generate_det_cases([], _Var, _Lval, EndLabel, Code) -->
	{ Code = node([label(EndLabel) - " End of switch"]) }.
	
	% Each case [except the last] consists of a tag test, followed
	% by the goal for that case, followed by a branch to the end of
	% the case. The goal is generated as a "forced" goal which ensures
	% that all variables which are live at the end of the case get
	% stored in their stack slot.
switch_gen__generate_det_cases([case(Cons, Goal)|Cases], Var, Lval, EndLabel,
								CasesCode) -->
	code_info__get_next_label(ElseLab),
	code_info__grab_code_info(CodeInfo),
	code_info__set_failure_cont(ElseLab),
	unify_gen__generate_tag_test(Var, Cons, TestCode),
	code_info__unset_failure_cont,
		% generate the case as a deterministic goal
	code_gen__generate_forced_det_goal(Goal, ThisCode),
	{ ElseLabel = node([
		goto(EndLabel) - "skip to the end of the switch",
		label(ElseLab) - "next case"
	]) },
		% generate the rest of the cases.
	(
		{ Cases = [_|_] }
	->
		code_info__slap_code_info(CodeInfo)
	;
		{ true }
	),
	switch_gen__generate_det_cases(Cases, Var, Lval, EndLabel, CasesCode0),
	{ CasesCode = tree(tree(TestCode, ThisCode),
			tree(ElseLabel, CasesCode0)) }.

%---------------------------------------------------------------------------%

	% A semideterministic switch contains semideterministic goals.

switch_gen__generate_semi_switch(CaseVar, Cases, Instr) -->
	code_info__flush_variable(CaseVar, VarCode),
	code_info__get_variable_register(CaseVar, Lval),
	code_info__get_next_label(EndLabel),
	switch_gen__generate_semi_cases(Cases, CaseVar, Lval,
						EndLabel, CasesCode),
	{ Instr = tree(VarCode, CasesCode) },
	code_info__remake_code_info.

:- pred switch_gen__generate_semi_cases(list(case), var, lval, label,
					code_tree, code_info, code_info).
:- mode switch_gen__generate_semi_cases(in, in, in, in, out, in, out) is det.

	% At the end of the switch, we fail because we came across a
	% tag which was not covered by one of the cases. It is followed
	% by the end of switch label to which the cases branch.
switch_gen__generate_semi_cases([], _Var, _Lval, EndLabel, Code) -->
	code_info__get_failure_cont(FallThrough),
	{ Code = node([
		goto(FallThrough) - "fail",
		label(EndLabel) - "End of switch"
	]) }.

	% A semidet cases consists of a tag-test followed by a semidet
	% goal and a label for the start of the next case.
switch_gen__generate_semi_cases([case(Cons, Goal)|Cases], Var, Lval, EndLabel,
								CasesCode) -->
	code_info__grab_code_info(CodeInfo),
	code_info__get_next_label(ElseLab),
	code_info__push_failure_cont(ElseLab),
	unify_gen__generate_tag_test(Var, Cons, TestCode),
	code_info__pop_failure_cont(_),
		% generate the case as a semi-deterministc goal
	code_gen__generate_forced_semi_goal(Goal, ThisCode),
	{ ElseLabel = node([
		goto(EndLabel) - "skip to the end of the switch",
		label(ElseLab) - "next case"
	]) },
		% If there are more cases, the we need to restore
		% the expression cache, etc.
	(
		{ Cases = [_|_] }
	->
		code_info__slap_code_info(CodeInfo)
	;
		{ true }
	),
		% generate the rest of the cases.
	switch_gen__generate_semi_cases(Cases, Var, Lval, EndLabel, CasesCode0),
	{ CasesCode = tree(tree(TestCode, ThisCode),
			tree(ElseLabel, CasesCode0)) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

