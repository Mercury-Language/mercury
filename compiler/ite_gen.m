%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module ite_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred ite_gen__generate_det_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_det_ite(in, in, in, out, in, out) is det.

:- pred ite_gen__generate_semidet_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_semidet_ite(in, in, in, out, in, out) is det.

:- pred ite_gen__generate_nondet_ite(hlds__goal, hlds__goal, hlds__goal,
					code_tree, code_info, code_info).
:- mode ite_gen__generate_nondet_ite(in, in, in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_next_label(ElseLab),
	code_info__set_failure_cont(ElseLab),
		% generate the semi-deterministc test goal
	code_gen__generate_semi_goal(CondGoal, TestCode),
	code_info__unset_failure_cont,
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_forced_det_goal(ThenGoal, ThenGoalCode),
		% generate code that executes the then condition
		% and branches to the end of the if-then-else
	code_info__slap_code_info(CodeInfo),
	code_gen__generate_forced_det_goal(ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab),
		% place the label marking the start of the then code,
		% then execute the then goal, and then mark the end
		% of the if-then-else
	{ ThenCode = tree(
		ThenGoalCode,
		node([ goto(EndLab) - "Jump the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			node([label(ElseLab) - "else case"]),
			ElseGoalCode
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(TestCode, tree(ThenCode, ElseCode)) },
	code_info__remake_code_info.

%---------------------------------------------------------------------------%

ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, Instr) -->
	code_info__get_next_label(ElseLab),
	code_info__push_failure_cont(ElseLab),
		% generate the semi-deterministc test goal
	code_gen__generate_semi_goal(CondGoal, CondCode),
	code_info__pop_failure_cont_det(_),
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_forced_semi_goal(ThenGoal, ThenGoalCode),
	code_info__slap_code_info(CodeInfo),
	code_gen__generate_forced_semi_goal(ElseGoal, ElseGoalCode),
	code_info__get_next_label(EndLab),
	{ TestCode = tree(
		node([comment("If-then-else") - ""]),
		CondCode
	) },
	{ ThenCode = tree(
		tree(
			node([comment("then-case") - ""]),
			ThenGoalCode
		),
		node([ goto(EndLab) - "Jump the end of if-then-else" ])
	) },
	{ ElseCode = tree(
		tree(
			node([label(ElseLab) - "else case"]),
			ElseGoalCode
		),
		node([label(EndLab) - "end of if-then-else"])
	) },
		% generate the then condition
	{ Instr = tree(TestCode, tree(ThenCode, ElseCode)) },
	code_info__remake_code_info.

%---------------------------------------------------------------------------%

ite_gen__generate_nondet_ite(_CondGoal, _ThenGoal, _ElseGoal, _Instr) -->
	{ error("Unimplemented") }.

%---------------------------------------------------------------------------%

