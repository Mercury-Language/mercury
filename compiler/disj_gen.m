%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module disj_gen.

:- interface.

:- import_module hlds, llds, code_gen, code_info, code_util.

:- pred disj_gen__generate_semi_disj(list(hlds__goal),
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_disj(in, out, in, out) is det.

:- pred disj_gen__generate_non_disj(list(hlds__goal),
					code_tree, code_info, code_info).
:- mode disj_gen__generate_non_disj(in, out, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

%---------------------------------------------------------------------------%

disj_gen__generate_semi_disj(Goals, GoalsCode) -->
	code_info__get_failure_cont(FallThrough),
	code_info__get_next_label(EndLabel),
	disj_gen__generate_semi_cases(Goals, FallThrough, EndLabel, GoalsCode),
	code_info__remake_code_info.

:- pred disj_gen__generate_semi_cases(list(hlds__goal), label, label,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_cases(in, in, in, out, in, out) is det.

disj_gen__generate_semi_cases([], FallThrough, _EndLabel, Code) -->
	{ Code = node([
		goto(FallThrough) - "explicit `fail'"
	]) }.
disj_gen__generate_semi_cases([Goal|Goals], FallThrough,
						EndLabel, GoalsCode) -->
	(
		{ Goals = [] }
	->
		code_info__set_failure_cont(FallThrough),
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		{ GoalsCode = tree(ThisCode, node([
			label(EndLabel) - "End of semideterministic disj"
		])) }
	;
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLab),
		code_info__set_failure_cont(ElseLab),
			% generate the case as a semi-deterministic goal
		code_gen__generate_forced_semi_goal(Goal, ThisCode),
		{ ElseLabel = node([
			goto(EndLabel) - "skip to the end of the disj",
			label(ElseLab) - "next case"
		]) },
			% If there are more cases, the we need to restore
			% the expression cache, etc.
		code_info__slap_code_info(CodeInfo),
			% generate the rest of the cases.
		disj_gen__generate_semi_cases(Goals, FallThrough, EndLabel,
			GoalsCode0),
		{ GoalsCode = tree(ThisCode, tree(ElseLabel, GoalsCode0)) }
	).

%---------------------------------------------------------------------------%

disj_gen__generate_non_disj(Goals, tree(SaveCode, GoalsCode)) -->
	code_info__generate_nondet_saves(SaveCode),
	code_info__get_next_label(ContLab),
	code_info__push_failure_cont(ContLab),
		% we remember the first failure continuation label,
		% so we can use it in the mkframe() in the procedure prologue
	(
		code_info__get_continuation(no)
	->
		code_info__set_continuation(yes(ContLab))
	;
		{ true }
	),
	code_info__get_next_label(EndLab),
	disj_gen__generate_non_disj_2(Goals, EndLab, GoalsCode).

:- pred disj_gen__generate_non_disj_2(list(hlds__goal), label, code_tree,
						code_info, code_info).
:- mode disj_gen__generate_non_disj_2(in, in, out, in, out) is det.

disj_gen__generate_non_disj_2([], EndLab, EndCode) -->
	{ EndCode = node([
		label(EndLab) - "End of disj"
	]) }.
disj_gen__generate_non_disj_2([Goal|Goals], EndLab, DisjCode) -->
	(
		{ Goals = [_|_] }
	->
		(
			code_info__failure_cont(ContLab0)
		->
			{ ContCode = node([
				modframe(yes(ContLab0)) -
						"Set failure continuation"
			]) },
			{ FailCode = node([
				goto(EndLab) - "Jump to end of disj",
				label(ContLab0) - "Start of next disjunct"
			]) }
		;
			{ error("Missing failure continuation") }
		)
	;
		(
			code_info__pop_failure_cont(_),
			code_info__failure_cont(ContLab1)
		->
			{ ContCode = node([
				modframe(yes(ContLab1)) -
						"Set failure continuation"
			]) },
			{ FailCode = empty }
		;
			{ ContCode = node([
				modframe(no) -
					"Failure continuation is fail()"
			]) },
			{ FailCode = empty }
		)
	),
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_forced_non_goal(Goal, GoalCode),
	(
		{ Goals = [_|_] },
		code_info__slap_code_info(CodeInfo),
		code_info__pop_failure_cont(_)
	->
		code_info__get_next_label(NextCont),
		code_info__push_failure_cont(NextCont)
	;
		{ true }
	),
	disj_gen__generate_non_disj_2(Goals, EndLab, RestCode),
	{ DisjCode = tree(tree(ContCode, GoalCode), tree(FailCode, RestCode)) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

