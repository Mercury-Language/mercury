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

%---------------------------------------------------------------------------%
:- implementation.

:- import_module tree, list, map, std_util, require.

%---------------------------------------------------------------------------%

disj_gen__generate_semi_disj(Goals, GoalsCode) -->
	code_info__get_fall_through(FallThrough),
	code_info__get_next_label(EndLabel),
	disj_gen__generate_semi_cases(Goals, FallThrough, EndLabel, GoalsCode),
	code_info__remake_code_info.

:- pred disj_gen__generate_semi_cases(list(hlds__goal), label, label,
					code_tree, code_info, code_info).
:- mode disj_gen__generate_semi_cases(in, in, in, out, in, out) is det.

disj_gen__generate_semi_cases([], FallThrough, EndLabel, Code) -->
	{ Code = node([
		goto(FallThrough) - "fail",
		label(EndLabel) - "End of semideterministic disj"
	]) }.
disj_gen__generate_semi_cases([Goal|Goals], FallThrough,
						EndLabel, GoalsCode) -->
	code_info__grab_code_info(CodeInfo),
	code_info__get_next_label(ElseLab),
	code_info__set_fall_through(ElseLab),
		% generate the case as a semi-deterministc goal
	code_gen__generate_forced_semi_goal(Goal, ThisCode),
	{ ElseLabel = node([
		goto(EndLabel) - "skip to the end of the disj",
		label(ElseLab) - "next case"
	]) },
		% If there are more cases, the we need to restore
		% the expression cache, etc.
	(
		{ Goals = [_|_] }
	->
		code_info__slap_code_info(CodeInfo)
	;
		{ true }
	),
		% generate the rest of the cases.
	disj_gen__generate_semi_cases(Goals, FallThrough, EndLabel, GoalsCode0),
	{ GoalsCode = tree(ThisCode, tree(ElseLabel, GoalsCode0)) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

