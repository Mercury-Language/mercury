%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_report.m - reporting of determinism errors and warnings.

% author: zs.

%-----------------------------------------------------------------------------%

:- module det_report.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, det_util.
:- import_module io.

:- type det_msg	--->
			% warnings
			multidet_disj(term__context, list(term__context))
		;	det_disj(term__context, list(term__context))
		;	semidet_disj(term__context, list(term__context))
		;	zero_soln_disj(term__context, list(term__context))
		;	zero_soln_disjunct(term__context)
		;	ite_cond_cannot_fail(term__context)
		;	ite_cond_cannot_succeed(term__context)
		;	negated_goal_cannot_fail(term__context)
		;	negated_goal_cannot_succeed(term__context)
		;	warn_obsolete(pred_id, term__context)
				% warning about calls to predicates
				% for which there is a `:- pragma obsolete'
				% declaration.
		;	warn_infinite_recursion(term__context)
				% warning about recursive calls
				% which would cause infinite loops.
		;	duplicate_call(seen_call_id, term__context,
				term__context)
				% multiple calls with the same input args.
			% errors
		;	cc_pred_in_wrong_context(hlds_goal_info, determinism,
				pred_id, proc_id)
		;	higher_order_cc_pred_in_wrong_context(hlds_goal_info,
				determinism)
		;	error_in_lambda(
				determinism, determinism, % declared, inferred
				hlds_goal, hlds_goal_info, pred_id, proc_id).

:- type seen_call_id
	--->	seen_call(pred_id, proc_id)
	;	higher_order_call.

%-----------------------------------------------------------------------------%

	% Check all the determinism declarations in this module.
	% This is the main predicate exported by this module.

:- pred global_checking_pass(pred_proc_list, module_info, module_info,
	io__state, io__state).
:- mode global_checking_pass(in, in, out, di, uo) is det.

	% Check a lambda goal with the specified declared and inferred
	% determinisms.

:- pred det_check_lambda(determinism, determinism, hlds_goal, hlds_goal_info,
			det_info, list(det_msg)).
:- mode det_check_lambda(in, in, in, in, in, out) is det.

	% Print some determinism warning and/or error messages,
	% and update the module info accordingly.

:- pred det_report_and_handle_msgs(list(det_msg), module_info, module_info,
		io__state, io__state).
:- mode det_report_and_handle_msgs(in, in, out, di, uo) is det.

	% Print some determinism warning and/or error messages,
	% and return the number of warnings and errors, so that code
	% somewhere elsewhere can update the module info.

:- pred det_report_msgs(list(det_msg), module_info, int, int,
	io__state, io__state).
:- mode det_report_msgs(in, in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, type_util, mode_util, inst_match.
:- import_module globals, options, prog_out, hlds_out, mercury_to_mercury.
:- import_module passes_aux.

:- import_module bool, int, list, map, set, varset, std_util, term, require.

%-----------------------------------------------------------------------------%

global_checking_pass([], ModuleInfo, ModuleInfo) --> [].
global_checking_pass([proc(PredId, ProcId) | Rest], ModuleInfo0, ModuleInfo) -->
	{
		module_info_preds(ModuleInfo0, PredTable),
		map__lookup(PredTable, PredId, PredInfo),
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ProcId, ProcInfo),
		proc_info_declared_determinism(ProcInfo, MaybeDetism),
		proc_info_inferred_determinism(ProcInfo, InferredDetism)
	},
	(
		{ MaybeDetism = no },
		{ ModuleInfo1 = ModuleInfo0 }
	;
		{ MaybeDetism = yes(DeclaredDetism) },
		{ compare_determinisms(DeclaredDetism, InferredDetism, Cmp) },
		(
			{ Cmp = sameas },
			{ ModuleInfo1 = ModuleInfo0 }
		;
			{ Cmp = looser },
			globals__io_lookup_bool_option(
				warn_det_decls_too_lax,
				ShouldIssueWarning),
			( { ShouldIssueWarning = yes } ->
				{ Message = "  warning: determinism declaration could be tighter.\n" },
				report_determinism_problem(PredId,
					ProcId, ModuleInfo0, Message,
					DeclaredDetism, InferredDetism)
			;
				[]
			),
			{ ModuleInfo1 = ModuleInfo0 }
		;
			{ Cmp = tighter },
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo1) },
			{ Message = "  error: determinism declaration not satisfied.\n" },
			report_determinism_problem(PredId,
				ProcId, ModuleInfo1, Message,
				DeclaredDetism, InferredDetism),
			{ proc_info_goal(ProcInfo, Goal) },
			globals__io_get_globals(Globals),
			{ det_info_init(ModuleInfo1, PredId, ProcId, Globals,
				DetInfo) },
			det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _)
			% XXX with the right verbosity options, we want to
			% call report_determinism_problem only if diagnose
			% returns false, i.e. it didn't print a message.
		)
	),
	% check that `main/2' cannot fail
	( 
		{ pred_info_name(PredInfo, "main") },
		{ pred_info_arity(PredInfo, 2) },
		{ pred_info_is_exported(PredInfo) },
		{
		  determinism_components(InferredDetism, can_fail, _)
		;
		  MaybeDetism = yes(DeclaredDeterminism),
		  determinism_components(DeclaredDeterminism, can_fail, _)
		}
	->
		{ proc_info_context(ProcInfo, Context) },
		prog_out__write_context(Context),
			% The error message is actually a lie -
			% main/2 can also be `erroneous'.  But mentioning
			% that would probably just confuse people.
		io__write_string(
			"Error: main/2 must be `det' or `cc_multi'.\n"),
		{ module_info_incr_errors(ModuleInfo1,
			ModuleInfo2) }
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	global_checking_pass(Rest, ModuleInfo2, ModuleInfo).

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, DetInfo,
		Msgs) :-
	compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
	( Cmp = tighter ->
		det_info_get_pred_id(DetInfo, PredId),
		det_info_get_proc_id(DetInfo, ProcId),
		Msgs = [error_in_lambda(DeclaredDetism, InferredDetism,
			Goal, GoalInfo, PredId, ProcId)]
	;
		% we don't bother issuing warnings if
		% the determinism was too loose; that will often
		% be the case, and should not be warned about.
		Msgs = []
	).

:- pred report_determinism_problem(pred_id, proc_id, module_info, string,
	determinism, determinism, io__state, io__state).
:- mode report_determinism_problem(in, in, in, in, in, in, di, uo) is det.

report_determinism_problem(PredId, ModeId, ModuleInfo, Message,
		DeclaredDetism, InferredDetism) -->
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	( { HaltAtWarn = yes } ->
		 io__set_exit_status(1)
	;
		[]
	),
	report_pred_proc_id(ModuleInfo, PredId, ModeId, no, Context),
	prog_out__write_context(Context),
	io__write_string(Message),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_determinism(DeclaredDetism),
	io__write_string("', inferred `"),
	hlds_out__write_determinism(InferredDetism),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- type det_comparison	--->	tighter ; sameas ; looser.

:- pred compare_determinisms(determinism, determinism, det_comparison).
:- mode compare_determinisms(in, in, out) is det.

compare_determinisms(DeclaredDetism, InferredDetism, CmpDetism) :-
	determinism_components(DeclaredDetism, DeclaredCanFail, DeclaredSolns),
	determinism_components(InferredDetism, InferredCanFail, InferredSolns),
	compare_canfails(DeclaredCanFail, InferredCanFail, CmpCanFail),
	compare_solncounts(DeclaredSolns, InferredSolns, CmpSolns),

	% We can get e.g. tighter canfail and looser solncount
	% e.g. for a predicate declared multidet and inferred semidet.
	% Therefore the ordering of the following two tests is important:
	% we want errors to take precedence over warnings.

	( ( CmpCanFail = tighter ; CmpSolns = tighter ) ->
		CmpDetism = tighter
	; ( CmpCanFail = looser ; CmpSolns = looser ) ->
		CmpDetism = looser
	;
		CmpDetism = sameas
	).

:- pred compare_canfails(can_fail, can_fail, det_comparison).
:- mode compare_canfails(in, in, out) is det.

compare_canfails(cannot_fail, cannot_fail, sameas).
compare_canfails(cannot_fail, can_fail,    tighter).
compare_canfails(can_fail,    cannot_fail, looser).
compare_canfails(can_fail,    can_fail,    sameas).

:- pred compare_solncounts(soln_count, soln_count, det_comparison).
:- mode compare_solncounts(in, in, out) is det.

compare_solncounts(at_most_zero,    at_most_zero,    sameas).
compare_solncounts(at_most_zero,    at_most_one,     tighter).
compare_solncounts(at_most_zero,    at_most_many_cc, tighter).
compare_solncounts(at_most_zero,    at_most_many,    tighter).

compare_solncounts(at_most_one,     at_most_zero,    looser).
compare_solncounts(at_most_one,     at_most_one,     sameas).
compare_solncounts(at_most_one,     at_most_many_cc, tighter).
compare_solncounts(at_most_one,     at_most_many,    tighter).

compare_solncounts(at_most_many_cc, at_most_zero,    looser).
compare_solncounts(at_most_many_cc, at_most_one,     looser).
compare_solncounts(at_most_many_cc, at_most_many_cc, sameas).
compare_solncounts(at_most_many_cc, at_most_many,    tighter).

compare_solncounts(at_most_many,    at_most_zero,    looser).
compare_solncounts(at_most_many,    at_most_one,     looser).
compare_solncounts(at_most_many,    at_most_many_cc, looser).
compare_solncounts(at_most_many,    at_most_many,    sameas).

%-----------------------------------------------------------------------------%

	% The given goal should have determinism Desired, but doesn't.
	% Find out what is wrong and print a report of the cause.

:- pred det_diagnose_goal(hlds_goal, determinism, list(switch_context),
	det_info, bool, io__state, io__state).
:- mode det_diagnose_goal(in, in, in, in, out, di, uo) is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, DetInfo,
		Diagnosed) -->
	{ goal_info_get_determinism(GoalInfo, Actual) },
	( { compare_determinisms(Desired, Actual, tighter) } ->
		det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual,
			SwitchContext, DetInfo, Diagnosed)
	;
		{ Diagnosed = no }
	).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds_goal_expr, hlds_goal_info,
	determinism, determinism, list(switch_context), det_info, bool,
	io__state, io__state).
:- mode det_diagnose_goal_2(in, in, in, in, in, in, out, di, uo) is det.

det_diagnose_goal_2(conj(Goals), _GoalInfo, Desired, _Actual, Context, DetInfo,
		Diagnosed) -->
	det_diagnose_conj(Goals, Desired, Context, DetInfo, Diagnosed).

det_diagnose_goal_2(disj(Goals, _), GoalInfo, Desired, Actual, SwitchContext,
		DetInfo, Diagnosed) -->
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo, 0,
		Clauses, Diagnosed1),
	{ determinism_components(Desired, _, DesSolns) },
	(
		{ DesSolns \= at_most_many },
		{ DesSolns \= at_most_many_cc },
		{ Clauses > 1 }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Disjunction has multiple clauses with solutions.\n"),
		{ Diagnosed = yes }
	;
		{ Diagnosed = Diagnosed1 }
	).

	% The determinism of a switch is the worst of the determinism of each of
	% the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the CanFail field.

det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases, _), GoalInfo,
		Desired, _Actual, SwitchContext, DetInfo, Diagnosed) -->
	(
		{ SwitchCanFail = can_fail },
		{ determinism_components(Desired, cannot_fail, _) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		det_diagnose_write_switch_context(Context, SwitchContext,
			DetInfo),
		prog_out__write_context(Context),
		{ det_get_proc_info(DetInfo, ProcInfo) },
		{ proc_info_variables(ProcInfo, Varset) },
		{ det_info_get_module_info(DetInfo, ModuleInfo) },
		(
			{ det_lookup_var_type(ModuleInfo, ProcInfo, Var,
				TypeDefn) },
			{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
			{ TypeBody = du_type(_, ConsTable, _) }
		->
			{ map__keys(ConsTable, ConsIds) },
			{ det_diagnose_missing_consids(ConsIds, Cases,
				Missing) },
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset, no),
			io__write_string(" does not cover "),
			det_output_consid_list(Missing, yes),
			io__write_string(".\n")
		;
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset, no),
			io__write_string(" can fail.\n")
		),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext, DetInfo,
		Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

det_diagnose_goal_2(call(PredId, ModeId, _, _, CallContext, _), GoalInfo,
		Desired, Actual, _, DetInfo, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	{ compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail) },
	( { CmpCanFail = tighter } ->
		det_report_call_context(Context, CallContext, DetInfo,
			PredId, ModeId),
		io__write_string("can fail.\n"),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	{ compare_solncounts(DesiredSolns, ActualSolns, CmpSolns) },
	( { CmpSolns = tighter } ->
		det_report_call_context(Context, CallContext, DetInfo,
			PredId, ModeId),
		io__write_string("can succeed"),
		( { DesiredSolns = at_most_one } ->
			io__write_string(" more than once.\n")
		;
			io__write_string(".\n")
		),
		{ Diagnosed2 = yes }
	;
		{ Diagnosed2 = no }
	),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) },
	(
		{ Diagnosed = yes }
	;
		{ Diagnosed = no },
		det_report_call_context(Context, CallContext, DetInfo,
			PredId, ModeId),
		io__write_string("has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(higher_order_call(_, _, _, _, _), GoalInfo,
		Desired, Actual, _, _MiscInfo, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	{ compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail) },
	( { CmpCanFail = tighter } ->
		prog_out__write_context(Context),
		io__write_string("  Higher-order predicate call can fail.\n"),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	{ compare_solncounts(DesiredSolns, ActualSolns, CmpSolns) },
	( { CmpSolns = tighter } ->
		prog_out__write_context(Context),
		io__write_string("  Higher-order predicate call can succeed"),
		( { DesiredSolns = at_most_one } ->
			io__write_string(" more than once.\n")
		;
			io__write_string(".\n")
		),
		{ Diagnosed2 = yes }
	;
		{ Diagnosed2 = no }
	),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) },
	(
		{ Diagnosed = yes }
	;
		{ Diagnosed = no },
		prog_out__write_context(Context),
		io__write_string("  Higher-order predicate call has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(unify(LT, RT, _, _, UnifyContext), GoalInfo,
		Desired, Actual, _, DetInfo, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, _DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, _ActualSolns) },
	{ First = yes, Last = yes },
	det_report_unify_context(First, Last, Context, UnifyContext, DetInfo,
				LT, RT),
	(
		{ DesiredCanFail = cannot_fail },
		{ ActualCanFail = can_fail }
	->
		io__write_string(" can fail.\n")
	;
		io__write_string(" has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else, _), _GoalInfo,
		Desired, _Actual, SwitchContext, DetInfo, Diagnosed) -->
	{
		determinism_components(Desired, _DesiredCanFail, DesiredSolns),
		Cond = _CondGoal - CondInfo,
		goal_info_get_determinism(CondInfo, CondDetism),
		determinism_components(CondDetism, _CondCanFail, CondSolns)
	},
	(
		{ CondSolns = at_most_many },
		{ DesiredSolns \= at_most_many }
	->
		{ determinism_components(DesiredCond, can_fail, DesiredSolns) },
		det_diagnose_goal(Cond, DesiredCond, SwitchContext, DetInfo,
			Diagnosed1)
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_goal(Then, Desired, SwitchContext, DetInfo, Diagnosed2),
	det_diagnose_goal(Else, Desired, SwitchContext, DetInfo, Diagnosed3),
	{ bool__or(Diagnosed2, Diagnosed3, Diagnosed23) },
	{ bool__or(Diagnosed1, Diagnosed23, Diagnosed) }.

det_diagnose_goal_2(not(_), GoalInfo, Desired, Actual, _, _, Diagnosed) -->
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	(
		{ DesiredCanFail = cannot_fail },
		{ ActualCanFail = can_fail }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Negated goal can succeed.\n"),
		{ Diagnosed = yes }
	;
		{ DesiredSolns = at_most_zero },
		{ ActualSolns \= at_most_zero }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Negated goal can fail.\n"),
		{ Diagnosed = yes }
	;
		{ Diagnosed = no }
	).

det_diagnose_goal_2(some(_Vars, Goal), _, Desired, Actual,
		SwitchContext, DetInfo, Diagnosed) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_determinism(GoalInfo, Internal) },
	{ Actual = Internal ->
		InternalDesired = Desired
	;
		determinism_components(Desired, CanFail, _),
		determinism_components(InternalDesired, CanFail, at_most_many)
	},
	det_diagnose_goal(Goal, InternalDesired, SwitchContext, DetInfo,
		Diagnosed).

det_diagnose_goal_2(pragma_c_code(_, _, _, _, _, _, _, _), GoalInfo, Desired, 
		_, _, _, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  Determinism declaration not satisfied. Desired \n"),
	prog_out__write_context(Context),
	io__write_string("  determinism is "),
	hlds_out__write_determinism(Desired),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	io__write_string("  pragma c_code declarations only allowed\n"),
	prog_out__write_context(Context),
	io__write_string("  for modes which don't succeed more than once.\n").
	% XXX

%-----------------------------------------------------------------------------%

:- pred det_diagnose_conj(list(hlds_goal), determinism,
	list(switch_context), det_info, bool, io__state, io__state).
:- mode det_diagnose_conj(in, in, in, in, out, di, uo) is det.

det_diagnose_conj([], _Desired, _SwitchContext, _DetInfo, no) --> [].
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, DetInfo,
		Diagnosed) -->
	det_diagnose_goal(Goal, Desired, SwitchContext, DetInfo, Diagnosed1),
	det_diagnose_conj(Goals, Desired, SwitchContext, DetInfo, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_disj(list(hlds_goal), determinism, determinism,
	list(switch_context), det_info, int, int, bool, io__state, io__state).
:- mode det_diagnose_disj(in, in, in, in, in, in, out, out, di, uo) is det.

det_diagnose_disj([], _Desired, _Actual, _SwitchContext, _DetInfo,
		Clauses, Clauses, no) --> [].
det_diagnose_disj([Goal | Goals], Desired, Actual, SwitchContext, DetInfo,
		Clauses0, Clauses, Diagnosed) -->
	{ determinism_components(Actual, ActualCanFail, _) },
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ DesiredCanFail = cannot_fail, ActualCanFail = can_fail ->
		% if the disjunction was declared to never fail,
		% but we inferred that it might fail, then we
		% want to print an error message for every disjunct
		% that might fail
		ClauseCanFail = cannot_fail
	;	
		% otherwise, either the disjunction is allowed to
		% fail, or there is at least one disjunct that we
		% inferred won't fail, so we don't want any error
		% messages for the disjuncts that might fail
		ClauseCanFail = can_fail
	},
	{ determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns) },
	det_diagnose_goal(Goal, ClauseDesired, SwitchContext, DetInfo,
		Diagnosed1),
	{ Clauses1 is Clauses0 + 1 },
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo,
		Clauses1, Clauses, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_switch(var, list(case), determinism,
	list(switch_context), det_info, bool, io__state, io__state).
:- mode det_diagnose_switch(in, in, in, in, in, out, di, uo) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _DetInfo, no) --> [].
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
		SwitchContext0, DetInfo, Diagnosed) -->
	{ SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0] },
	det_diagnose_goal(Goal, Desired, SwitchContext1, DetInfo, Diagnosed1),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext0, DetInfo,
		Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

%-----------------------------------------------------------------------------%

:- pred det_diagnose_missing_consids(list(cons_id), list(case), list(cons_id)).
:- mode det_diagnose_missing_consids(in, in, out) is det.

det_diagnose_missing_consids([], _, []).
det_diagnose_missing_consids([ConsId | ConsIds], Cases, Missing) :-
	det_diagnose_missing_consids(ConsIds, Cases, Missing0),
	(
		list__member(Case, Cases),
		Case = case(ConsId, _)
	->
		Missing = Missing0
	;
		Missing = [ConsId | Missing0]
	).

:- pred det_output_consid_list(list(cons_id), bool, io__state, io__state).
:- mode det_output_consid_list(in, in, di, uo) is det.

det_output_consid_list([], _) --> [].
det_output_consid_list([ConsId | ConsIds], First) -->
	( { First = yes } ->
		[]
	; { ConsIds = [] } ->
		io__write_string(" and ")
	;
		io__write_string(", ")
	),
	hlds_out__write_cons_id(ConsId),
	det_output_consid_list(ConsIds, no).

%-----------------------------------------------------------------------------%

:- type switch_context --->	switch_context(var, cons_id).

:- pred det_diagnose_write_switch_context(term__context, list(switch_context),
	det_info, io__state, io__state).
:- mode det_diagnose_write_switch_context(in, in, in, di, uo) is det.

det_diagnose_write_switch_context(_Context, [], _MiscInco) --> [].
det_diagnose_write_switch_context(Context, [SwitchContext | SwitchContexts],
		DetInfo) -->
	prog_out__write_context(Context),
	{ det_get_proc_info(DetInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ SwitchContext = switch_context(Var, ConsId) },
	io__write_string("  Inside the case "),
	hlds_out__write_cons_id(ConsId),
	io__write_string(" of the switch on "),
	mercury_output_var(Var, Varset, no),
	io__write_string(":\n"),
	det_diagnose_write_switch_context(Context, SwitchContexts, DetInfo).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(term__context, maybe(call_unify_context),
	det_info, pred_id, proc_id, io__state, io__state).
:- mode det_report_call_context(in, in, in, in, in, di, uo) is det.

det_report_call_context(Context, CallUnifyContext, DetInfo, PredId, ModeId) -->
	{ det_info_get_module_info(DetInfo, ModuleInfo) },
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	%
	% if the error was in a call to __Unify__ (i.e. in the unification
	% itself), then don't print out the predicate name, just print
	% out the context.  If it wasn't, then print them both out.
	% (The latter can happen if there is a determinism error in a
	% function call inside some unification.)
	%
	( { PredName = "__Unify__" } ->
		(
			{ CallUnifyContext = yes(
					call_unify_context(LT, RT, UC)) },
			{ First = yes, Last = yes },
			det_report_unify_context(First, Last,
				Context, UC, DetInfo, LT, RT),
			io__write_string(" ")
		;
			% this shouldn't happen; every call to __Unify__
			% should have a unify_context
			{ CallUnifyContext = no },
			prog_out__write_context(Context),
			io__write_string(
	"  Some weird unification (or explicit call to `__Unify__'?) ")
		)
	;
		(
			{ CallUnifyContext = yes(
					call_unify_context(LT, RT, UC)) },
			{ First = yes, Last = no },
			det_report_unify_context(First, Last,
				Context, UC, DetInfo, LT, RT),
			io__write_string(":\n")
		;
			{ CallUnifyContext = no }
		),
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ map__lookup(ProcTable, ModeId, ProcInfo) },
		{ proc_info_declared_argmodes(ProcInfo, ArgModes) },
		prog_out__write_context(Context),
		io__write_string("  call to `"),
		report_pred_name_mode(PredOrFunc, PredName, ArgModes),
		io__write_string("' ")
	).

%-----------------------------------------------------------------------------%

% det_report_unify_context prints out information about the context of an
% error, i.e. where the error occurred.
% The first two arguments are boolean flags that specify whether this is
% the first part of a sentence (in which case we start the error message
% with a capital letter) and whether it is the last part (in which case we
% omit the word "in" on the final "... in unification ...").

:- pred det_report_unify_context(bool, bool, term__context, unify_context,
	det_info, var, unify_rhs, io__state, io__state).
:- mode det_report_unify_context(in, in, in, in, in, in, in, di, uo) is det.

det_report_unify_context(First0, Last, Context, UnifyContext, DetInfo, LT, RT)
		-->
	hlds_out__write_unify_context(First0, UnifyContext, Context, First),
	prog_out__write_context(Context),
	{ det_get_proc_info(DetInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ det_info_get_module_info(DetInfo, ModuleInfo) },
	( { First = yes } ->
		( { Last = yes } ->
			io__write_string("  Unification ")
		;
			io__write_string("  In unification ")
		)
	;
		( { Last = yes } ->
			io__write_string("  unification ")
		;
			io__write_string("  in unification ")
		)
	),
	(
		{ varset__search_name(Varset, LT, _) }
	->
		(
			{ RT = var(RV) },
			\+ { varset__search_name(Varset, RV, _) }
		->
			io__write_string("with `"),
			mercury_output_var(LT, Varset, no)
		;
			io__write_string("of `"),
			mercury_output_var(LT, Varset, no),
			io__write_string("' and `"),
			hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, no, 3)
		)
	;
		io__write_string("with `"),
		hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, no, 3)
	),
	io__write_string("'").


%-----------------------------------------------------------------------------%

:- type det_msg_type	--->	warning ; error.

det_report_and_handle_msgs(Msgs, ModuleInfo0, ModuleInfo) -->
	( { Msgs = [] } ->
		% fast path for the usual case
		{ ModuleInfo = ModuleInfo0 }
	;
		det_report_msgs(Msgs, ModuleInfo0, WarnCnt, ErrCnt),
		globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
		(
			{
				ErrCnt > 0
			;
				WarnCnt > 0,
				HaltAtWarn = yes
			}
		->
			io__set_exit_status(1),
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }
		;
			{ ModuleInfo = ModuleInfo0 }
		)
	).

det_report_msgs(Msgs, ModuleInfo, WarnCnt, ErrCnt) -->
	globals__io_lookup_bool_option(warn_simple_code, Warn),
	det_report_msgs_2(Msgs, Warn, ModuleInfo, 0, WarnCnt, 0, ErrCnt).

:- pred det_report_msgs_2(list(det_msg), bool,  module_info, int, int,
	int, int, io__state, io__state).
:- mode det_report_msgs_2(in, in, in, in, out, in, out, di, uo) is det.

det_report_msgs_2([], _, _ModuleInfo, WarnCnt, WarnCnt, ErrCnt, ErrCnt) --> [].
det_report_msgs_2([Msg | Msgs], Warn, ModuleInfo,
		WarnCnt0, WarnCnt, ErrCnt0, ErrCnt) -->
	{ det_msg_get_type(Msg, MsgType) },
	( { Warn = no, MsgType = warning } ->
		{ WarnCnt1 = WarnCnt0 },
		{ ErrCnt1 = ErrCnt0 }
	;
		det_report_msg(Msg, ModuleInfo),
		(
			{ MsgType = warning },
			{ WarnCnt1 is WarnCnt0 + 1 },
			{ ErrCnt1 = ErrCnt0 }
		;
			{ MsgType = error },
			{ ErrCnt1 is ErrCnt0 + 1 },
			{ WarnCnt1 = WarnCnt0 }
		)
	),
	det_report_msgs_2(Msgs, Warn, ModuleInfo,
		WarnCnt1, WarnCnt, ErrCnt1, ErrCnt).

:- pred det_msg_get_type(det_msg, det_msg_type).
:- mode det_msg_get_type(in, out) is det.

det_msg_get_type(multidet_disj(_, _), warning).
det_msg_get_type(det_disj(_, _), warning).
det_msg_get_type(semidet_disj(_, _), warning).
det_msg_get_type(zero_soln_disj(_, _), warning).
det_msg_get_type(zero_soln_disjunct(_), warning).
det_msg_get_type(ite_cond_cannot_fail(_), warning).
det_msg_get_type(ite_cond_cannot_succeed(_), warning).
det_msg_get_type(negated_goal_cannot_fail(_), warning).
det_msg_get_type(negated_goal_cannot_succeed(_), warning).
det_msg_get_type(warn_obsolete(_, _), warning).
det_msg_get_type(warn_infinite_recursion(_), warning).
det_msg_get_type(duplicate_call(_, _, _), warning).
det_msg_get_type(cc_pred_in_wrong_context(_, _, _, _), error).
det_msg_get_type(higher_order_cc_pred_in_wrong_context(_, _), error).
det_msg_get_type(error_in_lambda(_, _, _, _, _, _), error).

:- pred det_report_msg(det_msg, module_info, io__state, io__state).
:- mode det_report_msg(in, in, di, uo) is det.

det_report_msg(multidet_disj(Context, DisjunctContexts), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	det_report_context_lines(DisjunctContexts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  has no outputs, but can succeed more than once.\n").
det_report_msg(det_disj(Context, DisjunctContexts), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	det_report_context_lines(DisjunctContexts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  will succeed exactly once.\n").
det_report_msg(semidet_disj(Context, DisjunctContexts), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	det_report_context_lines(DisjunctContexts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  is semidet, yet it has an output.\n").
det_report_msg(zero_soln_disj(Context, DisjunctContexts), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	det_report_context_lines(DisjunctContexts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  cannot succeed.\n").
det_report_msg(zero_soln_disjunct(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: this disjunct will never have any solutions.\n").
det_report_msg(ite_cond_cannot_fail(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the condition of this if-then-else cannot fail.\n").
det_report_msg(ite_cond_cannot_succeed(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the condition of this if-then-else cannot succeed.\n").
det_report_msg(negated_goal_cannot_fail(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the negated goal cannot fail.\n").
det_report_msg(negated_goal_cannot_succeed(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: the negated goal cannot succeed.\n").
det_report_msg(warn_obsolete(PredId, Context), ModuleInfo) -->
	prog_out__write_context(Context),
	io__write_string("Warning: call to obsolete "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(".\n").
det_report_msg(warn_infinite_recursion(Context), _ModuleInfo) -->
/*
% it would be better if we supplied more information
% than just the line number.
	prog_out__write_context(Context),
	io__write_string("In "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
*/
	prog_out__write_context(Context),
	io__write_string(
		"Warning: recursive call will lead to infinite recursion.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string(
"\tIf this recursive call is executed, the procedure will call itself
\twith exactly the same input arguments, leading to infinite recursion.\n")
	;
		[]
	).
det_report_msg(duplicate_call(SeenCall, PrevContext, Context), ModuleInfo) -->
	prog_out__write_context(Context),
	io__write_string("Warning: redundant "),
	det_report_seen_call_id(SeenCall, ModuleInfo),
	io__write_string(".\n"),
	prog_out__write_context(PrevContext),
	io__write_string("Here is the previous "),
	det_report_seen_call_id(SeenCall, ModuleInfo),
	io__write_string(".\n").
det_report_msg(cc_pred_in_wrong_context(GoalInfo, Detism, PredId, _ModeId),
		ModuleInfo) -->
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: call to "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" with determinism `"),
	mercury_output_det(Detism),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  occurs in a context which requires all solutions.\n"),
	io__set_exit_status(1).
det_report_msg(higher_order_cc_pred_in_wrong_context(GoalInfo, Detism),
		_ModuleInfo) -->
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: higher-order call to predicate with determinism `"),
	mercury_output_det(Detism),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  occurs in a context which requires all solutions.\n"),
	io__set_exit_status(1).
det_report_msg(error_in_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo,
			PredId, ProcId), ModuleInfo) -->
	report_pred_proc_id(ModuleInfo, PredId, ProcId, no, _ProcContext),
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Determinism error in lambda expression.\n"),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_determinism(DeclaredDetism),
	io__write_string("', inferred `"),
	hlds_out__write_determinism(InferredDetism),
	io__write_string("'.\n"),
	globals__io_get_globals(Globals),
	{ det_info_init(ModuleInfo, PredId, ProcId, Globals, DetInfo) },
	det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _),
	io__set_exit_status(1).

%-----------------------------------------------------------------------------%

:- pred det_report_seen_call_id(seen_call_id::in, module_info::in,
		io__state::di, io__state::uo) is det.
	
det_report_seen_call_id(SeenCall, ModuleInfo) -->
	(
		{ SeenCall = seen_call(PredId, _) },
		io__write_string("call to "),
		hlds_out__write_pred_id(ModuleInfo, PredId)
	;
		{ SeenCall = higher_order_call },
		io__write_string("higher-order call")
	).
%-----------------------------------------------------------------------------%

:- pred det_report_context_lines(list(term__context), bool, 
		io__state, io__state).
:- mode det_report_context_lines(in, in, di, uo) is det.

det_report_context_lines([], _) --> [].
det_report_context_lines([Context | Contexts], First) -->
	{ term__context_line(Context, Line) },
	( { First = yes } ->
		[]
	; { Contexts = [] } ->
		io__write_string(" and ")
	;
		io__write_string(", ")
	),
	io__write_int(Line),
	det_report_context_lines(Contexts, no).

%-----------------------------------------------------------------------------%
