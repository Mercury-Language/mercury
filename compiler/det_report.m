%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_report.m - reporting of determinism errors and warnings.

% XXX all warnings should be disable-able.  Many of the warnings here aren't.

% author: zs.

%-----------------------------------------------------------------------------%

:- module det_report.

:- interface.

:- import_module hlds, io.

	% Types used in both det_report and det_analysis.

:- type predproclist	==	list(pair(pred_id, proc_id)).

:- type maybe_changed	--->	changed ; unchanged.

:- type misc_info	--->	misc_info(
				% generally useful info:
					module_info,
				% the id of the procedure
				% we are currently processing:
					pred_id,	
					proc_id
				).

:- type det_msg	--->	multidet_disj(hlds__goal_info, list(hlds__goal))
		;	det_disj(hlds__goal_info, list(hlds__goal))
		;	semidet_disj(hlds__goal_info, list(hlds__goal))
		;	zero_soln_disj(hlds__goal_info, list(hlds__goal))
		;	zero_soln_disjunct(hlds__goal_info)
		;	ite_cond_cannot_fail(hlds__goal_info)
		;	cc_pred_in_wrong_context(hlds__goal_info, determinism,
				pred_id, proc_id)
		;	error_in_lambda(
				determinism, determinism, % declared, inferred
				hlds__goal, hlds__goal_info,
				pred_id, proc_id).

%-----------------------------------------------------------------------------%

	% Check all the determinism declarations in this module.
	% This is the main predicate exported by this module.

:- pred global_checking_pass(list(pair(pred_id, proc_id)),
	module_info, module_info, io__state, io__state).
:- mode global_checking_pass(in, in, out, di, uo) is det.

	% Check a lambda goal with the specified declared and inferred
	% determinisms.

:- pred det_check_lambda(determinism, determinism, hlds__goal, hlds__goal_info,
			misc_info, list(det_msg)).
:- mode det_check_lambda(in, in, in, in, in, out) is det.

	% Print some determinism warning messages.

:- pred det_report_msgs(list(det_msg), module_info, io__state, io__state).
:- mode det_report_msgs(in, in, di, uo) is det.

	% Some auxiliary predicates used in both det_report and det_analysis.

:- pred det_lookup_detism(misc_info, pred_id, proc_id, determinism).
:- mode det_lookup_detism(in, in, in, out) is det.

:- pred det_misc_get_proc_info(misc_info, proc_info).
:- mode det_misc_get_proc_info(in, out) is det.

:- pred det_lookup_var_type(module_info, proc_info, var, hlds__type_defn).
:- mode det_lookup_var_type(in, in, in, out) is semidet.

:- pred no_output_vars(set(var), instmap, instmap_delta, misc_info).
:- mode no_output_vars(in, in, in, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, int, list, map, set, varset, std_util, term, require.

:- import_module globals, options, prog_out, hlds_out, mercury_to_mercury.
:- import_module type_util, mode_util, inst_match.

%-----------------------------------------------------------------------------%

global_checking_pass([], ModuleInfo, ModuleInfo) --> [].
global_checking_pass([PredId - ModeId | Rest], ModuleInfo0, ModuleInfo) -->
	{
		module_info_preds(ModuleInfo0, PredTable),
		map__lookup(PredTable, PredId, PredInfo),
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ModeId, ProcInfo),
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
				{ Message = "  Warning: determinism declaration could be tighter.\n" },
				report_determinism_problem(PredId,
					ModeId, ModuleInfo0, Message,
					DeclaredDetism, InferredDetism)
			;
				[]
			),
			{ ModuleInfo1 = ModuleInfo0 }
		;
			{ Cmp = tighter },
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo1) },
			{ Message = "  Error: determinism declaration not satisfied.\n" },
			report_determinism_problem(PredId,
				ModeId, ModuleInfo1, Message,
				DeclaredDetism, InferredDetism),
			{ proc_info_goal(ProcInfo, Goal) },
			{ MiscInfo = misc_info(ModuleInfo1, PredId, ModeId) },
			det_diagnose_goal(Goal, DeclaredDetism, [], MiscInfo, _)
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

det_check_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo, MiscInfo,
		Msgs) :-
	compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
	( Cmp = tighter ->
		MiscInfo = misc_info(_, PredId, ProcId),
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
	det_report_pred_proc_id(ModuleInfo, PredId, ModeId, Context),
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

:- pred det_diagnose_goal(hlds__goal, determinism, list(switch_context),
	misc_info, bool, io__state, io__state).
:- mode det_diagnose_goal(in, in, in, in, out, di, uo) is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, MiscInfo,
		Diagnosed) -->
	{ goal_info_get_determinism(GoalInfo, Actual) },
	( { compare_determinisms(Desired, Actual, tighter) } ->
		det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual,
			SwitchContext, MiscInfo, Diagnosed)
	;
		{ Diagnosed = no }
	).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds__goal_expr, hlds__goal_info,
	determinism, determinism, list(switch_context), misc_info, bool,
	io__state, io__state).
:- mode det_diagnose_goal_2(in, in, in, in, in, in, out, di, uo) is det.

det_diagnose_goal_2(conj(Goals), _GoalInfo, Desired, _Actual, Context, MiscInfo,
		Diagnosed) -->
	det_diagnose_conj(Goals, Desired, Context, MiscInfo, Diagnosed).

det_diagnose_goal_2(disj(Goals), GoalInfo, Desired, _Actual, SwitchContext,
		MiscInfo, Diagnosed) -->
	det_diagnose_disj(Goals, Desired, SwitchContext, MiscInfo, 0, Clauses,
		Diagnosed1),
	{ determinism_components(Desired, _, DesSolns) },
	(
		{ DesSolns \= at_most_many },
		{ Clauses > 1 }
	->
		{ goal_info_context(GoalInfo, Context) },
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

det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases), GoalInfo,
		Desired, _Actual, SwitchContext, MiscInfo, Diagnosed) -->
	(
		{ SwitchCanFail = can_fail },
		{ determinism_components(Desired, cannot_fail, _) }
	->
		{ goal_info_context(GoalInfo, Context) },
		det_diagnose_write_switch_context(Context, SwitchContext,
			MiscInfo),
		prog_out__write_context(Context),
		{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
		{ proc_info_variables(ProcInfo, Varset) },
		{ MiscInfo = misc_info(ModuleInfo, _, _) },
		(
			{ det_lookup_var_type(ModuleInfo, ProcInfo, Var,
				TypeDefn) },
			{ TypeDefn = hlds__type_defn(_, _, TypeBody, _, _) },
			{ TypeBody = du_type(_, ConsTable, _) }
		->
			{ map__keys(ConsTable, ConsIds) },
			{ det_diagnose_missing_consids(ConsIds, Cases,
				Missing) },
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset),
			io__write_string(" does not cover "),
			det_output_consid_list(Missing, yes),
			io__write_string(".\n")
		;
			io__write_string("  The switch on "),
			mercury_output_var(Var, Varset),
			io__write_string(" can fail.\n")
		),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext, MiscInfo,
		Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

det_diagnose_goal_2(call(PredId, ModeId, _, _, CallContext, _, _), GoalInfo,
		Desired, Actual, _, MiscInfo, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	{ compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail) },
	( { CmpCanFail = tighter } ->
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("can fail.\n"),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	{ compare_solncounts(DesiredSolns, ActualSolns, CmpSolns) },
	( { CmpSolns = tighter } ->
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("can succeed"),
		( { DesiredSolns = at_most_one } ->
			io__write_string(" more than once\n.")
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
		det_report_call_context(Context, CallContext, MiscInfo,
			PredId, ModeId),
		io__write_string("has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(unify(LT, RT, _, _, UnifyContext), GoalInfo,
		Desired, Actual, _, MiscInfo, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	{ determinism_components(Desired, DesiredCanFail, _DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, _ActualSolns) },
	det_report_unify_context(Context, UnifyContext, MiscInfo, LT, RT),
	(
		{ DesiredCanFail = cannot_fail },
		{ ActualCanFail = can_fail }
	->
		io__write_string("can fail.\n")
	;
		io__write_string("has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else), _GoalInfo,
		Desired, _Actual, SwitchContext, MiscInfo, Diagnosed) -->
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
		det_diagnose_goal(Cond, DesiredCond, SwitchContext, MiscInfo,
			Diagnosed1)
	;
		{ Diagnosed1 = no }
	),
	det_diagnose_goal(Then, Desired, SwitchContext, MiscInfo, Diagnosed2),
	det_diagnose_goal(Else, Desired, SwitchContext, MiscInfo, Diagnosed3),
	{ bool__or(Diagnosed2, Diagnosed3, Diagnosed23) },
	{ bool__or(Diagnosed1, Diagnosed23, Diagnosed) }.

det_diagnose_goal_2(not(_), GoalInfo, Desired, Actual, _, _, Diagnosed) -->
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	(
		{ DesiredCanFail = cannot_fail },
		{ ActualCanFail = can_fail }
	->
		{ goal_info_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Negated goal can succeed.\n"),
		{ Diagnosed = yes }
	;
		{ DesiredSolns = at_most_zero },
		{ ActualSolns \= at_most_zero }
	->
		{ goal_info_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  Negated goal can fail.\n"),
		{ Diagnosed = yes }
	;
		{ Diagnosed = no }
	).

det_diagnose_goal_2(some(_Vars, Goal), _, Desired, Actual,
		SwitchContext, MiscInfo, Diagnosed) -->
	{ Goal = _ - GoalInfo },
	{ goal_info_get_determinism(GoalInfo, Internal) },
	{ Actual = Internal ->
		InternalDesired = Desired
	;
		determinism_components(Desired, CanFail, _),
		determinism_components(InternalDesired, CanFail, at_most_many)
	},
	det_diagnose_goal(Goal, InternalDesired, SwitchContext, MiscInfo,
		Diagnosed).

det_diagnose_goal_2(pragma_c_code(_, _, _, _, _), GoalInfo, Desired, 
		_, _, _, yes) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  Determinism declaration not satisfied. Desired \n"),
	prog_out__write_context(Context),
	io__write_string("  determinism is "),
	hlds_out__write_determinism(Desired),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	io__write_string("  pragma(c_code, ...) declarations only allowed\n"),
	prog_out__write_context(Context),
	io__write_string("  for deterministic modes.\n").

%-----------------------------------------------------------------------------%

:- pred det_diagnose_conj(list(hlds__goal), determinism,
	list(switch_context), misc_info, bool, io__state, io__state).
:- mode det_diagnose_conj(in, in, in, in, out, di, uo) is det.

det_diagnose_conj([], _Desired, _SwitchContext, _MiscInfo, no) --> [].
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, MiscInfo,
		Diagnosed) -->
	det_diagnose_goal(Goal, Desired, SwitchContext, MiscInfo, Diagnosed1),
	det_diagnose_conj(Goals, Desired, SwitchContext, MiscInfo, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_disj(list(hlds__goal), determinism,
	list(switch_context), misc_info, int, int, bool, io__state, io__state).
:- mode det_diagnose_disj(in, in, in, in, in, out, out, di, uo) is det.

det_diagnose_disj([], _Desired, _SwitchContext, _MiscInfo,
		Clauses, Clauses, no) --> [].
det_diagnose_disj([Goal | Goals], Desired, SwitchContext, MiscInfo,
		Clauses0, Clauses, Diagnosed) -->
	{ determinism_components(Desired, _, DesiredSolns) },
	{ determinism_components(ClauseDesired, can_fail, DesiredSolns) },
	det_diagnose_goal(Goal, ClauseDesired, SwitchContext, MiscInfo,
		Diagnosed1),
	{ Clauses1 is Clauses0 + 1 },
	det_diagnose_disj(Goals, Desired, SwitchContext, MiscInfo,
		Clauses1, Clauses, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_switch(var, list(case), determinism,
	list(switch_context), misc_info, bool, io__state, io__state).
:- mode det_diagnose_switch(in, in, in, in, in, out, di, uo) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _MiscInfo, no) --> [].
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
		SwitchContext0, MiscInfo, Diagnosed) -->
	{ SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0] },
	det_diagnose_goal(Goal, Desired, SwitchContext1, MiscInfo, Diagnosed1),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext0, MiscInfo,
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
	misc_info, io__state, io__state).
:- mode det_diagnose_write_switch_context(in, in, in, di, uo) is det.

det_diagnose_write_switch_context(_Context, [], _MiscInco) --> [].
det_diagnose_write_switch_context(Context, [SwitchContext | SwitchContexts],
		MiscInfo) -->
	prog_out__write_context(Context),
	{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ SwitchContext = switch_context(Var, ConsId) },
	io__write_string("  Inside the case "),
	hlds_out__write_cons_id(ConsId),
	io__write_string(" of the switch on "),
	mercury_output_var(Var, Varset),
	io__write_string(":\n"),
	det_diagnose_write_switch_context(Context, SwitchContexts, MiscInfo).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(term__context, maybe(call_unify_context),
	misc_info, pred_id, proc_id, io__state, io__state).
:- mode det_report_call_context(in, in, in, in, in, di, uo) is det.

det_report_call_context(Context, CallUnifyContext, MiscInfo, PredId, ModeId) -->
	(
		{ CallUnifyContext = yes(call_unify_context(LT, RT, UC)) },
		det_report_unify_context(Context, UC, MiscInfo, LT, RT)
	;
		{ CallUnifyContext = no },
		{ MiscInfo = misc_info(ModuleInfo, _, _) },
		{ module_info_preds(ModuleInfo, PredTable) },
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ map__lookup(PredTable, PredId, PredInfo) },
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ map__lookup(ProcTable, ModeId, ProcInfo) },
		{ proc_info_argmodes(ProcInfo, ArgModes) },
		prog_out__write_context(Context),
		io__write_string("  Call to `"),
		det_report_pred_name_mode(PredName, ArgModes),
		io__write_string("' ")
	).

%-----------------------------------------------------------------------------%

:- pred det_report_unify_context(term__context, unify_context,
	misc_info, var, unify_rhs, io__state, io__state).
:- mode det_report_unify_context(in, in, in, in, in, di, uo) is det.

det_report_unify_context(Context, UnifyContext, MiscInfo, LT, RT) -->
	hlds_out__write_unify_context(UnifyContext, Context),
	prog_out__write_context(Context),
	{ det_misc_get_proc_info(MiscInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	{ MiscInfo = misc_info(ModuleInfo, _, _) },
	(
		{ varset__lookup_name(Varset, LT, _) }
	->
		(
			{ RT = var(RV) },
			\+ { varset__lookup_name(Varset, RV, _) }
		->
			io__write_string("  unification with `"),
			mercury_output_var(LT, Varset)
		;
			io__write_string("  unification of `"),
			mercury_output_var(LT, Varset),
			io__write_string("' and `"),
			hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
		)
	;
		io__write_string("  unification with `"),
		hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
	),
	io__write_string("' ").

%-----------------------------------------------------------------------------%

:- pred det_report_pred_proc_id(module_info, pred_id, proc_id, term__context,
				io__state, io__state).
:- mode det_report_pred_proc_id(in, in, in, out, di, uo) is det.

det_report_pred_proc_id(ModuleInfo, PredId, ProcId, Context) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_arity(PredInfo, Arity) },
	{ proc_info_context(ProcInfo, Context) },
	{ proc_info_argmodes(ProcInfo, ArgModes0) },

	% We need to strip off the extra type_info arguments inserted at the
	% front by polymorphism.m - we only want the last `PredArity' of them.
	%
	{ list__length(ArgModes0, NumArgModes) },
	{ NumToDrop is NumArgModes - Arity },
	( { list__drop(NumToDrop, ArgModes0, ArgModes1) } ->
		{ ArgModes = ArgModes1 }
	;	
		{ error("report_determinism_problem: list__drop failed") }
	),

	prog_out__write_context(Context),
	io__write_string("In `"),
	det_report_pred_name_mode(PredName, ArgModes),
	io__write_string("':\n").

:- pred det_report_pred_name_mode(string, list((mode)), io__state, io__state).
:- mode det_report_pred_name_mode(in, in, di, uo) is det.

det_report_pred_name_mode(PredName, ArgModes) -->
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		{ varset__init(InstVarSet) },	% XXX inst var names
		io__write_string("("),
		mercury_output_mode_list(ArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	).

%-----------------------------------------------------------------------------%

det_report_msgs([], _ModuleInfo) --> [].
det_report_msgs([Msg | Msgs], ModuleInfo) -->
	det_report_msg(Msg, ModuleInfo),
	det_report_msgs(Msgs, ModuleInfo).

:- pred det_report_msg(det_msg, module_info, io__state, io__state).
:- mode det_report_msg(in, in, di, uo) is det.

det_report_msg(multidet_disj(GoalInfo, Disjuncts0), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  has no outputs, but can succeed more than once.\n").
det_report_msg(det_disj(GoalInfo, Disjuncts0), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  will succeed exactly once.\n").
det_report_msg(semidet_disj(GoalInfo, Disjuncts0), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  is semidet, yet it has an output.\n").
det_report_msg(zero_soln_disj(GoalInfo, Disjuncts0), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: the disjunction with arms on lines "),
	{ det_report_sort_context_lines(Disjuncts0, Disjuncts) },
	det_report_context_lines(Disjuncts, yes),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  cannot succeed.\n").
det_report_msg(zero_soln_disjunct(GoalInfo), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: this disjunct will never have any solutions.\n").
det_report_msg(ite_cond_cannot_fail(GoalInfo), _) -->
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Warning: the condition of this if-then-else cannot fail.\n").
det_report_msg(cc_pred_in_wrong_context(GoalInfo, Detism, PredId, ModeId), 
		ModuleInfo) -->
	det_report_pred_proc_id(ModuleInfo, PredId, ModeId, _ProcContext),
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: call to predicate with determinism `"),
	mercury_output_det(Detism),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  occurs in a context which requires all solutions.\n"),
	io__set_exit_status(1).
det_report_msg(error_in_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo,
			PredId, ProcId), ModuleInfo) -->
	det_report_pred_proc_id(ModuleInfo, PredId, ProcId, _ProcContext),
	{ goal_info_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Determinism error in lambda expression.\n"),
	prog_out__write_context(Context),
	io__write_string("  Declared `"),
	hlds_out__write_determinism(DeclaredDetism),
	io__write_string("', inferred `"),
	hlds_out__write_determinism(InferredDetism),
	io__write_string("'.\n"),
	{ MiscInfo = misc_info(ModuleInfo, PredId, ProcId) },
	det_diagnose_goal(Goal, DeclaredDetism, [], MiscInfo, _),
	io__set_exit_status(1).

%-----------------------------------------------------------------------------%

	% Insertion sort is good enough.

:- pred det_report_sort_context_lines(list(hlds__goal), list(hlds__goal)).
:- mode det_report_sort_context_lines(in, out) is det.

det_report_sort_context_lines([], []).
det_report_sort_context_lines([Goal0 | Goals0], Goals) :-
	det_report_sort_context_lines(Goals0, Goals1),
	det_report_insert_context_line(Goals1, Goal0, Goals).

:- pred det_report_insert_context_line(list(hlds__goal), hlds__goal,
	list(hlds__goal)).
:- mode det_report_insert_context_line(in, in, out) is det.

det_report_insert_context_line([], Goal, [Goal]).
det_report_insert_context_line([Goal0 | Goals0], Goal, Goals) :-
	Goal0 = _ - GoalInfo0,
	goal_info_context(GoalInfo0, Context0),
	term__context_line(Context0, Line0),
	Goal = _ - GoalInfo,
	goal_info_context(GoalInfo, Context),
	term__context_line(Context, Line),
	( Line < Line0 ->
		Goals = [Goal, Goal0 | Goals0]
	;
		det_report_insert_context_line(Goals0, Goal, Goals1),
		Goals = [Goal0 | Goals1]
	).

%-----------------------------------------------------------------------------%

:- pred det_report_context_lines(list(hlds__goal), bool, io__state, io__state).
:- mode det_report_context_lines(in, in, di, uo) is det.

det_report_context_lines([], _) --> [].
det_report_context_lines([_ - GoalInfo | Goals], First) -->
	{ goal_info_context(GoalInfo, Context) },
	{ term__context_line(Context, Line) },
	( { First = yes } ->
		[]
	; { Goals = [] } ->
		io__write_string(" and ")
	;
		io__write_string(", ")
	),
	io__write_int(Line),
	det_report_context_lines(Goals, no).

%-----------------------------------------------------------------------------%

	% det_lookup_detism(MiscInfo, PredId, ModeId, Category):
	% 	Given the MiscInfo, and the PredId & ModeId of a procedure,
	% 	look up the determinism of that procedure and return it
	% 	in Category.

det_lookup_detism(MiscInfo, PredId, ModeId, Detism) :-
	MiscInfo = misc_info(ModuleInfo, _, _),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo),
	proc_info_interface_determinism(ProcInfo, Detism).

det_misc_get_proc_info(MiscInfo, ProcInfo) :-
	MiscInfo = misc_info(ModuleInfo, PredId, ModeId),
	module_info_preds(ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ModeId, ProcInfo).

det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn) :-
	proc_info_vartypes(ProcInfo, VarTypes),
	map__lookup(VarTypes, Var, Type),
	(
		type_to_type_id(Type, TypeId, _)
	->
		module_info_types(ModuleInfo, TypeTable),
		map__search(TypeTable, TypeId, TypeDefn)
	;
		error("cannot lookup the type of a variable")
	).

no_output_vars(_, _, unreachable, _).
no_output_vars(Vars, InstMap0, reachable(InstMapDelta), MiscInfo) :-
	set__to_sorted_list(Vars, VarList),
	MiscInfo = misc_info(ModuleInfo, _, _),
	no_output_vars_2(VarList, InstMap0, InstMapDelta, ModuleInfo).

:- pred no_output_vars_2(list(var), instmap, instmapping, module_info).
:- mode no_output_vars_2(in, in, in, in) is semidet.

no_output_vars_2([], _, _, _).
no_output_vars_2([Var | Vars], InstMap0, InstMapDelta, ModuleInfo) :-
	( map__search(InstMapDelta, Var, Inst) ->
		% The instmap delta contains the variable, but the variable may
		% still not be output, if the change is just an increase in
		% information rather than an increase in instantiatedness.
		% We use `inst_matches_binding' to check that the new inst
		% has only added information or lost uniqueness,
		% not bound anything.
		instmap_lookup_var(InstMap0, Var, Inst0),
		inst_matches_binding(Inst, Inst0, ModuleInfo)
	;
		true
	),
	no_output_vars_2(Vars, InstMap0, InstMapDelta, ModuleInfo).

%-----------------------------------------------------------------------------%
