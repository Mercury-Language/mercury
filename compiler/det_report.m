%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_report.m - reporting of determinism errors and warnings.

% author: zs.

%-----------------------------------------------------------------------------%

:- module det_report.

:- interface.

:- import_module prog_data.
:- import_module hlds_module, hlds_pred, hlds_goal.
:- import_module det_util.

:- import_module io, list.

:- type det_msg	--->
			% warnings
			multidet_disj(prog_context, list(prog_context))
		;	det_disj(prog_context, list(prog_context))
		;	semidet_disj(prog_context, list(prog_context))
		;	zero_soln_disj(prog_context, list(prog_context))
		;	zero_soln_disjunct(prog_context)
		;	ite_cond_cannot_fail(prog_context)
		;	ite_cond_cannot_succeed(prog_context)
		;	negated_goal_cannot_fail(prog_context)
		;	negated_goal_cannot_succeed(prog_context)
		;	goal_cannot_succeed(prog_context)
		;	det_goal_has_no_outputs(prog_context)
		;	warn_obsolete(pred_id, prog_context)
				% warning about calls to predicates
				% for which there is a `:- pragma obsolete'
				% declaration.
		;	warn_infinite_recursion(prog_context)
				% warning about recursive calls
				% which would cause infinite loops.
		;	duplicate_call(seen_call_id, prog_context,
				prog_context)
				% multiple calls with the same input args.
			% errors
		;	cc_unify_can_fail(hlds_goal_info, prog_var, type,
				prog_varset, cc_unify_context)
		;	cc_unify_in_wrong_context(hlds_goal_info, prog_var,
				type, prog_varset, cc_unify_context)
		;	cc_pred_in_wrong_context(hlds_goal_info, determinism,
				pred_id, proc_id)
		;	higher_order_cc_pred_in_wrong_context(hlds_goal_info,
				determinism)
		;	error_in_lambda(
				determinism, determinism, % declared, inferred
				hlds_goal, hlds_goal_info, pred_id, proc_id)
		;	par_conj_not_det(determinism, pred_id, proc_id,
				hlds_goal_info, list(hlds_goal))
		; 	pragma_c_code_without_det_decl(pred_id, proc_id)
		.

:- type seen_call_id
	--->	seen_call(pred_id, proc_id)
	;	higher_order_call.

:- type cc_unify_context
	--->	unify(unify_context)
	;	switch.

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


:- type msg_modes
	--->    all_modes       % the warning should be reported only
				% if it occurs in all modes of the predicate
	;       any_mode	% the warning should be reported 
				% if it occurs in any mode of the predicate
	.

	% Return `yes' if the warning should be reported if it occurs in
	% any mode of the predicate, not only if it occurs in all modes.
:- pred det_msg_is_any_mode_msg(det_msg::in, msg_modes::out) is det.

%-----------------------------------------------------------------------------%

:- type options_to_restore.

	% Call this predicate before rerunning determinism analysis
	% after an optimization pass to disable all warnings. Errors will
	% still be reported.
:- pred disable_det_warnings(options_to_restore, io__state, io__state).
:- mode disable_det_warnings(out, di, uo) is det.

:- pred restore_det_warnings(options_to_restore, io__state, io__state).
:- mode restore_det_warnings(in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- type det_comparison	--->	tighter ; sameas ; looser.

:- pred compare_determinisms(determinism, determinism, det_comparison).
:- mode compare_determinisms(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_out. 
:- import_module hlds_data, type_util, mode_util, inst_match.
:- import_module hlds_out, mercury_to_mercury.
:- import_module code_util, passes_aux.
:- import_module globals, options.

:- import_module assoc_list, bool, int, map, set, std_util, require, string.
:- import_module getopt, term, varset.

%-----------------------------------------------------------------------------%

global_checking_pass([], ModuleInfo, ModuleInfo) --> [].
global_checking_pass([proc(PredId, ProcId) | Rest], ModuleInfo0, ModuleInfo) -->
	{ module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo, ProcInfo) },
	check_determinism(PredId, ProcId, PredInfo, ProcInfo,
		ModuleInfo0, ModuleInfo1),
	check_determinism_of_main(PredId, ProcId, PredInfo, ProcInfo,
		ModuleInfo1, ModuleInfo2),
	check_for_multisoln_func(PredId, ProcId, PredInfo, ProcInfo,
		ModuleInfo2, ModuleInfo3),
	global_checking_pass(Rest, ModuleInfo3, ModuleInfo).

:- pred check_determinism(pred_id, proc_id, pred_info, proc_info,
		module_info, module_info, io__state, io__state).
:- mode check_determinism(in, in, in, in, in, out, di, uo) is det.

check_determinism(PredId, ProcId, PredInfo0, ProcInfo0,
		ModuleInfo0, ModuleInfo) -->
	{ proc_info_declared_determinism(ProcInfo0, MaybeDetism) },
	{ proc_info_inferred_determinism(ProcInfo0, InferredDetism) },
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
			globals__io_lookup_bool_option(
				warn_inferred_erroneous,
				WarnAboutInferredErroneous),
			{ pred_info_get_markers(PredInfo0, Markers) },
			(
				{ ShouldIssueWarning = yes },

				% Don't report warnings for class method
				% implementations -- the determinism in the
				% `:- typeclass' declaration will be
				% the loosest of all possible instances.
				% This is similar to the reason we don't
				% report warnings for lambda expressions.
				{ \+ check_marker(Markers,
					class_instance_method) },

				% Don't report warnings for compiler-generated
				% Unify, Compare or Index procedures, since the
				% user has no way to shut these up. These can
				% happen for the Unify pred for the unit type,
				% if such types are not boxed (as they are not
				% boxed for the IL backend).
				{ \+ code_util__compiler_generated(PredInfo0) },

				% Don't warn about predicates which are
				% inferred erroneous when the appropiate
				% option is set.  This is to avoid
				% warnings about unimplemented
				% predicates.
				{ WarnAboutInferredErroneous = yes,
					true
				; WarnAboutInferredErroneous = no,
					InferredDetism \= erroneous
				}
			->
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
			{ proc_info_goal(ProcInfo0, Goal) },
			{ proc_info_vartypes(ProcInfo0, VarTypes) },
			globals__io_get_globals(Globals),
			{ det_info_init(ModuleInfo1, VarTypes, PredId, ProcId,
				Globals, DetInfo) },
			det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _)
			% XXX with the right verbosity options, we want to
			% call report_determinism_problem only if diagnose
			% returns false, i.e. it didn't print a message.
		)
	),
	
	% make sure the code model is valid given the eval method
	{ proc_info_eval_method(ProcInfo0, EvalMethod) },
	( 
		{ valid_determinism_for_eval_method(EvalMethod, InferredDetism) }
	->
		{
		    proc_info_set_eval_method(ProcInfo0, EvalMethod, ProcInfo),
		    pred_info_procedures(PredInfo0, ProcTable0),
		    map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
		    pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		    module_info_set_pred_info(ModuleInfo1, PredId, PredInfo, 
		    	ModuleInfo)
		}
	;
		{ proc_info_context(ProcInfo0, Context) },
		prog_out__write_context(Context),
		{ eval_method_to_string(EvalMethod, EvalMethodS) },
		io__write_string("Error: `pragma "),
		io__write_string(EvalMethodS),
		io__write_string("' declaration not allowed for procedure\n"),
		prog_out__write_context(Context),
		io__write_string("  with determinism `"),
		mercury_output_det(InferredDetism),
		io__write_string("'.\n"), 
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			io__write_string(
"\tThe pragma requested is only valid for the folowing determinism(s):\n"),
			{ solutions(get_valid_dets(EvalMethod), Sols) },
			print_dets(Sols)
		;
			[]
		),
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) }
	).

:- pred get_valid_dets(eval_method, determinism).
:- mode get_valid_dets(in, out) is nondet.

get_valid_dets(EvalMethod, Detism) :-
	determinism(Detism),
	valid_determinism_for_eval_method(EvalMethod, Detism).

	% generate all the possible determinisms
:- pred determinism(determinism).
:- mode determinism(out) is multi.
:- mode determinism(in) is det. % to ensure we don't forget any

determinism(det).
determinism(semidet).
determinism(multidet).
determinism(nondet).
determinism(cc_multidet).
determinism(cc_nondet).
determinism(erroneous).
determinism(failure).

:- pred print_dets(list(determinism), io__state, io__state).
:- mode print_dets(in, di, uo) is det.

print_dets([]) --> [].
print_dets([D|Rest]) -->
	io__write_string("\t\t"),
	mercury_output_det(D),
	io__nl,
	print_dets(Rest).
	
:- pred check_determinism_of_main(pred_id, proc_id, pred_info, proc_info,
		module_info, module_info, io__state, io__state).
:- mode check_determinism_of_main(in, in, in, in, in, out, di, uo) is det.

check_determinism_of_main(_PredId, _ProcId, PredInfo, ProcInfo,
		ModuleInfo0, ModuleInfo) -->
	%
	% check that `main/2' has determinism `det' or `cc_multi',
	% as required by the language reference manual
	%
	{ proc_info_declared_determinism(ProcInfo, MaybeDetism) },
	( 
		{ pred_info_name(PredInfo, "main") },
		{ pred_info_arity(PredInfo, 2) },
		{ pred_info_is_exported(PredInfo) },
		{ MaybeDetism = yes(DeclaredDetism) },
		{ DeclaredDetism \= det, DeclaredDetism \= cc_multidet }
	->
		{ proc_info_context(ProcInfo, Context1) },
		prog_out__write_context(Context1),
		io__write_string(
			"Error: main/2 must be `det' or `cc_multi'.\n"),
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }
	;
		{ ModuleInfo = ModuleInfo0 }
	).

:- pred check_for_multisoln_func(pred_id, proc_id, pred_info, proc_info,
		module_info, module_info, io__state, io__state).
:- mode check_for_multisoln_func(in, in, in, in, in, out, di, uo) is det.

check_for_multisoln_func(_PredId, _ProcId, PredInfo, ProcInfo,
		ModuleInfo0, ModuleInfo) -->
	{ proc_info_inferred_determinism(ProcInfo, InferredDetism) },

	% Functions can only have more than one solution if it is a
	% non-standard mode.  Otherwise, they would not be referentially
	% transparent.  (Nondeterministic "functions" like C's `rand()'
	% function are not allowed.)
	(
		% if it is a mode for a function...
		{ pred_info_get_is_pred_or_func(PredInfo, function) },
		% ... that can succeed more than once ...
		{ determinism_components(InferredDetism, _CanFail, NumSolns) },
		{ NumSolns \= at_most_zero },
		{ NumSolns \= at_most_one },
		% ... but for which all the arguments are input ...
		{ proc_info_argmodes(ProcInfo, PredArgModes) },
		{ pred_args_to_func_args(PredArgModes,
			FuncArgModes, _FuncResultMode) },
		{ \+ (
			list__member(FuncArgMode, FuncArgModes),
			\+ mode_is_fully_input(ModuleInfo0, FuncArgMode)
		  )
	 	} 
	->
		% ... then it is an error.
		{ pred_info_name(PredInfo, PredName) },

		{ proc_info_context(ProcInfo, FuncContext) },
		prog_out__write_context(FuncContext),
		io__write_string("Error: invalid determinism for function\n"),
		prog_out__write_context(FuncContext),
		io__write_string("  `"),
		report_pred_name_mode(function, PredName, PredArgModes),
		io__write_string("':\n"),
		prog_out__write_context(FuncContext),
		io__write_string(
			"  the primary mode for a function cannot be `"),
		mercury_output_det(InferredDetism),
		io__write_string(
			"'.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			io__write_strings([
"\tIn Mercury, a function is supposed to be a true mathematical function\n",
"\tof its arguments; that is, the value of the function's result should\n",
"\tbe determined only by the values of its arguments.\n",
"\t(Allowing functions to have more than one result for the same\n",
"\targuments would break referential transparency.)\n",
"\tMost likely, this procedure should be a predicate, not a function.\n"
			])
		;
			[]
		),
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }
	;
		{ ModuleInfo = ModuleInfo0 }
	).

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

det_diagnose_goal_2(par_conj(Goals, _SM), _GoalInfo, Desired, _Actual,
		Context, DetInfo, Diagnosed) -->
	det_diagnose_conj(Goals, Desired, Context, DetInfo, Diagnosed).

det_diagnose_goal_2(disj(Goals, _), GoalInfo, Desired, Actual, SwitchContext,
		DetInfo, Diagnosed) -->
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo, 0,
		ClausesWithSoln, Diagnosed1),
	{ determinism_components(Desired, _, DesSolns) },
	(
		{ DesSolns \= at_most_many },
		{ DesSolns \= at_most_many_cc },
		{ ClausesWithSoln > 1 }
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
		{ proc_info_varset(ProcInfo, Varset) },
		{ det_info_get_module_info(DetInfo, ModuleInfo) },
		(
			{ det_lookup_var_type(ModuleInfo, ProcInfo, Var,
				TypeDefn) },
			{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
			{ TypeBody = du_type(_, ConsTable, _, _) }
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
	det_diagnose_atomic_goal(Desired, Actual,
		det_report_call_context(Context, CallContext, DetInfo,
			PredId, ModeId),
		Context).

det_diagnose_goal_2(generic_call(GenericCall, _, _, _), GoalInfo,
		Desired, Actual, _, _DetInfo, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	det_diagnose_atomic_goal(Desired, Actual,
		report_generic_call_context(Context, GenericCall),
		Context).

det_diagnose_goal_2(unify(LT, RT, _, _, UnifyContext), GoalInfo,
		Desired, Actual, _, DetInfo, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ First = yes, Last = yes },
	det_diagnose_atomic_goal(Desired, Actual,
		det_report_unify_context(First, Last, Context, UnifyContext,
			DetInfo, LT, RT), Context).

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

det_diagnose_goal_2(some(_Vars, _, Goal), _, Desired, Actual,
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

det_diagnose_goal_2(foreign_proc(_, _, _, _, _, _, _), GoalInfo,
		Desired, _, _, _, yes) -->
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  Determinism declaration not satisfied. Desired \n"),
	prog_out__write_context(Context),
	io__write_string("  determinism is "),
	hlds_out__write_determinism(Desired),
	io__write_string(".\n").
	% The "clarification" below is now incorrect.
	% prog_out__write_context(Context),
	% io__write_string("  pragma c_code declarations only allowed\n"),
	% prog_out__write_context(Context),
	% io__write_string("  for modes which don't succeed more than once.\n").

det_diagnose_goal_2(shorthand(_), _, _, _, _, _, _) -->
	% these should have been expanded out by now
	{ error("det_diagnose_goal_2: unexpected shorthand") }.

%-----------------------------------------------------------------------------%

:- pred report_generic_call_context(prog_context::in,
		generic_call::in, io__state::di, io__state::uo) is det.
report_generic_call_context(Context, CallType) -->
	prog_out__write_context(Context),
	io__write_string("  "),
	{ hlds_goal__generic_call_id(CallType, CallId) },
	hlds_out__write_call_id(CallId).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_atomic_goal(determinism, determinism, 
		pred(io__state, io__state), prog_context,
		io__state, io__state).
:- mode det_diagnose_atomic_goal(in, in, pred(di, uo) is det, in,
		di, uo) is det.

det_diagnose_atomic_goal(Desired, Actual, WriteContext, Context) -->
	{ determinism_components(Desired, DesiredCanFail, DesiredSolns) },
	{ determinism_components(Actual, ActualCanFail, ActualSolns) },
	{ compare_canfails(DesiredCanFail, ActualCanFail, CmpCanFail) },
	( { CmpCanFail = tighter } ->
		call(WriteContext),
		io__write_string(" can fail.\n"),
		{ Diagnosed1 = yes }
	;
		{ Diagnosed1 = no }
	),
	{ compare_solncounts(DesiredSolns, ActualSolns, CmpSolns) },
	( { CmpSolns = tighter } ->
		call(WriteContext),
		io__write_string(" can succeed"),
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
		call(WriteContext),
		io__write_string(" has unknown determinism problem;\n"),
		prog_out__write_context(Context),
		io__write_string("  desired determinism is "),
		hlds_out__write_determinism(Desired),
		io__write_string(", while actual determinism is "),
		hlds_out__write_determinism(Actual),
		io__write_string(".\n")
	).

	% det_diagnose_conj is used for both normal [sequential]
	% conjunction and parallel conjunction.

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
		ClausesWithSoln, ClausesWithSoln, no) --> [].
det_diagnose_disj([Goal | Goals], Desired, Actual, SwitchContext, DetInfo,
		ClausesWithSoln0, ClausesWithSoln, Diagnosed) -->
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
	(
		{ Goal = _ - GoalInfo },
		{ goal_info_get_determinism(GoalInfo, GoalDetism) },
		{ determinism_components(GoalDetism, _, at_most_zero) }
	->
		{ ClausesWithSoln1 = ClausesWithSoln0 }
	;
		{ ClausesWithSoln1 is ClausesWithSoln0 + 1 }
	),
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo,
		ClausesWithSoln1, ClausesWithSoln, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_switch(prog_var, list(case), determinism,
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
		io__write_string(" and/or ")
	;
		io__write_string(", ")
	),
	hlds_out__write_cons_id(ConsId),
	det_output_consid_list(ConsIds, no).

%-----------------------------------------------------------------------------%

:- type switch_context --->	switch_context(prog_var, cons_id).

:- pred det_diagnose_write_switch_context(prog_context, list(switch_context),
	det_info, io__state, io__state).
:- mode det_diagnose_write_switch_context(in, in, in, di, uo) is det.

det_diagnose_write_switch_context(_Context, [], _MiscInco) --> [].
det_diagnose_write_switch_context(Context, [SwitchContext | SwitchContexts],
		DetInfo) -->
	prog_out__write_context(Context),
	{ det_get_proc_info(DetInfo, ProcInfo) },
	{ proc_info_varset(ProcInfo, Varset) },
	{ SwitchContext = switch_context(Var, ConsId) },
	io__write_string("  Inside the case "),
	hlds_out__write_cons_id(ConsId),
	io__write_string(" of the switch on "),
	mercury_output_var(Var, Varset, no),
	io__write_string(":\n"),
	det_diagnose_write_switch_context(Context, SwitchContexts, DetInfo).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(prog_context, maybe(call_unify_context),
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
				Context, UC, DetInfo, LT, RT)
		;
			% this shouldn't happen; every call to __Unify__
			% should have a unify_context
			{ CallUnifyContext = no },
			prog_out__write_context(Context),
			io__write_string(
	"  Some weird unification (or explicit call to `__Unify__'?)")
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
		io__write_string("'")
	).

%-----------------------------------------------------------------------------%

% det_report_unify_context prints out information about the context of an
% error, i.e. where the error occurred.
% The first two arguments are boolean flags that specify whether this is
% the first part of a sentence (in which case we start the error message
% with a capital letter) and whether it is the last part (in which case we
% omit the word "in" on the final "... in unification ...").

:- pred det_report_unify_context(bool, bool, prog_context, unify_context,
	det_info, prog_var, unify_rhs, io__state, io__state).
:- mode det_report_unify_context(in, in, in, in, in, in, in, di, uo) is det.

det_report_unify_context(First0, Last, Context, UnifyContext, DetInfo, LT, RT)
		-->
	hlds_out__write_unify_context(First0, UnifyContext, Context, First),
	prog_out__write_context(Context),
	{ det_get_proc_info(DetInfo, ProcInfo) },
	{ proc_info_varset(ProcInfo, Varset) },
	{ det_info_get_module_info(DetInfo, ModuleInfo) },
		% We don't have the inst varset - it's not in the
		% proc_info, so we'll just make one up....
	{ varset__init(InstVarSet) },
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
			hlds_out__write_unify_rhs(RT, ModuleInfo, Varset,
				InstVarSet, no, 3)
		)
	;
		io__write_string("with `"),
		hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, InstVarSet,
			no, 3)
	),
	io__write_string("'").


%-----------------------------------------------------------------------------%

:- type det_msg_type	--->	simple_code_warning ; call_warning ; error.

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
	globals__io_lookup_bool_option(warn_simple_code, WarnSimple),
	globals__io_lookup_bool_option(warn_duplicate_calls, WarnCalls),
	det_report_msgs_2(Msgs, WarnSimple, WarnCalls, ModuleInfo,
		0, WarnCnt, 0, ErrCnt).

:- pred det_report_msgs_2(list(det_msg), bool, bool, module_info, int, int,
	int, int, io__state, io__state).
:- mode det_report_msgs_2(in, in, in, in, in, out, in, out, di, uo) is det.

det_report_msgs_2([], _, _, _ModuleInfo,
		WarnCnt, WarnCnt, ErrCnt, ErrCnt) --> [].
det_report_msgs_2([Msg | Msgs], WarnSimple, WarnCalls, ModuleInfo,
		WarnCnt0, WarnCnt, ErrCnt0, ErrCnt) -->
	{ det_msg_get_type(Msg, MsgType) },
	( { WarnSimple = no, MsgType = simple_code_warning } ->
		{ WarnCnt1 = WarnCnt0 },
		{ ErrCnt1 = ErrCnt0 }
	; { WarnCalls = no, MsgType = call_warning } ->
		{ WarnCnt1 = WarnCnt0 },
		{ ErrCnt1 = ErrCnt0 }
	;
		det_report_msg(Msg, ModuleInfo),
		(
			{ MsgType = simple_code_warning },
			{ WarnCnt1 is WarnCnt0 + 1 },
			{ ErrCnt1 = ErrCnt0 }
		;
			{ MsgType = call_warning },
			{ WarnCnt1 is WarnCnt0 + 1 },
			{ ErrCnt1 = ErrCnt0 }
		;
			{ MsgType = error },
			{ ErrCnt1 is ErrCnt0 + 1 },
			{ WarnCnt1 = WarnCnt0 }
		)
	),
	det_report_msgs_2(Msgs, WarnSimple, WarnCalls, ModuleInfo,
		WarnCnt1, WarnCnt, ErrCnt1, ErrCnt).

:- pred det_msg_get_type(det_msg, det_msg_type).
:- mode det_msg_get_type(in, out) is det.

det_msg_get_type(multidet_disj(_, _), simple_code_warning).
det_msg_get_type(det_disj(_, _), simple_code_warning).
det_msg_get_type(semidet_disj(_, _), simple_code_warning).
det_msg_get_type(zero_soln_disj(_, _), simple_code_warning).
det_msg_get_type(zero_soln_disjunct(_), simple_code_warning).
det_msg_get_type(ite_cond_cannot_fail(_), simple_code_warning).
det_msg_get_type(ite_cond_cannot_succeed(_), simple_code_warning).
det_msg_get_type(negated_goal_cannot_fail(_), simple_code_warning).
det_msg_get_type(negated_goal_cannot_succeed(_), simple_code_warning).
det_msg_get_type(goal_cannot_succeed(_), simple_code_warning).
det_msg_get_type(det_goal_has_no_outputs(_), simple_code_warning).
	% XXX warn_obsolete isn't really a simple code warning.
	% We should add a separate warning type for this.
det_msg_get_type(warn_obsolete(_, _), simple_code_warning).
det_msg_get_type(warn_infinite_recursion(_), simple_code_warning).
det_msg_get_type(duplicate_call(_, _, _), call_warning).
det_msg_get_type(cc_unify_can_fail(_, _, _, _, _), error).
det_msg_get_type(cc_unify_in_wrong_context(_, _, _, _, _), error).
det_msg_get_type(cc_pred_in_wrong_context(_, _, _, _), error).
det_msg_get_type(higher_order_cc_pred_in_wrong_context(_, _), error).
det_msg_get_type(error_in_lambda(_, _, _, _, _, _), error).
det_msg_get_type(par_conj_not_det(_, _, _, _, _), error).
det_msg_get_type(pragma_c_code_without_det_decl(_, _), error).

det_msg_is_any_mode_msg(multidet_disj(_, _), all_modes).
det_msg_is_any_mode_msg(det_disj(_, _), all_modes).
det_msg_is_any_mode_msg(semidet_disj(_, _), all_modes).
det_msg_is_any_mode_msg(zero_soln_disj(_, _), all_modes).
det_msg_is_any_mode_msg(zero_soln_disjunct(_), all_modes).
det_msg_is_any_mode_msg(ite_cond_cannot_fail(_), all_modes).
det_msg_is_any_mode_msg(ite_cond_cannot_succeed(_), all_modes).
det_msg_is_any_mode_msg(negated_goal_cannot_fail(_), all_modes).
det_msg_is_any_mode_msg(negated_goal_cannot_succeed(_), all_modes).
det_msg_is_any_mode_msg(goal_cannot_succeed(_), all_modes).
det_msg_is_any_mode_msg(det_goal_has_no_outputs(_), all_modes).
det_msg_is_any_mode_msg(warn_obsolete(_, _), all_modes).
det_msg_is_any_mode_msg(warn_infinite_recursion(_), any_mode).
det_msg_is_any_mode_msg(duplicate_call(_, _, _), any_mode).
det_msg_is_any_mode_msg(cc_unify_can_fail(_, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(cc_unify_in_wrong_context(_, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(cc_pred_in_wrong_context(_, _, _, _), any_mode).
det_msg_is_any_mode_msg(higher_order_cc_pred_in_wrong_context(_, _), any_mode).
det_msg_is_any_mode_msg(error_in_lambda(_, _, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(par_conj_not_det(_, _, _, _, _), any_mode).
det_msg_is_any_mode_msg(pragma_c_code_without_det_decl(_, _), any_mode).

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
det_report_msg(goal_cannot_succeed(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: this goal cannot succeed.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string(
"\tThe compiler will optimize away this goal, replacing it with `fail'.
\tTo disable this optimization, use the `--fully-strict' option.\n")
	;
		[]
	).
det_report_msg(det_goal_has_no_outputs(Context), _) -->
	prog_out__write_context(Context),
	io__write_string("Warning: det goal has no outputs.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string(
"\tThe compiler will optimize away this goal, replacing it with `true'.
\tTo disable this optimization, use the `--fully-strict' option.\n")
	;
		[]
	).
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
det_report_msg(cc_unify_can_fail(GoalInfo, Var, Type, VarSet, GoalContext),
		_ModuleInfo) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ First0 = yes },
	( { GoalContext = switch },
		prog_out__write_context(Context),
		io__write_string("In switch on variable `"),
		mercury_output_var(Var, VarSet, no),
		io__write_string("':\n"),
		{ First = no }
	; { GoalContext = unify(UnifyContext) },
		hlds_out__write_unify_context(First0, UnifyContext, Context,
			First)
	),
	prog_out__write_context(Context),
	( { First = yes } ->
		io__write_string("Error: ")
	;
		io__write_string("  error: ")
	),
	io__write_string("unification for non-canonical type\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	( { type_to_type_id(Type, TypeId, _TypeArgs) } ->
		hlds_out__write_type_id(TypeId)
	;
		{ error("det_report_message: type_to_type_id failed") }
	),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  is not guaranteed to succeed.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_strings([
"	Since the type has a user-defined equality predicate, I must\n",
"	presume that there is more than one possible concrete\n",
"	representation for each abstract value of this type.  The success\n",
"	of this unification might depend on the choice of concrete\n",
"	representation.  Figuring out whether there is a solution to\n",
"	this unification would require backtracking over all possible\n",
"	representations, but I'm not going to do that implicitly.\n",
"	(If that's really what you want, you must do it explicitly.)\n"
		])
	;
		[]
	),
	io__set_exit_status(1).
det_report_msg(cc_unify_in_wrong_context(GoalInfo, Var, Type, VarSet,
		GoalContext), _ModuleInfo) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ First0 = yes },
	( { GoalContext = switch },
		prog_out__write_context(Context),
		io__write_string("In switch on variable `"),
		mercury_output_var(Var, VarSet, no),
		io__write_string("':\n"),
		{ First = no }
	; { GoalContext = unify(UnifyContext) },
		hlds_out__write_unify_context(First0, UnifyContext, Context,
			First)
	),
	prog_out__write_context(Context),
	( { First = yes } ->
		io__write_string("Error: ")
	;
		io__write_string("  error: ")
	),
	io__write_string("unification for non-canonical type\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	( { type_to_type_id(Type, TypeId, _TypeArgs) } ->
		hlds_out__write_type_id(TypeId)
	;
		{ error("det_report_message: type_to_type_id failed") }
	),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string(
		"  occurs in a context which requires all solutions.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_strings([
"	Since the type has a user-defined equality predicate, I must\n",
"	presume that there is more than one possible concrete\n",
"	representation for each abstract value of this type.  The results\n",
"	of this unification might depend on the choice of concrete\n",
"	representation.  Finding all possible solutions to this\n",
"	unification would require backtracking over all possible\n",
"	representations, but I'm not going to do that implicitly.\n",
"	(If that's really what you want, you must do it explicitly.)\n"
		])
	;
		[]
	),
	io__set_exit_status(1).
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
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals,
		DetInfo) },
	det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _),
	io__set_exit_status(1).
det_report_msg(par_conj_not_det(InferredDetism, PredId,
			ProcId, GoalInfo, Goals), ModuleInfo) -->
	{ goal_info_get_context(GoalInfo, Context) },
	prog_out__write_context(Context),
	{ determinism_components(InferredDetism, CanFail, MaxSoln) },
	(
		{ CanFail \= cannot_fail }
	->
		io__write_string("Error: parallel conjunct may fail.\n")
	;
		{ MaxSoln = at_most_many }
	->
		prog_out__write_context(Context),
		io__write_string("Error: parallel conjunct may have multiple solutions.\n")
	;
		{ error("strange determinism error for parallel conjunction") }
	),
	prog_out__write_context(Context),
	io__write_string(
		"  The current implementation supports only single-solution\n"
	),
	prog_out__write_context(Context),
	io__write_string("  non-failing parallel conjunctions.\n"),
	globals__io_get_globals(Globals),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_vartypes(ProcInfo, VarTypes) },
	{ det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals,
		DetInfo) },
	det_diagnose_conj(Goals, det, [], DetInfo, _),
	io__set_exit_status(1).
det_report_msg(pragma_c_code_without_det_decl(PredId, ProcId),
		ModuleInfo) -->
	report_pred_proc_id(ModuleInfo, PredId, ProcId, no, Context),
	prog_out__write_context(Context),	
	io__write_string("  error: `:- pragma c_code(...)' for a procedure"),
	io__nl,
	prog_out__write_context(Context),	
	io__write_string("  without a determinism declaration."),
	io__nl.

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

:- pred det_report_context_lines(list(prog_context), bool, 
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

:- type options_to_restore == assoc_list(option, option_data).

disable_det_warnings(OptionsToRestore) -->
	globals__io_lookup_option(warn_simple_code, WarnSimple),
	globals__io_lookup_option(warn_det_decls_too_lax,
		WarnDeclsTooLax),
	globals__io_set_option(warn_simple_code, bool(no)),
	globals__io_set_option(warn_det_decls_too_lax, bool(no)),
	{ OptionsToRestore = [
		warn_simple_code - WarnSimple,
		warn_det_decls_too_lax - WarnDeclsTooLax
	] }.

restore_det_warnings(OptionsToRestore) -->
	list__foldl(
	    (pred((Option - Value)::in, di, uo) is det -->
		globals__io_set_option(Option, Value)
	    ), OptionsToRestore).

%-----------------------------------------------------------------------------%
