%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% det_report.m - reporting of determinism errors and warnings.

% author: zs.

%-----------------------------------------------------------------------------%

:- module check_hlds__det_report.

:- interface.

:- import_module check_hlds__det_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

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
		;	has_io_state_but_not_det(pred_id, proc_id).

:- type seen_call_id
	--->	seen_call(pred_id, proc_id)
	;	higher_order_call.

:- type cc_unify_context
	--->	unify(unify_context)
	;	switch.

%-----------------------------------------------------------------------------%

	% Check all the determinism declarations in this module.
	% This is the main predicate exported by this module.

:- pred global_checking_pass(pred_proc_list::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

	% Check a lambda goal with the specified declared and inferred
	% determinisms.

:- pred det_check_lambda(determinism::in, determinism::in, hlds_goal::in,
	hlds_goal_info::in, det_info::in, list(det_msg)::out) is det.

	% Print some determinism warning and/or error messages,
	% and update the module info accordingly.

:- pred det_report_and_handle_msgs(list(det_msg)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

	% Print some determinism warning and/or error messages,
	% and return the number of warnings and errors, so that code
	% somewhere elsewhere can update the module info.

:- pred det_report_msgs(list(det_msg)::in, module_info::in, int::out, int::out,
	io::di, io::uo) is det.


:- type msg_modes
	--->	all_modes	% the warning should be reported only
				% if it occurs in all modes of the predicate
	;	any_mode.	% the warning should be reported
				% if it occurs in any mode of the predicate

	% Return `yes' if the warning should be reported if it occurs in
	% any mode of the predicate, not only if it occurs in all modes.
:- pred det_msg_is_any_mode_msg(det_msg::in, msg_modes::out) is det.

%-----------------------------------------------------------------------------%

:- type options_to_restore.

	% Call this predicate before rerunning determinism analysis
	% after an optimization pass to disable all warnings. Errors will
	% still be reported.
:- pred disable_det_warnings(options_to_restore::out, io::di, io::uo) is det.

:- pred restore_det_warnings(options_to_restore::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type det_comparison	--->	tighter ; sameas ; looser.

:- pred compare_determinisms(determinism::in, determinism::in,
	det_comparison::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_out.

:- import_module assoc_list, bool, int, map, set, std_util, require, string.
:- import_module getopt, term, varset.

%-----------------------------------------------------------------------------%

global_checking_pass([], !ModuleInfo, !IO).
global_checking_pass([proc(PredId, ProcId) | Rest], !ModuleInfo, !IO) :-
	module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo),
	check_determinism(PredId, ProcId, PredInfo, ProcInfo,
		!ModuleInfo, !IO),
	check_determinism_of_main(PredId, ProcId, PredInfo, ProcInfo,
		!ModuleInfo, !IO),
	check_for_multisoln_func(PredId, ProcId, PredInfo, ProcInfo,
		!ModuleInfo, !IO),
	global_checking_pass(Rest, !ModuleInfo, !IO).

:- pred check_determinism(pred_id::in, proc_id::in, pred_info::in,
	proc_info::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

check_determinism(PredId, ProcId, PredInfo0, ProcInfo0, !ModuleInfo, !IO) :-
	proc_info_declared_determinism(ProcInfo0, MaybeDetism),
	proc_info_inferred_determinism(ProcInfo0, InferredDetism),
	(
		MaybeDetism = no
	;
		MaybeDetism = yes(DeclaredDetism),
		compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
		(
			Cmp = sameas
		;
			Cmp = looser,
			globals__io_lookup_bool_option(warn_det_decls_too_lax,
				ShouldIssueWarning, !IO),
			globals__io_lookup_bool_option(warn_inferred_erroneous,
				WarnAboutInferredErroneous, !IO),
			pred_info_get_markers(PredInfo0, Markers),
			(
				ShouldIssueWarning = yes,

				% Don't report warnings for class method
				% implementations -- the determinism in the
				% `:- typeclass' declaration will be
				% the loosest of all possible instances.
				% This is similar to the reason we don't
				% report warnings for lambda expressions.
				\+ check_marker(Markers,
					class_instance_method),

				% Don't report warnings for procedures with
				% no clauses.
				\+ check_marker(Markers, stub),

				% Don't report warnings for compiler-generated
				% Unify, Compare or Index procedures, since the
				% user has no way to shut these up. These can
				% happen for the Unify pred for the unit type,
				% if such types are not boxed (as they are not
				% boxed for the IL backend).
				\+ is_unify_or_compare_pred(PredInfo0),

				% Don't warn about predicates which are
				% inferred erroneous when the appropiate
				% option is set.  This is to avoid
				% warnings about unimplemented
				% predicates.
				(
					WarnAboutInferredErroneous = yes
				;
					WarnAboutInferredErroneous = no,
					InferredDetism \= erroneous
				)
			->
				Message = "  warning: determinism " ++
					"declaration could be tighter.\n",
				report_determinism_problem(PredId, ProcId,
					!.ModuleInfo, Message, DeclaredDetism,
					InferredDetism, !IO)
			;
				true
			)
		;
			Cmp = tighter,
			module_info_incr_errors(!ModuleInfo),
			Message = "  error: determinism declaration " ++
				"not satisfied.\n",
			report_determinism_problem(PredId, ProcId,
				!.ModuleInfo, Message, DeclaredDetism,
				InferredDetism, !IO),
			proc_info_goal(ProcInfo0, Goal),
			proc_info_vartypes(ProcInfo0, VarTypes),
			globals__io_get_globals(Globals, !IO),
			det_info_init(!.ModuleInfo, VarTypes, PredId, ProcId,
				Globals, DetInfo),
			det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo,
				_, !IO)
			% XXX with the right verbosity options, we want to
			% call report_determinism_problem only if diagnose
			% returns false, i.e. it didn't print a message.
		)
	),

	% make sure the code model is valid given the eval method
	proc_info_eval_method(ProcInfo0, EvalMethod),
	(
		valid_determinism_for_eval_method(EvalMethod,
			InferredDetism) = yes
	->
		proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo),
		pred_info_procedures(PredInfo0, ProcTable0),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
		pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
		module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
	;
		proc_info_context(ProcInfo0, Context),
		write_error_pieces(Context, 0,
			[words("Error: `pragma "
				++ eval_method_to_string(EvalMethod)
				++ "'"),
			words("declaration not allowed for"),
			words("with determinism `"
				++ determinism_to_string(InferredDetism)
				++ "'.")], !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		(
			VerboseErrors = yes,
			solutions(get_valid_dets(EvalMethod), Detisms),
			DetismStrs = list__map(determinism_to_string, Detisms),
			list_to_pieces(DetismStrs, DetismPieces),
			write_error_pieces_not_first_line(Context, 0,
				[words("The pragma requested is only valid"),
				words("for the following determinism(s):") |
				DetismPieces], !IO)
		;
			VerboseErrors = no
		),
		module_info_incr_errors(!ModuleInfo)
	).

:- pred get_valid_dets(eval_method::in, determinism::out) is nondet.

get_valid_dets(EvalMethod, Detism) :-
	determinism(Detism),
	valid_determinism_for_eval_method(EvalMethod, Detism) = yes.

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

:- pred check_determinism_of_main(pred_id::in, proc_id::in,
	pred_info::in, proc_info::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

check_determinism_of_main(_PredId, _ProcId, PredInfo, ProcInfo,
		!ModuleInfo, !IO) :-
	%
	% check that `main/2' has determinism `det' or `cc_multi',
	% as required by the language reference manual
	%
	proc_info_declared_determinism(ProcInfo, MaybeDetism),
	(
		pred_info_name(PredInfo) = "main",
		pred_info_arity(PredInfo) = 2,
		pred_info_is_exported(PredInfo),
		MaybeDetism = yes(DeclaredDetism),
		DeclaredDetism \= det,
		DeclaredDetism \= cc_multidet
	->
		proc_info_context(ProcInfo, Context1),
		write_error_pieces(Context1, 0, 
			[words("Error: main/2 must be " ++
				"`det' or `cc_multi'.")], !IO),
		module_info_incr_errors(!ModuleInfo)
	;
		true
	).

:- pred check_for_multisoln_func(pred_id::in, proc_id::in, pred_info::in,
	proc_info::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

check_for_multisoln_func(PredId, _ProcId, PredInfo, ProcInfo,
		!ModuleInfo, !IO) :-
	proc_info_inferred_determinism(ProcInfo, InferredDetism),

	% Functions can only have more than one solution if it is a
	% non-standard mode.  Otherwise, they would not be referentially
	% transparent.  (Nondeterministic "functions" like C's `rand()'
	% function are not allowed.)
	(
		% if it is a mode for a function...
		pred_info_is_pred_or_func(PredInfo) = function,
		% ... that can succeed more than once ...
		determinism_components(InferredDetism, _CanFail, NumSolns),
		NumSolns \= at_most_zero,
		NumSolns \= at_most_one,
		% ... but for which all the arguments are input ...
		proc_info_argmodes(ProcInfo, PredArgModes),
		pred_args_to_func_args(PredArgModes,
			FuncArgModes, _FuncResultMode),
		\+ (
			list__member(FuncArgMode, FuncArgModes),
			\+ mode_is_fully_input(!.ModuleInfo, FuncArgMode)
		)
	->
		% ... then it is an error.
		proc_info_context(ProcInfo, FuncContext),
		proc_info_inst_varset(ProcInfo, InstVarSet),
		describe_one_pred_name_mode(!.ModuleInfo,
			should_not_module_qualify, PredId, InstVarSet,
			PredArgModes, PredModeDesc),
		Pieces = [words("Error: invalid determinism for"),
			fixed(PredModeDesc ++ ":"), nl,
			words("the primary mode of a function cannot be `" ++
				mercury_det_to_string(InferredDetism) ++
				"'.")],
		write_error_pieces(FuncContext, 0, Pieces, !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		( VerboseErrors = yes ->
			ExtMsg = func_primary_mode_det_msg,
			write_error_pieces_not_first_line(FuncContext, 0,
				[words(ExtMsg)], !IO)
		;
			true
		),
		module_info_incr_errors(!ModuleInfo)
	;
		true
	).

:- func func_primary_mode_det_msg = string.

func_primary_mode_det_msg =
	"In Mercury, a function is supposed to be a true mathematical" ++
	"function of its arguments; that is, the value of the function's" ++
	"result should be determined only by the values of its arguments." ++
	"(Allowing functions to have more than one result for the same" ++
	"arguments would break referential transparency.)" ++
	"Most likely, this procedure should be a predicate, not a function.".

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

:- pred report_determinism_problem(pred_id::in, proc_id::in, module_info::in,
	string::in, determinism::in, determinism::in, io::di, io::uo) is det.

report_determinism_problem(PredId, ProcId, ModuleInfo, Message,
		DeclaredDetism, InferredDetism, !IO) :-
	record_warning(!IO),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_context(ProcInfo, Context),
	describe_one_proc_name_mode(ModuleInfo, should_not_module_qualify,
		proc(PredId, ProcId), Desc),
	Pieces = [words("In " ++ Desc ++ ":"), nl,
		words(Message), nl,
		words("Declared `"
			++ determinism_to_string(DeclaredDetism)
			++ "', inferred `"
			++ determinism_to_string(InferredDetism)
			++ "'.")],
	write_error_pieces(Context, 0, Pieces, !IO).

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

:- pred compare_canfails(can_fail::in, can_fail::in, det_comparison::out)
	is det.

compare_canfails(cannot_fail, cannot_fail, sameas).
compare_canfails(cannot_fail, can_fail,    tighter).
compare_canfails(can_fail,    cannot_fail, looser).
compare_canfails(can_fail,    can_fail,    sameas).

:- pred compare_solncounts(soln_count::in, soln_count::in, det_comparison::out)
	is det.

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

:- pred det_diagnose_goal(hlds_goal::in, determinism::in,
	list(switch_context)::in, det_info::in, bool::out, io::di, io::uo)
	is det.

det_diagnose_goal(Goal - GoalInfo, Desired, SwitchContext, DetInfo,
		Diagnosed, !IO) :-
	goal_info_get_determinism(GoalInfo, Actual),
	( compare_determinisms(Desired, Actual, tighter) ->
		det_diagnose_goal_2(Goal, GoalInfo, Desired, Actual,
			SwitchContext, DetInfo, Diagnosed, !IO)
	;
		Diagnosed = no
	).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	determinism::in, determinism::in, list(switch_context)::in,
	det_info::in, bool::out, io::di, io::uo) is det.

det_diagnose_goal_2(conj(Goals), _GoalInfo, Desired, _Actual, Context, DetInfo,
		Diagnosed, !IO) :-
	det_diagnose_conj(Goals, Desired, Context, DetInfo, Diagnosed, !IO).

det_diagnose_goal_2(par_conj(Goals), _GoalInfo, Desired, _Actual,
		Context, DetInfo, Diagnosed, !IO) :-
	det_diagnose_conj(Goals, Desired, Context, DetInfo, Diagnosed, !IO).

det_diagnose_goal_2(disj(Goals), GoalInfo, Desired, Actual, SwitchContext,
		DetInfo, Diagnosed, !IO) :-
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo, 0,
		ClausesWithSoln, Diagnosed1, !IO),
	determinism_components(Desired, _, DesSolns),
	(
		DesSolns \= at_most_many,
		DesSolns \= at_most_many_cc,
		ClausesWithSoln > 1
	->
		goal_info_get_context(GoalInfo, Context),
		Msg = "Disjunction has multiple clauses with solutions.",
		write_error_pieces(Context, 2, [words(Msg)], !IO),
		Diagnosed = yes
	;
		Diagnosed = Diagnosed1
	).

	% The determinism of a switch is the worst of the determinism of each of
	% the cases. Also, if only a subset of the constructors are handled,
	% then it is semideterministic or worse - this is determined
	% in switch_detection.m and handled via the CanFail field.

det_diagnose_goal_2(switch(Var, SwitchCanFail, Cases), GoalInfo,
		Desired, _Actual, SwitchContext, DetInfo, Diagnosed, !IO) :-
	(
		SwitchCanFail = can_fail,
		determinism_components(Desired, cannot_fail, _)
	->
		goal_info_get_context(GoalInfo, Context),
		det_diagnose_write_switch_context(Context, SwitchContext,
			DetInfo, yes, IsFirst, !IO),
		det_get_proc_info(DetInfo, ProcInfo),
		proc_info_varset(ProcInfo, VarSet),
		det_info_get_module_info(DetInfo, ModuleInfo),
		VarStr = mercury_var_to_string(Var, VarSet, no),
		(
			det_lookup_var_type(ModuleInfo, ProcInfo, Var,
				TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			ConsTable = TypeBody ^ du_type_cons_tag_values
		->
			map__keys(ConsTable, ConsIds),
			det_diagnose_missing_consids(ConsIds, Cases, Missing),
			cons_id_list_to_pieces(Missing, yes, MissingPieces),
			list__append(MissingPieces, [words(".")],
				PiecesTail),
			Pieces = [words("The switch on "), fixed(VarStr),
				words("does not cover") | PiecesTail]
		;
			Pieces = [words("The switch on "), fixed(VarStr),
				words("can fail.")]
		),
		write_error_pieces_maybe_first_line(IsFirst, Context, 0,
			Pieces, !IO),
		Diagnosed1 = yes
	;
		Diagnosed1 = no
	),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext, DetInfo,
		Diagnosed2, !IO),
	bool__or(Diagnosed1, Diagnosed2, Diagnosed).

det_diagnose_goal_2(call(PredId, ModeId, _, _, CallContext, _), GoalInfo,
		Desired, Actual, _, DetInfo, yes, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	det_diagnose_atomic_goal(Desired, Actual,
		det_report_call_context(Context, CallContext, DetInfo,
			PredId, ModeId),
		Context, !IO).

det_diagnose_goal_2(generic_call(GenericCall, _, _, _), GoalInfo,
		Desired, Actual, _, _DetInfo, yes, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	det_diagnose_atomic_goal(Desired, Actual,
		report_generic_call_context(Context, GenericCall),
		Context, !IO).

det_diagnose_goal_2(unify(LHS, RHS, _, _, UnifyContext), GoalInfo,
		Desired, Actual, _, DetInfo, yes, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	( First = yes, Last = yes ),
	det_diagnose_atomic_goal(Desired, Actual,
		det_report_unify_context(First, Last, Context, UnifyContext,
			DetInfo, LHS, RHS), Context, !IO).

det_diagnose_goal_2(if_then_else(_Vars, Cond, Then, Else), _GoalInfo,
		Desired, _Actual, SwitchContext, DetInfo, Diagnosed, !IO) :-
	determinism_components(Desired, _DesiredCanFail, DesiredSolns),
	Cond = _CondGoal - CondInfo,
	goal_info_get_determinism(CondInfo, CondDetism),
	determinism_components(CondDetism, _CondCanFail, CondSolns),
	(
		CondSolns = at_most_many,
		DesiredSolns \= at_most_many
	->
		determinism_components(DesiredCond, can_fail, DesiredSolns),
		det_diagnose_goal(Cond, DesiredCond, SwitchContext, DetInfo,
			Diagnosed1, !IO)
	;
		Diagnosed1 = no
	),
	det_diagnose_goal(Then, Desired, SwitchContext, DetInfo, Diagnosed2,
		!IO),
	det_diagnose_goal(Else, Desired, SwitchContext, DetInfo, Diagnosed3,
		!IO),
	bool__or(Diagnosed2, Diagnosed3, Diagnosed23),
	bool__or(Diagnosed1, Diagnosed23, Diagnosed).

det_diagnose_goal_2(not(_), GoalInfo, Desired, Actual, _, _, Diagnosed, !IO) :-
	determinism_components(Desired, DesiredCanFail, DesiredSolns),
	determinism_components(Actual, ActualCanFail, ActualSolns),
	(
		DesiredCanFail = cannot_fail,
		ActualCanFail = can_fail
	->
		goal_info_get_context(GoalInfo, Context),
		write_error_pieces(Context, 0,
			[words("Negated goal can succeed.")], !IO),
		Diagnosed = yes
	;
		DesiredSolns = at_most_zero,
		ActualSolns \= at_most_zero
	->
		goal_info_get_context(GoalInfo, Context),
		write_error_pieces(Context, 0,
			[words("Negated goal can fail.")], !IO),
		Diagnosed = yes
	;
		Diagnosed = no
	).

det_diagnose_goal_2(some(_Vars, _, Goal), _, Desired, Actual,
		SwitchContext, DetInfo, Diagnosed, !IO) :-
	Goal = _ - GoalInfo,
	goal_info_get_determinism(GoalInfo, Internal),
	( Actual = Internal ->
		InternalDesired = Desired
	;
		determinism_components(Desired, CanFail, _),
		determinism_components(InternalDesired, CanFail, at_most_many)
	),
	det_diagnose_goal(Goal, InternalDesired, SwitchContext, DetInfo,
		Diagnosed, !IO).

det_diagnose_goal_2(foreign_proc(_, _, _, _, _, _, _), GoalInfo,
		Desired, _, _, _, yes, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	DesiredStr = determinism_to_string(Desired),
	Pieces = [words("Determinism declaration not satisfied."),
		words("Desired determinism is " ++ DesiredStr ++ ".")],
	write_error_pieces(Context, 0, Pieces, !IO).

det_diagnose_goal_2(shorthand(_), _, _, _, _, _, _, !IO) :-
	% these should have been expanded out by now
	error("det_diagnose_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred report_generic_call_context(prog_context::in, generic_call::in,
	io::di, io::uo) is det.

report_generic_call_context(Context, CallType, !IO) :-
	hlds_goal__generic_call_id(CallType, CallId),
	write_error_pieces(Context, 0, [words(call_id_to_string(CallId))],
		!IO).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_atomic_goal(determinism::in, determinism::in,
	pred(io, io)::in(pred(di, uo) is det), prog_context::in,
	io::di, io::uo) is det.

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

:- pred det_diagnose_conj(list(hlds_goal)::in, determinism::in,
	list(switch_context)::in, det_info::in, bool::out, io::di, io::uo)
	is det.

det_diagnose_conj([], _Desired, _SwitchContext, _DetInfo, no) --> [].
det_diagnose_conj([Goal | Goals], Desired, SwitchContext, DetInfo,
		Diagnosed) -->
	det_diagnose_goal(Goal, Desired, SwitchContext, DetInfo, Diagnosed1),
	det_diagnose_conj(Goals, Desired, SwitchContext, DetInfo, Diagnosed2),
	{ bool__or(Diagnosed1, Diagnosed2, Diagnosed) }.

:- pred det_diagnose_disj(list(hlds_goal)::in, determinism::in, determinism::in,
	list(switch_context)::in, det_info::in, int::in, int::out, bool::out,
	io::di, io::uo) is det.

det_diagnose_disj([], _Desired, _Actual, _SwitchContext, _DetInfo,
		!ClausesWithSoln, no, !IO).
det_diagnose_disj([Goal | Goals], Desired, Actual, SwitchContext, DetInfo,
		!ClausesWithSoln, Diagnosed, !IO) :-
	determinism_components(Actual, ActualCanFail, _),
	determinism_components(Desired, DesiredCanFail, DesiredSolns),
	( DesiredCanFail = cannot_fail, ActualCanFail = can_fail ->
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
	),
	determinism_components(ClauseDesired, ClauseCanFail, DesiredSolns),
	det_diagnose_goal(Goal, ClauseDesired, SwitchContext, DetInfo,
		Diagnosed1, !IO),
	(
		Goal = _ - GoalInfo,
		goal_info_get_determinism(GoalInfo, GoalDetism),
		determinism_components(GoalDetism, _, at_most_zero)
	->
		true
	;
		!:ClausesWithSoln = !.ClausesWithSoln + 1
	),
	det_diagnose_disj(Goals, Desired, Actual, SwitchContext, DetInfo,
		!ClausesWithSoln, Diagnosed2, !IO),
	bool__or(Diagnosed1, Diagnosed2, Diagnosed).

:- pred det_diagnose_switch(prog_var::in, list(case)::in, determinism::in,
	list(switch_context)::in, det_info::in, bool::out,
	io::di, io::uo) is det.

det_diagnose_switch(_Var, [], _Desired, _SwitchContext, _DetInfo, no, !IO).
det_diagnose_switch(Var, [case(ConsId, Goal) | Cases], Desired,
		SwitchContext0, DetInfo, Diagnosed, !IO) :-
	SwitchContext1 = [switch_context(Var, ConsId) | SwitchContext0],
	det_diagnose_goal(Goal, Desired, SwitchContext1, DetInfo, Diagnosed1,
		!IO),
	det_diagnose_switch(Var, Cases, Desired, SwitchContext0, DetInfo,
		Diagnosed2, !IO),
	bool__or(Diagnosed1, Diagnosed2, Diagnosed).

%-----------------------------------------------------------------------------%

:- pred det_diagnose_missing_consids(list(cons_id)::in, list(case)::in,
	list(cons_id)::out) is det.

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

:- pred cons_id_list_to_pieces(list(cons_id)::in, bool::in,
	list(format_component)::out) is det.

cons_id_list_to_pieces([], _, []).
cons_id_list_to_pieces([ConsId | ConsIds], First, Pieces) :-
	ConsIdStr = cons_id_to_string(ConsId),
	( First = yes ->
		PiecesHead = [words(ConsIdStr)]
	; ConsIds = [] ->
		PiecesHead = [words(ConsIdStr), words(" and/or ")]
	;
		PiecesHead = [words(ConsIdStr ++ ",")]
	),
	cons_id_list_to_pieces(ConsIds, no, PiecesTail),
	list__append(PiecesHead, PiecesTail, Pieces).

%-----------------------------------------------------------------------------%

:- type switch_context --->	switch_context(prog_var, cons_id).

:- pred det_diagnose_write_switch_context(prog_context::in,
	list(switch_context)::in, det_info::in, bool::in, bool::out,
	io::di, io::uo) is det.

det_diagnose_write_switch_context(_Context, [], _, !IsFirst, !IO).
det_diagnose_write_switch_context(Context, [SwitchContext | SwitchContexts],
		DetInfo, !IsFirst, !IO) :-
	det_get_proc_info(DetInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	SwitchContext = switch_context(Var, ConsId),
	ConsIdStr = cons_id_to_string(ConsId),
	VarStr = mercury_var_to_string(Var, VarSet, no),
	Pieces = [words("Inside the case"), fixed(ConsIdStr),
		words("of the switch on"), fixed(VarStr), words(":")],
	write_error_pieces_maybe_first_line(!.IsFirst, Context, 0, Pieces,
		!IO),
	!:IsFirst = no,
	det_diagnose_write_switch_context(Context, SwitchContexts, DetInfo,
		!IsFirst, !IO).

%-----------------------------------------------------------------------------%

:- pred det_report_call_context(prog_context::in,
	maybe(call_unify_context)::in, det_info::in, pred_id::in, proc_id::in,
	io::di, io::uo) is det.

det_report_call_context(Context, CallUnifyContext, DetInfo, PredId, ProcId,
		!IO) :-
	det_info_get_module_info(DetInfo, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	PredName = pred_info_name(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_get_maybe_special_pred(PredInfo, MaybeSpecial),
	%
	% if the error was in a call to a type-specific unification predicate
	% (i.e. in the unification itself), then don't print out the predicate
	% name, just print out the context.  If it wasn't, then print them
	% both out. (The latter can happen if there is a determinism error
	% in a function call inside some unification.)
	%
	( MaybeSpecial = yes(unify - _) ->
		(
			CallUnifyContext = yes(
				call_unify_context(LHS, RHS, UC)),
			First = yes, Last = yes,
			det_report_unify_context(First, Last, Context, UC,
				DetInfo, LHS, RHS, !IO)
		;
			% this shouldn't happen; every call to a compiler
			% generated type-specific unification predicate
			% should have a unify_context
			CallUnifyContext = no,
			write_error_pieces(Context, 0,
				[words("Some weird unification"
					++ "(or explicit call to a"
					++ "type-specific unify predicate?)")],
				!IO)
		)
	;
		(
			CallUnifyContext = yes(
				call_unify_context(LHS, RHS, UC)),
			First = yes,
			Last = no,
			det_report_unify_context(First, Last, Context, UC,
				DetInfo, LHS, RHS, !IO),
			io__write_string(":\n", !IO)
		;
			CallUnifyContext = no
		),
		pred_info_procedures(PredInfo, ProcTable),
		map__lookup(ProcTable, ProcId, ProcInfo),
		proc_info_declared_argmodes(ProcInfo, ArgModes),
		prog_out__write_context(Context, !IO),
		io__write_string("  call to `", !IO),
		report_pred_name_mode(PredOrFunc, PredName, ArgModes, !IO),
		io__write_string("'", !IO)
	).

%-----------------------------------------------------------------------------%

% det_report_unify_context prints out information about the context of an
% error, i.e. where the error occurred.
% The first two arguments are boolean flags that specify whether this is
% the first part of a sentence (in which case we start the error message
% with a capital letter) and whether it is the last part (in which case we
% omit the word "in" on the final "... in unification ...").

:- pred det_report_unify_context(bool::in, bool::in, prog_context::in,
	unify_context::in, det_info::in, prog_var::in, unify_rhs::in,
	io::di, io::uo) is det.

det_report_unify_context(First0, Last, Context, UnifyContext, DetInfo,
		LHS, RHS, !IO) :-
	hlds_out__write_unify_context(First0, UnifyContext, Context, First,
		!IO),
	prog_out__write_context(Context, !IO),
	det_get_proc_info(DetInfo, ProcInfo),
	proc_info_varset(ProcInfo, VarSet),
	det_info_get_module_info(DetInfo, ModuleInfo),
		% We don't have the inst varset - it's not in the
		% proc_info, so we'll just make one up....
	varset__init(InstVarSet),
	( First = yes ->
		( Last = yes ->
			io__write_string("  Unification ", !IO)
		;
			io__write_string("  In unification ", !IO)
		)
	;
		( Last = yes ->
			io__write_string("  unification ", !IO)
		;
			io__write_string("  in unification ", !IO)
		)
	),
	( varset__search_name(VarSet, LHS, _) ->
		(
			RHS = var(RV),
			\+ varset__search_name(VarSet, RV, _)
		->
			io__write_string("with `", !IO),
			mercury_output_var(LHS, VarSet, no, !IO),
			io__write_string("'", !IO)
		;
			io__write_string("of `", !IO),
			mercury_output_var(LHS, VarSet, no, !IO),
			io__write_string("' and `", !IO),
			hlds_out__write_unify_rhs(RHS, ModuleInfo, VarSet,
				InstVarSet, no, 3, !IO),
			io__write_string("'", !IO)
		)
	;
		io__write_string("with `", !IO),
		hlds_out__write_unify_rhs(RHS, ModuleInfo, VarSet, InstVarSet,
			no, 3, !IO),
		io__write_string("'", !IO)
	).

%-----------------------------------------------------------------------------%

:- type det_msg_type	--->	simple_code_warning ; call_warning ; error.

det_report_and_handle_msgs(Msgs, !ModuleInfo, !IO) :-
	( Msgs = [] ->
		% fast path for the usual case
		true
	;
		det_report_msgs(Msgs, !.ModuleInfo, WarnCnt, ErrCnt, !IO),
		globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
		(
			(
				ErrCnt > 0
			;
				WarnCnt > 0,
				HaltAtWarn = yes
			)
		->
			io__set_exit_status(1, !IO),
			module_info_incr_errors(!ModuleInfo)
		;
			true
		)
	).

det_report_msgs(Msgs, ModuleInfo, WarnCnt, ErrCnt, !IO) :-
	globals__io_lookup_bool_option(warn_simple_code, WarnSimple, !IO),
	globals__io_lookup_bool_option(warn_duplicate_calls, WarnCalls, !IO),
	det_report_msgs_2(Msgs, WarnSimple, WarnCalls, ModuleInfo,
		0, WarnCnt, 0, ErrCnt, !IO).

:- pred det_report_msgs_2(list(det_msg)::in, bool::in, bool::in,
	module_info::in, int::in, int::out, int::in, int::out,
	io::di, io::uo) is det.

det_report_msgs_2([], _, _, _ModuleInfo, !WarnCnt, !ErrCnt, !IO).
det_report_msgs_2([Msg | Msgs], WarnSimple, WarnCalls, ModuleInfo,
		!WarnCnt, !ErrCnt, !IO) :-
	det_msg_get_type(Msg, MsgType),
	( WarnSimple = no, MsgType = simple_code_warning ->
		true
	; WarnCalls = no, MsgType = call_warning ->
		true
	;
		det_report_msg(Msg, ModuleInfo, !IO),
		(
			MsgType = simple_code_warning,
			!:WarnCnt = !.WarnCnt + 1
		;
			MsgType = call_warning,
			!:WarnCnt = !.WarnCnt + 1
		;
			MsgType = error,
			!:ErrCnt = !.ErrCnt + 1
		)
	),
	det_report_msgs_2(Msgs, WarnSimple, WarnCalls, ModuleInfo,
		!WarnCnt, !ErrCnt, !IO).

:- pred det_msg_get_type(det_msg::in, det_msg_type::out) is det.

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
det_msg_get_type(has_io_state_but_not_det(_, _), error).

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
det_msg_is_any_mode_msg(has_io_state_but_not_det(_, _), any_mode).

:- pred det_report_msg(det_msg::in, module_info::in, io::di, io::uo) is det.

det_report_msg(multidet_disj(Context, DisjunctContexts), _, !IO) :-
	Pieces = [words("Warning: the disjunction with arms on lines"),
		words(det_report_context_lines(DisjunctContexts)),
		words("has no outputs, but can succeed more than once.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(det_disj(Context, DisjunctContexts), _, !IO) :-
	Pieces = [words("Warning: the disjunction with arms on lines"),
		words(det_report_context_lines(DisjunctContexts)),
		words("will succeed exactly once.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(semidet_disj(Context, DisjunctContexts), _, !IO) :-
	Pieces = [words("Warning: the disjunction with arms on lines"),
		words(det_report_context_lines(DisjunctContexts)),
		words("is semidet, yet it has an output.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(zero_soln_disj(Context, DisjunctContexts), _, !IO) :-
	Pieces = [words("Warning: the disjunction with arms on lines"),
		words(det_report_context_lines(DisjunctContexts)),
		words("cannot succeed.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(zero_soln_disjunct(Context), _, !IO) :-
	Pieces = [words("Warning: this disjunct"),
		words("will never have any solutions.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(ite_cond_cannot_fail(Context), _, !IO) :-
	Pieces = [words("Warning: the condition of this if-then-else"),
		words("cannot fail.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(ite_cond_cannot_succeed(Context), _, !IO) :-
	Pieces = [words("Warning: the condition of this if-then-else"),
		words("cannot succeed.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(negated_goal_cannot_fail(Context), _, !IO) :-
	Pieces = [words("Warning: the negated goal cannot fail.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(negated_goal_cannot_succeed(Context), _, !IO) :-
	Pieces = [words("Warning: the negated goal cannot succeed.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(goal_cannot_succeed(Context), _, !IO) :-
	Pieces0 = [words("Warning: this goal cannot succeed.")],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		Pieces1 = [words("The compiler will optimize away this goal,"),
			words("replacing it with `fail'."),
			words("To disable this optimization, use "),
			words("the `--fully-strict' option.")],
		Pieces = Pieces0 ++ Pieces1
	;
		VerboseErrors = no,
		Pieces = Pieces0
	),
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(det_goal_has_no_outputs(Context), _, !IO) :-
	Pieces0 = [words("Warning: det goal has no outputs.")],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		Pieces1 = [words("The compiler will optimize away this goal,"),
			words("replacing it with `true'."),
			words("To disable this optimization, use "),
			words("the `--fully-strict' option.")],
		Pieces = Pieces0 ++ Pieces1
	;
		VerboseErrors = no,
		Pieces = Pieces0
	),
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(warn_obsolete(PredId, Context), ModuleInfo, !IO) :-
	describe_one_pred_name(ModuleInfo, should_module_qualify, PredId,
		PredDesc),
	Pieces = [words("Warning: call to obsolete "),
		fixed(PredDesc ++ ".")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(warn_infinite_recursion(Context), _ModuleInfo, !IO) :-
	% it would be better if we supplied more information than just
	% the line number, e.g. we should print the name of the containing
	% predicate.
	Pieces0 = [words("Warning: recursive call will lead"),
		words("to infinite recursion.")],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		Pieces1 = [words("If this recursive call is executed,"),
			words("the procedure will call itself"),
			words("with exactly the same input arguments,"),
			words("leading to infinite recursion.")],
		Pieces = Pieces0 ++ Pieces1
	;
		VerboseErrors = no,
		Pieces = Pieces0
	),
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(duplicate_call(SeenCall, PrevContext, Context), ModuleInfo,
		!IO) :-
	CallPieces = det_report_seen_call_id(ModuleInfo, SeenCall),
	CurPieces = [words("Warning: redundant") |
		append_punctuation(CallPieces, '.')],
	PrevPieces = [words("Here is the previous") |
		append_punctuation(CallPieces, '.')],
	write_error_pieces(Context, 0, CurPieces, !IO),
	write_error_pieces(PrevContext, 0, PrevPieces, !IO).
det_report_msg(cc_unify_can_fail(GoalInfo, Var, Type, VarSet, GoalContext),
		_ModuleInfo, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	(
		GoalContext = switch,
		VarStr = mercury_var_to_string(Var, VarSet, no),
		Pieces0 = [words("In switch on variable `" ++ VarStr ++ "':"),
			nl]
	;
		GoalContext = unify(UnifyContext),
		hlds_out__unify_context_to_pieces(UnifyContext, [], Pieces0)
	),
	( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
		TypeCtorStr = hlds_out__type_ctor_to_string(TypeCtor)
	;
		error("det_report_msg: type_to_ctor_and_args failed")
	),
	(	
		Pieces0 = [],
		ErrorMsg = "Error:"
	;
		Pieces0 = [_ | _],
		ErrorMsg = "error:"
	),
	Pieces1 = [words(ErrorMsg),
		words("unification for non-canonical type"),
		words("`" ++ TypeCtorStr ++ "'"),
		words("is not guaranteed to succeed.")],
	Pieces = Pieces0 ++ Pieces1,
	write_error_pieces(Context, 0, Pieces, !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		VerbosePieces = [words("Since the type has a user-defined"),
			words("equality predicate, I must presume that"),
			words("there is more than one possible concrete"),
			words("representation for each abstract value"),
			words("of this type. The success of this unification"),
			words("might depend on the choice of concrete"),
			words("representation. Figuring out whether there is"),
			words("a solution to this unification would require"),
			words("backtracking over all possible"),
			words("representations, but I'm not going to do that"),
			words("implicitly. (If that's really what you want,"),
			words("you must do it explicitly.")],
		write_error_pieces_not_first_line(Context, 0, VerbosePieces,
			!IO)
	;
		VerboseErrors = no
	).
det_report_msg(cc_unify_in_wrong_context(GoalInfo, Var, Type, VarSet,
		GoalContext), _ModuleInfo, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	(
		GoalContext = switch,
		VarStr = mercury_var_to_string(Var, VarSet, no),
		Pieces0 = [words("In switch on variable `" ++ VarStr ++ "':"),
			nl]
	;
		GoalContext = unify(UnifyContext),
		hlds_out__unify_context_to_pieces(UnifyContext, [], Pieces0)
	),
	( type_to_ctor_and_args(Type, TypeCtor, _TypeArgs) ->
		TypeCtorStr = hlds_out__type_ctor_to_string(TypeCtor)
	;
		error("det_report_msg: type_to_ctor_and_args failed")
	),
	(	
		Pieces0 = [],
		ErrorMsg = "Error:"
	;
		Pieces0 = [_ | _],
		ErrorMsg = "error:"
	),
	Pieces1 = [words(ErrorMsg),
		words("unification for non-canonical type"),
		words("`" ++ TypeCtorStr ++ "'"),
		words("occurs in a context which requires all solutions.")],
	Pieces = Pieces0 ++ Pieces1,
	write_error_pieces(Context, 0, Pieces, !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		VerbosePieces = [words("Since the type has a user-defined"),
			words("equality predicate, I must presume that"),
			words("there is more than one possible concrete"),
			words("representation for each abstract value"),
			words("of this type. The results of this unification"),
			words("might depend on the choice of concrete"),
			words("representation. Finding all possible"),
			words("solutions to this unification would require"),
			words("backtracking over all possible"),
			words("representations, but I'm not going to do that"),
			words("implicitly. (If that's really what you want,"),
			words("you must do it explicitly.")],
		write_error_pieces_not_first_line(Context, 0, VerbosePieces,
			!IO)
	;
		VerboseErrors = no
	).
det_report_msg(cc_pred_in_wrong_context(GoalInfo, Detism, PredId, _ModeId),
		ModuleInfo, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	describe_one_pred_name(ModuleInfo, should_not_module_qualify, PredId,
		PredDesc),
	DetStr = mercury_det_to_string(Detism),
	Pieces = [words("Error: call to"),
		fixed(PredDesc),
		words("with determinism `" ++ DetStr ++ "'"),
		words("occurs in a context which requires all solutions.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(higher_order_cc_pred_in_wrong_context(GoalInfo, Detism),
		_ModuleInfo, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	DetStr = mercury_det_to_string(Detism),
	Pieces = [words("Error: higher-order call to predicate with"),
		words("determinism `" ++ DetStr ++ "'"),
		words("occurs in a context which requires all solutions.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(error_in_lambda(DeclaredDetism, InferredDetism, Goal, GoalInfo,
			PredId, ProcId), ModuleInfo, !IO) :-
	describe_one_proc_name_mode(ModuleInfo, should_not_module_qualify,
		proc(PredId, ProcId), Desc),
	goal_info_get_context(GoalInfo, Context),
	write_error_pieces(Context, 0, 
		[words("In " ++ Desc ++ ":"), nl,
		words("Determinism error in lambda expression."), nl,
		words("Declared `"
			++ determinism_to_string(DeclaredDetism)
			++ "', inferred `"
			++ determinism_to_string(InferredDetism)
			++ "'.")], !IO),
	globals__io_get_globals(Globals, !IO),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_vartypes(ProcInfo, VarTypes),
	det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals,
		DetInfo),
	det_diagnose_goal(Goal, DeclaredDetism, [], DetInfo, _, !IO).
det_report_msg(par_conj_not_det(InferredDetism, PredId,
		ProcId, GoalInfo, Goals), ModuleInfo, !IO) :-
	goal_info_get_context(GoalInfo, Context),
	determinism_components(InferredDetism, CanFail, MaxSoln),
	(
		CanFail \= cannot_fail
	->
		First = "Error: parallel conjunct may fail."
	;
		MaxSoln = at_most_many
	->
		First = "Error: parallel conjunct may have multiple solutions."
	;
		error("strange determinism error for parallel conjunction")
	),
	Rest = "The current implementation supports only single-solution"
		++ "non-failing parallel conjunctions.",
	write_error_pieces(Context, 0, [words(First), words(Rest)], !IO),
	globals__io_get_globals(Globals, !IO),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_vartypes(ProcInfo, VarTypes),
	det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals,
		DetInfo),
	det_diagnose_conj(Goals, det, [], DetInfo, _, !IO).
det_report_msg(pragma_c_code_without_det_decl(PredId, ProcId),
		ModuleInfo, !IO) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_context(ProcInfo, Context),
	describe_one_proc_name_mode(ModuleInfo, should_not_module_qualify,
		proc(PredId, ProcId), Desc),
	Pieces = [words("In " ++ Desc ++ ":"), nl,
		words("error: `:- pragma c_code(...)' for a procedure"),
		words("without a determinism declaration.")],
	write_error_pieces(Context, 0, Pieces, !IO).
det_report_msg(has_io_state_but_not_det(PredId, ProcId), ModuleInfo, !IO) :-
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_context(ProcInfo, Context),
	describe_one_proc_name_mode(ModuleInfo, should_not_module_qualify,
		proc(PredId, ProcId), Desc),
	Pieces = [words("In " ++ Desc ++ ":"), nl,
		words("error: invalid determinism for a predicate"),
		words("with I/O state arguments.")],
	write_error_pieces(Context, 0, Pieces, !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		VerbosePieces = [words("Valid determinisms are "),
			words("det, cc_multi and erroneous.")],
		write_error_pieces_not_first_line(Context, 0, VerbosePieces,
			!IO)
	;
		VerboseErrors = no
	).

%-----------------------------------------------------------------------------%

:- func det_report_seen_call_id(module_info, seen_call_id)
	= list(format_component).

det_report_seen_call_id(ModuleInfo, SeenCall) = Pieces :-
	(
		SeenCall = seen_call(PredId, _),
		describe_one_pred_name(ModuleInfo, should_module_qualify,
			PredId, PredDesc),
		Pieces = [words("call to"), fixed(PredDesc)]
	;
		SeenCall = higher_order_call,
		Pieces = [words("higher-order call")]
	).

%-----------------------------------------------------------------------------%

:- func det_report_context_lines(list(prog_context)) = string.

det_report_context_lines(Contexts) = det_report_context_lines_2(Contexts, yes).

:- func det_report_context_lines_2(list(prog_context), bool) = string.

det_report_context_lines_2([], _) = "".
det_report_context_lines_2([Context | Contexts], First) = Str :-
	term__context_line(Context, Line),
	( First = yes ->
		Punct = ""
	; Contexts = [] ->
		Punct = " and "
	;
		Punct = ", "
	),
	int_to_string(Line, This),
	Later = det_report_context_lines_2(Contexts, no),
	Str = Punct ++ This ++ Later.

%-----------------------------------------------------------------------------%

:- type options_to_restore == assoc_list(option, option_data).

disable_det_warnings(OptionsToRestore, !IO) :-
	globals__io_lookup_option(warn_simple_code, WarnSimple, !IO),
	globals__io_lookup_option(warn_det_decls_too_lax,
		WarnDeclsTooLax, !IO),
	globals__io_set_option(warn_simple_code, bool(no), !IO),
	globals__io_set_option(warn_det_decls_too_lax, bool(no), !IO),
	OptionsToRestore = [
		warn_simple_code - WarnSimple,
		warn_det_decls_too_lax - WarnDeclsTooLax
	].

restore_det_warnings(OptionsToRestore, !IO) :-
	list__foldl((pred((Option - Value)::in, di, uo) is det -->
		globals__io_set_option(Option, Value)
	), OptionsToRestore, !IO).

%-----------------------------------------------------------------------------%
