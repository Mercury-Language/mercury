%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_errors.m.
% Main author: fjh.

% This module contains all the error-reporting routines for the
% mode-checker.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__mode_errors.

:- interface.

:- import_module hlds__hlds_data, hlds__hlds_pred, hlds__hlds_module.
:- import_module hlds__hlds_goal.
:- import_module parse_tree__prog_data, check_hlds__mode_info.
:- import_module (parse_tree__inst).

:- import_module bool, set, assoc_list, list, io.

%-----------------------------------------------------------------------------%

:- type merge_context
	---> disj
	;    if_then_else.

:- type merge_errors == assoc_list(prog_var, list(inst)).

:- type delayed_goal
	--->	delayed_goal(
			set(prog_var),	% The vars it's waiting on
			mode_error_info,% The reason it can't be scheduled
			hlds_goal	% The goal itself
		).

:- type mode_error
	--->	mode_error_disj(merge_context, merge_errors)
			% different arms of a disjunction result in
			% different insts for some non-local variables
	;	mode_error_par_conj(merge_errors)
			% different arms of a parallel conj result in
			% mutually exclusive bindings - ie the process
			% of unifying the instmaps from the end of each
			% branch failed.
	;	mode_error_higher_order_pred_var(pred_or_func, prog_var, inst,
				arity)
			% the predicate variable in a higher-order predicate
			% or function call didn't have a higher-order
			% predicate or function inst of the appropriate arity
	;	mode_error_poly_unify(prog_var, inst)
			% A variable in a polymorphic unification with unknown
			% type has inst other than `ground' or `any'.
	;	mode_error_var_is_live(prog_var)
			% call to a predicate which will clobber its argument,
			% but the argument is still live
	;	mode_error_var_has_inst(prog_var, inst, inst)
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with one mode)
	;	mode_error_unify_pred(prog_var, mode_error_unify_rhs, type,
				pred_or_func)
			% an attempt was made to unify two higher-order
			% predicate or function variables.
	;	mode_error_implied_mode(prog_var, inst, inst)
			% a call to a predicate with an overly
			% instantiated variable would use an implied
			% mode of the predicate.  XXX This is temporary - 
			% once we've implemented implied modes we can
			% get rid of it.
	;	mode_error_no_mode_decl
			% a call to a predicate for which there are
			% no mode declarations (and mode inference is
			% not enabled)
	;	mode_error_no_matching_mode(list(prog_var), list(inst))
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with >1 mode)
	;	mode_error_in_callee(list(prog_var), list(inst),
			pred_id, proc_id, list(mode_error_info))
			% call to a predicate with initial argument insts
			% for which mode inference gave a mode error
			% in the callee
	;	mode_error_bind_var(var_lock_reason, prog_var, inst, inst)
			% attempt to bind a non-local variable inside
			% a negated context, or attempt to re-bind a variable
			% in a parallel conjunct
	;	mode_error_non_local_lambda_var(prog_var, inst)
			% attempt to pass a live non-ground var as a
			% non-local variable to a lambda goal
	;	mode_error_unify_var_var(prog_var, prog_var, inst, inst)
			% attempt to unify two free variables
	;	mode_error_unify_var_functor(prog_var, cons_id, list(prog_var),
							inst, list(inst))
			% attempt to unify a free var with a functor containing
			% free arguments
	;	mode_error_unify_var_lambda(prog_var, inst, inst)
			% some sort of error in
			% attempt to unify a variable with lambda expression
	;	mode_error_conj(list(delayed_goal), schedule_culprit)
			% a conjunction contains one or more unscheduleable
			% goals; schedule_culprit gives the reason why
			% they couldn't be scheduled.
	;	mode_error_final_inst(int, prog_var, inst, inst,
				final_inst_error)
			% one of the head variables did not have the
			% expected final inst on exit from the proc
	.

:- type schedule_culprit
	--->	goal_itself_was_impure
	;	goals_followed_by_impure_goal(hlds_goal)
	;	conj_floundered. % we've reached the end of a conjunction
				% and there were still delayed goals

:- type final_inst_error
	--->	too_instantiated
	;	not_instantiated_enough
	;	wrongly_instantiated.	% a catchall for anything that doesn't
					% fit into the above two categories.

:- type mode_error_unify_rhs
	--->	error_at_var(prog_var)
	;	error_at_functor(cons_id, list(prog_var))
	;	error_at_lambda(list(prog_var), list(mode)).

:- type mode_error_info
	---> mode_error_info(
		set(prog_var),	% the variables which caused the error
				% (we will attempt to reschedule the goal
				% if the one of these variables becomes
				% more instantiated)
		mode_error,	% the nature of the error
		prog_context,	% where the error occurred
		mode_context	% where the error occurred
	).

%-----------------------------------------------------------------------------%

        % If there were any errors recorded in the mode_info,
        % report them to the user now.
        %
:- pred report_mode_errors(mode_info, mode_info).
:- mode report_mode_errors(mode_info_di, mode_info_uo) is det.


	% print an error message describing a mode error

:- pred report_mode_error(mode_error, mode_info, io__state, io__state).
:- mode report_mode_error(in, mode_info_no_io, di, uo) is det.

	% report an error for a predicate with no mode declarations
	% unless mode inference is enabled and the predicate is local.

:- pred maybe_report_error_no_modes(pred_id, pred_info, module_info,
				io__state, io__state).
:- mode maybe_report_error_no_modes(in, in, in, di, uo) is det.

	% initialize the mode_context.

:- pred mode_context_init(mode_context).
:- mode mode_context_init(out) is det.

	% Write out the inferred `mode' declarations for a list of pred_ids.
	% The bool indicates whether or not to write out determinism
	% annotations on the modes (it should only be set to `yes' _after_
	% determinism analysis).

:- pred write_mode_inference_messages(list(pred_id), bool, module_info,
				io__state, io__state).
:- mode write_mode_inference_messages(in, in, in, di, uo) is det.

	% report an error for the case when two mode declarations
	% declare indistinguishable modes

:- pred report_indistinguishable_modes_error(proc_id, proc_id,
		pred_id, pred_info, module_info, io__state, io__state).
:- mode report_indistinguishable_modes_error(in, in, in, in, in, di, uo) is det.

:- pred output_mode_decl(proc_id, pred_info, io__state, io__state).
:- mode output_mode_decl(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_out.
:- import_module check_hlds__mode_info, check_hlds__mode_util.
:- import_module parse_tree__prog_out, parse_tree__mercury_to_mercury.
:- import_module libs__options, libs__globals.

:- import_module int, map, term_io, varset, term.
:- import_module std_util, require.

	% just dispatch on the diffferent sorts of mode errors

report_mode_error(mode_error_disj(MergeContext, ErrorList), ModeInfo) -->
	report_mode_error_disj(ModeInfo, MergeContext, ErrorList).
report_mode_error(mode_error_par_conj(ErrorList), ModeInfo) -->
	report_mode_error_par_conj(ModeInfo, ErrorList).
report_mode_error(mode_error_higher_order_pred_var(PredOrFunc, Var, Inst,
		Arity), ModeInfo) -->
	report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var,
		Inst, Arity).
report_mode_error(mode_error_poly_unify(Var, Inst), ModeInfo) -->
	report_mode_error_poly_unify(ModeInfo, Var, Inst).
report_mode_error(mode_error_var_is_live(Var), ModeInfo) -->
	report_mode_error_var_is_live(ModeInfo, Var).
report_mode_error(mode_error_var_has_inst(Var, InstA, InstB), ModeInfo) -->
	report_mode_error_var_has_inst(ModeInfo, Var, InstA, InstB).
report_mode_error(mode_error_unify_pred(Var, RHS, Type, PredOrFunc),
		ModeInfo) -->
	report_mode_error_unify_pred(ModeInfo, Var, RHS, Type, PredOrFunc).
report_mode_error(mode_error_implied_mode(Var, InstA, InstB), ModeInfo) -->
	report_mode_error_implied_mode(ModeInfo, Var, InstA, InstB).
report_mode_error(mode_error_no_mode_decl, ModeInfo) -->
	report_mode_error_no_mode_decl(ModeInfo).
report_mode_error(mode_error_bind_var(Reason, Var, InstA, InstB), ModeInfo) -->
	report_mode_error_bind_var(ModeInfo, Reason, Var, InstA, InstB).
report_mode_error(mode_error_non_local_lambda_var(Var, Inst), ModeInfo) -->
	report_mode_error_non_local_lambda_var(ModeInfo, Var, Inst).
report_mode_error(mode_error_unify_var_var(VarA, VarB, InstA, InstB),
		ModeInfo) -->
	report_mode_error_unify_var_var(ModeInfo, VarA, VarB, InstA, InstB).
report_mode_error(mode_error_unify_var_lambda(VarA, InstA, InstB),
		ModeInfo) -->
	report_mode_error_unify_var_lambda(ModeInfo, VarA, InstA, InstB).
report_mode_error(mode_error_unify_var_functor(Var, Name, Args, Inst,
			ArgInsts), ModeInfo) -->
	report_mode_error_unify_var_functor(ModeInfo, Var, Name, Args, Inst,
			ArgInsts).
report_mode_error(mode_error_conj(Errors, Culprit), ModeInfo) -->
	report_mode_error_conj(ModeInfo, Errors, Culprit).
report_mode_error(mode_error_no_matching_mode(Vars, Insts), ModeInfo) -->
	report_mode_error_no_matching_mode(ModeInfo, Vars, Insts).
report_mode_error(mode_error_in_callee(Vars, Insts,
		CalleePredId, CalleeProcId, CalleeErrors), ModeInfo) -->
	report_mode_error_in_callee(ModeInfo, Vars, Insts,
		CalleePredId, CalleeProcId, CalleeErrors).
report_mode_error(mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
		ModeInfo) -->
	report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst,
		Reason).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_conj(mode_info, list(delayed_goal), schedule_culprit,
				io__state, io__state).
:- mode report_mode_error_conj(mode_info_no_io, in, in, di, uo) is det.

report_mode_error_conj(ModeInfo, Errors, Culprit) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ find_important_errors(Errors, ImportantErrors, OtherErrors) },

	% if there's more than one error, and we have verbose-errors
	% enabled, report them all
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes, Errors = [_, _ | _] } ->
		mode_info_write_context(ModeInfo),
		prog_out__write_context(Context),
		io__write_string("  mode error in conjunction. The next "),
		{ list__length(Errors, NumErrors) },
		io__write_int(NumErrors),
		io__write_string(" error messages\n"),
		prog_out__write_context(Context),
		io__write_string("  indicate possible causes of this error.\n"),
		report_mode_error_conj_2(ImportantErrors, VarSet, Context,
			ModeInfo),
		report_mode_error_conj_2(OtherErrors, VarSet, Context, ModeInfo)
	;
		% in the normal case, only report the first error
		{ ImportantErrors = [FirstImportantError | _] }
	->
		report_mode_error_conj_2([FirstImportantError], VarSet, Context,
			ModeInfo)
	;
		{ OtherErrors = [FirstOtherError | _] }
	->
		report_mode_error_conj_2([FirstOtherError], VarSet, Context,
			ModeInfo)
	;
		% There wasn't any error to report!  This can't happen.
		{ error("report_mode_error_conj") }
	),

	% if the goal(s) couldn't be scheduled because we couldn't
	% reorder things past an impure goal, then report that.
	( { Culprit = conj_floundered },
		{ true } % we've already reported everything we can
	; { Culprit = goal_itself_was_impure },
		prog_out__write_context(Context),
		io__write_string(
		"  The goal could not be reordered, because it was impure.\n")
	; { Culprit = goals_followed_by_impure_goal(ImpureGoal) },
		prog_out__write_context(Context),
		io__write_string(
			"  The goal could not be reordered, because\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  it was followed by an impure goal.\n"),
		{ ImpureGoal = _ - ImpureGoalInfo },
		{ goal_info_get_context(ImpureGoalInfo, ImpureGoalContext) },
		prog_out__write_context(ImpureGoalContext),
		io__write_string(
			"  This is the location of the impure goal.\n")
	).

:- pred find_important_errors(list(delayed_goal), list(delayed_goal),
			list(delayed_goal)).
:- mode find_important_errors(in, out, out) is det.

find_important_errors([], [], []).
find_important_errors([Error | Errors], ImportantErrors, OtherErrors) :-
	Error = delayed_goal(_, mode_error_info(_, ModeError, _, ModeContext),
				_),
	(
		% an error is important unless it is a non-explicit unification,
		% i.e. a head unification or a call argument unification
		ModeContext = unify(unify_context(UnifyContext, _), _),
		UnifyContext \= explicit,
		% except that errors in lambda goals are important even
		% if the unification that creates the lambda goal is
		% an implicit one
		ModeError \= mode_error_non_local_lambda_var(_, _)
	->
		ImportantErrors1 = ImportantErrors,
		OtherErrors = [Error | OtherErrors1]
	;
		ImportantErrors = [Error | ImportantErrors1],
		OtherErrors1 = OtherErrors
	),
	find_important_errors(Errors, ImportantErrors1, OtherErrors1).

:- pred report_mode_error_conj_2(list(delayed_goal), prog_varset, prog_context,
				mode_info, io__state, io__state).
:- mode report_mode_error_conj_2(in, in, in, mode_info_no_io, di, uo) is det.

report_mode_error_conj_2([], _, _, _) --> [].
report_mode_error_conj_2([delayed_goal(Vars, Error, Goal) | Rest],
			VarSet, Context, ModeInfo) -->
	globals__io_lookup_bool_option(debug_modes, Debug),
	( { Debug = yes } ->
		prog_out__write_context(Context),
		io__write_string("Floundered goal, waiting on { "),
		{ set__to_sorted_list(Vars, VarList) },
		mercury_output_vars(VarList, VarSet, no),
		io__write_string(" } :\n")
	;
		[]
	),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("\t\t"),
		{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, no, 2, ".\n")
	;
		[]
	),
	{ Error = mode_error_info(_, ModeError, ErrorContext, ModeContext) },
	{ mode_info_set_context(ErrorContext, ModeInfo, ModeInfo1) },
	{ mode_info_set_mode_context(ModeContext, ModeInfo1, ModeInfo2) },
	report_mode_error(ModeError, ModeInfo2),
	report_mode_error_conj_2(Rest, VarSet, Context, ModeInfo2).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_disj(mode_info, merge_context, merge_errors,
				io__state, io__state).
:- mode report_mode_error_disj(mode_info_no_io, in, in, di, uo) is det.

report_mode_error_disj(ModeInfo, MergeContext, ErrorList) -->
	{ mode_info_get_context(ModeInfo, Context) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode mismatch in "),
	write_merge_context(MergeContext),
	io__write_string(".\n"),
	write_merge_error_list(ErrorList, ModeInfo).

:- pred report_mode_error_par_conj(mode_info, merge_errors,
				io__state, io__state).
:- mode report_mode_error_par_conj(mode_info_no_io, in, di, uo) is det.

report_mode_error_par_conj(ModeInfo, ErrorList) -->
	{ mode_info_get_context(ModeInfo, Context) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: mutually exclusive bindings in parallel conjunction.\n"),
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("              (The current implementation does not permit\n"),
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("              parallel conjunctions to fail.)\n"),
	write_merge_error_list(ErrorList, ModeInfo).

:- pred write_merge_error_list(merge_errors, mode_info, io__state, io__state).
:- mode write_merge_error_list(in, mode_info_no_io, di, uo) is det.

write_merge_error_list([], _) --> [].
write_merge_error_list([Var - Insts | Errors], ModeInfo) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' :: "),
	output_inst_list(Insts, ModeInfo),
	io__write_string(".\n"),
	write_merge_error_list(Errors, ModeInfo).

:- pred write_merge_context(merge_context, io__state, io__state).
:- mode write_merge_context(in, di, uo) is det.

write_merge_context(disj) -->
	io__write_string("disjunction").
write_merge_context(if_then_else) -->
	io__write_string("if-then-else").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_bind_var(mode_info, var_lock_reason, prog_var,
		inst, inst, io__state, io__state).
:- mode report_mode_error_bind_var(mode_info_ui, in, in, in, in, di, uo) is det.

report_mode_error_bind_var(ModeInfo, Reason, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  scope error: "),
	( { Reason = negation },
		io__write_string("attempt to bind a variable inside a negation.\n")
	; { Reason = if_then_else },
		io__write_string("attempt to bind a non-local variable\n"),
		prog_out__write_context(Context),
		io__write_string("  inside the condition of an if-then-else.\n")
	; { Reason = lambda(PredOrFunc) },
		{ hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncS) },
		io__write_string("attempt to bind a non-local variable inside\n"),
		prog_out__write_context(Context),
		io__write_strings(["  a ", PredOrFuncS, " lambda goal.\n"])
	; { Reason = par_conj },
		io__write_string("attempt to bind a non-local variable\n"),
		prog_out__write_context(Context),
		io__write_string("  inside more than one parallel conjunct.\n")
	),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	output_inst(Inst, ModeInfo),
	io__write_string("'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		( { Reason = negation },
			io__write_string("\tA negation is only allowed to bind variables which are local to the\n"),
			io__write_string("\tnegation, i.e. those which are implicitly existentially quantified\n"),
			io__write_string("\tinside the scope of the negation.\n")
		; { Reason = if_then_else },
			io__write_string("\tThe condition of an if-then-else is only allowed\n"),
			io__write_string("\tto bind variables which are local to the condition\n"),
			io__write_string("\tor which occur only in the condition and the `then' part.\n")
		; { Reason = lambda(_) },
			io__write_string("\tA lambda goal is only allowed to bind its arguments\n"),
			io__write_string("\tand variables local to the lambda expression.\n")
		; { Reason = par_conj },
			io__write_string("\tA nonlocal variable of a parallel conjunction may be\n"),
			io__write_string("\tbound in at most one conjunct.\n")
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_non_local_lambda_var(mode_info, prog_var, inst,
					io__state, io__state).
:- mode report_mode_error_non_local_lambda_var(mode_info_ui, in, in,
					di, uo) is det.

report_mode_error_non_local_lambda_var(ModeInfo, Var, VarInst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness for non-local variables\n"),
	prog_out__write_context(Context),
	io__write_string("  of lambda goals is `ground'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_in_callee(mode_info, list(prog_var),
		list(inst), pred_id, proc_id, list(mode_error_info),
		io__state, io__state).
:- mode report_mode_error_in_callee(mode_info_ui, in, in, in, in, in,
		di, uo) is det.

report_mode_error_in_callee(ModeInfo, Vars, Insts,
		CalleePredId, CalleeProcId, CalleeModeErrors) -->
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: arguments `"),
	mercury_output_vars(Vars, VarSet, no),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  have insts `"),
	output_inst_list(Insts, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  which does not match any of the valid modes for\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  the callee ("),
		hlds_out__write_pred_id(ModuleInfo, CalleePredId),
		io__write_string("),\n"),
		prog_out__write_context(Context),
		io__write_string("  because of the following error.\n")
	;
		prog_out__write_context(Context),
		io__write_string("  the callee, because of the following error.\n")
	),
	( { CalleeModeErrors = [First | _] } ->
		{ First = mode_error_info(_, CalleeModeError,
			CalleeContext, CalleeModeContext) },
		{ mode_info_set_predid(ModeInfo, CalleePredId, ModeInfo1) },
		{ mode_info_set_procid(ModeInfo1, CalleeProcId, ModeInfo2) },
		{ mode_info_set_context(CalleeContext, ModeInfo2, ModeInfo3) },
		{ mode_info_set_mode_context(CalleeModeContext,
			ModeInfo3, ModeInfo4) },
		report_mode_error(CalleeModeError, ModeInfo4)
	;
		{ error("report_mode_error_in_callee: no error") }
	).

:- pred report_mode_error_no_matching_mode(mode_info, list(prog_var),
		list(inst), io__state, io__state).
:- mode report_mode_error_no_matching_mode(mode_info_ui, in, in, di, uo) is det.

report_mode_error_no_matching_mode(ModeInfo, Vars, Insts) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: arguments `"),
	mercury_output_vars(Vars, VarSet, no),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  have insts `"),
	output_inst_list(Insts, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  which does not match any of the modes for "),
	{ mode_info_get_mode_context(ModeInfo, ModeContext) },
	( { ModeContext = call(CallId, _) } ->
		hlds_out__write_call_id(CallId)
	;
		{ error("report_mode_error_no_matching_mode: invalid context") }
	),
	io__write_string(".\n").

:- pred report_mode_error_higher_order_pred_var(mode_info, pred_or_func,
		prog_var, inst, arity, io__state, io__state).
:- mode report_mode_error_higher_order_pred_var(mode_info_ui, in, in, in, in,
					di, uo) is det.

report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var, VarInst,
		Arity) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	(	{ PredOrFunc = predicate },
		io__write_string("  expecting higher-order pred inst (of arity "),
		io__write_int(Arity)
	;	{ PredOrFunc = function },
		io__write_string("  expecting higher-order func inst (of arity "),
		{ Arity1 is Arity - 1 },
		io__write_int(Arity1)
	),
	io__write_string(").\n").

:- pred report_mode_error_poly_unify(mode_info, prog_var, inst,
					io__state, io__state).
:- mode report_mode_error_poly_unify(mode_info_ui, in, in, di, uo) is det.

report_mode_error_poly_unify(ModeInfo, Var, VarInst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  in polymorphically-typed unification:\n"),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string(
		"  expected instantiatedness was `ground' or `any'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string(
"\tWhen unifying two variables whose type will not be known until
\truntime, the variables must both be ground (or have inst `any').
\tUnifications of polymorphically-typed variables with partially
\tinstantiated modes are not allowed.\n")
	;
		[]
	).

:- pred report_mode_error_var_is_live(mode_info, prog_var,
		io__state, io__state).
:- mode report_mode_error_var_is_live(mode_info_ui, in, di, uo) is det.

report_mode_error_var_is_live(ModeInfo, Var) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  unique-mode error: the called procedure would clobber\n"),
	prog_out__write_context(Context),
	io__write_string("  its argument, but variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' is still live.\n").

:- pred report_mode_error_var_has_inst(mode_info, prog_var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_var_has_inst(mode_info_ui, in, in, in, di, uo) is det.

report_mode_error_var_has_inst(ModeInfo, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	output_inst(Inst, ModeInfo),
	io__write_string("'.\n").

:- pred report_mode_error_implied_mode(mode_info, prog_var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_implied_mode(mode_info_ui, in, in, in, di, uo) is det.

report_mode_error_implied_mode(ModeInfo, Var, VarInst, Inst) -->
		% This "error" message is really a "sorry, not implemented"
		% message.  We only print the message if we are actually
		% going to generating code.
	globals__io_lookup_bool_option(errorcheck_only, ErrorcheckOnly),
	( { ErrorcheckOnly = no } ->
		{ mode_info_get_context(ModeInfo, Context) },
		{ mode_info_get_varset(ModeInfo, VarSet) },
		mode_info_write_context(ModeInfo),
		prog_out__write_context(Context),
		io__write_string("  sorry, implied modes not implemented.\n"),
		prog_out__write_context(Context),
		io__write_string("  Variable `"),
		mercury_output_var(Var, VarSet, no),
		io__write_string("' has instantiatedness `"),
		output_inst(VarInst, ModeInfo),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected instantiatedness was `"),
		output_inst(Inst, ModeInfo),
		io__write_string("'.\n")
	;
		[]
	).

:- pred report_mode_error_no_mode_decl(mode_info, io__state, io__state).
:- mode report_mode_error_no_mode_decl(mode_info_ui, di, uo) is det.

report_mode_error_no_mode_decl(ModeInfo) -->
	{ mode_info_get_context(ModeInfo, Context) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  no mode declaration for called predicate.\n").

:- pred report_mode_error_unify_pred(mode_info, prog_var, mode_error_unify_rhs,
					type, pred_or_func,
					io__state, io__state).
:- mode report_mode_error_unify_pred(mode_info_ui, in, in, in, in,
					di, uo) is det.

report_mode_error_unify_pred(ModeInfo, X, RHS, Type, PredOrFunc) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  In unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' with `"),
	(
		{ RHS = error_at_var(Y) },
		mercury_output_var(Y, VarSet, no)
	;
		{ RHS = error_at_functor(ConsId, ArgVars) },
		{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
		hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet,
			ModuleInfo, no)
	;
		{ RHS = error_at_lambda(ArgVars, ArgModes) },
		io__write_string("lambda(["),
		hlds_out__write_var_modes(ArgVars, ArgModes, VarSet,
			InstVarSet, no),
		io__write_string("] ... )")
	),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  mode error: attempt at higher-order unification.\n"),
	prog_out__write_context(Context),
	io__write_string("  Cannot unify two terms of type `"),
	{ varset__init(TypeVarSet) },
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string("'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string("\tYour code is trying to test whether two "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string("s are equal,\n"),
		io__write_string("\tby unifying them.  In the general case, testing equivalence\n"),
		io__write_string("\tof "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string("s is an undecidable problem,\n"),
		io__write_string("\tand so this is not allowed by the Mercury mode system.\n"),
		io__write_string("\tIn some cases, you can achieve the same effect by\n"),
		io__write_string("\twriting an explicit universal quantification,\n"),
		io__write_string("\te.g. `all [X] call(P, X) <=> call(Q, X)',"),
		io__write_string(" instead of `P = Q'.\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_var(mode_info, prog_var, prog_var,
		inst, inst, io__state, io__state).
:- mode report_mode_error_unify_var_var(mode_info_ui, in, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_var(ModeInfo, X, Y, InstX, InstY) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' and `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstX, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  variable `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstY, ModeInfo),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_lambda(mode_info, prog_var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_unify_var_lambda(mode_info_ui, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_lambda(ModeInfo, X, InstX, InstY) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' and lambda expression.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstX, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  lambda expression has instantiatedness `"),
	output_inst(InstY, ModeInfo),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_functor(mode_info, prog_var, cons_id,
		list(prog_var), inst, list(inst), io__state, io__state).
:- mode report_mode_error_unify_var_functor(mode_info_ui, in, in, in, in, in,
			di, uo) is det.

report_mode_error_unify_var_functor(ModeInfo, X, ConsId, Args, InstX, ArgInsts)
		-->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' and `"),
	hlds_out__write_functor_cons_id(ConsId, Args, VarSet, ModuleInfo, no),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstX, ModeInfo),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  term `"),
	hlds_out__write_functor_cons_id(ConsId, Args, VarSet, ModuleInfo, no),
	( { Args \= [] } ->
		io__write_string("'\n"),
		prog_out__write_context(Context),
		io__write_string("  has instantiatedness `"),
		mercury_output_cons_id(ConsId, does_not_need_brackets),
		io__write_string("("),
		output_inst_list(ArgInsts, ModeInfo),
		io__write_string(")")
	;
		io__write_string("' has instantiatedness `"),
		mercury_output_cons_id(ConsId, does_not_need_brackets)
	),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred mode_info_write_context(mode_info, io__state, io__state).
:- mode mode_info_write_context(mode_info_no_io, di, uo) is det.

mode_info_write_context(ModeInfo) -->
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_predid(ModeInfo, PredId) },
	{ mode_info_get_procid(ModeInfo, ProcId) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_get_markers(PredInfo, PredMarkers) },
	{ proc_info_declared_argmodes(ProcInfo, Modes0) },
	{ strip_builtin_qualifiers_from_mode_list(Modes0, Modes) },
	{ pred_info_name(PredInfo, Name0) },
	{ Name = unqualified(Name0) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	{ MaybeDet = no },

	prog_out__write_context(Context),
	io__write_string("In clause for `"),
	mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
				MaybeDet, Context),
	io__write_string("':\n"),
	{ mode_info_get_mode_context(ModeInfo, ModeContext) },
	write_mode_context(ModeContext, Context, PredMarkers, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_final_inst(mode_info, int, prog_var, inst, inst,
				final_inst_error, io__state, io__state).
:- mode report_mode_error_final_inst(mode_info_ui, in, in, in, in, in,
				di, uo) is det.

report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst, Reason) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: argument "),
	io__write_int(ArgNum),
	( { Reason = too_instantiated } ->
		io__write_string(" became too instantiated")
	; { Reason = not_instantiated_enough } ->
		io__write_string(" did not get sufficiently instantiated")
	;
		% I don't think this can happen.  But just in case...
		io__write_string(" had the wrong instantiatedness")
	),
	io__write_string(".\n"),

	prog_out__write_context(Context),
	io__write_string("  Final instantiatedness of `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' was `"),
	output_inst(VarInst, ModeInfo),
	io__write_string("',\n"),

	prog_out__write_context(Context),
	io__write_string("  expected final instantiatedness was `"),
	output_inst(Inst, ModeInfo),
	io__write_string("'.\n").


%-----------------------------------------------------------------------------%

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

	% XXX some parts of the mode context never get set up

:- pred write_mode_context(mode_context, prog_context, pred_markers,
		module_info, io__state, io__state).
:- mode write_mode_context(in, in, in, in, di, uo) is det.

write_mode_context(uninitialized, _Context, _Markers, _ModuleInfo) -->
	[].

write_mode_context(call(CallId, ArgNum), Context, Markers, _ModuleInfo) -->
	prog_out__write_context(Context),
	io__write_string("  in "),
	hlds_out__write_call_arg_id(CallId, ArgNum, Markers),
	io__write_string(":\n").

write_mode_context(unify(UnifyContext, _Side), Context, _Markers,
		_ModuleInfo) -->
	hlds_out__write_unify_context(UnifyContext, Context).

%-----------------------------------------------------------------------------%

maybe_report_error_no_modes(PredId, PredInfo, ModuleInfo) -->
	{ pred_info_import_status(PredInfo, ImportStatus) },
	( { ImportStatus = local } ->
		globals__io_lookup_bool_option(infer_modes, InferModesOpt),
		( { InferModesOpt = yes } ->
			[]
		;
			io__set_exit_status(1),
			{ pred_info_context(PredInfo, Context) },
			prog_out__write_context(Context),
			io__write_string("Error: no mode declaration for "),
			hlds_out__write_pred_id(ModuleInfo, PredId),
			io__write_string(".\n"),
			globals__io_lookup_bool_option(verbose_errors,
				VerboseErrors),
			( { VerboseErrors = yes } ->
				prog_out__write_context(Context),
				io__write_string(
			"  (Use `--infer-modes' to enable mode inference.)\n")
			;
				[]
			)
		)
	;
		io__set_exit_status(1),
		{ pred_info_context(PredInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("Error: no mode declaration for exported\n"),
		prog_out__write_context(Context),
		io__write_string("  "),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		io__write_string(".\n")
	
	).

%-----------------------------------------------------------------------------%

	% write out the inferred `mode' declarations for a list of pred_ids.

write_mode_inference_messages([], _, _) --> [].
write_mode_inference_messages([PredId | PredIds], OutputDetism, ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_markers(PredInfo, Markers) },
	( { check_marker(Markers, infer_modes) } ->
		{ pred_info_all_procids(PredInfo, ProcIds) },
		{ pred_info_procedures(PredInfo, Procs) },
		write_mode_inference_messages_2(ProcIds, Procs, PredInfo,
			OutputDetism, ModuleInfo)
	;
		[]
	),
	write_mode_inference_messages(PredIds, OutputDetism, ModuleInfo).

	% write out the inferred `mode' declarations for a list of
	% proc_ids

:- pred write_mode_inference_messages_2(list(proc_id), proc_table, pred_info,
				bool, module_info, io__state, io__state).
:- mode write_mode_inference_messages_2(in, in, in, in, in, di, uo) is det.

write_mode_inference_messages_2([], _, _, _, _) --> [].
write_mode_inference_messages_2([ProcId | ProcIds], Procs, PredInfo,
		OutputDetism, ModuleInfo) -->
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	{ map__lookup(Procs, ProcId, ProcInfo) },
	(
		{
			% We always output `Inferred :- mode ...'
			proc_info_is_valid_mode(ProcInfo)
		;
			% We only output `REJECTED :- mode ...'
			% if --verbose-errors is enabled
			VerboseErrors = yes
		}
	->
		write_mode_inference_message(PredInfo, ProcInfo, OutputDetism,
			ModuleInfo)
	;
		[]
	),
	write_mode_inference_messages_2(ProcIds, Procs, PredInfo, OutputDetism,
		ModuleInfo).

	% write out the inferred `mode' declaration
	% for a single function or predicate.

:- pred write_mode_inference_message(pred_info, proc_info, bool,
		module_info, io__state, io__state).
:- mode write_mode_inference_message(in, in, in, in, di, uo) is det.

write_mode_inference_message(PredInfo, ProcInfo, OutputDetism, ModuleInfo) -->
	{ pred_info_name(PredInfo, PredName) },
	{ Name = unqualified(PredName) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_arity(PredInfo, PredArity) },
	{ proc_info_argmodes(ProcInfo, ArgModes0) },

	% We need to strip off the extra type_info arguments inserted at the
	% front by polymorphism.m - we only want the last `PredArity' of them.
	%
	{ list__length(ArgModes0, NumArgModes) },
	{ NumToDrop is NumArgModes - PredArity },
	( { list__drop(NumToDrop, ArgModes0, ArgModes1) } ->
		{ ArgModes2 = ArgModes1 }
	;
		{ error("report_pred_proc_id: list__drop failed") }
	),

	{ varset__init(VarSet) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	( { OutputDetism = yes } ->
		{ proc_info_inferred_determinism(ProcInfo, Detism) },
		{ MaybeDet0 = yes(Detism) }
	;
		{ MaybeDet0 = no }
	),
	prog_out__write_context(Context),
	( { proc_info_is_valid_mode(ProcInfo) } ->
		io__write_string("Inferred "),
		{ ArgModes3 = ArgModes2 },
		{ MaybeDet = MaybeDet0 }
	;
		io__write_string("REJECTED "),
		% Replace the final insts with dummy insts '...',
		% since they won't be valid anyway -- they are just
		% the results of whatever partial inference we did
		% before detecting the error.
		{ mode_list_get_initial_insts(ArgModes2, ModuleInfo,
			InitialInsts) }, 
		{ DummyInst = defined_inst(user_inst(unqualified("..."), [])) },
		{ list__duplicate(PredArity, DummyInst, FinalInsts) },
		{ ArgModes3 = list__map(func(I - F) = (I -> F),
			assoc_list__from_corresponding_lists(
				InitialInsts, FinalInsts)) },
		% Likewise delete the determinism.
		{ MaybeDet = no }
	),
	{ strip_builtin_qualifiers_from_mode_list(ArgModes3, ArgModes) },
	(	{ PredOrFunc = predicate },
		mercury_output_pred_mode_decl(VarSet, Name, ArgModes,
				MaybeDet, Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgModes, FuncArgModes, RetMode) },
		mercury_output_func_mode_decl(VarSet, Name, FuncArgModes,
				RetMode, MaybeDet, Context)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

        % If there were any errors recorded in the mode_info,
        % report them to the user now.

report_mode_errors(ModeInfo0, ModeInfo) :-
        mode_info_get_errors(ModeInfo0, Errors),
        ( Errors = [FirstError | _] ->	% XXX Why do we only report the first?
                FirstError = mode_error_info(_, ModeError,
                                                Context, ModeContext),
                mode_info_set_context(Context, ModeInfo0, ModeInfo1),
                mode_info_set_mode_context(ModeContext, ModeInfo1, ModeInfo2),
                mode_info_get_io_state(ModeInfo2, IOState0),
                report_mode_error(ModeError, ModeInfo2,
                                IOState0, IOState),
                mode_info_set_io_state(ModeInfo2, IOState, ModeInfo)
        ;
                ModeInfo = ModeInfo0
        ).

%-----------------------------------------------------------------------------%

:- pred output_inst((inst), mode_info, io__state, io__state).
:- mode output_inst(in, mode_info_ui, di, uo) is det.

output_inst(Inst0, ModeInfo) -->
	{ strip_builtin_qualifiers_from_inst(Inst0, Inst) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	mercury_output_expanded_inst(Inst, InstVarSet, ModuleInfo).

:- pred output_inst_list(list(inst), mode_info, io__state, io__state).
:- mode output_inst_list(in, in, di, uo) is det.

output_inst_list([], _) --> [].
output_inst_list([Inst | Insts], ModeInfo) -->
	output_inst(Inst, ModeInfo),
	( { Insts = [] } ->
		[]
	;
		io__write_string(", "),
		output_inst_list(Insts, ModeInfo)
	).

%-----------------------------------------------------------------------------%

report_indistinguishable_modes_error(OldProcId, NewProcId,
		PredId, PredInfo, ModuleInfo) -->

	io__set_exit_status(1),

	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, OldProcId, OldProcInfo) },
	{ map__lookup(Procs, NewProcId, NewProcInfo) },
	{ proc_info_context(OldProcInfo, OldContext) },
	{ proc_info_context(NewProcInfo, NewContext) },

	prog_out__write_context(NewContext),
	io__write_string("In mode declarations for "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),

	prog_out__write_context(NewContext),
	io__write_string("  error: duplicate mode declaration.\n"),

	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(NewContext),
		io__write_string("  Modes `"),
		output_mode_decl(OldProcId, PredInfo),
		io__write_string("'\n"),

		prog_out__write_context(NewContext),
		io__write_string("  and `"),
		output_mode_decl(NewProcId, PredInfo),
		io__write_string("'\n"),

		prog_out__write_context(NewContext),
		io__write_string("  are indistinguishable.\n")
	;
		[]
	),

	prog_out__write_context(OldContext),
	io__write_string(
		"  Here is the conflicting mode declaration.\n").

output_mode_decl(ProcId, PredInfo) -->
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_name(PredInfo, Name0) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_declared_argmodes(ProcInfo, Modes0) },
	{ proc_info_declared_determinism(ProcInfo, MaybeDet) },
	{ proc_info_context(ProcInfo, Context) },
	{ varset__init(InstVarSet) },
	{ Name = unqualified(Name0) },
	{ strip_builtin_qualifiers_from_mode_list(Modes0, Modes) },
	mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
				MaybeDet, Context).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
