%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_errors.m.
% Main author: fjh.

% This module contains all the error-reporting routines for the
% mode-checker.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mode_errors.

:- interface.

:- import_module hlds_data, prog_data, mode_info.
:- import_module set, assoc_list.

%-----------------------------------------------------------------------------%

:- type merge_context
	---> disj
	;    if_then_else.

:- type merge_errors == assoc_list(var, list(inst)).

:- type delayed_goal
	--->	delayed_goal(
			set(var),	% The vars it's waiting on
			mode_error_info,% The reason it can't be scheduled
			hlds_goal	% The goal itself
		).

:- type mode_error
	--->	mode_error_disj(merge_context, merge_errors)
			% different arms of a disjunction result in
			% different insts for some non-local variables
	;	mode_error_higher_order_pred_var(pred_or_func, var, inst, arity)
			% the predicate variable in a higher-order predicate
			% or function call didn't have a higher-order
			% predicate or function inst of the appropriate arity
	;	mode_error_var_is_live(var)
			% call to a predicate which will clobber its argument,
			% but the argument is still live
	;	mode_error_var_has_inst(var, inst, inst)
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with one mode)
	;	mode_error_unify_pred(var, mode_error_unify_rhs, type,
				pred_or_func)
			% an attempt was made to unify two higher-order
			% predicate or function variables.
	;	mode_error_implied_mode(var, inst, inst)
			% a call to a predicate with an overly
			% instantiated variable would use an implied
			% mode of the predicate.  XXX This is temporary - 
			% once we've implemented implied modes we can
			% get rid of it.
	;	mode_error_no_mode_decl
			% a call to a predicate for which there are
			% no mode declarations
	;	mode_error_no_matching_mode(list(var), list(inst))
			% call to a predicate with an insufficiently
			% instantiated variable (for preds with >1 mode)
	;	mode_error_bind_var(var, inst, inst)
			% attempt to bind a non-local variable inside
			% a negated context
	;	mode_error_unify_var_var(var, var, inst, inst)
			% attempt to unify two free variables
	;	mode_error_unify_var_functor(var, cons_id, list(var),
							inst, list(inst))
			% attempt to unify a free var with a functor containing
			% free arguments
	;	mode_error_unify_var_lambda(var, inst, inst)
			% some sort of error in
			% attempt to unify a variable with lambda expression
	;	mode_error_conj(list(delayed_goal))
			% a conjunction contains one or more unscheduleable
			% goals
	;	mode_error_final_inst(int, var, inst, inst, final_inst_error)
			% one of the head variables did not have the
			% expected final inst on exit from the proc
	;	mode_error_undefined_mode_in_lambda.
			% This is a dummy error - the actual message
			% is output by module_qual.m.

:- type final_inst_error
	--->	too_instantiated
	;	not_instantiated_enough
	;	wrongly_instantiated.	% a catchall for anything that doesn't
					% fit into the above two categories.

:- type mode_error_unify_rhs
	--->	error_at_var(var)
	;	error_at_functor(cons_id, list(var))
	;	error_at_lambda(list(var), list(mode)).

:- type mode_error_info
	---> mode_error_info(
		set(var),	% the variables which caused the error
				% (we will attempt to reschedule the goal
				% if the one of these variables becomes
				% more instantiated)
		mode_error,	% the nature of the error
		term__context,	% where the error occurred
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

	% write out the inferred `mode' declarations for a list of pred_ids.

:- pred write_mode_inference_messages(list(pred_id), module_info,
				io__state, io__state).
:- mode write_mode_inference_messages(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_module, hlds_pred, hlds_goal, hlds_out.
:- import_module mode_info, mode_util, prog_out, mercury_to_mercury.
:- import_module options, globals.
:- import_module bool, int, list, map, io, term, term_io, varset.
:- import_module std_util, require.

	% just dispatch on the diffferent sorts of mode errors

report_mode_error(mode_error_disj(MergeContext, ErrorList), ModeInfo) -->
	report_mode_error_disj(ModeInfo, MergeContext, ErrorList).
report_mode_error(mode_error_higher_order_pred_var(PredOrFunc, Var, Inst,
		Arity), ModeInfo) -->
	report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var,
		Inst, Arity).
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
report_mode_error(mode_error_bind_var(Var, InstA, InstB), ModeInfo) -->
	report_mode_error_bind_var(ModeInfo, Var, InstA, InstB).
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
report_mode_error(mode_error_conj(Errors), ModeInfo) -->
	report_mode_error_conj(ModeInfo, Errors).
report_mode_error(mode_error_no_matching_mode(Vars, Insts), ModeInfo) -->
	report_mode_error_no_matching_mode(ModeInfo, Vars, Insts).
report_mode_error(mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
		ModeInfo) -->
	report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst,
		Reason).
report_mode_error(mode_error_undefined_mode_in_lambda, _ModeInfo) --> [].

%-----------------------------------------------------------------------------%

:- pred report_mode_error_conj(mode_info, list(delayed_goal),
				io__state, io__state).
:- mode report_mode_error_conj(mode_info_no_io, in, di, uo) is det.

report_mode_error_conj(ModeInfo, Errors) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ find_important_errors(Errors, ImportantErrors, OtherErrors) },
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
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
	).

:- pred find_important_errors(list(delayed_goal), list(delayed_goal),
			list(delayed_goal)).
:- mode find_important_errors(in, out, out) is det.

find_important_errors([], [], []).
find_important_errors([Error | Errors], ImportantErrors, OtherErrors) :-
	Error = delayed_goal(_, mode_error_info(_, _, _, ModeContext), _),
	(
		% an error is important unless it is a non-explicit unification,
		% i.e. a head unification or a call argument unification
		ModeContext = unify(unify_context(UnifyContext, _), _),
		UnifyContext \= explicit
	->
		ImportantErrors1 = ImportantErrors,
		OtherErrors = [Error | OtherErrors1]
	;
		ImportantErrors = [Error | ImportantErrors1],
		OtherErrors1 = OtherErrors
	),
	find_important_errors(Errors, ImportantErrors1, OtherErrors1).

:- pred report_mode_error_conj_2(list(delayed_goal), varset, term__context,
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
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, no, 2, ".")
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

:- pred write_merge_error_list(merge_errors, mode_info, io__state, io__state).
:- mode write_merge_error_list(in, mode_info_no_io, di, uo) is det.

write_merge_error_list([], _) --> [].
write_merge_error_list([Var - Insts | Errors], ModeInfo) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' :: "),
	output_inst_list(Insts, InstVarSet),
	io__write_string(".\n"),
	write_merge_error_list(Errors, ModeInfo).

:- pred write_merge_context(merge_context, io__state, io__state).
:- mode write_merge_context(in, di, uo) is det.

write_merge_context(disj) -->
	io__write_string("disjunction").
write_merge_context(if_then_else) -->
	io__write_string("if-then-else").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_bind_var(mode_info, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_bind_var(mode_info_ui, in, in, in, di, uo) is det.

report_mode_error_bind_var(ModeInfo, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string(
		"  scope error: attempt to bind variable inside a negation.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	output_inst(Inst, InstVarSet),
	io__write_string("'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string("\tA negation is only allowed to bind variables which are local to the\n"),
		io__write_string("\tnegation, i.e. those which are implicitly existentially quantified\n"),
		io__write_string("\tinside the scope of the negation.\n"),
		io__write_string("\tNote that the condition of an if-then-else is implicitly\n"),
		io__write_string("\tnegated in the ""else"" part, so the condition can only bind\n"),
		io__write_string("\tvariables in the ""then"" part.\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_no_matching_mode(mode_info, list(var), list(inst),
					io__state, io__state).
:- mode report_mode_error_no_matching_mode(mode_info_ui, in, in, di, uo) is det.

report_mode_error_no_matching_mode(ModeInfo, Vars, Insts) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	{ mode_info_get_module_info(ModeInfo, ModuleInfo) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: arguments `"),
	mercury_output_vars(Vars, VarSet, no),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  have insts `"),
	output_inst_list(Insts, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  which does not match any of the modes for "),
	{ mode_info_get_mode_context(ModeInfo, ModeContext) },
	( { ModeContext = call(PredId, _) } ->
		hlds_out__write_pred_id(ModuleInfo, PredId)
	;
		{ error("report_mode_error_no_matching_mode: invalid context") }
	),
	io__write_string(".\n").

:- pred report_mode_error_higher_order_pred_var(mode_info, pred_or_func, var,
					inst, arity,
					io__state, io__state).
:- mode report_mode_error_higher_order_pred_var(mode_info_ui, in, in, in, in,
					di, uo) is det.

report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var, VarInst,
		Arity) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, InstVarSet),
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

:- pred report_mode_error_var_is_live(mode_info, var, io__state, io__state).
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

:- pred report_mode_error_var_has_inst(mode_info, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_var_has_inst(mode_info_ui, in, in, in, di, uo) is det.

report_mode_error_var_has_inst(ModeInfo, Var, VarInst, Inst) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error: variable `"),
	mercury_output_var(Var, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected instantiatedness was `"),
	output_inst(Inst, InstVarSet),
	io__write_string("'.\n").

:- pred report_mode_error_implied_mode(mode_info, var, inst, inst,
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
		{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
		mode_info_write_context(ModeInfo),
		prog_out__write_context(Context),
		io__write_string("  sorry, implied modes not implemented.\n"),
		prog_out__write_context(Context),
		io__write_string("  Variable `"),
		mercury_output_var(Var, VarSet, no),
		io__write_string("' has instantiatedness `"),
		output_inst(VarInst, InstVarSet),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected instantiatedness was `"),
		output_inst(Inst, InstVarSet),
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

:- pred report_mode_error_unify_pred(mode_info, var, mode_error_unify_rhs,
					type, pred_or_func,
					io__state, io__state).
:- mode report_mode_error_unify_pred(mode_info_ui, in, in, in, in,
					di, uo) is det.

report_mode_error_unify_pred(ModeInfo, X, RHS, Type, PredOrFunc) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
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
		hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet, no)
	;
		{ RHS = error_at_lambda(ArgVars, ArgModes) },
		io__write_string("lambda(["),
		hlds_out__write_var_modes(ArgVars, ArgModes, VarSet, no),
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

:- pred report_mode_error_unify_var_var(mode_info, var, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_unify_var_var(mode_info_ui, in, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_var(ModeInfo, X, Y, InstX, InstY) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
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
	output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  variable `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstY, InstVarSet),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_lambda(mode_info, var, inst, inst,
					io__state, io__state).
:- mode report_mode_error_unify_var_lambda(mode_info_ui, in, in, in, di, uo)
	is det.

report_mode_error_unify_var_lambda(ModeInfo, X, InstX, InstY) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' and lambda expression.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  lambda expression has instantiatedness `"),
	output_inst(InstY, InstVarSet),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_functor(mode_info, var, cons_id, list(var),
					inst, list(inst), io__state, io__state).
:- mode report_mode_error_unify_var_functor(mode_info_ui, in, in, in, in, in,
			di, uo) is det.

report_mode_error_unify_var_functor(ModeInfo, X, ConsId, Args, InstX, ArgInsts)
		-->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	mode_info_write_context(ModeInfo),
	prog_out__write_context(Context),
	io__write_string("  mode error in unification of `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' and `"),
	hlds_out__write_functor_cons_id(ConsId, Args, VarSet, no),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  Variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("' has instantiatedness `"),
	output_inst(InstX, InstVarSet),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  term `"),
	hlds_out__write_functor_cons_id(ConsId, Args, VarSet, no),
	( { Args \= [] } ->
		io__write_string("'\n"),
		prog_out__write_context(Context),
		io__write_string("  has instantiatedness `"),
		mercury_output_cons_id(ConsId, no),
		io__write_string("("),
		output_inst_list(ArgInsts, InstVarSet),
		io__write_string(")")
	;
		io__write_string("' has instantiatedness `"),
		mercury_output_cons_id(ConsId, no)
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
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_procedures(PredInfo, Procs) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_declared_argmodes(ProcInfo, Modes0) },
	{ strip_builtin_qualifiers_from_mode_list(Modes0, Modes) },
	{ pred_info_name(PredInfo, Name0) },
	{ Name = unqualified(Name0) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
	{ MaybeDet = no },

	prog_out__write_context(Context),
	io__write_string("In clause for `"),
	(	{ PredOrFunc = predicate },
		mercury_output_pred_mode_subdecl(InstVarSet, Name, Modes,
				MaybeDet, Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		mercury_output_func_mode_subdecl(InstVarSet, Name, ArgModes,
				RetMode, MaybeDet, Context)
	),
	io__write_string("':\n"),
	{ mode_info_get_mode_context(ModeInfo, ModeContext) },
	write_mode_context(ModeContext, Context, ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_final_inst(mode_info, int, var, inst, inst,
				final_inst_error, io__state, io__state).
:- mode report_mode_error_final_inst(mode_info_ui, in, in, in, in, in,
				di, uo) is det.

report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst, Reason) -->
	{ mode_info_get_context(ModeInfo, Context) },
	{ mode_info_get_varset(ModeInfo, VarSet) },
	{ mode_info_get_instvarset(ModeInfo, InstVarSet) },
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
	output_inst(VarInst, InstVarSet),
	io__write_string("',\n"),

	prog_out__write_context(Context),
	io__write_string("  expected final instantiatedness was `"),
	output_inst(Inst, InstVarSet),
	io__write_string("'.\n").


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

	% XXX some parts of the mode context never get set up

:- pred write_mode_context(mode_context, term__context, module_info,
				io__state, io__state).
:- mode write_mode_context(in, in, in, di, uo) is det.

write_mode_context(uninitialized, _Context, _ModuleInfo) -->
	[].

write_mode_context(higher_order_call(PredOrFunc, ArgNum), Context, _ModuleInfo)
		-->
	prog_out__write_context(Context),
	io__write_string("  in "),
	( { ArgNum = 0 } ->
		io__write_string("higher-order "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" call:\n")
	;
		io__write_string("argument "),
		io__write_int(ArgNum),
		io__write_string(" of higher-order "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" call\n"),
		prog_out__write_context(Context),
		io__write_string("  (i.e. in "),
		( { ArgNum = 1 } ->
			io__write_string("the "),
			hlds_out__write_pred_or_func(PredOrFunc),
			io__write_string(" term")
		;
			io__write_string("argument "),
			{ ArgNum1 is ArgNum - 1 },
			io__write_int(ArgNum1),
			io__write_string(" of the called "),
			hlds_out__write_pred_or_func(PredOrFunc)
		),
		io__write_string("):\n")
	).

write_mode_context(call(PredId, ArgNum), Context, ModuleInfo) -->
	prog_out__write_context(Context),
	io__write_string("  in "),
	( { ArgNum = 0 } ->
		[]
	;
		io__write_string("argument "),
		io__write_int(ArgNum),
		io__write_string(" of ")
	),
	io__write_string("call to "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n").

write_mode_context(unify(UnifyContext, _Side), Context, _ModuleInfo) -->
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

write_mode_inference_messages([], _) --> [].
write_mode_inference_messages([PredId | PredIds], ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_marker_list(PredInfo, Markers) },
	( { list__member(request(infer_modes), Markers) } ->
		{ pred_info_procedures(PredInfo, Procs) },
		{ map__keys(Procs, ProcIds) },
		write_mode_inference_messages_2(ProcIds, Procs, PredInfo)
	;
		[]
	),
	write_mode_inference_messages(PredIds, ModuleInfo).

	% write out the inferred `mode' declarations for a list of
	% proc_ids

:- pred write_mode_inference_messages_2(list(proc_id), proc_table, pred_info,
				io__state, io__state).
:- mode write_mode_inference_messages_2(in, in, in, di, uo) is det.

write_mode_inference_messages_2([], _, _) --> [].
write_mode_inference_messages_2([ProcId | ProcIds], Procs, PredInfo) -->
	{ map__lookup(Procs, ProcId, ProcInfo) },
	write_mode_inference_message(PredInfo, ProcInfo),
	write_mode_inference_messages_2(ProcIds, Procs, PredInfo).

	% write out the inferred `mode' declaration
	% for a single function or predicate.

:- pred write_mode_inference_message(pred_info, proc_info,
				io__state, io__state).
:- mode write_mode_inference_message(in, in, di, uo) is det.

write_mode_inference_message(PredInfo, ProcInfo) -->
	{ pred_info_name(PredInfo, PredName) },
	{ Name = unqualified(PredName) },
	{ pred_info_context(PredInfo, Context) },
	{ proc_info_argmodes(ProcInfo, Modes0) },
	{ varset__init(VarSet) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ MaybeDet = no },
	prog_out__write_context(Context),
	{ strip_builtin_qualifiers_from_mode_list(Modes0, Modes) },
	io__write_string("Inferred "),
	(	{ PredOrFunc = predicate },
		mercury_output_pred_mode_decl(VarSet, Name, Modes,
				MaybeDet, Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Modes, ArgModes, RetMode) },
		mercury_output_func_mode_decl(VarSet, Name, ArgModes, RetMode,
				MaybeDet, Context)
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

:- pred output_inst((inst), varset, io__state, io__state).
:- mode output_inst(in, in, di, uo) is det.

output_inst(Inst0, VarSet) -->
	{ strip_builtin_qualifiers_from_inst(Inst0, Inst) },
	mercury_output_inst(Inst, VarSet).

:- pred output_inst_list(list(inst), varset, io__state, io__state).
:- mode output_inst_list(in, in, di, uo) is det.

output_inst_list(Insts0, VarSet) -->
	{ strip_builtin_qualifiers_from_inst_list(Insts0, Insts) },
	mercury_output_inst_list(Insts, VarSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
