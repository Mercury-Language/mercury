%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File      : purity.m
% Authors   : scachte (Peter Schachte)
% 		(main author and designer of purity system)
%	      trd (modifications for impure functions)
% Purpose   : handle `impure' and `promise_pure' declarations;
%	      finish off type checking.
%
% The main purpose of this module is check the consistency of the
% `impure' and `promise_pure' (etc.) declarations, and to thus report
% error messages if the program is not "purity-correct".
% This includes treating procedures with different clauses for
% different modes as impure, unless promised pure.
%
% This module also calls post_typecheck.m to perform the final parts of
% type analysis, including resolution of predicate and function overloading
% (see the comments in that file).
%
% These actions cannot be done until after type inference is complete,
% so they need to be a separate "post-typecheck pass"; they are done
% here in combination with the purity-analysis pass for efficiency reasons.
%
% We also do elimination of double-negation in this pass.
% It needs to be done somewhere after quantification analysis and
% before mode analysis, and this is convenient place to do it.
%
% This pass also converts calls to `private_builtin.unsafe_type_cast'
% into `generic_call(unsafe_cast, ...)' goals.
%
%-----------------------------------------------------------------------------%
%
% The aim of Mercury's purity system is to allow one to declare certain parts
% of one's program to be impure, thereby forbidding the compiler from making
% certain optimizations to that part of the code.  Since one can often
% implement a perfectly pure predicate or function in terms of impure
% predicates and functions, one is also allowed to promise to the compiler
% that a predicate *is* pure, despite calling impure predicates and
% functions.
%
% To keep purity/impurity consistent, it is required that every impure
% predicate/function be declared so.  A predicate is impure if:
%
%	1.  It's declared impure, or
%	2a. It's not promised pure, and
%	2b. It calls some impure predicates or functions.
%
% A predicate or function is declared impure by preceding the `pred' or
% `func' in its declaration with `impure'.  It is promised to be pure with a
%
%	:- pragma promise_pure(Name/Arity).
%
% directive.
%
% Calls to impure predicates may not be optimized away.  Neither may they be
% reodered relative to any other goals in a given conjunction; ie, an impure
% goal cleaves a conjunction into the stuff before it and the stuff after it.
% Both of these groups may be reordered separately, but no goal from either
% group may move into the other.  Similarly for disjunctions.
%
% Semipure goals are goals that are sensitive to the effects of impure goals.
% They may be reordered and optimized away just like pure goals, except that
% a semipure goal may behave differently after a call to an impure goal than
% before.  This means that semipure (as well as impure) predicates must not
% be tabled.  Further, duplicate semipure goals on different sides of an
% impure goal must not be optimized away.  In the current implementation, we
% simply do not optimize away duplicate semipure (or impure) goals at all.
%
% A predicate either has no purity declaration and so is assumed pure, or is
% declared semipure or impure, or is promised to be pure despite calling
% semipure or impure predicates.  This promise cannot be checked, so we must
% trust the programmer.
%
% See the language reference manual for more information on syntax and
% semantics.
%
% The current implementation now handles impure functions.
% They are limited to being used as part of an explicit unification
% with a purity indicator before the goal.
%
% 	impure X = some_impure_func(Arg1, Arg2, ...)
%
% This eliminates any need to define some order of evaluation of nested
% impure functions.
%
% Of course it also eliminates the benefits of using functions to
% cut down on the number of variables introduced.  The main use of
% impure functions is to interface nicely with foreign language
% functions.
%
% Any non-variable arguments to the function are flattened into
% unification goals (see make_hlds__unravel_unifications) which are
% placed as pure goals before the function call itself.
%
% Wishlist:
% 	It would be nice to use impure functions in DCG goals as well as
% 	normal unifications.
%
% 	It might be nice to allow
% 		X = impure some_impure_fuc(Arg1, Arg2, ...)
%	syntax as well.  But there are advantages to having the
%	impure/semipure annotation in a regular position (on the left
%	hand side of a goal) too.  If this is implemented it should
%	probably be handled in prog_io, and turned into an impure
%	unify item.
%
%	It may also be nice to allow semipure function calls to occur
%	inline (since ordering is not an issue for them).
%
%-----------------------------------------------------------------------------%
:- module check_hlds__purity.
:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module io, bool.

% The purity type itself is defined in prog_data.m as follows:
% :- type purity	--->	pure
% 			;	(semipure)
% 			;	(impure).

	% Purity check a whole module.  Also do the post-typecheck stuff
	% described above, and eliminate double negations and calls
	% to `private_builtin.unsafe_type_cast/2'.
	% The first argument specifies whether there were any type
	% errors (if so, we suppress some diagnostics in post_typecheck.m
	% because they are usually spurious).
	% The third argument specifies whether post_typecheck.m detected
	% any errors that would cause problems for later passes
	% (if so, we stop compilation after this pass).
	%
:- pred puritycheck(bool::in, bool::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

	% Rerun purity checking on a procedure after an optimization pass has
	% performed transformations which might affect the procedure's purity.
	% repuritycheck_proc makes sure that the goal_infos contain the correct
	% purity, and that the pred_info contains the promised_pure or
	% promised_semipure markers which might be needed if a promised pure
	% procedure was inlined into the procedure being checked.
	%
:- pred repuritycheck_proc(module_info::in, pred_proc_id::in, pred_info::in,
	pred_info::out) is det.

	% Sort of a "maximum" for impurity.
	%
:- func worst_purity(purity, purity) = purity.

	% Compare two purities.
	%
:- pred less_pure(purity::in, purity::in) is semidet.

	% Give an error message for unifications marked impure/semipure
	% that are not function calls (e.g. impure X = 4)
	%
:- pred impure_unification_expr_error(prog_context::in, purity::in,
	io::di, io::uo) is det.

:- implementation.

:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__inst_util.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__post_typecheck.
:- import_module check_hlds__type_util.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__unify_proc.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module map, varset, term, string, require, std_util.
:- import_module assoc_list, bool, int, list, set.

%-----------------------------------------------------------------------------%
%
% Public Predicates

puritycheck(FoundTypeError, PostTypecheckError, !HLDS, !IO) :-
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	globals__io_lookup_bool_option(verbose, Verbose, !IO),

	maybe_write_string(Verbose, "% Purity-checking clauses...\n", !IO),
	check_preds_purity(FoundTypeError, PostTypecheckError, !HLDS, !IO),
	maybe_report_stats(Statistics, !IO).

% worst_purity/3 could be written more compactly, but this definition
% guarantees us a determinism error if we add to type `purity'.  We also
% define less_pure/2 in terms of worst_purity/3 rather than the other way
% around for the same reason.

worst_purity(pure, pure) = pure.
worst_purity(pure, (semipure)) = (semipure).
worst_purity(pure, (impure)) = (impure).
worst_purity((semipure), pure) = (semipure).
worst_purity((semipure), (semipure)) = (semipure).
worst_purity((semipure), (impure)) = (impure).
worst_purity((impure), pure) = (impure).
worst_purity((impure), (semipure)) = (impure).
worst_purity((impure), (impure)) = (impure).

less_pure(P1, P2) :-
	\+ ( worst_purity(P1, P2) = P2).

%-----------------------------------------------------------------------------%

	% Purity-check the code for all the predicates in a module.

:- pred check_preds_purity(bool::in, bool::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_preds_purity(FoundTypeError, PostTypecheckError, !ModuleInfo, !IO) :-
	module_info_predids(!.ModuleInfo, PredIds),

	% Only report error messages for unbound type variables
	% if we didn't get any type errors already; this avoids
	% a lot of spurious diagnostics.
	ReportTypeErrors = bool__not(FoundTypeError),
	post_typecheck__finish_preds(PredIds, ReportTypeErrors, NumErrors1,
		PostTypecheckError, !ModuleInfo, !IO),

	check_preds_purity_2(PredIds, !ModuleInfo, NumErrors1, NumErrors, !IO),
	module_info_num_errors(!.ModuleInfo, Errs0),
	Errs = Errs0 + NumErrors,
	module_info_set_num_errors(Errs, !ModuleInfo).

:- pred check_preds_purity_2(list(pred_id)::in,
	module_info::in, module_info::out, int::in, int::out,
	io::di, io::uo) is det.

check_preds_purity_2([], !ModuleInfo, !NumErrors, !IO).
check_preds_purity_2([PredId | PredIds], !ModuleInfo, !NumErrors, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
	(
		( pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0)
		)
	->
		PredInfo = PredInfo0
	;
		write_pred_progress_message("% Purity-checking ", PredId,
			!.ModuleInfo, !IO),
		puritycheck_pred(PredId, PredInfo0, PredInfo, !.ModuleInfo,
			PurityErrsInThisPred, !IO),
		!:NumErrors = !.NumErrors + PurityErrsInThisPred,
		module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
	),

		% finish processing of promise declarations
	pred_info_get_goal_type(PredInfo, GoalType),
	( GoalType = promise(PromiseType) ->
		post_typecheck__finish_promise(PromiseType, PredId,
			!ModuleInfo, !IO)
	;
		true
	),
	check_preds_purity_2(PredIds, !ModuleInfo, !NumErrors, !IO).

	% Purity-check the code for single predicate, reporting any errors.

%-----------------------------------------------------------------------------%
%
% Check purity of a single predicate
%
% Purity checking is quite simple.  Since impurity /must/ be declared, we can
% perform a single pass checking that the actual purity of each predicate
% matches the declared (or implied) purity.  A predicate is just as pure as
% its least pure goal.  While we're doing this, we attach a `feature' to each
% goal that is not pure, including non-atomic goals, indicating its purity.
% This information must be maintained by later compilation passes, at least
% until after the last pass that may perform transformations that would not
% be correct for impure code.  As we check purity and attach impurity
% features, we also check that impure (semipure) atomic goals were marked in
% the source code as impure (semipure).  At this stage in the computation,
% this is indicated by already having the appropriate goal feature.  (During
% the translation from term to goal, calls have their purity attached to
% them, and in the translation from goal to hlds_goal, the attached purity is
% turned into the appropriate feature in the hlds_goal_info.)

:- pred puritycheck_pred(pred_id::in, pred_info::in, pred_info::out,
	module_info::in, int::out, io::di, io::uo) is det.

puritycheck_pred(PredId, !PredInfo, ModuleInfo, NumErrors, !IO) :-
	pred_info_get_purity(!.PredInfo, DeclPurity) ,
	pred_info_get_promised_purity(!.PredInfo, PromisedPurity),

	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	ProcIds = pred_info_procids(!.PredInfo),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	clauses_info_vartypes(ClausesInfo0, VarTypes0),
	clauses_info_varset(ClausesInfo0, VarSet0),
	RunPostTypecheck = yes,
	PurityInfo0 = purity_info(ModuleInfo, RunPostTypecheck,
		!.PredInfo, VarTypes0, VarSet0, []),
	pred_info_get_goal_type(!.PredInfo, GoalType),
	compute_purity(GoalType, Clauses0, Clauses, ProcIds, pure, Purity,
		PurityInfo0, PurityInfo),
	PurityInfo = purity_info(_, _, !:PredInfo,
		VarTypes, VarSet, RevMessages),
	clauses_info_set_vartypes(VarTypes, ClausesInfo0, ClausesInfo1),
	clauses_info_set_varset(VarSet, ClausesInfo1, ClausesInfo2),
	Messages = list__reverse(RevMessages),
	list__foldl(report_post_typecheck_message(ModuleInfo), Messages, !IO),
	NumErrors0 = list__length(
		list__filter((pred(error(_)::in) is semidet),
		Messages)),
	clauses_info_set_clauses(Clauses, ClausesInfo2, ClausesInfo),
	pred_info_set_clauses_info(ClausesInfo, !PredInfo),
	WorstPurity = Purity,
	perform_pred_purity_checks(!.PredInfo, Purity, DeclPurity,
		PromisedPurity, PurityCheckResult),
	(
		PurityCheckResult = inconsistent_promise,
		NumErrors = NumErrors0 + 1,
		error_inconsistent_promise(ModuleInfo, !.PredInfo, PredId,
			DeclPurity, !IO)
	;
		PurityCheckResult = unnecessary_decl,
		NumErrors = NumErrors0,
		warn_exaggerated_impurity_decl(ModuleInfo, !.PredInfo, PredId,
			DeclPurity, WorstPurity, !IO)
	;
		PurityCheckResult = insufficient_decl,
		NumErrors = NumErrors0 + 1,
		error_inferred_impure(ModuleInfo, !.PredInfo, PredId, Purity,
			!IO)
	;
		PurityCheckResult = unnecessary_promise_pure,
		NumErrors = NumErrors0,
		warn_unnecessary_promise_pure(ModuleInfo, !.PredInfo, PredId,
			PromisedPurity, !IO)
	;
		PurityCheckResult = no_worries,
		NumErrors = NumErrors0
	).

repuritycheck_proc(ModuleInfo, proc(_PredId, ProcId), !PredInfo) :-
	pred_info_procedures(!.PredInfo, Procs0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_varset(ProcInfo0, VarSet0),
	RunPostTypeCheck = no,
	PurityInfo0 = purity_info(ModuleInfo, RunPostTypeCheck,
		!.PredInfo, VarTypes0, VarSet0, []),
	compute_goal_purity(Goal0, Goal, Bodypurity, PurityInfo0, PurityInfo),
	PurityInfo = purity_info(_, _, !:PredInfo, VarTypes, VarSet, _),
	proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
	proc_info_set_vartypes(VarTypes, ProcInfo1, ProcInfo2),
	proc_info_set_varset(VarSet, ProcInfo2, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(Procs, !PredInfo),

	%
	% A predicate should never become less pure after inlining,
	% so update any promises in the pred_info if the purity of
	% the goal worsened (for example if a promised pure predicate
	% was inlined).
	%
	pred_info_get_purity(!.PredInfo, OldPurity),
	pred_info_get_markers(!.PredInfo, Markers0),
	(
		less_pure(Bodypurity, OldPurity)
	->
		(
			OldPurity = pure,
			remove_marker(promised_semipure, Markers0, Markers1),
			add_marker(promised_pure, Markers1, Markers)
		;
			OldPurity = (semipure),
			add_marker(promised_semipure, Markers0, Markers)
		;
			OldPurity = (impure),
			Markers = Markers0
		),
		pred_info_set_markers(Markers, !PredInfo)
	;
		less_pure(OldPurity, Bodypurity),
		[_] = pred_info_procids(!.PredInfo)
	->
		%
		% If there is only one procedure, update the
		% purity in the pred_info if the purity improved.
		%
		% XXX Storing the purity in the pred_info is the
		% wrong thing to do, because optimizations can
		% make some procedures more pure than others.
		%

		(
			Bodypurity = pure,
			remove_marker((impure), Markers0, Markers1),
			remove_marker((semipure), Markers1, Markers)
		;
			Bodypurity = (semipure),
			remove_marker((impure), Markers0, Markers1),
			add_marker((semipure), Markers1, Markers)
		;
			Bodypurity = (impure),
			Markers = Markers0
		),
		pred_info_set_markers(Markers, !PredInfo)
	;
		true
	).

% Infer the purity of a single (non-pragma c_code) predicate

:- pred compute_purity(goal_type::in, list(clause)::in, list(clause)::out,
	list(proc_id)::in, purity::in, purity::out,
	purity_info::in, purity_info::out) is det.

compute_purity(_, [], [], _, Purity, Purity, !Info).
compute_purity(GoalType, [Clause0 | Clauses0], [Clause | Clauses], ProcIds,
		Purity0, Purity, !Info) :-
	Clause0 = clause(Ids, Body0 - Info0, Lang, Context),
	compute_expr_purity(Body0, Body, Info0, Bodypurity0, !Info),
	% If this clause doesn't apply to all modes of this procedure,
	% i.e. the procedure has different clauses for different modes,
	% then we must treat it as impure.
	% the default impurity of foreign_proc procedures is handled when
	% processing the foreign_proc goal -- they are not counted as impure
	% here simply because they have different clauses for different modes
	(
		( applies_to_all_modes(Clause0, ProcIds)
		; GoalType = pragmas
		)
	->
		Clausepurity = (pure)
	;
		Clausepurity = (impure)
	),
	worst_purity(Bodypurity0, Clausepurity) = Bodypurity,
	add_goal_info_purity_feature(Info0, Bodypurity, Info),
	worst_purity(Purity0, Bodypurity) = Purity1,
	Clause = clause(Ids, Body - Info, Lang, Context),
	compute_purity(GoalType, Clauses0, Clauses, ProcIds, Purity1, Purity,
		!Info).

:- pred applies_to_all_modes(clause::in, list(proc_id)::in) is semidet.

applies_to_all_modes(clause(ClauseProcIds, _, _, _), ProcIds) :-
	(
		% an empty list here means that the clause applies
		% to *all* procedures
		ClauseProcIds = []
	;
		% Otherwise the clause applies to the procids in the
		% list.  Check if this is the same as the procids for
		% this procedure.
		list__sort(ClauseProcIds, SortedIds),
		SortedIds = ProcIds
	).

:- pred compute_expr_purity(hlds_goal_expr::in, hlds_goal_expr::out,
	hlds_goal_info::in, purity::out, purity_info::in, purity_info::out)
	is det.

compute_expr_purity(conj(Goals0), conj(Goals), _, Purity, !Info) :-
	compute_goals_purity(Goals0, Goals, pure, Purity, !Info).
compute_expr_purity(par_conj(Goals0), par_conj(Goals), _, Purity, !Info) :-
	compute_goals_purity(Goals0, Goals, pure, Purity, !Info).
compute_expr_purity(Goal0, Goal, GoalInfo, ActualPurity, !Info) :-
	Goal0 = call(PredId0, ProcId, Vars, BIState, UContext, Name0),
	RunPostTypecheck = !.Info ^ run_post_typecheck,
	PredInfo = !.Info ^ pred_info,
	ModuleInfo = !.Info ^ module_info,
	(
		RunPostTypecheck = yes,
		post_typecheck__resolve_pred_overloading(Vars, PredInfo,
			ModuleInfo, Name0, Name, PredId0, PredId),
		(
			% Convert any calls to private_builtin.unsafe_type_cast
			% into unsafe_cast goals.
			Name = qualified(
				mercury_private_builtin_module,
				"unsafe_type_cast"),
			Vars = [InputArg, OutputArg]
		->
			Goal = generic_call(unsafe_cast,
				[InputArg, OutputArg],
				[in_mode, out_mode], det)
		;
			Goal = call(PredId, ProcId, Vars,
				BIState, UContext, Name)
		)
	;
		RunPostTypecheck = no,
		PredId = PredId0,
		Goal = Goal0
	),
	infer_goal_info_purity(GoalInfo, DeclaredPurity),
	goal_info_get_context(GoalInfo, CallContext),
	perform_goal_purity_checks(CallContext, PredId,
		DeclaredPurity, ActualPurity, !Info).
compute_expr_purity(generic_call(GenericCall0, Args, Modes0, Det),
		GoalExpr, GoalInfo, Purity, !Info) :-
	(
		GenericCall0 = higher_order(_, Purity, _, _),
		GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
	;
		GenericCall0 = class_method(_, _, _, _),
		Purity = pure, % XXX this is wrong!
		GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
	;
		GenericCall0 = unsafe_cast,
		Purity = pure,
		GoalExpr = generic_call(GenericCall0, Args, Modes0, Det)
	;
		GenericCall0 = aditi_builtin(Builtin0, CallId0),
		Purity = pure,
		goal_info_get_context(GoalInfo, Context),
		RunPostTypecheck = !.Info ^ run_post_typecheck,
		(
			RunPostTypecheck = yes,
			ModuleInfo = !.Info ^ module_info,
			PredInfo = !.Info ^ pred_info,
			post_typecheck__finish_aditi_builtin(ModuleInfo,
				PredInfo, Args, Context, Builtin0, Builtin,
				CallId0, CallId, Modes, MaybeMessage),
			(
				MaybeMessage = yes(Message),
				purity_info_add_message(
					error(aditi_builtin_error(Message)),
					!Info)
			;
				MaybeMessage = no
			),
			GenericCall = aditi_builtin(Builtin, CallId)
		;
			RunPostTypecheck = no,
			GenericCall = GenericCall0,
			Modes = Modes0
		),
		GoalExpr = generic_call(GenericCall, Args, Modes, Det)
	).
compute_expr_purity(switch(Var, Canfail, Cases0),
		switch(Var, Canfail, Cases), _, Purity, !Info) :-
	compute_cases_purity(Cases0, Cases, pure, Purity, !Info).
compute_expr_purity(Unif0, GoalExpr, GoalInfo, ActualPurity, !Info) :-
	Unif0 = unify(Var, RHS0, Mode, Unification, UnifyContext),
	(
		RHS0 = lambda_goal(LambdaPurity, F, EvalMethod,
			FixModes, H, Vars, Modes0, K, Goal0 - Info0)
	->
		RHS = lambda_goal(LambdaPurity, F, EvalMethod,
			modes_are_ok, H, Vars, Modes, K, Goal - Info0),
		compute_expr_purity(Goal0, Goal, Info0, GoalPurity, !Info),
		check_closure_purity(GoalInfo, LambdaPurity, GoalPurity,
			!Info),

		VarTypes = !.Info ^ vartypes,
		(
			FixModes = modes_are_ok,
			Modes = Modes0
		;
			FixModes = modes_need_fixing,
			(
				EvalMethod = normal,
				error("compute_expr_purity: modes need " ++
					"fixing for normal lambda_goal")
			;
				EvalMethod = (aditi_bottom_up),
				% Make sure `aditi_bottom_up' expressions have
				% a `ui' mode for their aditi_state.
				StateMode = aditi_mui_mode
			),
			map__apply_to_list(Vars, VarTypes, LambdaVarTypes),
			SeenState = no,
			fix_aditi_state_modes(SeenState, StateMode,
				LambdaVarTypes, Modes0, Modes)
		),
		GoalExpr = unify(Var, RHS, Mode, Unification, UnifyContext),
		% the unification itself is always pure,
		% even if the lambda expression body is impure
		ActualPurity = (pure)
	;
		RHS0 = functor(ConsId, _, Args)
	->
		RunPostTypecheck = !.Info ^ run_post_typecheck,
		(
			RunPostTypecheck = yes,
			ModuleInfo = !.Info ^ module_info,
			PredInfo0 = !.Info ^ pred_info,
			VarTypes0 = !.Info ^ vartypes,
			VarSet0 = !.Info ^ varset,
			post_typecheck__resolve_unify_functor(Var, ConsId,
				Args, Mode, Unification, UnifyContext,
				GoalInfo, ModuleInfo, PredInfo0, PredInfo,
				VarTypes0, VarTypes, VarSet0, VarSet, Goal1),
			!:Info = !.Info ^ vartypes := VarTypes,
			!:Info = !.Info ^ varset := VarSet,
			!:Info = !.Info ^ pred_info := PredInfo
		;
			RunPostTypecheck = no,
			Goal1 = Unif0 - GoalInfo
		),
		( Goal1 = unify(_, _, _, _, _) - _ ->
			check_higher_order_purity(GoalInfo, ConsId,
				Var, Args, ActualPurity, !Info),
			Goal = Goal1
		;
			compute_goal_purity(Goal1, Goal, ActualPurity, !Info)
		),
		Goal = GoalExpr - _
	;
		GoalExpr = Unif0,
		ActualPurity = pure
	).
compute_expr_purity(disj(Goals0), disj(Goals), _, Purity, !Info) :-
	compute_goals_purity(Goals0, Goals, pure, Purity, !Info).
compute_expr_purity(not(Goal0), NotGoal, GoalInfo0, Purity, !Info) :-
	%
	% eliminate double negation
	%
	negate_goal(Goal0, GoalInfo0, NotGoal0),
	( NotGoal0 = not(Goal1) - _GoalInfo1 ->
		compute_goal_purity(Goal1, Goal, Purity, !Info),
		NotGoal = not(Goal)
	;
		compute_goal_purity(NotGoal0, NotGoal1, Purity, !Info),
		NotGoal1 = NotGoal - _
	).
compute_expr_purity(some(Vars, CanRemove, Goal0), some(Vars, CanRemove, Goal),
		_, Purity, !Info) :-
	compute_goal_purity(Goal0, Goal, Purity, !Info).
compute_expr_purity(if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else), _, Purity, !Info) :-
	compute_goal_purity(Cond0, Cond, Purity1, !Info),
	compute_goal_purity(Then0, Then, Purity2, !Info),
	compute_goal_purity(Else0, Else, Purity3, !Info),
	worst_purity(Purity1, Purity2) = Purity12,
	worst_purity(Purity12, Purity3) = Purity.
compute_expr_purity(ForeignProc0, ForeignProc, _, Purity, !Info) :-
	ForeignProc0 = foreign_proc(_, _, _, _, _, _),
	Attributes = ForeignProc0 ^ foreign_attr,
	PredId = ForeignProc0 ^ foreign_pred_id,
	ModuleInfo = !.Info ^ module_info,
	( legacy_purity_behaviour(Attributes) = yes ->
		% get the purity from the declaration, and set it
		% here so that it is correct for later use

		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_purity(PredInfo, Purity),
		set_purity(Purity, Attributes, NewAttributes),
		ForeignProc = ForeignProc0 ^ foreign_attr := NewAttributes
	;
		ForeignProc = ForeignProc0,
		Purity = purity(Attributes)
	).

compute_expr_purity(shorthand(_), _, _, _, !Info) :-
	% these should have been expanded out by now
	error("compute_expr_purity: unexpected shorthand").

:- pred check_higher_order_purity(hlds_goal_info::in, cons_id::in,
	prog_var::in, list(prog_var)::in, purity::out,
	purity_info::in, purity_info::out) is det.

check_higher_order_purity(GoalInfo, ConsId, Var, Args, ActualPurity, !Info) :-
	%
	% Check that the purity of the ConsId matches the purity of the
	% variable's type.
	%
	VarTypes = !.Info ^ vartypes,
	map__lookup(VarTypes, Var, TypeOfVar),
	(
		ConsId = cons(PName, _),
		type_is_higher_order(TypeOfVar, TypePurity, PredOrFunc,
			_EvalMethod, VarArgTypes)
	->
		PredInfo = !.Info ^ pred_info,
		pred_info_typevarset(PredInfo, TVarSet),
		map__apply_to_list(Args, VarTypes, ArgTypes0),
		list__append(ArgTypes0, VarArgTypes, PredArgTypes),
		ModuleInfo = !.Info ^ module_info,
		CallerPredInfo = !.Info ^ pred_info,
		pred_info_get_markers(CallerPredInfo, CallerMarkers),
		(
			get_pred_id(calls_are_fully_qualified(CallerMarkers),
				PName, PredOrFunc, TVarSet, PredArgTypes,
				ModuleInfo, CalleePredId)
		->
			module_info_pred_info(ModuleInfo,
				CalleePredId, CalleePredInfo),
			pred_info_get_purity(CalleePredInfo, CalleePurity),
			check_closure_purity(GoalInfo, TypePurity,
				CalleePurity, !Info)
		;
			% If we can't find the type of the function,
			% it's because typecheck couldn't give it one.
			% Typechecking gives an error in this case, we
			% just keep silent.
			true
		)
	;
		true
	),

	% The unification itself is always pure,
	% even if it is a unification with an impure higher-order term.
	ActualPurity = pure,

	% Check for a bogus purity annotation on the unification
	infer_goal_info_purity(GoalInfo, DeclaredPurity),
	( DeclaredPurity \= pure ->
		goal_info_get_context(GoalInfo, Context),
		Message = impure_unification_expr_error(Context,
			DeclaredPurity),
		purity_info_add_message(error(Message), !Info)
	;
		true
	).

	% the possible results of a purity check
:- type purity_check_result
	--->	no_worries		% all is well
	;	insufficient_decl	% purity decl is less than
					% required.
	;	inconsistent_promise    % promise is given
					% but decl is impure
	;	unnecessary_promise_pure % purity promise is given
					% but not required
	;	unnecessary_decl.	% purity decl is more than is
					% required.

	% Peform purity checking of the actual and declared purity,
	% and check that promises are consistent.
	%
	% ActualPurity: The inferred purity of the pred
	% DeclaredPurity: The declared purity of the pred
	% InPragmaCCode: Is this a pragma c code?
	% Promised: Did we promise this pred as pure?
	%
:- pred perform_pred_purity_checks(pred_info::in, purity::in, purity::in,
	purity::in, purity_check_result::out) is det.

perform_pred_purity_checks(PredInfo, ActualPurity, DeclaredPurity,
		PromisedPurity, PurityCheckResult) :-
	(
		% The declared purity must match any promises.
		% (A promise of impure means no promise was made).
		PromisedPurity \= (impure), DeclaredPurity \= PromisedPurity
	->
		PurityCheckResult = inconsistent_promise
	;
		% You shouldn't promise pure unnecessarily.
		% It's OK in the case of foreign_procs though.
		PromisedPurity \= (impure), ActualPurity = PromisedPurity,
		not pred_info_pragma_goal_type(PredInfo)
	->
		PurityCheckResult = unnecessary_promise_pure
	;
		% The purity should match the declaration.
		ActualPurity = DeclaredPurity
	->
		PurityCheckResult = no_worries
	;
		less_pure(ActualPurity, DeclaredPurity)
	->
		( PromisedPurity = (impure) ->
			PurityCheckResult = insufficient_decl
		;
			PurityCheckResult = no_worries
		)
	;
		% We don't warn about exaggerated impurity decls in
		% class methods or instance methods --- it just
		% means that the predicate provided as an
		% implementation was more pure than necessary.
		%
		% We don't warn about exaggerated impurity
		% decls in c_code -- this is just because we
		% assume they are pure, but you can declare them
		% to be impure.
		%
		% We don't warn about exaggerated impurity declarations
		% for "stub" procedures, i.e. procedures which
		% originally had no clauses.

		pred_info_get_markers(PredInfo, Markers),
		pred_info_get_goal_type(PredInfo, GoalType),
		(
			GoalType = pragmas
		;
			GoalType = clauses_and_pragmas
		;
			check_marker(Markers, class_method)
		;
			check_marker(Markers, class_instance_method)
		;
			check_marker(Markers, stub)
		)
	->
		PurityCheckResult = no_worries
	;
		PurityCheckResult = unnecessary_decl
	).

	% Peform purity checking of the actual and declared purity,
	% and check that promises are consistent.
	%
	% ActualPurity: The inferred purity of the goal
	% DeclaredPurity: The declared purity of the goal
:- pred perform_goal_purity_checks(prog_context::in, pred_id::in, purity::in,
	purity::out, purity_info::in, purity_info::out) is det.

perform_goal_purity_checks(Context, PredId, DeclaredPurity, ActualPurity,
		!Info) :-
	ModuleInfo = !.Info ^ module_info,
	PredInfo = !.Info ^ pred_info,
	module_info_pred_info(ModuleInfo, PredId, CalleePredInfo),
	pred_info_get_purity(CalleePredInfo, ActualPurity),
	(
		% The purity of the callee should match the
		% purity declared at the call
		ActualPurity = DeclaredPurity
	->
		true
	;
		% Don't require purity annotations on calls in
		% compiler-generated code.
		is_unify_or_compare_pred(PredInfo)
	->
		true
	;
		less_pure(ActualPurity, DeclaredPurity)
	->
		purity_info_add_message(
			error(missing_body_impurity_error(Context, PredId)),
			!Info)
	;
		% We don't warn about exaggerated impurity decls in
		% class methods or instance methods --- it just
		% means that the predicate provided as an
		% implementation was more pure than necessary.

		pred_info_get_markers(PredInfo, Markers),
		(
			check_marker(Markers, class_method)
		;
			check_marker(Markers, class_instance_method)
		)
	->
		true
	;
		purity_info_add_message(
			warning(unnecessary_body_impurity_decl(Context,
				PredId, DeclaredPurity)),
			!Info)
	).

:- pred compute_goal_purity(hlds_goal::in, hlds_goal::out, purity::out,
	purity_info::in, purity_info::out) is det.

compute_goal_purity(Goal0 - GoalInfo0, Goal - GoalInfo, Purity, !Info) :-
	compute_expr_purity(Goal0, Goal, GoalInfo0, Purity, !Info),
	add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo).

	% Compute the purity of a list of hlds_goals.  Since the purity of a
	% disjunction is computed the same way as the purity of a conjunction,
	% we use the same code for both

:- pred compute_goals_purity(list(hlds_goal)::in, list(hlds_goal)::out,
	purity::in, purity::out, purity_info::in, purity_info::out) is det.

compute_goals_purity([], [], !Purity, !Info).
compute_goals_purity([Goal0 | Goals0], [Goal | Goals], !Purity, !Info) :-
	compute_goal_purity(Goal0, Goal, GoalPurity, !Info),
	worst_purity(GoalPurity, !.Purity) = !:Purity,
	compute_goals_purity(Goals0, Goals, !Purity, !Info).

:- pred compute_cases_purity(list(case)::in, list(case)::out,
	purity::in, purity::out, purity_info::in, purity_info::out) is det.

compute_cases_purity([], [], !Purity, !Info).
compute_cases_purity([case(Ctor, Goal0) | Cases0], [case(Ctor, Goal) | Cases],
		!Purity, !Info) :-
	compute_goal_purity(Goal0, Goal, GoalPurity, !Info),
	worst_purity(GoalPurity, !.Purity) = !:Purity,
	compute_cases_purity(Cases0, Cases, !Purity, !Info).

	% Make sure lambda expressions introduced by the compiler
	% have the correct mode for their `aditi__state' arguments.
:- pred fix_aditi_state_modes(bool::in, (mode)::in, list(type)::in,
	list(mode)::in, list(mode)::out) is det.

fix_aditi_state_modes(_, _, [], [], []).
fix_aditi_state_modes(_, _, [_|_], [], []) :-
	error("purity:fix_aditi_state_modes").
fix_aditi_state_modes(_, _, [], [_|_], []) :-
	error("purity:fix_aditi_state_modes").
fix_aditi_state_modes(SeenState0, AditiStateMode, [Type | Types],
		[ArgMode0 | Modes0], [ArgMode | Modes]) :-
	( type_is_aditi_state(Type) ->
		(
			SeenState0 = yes,
			% The only Aditi builtin which takes a closure
			% with two `aditi__state' arguments is
			% `aditi_bulk_modify'.
			% The second `aditi__state' argument has mode
			% unused.
			unused_mode(ArgMode)
		;
			SeenState0 = no,
			ArgMode = AditiStateMode
		),
		SeenState = yes
	;
		ArgMode = ArgMode0,
		SeenState = SeenState0
	),
	fix_aditi_state_modes(SeenState, AditiStateMode, Types, Modes0, Modes).

%-----------------------------------------------------------------------------%

:- func pred_context(module_info, pred_info, pred_id) = list(format_component).

pred_context(ModuleInfo, _PredInfo, PredId) = Pieces :-
	PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
		PredId),
	Pieces = [words("In")] ++ PredPieces ++ [suffix(":"), nl].

:- pred error_inconsistent_promise(module_info::in, pred_info::in,
	pred_id::in, purity::in, io::di, io::uo) is det.

error_inconsistent_promise(ModuleInfo, PredInfo, PredId, Purity, !IO) :-
	pred_info_context(PredInfo, Context),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
	purity_name(Purity, PurityName),
	PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
	Pieces1 = PredContextPieces ++
		[words("warning: declared"), fixed(PurityName),
		words("but promised pure.")],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		Pieces = Pieces1 ++
			[words("A pure"), fixed(PredOrFuncStr),
			words("that invokes impure or semipure code"),
			words("should be promised pure and should have"),
			words("no impurity declaration.")]
	;
		VerboseErrors = no,
		Pieces = Pieces1
	),
	write_error_pieces(Context, 0, Pieces, !IO),
	record_warning(!IO).

:- pred warn_exaggerated_impurity_decl(module_info::in, pred_info::in,
	pred_id::in, purity::in, purity::in,
	io::di, io::uo) is det.

warn_exaggerated_impurity_decl(ModuleInfo, PredInfo, PredId,
		DeclPurity, ActualPurity, !IO) :-
	pred_info_context(PredInfo, Context),
	PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
	purity_name(DeclPurity, DeclPurityName),
	purity_name(ActualPurity, ActualPurityName),
	Pieces = PredContextPieces ++
		[words("warning: declared"), fixed(DeclPurityName),
		words("but actually"), fixed(ActualPurityName ++ ".")],
	write_error_pieces(Context, 0, Pieces, !IO),
	record_warning(!IO).

:- pred warn_unnecessary_promise_pure(module_info::in, pred_info::in,
	pred_id::in, purity::in, io::di, io::uo) is det.

warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId, PromisedPurity,
		!IO) :-
	pred_info_context(PredInfo, Context),
	PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
	(
		PromisedPurity = pure,
		Pragma = "promise_pure",
		CodeStr = "impure or semipure"
	;
		PromisedPurity = (semipure),
		Pragma = "promise_semipure",
		CodeStr = "impure"
	;
		PromisedPurity = (impure),
		error("purity__warn_unnecessary_promise_pure: promise_impure?")
	),
	Pieces1 = [words("warning: unnecessary `" ++ Pragma ++ "' pragma."),
		nl],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		PredOrFunc = pred_info_is_pred_or_func(PredInfo),
		PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
		Pieces2 = [words("This"), fixed(PredOrFuncStr),
			words("does not invoke any"), fixed(CodeStr),
			words("code, so there is no need for a"),
			words("`" ++ Pragma ++ "' pragma.")],
		Pieces = PredContextPieces ++ Pieces1 ++ Pieces2
	;
		VerboseErrors = no,
		Pieces = PredContextPieces ++ Pieces1
	),
	write_error_pieces(Context, 0, Pieces, !IO),
	record_warning(!IO).

:- pred error_inferred_impure(module_info::in, pred_info::in, pred_id::in,
	purity::in, io::di, io::uo) is det.

error_inferred_impure(ModuleInfo, PredInfo, PredId, Purity, !IO) :-
	pred_info_context(PredInfo, Context),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
	PredContextPieces = pred_context(ModuleInfo, PredInfo, PredId),
	pred_info_get_purity(PredInfo, DeclaredPurity),
	purity_name(Purity, PurityName),
	purity_name(DeclaredPurity, DeclaredPurityName),

	Pieces1 = [words("purity error:"), fixed(PredOrFuncStr),
		words("is"), fixed(PurityName ++ "."), nl],
	( is_unify_or_compare_pred(PredInfo) ->
		Pieces2 = [words("It must be pure.")]
	;
		Pieces2 = [words("It must be declared"),
			fixed("`" ++ PurityName ++ "'"),
			words("or promised"),
			fixed(DeclaredPurityName ++ ".")]
	),
	write_error_pieces(Context, 0, PredContextPieces ++ Pieces1 ++ Pieces2,
		!IO).

	% Errors and warnings reported by purity.m and post_typecheck.m
	% for problems within a goal.
:- type post_typecheck_message
	--->	error(post_typecheck_error)
	;	warning(post_typecheck_warning).

:- type post_typecheck_messages == list(post_typecheck_message).

:- type post_typecheck_error
	--->	missing_body_impurity_error(prog_context, pred_id)
	;	closure_purity_error(prog_context, purity, purity)
		% closure_purity_error(Context, DeclaredPurity, ActualPurity)
	;	impure_unification_expr_error(prog_context, purity)
	;	aditi_builtin_error(aditi_builtin_error).

:- type post_typecheck_warning
	--->	unnecessary_body_impurity_decl(prog_context, pred_id, purity).

:- pred report_post_typecheck_message(module_info::in,
	post_typecheck_message::in, io::di, io::uo) is det.

report_post_typecheck_message(ModuleInfo, error(Message), !IO) :-
	io__set_exit_status(1, !IO),
	(
		Message = missing_body_impurity_error(Context, PredId),
		error_missing_body_impurity_decl(ModuleInfo, PredId, Context,
			!IO)
	;
		Message = closure_purity_error(Context, DeclaredPurity,
			ActualPurity),
		report_error_closure_purity(Context, DeclaredPurity,
			ActualPurity, !IO)
	;
		Message = impure_unification_expr_error(Context, Purity),
		impure_unification_expr_error(Context, Purity, !IO)
	;
		Message = aditi_builtin_error(AditiError),
		report_aditi_builtin_error(AditiError, !IO)
	).

report_post_typecheck_message(ModuleInfo, Warning, !IO) :-
	Warning = warning(unnecessary_body_impurity_decl(Context,
		PredId, DeclaredPurity)),
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
	(
		HaltAtWarn = yes,
		io__set_exit_status(1, !IO)
	;
		HaltAtWarn = no
	),
	warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
		DeclaredPurity, !IO).

:- pred error_missing_body_impurity_decl(module_info::in, pred_id::in,
	prog_context::in, io::di, io::uo) is det.

error_missing_body_impurity_decl(ModuleInfo, PredId, Context, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_get_purity(PredInfo, Purity),
	purity_name(Purity, PurityName),
	PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
		PredId),
	Pieces1 = [words("In call to "), fixed(PurityName)] ++
		PredPieces ++ [suffix(":"), nl],
	(
		PredOrFunc = predicate,
		Pieces2 = [words("purity error: call must be preceded by"),
			fixed("`" ++ PurityName ++ "'"),
			words("indicator.")]
	;
		PredOrFunc = function,
		Pieces2 = [words("purity error: call must be in an " ++
			"explicit unification which is preceded by"),
			fixed("`" ++ PurityName ++ "'"),
			words("indicator.")]
	),
	write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO).

:- pred warn_unnecessary_body_impurity_decl(module_info::in, pred_id::in,
	prog_context::in, purity::in, io::di, io::uo) is det.

warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
		DeclaredPurity, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_purity(PredInfo, ActualPurity),
	purity_name(DeclaredPurity, DeclaredPurityName),
	purity_name(ActualPurity, ActualPurityName),
	PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
		PredId),

	Pieces1 = [words("In call to")] ++ PredPieces ++ [suffix(":"), nl,
		words("warning: unnecessary"),
		fixed("`" ++ DeclaredPurityName ++ "'"),
		words("indicator."), nl],
	( ActualPurity = pure ->
		Pieces2 = [words("No purity indicator is necessary.")]
	;
		Pieces2 = [words("A purity indicator of"),
			fixed("`" ++ ActualPurityName ++ "'"),
			words("is sufficient.")]
	),
	write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO).

:- pred check_closure_purity(hlds_goal_info::in, purity::in, purity::in,
	purity_info::in, purity_info::out) is det.

check_closure_purity(GoalInfo, DeclaredPurity, ActualPurity, !IO) :-
	( ActualPurity `less_pure` DeclaredPurity ->
		goal_info_get_context(GoalInfo, Context),
		purity_info_add_message(error(closure_purity_error(Context,
			DeclaredPurity, ActualPurity)), !IO)
	;
		% we don't bother to warn if the DeclaredPurity is less
		% pure than the ActualPurity; that would lead to too many
		% spurious warnings.
		true
	).

:- pred report_error_closure_purity(prog_context::in, purity::in, purity::in,
	io::di, io::uo) is det.

report_error_closure_purity(Context, _DeclaredPurity, ActualPurity, !IO) :-
	purity_name(ActualPurity, ActualPurityName),
	Pieces = [words("Purity error in closure: closure body is"),
		fixed(ActualPurityName ++ ","),
		words("but closure was not declared"),
		fixed(ActualPurityName ++ ".")],
	write_error_pieces(Context, 0, Pieces, !IO).

impure_unification_expr_error(Context, Purity, !IO) :-
	purity_name(Purity, PurityName),
	Pieces = [words("Purity error: unification with expression"),
		words("was declared"), fixed(PurityName ++ ","),
		words("but expression was not a function call.")],
	write_error_pieces(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

:- type purity_info
	--->	purity_info(
			% fields not changed by purity checking.
			module_info		:: module_info,
			run_post_typecheck	:: bool,

			% fields which may be changed.
			pred_info		:: pred_info,
			vartypes		:: vartypes,
			varset			:: prog_varset,
			messages		:: post_typecheck_messages
		).

:- pred purity_info_add_message(post_typecheck_message::in,
	purity_info::in, purity_info::out) is det.

purity_info_add_message(Message, Info,
	Info ^ messages := [Message | Info ^ messages]).

%-----------------------------------------------------------------------------%
