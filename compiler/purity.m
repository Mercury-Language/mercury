%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2002 The University of Melbourne.
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
%  The main purpose of this module is check the consistency of the
%  `impure' and `promise_pure' (etc.) declarations, and to thus report
%  error messages if the program is not "purity-correct".
%  This includes treating procedures with different clauses for
%  different modes as impure, unless promised pure.
%
%  This module also does two final parts of type analysis:
%	- it resolves predicate overloading
%	  (perhaps it ought to also resolve function overloading,
%	  converting unifications that are function calls into
%	  HLDS call instructions, but currently that is done
%	  in polymorphism.m)
%	- it checks for unbound type variables and if there are any,
%	  it reports an error (or a warning, binding them to the type `void');
%	  similarly it checks for unsatisfied type class constraints.
%
%  These actions cannot be done until after type inference is complete,
%  so they need to be a separate "post-typecheck pass"; they are done
%  here in combination with the purity-analysis pass for efficiency reasons.
%
%  We also do elimination of double-negation in this pass.
%  It needs to be done somewhere after quantification analysis and
%  before mode analysis, and this is convenient place to do it.
%
%
%  The aim of Mercury's purity system is to allow one to declare certain parts
%  of one's program to be impure, thereby forbidding the compiler from making
%  certain optimizations to that part of the code.  Since one can often
%  implement a perfectly pure predicate or function in terms of impure
%  predicates and functions, one is also allowed to promise to the compiler
%  that a predicate *is* pure, despite calling impure predicates and
%  functions.
%
%  To keep purity/impurity consistent, it is required that every impure
%  predicate/function be declared so.  A predicate is impure if:
%
%	1.  It's declared impure, or
%	2a. It's not promised pure, and
%	2b. It calls some impure predicates or functions.
%
%  A predicate or function is declared impure by preceding the `pred' or
%  `func' in its declaration with `impure'.  It is promised to be pure with a
%
%	:- pragma promise_pure(Name/Arity).
%
%  directive.
%
%  Calls to impure predicates may not be optimized away.  Neither may they be
%  reodered relative to any other goals in a given conjunction; ie, an impure
%  goal cleaves a conjunction into the stuff before it and the stuff after it.
%  Both of these groups may be reordered separately, but no goal from either
%  group may move into the other.  Similarly for disjunctions.
%
%  Semipure goals are goals that are sensitive to the effects of impure goals.
%  They may be reordered and optimized away just like pure goals, except that
%  a semipure goal may behave differently after a call to an impure goal than
%  before.  This means that semipure (as well as impure) predicates must not
%  be tabled.  Further, duplicate semipure goals on different sides of an
%  impure goal must not be optimized away.  In the current implementation, we
%  simply do not optimize away duplicate semipure (or impure) goals at all.
%
%  A predicate either has no purity declaration and so is assumed pure, or is
%  declared semipure or impure, or is promised to be pure despite calling
%  semipure or impure predicates.  This promise cannot be checked, so we must
%  trust the programmer.
%
%  See the language reference manual for more information on syntax and
%  semantics.
%
%  The current implementation now handles impure functions. 
%  They are limited to being used as part of an explicit unification
%  with a purity indicator before the goal.
%
%  	impure X = some_impure_func(Arg1, Arg2, ...)
%  
%  This eliminates any need to define some order of evaluation of nested
%  impure functions.
%
%  Of course it also eliminates the benefits of using functions to
%  cut down on the number of variables introduced.  The main use of
%  impure functions is to interface nicely with foreign language
%  functions.  
%
%  Any non-variable arguments to the function are flattened into
%  unification goals (see make_hlds__unravel_unifications) which are 
%  placed as pure goals before the function call itself.
%
%  Wishlist:
%  	It would be nice to use impure functions in DCG goals as well as
%  	normal unifications.  
%
%  	We could give better error messages for impure calls inside
%  	closures.  It's possible to give the context of these calls,
%  	although we should be careful to pinpoint these as the source of
%  	the error (no impurity allowed in closures) rather than as
%  	errors to be corrected.
%
%  	It might be nice to allow
%  		X = impure some_impure_fuc(Arg1, Arg2, ...)
%	syntax as well.  But there are advantages to having the
%	impure/semipure annotation in a regular position (on the left 
%	hand side of a goal) too.  If this is implemented it should
%	probably be handled in prog_io, and turned into an impure
%	unify item.
%
%	It may also be nice to allow semipure function calls to occur
%	inline (since ordering is not an issue for them).
%


:- module check_hlds__purity.
:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_module, hlds__hlds_goal.
:- import_module hlds__hlds_pred.
:- import_module io, bool, list.

% The purity type itself is defined in prog_data.m as follows:
% :- type purity	--->	pure
% 			;	(semipure)
% 			;	(impure).

%  Purity check a whole module.  Also do the post-typecheck stuff
%  described above, and eliminate double negations.
%  The first argument specifies whether there were any type
%  errors (if so, we suppress some diagnostics in post_typecheck.m
%  because they are usually spurious).
%  The third argument specifies whether post_typecheck.m detected
%  any errors that would cause problems for later passes
%  (if so, we stop compilation after this pass).
:- pred puritycheck(bool, module_info, bool, module_info, io__state, io__state).
:- mode puritycheck(in, in, out, out, di, uo) is det.

% Rerun purity checking on a procedure after an optimization pass has
% performed transformations which might affect the procedure's purity.
% repuritycheck_proc makes sure that the goal_infos contain the correct
% purity, and that the pred_info contains the promised_pure or
% promised_semipure markers which might be needed if a promised pure
% procedure was inlined into the procedure being checked. 
:- pred repuritycheck_proc(module_info, pred_proc_id, pred_info, pred_info).
:- mode repuritycheck_proc(in, in, in, out) is det.

%  Sort of a "maximum" for impurity.
:- pred worst_purity(purity, purity, purity).
:- mode worst_purity(in, in, out) is det.

%  Compare two purities.
:- pred less_pure(purity, purity).
:- mode less_pure(in, in) is semidet.

%  Print out a purity name.
:- pred write_purity(purity, io__state, io__state).
:- mode write_purity(in, di, uo) is det.

%  Print out a purity prefix.
%  This works under the assumptions that all purity names but `pure' are prefix
%  Operators, and that we never need `pure' indicators/declarations.
:- pred write_purity_prefix(purity, io__state, io__state).
:- mode write_purity_prefix(in, di, uo) is det.

:- func purity_prefix_to_string(purity) = string.

%  Get a purity name as a string.
:- pred purity_name(purity, string).
:- mode purity_name(in, out) is det.

%  Update a goal info to reflect the specified purity
:- pred add_goal_info_purity_feature(hlds_goal_info, purity, hlds_goal_info).
:- mode add_goal_info_purity_feature(in, in, out) is det.

%  Determine the purity of a goal from its hlds_goal_info.
:- pred infer_goal_info_purity(hlds_goal_info, purity).
:- mode infer_goal_info_purity(in, out) is det.

%  Check if a hlds_goal_info is for a pure goal
:- pred goal_info_is_pure(hlds_goal_info).
:- mode goal_info_is_pure(in) is semidet.

%  Check if a hlds_goal_info is for an impure goal.  Fails if the goal is
%  semipure, so this isn't the same as \+ goal_info_is_pure.
:- pred goal_info_is_impure(hlds_goal_info).
:- mode goal_info_is_impure(in) is semidet.

% Work out the purity of a list of goals. 
:- pred goal_list_purity(list(hlds_goal), purity).
:- mode goal_list_purity(in, out) is det.

% Give an error message for unifications marked impure/semipure that are  
% not function calls (e.g. impure X = 4)
:- pred impure_unification_expr_error(prog_context, purity,
	io__state, io__state).
:- mode impure_unification_expr_error(in, in, di, uo) is det.

:- implementation.

:- import_module hlds__hlds_data, parse_tree__prog_io_util.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module ll_backend__code_util, parse_tree__prog_data.
:- import_module check_hlds__unify_proc.
:- import_module libs__globals, libs__options, parse_tree__mercury_to_mercury.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux, check_hlds__typecheck.
:- import_module parse_tree__module_qual, check_hlds__clause_to_proc.
:- import_module check_hlds__inst_util, parse_tree__prog_out.
:- import_module check_hlds__post_typecheck.

:- import_module map, varset, term, string, require, std_util.
:- import_module assoc_list, bool, int, set.

%-----------------------------------------------------------------------------%
%				Public Predicates


puritycheck(FoundTypeError, HLDS0, PostTypecheckError, HLDS) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),

	maybe_write_string(Verbose, "% Purity-checking clauses...\n"),
	check_preds_purity(FoundTypeError, HLDS0, PostTypecheckError, HLDS),
	maybe_report_stats(Statistics).


%  worst_purity/3 could be written more compactly, but this definition
%  guarantees us a determinism error if we add to type `purity'.  We also
%  define less_pure/2 in terms of worst_purity/3 rather than the other way
%  around for the same reason.

worst_purity(pure, pure, pure).
worst_purity(pure, (semipure), (semipure)).
worst_purity(pure, (impure), (impure)).
worst_purity((semipure), pure, (semipure)).
worst_purity((semipure), (semipure), (semipure)).
worst_purity((semipure), (impure), (impure)).
worst_purity((impure), pure, (impure)).
worst_purity((impure), (semipure), (impure)).
worst_purity((impure), (impure), (impure)).


less_pure(P1, P2) :-
	\+ worst_purity(P1, P2, P2).


add_goal_info_purity_feature(GoalInfo0, pure, GoalInfo) :-
	goal_info_remove_feature(GoalInfo0, (semipure), GoalInfo1),
	goal_info_remove_feature(GoalInfo1, (impure), GoalInfo).
add_goal_info_purity_feature(GoalInfo0, (semipure), GoalInfo) :-
	goal_info_remove_feature(GoalInfo0, (impure), GoalInfo1),
	goal_info_add_feature(GoalInfo1, (semipure), GoalInfo).
add_goal_info_purity_feature(GoalInfo0, (impure), GoalInfo) :-
	goal_info_remove_feature(GoalInfo0, (semipure), GoalInfo1),
	goal_info_add_feature(GoalInfo1, (impure), GoalInfo).


infer_goal_info_purity(GoalInfo, Purity) :-
	(
	    goal_info_has_feature(GoalInfo, (impure)) ->
		Purity = (impure)
	;
	    goal_info_has_feature(GoalInfo, (semipure)) ->
		Purity = (semipure)
	;
		Purity = pure
	).


goal_list_purity(Goals, Purity) :-
	Purity = list__foldl(
			(func(_ - GoalInfo, Purity0) = Purity1 :-
				infer_goal_info_purity(GoalInfo, GoalPurity),
		    		worst_purity(GoalPurity, Purity0, Purity1)
			), Goals, pure).
			
goal_info_is_pure(GoalInfo) :-
	\+ goal_info_has_feature(GoalInfo, (impure)),
	\+ goal_info_has_feature(GoalInfo, (semipure)).
	

goal_info_is_impure(GoalInfo) :-
	goal_info_has_feature(GoalInfo, (impure)).
	

% this works under the assumptions that all purity names but `pure' are prefix
% operators, and that we never need `pure' indicators/declarations.

write_purity_prefix(Purity) -->
	( { Purity = pure } ->
		[]
	;
		write_purity(Purity),
		io__write_string(" ")
	).

purity_prefix_to_string(Purity) = String :-
	( Purity = pure ->
		String = ""
	;
		purity_name(Purity, PurityName),
		String = string__append(PurityName, " ")
	).

write_purity(Purity) -->
	{ purity_name(Purity, String) },
	io__write_string(String).

purity_name(pure, "pure").
purity_name((semipure), "semipure").
purity_name((impure), "impure").



%-----------------------------------------------------------------------------%
%	 Purity-check the code for all the predicates in a module

:- pred check_preds_purity(bool, module_info, bool, module_info,
			io__state, io__state).
:- mode check_preds_purity(in, in, out, out, di, uo) is det.

check_preds_purity(FoundTypeError, ModuleInfo0,
		PostTypecheckError, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },

	% Only report error messages for unbound type variables
	% if we didn't get any type errors already; this avoids
	% a lot of spurious diagnostics.
	{ ReportTypeErrors = bool__not(FoundTypeError) },
	post_typecheck__finish_preds(PredIds, ReportTypeErrors, NumErrors1,
		PostTypecheckError, ModuleInfo0, ModuleInfo1),

	check_preds_purity_2(PredIds, ModuleInfo1, ModuleInfo2,
		NumErrors1, NumErrors),
	{ module_info_num_errors(ModuleInfo2, Errs0) },
	{ Errs is Errs0 + NumErrors },
	{ module_info_set_num_errors(ModuleInfo2, Errs, ModuleInfo) }.

:- pred check_preds_purity_2(list(pred_id), module_info, module_info,
			int, int, io__state, io__state).
:- mode check_preds_purity_2(in, in, out, in, out, di, uo) is det.

check_preds_purity_2([], ModuleInfo, ModuleInfo, NumErrors, NumErrors) --> [].
check_preds_purity_2([PredId | PredIds], ModuleInfo0, ModuleInfo,
		NumErrors0, NumErrors) -->
	{ module_info_pred_info(ModuleInfo0, PredId, PredInfo0) },
	(	
		{ pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0) }
	->
		{ ModuleInfo1 = ModuleInfo0 },
		{ PredInfo = PredInfo0 },
		{ NumErrors1 = NumErrors0 }
	;
		write_pred_progress_message("% Purity-checking ", PredId,
					    ModuleInfo0),
		puritycheck_pred(PredId, PredInfo0, PredInfo, ModuleInfo0,
				PurityErrsInThisPred),
		{ NumErrors1 = NumErrors0 + PurityErrsInThisPred },
		{ module_info_set_pred_info(ModuleInfo0, PredId,
				PredInfo, ModuleInfo1) }
	),

		% finish processing of promise declarations
	{ pred_info_get_goal_type(PredInfo, GoalType) },
	( { GoalType = promise(PromiseType) } ->
		post_typecheck__finish_promise(PromiseType, ModuleInfo1,
				PredId, ModuleInfo2)
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	check_preds_purity_2(PredIds, ModuleInfo2, ModuleInfo,
			  NumErrors1, NumErrors).

	% Purity-check the code for single predicate, reporting any errors.

%-----------------------------------------------------------------------------%
%			Check purity of a single predicate
%
%  Purity checking is quite simple.  Since impurity /must/ be declared, we can
%  perform a single pass checking that the actual purity of each predicate
%  matches the declared (or implied) purity.  A predicate is just as pure as
%  its least pure goal.  While we're doing this, we attach a `feature' to each
%  goal that is not pure, including non-atomic goals, indicating its purity.
%  This information must be maintained by later compilation passes, at least
%  until after the last pass that may perform transformations that would not
%  be correct for impure code.  As we check purity and attach impurity
%  features, we also check that impure (semipure) atomic goals were marked in
%  the source code as impure (semipure).  At this stage in the computation,
%  this is indicated by already having the appropriate goal feature.  (During
%  the translation from term to goal, calls have their purity attached to
%  them, and in the translation from goal to hlds_goal, the attached purity is
%  turned into the appropriate feature in the hlds_goal_info.)

:- pred puritycheck_pred(pred_id, pred_info, pred_info, module_info, int,
		io__state, io__state).
:- mode puritycheck_pred(in, in, out, in, out, di, uo) is det.

puritycheck_pred(PredId, PredInfo0, PredInfo, ModuleInfo, NumErrors) -->
	{ pred_info_get_purity(PredInfo0, DeclPurity) } ,
	{ pred_info_get_promised_purity(PredInfo0, PromisedPurity) },

	{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
	{ pred_info_procids(PredInfo0, ProcIds) },
	{ clauses_info_clauses(ClausesInfo0, Clauses0) },
	{ clauses_info_vartypes(ClausesInfo0, VarTypes0) },
	{ clauses_info_varset(ClausesInfo0, VarSet0) },
	{ RunPostTypecheck = yes },
	{ PurityInfo0 = purity_info(ModuleInfo, RunPostTypecheck,
		PredInfo0, VarTypes0, VarSet0, []) },
	{ pred_info_get_goal_type(PredInfo0, GoalType) },
	{ compute_purity(GoalType, Clauses0, Clauses, ProcIds, pure, Purity,
		PurityInfo0, PurityInfo) },
	{ PurityInfo = purity_info(_, _, PredInfo1,
		VarTypes, VarSet, RevMessages) },
	{ clauses_info_set_vartypes(ClausesInfo0, VarTypes, ClausesInfo1) },
	{ clauses_info_set_varset(ClausesInfo1, VarSet, ClausesInfo2) },
	{ Messages = list__reverse(RevMessages) },
	list__foldl(report_post_typecheck_message(ModuleInfo), Messages),
	{ NumErrors0 = list__length(
			list__filter((pred(error(_)::in) is semidet),
			Messages)) },
	{ clauses_info_set_clauses(ClausesInfo2, Clauses, ClausesInfo) },
	{ pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo) },
	{ WorstPurity = Purity },
	{ perform_pred_purity_checks(PredInfo, Purity, DeclPurity,
		PromisedPurity, PurityCheckResult) },
	( { PurityCheckResult = inconsistent_promise },
		{ NumErrors is NumErrors0 + 1 },
		error_inconsistent_promise(ModuleInfo, PredInfo, PredId,
					  DeclPurity)
	; { PurityCheckResult = unnecessary_decl },
		{ NumErrors = NumErrors0 },
		warn_exaggerated_impurity_decl(ModuleInfo, PredInfo, PredId,
					     DeclPurity, WorstPurity)
	; { PurityCheckResult = insufficient_decl },
		{ NumErrors is NumErrors0 + 1 },
		error_inferred_impure(ModuleInfo, PredInfo, PredId, Purity)
	; { PurityCheckResult = unnecessary_promise_pure },
		{ NumErrors = NumErrors0 },
		warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId,
			PromisedPurity)
	; { PurityCheckResult = no_worries },
		{ NumErrors = NumErrors0 }
	).

repuritycheck_proc(ModuleInfo, proc(_PredId, ProcId), PredInfo0, PredInfo) :-
	pred_info_procedures(PredInfo0, Procs0),
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_varset(ProcInfo0, VarSet0),
	RunPostTypeCheck = no,
	PurityInfo0 = purity_info(ModuleInfo, RunPostTypeCheck,
		PredInfo0, VarTypes0, VarSet0, []),
	InClosure = no,
	compute_goal_purity(Goal0, Goal, InClosure, Bodypurity,
		PurityInfo0, PurityInfo),
	PurityInfo = purity_info(_, _, PredInfo1, VarTypes, VarSet, _),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs),
	pred_info_set_procedures(PredInfo1, Procs, PredInfo2),

	%
	% A predicate should never become less pure after inlining,
	% so update any promises in the pred_info if the purity of
	% the goal worsened (for example if a promised pure predicate
	% was inlined).
	%
	pred_info_get_purity(PredInfo2, OldPurity),
	pred_info_get_markers(PredInfo2, Markers0),
	(
		less_pure(Bodypurity, OldPurity)
	->
		(
			OldPurity = pure,
			remove_marker(Markers0, promised_semipure, Markers1),
			add_marker(Markers1, promised_pure, Markers)
		;
			OldPurity = (semipure),
			add_marker(Markers0, promised_semipure, Markers)
		;
			OldPurity = (impure),
			Markers = Markers0
		),
		pred_info_set_markers(PredInfo2, Markers, PredInfo)
	;
		less_pure(OldPurity, Bodypurity),
		pred_info_procids(PredInfo2, [_])
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
			remove_marker(Markers0, (impure), Markers1),
			remove_marker(Markers1, (semipure), Markers)
		;
			Bodypurity = (semipure),
			remove_marker(Markers0, (impure), Markers1),
			add_marker(Markers1, (semipure), Markers)
		;
			Bodypurity = (impure),
			Markers = Markers0
		),
		pred_info_set_markers(PredInfo2, Markers, PredInfo)
	;
		PredInfo = PredInfo2
	).

% Infer the purity of a single (non-pragma c_code) predicate

:- pred compute_purity(goal_type, list(clause), list(clause), list(proc_id),
	purity, purity, purity_info, purity_info).
:- mode compute_purity(in, in, out, in, in, out, in, out) is det.

compute_purity(_, [], [], _, Purity, Purity) --> [].
compute_purity(GoalType, [Clause0|Clauses0], [Clause|Clauses], ProcIds,
		Purity0, Purity) -->
	{ Clause0 = clause(Ids, Body0 - Info0, Lang, Context) },
	compute_expr_purity(Body0, Body, Info0, no, Bodypurity0),
	% If this clause doesn't apply to all modes of this procedure,
	% i.e. the procedure has different clauses for different modes,
	% then we must treat it as impure.
	% the default impurity of foreign_proc procedures is handled when
	% processing the foreign_proc goal -- they are not counted as impure
	% here simply because they have different clauses for different modes
	{
		( applies_to_all_modes(Clause0, ProcIds)
		; GoalType = pragmas
		)
	->
		Clausepurity = (pure)
	;
		Clausepurity = (impure)
	},
	{ worst_purity(Bodypurity0, Clausepurity, Bodypurity) },
	{ add_goal_info_purity_feature(Info0, Bodypurity, Info) },
	{ worst_purity(Purity0, Bodypurity, Purity1) },
	{ Clause = clause(Ids, Body - Info, Lang, Context) },
	compute_purity(GoalType, Clauses0, Clauses, ProcIds, Purity1, Purity).

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

:- pred compute_expr_purity(hlds_goal_expr, hlds_goal_expr,
	hlds_goal_info, bool, purity, purity_info, purity_info).
:- mode compute_expr_purity(in, out, in, in, out, in, out) is det.

compute_expr_purity(conj(Goals0), conj(Goals), _, InClosure, Purity) -->
	compute_goals_purity(Goals0, Goals, InClosure, pure, Purity).
compute_expr_purity(par_conj(Goals0), par_conj(Goals), _,
		InClosure, Purity) -->
	compute_goals_purity(Goals0, Goals, InClosure, pure, Purity).
compute_expr_purity(call(PredId0,ProcId,Vars,BIState,UContext,Name0),
		call(PredId,ProcId,Vars,BIState,UContext,Name),
		GoalInfo, InClosure, ActualPurity) -->
	RunPostTypecheck =^ run_post_typecheck,
	PredInfo =^ pred_info,
	ModuleInfo =^ module_info,
	{
		RunPostTypecheck = yes,
		post_typecheck__resolve_pred_overloading(PredId0,
			Vars, PredInfo, ModuleInfo, Name0, Name, PredId)
	;	
		RunPostTypecheck = no,
		PredId = PredId0,
		Name = Name0
	},
	{ infer_goal_info_purity(GoalInfo, DeclaredPurity) },
	{ goal_info_get_context(GoalInfo, CallContext) },

	perform_goal_purity_checks(CallContext, PredId,
		DeclaredPurity, InClosure, ActualPurity).

compute_expr_purity(generic_call(GenericCall0, Args, Modes0, Det),
		GoalExpr, GoalInfo, _InClosure, Purity) -->
	(
		{ GenericCall0 = higher_order(_, _, _) },
		{ Purity = pure },
		{ GoalExpr = generic_call(GenericCall0, Args, Modes0, Det) }
	;
		{ GenericCall0 = class_method(_, _, _, _) },
		{ Purity = pure },
		{ GoalExpr = generic_call(GenericCall0, Args, Modes0, Det) }
	;
		{ GenericCall0 = aditi_builtin(Builtin0, CallId0) },
		{ Purity = pure },
		{ goal_info_get_context(GoalInfo, Context) },
		RunPostTypecheck =^ run_post_typecheck,
		(
			{ RunPostTypecheck = yes },
			ModuleInfo =^ module_info,
			PredInfo =^ pred_info,
			{ post_typecheck__finish_aditi_builtin(ModuleInfo,
				PredInfo, Args, Context, Builtin0, Builtin,
				CallId0, CallId, Modes, MaybeMessage) },
			(
				{ MaybeMessage = yes(Message) },
				purity_info_add_message(
					error(aditi_builtin_error(Message)))
			;
				{ MaybeMessage = no }
			),
			{ GenericCall = aditi_builtin(Builtin, CallId) }
		;
			{ RunPostTypecheck = no },
			{ GenericCall = GenericCall0 },
			{ Modes = Modes0 }
		),

		{ GoalExpr = generic_call(GenericCall, Args, Modes, Det) }
	).
compute_expr_purity(switch(Var, Canfail, Cases0),
		switch(Var, Canfail, Cases), _, InClosure, Purity) -->
	compute_cases_purity(Cases0, Cases, InClosure, pure, Purity).
compute_expr_purity(Unif0, GoalExpr, GoalInfo, InClosure,
		ActualPurity) -->
	{ Unif0 = unify(Var, RHS0, Mode, Unification, UnifyContext) },
	(
		{ RHS0 = lambda_goal(F, EvalMethod, FixModes, H, Vars,
			Modes0, K, Goal0 - Info0) }
	->
		{ RHS = lambda_goal(F, EvalMethod, modes_are_ok, H, Vars,
			Modes, K, Goal - Info0) },
		compute_expr_purity(Goal0, Goal, Info0, yes, Purity),
		error_if_closure_impure(GoalInfo, Purity),

		VarTypes =^ vartypes,

		{
			FixModes = modes_are_ok,
			Modes = Modes0
		;
			FixModes = modes_need_fixing,
			(
				EvalMethod = normal,
				error(
	"compute_expr_purity: modes need fixing for normal lambda_goal")
			;
				EvalMethod = (aditi_top_down),
				% `aditi_top_down' predicates can't call
				% database predicates, so their `aditi__state'
				% arguments must have mode `unused'.
				% The `aditi__state's are passed even
				% though they are not used so that the
				% arguments of the closure and the
				% base relation being updated match.
				unused_mode(StateMode)
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
		},
		{ GoalExpr = unify(Var, RHS, Mode, Unification, UnifyContext) },
		{ ActualPurity = pure }
	;
		{ RHS0 = functor(ConsId, _, Args) } 
	->
		RunPostTypecheck =^ run_post_typecheck,
		(
			{ RunPostTypecheck = yes },
			ModuleInfo =^ module_info,
			PredInfo0 =^ pred_info,
			VarTypes0 =^ vartypes,
			VarSet0 =^ varset,
			{ post_typecheck__resolve_unify_functor(Var, ConsId,
				Args, Mode, Unification, UnifyContext,
				GoalInfo, ModuleInfo, PredInfo0, PredInfo,
				VarTypes0, VarTypes, VarSet0, VarSet, Goal1) },
			^ vartypes := VarTypes,
			^ varset := VarSet,
			^ pred_info := PredInfo
		;
			{ RunPostTypecheck = no },
			{ Goal1 = Unif0 - GoalInfo }
		),
		( 
			{ Goal1 \= unify(_, _, _, _, _) - _ }
		->
			compute_goal_purity(Goal1, Goal,
				InClosure, ActualPurity)
		;
			check_higher_order_purity(GoalInfo, ConsId,
				Var, Args, ActualPurity),
			{ Goal = Goal1 }
		),
		{ Goal = GoalExpr - _ }
	;
		{ GoalExpr = Unif0 },
		{ ActualPurity = pure }
	).
compute_expr_purity(disj(Goals0), disj(Goals), _, InClosure, Purity) -->
	compute_goals_purity(Goals0, Goals, InClosure, pure, Purity).
compute_expr_purity(not(Goal0), NotGoal, GoalInfo0, InClosure, Purity) -->
	%
	% eliminate double negation
	%
	{ negate_goal(Goal0, GoalInfo0, NotGoal0) },
	( { NotGoal0 = not(Goal1) - _GoalInfo1 } ->
		compute_goal_purity(Goal1, Goal, InClosure, Purity),
		{ NotGoal = not(Goal) }
	;
		compute_goal_purity(NotGoal0, NotGoal1, InClosure, Purity),
		{ NotGoal1 = NotGoal - _ }
	).
compute_expr_purity(some(Vars, CanRemove, Goal0), some(Vars, CanRemove, Goal),
		_, InClosure, Purity) -->
	compute_goal_purity(Goal0, Goal, InClosure, Purity).
compute_expr_purity(if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else), _,
		InClosure, Purity) -->
	compute_goal_purity(Cond0, Cond, InClosure, Purity1),
	compute_goal_purity(Then0, Then, InClosure, Purity2),
	compute_goal_purity(Else0, Else, InClosure, Purity3),
	{ worst_purity(Purity1, Purity2, Purity12) },
	{ worst_purity(Purity12, Purity3, Purity) }.
compute_expr_purity(ForeignProc0, ForeignProc, _, _, Purity) -->
	{ ForeignProc0 = foreign_proc(_, _, _, _, _, _, _) },
	{ Attributes = ForeignProc0 ^ foreign_attr },
	{ PredId = ForeignProc0 ^ foreign_pred_id },
	ModuleInfo =^ module_info,
	{ 
		legacy_purity_behaviour(Attributes, yes)
	->
			% get the purity from the declaration, and set it 
			% here so that it is correct for later use
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_purity(PredInfo, Purity),
		set_purity(Attributes, Purity, NewAttributes),
		ForeignProc = ForeignProc0 ^ foreign_attr := NewAttributes
	;
		ForeignProc = ForeignProc0,
		purity(Attributes, AttributesPurity),
		Purity = AttributesPurity
	}.

compute_expr_purity(shorthand(_), _, _, _, _) -->
	% these should have been expanded out by now
	{ error("compute_expr_purity: unexpected shorthand") }.


:- pred check_higher_order_purity(hlds_goal_info, cons_id, prog_var,
	list(prog_var), purity, purity_info, purity_info).
:- mode check_higher_order_purity(in, in, in, in, out, in, out) is det.
check_higher_order_purity(GoalInfo, ConsId, Var, Args, ActualPurity) -->
	VarTypes =^ vartypes,
	{ map__lookup(VarTypes, Var, TypeOfVar) },
	( 
		{ ConsId = cons(PName, _) },
		{ type_is_higher_order(TypeOfVar, PredOrFunc,
			_EvalMethod, VarArgTypes) }
	->
		PredInfo =^ pred_info,
		{ pred_info_typevarset(PredInfo, TVarSet) },
		{ map__apply_to_list(Args, VarTypes, ArgTypes0) },
		{ list__append(ArgTypes0, VarArgTypes, PredArgTypes) },
		ModuleInfo =^ module_info,
		( 
			{ get_pred_id(PName, PredOrFunc, TVarSet, PredArgTypes,
				ModuleInfo, CalleePredId) }
		->
			{ module_info_pred_info(ModuleInfo,
				CalleePredId, CalleePredInfo) },
			{ pred_info_get_purity(CalleePredInfo, Purity) },
			( { Purity = pure } ->
				[]
			;
				{ goal_info_get_context(GoalInfo,
					CallContext) },
				{ Message = missing_body_impurity_error(
						CallContext, CalleePredId) },
				purity_info_add_message(error(Message))
			)
		;
			% If we can't find the type of the function, 
			% it's because typecheck couldn't give it one.
			% Typechecking gives an error in this case, we
			% just keep silent.
			{ Purity = pure }
		),
		{ ActualPurity = Purity }
	;
		{ infer_goal_info_purity(GoalInfo, DeclaredPurity) },
		( { DeclaredPurity \= pure } ->
			{ goal_info_get_context(GoalInfo, Context) },
			{ Message = impure_unification_expr_error(Context,
					DeclaredPurity) },
			purity_info_add_message(error(Message))
		;
			[]
		),
		{ ActualPurity = pure }
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
		( 
			PromisedPurity = (impure)
		->
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
	% InClosure: Is this a goal inside a closure?
:- pred perform_goal_purity_checks(prog_context::in, pred_id::in, purity::in,
	bool::in, purity::out, purity_info::in, purity_info::out) is det.

perform_goal_purity_checks(Context, PredId, DeclaredPurity,
		_InClosure, ActualPurity) -->
	ModuleInfo =^ module_info,
	PredInfo =^ pred_info,
	{ module_info_pred_info(ModuleInfo, PredId, CalleePredInfo) },
	{ pred_info_get_purity(CalleePredInfo, ActualPurity) },
	( 
		% The purity should match the declaration
		{ ActualPurity = DeclaredPurity }
	->
		[]
	; 
		% Don't require purity annotations on calls in
		% compiler-generated code.
		{ code_util__compiler_generated(PredInfo) }
	->
		[]
	; 
		{ less_pure(ActualPurity, DeclaredPurity) }
	->
		purity_info_add_message(
			error(missing_body_impurity_error(Context, PredId)))
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
		{ pred_info_get_markers(PredInfo, Markers) },
		{ 
			check_marker(Markers, class_method) 
		;
			check_marker(Markers, class_instance_method) 
		}
	->
		[]
	;
		purity_info_add_message(
			warning(unnecessary_body_impurity_decl(Context,
				PredId, DeclaredPurity)))
	).

:- pred compute_goal_purity(hlds_goal, hlds_goal,
		bool, purity, purity_info, purity_info).
:- mode compute_goal_purity(in, out, in, out, in, out) is det.

compute_goal_purity(Goal0 - GoalInfo0, Goal - GoalInfo, InClosure, Purity) -->
	compute_expr_purity(Goal0, Goal, GoalInfo0, InClosure, Purity),
	{ add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo) }.


%  Compute the purity of a list of hlds_goals.  Since the purity of a
%  disjunction is computed the same way as the purity of a conjunction, we use
%  the same code for both

:- pred compute_goals_purity(list(hlds_goal), list(hlds_goal),
	bool, purity, purity, purity_info, purity_info).
:- mode compute_goals_purity(in, out, in, in, out, in, out) is det.

compute_goals_purity([], [], _, Purity, Purity) --> [].
compute_goals_purity([Goal0|Goals0], [Goal|Goals], InClosure,
		Purity0, Purity) -->
	compute_goal_purity(Goal0, Goal, InClosure, Purity1),
	{ worst_purity(Purity0, Purity1, Purity2) },
	compute_goals_purity(Goals0, Goals, InClosure, Purity2, Purity).



:- pred compute_cases_purity(list(case), list(case),
	bool, purity, purity, purity_info, purity_info).
:- mode compute_cases_purity(in, out, in, in, out, in, out) is det.

compute_cases_purity([], [], _,
		Purity, Purity) --> [].
compute_cases_purity([case(Ctor,Goal0)|Goals0], [case(Ctor,Goal)|Goals],
		InClosure, Purity0, Purity) -->
	compute_goal_purity(Goal0, Goal, InClosure, Purity1),
	{ worst_purity(Purity0, Purity1, Purity2) },
	compute_cases_purity(Goals0, Goals, InClosure, Purity2, Purity).

	% Make sure lambda expressions introduced by the compiler
	% have the correct mode for their `aditi__state' arguments.
:- pred fix_aditi_state_modes(bool, (mode), list(type),
		list(mode), list(mode)).
:- mode fix_aditi_state_modes(in, in, in, in, out) is det.

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

:- pred error_inconsistent_promise(module_info, pred_info, pred_id, purity,
				  io__state, io__state).
:- mode error_inconsistent_promise(in, in, in, in, di, uo) is det.

error_inconsistent_promise(ModuleInfo, PredInfo, PredId, Purity) -->
	{ pred_info_context(PredInfo, Context) },
	write_context_and_pred_id(ModuleInfo, PredInfo, PredId),
	prog_out__write_context(Context),
	report_warning("  warning: declared `"),
	write_purity(Purity),
	io__write_string("' but promised pure.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		prog_out__write_context(Context),
		io__write_string("  A pure "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" that invokes impure or semipure code should\n"),
		prog_out__write_context(Context),
		io__write_string(
		    "  be promised pure and should have no impurity declaration.\n"
		)
	;
		[]
	).


:- pred warn_exaggerated_impurity_decl(module_info, pred_info, pred_id,
				       purity, purity, io__state, io__state).
:- mode warn_exaggerated_impurity_decl(in, in, in, in, in, di, uo) is det.

warn_exaggerated_impurity_decl(ModuleInfo, PredInfo, PredId,
		DeclPurity, AcutalPurity) -->
	{ pred_info_context(PredInfo, Context) },
	write_context_and_pred_id(ModuleInfo, PredInfo, PredId),
	prog_out__write_context(Context),
	report_warning("  warning: declared `"),
	write_purity(DeclPurity),
	io__write_string("' but actually "),
	write_purity(AcutalPurity),
	io__write_string(".\n").

:- pred warn_unnecessary_promise_pure(module_info, pred_info, pred_id, purity,
				  io__state, io__state).
:- mode warn_unnecessary_promise_pure(in, in, in, in, di, uo) is det.

warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId, PromisedPurity) -->
	{ pred_info_context(PredInfo, Context) },
	write_context_and_pred_id(ModuleInfo, PredInfo, PredId),
	prog_out__write_context(Context),
	{
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
	},

	report_warning("  warning: unnecessary `"),
	io__write_string(Pragma),
	io__write_string("' pragma.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		io__write_string("  This "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" does not invoke any "),
		io__write_string(CodeStr),  
		io__write_string(" code,\n"),
		prog_out__write_context(Context),
		io__write_string("  so there is no need for a `"),
		io__write_string(Pragma),
		io__write_string("' pragma.\n")
	;
		[]
	).


:- pred error_inferred_impure(module_info, pred_info, pred_id, purity,
	io__state, io__state).
:- mode error_inferred_impure(in, in, in, in, di, uo) is det.

error_inferred_impure(ModuleInfo, PredInfo, PredId, Purity) -->
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	write_context_and_pred_id(ModuleInfo, PredInfo, PredId),
	prog_out__write_context(Context),
	io__write_string("  purity error: "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" is "),
	write_purity(Purity),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	{ pred_info_get_purity(PredInfo, DeclaredPurity) },
	( { code_util__compiler_generated(PredInfo) } ->
		io__write_string("  It must be pure.\n")
	;
		io__write_string("  It must be declared `"),
		write_purity(Purity),
		io__write_string("' or promised "),
		write_purity(DeclaredPurity),
		io__write_string(".\n")
	).

	% Errors and warnings reported by purity.m and post_typecheck.m
	% for problems within a goal.
:- type post_typecheck_message
	--->	error(post_typecheck_error)
	;	warning(post_typecheck_warning)
	.

:- type post_typecheck_messages == list(post_typecheck_message).
	
:- type post_typecheck_error
	--->	missing_body_impurity_error(prog_context, pred_id)
	;	impure_closure(prog_context, purity)
	;	impure_unification_expr_error(prog_context, purity)
	;	aditi_builtin_error(aditi_builtin_error)
	.

:- type post_typecheck_warning
	--->	unnecessary_body_impurity_decl(prog_context, pred_id, purity).

:- pred report_post_typecheck_message(module_info, post_typecheck_message, 
		io__state, io__state).
:- mode report_post_typecheck_message(in, in, di, uo) is det.

report_post_typecheck_message(ModuleInfo, error(Message)) -->
	io__set_exit_status(1),
	(
		{ Message = missing_body_impurity_error(Context, PredId) },
		error_missing_body_impurity_decl(ModuleInfo, PredId, Context)
	;
		{ Message = impure_closure(Context, Purity) },
		report_error_impure_closure(Context, Purity)
	;
		{ Message = impure_unification_expr_error(Context, Purity) },
		impure_unification_expr_error(Context, Purity)
	;
		{ Message = aditi_builtin_error(AditiError) },
		report_aditi_builtin_error(AditiError)
	).

report_post_typecheck_message(ModuleInfo,
		warning(unnecessary_body_impurity_decl(Context,
			PredId, DeclaredPurity))) -->
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	(
		{ HaltAtWarn = yes },
		io__set_exit_status(1)
	;
		{ HaltAtWarn = no }
	),
	warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
		DeclaredPurity). 

:- pred error_missing_body_impurity_decl(module_info, pred_id,
				  prog_context, io__state, io__state).
:- mode error_missing_body_impurity_decl(in, in, in, di, uo) is det.

error_missing_body_impurity_decl(ModuleInfo, PredId, Context) -->
	prog_out__write_context(Context),
	io__write_string("In call to "),
	write_purity(Purity),
	io__write_string(" "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_get_purity(PredInfo, Purity) },
	( { PredOrFunc = predicate } ->
		io__write_string("  purity error: call must be preceded by `"),
		write_purity(Purity),
		io__write_string("' indicator.\n")
	;
		io__write_string("  purity error: call must be in an explicit unification\n"),
		prog_out__write_context(Context),
		io__write_string("  which is preceded by `"),
		write_purity(Purity),
		io__write_string("' indicator.\n")

	).

:- pred warn_unnecessary_body_impurity_decl(module_info, pred_id,
	prog_context, purity, io__state, io__state).
:- mode warn_unnecessary_body_impurity_decl(in, in, in, in, di, uo)
	is det.

warn_unnecessary_body_impurity_decl(ModuleInfo, PredId, Context,
		DeclaredPurity) -->
	prog_out__write_context(Context),
	io__write_string("In call to "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  warning: unnecessary `"),
	write_purity(DeclaredPurity),
	io__write_string("' indicator.\n"),
	prog_out__write_context(Context),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_purity(PredInfo, ActualPurity) },
	( { ActualPurity = pure } ->
		io__write_string("  No purity indicator is necessary.\n")
	;
		io__write_string("  A purity indicator of `"),
		write_purity(ActualPurity),
		io__write_string("' is sufficient.\n")
	).
	
:- pred error_if_closure_impure(hlds_goal_info, purity,
		purity_info, purity_info).	
:- mode error_if_closure_impure(in, in, in, out) is det.

error_if_closure_impure(GoalInfo, Purity) -->
	( { Purity = pure } ->
		[]
	;
		{ goal_info_get_context(GoalInfo, Context) },
		purity_info_add_message(
			error(impure_closure(Context, Purity)))
	).

:- pred report_error_impure_closure(prog_context, purity,
		io__state, io__state).
:- mode report_error_impure_closure(in, in, di, uo) is det.

report_error_impure_closure(Context, Purity) -->
	prog_out__write_context(Context),
	io__write_string("Purity error in closure: closure is "),
	write_purity(Purity),
	io__write_string(".\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  All closures must be pure.\n")
	;   
		[]
	).

:- pred write_context_and_pred_id(module_info, pred_info, pred_id,
				  io__state, io__state).
:- mode write_context_and_pred_id(in, in, in, di, uo) is det.

write_context_and_pred_id(ModuleInfo, PredInfo, PredId) -->
	{ pred_info_context(PredInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("In "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n").

impure_unification_expr_error(Context, Purity) -->
	prog_out__write_context(Context),
	io__write_string("Purity error: unification with expression was declared\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	write_purity(Purity),
	io__write_string(", but expression was not a function call.\n").

%-----------------------------------------------------------------------------%

:- type purity_info
	--->	purity_info(
			% fields not changed by purity checking.
			module_info :: module_info,
			run_post_typecheck :: bool,

			% fields which may be changed.
			pred_info :: pred_info,
			vartypes :: vartypes,
			varset :: prog_varset,
			messages :: post_typecheck_messages
	).

:- pred purity_info_add_message(post_typecheck_message,
		purity_info, purity_info).
:- mode purity_info_add_message(in, in, out) is det.

purity_info_add_message(Message, Info,
		Info ^ messages := [Message | Info ^ messages]).

%-----------------------------------------------------------------------------%
