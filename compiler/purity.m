%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File      : purity.m
% Authors   : pets (Peter Schachte)
% Purpose   : handle `impure' and `promise_pure' declarations;
%	      finish off type checking.
%
%  The main purpose of this module is check the consistency of the
%  `impure' and `promise_pure' (etc.) declarations, and to thus report
%  error messages if the program is not "purity-correct".
%
%  This module also does two final parts of type analysis:
%	- it resolves predicate overloading
%	  (perhaps it ought to also resolve function overloading,
%	  converting unifications that are function calls into
%	  HLDS call instructions, but currently that is done
%	  in polymorphism.m)
%	- it checks for unbound type variables and if there are any,
%	  it reports an error (or a warning, binding them to the type `void').
%  These actions cannot be done until after type inference is complete,
%  so they need to be a separate "post-typecheck pass"; they are done
%  here in combination with the purity-analysis pass for efficiency reasons.
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
%  XXX The current implementation doesn't handle impure functions.  The main
%      reason is that handling of nested functions is likely to get pretty 
%      confusing.  Because impure functions can't be reordered, the execution
%      order would have to be strictly innermost-first, left-to-right, and 
%      predicate arguments would always have to be evaluated before the
%      predicate call.  Implied modes are right out.  All in all, they just
%      won't be as natural as one might think at first.


:- module purity.
:- interface.

:- import_module prog_data, hlds_module, hlds_goal.
:- import_module io, bool.

% The purity type itself is defined in prog_data.m as follows:
% :- type purity	--->	pure
% 			;	(semipure)
% 			;	(impure).

%  Purity check a whole module.
%  The first argument specifies whether there were any type
%  errors (if so, we suppress some diagnostics in post_typecheck.m
%  because they are usually spurious).
:- pred puritycheck(bool, module_info, module_info, io__state, io__state).
:- mode puritycheck(in, in, out, di, uo) is det.

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

:- implementation.

:- import_module make_hlds, hlds_pred, prog_io_util.
:- import_module type_util, mode_util, code_util, prog_data, unify_proc.
:- import_module globals, options, mercury_to_mercury, hlds_out.
:- import_module passes_aux, typecheck, module_qual, clause_to_proc.
:- import_module inst_util, prog_out.
:- import_module post_typecheck.

:- import_module list, map, varset, term, string, require, std_util.
:- import_module assoc_list, bool, int, set.

%-----------------------------------------------------------------------------%
%				Public Predicates


puritycheck(FoundTypeError, HLDS0, HLDS) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),

	maybe_write_string(Verbose, "% Purity-checking clauses...\n"),
	check_preds_purity(FoundTypeError, HLDS0, HLDS),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).


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
	goal_info_add_feature(GoalInfo0, (semipure), GoalInfo).
add_goal_info_purity_feature(GoalInfo0, (impure), GoalInfo) :-
	goal_info_add_feature(GoalInfo0, (impure), GoalInfo).


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

write_purity(Purity) -->
	{ purity_name(Purity, String) },
	io__write_string(String).

purity_name(pure, "pure").
purity_name((semipure), "semipure").
purity_name((impure), "impure").



%-----------------------------------------------------------------------------%
%	 Purity-check the code for all the predicates in a module

:- pred check_preds_purity(bool, module_info, module_info,
			io__state, io__state).
:- mode check_preds_purity(in, in, out, di, uo) is det.

check_preds_purity(FoundTypeError, ModuleInfo0, ModuleInfo) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	check_preds_purity_2(PredIds, FoundTypeError, ModuleInfo0,
		ModuleInfo1, 0, NumErrors),
	{ module_info_num_errors(ModuleInfo1, Errs0) },
	{ Errs is Errs0 + NumErrors },
	{ module_info_set_num_errors(ModuleInfo1, Errs, ModuleInfo) }.


:- pred check_preds_purity_2(list(pred_id), bool, module_info, module_info,
			int, int, io__state, io__state).
:- mode check_preds_purity_2(in, in, in, out, in, out, di, uo) is det.

check_preds_purity_2([], _, ModuleInfo, ModuleInfo,
		NumErrors, NumErrors) --> [].
check_preds_purity_2([PredId | PredIds], FoundTypeError, ModuleInfo0,
		ModuleInfo, NumErrors0, NumErrors) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	(	
		{ pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0) }
	->
		post_typecheck__finish_imported_pred(ModuleInfo0, PredId,
				PredInfo0, PredInfo),
		{ NumErrors1 = NumErrors0 }
	;
		write_pred_progress_message("% Purity-checking ", PredId,
					    ModuleInfo0),
		%
		% Only report error messages for unbound type variables
		% if we didn't get any type errors already; this avoids
		% a lot of spurious diagnostics.
		%
		{ bool__not(FoundTypeError, ReportErrs) },
		post_typecheck__check_type_bindings(PredId, PredInfo0,
				ModuleInfo0, ReportErrs,
				PredInfo1, UnboundTypeErrsInThisPred),
		puritycheck_pred(PredId, PredInfo1, PredInfo2, ModuleInfo0,
				PurityErrsInThisPred),
		post_typecheck__finish_pred(ModuleInfo0, PredId, PredInfo2,
				PredInfo),
		{ NumErrors1 is NumErrors0 + UnboundTypeErrsInThisPred
					   + PurityErrsInThisPred }
	),
	{ map__det_update(Preds0, PredId, PredInfo, Preds) },
	{ module_info_get_predicate_table(ModuleInfo0, PredTable0) },
	{ predicate_table_set_preds(PredTable0, Preds, PredTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredTable,
					  ModuleInfo1) },

	(
		{ pred_info_get_goal_type(PredInfo0, assertion) }
	->
		{ post_typecheck__finish_assertion(ModuleInfo1,
				PredId, ModuleInfo2) }
	;
		{ ModuleInfo2 = ModuleInfo1 }
	),
	check_preds_purity_2(PredIds, FoundTypeError, ModuleInfo2, ModuleInfo,
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
	{ pred_info_get_promised_pure(PredInfo0, Promised) },
	( { pred_info_get_goal_type(PredInfo0, pragmas) } ->
		{ WorstPurity = (impure) },
		{ Purity = pure },
		{ PredInfo = PredInfo0 },
		{ NumErrors0 = 0 }
	;   
		{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
		{ clauses_info_clauses(ClausesInfo0, Clauses0) },
		compute_purity(Clauses0, Clauses, PredInfo0, ModuleInfo,
				pure, Purity, 0, NumErrors0),
		{ clauses_info_set_clauses(ClausesInfo0, Clauses,
				ClausesInfo) },
		{ pred_info_set_clauses_info(PredInfo0, ClausesInfo,
				PredInfo) },
		{ WorstPurity = Purity }
	),
	( { DeclPurity \= pure, Promised = yes } ->
		{ NumErrors is NumErrors0 + 1 },
		error_inconsistent_promise(ModuleInfo, PredInfo, PredId,
					  DeclPurity)
	; { less_pure(DeclPurity, WorstPurity) } ->
		{ NumErrors = NumErrors0 },
		warn_exaggerated_impurity_decl(ModuleInfo, PredInfo, PredId,
					     DeclPurity, WorstPurity)
	; { less_pure(Purity, DeclPurity), Promised = no } ->
		{ NumErrors is NumErrors0 + 1 },
		error_inferred_impure(ModuleInfo, PredInfo, PredId, Purity)
	; { Purity = pure, Promised = yes } ->
		{ NumErrors = NumErrors0 },
		warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId)
	;
		{ NumErrors = NumErrors0 }
	).


% Infer the purity of a single (non-pragma c_code) predicate

:- pred compute_purity(list(clause), list(clause), pred_info, module_info,
	purity, purity, int, int, io__state, io__state).
:- mode compute_purity(in, out, in, in, in, out, in, out, di, uo) is det.

compute_purity([], [], _, _, Purity, Purity, NumErrors, NumErrors) -->
	[].
compute_purity([Clause0|Clauses0], [Clause|Clauses], PredInfo, ModuleInfo,
		Purity0, Purity, NumErrors0, NumErrors) -->
	{ Clause0 = clause(Ids, Body0 - Info0, Context) },
	compute_expr_purity(Body0, Body, Info0, PredInfo, ModuleInfo,
			    no, Bodypurity, NumErrors0, NumErrors1),
	{ add_goal_info_purity_feature(Info0, Bodypurity, Info) },
	{ worst_purity(Purity0, Bodypurity, Purity1) },
	{ Clause = clause(Ids, Body - Info, Context) },
	compute_purity(Clauses0, Clauses, PredInfo, ModuleInfo,
		       Purity1, Purity, NumErrors1, NumErrors).

:- pred compute_expr_purity(hlds_goal_expr, hlds_goal_expr, hlds_goal_info,
	pred_info, module_info, bool, purity, int, int, io__state, io__state).
:- mode compute_expr_purity(in, out, in, in, in, in, out, in, out, di, uo)
	is det.

compute_expr_purity(conj(Goals0), conj(Goals), _, PredInfo, ModuleInfo,
		InClosure, Purity, NumErrors0, NumErrors) -->
	compute_goals_purity(Goals0, Goals, PredInfo, ModuleInfo,
			     InClosure, pure, Purity, NumErrors0, NumErrors).
compute_expr_purity(par_conj(Goals0, SM), par_conj(Goals, SM), _, PredInfo,
		ModuleInfo, InClosure, Purity, NumErrors0, NumErrors) -->
	compute_goals_purity(Goals0, Goals, PredInfo, ModuleInfo,
			     InClosure, pure, Purity, NumErrors0, NumErrors).
compute_expr_purity(call(PredId0,ProcId,Vars,BIState,UContext,Name0),
		call(PredId,ProcId,Vars,BIState,UContext,Name), GoalInfo,
		PredInfo, ModuleInfo, InClosure, ActualPurity,
		NumErrors0, NumErrors) -->
	{ post_typecheck__resolve_pred_overloading(PredId0, Vars, PredInfo,
		ModuleInfo, Name0, Name, PredId) },
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, CalleePredInfo) },
	{ pred_info_get_purity(CalleePredInfo, ActualPurity) },
	{ infer_goal_info_purity(GoalInfo, DeclaredPurity) },
	{ goal_info_get_context(GoalInfo, CallContext) },
	( { code_util__compiler_generated(PredInfo) } ->
		% Don't require purity annotations on calls in
		% compiler-generated code
		{ NumErrors = NumErrors0 }
	; { ActualPurity = DeclaredPurity } ->
		{ NumErrors = NumErrors0 }
	; { InClosure = yes } ->
		% Don't report purity errors inside closures:  the whole
		% closure is an error if it's not pure
		{ NumErrors = NumErrors0 }
	; { less_pure(ActualPurity, DeclaredPurity) } ->
		error_missing_body_impurity_decl(ModuleInfo, CalleePredInfo,
						 PredId, CallContext,
						 ActualPurity),
		{ NumErrors is NumErrors0 + 1 }
	;
		warn_unnecessary_body_impurity_decl(ModuleInfo, CalleePredInfo,
						    PredId, CallContext,
						    ActualPurity,
						    DeclaredPurity),
		{ NumErrors = NumErrors0 }
	).
compute_expr_purity(generic_call(GenericCall0, Args, Modes0, Det),
		generic_call(GenericCall, Args, Modes, Det),
		GoalInfo, PredInfo, ModuleInfo, _InClosure, Purity,
		NumErrors, NumErrors) -->
	{ Purity = pure },
	(
		{ GenericCall0 = higher_order(_, _, _) },
		{ GenericCall = GenericCall0 },
		{ Modes = Modes0 }
	;
		{ GenericCall0 = class_method(_, _, _, _) },
		{ GenericCall = GenericCall0 },
		{ Modes = Modes0 }
	;
		{ GenericCall0 = aditi_builtin(Builtin0, CallId0) },
		{ goal_info_get_context(GoalInfo, Context) },
		post_typecheck__finish_aditi_builtin(ModuleInfo, PredInfo,
			Args, Context, Builtin0, Builtin,
			CallId0, CallId, Modes),
		{ GenericCall = aditi_builtin(Builtin, CallId) }
	).
compute_expr_purity(switch(Var,Canfail,Cases0,Storemap),
		switch(Var,Canfail,Cases,Storemap), _, PredInfo,
		ModuleInfo, InClosure, Purity, NumErrors0, NumErrors) -->
	compute_cases_purity(Cases0, Cases, PredInfo, ModuleInfo,
			     InClosure, pure, Purity, NumErrors0, NumErrors).
compute_expr_purity(Unif0, Unif, GoalInfo, PredInfo, ModuleInfo, _,
		pure, NumErrors0, NumErrors) -->
	{ Unif0 = unify(A,RHS0,C,D,E) },
	{ Unif  = unify(A,RHS,C,D,E) },
	(
		{ RHS0 = lambda_goal(F, EvalMethod, FixModes, H, Vars,
			Modes0, K, Goal0 - Info0) }
	->
		{ RHS = lambda_goal(F, EvalMethod, modes_are_ok, H, Vars,
			Modes, K, Goal - Info0) },
		compute_expr_purity(Goal0, Goal, Info0, PredInfo, ModuleInfo,
				    yes, Purity, NumErrors0, NumErrors1),
		error_if_closure_impure(GoalInfo, Purity,
					NumErrors1, NumErrors),
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
				StateMode = aditi_ui_mode
			),
			pred_info_clauses_info(PredInfo, ClausesInfo),
			clauses_info_vartypes(ClausesInfo, VarTypes),
			map__apply_to_list(Vars, VarTypes, LambdaVarTypes),
			fix_aditi_state_modes(StateMode, LambdaVarTypes,
				Modes0, Modes)
		}
	;
		{ RHS = RHS0 },
		{ NumErrors = NumErrors0 }
	).
compute_expr_purity(disj(Goals0,Store), disj(Goals,Store), _, PredInfo,
		ModuleInfo, InClosure, Purity, NumErrors0, NumErrors) -->
	compute_goals_purity(Goals0, Goals, PredInfo, ModuleInfo,
			     InClosure, pure, Purity, NumErrors0, NumErrors).
compute_expr_purity(not(Goal0), not(Goal), _, PredInfo, ModuleInfo,
		InClosure, Purity, NumErrors0, NumErrors) -->
	compute_goal_purity(Goal0, Goal, PredInfo, ModuleInfo, 
			    InClosure, Purity, NumErrors0, NumErrors).
compute_expr_purity(some(Vars, CanRemove, Goal0), some(Vars, CanRemove, Goal),
		_, PredInfo, ModuleInfo, InClosure, Purity,
		NumErrors0, NumErrors) -->
	compute_goal_purity(Goal0, Goal, PredInfo, ModuleInfo, 
			    InClosure, Purity, NumErrors0, NumErrors).
compute_expr_purity(if_then_else(Vars,Goali0,Goalt0,Goale0,Store),
		if_then_else(Vars,Goali,Goalt,Goale,Store), _, PredInfo,
		ModuleInfo, InClosure, Purity, NumErrors0, NumErrors) -->
	compute_goal_purity(Goali0, Goali, PredInfo, ModuleInfo,
			    InClosure, Purity1, NumErrors0, NumErrors1),
	compute_goal_purity(Goalt0, Goalt, PredInfo, ModuleInfo,
			    InClosure, Purity2, NumErrors1, NumErrors2),
	compute_goal_purity(Goale0, Goale, PredInfo, ModuleInfo,
			    InClosure, Purity3, NumErrors2, NumErrors),
	{ worst_purity(Purity1, Purity2, Purity12) },
	{ worst_purity(Purity12, Purity3, Purity) }.
compute_expr_purity(Ccode, Ccode, _, _, ModuleInfo, _, Purity,
		NumErrors, NumErrors) -->
	{ Ccode = pragma_c_code(_,PredId,_,_,_,_,_) },
	{ module_info_preds(ModuleInfo, Preds) },
	{ map__lookup(Preds, PredId, PredInfo) },
	{ pred_info_get_purity(PredInfo, Purity) }.

:- pred compute_goal_purity(hlds_goal, hlds_goal, pred_info,
	module_info, bool, purity, int, int, io__state, io__state).
:- mode compute_goal_purity(in, out, in, in, in, out, in, out, di, uo) is det.

compute_goal_purity(Goal0 - GoalInfo0, Goal - GoalInfo, PredInfo, ModuleInfo,
		InClosure, Purity, NumErrors0, NumErrors) -->
	compute_expr_purity(Goal0, Goal, GoalInfo0, PredInfo, ModuleInfo,
			    InClosure, Purity, NumErrors0, NumErrors),
	{ add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo) }.


%  Compute the purity of a list of hlds_goals.  Since the purity of a
%  disjunction is computed the same way as the purity of a conjunction, we use
%  the same code for both

:- pred compute_goals_purity(list(hlds_goal), list(hlds_goal), pred_info,
	module_info, bool, purity, purity, int, int, io__state, io__state).
:- mode compute_goals_purity(in, out, in, in, in, in, out, in, out, di, uo)
	is det.

compute_goals_purity([], [], _, _, _, Purity, Purity, NumErrors, NumErrors) -->
	[].
compute_goals_purity([Goal0|Goals0], [Goal|Goals], PredInfo, ModuleInfo,
		InClosure, Purity0, Purity, NumErrors0, NumErrors) -->
	compute_goal_purity(Goal0, Goal, PredInfo, ModuleInfo, 
			    InClosure, Purity1, NumErrors0, NumErrors1),
	{ worst_purity(Purity0, Purity1, Purity2) },
	compute_goals_purity(Goals0, Goals, PredInfo, ModuleInfo, InClosure,
			     Purity2, Purity, NumErrors1, NumErrors).



:- pred compute_cases_purity(list(case), list(case), pred_info, module_info,
	bool, purity, purity, int, int, io__state, io__state).
:- mode compute_cases_purity(in, out, in, in, in, in, out, in, out, di, uo)
	is det.

compute_cases_purity([], [], _, _, _, Purity, Purity, NumErrors, NumErrors) -->
	[].
compute_cases_purity([case(Ctor,Goal0)|Goals0], [case(Ctor,Goal)|Goals],
		PredInfo, ModuleInfo, InClosure, Purity0, Purity,
		NumErrors0, NumErrors) -->
	compute_goal_purity(Goal0, Goal, PredInfo, ModuleInfo, 
			    InClosure, Purity1, NumErrors0, NumErrors1),
	{ worst_purity(Purity0, Purity1, Purity2) },
	compute_cases_purity(Goals0, Goals, PredInfo, ModuleInfo, InClosure,
			     Purity2, Purity, NumErrors1, NumErrors).

	% Make sure lambda expressions introduced by the compiler
	% have the correct mode for their `aditi__state' arguments.
:- pred fix_aditi_state_modes((mode), list(type), list(mode), list(mode)).
:- mode fix_aditi_state_modes(in, in, in, out) is det.

fix_aditi_state_modes(_, [], [], []).
fix_aditi_state_modes(_, [_|_], [], []) :-
	error("purity:fix_aditi_state_modes").
fix_aditi_state_modes(_, [], [_|_], []) :-
	error("purity:fix_aditi_state_modes").
fix_aditi_state_modes(AditiStateMode, [Type | Types],
		[ArgMode0 | Modes0], [ArgMode | Modes]) :-
	( type_is_aditi_state(Type) ->
		ArgMode = AditiStateMode
	;
		ArgMode = ArgMode0
	),
	fix_aditi_state_modes(AditiStateMode, Types, Modes0, Modes).

%-----------------------------------------------------------------------------%
%				Print error messages


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

:- pred warn_unnecessary_promise_pure(module_info, pred_info, pred_id,
				  io__state, io__state).
:- mode warn_unnecessary_promise_pure(in, in, in, di, uo) is det.

warn_unnecessary_promise_pure(ModuleInfo, PredInfo, PredId) -->
	{ pred_info_context(PredInfo, Context) },
	write_context_and_pred_id(ModuleInfo, PredInfo, PredId),
	prog_out__write_context(Context),
	report_warning("  warning: unnecessary `promise_pure' pragma.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		io__write_string("  This "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(
		    " does not invoke any impure or semipure code,\n"
		),
		prog_out__write_context(Context),
		io__write_string(
		    "  so there is no need for a `promise_pure' pragma.\n"
		)
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
	io__write_string("  error: "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" is "),
	write_purity(Purity),
	io__write_string(".\n"),
	prog_out__write_context(Context),
	( { code_util__compiler_generated(PredInfo) } ->
		io__write_string("  It must be pure.\n")
	;
		io__write_string("  It must be declared `"),
		write_purity(Purity),
		io__write_string("' or promised pure.\n")
	).


:- pred error_missing_body_impurity_decl(module_info, pred_info, pred_id,
				  prog_context, purity, io__state, io__state).
:- mode error_missing_body_impurity_decl(in, in, in, in, in, di, uo) is det.

error_missing_body_impurity_decl(ModuleInfo, _, PredId, Context,
		Purity) -->
	prog_out__write_context(Context),
	io__write_string("In call to "),
	write_purity(Purity),
	io__write_string(" "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: call must be preceded by `"),
	write_purity(Purity),
	io__write_string("' indicator.\n").


:- pred warn_unnecessary_body_impurity_decl(module_info, pred_info, pred_id,
	prog_context, purity, purity, io__state, io__state).
:- mode warn_unnecessary_body_impurity_decl(in, in, in, in, in, in, di, uo)
	is det.

warn_unnecessary_body_impurity_decl(ModuleInfo, _, PredId, Context,
		ActualPurity, DeclaredPurity) -->
	prog_out__write_context(Context),
	io__write_string("In call to "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  warning: unnecessary `"),
	write_purity(DeclaredPurity),
	io__write_string("' indicator.\n"),
	prog_out__write_context(Context),
	( { ActualPurity = pure } ->
		io__write_string("  No purity indicator is necessary.\n")
	;
		io__write_string("  A purity indicator of `"),
		write_purity(ActualPurity),
		io__write_string("' is sufficient.\n")
	).
	

:- pred error_if_closure_impure(hlds_goal_info, purity, int, int,
	io__state, io__state).
:- mode error_if_closure_impure(in, in, in, out, di, uo) is det.

error_if_closure_impure(GoalInfo, Purity, NumErrors0, NumErrors) -->
	( { Purity = pure } ->
		{ NumErrors = NumErrors0 }
	;
		{ NumErrors is NumErrors0 + 1 },
		{ goal_info_get_context(GoalInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("Error in closure: closure is "),
		write_purity(Purity),
		io__write_string(".\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  All closures must be pure.\n")
		;   
			[]
		)
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


%-----------------------------------------------------------------------------%
