%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_constraint.m
% Main author: dmo
%

:- module check_hlds__mode_ordering.

:- interface.

:- import_module check_hlds__mode_constraint_robdd.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__inst_graph.

:- import_module io, list, map.

:- type pred_constraint_info --->
	pci(		% pci: pred_constrain_info
		mode_constraint,
		mode_constraint_info
	).

:- type pred_constraint_map == map(pred_id, pred_constraint_info).

% Given a top-down list of predicate SCCs, attempt to schedule goals for mode
% of each predicate, and determine which modes are needed for each predicate.

:- pred mode_ordering(pred_constraint_map::in, list(list(pred_id))::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

:- pred mode_ordering__proc(inst_graph::in, mode_constraint::in,
	mode_constraint_info::in, module_info::in, pred_constraint_map::in,
	proc_info::in, proc_info::out) is det.

:- implementation.

:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__mode_constraint_robdd.
:- import_module hlds__hlds_goal.
:- import_module mode_robdd.
% :- import_module mode_robdd__check.
% :- import_module mode_robdd__tfeir.
:- import_module mode_robdd__tfeirn.
:- import_module parse_tree__prog_data.

:- import_module set, stack, std_util, require, relation, assoc_list.

mode_ordering(PredConstraintMap, SCCs, !ModuleInfo, !IO) :-
	list__foldl(mode_ordering__scc(PredConstraintMap), SCCs, !ModuleInfo),
	report_mode_errors(!.ModuleInfo, !IO).

:- pred mode_ordering__scc(pred_constraint_map::in, list(pred_id)::in,
	module_info::in, module_info::out) is det.

mode_ordering__scc(PredConstraintMap, SCC, !ModuleInfo) :-
	copy_module_clauses_to_procs(SCC, !ModuleInfo),
	list__foldl(mode_ordering__pred(PredConstraintMap, SCC), SCC,
		!ModuleInfo).

:- pred mode_ordering__pred(pred_constraint_map::in, list(pred_id)::in,
	pred_id::in, module_info::in, module_info::out) is det.

mode_ordering__pred(PredConstraintMap, _SCC, PredId, !ModuleInfo) :-
	% XXX Mode inference NYI.
	RequestedProcsMap0 = map__init,

	module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
	lookup_pred_constraint(PredConstraintMap, PredId,
		ModeConstraint0, ModeConstraintInfo),
	( pred_info_infer_modes(PredInfo0) ->
		( map__search(RequestedProcsMap0, PredId, RequestedProcs) ->
			list__foldl(mode_ordering__infer_proc(ModeConstraint0,
					ModeConstraintInfo, !.ModuleInfo,
					PredConstraintMap),
				RequestedProcs, PredInfo0, PredInfo)
		;
			% XXX Maybe we should remove the predicate from the
			% module_info here since it is not used.
			PredInfo = PredInfo0
		)
	;
		ProcIds = pred_info_non_imported_procids(PredInfo0),
		list__foldl(mode_ordering__check_proc(ModeConstraint0,
				ModeConstraintInfo, !.ModuleInfo,
				PredConstraintMap),
			ProcIds, PredInfo0, PredInfo)
	),
	module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred mode_ordering__infer_proc(mode_constraint::in,
	mode_constraint_info::in, module_info::in, pred_constraint_map::in,
	mode_constraint::in, pred_info::in, pred_info::out) is det.

mode_ordering__infer_proc(Constraint0, ModeConstraintInfo, ModuleInfo,
		PredConstraintMap, ModeDeclConstraint, PredInfo0, PredInfo) :-
	pred_info_create_proc_info_for_mode_decl_constraint(PredInfo0,
		ModeDeclConstraint, ProcId, PredInfo1),
	mode_ordering__check_proc(Constraint0, ModeConstraintInfo, ModuleInfo,
		PredConstraintMap, ProcId, PredInfo1, PredInfo).

:- pred mode_ordering__check_proc(mode_constraint::in,
	mode_constraint_info::in, module_info::in, pred_constraint_map::in,
	proc_id::in, pred_info::in, pred_info::out) is det.

mode_ordering__check_proc(Constraint0, ModeConstraintInfo, ModuleInfo,
		PredConstraintMap, ProcId, PredInfo0, PredInfo) :-
	pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),
	proc_info_head_modes_constraint(ProcInfo0, ModeDeclConstraint),
	Constraint = Constraint0 * ModeDeclConstraint,
	InstGraph = PredInfo0 ^ inst_graph_info ^ implementation_inst_graph,
	mode_ordering__proc(InstGraph, Constraint, ModeConstraintInfo,
		ModuleInfo, PredConstraintMap, ProcInfo0, ProcInfo),
	pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo).

% Perform mode ordering for a procedure. The ModeConstraint must be constrained
% to contain just the mode information for this procedure.

mode_ordering__proc(InstGraph, ModeConstraint, ModeConstraintInfo, ModuleInfo,
		PredConstraintMap, !ProcInfo) :-
	Info0 = mode_ordering__info(InstGraph,
		atomic_prodvars_map(ModeConstraint, ModeConstraintInfo),
		stack__init, ModuleInfo, PredConstraintMap),
	proc_info_goal(!.ProcInfo, Goal0),
	mode_ordering__goal(Goal0, Goal, Info0, _Info),
	proc_info_set_goal(Goal, !ProcInfo).

:- type mode_ordering__info
	--->	mode_ordering__info(
			inst_graph		:: inst_graph,
			prodvars_map		:: prodvars_map,
			lambda_nesting		:: lambda_path,
			module_info		:: module_info,
			pred_constraint_map	:: pred_constraint_map
		).

:- pred enter_lambda_goal(goal_path::in, mode_ordering__info::in,
	mode_ordering__info::out) is det.

enter_lambda_goal(GoalPath, !MOI) :-
	LambdaNesting0 = !.MOI ^ lambda_nesting,
	!:MOI = !.MOI ^ lambda_nesting
		:= stack__push(LambdaNesting0, GoalPath).

:- pred leave_lambda_goal(mode_ordering__info::in, mode_ordering__info::out)
	is det.

leave_lambda_goal(!MOI) :-
	LambdaNesting0 = !.MOI ^ lambda_nesting,
	stack__pop_det(LambdaNesting0, _, LambdaNesting),
	!:MOI = !.MOI ^ lambda_nesting := LambdaNesting.

:- pred mode_ordering__goal(hlds_goal::in, hlds_goal::out,
	mode_ordering__info::in, mode_ordering__info::out) is det.

mode_ordering__goal(GoalExpr0 - GoalInfo0, GoalExpr - GoalInfo, !MOI) :-
	mode_ordering__goal_2(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !MOI).

:- pred mode_ordering__goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
	hlds_goal_info::in, hlds_goal_info::out,
	mode_ordering__info::in, mode_ordering__info::out) is det.

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = conj(Goals0),
	Goal = conj(Goals), 
	list__map_foldl(mode_ordering__goal, Goals0, Goals1, !MOI),
	mode_ordering__conj(Goals1, Goals),
	union_mode_vars_sets(Goals, GoalInfo0, GoalInfo1),
	ConsVars = GoalInfo1 ^ consuming_vars,
	GoalInfo2 = GoalInfo1 ^ consuming_vars :=
			ConsVars `difference` GoalInfo1 ^ producing_vars,
	NeedVars = GoalInfo2 ^ need_visible_vars,
	GoalInfo = GoalInfo2 ^ need_visible_vars :=
			NeedVars `difference` GoalInfo2 ^ make_visible_vars.

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = call(PredId, _, Args, _, _, _),
	Goal = Goal0 ^ call_proc_id := ProcId,
	set_atomic_prod_vars(ProdVars, GoalInfo0, GoalInfo1, !MOI),
	MakeVisibleVars = list_to_set(Args) `intersect` ProdVars,
	
	find_matching_proc(PredId, Args, ProdVars, ProcId, ConsumingVars, !MOI),
	NeedVisibleVars = list_to_set(Args) `intersect` ConsumingVars,

	goal_info_set_consuming_vars(GoalInfo1, ConsumingVars, GoalInfo2),
	goal_info_set_make_visible_vars(GoalInfo2, MakeVisibleVars,
		GoalInfo3),
	goal_info_set_need_visible_vars(GoalInfo3, NeedVisibleVars,
		GoalInfo).

mode_ordering__goal_2(Goal0, _, _, _, !MOI) :-
	Goal0 = generic_call(_GenericCall0, _Args, _Modes0, _Det),
	error("mode_ordering__goal_2: generic_call NYI").

mode_ordering__goal_2(Goal0, _, _, _, !MOI) :-
	Goal0 = switch(_Var, _CanFail0, _Cases0),
	error("mode_ordering__goal_2: switch").

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = unify(VarA, RHS0, UnifyMode, Unification0, Context),
	Goal = unify(VarA, RHS, UnifyMode, Unification, Context),
	set_atomic_prod_vars(ProdVars, GoalInfo0, GoalInfo1, !MOI),
	InstGraph = !.MOI ^ inst_graph,
	(
		RHS0 = var(VarB),
		RHS = RHS0,
		( ProdVars `contains` VarA ->
			Unification = assign(VarA, VarB),
			MakeVisibleVars = make_singleton_set(VarA),
			NeedVisibleVars = make_singleton_set(VarB)
		; ProdVars `contains` VarB ->
			Unification = assign(VarB, VarA),
			MakeVisibleVars = make_singleton_set(VarB),
			NeedVisibleVars = make_singleton_set(VarA)
		;
			Unification = simple_test(VarA, VarB),
			% XXX may be complicated unify -- need to check.
			MakeVisibleVars = set__init,
			NeedVisibleVars = list_to_set([VarA, VarB])
		),
		ConsumingVars = solutions_set((pred(Var::out) is nondet :-
			inst_graph__corresponding_nodes(InstGraph, VarA, VarB,
				VarC, VarD),
			( ProdVars `contains` VarC ->
				Var = VarD
			; ProdVars `contains` VarD ->
				Var = VarC
			;
				fail
			)))
	;
		RHS0 = functor(_ConsId, _IsExistConstruct, ArgVars),
		RHS = RHS0,
		( ProdVars `contains` VarA ->
			% Unification = construct(VarA, ConsId, ArgVars,
			%	_UniModes, _HowTo, _CellUniq, _MaybeRLExprId),
			Unification = Unification0, % XXX
			ConsumingVars = set__init,
			MakeVisibleVars = list_to_set([VarA | ArgVars]),
			NeedVisibleVars = set__init
		;
			% Unification = deconstruct(VarA, ConsId, ArgVars,
			%	_UniModes, _CanFail, _CanCGC),
			Unification = Unification0, % XXX
			ConsumingVars = make_singleton_set(VarA),
			MakeVisibleVars = list_to_set(ArgVars),
			NeedVisibleVars = make_singleton_set(VarA)
		)
	;
		% Unification = construct(VarA, _ConsId, _ArgVars,
		%	_UniModes, _HowTo, _CellUniq, _MaybeRLExprId),
		Unification = Unification0, % XXX
		RHS0 = lambda_goal(A, B, C, D, NonLocals, LambdaVars, Modes0,
				H, SubGoal0),
		Modes = Modes0, % XXX
		RHS = lambda_goal(A, B, C, D, NonLocals, LambdaVars, Modes,
				H, SubGoal),
		
		goal_info_get_goal_path(GoalInfo1, GoalPath),
		enter_lambda_goal(GoalPath, !MOI),
		mode_ordering__goal(SubGoal0, SubGoal, !MOI),
		leave_lambda_goal(!MOI),

		ConsumingVars = solutions_set(
			inst_graph__reachable_from_list(InstGraph, NonLocals)),
		MakeVisibleVars = make_singleton_set(VarA),
		NeedVisibleVars = list_to_set(NonLocals)
	),
	goal_info_set_consuming_vars(GoalInfo1, ConsumingVars, GoalInfo2),
	goal_info_set_make_visible_vars(GoalInfo2, MakeVisibleVars,
		GoalInfo3),
	goal_info_set_need_visible_vars(GoalInfo3, NeedVisibleVars,
		GoalInfo).
	
mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = disj(Goals0),
	Goal = disj(Goals), 
	list__map_foldl(mode_ordering__goal, Goals0, Goals, !MOI),
	mode_ordering__disj(Goals, GoalInfo0, GoalInfo).

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = not(SubGoal0),
	Goal = not(SubGoal), 
	mode_ordering__goal(SubGoal0, SubGoal, !MOI),
	goal_info_copy_mode_var_sets(SubGoal ^ snd, GoalInfo0, GoalInfo).

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = some(Vars, CanRemove, SubGoal0),
	Goal = some(Vars, CanRemove, SubGoal),
	mode_ordering__goal(SubGoal0, SubGoal, !MOI),
	goal_info_copy_mode_var_sets(SubGoal ^ snd, GoalInfo0, GoalInfo).

mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = if_then_else(Locals, Cond0, Then0, Else0),
	Goal = if_then_else(Locals, Cond, Then, Else),
	mode_ordering__goal(Cond0, Cond, !MOI),
	mode_ordering__goal(Then0, Then, !MOI),
	mode_ordering__goal(Else0, Else, !MOI),
	% XXX Ned to make sure that Cond can be scheduled before Then and Else.

	union_mode_vars_sets([Cond, Then], GoalInfo0, GoalInfo1),
	ConsVars = GoalInfo1 ^ consuming_vars,
	GoalInfo2 = GoalInfo1 ^ consuming_vars :=
			ConsVars `difference` GoalInfo1 ^ producing_vars,
	NeedVars = GoalInfo2 ^ need_visible_vars,
	GoalInfo3 = GoalInfo2 ^ need_visible_vars :=
			NeedVars `difference` GoalInfo2 ^ make_visible_vars,

	combine_mode_vars_sets(Else ^ snd, GoalInfo3, GoalInfo).

mode_ordering__goal_2(Goal0, _, _, _, !MOI) :-
	Goal0 = foreign_proc(_, _, _, _, _, _),
	error("mode_ordering__goal_2: pragma_foreign_code NYI").
	
mode_ordering__goal_2(Goal0, Goal, GoalInfo0, GoalInfo, !MOI) :-
	Goal0 = par_conj(Goals0),
	Goal = par_conj(Goals), 
	list__map_foldl(mode_ordering__goal, Goals0, Goals, !MOI),
	union_mode_vars_sets(Goals, GoalInfo0, GoalInfo).

mode_ordering__goal_2(Goal0, _, _, _, !MOI) :-
	Goal0 = shorthand(_), 
	error("mode_ordering__goal_2: shorthand").

:- pred mode_ordering__disj(hlds_goals::in,
	hlds_goal_info::in, hlds_goal_info::out) is det.

mode_ordering__disj([], !GoalInfo).
mode_ordering__disj([_ - GI | Goals], !GoalInfo) :-
	goal_info_copy_mode_var_sets(GI, !GoalInfo),
	list__foldl(mode_ordering__disj_2, Goals, !GoalInfo).

:- pred mode_ordering__disj_2(hlds_goal::in,
	hlds_goal_info::in, hlds_goal_info::out) is det.

mode_ordering__disj_2(_ - GI, !GoalInfo) :-
	combine_mode_vars_sets(GI, !GoalInfo).

:- pred combine_mode_vars_sets(hlds_goal_info::in,
	hlds_goal_info::in, hlds_goal_info::out) is det.

combine_mode_vars_sets(GI) -->
	ProdVars0 =^ producing_vars,
	ConsumVars0 =^ consuming_vars,
	MakeVisibleVars0 =^ make_visible_vars,
	NeedVisibleVars0 =^ need_visible_vars,

	^producing_vars := ProdVars0 `intersect` GI^producing_vars,
	^consuming_vars := ConsumVars0 `union` GI^consuming_vars,
	^make_visible_vars := MakeVisibleVars0 `intersect` GI^make_visible_vars,
	^need_visible_vars := NeedVisibleVars0 `union` GI^need_visible_vars.

:- pred union_mode_vars_sets(hlds_goals::in,
	hlds_goal_info::in, hlds_goal_info::out) is det.

union_mode_vars_sets(Goals) -->
	list__foldl((pred(Goal::in, in, out) is det -->
	    ProdVars0 =^ producing_vars,
	    ConsumVars0 =^ consuming_vars,
	    MakeVisibleVars0 =^ make_visible_vars,
	    NeedVisibleVars0 =^ need_visible_vars,
	    { Goal = _ - GI },

	    ^producing_vars := ProdVars0 `union` GI^producing_vars,
	    ^consuming_vars := ConsumVars0 `union` GI^consuming_vars,
	    ^make_visible_vars := MakeVisibleVars0 `union` GI^make_visible_vars,
	    ^need_visible_vars := NeedVisibleVars0 `union` GI^need_visible_vars
	    ), Goals).

:- pred goal_info_copy_mode_var_sets(hlds_goal_info::in,
	hlds_goal_info::in, hlds_goal_info::out) is det.

goal_info_copy_mode_var_sets(GI) -->
	^ producing_vars := GI^producing_vars,
	^ consuming_vars := GI^consuming_vars,
	^ make_visible_vars := GI^make_visible_vars,
	^ need_visible_vars := GI^need_visible_vars.
	
:- pred mode_ordering__conj(hlds_goals::in, hlds_goals::out) is det.

mode_ordering__conj(Goals0, Goals) :-
	GoalMap = list__foldl((func(G, GM) = map__det_insert(GM, Index, G) :-
		(
			G = _ - GI,
			goal_info_get_goal_path(GI, GP),
			GP = [conj(Index0) | _]
		->
			Index = Index0
		;
			error("mode_ordering__conj: goal_path error")
		)), Goals0, map__init),

	ProdMap =
	    map__foldl((func(I, G, PM0) =
		    list__foldl((func(V, PM1) = map__det_insert(PM1, V, I)),
			set__to_sorted_list(G ^ snd ^ producing_vars), PM0)
		), GoalMap, map__init),

	MakeVisMap =
	    map__foldl((func(I, G, MVM0) =
		    list__foldl((func(V, MVM1) = map__set(MVM1, V, I)),
		    				% XXX disjunction required!
			set__to_sorted_list(G ^ snd ^ make_visible_vars), MVM0)
		), GoalMap, map__init),

	Relation = map__foldl((func(I, G, R0) = R :-
		GI = G ^ snd,
		relation__add_element(R0, I, Key0, R1),
		R2 = list__foldl((func(V, R10) = R12 :-
			    ( Index1 = map__search(ProdMap, V) ->
				relation__add_element(R10, Index1, Key1, R11),
				relation__add(R11, Key1, Key0, R12)
			    ;
			    	R12 = R10
			    )
			), set__to_sorted_list(GI ^ consuming_vars), R1),
		R = list__foldl((func(V, R20) = R22 :-
			    ( Index2 = map__search(MakeVisMap, V) ->
				relation__add_element(R20, Index2, Key2, R21),
				relation__add(R21, Key2, Key0, R22)
			    ;
			    	R22 = R20
			    )
			), set__to_sorted_list(GI ^ need_visible_vars), R2)
		), GoalMap, relation__init),

	( relation__tsort(Relation, TSort) ->
		Goals = map__apply_to_list(TSort, GoalMap)
	;
		% XXX Report a mode error for this.
		error("mode_ordering__conj: Cycle in goal dependencies.")
	).

:- pred set_atomic_prod_vars(set(prog_var)::out, hlds_goal_info::in,
	hlds_goal_info::out, mode_ordering__info::in,
	mode_ordering__info::out) is det.

set_atomic_prod_vars(ProdVars, GoalInfo0, GoalInfo) -->
	LambdaNesting =^ lambda_nesting,
	AtomicProdVars =^ prodvars_map,
	{ goal_info_get_goal_path(GoalInfo0, GoalPath) },
	{ ProdVars = ( map__search(AtomicProdVars,
			stack__push(LambdaNesting, GoalPath), ProdVars0)
	->
		ProdVars0
	;
		set__init
	)},
	{ goal_info_set_producing_vars(GoalInfo0, ProdVars, GoalInfo) }.

:- pred pred_info_create_proc_info_for_mode_decl_constraint(pred_info::in,
	mode_constraint::in, proc_id::out, pred_info::out) is det.

pred_info_create_proc_info_for_mode_decl_constraint(PredInfo0,
		_ModeDeclConstraint, ProcId, PredInfo) :-
	( semidet_succeed ->
		% XXX
		error("NYI: pred_info_create_proc_info_for_mode_decl_constraint")
	;
		% XXX keep det checker happy.
		PredInfo = PredInfo0,
		ProcId = initial_proc_id
	).

:- pred find_matching_proc(pred_id::in, list(prog_var)::in, set(prog_var)::in,
	proc_id::out, set(prog_var)::out, mode_ordering__info::in,
	mode_ordering__info::out) is det.

find_matching_proc(PredId, Args, ProdVars, ProcId, ConsumingVars) -->
	ModuleInfo =^ module_info,
	CallerInstGraph =^ inst_graph,
	PredConstraintMap =^ pred_constraint_map,
	{ lookup_pred_constraint(PredConstraintMap, PredId, _, MCInfo) },
	
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ CalleeInstGraph = PredInfo^inst_graph_info^interface_inst_graph },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__to_assoc_list(ProcTable, ProcList) },
	(
		{ find_matching_proc_2(ProcList, ProdVars, Args,
			CallerInstGraph, CalleeInstGraph, MCInfo, ProcId0,
			ConsumingVars0) }
	->
		{ ProcId = ProcId0 },
		{ ConsumingVars = ConsumingVars0 }
	;
		{ pred_info_infer_modes(PredInfo) }
	->
		% XXX We are inferring modes for the called predicate.  Need to
		% add a new mode to the requested procs map.
		{ error("find_matching_proc: infer_modes NYI") }
	;
		% If we get here, it means there is a mode error which should
		% have been picked up by the constraints pass but was missed
		% some how.
		{ error("find_matching_proc: unexpected mode error") }
	).

:- pred find_matching_proc_2(assoc_list(proc_id, proc_info)::in,
	set(prog_var)::in, list(prog_var)::in, inst_graph::in, inst_graph::in,
	mode_constraint_info::in, proc_id::out, set(prog_var)::out) is semidet.

find_matching_proc_2([ProcId0 - ProcInfo | ProcList], ProdVars, Args,
		CallerInstGraph, CalleeInstGraph, MCInfo, ProcId,
		ConsumingVars) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_head_modes_constraint(ProcInfo, Constraint0),
	Constraint = ensure_normalised(Constraint0),
	(
	    all [X, Y] inst_graph__corresponding_nodes_from_lists(
		CallerInstGraph, CalleeInstGraph, Args, HeadVars, X, Y)
	    =>
		(
		    ProdVars `contains` X
		<=>
		    (
		    	var_entailed(Constraint,
				mode_constraint_var(MCInfo, out(Y))),
		    	\+ var_entailed(Constraint,
				mode_constraint_var(MCInfo, in(Y)))
		    )
		)
	->
	    ProcId = ProcId0,
	    ConsumingVars = solutions_set(pred(X::out) is nondet :-
		some [Y] (
		    inst_graph__corresponding_nodes_from_lists(CallerInstGraph,
			CalleeInstGraph, Args, HeadVars, X, Y),
		    var_entailed(Constraint, mode_constraint_var(MCInfo, in(Y)))
		))
	;
	    find_matching_proc_2(ProcList, ProdVars, Args, CallerInstGraph,
		CalleeInstGraph, MCInfo, ProcId, ConsumingVars)
	).

:- pred report_mode_errors(module_info::in, io__state::di, io__state::uo)
		is det.

report_mode_errors(_) --> [].
	% XXX
	%io__stderr_stream(StdErr),
	%io__write_string(StdErr, "Mode error reporting NYI").

:- pred lookup_pred_constraint(pred_constraint_map::in, pred_id::in,
		mode_constraint::out, mode_constraint_info::out) is det.

lookup_pred_constraint(PCM, PredId, MC, MCInfo) :-
	map__lookup(PCM, PredId, pci(MC, MCInfo)).
