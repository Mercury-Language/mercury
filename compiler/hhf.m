%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Convert superhomogeneous form to hyperhomogeneous form and output an
% inst graph for the predicate based on this transformation.
%
% Hyperhomogeneous form and the transformation are documented in
% David Overton's PhD thesis.

:- module hlds__hhf.

:- interface.

:- import_module hlds__hlds_pred.
:- import_module hlds__hlds_module.
:- import_module hlds__inst_graph.

:- import_module bool.
:- import_module io.

:- pred hhf__process_pred(bool::in, pred_id::in, module_info::in,
	module_info::out, io__state::di, io__state::uo) is det.

:- pred hhf__process_clauses_info(bool::in, module_info::in, clauses_info::in,
	clauses_info::out, inst_graph::out) is det.

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

hhf__process_pred(Simple, PredId, !ModuleInfo, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
	( pred_info_is_imported(PredInfo0) ->
		% AAA
		% PredInfo2 = PredInfo0
		pred_info_clauses_info(PredInfo0, ClausesInfo),
		clauses_info_headvars(ClausesInfo, HeadVars),
		clauses_info_varset(ClausesInfo, VarSet),
		IGI0 = PredInfo0 ^ inst_graph_info,
		inst_graph__init(HeadVars, InstGraph),
		IGI1 = IGI0 ^ implementation_inst_graph := InstGraph,
		IGI2 = IGI1 ^ interface_inst_graph := InstGraph,
		IGI3 = IGI2 ^ interface_vars := HeadVars,
		IGI4 = IGI3 ^ interface_varset := VarSet,
		PredInfo2 = PredInfo0 ^ inst_graph_info := IGI4
	;
		write_pred_progress_message(
			"% Calculating HHF and inst graph for ",
			PredId, !.ModuleInfo, !IO),

		pred_info_clauses_info(PredInfo0, ClausesInfo0),
		hhf__process_clauses_info(Simple, !.ModuleInfo, ClausesInfo0,
			ClausesInfo, ImplementationInstGraph),
		pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo1),
		IGI0 = PredInfo1 ^ inst_graph_info,
		IGI1 = IGI0 ^ implementation_inst_graph :=
				ImplementationInstGraph,

		% AAA only for non-imported preds with no mode decls.
		clauses_info_headvars(ClausesInfo, HeadVars),
		clauses_info_varset(ClausesInfo, VarSet),
		IGI2 = IGI1 ^ interface_inst_graph := ImplementationInstGraph,
		solutions((pred(V::out) is nondet :-
				list__member(V0, HeadVars),
				inst_graph__reachable(ImplementationInstGraph,
				V0, V)
			), InterfaceVars),
		IGI3 = IGI2 ^ interface_vars := InterfaceVars,
		IGI = IGI3 ^ interface_varset := VarSet,

		PredInfo2 = PredInfo1 ^ inst_graph_info := IGI
	),

% 	pred_info_get_markers(PredInfo2, Markers),
% 	( check_marker(Markers, infer_modes) ->
% 		% No mode declarations.  If not imported, use implementation
% 		% inst_graph.
% 		% ...
% 	;
% 		pred_info_clauses_info(PredInfo2, ClausesInfo2),
% 		clauses_info_headvars(ClausesInfo2, HeadVars),
% 		clauses_info_varset(ClausesInfo2, VarSet),
% 		inst_graph__init(HeadVars, InterfaceInstGraph),
% 		InstGraphInfo0 = ( (PredInfo2 ^ inst_graph_info)
% 			^ interface_inst_graph := InterfaceInstGraph )
% 			^ interface_varset := VarSet,
% 		map__foldl(hhf__process_proc(ModuleInfo0, HeadVars),
% 			Procedures, InstGraphInfo0, InstGraphInfo1),
% 
% 		% Calculate interface vars.
% 		solutions((pred(V::out) is nondet :-
% 				list__member(V0, HeadVars),
% 				inst_graph__reachable(InstGraph, V0, V)
% 			), InterfaceVars),
% 		InstGraphInfo = InstGraphInfo1 ^ interface_vars :=
% 			InterfaceVars,
% 
% 		PredInfo = PredInfo2 ^ inst_graph_info := InstGraphInfo
% 	),

	PredInfo = PredInfo2, % AAA
	module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

hhf__process_clauses_info(Simple, ModuleInfo, !ClausesInfo, InstGraph) :-
	clauses_info_varset(!.ClausesInfo, VarSet0),
	clauses_info_vartypes(!.ClausesInfo, VarTypes0),
	inst_graph__init(VarTypes0 ^ keys, InstGraph0),
	Info0 = hhf_info(InstGraph0, VarSet0, VarTypes0),

	clauses_info_headvars(!.ClausesInfo, HeadVars),
	clauses_info_clauses(!.ClausesInfo, Clauses0),

	(
	%	% For simple mode checking we do not give the inst_graph any
	%	% structure.
	%	Simple = yes,
	%	Clauses = Clauses0,
	%	Info1 = Info0
	%;
	%	Simple = no,
		list__map_foldl(hhf__process_clause(HeadVars),
			Clauses0, Clauses, Info0, Info1)
	),

	clauses_info_set_clauses(Clauses, !ClausesInfo),

	complete_inst_graph(ModuleInfo, Info1, Info),
	% XXX Comment out the above line for incomplete, quick checking.
	% Info = Info1,

	Info = hhf_info(InstGraph1, VarSet, VarTypes),
	( Simple = yes, inst_graph__init(VarTypes ^ keys, InstGraph)
	; Simple = no,  InstGraph = InstGraph1
	),

	% XXX do we need this (it slows things down a lot (i.e. uses 50%
	% of the runtime).
	% varset__vars(VarSet1, Vars1),
	% varset__ensure_unique_names(Vars1, "_", VarSet1, VarSet),

	clauses_info_set_varset(VarSet, !ClausesInfo),
	clauses_info_set_vartypes(VarTypes, !ClausesInfo).

:- type hhf_info
	--->	hhf_info(
			inst_graph	:: inst_graph,
			varset		:: prog_varset,
			vartypes	:: vartypes
		).

:- pred hhf__process_clause(list(prog_var)::in, clause::in, clause::out,
	hhf_info::in, hhf_info::out) is det.

hhf__process_clause(_HeadVars, clause(ProcIds, Goal0, Lang, Context),
		clause(ProcIds, Goal, Lang, Context), !HI) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_nonlocals(GoalInfo0, NonLocals),

	hhf__goal(NonLocals, Goal0, Goal, !HI).
% XXX We probably need to requantify, but it stuffs up the inst_graph to do
% that.
% 	VarSet1 = !.HI ^ varset,
% 	VarTypes1 = !.HI ^ vartypes,
% 	implicitly_quantify_clause_body(HeadVars, Goal1, VarSet1, VarTypes1,
% 		Goal, VarSet, VarTypes, _Warnings),
% 	!:HI = !.HI varset := VarSet,
% 	!:HI = !.HI vartypes := VarTypes.

:- pred hhf__goal(set(prog_var)::in, hlds_goal::in, hlds_goal::out,
	hhf_info::in, hhf_info::out) is det.

hhf__goal(NonLocals, GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, !HI) :-
	hhf__goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr, !HI).

:- pred hhf__goal_use_own_nonlocals(hlds_goal::in, hlds_goal::out,
	hhf_info::in, hhf_info::out) is det.

hhf__goal_use_own_nonlocals(GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, !HI) :-
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	hhf__goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr, !HI).

:- pred hhf__goal_expr(set(prog_var)::in, hlds_goal_info::in,
	hlds_goal_expr::in, hlds_goal_expr::out, hhf_info::in, hhf_info::out)
	is det.

hhf__goal_expr(NonLocals, _, conj(Goals0), conj(Goals), !HI) :-
	list__map_foldl(hhf__goal(NonLocals), Goals0, Goals1, !HI),
	flatten_conj(Goals1, Goals).
hhf__goal_expr(_, _, call(A, B, C, D, E, F), call(A, B, C, D, E, F), !HI).
hhf__goal_expr(_, _, generic_call(A, B, C, D), generic_call(A, B, C, D), !HI).
hhf__goal_expr(_, _, switch(_, _, _), _, !HI) :-
	error("hhf_goal_expr: found switch").
hhf__goal_expr(_, _, foreign_proc(A,B,C,D,E,F), foreign_proc(A,B,C,D,E,F),
		!HI).
hhf__goal_expr(_, _, shorthand(_), _, !HI) :-
	error("hhf_goal_expr: found shorthand").
hhf__goal_expr(NonLocals, _, scope(Reason, Goal0), scope(Reason, Goal), !HI) :-
	hhf__goal(NonLocals, Goal0, Goal, !HI).
hhf__goal_expr(_, _, disj(Goals0), disj(Goals), !HI) :-
	list__map_foldl(hhf__goal_use_own_nonlocals, Goals0, Goals, !HI).
hhf__goal_expr(NonLocals, _, not(Goal0), not(Goal), !HI) :-
	hhf__goal(NonLocals, Goal0, Goal, !HI).
hhf__goal_expr(NonLocals, _, if_then_else(Vs, Cond0, Then0, Else0),
		if_then_else(Vs, Cond, Then, Else), !HI) :-
	hhf__goal(NonLocals, Cond0, Cond, !HI),
	Then0 = ThenExpr0 - ThenInfo,
	goal_info_get_nonlocals(ThenInfo, ThenNonLocals),
	hhf__goal_expr(ThenNonLocals, ThenInfo, ThenExpr0, ThenExpr, !HI),
	Then = ThenExpr - ThenInfo,
	Else0 = ElseExpr0 - ElseInfo,
	goal_info_get_nonlocals(ElseInfo, ElseNonLocals),
	hhf__goal_expr(ElseNonLocals, ElseInfo, ElseExpr0, ElseExpr, !HI),
	Else = ElseExpr - ElseInfo.
hhf__goal_expr(NonLocals, _, par_conj(Goals0), par_conj(Goals), !HI) :-
	list__map_foldl(hhf__goal(NonLocals), Goals0, Goals, !HI).
hhf__goal_expr(NonLocals, GoalInfo, unify(Var, RHS, Mode, Unif, Context),
		GoalExpr, !HI) :-
	hhf__unify(RHS, NonLocals, GoalInfo, Var, Mode, Unif, Context,
		GoalExpr, !HI).

:- pred hhf__unify(unify_rhs::in, set(prog_var)::in, hlds_goal_info::in,
	prog_var::in, unify_mode::in, unification::in, unify_context::in,
	hlds_goal_expr::out, hhf_info::in, hhf_info::out) is det.

hhf__unify(var(Y), _, _, X, Mode, Unif, Context, GoalExpr, !HI) :-
	GoalExpr = unify(X, var(Y), Mode, Unif, Context).
hhf__unify(lambda_goal(A,B,C,D,E,F,G,H,LambdaGoal0), NonLocals, _, X, Mode,
		Unif, Context, GoalExpr, !HI) :-
	hhf__goal(NonLocals, LambdaGoal0, LambdaGoal, !HI),
	GoalExpr = unify(X, lambda_goal(A,B,C,D,E,F,G,H,LambdaGoal), Mode,
		Unif, Context).
hhf__unify(functor(ConsId0, IsExistConstruct, ArgsA), NonLocals, GoalInfo0,
		X, Mode, Unif, Context, GoalExpr, !HI) :-
	TypeOfX = !.HI ^ vartypes ^ det_elem(X),
	qualify_cons_id(TypeOfX, ArgsA, ConsId0, _, ConsId),
	InstGraph0 = !.HI ^ inst_graph,
	map__lookup(InstGraph0, X, node(Functors0, MaybeParent)),
	( map__search(Functors0, ConsId, ArgsB) ->
		hhf__make_unifications(ArgsA, ArgsB, GoalInfo0, Mode, Unif,
			Context, Unifications),
		Args = ArgsB
	;
		hhf__add_unifications(ArgsA, NonLocals, GoalInfo0, Mode, Unif,
			Context, Args, Unifications, !HI),
		InstGraph1 = !.HI ^ inst_graph,
		map__det_insert(Functors0, ConsId, Args, Functors),
		map__det_update(InstGraph1, X, node(Functors, MaybeParent),
			InstGraph2),
		list__foldl(inst_graph__set_parent(X), Args, InstGraph2,
			InstGraph),
		!:HI = !.HI ^ inst_graph := InstGraph
	),
	goal_info_get_nonlocals(GoalInfo0, GINonlocals0),
	GINonlocals = GINonlocals0 `set__union` list_to_set(Args),
	goal_info_set_nonlocals(GoalInfo0, GINonlocals, GoalInfo),
	UnifyGoal = unify(X, functor(ConsId, IsExistConstruct, Args),
		Mode, Unif, Context) - GoalInfo,
	GoalExpr = conj([UnifyGoal | Unifications]).

:- pred hhf__make_unifications(list(prog_var)::in, list(prog_var)::in,
	hlds_goal_info::in, unify_mode::in, unification::in, unify_context::in,
	hlds_goals::out) is det.

hhf__make_unifications([], [], _, _, _, _, []).
hhf__make_unifications([_|_], [], _, _, _, _, _) :-
	error("hhf_make_unifications: length mismatch").
hhf__make_unifications([], [_|_], _, _, _, _, _) :-
	error("hhf_make_unifications: length mismatch").
hhf__make_unifications([A | As], [B | Bs], GI0, M, U, C,
		[unify(A, var(B), M, U, C) - GI | Us]) :-
	goal_info_get_nonlocals(GI0, GINonlocals0),
	GINonlocals = GINonlocals0 `set__insert` A `set__insert` B,
	goal_info_set_nonlocals(GI0, GINonlocals, GI),
	hhf__make_unifications(As, Bs, GI0, M, U, C, Us).

:- pred hhf__add_unifications(list(prog_var)::in, set(prog_var)::in,
	hlds_goal_info::in, unify_mode::in, unification::in, unify_context::in,
	list(prog_var)::out, hlds_goals::out, hhf_info::in, hhf_info::out)
	is det.

hhf__add_unifications([], _, _, _, _, _, [], [], !HI).
hhf__add_unifications([A | As], NonLocals, GI0, M, U, C, [V | Vs], Goals,
		!HI) :-
	hhf__add_unifications(As, NonLocals, GI0, M, U, C, Vs, Goals0, !HI),
	InstGraph0 = !.HI ^ inst_graph,
	(
		( 
			map__lookup(InstGraph0, A, Node),
			Node = node(_, parent(_))
		;
			A `set__member` NonLocals
		)
	->
		VarSet0 = !.HI ^ varset,
		VarTypes0 = !.HI ^ vartypes,
		varset__new_var(VarSet0, V, VarSet),
		map__lookup(VarTypes0, A, Type),
		map__det_insert(VarTypes0, V, Type, VarTypes),
		map__init(Empty),
		map__det_insert(InstGraph0, V, node(Empty, top_level),
			InstGraph),
		!:HI = !.HI ^ varset := VarSet,
		!:HI = !.HI ^ vartypes := VarTypes,
		!:HI = !.HI ^ inst_graph := InstGraph,
		goal_info_get_nonlocals(GI0, GINonlocals0),
		GINonlocals = GINonlocals0 `set__insert` V,
		goal_info_set_nonlocals(GI0, GINonlocals, GI),
		Goals = [unify(A, var(V), M, U, C) - GI | Goals0]
	;
		V = A,
		Goals = Goals0
	).

:- pred flatten_conj(hlds_goals::in, hlds_goals::out) is det.

flatten_conj([], []).
flatten_conj([Goal | Goals0], Goals) :-
	flatten_conj(Goals0, Goals1),
	( Goal = conj(SubGoals) - _ ->
		list__append(SubGoals, Goals1, Goals)
	;
		Goals = [Goal | Goals1]
	).

:- pred complete_inst_graph(module_info::in, hhf_info::in, hhf_info::out)
	is det.

complete_inst_graph(ModuleInfo, !HI) :-
	InstGraph0 = !.HI ^ inst_graph,
	map__keys(InstGraph0, Vars),
	list__foldl(complete_inst_graph_node(ModuleInfo, Vars), Vars, !HI).

:- pred complete_inst_graph_node(module_info::in, list(prog_var)::in,
	prog_var::in, hhf_info::in, hhf_info::out) is det.

complete_inst_graph_node(ModuleInfo, BaseVars, Var, !HI) :-
	VarTypes0 = !.HI ^ vartypes,
	(
		map__search(VarTypes0, Var, Type),
		type_constructors(Type, ModuleInfo, Constructors),
		type_to_ctor_and_args(Type, TypeId, _)
	->
		list__foldl(
			maybe_add_cons_id(Var, ModuleInfo, BaseVars, TypeId),
			Constructors, !HI)
	;
		true
	).

:- pred maybe_add_cons_id(prog_var::in, module_info::in, list(prog_var)::in,
	type_ctor::in, constructor::in, hhf_info::in, hhf_info::out) is det.

maybe_add_cons_id(Var, ModuleInfo, BaseVars, TypeId, Ctor, !HI) :-
	Ctor = ctor(_, _, Name, Args),
	ConsId = make_cons_id(Name, Args, TypeId),
	map__lookup(!.HI ^ inst_graph, Var, node(Functors0, MaybeParent)),
	( map__contains(Functors0, ConsId) ->
	    	true
	;
		list__map_foldl(add_cons_id(Var, ModuleInfo, BaseVars),
			Args, NewVars, !HI),
		map__det_insert(Functors0, ConsId, NewVars, Functors),
		!:HI = !.HI ^ inst_graph :=
			map__det_update(!.HI ^ inst_graph, Var,
				node(Functors, MaybeParent))
	).

:- pred add_cons_id(prog_var::in, module_info::in, list(prog_var)::in,
	constructor_arg::in, prog_var::out, hhf_info::in, hhf_info::out)
	is det.

add_cons_id(Var, ModuleInfo, BaseVars, Arg, NewVar, !HI) :-
	Arg = _ - ArgType,
	!.HI = hhf_info(InstGraph0, VarSet0, VarTypes0),
	(
		find_var_with_type(Var, ArgType, InstGraph0, VarTypes0,
			BaseVars, NewVar0)
	->
		NewVar = NewVar0
	;
		varset__new_var(VarSet0, NewVar, VarSet),
		map__det_insert(VarTypes0, NewVar, ArgType, VarTypes),
		map__init(Empty),
		map__det_insert(InstGraph0, NewVar, node(Empty, parent(Var)),
			InstGraph),
		!:HI = hhf_info(InstGraph, VarSet, VarTypes),
		complete_inst_graph_node(ModuleInfo, BaseVars, NewVar, !HI)
	).

:- pred find_var_with_type(prog_var::in, (type)::in, inst_graph::in,
	vartypes::in, list(prog_var)::in, prog_var::out) is semidet.

find_var_with_type(Var0, Type, InstGraph, VarTypes, BaseVars, Var) :-
	(
		map__search(VarTypes, Var0, Type0),
		same_type(Type0, Type)
	->
		Var = Var0
	;
		map__lookup(InstGraph, Var0, node(_, parent(Var1))),
		\+ Var1 `list__member` BaseVars,
		find_var_with_type(Var1, Type, InstGraph, VarTypes, BaseVars,
			Var)
	).

:- pred same_type((type)::in, (type)::in) is semidet.

same_type(term__variable(_), term__variable(_)).
same_type(term__functor(Const, ArgsA, _), term__functor(Const, ArgsB, _)) :-
	list__same_length(ArgsA, ArgsB),
	all [A, B] (
		corresponding_members(ArgsA, ArgsB, A, B)
	=>
		same_type(A, B)
	).

%------------------------------------------------------------------------%

% 	% Add the information from the procedure's mode declaration
% 	% to the inst_graph.
% :- pred hhf__process_proc(module_info::in, list(prog_var)::in, proc_id::in,
% 	proc_info::in, inst_graph::out, prog_varset::out) is det.
% 
% hhf__process_proc(ModuleInfo, HeadVars, _ProcId, ProcInfo, Info0, Info) :-
% 	proc_info_argmodes(ProcInfo, ArgModes),
% 
% 	mode_list_get_initial_insts(ArgModes, ModuleInfo, InstsI),
% 	assoc_list__from_corresponding_lists(HeadVars, InstsI, VarInstsI),
% 	list__foldl(hhf__process_arg(ModuleInfo), VarInstsI, Info0, Info),
% 
% 	mode_list_get_final_insts(ArgModes, ModuleInfo, InstsF),
% 	assoc_list__from_corresponding_lists(HeadVars, InstsF, VarInstsF),
% 	list__foldl(hhf__process_arg(ModuleInfo), VarInstsF, Info0, Info).
% 
% :- pred hhf__process_arg(module_info::in, pair(prog_var, inst)::in,
% 		inst_graph_info::in, inst_graph_info::out) is det.
% 
% hhf__process_arg(ModuleInfo, Var - Inst, Info0, Info) :-
% 	map__init(Seen0),
% 	hhf__process_arg_inst(ModuleInfo, Var, Seen0, Inst, Info0, Info).
% 
% :- pred hhf__process_arg_inst(module_info::in, prog_var::in,
% 		map(inst_name, prog_var)::in, inst::in, inst_graph_info::in,
% 		inst_graph_info::out) is det.
% 
% hhf__process_arg_inst(ModuleInfo, Var, Seen0, Inst0, Info0, Info) :-
% 	( Inst0 = defined_inst(InstName) ->
% 		map__det_insert(Seen0, InstName, Var, Seen),
% 		inst_lookup(ModuleInfo, InstName, Inst),
% 		hhf__process_arg_inst(Inst, ModuleInfo, Var, Seen, Info0, Info)
% 	; Inst0 = bound(_, BoundInsts) ->
% 		list__foldl(hhf__process_bound_inst(ModuleInfo, Var, Seen0),
% 			BoundInts, Info0, Info)
% 	;
% 		Info = Info0
% 	).
% 
% :- pred hhf__process_bound_inst(module_info::in, prog_var::in,
% 		map(inst_name, prog_var)::in, bound_inst::in,
% 		inst_graph_info::in, inst_graph_info::out) is det.
