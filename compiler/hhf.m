%-----------------------------------------------------------------------------%
% Copyright (C) 2001 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
:- module hhf.

% Convert superhomogeneous form to hyperhomogeneous form and output an
% inst graph for the predicate.
% 
:- interface.

:- import_module hlds_pred, hlds_module, inst_graph.
:- import_module io.

:- pred hhf__process_pred(pred_id::in, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

:- pred hhf__process_clauses_info(module_info::in, clauses_info::in,
		clauses_info::out, inst_graph::out) is det.

:- implementation.

:- import_module prog_data, hlds_data, hlds_goal, type_util, prog_util.
:- import_module passes_aux, quantification.
:- import_module term, varset, map, list, set, std_util, require, bool.

hhf__process_pred(PredId, ModuleInfo0, ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo0, PredId, PredInfo0) },

	( { pred_info_is_imported(PredInfo0) } ->
		% AAA
		% { PredInfo2 = PredInfo0 }
		{ pred_info_clauses_info(PredInfo0, ClausesInfo) },
		{ clauses_info_headvars(ClausesInfo, HeadVars) },
		{ clauses_info_varset(ClausesInfo, VarSet) },
		{ IGI0 = PredInfo0^inst_graph_info },
		{ inst_graph__init(HeadVars, InstGraph) },
		{ IGI1 = IGI0^implementation_inst_graph := InstGraph },
		{ IGI2 = IGI1^interface_inst_graph := InstGraph },
		{ IGI3 = IGI2^interface_vars := HeadVars },
		{ IGI4 = IGI3^interface_varset := VarSet },
		{ PredInfo2 = PredInfo0^inst_graph_info := IGI4 }
	;
		write_pred_progress_message(
			"% Calculating HHF and inst graph for ",
			PredId, ModuleInfo0),

		{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
		{ hhf__process_clauses_info(ModuleInfo0, ClausesInfo0,
			ClausesInfo, ImplementationInstGraph) },
		{ pred_info_set_clauses_info(PredInfo0, ClausesInfo,
			PredInfo1) },
		{ IGI0 = PredInfo1^inst_graph_info },
		{ IGI1 = IGI0^implementation_inst_graph :=
				ImplementationInstGraph },

		% AAA only for non-imported preds with no mode decls.
		{ clauses_info_headvars(ClausesInfo, HeadVars) },
		{ clauses_info_varset(ClausesInfo, VarSet) },
		{ IGI2 = IGI1^interface_inst_graph := ImplementationInstGraph },
		{ solutions((pred(V::out) is nondet :-
				list__member(V0, HeadVars),
				inst_graph__reachable(ImplementationInstGraph,
				V0, V)
			), InterfaceVars) },
		{ IGI3 = IGI2^interface_vars := InterfaceVars },
		{ IGI = IGI3^interface_varset := VarSet },

		{ PredInfo2 = PredInfo1^inst_graph_info := IGI }
	),

	/*
	{ pred_info_get_markers(PredInfo2, Markers) },
	{ check_marker(Markers, infer_modes) ->
		% No mode declarations.  If not imported, use implementation
		% inst_graph.
		% ...
	;
		pred_info_clauses_info(PredInfo2, ClausesInfo2),
		clauses_info_headvars(ClausesInfo2, HeadVars),
		clauses_info_varset(ClausesInfo2, VarSet),
		inst_graph__init(HeadVars, InterfaceInstGraph),
		InstGraphInfo0 = ( (PredInfo2^inst_graph_info)
			^interface_inst_graph := InterfaceInstGraph )
			^interface_varset := VarSet,
		map__foldl(hhf__process_proc(ModuleInfo0, HeadVars),
			Procedures, InstGraphInfo0, InstGraphInfo1),

		% Calculate interface vars.
		solutions((pred(V::out) is nondet :-
				list__member(V0, HeadVars),
				inst_graph__reachable(InstGraph, V0, V)
			), InterfaceVars),
		InstGraphInfo = InstGraphInfo1^interface_vars := InterfaceVars,

		PredInfo = PredInfo2^inst_graph_info := InstGraphInfo
	},
	*/
	{ PredInfo = PredInfo2 }, % AAA
	{ module_info_set_pred_info(ModuleInfo0, PredId, PredInfo,
		ModuleInfo) }.

hhf__process_clauses_info(ModuleInfo, ClausesInfo0, ClausesInfo, InstGraph) :-
	clauses_info_varset(ClausesInfo0, VarSet0),
	clauses_info_vartypes(ClausesInfo0, VarTypes0),
	map__keys(VarTypes0, Vars0),
	inst_graph__init(Vars0, InstGraph0),
	Info0 = hhf_info(InstGraph0, VarSet0, VarTypes0),

	clauses_info_headvars(ClausesInfo0, HeadVars),
	clauses_info_clauses(ClausesInfo0, Clauses0),

	list__map_foldl(hhf__process_clause(HeadVars),
		Clauses0, Clauses, Info0, Info1),

	clauses_info_set_clauses(ClausesInfo0, Clauses, ClausesInfo1),

	complete_inst_graph(ModuleInfo, Info1, Info),

	Info = hhf_info(InstGraph, VarSet, VarTypes),

	% 	XXX do we need this (it slows things down a lot (i.e.  uses 50%
	% 	of the runtime).
	% varset__vars(VarSet1, Vars1),
	% varset__ensure_unique_names(Vars1, "_", VarSet1, VarSet),

	clauses_info_set_varset(ClausesInfo1, VarSet, ClausesInfo2),
	clauses_info_set_vartypes(ClausesInfo2, VarTypes, ClausesInfo).


:- type hhf_info
	--->	hhf_info(
			inst_graph	:: inst_graph,
			varset		:: prog_varset,
			vartypes	:: vartypes
		).

:- pred hhf__process_clause(list(prog_var), clause, clause,
		hhf_info, hhf_info).
:- mode hhf__process_clause(in, in, out, in, out) is det.

hhf__process_clause(HeadVars, clause(ProcIds, Goal0, Context),
		clause(ProcIds, Goal, Context)) -->
	{ Goal0 = _ - GoalInfo0 },
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },

	hhf__goal(NonLocals, Goal0, Goal1),

	VarSet1 =^ varset,
	VarTypes1 =^ vartypes,
	{ implicitly_quantify_clause_body(HeadVars, Goal1, VarSet1, VarTypes1,
		Goal, VarSet, VarTypes, _Warnings) },
	^varset := VarSet,
	^vartypes := VarTypes.

:- pred hhf__goal(set(prog_var), hlds_goal, hlds_goal, hhf_info, hhf_info).
:- mode hhf__goal(in, in, out, in, out) is det.

hhf__goal(NonLocals, GoalExpr0 - GoalInfo, GoalExpr - GoalInfo) -->
	hhf__goal_expr(NonLocals, GoalInfo, GoalExpr0, GoalExpr).

:- pred hhf__goal_expr(set(prog_var), hlds_goal_info, hlds_goal_expr,
		hlds_goal_expr, hhf_info, hhf_info).
:- mode hhf__goal_expr(in, in, in, out, in, out) is det.

hhf__goal_expr(NonLocals, _, conj(Goals0), conj(Goals)) -->
	list__map_foldl(hhf__goal(NonLocals), Goals0, Goals1),
	{ flatten_conj(Goals1, Goals) }.
hhf__goal_expr(_, _, call(A, B, C, D, E, F), call(A, B, C, D, E, F)) --> [].
hhf__goal_expr(_, _, generic_call(A, B, C, D), generic_call(A, B, C, D)) --> [].
hhf__goal_expr(_, _, switch(_, _, _, _), _) -->
	{ error("hhf_goal_expr: found switch") }.
hhf__goal_expr(_, _, pragma_foreign_code(A,B,C,D,E,F,G),
		pragma_foreign_code(A,B,C,D,E,F,G)) --> [].
hhf__goal_expr(_, _, bi_implication(_, _), _) -->
	{ error("hhf_goal_expr: found bi_implication") }.
hhf__goal_expr(NonLocals, _, some(A, B, Goal0), some(A, B, Goal)) -->
	hhf__goal(NonLocals, Goal0, Goal).
hhf__goal_expr(_, _, disj(Goals0, SM), disj(Goals, SM)) -->
	list__map_foldl((pred((E0 - I)::in, (E - I)::out, in, out) is det -->
			{ goal_info_get_nonlocals(I, NonLocals) },
			hhf__goal_expr(NonLocals, I, E0, E)),
		Goals0, Goals).
hhf__goal_expr(NonLocals, _, not(Goal0), not(Goal)) -->
	hhf__goal(NonLocals, Goal0, Goal).
hhf__goal_expr(NonLocals, _, if_then_else(Vs, Cond0, Then0, Else0, SM),
		if_then_else(Vs, Cond, Then, Else, SM)) -->
	hhf__goal(NonLocals, Cond0, Cond),
	{ Then0 = ThenExpr0 - ThenInfo },
	{ goal_info_get_nonlocals(ThenInfo, ThenNonLocals) },
	hhf__goal_expr(ThenNonLocals, ThenInfo, ThenExpr0, ThenExpr),
	{ Then = ThenExpr - ThenInfo },
	{ Else0 = ElseExpr0 - ElseInfo },
	{ goal_info_get_nonlocals(ElseInfo, ElseNonLocals) },
	hhf__goal_expr(ElseNonLocals, ElseInfo, ElseExpr0, ElseExpr),
	{ Else = ElseExpr - ElseInfo }.
hhf__goal_expr(NonLocals, _, par_conj(Goals0, SM), par_conj(Goals, SM)) -->
	list__map_foldl(hhf__goal(NonLocals), Goals0, Goals).
hhf__goal_expr(NonLocals, GoalInfo, unify(Var, RHS, Mode, Unif, Context),
		GoalExpr) -->
	hhf__unify(RHS, NonLocals, GoalInfo, Var, Mode, Unif, Context,
		GoalExpr).

:- pred hhf__unify(unify_rhs, set(prog_var), hlds_goal_info, prog_var,
		unify_mode, unification, unify_context, hlds_goal_expr,
		hhf_info, hhf_info).
:- mode hhf__unify(in, in, in, in, in, in, in, out, in, out) is det.

hhf__unify(var(Y), _, _, X, Mode, Unif, Context, GoalExpr) -->
	{ GoalExpr = unify(X, var(Y), Mode, Unif, Context) }.
hhf__unify(lambda_goal(A,B,C,D,E,F,G,LambdaGoal0), NonLocals, _, X, Mode,
		Unif, Context, GoalExpr) -->
	hhf__goal(NonLocals, LambdaGoal0, LambdaGoal),
	{ GoalExpr = unify(X, lambda_goal(A,B,C,D,E,F,G,LambdaGoal), Mode,
			Unif, Context) }.
hhf__unify(functor(ConsId, ArgsA), NonLocals, GoalInfo, X, Mode, Unif, Context,
		GoalExpr) -->
	InstGraph0 =^ inst_graph,
	{ map__lookup(InstGraph0, X, node(Functors0, MaybeParent)) },
	( { map__search(Functors0, ConsId, ArgsB) } ->
		{ hhf__make_unifications(ArgsA, ArgsB, GoalInfo, Mode, Unif,
			Context, Unifications) },
		{ Args = ArgsB }
	;
		hhf__add_unifications(ArgsA, NonLocals, GoalInfo, Mode, Unif,
			Context, Args, Unifications),
		InstGraph1 =^ inst_graph,
		{ map__det_insert(Functors0, ConsId, Args, Functors) },
		{ map__det_update(InstGraph1, X, node(Functors, MaybeParent),
			InstGraph2) },
		{ list__foldl(inst_graph__set_parent(X), Args, InstGraph2,
			InstGraph) },
		^inst_graph := InstGraph
	),
	{ UnifyGoal = unify(X, functor(ConsId, Args), Mode, Unif, Context)
		- GoalInfo },
	{ GoalExpr = conj([UnifyGoal | Unifications]) }.

:- pred hhf__make_unifications(list(prog_var), list(prog_var), hlds_goal_info,
		unify_mode, unification, unify_context, hlds_goals).
:- mode hhf__make_unifications(in, in, in, in, in, in, out) is det.

hhf__make_unifications([], [], _, _, _, _, []).
hhf__make_unifications([_|_], [], _, _, _, _, _) :-
	error("hhf_make_unifications: length mismatch").
hhf__make_unifications([], [_|_], _, _, _, _, _) :-
	error("hhf_make_unifications: length mismatch").
hhf__make_unifications([A | As], [B | Bs], GI, M, U, C,
		[unify(A, var(B), M, U, C) - GI | Us]) :-
	hhf__make_unifications(As, Bs, GI, M, U, C, Us).

:- pred hhf__add_unifications(list(prog_var), set(prog_var), hlds_goal_info,
		unify_mode, unification, unify_context, list(prog_var),
		hlds_goals, hhf_info, hhf_info).
:- mode hhf__add_unifications(in, in, in, in, in, in, out, out, in, out) is det.

hhf__add_unifications([], _, _, _, _, _, [], []) --> [].
hhf__add_unifications([A | As], NonLocals, GI, M, U, C, [V | Vs], Goals) -->
	hhf__add_unifications(As, NonLocals, GI, M, U, C, Vs, Goals0),
	InstGraph0 =^ inst_graph,
	(
		{ 
			map__lookup(InstGraph0, A, Node),
			Node = node(_, parent(_))
		;
			A `member` NonLocals
		}
	->
		VarSet0 =^ varset,
		VarTypes0 =^ vartypes,
		{ varset__new_var(VarSet0, V, VarSet) },
		{ map__lookup(VarTypes0, A, Type) },
		{ map__det_insert(VarTypes0, V, Type, VarTypes) },
		{ map__init(Empty) },
		{ map__det_insert(InstGraph0, V, node(Empty, top_level),
			InstGraph) },
		^varset := VarSet,
		^vartypes := VarTypes,
		^inst_graph := InstGraph,
		{ Goals = [unify(A, var(V), M, U, C) - GI | Goals0] }
	;
		{ V = A },
		{ Goals = Goals0 }
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

complete_inst_graph(ModuleInfo) -->
	=(hhf_info(InstGraph0,_,_)),
	{ map__keys(InstGraph0, Vars) },
	list__foldl(complete_inst_graph_node(ModuleInfo), Vars).

:- pred complete_inst_graph_node(module_info::in, prog_var::in, hhf_info::in,
		hhf_info::out) is det.

complete_inst_graph_node(ModuleInfo, Var, Info0, Info) :-
	Info0 = hhf_info(_InstGraph0, _VarSet0, VarTypes0),
	map__lookup(VarTypes0, Var, Type),
	(
		type_constructors(Type, ModuleInfo, Constructors),
		type_to_type_id(Type, TypeId, _)
	->
		list__foldl(maybe_add_cons_id(Var, ModuleInfo, TypeId),
			Constructors, Info0, Info)
	;
		Info = Info0
	).

:- pred maybe_add_cons_id(prog_var::in, module_info::in, type_id::in,
		constructor::in, hhf_info::in, hhf_info::out) is det.

maybe_add_cons_id(Var, ModuleInfo, TypeId, Ctor, Info0, Info) :-
	Ctor = ctor(_, _, QualifiedName, Args),
	unqualify_name(QualifiedName, Name),
	TypeId = QualifiedTypeIdName - Arity,
	unqualify_name(QualifiedTypeIdName, TypeIdName),
	make_cons_id(unqualified(Name), Args, unqualified(TypeIdName) - Arity,
		ConsId),
	map__lookup(Info0^inst_graph, Var, node(Functors0, MaybeParent)),
	( map__contains(Functors0, ConsId) ->
	    Info = Info0
	;
	    list__map_foldl(
		(pred(Arg::in, NewVar::out, I0::in, I::out) is det :-
		    Arg = _ - ArgType,
		    I0 = hhf_info(IG0, VS0, VT0),
		    ( find_var_with_type(Var, ArgType, IG0, VT0, NewVar0) ->
			NewVar = NewVar0,
			I = I0
		    ;
			varset__new_var(VS0, NewVar, VS),
			map__det_insert(VT0, NewVar, ArgType, VT),
			map__init(Empty),
			map__det_insert(IG0, NewVar, node(Empty, parent(Var)),
				IG),
			I1 = hhf_info(IG, VS, VT),
			complete_inst_graph_node(ModuleInfo, NewVar, I1, I)
		    )
		), Args, NewVars, Info0, Info1),
	    map__det_insert(Functors0, ConsId, NewVars, Functors),
	    Info = Info1^inst_graph := map__det_update(Info1^inst_graph,
	    		Var, node(Functors, MaybeParent))
	).

:- pred find_var_with_type(prog_var::in, (type)::in, inst_graph::in,
		vartypes::in, prog_var::out) is semidet.

find_var_with_type(Var0, Type, InstGraph, VarTypes, Var) :-
	map__lookup(VarTypes, Var0, Type0),
	( same_type(Type0, Type) ->
		Var = Var0
	;
		map__lookup(InstGraph, Var0, node(_, parent(Var1))),
		find_var_with_type(Var1, Type, InstGraph, VarTypes, Var)
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

/*
	% Add the information from the procedure's mode declaration
	% to the inst_graph.
:- pred hhf__process_proc(module_info::in, list(prog_var)::in, proc_id::in,
	proc_info::in, inst_graph::out, prog_varset::out) is det.

hhf__process_proc(ModuleInfo, HeadVars, _ProcId, ProcInfo, Info0, Info) :-
	proc_info_argmodes(ProcInfo, ArgModes),

	mode_list_get_initial_insts(ArgModes, ModuleInfo, InstsI),
	assoc_list__from_corresponding_lists(HeadVars, InstsI, VarInstsI),
	list__foldl(hhf__process_arg(ModuleInfo), VarInstsI, Info0, Info),

	mode_list_get_final_insts(ArgModes, ModuleInfo, InstsF),
	assoc_list__from_corresponding_lists(HeadVars, InstsF, VarInstsF),
	list__foldl(hhf__process_arg(ModuleInfo), VarInstsF, Info0, Info).

:- pred hhf__process_arg(module_info::in, pair(prog_var, inst)::in,
		inst_graph_info::in, inst_graph_info::out) is det.

hhf__process_arg(ModuleInfo, Var - Inst, Info0, Info) :-
	map__init(Seen0),
	hhf__process_arg_inst(ModuleInfo, Var, Seen0, Inst, Info0, Info).

:- pred hhf__process_arg_inst(module_info::in, prog_var::in,
		map(inst_name, prog_var)::in, inst::in, inst_graph_info::in,
		inst_graph_info::out) is det.

hhf__process_arg_inst(ModuleInfo, Var, Seen0, Inst0, Info0, Info) :-
	( Inst0 = defined_inst(InstName) ->
		map__det_insert(Seen0, InstName, Var, Seen),
		inst_lookup(ModuleInfo, InstName, Inst),
		hhf__process_arg_inst(Inst, ModuleInfo, Var, Seen, Info0, Info)
	; Inst0 = bound(_, BoundInsts) ->
		list__foldl(hhf__process_bound_inst(ModuleInfo, Var, Seen0),
			BoundInts, Info0, Info)
	;
		Info = Info0
	).

:- pred hhf__process_bound_inst(module_info::in, prog_var::in,
		map(inst_name, prog_var)::in, bound_inst::in,
		inst_graph_info::in, inst_graph_info::out) is det.

hhf__process_bound_inst(ModuleInfo, Var, 

*/
