%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_ordering.m.
% Main author: dmo.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_ordering.
:- interface.

:- import_module check_hlds.mode_constraint_robdd.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module list.
:- import_module map.

:- type pred_constraint_info
    --->    pci(
                pci_mode_constraint :: mode_constraint,
                pci_mci             :: mode_constraint_info
            ).

:- type pred_constraint_map == map(pred_id, pred_constraint_info).

    % Given a top-down list of predicate SCCs, attempt to schedule goals
    % for mode of each predicate, and determine which modes are needed
    % for each predicate.
    %
:- pred mode_ordering(pred_constraint_map::in, list(list(pred_id))::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module hlds.hlds_goal.
:- import_module hlds.inst_graph.
:- import_module mode_robdd.
:- import_module mode_robdd.tfeirn.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module digraph.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module stack.

mode_ordering(PredConstraintMap, SCCs, !ModuleInfo, !IO) :-
    list.foldl(mode_ordering_scc(PredConstraintMap), SCCs, !ModuleInfo),
    report_ordering_mode_errors(!.ModuleInfo, !IO).

:- pred mode_ordering_scc(pred_constraint_map::in, list(pred_id)::in,
    module_info::in, module_info::out) is det.

mode_ordering_scc(PredConstraintMap, SCC, !ModuleInfo) :-
    % XXX This call to copy_clauses_to_procs_for_preds_in_module_info
    % should *not* be necessary.
    copy_clauses_to_nonmethod_procs_for_preds_in_module_info(SCC, !ModuleInfo),
    list.foldl(mode_ordering_pred(PredConstraintMap, SCC), SCC, !ModuleInfo).

:- pred mode_ordering_pred(pred_constraint_map::in, list(pred_id)::in,
    pred_id::in, module_info::in, module_info::out) is det.

mode_ordering_pred(PredConstraintMap, _SCC, PredId, !ModuleInfo) :-
    % XXX Mode inference NYI.
    RequestedProcsMap0 = map.init,

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    lookup_pred_constraint(PredConstraintMap, PredId, ModeConstraint0, MCI),
    ( if pred_info_infer_modes(PredInfo0) then
        ( if map.search(RequestedProcsMap0, PredId, RequestedProcs) then
            list.foldl(
                mode_ordering_infer_proc(!.ModuleInfo, PredConstraintMap,
                    PredId, MCI, ModeConstraint0),
                RequestedProcs, PredInfo0, PredInfo)
        else
            % XXX Maybe we should remove the predicate from the
            % module_info here since it is not used.
            PredInfo = PredInfo0
        )
    else
        ProcIds = pred_info_all_non_imported_procids(PredInfo0),
        list.foldl(
            mode_ordering_check_proc(!.ModuleInfo, PredConstraintMap,
                PredId, MCI, ModeConstraint0),
            ProcIds, PredInfo0, PredInfo)
    ),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred mode_ordering_infer_proc(module_info::in, pred_constraint_map::in,
    pred_id::in, mode_constraint_info::in,
    mode_constraint::in, mode_constraint::in,
    pred_info::in, pred_info::out) is det.

mode_ordering_infer_proc(ModuleInfo, PredConstraintMap, PredId, MCI,
        Constraint0, ModeDeclConstraint, !PredInfo) :-
    pred_info_create_proc_info_for_mode_decl_constraint(ModeDeclConstraint,
        ProcId, !PredInfo),
    mode_ordering_check_proc(ModuleInfo, PredConstraintMap, PredId, MCI,
        Constraint0, ProcId, !PredInfo).

:- pred mode_ordering_check_proc(module_info::in, pred_constraint_map::in,
    pred_id::in, mode_constraint_info::in, mode_constraint::in, proc_id::in,
    pred_info::in, pred_info::out) is det.

mode_ordering_check_proc(ModuleInfo, PredConstraintMap, PredId, MCI,
        Constraint0, ProcId, !PredInfo) :-
    pred_info_proc_info(!.PredInfo, ProcId, ProcInfo0),
    proc_info_head_modes_constraint(ProcInfo0, ModeDeclConstraint),
    Constraint = Constraint0 * ModeDeclConstraint,
    pred_info_get_inst_graph_info(!.PredInfo, InstGraphInfo),
    InstGraph = InstGraphInfo ^ implementation_inst_graph,
    mode_ordering_proc(ModuleInfo, PredConstraintMap, PredId, MCI, InstGraph,
        Constraint, ProcInfo0, ProcInfo),
    pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo).

    % Perform mode ordering for a procedure. The ModeConstraint must be
    % constrained to contain just the mode information for this procedure.
    %
:- pred mode_ordering_proc(module_info::in, pred_constraint_map::in,
    pred_id::in, mode_constraint_info::in, inst_graph::in,
    mode_constraint::in, proc_info::in, proc_info::out) is det.

mode_ordering_proc(ModuleInfo, PredConstraintMap, PredId, MCI, InstGraph,
        ModeConstraint, !ProcInfo) :-
    ProdVarsMap = atomic_prodvars_map(ModeConstraint, MCI),
    LambdaNesting0 = stack.init,
    get_forward_goal_path_map_for_pred(MCI, PredId, ForwardGoalPathMap),
    MOI0 = mode_ordering_info(InstGraph, ProdVarsMap, LambdaNesting0,
        ModuleInfo, PredConstraintMap, ForwardGoalPathMap),

    proc_info_get_goal(!.ProcInfo, Goal0),
    mode_order_goal(Goal0, Goal, MOI0, _MOI),
    proc_info_set_goal(Goal, !ProcInfo).

:- type mode_ordering_info
    --->    mode_ordering_info(
                moi_inst_graph          :: inst_graph,
                moi_prodvars_map        :: prodvars_map,
                moi_lambda_nesting      :: lambda_path,
                moi_module_info         :: module_info,
                moi_pred_constraint_map :: pred_constraint_map,
                moi_goal_path_map       :: goal_forward_path_map
            ).

:- pred enter_lambda_goal(goal_id::in,
    mode_ordering_info::in, mode_ordering_info::out) is det.

enter_lambda_goal(GoalId, !MOI) :-
    LambdaNesting0 = !.MOI ^ moi_lambda_nesting,
    !MOI ^ moi_lambda_nesting := stack.push(LambdaNesting0, GoalId).

:- pred leave_lambda_goal(mode_ordering_info::in, mode_ordering_info::out)
    is det.

leave_lambda_goal(!MOI) :-
    LambdaNesting0 = !.MOI ^ moi_lambda_nesting,
    stack.det_pop(_, LambdaNesting0, LambdaNesting),
    !MOI ^ moi_lambda_nesting := LambdaNesting.

:- pred mode_order_goal(hlds_goal::in, hlds_goal::out,
    mode_ordering_info::in, mode_ordering_info::out) is det.

mode_order_goal(Goal0, Goal, !MOI) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    mode_order_goal_2(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !MOI),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred mode_order_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    mode_ordering_info::in, mode_ordering_info::out) is det.

mode_order_goal_2(GoalExpr0, GoalExpr, !GoalInfo, !MOI) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            list.map_foldl(mode_order_goal, Goals0, Goals1, !MOI),
            ForwardGoalPathMap = !.MOI ^ moi_goal_path_map,
            mode_order_conj(ForwardGoalPathMap, Goals1, Goals),
            union_mode_vars_sets(Goals, !GoalInfo),
            ConsVars = !.GoalInfo ^ consuming_vars,
            !GoalInfo ^ consuming_vars :=
                ConsVars `set_of_var.difference` !.GoalInfo ^ producing_vars,
            NeedVars = !.GoalInfo ^ need_visible_vars,
            !GoalInfo ^ need_visible_vars :=
                NeedVars `set_of_var.difference` !.GoalInfo ^ make_visible_vars
        ;
            ConjType = parallel_conj,
            list.map_foldl(mode_order_goal, Goals0, Goals, !MOI),
            union_mode_vars_sets(Goals, !GoalInfo)
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = plain_call(PredId, _, Args, _, _, _),
        set_atomic_prod_vars(ProdVars, !GoalInfo, !MOI),
        MakeVisibleVars =
            set_of_var.list_to_set(Args) `set_of_var.intersect` ProdVars,

        find_matching_proc(PredId, Args, ProdVars, ProcId, ConsumingVars,
            !MOI),
        NeedVisibleVars =
            set_of_var.list_to_set(Args) `set_of_var.intersect` ConsumingVars,

        goal_info_set_consuming_vars(ConsumingVars, !GoalInfo),
        goal_info_set_make_visible_vars(MakeVisibleVars, !GoalInfo),
        goal_info_set_need_visible_vars(NeedVisibleVars, !GoalInfo),
        GoalExpr = GoalExpr0 ^ call_proc_id := ProcId
    ;
        GoalExpr0 = generic_call(_GenericCall0, _Args, _Modes0, _, _Det),
        unexpected($pred, "generic_call NYI")
    ;
        GoalExpr0 = switch(_Var, _CanFail0, _Cases0),
        unexpected($pred, "switch")
    ;
        GoalExpr0 = unify(VarA, RHS0, UnifyMode, Unification0, Context),
        set_atomic_prod_vars(ProdVars, !GoalInfo, !MOI),
        InstGraph = !.MOI ^ moi_inst_graph,
        (
            RHS0 = rhs_var(VarB),
            RHS = RHS0,
            ( if set_of_var.contains(ProdVars, VarA) then
                Unification = assign(VarA, VarB),
                MakeVisibleVars = set_of_var.make_singleton(VarA),
                NeedVisibleVars = set_of_var.make_singleton(VarB)
            else if set_of_var.contains(ProdVars, VarB) then
                Unification = assign(VarB, VarA),
                MakeVisibleVars = set_of_var.make_singleton(VarB),
                NeedVisibleVars = set_of_var.make_singleton(VarA)
            else
                Unification = simple_test(VarA, VarB),
                % XXX may be complicated unify -- need to check.
                MakeVisibleVars = set_of_var.init,
                NeedVisibleVars = set_of_var.list_to_set([VarA, VarB])
            ),
            ConsumingVarsList = solutions.solutions(
                ( pred(Var::out) is nondet :-
                    inst_graph.same_graph_corresponding_nodes(InstGraph,
                        VarA, VarB, VarC, VarD),
                    ( if set_of_var.contains(ProdVars, VarC) then
                        Var = VarD
                    else if set_of_var.contains(ProdVars, VarD) then
                        Var = VarC
                    else
                        fail
                    )
                )
            ),
            ConsumingVars = set_of_var.sorted_list_to_set(ConsumingVarsList)
        ;
            RHS0 = rhs_functor(_ConsId, _IsExistConstruct, ArgVars),
            RHS = RHS0,
            ( if set_of_var.contains(ProdVars, VarA) then
                % Unification = construct(VarA, ConsId, ArgVars,
                %   _UniModes, _HowTo, _CellUniq, _MaybeRLExprId),
                Unification = Unification0, % XXX
                ConsumingVars = set_of_var.init,
                MakeVisibleVars = set_of_var.list_to_set([VarA | ArgVars]),
                NeedVisibleVars = set_of_var.init
            else
                % Unification = deconstruct(VarA, ConsId, ArgVars,
                %   _UniModes, _CanFail, _CanCGC),
                Unification = Unification0, % XXX
                ConsumingVars = set_of_var.make_singleton(VarA),
                MakeVisibleVars = set_of_var.list_to_set(ArgVars),
                NeedVisibleVars = set_of_var.make_singleton(VarA)
            )
        ;
            % Unification = construct(VarA, _ConsId, _ArgVars,
            %   _UniModes, _HowTo, _CellUniq, _MaybeRLExprId),
            Unification = Unification0, % XXX
            RHS0 = rhs_lambda_goal(A, B, C, D, NonLocals, ArgVarsModes0,
                H, SubGoal0),
            ArgVarsModes = ArgVarsModes0, % XXX
            RHS = rhs_lambda_goal(A, B, C, D, NonLocals, ArgVarsModes,
                H, SubGoal),

            GoalId = goal_info_get_goal_id(!.GoalInfo),
            enter_lambda_goal(GoalId, !MOI),
            mode_order_goal(SubGoal0, SubGoal, !MOI),
            leave_lambda_goal(!MOI),

            ConsumingVarsList = solutions.solutions(
                inst_graph.reachable_from_list(InstGraph, NonLocals)),
            ConsumingVars = set_of_var.sorted_list_to_set(ConsumingVarsList),
            MakeVisibleVars = set_of_var.make_singleton(VarA),
            NeedVisibleVars = set_of_var.list_to_set(NonLocals)
        ),
        goal_info_set_consuming_vars(ConsumingVars, !GoalInfo),
        goal_info_set_make_visible_vars(MakeVisibleVars, !GoalInfo),
        goal_info_set_need_visible_vars(NeedVisibleVars, !GoalInfo),

        GoalExpr = unify(VarA, RHS, UnifyMode, Unification, Context)
    ;
        GoalExpr0 = disj(Goals0),
        list.map_foldl(mode_order_goal, Goals0, Goals, !MOI),
        mode_order_disj(Goals, !GoalInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = negation(SubGoal0),
        mode_order_goal(SubGoal0, SubGoal, !MOI),
        goal_info_copy_mode_var_sets(SubGoal ^ hg_info, !GoalInfo),
        GoalExpr = negation(SubGoal)
    ;
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        GoalExpr0 = scope(Reason, SubGoal0),
        mode_order_goal(SubGoal0, SubGoal, !MOI),
        goal_info_copy_mode_var_sets(SubGoal ^ hg_info, !GoalInfo),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(Locals, Cond0, Then0, Else0),
        mode_order_goal(Cond0, Cond, !MOI),
        mode_order_goal(Then0, Then, !MOI),
        mode_order_goal(Else0, Else, !MOI),
        % XXX Ned to make sure that Cond can be scheduled before Then and Else.

        union_mode_vars_sets([Cond, Then], !GoalInfo),
        ConsVars = !.GoalInfo ^ consuming_vars,
        !GoalInfo ^ consuming_vars :=
            ConsVars `set_of_var.difference` !.GoalInfo ^ producing_vars,
        NeedVars = !.GoalInfo ^ need_visible_vars,
        !GoalInfo ^ need_visible_vars :=
            NeedVars `set_of_var.difference` !.GoalInfo ^ make_visible_vars,

        combine_mode_vars_sets(Else ^ hg_info, !GoalInfo),
        GoalExpr = if_then_else(Locals, Cond, Then, Else)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        unexpected($pred, "pragma_foreign_code NYI")
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).
% mode_order_goal_2(Goal0, Goal, !GoalInfo, !MOI) :-
%     Goal0 = atomic_goal(GoalType, Outer, Inner, Vars, MainGoal0,
%         OrElseGoals0),
%     mode_order_goal(MainGoal0, MainGoal, !MOI),
%     list.map_foldl(mode_order_goal, OrElseGoals0, OrElseGoals, !MOI),
%     mode_order_disj(OrElseGoals, !GoalInfo),
%     Goal = atomic_goal(GoalType, Outer, Inner, Vars, MainGoal, OrElseGoals).

:- pred mode_order_disj(hlds_goals::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

mode_order_disj([], !GoalInfo).
mode_order_disj([hlds_goal(_, GI) | Goals], !GoalInfo) :-
    goal_info_copy_mode_var_sets(GI, !GoalInfo),
    list.foldl(mode_order_disj_2, Goals, !GoalInfo).

:- pred mode_order_disj_2(hlds_goal::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

mode_order_disj_2(hlds_goal(_, GI), !GoalInfo) :-
    combine_mode_vars_sets(GI, !GoalInfo).

:- pred combine_mode_vars_sets(hlds_goal_info::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

combine_mode_vars_sets(GI, !GoalInfo) :-
    ProdVars0 = !.GoalInfo ^ producing_vars,
    ConsumVars0 = !.GoalInfo ^ consuming_vars,
    MakeVisibleVars0 = !.GoalInfo ^ make_visible_vars,
    NeedVisibleVars0 = !.GoalInfo ^ need_visible_vars,

    !GoalInfo ^ producing_vars
        := ProdVars0 `set_of_var.intersect` GI ^ producing_vars,
    !GoalInfo ^ consuming_vars
        := ConsumVars0 `set_of_var.union` GI ^ consuming_vars,
    !GoalInfo ^ make_visible_vars
        := MakeVisibleVars0 `set_of_var.intersect` GI ^ make_visible_vars,
    !GoalInfo ^ need_visible_vars
        := NeedVisibleVars0 `set_of_var.union` GI ^ need_visible_vars.

:- pred union_mode_vars_sets(hlds_goals::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

union_mode_vars_sets(Goals, !GoalInfo) :-
    list.foldl(union_mode_vars_set, Goals, !GoalInfo).

:- pred union_mode_vars_set(hlds_goal::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

union_mode_vars_set(Goal, !GoalInfo) :-
    ProdVars0 = !.GoalInfo ^ producing_vars,
    ConsumVars0 = !.GoalInfo ^ consuming_vars,
    MakeVisibleVars0 = !.GoalInfo ^ make_visible_vars,
    NeedVisibleVars0 = !.GoalInfo ^ need_visible_vars,
    Goal = hlds_goal(_, GI),

    !GoalInfo ^ producing_vars
        := ProdVars0 `set_of_var.union` GI ^ producing_vars,
    !GoalInfo ^ consuming_vars
        := ConsumVars0 `union` GI ^ consuming_vars,
    !GoalInfo ^ make_visible_vars
        := MakeVisibleVars0 `set_of_var.union` GI ^ make_visible_vars,
    !GoalInfo ^ need_visible_vars
        := NeedVisibleVars0 `set_of_var.union` GI ^ need_visible_vars.

:- pred goal_info_copy_mode_var_sets(hlds_goal_info::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

goal_info_copy_mode_var_sets(GI, !GoalInfo) :-
    !GoalInfo ^ producing_vars := GI ^ producing_vars,
    !GoalInfo ^ consuming_vars := GI ^ consuming_vars,
    !GoalInfo ^ make_visible_vars := GI ^ make_visible_vars,
    !GoalInfo ^ need_visible_vars := GI ^ need_visible_vars.

:- pred mode_order_conj(goal_forward_path_map::in,
    hlds_goals::in, hlds_goals::out) is det.

mode_order_conj(ForwardGoalPathMap, Goals0, Goals) :-
    GoalMap = list.foldl(
        ( func(G, GM) = map.det_insert(GM, Index, G) :-
            G = hlds_goal(_, GI),
            GoalId = goal_info_get_goal_id(GI),
            map.lookup(ForwardGoalPathMap, GoalId, GoalPath),
            ( if
                goal_path_get_last(GoalPath, LastStep),
                LastStep = step_conj(Index0)
            then
                Index = Index0
            else
                unexpected($pred, "goal_path error")
            )
        ), Goals0, map.init),

    ProdMap =
        map.foldl(
            ( func(I, G, PM0) =
                list.foldl(
                    (func(V, PM1) = map.det_insert(PM1, V, I)),
                    set_of_var.to_sorted_list(G ^ hg_info ^ producing_vars),
                    PM0)
            ), GoalMap, map.init),

    MakeVisMap =
        map.foldl(
            ( func(I, G, MVM0) =
                list.foldl(
                    (func(V, MVM1) = map.set(MVM1, V, I)),
                    % XXX disjunction required!
                    set_of_var.to_sorted_list(G ^ hg_info ^ make_visible_vars),
                    MVM0)
            ), GoalMap, map.init),

    Graph = map.foldl(
        ( func(I, G, !.R) = !:R :-
            GI = G ^ hg_info,
            digraph.add_vertex(I, Key0, !R),
            !:R = list.foldl(
                (func(V, !.R1) = !:R1 :-
                    ( if Index1 = map.search(ProdMap, V) then
                        digraph.add_vertex(Index1, Key1, !R1),
                        digraph.add_edge(Key1, Key0, !R1)
                    else
                        true
                    )
                ), set_of_var.to_sorted_list(GI ^ consuming_vars), !.R),
            !:R = list.foldl(
                ( func(V, !.R2) = !:R2 :-
                    ( if Index2 = map.search(MakeVisMap, V) then
                        digraph.add_vertex(Index2, Key2, !R2),
                        digraph.add_edge(Key2, Key0, !R2)
                    else
                        true
                    )
                ), set_of_var.to_sorted_list(GI ^ need_visible_vars), !.R)
        ), GoalMap, digraph.init),

    ( if digraph.tsort(Graph, TSort) then
        Goals = map.apply_to_list(TSort, GoalMap)
    else
        % XXX Report a mode error for this.
        unexpected($pred, "tsort failed")
    ).

:- pred set_atomic_prod_vars(set_of_progvar::out,
    hlds_goal_info::in, hlds_goal_info::out,
    mode_ordering_info::in, mode_ordering_info::out) is det.

set_atomic_prod_vars(ProdVars, !GoalInfo, !MOI) :-
    LambdaNesting = !.MOI ^ moi_lambda_nesting,
    AtomicProdVars = !.MOI ^ moi_prodvars_map,
    GoalId = goal_info_get_goal_id(!.GoalInfo),
    ( if
        map.search(AtomicProdVars, stack.push(LambdaNesting, GoalId),
            ProdVars0)
    then
        ProdVars = ProdVars0
    else
        ProdVars = set_of_var.init
    ),
    goal_info_set_producing_vars(ProdVars, !GoalInfo).

:- pred pred_info_create_proc_info_for_mode_decl_constraint(
    mode_constraint::in, proc_id::out, pred_info::in, pred_info::out) is det.

pred_info_create_proc_info_for_mode_decl_constraint(_ModeDeclConstraint,
        ProcId, !PredInfo) :-
    ( if semidet_succeed then
        % XXX
        sorry($pred, "NYI")
    else
        % XXX keep det checker happy.
        ProcId = initial_proc_id
    ).

:- pred find_matching_proc(pred_id::in, list(prog_var)::in, set_of_progvar::in,
    proc_id::out, set_of_progvar::out,
    mode_ordering_info::in, mode_ordering_info::out) is det.

find_matching_proc(PredId, Args, ProdVars, ProcId, ConsumingVars, !MOI) :-
    ModuleInfo = !.MOI ^ moi_module_info,
    CallerInstGraph = !.MOI ^ moi_inst_graph,
    PredConstraintMap = !.MOI ^ moi_pred_constraint_map,
    lookup_pred_constraint(PredConstraintMap, PredId, _, MCInfo),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_inst_graph_info(PredInfo, CalleeInstGraphInfo),
    CalleeInstGraph = CalleeInstGraphInfo ^ interface_inst_graph,
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, ProcList),
    ( if
        find_matching_proc_2(ProcList, ProdVars, Args,
            CallerInstGraph, CalleeInstGraph, MCInfo, ProcId0, ConsumingVars0)
    then
        ProcId = ProcId0,
        ConsumingVars = ConsumingVars0
    else if
        pred_info_infer_modes(PredInfo)
    then
        % XXX We are inferring modes for the called predicate. Need to add
        % a new mode to the requested procs map.
        unexpected($pred, "infer_modes NYI")
    else
        % If we get here, it means there is a mode error which should have been
        % picked up by the constraints pass but was missed some how.
        unexpected($pred, "unexpected mode error")
    ).

:- pred find_matching_proc_2(assoc_list(proc_id, proc_info)::in,
    set_of_progvar::in, list(prog_var)::in, inst_graph::in, inst_graph::in,
    mode_constraint_info::in, proc_id::out, set_of_progvar::out) is semidet.

find_matching_proc_2([ProcId0 - ProcInfo | ProcList], ProdVars, Args,
        CallerInstGraph, CalleeInstGraph, MCInfo, ProcId, ConsumingVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_head_modes_constraint(ProcInfo, Constraint0),
    Constraint = ensure_normalised(Constraint0),
    ( if
        (
            all [X, Y] (
                inst_graph.corresponding_nodes_from_lists(
                    CallerInstGraph, CalleeInstGraph, Args, HeadVars, X, Y)
            )
        =>
            (
                set_of_var.contains(ProdVars, X)
            <=>
                (
                    var_entailed(Constraint,
                        mode_constraint_var(MCInfo, out(Y))),
                    not var_entailed(Constraint,
                        mode_constraint_var(MCInfo, in(Y)))
                )
            )
        )
    then
        ProcId = ProcId0,
        GenPred =
            ( pred(X::out) is nondet :-
                some [Y] (
                    inst_graph.corresponding_nodes_from_lists(CallerInstGraph,
                    CalleeInstGraph, Args, HeadVars, X, Y),
                    var_entailed(Constraint,
                        mode_constraint_var(MCInfo, in(Y)))
                )
            ),
        ConsumingVarsList = solutions.solutions(GenPred),
        set_of_var.sorted_list_to_set(ConsumingVarsList, ConsumingVars)
    else
        find_matching_proc_2(ProcList, ProdVars, Args, CallerInstGraph,
        CalleeInstGraph, MCInfo, ProcId, ConsumingVars)
    ).

:- pred report_ordering_mode_errors(module_info::in, io::di, io::uo) is det.

report_ordering_mode_errors(_, !IO).
    % XXX
    % io.stderr_stream(StdErr, !IO),
    % io.write_string(StdErr, "Mode error reporting NYI", !IO).

:- pred lookup_pred_constraint(pred_constraint_map::in, pred_id::in,
    mode_constraint::out, mode_constraint_info::out) is det.

lookup_pred_constraint(PCM, PredId, MC, MCI) :-
    map.lookup(PCM, PredId, pci(MC, MCI)).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.mode_ordering.
%-----------------------------------------------------------------------------%
