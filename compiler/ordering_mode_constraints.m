%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ordering_mode_constraints.m.
% Main author: richardf.

% This module contains code for ordering conjuncts in a predicate
% according to variable producer consumer relationships and
% other mode analysis constraints.

%-----------------------------------------------------------------------------%

:- module check_hlds.ordering_mode_constraints.
:- interface.

:- import_module check_hlds.build_mode_constraints.
:- import_module check_hlds.prop_mode_constraints.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.goal_path.

:- import_module io.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Original position in a conjunction. Count starts at one.
    %
:- type conjunct_id == int.

:- type mode_ordering_constraints == list(mode_ordering_constraint).

    % Mode ordering constraints.
    %
:- type mode_ordering_constraint
    --->    lt(
                first       :: conjunct_id,     % Typically the producer
                second      :: conjunct_id      % Typically the consumer
            ).

    % Store for the ordering constraints for one conjunction.
    %
:- type ordering_constraints_info
    --->    ordering_constraints_info(
                oci_containing_map      :: containing_goal_map,

                % The number of conjucts in this conjunction
                oci_num_conjuncts       :: int,

                % Constraints on the conjuncts.
                oci_constraints         :: set(mode_ordering_constraint)
            ).

%-----------------------------------------------------------------------------%

    % mode_reordering(Constraints, VarMap, SCCs, !ModuleInfo) orders
    % conjunctions for each predicate in SCCs in the ModuleInfo
    % according to the modes implied by the producer/consumer
    % constraints in Constraints. All constraint variables relevant to
    % the predicates in SCCs should be stored in the VarMap.
    %
:- pred mode_reordering(pred_constraints_map::in, mc_var_map::in,
    list(list(pred_id))::in, module_info::in, module_info::out) is det.

    % add_ordering_constraint(Constraint, !OCI) adds Constraint
    % to the ordering constraints store. It fails if it immediately
    % detects a contradiction (at the moment, this means it has
    % detected a loop in the producer consumer dependency graph,
    % such as in the conjunction:
    % (p(A, B), p(B, C), p(C, A)) where p is pred(in, out)).
    %
    % NOTE: behaviour when constrained conjuncts are outside the
    % possible range is undefined.
    %
:- pred add_ordering_constraint(mode_ordering_constraint::in,
    ordering_constraints_info::in, ordering_constraints_info::out) is semidet.

    % add_lt_constraint(A, B, !OCI) constrains conjunct A to come
    % before conjunct B, in the constraints store.
    %
:- pred add_lt_constraint(conjunct_id::in, conjunct_id::in,
    ordering_constraints_info::in, ordering_constraints_info::out) is semidet.

%-----------------------------------------------------------------------------%

    % dump_goal_paths(ModuleInfo, PredIds, !IO)
    %
    % Dumps the goal paths of each goal in the order they appear for each
    % predicate in PredIds for the purposes of visually checking re-ordering.
    %
:- pred dump_goal_paths(module_info::in, list(pred_id)::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.abstract_mode_constraints.
:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.mcsolver.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%

    % This type stores information about mode analysis failures.
    % The information is used when preparing error messages.
    %
:- type mode_analysis_failures == list(mode_analysis_failure).

    % This type stores information about a mode analysis failure.
    % The information is used when preparing error messages.
    %
:- type mode_analysis_failure
   --->     no_producer_consumer_sols(
                % The predicate for which the producer/consumer analysis
                % couldn't be solved.
                failing_predicate   ::  pred_proc_id
            )
    ;       mode_inference_failed(
                % The caller of the predicate for which mode inference
                % has failed.
                caller              ::  pred_id,

                % The SCC of predicates to be mode inferred for which
                % mode inference has failed.
                scc                 :: list(pred_id)
            )
    ;       conjunct_ordering_failed(pred_proc_id).

    % A map from program variables to related producer/consumer
    % constraint variables' abstract representations. The constraint
    % variables should each represent the proposition that the
    % program variable is produced at some particular conjunct, all
    % in the one conjunction.
    %
:- type prog_var_at_conjuncts_map == multi_map(prog_var, mc_rep_var).

%-----------------------------------------------------------------------------%
%
% Predicate reordering
%

mode_reordering(PredConstraintsMap, VarMap, SCCs, !ModuleInfo) :-
    list.foldl(scc_reordering(PredConstraintsMap, VarMap), SCCs, !ModuleInfo).

    % scc_reording(PredConstraintsMap, VarMap, SCC, !ModuleInfo)
    %
    % Copies the clauses of predicates in SCC into the body goal of their
    % procedures and performs conjunction reordering according to the
    % producer consumer constraints in PredConstraintsMap.
    %
:- pred scc_reordering(pred_constraints_map::in, mc_var_map::in,
    list(pred_id)::in, module_info::in, module_info::out) is det.

scc_reordering(PredConstraintsMap, VarMap, SCC0, !ModuleInfo) :-
    % Process only predicates from this module.
    list.filter(module_info_pred_status_is_imported(!.ModuleInfo),
        SCC0, _, SCC),

    list.filter(
        ( pred(PredId::in) is semidet :-
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_infer_modes(PredInfo)
        ), SCC, PredsToInfer, PredsToCheck),

    (
        PredsToInfer = [_ | _],
        sorry($pred, "NYI: mode inference")
    ;
        PredsToInfer = []
    ),

    list.foldl(pred_reordering(PredConstraintsMap, VarMap), PredsToCheck,
        !ModuleInfo).

    % pred_reordering(PredConstraintsMap, VarMap, PredId, !ModuleInfo)
    % applies mode reordering to conjunctions in the body goal of the
    % predicate PredId for each procedure in that predicate.
    %
:- pred pred_reordering(pred_constraints_map::in, mc_var_map::in,
    pred_id::in, module_info::in, module_info::out) is det.

pred_reordering(PredConstraintsMap, VarMap, PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ( pred_info_infer_modes(PredInfo0) ->
        % XXX GIVE UP FOR NOW!!!! In reality, execution shouldn't reach here
        % if the pred is to be mode inferred, should it?
        sorry($pred, "mode inference constraints")
    ;
        % XXX Maybe move this outside of this predicate - then
        % the predicate can assume that the correct procedures
        % have been created and that they have the correct bodies.
        % XXX Indeed, this job *should* have been done *before* this pass
        % was invoked.
        copy_clauses_to_procs_for_pred_in_module_info(PredId, !ModuleInfo),

        module_info_pred_info(!.ModuleInfo, PredId, PredInfo1),

        map.lookup(PredConstraintsMap, PredId,
            {ContainingGoalMap, PredConstraints}),
        ProcIds = pred_info_all_procids(PredInfo1),
        list.foldl2(
            proc_reordering(ContainingGoalMap, PredConstraints, VarMap,
                PredId),
            ProcIds, [], Errors, PredInfo1, PredInfo),

        (
            Errors = [],
            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
        ;
            Errors = [_ | _],
            % XXX Deal with mode errors here!
            % This is a placeholder error message.
            ErrorsString = string.string(Errors),
            sorry($pred, "mode checking failure: " ++ ErrorsString)
        )
    ).

    % proc_reordering(PredConstraints, VarMap, PredId, ProcId, !PredInfo)
    %
    % Orders conjunctions in procedure ProcId of predicate PredId, according
    % to the producer consumer constraints in PredConstraints. The procedure
    % with the modified body goal replaces its original in PredInfo.
    %
:- pred proc_reordering(containing_goal_map::in, pred_p_c_constraints::in,
    mc_var_map::in, pred_id::in, proc_id::in,
    mode_analysis_failures::in, mode_analysis_failures::out,
    pred_info::in, pred_info::out) is det.

proc_reordering(ContainingGoalMap, PredConstraints, VarMap, PredId, ProcId,
        !Errors, !PredInfo) :-
    pred_info_proc_info(!.PredInfo, ProcId, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),

    ConstraintsForProc = all_constraints_for_proc(ProcId, PredConstraints),

    PrepConstraints0 = new_prep_cstrts,
    prepare_abstract_constraints(ConstraintsForProc, PrepConstraints0,
        PrepConstraints1),
    SolverConstraints = make_solver_cstrts(PrepConstraints1),

    % solve_proc_reordering is cc_multi because each of its solutions
    % is equivalent in the sense that they all contain the same goals
    % and conjunctions are ordered according to some legitimate
    % solution to the producing and consuming goals of program
    % variables.
    Errors0 = !.Errors,
    promise_equivalent_solutions [Errors1, Goal] (
        solve_proc_reordering(ContainingGoalMap, VarMap, PredId, ProcId,
            SolverConstraints, Errors0, Errors1, Goal0, Goal)
    ),
    !:Errors = Errors1,

    proc_info_set_goal(Goal, ProcInfo0, ProcInfo),
    pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo).

    % solve_proc_reordering(VarMap, PredId, ProcId, SolverConstraints,
    %   !Errors, !Goal)
    %
    % Performs the nondeterministic constraint solving for proc_reordering
    % - using the constraints in SolverConstraints to order the goals
    % in Goal (from procedure ProcId in predicate PredId). Any failure
    % is stored in Errors, and the predicate still proceeds.
    % VarMap should contain any constraint variables referring to Goal
    % and the program variables in it.
    %
    % solve_proc_reordering is cc_multi because each of its solutions
    % is equivalent in the sense that they all contain the same goals
    % and conjunctions are ordered according to some legitimate
    % solution to the producing and consuming goals of program
    % variables.
    %
:- pred solve_proc_reordering(containing_goal_map::in, mc_var_map::in,
    pred_id::in, proc_id::in, solver_cstrts::in,
    mode_analysis_failures::in, mode_analysis_failures::out,
    hlds_goal::in, hlds_goal::out) is cc_multi.

solve_proc_reordering(ContainingGoalMap, VarMap, PredId, ProcId,
        SolverConstraints, !Errors, !Goal) :-
    (
        mcsolver.solve(SolverConstraints, Bindings),
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings, !Goal)
    ->
        true
    ;
        ( mcsolver.solve(SolverConstraints, _) ->
            list.cons(conjunct_ordering_failed(proc(PredId, ProcId)), !Errors)
        ;
            list.cons(no_producer_consumer_sols(proc(PredId, ProcId)), !Errors)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Conjunction reordering.
%

    % goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings, !Goal)
    % applies mode reordering to conjunctions in Goal (from predicate PredId,
    % which has the given ContainingGoalMap) and its children. VarMap should
    % contain all producer/consumer constraint variables relevant to said
    % conjunctions, and Bindings should contain bindings for them.
    %
:- pred goal_reordering(containing_goal_map::in, pred_id::in,
    mc_var_map::in, mc_bindings::in, hlds_goal::in, hlds_goal::out) is semidet.

goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        % Atomic goals cannot be reordered.
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            % Build constraints for this conjunction.
            make_conjuncts_nonlocal_repvars(PredId, Goals0, RepVarMap),
            OCInfo0 = ordering_init(ContainingGoalMap, list.length(Goals0)),
            conjunct_ordering_constraints(VarMap, Bindings, RepVarMap,
                OCInfo0, OCInfo),

            % Then solve the constraints and reorder.
            minimum_reordering(OCInfo, Order),
            list.map(list.det_index1(Goals0), Order, Goals1),

            % Then recurse on the reordered goals.
            list.map(
                goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings),
                Goals1, Goals)
        ;
            ConjType = parallel_conj,
            list.map(
                goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings),
                Goals0, Goals)
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        list.map(goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings),
            Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(_, _, _),
        % We haven't yet even tried to turn disjunctions into switches.
        unexpected($pred, "switch")
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings,
            Cond0, Cond),
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings,
            Then0, Then),
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings,
            Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings,
            SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % Is it possible to special-case the handling of
        % from_ground_term_construct scopes?
        goal_reordering(ContainingGoalMap, PredId, VarMap, Bindings,
            SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(_),
        % XXX We need to handle atomic goals.
        % XXX We need to handle try goals.
        unexpected($pred, "NYI: shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

    % ordering_init(N) creates a new ordering constraint system for
    % a conjunction with N conjuncts.
    %
:- func ordering_init(containing_goal_map, int) = ordering_constraints_info.

ordering_init(ContainingGoalMap, N) =
    ordering_constraints_info(ContainingGoalMap, N, set.init).

%-----------------------------------------------------------------------------%

add_ordering_constraint(Constraint, !OCI) :-
    ( set.member(Constraint, !.OCI ^ oci_constraints) ->
        true
    ;
        constraint_transitive_closure(!.OCI, Constraint, NewConstraints),

        % No cycles. (lt(X, X) is a contradiction)
        set.is_empty(
            set.filter(pred(lt(X, X)::in) is semidet, NewConstraints)),

        !OCI ^ oci_constraints :=
            set.union(NewConstraints, !.OCI ^ oci_constraints)
    ).

    % constraint_transitive_closure(OCI, Constraint, NewConstraints)
    % returns a list of constraints in NewConstraints containing
    % Constraint itself, and also all constraints which must be added to
    % OCI to maintain a transitive closure of partial ordering
    % constraints.
    %
:- pred constraint_transitive_closure(ordering_constraints_info::in,
    mode_ordering_constraint::in, set(mode_ordering_constraint)::out) is det.

constraint_transitive_closure(OCI, Constraint, NewConstraints) :-
    Constraints = OCI ^ oci_constraints,
    Constraint = lt(From, To),
    ComesBefore = set.filter_map(
        func(lt(B, F)::in) = (B::out) is semidet :- F = From, Constraints),
    ComesAfter = set.filter_map(
        func(lt(T, A)::in) = (A::out) is semidet :- T = To, Constraints),

    % Each conjunct in the ComesBefore set and the From conjunct must precede
    % the To conjunct and each of the conjuncts in the ComesAfter set.
    set.fold(insert_lt_constraints(set.insert(ComesAfter, To)),
        set.insert(ComesBefore, From), set.init, NewConstraints).

    % insert_lt_constraints(Bs, A, !Cs) adds a lt(A, B) constraint to
    % the set of constraints Cs for each conjunct_id B in set Bs.
    % Note the reversed order of Bs and A.
    %
:- pred insert_lt_constraints(set(conjunct_id)::in, conjunct_id::in,
    set(mode_ordering_constraint)::in, set(mode_ordering_constraint)::out)
    is det.

insert_lt_constraints(Bs, A, !Cs) :-
    set.fold(insert_lt_constraint(A), Bs, !Cs).

    % insert_lt_constraint(A, B, !Cs) adds a lt(A, B) constraint to the set
    % of constraints.
    %
:- pred insert_lt_constraint(conjunct_id::in, conjunct_id::in,
    set(mode_ordering_constraint)::in, set(mode_ordering_constraint)::out)
    is det.

insert_lt_constraint(A, B, !Cs) :-
    set.insert(lt(A, B), !Cs).

%-----------------------------------------------------------------------------%

add_lt_constraint(A, B, !OCI) :-
    add_ordering_constraint(lt(A, B), !OCI).

%-----------------------------------------------------------------------------%

    % make_conjuncts_nonlocal_repvars(PredId, Goals, RepvarMap)
    %
    % The keys of RepvarMap are the program variables nonlocal to Goals that
    % appear in goals. Each is mapped to the mc_rep_var representation of the
    % proposition that it is produced at a Goal in Goals, for every Goal in
    % Goals it is nonlocal to.
    %
:- pred make_conjuncts_nonlocal_repvars(pred_id::in, hlds_goals::in,
    prog_var_at_conjuncts_map::out) is det.

make_conjuncts_nonlocal_repvars(PredId, Goals, RepvarMap) :-
    list.foldl(make_conjunct_nonlocal_repvars(PredId), Goals,
        multi_map.init, RepvarMap).

    % See make_conjuncts_nonlocal_repvars; acts on a single conjunct.
    %
:- pred make_conjunct_nonlocal_repvars(pred_id::in, hlds_goal::in,
    prog_var_at_conjuncts_map::in, prog_var_at_conjuncts_map::out) is det.

make_conjunct_nonlocal_repvars(PredId, Goal, !RepvarMap) :-
    Goal = hlds_goal(_, GoalInfo),
    Nonlocals = goal_info_get_nonlocals(GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),

    set_of_var.fold(
        ( pred(NL::in, RMap0::in, RMap::out) is det :-
            multi_map.set(NL, NL `in` PredId `at` GoalId, RMap0, RMap)
        ),
        Nonlocals, !RepvarMap).

%-----------------------------------------------------------------------------%

    % conjunct_ordering_constraints(VarMap, Bindings, RepVarMap, !OCInfo)
    %
    % Adds ordering constraints based on producer/consumer analysis of
    % variables nonlocal to conjuncts in a single conjunction, to the
    % OCInfo.
    %
    % For the conjunction in question, RepVarMap should contain any
    % program variable nonlocal to any conjunct. The program variables
    % should be mapped to any constraint variable concerning them
    % related to any of the conjuncts in the conjunction. (Actually,
    % they will be mapped to an abstract rep_var representing this
    % constraint variable, and the VarMap is required to look up
    % the constraint variable using the rep_var.) Bindings should
    % contain bindings for all such constraint variables.
    %
:- pred conjunct_ordering_constraints(mc_var_map::in, mc_bindings::in,
    prog_var_at_conjuncts_map::in, ordering_constraints_info::in,
    ordering_constraints_info::out) is semidet.

conjunct_ordering_constraints(VarMap, Bindings, RepVarMap, !OCInfo) :-
    map.foldl(prog_var_ordering_constraints(VarMap, Bindings),
        RepVarMap, !OCInfo).

    % prog_var_ordering_constraints(VarMap, Bindings, ProgVar, RepVars,
    %   !OCInfo)
    %
    % Adds ordering constraints for a conjunction to OCInfo.
    % Specifically, those relating to which conjuncts produce and which
    % consume the variable ProgVar. See conjunct_ordering_constraints
    % for details.
    %
:- pred prog_var_ordering_constraints(mc_var_map::in, mc_bindings::in,
    prog_var::in, list(mc_rep_var)::in,
    ordering_constraints_info::in, ordering_constraints_info::out) is semidet.

prog_var_ordering_constraints(VarMap, Bindings, _ProgVar, RepVars, !OCInfo) :-
    list.filter(produced_at_path(VarMap, Bindings), RepVars,
        ProgVarAtProducers, ProgVarAtConsumers),
    (
        ProgVarAtProducers = []
        % Variable not produced here - no constraints.
    ;
        ProgVarAtProducers = [RepVar],      % Should be only one producer
        ContainingGoalMap = !.OCInfo ^ oci_containing_map,
        get_position_in_conj(ContainingGoalMap, RepVar, First),
        list.map(get_position_in_conj(ContainingGoalMap), ProgVarAtConsumers,
            Laters),

        list.foldl(add_lt_constraint(First), Laters, !OCInfo)
    ).

    % produced_at_path(VarMap, Bindings, ProgVar `at` GoalPath `in` _)
    % succeeds if ProgVar is produced at GoalPath, according to the
    % solution to the producer/consumer constraint solutions in Bindings.
    %
:- pred produced_at_path(mc_var_map::in, mc_bindings::in, mc_rep_var::in)
    is semidet.

produced_at_path(VarMap, Bindings, RepVar) :-
    map.lookup(Bindings, bimap.lookup(VarMap, RepVar)) = yes.

    % get_position_in_conj(RepVar, N) fails if the deepest level of the
    % goalpath in RepVar is not a conjunction, otherwise it returns in N
    % the number of the conjunct the RepVar refers to.
    %
:- pred get_position_in_conj(containing_goal_map::in, mc_rep_var::in,
    conjunct_id::out) is semidet.

get_position_in_conj(ContainingGoalMap, _Var `in` _PredId `at` GoalId, N) :-
    map.lookup(ContainingGoalMap, GoalId, ContainingGoal),
    ContainingGoal = containing_goal(_, LastStep),
    LastStep = step_conj(N).

%-----------------------------------------------------------------------------%

    % minimum_reordering(OCI, Order)
    %
    % Order is a minimum re-ordering of conjuncts, conforming to the
    % constraints in OCI. The values of the conjunct_ids returned represent
    % the original position of a conjunct, the position of the conjunct_id
    % in the Order represents the new position of the conjunct.
    %
    % Fails if no reordering conforms with the constraints.
    %
:- pred minimum_reordering(ordering_constraints_info::in,
    list(conjunct_id)::out) is semidet.

minimum_reordering(OCI, Order) :-
%    % Heavy handed - a topological sort can more easily be used to
%    % achieve minimum reordering.
%    original_order_constraints(OCI ^ num_conjuncts, OriginalOrderConstraints),
%    constrain_if_possible(OriginalOrderConstraints, OCI0, OCI1),

    Conjuncts = set.from_sorted_list(1 `..` OCI ^ oci_num_conjuncts),
    topological_sort_min_reordering(OCI ^ oci_constraints, Conjuncts, Order).

    % original_order_constraints(N, MOCs) produces a list of constraints MOCs
    % that describe a complete order for N conjuncts, such that they are not
    % reordered at all from their original positions.
    %
:- pred original_order_constraints(int::in,
    mode_ordering_constraints::out) is det.
:- pragma consider_used(original_order_constraints/2).

original_order_constraints(N, MOCs) :-
    complete_order_constraints(1 `..` N, MOCs).

    % complete_order_constraints(Xs) produces a list of constraints that
    % describe a compete order for Xs such that it is not reordered at all.
    %
:- pred complete_order_constraints(list(conjunct_id)::in,
    mode_ordering_constraints::out) is det.

complete_order_constraints(Xs, MOCs) :-
    add_complete_order_constraints(Xs, set.init, MOCs0),
    MOCs = set.to_sorted_list(MOCs0).

    % add_complete_order_constraints(Xs, !MOCs) adds a list of constraints
    % that describe a compete order for Xs such that it is not reordered
    % at all.
    %
:- pred add_complete_order_constraints(list(conjunct_id)::in,
    set(mode_ordering_constraint)::in, set(mode_ordering_constraint)::out)
    is det.

add_complete_order_constraints([], !MOCs).
add_complete_order_constraints([Conjunct | Conjuncts], !MOCs) :-
    list.foldl(insert_lt_constraint(Conjunct), Conjuncts, !MOCs),
    add_complete_order_constraints(Conjuncts, !MOCs).

    % constraint_if_possible(Constraints, !OCI)
    %
    % Adds the given Constraints to the constraints info OCI, but only
    % if no direct contradiction is found.
    %
:- pred constrain_if_possible(mode_ordering_constraints::in,
    ordering_constraints_info::in, ordering_constraints_info::out) is det.
:- pragma consider_used(constrain_if_possible/3).

constrain_if_possible([], !OCI).
constrain_if_possible([Constraint | Constraints], !OCI) :-
    ( add_ordering_constraint(Constraint, !OCI) ->
        constrain_if_possible(Constraints, !OCI)
    ;
        constrain_if_possible(Constraints, !OCI)
    ).

%-----------------------------------------------------------------------------%

    % topological_sort_min_reordering(Constraints, Conjuncts, Ordering)
    %
    % Succeeds if Ordering is a minimum re-ordering of Conjuncts
    % consistent with the system of Constraints.
    %
:- pred topological_sort_min_reordering(set(mode_ordering_constraint)::in,
    set(conjunct_id)::in, list(conjunct_id)::out) is semidet.

topological_sort_min_reordering(Constraints0, Conjuncts0, Ordering) :-
    NotFirst = set.map(func(lt(_From, To)) = To, Constraints0),
    CandidatesForFirst = set.difference(Conjuncts0, NotFirst),

    ( set.remove_least(First, CandidatesForFirst, _) ->
        % Remove First from the system.
        set.remove(First, Conjuncts0, Conjuncts),
        Constraints = set.filter(
            (pred(lt(From, _To)::in) is semidet :- From \= First),
            Constraints0),

        % Order the rest, then put First at the head.
        topological_sort_min_reordering(Constraints, Conjuncts, Ordering0),
        Ordering = [First | Ordering0]
    ;
        % No cantidates for First, so we are only done if there were
        % no nodes (conjuncts) left to begin with.
        set.is_empty(Conjuncts0),
        Ordering = []
    ).

%-----------------------------------------------------------------------------%

dump_goal_paths(ModuleInfo, PredIds0, !IO) :-
    % Process only predicates from this module.
    list.filter(module_info_pred_status_is_imported(ModuleInfo),
        PredIds0, _, PredIds),
    list.foldl(dump_pred_goal_paths(ModuleInfo), PredIds, !IO).

    % dump_pred_goal_paths(ModuleInfo, PredId, !IO)
    %
    % Dumps the goal paths of each goal in the order they appear for
    % predicate PredId for the purposes of visually checking re-ordering.
    %
:- pred dump_pred_goal_paths(module_info::in, pred_id::in, io::di, io::uo)
    is det.

dump_pred_goal_paths(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    ProcIds = map.keys(ProcTable),

    % Start with a blank line.
    module_info_get_globals(ModuleInfo, Globals),
    write_error_pieces_plain(Globals, [fixed("")], !IO),

    PredHeaderFormat = [words("Goal paths for")] ++
        describe_one_pred_info_name(should_module_qualify, PredInfo) ++
        [suffix("."), nl],

    write_error_pieces_plain(Globals, PredHeaderFormat, !IO),

    (
        ProcIds = [],
        pred_info_get_clauses_info(PredInfo, ClausesInfo),
        clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
        get_clause_list_maybe_repeated(ClausesRep, Clauses),
        Goals = list.map(func(Clause) = clause_body(Clause), Clauses),
        Indent = 0,
        list.foldl(dump_goal_goal_paths(Globals, Indent), Goals, !IO)
    ;
        ProcIds = [_ | _],
        list.foldl(dump_proc_goal_paths(Globals, ProcTable), ProcIds, !IO)
    ).

    % dump_proc_goal_paths(Globals, ProcTable, ProcId, !IO)
    %
    % Dumps the goal paths of each goal in the order they appear for
    % procedure ProcId for the purposes of visually checking re-ordering.
    %
:- pred dump_proc_goal_paths(globals::in, proc_table::in, proc_id::in,
    io::di, io::uo) is det.

dump_proc_goal_paths(Globals, ProcTable, ProcId, !IO) :-
    ProcIdString = string.from_int(proc_id_to_int(ProcId)),
    ProcHeaderFormat = [words("mode"), words(ProcIdString), suffix(":")],
    write_error_pieces_plain(Globals, ProcHeaderFormat, !IO),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    Indent = 0,
    dump_goal_goal_paths(Globals, Indent, Goal, !IO).

    % dump_goal_goal_paths(Globals, Indent, Goal, !IO)
    %
    % Dumps the goal paths for this goal at the indent depth Indent, then
    % recurses for each sub-goal at one further level of indent,
    % in the order they appear, for the purposes of visually checking
    % re-ordering.
    %
:- pred dump_goal_goal_paths(globals::in, int::in, hlds_goal::in,
    io::di, io::uo) is det.

dump_goal_goal_paths(Globals, Indent, Goal, !IO) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),
    GoalId = goal_id(GoalIdNum),
    GoalIdPieces = [words(string.int_to_string(GoalIdNum)), nl],
    write_error_pieces_maybe_with_context(Globals, no, Indent, GoalIdPieces,
        !IO),

    % Dump the goal paths for each subgoal in GoalExpr at SubGoalIndent,
    % in the order they appear, for the purposes of visually checking
    % reordering.
    SubGoalIndent = Indent + 1,
    (
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        )
        % There are no subgoals to recurse on.
    ;
        GoalExpr = conj(_, Goals),
        list.foldl(dump_goal_goal_paths(Globals, SubGoalIndent), Goals, !IO)
    ;
        GoalExpr = disj(Goals),
        list.foldl(dump_goal_goal_paths(Globals, SubGoalIndent), Goals, !IO)
    ;
        GoalExpr = switch(_, _, _),
        unexpected($pred, "switch")
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        Goals = [CondGoal, ThenGoal, ElseGoal],
        list.foldl(dump_goal_goal_paths(Globals, SubGoalIndent), Goals, !IO)
    ;
        GoalExpr = negation(SubGoal),
        dump_goal_goal_paths(Globals, SubGoalIndent, SubGoal, !IO)
    ;
        GoalExpr = scope(_, SubGoal),
        dump_goal_goal_paths(Globals, SubGoalIndent, SubGoal, !IO)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            Goals = [MainGoal | OrElseGoals],
            list.foldl(dump_goal_goal_paths(Globals, SubGoalIndent), Goals,
                !IO)
        ;
            ShortHand = try_goal(_, _, _),
            unexpected($pred, "try_goal")
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.ordering_mode_constraints.
%-----------------------------------------------------------------------------%
