%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: unneeded_code.m.
% Author: zs.
%
% This module implements two related source-to-source transforms,
% both of which focus on goals that produce some variables, where these
% variables are not always required by the following computation.
%
% If there are no computation paths on which the variables produced by a goal
% may be needed, then the first transform deletes that goal.
%
% If the variables produced by a goal may be needed on some but not all
% computation paths, then the second transform moves that goal to the starts
% of those computation paths, thus avoiding the cost of executing the goal
% on all other computation paths. (This is related to the concept of partial
% redundancy elimination (PRE) for imperative languages.)
%
% Mercury has two constructs that make it possible for a variable to be needed
% on some computation paths but not others: switches and if-then-elses.
%
% In the case of switches, the alternative computation paths are those
% corresponding to the possible values of the switched-on variable, and
% not just the switch arms. Even if all switch arms need a variable, it
% is an optimization to copy the code generating that variable to the starts of
% all the switch arms if the switch is can_fail, i.e. there are some function
% symbols that the switched-on variable can be bound to that do not have arms.
%
% In the case of if-then-elses, the alternatives are the then part and
% the else part. Any variable needed by the condition is needed in both those
% computation paths.
%
% From the point of view of this transform, disjunctions are not branched
% control structures, because entering a disjunct does not preclude later
% entering another disjunct. Any variable needed by any disjunct must therefore
% be produced before control enters the disjunction. (In theory, a disjunct
% that cannot fail in a model_semi disjunction prevents entry to the following
% disjuncts, but any such following disjuncts will have been removed long ago
% by simplification.)
%
% Note that by avoiding the execution of a goal that appears in the original
% source code of the program, both these transforms can in general change the
% operational semantics of the program. Therefore a goal can only be eliminated
% or moved if the goal is has no observable effect except the result it
% generates (i.e is pure, cannot fail, cannot loop, cannot raise an exception),
% which is usually true only of goals composed entirely of builtins, or if
% the semantics options explicitly permit the change in the operational
% semantics, which will usually be an improvement (e.g. avoiding an infinite
% loop or an unnecessary exception).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.unneeded_code.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred unneeded_process_proc_msg(pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.goal_form.
:- import_module hlds.goal_path.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

    % The branch_alts and branch_point types record the information the
    % transform needs to know about a particular branched control
    % structure: where it is, what kind it is, and how many alternatives
    % it has.
    %
:- type branch_point
    --->    branch_point(
                % The id of the branch point.
                goal_id,

                % What kind of goal the branch point is, and many branches
                % it has. Note that the second argument is a function of
                % the first.
                branch_alts
            ).

:- type branch_alts
    --->    alt_ite
            % If-then-elses always have two alternatives: the then branch
            % (numbered 1) and the else branch (numbered 2).

    ;       alt_switch(maybe_switch_num_functors).
            % The number of alternatives in a switch is equal to the number of
            % function symbols in the type of the switched-on variable. This
            % number is given by the argument integer, if present; if the
            % argument is "no", then the number of function symbols in the type
            % is effectively infinite (this can happen for builtin types
            % such as "int"). If the switch cannot_fail, then this will be
            % equal to the number of cases; if the switch can_fail, there
            % will be strictly fewer cases than this.

    % The location type identifies one arm of a branched control structure.
    % The branched control structure id is a branch_point instead of a
    % simple goal_path because without the branch_alts info, the
    % transformation cannot tell if a given set of branches of a branched
    % control structure covers all possible execution paths or not.
    %
:- type location
    --->    location(
                branch_point,   % To which branched control structure
                                % does the location belong.
                int             % The branch within that control structure.
            ).

    % The where_needed_map type maps each variable to the set of
    % computation branches where it is needed. If a variable is needed
    % everywhere, then the computation producing it cannot be eliminated
    % or moved. If it is not needed at all, its producer can be eliminated.
    % If it is needed on some but not all branches, then the producer
    % can be moved to the starts of those branches (or, preferably,
    % to the first point in those branches that need them, but we do not do
    % that yet).
    %
    % The set of branches to whose starts the producer can be moved
    % is represented as a map from the id of the branched control
    % structure to the set of branch numbers within that branched control
    % structure. If the branched control structure at goal path gp is
    % mapped to a set including N, then the producer of that variable
    % may be moved to the start of the goal with goal path <gp>;sN;
    % (if the control structure is a switch) or <gp>;t; or <gp>;e;
    % (if the control structure is an if-then-else).
    %
    % Since <gp>;sN; is conjoined with e.g. <gp>;sN;<gp2>;sM;
    % it would be a mode error (variable having two conjoined producers)
    % for the transformed code to have the producer of some variable
    % inserted at the start of both those goals. It is therefore an
    % invariant that a where_needed structure mapping gp to N
    % will not contain any keys whose goal_path includes <gp>;sN;
    % or its if-then-else equivalent.
    %
    % An example:
    %
    %   % switch on X at goal path gp
    %   ( % s1
    %       X = a,
    %       ... code that needs Y and Z ...
    %   ; % s2
    %       X = b,
    %       ( if Y = f then
    %           ... code that needs only Z ...
    %       else
    %           ... code that does not need Y or Z ...
    %       )
    %   )
    %
    % X is needed everywhere, since even if X is bound to c, its value must
    % be tested.
    %
    % Y is needed everywhere iff the type of X contains only a and b,
    % otherwise it is needed only in the <gp>;s1; and <gp>;s2; switch arms.
    %
    % Z is needed in <gp>;s1; and <gp>;s2;t; but is not needed in the
    % <gp>;s2;e; else arm. Therefore the where_needed_branches map for Z
    % will map gp to 1 and <gp>;s2; to 1.
    %
:- type where_needed_map    ==  map(prog_var, where_needed).

:- type where_needed
    --->    everywhere
    ;       branches(where_needed_branches).

:- type where_needed_branches   ==  map(branch_point, set(int)).

    % The refined_goal_map structure maps branch goals to the list of
    % producers that should be moved to the start of that branch.
    % The order is important, since some of the producers in such a list
    % may depend on variables produced by other goals that precede them
    % in the list.
    %
:- type refined_goal_map == map(pair(goal_id, int), list(hlds_goal)).

%-----------------------------------------------------------------------------%

unneeded_process_proc_msg(PredProcId, !ProcInfo, !ModuleInfo) :-
    trace [io(!IO)] (
        write_proc_progress_message(!.ModuleInfo,
            "Removing dead code in", PredProcId, !IO)
    ),
    % The transformation considers every nonlocal variable of a goal
    % that is bound on entry to be consumed by that goal. If the nonlocal set
    % contains any such variables that are not actually needed by the goal,
    % then the transformation will not be as effective as it could be.
    % Therefore we preprocess the procedure body to ensure that the nonlocals
    % sets are accurate reflections of the true needs of goals.
    unneeded_pre_process_proc(!ProcInfo),
    PredProcId = proc(PredId, _),
    unneeded_process_proc(!ProcInfo, !ModuleInfo, PredId, 1, _Successful).

:- pred unneeded_pre_process_proc(proc_info::in, proc_info::out) is det.

unneeded_pre_process_proc(!ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset_vartypes(!.ProcInfo, VarSet0, VarTypes0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    implicitly_quantify_clause_body_general(ordinary_nonlocals_no_lambda,
        HeadVars, _Warnings, Goal0, Goal,
        VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset_vartypes(VarSet, VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo).

% The source-to-source transform operates in two phases.
%
% The first phase traverses the procedure body, keeping track of which
% variables are needed where. When it finds a goal that can be deleted,
% it deletes it by replacing it with the goal `true'. When it finds a goal
% that can be moved, it does the same, but also records in the RefinedGoalsMap
% that the deleted goal must later be inserted at the starts of the branches
% where its outputs may be needed, and accordingly notes that its own inputs
% are needed in those branches.
%
% The second phase traverses the modified problem body, and inserts the
% goals in the RefinedGoalsMap at the starts of the indicated branches.
% This phase identified the indicated branches by the goal_path annotations
% on their parents. These may be out of date since the first phase will have
% deleted some goals, but since neither phase modifies the goal_path annotation
% on a goal once that goal has been inserted into the RefinedGoalsMap,
% this does not matter.
%
% Neither phase traverses the internals of a goal that has been moved.
% To make sure that such goals are optimized whenever possible, the algorithm
% invokes itself recursively whenever it was able to successfully (delete or)
% move a goal. This cannot lead to infinite recursion, since each iteration
% will strictly reduce the number of computation paths on which a subgoal
% of the procedure body is executed. Since both the number of subgoals and
% computation paths are finite, the recursion must end.

:- type uc_option_values
    --->    uc_option_values(
                uc_fully_strict         :: bool,
                uc_reorder_conj         :: bool,
                uc_debug                :: bool,
                uc_copy_limit           :: int
            ).

:- type unneeded_code_info
    --->    unneeded_code_info(
                uci_module_info         :: module_info,
                uci_vartypes            :: vartypes,
                uci_options             :: uc_option_values,
                uci_containing_goal_map :: containing_goal_map
            ).

:- pred unneeded_process_proc(proc_info::in, proc_info::out,
    module_info::in, module_info::out, pred_id::in, int::in, bool::out) is det.

unneeded_process_proc(!ProcInfo, !ModuleInfo, PredId, Pass, Successful) :-
    fill_goal_id_slots_in_proc(!.ModuleInfo, ContainingGoalMap, !ProcInfo),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset_vartypes(!.ProcInfo, VarSet0, VarTypes0),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
    Goal0 = hlds_goal(_, GoalInfo0),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
    apply_instmap_delta(InstMapDelta, InitInstMap, FinalInstMap),
    proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo, NeededVarsList),
    map.init(WhereNeededMap0),
    NeededEverywhere =
        ( pred(Var::in, NeededMap0::in, NeededMap::out) is det :-
            map.det_insert(Var, everywhere, NeededMap0, NeededMap)
        ),
    list.foldl(NeededEverywhere, NeededVarsList,
        WhereNeededMap0, WhereNeededMap1),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, reorder_conj, ReorderConj),
    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    globals.get_opt_tuple(Globals, OptTuple),
    Limit = OptTuple ^ ot_opt_unneeded_code_copy_limit,
    globals.lookup_bool_option(Globals, unneeded_code_debug, Debug),
    Options = uc_option_values(FullyStrict, ReorderConj, Debug, Limit),
    (
        Debug = no
    ;
        Debug = yes,
        trace [io(!IO)] (
            io.output_stream(Stream, !IO),
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            PredName = pred_info_name(PredInfo),
            globals.lookup_accumulating_option(Globals,
                unneeded_code_debug_pred_name, DebugPredNames),
            (
                DebugPredNames = [],
                io.format(Stream, "%% Starting unneededed code pass %d\n",
                    [i(Pass)], !IO)
            ;
                DebugPredNames = [_ | _],
                ( if list.member(PredName, DebugPredNames) then
                    io.format(Stream, "%% Starting unneededed code pass %d\n",
                        [i(Pass)], !IO),
                    OutInfo = init_hlds_out_info(Globals, output_debug),
                    write_goal(OutInfo, Stream, !.ModuleInfo, VarSet0,
                        print_name_and_num, 0, ".\n", Goal0, !IO)
                else
                    true
                )
            )
        )
    ),
    UnneededInfo = unneeded_code_info(!.ModuleInfo, VarTypes0, Options,
        ContainingGoalMap),
    unneeded_process_goal(UnneededInfo, Goal0, Goal1,
        InitInstMap, FinalInstMap, WhereNeededMap1, _,
        map.init, RefinedGoals1, no, Changed),
    unneeded_refine_goal(Goal1, Goal2, RefinedGoals1, RefinedGoals),
    expect(map.is_empty(RefinedGoals), $pred,
        "goal reattachment unsuccessful"),
    (
        Changed = yes,
        % We need to fix up the goal_info by recalculating the nonlocal vars
        % and the non-atomic instmap deltas.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
        implicitly_quantify_clause_body_general(ordinary_nonlocals_no_lambda,
            HeadVars, _Warnings,
            Goal2, Goal3, VarSet0, VarSet, VarTypes0, VarTypes,
            RttiVarMaps0, RttiVarMaps),
        recompute_instmap_delta(do_not_recompute_atomic_instmap_deltas,
            Goal3, Goal, VarTypes, InstVarSet, InitInstMap, !ModuleInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset_vartypes(VarSet, VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
        ( if Pass > 3 then
            true
        else
            unneeded_process_proc(!ProcInfo, !ModuleInfo, PredId, Pass + 1, _)
        ),
        Successful = yes
    ;
        Changed = no,
        Successful = no
    ).

:- pred unneeded_process_goal(unneeded_code_info::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_goal(UnneededInfo, Goal0, Goal, InitInstMap, FinalInstMap,
        !WhereNeededMap, !RefinedGoals, !Changed) :-
    can_eliminate_or_move(UnneededInfo, Goal0, InitInstMap, FinalInstMap,
        !.WhereNeededMap, WhereInfo),
    (
        WhereInfo = everywhere,
        unneeded_process_goal_internal(UnneededInfo, Goal0, Goal,
            InitInstMap, FinalInstMap,
            !WhereNeededMap, !RefinedGoals, !Changed)
    ;
        WhereInfo = branches(Branches),
        demand_inputs(UnneededInfo, Goal0, InitInstMap, WhereInfo,
            !WhereNeededMap),
        map.to_assoc_list(Branches, BranchList),
        list.foldl(insert_branch_into_refined_goals(Goal0), BranchList,
            !RefinedGoals),
        Goal = true_goal,
        !:Changed = yes,

        Options = UnneededInfo ^ uci_options,
        Debug = Options ^ uc_debug,
        (
            Debug = no
        ;
            Debug = yes,
            Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
            goal_info_get_goal_id(GoalInfo0) = goal_id(GoalIdNum0),
            trace [io(!IO)] (
                io.output_stream(Stream, !IO),
                io.format(Stream, "unneeded code at goal id %d\n",
                    [i(GoalIdNum0)], !IO)
            )
        )
    ),
    ModuleInfo = UnneededInfo ^ uci_module_info,
    undemand_virgin_outputs(Goal0, ModuleInfo, InitInstMap, !WhereNeededMap),
    ( if goal_get_purity(Goal) = purity_impure then
        % By saying that all vars that are live before the impure goal are
        % needed everywhere, we prevent the movement of the goals producing
        % those vars across the impure goal.
        %
        % This code requires compound goals containing impure code
        % to also be marked impure.
        map.map_values_only(demand_var_everywhere, !WhereNeededMap)
    else
        true
    ).

:- pred insert_branch_into_refined_goals(hlds_goal::in,
    pair(branch_point, set(int))::in,
    refined_goal_map::in, refined_goal_map::out) is det.

insert_branch_into_refined_goals(Goal, BranchPoint - BranchNumSet,
        !RefinedGoals) :-
    BranchPoint = branch_point(GoalPath, _),
    set.to_sorted_list(BranchNumSet, BranchNums),
    list.foldl(insert_branch_arm_into_refined_goals(Goal, GoalPath),
        BranchNums, !RefinedGoals).

:- pred insert_branch_arm_into_refined_goals(hlds_goal::in, goal_id::in,
    int::in, refined_goal_map::in, refined_goal_map::out) is det.

insert_branch_arm_into_refined_goals(Goal, GoalPath, BranchNum,
        !RefinedGoals) :-
    Key = GoalPath - BranchNum,
    ( if map.search(!.RefinedGoals, Key, Goals0) then
        Goals = [Goal | Goals0],
        map.det_update(Key, Goals, !RefinedGoals)
    else
        map.det_insert(Key, [Goal], !RefinedGoals)
    ).

%-----------------------------------------------------------------------------%

:- pred can_eliminate_or_move(unneeded_code_info::in, hlds_goal::in,
    instmap::in, instmap::in,
    where_needed_map::in, where_needed::out) is det.

can_eliminate_or_move(UnneededInfo, Goal, InitInstMap, FinalInstMap,
        WhereNeededMap, !:WhereInfo) :-
    ModuleInfo = UnneededInfo ^ uci_module_info,
    VarTypes = UnneededInfo ^ uci_vartypes,
    instmap_changed_vars(ModuleInfo, VarTypes, InitInstMap, FinalInstMap,
        ChangedVarSet),
    set_of_var.to_sorted_list(ChangedVarSet, ChangedVars),
    map.init(Empty),
    !:WhereInfo = branches(Empty),
    Goal = hlds_goal(_, GoalInfo),
    CurrentId = goal_info_get_goal_id(GoalInfo),
    ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
    list.foldl(
        collect_where_needed(ContainingGoalMap, CurrentId, WhereNeededMap),
        ChangedVars, !WhereInfo),
    Options = UnneededInfo ^ uci_options,
    adjust_where_needed(Goal, Options, !WhereInfo).

:- pred collect_where_needed(containing_goal_map::in, goal_id::in,
    where_needed_map::in, prog_var::in, where_needed::in, where_needed::out)
    is det.

collect_where_needed(ContainingGoalMap, CurrentId, WhereNeededMap, ChangedVar,
        !WhereInfo) :-
    ( if map.search(WhereNeededMap, ChangedVar, Where) then
        where_needed_upper_bound(ContainingGoalMap, CurrentId, Where,
            !WhereInfo)
    else
        true
    ).

    % This is the predicate responsible for ensuring that the act of optimizing
    % away the execution of a goal on some or all computation paths changes the
    % operational semantics only in ways that are explicitly permitted by the
    % programmer.
    %
:- pred adjust_where_needed(hlds_goal::in, uc_option_values::in,
    where_needed::in, where_needed::out) is det.

adjust_where_needed(Goal, Options, !WhereInfo) :-
    ( if
        Goal = hlds_goal(GoalExpr, GoalInfo),
        (
            % Do not move goals that can fail, since doing so can cause
            % execution to reach goals it shouldn't, and those goals may have
            % undesirable behavior (e.g. infinite loops).
            Detism = goal_info_get_determinism(GoalInfo),
            detism_is_moveable(Detism, no)
        ;
            % Do not move impure or semipure goals, since their ordering
            % wrt other such goals must be preserved.
            goal_info_get_purity(GoalInfo) \= purity_pure
        ;
            % With --fully-strict, we cannot optimize away infinite loops
            % or exceptions.
            Options ^ uc_fully_strict = yes,
            goal_can_loop_or_throw(Goal)
        ;
            % With --no-reorder-conj, we cannot move infinite loops or
            % exceptions, but we can delete them.
            Options ^ uc_reorder_conj = no,
            goal_can_loop_or_throw(Goal),
            !.WhereInfo = branches(BranchMap),
            not map.is_empty(BranchMap)
        ;
            % Do not delete the `true' goal, since deleting it is a no-op,
            % and thus does *not* strictly reduce the number of computation
            % paths on which a subgoal of the procedure body is executed.
            GoalExpr = true_goal_expr
        ;
            !.WhereInfo = branches(BranchMap),
            map.values(BranchMap, BranchArms),
            list.map(set.count, BranchArms, BranchArmCounts),
            BranchArmCount = list.foldl(int.plus, BranchArmCounts, 0),
            BranchArmCount > Options ^ uc_copy_limit

            % We may also want to add other space time tradeoffs. E.g. if
            % profiling shows that Goal is required in 10 branches that
            % account for 99% of all executions and is not required in 5
            % branches that account for the remaining 1%, and Goal itself
            % is sufficiently cheap to execute, then not moving Goal may cost
            % a small slowdown in 1% of cases but avoid 9 extra copies of Goal.
            % Due to better instruction cache behavior, not moving Goal
            % may in fact yield faster code after all.
        )
    then
        !:WhereInfo = everywhere
    else
        true
    ).

:- pred detism_is_moveable(determinism::in, bool::out) is det.

detism_is_moveable(detism_det, yes).
detism_is_moveable(detism_semi, no).
detism_is_moveable(detism_non, no).
detism_is_moveable(detism_multi, yes).
detism_is_moveable(detism_erroneous, no).
detism_is_moveable(detism_failure, no).
detism_is_moveable(detism_cc_non, no).
detism_is_moveable(detism_cc_multi, yes).

%---------------------------------------------------------------------------%

:- pred demand_inputs(unneeded_code_info::in, hlds_goal::in, instmap::in,
    where_needed::in, where_needed_map::in, where_needed_map::out) is det.

demand_inputs(UnneededInfo, Goal, InitInstMap, WhereNeeded, !WhereNeededMap) :-
    Goal = hlds_goal(_, GoalInfo),
    NonLocalSet = goal_info_get_nonlocals(GoalInfo),
    GoalId = goal_info_get_goal_id(GoalInfo),
    set_of_var.to_sorted_list(NonLocalSet, NonLocals),
    ModuleInfo = UnneededInfo ^ uci_module_info,
    list.filter(nonlocal_may_be_input(ModuleInfo, InitInstMap), NonLocals,
        Inputs),
    ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
    list.foldl(demand_var(ContainingGoalMap, GoalId, WhereNeeded), Inputs,
        !WhereNeededMap).

:- pred nonlocal_may_be_input(module_info::in, instmap::in,
    prog_var::in) is semidet.

nonlocal_may_be_input(ModuleInfo, InstMap, Var) :-
    instmap_lookup_var(InstMap, Var, Inst),
    inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred undemand_virgin_outputs(hlds_goal::in, module_info::in,
    instmap::in, where_needed_map::in, where_needed_map::out) is det.

undemand_virgin_outputs(Goal, ModuleInfo, InstMap, !WhereNeededMap) :-
    Goal = hlds_goal(_, GoalInfo),
    NonLocalSet = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocalSet, NonLocals),
    list.filter(nonlocal_is_virgin_output(ModuleInfo, InstMap), NonLocals,
        VirginOutputs),
    list.foldl(undemand_var, VirginOutputs, !WhereNeededMap).

:- pred nonlocal_is_virgin_output(module_info::in, instmap::in,
    prog_var::in) is semidet.

nonlocal_is_virgin_output(ModuleInfo, InstMap, Var) :-
    instmap_lookup_var(InstMap, Var, Inst),
    not inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred demand_var(containing_goal_map::in, goal_id::in,
    where_needed::in, prog_var::in,
    where_needed_map::in, where_needed_map::out) is det.

demand_var(ContainingGoalMap, CurrentId, WhereNeeded, Var, !WhereNeededMap) :-
    ( if map.search(!.WhereNeededMap, Var, Where0) then
        where_needed_upper_bound(ContainingGoalMap, CurrentId,
            WhereNeeded, Where0, Where),
        map.det_update(Var, Where, !WhereNeededMap)
    else
        map.det_insert(Var, WhereNeeded, !WhereNeededMap)
    ).

:- pred undemand_var(prog_var::in,
    where_needed_map::in, where_needed_map::out) is det.

undemand_var(Var, !WhereNeededMap) :-
    map.delete(Var, !WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred demand_var_everywhere(where_needed::in, where_needed::out) is det.

demand_var_everywhere(_WhereNeeded0, everywhere).

%---------------------------------------------------------------------------%

:- pred unneeded_process_goal_internal(unneeded_code_info::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_goal_internal(UnneededInfo, Goal0, Goal,
        InitInstMap, FinalInstMap, !WhereNeededMap, !RefinedGoals, !Changed) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0,
        demand_inputs(UnneededInfo, Goal, InitInstMap, everywhere,
            !WhereNeededMap)
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        (
            ConjType = plain_conj,
            unneeded_process_conj(UnneededInfo, Conjuncts0, Conjuncts,
                InitInstMap, FinalInstMap,
                !WhereNeededMap, !RefinedGoals, !Changed),
            GoalExpr = conj(plain_conj, Conjuncts),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            demand_inputs(UnneededInfo, Goal, InitInstMap, everywhere,
                !WhereNeededMap)
        )
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
        ( if
            Cases0 = [FirstCase0 | _],
            FirstCase0 = case(_, _, FirstCaseGoal0),
            FirstCaseGoal0 = hlds_goal(_, FirstCaseGoalInfo0),
            FirstCaseGoalId0 = goal_info_get_goal_id(FirstCaseGoalInfo0),
            map.lookup(ContainingGoalMap, FirstCaseGoalId0, GoalContaining0),
            GoalContaining0 = containing_goal(_ContainingGoalId,
                FirstCaseLastStep),
            FirstCaseLastStep = step_switch(_, MaybeNumAltPrime)
        then
            MaybeNumAlt = MaybeNumAltPrime
        else
            unexpected($pred, "switch count")
        ),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        BranchPoint = branch_point(GoalId, alt_switch(MaybeNumAlt)),
        map.map_values_only(demand_var_everywhere, !WhereNeededMap),
        map.init(BranchNeededMap0),
        unneeded_process_cases(UnneededInfo, Cases0, Cases, BranchPoint, 1,
            InitInstMap, FinalInstMap, GoalId,
            !.WhereNeededMap, BranchNeededMap0, BranchNeededMap,
            !RefinedGoals, !Changed),
        merge_where_needed_maps(ContainingGoalMap, GoalId, !.WhereNeededMap,
            BranchNeededMap, !:WhereNeededMap),
        demand_var(ContainingGoalMap, GoalId, everywhere, SwitchVar,
            !WhereNeededMap),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        map.map_values_only(demand_var_everywhere, !WhereNeededMap),
        unneeded_process_disj(UnneededInfo, Disjuncts0, Disjuncts,
            InitInstMap, FinalInstMap, GoalId,
            !.WhereNeededMap, !.WhereNeededMap, !:WhereNeededMap,
            !RefinedGoals, !Changed),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        BranchPoint = branch_point(GoalId, alt_ite),
        map.map_values_only(demand_var_everywhere, !WhereNeededMap),
        unneeded_process_ite(UnneededInfo, Cond0, Cond,
            Then0, Then, Else0, Else, BranchPoint, InitInstMap, FinalInstMap,
            GoalId, !WhereNeededMap, !RefinedGoals, !Changed),
        GoalExpr = if_then_else(Quant, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        unneeded_process_goal(UnneededInfo, NegGoal0, NegGoal,
            InitInstMap, FinalInstMap,
            !WhereNeededMap, !RefinedGoals, !Changed),
        GoalExpr = negation(NegGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            Goal = Goal0
        else
            unneeded_process_goal(UnneededInfo, SomeGoal0, SomeGoal,
                InitInstMap, FinalInstMap,
                !WhereNeededMap, !RefinedGoals, !Changed),
            GoalExpr = scope(Reason, SomeGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%---------------------------------------------------------------------------%

:- type bracketed_goal
    --->    bracketed_goal(hlds_goal, instmap, instmap).

:- pred unneeded_process_conj(unneeded_code_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out, instmap::in, instmap::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_conj(UnneededInfo, Goals0, Goals, InitInstMap, _FinalInstMap,
        !WhereNeededMap, !RefinedGoals, !Changed) :-
    build_bracketed_conj(Goals0, InitInstMap, BracketedGoals),
    list.reverse(BracketedGoals, RevBracketedGoals),
    unneeded_process_rev_bracketed_conj(UnneededInfo,
        RevBracketedGoals, RevGoals, !WhereNeededMap, !RefinedGoals, !Changed),
    list.reverse(RevGoals, Goals).

:- pred build_bracketed_conj(list(hlds_goal)::in, instmap::in,
    list(bracketed_goal)::out) is det.

build_bracketed_conj([], _, []).
build_bracketed_conj([Goal | Goals], InitInstMap, BracketedGoals) :-
    ( if instmap_is_unreachable(InitInstMap) then
        BracketedGoals = []
    else
        Goal = hlds_goal(_, GoalInfo),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        apply_instmap_delta(InstMapDelta, InitInstMap, FinalInstMap),
        build_bracketed_conj(Goals, FinalInstMap, BracketedTail),
        BracketedGoal = bracketed_goal(Goal, InitInstMap, FinalInstMap),
        BracketedGoals = [BracketedGoal | BracketedTail]
    ).

:- pred unneeded_process_rev_bracketed_conj(unneeded_code_info::in,
    list(bracketed_goal)::in, list(hlds_goal)::out,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_rev_bracketed_conj(_, [], [],
        !WhereNeededMap, !RefinedGoals, !Changed).
unneeded_process_rev_bracketed_conj(UnneededInfo,
        [BracketedGoal | BracketedGoals], Goals,
        !WhereNeededMap, !RefinedGoals, !Changed) :-
    BracketedGoal = bracketed_goal(Goal0, InitInstMap, FinalInstMap),
    unneeded_process_goal(UnneededInfo, Goal0, Goal1,
        InitInstMap, FinalInstMap, !WhereNeededMap, !RefinedGoals, !Changed),
    unneeded_process_rev_bracketed_conj(UnneededInfo, BracketedGoals, Goals1,
        !WhereNeededMap, !RefinedGoals, !Changed),
    ( if Goal1 = hlds_goal(true_goal_expr, _) then
        Goals = Goals1
    else
        Goals = [Goal1 | Goals1]
    ).

%---------------------------------------------------------------------------%

:- pred unneeded_process_disj(unneeded_code_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, instmap::in, goal_id::in,
    where_needed_map::in, where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_disj(_, [], [], _, _, _, _,
        !WhereNeededMap, !RefinedGoals, !Changed).
unneeded_process_disj(UnneededInfo, [Goal0 | Goals0], [Goal | Goals],
        InitInstMap, FinalInstMap, CurrentId,
        StartWhereNeededMap, !WhereNeededMap, !RefinedGoals, !Changed) :-
    unneeded_process_goal(UnneededInfo, Goal0, Goal, InitInstMap, FinalInstMap,
        StartWhereNeededMap, WhereNeededMapFirst, !RefinedGoals, !Changed),
    map.to_assoc_list(WhereNeededMapFirst, WhereNeededList),
    ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
    add_where_needed_list(ContainingGoalMap, WhereNeededList, CurrentId,
        !WhereNeededMap),
    unneeded_process_disj(UnneededInfo, Goals0, Goals,
        InitInstMap, FinalInstMap, CurrentId, StartWhereNeededMap,
        !WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

:- pred unneeded_process_cases(unneeded_code_info::in,
    list(case)::in, list(case)::out, branch_point::in, int::in,
    instmap::in, instmap::in, goal_id::in,
    where_needed_map::in, where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out,
    bool::in, bool::out) is det.

unneeded_process_cases(_, [], [], _, _, _, _, _, _,
        !WhereNeededMap, !RefinedGoals, !Changed).
unneeded_process_cases(UnneededInfo, [Case0 | Cases0], [Case | Cases],
        BranchPoint, BranchNum, InitInstMap, FinalInstMap, CurrentId,
        StartWhereNeededMap, !WhereNeededMap, !RefinedGoals, !Changed) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    unneeded_process_goal(UnneededInfo, Goal0, Goal, InitInstMap, FinalInstMap,
        StartWhereNeededMap, WhereNeededMapFirst, !RefinedGoals, !Changed),
    Case = case(MainConsId, OtherConsIds, Goal),
    map.to_assoc_list(WhereNeededMapFirst, WhereNeededList),
    ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
    add_alt_start(ContainingGoalMap, WhereNeededList, BranchPoint, BranchNum,
        CurrentId, !WhereNeededMap),
    unneeded_process_cases(UnneededInfo, Cases0, Cases,
        BranchPoint, BranchNum + 1, InitInstMap, FinalInstMap, CurrentId,
        StartWhereNeededMap, !WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

:- pred unneeded_process_ite(unneeded_code_info::in,
    hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    branch_point::in, instmap::in, instmap::in, goal_id::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

unneeded_process_ite(UnneededInfo, Cond0, Cond, Then0, Then, Else0, Else,
        BranchPoint, InitInstMap, FinalInstMap, CurrentId,
        !WhereNeededMap, !RefinedGoals, !Changed) :-
    Cond0 = hlds_goal(_, CondInfo0),
    InstMapDelta = goal_info_get_instmap_delta(CondInfo0),
    apply_instmap_delta(InstMapDelta, InitInstMap, InstMapCond),

    unneeded_process_goal(UnneededInfo, Else0, Else, InitInstMap, FinalInstMap,
        !.WhereNeededMap, WhereNeededMapElse, !RefinedGoals, !Changed),
    unneeded_process_goal(UnneededInfo, Then0, Then, InstMapCond, FinalInstMap,
        !.WhereNeededMap, WhereNeededMapThen, !RefinedGoals, !Changed),

    ContainingGoalMap = UnneededInfo ^ uci_containing_goal_map,
    map.init(BranchNeededMap0),
    map.to_assoc_list(WhereNeededMapElse, WhereNeededListElse),
    add_alt_start(ContainingGoalMap, WhereNeededListElse, BranchPoint, 2,
        CurrentId, BranchNeededMap0, BranchNeededMap1),
    map.to_assoc_list(WhereNeededMapThen, WhereNeededListThen),
    add_alt_start(ContainingGoalMap, WhereNeededListThen, BranchPoint, 1,
        CurrentId, BranchNeededMap1, BranchNeededMap),
    merge_where_needed_maps(ContainingGoalMap, CurrentId,
        !.WhereNeededMap, BranchNeededMap, WhereNeededMapCond),

    unneeded_process_goal(UnneededInfo, Cond0, Cond, InitInstMap, InstMapCond,
        WhereNeededMapCond, !:WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

    % Merge two where_needed_maps, so that if var V is needed at branch B
    % in the resulting where_needed_map iff it is needed there in one of
    % the input maps.
    %
:- pred merge_where_needed_maps(containing_goal_map::in, goal_id::in,
    where_needed_map::in, where_needed_map::in, where_needed_map::out) is det.

merge_where_needed_maps(ContainingGoalMap, CurrentId,
        WhereNeededMap1, WhereNeededMap2, WhereNeededMap) :-
    map.to_assoc_list(WhereNeededMap1, WhereNeededList1),
    add_where_needed_list(ContainingGoalMap, WhereNeededList1, CurrentId,
        WhereNeededMap2, WhereNeededMap).

:- pred add_where_needed_list(containing_goal_map::in,
    assoc_list(prog_var, where_needed)::in, goal_id::in,
    where_needed_map::in, where_needed_map::out) is det.

add_where_needed_list(_, [], _, !WhereNeededMap).
add_where_needed_list(ContainingGoalMap, [Var - BranchWhere | WhereNeededList],
        CurrentId, !WhereNeededMap) :-
    ( if map.search(!.WhereNeededMap, Var, OldWhere) then
        where_needed_upper_bound(ContainingGoalMap, CurrentId,
            BranchWhere, OldWhere, CombinedWhere),
        map.det_update(Var, CombinedWhere, !WhereNeededMap)
    else
        map.det_insert(Var, BranchWhere, !WhereNeededMap)
    ),
    add_where_needed_list(ContainingGoalMap, WhereNeededList, CurrentId,
        !WhereNeededMap).

    % Given a where_needed_map, add to it the where_needed information for the
    % start of an alternative in a branched goal. This source is important,
    % because if the analysis *at the start of an alternative* says that the
    % variable is needed everywhere, the scope of this "everywhere" is only
    % that alternative.
    %
:- pred add_alt_start(containing_goal_map::in,
    assoc_list(prog_var, where_needed)::in, branch_point::in,
    int::in, goal_id::in, where_needed_map::in, where_needed_map::out) is det.

add_alt_start(_, [], _, _, _, !WhereNeededMap).
add_alt_start(ContainingGoalMap, [Var - BranchWhere0 | WhereNeededList],
        BranchPoint, BranchNum, CurrentId, !WhereNeededMap) :-
    (
        BranchWhere0 = everywhere,
        BranchNumSet = set.make_singleton_set(BranchNum),
        BranchMap = map.singleton(BranchPoint, BranchNumSet),
        BranchWhere = branches(BranchMap)
    ;
        BranchWhere0 = branches(_),
        BranchWhere = BranchWhere0
    ),
    ( if map.search(!.WhereNeededMap, Var, OldWhere) then
        where_needed_upper_bound(ContainingGoalMap, CurrentId,
            BranchWhere, OldWhere, CombinedWhere),
        map.det_update(Var, CombinedWhere, !WhereNeededMap)
    else
        map.det_insert(Var, BranchWhere, !WhereNeededMap)
    ),
    add_alt_start(ContainingGoalMap, WhereNeededList, BranchPoint, BranchNum,
        CurrentId, !WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred unneeded_refine_goal(hlds_goal::in, hlds_goal::out,
    refined_goal_map::in, refined_goal_map::out) is det.

unneeded_refine_goal(Goal0, Goal, !RefinedGoals) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        (
            ConjType = plain_conj,
            unneeded_refine_conj(Conjuncts0, Conjuncts, !RefinedGoals),
            GoalExpr = conj(ConjType, Conjuncts),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ConjType = parallel_conj,
            Goal = Goal0
        )
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        unneeded_refine_cases(Cases0, Cases, !RefinedGoals, GoalId, 1),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        unneeded_refine_disj(Disjuncts0, Disjuncts, !RefinedGoals,
            GoalId, 1),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        unneeded_refine_ite(Cond0, Cond, Then0, Then, Else0, Else,
            !RefinedGoals, GoalId),
        GoalExpr = if_then_else(Quant, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        unneeded_refine_goal(NegGoal0, NegGoal, !RefinedGoals),
        GoalExpr = negation(NegGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            Goal = Goal0
        else
            unneeded_refine_goal(SomeGoal0, SomeGoal, !RefinedGoals),
            GoalExpr = scope(Reason, SomeGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred unneeded_refine_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    refined_goal_map::in, refined_goal_map::out) is det.

unneeded_refine_conj([], [], !RefinedGoals).
unneeded_refine_conj([Goal0 | Goals0], Goals, !RefinedGoals) :-
    unneeded_refine_goal(Goal0, HeadGoal, !RefinedGoals),
    unneeded_refine_conj(Goals0, TailGoals, !RefinedGoals),
    ( if HeadGoal = hlds_goal(conj(plain_conj, HeadGoals), _) then
        Goals = HeadGoals ++ TailGoals
    else
        Goals = [HeadGoal | TailGoals]
    ).

:- pred unneeded_refine_cases(list(case)::in, list(case)::out,
    refined_goal_map::in, refined_goal_map::out,
    goal_id::in, int::in) is det.

unneeded_refine_cases([], [], !RefinedGoals, _, _).
unneeded_refine_cases([Case0 | Cases0], [Case | Cases], !RefinedGoals,
        GoalId, BranchNum) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    unneeded_refine_goal(Goal0, Goal1, !RefinedGoals),
    ( if map.search(!.RefinedGoals, GoalId - BranchNum, ToInsertGoals) then
        insert_refine_goals(ToInsertGoals, Goal1, Goal),
        map.delete(GoalId - BranchNum, !RefinedGoals)
    else
        Goal = Goal1
    ),
    Case = case(MainConsId, OtherConsIds, Goal),
    unneeded_refine_cases(Cases0, Cases, !RefinedGoals,
        GoalId, BranchNum + 1).

:- pred unneeded_refine_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    refined_goal_map::in, refined_goal_map::out,
    goal_id::in, int::in) is det.

unneeded_refine_disj([], [], !RefinedGoals, _, _).
unneeded_refine_disj([Goal0 | Goals0], [Goal | Goals], !RefinedGoals,
        GoalId, BranchNum) :-
    unneeded_refine_goal(Goal0, Goal1, !RefinedGoals),
    ( if map.search(!.RefinedGoals, GoalId - BranchNum, ToInsertGoals) then
        insert_refine_goals(ToInsertGoals, Goal1, Goal),
        map.delete(GoalId - BranchNum, !RefinedGoals)
    else
        Goal = Goal1
    ),
    unneeded_refine_disj(Goals0, Goals, !RefinedGoals,
        GoalId, BranchNum + 1).

:- pred unneeded_refine_ite(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    refined_goal_map::in, refined_goal_map::out, goal_id::in) is det.

unneeded_refine_ite(Cond0, Cond, Then0, Then, Else0, Else,
        !RefinedGoals, GoalId) :-
    unneeded_refine_goal(Cond0, Cond, !RefinedGoals),
    unneeded_refine_goal(Then0, Then1, !RefinedGoals),
    unneeded_refine_goal(Else0, Else1, !RefinedGoals),

    ( if map.search(!.RefinedGoals, GoalId - 1, ToInsertGoalsThen) then
        insert_refine_goals(ToInsertGoalsThen, Then1, Then),
        map.delete(GoalId - 1, !RefinedGoals)
    else
        Then = Then1
    ),
    ( if map.search(!.RefinedGoals, GoalId - 2, ToInsertGoalsElse) then
        insert_refine_goals(ToInsertGoalsElse, Else1, Else),
        map.delete(GoalId - 2, !RefinedGoals)
    else
        Else = Else1
    ).

:- pred insert_refine_goals(list(hlds_goal)::in, hlds_goal::in,
    hlds_goal::out) is det.

insert_refine_goals(ToInsertGoals, Goal0, Goal) :-
    list.append(ToInsertGoals, [Goal0], Conj),
    % XXX GoalInfo0
    Goal0 = hlds_goal(_, GoalInfo0),
    conj_list_to_goal(Conj, GoalInfo0, Goal).

%-----------------------------------------------------------------------------%

    % Given two sets of requirements about where a goal is needed, return
    % a single requirement that contains all the demands. The main purpose
    % of this predicate is to discover when the union of two sets of
    % requirements (e.g. branch sets {b1,b2} and {b3}) covers all
    % computation paths.
    %
:- pred where_needed_upper_bound(containing_goal_map::in, goal_id::in,
    where_needed::in, where_needed::in, where_needed::out) is det.

where_needed_upper_bound(ContainingGoalMap, CurrentId,
        WhereNeededA, WhereNeededB, WhereNeeded) :-
    (
        WhereNeededA = everywhere,
        WhereNeeded = everywhere
    ;
        WhereNeededA = branches(BranchesA),
        (
            WhereNeededB = everywhere,
            WhereNeeded = everywhere
        ;
            WhereNeededB = branches(BranchesB),
            where_needed_branches_upper_bound(ContainingGoalMap, CurrentId,
                BranchesA, BranchesB, WhereNeeded)
        )
    ).

:- pred where_needed_branches_upper_bound(containing_goal_map::in, goal_id::in,
    where_needed_branches::in, where_needed_branches::in, where_needed::out)
    is det.

where_needed_branches_upper_bound(ContainingGoalMap, CurrentId,
        BranchesA, BranchesB, WhereNeeded) :-
    % We should select the smaller map to convert to list.
    map.to_assoc_list(BranchesA, BranchesList),
    where_needed_branches_upper_bound_2(ContainingGoalMap, CurrentId,
        BranchesList, BranchesB, WhereNeeded).

:- pred where_needed_branches_upper_bound_2(containing_goal_map::in,
    goal_id::in, assoc_list(branch_point, set(int))::in,
    where_needed_branches::in, where_needed::out) is det.

where_needed_branches_upper_bound_2(_, _, [],
        Branches, branches(Branches)).
where_needed_branches_upper_bound_2(ContainingGoalMap, CurrentId,
        [First | Rest], Branches0, WhereNeeded) :-
    First = BranchPoint - NewAlts,
    ( if map.search(Branches0, BranchPoint, OldAlts) then
        set.union(OldAlts, NewAlts, Alts),
        BranchPoint = branch_point(BranchGoalId, BranchAlts),
        ( if branch_point_is_complete(BranchAlts, Alts) then
            ( if
                get_parent_branch_point(ContainingGoalMap, BranchGoalId,
                    ParentBranchGoalId, ParentBranchArmGoalId,
                    ParentBranchAlt, ParentBranchNum),
                not goal_id_inside(ContainingGoalMap, ParentBranchArmGoalId,
                    CurrentId)
            then
                map.delete(BranchPoint, Branches0, Branches1),
                ParentBranchPoint = branch_point(ParentBranchGoalId,
                    ParentBranchAlt),
                ParentAlts = set.make_singleton_set(ParentBranchNum),
                where_needed_branches_upper_bound_2(ContainingGoalMap,
                    CurrentId, [ParentBranchPoint - ParentAlts | Rest],
                    Branches1, WhereNeeded)
            else
                WhereNeeded = everywhere
            )
        else
            map.det_update(BranchPoint, Alts, Branches0, Branches1),
            where_needed_branches_upper_bound_2(ContainingGoalMap, CurrentId,
                Rest, Branches1, WhereNeeded)
        )
    else
        map.det_insert(BranchPoint, NewAlts, Branches0, Branches1),
        where_needed_branches_upper_bound_2(ContainingGoalMap, CurrentId,
            Rest, Branches1, WhereNeeded)
    ).

:- pred get_parent_branch_point(containing_goal_map::in, goal_id::in,
    goal_id::out, goal_id::out, branch_alts::out, int::out) is semidet.

get_parent_branch_point(ContainingGoalMap, GoalId, BranchGoalId,
        BranchArmGoalId, BranchAlt, BranchNum) :-
    map.lookup(ContainingGoalMap, GoalId, GoalContaining),
    GoalContaining = containing_goal(ContainingGoalId, LastStep),
    require_complete_switch [LastStep]
    (
        LastStep = step_switch(Arm, MaybeNumAlts),
        BranchGoalId = ContainingGoalId,
        BranchArmGoalId = GoalId,
        BranchAlt = alt_switch(MaybeNumAlts),
        BranchNum = Arm
    ;
        LastStep = step_ite_then,
        BranchGoalId = ContainingGoalId,
        BranchArmGoalId = GoalId,
        BranchAlt = alt_ite,
        BranchNum = 1
    ;
        LastStep = step_ite_else,
        BranchGoalId = ContainingGoalId,
        BranchArmGoalId = GoalId,
        BranchAlt = alt_ite,
        BranchNum = 2
    ;
        ( LastStep = step_ite_cond
        ; LastStep = step_neg
        ; LastStep = step_scope(_)
        ; LastStep = step_conj(_)
        ; LastStep = step_disj(_)
        ),
        get_parent_branch_point(ContainingGoalMap, ContainingGoalId,
            BranchGoalId, BranchArmGoalId, BranchAlt, BranchNum)
    ;
        % The optimization has not yet been extended to handle atomic goals.
        ( LastStep = step_atomic_main
        ; LastStep = step_atomic_orelse(_)
        ),
        fail
    ;
        % Lambdas and try scopes should have been expanded out
        % by the time this optimization is invoked.
        ( LastStep = step_lambda
        ; LastStep = step_try
        ),
        fail
    ).

:- pred branch_point_is_complete(branch_alts::in, set(int)::in) is semidet.

branch_point_is_complete(BranchAlts, Alts) :-
    (
        BranchAlts = alt_ite,
        set.count(Alts, NumAlts),
        NumAlts = 2
    ;
        BranchAlts = alt_switch(MaybeSwitchNumFunctors),
        MaybeSwitchNumFunctors = known_num_functors_in_type(NumFunctors),
        set.count(Alts, NumAlts),
        NumAlts = NumFunctors
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.unneeded_code.
%---------------------------------------------------------------------------%
