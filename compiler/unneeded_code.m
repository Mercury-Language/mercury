%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: unneeded_code.m.
% Author: zs.

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

%-----------------------------------------------------------------------------%

:- module transform_hlds.unneeded_code.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred process_proc_msg(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module svmap.

%-----------------------------------------------------------------------------%

    % The branch_alts and branch_point types record the information the
    % transform needs to know about a particular branched control
    % structure: where it is, what kind it is, and how many alternatives
    % it has.
    %
:- type branch_point
    --->    branch_point(
                goal_path,  % The position of the branch point.
                branch_alts % What kind of goal the branch point
                            % is, and many branches it has.
                            % Note that the second argument is a
                            % function of the first.
            ).

:- type branch_alts
    --->    ite             % If-then-elses always have two alternatives:
                            % the then branch (numbered 1) and the else branch
                            % (numbered 2).

    ;       switch(int).    % The number of alternatives in a switch is equal
                            % to the number of function symbols in the type of
                            % the switched-on variable; this number is given by
                            % the argument. If the switch cannot_fail, then
                            % this will be equal to the number of cases; if
                            % the switch can_fail, there will be strictly
                            % fewer cases than this.

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
    % can be moved to the starts of those branches.
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
    %       ( Y = f ->
    %           ... code that needs only Z ...
    %       ;
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

:- type refined_goal_map == map(pair(goal_path, int), list(hlds_goal)).

%-----------------------------------------------------------------------------%

process_proc_msg(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    % The transformation considers every nonlocal variable of a goal
    % that is bound on entry to be consumed by that goal. If the nonlocal set
    % contains any such variables that are not actually needed by the goal,
    % then the transformation will not be as effective as it could be.
    % Therefore we preprocess the procedure body to ensure that the nonlocals
    % sets are accurate reflections of the true needs of goals.

    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io.write_string("% Removing dead code in ", !IO),
        hlds_out.write_pred_proc_id(!.ModuleInfo, PredId, ProcId, !IO),
        io.write_string(": ", !IO),
        pre_process_proc(!ProcInfo),
        process_proc(!ProcInfo, !ModuleInfo, Successful),
        (
            Successful = yes,
            io.write_string("done.\n", !IO)
        ;
            Successful = no,
            io.write_string("none found.\n", !IO)
        )
    ;
        VeryVerbose = no,
        pre_process_proc(!ProcInfo),
        process_proc(!ProcInfo, !ModuleInfo, _)
    ).

:- pred pre_process_proc(proc_info::in, proc_info::out) is det.

pre_process_proc(!ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    implicitly_quantify_clause_body(HeadVars, _Warnings, Goal0, Goal,
        Varset0, Varset, VarTypes0, VarTypes),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

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

:- type option_values
    --->    option_values(
                fully_strict    ::  bool,
                reorder_conj    ::  bool,
                copy_limit      ::  int
            ).

:- pred process_proc(proc_info::in, proc_info::out,
    module_info::in, module_info::out, bool::out) is det.

process_proc(!ProcInfo, !ModuleInfo, Successful) :-
    fill_goal_path_slots(!.ModuleInfo, !ProcInfo),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
    Goal0 = _ - GoalInfo0,
    goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
    instmap.apply_instmap_delta(InitInstMap, InstMapDelta, FinalInstMap),
    proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo, NeededVarsList),
    map.init(WhereNeededMap0),
    NeededEverywhere =
        (pred(Var::in, NeededMap0::in, NeededMap::out) is det :-
            map.det_insert(NeededMap0, Var, everywhere, NeededMap)
        ),
    list.foldl(NeededEverywhere, NeededVarsList,
        WhereNeededMap0, WhereNeededMap1),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, reorder_conj, ReorderConj),
    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    globals.lookup_int_option(Globals, unneeded_code_copy_limit, Limit),
    Options = option_values(FullyStrict, ReorderConj, Limit),
    process_goal(Goal0, Goal1, InitInstMap, FinalInstMap, VarTypes0,
        !.ModuleInfo, Options, WhereNeededMap1, _, map.init, RefinedGoals1,
        no, Changed),
    refine_goal(Goal1, Goal2, RefinedGoals1, RefinedGoals),
    expect(map.is_empty(RefinedGoals),
        this_file, "process_proc: goal reattachment unsuccessful"),
    (
        Changed = yes,
            % We need to fix up the goal_info by recalculating
            % the nonlocal vars and the non-atomic instmap deltas.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        implicitly_quantify_clause_body(HeadVars, _Warnings,
            Goal2, Goal3, Varset0, Varset, VarTypes0, VarTypes),
        recompute_instmap_delta(no, Goal3, Goal, VarTypes, InstVarSet,
            InitInstMap, !ModuleInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset(Varset, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        process_proc(!ProcInfo, !ModuleInfo, _),
        Successful = yes
    ;
        Changed = no,
        Successful = no
    ).

:- pred process_goal(hlds_goal::in, hlds_goal::out, instmap::in, instmap::in,
    vartypes::in, module_info::in, option_values::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_goal(Goal0, Goal, InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        Options, !WhereNeededMap, !RefinedGoals, !Changed) :-
    can_eliminate_or_move(Goal0, InitInstMap, FinalInstMap,
        VarTypes, ModuleInfo, Options, !.WhereNeededMap, WhereInfo),
    (
        WhereInfo = everywhere,
        process_goal_internal(Goal0, Goal, InitInstMap, FinalInstMap, VarTypes,
            ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed)
    ;
        WhereInfo = branches(Branches),
        demand_inputs(Goal0, ModuleInfo, InitInstMap, WhereInfo,
            !WhereNeededMap),
        map.to_assoc_list(Branches, BranchList),
        list.foldl(insert_branch_into_refined_goals(Goal0), BranchList,
            !RefinedGoals),
        Goal = true_goal,
        !:Changed = yes
    ),
    undemand_virgin_outputs(Goal0, ModuleInfo, InitInstMap,
        !WhereNeededMap),
    (
        goal_get_purity(Goal, Purity),
        Purity = purity_impure
    ->
        % By saying that all vars that are live before the impure goal are
        % needed everywhere, we prevent the movement of the goals producing
        % those vars across the impure goal.
        %
        % This code requires compound goals containing impure code
        % to also be marked impure.
        map.map_values(demand_var_everywhere, !WhereNeededMap)
    ;
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

:- pred insert_branch_arm_into_refined_goals(hlds_goal::in, goal_path::in,
    int::in, refined_goal_map::in, refined_goal_map::out) is det.

insert_branch_arm_into_refined_goals(Goal, GoalPath, BranchNum,
        !RefinedGoals) :-
    Key = GoalPath - BranchNum,
    ( map.search(!.RefinedGoals, Key, Goals0) ->
        Goals = [Goal | Goals0],
        map.det_update(!.RefinedGoals, Key, Goals, !:RefinedGoals)
    ;
        map.det_insert(!.RefinedGoals, Key, [Goal], !:RefinedGoals)
    ).

%-----------------------------------------------------------------------------%

:- pred can_eliminate_or_move(hlds_goal::in, instmap::in,
    instmap::in, vartypes::in, module_info::in, option_values::in,
    where_needed_map::in, where_needed::out) is det.

can_eliminate_or_move(Goal, InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        Options, WhereNeededMap, !:WhereInfo) :-
    instmap_changed_vars(InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        ChangedVarSet),
    set.to_sorted_list(ChangedVarSet, ChangedVars),
    map.init(Empty),
    !:WhereInfo = branches(Empty),
    Goal = _ - GoalInfo,
    goal_info_get_goal_path(GoalInfo, CurrentPath),
    list.foldl(collect_where_needed(CurrentPath, WhereNeededMap), ChangedVars,
        !WhereInfo),
    adjust_where_needed(Goal, Options, !WhereInfo).

:- pred collect_where_needed(goal_path::in, where_needed_map::in, prog_var::in,
    where_needed::in, where_needed::out) is det.

collect_where_needed(CurrentPath, WhereNeededMap, ChangedVar, !WhereInfo) :-
    ( map.search(WhereNeededMap, ChangedVar, Where) ->
        where_needed_upper_bound(CurrentPath, Where, !WhereInfo)
    ;
        true
    ).

    % This is the predicate responsible for ensuring that the act of optimizing
    % away the execution of a goal on some or all computation paths changes the
    % operational semantics only in ways that are explicitly permitted by the
    % programmer.
    %
:- pred adjust_where_needed(hlds_goal::in, option_values::in,
    where_needed::in, where_needed::out) is det.

adjust_where_needed(Goal, Options, !WhereInfo) :-
    (
        Goal = GoalExpr - GoalInfo,
        (
            % Do not move goals that can fail, since doing so can cause
            % execution to reach goals it shouldn't, and those goals may have
            % undesirable behavior (e.g. infinite loops).
            goal_info_get_determinism(GoalInfo, Detism),
            detism_is_moveable(Detism, no)
        ;
            % Do not move impure or semipure goals, since their ordering
            % wrt other such goals must be preserved.
            goal_info_get_purity(GoalInfo, Purity),
            Purity \= purity_pure
        ;
            % With --fully-strict, we cannot optimize away infinite loops
            % or exceptions.
            Options ^ fully_strict = yes,
            goal_can_loop_or_throw(Goal)
        ;
            % With --no-reorder-conj, we cannot move infinite loops or
            % exceptions, but we can delete them.
            Options ^ reorder_conj = no,
            goal_can_loop_or_throw(Goal),
            !.WhereInfo = branches(BranchMap),
            \+ map.is_empty(BranchMap)
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
            BranchArmCount > Options ^ copy_limit

            % We may also want to add other space time tradeoffs. E.g. if
            % profiling shows that Goal is required in 10 branches that
            % account for 99% of all executions and is not required in 5
            % branches that account for the remaining 1%, and Goal itself
            % is sufficiently cheap to execute, then not moving Goal may cost
            % a small slowdown in 1% of cases but avoid 9 extra copies of Goal.
            % Due to better instruction cache behavior, not moving Goal
            % may in fact yield faster code after all.
        )
    ->
        !:WhereInfo = everywhere
    ;
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

:- pred demand_inputs(hlds_goal::in, module_info::in,
    instmap::in, where_needed::in,
    where_needed_map::in, where_needed_map::out) is det.

demand_inputs(Goal, ModuleInfo, InitInstMap, WhereNeeded, !WhereNeededMap) :-
    Goal = _ - GoalInfo,
    goal_info_get_nonlocals(GoalInfo, NonLocalSet),
    goal_info_get_goal_path(GoalInfo, GoalPath),
    set.to_sorted_list(NonLocalSet, NonLocals),
    list.filter(nonlocal_may_be_input(ModuleInfo, InitInstMap), NonLocals,
        Inputs),
    list.foldl(demand_var(GoalPath, WhereNeeded), Inputs, !WhereNeededMap).

:- pred nonlocal_may_be_input(module_info::in, instmap::in,
    prog_var::in) is semidet.

nonlocal_may_be_input(ModuleInfo, InstMap, Var) :-
    instmap.lookup_var(InstMap, Var, Inst),
    inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred undemand_virgin_outputs(hlds_goal::in, module_info::in,
    instmap::in, where_needed_map::in, where_needed_map::out) is det.

undemand_virgin_outputs(Goal, ModuleInfo, InstMap, !WhereNeededMap) :-
    Goal = _ - GoalInfo,
    goal_info_get_nonlocals(GoalInfo, NonLocalSet),
    set.to_sorted_list(NonLocalSet, NonLocals),
    list.filter(nonlocal_is_virgin_output(ModuleInfo, InstMap), NonLocals,
        VirginOutputs),
    list.foldl(undemand_var, VirginOutputs, !WhereNeededMap).

:- pred nonlocal_is_virgin_output(module_info::in, instmap::in,
    prog_var::in) is semidet.

nonlocal_is_virgin_output(ModuleInfo, InstMap, Var) :-
    instmap.lookup_var(InstMap, Var, Inst),
    \+ inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred demand_var(goal_path::in, where_needed::in,
    prog_var::in, where_needed_map::in, where_needed_map::out) is det.

demand_var(CurrentPath, WhereNeeded, Var, !WhereNeededMap) :-
    ( map.search(!.WhereNeededMap, Var, Where0) ->
        where_needed_upper_bound(CurrentPath, WhereNeeded, Where0, Where),
        svmap.det_update(Var, Where, !WhereNeededMap)
    ;
        svmap.det_insert(Var, WhereNeeded, !WhereNeededMap)
    ).

:- pred undemand_var(prog_var::in,
    where_needed_map::in, where_needed_map::out) is det.

undemand_var(Var, WhereNeededMap0, WhereNeededMap) :-
    map.delete(WhereNeededMap0, Var, WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred demand_var_everywhere(prog_var::in, where_needed::in,
    where_needed::out) is det.

demand_var_everywhere(_Var, _WhereNeeded0, everywhere).

%---------------------------------------------------------------------------%

:- pred process_goal_internal(hlds_goal::in, hlds_goal::out,
    instmap::in, instmap::in, vartypes::in, module_info::in,
    option_values::in, where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_goal_internal(Goal0, Goal, InitInstMap, FinalInstMap, VarTypes,
        ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    (
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal0,
        demand_inputs(Goal, ModuleInfo, InitInstMap, everywhere,
            !WhereNeededMap)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        Goal = Goal0,
        demand_inputs(Goal, ModuleInfo, InitInstMap, everywhere,
            !WhereNeededMap)
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        Goal = Goal0,
        demand_inputs(Goal, ModuleInfo, InitInstMap, everywhere,
            !WhereNeededMap)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = Goal0,
        demand_inputs(Goal, ModuleInfo, InitInstMap, everywhere,
            !WhereNeededMap)
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        (
            ConjType = plain_conj,
            process_conj(Conjuncts0, Conjuncts, InitInstMap, FinalInstMap,
                VarTypes, ModuleInfo, Options, !WhereNeededMap, !RefinedGoals,
                !Changed),
            GoalExpr = conj(plain_conj, Conjuncts),
            Goal = GoalExpr - GoalInfo0
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            demand_inputs(Goal, ModuleInfo, InitInstMap, everywhere,
                !WhereNeededMap)
        )
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        (
            Cases0 = [case(_, _ - FirstCaseGoalInfo) | _],
            goal_info_get_goal_path(FirstCaseGoalInfo, FirstCaseGoalPath),
            FirstCaseGoalPath = [SwitchStep | _],
            SwitchStep = switch(_, NumCases)
        ->
            NumAlt = NumCases
        ;
            unexpected(this_file, "process_goal_internal: switch count")
        ),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        BranchPoint = branch_point(GoalPath, switch(NumAlt)),
        map.map_values(demand_var_everywhere, !WhereNeededMap),
        map.init(BranchNeededMap0),
        process_cases(Cases0, Cases, BranchPoint, 1, InitInstMap, FinalInstMap,
            VarTypes, ModuleInfo, Options, GoalPath, !.WhereNeededMap,
            BranchNeededMap0, BranchNeededMap, !RefinedGoals, !Changed),
        merge_where_needed_maps(GoalPath, !.WhereNeededMap,
            BranchNeededMap, !:WhereNeededMap),
        demand_var(GoalPath, everywhere, SwitchVar, !WhereNeededMap),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = disj(Disjuncts0),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        map.map_values(demand_var_everywhere, !WhereNeededMap),
        process_disj(Disjuncts0, Disjuncts, InitInstMap, FinalInstMap,
            VarTypes, ModuleInfo, Options, GoalPath,
            !.WhereNeededMap, !.WhereNeededMap, !:WhereNeededMap,
            !RefinedGoals, !Changed),
        GoalExpr = disj(Disjuncts),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        BranchPoint = branch_point(GoalPath, ite),
        map.map_values(demand_var_everywhere, !WhereNeededMap),
        process_ite(Cond0, Cond, Then0, Then, Else0, Else, BranchPoint,
            InitInstMap, FinalInstMap, VarTypes, ModuleInfo, Options, GoalPath,
            !WhereNeededMap, !RefinedGoals, !Changed),
        GoalExpr = if_then_else(Quant, Cond, Then, Else),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = negation(NegGoal0),
        process_goal(NegGoal0, NegGoal, InitInstMap, FinalInstMap,
            VarTypes, ModuleInfo, Options,
            !WhereNeededMap, !RefinedGoals, !Changed),
        GoalExpr = negation(NegGoal),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        process_goal(SomeGoal0, SomeGoal, InitInstMap, FinalInstMap, VarTypes,
            ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed),
        GoalExpr = scope(Reason, SomeGoal),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "shorthand in process_goal_internal")
    ).

%---------------------------------------------------------------------------%

:- type bracketed_goal
    --->    bracketed_goal(hlds_goal, instmap, instmap).

:- pred process_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, instmap::in, vartypes::in, module_info::in,
    option_values::in, where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_conj(Goals0, Goals, InitInstMap, _FinalInstMap, VarTypes, ModuleInfo,
        Options, !WhereNeededMap, !RefinedGoals, !Changed) :-
    build_bracketed_conj(Goals0, InitInstMap, BracketedGoals),
    list.reverse(BracketedGoals, RevBracketedGoals),
    process_rev_bracketed_conj(RevBracketedGoals, RevGoals, VarTypes,
        ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed),
    list.reverse(RevGoals, Goals).

:- pred build_bracketed_conj(list(hlds_goal)::in, instmap::in,
    list(bracketed_goal)::out) is det.

build_bracketed_conj([], _, []).
build_bracketed_conj([Goal | Goals], InitInstMap, BracketedGoals) :-
    ( instmap.is_unreachable(InitInstMap) ->
        BracketedGoals = []
    ;
        Goal = _ - GoalInfo,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        instmap.apply_instmap_delta(InitInstMap, InstMapDelta, FinalInstMap),
        build_bracketed_conj(Goals, FinalInstMap, BracketedTail),
        BracketedGoal = bracketed_goal(Goal, InitInstMap, FinalInstMap),
        BracketedGoals = [BracketedGoal | BracketedTail]
    ).

:- pred process_rev_bracketed_conj(list(bracketed_goal)::in,
    list(hlds_goal)::out, vartypes::in, module_info::in, option_values::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_rev_bracketed_conj([], [], _, _, _,
        !WhereNeededMap, !RefinedGoals, !Changed).
process_rev_bracketed_conj([BracketedGoal | BracketedGoals], Goals, VarTypes,
        ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed) :-
    BracketedGoal = bracketed_goal(Goal0, InitInstMap, FinalInstMap),
    process_goal(Goal0, Goal1, InitInstMap, FinalInstMap, VarTypes,
        ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed),
    process_rev_bracketed_conj(BracketedGoals, Goals1, VarTypes,
        ModuleInfo, Options, !WhereNeededMap, !RefinedGoals, !Changed),
    ( Goal1 = true_goal_expr - _ ->
        Goals = Goals1
    ;
        Goals = [Goal1 | Goals1]
    ).

%---------------------------------------------------------------------------%

:- pred process_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, instmap::in, vartypes::in, module_info::in,
    option_values::in, goal_path::in,
    where_needed_map::in, where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_disj([], [], _, _, _, _, _, _, _,
        !WhereNeededMap, !RefinedGoals, !Changed).
process_disj([Goal0 | Goals0], [Goal | Goals], InitInstMap, FinalInstMap,
        VarTypes, ModuleInfo, Options, CurrentPath,
        StartWhereNeededMap, !WhereNeededMap, !RefinedGoals, !Changed) :-
    process_goal(Goal0, Goal, InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        Options, StartWhereNeededMap, WhereNeededMapFirst, !RefinedGoals,
        !Changed),
    map.to_assoc_list(WhereNeededMapFirst, WhereNeededList),
    add_where_needed_list(WhereNeededList, CurrentPath, !WhereNeededMap),
    process_disj(Goals0, Goals, InitInstMap, FinalInstMap, VarTypes,
        ModuleInfo, Options, CurrentPath, StartWhereNeededMap,
        !WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

:- pred process_cases(list(case)::in, list(case)::out, branch_point::in,
    int::in, instmap::in, instmap::in, vartypes::in, module_info::in,
    option_values::in, goal_path::in, where_needed_map::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out,
    bool::in, bool::out) is det.

process_cases([], [], _, _, _, _, _, _, _, _, _,
        !WhereNeededMap, !RefinedGoals, !Changed).
process_cases([case(Var, Goal0) | Cases0], [case(Var, Goal) | Cases],
        BranchPoint, BranchNum, InitInstMap, FinalInstMap, VarTypes,
        ModuleInfo, Options, CurrentPath, StartWhereNeededMap,
        !WhereNeededMap, !RefinedGoals, !Changed) :-
    process_goal(Goal0, Goal, InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        Options, StartWhereNeededMap, WhereNeededMapFirst, !RefinedGoals,
        !Changed),
    map.to_assoc_list(WhereNeededMapFirst, WhereNeededList),
    add_alt_start(WhereNeededList, BranchPoint, BranchNum, CurrentPath,
        !WhereNeededMap),
    process_cases(Cases0, Cases, BranchPoint, BranchNum + 1,
        InitInstMap, FinalInstMap, VarTypes, ModuleInfo, Options, CurrentPath,
        StartWhereNeededMap, !WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

:- pred process_ite(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    branch_point::in, instmap::in, instmap::in, vartypes::in,
    module_info::in, option_values::in, goal_path::in,
    where_needed_map::in, where_needed_map::out,
    refined_goal_map::in, refined_goal_map::out, bool::in, bool::out) is det.

process_ite(Cond0, Cond, Then0, Then, Else0, Else, BranchPoint,
        InitInstMap, FinalInstMap, VarTypes, ModuleInfo, Options,
        CurrentPath, !WhereNeededMap, !RefinedGoals, !Changed) :-
    Cond0 = _ - CondInfo0,
    goal_info_get_instmap_delta(CondInfo0, InstMapDelta),
    instmap.apply_instmap_delta(InitInstMap, InstMapDelta, InstMapCond),

    process_goal(Else0, Else, InitInstMap, FinalInstMap, VarTypes, ModuleInfo,
        Options, !.WhereNeededMap, WhereNeededMapElse, !RefinedGoals,
        !Changed),
    process_goal(Then0, Then, InstMapCond, FinalInstMap, VarTypes, ModuleInfo,
        Options, !.WhereNeededMap, WhereNeededMapThen, !RefinedGoals,
        !Changed),

    map.init(BranchNeededMap0),
    map.to_assoc_list(WhereNeededMapElse, WhereNeededListElse),
    add_alt_start(WhereNeededListElse, BranchPoint, 2,
        CurrentPath, BranchNeededMap0, BranchNeededMap1),
    map.to_assoc_list(WhereNeededMapThen, WhereNeededListThen),
    add_alt_start(WhereNeededListThen, BranchPoint, 1,
        CurrentPath, BranchNeededMap1, BranchNeededMap),
    merge_where_needed_maps(CurrentPath,
        !.WhereNeededMap, BranchNeededMap, WhereNeededMapCond),

    process_goal(Cond0, Cond, InitInstMap, InstMapCond,
        VarTypes, ModuleInfo, Options, WhereNeededMapCond,
        !:WhereNeededMap, !RefinedGoals, !Changed).

%---------------------------------------------------------------------------%

    % Merge two where_needed_maps, so that if var V is needed at branch B
    % in the resulting where_needed_map iff it is needed there in one of
    % the input maps.
    %
:- pred merge_where_needed_maps(goal_path::in,
    where_needed_map::in, where_needed_map::in, where_needed_map::out)
    is det.

merge_where_needed_maps(CurrentPath,
        WhereNeededMap1, WhereNeededMap2, WhereNeededMap) :-
    map.to_assoc_list(WhereNeededMap1, WhereNeededList1),
    add_where_needed_list(WhereNeededList1, CurrentPath,
        WhereNeededMap2, WhereNeededMap).

:- pred add_where_needed_list(assoc_list(prog_var, where_needed)::in,
    goal_path::in, where_needed_map::in, where_needed_map::out) is det.

add_where_needed_list([], _, !WhereNeededMap).
add_where_needed_list([Var - BranchWhere | WhereNeededList],
        CurrentPath, !WhereNeededMap) :-
    ( map.search(!.WhereNeededMap, Var, OldWhere) ->
        where_needed_upper_bound(CurrentPath, BranchWhere, OldWhere,
            CombinedWhere),
        svmap.det_update(Var, CombinedWhere, !WhereNeededMap)
    ;
        svmap.det_insert(Var, BranchWhere, !WhereNeededMap)
    ),
    add_where_needed_list(WhereNeededList, CurrentPath,
        !WhereNeededMap).

    % Given a where_needed_map, add to it the where_needed information for the
    % start of an alternative in a branched goal. This source is important,
    % because if the analysis *at the start of an alternative* says that the
    % variable is needed everywhere, the scope of this "everywhere" is only
    % that alternative.
    %
:- pred add_alt_start(assoc_list(prog_var, where_needed)::in,
    branch_point::in, int::in, goal_path::in,
    where_needed_map::in, where_needed_map::out) is det.

add_alt_start([], _, _, _, !WhereNeededMap).
add_alt_start([Var - BranchWhere0 | WhereNeededList],
        BranchPoint, BranchNum, CurrentPath, !WhereNeededMap) :-
    (
        BranchWhere0 = everywhere,
        map.init(Empty),
        set.singleton_set(BranchNumSet, BranchNum),
        map.det_insert(Empty, BranchPoint, BranchNumSet, BranchMap),
        BranchWhere = branches(BranchMap)
    ;
        BranchWhere0 = branches(_),
        BranchWhere = BranchWhere0
    ),
    ( map.search(!.WhereNeededMap, Var, OldWhere) ->
        where_needed_upper_bound(CurrentPath, BranchWhere, OldWhere,
            CombinedWhere),
        svmap.det_update(Var, CombinedWhere, !WhereNeededMap)
    ;
        svmap.det_insert(Var, BranchWhere, !WhereNeededMap)
    ),
    add_alt_start(WhereNeededList, BranchPoint, BranchNum,
        CurrentPath, !WhereNeededMap).

%---------------------------------------------------------------------------%

:- pred refine_goal(hlds_goal::in, hlds_goal::out,
    refined_goal_map::in, refined_goal_map::out) is det.

refine_goal(Goal0, Goal, !RefinedGoals) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        (
            ConjType = plain_conj,
            refine_conj(Conjuncts0, Conjuncts, !RefinedGoals),
            GoalExpr = conj(ConjType, Conjuncts),
            Goal = GoalExpr - GoalInfo0
        ;
            ConjType = parallel_conj,
            Goal = Goal0
        )
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        refine_cases(Cases0, Cases, !RefinedGoals, GoalPath, 1),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = disj(Disjuncts0),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        refine_disj(Disjuncts0, Disjuncts, !RefinedGoals, GoalPath, 1),
        GoalExpr = disj(Disjuncts),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        goal_info_get_goal_path(GoalInfo0, GoalPath),
        refine_ite(Cond0, Cond, Then0, Then, Else0, Else, !RefinedGoals,
            GoalPath),
        GoalExpr = if_then_else(Quant, Cond, Then, Else),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = negation(NegGoal0),
        refine_goal(NegGoal0, NegGoal, !RefinedGoals),
        GoalExpr = negation(NegGoal),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        refine_goal(SomeGoal0, SomeGoal, !RefinedGoals),
        GoalExpr = scope(Reason, SomeGoal),
        Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "shorthand in refine_goal")
    ).

:- pred refine_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    refined_goal_map::in, refined_goal_map::out) is det.

refine_conj([], [], !RefinedGoals).
refine_conj([Goal0 | Goals0], Goals, !RefinedGoals) :-
    refine_goal(Goal0, HeadGoal, !RefinedGoals),
    refine_conj(Goals0, TailGoals, !RefinedGoals),
    ( HeadGoal = conj(plain_conj, HeadGoals) - _ ->
        Goals = HeadGoals ++ TailGoals
    ;
        Goals = [HeadGoal | TailGoals]
    ).

:- pred refine_cases(list(case)::in, list(case)::out,
    refined_goal_map::in, refined_goal_map::out,
    goal_path::in, int::in) is det.

refine_cases([], [], !RefinedGoals, _, _).
refine_cases([case(Var, Goal0) | Cases0], [case(Var, Goal) | Cases],
        !RefinedGoals, GoalPath, BranchNum) :-
    refine_goal(Goal0, Goal1, !RefinedGoals),
    ( map.search(!.RefinedGoals, GoalPath - BranchNum, ToInsertGoals) ->
        insert_refine_goals(ToInsertGoals, Goal1, Goal),
        svmap.delete(GoalPath - BranchNum, !RefinedGoals)
    ;
        Goal = Goal1
    ),
    refine_cases(Cases0, Cases, !RefinedGoals, GoalPath, BranchNum + 1).

:- pred refine_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    refined_goal_map::in, refined_goal_map::out,
    goal_path::in, int::in) is det.

refine_disj([], [], !RefinedGoals, _, _).
refine_disj([Goal0 | Goals0], [Goal | Goals], !RefinedGoals,
        GoalPath, BranchNum) :-
    refine_goal(Goal0, Goal1, !RefinedGoals),
    ( map.search(!.RefinedGoals, GoalPath - BranchNum, ToInsertGoals) ->
        insert_refine_goals(ToInsertGoals, Goal1, Goal),
        svmap.delete(GoalPath - BranchNum, !RefinedGoals)
    ;
        Goal = Goal1
    ),
    refine_disj(Goals0, Goals, !RefinedGoals, GoalPath, BranchNum + 1).

:- pred refine_ite(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    refined_goal_map::in, refined_goal_map::out, goal_path::in) is det.

refine_ite(Cond0, Cond, Then0, Then, Else0, Else,
        !RefinedGoals, GoalPath) :-
    refine_goal(Cond0, Cond, !RefinedGoals),
    refine_goal(Then0, Then1, !RefinedGoals),
    refine_goal(Else0, Else1, !RefinedGoals),

    ( map.search(!.RefinedGoals, GoalPath - 1, ToInsertGoalsThen) ->
        insert_refine_goals(ToInsertGoalsThen, Then1, Then),
        svmap.delete(GoalPath - 1, !RefinedGoals)
    ;
        Then = Then1
    ),
    ( map.search(!.RefinedGoals, GoalPath - 2, ToInsertGoalsElse) ->
        insert_refine_goals(ToInsertGoalsElse, Else1, Else),
        svmap.delete(GoalPath - 2, !RefinedGoals)
    ;
        Else = Else1
    ).

:- pred insert_refine_goals(list(hlds_goal)::in, hlds_goal::in,
    hlds_goal::out) is det.

insert_refine_goals(ToInsertGoals, Goal0, Goal) :-
    list.append(ToInsertGoals, [Goal0], Conj),
    % XXX GoalInfo0
    Goal0 = _ - GoalInfo0,
    conj_list_to_goal(Conj, GoalInfo0, Goal).

%-----------------------------------------------------------------------------%

    % Given two sets of requirements about where a goal is needed, return
    % a single requirement that contains all the demands. The main purpose
    % of this predicate is to discover when the union of two sets of
    % requirements (e.g. branch sets {b1,b2} and {b3} covers all
    % computation paths.
    %
:- pred where_needed_upper_bound(goal_path::in,
    where_needed::in, where_needed::in, where_needed::out) is det.

where_needed_upper_bound(CurrentPath,
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
            where_needed_branches_upper_bound(CurrentPath,
                BranchesA, BranchesB, WhereNeeded)
        )
    ).

:- pred where_needed_branches_upper_bound(goal_path::in,
    where_needed_branches::in, where_needed_branches::in, where_needed::out)
    is det.

where_needed_branches_upper_bound(CurrentPath, BranchesA, BranchesB,
        WhereNeeded) :-
    % should select smaller map to convert to list
    map.to_assoc_list(BranchesA, BranchesList),
    where_needed_branches_upper_bound_2(CurrentPath,
        BranchesList, BranchesB, WhereNeeded).

:- pred where_needed_branches_upper_bound_2(goal_path::in,
    assoc_list(branch_point, set(int))::in, where_needed_branches::in,
    where_needed::out) is det.

where_needed_branches_upper_bound_2(_, [],
        Branches, branches(Branches)).
where_needed_branches_upper_bound_2(CurrentPath, [First | Rest],
        Branches0, WhereNeeded) :-
    First = BranchPoint - NewAlts,
    ( map.search(Branches0, BranchPoint, OldAlts) ->
        set.union(OldAlts, NewAlts, Alts),
        BranchPoint = branch_point(GoalPath, BranchAlts),
        ( branch_point_is_complete(BranchAlts, Alts) ->
            (
                get_parent_branch_point(GoalPath,
                    ParentGoalPath, ParentGoalPathStep,
                    ParentBranchAlt, ParentBranchNum),
                \+ list.remove_suffix(CurrentPath,
                    [ParentGoalPathStep | ParentGoalPath], _)
            ->
                map.delete(Branches0, BranchPoint, Branches1),
                ParentBranchPoint = branch_point(ParentGoalPath,
                    ParentBranchAlt),
                set.singleton_set(ParentAlts, ParentBranchNum),
                where_needed_branches_upper_bound_2(CurrentPath,
                    [ParentBranchPoint - ParentAlts | Rest], Branches1,
                    WhereNeeded)
            ;
                WhereNeeded = everywhere
            )
        ;
            map.det_update(Branches0, BranchPoint, Alts, Branches1),
            where_needed_branches_upper_bound_2(CurrentPath,
                Rest, Branches1, WhereNeeded)
        )
    ;
        map.det_insert(Branches0, BranchPoint, NewAlts, Branches1),
        where_needed_branches_upper_bound_2(CurrentPath, Rest,
            Branches1, WhereNeeded)
    ).

:- pred get_parent_branch_point(goal_path::in, goal_path::out,
    goal_path_step::out, branch_alts::out, int::out) is semidet.

get_parent_branch_point([First | Rest], Parent, ParentStep,
        BranchAlt, BranchNum) :-
    ( First = switch(Arm, NumAlts) ->
        Parent = Rest,
        ParentStep = First,
        BranchAlt = switch(NumAlts),
        BranchNum = Arm
    ; First = ite_then ->
        Parent = Rest,
        ParentStep = First,
        BranchAlt = ite,
        BranchNum = 1
    ; First = ite_else ->
        Parent = Rest,
        ParentStep = First,
        BranchAlt = ite,
        BranchNum = 2
    ;
        get_parent_branch_point(Rest, Parent, ParentStep, BranchAlt, BranchNum)
    ).

:- pred branch_point_is_complete(branch_alts::in, set(int)::in)
    is semidet.

branch_point_is_complete(ite, Alts) :-
    set.count(Alts, NumAlts),
    NumAlts = 2.
branch_point_is_complete(switch(NumFunctors), Alts) :-
    set.count(Alts, NumAlts),
    NumAlts = NumFunctors.

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "unneeded_code.m".

%---------------------------------------------------------------------------%
:- end_module unneeded_code.
%---------------------------------------------------------------------------%
