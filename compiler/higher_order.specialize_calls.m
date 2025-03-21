%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.specialize_calls.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.higher_order.higher_order_global_info.

:- import_module set.

%---------------------------------------------------------------------------%
%
% This is the main predicate of this module. It is exported to the
% top module of the higher_order package, specialize_in_modules.
%

:- pred acc_specialization_requests(pred_id::in,
    set(ho_request)::in, set(ho_request)::out,
    higher_order_global_info::in, higher_order_global_info::out) is det.

%---------------------------------------------------------------------------%

    % This is called when the first procedure of a predicate was changed.
    % It fixes up all the other procedures, ignoring the goal_size and requests
    % that come out, since that information has already been collected.
    % XXX This old comment is less than informative (for example: what does
    % "fixes up" mean?), and is quite likely to have suffered bit rot.
    %
:- pred ho_traverse_proc(must_recompute::in, pred_id::in, proc_id::in,
    set(ho_request)::out,
    higher_order_global_info::in, higher_order_global_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.higher_order.higher_order_info.
:- import_module transform_hlds.higher_order.specialize_unify_compare.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

acc_specialization_requests(PredId, !Requests, !GlobalInfo) :-
    ModuleInfo0 = hogi_get_module_info(!.GlobalInfo),
    module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
    NonImportedProcs = pred_info_all_non_imported_procids(PredInfo0),
    (
        NonImportedProcs = []
    ;
        NonImportedProcs = [FirstProcId | _],
        list.map_foldl(ho_traverse_proc(need_not_recompute, PredId),
            NonImportedProcs, NewRequestSets, !GlobalInfo),
        NewRequests = set.union_list(NewRequestSets),
        set.union(NewRequests, !Requests),

        ModuleInfo1 = hogi_get_module_info(!.GlobalInfo),
        module_info_proc_info(ModuleInfo1, PredId, FirstProcId, FirstProcInfo),
        proc_info_get_goal(FirstProcInfo, FirstProcGoal),
        goal_size(FirstProcGoal, FirstProcGoalSize),
        % It is possible, though quite unlikely, for the sizes of the other
        % procedures to be significantly different from the size of the first.
        % The sizes will in general start out different if the predicate is
        % defined using mode-specific clauses, and even procedures that start
        % out with identical sizes may come to differ due to e.g. different
        % procedures having different switch arms eliminated as unreachable.
        %
        % This should not matter too much for a heuristic. On the other hand,
        % since the avegare number of procedures per predicate is around 1.02,
        % recording the size of every procedure separately should not cost
        % too much either.
        hogi_add_goal_size(PredId, FirstProcGoalSize, !GlobalInfo)
    ).

%---------------------------------------------------------------------------%
%
% Goal traversal.
% XXX Document the *purpose* of the traversal.
%

ho_traverse_proc(MustRecompute, PredId, ProcId, Requests, !GlobalInfo) :-
    Info0 = hoi_init(!.GlobalInfo, PredId, ProcId),
    ho_traverse_proc_body(MustRecompute, Info0, Info),
    hoi_results(Info, !:GlobalInfo, PredInfo, ProcInfo, Requests),
    ModuleInfo1 = hogi_get_module_info(!.GlobalInfo),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo1, ModuleInfo),
    hogi_set_module_info(ModuleInfo, !GlobalInfo).

:- pred ho_traverse_proc_body(must_recompute::in,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_proc_body(MustRecompute, !Info) :-
    % If this procedure is a specialised version, look up the initial
    % known bindings of the variables.
    % XXX Document the purpose of this action.
    VersionInfoMap = hogi_get_version_info_map(hoi_get_global_info(!.Info)),
    ( if
        map.search(VersionInfoMap, hoi_get_pred_proc_id(!.Info), VersionInfo),
        VersionInfo = version_info(_, _, KnownVarMap, _)
    then
        hoi_set_known_var_map(KnownVarMap, !Info)
    else
        true
    ),
    proc_info_get_goal(hoi_get_proc_info(!.Info), Goal0),
    ho_traverse_goal(Goal0, Goal, !Info),
    ho_rebuild_nonlocals_instmaps_if_needed(MustRecompute, Goal, !Info).

:- pred ho_rebuild_nonlocals_instmaps_if_needed(must_recompute::in,
    hlds_goal::in, higher_order_info::in, higher_order_info::out) is det.

ho_rebuild_nonlocals_instmaps_if_needed(MustRecompute, !.Goal, !Info) :-
    ( if
        ( hoi_get_changed(!.Info) = hoc_changed
        ; MustRecompute = must_recompute
        )
    then
        % XXX The code whose effects we are now fixing up can eliminate
        % some variables from the code of the procedure. Some of those
        % variables appear in the RTTI varmaps, yet we do not delete them
        % from there. This is a bug.
        some [!ProcInfo] (
            ModuleInfo0 = hogi_get_module_info(hoi_get_global_info(!.Info)),
            !:ProcInfo = hoi_get_proc_info(!.Info),
            proc_info_set_goal(!.Goal, !ProcInfo),
            requantify_proc_general(ord_nl_no_lambda, !ProcInfo),
            proc_info_get_goal(!.ProcInfo, !:Goal),
            proc_info_get_initial_instmap(ModuleInfo0, !.ProcInfo, InstMap),
            proc_info_get_var_table(!.ProcInfo, VarTable),
            proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
            recompute_instmap_delta(no_recomp_atomics, VarTable, InstVarSet,
                InstMap, !Goal, ModuleInfo0, ModuleInfo),
            proc_info_set_goal(!.Goal, !ProcInfo),
            hoi_set_proc_info(!.ProcInfo, !Info),
            GlobalInfo1 = hoi_get_global_info(!.Info),
            hogi_set_module_info(ModuleInfo, GlobalInfo1, GlobalInfo),
            hoi_set_global_info(GlobalInfo, !Info)
        )
    else
        true
    ).

%---------------------%

    % Traverse the goal collecting higher order variables for which the value
    % is known, specialize calls, and add specialization requests to the
    % request_info structure.
    %
:- pred ho_traverse_goal(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            list.map_foldl(ho_traverse_goal, Goals0, Goals, !Info)
        ;
            ConjType = parallel_conj,
            ho_traverse_parallel_conj(Goals0, Goals, !Info)
        ),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        (
            Goals0 = [],
            Goals = []
        ;
            Goals0 = [HeadGoal0 | TailGoals0],
            % To process a disjunction, we process each disjunct with the
            % specialization information before the goal, then merge the
            % results to give the specialization information after the
            % disjunction.
            get_pre_branch_info(!.Info, PreInfo),
            ho_traverse_disjuncts(PreInfo, HeadGoal0, TailGoals0,
                HeadGoal, TailGoals, [], PostInfos, !Info),
            Goals = [HeadGoal | TailGoals],
            merge_post_branch_infos_into_one(PostInfos, MergedPostInfo),
            set_post_branch_info(MergedPostInfo, !Info)
        ),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        (
            Cases0 = [],
            unexpected($pred, "switch with no cases")
        ;
            Cases0 = [HeadCase0 | TailCases0],
            % We treat switches as we treat disjunctions, the only difference
            % being that we have to keep the association between a case's
            % goal and its cons_ids.
            get_pre_branch_info(!.Info, PreInfo),
            ho_traverse_cases(PreInfo, HeadCase0, TailCases0,
                HeadCase, TailCases, [], PostInfos, !Info),
            Cases = [HeadCase | TailCases],
            merge_post_branch_infos_into_one(PostInfos, MergedPostInfo),
            set_post_branch_info(MergedPostInfo, !Info)
        ),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = generic_call(GenericCall, Args, _, _, _),
        % Check whether this call could be specialized.
        (
            GenericCall = higher_order(Var, _, _, _, _),
            maybe_specialize_higher_order_call(Var, Args, Goal0, Goal, !Info)
        ;
            GenericCall = class_method(Var, Method, _, _),
            maybe_specialize_method_call(Var, Method, Args, Goal0, Goal, !Info)
        ;
            ( GenericCall = event_call(_)
            ; GenericCall = cast(_)
            ),
            Goal = Goal0
        )
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        % Check whether this call can be specialized.
        % XXX Due to the absence of alias tracking, passing Goal0 instead
        % of Goal1 to maybe_specialize_call would result in a mode error.
        Goal1 = hlds_goal(GoalExpr0, GoalInfo0),
        maybe_specialize_call(Goal1, Goal, !Info)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        % If-then-elses are handled as disjunctions.
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_goal(Cond0, Cond, !Info),
        ho_traverse_goal(Then0, Then, !Info),
        get_post_branch_info_for_goal(!.Info, Then, PostThenInfo),
        set_pre_branch_info(PreInfo, !Info),
        ho_traverse_goal(Else0, Else, !Info),
        get_post_branch_info_for_goal(!.Info, Else, PostElseInfo),
        merge_post_branch_infos(PostThenInfo, PostElseInfo, PostInfo),
        set_post_branch_info(PostInfo, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        ho_traverse_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Goal = Goal0
        else
            ho_traverse_goal(SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = Goal0
    ;
        GoalExpr0 = unify(_, _, _, Unification0, _),
        ( if
            Unification0 = construct(_, closure_cons(_), _, _, _, _, _)
        then
            maybe_specialize_pred_const(Goal0, Goal, !Info)
        else
            Goal = Goal0
        ),
        ( if Goal = hlds_goal(unify(_, _, _, Unification, _), _) then
            record_interesting_consts(Unification, !Info)
        else
            true
        )
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%---------------------%

    % To process a parallel conjunction, we process each conjunct with the
    % specialization information before the conjunct, then merge the
    % results to give the specialization information after the conjunction.
    %
:- pred ho_traverse_parallel_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_parallel_conj(Goals0, Goals, !Info) :-
    (
        Goals0 = [],
        unexpected($pred, "empty list")
    ;
        Goals0 = [_ | _],
        get_pre_branch_info(!.Info, PreInfo),
        ho_traverse_parallel_conj_loop(PreInfo, Goals0, Goals,
            [], PostInfos, !Info),
        merge_post_branch_infos_into_one(PostInfos, PostInfo),
        set_post_branch_info(PostInfo, !Info)
    ).

:- pred ho_traverse_parallel_conj_loop(pre_branch_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_parallel_conj_loop(_, [], [], !PostInfos, !Info).
ho_traverse_parallel_conj_loop(PreInfo, [Goal0 | Goals0], [Goal | Goals],
        !PostInfos, !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    ho_traverse_goal(Goal0, Goal, !Info),
    get_post_branch_info_for_goal(!.Info, Goal, GoalPostInfo),
    !:PostInfos = [GoalPostInfo | !.PostInfos],
    ho_traverse_parallel_conj_loop(PreInfo, Goals0, Goals, !PostInfos, !Info).

%---------------------%

:- pred ho_traverse_disjuncts(pre_branch_info::in,
    hlds_goal::in, list(hlds_goal)::in, hlds_goal::out, list(hlds_goal)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_disjuncts(PreInfo, HeadGoal0, TailGoals0, HeadGoal, TailGoals,
        !PostInfos, !Info) :-
    ho_traverse_disjunct(PreInfo, HeadGoal0, HeadGoal, HeadPostInfo, !Info),
    !:PostInfos = [HeadPostInfo | !.PostInfos],
    (
        TailGoals0 = [],
        TailGoals = []
    ;
        TailGoals0 = [HeadTailGoal0 | TailTailGoals0],
        ho_traverse_disjuncts(PreInfo, HeadTailGoal0, TailTailGoals0,
            HeadTailGoal, TailTailGoals, !PostInfos, !Info),
        TailGoals = [HeadTailGoal | TailTailGoals]
    ).

:- pred ho_traverse_disjunct(pre_branch_info::in,
    hlds_goal::in, hlds_goal::out, post_branch_info::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_disjunct(PreInfo, Goal0, Goal, PostInfo, !Info) :-
    set_pre_branch_info(PreInfo, !Info),
    ho_traverse_goal(Goal0, Goal, !Info),
    get_post_branch_info_for_goal(!.Info, Goal, PostInfo).

%---------------------%

:- pred ho_traverse_cases(pre_branch_info::in,
    case::in, list(case)::in, case::out, list(case)::out,
    list(post_branch_info)::in, list(post_branch_info)::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_cases(PreInfo, HeadCase0, TailCases0, HeadCase, TailCases,
        !PostInfos, !Info) :-
    ho_traverse_case(PreInfo, HeadCase0, HeadCase, HeadPostInfo, !Info),
    !:PostInfos = [HeadPostInfo | !.PostInfos],
    (
        TailCases0 = [],
        TailCases = []
    ;
        TailCases0 = [HeadTailCase0 | TailTailCases0],
        ho_traverse_cases(PreInfo, HeadTailCase0, TailTailCases0,
            HeadTailCase, TailTailCases, !PostInfos, !Info),
        TailCases = [HeadTailCase | TailTailCases]
    ).

:- pred ho_traverse_case(pre_branch_info::in, case::in, case::out,
    post_branch_info::out,
    higher_order_info::in, higher_order_info::out) is det.

ho_traverse_case(PreInfo, Case0, Case, PostInfo, !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    ho_traverse_disjunct(PreInfo, Goal0, Goal, PostInfo, !Info),
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------%

:- type pre_branch_info
    --->    pre_branch_info(known_var_map).

:- type reachability
    --->    reachable
    ;       unreachable.

:- type post_branch_info
    --->    post_branch_info(known_var_map, reachability).

:- pred get_pre_branch_info(higher_order_info::in, pre_branch_info::out)
    is det.

get_pre_branch_info(Info, pre_branch_info(hoi_get_known_var_map(Info))).

:- pred set_pre_branch_info(pre_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_pre_branch_info(pre_branch_info(KnownVarMap), !Info) :-
    hoi_set_known_var_map(KnownVarMap, !Info).

:- pred get_post_branch_info_for_goal(higher_order_info::in, hlds_goal::in,
    post_branch_info::out) is det.

get_post_branch_info_for_goal(Info, Goal, PostBranchInfo) :-
    InstMapDelta = goal_info_get_instmap_delta(Goal ^ hg_info),
    ( if instmap_delta_is_reachable(InstMapDelta) then
        Reachability = reachable
    else
        Reachability = unreachable
    ),
    PostBranchInfo =
        post_branch_info(hoi_get_known_var_map(Info), Reachability).

:- pred set_post_branch_info(post_branch_info::in,
    higher_order_info::in, higher_order_info::out) is det.

set_post_branch_info(post_branch_info(KnownVarMap, _), !Info) :-
    hoi_set_known_var_map(KnownVarMap, !Info).

    % Merge a bunch of post_branch_infos into one.
    %
:- pred merge_post_branch_infos_into_one(list(post_branch_info)::in,
    post_branch_info::out) is det.

merge_post_branch_infos_into_one(PostInfos, MergedPostInfo) :-
    IsReachable =
        ( pred(PostInfo::in, VarMap::out) is semidet :-
            PostInfo = post_branch_info(VarMap, reachable)
        ),
    list.filter_map(IsReachable, PostInfos, ReachableVarMaps),
    (
        ReachableVarMaps = [],
        MergedPostInfo = post_branch_info(map.init, unreachable)
    ;
        ReachableVarMaps = [HeadVarMap | TailVarMaps],
        merge_post_branch_var_maps_passes(HeadVarMap, TailVarMaps,
            MergedVarMap),
        MergedPostInfo = post_branch_info(MergedVarMap, reachable)
    ).

:- pred merge_post_branch_var_maps_passes(known_var_map::in,
    list(known_var_map)::in, known_var_map::out) is det.

merge_post_branch_var_maps_passes(VarMap1, VarMaps2Plus, MergedVarMap) :-
    merge_post_branch_var_maps_pass(VarMap1, VarMaps2Plus,
        HeadMergedVarMap, TailMergedVarMaps),
    (
        TailMergedVarMaps = [],
        MergedVarMap = HeadMergedVarMap
    ;
        TailMergedVarMaps = [_ | _],
        merge_post_branch_var_maps_passes(HeadMergedVarMap, TailMergedVarMaps,
            MergedVarMap)
    ).

:- pred merge_post_branch_var_maps_pass(known_var_map::in,
    list(known_var_map)::in,
    known_var_map::out, list(known_var_map)::out) is det.

merge_post_branch_var_maps_pass(VarMap1, VarMaps2Plus,
        HeadMergedVarMap, TailMergedVarMaps) :-
    (
        VarMaps2Plus = [],
        HeadMergedVarMap = VarMap1,
        TailMergedVarMaps = []
    ;
        VarMaps2Plus = [VarMap2 | VarMaps3Plus],
        merge_post_branch_known_var_maps(VarMap1, VarMap2, HeadMergedVarMap),
        (
            VarMaps3Plus = [],
            TailMergedVarMaps = []
        ;
            VarMaps3Plus = [VarMap3 | VarMaps4Plus],
            merge_post_branch_var_maps_pass(VarMap3, VarMaps4Plus,
                HeadTailMergedVarMap, TailTailMergedVarMaps),
            TailMergedVarMaps = [HeadTailMergedVarMap | TailTailMergedVarMaps]
        )
    ).

    % Merge two the known_var_maps of post_branch_infos.
    %
    % If a variable appears in one post_branch_info, but not the other,
    % it is dropped. Such a variable is either local to the branch arm,
    % in which case no subsequent specialization opportunities exist,
    % or it does not have a unique constant value in one of the branch arms,
    % so we can't specialize it outside the branch anyway. A third possibility
    % is that the branch without the variable is unreachable. In that case
    % we include the variable in the result.
    %
:- pred merge_post_branch_known_var_maps(known_var_map::in,
    known_var_map::in, known_var_map::out) is det.

merge_post_branch_known_var_maps(VarConstMapA, VarConstMapB, VarConstMapAB) :-
    map.keys_as_set(VarConstMapA, VarsA),
    map.keys_as_set(VarConstMapB, VarsB),
    set.intersect(VarsA, VarsB, CommonVars),
    VarConstCommonMapA = map.select(VarConstMapA, CommonVars),
    VarConstCommonMapB = map.select(VarConstMapB, CommonVars),
    map.to_assoc_list(VarConstCommonMapA, VarConstCommonListA),
    map.to_assoc_list(VarConstCommonMapB, VarConstCommonListB),
    merge_common_var_const_list(VarConstCommonListA, VarConstCommonListB,
        [], VarConstCommonList),
    map.from_assoc_list(VarConstCommonList, VarConstMapAB).

:- pred merge_post_branch_infos(post_branch_info::in,
    post_branch_info::in, post_branch_info::out) is det.

merge_post_branch_infos(PostA, PostB, Post) :-
    (
        PostA = post_branch_info(VarConstMapA, reachable),
        PostB = post_branch_info(VarConstMapB, reachable),
        merge_post_branch_known_var_maps(VarConstMapA, VarConstMapB,
            VarConstMapAB),
        Post = post_branch_info(VarConstMapAB, reachable)
    ;
        PostA = post_branch_info(_, unreachable),
        PostB = post_branch_info(_, reachable),
        Post = PostB
    ;
        PostA = post_branch_info(_, reachable),
        PostB = post_branch_info(_, unreachable),
        Post = PostA
    ;
        PostA = post_branch_info(_, unreachable),
        PostB = post_branch_info(_, unreachable),
        Post = post_branch_info(map.init, unreachable)
    ).

:- pred merge_common_var_const_list(assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::in,
    assoc_list(prog_var, known_const)::out) is det.

merge_common_var_const_list([], [], !List).
merge_common_var_const_list([], [_ | _], !MergedList) :-
    unexpected($pred, "mismatched list").
merge_common_var_const_list([_ | _], [], !MergedList) :-
    unexpected($pred, "mismatched list").
merge_common_var_const_list([VarA - ValueA | ListA], [VarB - ValueB | ListB],
        !MergedList) :-
    expect(unify(VarA, VarB), $pred, "var mismatch"),
    ( if ValueA = ValueB then
        !:MergedList = [VarA - ValueA | !.MergedList]
    else
        !:MergedList = !.MergedList
    ),
    merge_common_var_const_list(ListA, ListB, !MergedList).

%---------------------------------------------------------------------------%

:- pred record_interesting_consts(unification::in,
    higher_order_info::in, higher_order_info::out) is det.

record_interesting_consts(Unification, !Info) :-
    (
        Unification = simple_test(_, _)
        % Testing two higher order terms for equality is not allowed.
    ;
        Unification = assign(Var1, Var2),
        maybe_add_alias(Var1, Var2, !Info)
    ;
        Unification = deconstruct(_, _, _, _, _, _)
        % Deconstructing a higher order term is not allowed.
    ;
        Unification = construct(LVar, ConsId, Args, _Modes, _, _, _),
        Params = hogi_get_params(hoi_get_global_info(!.Info)),
        IsInteresting = is_interesting_cons_id(Params, ConsId),
        (
            IsInteresting = yes,
            KnownVarMap0 = hoi_get_known_var_map(!.Info),
            KnownConst = known_const(ConsId, Args),
            % A variable cannot be constructed twice.
            map.det_insert(LVar, KnownConst, KnownVarMap0, KnownVarMap),
            hoi_set_known_var_map(KnownVarMap, !Info)
        ;
            IsInteresting = no
        )
    ;
        Unification = complicated_unify(_, _, _),
        unexpected($pred, "complicated unification")
    ).

    % If the right argument of an assignment is a higher order term with a
    % known value, we need to add an entry for the left argument.
    %
:- pred maybe_add_alias(prog_var::in, prog_var::in,
    higher_order_info::in, higher_order_info::out) is det.

maybe_add_alias(LVar, RVar, !Info) :-
    KnownVarMap0 = hoi_get_known_var_map(!.Info),
    ( if map.search(KnownVarMap0, RVar, KnownConst) then
        map.det_insert(LVar, KnownConst, KnownVarMap0, KnownVarMap),
        hoi_set_known_var_map(KnownVarMap, !Info)
    else
        true
    ).

:- func is_interesting_cons_id(ho_params, cons_id) = bool.

is_interesting_cons_id(Params, ConsId) = IsInteresting :-
    (
        ( ConsId = du_data_ctor(_)
        ; ConsId = tuple_cons(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_entry_desc(_)
        ),
        IsInteresting = no
    ;
        ConsId = some_int_const(IntConst),
        (
            ( IntConst = uint_const(_)
            ; IntConst = int8_const(_)
            ; IntConst = uint8_const(_)
            ; IntConst = int16_const(_)
            ; IntConst = uint16_const(_)
            ; IntConst = int32_const(_)
            ; IntConst = uint32_const(_)
            ; IntConst = int64_const(_)
            ; IntConst = uint64_const(_)
            ),
            IsInteresting = no
        ;
            % We need to keep track of int_consts so we can interpret
            % calls to the builtins superclass_info_from_typeclass_info and
            % typeinfo_from_typeclass_info. We do not specialize based on
            % integers alone.
            IntConst = int_const(_),
            UserTypeSpec = Params ^ param_do_user_type_spec,
            (
                UserTypeSpec = spec_types_user_guided,
                IsInteresting = yes
            ;
                UserTypeSpec = do_not_spec_types_user_guided,
                IsInteresting = no
            )
        )
    ;
        ( ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ),
        UserTypeSpec = Params ^ param_do_user_type_spec,
        (
            UserTypeSpec = spec_types_user_guided,
            IsInteresting = yes
        ;
            UserTypeSpec = do_not_spec_types_user_guided,
            IsInteresting = no
        )
    ;
        ConsId = closure_cons(_),
        HigherOrder = Params ^ param_do_higher_order_spec,
        (
            HigherOrder = opt_higher_order,
            IsInteresting = yes
        ;
            HigherOrder = do_not_opt_higher_order,
            IsInteresting = no
        )
    ).

%---------------------------------------------------------------------------%

    % Process a higher-order call to see if it could possibly be specialized.
    %
:- pred maybe_specialize_higher_order_call(prog_var::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_higher_order_call(PredVar, Args, Goal0, Goal, !Info) :-
    % We can specialize calls to call/N if the closure has a known value.
    KnownVarMap0 = hoi_get_known_var_map(!.Info),
    ( if
        map.search(KnownVarMap0, PredVar, known_const(ConsId, CurriedArgs)),
        ConsId = closure_cons(ShroudedPredProcId)
    then
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        AllArgs = CurriedArgs ++ Args,
        Goal0 = hlds_goal(_, GoalInfo),
        construct_specialized_higher_order_call(PredId, ProcId, AllArgs,
            GoalInfo, Goal, !Info)
    else
        % Non-specializable call/N.
        Goal = Goal0
    ).

%---------------------%

    % Process a class_method_call to see if it could possibly be specialized.
    %
:- pred maybe_specialize_method_call(prog_var::in, method_proc_num::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_method_call(TypeClassInfoVar, MethodProcNum, Args,
        Goal0, Goal, !Info) :-
    MethodProcNum = method_proc_num(MethodNum),
    Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    KnownVarMap0 = hoi_get_known_var_map(!.Info),
    % We can specialize calls to class_method_call/N if the typeclass_info
    % has a known value.
    ( if
        % XXX We could duplicate this code, replacing the tests of
        % ConsId and BaseConsId with equivalent tests on const_structs.
        % However, how would we compute an equivalent of
        % InstanceConstraintArgs?
        map.search(KnownVarMap0, TypeClassInfoVar,
            known_const(ConsId, TCIArgs)),
        % A typeclass_info variable should consist of a known
        % base_typeclass_info and some argument typeclass_infos.
        ConsId = typeclass_info_cell_constructor,
        TCIArgs = [BaseTypeClassInfo | OtherTypeClassInfoArgs],
        map.search(KnownVarMap0, BaseTypeClassInfo,
            known_const(BaseConsId, _)),
        BaseConsId = base_typeclass_info_const(_, ClassId, Instance, _),

        module_info_get_instance_table(ModuleInfo, InstanceTable),
        map.lookup(InstanceTable, ClassId, InstanceList),
        list.det_index1(InstanceList, Instance, InstanceDefn),
        InstanceDefn = hlds_instance_defn(_, _, _, _, InstanceTypes0,
            InstanceConstraints,  _,_, _, yes(MethodInfos), _),
        type_vars_in_types(InstanceTypes0, InstanceTvars),
        get_unconstrained_tvars(InstanceTvars,
            InstanceConstraints, UnconstrainedTVars),
        NumArgsToExtract = list.length(InstanceConstraints)
            + list.length(UnconstrainedTVars),
        list.take(NumArgsToExtract, OtherTypeClassInfoArgs,
            InstanceConstraintArgs)
    then
        list.det_index1(MethodInfos, MethodNum, MethodInfo),
        MethodInfo ^ method_cur_proc = proc(PredId, ProcId),
        AllArgs = InstanceConstraintArgs ++ Args,
        construct_specialized_higher_order_call(PredId, ProcId, AllArgs,
            GoalInfo0, Goal, !Info)
    else if
        % Handle a class method call where we know which instance is being
        % used, but we haven't seen a construction for the typeclass_info.
        % This can happen for user-guided typeclass specialization, because
        % the type-specialized class constraint is still in the constraint
        % list, so a typeclass_info is passed in by the caller rather than
        % being constructed locally.
        %
        % The problem is that in importing modules we don't know which
        % instance declarations are visible in the imported module, so we
        % don't know which class constraints are redundant after type
        % specialization.

        CallerPredInfo0 = hoi_get_pred_info(!.Info),
        CallerProcInfo0 = hoi_get_proc_info(!.Info),
        proc_info_get_rtti_varmaps(CallerProcInfo0, CallerRttiVarMaps),
        rtti_varmaps_var_info(CallerRttiVarMaps, TypeClassInfoVar,
            typeclass_info_var(ClassConstraint)),
        ClassConstraint = constraint(ClassName, ClassArgTypes),
        list.length(ClassArgTypes, ClassArity),
        module_info_get_instance_table(ModuleInfo, InstanceTable),
        map.lookup(InstanceTable, class_id(ClassName, ClassArity), Instances),
        pred_info_get_typevarset(CallerPredInfo0, TVarSet0),
        find_matching_instance_method(Instances, MethodNum, ClassArgTypes,
            PredId, ProcId, InstanceConstraints, UnconstrainedTVarTypes,
            TVarSet0, TVarSet)
    then
        pred_info_set_typevarset(TVarSet, CallerPredInfo0, CallerPredInfo),
        % Pull out the argument typeclass_infos.
        ( if
            InstanceConstraints = [],
            UnconstrainedTVarTypes = []
        then
            ExtraGoals = [],
            CallerProcInfo = CallerProcInfo0,
            AllArgs = Args
        else
            get_unconstrained_instance_type_infos(ModuleInfo,
                TypeClassInfoVar, UnconstrainedTVarTypes, 1,
                ArgTypeInfoGoals, ArgTypeInfoVars,
                CallerProcInfo0, CallerProcInfo1),
            FirstArgTypeclassInfo = list.length(UnconstrainedTVarTypes) + 1,
            get_arg_typeclass_infos(ModuleInfo, TypeClassInfoVar,
                InstanceConstraints, FirstArgTypeclassInfo,
                ArgTypeClassInfoGoals, ArgTypeClassInfoVars,
                CallerProcInfo1, CallerProcInfo),
            list.condense([ArgTypeInfoVars, ArgTypeClassInfoVars, Args],
                AllArgs),
            ExtraGoals = ArgTypeInfoGoals ++ ArgTypeClassInfoGoals
        ),
        hoi_set_pred_info(CallerPredInfo, !Info),
        hoi_set_proc_info(CallerProcInfo, !Info),
        construct_specialized_higher_order_call(PredId, ProcId,
            AllArgs, GoalInfo0, SpecGoal, !Info),
        conj_list_to_goal(ExtraGoals ++ [SpecGoal], GoalInfo0, Goal)
    else
        % Non-specializable class_method_call/N.
        Goal = Goal0
    ).

:- pred find_matching_instance_method(list(hlds_instance_defn)::in, int::in,
    list(mer_type)::in, pred_id::out, proc_id::out,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

find_matching_instance_method([Instance | Instances], MethodNum, ClassTypes,
        PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet) :-
    ( if
        instance_matches(ClassTypes, Instance, Constraints0,
            UnconstrainedTVarTypes0, !TVarSet)
    then
        Constraints = Constraints0,
        UnconstrainedTVarTypes = UnconstrainedTVarTypes0,
        Instance ^ instdefn_maybe_method_infos = yes(MethodInfos),
        list.det_index1(MethodInfos, MethodNum, MethodInfo),
        MethodInfo ^ method_cur_proc = proc(PredId, ProcId)
    else
        find_matching_instance_method(Instances, MethodNum, ClassTypes,
            PredId, ProcId, Constraints, UnconstrainedTVarTypes, !TVarSet)
    ).

:- pred instance_matches(list(mer_type)::in, hlds_instance_defn::in,
    list(prog_constraint)::out, list(mer_type)::out,
    tvarset::in, tvarset::out) is semidet.

instance_matches(ClassTypes, Instance, Constraints, UnconstrainedTVarTypes,
        TVarSet0, TVarSet) :-
    Instance = hlds_instance_defn(_, _, InstanceTVarSet, _, InstanceTypes0,
        Constraints0, _, _, _, _, _),
    tvarset_merge_renaming(TVarSet0, InstanceTVarSet, TVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, InstanceTypes0,
        InstanceTypes),
    apply_variable_renaming_to_prog_constraint_list(Renaming, Constraints0,
        Constraints1),
    type_vars_in_types(InstanceTypes, InstanceTVars),
    get_unconstrained_tvars(InstanceTVars, Constraints1, UnconstrainedTVars0),

    type_list_subsumes(InstanceTypes, ClassTypes, Subst),
    apply_rec_subst_to_prog_constraint_list(Subst, Constraints1, Constraints),

    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(KindMap),
    apply_rec_subst_to_tvar_list(KindMap, Subst, UnconstrainedTVars0,
        UnconstrainedTVarTypes).

    % Build calls to
    % `private_builtin.instance_constraint_from_typeclass_info/3'
    % to extract the typeclass_infos for the constraints on an instance.
    % This simulates the action of `do_call_class_method' in
    % runtime/mercury_ho_call.c.
    %
:- pred get_arg_typeclass_infos(module_info::in, prog_var::in,
    list(prog_constraint)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_arg_typeclass_infos(ModuleInfo, TypeClassInfoVar, InstanceConstraints,
        Index, Goals, Vars, !ProcInfo) :-
    MakeResultType = (func(_) = typeclass_info_type),
    get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
        "instance_constraint_from_typeclass_info", MakeResultType,
        InstanceConstraints, Index, Goals, Vars, !ProcInfo).

    % Build calls to
    % `private_builtin.unconstrained_type_info_from_typeclass_info/3'
    % to extract the typeinfos for the unconstrained type variables
    % of an instance declaration.
    % This simulates the action of `do_call_class_method' in
    % runtime/mercury_ho_call.c.
    %
:- pred get_unconstrained_instance_type_infos(module_info::in,
    prog_var::in, list(mer_type)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_unconstrained_instance_type_infos(ModuleInfo, TypeClassInfoVar,
        UnconstrainedTVarTypes, Index, Goals, Vars, !ProcInfo) :-
    MakeResultType = build_type_info_type,
    get_typeclass_info_args(ModuleInfo, TypeClassInfoVar,
        "unconstrained_type_info_from_typeclass_info",
        MakeResultType, UnconstrainedTVarTypes,
        Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args(module_info::in, prog_var::in, string::in,
    (func(T) = mer_type)::in, list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args(ModuleInfo, TypeClassInfoVar, PredName, MakeResultType,
        Args, Index, Goals, Vars, !ProcInfo) :-
    lookup_builtin_pred_proc_id(ModuleInfo, mercury_private_builtin_module,
        PredName, pf_predicate, user_arity(3), only_mode, ExtractArgPredId,
        ExtractArgProcId),
    get_typeclass_info_args_loop(ModuleInfo, TypeClassInfoVar,
        ExtractArgPredId, ExtractArgProcId,
        qualified(mercury_private_builtin_module, PredName),
        MakeResultType, Args, Index, Goals, Vars, !ProcInfo).

:- pred get_typeclass_info_args_loop(module_info::in, prog_var::in,
    pred_id::in, proc_id::in, sym_name::in, (func(T) = mer_type)::in,
    list(T)::in, int::in, list(hlds_goal)::out,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

get_typeclass_info_args_loop(_, _, _, _, _, _, [], _, [], [], !ProcInfo).
get_typeclass_info_args_loop(ModuleInfo, TypeClassInfoVar, PredId, ProcId,
        SymName, MakeResultType, [Arg | Args], Index,
        [IndexGoal, CallGoal | Goals], [ResultVar | Vars], !ProcInfo) :-
    ResultType = MakeResultType(Arg),
    IsDummy = is_type_a_dummy(ModuleInfo, ResultType),
    proc_info_create_var_from_type("", ResultType, IsDummy,
        ResultVar, !ProcInfo),
    MaybeContext = no,
    make_int_const_construction_alloc_in_proc(Index, "", IndexGoal, IndexVar,
        !ProcInfo),
    CallArgs = [TypeClassInfoVar, IndexVar, ResultVar],

    set_of_var.list_to_set(CallArgs, NonLocals),
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_insert_var(ResultVar, ground(shared, none_or_default_func),
        InstMapDelta0, InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, GoalInfo),
    CallGoalExpr = plain_call(PredId, ProcId, CallArgs, not_builtin,
        MaybeContext, SymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    get_typeclass_info_args_loop(ModuleInfo, TypeClassInfoVar, PredId, ProcId,
        SymName, MakeResultType, Args, Index + 1, Goals, Vars, !ProcInfo).

%---------------------------------------------------------------------------%

:- pred construct_specialized_higher_order_call(pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_specialized_higher_order_call(PredId, ProcId, AllArgs, GoalInfo,
        hlds_goal(GoalExpr, GoalInfo), !Info) :-
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(ModuleName, PredName),
    proc(CallerPredId, _) = hoi_get_pred_proc_id(!.Info),
    Builtin = builtin_state(ModuleInfo, CallerPredId, PredId, ProcId),

    MaybeContext = no,
    GoalExpr1 =
        plain_call(PredId, ProcId, AllArgs, Builtin, MaybeContext, SymName),
    hoi_set_changed(hoc_changed, !Info),
    maybe_specialize_call(hlds_goal(GoalExpr1, GoalInfo),
        hlds_goal(GoalExpr, _), !Info).

%---------------------------------------------------------------------------%

:- pred maybe_specialize_call(hlds_goal::in(goal_plain_call), hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_call(hlds_goal(GoalExpr0, GoalInfo),
        hlds_goal(GoalExpr, GoalInfo), !Info) :-
    ModuleInfo0 = hogi_get_module_info(hoi_get_global_info(!.Info)),
    GoalExpr0 = plain_call(CalleePredId, CalleeProcId, Args0, IsBuiltin,
        MaybeContext, _SymName0),
    module_info_pred_proc_info(ModuleInfo0, CalleePredId, CalleeProcId,
        CalleePredInfo, CalleeProcInfo),
    ( if
        % Look for calls to unify/2 and compare/3 that can be specialized.
        specialize_call_to_unify_or_compare(CalleePredInfo, CalleeProcId,
            Args0, MaybeContext, GoalInfo, GoalExpr1, !Info)
    then
        GoalExpr = GoalExpr1,
        hoi_set_changed(hoc_changed, !Info)
    else if
        is_typeclass_info_manipulator(CalleePredInfo, Manipulator)
    then
        interpret_typeclass_info_manipulator(Manipulator, Args0,
            GoalExpr0, GoalExpr, !Info)
    else if
        (
            pred_info_is_imported(CalleePredInfo),
            module_info_get_type_spec_tables(ModuleInfo0, TypeSpecTables),
            TypeSpecTables = type_spec_tables(TypeSpecProcs, _, _, _),
            not set.member(proc(CalleePredId, CalleeProcId), TypeSpecProcs)
        ;
            pred_info_is_pseudo_imported(CalleePredInfo),
            hlds_pred.in_in_unification_proc_id(CalleeProcId)
        ;
            pred_info_defn_has_foreign_proc(CalleePredInfo)
        )
    then
        GoalExpr = GoalExpr0
    else
        CalleePredProcId = proc(CalleePredId, CalleeProcId),
        maybe_specialize_ordinary_call(can_request, CalleePredProcId,
            CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals, GoalExpr1),
            goal_to_conj_list(hlds_goal(GoalExpr1, GoalInfo), GoalList1),
            GoalList = ExtraTypeInfoGoals ++ GoalList1,
            GoalExpr = conj(plain_conj, GoalList)
        ;
            Result = not_specialized,
            GoalExpr = GoalExpr0
        )
    ).

%---------------------------------------------------------------------------%

    % Try to specialize constructions of higher-order terms.
    % This is useful if we don't have the code for the predicates to which
    % the higher-order term we are now consructing will later be passed.
    %
    % We do this specialization by treating
    %   Pred = foo(A, B, ...)
    % as
    %   pred(X::<mode1>, Y::<mode2>, ...) is <det> :-
    %       foo(A, B, ..., X, Y, ...)
    % and specializing the call.
    %
:- pred maybe_specialize_pred_const(hlds_goal::in, hlds_goal::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_pred_const(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    GlobalInfo0 = hoi_get_global_info(!.Info),
    ModuleInfo = hogi_get_module_info(GlobalInfo0),
    NewPredMap = hogi_get_new_pred_map(GlobalInfo0),
    ProcInfo0  = hoi_get_proc_info(!.Info),
    ( if
        GoalExpr0 = unify(_, _, UniMode, Unify0, Context),
        Unify0 = construct(LVar, ConsId0, Args0, _,
            HowToConstruct, CellIsUnique, SubInfo),
        (
            SubInfo = no_construct_sub_info
        ;
            SubInfo = construct_sub_info(no, no)
        ),
        ConsId0 = closure_cons(ShroudedPredProcId),
        CalleePredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        map.contains(NewPredMap, CalleePredProcId),
        proc_info_get_var_table(ProcInfo0, VarTable0),
        lookup_var_type(VarTable0, LVar, LVarType),
        type_is_higher_order_details(LVarType, _, _, ArgTypes)
    then
        proc_info_create_vars_from_types(ModuleInfo, ArgTypes, UncurriedArgs,
            ProcInfo0, ProcInfo1),
        Args1 = Args0 ++ UncurriedArgs,
        hoi_set_proc_info(ProcInfo1, !Info),

        module_info_pred_proc_info(ModuleInfo, CalleePredProcId,
            CalleePredInfo, CalleeProcInfo),

        % We don't create requests for higher-order terms because that would
        % result in duplication of effort if all uses of the constant end up
        % being specialized. For parser combinator programs it would also
        % result in huge numbers of requests with no easy way to control
        % which ones should be created.

        IsBuiltin = not_builtin,
        MaybeContext = no,
        maybe_specialize_ordinary_call(can_not_request, CalleePredProcId,
            CalleePredInfo, CalleeProcInfo, Args1, IsBuiltin, MaybeContext,
            GoalInfo, Result, !Info),
        (
            Result = specialized(ExtraTypeInfoGoals0, GoalExpr1),
            ( if
                GoalExpr1 =
                    plain_call(NewPredId0, NewProcId0, NewArgs0, _, _, _),
                list.remove_suffix(NewArgs0, UncurriedArgs, NewArgs1)
            then
                NewPredId = NewPredId0,
                NewProcId = NewProcId0,
                NewArgs = NewArgs1
            else
                unexpected($pred, "cannot get NewArgs")
            ),

            module_info_proc_info(ModuleInfo, NewPredId, NewProcId,
                NewCalleeProcInfo),
            proc_info_get_argmodes(NewCalleeProcInfo, NewCalleeArgModes),
            ( if
                list.take(list.length(NewArgs), NewCalleeArgModes,
                    CurriedArgModesPrime)
            then
                CurriedArgModes = CurriedArgModesPrime
            else
                unexpected($pred, "cannot get CurriedArgModes")
            ),
            ArgModes =
                list.map(mode_to_unify_mode(ModuleInfo), CurriedArgModes),

            % The dummy arguments can't be used anywhere.
            ProcInfo2 = hoi_get_proc_info(!.Info),
            proc_info_get_var_table(ProcInfo2, VarTable2),
            delete_var_entries(UncurriedArgs, VarTable2, VarTable),
            proc_info_set_var_table(VarTable, ProcInfo2, ProcInfo),
            hoi_set_proc_info(ProcInfo, !Info),

            NewPredProcId = proc(NewPredId, NewProcId),
            NewShroudedPredProcId = shroud_pred_proc_id(NewPredProcId),
            NewConsId = closure_cons(NewShroudedPredProcId),
            Unify = construct(LVar, NewConsId, NewArgs, ArgModes,
                HowToConstruct, CellIsUnique, no_construct_sub_info),
            GoalExpr2 = unify(LVar,
                rhs_functor(NewConsId, is_not_exist_constr, NewArgs),
                UniMode, Unify, Context),

            % Make sure any constants in the ExtraTypeInfoGoals are recorded.
            list.map_foldl(ho_traverse_goal, ExtraTypeInfoGoals0,
                ExtraTypeInfoGoals, !Info),
            (
                ExtraTypeInfoGoals = [],
                GoalExpr = GoalExpr2
            ;
                ExtraTypeInfoGoals = [_ | _],
                GoalExpr = conj(plain_conj,
                    ExtraTypeInfoGoals ++ [hlds_goal(GoalExpr2, GoalInfo)])
            ),
            Goal = hlds_goal(GoalExpr, GoalInfo)
        ;
            Result = not_specialized,
            % The dummy arguments can't be used anywhere.
            hoi_set_proc_info(ProcInfo0, !Info),
            Goal = Goal0
        )
    else
        Goal = Goal0
    ).

%---------------------------------------------------------------------------%

:- type specialization_result
    --->    specialized(
                % Goals to construct extra typeinfos.
                list(hlds_goal),

                % The specialized call.
                hlds_goal_expr
            )
    ;       not_specialized.

:- type can_request
    --->    can_request
    ;       can_not_request.

:- pred maybe_specialize_ordinary_call(can_request::in, pred_proc_id::in,
    pred_info::in, proc_info::in, list(prog_var)::in, builtin_state::in,
    maybe(call_unify_context)::in,
    hlds_goal_info::in, specialization_result::out,
    higher_order_info::in, higher_order_info::out) is det.

maybe_specialize_ordinary_call(CanRequest, CalleePredProcId,
        CalleePredInfo, CalleeProcInfo, Args0, IsBuiltin,
        MaybeContext, GoalInfo, Result, !Info) :-
    ModuleInfo0 = hogi_get_module_info(hoi_get_global_info(!.Info)),
    pred_info_get_status(CalleePredInfo, CalleeStatus),
    proc_info_get_var_table(CalleeProcInfo, CalleeVarTable),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    lookup_var_types(CalleeVarTable, CalleeHeadVars, CalleeArgTypes),

    CallerProcInfo0 = hoi_get_proc_info(!.Info),
    proc_info_get_var_table(CallerProcInfo0, VarTable),
    proc_info_get_rtti_varmaps(CallerProcInfo0, RttiVarMaps),
    find_higher_order_args(ModuleInfo0, CalleeStatus, Args0, CalleeArgTypes,
        VarTable, RttiVarMaps, hoi_get_known_var_map(!.Info),
        1, cord.init, HigherOrderArgsCord),
    HigherOrderArgs = cord.list(HigherOrderArgsCord),

    proc(CallerPredId, _) = hoi_get_pred_proc_id(!.Info),
    module_info_get_type_spec_tables(ModuleInfo0, TypeSpecTables),
    TypeSpecTables = type_spec_tables(_, ForceVersions, _, _),
    ( if set.contains(ForceVersions, CallerPredId) then
        RequestKind = user_type_spec
    else
        RequestKind = non_user_type_spec
    ),
    ( if
        % There are three reasons why we may want to specialize a call.
        (
            % Reason one: it has higher order arguments, whose values
            % *may* be known. This should be checked by find_matching_version.
            HigherOrderArgs = [_ | _]
        ;
            % Reason two: because the user has requested the specialization
            % of the callee.
            RequestKind = user_type_spec
        ;
            % Reason three: because we can replace a call to an abstract
            % method of a type class with a call to the concrete method
            % of the instance that is applicable in this case.
            Params = hogi_get_params(hoi_get_global_info(!.Info)),
            Params ^ param_do_user_type_spec = spec_types_user_guided,
            lookup_var_types(VarTable, Args0, ArgTypes),

            % Check whether any typeclass constraints now match an instance.
            pred_info_get_class_context(CalleePredInfo, CalleeClassContext),
            CalleeClassContext =
                univ_exist_constraints(CalleeUnivConstraints0, _),
            pred_info_get_typevarset(CalleePredInfo, CalleeTVarSet),
            pred_info_get_exist_quant_tvars(CalleePredInfo, CalleeExistQTVars),
            CallerPredInfo0 = hoi_get_pred_info(!.Info),
            pred_info_get_typevarset(CallerPredInfo0, TVarSet),
            pred_info_get_univ_quant_tvars(CallerPredInfo0, CallerUnivQTVars),
            type_subst_makes_some_instance_known(ModuleInfo0,
                CalleeUnivConstraints0, TVarSet,
                CallerUnivQTVars, ArgTypes, CalleeTVarSet,
                CalleeExistQTVars, CalleeArgTypes)
        )
    then
        % At least one of the above reasons applies.
        % Have we already created the specialized version?
        Context = goal_info_get_context(GoalInfo),
        find_matching_version(!.Info, CalleePredProcId, Args0, HigherOrderArgs,
            RequestKind, Context, FindResult),
        (
            FindResult = find_result_match(Match),
            % Yes, we have created it. Return the code to call it.
            Match = match(MatchedNewPred, _, Args1, ExtraTypeInfoTypes),
            MatchedNewPred =
                new_pred(NewPredProcId, _, _, NewName, _, _, _, _, _),

            construct_extra_type_infos(ExtraTypeInfoTypes,
                ExtraTypeInfoVars, ExtraTypeInfoGoals, !Info),

            NewPredProcId = proc(NewCalleePred, NewCalleeProc),
            Args = ExtraTypeInfoVars ++ Args1,
            CallGoal = plain_call(NewCalleePred, NewCalleeProc, Args,
                IsBuiltin, MaybeContext, NewName),
            Result = specialized(ExtraTypeInfoGoals, CallGoal),
            hoi_set_changed(hoc_changed, !Info)
        ;
            % We have not created it yet. If we may do so, ask for it
            % to be created, so a later pass can call it.
            FindResult = find_result_request(Request),
            Result = not_specialized,
            (
                CanRequest = can_request,
                hoi_add_request(Request, !Info)
            ;
                CanRequest = can_not_request
            )
        ;
            FindResult = find_result_no_request,
            % We have not created it, and its creation cannot be requested.
            Result = not_specialized
        )
    else
        Result = not_specialized
    ).

%---------------------%

    % Returns a list of the higher-order arguments in a call that have
    % a known value.
    %
:- pred find_higher_order_args(module_info::in, pred_status::in,
    list(prog_var)::in, list(mer_type)::in, var_table::in,
    rtti_varmaps::in, known_var_map::in, int::in,
    cord(higher_order_arg)::in, cord(higher_order_arg)::out) is det.

find_higher_order_args(_, _, [], _, _, _, _, _, !HOArgsCord).
find_higher_order_args(_, _, [_ | _], [], _, _, _, _, _, _) :-
    unexpected($pred, "length mismatch").
find_higher_order_args(ModuleInfo, CalleeStatus, [Arg | Args],
        [CalleeArgType | CalleeArgTypes], VarTable, RttiVarMaps,
        KnownVarMap, ArgNo, !HOArgsCord) :-
    NextArg = ArgNo + 1,
    ( if
        % We don't specialize arguments whose declared type is polymorphic.
        % The closure they pass cannot possibly be called within the called
        % predicate, since that predicate doesn't know it is a closure
        % (without some dodgy use of type_to_univ and univ_to_type).
        map.search(KnownVarMap, Arg, known_const(ConsId, CurriedArgs)),

        % We don't specialize based on int_consts (we only keep track of them
        % to interpret calls to the procedures which extract fields from
        % typeclass_infos).
        ConsId \= some_int_const(int_const(_)),

        ( if ConsId = closure_cons(_) then
            % If we don't have clauses for the callee, we can't specialize
            % any higher-order arguments. We may be able to do user guided
            % type specialization.
            CalleeStatus \= pred_status(status_imported(_)),
            CalleeStatus \= pred_status(status_external(_)),
            type_is_higher_order(CalleeArgType)
        else
            true
        )
    then
        % Find any known higher-order arguments in the list of curried
        % arguments.
        lookup_var_types(VarTable, CurriedArgs, CurriedArgTypes),
        list.map(rtti_varmaps_var_info(RttiVarMaps), CurriedArgs,
            CurriedArgRttiInfo),
        ( if ConsId = closure_cons(ShroudedPredProcId) then
            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, CurriedCalleeArgTypes)
        else
            CurriedCalleeArgTypes = CurriedArgTypes
        ),
        find_higher_order_args(ModuleInfo, CalleeStatus, CurriedArgs,
            CurriedCalleeArgTypes, VarTable, RttiVarMaps,
            KnownVarMap, 1, cord.init, HOCurriedArgsCord),
        HOCurriedArgs = cord.list(HOCurriedArgsCord),
        list.length(CurriedArgs, NumCurriedArgs),
        list.length(HOCurriedArgs, NumHOCurriedArgs),
        ( if
            % XXX The reason for this equality test is unclear to me (zs).
            % Unfortunately, Simon's commit of the original version of this
            % code in July 2001 does not explain its purpose, and that diff
            % had no reviews, so no reviewer raised the issue either.
            NumCurriedArgs = NumHOCurriedArgs,
            all_higher_order_args_are_constant(HOCurriedArgs)
        then
            IsConst = arg_is_const
        else
            IsConst = arg_is_not_const
        ),
        HOArg = higher_order_arg(ConsId, ArgNo, NumCurriedArgs, CurriedArgs,
            CurriedArgTypes, CurriedArgRttiInfo, HOCurriedArgs, IsConst),
        cord.snoc(HOArg, !HOArgsCord)
    else
        true
    ),
    find_higher_order_args(ModuleInfo, CalleeStatus, Args, CalleeArgTypes,
        VarTable, RttiVarMaps, KnownVarMap, NextArg, !HOArgsCord).

    % Succeeds if the type substitution for a call makes any of the
    % class constraints match an instance which was not matched before.
    %
:- pred type_subst_makes_some_instance_known(module_info::in,
    list(prog_constraint)::in, tvarset::in, list(tvar)::in, list(mer_type)::in,
    tvarset::in, existq_tvars::in, list(mer_type)::in) is semidet.

type_subst_makes_some_instance_known(ModuleInfo, CalleeUnivConstraints0,
        TVarSet0, CallerHeadTypeParams, ArgTypes, CalleeTVarSet,
        CalleeExistQVars, CalleeArgTypes0) :-
    CalleeUnivConstraints0 = [_ | _],
    tvarset_merge_renaming(TVarSet0, CalleeTVarSet, TVarSet, TypeRenaming),
    apply_variable_renaming_to_type_list(TypeRenaming, CalleeArgTypes0,
        CalleeArgTypes1),

    % Substitute the types in the callee's class constraints.
    compute_caller_callee_type_substitution(CalleeArgTypes1, ArgTypes,
        CallerHeadTypeParams, CalleeExistQVars, TypeSubn),
    apply_variable_renaming_to_prog_constraint_list(TypeRenaming,
        CalleeUnivConstraints0, CalleeUnivConstraints1),
    apply_rec_subst_to_prog_constraint_list(TypeSubn,
        CalleeUnivConstraints1, CalleeUnivConstraints),
    some_updated_constraint_makes_an_instance_known_loop(ModuleInfo, TVarSet,
        CalleeUnivConstraints0, CalleeUnivConstraints).

    % Go through each constraint in turn, checking whether any instances match
    % with its updated form which didn't match before the update.
    %
:- pred some_updated_constraint_makes_an_instance_known_loop(module_info::in,
    tvarset::in, list(prog_constraint)::in, list(prog_constraint)::in)
    is semidet.

some_updated_constraint_makes_an_instance_known_loop(_, _, [], []) :-
    fail.
some_updated_constraint_makes_an_instance_known_loop(_, _, [], [_ | _]) :-
    unexpected($pred, "length mismatch").
some_updated_constraint_makes_an_instance_known_loop(_, _, [_ | _], []) :-
    unexpected($pred, "length mismatch").
some_updated_constraint_makes_an_instance_known_loop(ModuleInfo, TVarSet,
        [CalleeUnivConstraint0 | CalleeUnivConstraints0],
        [CalleeUnivConstraint | CalleeUnivConstraints]) :-
    CalleeUnivConstraint0 = constraint(ClassName, ArgTypes0),
    CalleeUnivConstraint = constraint(_ClassName, ArgTypes),
    % The class name and the arity should be identical before and after
    % the update.
    list.length(ArgTypes0, ClassArity),
    ClassId = class_id(ClassName, ClassArity),
    module_info_get_instance_table(ModuleInfo, InstanceTable),
    ( if
        map.search(InstanceTable, ClassId, Instances),
        some [Instance] (
            list.member(Instance, Instances),
            instance_matches(ArgTypes, Instance, _, _, TVarSet, _),
            not instance_matches(ArgTypes0, Instance, _, _, TVarSet, _)
        )
    then
        true
    else
        some_updated_constraint_makes_an_instance_known_loop(ModuleInfo,
            TVarSet, CalleeUnivConstraints0, CalleeUnivConstraints)
    ).

%---------------------%

:- type find_result
    --->    find_result_match(match)
    ;       find_result_request(ho_request)
    ;       find_result_no_request.

    % WARNING - do not filter out higher-order arguments from the request
    % returned by find_matching_version, otherwise some typeinfos that the
    % call specialization code is expecting to come from the curried arguments
    % of the higher-order arguments will not be present in the specialized
    % argument list.
    %
:- pred find_matching_version(higher_order_info::in, pred_proc_id::in,
    list(prog_var)::in, list(higher_order_arg)::in,
    ho_request_kind::in, prog_context::in, find_result::out) is det.

find_matching_version(Info, CalleePredProcId, Args0, HigherOrderArgs,
        RequestKind, Context, Result) :-
    % Args0 is the original list of arguments.
    % Args is the original list of arguments with the curried arguments
    % of known higher-order arguments added.

    GlobalInfo0 = hoi_get_global_info(Info),
    ModuleInfo = hogi_get_module_info(GlobalInfo0),
    Params = hogi_get_params(GlobalInfo0),
    NewPredMap = hogi_get_new_pred_map(GlobalInfo0),
    CallerPredProcId = hoi_get_pred_proc_id(Info),
    PredInfo = hoi_get_pred_info(Info),
    ProcInfo = hoi_get_proc_info(Info),

    % WARNING - do not filter out higher-order arguments after this step,
    % except when partially matching against a previously produced
    % specialization, otherwise some typeinfos that the call
    % specialization code is expecting to come from the curried arguments
    % of the higher-order arguments will not be present in the
    % specialized argument list.

    get_extra_arguments(HigherOrderArgs, Args0, Args),
    compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars),

    proc_info_get_var_table(ProcInfo, VarTable),
    PairWithType =
        ( pred(V::in, (V - T)::out) is det :-
            lookup_var_type(VarTable, V, T)
        ),
    list.map(PairWithType, Args0, ArgsTypes0),
    pred_info_get_typevarset(PredInfo, TVarSet),

    Request = ho_request(CallerPredProcId, CalleePredProcId, ArgsTypes0,
        ExtraTypeInfoTVars, HigherOrderArgs, TVarSet, RequestKind, Context),

    % Check to see if any of the specialized versions of the called pred
    % apply here.
    ( if
        map.search(NewPredMap, CalleePredProcId, VersionSet),
        set.to_sorted_list(VersionSet, Versions),
        search_for_version(Info, Params, ModuleInfo, Request, Versions,
            no, Match)
    then
        Result = find_result_match(Match)
    else if
        HigherOrder = Params ^ param_do_higher_order_spec,
        TypeSpec = Params ^ param_do_type_spec,
        UserTypeSpec = Params ^ param_do_user_type_spec,
        (
            UserTypeSpec = spec_types_user_guided,
            RequestKind = user_type_spec
        ;
            CalleePredProcId = proc(CalleePredId, _),
            module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
            not pred_info_is_imported(CalleePredInfo),
            (
                % This handles the predicates introduced by check_typeclass.m
                % to call the class methods for a specific instance. Without
                % this, user-specified specialized versions of class methods
                % won't be called.
                UserTypeSpec = spec_types_user_guided,
                pred_info_get_markers(CalleePredInfo, Markers),
                ( marker_is_present(Markers, marker_class_method)
                ; marker_is_present(Markers, marker_class_instance_method)
                )
            ;
                HigherOrder = opt_higher_order,
                some [HOArg] (
                    list.member(HOArg, HigherOrderArgs),
                    HOArg ^ hoa_cons_id = closure_cons(_)
                )
            ;
                TypeSpec = spec_types
            )
        )
    then
        Result = find_result_request(Request)
    else
        Result = find_result_no_request
    ).

    % Specializing type `T' to `list(U)' requires passing in the
    % typeinfo for `U'. This predicate works out which extra variables
    % to pass in given the argument list for the call. This needs to be done
    % even if --typeinfo-liveness is not set, because the typeinfos
    % may be needed when specializing calls inside the specialized version.
    %
:- pred compute_extra_typeinfos(higher_order_info::in,
    list(prog_var)::in, list(tvar)::out) is det.

compute_extra_typeinfos(Info, Args, ExtraTypeInfoTVars) :-
    % Work out which type variables don't already have typeinfos in the
    % list of argument types. The list is in the order which the type variables
    % occur in the list of argument types so that the extra typeinfo arguments
    % for calls to imported user-guided type specialization procedures
    % can be matched against the specialized version.
    % (We don't use `goal_util.extra_nonlocal_typeinfos_typeclass_infos' here
    % because it returns a set of typeinfo variables, but we need the
    % typeinfo vars in the order implicit in Args.)
    ProcInfo = hoi_get_proc_info(Info),
    proc_info_get_var_table(ProcInfo, VarTable),
    lookup_var_types(VarTable, Args, ArgTypes),
    type_vars_in_types(ArgTypes, AllTVars),
    (
        AllTVars = [],
        ExtraTypeInfoTVars = []
    ;
        AllTVars = [_ | _],
        proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
        list.foldl(arg_contains_type_info_for_tvar(RttiVarMaps),
            Args, [], TypeInfoTVars),
        list.delete_elems(AllTVars, TypeInfoTVars, ExtraTypeInfoTVars0),
        list.remove_dups(ExtraTypeInfoTVars0, ExtraTypeInfoTVars)
    ).

:- pred arg_contains_type_info_for_tvar(rtti_varmaps::in, prog_var::in,
    list(tvar)::in, list(tvar)::out) is det.

arg_contains_type_info_for_tvar(RttiVarMaps, Var, !TVars) :-
    rtti_varmaps_var_info(RttiVarMaps, Var, VarInfo),
    (
        VarInfo = type_info_var(Type),
        ( if Type = type_variable(TVar, _) then
            !:TVars = [TVar | !.TVars]
        else
            true
        )
    ;
        VarInfo = typeclass_info_var(Constraint),
        Constraint = constraint(_ClassName, ClassArgTypes),
        % Find out what tvars the typeclass-info contains the typeinfos for.
        list.filter_map(
            ( pred(ClassArgType::in, ClassTVar::out) is semidet :-
                ClassArgType = type_variable(ClassTVar, _)
            ), ClassArgTypes, ClassTVars),
        !:TVars = ClassTVars ++ !.TVars
    ;
        VarInfo = non_rtti_var
    ).

:- pred construct_extra_type_infos(list(mer_type)::in,
    list(prog_var)::out, list(hlds_goal)::out,
    higher_order_info::in, higher_order_info::out) is det.

construct_extra_type_infos(Types, TypeInfoVars, TypeInfoGoals, !Info) :-
    GlobalInfo0 = hoi_get_global_info(!.Info),
    ModuleInfo0 = hogi_get_module_info(GlobalInfo0),
    PredInfo0 = hoi_get_pred_info(!.Info),
    ProcInfo0 = hoi_get_proc_info(!.Info),
    polymorphism_make_type_info_vars_mi(Types, dummy_context,
        TypeInfoVars, TypeInfoGoals, ModuleInfo0, ModuleInfo,
        PredInfo0, PredInfo, ProcInfo0, ProcInfo),
    hoi_set_pred_info(PredInfo, !Info),
    hoi_set_proc_info(ProcInfo, !Info),
    GlobalInfo1 = hoi_get_global_info(!.Info),
    hogi_set_module_info(ModuleInfo, GlobalInfo1, GlobalInfo),
    hoi_set_global_info(GlobalInfo, !Info).

%---------------------%

    % search_for_version(Info, Params, ModuleInfo, Request, Versions,
    %   MaybeBestPartialSoFar0, Match):
    %
    % Search for a match for Request among Versions, returning either
    %
    % - the first complete match, if one exists, and
    % - the best partial match, if no complete match exists.
    %
    % MaybeBestPartialSoFar0 should hold the best partial match (if any)
    % that we have so far. The top-level caller should pass "no"
    % in this arg position.
    % 
:- pred search_for_version(higher_order_info::in, ho_params::in,
    module_info::in, ho_request::in, list(new_pred)::in,
    maybe(match)::in, match::out) is semidet.

search_for_version(_, _, _, _, [], yes(Match), Match).
search_for_version(Info, Params, ModuleInfo, Request, [Version | Versions],
        MaybeBestPartialSoFar0, Match) :-
    ( if version_matches(Params, ModuleInfo, Request, Version, Match1) then
        Match1 = match(_, MatchCompleteness1, _, _),
        (
            MatchCompleteness1 = complete_match,
            Match = Match1
        ;
            MatchCompleteness1 = partial_match(NumMatches1),
            (
                MaybeBestPartialSoFar0 = no,
                MaybeBestPartialSoFar1 = yes(Match1)
            ;
                MaybeBestPartialSoFar0 = yes(BestPartialSoFar0),
                BestPartialSoFar0 = match(_, BestPartialCompleteness0, _, _),
                (
                    BestPartialCompleteness0 = partial_match(NumMatches0),
                    % Pick the best match.
                    % XXX We prefer Match1 over Match0
                    % when NumMatches0 = NumMatches1.
                    ( if NumMatches0 > NumMatches1 then
                        MaybeBestPartialSoFar1 = MaybeBestPartialSoFar0
                    else
                        MaybeBestPartialSoFar1 = yes(Match1)
                    )
                ;
                    BestPartialCompleteness0 = complete_match,
                    unexpected($pred, "complete_match")
                )
            ),
            search_for_version(Info, Params, ModuleInfo, Request, Versions,
                MaybeBestPartialSoFar1, Match)
        )
    else
        search_for_version(Info, Params, ModuleInfo, Request, Versions,
            MaybeBestPartialSoFar0, Match)
    ).

%---------------------%

:- type typeclass_info_manipulator
    --->    type_info_from_typeclass_info
    ;       superclass_from_typeclass_info
    ;       instance_constraint_from_typeclass_info.

    % Succeed if the predicate is one of the predicates defined in
    % library/private_builtin.m to extract type_infos or typeclass_infos
    % from typeclass_infos.
    %
:- pred is_typeclass_info_manipulator(pred_info::in,
    typeclass_info_manipulator::out) is semidet.

is_typeclass_info_manipulator(PredInfo, TypeClassManipulator) :-
    mercury_private_builtin_module = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    (
        PredName = "type_info_from_typeclass_info",
        TypeClassManipulator = type_info_from_typeclass_info
    ;
        PredName = "superclass_from_typeclass_info",
        TypeClassManipulator = superclass_from_typeclass_info
    ;
        PredName = "instance_constraint_from_typeclass_info",
        TypeClassManipulator = instance_constraint_from_typeclass_info
    ).

    % Interpret a call to `type_info_from_typeclass_info',
    % `superclass_from_typeclass_info' or
    % `instance_constraint_from_typeclass_info'.
    % This should be kept in sync with compiler/polymorphism.m,
    % library/private_builtin.m and runtime/mercury_type_info.h.
    %
:- pred interpret_typeclass_info_manipulator(typeclass_info_manipulator::in,
    list(prog_var)::in, hlds_goal_expr::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

interpret_typeclass_info_manipulator(Manipulator, Args, Goal0, Goal, !Info) :-
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    KnownVarMap0 = hoi_get_known_var_map(!.Info),
    ( if
        Args = [TypeClassInfoVar, IndexVar, OutputVar],
        map.search(KnownVarMap0, TypeClassInfoVar,
            known_const(TypeClassInfoConsId, TypeClassInfoArgs)),
        find_typeclass_info_components(ModuleInfo, KnownVarMap0,
            TypeClassInfoConsId, TypeClassInfoArgs,
            _ModuleName, ClassId, InstanceNum, _Instance, OtherArgs),

        map.search(KnownVarMap0, IndexVar, IndexMaybeConst),
        IndexMaybeConst = known_const(some_int_const(int_const(Index0)), [])
    then
        (
            ( Manipulator = type_info_from_typeclass_info
            ; Manipulator = superclass_from_typeclass_info
            ),
            % polymorphism.m adds MR_typeclass_info_num_extra_instance_args
            % to the index.
            module_info_get_instance_table(ModuleInfo, InstanceTable),
            map.lookup(InstanceTable, ClassId, InstanceDefns),
            list.det_index1(InstanceDefns, InstanceNum, InstanceDefn),
            num_extra_instance_args(InstanceDefn, NumExtra),
            Index = Index0 + NumExtra
        ;
            Manipulator = instance_constraint_from_typeclass_info,
            Index = Index0
        ),

        (
            OtherArgs = tci_arg_vars(OtherVars),
            list.det_index1(OtherVars, Index, SelectedArg),
            maybe_add_alias(OutputVar, SelectedArg, !Info),
            UnifyMode = unify_modes_li_lf_ri_rf(free, ground_inst,
                ground_inst, ground_inst),
            Unification = assign(OutputVar, SelectedArg),
            Goal = unify(OutputVar, rhs_var(SelectedArg), UnifyMode,
                Unification, unify_context(umc_explicit, [])),

            ProcInfo0 = hoi_get_proc_info(!.Info),
            proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps0),
            rtti_var_info_duplicate_replace(SelectedArg, OutputVar,
                RttiVarMaps0, RttiVarMaps),
            proc_info_set_rtti_varmaps(RttiVarMaps, ProcInfo0, ProcInfo),
            hoi_set_proc_info(ProcInfo, !Info),

            % Sanity check.
            proc_info_get_var_table(ProcInfo, VarTable),
            lookup_var_type(VarTable, OutputVar, OutputVarType),
            lookup_var_type(VarTable, SelectedArg, SelectedArgType),
            ( if OutputVarType = SelectedArgType then
                true
            else
                unexpected($pred, "type mismatch")
            )
        ;
            OtherArgs = tci_arg_consts(OtherConstArgs),
            list.det_index1(OtherConstArgs, Index, SelectedConstArg),
            (
                SelectedConstArg = csa_constant(SelectedConsId, _),
                SelectedConstInst = bound(shared, inst_test_results_fgtc,
                    [bound_functor(SelectedConsId, [])])
            ;
                SelectedConstArg = csa_const_struct(SelectedConstNum),
                module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
                lookup_const_struct_num(ConstStructDb, SelectedConstNum,
                    SelectedConstStruct),
                SelectedConstStruct = const_struct(SelectedConstConsId, _, _,
                    SelectedConstInst, _),
                ( if
                    ( SelectedConstConsId = type_info_cell_constructor(_)
                    ; SelectedConstConsId = type_info_const(_)
                    )
                then
                    SelectedConsId = type_info_const(SelectedConstNum)
                else if
                    ( SelectedConstConsId = typeclass_info_cell_constructor
                    ; SelectedConstConsId = typeclass_info_const(_)
                    )
                then
                    SelectedConsId = typeclass_info_const(SelectedConstNum)
                else
                    unexpected($pred, "bad SelectedConstStructConsId")
                )
            ),
            map.det_insert(OutputVar, known_const(SelectedConsId, []),
                KnownVarMap0, KnownVarMap),
            hoi_set_known_var_map(KnownVarMap, !Info),

            SelectedConsIdRHS =
                rhs_functor(SelectedConsId, is_not_exist_constr, []),
            UnifyMode = unify_modes_li_lf_ri_rf(free, SelectedConstInst,
                SelectedConstInst, SelectedConstInst),
            Unification = construct(OutputVar, SelectedConsId, [], [],
                construct_dynamically, cell_is_shared, no_construct_sub_info),
            Goal = unify(OutputVar, SelectedConsIdRHS, UnifyMode,
                Unification, unify_context(umc_explicit, []))
            % XXX do we need to update the rtti varmaps?
        ),
        hoi_set_changed(hoc_changed, !Info)
    else
        Goal = Goal0
    ).

:- type type_class_info_args
    --->    tci_arg_vars(list(prog_var))
    ;       tci_arg_consts(list(const_struct_arg)).

:- pred find_typeclass_info_components(module_info::in, known_var_map::in,
    cons_id::in, list(prog_var)::in,
    module_name::out, class_id::out, int::out, string::out,
    type_class_info_args::out) is semidet.

find_typeclass_info_components(ModuleInfo, KnownVarMap,
        TypeClassInfoConsId, TypeClassInfoArgs,
        ModuleName, ClassId, InstanceNum, Instance, Args) :-
    (
        TypeClassInfoConsId = typeclass_info_cell_constructor,
        % Extract the number of class constraints on the instance
        % from the base_typeclass_info.
        % If we have a variable for the base typeclass info,
        % it cannot be bound to a constant structure, since
        % as far as the HLDS is concerned, a base typeclass info
        % is just a bare cons_id, and not a structure that needs a cell
        % on the heap.
        TypeClassInfoArgs = [BaseTypeClassInfoVar | OtherVars],

        map.search(KnownVarMap, BaseTypeClassInfoVar,
            BaseTypeClassInfoMaybeConst),
        BaseTypeClassInfoMaybeConst = known_const(BaseTypeClassInfoConsId, _),
        Args = tci_arg_vars(OtherVars)
    ;
        TypeClassInfoConsId = typeclass_info_const(TCIConstNum),
        TypeClassInfoArgs = [],
        module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
        lookup_const_struct_num(ConstStructDb, TCIConstNum, TCIConstStruct),
        TCIConstStruct = const_struct(TCIConstConsId, TCIConstArgs, _, _, _),
        expect(unify(TCIConstConsId, typeclass_info_cell_constructor), $pred,
            "TCIConstConsId != typeclass_info_cell_constructor"),
        TCIConstArgs = [BaseTypeClassInfoConstArg | OtherConstArgs],
        BaseTypeClassInfoConstArg = csa_constant(BaseTypeClassInfoConsId, _),
        Args = tci_arg_consts(OtherConstArgs)
    ),
    BaseTypeClassInfoConsId =
        base_typeclass_info_const(ModuleName, ClassId, InstanceNum, Instance).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.specialize_calls.
%---------------------------------------------------------------------------%
