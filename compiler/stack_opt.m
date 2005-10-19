%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_opt.
%
% Author: zs.
%
% The input to this module is a HLDS structure with annotations on three kinds
% of goals:
%
% - calls, including generic calls and foreign_proc goals which may
%   call back to Mercury, should have need_across_call annotations;
%
% - goals that have resume points before them (the conditions of if-then-elses
%   and the non-last disjuncts of disjunction) should have need_in_resume
%   annotations on them, provided that the resume point has a label that
%   expects its variables to be on the stack;
%
% - parallel conjunctions should have need_in_par_conj annotations.
%
% The code in this module puts stack_save_map annotations on goals that have
% need_across_call annotations, on if-then-else goals whose condition has a
% need_in_resume annotation, and on disjunction goals whose first disjunct has
% a need_in_resume annotation. The stack_save map annotation tells the
% code generator which of the relevant variables need to be saved in their own
% stack slots, and which can be accessed through other variables on the stack.
%
% The code in this module processes procedures one by one. It makes two passes
% over each procedure.
%
% The first pass traverses the procedure body backward, building a graph
% structure as it goes along. The nodes of the graphs are *anchors*. Points
% at which stack flushes may be required are anchors, and so are the beginnings
% and ends of branched control structures and of the procedure body itself.
% The graph associates with the edge between two anchors the set of variables
% accessed by the program fragment between those two anchors.
%
% When the traversal reaches a deconstruction unification, we sweep forward
% over the graph. During this sweep, we build a set of *paths*, with the
% intention that this set should contain an element for each path that control
% can take from the starting unification to the end of the procedure body.
% Each path is a sequence of *intervals*. An interval starts either at the
% starting unification or at a stack flush point; it ends at a stack flush
% point or the end of the procedure body. An interval is associated with one
% or more edges in the graph; the first of these associated edges will not
% have a left anchor yet.
%
% We give each path to the matching algorithm one by one. The matching
% algorithm finds out which set of variables should be accessed via
% the cell variable on that path. Since the decisions made for different
% paths are not independent, we have to apply a fixpoint iteration until
% we get a consistent set of answers.
%
% The first pass (whose main predicate is optimize_live_sets_in_goal) records
% its results in the left_anchor_inserts field of the stack_opt_info data
% structure it passes around. This field then becomes the main input to the
% second pass (whose main predicate is record_decisions_in_goal), which
% performs the source-to-source transformation that makes each segment access
% via the cell variable the field variables that have been selected to be so
% accessed by the first pass.
%
% The principles of this optimization are documented in the paper "Using the
% heap to eliminate stack accesses" by Zoltan Somogyi and Peter Stuckey.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__stack_opt.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module io.

:- pred stack_opt_cell(pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__interval.
:- import_module backend_libs__matching.
:- import_module check_hlds__goal_path.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__simplify.
:- import_module check_hlds__type_util.
:- import_module hlds__arg_info.
:- import_module hlds__code_model.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__call_gen.
:- import_module ll_backend__live_vars.
:- import_module ll_backend__liveness.
:- import_module ll_backend__store_alloc.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

% The opt_stack_alloc structure is constructed by live_vars.m. It contains
% the set of vars that definitely need their own stack slots, and which this
% optimization should not try to make reachable from a heap cell. At the
% moment, the only variables we treat this way are those that are required to
% be on the stack by a parallel conjunction.

:- type opt_stack_alloc
    --->    opt_stack_alloc(
                par_conj_own_slots  :: set(prog_var)
            ).

:- type stack_opt_params
    --->    stack_opt_params(
                matching_params     :: matching_params,
                all_path_node_ratio :: int,
                fixpoint_loop       :: bool,
                full_path           :: bool,
                on_stack            :: bool,
                non_candidate_vars  :: set(prog_var)
            ).

:- type matching_result
    --->    matching_result(
                prog_var,
                cons_id,
                list(prog_var),
                set(prog_var),
                goal_path,
                set(interval_id),
                set(interval_id),
                set(anchor),
                set(anchor)
            ).

:- type stack_opt_info
    --->    stack_opt_info(
                stack_opt_params    :: stack_opt_params,
                left_anchor_inserts :: insert_map,
                matching_results    :: list(matching_result)
            ).

stack_opt_cell(PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    % This simplication is necessary to fix some bad inputs from
    % getting to the liveness computation.
    % (see tests/valid/stack_opt_simplify.m)
    simplify_proc([], PredId, ProcId, !ModuleInfo, !ProcInfo, !IO),
    detect_liveness_proc(PredId, ProcId, !.ModuleInfo, !ProcInfo, !IO),
    initial_liveness(!.ProcInfo, PredId, !.ModuleInfo, Liveness0),
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    globals__lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    AllocData = alloc_data(!.ModuleInfo, !.ProcInfo, TypeInfoLiveness,
        OptNoReturnCalls),
    goal_path__fill_slots(!.ModuleInfo, !ProcInfo),
    proc_info_goal(!.ProcInfo, Goal2),
    OptStackAlloc0 = init_opt_stack_alloc,
    set__init(FailVars),
    set__init(NondetLiveness0),
    build_live_sets_in_goal(Goal2, Goal, FailVars, AllocData,
        OptStackAlloc0, OptStackAlloc, Liveness0, _Liveness,
        NondetLiveness0, _NondetLiveness),
    proc_info_set_goal(Goal, !ProcInfo),
    allocate_store_maps(for_stack_opt, PredId, !.ModuleInfo, !ProcInfo),
    globals__lookup_int_option(Globals, debug_stack_opt, DebugStackOpt),
    pred_id_to_int(PredId, PredIdInt),
    maybe_write_progress_message("\nbefore stack opt cell",
        DebugStackOpt, PredIdInt, !.ProcInfo, !.ModuleInfo, !IO),
    optimize_live_sets(!.ModuleInfo, OptStackAlloc, !ProcInfo,
        Changed, DebugStackOpt, PredIdInt, !IO),
    (
        Changed = yes,
        maybe_write_progress_message("\nafter stack opt transformation",
            DebugStackOpt, PredIdInt, !.ProcInfo, !.ModuleInfo, !IO),
        requantify_proc(!ProcInfo),
        maybe_write_progress_message("\nafter stack opt requantify",
            DebugStackOpt, PredIdInt, !.ProcInfo, !.ModuleInfo, !IO),
        recompute_instmap_delta_proc(yes, !ProcInfo, !ModuleInfo),
        maybe_write_progress_message("\nafter stack opt recompute instmaps",
            DebugStackOpt, PredIdInt, !.ProcInfo, !.ModuleInfo, !IO)
    ;
        Changed = no
    ).

:- func init_opt_stack_alloc = opt_stack_alloc.

init_opt_stack_alloc = opt_stack_alloc(set__init).

:- pred optimize_live_sets(module_info::in, opt_stack_alloc::in,
    proc_info::in, proc_info::out, bool::out, int::in, int::in,
    io::di, io::uo) is det.

optimize_live_sets(ModuleInfo, OptAlloc, !ProcInfo, Changed, DebugStackOpt,
        PredIdInt, !IO) :-
    proc_info_goal(!.ProcInfo, Goal0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_varset(!.ProcInfo, VarSet0),
    OptAlloc = opt_stack_alloc(ParConjOwnSlot),
    arg_info__partition_proc_args(!.ProcInfo, ModuleInfo,
        InputArgs, OutputArgs, UnusedArgs),
    HeadVars = set__union_list([InputArgs, OutputArgs, UnusedArgs]),
    module_info_get_globals(ModuleInfo, Globals),
    globals__lookup_bool_option(Globals,
        optimize_saved_vars_cell_candidate_headvars, CandHeadvars),
    (
        CandHeadvars = no,
        set__union(HeadVars, ParConjOwnSlot, NonCandidateVars)
    ;
        CandHeadvars = yes,
        NonCandidateVars = ParConjOwnSlot
    ),
    Counter0 = counter__init(1),
    counter__allocate(CurInterval, Counter0, Counter1),
    CurIntervalId = interval_id(CurInterval),
    EndMap0 = map__det_insert(map__init, CurIntervalId, proc_end),
    InsertMap0 = map__init,
    StartMap0 = map__init,
    SuccMap0 = map__det_insert(map__init, CurIntervalId, []),
    VarsMap0 = map__det_insert(map__init, CurIntervalId, OutputArgs),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_store_cost, CellVarStoreCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_load_cost, CellVarLoadCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_store_cost, FieldVarStoreCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_load_cost, FieldVarLoadCost),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_op_ratio, OpRatio),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_node_ratio, NodeRatio),
    globals__lookup_bool_option(Globals,
        optimize_saved_vars_cell_include_all_candidates, InclAllCand),
    MatchingParams = matching_params(CellVarStoreCost, CellVarLoadCost,
        FieldVarStoreCost, FieldVarLoadCost, OpRatio, NodeRatio,
        InclAllCand),
    globals__lookup_int_option(Globals,
        optimize_saved_vars_cell_all_path_node_ratio,
        AllPathNodeRatio),
    globals__lookup_bool_option(Globals,
        optimize_saved_vars_cell_loop, FixpointLoop),
    globals__lookup_bool_option(Globals,
        optimize_saved_vars_cell_full_path, FullPath),
    globals__lookup_bool_option(Globals,
        optimize_saved_vars_cell_on_stack, OnStack),
    globals__lookup_bool_option(Globals,
        opt_no_return_calls, OptNoReturnCalls),
    IntParams = interval_params(ModuleInfo, VarTypes0, OptNoReturnCalls),
    IntervalInfo0 = interval_info(IntParams, set__init, OutputArgs,
        map__init, map__init, map__init, CurIntervalId, Counter1,
        set__make_singleton_set(CurIntervalId),
        map__init, set__init, StartMap0, EndMap0,
        SuccMap0, VarsMap0, map__init),
    StackOptParams = stack_opt_params(MatchingParams, AllPathNodeRatio,
        FixpointLoop, FullPath, OnStack, NonCandidateVars),
    StackOptInfo0 = stack_opt_info(StackOptParams, InsertMap0, []),
    build_interval_info_in_goal(Goal0, IntervalInfo0, IntervalInfo,
        StackOptInfo0, StackOptInfo),
    ( DebugStackOpt = PredIdInt ->
        dump_interval_info(IntervalInfo, !IO),
        dump_stack_opt_info(StackOptInfo, !IO)
    ;
        true
    ),
    InsertMap = StackOptInfo ^ left_anchor_inserts,
    ( map__is_empty(InsertMap) ->
        Changed = no
    ;
        record_decisions_in_goal(Goal0, Goal1, VarSet0, VarSet,
            VarTypes0, VarTypes, map__init, RenameMap,
            InsertMap, yes(stack_opt)),
        apply_headvar_correction(HeadVars, RenameMap, Goal1, Goal),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        Changed = yes
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance stack_alloc_info(opt_stack_alloc) where [
    pred(at_call_site/4) is opt_at_call_site,
    pred(at_resume_site/4) is opt_at_resume_site,
    pred(at_par_conj/4) is opt_at_par_conj
].

:- pred opt_at_call_site(need_across_call::in, hlds_goal_info::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_call_site(_NeedAtCall, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_resume_site(need_in_resume::in, hlds_goal_info::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_resume_site(_NeedAtResume, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_par_conj(NeedParConj, _GoalInfo, StackAlloc0, StackAlloc) :-
    NeedParConj = need_in_par_conj(StackVars),
    ParConjOwnSlots0 = StackAlloc0 ^ par_conj_own_slots,
    ParConjOwnSlots = set__union(StackVars, ParConjOwnSlots0),
    StackAlloc = StackAlloc0 ^ par_conj_own_slots := ParConjOwnSlots.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance build_interval_info_acc(stack_opt_info) where [
    pred(use_cell/8) is stack_opt__use_cell
].

:- type match_path_info
    --->    match_path_info(
                set(prog_var),      % The set of vars referenced in
                                    % the first interval, before
                                    % the first flush point.
                list(set(prog_var)) % The set of vars referenced in
                                    % later intervals, after the
                                    % first flush point.
            ).

:- type match_info
    --->    match_info(
                list(match_path_info),  % Information about the
                                        % variables used along each
                                        % path.
                set(prog_var),          % The variables used after the
                                        % deconstruction goes out of
                                        % scope.
                bool,                   % Have we stepped over a
                                        % model_non goal?
                set(anchor),            % The set of save points
                                        % to which the results of the
                                        % matching applies.
                set(interval_id)
            ).

:- pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in, hlds_goal::in,
    interval_info::in, interval_info::out, stack_opt_info::in,
    stack_opt_info::out) is det.

use_cell(CellVar, FieldVarList, ConsId, Goal, !IntervalInfo, !StackOptInfo) :-
    FlushedLater = !.IntervalInfo ^ flushed_later,
    StackOptParams = !.StackOptInfo ^ stack_opt_params,
    NonCandidateVars = StackOptParams ^ non_candidate_vars,
    set__list_to_set(FieldVarList, FieldVars),
    set__intersect(FieldVars, FlushedLater, FlushedLaterFieldVars),
    set__difference(FlushedLaterFieldVars, NonCandidateVars,
        CandidateArgVars0),
    (
        set__empty(CandidateArgVars0)
    ->
        true
    ;
        ConsId = cons(_Name, _Arity),
        IntParams = !.IntervalInfo ^ interval_params,
        VarTypes = IntParams ^ var_types,
        map__lookup(VarTypes, CellVar, Type),
        (
            type_is_tuple(Type, _)
        ->
            FreeOfCost = no
        ;
            type_to_ctor_and_args(Type, TypeCtor, _),
            ModuleInfo = IntParams ^ module_info,
            module_info_get_type_table(ModuleInfo, TypeTable),
            map__lookup(TypeTable, TypeCtor, TypeDefn),
            hlds_data__get_type_defn_body(TypeDefn, TypeBody),
            ConsTable = TypeBody ^ du_type_cons_tag_values
        ->
            map__lookup(ConsTable, ConsId, ConsTag),
            ( ConsTag = no_tag ->
                FreeOfCost = yes
            ;
                FreeOfCost = no
            )
        ;
            fail
        )
    ->
        RelevantVars = set__insert(FieldVars, CellVar),
        find_all_branches_from_cur_interval(RelevantVars, MatchInfo,
            !.IntervalInfo, !.StackOptInfo),
        MatchInfo = match_info(PathsInfo, RelevantAfterVars,
            AfterModelNon, InsertAnchors, InsertIntervals),
        (
            FreeOfCost = yes,
            set__difference(CandidateArgVars0, RelevantAfterVars, ViaCellVars),
            record_matching_result(CellVar, ConsId, FieldVarList, ViaCellVars,
                Goal, InsertAnchors, InsertIntervals, !IntervalInfo,
                !StackOptInfo)
        ;
            FreeOfCost = no,
            (
                AfterModelNon = no,
                OnStack = StackOptParams ^ on_stack,
                set__difference(CandidateArgVars0, RelevantAfterVars,
                    CandidateArgVars),
                (
                    OnStack = yes,
                    ( set__member(CellVar, FlushedLater) ->
                        CellVarFlushedLater = yes
                    ;
                        CellVarFlushedLater = no
                    )
                ;
                    OnStack = no,
                    (
                        list__member(PathInfo, PathsInfo),
                        PathInfo = match_path_info(_, Segments),
                        list__member(Segment, Segments),
                        set__member(CellVar, Segment)
                    ->
                        CellVarFlushedLater = yes
                    ;
                        CellVarFlushedLater = no
                    )
                ),
                apply_matching(CellVar, CellVarFlushedLater, IntParams,
                    StackOptParams, PathsInfo, CandidateArgVars, ViaCellVars),
                record_matching_result(CellVar, ConsId, FieldVarList,
                    ViaCellVars, Goal, InsertAnchors, InsertIntervals,
                    !IntervalInfo, !StackOptInfo)
            ;
                AfterModelNon = yes
            )
        )
    ;
        true
    ).

:- pred apply_matching(prog_var::in, bool::in, interval_params::in,
    stack_opt_params::in, list(match_path_info)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

apply_matching(CellVar, CellVarFlushedLater, IntParams, StackOptParams,
        PathInfos, CandidateArgVars0, ViaCellVars) :-
    apply_matching_loop(CellVar, CellVarFlushedLater, IntParams,
        StackOptParams, PathInfos, CandidateArgVars0,
        BenefitNodeSets, CostNodeSets, ViaCellVars0),
    BenefitNodes = set__union_list(BenefitNodeSets),
    CostNodes = set__union_list(CostNodeSets),
    set__count(BenefitNodes, NumBenefitNodes),
    set__count(CostNodes, NumCostNodes),
    AllPathNodeRatio = StackOptParams ^ all_path_node_ratio,
    ( NumBenefitNodes * 100 >= NumCostNodes * AllPathNodeRatio ->
        ViaCellVars = ViaCellVars0
    ;
        ViaCellVars = set__init
    ).

:- pred apply_matching_loop(prog_var::in, bool::in, interval_params::in,
    stack_opt_params::in, list(match_path_info)::in, set(prog_var)::in,
    list(set(benefit_node))::out, list(set(cost_node))::out,
    set(prog_var)::out) is det.

apply_matching_loop(CellVar, CellVarFlushedLater, IntParams, StackOptParams,
        PathInfos, CandidateArgVars0, BenefitNodeSets, CostNodeSets,
        ViaCellVars) :-
    list__map3(apply_matching_for_path(CellVar, CellVarFlushedLater,
        StackOptParams, CandidateArgVars0), PathInfos,
        BenefitNodeSets0, CostNodeSets0, PathViaCellVars),
    ( list__all_same(PathViaCellVars) ->
        BenefitNodeSets = BenefitNodeSets0,
        CostNodeSets = CostNodeSets0,
        ( PathViaCellVars = [ViaCellVarsPrime | _] ->
            ViaCellVars = ViaCellVarsPrime
        ;
            ViaCellVars = set__init
        )
    ;
        CandidateArgVars1 = set__intersect_list(PathViaCellVars),
        FixpointLoop = StackOptParams ^ fixpoint_loop,
        (
            FixpointLoop = no,
            BenefitNodeSets = BenefitNodeSets0,
            CostNodeSets = CostNodeSets0,
            ViaCellVars = CandidateArgVars1
        ;
            FixpointLoop = yes,
            apply_matching_loop(CellVar, CellVarFlushedLater,
                IntParams, StackOptParams, PathInfos, CandidateArgVars1,
                BenefitNodeSets, CostNodeSets, ViaCellVars)
        )
    ).

:- pred apply_matching_for_path(prog_var::in, bool::in, stack_opt_params::in,
    set(prog_var)::in, match_path_info::in,
    set(benefit_node)::out, set(cost_node)::out, set(prog_var)::out) is det.

apply_matching_for_path(CellVar, CellVarFlushedLater, StackOptParams,
        CandidateArgVars, PathInfo, BenefitNodes, CostNodes, ViaCellVars) :-
    ( set__empty(CandidateArgVars) ->
        BenefitNodes = set__init,
        CostNodes = set__init,
        ViaCellVars = set__init
    ;
        PathInfo = match_path_info(FirstSegment, LaterSegments),
        MatchingParams = StackOptParams ^ matching_params,
        find_via_cell_vars(CellVar, CandidateArgVars, CellVarFlushedLater,
            FirstSegment, LaterSegments, MatchingParams,
            BenefitNodes, CostNodes, ViaCellVars)
    ).

:- pred record_matching_result(prog_var::in, cons_id::in, list(prog_var)::in,
    set(prog_var)::in, hlds_goal::in, set(anchor)::in,
    set(interval_id)::in, interval_info::in, interval_info::out,
    stack_opt_info::in, stack_opt_info::out) is det.

record_matching_result(CellVar, ConsId, ArgVars, ViaCellVars, Goal,
        PotentialAnchors, PotentialIntervals, !IntervalInfo, !StackOptInfo) :-
    ( set__empty(ViaCellVars) ->
        true
    ;
        set__to_sorted_list(PotentialIntervals, PotentialIntervalList),
        set__to_sorted_list(PotentialAnchors, PotentialAnchorList),
        list__foldl3(record_cell_var_for_interval(CellVar, ViaCellVars),
            PotentialIntervalList, !IntervalInfo, !StackOptInfo,
            set__init, InsertIntervals),
        list__foldl3(add_anchor_inserts(Goal, ViaCellVars, InsertIntervals),
            PotentialAnchorList, !IntervalInfo, !StackOptInfo,
            set__init, InsertAnchors),
        Goal = _ - GoalInfo,
        goal_info_get_goal_path(GoalInfo, GoalPath),
        MatchingResult = matching_result(CellVar, ConsId,
            ArgVars, ViaCellVars, GoalPath,
            PotentialIntervals, InsertIntervals,
            PotentialAnchors, InsertAnchors),
        MatchingResults0 = !.StackOptInfo ^ matching_results,
        MatchingResults = [MatchingResult | MatchingResults0],
        !:StackOptInfo = !.StackOptInfo ^ matching_results := MatchingResults
    ).

:- pred record_cell_var_for_interval(prog_var::in, set(prog_var)::in,
    interval_id::in, interval_info::in, interval_info::out,
    stack_opt_info::in, stack_opt_info::out,
    set(interval_id)::in, set(interval_id)::out) is det.

record_cell_var_for_interval(CellVar, ViaCellVars, IntervalId,
        !IntervalInfo, !StackOptInfo, !InsertIntervals) :-
    record_interval_vars(IntervalId, [CellVar], !IntervalInfo),
    delete_interval_vars(IntervalId, ViaCellVars, DeletedVars, !IntervalInfo),
    ( set__non_empty(DeletedVars) ->
        svset__insert(IntervalId, !InsertIntervals)
    ;
        true
    ).

:- pred add_anchor_inserts(hlds_goal::in, set(prog_var)::in,
    set(interval_id)::in, anchor::in, interval_info::in,
    interval_info::out, stack_opt_info::in, stack_opt_info::out,
    set(anchor)::in, set(anchor)::out) is det.

add_anchor_inserts(Goal, ArgVarsViaCellVar, InsertIntervals, Anchor,
        !IntervalInfo, !StackOptInfo, !InsertAnchors) :-
    map__lookup(!.IntervalInfo ^ anchor_follow_map, Anchor, AnchorFollow),
    AnchorFollow = _ - AnchorIntervals,
    set__intersect(AnchorIntervals, InsertIntervals,
        AnchorInsertIntervals),
    ( set__non_empty(AnchorInsertIntervals) ->
        Insert = insert_spec(Goal, ArgVarsViaCellVar),
        InsertMap0 = !.StackOptInfo ^ left_anchor_inserts,
        ( map__search(InsertMap0, Anchor, Inserts0) ->
            Inserts = [Insert | Inserts0],
            svmap__det_update(Anchor, Inserts, InsertMap0, InsertMap)
        ;
            Inserts = [Insert],
            svmap__det_insert(Anchor, Inserts, InsertMap0, InsertMap)
        ),
        !:StackOptInfo = !.StackOptInfo ^ left_anchor_inserts := InsertMap,
        svset__insert(Anchor, !InsertAnchors)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type current_segment_first_flush
    --->    current_is_before_first_flush
    ;       current_is_after_first_flush.

:- type path
    --->    path(
                flush_state             :: current_segment_first_flush,
                current_segment         :: set(prog_var),
                first_segment           :: set(prog_var),
                other_segments          :: list(set(prog_var)),
                flush_anchors           :: set(anchor),
                occurring_intervals     :: set(interval_id)
            ).

:- type all_paths
    --->    all_paths(
                paths_so_far            :: set(path),
                                        % The set of all paths so far.
                stepped_over_model_non  :: bool,
                                        % Have we stepped over
                                        % model_non goals?
                used_after_scope        :: set(prog_var)
                                        % The vars which are known to be used
                                        % after the deconstruction goes out of
                                        % scope.
            ).

:- pred extract_match_and_save_info(path::in, match_path_info::out,
    set(anchor)::out, set(interval_id)::out) is det.

extract_match_and_save_info(Path0, MatchPathInfo, Anchors, Intervals) :-
    Path = close_path(Path0),
    FirstSegment = Path ^ first_segment,
    OtherSegments = Path ^ other_segments,
    MatchPathInfo = match_path_info(FirstSegment, OtherSegments),
    Anchors = Path ^ flush_anchors,
    Intervals = Path ^ occurring_intervals.

:- func close_path(path) = path.

close_path(Path0) = Path :-
    Path0 = path(FlushState, CurSegment, FirstSegment0, OtherSegments0,
        FlushAnchors, IntervalIds),
    ( FlushState = current_is_before_first_flush ->
        require(set__empty(FirstSegment0),
            "close_path: FirstSegment0 not empty"),
        FirstSegment = CurSegment,
        OtherSegments = OtherSegments0
    ; set__empty(CurSegment) ->
        FirstSegment = FirstSegment0,
        OtherSegments = OtherSegments0
    ;
        FirstSegment = FirstSegment0,
        OtherSegments = [CurSegment | OtherSegments0]
    ),
    Path = path(current_is_after_first_flush, set__init,
        FirstSegment, OtherSegments, FlushAnchors, IntervalIds).

:- func add_interval_to_path(interval_id, set(prog_var), path) = path.

add_interval_to_path(IntervalId, Vars, !.Path) = !:Path :-
    ( set__empty(Vars) ->
        true
    ;
        CurSegment0 = !.Path ^ current_segment,
        CurSegment = set__union(Vars, CurSegment0),
        OccurringIntervals0 = !.Path ^ occurring_intervals,
        svset__insert(IntervalId, OccurringIntervals0, OccurringIntervals),
        !:Path = !.Path ^ current_segment := CurSegment,
        !:Path = !.Path ^ occurring_intervals := OccurringIntervals
    ).

:- func add_anchor_to_path(anchor, path) = path.

add_anchor_to_path(Anchor, !.Path) = !:Path :-
    Anchors0 = !.Path ^ flush_anchors,
    svset__insert(Anchor, Anchors0, Anchors),
    !:Path = !.Path ^ flush_anchors := Anchors.

:- func anchor_requires_close(interval_info, anchor) = bool.

anchor_requires_close(_, proc_start) = no.
anchor_requires_close(_, proc_end) = yes.
anchor_requires_close(IntervalInfo, branch_start(_, GoalPath)) =
        resume_save_status_requires_close(ResumeSaveStatus) :-
    map__lookup(IntervalInfo ^ branch_resume_map, GoalPath, ResumeSaveStatus).
anchor_requires_close(_, cond_then(_)) = no.
anchor_requires_close(_, branch_end(BranchConstruct, _)) =
    ( BranchConstruct = neg ->
        no
    ;
        yes
    ).
anchor_requires_close(_, call_site(_)) = yes.

:- func resume_save_status_requires_close(resume_save_status) = bool.

resume_save_status_requires_close(has_resume_save) = yes.
resume_save_status_requires_close(has_no_resume_save) = no.

:- pred may_have_no_successor(anchor::in) is semidet.

may_have_no_successor(Anchor) :-
    may_have_no_successor(Anchor, yes).

:- pred may_have_no_successor(anchor::in, bool::out) is det.

may_have_no_successor(proc_start, no).
may_have_no_successor(proc_end, yes).
may_have_no_successor(branch_start(_, _), no).
may_have_no_successor(cond_then(_), no).
may_have_no_successor(branch_end(_, _), no).
may_have_no_successor(call_site(_), yes).   % if the call cannot succeed

:- pred may_have_one_successor(anchor::in) is semidet.

may_have_one_successor(Anchor) :-
    may_have_one_successor(Anchor, yes).

:- pred may_have_one_successor(anchor::in, bool::out) is det.

may_have_one_successor(proc_start, yes).
may_have_one_successor(proc_end, no).
may_have_one_successor(branch_start(_, _), yes).
may_have_one_successor(cond_then(_), yes).
may_have_one_successor(branch_end(_, _), yes).
may_have_one_successor(call_site(_), yes).

:- pred may_have_more_successors(anchor::in) is semidet.

may_have_more_successors(Anchor) :-
    may_have_more_successors(Anchor, yes).

:- pred may_have_more_successors(anchor::in, bool::out) is det.

may_have_more_successors(proc_start, no).
may_have_more_successors(proc_end, no).
may_have_more_successors(branch_start(Type, _), MayHave) :-
    ( Type = neg ->
        MayHave = no
    ;
        MayHave = yes
    ).
may_have_more_successors(cond_then(_), no).
may_have_more_successors(branch_end(_, _), no).
may_have_more_successors(call_site(_), no).

%-----------------------------------------------------------------------------%

:- pred find_all_branches_from_cur_interval(set(prog_var)::in,
    match_info::out, interval_info::in, stack_opt_info::in) is det.

find_all_branches_from_cur_interval(RelevantVars, MatchInfo, IntervalInfo,
        StackOptInfo) :-
    IntervalId = IntervalInfo ^ cur_interval,
    map__lookup(IntervalInfo ^ interval_vars, IntervalId, IntervalVars),
    IntervalRelevantVars = set__intersect(RelevantVars, IntervalVars),
    Path0 = path(current_is_before_first_flush, IntervalRelevantVars,
        set__init, [], set__init, set__init),
    AllPaths0 = all_paths(set__make_singleton_set(Path0), no, set__init),
    find_all_branches(RelevantVars, IntervalId, no, IntervalInfo,
        StackOptInfo, AllPaths0, AllPaths),
    AllPaths = all_paths(Paths, AfterModelNon, RelevantAfter),
    set__to_sorted_list(Paths, PathList),
    list__map3(extract_match_and_save_info, PathList,
        MatchInputs, FlushAnchorSets, OccurringIntervalSets),
    FlushAnchors = set__union_list(FlushAnchorSets),
    OccurringIntervals = set__union_list(OccurringIntervalSets),
    MatchInfo = match_info(MatchInputs, RelevantAfter, AfterModelNon,
        FlushAnchors, OccurringIntervals).

:- pred find_all_branches(set(prog_var)::in, interval_id::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    all_paths::in, all_paths::out) is det.

find_all_branches(RelevantVars, IntervalId, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, !AllPaths) :-
    map__lookup(IntervalInfo ^ interval_end, IntervalId, End),
    map__lookup(IntervalInfo ^ interval_succ, IntervalId, SuccessorIds),
    (
        SuccessorIds = [],
        require(may_have_no_successor(End),
            "find_all_branches: unexpected no successor")
        % require(unify(MaybeSearchAnchor0, no),
        %   "find_all_branches: no successor while in search"),
        % that test may fail if we come to a call that cannot succeed
    ;
        SuccessorIds = [SuccessorId | MoreSuccessorIds],
        (
            MoreSuccessorIds = [],
            require(may_have_one_successor(End),
                "find_all_branches: unexpected one successor")
        ;
            MoreSuccessorIds = [_ | _],
            require(may_have_more_successors(End),
                "find_all_branches: unexpected more successors")
        ),
        (
            MaybeSearchAnchor0 = yes(SearchAnchor0),
            End = SearchAnchor0
        ->
            !:AllPaths = !.AllPaths ^ used_after_scope := set__init
        ;
            End = branch_end(_, EndGoalPath),
            map__lookup(IntervalInfo ^ branch_end_map, EndGoalPath,
                BranchEndInfo),
            OnStackAfterBranch = BranchEndInfo ^ flushed_after_branch,
            AccessedAfterBranch = BranchEndInfo ^ accessed_after_branch,
            NeededAfterBranch = set__union(OnStackAfterBranch,
                AccessedAfterBranch),
            RelevantAfter = set__intersect(RelevantVars, NeededAfterBranch),
            set__non_empty(RelevantAfter)
        ->
            !:AllPaths = !.AllPaths ^ used_after_scope := RelevantAfter
        ;
            find_all_branches_from(End, RelevantVars,
                MaybeSearchAnchor0, IntervalInfo, StackOptInfo,
                [SuccessorId | MoreSuccessorIds], !AllPaths)
        )
    ).

:- pred find_all_branches_from(anchor::in, set(prog_var)::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    list(interval_id)::in, all_paths::in, all_paths::out) is det.

find_all_branches_from(End, RelevantVars, MaybeSearchAnchor0, IntervalInfo,
        StackOptInfo, SuccessorIds, !AllPaths) :-
    ( anchor_requires_close(IntervalInfo, End) = yes ->
        Paths0 = !.AllPaths ^ paths_so_far,
        Paths1 = set__map(close_path, Paths0),
        !:AllPaths = !.AllPaths ^ paths_so_far := Paths1
    ;
        true
    ),
    StackOptParams = StackOptInfo ^ stack_opt_params,
    FullPath = StackOptParams ^ full_path,
    (
        FullPath = yes,
        End = branch_start(disj, EndGoalPath)
    ->
        MaybeSearchAnchor1 = yes(branch_end(disj, EndGoalPath)),
        one_after_another(RelevantVars, MaybeSearchAnchor1,
            IntervalInfo, StackOptInfo, SuccessorIds, !AllPaths),
        map__lookup(IntervalInfo ^ branch_end_map, EndGoalPath,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchor0, IntervalInfo, StackOptInfo,
            ContinueId, !AllPaths)
    ;
        FullPath = yes,
        End = branch_start(ite, EndGoalPath)
    ->
        ( SuccessorIds = [ElseStartIdPrime, CondStartIdPrime] ->
            ElseStartId = ElseStartIdPrime,
            CondStartId = CondStartIdPrime
        ;
            error("find_all_branches_from: ite not else, cond")
        ),
        MaybeSearchAnchorCond = yes(cond_then(EndGoalPath)),
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchorCond, IntervalInfo, StackOptInfo,
            CondStartId, !AllPaths),
        MaybeSearchAnchorEnd = yes(branch_end(ite, EndGoalPath)),
        CondEndMap = IntervalInfo ^ cond_end_map,
        map__lookup(CondEndMap, EndGoalPath, ThenStartId),
        one_after_another(RelevantVars, MaybeSearchAnchorEnd,
            IntervalInfo, StackOptInfo, [ThenStartId, ElseStartId],
            !AllPaths),
        map__lookup(IntervalInfo ^ branch_end_map, EndGoalPath,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchor0, IntervalInfo, StackOptInfo,
            ContinueId, !AllPaths)
    ;
        End = branch_start(BranchType, EndGoalPath)
    ->
        MaybeSearchAnchor1 = yes(branch_end(BranchType, EndGoalPath)),
        list__map(apply_interval_find_all_branches_map(RelevantVars,
            MaybeSearchAnchor1, IntervalInfo, StackOptInfo, !.AllPaths),
            SuccessorIds, AllPathsList),
        consolidate_after_join(AllPathsList, !:AllPaths),
        map__lookup(IntervalInfo ^ branch_end_map, EndGoalPath, BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchor0, IntervalInfo, StackOptInfo,
            ContinueId, !AllPaths)
    ;
        ( SuccessorIds = [SuccessorId] ->
            apply_interval_find_all_branches(RelevantVars,
                MaybeSearchAnchor0, IntervalInfo,
                StackOptInfo, SuccessorId, !AllPaths)
        ;
            error("more successor ids")
        )
    ).

:- pred one_after_another(set(prog_var)::in, maybe(anchor)::in,
    interval_info::in, stack_opt_info::in, list(interval_id)::in,
    all_paths::in, all_paths::out) is det.

one_after_another(_, _, _, _, [], !AllPaths).
one_after_another(RelevantVars, MaybeSearchAnchor1, IntervalInfo, StackOptInfo,
        [SuccessorId | MoreSuccessorIds], !AllPaths) :-
    apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor1,
        IntervalInfo, StackOptInfo, SuccessorId, !AllPaths),
    one_after_another(RelevantVars, MaybeSearchAnchor1, IntervalInfo,
        StackOptInfo, MoreSuccessorIds, !AllPaths).

    % We need a version of apply_interval_find_all_branches with this
    % argument order for use in higher order caode.
    %
:- pred apply_interval_find_all_branches_map(set(prog_var)::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    all_paths::in, interval_id::in, all_paths::out) is det.

apply_interval_find_all_branches_map(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, !.AllPaths, IntervalId,
        !:AllPaths) :-
    apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths).

:- pred apply_interval_find_all_branches(set(prog_var)::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    interval_id::in, all_paths::in, all_paths::out) is det.

apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths) :-
    map__lookup(IntervalInfo ^ interval_vars, IntervalId, IntervalVars),
    RelevantIntervalVars = set__intersect(RelevantVars, IntervalVars),
    !.AllPaths = all_paths(Paths0, AfterModelNon0, RelevantAfter),
    Paths1 = set__map(add_interval_to_path(IntervalId, RelevantIntervalVars),
        Paths0),
    map__lookup(IntervalInfo ^ interval_start, IntervalId, Start),
    (
        % Check if intervals starting at Start use any RelevantVars.
        ( Start = call_site(_)
        ; Start = branch_end(_, _)
        ; Start = branch_start(_, _)
        ),
        map__search(IntervalInfo ^ anchor_follow_map, Start, StartInfo),
        StartInfo = AnchorFollowVars - _,
        set__intersect(RelevantVars, AnchorFollowVars, NeededVars),
        set__non_empty(NeededVars)
    ->
        Paths2 = set__map(add_anchor_to_path(Start), Paths1)
    ;
        Paths2 = Paths1
    ),
    ( set__member(Start, IntervalInfo ^ model_non_anchors) ->
        AfterModelNon = yes
    ;
        AfterModelNon = AfterModelNon0
    ),
    !:AllPaths = all_paths(Paths2, AfterModelNon, RelevantAfter),
    find_all_branches(RelevantVars, IntervalId,
        MaybeSearchAnchor0, IntervalInfo, StackOptInfo, !AllPaths).

:- pred consolidate_after_join(list(all_paths)::in, all_paths::out) is det.

consolidate_after_join([], _) :-
    error("consolidate_after_join: no paths to join").
consolidate_after_join([First | Rest], AllPaths) :-
    PathsList = list__map(project_paths_from_all_paths, [First | Rest]),
    Paths0 = set__union_list(PathsList),
    Paths = compress_paths(Paths0),
    AfterModelNonList = list__map(project_after_model_non_from_all_paths,
        [First | Rest]),
    bool__or_list(AfterModelNonList, AfterModelNon),
    AllPaths = all_paths(Paths, AfterModelNon, set__init).

:- func project_paths_from_all_paths(all_paths) = set(path).

project_paths_from_all_paths(all_paths(Paths, _, _)) = Paths.

:- func project_after_model_non_from_all_paths(all_paths) = bool.

project_after_model_non_from_all_paths(all_paths(_, AfterModelNon, _)) =
    AfterModelNon.

:- func compress_paths(set(path)) = set(path).

compress_paths(Paths) = Paths.
    % XXX should reduce the cardinality of Paths below a threshold.
    % XXX should try to preserve the current segment.

%-----------------------------------------------------------------------------%

% This predicate can help debug the correctness of the transformation.

:- pred maybe_write_progress_message(string::in, int::in, int::in,
    proc_info::in, module_info::in, io::di, io::uo) is det.

maybe_write_progress_message(Message, DebugStackOpt, PredIdInt, ProcInfo,
        ModuleInfo, !IO) :-
    ( DebugStackOpt = PredIdInt ->
        io__write_string(Message, !IO),
        io__write_string(":\n", !IO),
        proc_info_goal(ProcInfo, Goal),
        proc_info_varset(ProcInfo, VarSet),
        hlds_out__write_goal(Goal, ModuleInfo, VarSet, yes, 0, "\n", !IO),
        io__write_string("\n", !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % This predicate (along with dump_interval_info) can help debug the
    % performance of the transformation.
    %
:- pred dump_stack_opt_info(stack_opt_info::in, io::di, io::uo) is det.

dump_stack_opt_info(StackOptInfo, !IO) :-
    map__to_assoc_list(StackOptInfo ^ left_anchor_inserts, Inserts),
    io__write_string("\nANCHOR INSERT:\n", !IO),
    list__foldl(dump_anchor_inserts, Inserts, !IO),

    io__write_string("\nMATCHING RESULTS:\n", !IO),
    list__foldl(dump_matching_result, StackOptInfo ^ matching_results, !IO),
    io__write_string("\n", !IO).

:- pred dump_anchor_inserts(pair(anchor, list(insert_spec))::in,
    io::di, io::uo) is det.

dump_anchor_inserts(Anchor - InsertSpecs, !IO) :-
    io__write_string("\ninsertions after ", !IO),
    io__write(Anchor, !IO),
    io__write_string(":\n", !IO),
    list__foldl(dump_insert, InsertSpecs, !IO).

:- pred dump_insert(insert_spec::in, io::di, io::uo) is det.

dump_insert(insert_spec(Goal, Vars), !IO) :-
    list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums),
    io__write_string("vars [", !IO),
    write_int_list(VarNums, !IO),
    io__write_string("]: ", !IO),
    (
        Goal = unify(_, _, _, Unification, _) - _,
        Unification = deconstruct(CellVar, ConsId, ArgVars, _,_,_)
    ->
        term__var_to_int(CellVar, CellVarNum),
        io__write_int(CellVarNum, !IO),
        io__write_string(" => ", !IO),
        mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
        io__write_string("(", !IO),
        list__map(term__var_to_int, ArgVars, ArgVarNums),
        write_int_list(ArgVarNums, !IO),
        io__write_string(")\n", !IO)
    ;
        io__write_string("BAD INSERT GOAL\n", !IO)
    ).

:- pred dump_matching_result(matching_result::in,
    io::di, io::uo) is det.

dump_matching_result(MatchingResult, !IO) :-
    MatchingResult = matching_result(CellVar, ConsId, ArgVars, ViaCellVars,
        GoalPath, PotentialIntervals, InsertIntervals,
        PotentialAnchors, InsertAnchors),
    io__write_string("\nmatching result at ", !IO),
    io__write(GoalPath, !IO),
    io__write_string("\n", !IO),
    term__var_to_int(CellVar, CellVarNum),
    list__map(term__var_to_int, ArgVars, ArgVarNums),
    list__map(term__var_to_int, set__to_sorted_list(ViaCellVars),
        ViaCellVarNums),
    io__write_int(CellVarNum, !IO),
    io__write_string(" => ", !IO),
    mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
    io__write_string("(", !IO),
    write_int_list(ArgVarNums, !IO),
    io__write_string("): via cell ", !IO),
    write_int_list(ViaCellVarNums, !IO),
    io__write_string("\n", !IO),

    io__write_string("potential intervals: ", !IO),
    PotentialIntervalNums = list__map(interval_id_to_int,
        set__to_sorted_list(PotentialIntervals)),
    write_int_list(PotentialIntervalNums, !IO),
    io__write_string("\n", !IO),
    io__write_string("insert intervals: ", !IO),
    InsertIntervalNums = list__map(interval_id_to_int,
        set__to_sorted_list(InsertIntervals)),
    write_int_list(InsertIntervalNums, !IO),
    io__write_string("\n", !IO),

    io__write_string("potential anchors: ", !IO),
    io__write_list(set__to_sorted_list(PotentialAnchors), " ", io__write, !IO),
    io__write_string("\n", !IO),
    io__write_string("insert anchors: ", !IO),
    io__write_list(set__to_sorted_list(InsertAnchors), " ", io__write, !IO),
    io__write_string("\n", !IO).

%-----------------------------------------------------------------------------%
