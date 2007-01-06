%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_opt.
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

:- module ll_backend.stack_opt.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred stack_opt_cell(pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.interval.
:- import_module backend_libs.matching.
:- import_module check_hlds.goal_path.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.simplify.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.live_vars.
:- import_module ll_backend.liveness.
:- import_module ll_backend.store_alloc.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module svmap.
:- import_module svset.
:- import_module term.

%-----------------------------------------------------------------------------%

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
    Simplications = list_to_simplifications([]),
    simplify_proc(Simplications, PredId, ProcId, !ModuleInfo, !ProcInfo, !IO),
    detect_liveness_proc(PredId, ProcId, !.ModuleInfo, !ProcInfo, !IO),
    initial_liveness(!.ProcInfo, PredId, !.ModuleInfo, Liveness0),
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    AllocData = alloc_data(!.ModuleInfo, !.ProcInfo, TypeInfoLiveness,
        OptNoReturnCalls),
    fill_goal_path_slots(!.ModuleInfo, !ProcInfo),
    proc_info_get_goal(!.ProcInfo, Goal2),
    OptStackAlloc0 = init_opt_stack_alloc,
    set.init(FailVars),
    set.init(NondetLiveness0),
    build_live_sets_in_goal_no_par_stack(Goal2, Goal, FailVars, AllocData,
        OptStackAlloc0, OptStackAlloc, Liveness0, _Liveness,
        NondetLiveness0, _NondetLiveness),
    proc_info_set_goal(Goal, !ProcInfo),
    allocate_store_maps(for_stack_opt, PredId, !.ModuleInfo, !ProcInfo),
    globals.lookup_int_option(Globals, debug_stack_opt, DebugStackOpt),
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

init_opt_stack_alloc = opt_stack_alloc(set.init).

:- pred optimize_live_sets(module_info::in, opt_stack_alloc::in,
    proc_info::in, proc_info::out, bool::out, int::in, int::in,
    io::di, io::uo) is det.

optimize_live_sets(ModuleInfo, OptAlloc, !ProcInfo, Changed, DebugStackOpt,
        PredIdInt, !IO) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    OptAlloc = opt_stack_alloc(ParConjOwnSlot),
    arg_info.partition_proc_args(!.ProcInfo, ModuleInfo,
        InputArgs, OutputArgs, UnusedArgs),
    HeadVars = set.union_list([InputArgs, OutputArgs, UnusedArgs]),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals,
        optimize_saved_vars_cell_candidate_headvars, CandHeadvars),
    (
        CandHeadvars = no,
        set.union(HeadVars, ParConjOwnSlot, NonCandidateVars)
    ;
        CandHeadvars = yes,
        NonCandidateVars = ParConjOwnSlot
    ),
    Counter0 = counter.init(1),
    counter.allocate(CurInterval, Counter0, Counter1),
    CurIntervalId = interval_id(CurInterval),
    EndMap0 = map.det_insert(map.init, CurIntervalId, anchor_proc_end),
    InsertMap0 = map.init,
    StartMap0 = map.init,
    SuccMap0 = map.det_insert(map.init, CurIntervalId, []),
    VarsMap0 = map.det_insert(map.init, CurIntervalId, OutputArgs),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_store_cost, CellVarStoreCost),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_cv_load_cost, CellVarLoadCost),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_store_cost, FieldVarStoreCost),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_fv_load_cost, FieldVarLoadCost),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_op_ratio, OpRatio),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_node_ratio, NodeRatio),
    globals.lookup_bool_option(Globals,
        optimize_saved_vars_cell_include_all_candidates, InclAllCand),
    MatchingParams = matching_params(CellVarStoreCost, CellVarLoadCost,
        FieldVarStoreCost, FieldVarLoadCost, OpRatio, NodeRatio,
        InclAllCand),
    globals.lookup_int_option(Globals,
        optimize_saved_vars_cell_all_path_node_ratio,
        AllPathNodeRatio),
    globals.lookup_bool_option(Globals,
        optimize_saved_vars_cell_loop, FixpointLoop),
    globals.lookup_bool_option(Globals,
        optimize_saved_vars_cell_full_path, FullPath),
    globals.lookup_bool_option(Globals,
        optimize_saved_vars_cell_on_stack, OnStack),
    globals.lookup_bool_option(Globals,
        opt_no_return_calls, OptNoReturnCalls),
    IntParams = interval_params(ModuleInfo, VarTypes0, OptNoReturnCalls),
    IntervalInfo0 = interval_info(IntParams, set.init, OutputArgs,
        map.init, map.init, map.init, CurIntervalId, Counter1,
        set.make_singleton_set(CurIntervalId),
        map.init, set.init, StartMap0, EndMap0,
        SuccMap0, VarsMap0, map.init),
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
    ( map.is_empty(InsertMap) ->
        Changed = no
    ;
        record_decisions_in_goal(Goal0, Goal1, VarSet0, VarSet,
            VarTypes0, VarTypes, map.init, RenameMap,
            InsertMap, yes(feature_stack_opt)),
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
    ParConjOwnSlots = set.union(StackVars, ParConjOwnSlots0),
    StackAlloc = StackAlloc0 ^ par_conj_own_slots := ParConjOwnSlots.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance build_interval_info_acc(stack_opt_info) where [
    pred(use_cell/8) is stack_opt.use_cell
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
    set.list_to_set(FieldVarList, FieldVars),
    set.intersect(FieldVars, FlushedLater, FlushedLaterFieldVars),
    set.difference(FlushedLaterFieldVars, NonCandidateVars,
        CandidateArgVars0),
    (
        set.empty(CandidateArgVars0)
    ->
        true
    ;
        ConsId = cons(_Name, _Arity),
        IntParams = !.IntervalInfo ^ interval_params,
        VarTypes = IntParams ^ var_types,
        map.lookup(VarTypes, CellVar, Type),
        (
            type_is_tuple(Type, _)
        ->
            FreeOfCost = no
        ;
            type_to_ctor_and_args(Type, TypeCtor, _),
            ModuleInfo = IntParams ^ module_info,
            module_info_get_type_table(ModuleInfo, TypeTable),
            map.lookup(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            ConsTable = TypeBody ^ du_type_cons_tag_values
        ->
            map.lookup(ConsTable, ConsId, ConsTag),
            ( ConsTag = no_tag ->
                FreeOfCost = yes
            ;
                FreeOfCost = no
            )
        ;
            fail
        )
    ->
        RelevantVars = set.insert(FieldVars, CellVar),
        find_all_branches_from_cur_interval(RelevantVars, MatchInfo,
            !.IntervalInfo, !.StackOptInfo),
        MatchInfo = match_info(PathsInfo, RelevantAfterVars,
            AfterModelNon, InsertAnchors, InsertIntervals),
        (
            FreeOfCost = yes,
            set.difference(CandidateArgVars0, RelevantAfterVars, ViaCellVars),
            record_matching_result(CellVar, ConsId, FieldVarList, ViaCellVars,
                Goal, InsertAnchors, InsertIntervals, !IntervalInfo,
                !StackOptInfo)
        ;
            FreeOfCost = no,
            (
                AfterModelNon = no,
                OnStack = StackOptParams ^ on_stack,
                set.difference(CandidateArgVars0, RelevantAfterVars,
                    CandidateArgVars),
                (
                    OnStack = yes,
                    ( set.member(CellVar, FlushedLater) ->
                        CellVarFlushedLater = yes
                    ;
                        CellVarFlushedLater = no
                    )
                ;
                    OnStack = no,
                    (
                        list.member(PathInfo, PathsInfo),
                        PathInfo = match_path_info(_, Segments),
                        list.member(Segment, Segments),
                        set.member(CellVar, Segment)
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
    BenefitNodes = set.union_list(BenefitNodeSets),
    CostNodes = set.union_list(CostNodeSets),
    set.count(BenefitNodes, NumBenefitNodes),
    set.count(CostNodes, NumCostNodes),
    AllPathNodeRatio = StackOptParams ^ all_path_node_ratio,
    ( NumBenefitNodes * 100 >= NumCostNodes * AllPathNodeRatio ->
        ViaCellVars = ViaCellVars0
    ;
        ViaCellVars = set.init
    ).

:- pred apply_matching_loop(prog_var::in, bool::in, interval_params::in,
    stack_opt_params::in, list(match_path_info)::in, set(prog_var)::in,
    list(set(benefit_node))::out, list(set(cost_node))::out,
    set(prog_var)::out) is det.

apply_matching_loop(CellVar, CellVarFlushedLater, IntParams, StackOptParams,
        PathInfos, CandidateArgVars0, BenefitNodeSets, CostNodeSets,
        ViaCellVars) :-
    list.map3(apply_matching_for_path(CellVar, CellVarFlushedLater,
        StackOptParams, CandidateArgVars0), PathInfos,
        BenefitNodeSets0, CostNodeSets0, PathViaCellVars),
    ( list.all_same(PathViaCellVars) ->
        BenefitNodeSets = BenefitNodeSets0,
        CostNodeSets = CostNodeSets0,
        ( PathViaCellVars = [ViaCellVarsPrime | _] ->
            ViaCellVars = ViaCellVarsPrime
        ;
            ViaCellVars = set.init
        )
    ;
        CandidateArgVars1 = set.intersect_list(PathViaCellVars),
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
    ( set.empty(CandidateArgVars) ->
        BenefitNodes = set.init,
        CostNodes = set.init,
        ViaCellVars = set.init
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
    ( set.empty(ViaCellVars) ->
        true
    ;
        set.to_sorted_list(PotentialIntervals, PotentialIntervalList),
        set.to_sorted_list(PotentialAnchors, PotentialAnchorList),
        list.foldl3(record_cell_var_for_interval(CellVar, ViaCellVars),
            PotentialIntervalList, !IntervalInfo, !StackOptInfo,
            set.init, InsertIntervals),
        list.foldl3(add_anchor_inserts(Goal, ViaCellVars, InsertIntervals),
            PotentialAnchorList, !IntervalInfo, !StackOptInfo,
            set.init, InsertAnchors),
        Goal = hlds_goal(_, GoalInfo),
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
    ( set.non_empty(DeletedVars) ->
        svset.insert(IntervalId, !InsertIntervals)
    ;
        true
    ).

:- pred add_anchor_inserts(hlds_goal::in, set(prog_var)::in,
    set(interval_id)::in, anchor::in, interval_info::in,
    interval_info::out, stack_opt_info::in, stack_opt_info::out,
    set(anchor)::in, set(anchor)::out) is det.

add_anchor_inserts(Goal, ArgVarsViaCellVar, InsertIntervals, Anchor,
        !IntervalInfo, !StackOptInfo, !InsertAnchors) :-
    map.lookup(!.IntervalInfo ^ anchor_follow_map, Anchor, AnchorFollow),
    AnchorFollow = anchor_follow_info(_, AnchorIntervals),
    set.intersect(AnchorIntervals, InsertIntervals,
        AnchorInsertIntervals),
    ( set.non_empty(AnchorInsertIntervals) ->
        Insert = insert_spec(Goal, ArgVarsViaCellVar),
        InsertMap0 = !.StackOptInfo ^ left_anchor_inserts,
        ( map.search(InsertMap0, Anchor, Inserts0) ->
            Inserts = [Insert | Inserts0],
            svmap.det_update(Anchor, Inserts, InsertMap0, InsertMap)
        ;
            Inserts = [Insert],
            svmap.det_insert(Anchor, Inserts, InsertMap0, InsertMap)
        ),
        !:StackOptInfo = !.StackOptInfo ^ left_anchor_inserts := InsertMap,
        svset.insert(Anchor, !InsertAnchors)
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
        expect(set.empty(FirstSegment0), this_file,
            "close_path: FirstSegment0 not empty"),
        FirstSegment = CurSegment,
        OtherSegments = OtherSegments0
    ; set.empty(CurSegment) ->
        FirstSegment = FirstSegment0,
        OtherSegments = OtherSegments0
    ;
        FirstSegment = FirstSegment0,
        OtherSegments = [CurSegment | OtherSegments0]
    ),
    Path = path(current_is_after_first_flush, set.init,
        FirstSegment, OtherSegments, FlushAnchors, IntervalIds).

:- func add_interval_to_path(interval_id, set(prog_var), path) = path.

add_interval_to_path(IntervalId, Vars, !.Path) = !:Path :-
    ( set.empty(Vars) ->
        true
    ;
        CurSegment0 = !.Path ^ current_segment,
        CurSegment = set.union(Vars, CurSegment0),
        OccurringIntervals0 = !.Path ^ occurring_intervals,
        svset.insert(IntervalId, OccurringIntervals0, OccurringIntervals),
        !:Path = !.Path ^ current_segment := CurSegment,
        !:Path = !.Path ^ occurring_intervals := OccurringIntervals
    ).

:- func add_anchor_to_path(anchor, path) = path.

add_anchor_to_path(Anchor, !.Path) = !:Path :-
    Anchors0 = !.Path ^ flush_anchors,
    svset.insert(Anchor, Anchors0, Anchors),
    !:Path = !.Path ^ flush_anchors := Anchors.

:- func anchor_requires_close(interval_info, anchor) = bool.

anchor_requires_close(_, anchor_proc_start) = no.
anchor_requires_close(_, anchor_proc_end) = yes.
anchor_requires_close(IntervalInfo, anchor_branch_start(_, GoalPath)) =
        resume_save_status_requires_close(ResumeSaveStatus) :-
    map.lookup(IntervalInfo ^ branch_resume_map, GoalPath, ResumeSaveStatus).
anchor_requires_close(_, anchor_cond_then(_)) = no.
anchor_requires_close(_, anchor_branch_end(BranchConstruct, _)) =
    ( BranchConstruct = branch_neg ->
        no
    ;
        yes
    ).
anchor_requires_close(_, anchor_call_site(_)) = yes.

:- func resume_save_status_requires_close(resume_save_status) = bool.

resume_save_status_requires_close(has_resume_save) = yes.
resume_save_status_requires_close(has_no_resume_save) = no.

:- func may_have_no_successor(anchor) = bool.

may_have_no_successor(anchor_proc_start) = no.
may_have_no_successor(anchor_proc_end) = yes.
may_have_no_successor(anchor_branch_start(_, _)) = no.
may_have_no_successor(anchor_cond_then(_)) = no.
may_have_no_successor(anchor_branch_end(_, _)) = no.
may_have_no_successor(anchor_call_site(_)) = yes. % if the call cannot succeed

:- func may_have_one_successor(anchor) = bool.

may_have_one_successor(anchor_proc_start) = yes.
may_have_one_successor(anchor_proc_end) = no.
may_have_one_successor(anchor_branch_start(_, _)) = yes.
may_have_one_successor(anchor_cond_then(_)) = yes.
may_have_one_successor(anchor_branch_end(_, _)) = yes.
may_have_one_successor(anchor_call_site(_)) = yes.

:- func may_have_more_successors(anchor) = bool.

may_have_more_successors(anchor_proc_start) = no.
may_have_more_successors(anchor_proc_end) = no.
may_have_more_successors(anchor_branch_start(BranchType, _)) =
    ( BranchType = branch_neg ->
        no
    ;
        yes
    ).
may_have_more_successors(anchor_cond_then(_)) = no.
may_have_more_successors(anchor_branch_end(_, _)) = no.
may_have_more_successors(anchor_call_site(_)) = no.

%-----------------------------------------------------------------------------%

:- pred find_all_branches_from_cur_interval(set(prog_var)::in,
    match_info::out, interval_info::in, stack_opt_info::in) is det.

find_all_branches_from_cur_interval(RelevantVars, MatchInfo, IntervalInfo,
        StackOptInfo) :-
    IntervalId = IntervalInfo ^ cur_interval,
    map.lookup(IntervalInfo ^ interval_vars, IntervalId, IntervalVars),
    IntervalRelevantVars = set.intersect(RelevantVars, IntervalVars),
    Path0 = path(current_is_before_first_flush, IntervalRelevantVars,
        set.init, [], set.init, set.init),
    AllPaths0 = all_paths(set.make_singleton_set(Path0), no, set.init),
    find_all_branches(RelevantVars, IntervalId, no, IntervalInfo,
        StackOptInfo, AllPaths0, AllPaths),
    AllPaths = all_paths(Paths, AfterModelNon, RelevantAfter),
    set.to_sorted_list(Paths, PathList),
    list.map3(extract_match_and_save_info, PathList,
        MatchInputs, FlushAnchorSets, OccurringIntervalSets),
    FlushAnchors = set.union_list(FlushAnchorSets),
    OccurringIntervals = set.union_list(OccurringIntervalSets),
    MatchInfo = match_info(MatchInputs, RelevantAfter, AfterModelNon,
        FlushAnchors, OccurringIntervals).

:- pred find_all_branches(set(prog_var)::in, interval_id::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    all_paths::in, all_paths::out) is det.

find_all_branches(RelevantVars, IntervalId, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, !AllPaths) :-
    map.lookup(IntervalInfo ^ interval_end, IntervalId, End),
    map.lookup(IntervalInfo ^ interval_succ, IntervalId, SuccessorIds),
    (
        SuccessorIds = [],
        expect(unify(may_have_no_successor(End), yes), this_file,
            "find_all_branches: unexpected no successor")
        % expect(unify(MaybeSearchAnchor0, no), this_file,
        %   "find_all_branches: no successor while in search"),
        % that test may fail if we come to a call that cannot succeed
    ;
        SuccessorIds = [SuccessorId | MoreSuccessorIds],
        (
            MoreSuccessorIds = [],
            expect(unify(may_have_one_successor(End), yes), this_file,
                "find_all_branches: unexpected one successor")
        ;
            MoreSuccessorIds = [_ | _],
            expect(unify(may_have_more_successors(End), yes), this_file,
                "find_all_branches: unexpected more successors")
        ),
        (
            MaybeSearchAnchor0 = yes(SearchAnchor0),
            End = SearchAnchor0
        ->
            !:AllPaths = !.AllPaths ^ used_after_scope := set.init
        ;
            End = anchor_branch_end(_, EndGoalPath),
            map.lookup(IntervalInfo ^ branch_end_map, EndGoalPath,
                BranchEndInfo),
            OnStackAfterBranch = BranchEndInfo ^ flushed_after_branch,
            AccessedAfterBranch = BranchEndInfo ^ accessed_after_branch,
            NeededAfterBranch = set.union(OnStackAfterBranch,
                AccessedAfterBranch),
            RelevantAfter = set.intersect(RelevantVars, NeededAfterBranch),
            set.non_empty(RelevantAfter)
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
        Paths1 = set.map(close_path, Paths0),
        !:AllPaths = !.AllPaths ^ paths_so_far := Paths1
    ;
        true
    ),
    StackOptParams = StackOptInfo ^ stack_opt_params,
    FullPath = StackOptParams ^ full_path,
    (
        FullPath = yes,
        End = anchor_branch_start(branch_disj, EndGoalPath)
    ->
        MaybeSearchAnchor1 = yes(anchor_branch_end(branch_disj, EndGoalPath)),
        one_after_another(RelevantVars, MaybeSearchAnchor1,
            IntervalInfo, StackOptInfo, SuccessorIds, !AllPaths),
        map.lookup(IntervalInfo ^ branch_end_map, EndGoalPath, BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    ;
        FullPath = yes,
        End = anchor_branch_start(branch_ite, EndGoalPath)
    ->
        ( SuccessorIds = [ElseStartIdPrime, CondStartIdPrime] ->
            ElseStartId = ElseStartIdPrime,
            CondStartId = CondStartIdPrime
        ;
            unexpected(this_file,
                "find_all_branches_from: ite not else, cond")
        ),
        MaybeSearchAnchorCond = yes(anchor_cond_then(EndGoalPath)),
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchorCond, IntervalInfo, StackOptInfo,
            CondStartId, !AllPaths),
        MaybeSearchAnchorEnd = yes(anchor_branch_end(branch_ite, EndGoalPath)),
        CondEndMap = IntervalInfo ^ cond_end_map,
        map.lookup(CondEndMap, EndGoalPath, ThenStartId),
        one_after_another(RelevantVars, MaybeSearchAnchorEnd,
            IntervalInfo, StackOptInfo, [ThenStartId, ElseStartId], !AllPaths),
        map.lookup(IntervalInfo ^ branch_end_map, EndGoalPath,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    ;
        End = anchor_branch_start(BranchType, EndGoalPath)
    ->
        MaybeSearchAnchor1 = yes(anchor_branch_end(BranchType, EndGoalPath)),
        list.map(apply_interval_find_all_branches_map(RelevantVars,
            MaybeSearchAnchor1, IntervalInfo, StackOptInfo, !.AllPaths),
            SuccessorIds, AllPathsList),
        consolidate_after_join(AllPathsList, !:AllPaths),
        map.lookup(IntervalInfo ^ branch_end_map, EndGoalPath, BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    ;
        ( SuccessorIds = [SuccessorId] ->
            apply_interval_find_all_branches(RelevantVars,
                MaybeSearchAnchor0, IntervalInfo,
                StackOptInfo, SuccessorId, !AllPaths)
        ;
            unexpected(this_file,
                "find_all_branches_from: more successor ids")
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
        IntervalInfo, StackOptInfo, !.AllPaths, IntervalId, !:AllPaths) :-
    apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths).

:- pred apply_interval_find_all_branches(set(prog_var)::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    interval_id::in, all_paths::in, all_paths::out) is det.

apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths) :-
    map.lookup(IntervalInfo ^ interval_vars, IntervalId, IntervalVars),
    RelevantIntervalVars = set.intersect(RelevantVars, IntervalVars),
    !.AllPaths = all_paths(Paths0, AfterModelNon0, RelevantAfter),
    Paths1 = set.map(add_interval_to_path(IntervalId, RelevantIntervalVars),
        Paths0),
    map.lookup(IntervalInfo ^ interval_start, IntervalId, Start),
    (
        % Check if intervals starting at Start use any RelevantVars.
        ( Start = anchor_call_site(_)
        ; Start = anchor_branch_end(_, _)
        ; Start = anchor_branch_start(_, _)
        ),
        map.search(IntervalInfo ^ anchor_follow_map, Start, StartInfo),
        StartInfo = anchor_follow_info(AnchorFollowVars, _),
        set.intersect(RelevantVars, AnchorFollowVars, NeededVars),
        set.non_empty(NeededVars)
    ->
        Paths2 = set.map(add_anchor_to_path(Start), Paths1)
    ;
        Paths2 = Paths1
    ),
    ( set.member(Start, IntervalInfo ^ model_non_anchors) ->
        AfterModelNon = yes
    ;
        AfterModelNon = AfterModelNon0
    ),
    !:AllPaths = all_paths(Paths2, AfterModelNon, RelevantAfter),
    find_all_branches(RelevantVars, IntervalId,
        MaybeSearchAnchor0, IntervalInfo, StackOptInfo, !AllPaths).

:- pred consolidate_after_join(list(all_paths)::in, all_paths::out) is det.

consolidate_after_join([], _) :-
    unexpected(this_file, "consolidate_after_join: no paths to join").
consolidate_after_join([First | Rest], AllPaths) :-
    PathsList = list.map(project_paths_from_all_paths, [First | Rest]),
    Paths0 = set.union_list(PathsList),
    Paths = compress_paths(Paths0),
    AfterModelNonList = list.map(project_after_model_non_from_all_paths,
        [First | Rest]),
    bool.or_list(AfterModelNonList, AfterModelNon),
    AllPaths = all_paths(Paths, AfterModelNon, set.init).

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
        io.write_string(Message, !IO),
        io.write_string(":\n", !IO),
        proc_info_get_goal(ProcInfo, Goal),
        proc_info_get_varset(ProcInfo, VarSet),
        hlds_out.write_goal(Goal, ModuleInfo, VarSet, yes, 0, "\n", !IO),
        io.write_string("\n", !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % This predicate (along with dump_interval_info) can help debug the
    % performance of the transformation.
    %
:- pred dump_stack_opt_info(stack_opt_info::in, io::di, io::uo) is det.

dump_stack_opt_info(StackOptInfo, !IO) :-
    map.to_assoc_list(StackOptInfo ^ left_anchor_inserts, Inserts),
    io.write_string("\nANCHOR INSERT:\n", !IO),
    list.foldl(dump_anchor_inserts, Inserts, !IO),

    io.write_string("\nMATCHING RESULTS:\n", !IO),
    list.foldl(dump_matching_result, StackOptInfo ^ matching_results, !IO),
    io.write_string("\n", !IO).

:- pred dump_anchor_inserts(pair(anchor, list(insert_spec))::in,
    io::di, io::uo) is det.

dump_anchor_inserts(Anchor - InsertSpecs, !IO) :-
    io.write_string("\ninsertions after ", !IO),
    io.write(Anchor, !IO),
    io.write_string(":\n", !IO),
    list.foldl(dump_insert, InsertSpecs, !IO).

:- pred dump_insert(insert_spec::in, io::di, io::uo) is det.

dump_insert(insert_spec(Goal, Vars), !IO) :-
    list.map(term.var_to_int, set.to_sorted_list(Vars), VarNums),
    io.write_string("vars [", !IO),
    write_int_list(VarNums, !IO),
    io.write_string("]: ", !IO),
    (
        Goal = hlds_goal(unify(_, _, _, Unification, _), _),
        Unification = deconstruct(CellVar, ConsId, ArgVars, _,_,_)
    ->
        term.var_to_int(CellVar, CellVarNum),
        io.write_int(CellVarNum, !IO),
        io.write_string(" => ", !IO),
        mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
        io.write_string("(", !IO),
        list.map(term.var_to_int, ArgVars, ArgVarNums),
        write_int_list(ArgVarNums, !IO),
        io.write_string(")\n", !IO)
    ;
        io.write_string("BAD INSERT GOAL\n", !IO)
    ).

:- pred dump_matching_result(matching_result::in,
    io::di, io::uo) is det.

dump_matching_result(MatchingResult, !IO) :-
    MatchingResult = matching_result(CellVar, ConsId, ArgVars, ViaCellVars,
        GoalPath, PotentialIntervals, InsertIntervals,
        PotentialAnchors, InsertAnchors),
    io.write_string("\nmatching result at ", !IO),
    io.write(GoalPath, !IO),
    io.write_string("\n", !IO),
    term.var_to_int(CellVar, CellVarNum),
    list.map(term.var_to_int, ArgVars, ArgVarNums),
    list.map(term.var_to_int, set.to_sorted_list(ViaCellVars),
        ViaCellVarNums),
    io.write_int(CellVarNum, !IO),
    io.write_string(" => ", !IO),
    mercury_output_cons_id(ConsId, does_not_need_brackets, !IO),
    io.write_string("(", !IO),
    write_int_list(ArgVarNums, !IO),
    io.write_string("): via cell ", !IO),
    write_int_list(ViaCellVarNums, !IO),
    io.write_string("\n", !IO),

    io.write_string("potential intervals: ", !IO),
    PotentialIntervalNums = list.map(interval_id_to_int,
        set.to_sorted_list(PotentialIntervals)),
    write_int_list(PotentialIntervalNums, !IO),
    io.write_string("\n", !IO),
    io.write_string("insert intervals: ", !IO),
    InsertIntervalNums = list.map(interval_id_to_int,
        set.to_sorted_list(InsertIntervals)),
    write_int_list(InsertIntervalNums, !IO),
    io.write_string("\n", !IO),

    io.write_string("potential anchors: ", !IO),
    io.write_list(set.to_sorted_list(PotentialAnchors), " ", io.write, !IO),
    io.write_string("\n", !IO),
    io.write_string("insert anchors: ", !IO),
    io.write_list(set.to_sorted_list(InsertAnchors), " ", io.write, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "stack_opt.m".

%-----------------------------------------------------------------------------%
