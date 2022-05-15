%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
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

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred stack_opt_cell(pred_proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.interval.
:- import_module backend_libs.matching.
:- import_module check_hlds.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.goal_path.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.live_vars.
:- import_module ll_backend.liveness.
:- import_module ll_backend.store_alloc.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

% The opt_stack_alloc structure is constructed by live_vars.m. It contains
% the set of vars that definitely need their own stack slots, and which this
% optimization should not try to make reachable from a heap cell. At the
% moment, the only variables we treat this way are those that are required to
% be on the stack by a parallel conjunction or loop control scope.

:- type opt_stack_alloc
    --->    opt_stack_alloc(
                % XXX: this is an over-simplification, it gives stack slots to
                % variables that may not need them. For example, vars local to
                % loop control scopes and parallel conjunctions, And vars used
                % after a loop control scope but before a recursive call don't
                % need to be placed here.
                par_conj_own_slots      :: set_of_progvar
            ).

:- type stack_opt_params
    --->    stack_opt_params(
                % These two fields are used to find the appropriate
                % debug output stream.
                sop_globals             :: globals,
                sop_module_name         :: module_name,

                sop_matching_params     :: matching_params,
                sop_all_path_node_ratio :: int,
                sop_fixpoint_loop       :: maybe_opt_svcell_loop,
                sop_full_path           :: maybe_opt_svcell_full_path,
                sop_on_stack            :: maybe_opt_svcell_on_stack,
                sop_non_candidate_vars  :: set_of_progvar
            ).

:- type matching_result
    --->    matching_result(
                prog_var,
                cons_id,
                list(prog_var),
                set_of_progvar,
                goal_id,
                set(interval_id),
                set(interval_id),
                set(anchor),
                set(anchor)
            ).

:- type stack_opt_info
    --->    stack_opt_info(
                soi_stack_opt_params    :: stack_opt_params,
                soi_left_anchor_inserts :: insert_map,
                soi_matching_results    :: list(matching_result)
            ).

stack_opt_cell(PredProcId, !ProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    % This simplication is necessary to fix some bad inputs from
    % getting to the liveness computation.
    % (see tests/valid/stack_opt_simplify.m)
    module_info_get_globals(!.ModuleInfo, Globals),
    SimplifyTasks = list_to_simplify_tasks(Globals, []),
    simplify_proc(SimplifyTasks, PredId, ProcId, !ModuleInfo, !ProcInfo),
    detect_liveness_proc(!.ModuleInfo, PredProcId, !ProcInfo),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    initial_liveness(!.ModuleInfo, PredInfo, !.ProcInfo, Liveness0),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    AllocData = alloc_data(!.ModuleInfo, !.ProcInfo, PredProcId,
        no_var_is_dummy, TypeInfoLiveness, OptNoReturnCalls),
    fill_goal_id_slots_in_proc(!.ModuleInfo, _, !ProcInfo),
    proc_info_get_goal(!.ProcInfo, Goal2),
    OptStackAlloc0 = init_opt_stack_alloc,
    set.init(FailVars),
    set.init(NondetLiveness0),
    build_live_sets_in_goal_no_par_stack(AllocData, set_to_bitset(FailVars),
        Goal2, Goal, OptStackAlloc0, OptStackAlloc, Liveness0, _Liveness,
        set_to_bitset(NondetLiveness0), _NondetLiveness),
    proc_info_set_goal(Goal, !ProcInfo),
    allocate_store_maps(for_stack_opt, !.ModuleInfo, PredProcId, !ProcInfo),
    globals.lookup_int_option(Globals, debug_stack_opt, DebugStackOpt),
    pred_id_to_int(PredId, PredIdInt),
    trace [io(!IO)] (
        maybe_write_progress_message(!.ModuleInfo, !.ProcInfo,
            PredIdInt, DebugStackOpt, "\nbefore stack opt cell", !IO)
    ),
    optimize_live_sets(!.ModuleInfo, OptStackAlloc, !ProcInfo,
        Changed, DebugStackOpt, PredIdInt),
    (
        Changed = yes,
        trace [io(!IO)] (
            maybe_write_progress_message(!.ModuleInfo, !.ProcInfo,
                PredIdInt, DebugStackOpt,
                "\nafter stack opt transformation", !IO)
        ),
        requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
        trace [io(!IO)] (
            maybe_write_progress_message(!.ModuleInfo, !.ProcInfo,
                PredIdInt, DebugStackOpt,
                "\nafter stack opt requantify", !IO)
        ),
        recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
            !ProcInfo, !ModuleInfo),
        trace [io(!IO)] (
            maybe_write_progress_message(!.ModuleInfo, !.ProcInfo,
                PredIdInt, DebugStackOpt,
                "\nafter stack opt recompute instmaps", !IO)
        )
    ;
        Changed = no
    ).

:- func init_opt_stack_alloc = opt_stack_alloc.

init_opt_stack_alloc = opt_stack_alloc(set_of_var.init).

:- pred optimize_live_sets(module_info::in, opt_stack_alloc::in,
    proc_info::in, proc_info::out, bool::out, int::in, int::in) is det.

optimize_live_sets(ModuleInfo, OptAlloc, !ProcInfo, Changed, DebugStackOpt,
        PredIdInt) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_var_table(ModuleInfo, !.ProcInfo, VarTable0),
    OptAlloc = opt_stack_alloc(ParConjOwnSlot),
    arg_info.partition_proc_args(ModuleInfo, !.ProcInfo,
        InputArgs, OutputArgs, UnusedArgs),
    HeadVars = set.union_list([InputArgs, OutputArgs, UnusedArgs]),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    CandHeadvars = OptTuple ^ ot_opt_svcell_candidate_headvars,
    (
        CandHeadvars = do_not_opt_svcell_candidate_headvars,
        set_of_var.union(set_to_bitset(HeadVars), ParConjOwnSlot,
            NonCandidateVars)
    ;
        CandHeadvars = opt_svcell_candidate_headvars,
        NonCandidateVars = ParConjOwnSlot
    ),
    Counter0 = counter.init(1),
    counter.allocate(CurInterval, Counter0, Counter1),
    CurIntervalId = interval_id(CurInterval),
    EndMap0 = map.singleton(CurIntervalId, anchor_proc_end),
    map.init(InsertMap0),
    map.init(StartMap0),
    SuccMap0 = map.singleton(CurIntervalId, []),
    VarsMap0 = map.singleton(CurIntervalId, set_to_bitset(OutputArgs)),
    CellVarStoreCost = OptTuple ^ ot_opt_svcell_cv_store_cost,
    CellVarLoadCost = OptTuple ^ ot_opt_svcell_cv_load_cost,
    FieldVarStoreCost = OptTuple ^ ot_opt_svcell_fv_store_cost,
    FieldVarLoadCost = OptTuple ^ ot_opt_svcell_fv_load_cost,
    OpRatio = OptTuple ^ ot_opt_svcell_op_ratio,
    NodeRatio = OptTuple ^ ot_opt_svcell_node_ratio,
    InclAllCand = OptTuple ^ ot_opt_svcell_all_candidates,
    MatchingParams = matching_params(CellVarStoreCost, CellVarLoadCost,
        FieldVarStoreCost, FieldVarLoadCost, OpRatio, NodeRatio, InclAllCand),
    AllPathNodeRatio = OptTuple ^ ot_opt_svcell_all_path_node_ratio,
    FixpointLoop = OptTuple ^ ot_opt_svcell_loop,
    FullPath = OptTuple ^ ot_opt_svcell_full_path,
    OnStack = OptTuple ^ ot_opt_svcell_on_stack,
    globals.lookup_bool_option(Globals, opt_no_return_calls, OptNoReturnCalls),
    IntParams = interval_params(ModuleInfo, VarTable0, OptNoReturnCalls),
    IntervalInfo0 = interval_info(IntParams,
        set_of_var.init, set_to_bitset(OutputArgs),
        map.init, map.init, map.init, CurIntervalId, Counter1,
        set.make_singleton_set(CurIntervalId),
        map.init, set.init, StartMap0, EndMap0,
        SuccMap0, VarsMap0, map.init),
    module_info_get_name(ModuleInfo, ModuleName),
    StackOptParams = stack_opt_params(Globals, ModuleName, MatchingParams,
        AllPathNodeRatio, FixpointLoop, FullPath, OnStack, NonCandidateVars),
    StackOptInfo0 = stack_opt_info(StackOptParams, InsertMap0, []),
    build_interval_info_in_goal(Goal0, IntervalInfo0, IntervalInfo,
        StackOptInfo0, StackOptInfo),
    ( if DebugStackOpt = PredIdInt then
        trace [io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            dump_interval_info(DebugStream, IntervalInfo, !IO),
            dump_stack_opt_info(DebugStream, StackOptInfo, !IO)
        )
    else
        true
    ),
    InsertMap = StackOptInfo ^ soi_left_anchor_inserts,
    ( if map.is_empty(InsertMap) then
        Changed = no
    else
        record_decisions_in_goal(yes(feature_stack_opt), InsertMap,
            Goal0, Goal1, VarTable0, VarTable, map.init, RenameMap),
        apply_headvar_correction(set_of_var.set_to_bitset(HeadVars), RenameMap,
            Goal1, Goal),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),
        Changed = yes
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance stack_alloc_info(opt_stack_alloc) where [
    pred(at_call_site/4) is opt_at_call_site,
    pred(at_resume_site/4) is opt_at_resume_site,
    pred(at_par_conj/4) is opt_at_par_conj,
    pred(at_recursive_call_for_loop_control/4) is
        opt_at_recursive_call_for_loop_control
].

:- pred opt_at_call_site(need_across_call::in, alloc_data::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_call_site(_NeedAtCall, _AllocData, !StackAlloc).

:- pred opt_at_resume_site(need_in_resume::in, alloc_data::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_resume_site(_NeedAtResume, _AllocData, !StackAlloc).

:- pred opt_at_par_conj(need_in_par_conj::in, alloc_data::in,
    opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_par_conj(NeedParConj, _AllocData, !StackAlloc) :-
    NeedParConj = need_in_par_conj(StackVars),
    ParConjOwnSlots0 = !.StackAlloc ^ par_conj_own_slots,
    ParConjOwnSlots = set_of_var.union(StackVars, ParConjOwnSlots0),
    !StackAlloc ^ par_conj_own_slots := ParConjOwnSlots.

:- pred opt_at_recursive_call_for_loop_control(need_for_loop_control::in,
    alloc_data::in, opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_recursive_call_for_loop_control(NeedLC, _AllocData, !StackAlloc) :-
    NeedLC = need_for_loop_control(StackVarsSets),
    StackVars = set_of_var.union_list(StackVarsSets),
    ParConjOwnSlots0 = !.StackAlloc ^ par_conj_own_slots,
    ParConjOwnSlots = set_of_var.union(StackVars, ParConjOwnSlots0),
    !StackAlloc ^ par_conj_own_slots := ParConjOwnSlots.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance build_interval_info_acc(stack_opt_info) where [
    pred(use_cell/8) is stack_opt.use_cell
].

:- type match_path_info
    --->    match_path_info(
                % The set of vars referenced in the first interval,
                % before the first flush point.
                set_of_progvar,

                % The set of vars referenced in later intervals,
                % after the first flush point.
                list(set_of_progvar)
            ).

:- type match_info
    --->    match_info(
                % Information about the variables used along each path.
                list(match_path_info),

                % The variables used after the deconstruction
                % goes out of scope.
                set_of_progvar,

                % Have we stepped over a model_non goal?
                bool,

                % The set of save points to which the results of the
                % matching applies.
                set(anchor),

                set(interval_id)
            ).

:- pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in,
    hlds_goal::in, interval_info::in, interval_info::out, stack_opt_info::in,
    stack_opt_info::out) is det.

use_cell(CellVar, FieldVarList, ConsId, Goal, !IntervalInfo, !StackOptInfo) :-
    FlushedLater = !.IntervalInfo ^ ii_flushed_later,
    StackOptParams = !.StackOptInfo ^ soi_stack_opt_params,
    NonCandidateVars = StackOptParams ^ sop_non_candidate_vars,
    FieldVars = set_of_var.list_to_set(FieldVarList),
    set_of_var.intersect(FieldVars, FlushedLater, FlushedLaterFieldVars),
    set_of_var.difference(FlushedLaterFieldVars, NonCandidateVars,
        CandidateArgVars0),
    ( if
        set_of_var.is_empty(CandidateArgVars0)
    then
        true
    else if
        ConsId = cons(_Name, _Arity, _TypeCtor),
        IntParams = !.IntervalInfo ^ ii_interval_params,
        VarTable = IntParams ^ ip_var_table,
        lookup_var_type(VarTable, CellVar, Type),
        ( if
            type_is_tuple(Type, _)
        then
            FreeOfCost = no
        else if
            ModuleInfo = IntParams ^ ip_module_info,
            get_cons_repn_defn(ModuleInfo, ConsId, ConsRepn)
        then
            ConsTag = ConsRepn ^ cr_tag,
            ( if ConsTag = no_tag then
                FreeOfCost = yes
            else
                FreeOfCost = no
            )
        else
            fail
        )
    then
        set_of_var.insert(CellVar, FieldVars, RelevantVars),
        find_all_branches_from_cur_interval(RelevantVars, MatchInfo,
            !.IntervalInfo, !.StackOptInfo),
        MatchInfo = match_info(PathsInfo, RelevantAfterVars,
            AfterModelNon, InsertAnchors, InsertIntervals),
        (
            FreeOfCost = yes,
            set_of_var.difference(CandidateArgVars0, RelevantAfterVars,
                ViaCellVars),
            record_matching_result(CellVar, ConsId, FieldVarList, ViaCellVars,
                Goal, InsertAnchors, InsertIntervals, !IntervalInfo,
                !StackOptInfo)
        ;
            FreeOfCost = no,
            (
                AfterModelNon = no,
                OnStack = StackOptParams ^ sop_on_stack,
                set_of_var.difference(CandidateArgVars0, RelevantAfterVars,
                    CandidateArgVars),
                (
                    OnStack = opt_svcell_on_stack,
                    ( if set_of_var.member(FlushedLater, CellVar) then
                        CellVarFlushedLater = yes
                    else
                        CellVarFlushedLater = no
                    )
                ;
                    OnStack = do_not_opt_svcell_on_stack,
                    ( if
                        list.member(PathInfo, PathsInfo),
                        PathInfo = match_path_info(_, Segments),
                        list.member(Segment, Segments),
                        set_of_var.member(Segment, CellVar)
                    then
                        CellVarFlushedLater = yes
                    else
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
    else
        true
    ).

:- pred apply_matching(prog_var::in, bool::in, interval_params::in,
    stack_opt_params::in, list(match_path_info)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

apply_matching(CellVar, CellVarFlushedLater, IntParams, StackOptParams,
        PathInfos, CandidateArgVars0, ViaCellVars) :-
    apply_matching_loop(CellVar, CellVarFlushedLater, IntParams,
        StackOptParams, PathInfos, CandidateArgVars0,
        BenefitNodeSets, CostNodeSets, ViaCellVars0),
    BenefitNodes = set.union_list(BenefitNodeSets),
    CostNodes = set.union_list(CostNodeSets),
    set.count(BenefitNodes, NumBenefitNodes),
    set.count(CostNodes, NumCostNodes),
    AllPathNodeRatio = StackOptParams ^ sop_all_path_node_ratio,
    ( if NumBenefitNodes * 100 >= NumCostNodes * AllPathNodeRatio then
        ViaCellVars = ViaCellVars0
    else
        ViaCellVars = set_of_var.init
    ).

:- pred apply_matching_loop(prog_var::in, bool::in, interval_params::in,
    stack_opt_params::in, list(match_path_info)::in, set_of_progvar::in,
    list(set(benefit_node))::out, list(set(cost_node))::out,
    set_of_progvar::out) is det.

apply_matching_loop(CellVar, CellVarFlushedLater, IntParams, StackOptParams,
        PathInfos, CandidateArgVars0, BenefitNodeSets, CostNodeSets,
        ViaCellVars) :-
    list.map3(
        apply_matching_for_path(StackOptParams,
            CellVar, CellVarFlushedLater, CandidateArgVars0),
        PathInfos, BenefitNodeSets0, CostNodeSets0, PathViaCellVars),
    ( if list.all_same(PathViaCellVars) then
        BenefitNodeSets = BenefitNodeSets0,
        CostNodeSets = CostNodeSets0,
        (
            PathViaCellVars = [ViaCellVars | _]
        ;
            PathViaCellVars = [],
            ViaCellVars = set_of_var.init
        )
    else
        CandidateArgVars1 = set_of_var.intersect_list(PathViaCellVars),
        FixpointLoop = StackOptParams ^ sop_fixpoint_loop,
        (
            FixpointLoop = do_not_opt_svcell_loop,
            BenefitNodeSets = BenefitNodeSets0,
            CostNodeSets = CostNodeSets0,
            ViaCellVars = CandidateArgVars1
        ;
            FixpointLoop = opt_svcell_loop,
            apply_matching_loop(CellVar, CellVarFlushedLater,
                IntParams, StackOptParams, PathInfos, CandidateArgVars1,
                BenefitNodeSets, CostNodeSets, ViaCellVars)
        )
    ).

:- pred apply_matching_for_path(stack_opt_params::in, prog_var::in, bool::in,
    set_of_progvar::in, match_path_info::in,
    set(benefit_node)::out, set(cost_node)::out, set_of_progvar::out) is det.

apply_matching_for_path(StackOptParams, CellVar, CellVarFlushedLater,
        CandidateArgVars, PathInfo, BenefitNodes, CostNodes, ViaCellVars) :-
    ( if set_of_var.is_empty(CandidateArgVars) then
        BenefitNodes = set.init,
        CostNodes = set.init,
        ViaCellVars = set_of_var.init
    else
        PathInfo = match_path_info(FirstSegment, LaterSegments),
        Globals = StackOptParams ^ sop_globals,
        ModuleName = StackOptParams ^ sop_module_name,
        MatchingParams = StackOptParams ^ sop_matching_params,
        find_via_cell_vars(Globals, ModuleName, MatchingParams, CellVar,
            CandidateArgVars, CellVarFlushedLater, FirstSegment, LaterSegments,
            BenefitNodes, CostNodes, ViaCellVars)
    ).

:- pred record_matching_result(prog_var::in, cons_id::in,
    list(prog_var)::in, set_of_progvar::in, hlds_goal::in, set(anchor)::in,
    set(interval_id)::in, interval_info::in, interval_info::out,
    stack_opt_info::in, stack_opt_info::out) is det.

record_matching_result(CellVar, ConsId, ArgVars, ViaCellVars, Goal,
        PotentialAnchors, PotentialIntervals, !IntervalInfo, !StackOptInfo) :-
    ( if set_of_var.is_empty(ViaCellVars) then
        true
    else
        set.to_sorted_list(PotentialIntervals, PotentialIntervalList),
        set.to_sorted_list(PotentialAnchors, PotentialAnchorList),
        list.foldl3(record_cell_var_for_interval(CellVar, ViaCellVars),
            PotentialIntervalList, !IntervalInfo, !StackOptInfo,
            set.init, InsertIntervals),
        list.foldl3(add_anchor_inserts(Goal, ViaCellVars, InsertIntervals),
            PotentialAnchorList, !IntervalInfo, !StackOptInfo,
            set.init, InsertAnchors),
        Goal = hlds_goal(_, GoalInfo),
        GoalId = goal_info_get_goal_id(GoalInfo),
        MatchingResult = matching_result(CellVar, ConsId,
            ArgVars, ViaCellVars, GoalId,
            PotentialIntervals, InsertIntervals,
            PotentialAnchors, InsertAnchors),
        MatchingResults0 = !.StackOptInfo ^ soi_matching_results,
        MatchingResults = [MatchingResult | MatchingResults0],
        !StackOptInfo ^ soi_matching_results := MatchingResults
    ).

:- pred record_cell_var_for_interval(prog_var::in, set_of_progvar::in,
    interval_id::in, interval_info::in, interval_info::out,
    stack_opt_info::in, stack_opt_info::out,
    set(interval_id)::in, set(interval_id)::out) is det.

record_cell_var_for_interval(CellVar, ViaCellVars, IntervalId,
        !IntervalInfo, !StackOptInfo, !InsertIntervals) :-
    record_interval_vars(IntervalId, [CellVar], !IntervalInfo),
    delete_interval_vars(IntervalId, ViaCellVars, DeletedVars, !IntervalInfo),
    ( if set_of_var.is_non_empty(DeletedVars) then
        set.insert(IntervalId, !InsertIntervals)
    else
        true
    ).

:- pred add_anchor_inserts(hlds_goal::in, set_of_progvar::in,
    set(interval_id)::in, anchor::in, interval_info::in,
    interval_info::out, stack_opt_info::in, stack_opt_info::out,
    set(anchor)::in, set(anchor)::out) is det.

add_anchor_inserts(Goal, ArgVarsViaCellVar, InsertIntervals, Anchor,
        !IntervalInfo, !StackOptInfo, !InsertAnchors) :-
    map.lookup(!.IntervalInfo ^ ii_anchor_follow_map, Anchor, AnchorFollow),
    AnchorFollow = anchor_follow_info(_, AnchorIntervals),
    set.intersect(AnchorIntervals, InsertIntervals,
        AnchorInsertIntervals),
    ( if set.is_non_empty(AnchorInsertIntervals) then
        Insert = insert_spec(Goal, ArgVarsViaCellVar),
        InsertMap0 = !.StackOptInfo ^ soi_left_anchor_inserts,
        ( if map.search(InsertMap0, Anchor, Inserts0) then
            Inserts = [Insert | Inserts0],
            map.det_update(Anchor, Inserts, InsertMap0, InsertMap)
        else
            Inserts = [Insert],
            map.det_insert(Anchor, Inserts, InsertMap0, InsertMap)
        ),
        !StackOptInfo ^ soi_left_anchor_inserts := InsertMap,
        set.insert(Anchor, !InsertAnchors)
    else
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
                current_segment         :: set_of_progvar,
                first_segment           :: set_of_progvar,
                other_segments          :: list(set_of_progvar),
                flush_anchors           :: set(anchor),
                occurring_intervals     :: set(interval_id)
            ).

:- type all_paths
    --->    all_paths(
                % The set of all paths so far.
                paths_so_far            :: set(path),

                % Have we stepped over model_non goals?
                stepped_over_model_non  :: bool,

                % The vars which are known to be used after the deconstruction
                % goes out of scope.
                used_after_scope        :: set_of_progvar
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
    (
        FlushState = current_is_before_first_flush,
        expect(set_of_var.is_empty(FirstSegment0), $pred,
            "FirstSegment0 not empty"),
        FirstSegment = CurSegment,
        OtherSegments = OtherSegments0
    ;
        FlushState = current_is_after_first_flush,
        ( if set_of_var.is_empty(CurSegment) then
            FirstSegment = FirstSegment0,
            OtherSegments = OtherSegments0
        else
            FirstSegment = FirstSegment0,
            OtherSegments = [CurSegment | OtherSegments0]
        )
    ),
    Path = path(current_is_after_first_flush, set_of_var.init,
        FirstSegment, OtherSegments, FlushAnchors, IntervalIds).

:- func add_interval_to_path(interval_id, set_of_progvar, path) = path.

add_interval_to_path(IntervalId, Vars, !.Path) = !:Path :-
    ( if set_of_var.is_empty(Vars) then
        true
    else
        CurSegment0 = !.Path ^ current_segment,
        CurSegment = set_of_var.union(Vars, CurSegment0),
        OccurringIntervals0 = !.Path ^ occurring_intervals,
        set.insert(IntervalId, OccurringIntervals0, OccurringIntervals),
        !Path ^ current_segment := CurSegment,
        !Path ^ occurring_intervals := OccurringIntervals
    ).

:- func add_anchor_to_path(anchor, path) = path.

add_anchor_to_path(Anchor, !.Path) = !:Path :-
    Anchors0 = !.Path ^ flush_anchors,
    set.insert(Anchor, Anchors0, Anchors),
    !Path ^ flush_anchors := Anchors.

:- func anchor_requires_close(interval_info, anchor) = bool.

anchor_requires_close(_, anchor_proc_start) = no.
anchor_requires_close(_, anchor_proc_end) = yes.
anchor_requires_close(IntervalInfo, anchor_branch_start(_, GoalId)) =
        resume_save_status_requires_close(ResumeSaveStatus) :-
    map.lookup(IntervalInfo ^ ii_branch_resume_map, GoalId,
        ResumeSaveStatus).
anchor_requires_close(_, anchor_cond_then(_)) = no.
anchor_requires_close(_, anchor_branch_end(BranchType, _)) = NeedsClose :-
    (
        BranchType = branch_neg,
        NeedsClose = no
    ;
        ( BranchType = branch_ite
        ; BranchType = branch_disj
        ; BranchType = branch_switch
        ; BranchType = branch_par_conj
        ),
        NeedsClose = yes
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
may_have_more_successors(anchor_branch_start(BranchType, _)) = MaybeHaveMore :-
    (
        BranchType = branch_neg,
        MaybeHaveMore = no
    ;
        ( BranchType = branch_ite
        ; BranchType = branch_disj
        ; BranchType = branch_switch
        ; BranchType = branch_par_conj
        ),
        MaybeHaveMore = yes
    ).
may_have_more_successors(anchor_cond_then(_)) = no.
may_have_more_successors(anchor_branch_end(_, _)) = no.
may_have_more_successors(anchor_call_site(_)) = no.

%-----------------------------------------------------------------------------%

:- pred find_all_branches_from_cur_interval(set_of_progvar::in,
    match_info::out, interval_info::in, stack_opt_info::in) is det.

find_all_branches_from_cur_interval(RelevantVars, MatchInfo, IntervalInfo,
        StackOptInfo) :-
    IntervalId = IntervalInfo ^ ii_cur_interval,
    map.lookup(IntervalInfo ^ ii_interval_vars, IntervalId, IntervalVars),
    IntervalRelevantVars = set_of_var.intersect(RelevantVars, IntervalVars),
    Path0 = path(current_is_before_first_flush, IntervalRelevantVars,
        set_of_var.init, [], set.init, set.init),
    AllPaths0 = all_paths(set.make_singleton_set(Path0), no, set_of_var.init),
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

:- pred find_all_branches(set_of_progvar::in, interval_id::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    all_paths::in, all_paths::out) is det.

find_all_branches(RelevantVars, IntervalId, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, !AllPaths) :-
    map.lookup(IntervalInfo ^ ii_interval_end, IntervalId, End),
    map.lookup(IntervalInfo ^ ii_interval_succ, IntervalId, SuccessorIds),
    (
        SuccessorIds = [],
        expect(unify(may_have_no_successor(End), yes), $pred,
            "unexpected no successor")
        % expect(unify(MaybeSearchAnchor0, no), $pred,
        %   "find_all_branches: no successor while in search"),
        % that test may fail if we come to a call that cannot succeed
    ;
        SuccessorIds = [SuccessorId | MoreSuccessorIds],
        (
            MoreSuccessorIds = [],
            expect(unify(may_have_one_successor(End), yes), $pred,
                "unexpected one successor")
        ;
            MoreSuccessorIds = [_ | _],
            expect(unify(may_have_more_successors(End), yes), $pred,
                "unexpected more successors")
        ),
        ( if
            MaybeSearchAnchor0 = yes(SearchAnchor0),
            End = SearchAnchor0
        then
            !AllPaths ^ used_after_scope := set_of_var.init
        else if
            End = anchor_branch_end(_, EndGoalId),
            map.lookup(IntervalInfo ^ ii_branch_end_map, EndGoalId,
                BranchEndInfo),
            OnStackAfterBranch = BranchEndInfo ^ flushed_after_branch,
            AccessedAfterBranch = BranchEndInfo ^ accessed_after_branch,
            NeededAfterBranch = set_of_var.union(OnStackAfterBranch,
                AccessedAfterBranch),
            RelevantAfter = set_of_var.intersect(RelevantVars,
                NeededAfterBranch),
            set_of_var.is_non_empty(RelevantAfter)
        then
            !AllPaths ^ used_after_scope := RelevantAfter
        else
            find_all_branches_from(End, RelevantVars,
                MaybeSearchAnchor0, IntervalInfo, StackOptInfo,
                [SuccessorId | MoreSuccessorIds], !AllPaths)
        )
    ).

:- pred find_all_branches_from(anchor::in, set_of_progvar::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    list(interval_id)::in, all_paths::in, all_paths::out) is det.

find_all_branches_from(End, RelevantVars, MaybeSearchAnchor0, IntervalInfo,
        StackOptInfo, SuccessorIds, !AllPaths) :-
    AnchorRequiresClose = anchor_requires_close(IntervalInfo, End),
    (
        AnchorRequiresClose = yes,
        Paths0 = !.AllPaths ^ paths_so_far,
        Paths1 = set.map(close_path, Paths0),
        !AllPaths ^ paths_so_far := Paths1
    ;
        AnchorRequiresClose = no
    ),
    StackOptParams = StackOptInfo ^ soi_stack_opt_params,
    FullPath = StackOptParams ^ sop_full_path,
    ( if
        FullPath = opt_svcell_full_path,
        End = anchor_branch_start(branch_disj, EndGoalId)
    then
        MaybeSearchAnchor1 = yes(anchor_branch_end(branch_disj, EndGoalId)),
        one_after_another(RelevantVars, MaybeSearchAnchor1,
            IntervalInfo, StackOptInfo, SuccessorIds, !AllPaths),
        map.lookup(IntervalInfo ^ ii_branch_end_map, EndGoalId,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    else if
        FullPath = opt_svcell_full_path,
        End = anchor_branch_start(branch_ite, EndGoalId)
    then
        ( if SuccessorIds = [ElseStartIdPrime, CondStartIdPrime] then
            ElseStartId = ElseStartIdPrime,
            CondStartId = CondStartIdPrime
        else
            unexpected($pred, "ite not else, cond")
        ),
        MaybeSearchAnchorCond = yes(anchor_cond_then(EndGoalId)),
        apply_interval_find_all_branches(RelevantVars,
            MaybeSearchAnchorCond, IntervalInfo, StackOptInfo,
            CondStartId, !AllPaths),
        MaybeSearchAnchorEnd = yes(anchor_branch_end(branch_ite, EndGoalId)),
        CondEndMap = IntervalInfo ^ ii_cond_end_map,
        map.lookup(CondEndMap, EndGoalId, ThenStartId),
        one_after_another(RelevantVars, MaybeSearchAnchorEnd,
            IntervalInfo, StackOptInfo, [ThenStartId, ElseStartId], !AllPaths),
        map.lookup(IntervalInfo ^ ii_branch_end_map, EndGoalId,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    else if
        End = anchor_branch_start(BranchType, EndGoalId)
    then
        MaybeSearchAnchor1 = yes(anchor_branch_end(BranchType, EndGoalId)),
        list.map(apply_interval_find_all_branches_map(RelevantVars,
            MaybeSearchAnchor1, IntervalInfo, StackOptInfo, !.AllPaths),
            SuccessorIds, AllPathsList),
        consolidate_after_join(AllPathsList, !:AllPaths),
        map.lookup(IntervalInfo ^ ii_branch_end_map, EndGoalId,
            BranchEndInfo),
        ContinueId = BranchEndInfo ^ interval_after_branch,
        apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
            IntervalInfo, StackOptInfo, ContinueId, !AllPaths)
    else
        ( if SuccessorIds = [SuccessorId] then
            apply_interval_find_all_branches(RelevantVars,
                MaybeSearchAnchor0, IntervalInfo,
                StackOptInfo, SuccessorId, !AllPaths)
        else
            unexpected($pred, "more successor ids")
        )
    ).

:- pred one_after_another(set_of_progvar::in, maybe(anchor)::in,
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
:- pred apply_interval_find_all_branches_map(set_of_progvar::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    all_paths::in, interval_id::in, all_paths::out) is det.

apply_interval_find_all_branches_map(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, !.AllPaths, IntervalId, !:AllPaths) :-
    apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths).

:- pred apply_interval_find_all_branches(set_of_progvar::in,
    maybe(anchor)::in, interval_info::in, stack_opt_info::in,
    interval_id::in, all_paths::in, all_paths::out) is det.

apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
        IntervalInfo, StackOptInfo, IntervalId, !AllPaths) :-
    map.lookup(IntervalInfo ^ ii_interval_vars, IntervalId, IntervalVars),
    RelevantIntervalVars = set_of_var.intersect(RelevantVars, IntervalVars),
    !.AllPaths = all_paths(Paths0, AfterModelNon0, RelevantAfter),
    Paths1 = set.map(add_interval_to_path(IntervalId, RelevantIntervalVars),
        Paths0),
    map.lookup(IntervalInfo ^ ii_interval_start, IntervalId, Start),
    ( if
        % Check if intervals starting at Start use any RelevantVars.
        ( Start = anchor_call_site(_)
        ; Start = anchor_branch_end(_, _)
        ; Start = anchor_branch_start(_, _)
        ),
        map.search(IntervalInfo ^ ii_anchor_follow_map, Start, StartInfo),
        StartInfo = anchor_follow_info(AnchorFollowVars, _),
        set_of_var.intersect(RelevantVars, AnchorFollowVars, NeededVars),
        set_of_var.is_non_empty(NeededVars)
    then
        Paths2 = set.map(add_anchor_to_path(Start), Paths1)
    else
        Paths2 = Paths1
    ),
    ( if set.member(Start, IntervalInfo ^ ii_model_non_anchors) then
        AfterModelNon = yes
    else
        AfterModelNon = AfterModelNon0
    ),
    !:AllPaths = all_paths(Paths2, AfterModelNon, RelevantAfter),
    find_all_branches(RelevantVars, IntervalId,
        MaybeSearchAnchor0, IntervalInfo, StackOptInfo, !AllPaths).

:- pred consolidate_after_join(list(all_paths)::in, all_paths::out) is det.

consolidate_after_join([], _) :-
    unexpected($pred, "no paths to join").
consolidate_after_join([First | Rest], AllPaths) :-
    PathsList = list.map(project_paths_from_all_paths, [First | Rest]),
    Paths0 = set.union_list(PathsList),
    Paths = compress_paths(Paths0),
    AfterModelNonList = list.map(project_after_model_non_from_all_paths,
        [First | Rest]),
    bool.or_list(AfterModelNonList, AfterModelNon),
    AllPaths = all_paths(Paths, AfterModelNon, set_of_var.init).

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

:- pred maybe_write_progress_message(module_info::in, proc_info::in,
    int::in, int::in, string::in, io::di, io::uo) is det.

maybe_write_progress_message(ModuleInfo, ProcInfo, PredIdInt, DebugStackOpt,
        Message, !IO) :-
    ( if DebugStackOpt = PredIdInt then
        proc_info_get_goal(ProcInfo, Goal),
        proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
        module_info_get_globals(ModuleInfo, Globals),
        io.output_stream(Stream, !IO),
        io.write_string(Stream, Message, !IO),
        io.write_string(Stream, ":\n", !IO),
        OutInfo = init_hlds_out_info(Globals, output_debug),
        write_goal(OutInfo, Stream, ModuleInfo, vns_var_table(VarTable),
            print_name_and_num, 0, "\n", Goal, !IO),
        io.write_string(Stream, "\n", !IO)
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % This predicate (along with dump_interval_info) can help debug the
    % performance of the transformation.
    %
:- pred dump_stack_opt_info(io.text_output_stream::in, stack_opt_info::in,
    io::di, io::uo) is det.

dump_stack_opt_info(Stream, StackOptInfo, !IO) :-
    map.to_assoc_list(StackOptInfo ^ soi_left_anchor_inserts, Inserts),
    io.write_string(Stream, "\nANCHOR INSERT:\n", !IO),
    list.foldl(dump_anchor_inserts(Stream), Inserts, !IO),

    io.write_string(Stream, "\nMATCHING RESULTS:\n", !IO),
    list.foldl(dump_matching_result(Stream),
        StackOptInfo ^ soi_matching_results, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred dump_anchor_inserts(io.text_output_stream::in,
    pair(anchor, list(insert_spec))::in, io::di, io::uo) is det.

dump_anchor_inserts(Stream, Anchor - InsertSpecs, !IO) :-
    io.write_string(Stream, "\ninsertions after ", !IO),
    io.write(Stream, Anchor, !IO),
    io.write_string(Stream, ":\n", !IO),
    list.foldl(dump_insert(Stream), InsertSpecs, !IO).

:- pred dump_insert(io.text_output_stream::in, insert_spec::in,
    io::di, io::uo) is det.

dump_insert(Stream, insert_spec(Goal, Vars), !IO) :-
    list.map(term.var_to_int, set_of_var.to_sorted_list(Vars), VarNums),
    io.format(Stream, "vars [%s]:", [s(int_list_to_string(VarNums))], !IO),
    ( if
        Goal = hlds_goal(unify(_, _, _, Unification, _), _),
        Unification = deconstruct(CellVar, ConsId, ArgVars, _,_,_)
    then
        term.var_to_int(CellVar, CellVarNum),
        list.map(term.var_to_int, ArgVars, ArgVarNums),
        io.format(Stream, "%d => %s(%s)\n",
            [i(CellVarNum), s(cons_id_and_arity_to_string(ConsId)),
            s(int_list_to_string(ArgVarNums))], !IO)
    else
        io.write_string(Stream, "BAD INSERT GOAL\n", !IO)
    ).

:- pred dump_matching_result(io.text_output_stream::in, matching_result::in,
    io::di, io::uo) is det.

dump_matching_result(Stream, MatchingResult, !IO) :-
    MatchingResult = matching_result(CellVar, ConsId, ArgVars, ViaCellVars,
        GoalId, PotentialIntervals, InsertIntervals,
        PotentialAnchors, InsertAnchors),
    io.write_string(Stream, "\nmatching result at ", !IO),
    io.write_line(Stream, GoalId, !IO),
    term.var_to_int(CellVar, CellVarNum),
    list.map(term.var_to_int, ArgVars, ArgVarNums),
    list.map(term.var_to_int, set_of_var.to_sorted_list(ViaCellVars),
        ViaCellVarNums),
    io.format(Stream, "%d => %s(%s): via cell %s\n",
        [i(CellVarNum), s(cons_id_and_arity_to_string(ConsId)),
        s(int_list_to_string(ArgVarNums)),
        s(int_list_to_string(ViaCellVarNums))], !IO),

    PotentialIntervalNums = list.map(interval_id_to_int,
        set.to_sorted_list(PotentialIntervals)),
    InsertIntervalNums = list.map(interval_id_to_int,
        set.to_sorted_list(InsertIntervals)),
    io.format(Stream, "potential intervals: %s\n",
        [s(int_list_to_string(PotentialIntervalNums))], !IO),
    io.format(Stream, "insert intervals: %s\n",
        [s(int_list_to_string(InsertIntervalNums))], !IO),

    PotentialAnchorStrs = list.map(string.string,
        set.to_sorted_list(PotentialAnchors)),
    InsertAnchorStrs = list.map(string.string,
        set.to_sorted_list(InsertAnchors)),
    PotentialAnchorsStr = string.join_list(" ", PotentialAnchorStrs),
    InsertAnchorsStr = string.join_list(" ", InsertAnchorStrs),
    io.format(Stream, "potential anchors: %s\n",
        [s(PotentialAnchorsStr)], !IO),
    io.format(Stream, "insert anchors: %s\n",
        [s(InsertAnchorsStr)], !IO).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.stack_opt.
%-----------------------------------------------------------------------------%
