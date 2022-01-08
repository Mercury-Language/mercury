%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: pbone, zs.
%
% This file implements the coverage propagation algorithm, which attaches
% coverage information to the component goals of a procedure body.
%
% This module can be compiled with the following trace flag to enable
% debugging: debug_first_var_use. See Mercury.options in this directory.
%
%---------------------------------------------------------------------------%

:- module var_use_analysis.

:- interface.

:- import_module analysis_utils.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module coverage.
:- import_module measurements.
:- import_module profile.
:- import_module report.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

    % Gives information about the use of a variable measured in average call
    % sequence counts since either the beginning or the end of the procedure.
    %
:- type var_use_info
    --->    var_use_info(
                vui_cost_until_use          :: float,
                vui_proc_cost               :: float,
                vui_use_type                :: var_use_type
            ).

:- type var_use_options
    --->    var_use_options(
                vuo_deep                    :: deep,
                vuo_intermodule_var_use     :: intermodule_var_use,
                vuo_var_use_type            :: var_use_type
            ).

:- type intermodule_var_use
    --->    follow_calls_into_module(string)
    ;       follow_any_call.

:- type var_use_type
    --->    var_use_production
            % The variable is produced: free >> ground
    ;       var_use_consumption
            % The variable is consumed: free >> free
    ;       var_use_other.
            % The variable is used in some other way.

%---------------------------------------------------------------------------%

:- func average_var_use(list(var_use_info)) = var_use_info.

:- pred var_mode_to_var_use_type(var_mode_rep::in, var_use_type::out) is det.

:- pred pessimistic_var_use_info(var_use_type::in, float::in,
    var_use_info::out) is det.

:- pred pessimistic_var_use_time(var_use_type::in, float::in, float::out)
    is det.

%---------------------------------------------------------------------------%

    % get_call_site_dynamic_var_use_info(Deep, CSDPtr, ArgNum, RT, VarUseType,
    %   MaybeVarUseInfo):
    %
    % Lookup when the call site CSDPtr will produce ArgNum. RT is the type of
    % recursion in the call site's parent clique and VarUseType is the type of
    % variable use that is expected, VarUseType is used to produce conservative
    % defaults if a callee cannot be analyzed.
    %
    % The first mode avoids a check to ensure that this recursion type provides
    % enough information.
    %
:- pred get_call_site_dynamic_var_use_info(call_site_dynamic_ptr, int,
    recursion_type, float, var_use_options, maybe_error(var_use_info)).
:- mode get_call_site_dynamic_var_use_info(in, in,
    in(recursion_type_known_costs), in, in, out) is det.
:- mode get_call_site_dynamic_var_use_info(in, in,
    in, in, in, out) is det.

    % get_call_site_dynamic_var_use_info_rec_level(Deep, CliquePtr, CSDPtr,
    %   ArgNum, RT, MaybeCurDepth, Cost, CallStack, VarUseType,
    %   MaybeVarUseInfo).
    %
    % As above. This alternative should be used if searching starts at a
    % different recursion level or from within a current variable use analysis.
    %
    % CliquePtr is the current clique, MaybeCurDepth is the current recursion
    % depth within the clique if known, CallStack is the set of proc dynamics
    % that have already been explored.
    %
    % Cost is the cost of the call.
    %
:- pred get_call_site_dynamic_var_use_info_rec_level(clique_ptr,
    call_site_dynamic_ptr, int, recursion_type, maybe(recursion_depth),
    float, set(proc_dynamic_ptr), var_use_options, maybe_error(var_use_info)).
:- mode get_call_site_dynamic_var_use_info_rec_level(in, in, in,
    in(recursion_type_known_costs), in(maybe_yes(ground)), in, in, in, out)
    is det.
:- mode get_call_site_dynamic_var_use_info_rec_level(in, in, in,
    in, in, in, in, in, out) is det.

:- pred clique_var_use_info(clique_ptr::in, int::in,
    var_use_options::in, maybe_error(var_use_info)::out) is det.

:- pred proc_dynamic_var_use_info(clique_ptr::in,
    proc_dynamic_ptr::in, int::in,
    recursion_type::in(recursion_type_known_costs), recursion_depth::in,
    float::in, set(proc_dynamic_ptr)::in, var_use_options::in,
    maybe_error(var_use_info)::out) is det.

%---------------------------------------------------------------------------%

    % Find the first use of a variable in an arbitrary goal.
    %
:- pred var_first_use(clique_ptr::in,
    map(reverse_goal_path, cost_and_callees)::in,
    map(reverse_goal_path, cs_cost_csq)::in,
    containing_goal_map::in, goal_attr_array(coverage_info)::in,
    recursion_type::in(recursion_type_known_costs), recursion_depth::in,
    goal_rep(goal_id)::in, reverse_goal_path::in, float::in, var_rep::in,
    var_use_options::in, var_use_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module measurement_units.
:- import_module program_representation_utils.
:- import_module recursion_patterns.

:- import_module assoc_list.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

average_var_use(Uses) = var_use_info(CostUntilUse, AvgProcCost, Type) :-
    (
        Uses = [],
        unexpected($pred, "cannot average zero items")
    ;
        Uses = [var_use_info(_, _, Type) | _],
        foldl2(sum_use_info_costs, Uses, 0.0, SumCost, 0.0, SumProcCost),
        Num = float(length(Uses)),
        CostUntilUse = SumCost / Num,
        AvgProcCost = SumProcCost / Num,
        TestType =
            ( pred(var_use_info(_, _, TypeI)::in) is semidet :-
                Type = TypeI
            ),
        ( if all_true(TestType, Uses) then
            true
        else
            unexpected($pred, "use types do not match")
        )
    ).

:- pred sum_use_info_costs(var_use_info::in, float::in, float::out,
    float::in, float::out) is det.

sum_use_info_costs(var_use_info(Cost, ProcCost, _), !AccCost, !AccProcCost) :-
    !:AccCost = !.AccCost + Cost,
    !:AccProcCost = !.AccProcCost + ProcCost.

%---------------------------------------------------------------------------%

var_mode_to_var_use_type(var_mode_rep(InitialInst, FinalInst), VarUseType) :-
    ( if
        InitialInst = ir_ground_rep,
        FinalInst = ir_ground_rep
    then
        VarUseType = var_use_consumption
    else if
        InitialInst = ir_free_rep,
        FinalInst = ir_ground_rep
    then
        VarUseType = var_use_production
    else
        VarUseType = var_use_other
    ).

%---------------------------------------------------------------------------%

pessimistic_var_use_info(VarUseType, ProcCost, VarUseInfo) :-
    pessimistic_var_use_time(VarUseType, ProcCost, CostUntilUse),
    VarUseInfo = var_use_info(CostUntilUse, ProcCost, VarUseType).

pessimistic_var_use_time(VarUseType, ProcCost, CostUntilUse) :-
    (
        VarUseType = var_use_consumption,
        CostUntilUse = 0.0
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        CostUntilUse = ProcCost
    ).

%---------------------------------------------------------------------------%

get_call_site_dynamic_var_use_info(CSDPtr, ArgNum, RecursionType, Cost,
        VarUseOptions, MaybeVarUseInfo) :-
    % XXX If the CSD represents a closure then the argument position will be
    % incorrect. This is not currently important, as we assume that the
    % compiler will not push signals and waits into higher order calls.
    % Therefore this should never be called for a higher order call site.
    Deep = VarUseOptions ^ vuo_deep,
    deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
    deep_lookup_clique_index(Deep, CSD ^ csd_caller, ParentCliquePtr),
    recursion_type_get_maybe_avg_max_depth(RecursionType, MaybeCurDepth),
    get_call_site_dynamic_var_use_info_rec_level(ParentCliquePtr, CSDPtr,
        ArgNum, RecursionType, MaybeCurDepth, Cost, set.init, VarUseOptions,
        MaybeVarUseInfo).

get_call_site_dynamic_var_use_info_rec_level(ParentCliquePtr, CSDPtr, ArgNum,
        RecursionType, MaybeDepth, Cost, CallStack0, VarUseOptions,
        MaybeVarUseInfo) :-
    Deep = VarUseOptions ^ vuo_deep,
    deep_lookup_clique_maybe_child(Deep, CSDPtr, MaybeCalleeCliquePtr),
    (
        MaybeCalleeCliquePtr = yes(CalleeCliquePtr),
        % This is a non-recursive call site.

        % We don't check if this call crossed a module boundary here,
        % that is done in clique_var_use_info.
        clique_var_use_info(CalleeCliquePtr, ArgNum, VarUseOptions,
            MaybeVarUseInfo)
    ;
        MaybeCalleeCliquePtr = no,
        % This is a recursive call site.

        deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
        CalleePDPtr = CSD ^ csd_callee,
        ( if
            not intermodule_var_use_should_follow_csd(VarUseOptions, CSDPtr)
        then
            % Don't follow this call across module boundaries.
            pessimistic_var_use_info(VarUseOptions ^ vuo_var_use_type, Cost,
                VarUseInfo),
            MaybeVarUseInfo = ok(VarUseInfo)
        else if set.member(CalleePDPtr, CallStack0) then
            % Return a first use time of 1.0. This is used by the formula
            % below.
            MaybeVarUseInfo = ok(var_use_info(1.0, Cost,
                VarUseOptions ^ vuo_var_use_type))
        else
            (
                MaybeDepth = yes(Depth0),
                recursion_depth_descend(Depth0, Depth),
                (
                    ( RecursionType = rt_not_recursive
                    ; RecursionType = rt_single(_, _, _, _, _)
                    ),
                    % Don't follow this recursive call as normal, doing so
                    % would make this analysis take too long. We can compute
                    % the cost of variable use time by the following formula:
                    set.insert(CalleePDPtr, CallStack0, CallStack),
                    proc_dynamic_recursive_var_use_info(ParentCliquePtr,
                        CalleePDPtr, ArgNum, RecursionType, Depth, Cost,
                        CallStack, VarUseOptions, MaybeVarUseInfo0),
                    (
                        MaybeVarUseInfo0 = ok(var_use_info(UseTime0, ProcTime0,
                            UseType)),
                        UseTime = UseTime0 + 1.0,
                        ProcTime = ProcTime0 + 1.0,
                        % Add a call sequence count for the cost of this call.
                        MaybeVarUseInfo = ok(var_use_info(UseTime, ProcTime,
                            UseType))
                    ;
                        MaybeVarUseInfo0 = error(_),
                        MaybeVarUseInfo = MaybeVarUseInfo0
                    )
                ;
                    ( RecursionType = rt_divide_and_conquer(_, _)
                    ; RecursionType = rt_mutual_recursion(_)
                    ; RecursionType = rt_other(_)
                    ; RecursionType = rt_errors(_)
                    ),
                    MaybeVarUseInfo = error(
                        "Cannot compute var use on a recursive call site "
                        ++ "for an unknown recursion type")
                )
            ;
                MaybeDepth = no,
                MaybeVarUseInfo = error(
                    "Cannot compute var use on a recursive call site "
                    ++ "for an unknown recursion depth")
            )
        )
    ).

clique_var_use_info(CliquePtr, ArgNum, VarUseOptions, MaybeVarUseInfo) :-
    Deep = VarUseOptions ^ vuo_deep,
    deep_lookup_clique_parents(Deep, CliquePtr, CSDPtr),
    deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
    Cost = float(inherit_callseqs(CSDDesc)),
    find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstProc,
        _OtherProcs),
    (
        MaybeFirstProc = yes(FirstPDPtr),
        VarUseType = VarUseOptions ^ vuo_var_use_type,
        ( if intermodule_var_use_should_follow_csd(VarUseOptions, CSDPtr) then
            create_clique_recursion_costs_report(Deep, CliquePtr,
                MaybeRecursionReport),
            (
                MaybeRecursionReport = ok(RecursionReport),
                RecursionType = RecursionReport ^ crr_recursion_type
            ;
                MaybeRecursionReport = error(Error),
                RecursionType = rt_errors([Error])
            ),
            (
                (
                    RecursionType = rt_not_recursive,
                    recursion_type_get_maybe_avg_max_depth(RecursionType,
                        yes(Depth)),
                    % XXX This shouldn't need the recursion information
                    % anymore.
                    proc_dynamic_var_use_info(CliquePtr, FirstPDPtr,
                        ArgNum, RecursionType, Depth, Cost, set.init,
                        VarUseOptions, MaybeVarUseInfo0)
                ;
                    RecursionType = rt_single(_, _, _, _, _),
                    recursion_type_get_maybe_avg_max_depth(RecursionType,
                        yes(Depth)),
                    % Add this PD to the proc dynamic pointer, we don't use a
                    % second recursion in this case.
                    proc_dynamic_recursive_var_use_info(CliquePtr, FirstPDPtr,
                        ArgNum, RecursionType, Depth, Cost,
                        set.make_singleton_set(FirstPDPtr),
                        VarUseOptions, MaybeVarUseInfo0)
                ),
                (
                    MaybeVarUseInfo0 = ok(var_use_info(UseTime0, ProcTime0,
                        UseType)),
                    UseTime = UseTime0 + 1.0,
                    ProcTime = ProcTime0 + 1.0,
                    % Add a call sequence count for the cost of this call.
                    MaybeVarUseInfo = ok(var_use_info(UseTime, ProcTime,
                        UseType))
                ;
                    MaybeVarUseInfo0 = error(_),
                    MaybeVarUseInfo = MaybeVarUseInfo0
                )
            ;
                ( RecursionType = rt_divide_and_conquer(_, _)
                ; RecursionType = rt_mutual_recursion(_)
                ; RecursionType = rt_other(_)
                ; RecursionType = rt_errors(_)
                ),
                pessimistic_var_use_info(VarUseType, Cost, VarUseInfo),
                MaybeVarUseInfo = ok(VarUseInfo)
            )
        else
            pessimistic_var_use_info(VarUseType, Cost, VarUseInfo),
            MaybeVarUseInfo = ok(VarUseInfo)
        )
    ;
        MaybeFirstProc = no,
        unexpected($pred, "clique has no first procedure")
    ).

proc_dynamic_var_use_info(CliquePtr, PDPtr, ArgNum, RecursionType,
        Depth, ProcCost, CallStack, VarUseOptions, MaybeVarUseInfo) :-
    prepare_for_proc_var_first_use(CliquePtr, PDPtr, ArgNum, RecursionType,
        Depth, VarUseOptions, Info),
    (
        Info = proc_var_first_use_prepared_info(Goal, _LastGoalId,
            ContainingGoalMap, CoverageArray, CallSiteCostMap,
            RecursiveCallSiteCostMap, Var),
        goal_var_first_use_wrapper(CliquePtr, CallStack,
            ContainingGoalMap, CoverageArray, CallSiteCostMap,
            RecursiveCallSiteCostMap, RecursionType, Depth, Goal, ProcCost,
            Var, VarUseOptions, VarUseInfo),
        MaybeVarUseInfo = ok(VarUseInfo)
    ;
        Info = error(Error),
        MaybeVarUseInfo = error(Error)
    ).

    % Like proc_dynamic_recursive_var_use_info except that it handles recursive
    % code by induction.
    %
    %   UseTime = BaseUseTime + (RecUseTime + 1.0) * Depth
    %
    % Where BaseUseTime is the use time in the base case execution and
    % RecUseTime is the use time before the recursive call. This works for any
    % self-recursion pattern.
    %
:- pred proc_dynamic_recursive_var_use_info(clique_ptr::in,
    proc_dynamic_ptr::in, int::in,
    recursion_type::in(recursion_type_known_costs), recursion_depth::in,
    float::in, set(proc_dynamic_ptr)::in, var_use_options::in,
    maybe_error(var_use_info)::out) is det.

proc_dynamic_recursive_var_use_info(CliquePtr, PDPtr, ArgNum,
        RecursionType, Depth, _Cost, CallStack, VarUseOptions,
        MaybeVarUseInfo) :-
    prepare_for_proc_var_first_use(CliquePtr, PDPtr, ArgNum, RecursionType,
        Depth, VarUseOptions, Info),
    (
        Info = proc_var_first_use_prepared_info(Goal, LastGoalId,
            ContainingGoalMap, CoverageArray, CallSiteCostMap,
            RecursiveCallSiteCostMap, Var),
        VarUseInfo = var_first_use_static_info(CliquePtr,
            CallSiteCostMap, RecursiveCallSiteCostMap, ContainingGoalMap,
            CoverageArray, Var, VarUseOptions, CallStack, RecursionType, Depth,
            no_recursion_info),

        RecProbsArray0 = create_goal_id_array(LastGoalId, impossible),
        build_recursive_call_sites_list(RecursiveCallSiteCostMap,
            RecursiveCalls),
        goal_rec_prob(Goal, RecursiveCalls, VarUseInfo, RecProb,
            RecProbsArray0, RecProbsArray),

        VarFirstUseInfoRecCase = VarUseInfo ^ fui_rec_info :=
            first_use_rec_info(RecProbsArray, recursive_case),
        rec_goal_var_first_use(Goal, RecursiveCalls, VarFirstUseInfoRecCase,
            RecFoundFirstUse, 0.0, RecTotalTime),
        VarFirstUseInfoBaseCase = VarUseInfo ^ fui_rec_info :=
            first_use_rec_info(RecProbsArray, base_case),
        rec_goal_var_first_use(Goal, RecursiveCalls, VarFirstUseInfoBaseCase,
            BaseFoundFirstUse, 0.0, BaseTotalTime),

        VarUseType = VarUseOptions ^ vuo_var_use_type,
        (
            RecFoundFirstUse = found_first_use(VarUseTimeRec)
        ;
            RecFoundFirstUse = have_not_found_first_use,
            (
                VarUseType = var_use_consumption,
                VarUseTimeRec = 0.0
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                VarUseTimeRec = RecTotalTime
            )
        ),
        (
            BaseFoundFirstUse = found_first_use(VarUseTimeBase)
        ;
            BaseFoundFirstUse = have_not_found_first_use,
            (
                VarUseType = var_use_consumption,
                VarUseTimeBase = 0.0
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                VarUseTimeBase = BaseTotalTime
            )
        ),

        DepthF = recursion_depth_to_float(Depth),
        VarUseTime = VarUseTimeBase +
            DepthF * VarUseTimeRec,
        Cost = RecTotalTime * probability_to_float(RecProb) +
            BaseTotalTime * probability_to_float(not_probability(RecProb)),
        MaybeVarUseInfo = ok(var_use_info(VarUseTime, Cost,
            VarUseOptions ^ vuo_var_use_type)),
        trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
            io.output_stream(OutputStream, !IO),
            io.write_string(OutputStream,
                "\nUseTime = BaseUseTime + RecUseTime * Depth\n", !IO),
            io.format(OutputStream, "%f = %f + %f * %f\n",
                [f(VarUseTime), f(VarUseTimeBase),
                f(VarUseTimeRec), f(DepthF)], !IO)
        )
    ;
        Info = error(Error),
        MaybeVarUseInfo = error(Error)
    ).

:- type proc_var_first_use_prepared_info
    --->    error(string)
    ;       proc_var_first_use_prepared_info(
                goal_rep(goal_id),
                goal_id,
                containing_goal_map,
                goal_attr_array(coverage_info),
                map(reverse_goal_path, cost_and_callees),
                map(reverse_goal_path, cs_cost_csq),
                var_rep
            ).

:- pred prepare_for_proc_var_first_use(clique_ptr::in, proc_dynamic_ptr::in,
    int::in, recursion_type::in(recursion_type_known_costs),
    recursion_depth::in, var_use_options::in,
    proc_var_first_use_prepared_info::out) is det.

prepare_for_proc_var_first_use(CliquePtr, PDPtr, ArgNum, RecursionType, Depth,
        VarUseOptions, Info) :-
    Deep = VarUseOptions ^ vuo_deep,
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    deep_get_maybe_procrep(Deep, PSPtr, MaybeProcrep),
    (
        MaybeProcrep = ok(ProcRep),
        ProcDefn = ProcRep ^ pr_defn,
        HeadVars = ProcDefn ^ pdr_head_vars,
        ( if index0(HeadVars, ArgNum, head_var_rep(Var, Mode)) then
            var_mode_to_var_use_type(Mode, ComputedUse),
            VarUseType = VarUseOptions ^ vuo_var_use_type,
            ( if VarUseType = ComputedUse then
                true
            else
                PDPtr = proc_dynamic_ptr(PDNum),
                string.format(
                    "Var uses do not match, passed: %s calculated from "
                    ++ "procrep: %s, Arg %d in proc dynamic %d",
                    [s(string(VarUseType)), s(string(ComputedUse)),
                     i(ArgNum), i(PDNum)], Msg),
                unexpected($pred, Msg)
            ),

            % Prepare callsite information.
            proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
            list.foldl(build_dynamic_call_site_cost_and_callee_map(Deep),
                Slots, map.init, CallSiteCostMap),

            build_recursive_call_site_cost_map(Deep, CliquePtr, PDPtr,
                RecursionType, yes(Depth), MaybeRecursiveCallSiteCostMap),
            MaybeRecursiveCallSiteCostMap = ok(RecursiveCallSiteCostMap),

            % Build procrep
            Goal0 = ProcDefn ^ pdr_goal,
            label_goals(LastGoalId, ContainingGoalMap, Goal0, Goal),

            % Build coverage annotation.
            deep_lookup_proc_statics(Deep, PSPtr, PS),
            ProcLabel = PS ^ ps_id,
            coverage_point_arrays_to_list(PS ^ ps_coverage_point_infos,
                CoveragePointsArray, CoveragePoints),
            MaybeCoveragePointsArray = PD ^ pd_maybe_coverage_points,
            (
                MaybeCoveragePointsArray = yes(CoveragePointsArray)
            ;
                MaybeCoveragePointsArray = no,
                unexpected($pred, "Couldn't get coverage info")
            ),
            list.foldl2(add_coverage_point_to_map, CoveragePoints,
                map.init, SolnsCoveragePointMap,
                map.init, BranchCoveragePointMap),
            deep_lookup_pd_own(Deep, PDPtr, Own),
            goal_annotate_with_coverage(ProcLabel, Goal, Own, CallSiteCostMap,
                SolnsCoveragePointMap, BranchCoveragePointMap,
                ContainingGoalMap, LastGoalId, CoverageArray),

            Info = proc_var_first_use_prepared_info(Goal, LastGoalId,
                ContainingGoalMap, CoverageArray, CallSiteCostMap,
                RecursiveCallSiteCostMap, Var)
        else
            PDPtr = proc_dynamic_ptr(PDNum),
            string.format(
                "proc_dynamic_var_use_info: ArgNum %d out of range for PD %d",
                [i(ArgNum), i(PDNum)], Msg),
            Info = error(Msg)
        )
    ;
        MaybeProcrep = error(Error),
        Info = error(Error)
    ).

%---------------------------------------------------------------------------%
%
% The actual first use analysis code.
%
% Any changes here should be reflected in the recursion and base case specific
% versions of this code below.
%
% TODO: This code is currently out of date with the code below, corrections
% need to be made to this code, see XXX markers below.
%

    % This type represents whether the first use of a variable has been found
    % or not. If it has then the call sequence counts since it was found is
    % stored in this type also.
    %
:- type found_first_use
    --->    have_not_found_first_use
    ;       found_first_use(
                cost_before_use     :: float
            ).

:- inst found_first_use_found for found_first_use/0
    --->    found_first_use(ground).

:- type var_first_use_static_info
    --->    var_first_use_static_info(
                fui_clique              :: clique_ptr,
                fui_call_site_map       :: map(reverse_goal_path,
                                                cost_and_callees),
                fui_rec_call_site_map   :: map(reverse_goal_path,
                                                cs_cost_csq),
                fui_containing_goal_map :: containing_goal_map,
                fui_coverage_array      :: goal_attr_array(coverage_info),
                fui_var                 :: var_rep,
                fui_var_use_opts        :: var_use_options,

                % A set of call sites whose analysis has started but not yet
                % completed. We keep this set to prevent infinite recursion
                % in the analysis itself.
                fui_call_stack          :: set(proc_dynamic_ptr),

                fui_recursion_type      :: recursion_type,
                fui_cur_depth           :: recursion_depth,
                fui_rec_info            :: first_use_rec_info
            ).

:- inst var_first_use_static_info for var_first_use_static_info/0
    --->    var_first_use_static_info(
                ground, ground, ground, ground, ground, ground, ground, ground,
                recursion_type_known_costs,
                ground, ground
            ).

:- inst var_first_use_static_info_rec for var_first_use_static_info/0
    --->    var_first_use_static_info(
                ground, ground, ground, ground, ground, ground, ground, ground,
                recursion_type_known_costs,
                ground,
                first_use_rec_info
            ).

:- type first_use_rec_info
    --->    first_use_rec_info(
                furi_rec_prob_array     :: goal_attr_array(probability),
                furi_rec_case           :: recursive_case
            )
    ;       no_recursion_info.

:- inst first_use_rec_info for first_use_rec_info/0
    --->    first_use_rec_info(ground, ground).

:- inst no_recursion_info for first_use_rec_info/0
    --->    no_recursion_info.

:- type recursive_case
    --->    recursive_case
    ;       base_case.

    % Find the first use of a variable in a goal.
    % Procedure calls can be resolved via the call site which we'll need to
    % lookup anyway to find cost information, This will callback to the deep
    % profiler as it crosses procedure boundaries.
    %
    % This does not follow higher order or method calls. It may be possible to
    % follow call the calls seen during profiling and aggregate their variable
    % use information based on how often they are called from that call site.
    %
:- pred goal_var_first_use(reverse_goal_path::in, goal_rep(goal_id)::in,
    var_first_use_static_info::in(var_first_use_static_info), float::in,
    float::out, found_first_use::out) is det.

goal_var_first_use(RevGoalPath, Goal, StaticInfo, !CostSoFar, FoundFirstUse) :-
    Goal = goal_rep(GoalExpr, Detism, _),
    CoverageArray = StaticInfo ^ fui_coverage_array,
    Coverage = get_goal_attribute_det(CoverageArray, Goal ^ goal_annotation),
    ( if
        % Do not bother exploring this goal if it is never entered. Or never
        % finishes and we're looking for a production.
        (
            get_coverage_before(Coverage, 0)
        ;
            VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
            VarUseType = var_use_production,
            (
                Detism = erroneous_rep
            ;
                Detism = failure_rep
            ;
                get_coverage_after(Coverage, 0)
            )
        )
    then
        FoundFirstUse = have_not_found_first_use
    else
        (
            GoalExpr = conj_rep(Conjuncts),
            conj_var_first_use(RevGoalPath, 1, Conjuncts, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            GoalExpr = disj_rep(Disjuncts),
            disj_var_first_use(RevGoalPath, Disjuncts, Detism, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            GoalExpr = switch_rep(SwitchedOnVar, _CanFail, Cases),
            switch_var_first_use(RevGoalPath, SwitchedOnVar, Cases,
                StaticInfo, !CostSoFar, FoundFirstUse)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_var_first_use(RevGoalPath, Cond, Then, Else, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            (
                GoalExpr = negation_rep(SubGoal),
                GoalPathStep = step_neg
            ;
                GoalExpr = scope_rep(SubGoal, ScopeIsCut),
                GoalPathStep = step_scope(ScopeIsCut)
            ),
            RevSubGoalPath = rgp_cons(RevGoalPath, GoalPathStep),
            goal_var_first_use(RevSubGoalPath, SubGoal, StaticInfo,
                !CostSoFar, FoundFirstUse)
        ;
            GoalExpr = atomic_goal_rep(_, _, BoundVars, AtomicGoal),
            (
                ( AtomicGoal = plain_call_rep(_, _, _)
                ; AtomicGoal = higher_order_call_rep(_, _)
                ; AtomicGoal = method_call_rep(_, _, _)
                ),
                call_var_first_use(AtomicGoal, BoundVars, RevGoalPath,
                    StaticInfo, FoundFirstUse, !CostSoFar)
            ;
                ( AtomicGoal = unify_construct_rep(_, _, _)
                ; AtomicGoal = unify_deconstruct_rep(_, _, _)
                ; AtomicGoal = partial_construct_rep(_, _, _)
                ; AtomicGoal = partial_deconstruct_rep(_, _, _)
                ; AtomicGoal = unify_assign_rep(_, _)
                ; AtomicGoal = cast_rep(_, _)
                ; AtomicGoal = unify_simple_test_rep(_, _)
                ; AtomicGoal = pragma_foreign_code_rep(_)
                ; AtomicGoal = event_call_rep(_, _)
                ; AtomicGoal = builtin_call_rep(_, _, _)
                ),
                % trivial goals have a zero cost, so !CostSoFar is not updated.
                atomic_trivial_var_first_use(AtomicGoal, BoundVars,
                    !.CostSoFar, StaticInfo, FoundFirstUse)
            )
        )
    ),
    trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
        io.output_stream(OutputStream, !IO),
        io.format(OutputStream, "Trace: goal_var_first_use: %s\n",
            [s(rev_goal_path_to_string(RevGoalPath))], !IO)
    ).

:- inst atomic_goal_rep_call
    for mdbcomp.program_representation.atomic_goal_rep/0
    --->    plain_call_rep(ground, ground, ground)
    ;       higher_order_call_rep(ground, ground)
    ;       method_call_rep(ground, ground, ground).

:- pred call_var_first_use(atomic_goal_rep::in(atomic_goal_rep_call),
    list(var_rep)::in, reverse_goal_path::in,
    var_first_use_static_info::in(var_first_use_static_info),
    found_first_use::out, float::in, float::out) is det.

call_var_first_use(AtomicGoal, BoundVars, RevGoalPath, StaticInfo,
        FoundFirstUse, !CostSoFar) :-
    CostBefore = !.CostSoFar,
    StaticInfo = var_first_use_static_info(CliquePtr, CostMap,
        RecCostMap, _ContainingGoalMap, _CoverageArray, Var, VarUseOptions,
        _CallStack, _RecursionType, _MaybeCurDepth, _RecInfo),
    VarUseType = VarUseOptions ^ vuo_var_use_type,
    map.lookup(CostMap, RevGoalPath, CostAndCallees),

    % Get the cost of the call.
    ( if cost_and_callees_is_recursive(CliquePtr, CostAndCallees) then
        map.lookup(RecCostMap, RevGoalPath, Cost0)
    else
        Cost0 = CostAndCallees ^ cac_cost
    ),
    ( if cs_cost_get_calls(Cost0) = 0.0 then
        Cost = 0.0
    else
        Cost = cs_cost_get_percall(Cost0)
    ),
    !:CostSoFar = !.CostSoFar + Cost,

    % Determine if the variable we are searching for uses of is involved with
    % this call.
    (
        AtomicGoal = plain_call_rep(_, _, Args),
        Vars = Args
    ;
        ( AtomicGoal = higher_order_call_rep(HOVar, Args)
        ; AtomicGoal = method_call_rep(HOVar, _, Args)
        ),
        Vars = [HOVar | Args]
    ),
    ( if list.member(Var, Vars) then
        (
            ( AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            % Conservative assumption, but we *could* do better, since we
            % can find out what procedures are actually called at this call
            % site.
            FirstTime = 0.0
        ;
            AtomicGoal = plain_call_rep(_, _, _),
            call_args_first_use(Args, Cost, StaticInfo, CostAndCallees, Times),
            (
                Times = [],
                unexpected($pred, "no solutions for variable first use time")
            ;
                Times = [FirstTime | LaterTimes],
                ( if
                    VarUseType = var_use_production,
                    LaterTimes = [_ | _]
                then
                    unexpected($pred,
                        "multiple solutions for variable production time")
                else
                    true
                )
            )
        ),

        FoundFirstUse = found_first_use(CostBefore + FirstTime),

        % Assertions.
        ( if
            VarUseType = var_use_production,
            not list.member(Var, BoundVars)
        then
            unexpected($pred,
                "a bound var must be produced by a call if it is an argument.")
        else
            true
        ),
        ( if
            VarUseType = var_use_consumption,
            list.member(Var, BoundVars)
        then
            unexpected($pred,
                "a consumed var must not be mentioned in BoundVars")
        else
            true
        ),
        ( if
            VarUseType = var_use_production,
            ( AtomicGoal = higher_order_call_rep(Var, _)
            ; AtomicGoal = method_call_rep(Var, _, _)
            )
        then
            unexpected($pred, "a HO call site cannot produce its own HO value")
        else
            true
        )
    else
        FoundFirstUse = have_not_found_first_use
    ).

:- pred consume_ho_arg(atomic_goal_rep::in(atomic_goal_rep_call),
    var_rep::in, float::out) is semidet.

consume_ho_arg(higher_order_call_rep(Var, _), Var, 0.0).
consume_ho_arg(method_call_rep(Var, _, _), Var, 0.0).

:- pred call_args_first_use(list(var_rep)::in, float::in,
    var_first_use_static_info::in(var_first_use_static_info),
    cost_and_callees::in, list(float)::out) is det.

call_args_first_use(Args, Cost, StaticInfo, CostAndCallees, Times) :-
    StaticInfo = var_first_use_static_info(CliquePtr, _CostMap,
        _RecCostMap, _ContainingGoalMap, _CoverageArray, Var, VarUseOptions,
        CallStack, RecursionType, CurDepth, _RecInfo),
    VarUseType = VarUseOptions ^ vuo_var_use_type,
    HigherOrder = CostAndCallees ^ cac_call_site_is_ho,
    (
        HigherOrder = first_order_call,
        Callees = CostAndCallees ^ cac_callees,
        list.member_indexes0(Var, Args, ArgNums),
        ( if set.is_empty(Callees) then
            % There are no callees; this code is never called.
            pessimistic_var_use_time(VarUseType, Cost, Time),
            Times = [Time]
        else if set.is_singleton(Callees, SingletonCallee) then
            CSDPtr = SingletonCallee ^ c_csd,
            list.map(
                get_call_site_dynamic_var_use_time(CliquePtr, CSDPtr,
                    RecursionType, CurDepth, Cost, CallStack, VarUseOptions),
                ArgNums, Times0),
            list.sort_and_remove_dups(Times0, Times)
        else
            unexpected($pred, "wrong number of callees for normal call site")
        )
    ;
        HigherOrder = higher_order_call,
        % The compiler cannot push signals or waits into higher order calls.
        % Therefore we assume a pessimistic default here.
        pessimistic_var_use_time(VarUseType, Cost, Time),
        Times = [Time]
    ).

:- pred get_call_site_dynamic_var_use_time(clique_ptr::in,
    call_site_dynamic_ptr::in, recursion_type::in,
    recursion_depth::in, float::in, set(proc_dynamic_ptr)::in,
    var_use_options::in, int::in, float::out) is det.

get_call_site_dynamic_var_use_time(CliquePtr, CSDPtr,
        RecursionType, CurDepth, Cost, CallStack,
        VarUseOptions, ArgNum, Time) :-
    get_call_site_dynamic_var_use_info_rec_level(CliquePtr, CSDPtr,
        ArgNum, RecursionType, yes(CurDepth), Cost, CallStack,
        VarUseOptions, MaybeVarUseInfo),
    (
        MaybeVarUseInfo = ok(VarUseInfo),
        VarUseInfo = var_use_info(Time, _, _)
    ;
        MaybeVarUseInfo = error(_),
        VarUseType = VarUseOptions ^ vuo_var_use_type,
        pessimistic_var_use_time(VarUseType, Cost, Time)
    ).

:- inst atomic_trivial_goal_rep
    for mdbcomp.program_representation.atomic_goal_rep/0
    --->    unify_construct_rep(ground, ground, ground)
    ;       unify_deconstruct_rep(ground, ground, ground)
    ;       partial_construct_rep(ground, ground, ground)
    ;       partial_deconstruct_rep(ground, ground, ground)
    ;       unify_assign_rep(ground, ground)
    ;       cast_rep(ground, ground)
    ;       unify_simple_test_rep(ground, ground)
    ;       pragma_foreign_code_rep(ground)
    ;       event_call_rep(ground, ground)
    ;       builtin_call_rep(ground, ground, ground).

:- pred atomic_trivial_var_first_use(
    atomic_goal_rep::in(atomic_trivial_goal_rep), list(var_rep)::in, float::in,
    var_first_use_static_info::in, found_first_use::out) is det.

atomic_trivial_var_first_use(AtomicGoal, BoundVars, CostSoFar, StaticInfo,
        FoundFirstUse) :-
    Var = StaticInfo ^ fui_var,
    VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
    atomic_goal_get_vars(AtomicGoal, Vars),
    ( if
        set.member(Var, Vars),
        (
            VarUseType = var_use_consumption
        ;
            VarUseType = var_use_production,
            list.member(Var, BoundVars)
        ;
            VarUseType = var_use_other
        )
    then
        FoundFirstUse = found_first_use(CostSoFar)
    else
        FoundFirstUse = have_not_found_first_use
    ).

    % Find the first use of a variable within a conjunction. Note that when
    % looking for a production of the variable we search backward and add the
    % time from the end of the goal. Similarly with other goal types that have
    % an execution order, namely disjunctions and if-then-elses.
    %
:- pred conj_var_first_use(reverse_goal_path::in, int::in,
    list(goal_rep(goal_id))::in,
    var_first_use_static_info::in(var_first_use_static_info),
    float::in, float::out, found_first_use::out) is det.

conj_var_first_use(_, _, [], _, !Cost, have_not_found_first_use).
conj_var_first_use(RevGoalPath, ConjNum, [Conj | Conjs], StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    goal_var_first_use(rgp_cons(RevGoalPath, step_conj(ConjNum)), Conj,
        StaticInfo, !CostSoFar, HeadFoundFirstUse),
    (
        % If a variable is bound more than once, then we want to use
        % the last time it is bound, since all of the earlier bindings
        % must have only partially instantiated it. Instmaps can be used
        % to track this. This is relevant when searching for the producer
        % of a variable.
        % XXX For now, we assume that no variable is bound more than once.
        HeadFoundFirstUse = found_first_use(_),
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = have_not_found_first_use,
        conj_var_first_use(RevGoalPath, ConjNum + 1, Conjs, StaticInfo,
            !CostSoFar, TailFoundFirstUse),
        FoundFirstUse = TailFoundFirstUse
    ).

:- pred disj_var_first_use(reverse_goal_path::in,
    list(goal_rep(goal_id))::in, detism_rep::in,
    var_first_use_static_info::in(var_first_use_static_info),
    float::in, float::out, found_first_use::out) is det.

disj_var_first_use(RevGoalPath, Disjuncts, Detism, StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    % We cannot handle nondet/multi disjunctions. So we use pessimistic
    % defaults for FoundFirstUse if this disjunction is nondet or multi.
    % For calculating the cost of the disjunction, assume that it is a semidet
    % disjunction. Doing this will find the incorrect cost for the
    % disjunction. However disjunctions occur rarely, so this is not likely
    % to dramatically affect anything.
    CostBeforeDisjunction = !.CostSoFar,
    disj_var_first_use_2(RevGoalPath, 1, Disjuncts, StaticInfo,
        !CostSoFar, FoundFirstUse0),
    CostAfterDisjunction = !.CostSoFar,
    ( if
        detism_get_solutions(Detism) = at_most_many_rep,
        FoundFirstUse0 = found_first_use(_)
    then
        VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
        (
            VarUseType = var_use_consumption,
            FoundFirstUse = found_first_use(CostBeforeDisjunction)
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            FoundFirstUse = found_first_use(CostAfterDisjunction)
        )
    else
        FoundFirstUse = FoundFirstUse0
    ).

:- pred disj_var_first_use_2(reverse_goal_path::in, int::in,
    list(goal_rep(goal_id))::in,
    var_first_use_static_info::in(var_first_use_static_info),
    float::in, float::out, found_first_use::out) is det.

disj_var_first_use_2(_, _, [], _, !CostSoFar, have_not_found_first_use).
disj_var_first_use_2(RevGoalPath, DisjNum, [Disj | Disjs], StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
    CoverageArray = StaticInfo ^ fui_coverage_array,
    goal_var_first_use(rgp_cons(RevGoalPath, step_disj(DisjNum)), Disj,
        StaticInfo, !CostSoFar, HeadFoundFirstUse),
    disj_var_first_use_2(RevGoalPath, DisjNum + 1, Disjs, StaticInfo,
        !CostSoFar, TailFoundFirstUse),
    (
        HeadFoundFirstUse = have_not_found_first_use,
        TailFoundFirstUse = have_not_found_first_use,
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = have_not_found_first_use,
        TailFoundFirstUse = found_first_use(_),
        FoundFirstUse = TailFoundFirstUse
    ;
        HeadFoundFirstUse = found_first_use(_),
        TailFoundFirstUse = have_not_found_first_use,
        FoundFirstUse = HeadFoundFirstUse
    ;
        HeadFoundFirstUse = found_first_use(HeadCost),
        TailFoundFirstUse = found_first_use(TailCost),
        (
            VarUseType = var_use_consumption,
            % The variable is probably consumed in the first disjunct
            % even if it fails. This is also the pessimistic default.
            Cost = HeadCost
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            % Use a weighted average to reflect the likely success
            % of the first disjunct.
            DisjCoverage =
                get_goal_attribute_det(CoverageArray, Disj ^ goal_annotation),
            ( if get_coverage_before(DisjCoverage, HeadCount) then
                HeadWeight = float(HeadCount)
            else
                unexpected($pred, "unknown coverage before disjunct")
            ),
            (
                Disjs = [],
                TailWeight = 0.0
            ;
                Disjs = [FirstTailDisj | _],
                FirstTailCoverage = get_goal_attribute_det(CoverageArray,
                    FirstTailDisj ^ goal_annotation),
                ( if get_coverage_before(FirstTailCoverage, TailCount) then
                    TailWeight = float(TailCount)
                else
                    unexpected($pred, "unknown coverage before disjunct")
                )
            ),
            weighted_average([HeadWeight, TailWeight], [HeadCost, TailCost],
                Cost)
        ),
        FoundFirstUse = found_first_use(Cost)
    ).

:- pred switch_var_first_use(reverse_goal_path::in, var_rep::in,
    list(case_rep(goal_id))::in,
    var_first_use_static_info::in(var_first_use_static_info),
    float::in, float::out, found_first_use::out) is det.

switch_var_first_use(RevGoalPath, SwitchedOnVar, Cases, StaticInfo,
        CostBeforeSwitch, CostAfterSwitch, FoundFirstUse) :-
    switch_var_first_use_2(RevGoalPath, 1, StaticInfo, Cases, CaseWeights,
        CostBeforeSwitch, CostCases, FoundFirstUseCases),
    weighted_average(CaseWeights, CostCases, CostAfterSwitch),
    Var = StaticInfo ^ fui_var,
    ( if Var = SwitchedOnVar then
        % This can only possibly be a consumption of this variable.
        FoundFirstUse = found_first_use(CostBeforeSwitch)
    else
        ( if
            list.all_true(unify(have_not_found_first_use), FoundFirstUseCases)
        then
            % No case contained a first-use of this variable.
            FoundFirstUse = have_not_found_first_use
        else
            VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostAfterSwitch
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostBeforeSwitch
            ),
            list.map(ffu_to_float(DefaultCost), FoundFirstUseCases,
                FirstUseTimes),
            weighted_average(CaseWeights, FirstUseTimes, AvgFirstUseTime),
            FoundFirstUse = found_first_use(AvgFirstUseTime)
        )
    ).

:- pred switch_var_first_use_2(reverse_goal_path::in, int::in,
    var_first_use_static_info::in(var_first_use_static_info),
    list(case_rep(goal_id))::in, list(float)::out, float::in,
    list(float)::out, list(found_first_use)::out) is det.

switch_var_first_use_2(_, _, _, [], [], _, [], []).
switch_var_first_use_2(RevGoalPath, CaseNum, StaticInfo, [Case | Cases],
        [Weight | Weights], Cost0, [Cost | Costs],
        [FoundFirstUse | FoundFirstUses]) :-
    switch_var_first_use_2(RevGoalPath, CaseNum + 1, StaticInfo,
        Cases, Weights, Cost0, Costs, FoundFirstUses),
    Case = case_rep(_, _, Goal),
    RevArmPath = rgp_cons(RevGoalPath,
        step_switch(CaseNum, unknown_num_functors_in_type)),
    goal_var_first_use(RevArmPath, Goal, StaticInfo, Cost0, Cost,
        FoundFirstUse),
    Coverage = get_goal_attribute_det(StaticInfo ^ fui_coverage_array,
        Goal ^ goal_annotation),
    get_coverage_before_det(Coverage, BeforeCount),
    Weight = float(BeforeCount).

:- pred ite_var_first_use(reverse_goal_path::in,
    goal_rep(goal_id)::in, goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    var_first_use_static_info::in(var_first_use_static_info),
    float::in, float::out, found_first_use::out)
    is det.

ite_var_first_use(RevGoalPath, Cond, Then, Else, StaticInfo,
        !CostSoFar, FoundFirstUse) :-
    CoverageArray = StaticInfo ^ fui_coverage_array,
    ThenCoverage =
        get_goal_attribute_det(CoverageArray, Then ^ goal_annotation),
    get_coverage_before_det(ThenCoverage, CountBeforeThen),
    ElseCoverage =
        get_goal_attribute_det(CoverageArray, Else ^ goal_annotation),
    get_coverage_before_det(ElseCoverage, CountBeforeElse),
    Weights = [float(CountBeforeThen), float(CountBeforeElse)],
    RevCondGoalPath = rgp_cons(RevGoalPath, step_ite_cond),
    RevThenGoalPath = rgp_cons(RevGoalPath, step_ite_then),
    RevElseGoalPath = rgp_cons(RevGoalPath, step_ite_else),
    VarUseType = StaticInfo ^ fui_var_use_opts ^ vuo_var_use_type,
    CostBeforeITE = !.CostSoFar,
    goal_var_first_use(RevCondGoalPath, Cond, StaticInfo,
        CostBeforeITE, CostAfterCond, CondFoundFirstUse),
    goal_var_first_use(RevThenGoalPath, Then, StaticInfo,
        CostAfterCond, CostAfterThen, ThenFoundFirstUse),
    goal_var_first_use(RevElseGoalPath, Else, StaticInfo,
        CostAfterCond, CostAfterElse, ElseFoundFirstUse),
    weighted_average(Weights, [CostAfterThen, CostAfterElse], CostAfterITE),
    !:CostSoFar = CostAfterITE,
    (
        CondFoundFirstUse = found_first_use(_),
        FoundFirstUse = CondFoundFirstUse
    ;
        CondFoundFirstUse = have_not_found_first_use,
        ( if
            ThenFoundFirstUse = have_not_found_first_use,
            ElseFoundFirstUse = have_not_found_first_use
        then
            FoundFirstUse = have_not_found_first_use
        else
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostAfterCond
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostAfterITE
            ),
            ffu_to_float(DefaultCost, ThenFoundFirstUse, ThenVarUseTime),
            ffu_to_float(DefaultCost, ElseFoundFirstUse, ElseVarUseTime),
            weighted_average(Weights, [ThenVarUseTime, ElseVarUseTime],
                VarUseTime),
            FoundFirstUse = found_first_use(VarUseTime),
            trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
                io.output_stream(OutputStream, !IO),
                io.format(OutputStream,
                    "Trace: ITE: Weights: %s, Then: %f, Else: %f, " ++
                    "VarUseTime: %f\n",
                    [s(string(Weights)), f(ThenVarUseTime), f(ElseVarUseTime),
                        f(VarUseTime)],
                    !IO)
            )
        )
    ).

:- pred ffu_to_float(float::in, found_first_use::in, float::out) is det.

ffu_to_float(Default, have_not_found_first_use, Default).
ffu_to_float(_, found_first_use(UseTime), UseTime).

%---------------------------------------------------------------------------%
%
% Goal var first use analysis for the base and recursive cases of a recursive
% procedure.
%

:- type recursive_calls_list ==
    assoc_list(forward_goal_path, float).

:- pred build_recursive_call_sites_list(
    map(reverse_goal_path, cs_cost_csq)::in,
    recursive_calls_list::out) is det.

build_recursive_call_sites_list(Map, List) :-
    List0 = to_assoc_list(Map),
    list.map(
        ( pred((RevGoalPath - Cost)::in, (GoalPath - Calls)::out) is det :-
            Calls = cs_cost_get_calls(Cost),
            rgp_to_fgp(RevGoalPath, GoalPath)
        ), List0, List).

:- pred filter_recursive_call_sites(goal_path_step::in,
    recursive_calls_list::in, recursive_calls_list::out) is det.

filter_recursive_call_sites(GoalPathStep, !RecCallSites) :-
    filter_map((pred((GP0 - Coverage)::in, (GP - Coverage)::out) is semidet :-
            GP0 = fgp_cons(GoalPathStep, GP)
        ), !RecCallSites).

    % rec_goal_var_first_use(Goal, RecCalls, Info, FoundFirstUse, !CostSoFar).
    %
    % Find the first use in both the recursive and base cases.
    %
    % This works under the following assumptions.
    %   + All sub goals succeed at most once.
    %   + The first use is not conjoined with a switch where some switch
    %     branches are recursive and some are not. (or ITE)
    %   + The cost of disjunctions is not computed correctly.
    %
:- pred rec_goal_var_first_use(goal_rep(goal_id)::in,
    recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_goal_var_first_use(Goal, RecCalls, Info, FoundFirstUse,
        !CostSoFar) :-
    Goal = goal_rep(GoalExpr, Detism, GoalId),
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, GoalId),
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( if
        % Do not bother exploring this goal if it is never entered.
        % Or never finishes and we are looking for a production.
        (
            Before = 0
        ;
            VarUseType = Info ^ fui_var_use_opts ^ vuo_var_use_type,
            VarUseType = var_use_production,
            ( Detism = erroneous_rep
            ; Detism = failure_rep
            ; After = 0
            )
        )
    then
        FoundFirstUse = have_not_found_first_use
    else
        (
            GoalExpr = conj_rep(Conjs),
            rec_conj_var_first_use(Conjs, 1, RecCalls, Info,
                FoundFirstUse, !CostSoFar)
        ;
            GoalExpr = disj_rep(Disjs),
            rec_disj_var_first_use(Disjs, RecCalls, Info, FoundFirstUse,
                !CostSoFar)
        ;
            GoalExpr = switch_rep(SwitchedOnVar, _CanFail, Cases),
            rec_switch_var_first_use(Cases, SwitchedOnVar, RecCalls,
                Info, FoundFirstUse, !CostSoFar)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            rec_ite_var_first_use(Cond, Then, Else,
                RecCalls, Info, FoundFirstUse, !CostSoFar)
        ;
            (
                GoalExpr = negation_rep(SubGoal),
                GoalPathStep = step_neg
            ;
                GoalExpr = scope_rep(SubGoal, ScopeIsCut),
                GoalPathStep = step_scope(ScopeIsCut)
            ),
            filter_recursive_call_sites(GoalPathStep, RecCalls,
                RecCallsSubGoal),
            rec_goal_var_first_use(SubGoal, RecCallsSubGoal,
                Info, FoundFirstUse, !CostSoFar)
        ;
            GoalExpr = atomic_goal_rep(_, _, BoundVars, AtomicGoal),
            (
                ( AtomicGoal = plain_call_rep(_, _, _)
                ; AtomicGoal = higher_order_call_rep(_, _)
                ; AtomicGoal = method_call_rep(_, _, _)
                ),
                ContainingGoalMap = Info ^ fui_containing_goal_map,
                RevGoalPath =
                    goal_id_to_reverse_path(ContainingGoalMap, GoalId),
                call_var_first_use(AtomicGoal, BoundVars, RevGoalPath,
                    Info, FoundFirstUse, !CostSoFar)
            ;
                ( AtomicGoal = unify_construct_rep(_, _, _)
                ; AtomicGoal = unify_deconstruct_rep(_, _, _)
                ; AtomicGoal = partial_construct_rep(_, _, _)
                ; AtomicGoal = partial_deconstruct_rep(_, _, _)
                ; AtomicGoal = unify_assign_rep(_, _)
                ; AtomicGoal = cast_rep(_, _)
                ; AtomicGoal = unify_simple_test_rep(_, _)
                ; AtomicGoal = pragma_foreign_code_rep(_)
                ; AtomicGoal = event_call_rep(_, _)
                ; AtomicGoal = builtin_call_rep(_, _, _)
                ),
                % trivial goals have a zero cost, so !CostSoFar is not updated.
                atomic_trivial_var_first_use(AtomicGoal, BoundVars,
                    !.CostSoFar, Info, FoundFirstUse)
            )
        )
    ),
    trace [compile_time(flag("debug_first_var_use")), io(!IO)] (
        some [ContainingGoalMap, RevGoalPath] (
            ContainingGoalMap = Info ^ fui_containing_goal_map,
            RevGoalPath =
                goal_id_to_reverse_path(ContainingGoalMap, GoalId),
            io.output_stream(OutputStream, !IO),
            io.format(OutputStream, "Trace: goal_var_first_use: %s\n",
                [s(rev_goal_path_to_string(RevGoalPath))], !IO)
        )
    ).

:- pred rec_conj_var_first_use(list(goal_rep(goal_id))::in, int::in,
    recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_conj_var_first_use([], _, _, _, have_not_found_first_use, !CostSoFar).
rec_conj_var_first_use([Conj | Conjs], ConjNum, RecCalls, StaticInfo,
        FoundFirstUse, !CostSoFar) :-
    filter_recursive_call_sites(step_conj(ConjNum), RecCalls, ConjRecCalls),
    rec_goal_var_first_use(Conj, ConjRecCalls, StaticInfo,
        ConjFoundFirstUse, !CostSoFar),
    rec_conj_var_first_use(Conjs, ConjNum + 1, RecCalls, StaticInfo,
        ConjsFoundFirstUse, !CostSoFar),
    (
        % XXX if a variable is bound more than once, because it's used
        % with partial instantiation then we want to use the last time it
        % is bound. Instmaps can be used to track this. This is relevant
        % when searching for the producer of a variable.
        ConjFoundFirstUse = found_first_use(UseTime),
        FoundFirstUse = found_first_use(UseTime)
    ;
        ConjFoundFirstUse = have_not_found_first_use,
        % XXX Use time should be adjusted for the probability of entering
        % Conjs (the success of Conj) But doing so means a weighted average
        % between the success and failure paths, which only makes sense if
        % the consumption (because this is semidet) might be done in the
        % failure case. This also has to be done if the probability of
        % recursion in one of the two cases is different. For now we assume
        % that Conjs will always be entered.
        FoundFirstUse = ConjsFoundFirstUse
    ).

:- pred rec_disj_var_first_use(list(goal_rep(goal_id))::in,
    recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_disj_var_first_use(Disjs, RecCalls, Info, FoundFirstUse, !CostSoFar) :-
    % We do not handle disjunctions, just use a pessimistic default.
    % For calculating the cost of the disjunction, assume that it is a semidet
    % disjunction. Doing this will find the incorrect cost for the
    % disjunction, however disjunctions occur rarely, so this is not likely
    % to dramatically affect anything.
    CostBeforeDisjunction = !.CostSoFar,
    rec_disj_var_first_use_2(Disjs, 1, RecCalls, Info, FoundFirstUse0,
        !CostSoFar),
    CostAfterDisjunction = !.CostSoFar,
    (
        FoundFirstUse0 = found_first_use(_),
        VarUseType = Info ^ fui_var_use_opts ^ vuo_var_use_type,
        (
            VarUseType = var_use_consumption,
            FoundFirstUse = found_first_use(CostBeforeDisjunction)
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            FoundFirstUse = found_first_use(CostAfterDisjunction)
        )
    ;
        FoundFirstUse0 = have_not_found_first_use,
        FoundFirstUse = FoundFirstUse0
    ).

:- pred rec_disj_var_first_use_2(list(goal_rep(goal_id))::in, int::in,
    recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_disj_var_first_use_2([], _, _, _, have_not_found_first_use, !CostSoFar).
rec_disj_var_first_use_2([Disj | Disjs], DisjNum, RecCalls, Info,
        FoundFirstUse, !CostSoFar) :-
    filter_recursive_call_sites(step_disj(DisjNum), RecCalls, DisjRecCalls),
    rec_goal_var_first_use(Disj, DisjRecCalls, Info, DisjFoundFirstUse,
        !CostSoFar),

    CoverageArray = Info ^ fui_coverage_array,
    Coverage = get_goal_attribute_det(CoverageArray, Disj ^ goal_annotation),
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( if Before = 0 then
        % Avoid a divide by zero.
        CostDisjs = 0.0,
        FoundFirstUse = DisjFoundFirstUse
    else
        rec_disj_var_first_use_2(Disjs, DisjNum + 1, RecCalls, Info,
            DisjsFoundFirstUse, 0.0, CostDisjs0),
        FailureProb = probable(float(Before - After) / float(Before)),
        CostDisjs = CostDisjs0 * probability_to_float(FailureProb),
        (
            DisjFoundFirstUse = have_not_found_first_use,
            FoundFirstUse = DisjsFoundFirstUse
        ;
            DisjFoundFirstUse = found_first_use(UseTime),
            FoundFirstUse = found_first_use(UseTime)
        )
    ),
    !:CostSoFar = !.CostSoFar + CostDisjs.

:- pred rec_switch_var_first_use(list(case_rep(goal_id))::in,
    var_rep::in, recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_switch_var_first_use(Cases, SwitchedOnVar, RecCalls, Info,
        FoundFirstUse, CostBeforeSwitch, CostAfterSwitch) :-
    rec_switch_var_first_use_2(Cases, 1, RecCalls, Info, CostBeforeSwitch,
        CaseWeights0, FoundFirstUseCases, CostAfterCases, RecProbs),
    RecCase = Info ^ fui_rec_info ^ furi_rec_case,
    map_corresponding(adjust_weight_for_recursion(RecCase),
        RecProbs, CaseWeights0, CaseWeights),
    weighted_average(CaseWeights, CostAfterCases, CostAfterSwitch),

    Var = Info ^ fui_var,
    ( if Var = SwitchedOnVar then
        FoundFirstUse = found_first_use(CostBeforeSwitch)
    else
        ( if
            list.all_true(unify(have_not_found_first_use), FoundFirstUseCases)
        then
            % No case contained a first-use of this variable.
            FoundFirstUse = have_not_found_first_use
        else
            VarUseType = Info ^ fui_var_use_opts ^ vuo_var_use_type,
            % XXX this is also flawed, the default costs should not be the
            % average costs, they should be the cost for the specific case
            % where that default would be used.
            % XXX Secondly, this needs to also support the 'don't insert waits
            % on all branches' optimisation.
            (
                VarUseType = var_use_consumption,
                DefaultCost = CostAfterSwitch
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                DefaultCost = CostBeforeSwitch
            ),
            list.map(ffu_to_float(DefaultCost),
                FoundFirstUseCases, FirstUseTimes),
            weighted_average(CaseWeights, FirstUseTimes, UseTime),
            FoundFirstUse = found_first_use(UseTime)
        )
    ).

:- pred rec_switch_var_first_use_2(list(case_rep(goal_id))::in, int::in,
    recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    float::in, list(float)::out, list(found_first_use)::out,
    list(float)::out, list(probability)::out) is det.

rec_switch_var_first_use_2([], _, _, _, _, [], [], [], []).
rec_switch_var_first_use_2([Case | Cases], CaseNum, RecCalls, Info,
        CostBeforeSwitch, Weights, FoundFirstUses, CostsAfter, RecProbs) :-
    rec_switch_var_first_use_2(Cases, CaseNum + 1, RecCalls, Info,
        CostBeforeSwitch, CasesWeights, CasesFoundFirstUses, CasesCostsAfter,
        CasesRecProbs),
    Goal = Case ^ cr_case_goal,
    GoalId = Goal ^ goal_annotation,
    filter_recursive_call_sites(
        step_switch(CaseNum, unknown_num_functors_in_type),
        RecCalls, CaseRecCalls),
    rec_goal_var_first_use(Goal, CaseRecCalls, Info, FoundFirstUse,
        CostBeforeSwitch, CaseCostAfter),
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, GoalId),
    get_coverage_before_det(Coverage, Before),
    RecProb = get_goal_attribute_det(Info ^ fui_rec_info ^ furi_rec_prob_array,
        GoalId),

    Weight = float(Before),
    Weights = [Weight | CasesWeights],
    FoundFirstUses = [FoundFirstUse | CasesFoundFirstUses],
    CostsAfter = [CaseCostAfter | CasesCostsAfter],
    RecProbs = [RecProb | CasesRecProbs].

:- pred rec_ite_var_first_use(goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    goal_rep(goal_id)::in, recursive_calls_list::in,
    var_first_use_static_info::in(var_first_use_static_info_rec),
    found_first_use::out, float::in, float::out) is det.

rec_ite_var_first_use(Cond, Then, Else, RecCalls, Info, FoundFirstUse,
        !CostSoFar) :-
    filter_recursive_call_sites(step_ite_cond, RecCalls, CondRecCalls),
    filter_recursive_call_sites(step_ite_then, RecCalls, ThenRecCalls),
    filter_recursive_call_sites(step_ite_else, RecCalls, ElseRecCalls),
    rec_goal_var_first_use(Cond, CondRecCalls, Info, CondFoundFirstUse,
        !CostSoFar),
    CostAfterCond = !.CostSoFar,
    rec_goal_var_first_use(Then, ThenRecCalls, Info, ThenFoundFirstUse,
        !.CostSoFar, CostAfterThen),
    rec_goal_var_first_use(Else, ElseRecCalls, Info, ElseFoundFirstUse,
        !.CostSoFar, CostAfterElse),

    % Determine goal weights.
    CoverageArray = Info ^ fui_coverage_array,
    Then = goal_rep(_, _, ThenId),
    Else = goal_rep(_, _, ElseId),
    ThenCoverage = get_goal_attribute_det(CoverageArray, ThenId),
    ElseCoverage = get_goal_attribute_det(CoverageArray, ElseId),
    get_coverage_before_det(ThenCoverage, BeforeThen),
    get_coverage_before_det(ElseCoverage, BeforeElse),
    RecProbArray = Info ^ fui_rec_info ^ furi_rec_prob_array,
    ThenRecProb = get_goal_attribute_det(RecProbArray, ThenId),
    ElseRecProb = get_goal_attribute_det(RecProbArray, ElseId),
    Weights0 = map(float, [BeforeThen, BeforeElse]),
    map_corresponding(
        adjust_weight_for_recursion(Info ^ fui_rec_info ^ furi_rec_case),
        [ThenRecProb, ElseRecProb], Weights0, Weights),

    % Get the weighted average of the costs for !:CostSoFar,
    weighted_average(Weights, [CostAfterThen, CostAfterElse], !:CostSoFar),

    % Determine FoundFirstUse
    (
        CondFoundFirstUse = found_first_use(_),
        FoundFirstUse = CondFoundFirstUse
    ;
        CondFoundFirstUse = have_not_found_first_use,

        ( if
            ThenFoundFirstUse = have_not_found_first_use,
            ElseFoundFirstUse = have_not_found_first_use
        then
            FoundFirstUse = have_not_found_first_use
        else
            VarUseType = Info ^ fui_var_use_opts ^ vuo_var_use_type,
            (
                VarUseType = var_use_consumption,
                Default = !.CostSoFar
            ;
                ( VarUseType = var_use_production
                ; VarUseType = var_use_other
                ),
                Default = CostAfterCond
            ),
            list.map(ffu_to_float(Default),
                [ThenFoundFirstUse, ElseFoundFirstUse], UseTimes),
            weighted_average(Weights, UseTimes, UseTime),
            FoundFirstUse = found_first_use(UseTime)
        )
    ).

:- pred adjust_weight_for_recursion(recursive_case::in,
    probability::in, float::in, float::out) is det.

adjust_weight_for_recursion(RecCase, RecProb, !Weight) :-
    (
        RecCase = recursive_case,
        Prob = RecProb
    ;
        RecCase = base_case,
        Prob = not_probability(RecProb)
    ),
    !:Weight = !.Weight * probability_to_float(Prob).

%---------------------------------------------------------------------------%

    % Give the probability that this goal leads to a recursion.
    %
    % Note that this does not compute whether this goal is on a recursive path,
    % so it is not sufficient on its own. See rec_goal_var_first_use.
    %
:- pred goal_rec_prob(goal_rep(goal_id)::in, recursive_calls_list::in,
    var_first_use_static_info::in, probability::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

goal_rec_prob(Goal, RecCalls, Info, Prob, !ProbArray) :-
    Goal = goal_rep(GoalExpr, _, GoalId),
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, GoalId),
    get_coverage_before_det(Coverage, Before),
    ( if Before = 0 then
        % Avoid a divide by zero and provide a short-cut.
        Prob = impossible
        % There is no need to update the array. The default value is already
        % impossible.
    else
        (
            GoalExpr = conj_rep(Conjs),
            conj_rec_prob(Conjs, 1, RecCalls, Info, Prob, !ProbArray)
        ;
            GoalExpr = disj_rep(Disjs),
            disj_rec_prob(Disjs, 1, RecCalls, Info, Prob, !ProbArray)
        ;
            GoalExpr = switch_rep(_, _, Cases),
            switch_rec_prob(Cases, Before, RecCalls, Info, Prob, !ProbArray)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_rec_prob(Cond, Then, Else, RecCalls, Info, Prob, !ProbArray)
        ;
            (
                GoalExpr = negation_rep(SubGoal),
                Step = step_neg
            ;
                GoalExpr = scope_rep(SubGoal, MaybeCut),
                Step = step_scope(MaybeCut)
            ),
            filter_recursive_call_sites(Step, RecCalls, SubGoalRecCalls),
            goal_rec_prob(SubGoal, SubGoalRecCalls, Info, Prob, !ProbArray)
        ;
            GoalExpr = atomic_goal_rep(_, _, _, _),
            (
                RecCalls = [],
                Prob = impossible
            ;
                RecCalls = [_ | _],
                Prob = certain
            )
        ),
        update_goal_attribute(GoalId, Prob, !ProbArray)
    ).

:- pred conj_rec_prob(list(goal_rep(goal_id))::in, int::in,
    recursive_calls_list::in, var_first_use_static_info::in, probability::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

conj_rec_prob([], _, _, _, impossible, !ProbArray).
conj_rec_prob([Conj | Conjs], ConjNum, RecCalls, Info, Prob, !ProbArray) :-
    ConjId = Conj ^ goal_annotation,
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, ConjId),
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( if Before = 0 then
        % This code is dead. Return the result without a division
        % to prevent a divide by zero.
        Prob = impossible
    else
        conj_rec_prob(Conjs, ConjNum + 1, RecCalls, Info, ConjsProb0,
            !ProbArray),
        SuccessProb = probable(float(After) / float(Before)),
        ConjsProb = and(SuccessProb, ConjsProb0),

        filter_recursive_call_sites(step_conj(ConjNum),
            RecCalls, ConjRecCalls),
        goal_rec_prob(Conj, ConjRecCalls, Info, ConjProb, !ProbArray),
        Prob = or(ConjProb, ConjsProb)
    ).

:- pred disj_rec_prob(list(goal_rep(goal_id))::in, int::in,
    recursive_calls_list::in, var_first_use_static_info::in, probability::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

disj_rec_prob([], _, _, _, impossible, !ProbArray).
disj_rec_prob([Disj | Disjs], DisjNum, RecCalls, Info, Prob, !ProbArray) :-
    DisjId = Disj ^ goal_annotation,
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, DisjId),
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( if Before = 0 then
        % As above, this code is dead.
        Prob = impossible
    else
        disj_rec_prob(Disjs, DisjNum + 1, RecCalls, Info, DisjsProb0,
            !ProbArray),
        % Assume that this disjunction is in a single solution context.
        FailureProb = probable(float(Before - After) / float(Before)),
        DisjsProb = and(FailureProb, DisjsProb0),

        filter_recursive_call_sites(step_disj(DisjNum),
            RecCalls, DisjRecCalls),
        goal_rec_prob(Disj, DisjRecCalls, Info, DisjProb, !ProbArray),
        Prob = or(DisjProb, DisjsProb)
    ).

:- pred switch_rec_prob(list(case_rep(goal_id))::in, int::in,
    recursive_calls_list::in, var_first_use_static_info::in, probability::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

switch_rec_prob(Cases, TotalCalls, RecCalls, Info, Prob, !ProbArray) :-
    switch_rec_prob_2(Cases, 1, TotalCalls, RecCalls, Info, Probs, Weights,
        !ProbArray),
    weighted_average(Weights, map(probability_to_float, Probs), ProbFloat),
    Prob = probable(ProbFloat).

:- pred switch_rec_prob_2(list(case_rep(goal_id))::in, int::in, int::in,
    recursive_calls_list::in, var_first_use_static_info::in,
    list(probability)::out, list(float)::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

switch_rec_prob_2([], _, _, _, _, [], [], !ProbArray).
switch_rec_prob_2([Case | Cases], CaseNum, TotalCalls, RecCalls, Info, Probs,
        Weights, !ProbArray) :-
    switch_rec_prob_2(Cases, CaseNum + 1, TotalCalls, RecCalls, Info,
        Probs0, Weights0, !ProbArray),

    Case = case_rep(_, _, Goal),
    filter_recursive_call_sites(
        step_switch(CaseNum, unknown_num_functors_in_type),
        RecCalls, CaseRecCalls),
    goal_rec_prob(Goal, CaseRecCalls, Info, Prob, !ProbArray),
    Goal = goal_rep(_, _, GoalId),
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, GoalId),
    get_coverage_before_det(Coverage, Before),
    Weight = float(Before) / float(TotalCalls),

    Probs = [Prob | Probs0],
    Weights = [Weight | Weights0].

:- pred ite_rec_prob(goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    goal_rep(goal_id)::in, recursive_calls_list::in,
    var_first_use_static_info::in, probability::out,
    goal_attr_array(probability)::gaa_di,
    goal_attr_array(probability)::gaa_uo) is det.

ite_rec_prob(Cond, Then, Else, RecCalls, Info, Prob, !ProbArray) :-
    filter_recursive_call_sites(step_ite_cond, RecCalls, CondRecCalls),
    filter_recursive_call_sites(step_ite_then, RecCalls, ThenRecCalls),
    filter_recursive_call_sites(step_ite_else, RecCalls, ElseRecCalls),
    goal_rec_prob(Cond, CondRecCalls, Info, CondProb, !ProbArray),
    goal_rec_prob(Then, ThenRecCalls, Info, ThenProb0, !ProbArray),
    goal_rec_prob(Else, ElseRecCalls, Info, ElseProb0, !ProbArray),
    CondId = Cond ^ goal_annotation,
    Coverage = get_goal_attribute_det(Info ^ fui_coverage_array, CondId),
    get_coverage_before_and_after_det(Coverage, Before, After),
    ThenCallProb = probable(float(After) / float(Before)),
    ElseCallProb = probable(float(Before - After) / float(Before)),
    ThenProb = and(ThenProb0, ThenCallProb),
    ElseProb = and(ElseProb0, ElseCallProb),
    ThenElseProb = or(ThenProb, ElseProb),
    Prob = or(CondProb, ThenElseProb).

%---------------------------------------------------------------------------%

:- pred goal_var_first_use_wrapper(clique_ptr::in, set(proc_dynamic_ptr)::in,
    containing_goal_map::in, goal_attr_array(coverage_info)::in,
    map(reverse_goal_path, cost_and_callees)::in,
    map(reverse_goal_path, cs_cost_csq)::in,
    recursion_type::in(recursion_type_known_costs), recursion_depth::in,
    goal_rep(goal_id)::in, float::in, var_rep::in,
    var_use_options::in, var_use_info::out) is det.

goal_var_first_use_wrapper(CliquePtr, CallStack, ContainingGoalMap,
        CoverageArray, CallSiteMap, RecursiveCallSiteMap, RT, CurDepth, Goal,
        ProcCost, Var, VarUseOptions, VarUseInfo) :-
    goal_var_first_use(rgp_nil, Goal,
        var_first_use_static_info(CliquePtr, CallSiteMap, RecursiveCallSiteMap,
            ContainingGoalMap, CoverageArray, Var, VarUseOptions, CallStack,
            RT, CurDepth, no_recursion_info),
        0.0, _Cost, FoundFirstUse),
    VarUseType = VarUseOptions ^ vuo_var_use_type,
    found_first_use_to_use_info(FoundFirstUse, ProcCost, VarUseType,
        VarUseInfo).

var_first_use(CliquePtr, CallSiteMap, RecursiveCallSiteMap, ContainingGoalMap,
        CoverageArray, RT, CurDepth, Goal, RevGoalPath, Cost, Var,
        VarUseOptions, VarUseInfo) :-
    goal_var_first_use(RevGoalPath, Goal,
        var_first_use_static_info(CliquePtr, CallSiteMap, RecursiveCallSiteMap,
            ContainingGoalMap, CoverageArray, Var, VarUseOptions, set.init, RT,
            CurDepth, no_recursion_info),
        0.0, _, FoundFirstUse),
    VarUseType = VarUseOptions ^ vuo_var_use_type,
    found_first_use_to_use_info(FoundFirstUse, Cost, VarUseType, VarUseInfo).

:- pred found_first_use_to_use_info(found_first_use::in, float::in,
    var_use_type::in, var_use_info::out) is det.

found_first_use_to_use_info(FoundFirstUse, Cost, VarUseType, VarUseInfo) :-
    (
        FoundFirstUse = found_first_use(CostUntilUse),
        VarUseInfo = var_use_info(CostUntilUse, Cost, VarUseType)
    ;
        FoundFirstUse = have_not_found_first_use,
        % If the first use has not been found then:
        %  a) for productions: they must have been produced, this is an error.
        %  b) For consumptions: the compiler will insert a wait so any calls
        %     following this one can assume that a wait has already been
        %     performed.
        (
            VarUseType = var_use_production,
            unexpected($pred,
                "goal did not produce a variable that it should have")
        ;
            VarUseType = var_use_consumption,
            VarUseInfo = var_use_info(Cost, Cost, VarUseType)
        ;
            VarUseType = var_use_other,
            pessimistic_var_use_info(VarUseType, Cost, VarUseInfo)
        )
    ).

%---------------------------------------------------------------------------%

:- pred intermodule_var_use_should_follow_csd(var_use_options::in,
    call_site_dynamic_ptr::in) is semidet.

intermodule_var_use_should_follow_csd(VarUseOptions, CSDPtr) :-
    FollowCall = VarUseOptions ^ vuo_intermodule_var_use,
    (
        FollowCall = follow_calls_into_module(Module),
        Deep = VarUseOptions ^ vuo_deep,
        deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
        PDPtr = CSD ^ csd_callee,
        intermodule_var_use_should_follow_pd_2(Deep, Module, PDPtr)
    ;
        FollowCall = follow_any_call
    ).

:- pred intermodule_var_use_should_follow_pd(var_use_options::in,
    proc_dynamic_ptr::in) is semidet.

intermodule_var_use_should_follow_pd(VarUseOptions, PDPtr) :-
    FollowCall = VarUseOptions ^ vuo_intermodule_var_use,
    (
        FollowCall = follow_calls_into_module(Module),
        Deep = VarUseOptions ^ vuo_deep,
        intermodule_var_use_should_follow_pd_2(Deep, Module, PDPtr)
    ;
        FollowCall = follow_any_call
    ).

:- pred intermodule_var_use_should_follow_pd_2(deep::in, string::in,
    proc_dynamic_ptr::in) is semidet.

intermodule_var_use_should_follow_pd_2(Deep, Module, PDPtr) :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    Label = PS ^ ps_id,
    ( Label = str_ordinary_proc_label(_, _, Module, _, _, _)
    ; Label = str_special_proc_label(_, _, Module, _, _, _)
    ).

%---------------------------------------------------------------------------%
:- end_module var_use_analysis.
%---------------------------------------------------------------------------%
