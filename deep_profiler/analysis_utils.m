%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: analysis_utils.m.
% Author: pbone.
%
% This module contains utility code that is useful for writing profile
% analyses.
%
%---------------------------------------------------------------------------%

:- module analysis_utils.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module profile.
:- import_module report.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%

    % Instead of using the clique report above to find proc dynamics for a
    % clique, use this as it is much faster.
    %
:- pred find_clique_first_and_other_procs(deep::in, clique_ptr::in,
    maybe(proc_dynamic_ptr)::out, list(proc_dynamic_ptr)::out) is det.

%---------------------------------------------------------------------------%

    % Lookup a procedure representation from the deep structure.
    %
    % (Perhaps this should be a new report).
    %
:- pred deep_get_maybe_procrep(deep::in, proc_static_ptr::in,
    maybe_error(proc_rep)::out) is det.

%---------------------------------------------------------------------------%

:- type cost_and_callees == cost_and_callees(callee).

:- type cost_and_callees(Callee)
    --->    cost_and_callees(
                cac_cost            :: cs_cost_csq,
                cac_exits           :: int,
                cac_callees         :: set(Callee),
                cac_call_site_is_ho :: higher_order
            ).

:- type callee
    --->    callee(
                c_clique            :: clique_ptr,
                c_csd               :: call_site_dynamic_ptr
            ).

:- type higher_order
    --->    first_order_call
    ;       higher_order_call.

:- pred build_static_call_site_cost_and_callee_map(deep::in,
    call_site_static_ptr::in,
    map(reverse_goal_path, cost_and_callees(proc_static_ptr))::in,
    map(reverse_goal_path, cost_and_callees(proc_static_ptr))::out) is det.

:- pred build_dynamic_call_site_cost_and_callee_map(deep::in,
    pair(call_site_static_ptr, call_site_array_slot)::in,
    map(reverse_goal_path, cost_and_callees)::in,
    map(reverse_goal_path, cost_and_callees)::out) is det.

%---------------------------------------------------------------------------%

    % Estimate the cost of the recursive calls under the assumption that
    % current call to this procedure had a particular cost.
    %
:- pred build_recursive_call_site_cost_map(deep, clique_ptr,
    proc_dynamic_ptr, recursion_type, maybe(recursion_depth),
    maybe_error(map(reverse_goal_path, cs_cost_csq))).
:- mode build_recursive_call_site_cost_map(in, in, in,
    in(recursion_type_known_costs), in(maybe_yes(ground)),
    out(maybe_error_ok(ground))) is det.
:- mode build_recursive_call_site_cost_map(in, in, in, in, in, out) is det.

%---------------------------------------------------------------------------%

:- pred proc_dynamic_paired_call_site_slots(deep::in, proc_dynamic_ptr::in,
    assoc_list(call_site_static_ptr, call_site_array_slot)::out) is det.

%---------------------------------------------------------------------------%

:- pred cost_and_callees_is_recursive(clique_ptr::in, cost_and_callees::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module program_representation_utils.

:- import_module array.
:- import_module bool.
:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module message.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstPDPtr,
        OtherPDPtrs) :-
    deep_lookup_clique_members(Deep, CliquePtr, PDPtrs),
    deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
    ( if valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) then
        deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
        FirstPDPtr = EntryCSD ^ csd_callee,
        MaybeFirstPDPtr = yes(FirstPDPtr),
        list.negated_filter(unify(FirstPDPtr), PDPtrs, OtherPDPtrs)
    else
        MaybeFirstPDPtr = no,
        OtherPDPtrs = PDPtrs
    ).

%---------------------------------------------------------------------------%

deep_get_maybe_procrep(Deep, PSPtr, MaybeProcRep) :-
    deep_get_maybe_progrep(Deep, MaybeProgRep),
    (
        MaybeProgRep = ok(ProgRep),
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        ProcLabel = PS ^ ps_id,
        ( if progrep_search_proc(ProgRep, ProcLabel, ProcRep) then
            MaybeProcRep = ok(ProcRep)
        else
            MaybeProcRep = error("Cannot find procedure representation")
        )
    ;
        MaybeProgRep = error(Error),
        MaybeProcRep = error(Error)
    ).

%---------------------------------------------------------------------------%

build_static_call_site_cost_and_callee_map(Deep, CSSPtr, !CallSitesMap) :-
    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    deep_lookup_css_own(Deep, CSSPtr, Own),
    deep_lookup_css_desc(Deep, CSSPtr, Inherit),
    CostCsq = build_cs_cost_csq(calls(Own),
        float(callseqs(Own) + inherit_callseqs(Inherit))),
    Exits = exits(Own),
    KindAndCallee = CSS ^ css_kind,
    call_site_kind_to_higher_order(KindAndCallee, HigherOrder),
    call_site_kind_to_maybe_callee(KindAndCallee, MaybeCallee),
    (
        MaybeCallee = yes(Callee),
        Callees = set.make_singleton_set(Callee)
    ;
        MaybeCallee = no,
        Callees = set.init
    ),
    CostAndCallees = cost_and_callees(CostCsq, Exits, Callees, HigherOrder),
    RevGoalPath = CSS ^ css_goal_path,
    map.det_insert(RevGoalPath, CostAndCallees, !CallSitesMap).

%---------------------------------------------------------------------------%

build_dynamic_call_site_cost_and_callee_map(Deep, CSSPtr - Slot,
        !CallSitesMap) :-
    (
        Slot = slot_normal(CSDPtr),
        ( if valid_call_site_dynamic_ptr(Deep, CSDPtr) then
            call_site_dynamic_get_callee_and_costs(Deep, CSDPtr, Callee,
                Own, Inherit),
            CostCsq = build_cs_cost_csq(calls(Own),
                float(callseqs(Own) + inherit_callseqs(Inherit))),
            Callees = [Callee],
            Exits = exits(Own)
        else
            CostCsq = build_cs_cost_csq(0, 0.0),
            Callees = [],
            Exits = 0
        )
    ;
        Slot = slot_multi(_, CSDPtrsArray),
        to_list(CSDPtrsArray, CSDPtrs),
        map3(call_site_dynamic_get_callee_and_costs(Deep), CSDPtrs,
            Callees, Owns, Inherits),
        Own = sum_own_infos(Owns),
        Inherit = sum_inherit_infos(Inherits),
        CostCsq = build_cs_cost_csq(calls(Own),
            float(callseqs(Own) + inherit_callseqs(Inherit))),
        Exits = exits(Own)
    ),
    CostAndCallees = cost_and_callees(CostCsq, Exits,
        set.list_to_set(Callees), HigherOrder),
    lookup_call_site_statics(Deep ^ call_site_statics, CSSPtr, CSS),
    call_site_kind_to_higher_order(CSS ^ css_kind, HigherOrder),
    RevGoalPath = CSS ^ css_goal_path,
    map.det_insert(RevGoalPath, CostAndCallees, !CallSitesMap).

:- pred call_site_dynamic_get_callee_and_costs(deep::in,
    call_site_dynamic_ptr::in, callee::out, own_prof_info::out,
    inherit_prof_info::out) is det.

call_site_dynamic_get_callee_and_costs(Deep, CSDPtr,
        callee(CalleeCliquePtr, CSDPtr), Own, Inherit) :-
    lookup_call_site_dynamics(Deep ^ call_site_dynamics, CSDPtr, CSD),
    lookup_csd_desc(Deep ^ csd_desc, CSDPtr, Inherit),
    PDPtr = CSD ^ csd_callee,
    lookup_clique_index(Deep ^ clique_index, PDPtr, CalleeCliquePtr),
    Own = CSD ^ csd_own_prof.

:- pred call_site_kind_to_higher_order(call_site_kind_and_callee::in,
    higher_order::out) is det.

call_site_kind_to_higher_order(CallSiteKind, HigherOrder) :-
    (
        ( CallSiteKind = normal_call_and_callee(_, _)
        ; CallSiteKind = callback_and_no_callee
        ),
        HigherOrder = first_order_call
    ;
        ( CallSiteKind = special_call_and_no_callee
        ; CallSiteKind = higher_order_call_and_no_callee
        ; CallSiteKind = method_call_and_no_callee
        ),
        HigherOrder = higher_order_call
    ).

:- pred call_site_kind_to_maybe_callee(call_site_kind_and_callee::in,
    maybe(proc_static_ptr)::out) is det.

call_site_kind_to_maybe_callee(normal_call_and_callee(Callee, _), yes(Callee)).
call_site_kind_to_maybe_callee(special_call_and_no_callee, no).
call_site_kind_to_maybe_callee(higher_order_call_and_no_callee, no).
call_site_kind_to_maybe_callee(method_call_and_no_callee, no).
call_site_kind_to_maybe_callee(callback_and_no_callee, no).

%---------------------------------------------------------------------------%

build_recursive_call_site_cost_map(Deep, CliquePtr, PDPtr, RecursionType,
        MaybeDepth, MaybeRecursiveCallSiteCostMap) :-
    (
        RecursionType = rt_not_recursive,
        MaybeRecursiveCallSiteCostMap = ok(map.init)
    ;
        RecursionType = rt_single(_, _, MaxDepth, _AvgRecCost, CostFn),
        (
            MaybeDepth = yes(Depth0),
            ( if recursion_depth_is_base_case(Depth0) then
                MaybeRecursiveCallSiteCostMap = ok(map.init)
            else
                % Descend once to move to the depth of the recursive callees.
                get_recursive_calls_and_counts(Deep, CliquePtr, PDPtr,
                    CallCountsMap),
                RecursiveCallSiteCostMap = map_values_only(
                    ( func(Count) =
                            build_cs_cost_csq_percall(float(Count),
                                CostFn(DepthI)) :-
                        DepthI = round_to_int(MaxDepth / float(Count))
                    ),
                    CallCountsMap),
                MaybeRecursiveCallSiteCostMap = ok(RecursiveCallSiteCostMap),

                trace [compile_time(flag("debug_recursive_call_costs")),
                        io(!IO)] (
                    format_recursive_call_site_cost_map(
                        RecursiveCallSiteCostMap, PrettyCostMapCord),
                    PrettyCostMap = append_list(cord.list(PrettyCostMapCord)),
                    io.output_stream(OutputStream, !IO),
                    io.format(OutputStream,
                        "D: In clique %s recursive call site cost map is:" ++
                            "\n%s\n",
                        [s(string(CliquePtr)), s(PrettyCostMap)], !IO),
                    io.flush_output(OutputStream, !IO)
                )
            )
        ;
            MaybeDepth = no,
            unexpected($pred, "Expected valid depth for known recursion type")
        )
    ;
        ( RecursionType = rt_divide_and_conquer(_, _)
        ; RecursionType = rt_mutual_recursion(_)
        ; RecursionType = rt_other(_)
        ; RecursionType = rt_errors(_)
        ),
        (
            ( RecursionType = rt_divide_and_conquer(_, _)
            ; RecursionType = rt_mutual_recursion(_)
            ),
            Error = "No average recursion depth for this recursion type"
        ;
            RecursionType = rt_other(_),
            Error = "Could not detect recursion type"
        ;
            RecursionType = rt_errors(Errors),
            Error = join_list("; and ", Errors)
        ),
        MaybeRecursiveCallSiteCostMap = error(Error)
    ).

:- pred get_recursive_calls_and_counts(deep::in, clique_ptr::in,
    proc_dynamic_ptr::in, map(reverse_goal_path, int)::out) is det.

get_recursive_calls_and_counts(Deep, CliquePtr, PDPtr, CallCountsMap) :-
    proc_dynamic_paired_call_site_slots(Deep, PDPtr, SiteSlots),
    foldl(build_recursive_call_site_counts_map(Deep, CliquePtr), SiteSlots,
        map.init, CallCountsMap).

:- pred build_recursive_call_site_counts_map(deep::in, clique_ptr::in,
    pair(call_site_static_ptr, call_site_array_slot)::in,
    map(reverse_goal_path, int)::in, map(reverse_goal_path, int)::out) is det.

build_recursive_call_site_counts_map(Deep, CliquePtr, CSSPtr - CSDSlot,
        !Map) :-
    (
        CSDSlot = slot_normal(CSDPtr),
        ( if valid_call_site_dynamic_ptr(Deep, CSDPtr) then
            call_site_dynamic_get_count_and_callee(Deep, CSDPtr, Count,
                MaybeCallee),
            ( if maybe_equals_or_is_no(CliquePtr, MaybeCallee) then
                Recursive = yes
            else
                Recursive = no
            )
        else
            Recursive = no,
            Count = 0
        )
    ;
        CSDSlot = slot_multi(_, CSDPtrs),
        map2(call_site_dynamic_get_count_and_callee(Deep), to_list(CSDPtrs),
            Counts, MaybeCallees),
        Count = foldl(plus, Counts, 0),
        ( if
            member(MaybeCallee, MaybeCallees),
            maybe_equals_or_is_no(CliquePtr, MaybeCallee)
        then
            Recursive = yes
        else
            Recursive = no
        )
    ),
    (
        Recursive = yes,
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        RevGoalPath = CSS ^ css_goal_path,
        map.det_insert(RevGoalPath, Count, !Map)
    ;
        Recursive = no
    ).

:- pred maybe_equals_or_is_no(T::in, maybe(T)::in) is semidet.

maybe_equals_or_is_no(_, no).
maybe_equals_or_is_no(X, yes(X)).

:- pred call_site_dynamic_get_count_and_callee(deep::in,
    call_site_dynamic_ptr::in, int::out, maybe(clique_ptr)::out) is det.

call_site_dynamic_get_count_and_callee(Deep, CSDPtr, Count, MaybeCallee) :-
    ( if valid_call_site_dynamic_ptr(Deep, CSDPtr) then
        deep_lookup_csd_own(Deep, CSDPtr, Own),
        Count = calls(Own),
        deep_lookup_clique_maybe_child(Deep, CSDPtr, MaybeCallee)
    else
        Count = 0,
        MaybeCallee = no
    ).

:- pred format_recursive_call_site_cost_map(
    map(reverse_goal_path, cs_cost_csq)::in, cord(string)::out) is det.

format_recursive_call_site_cost_map(Map, Result) :-
    map.foldl(format_recursive_call_site_cost, Map, cord.empty, Result).

:- pred format_recursive_call_site_cost(reverse_goal_path::in, cs_cost_csq::in,
    cord(string)::in, cord(string)::out) is det.

format_recursive_call_site_cost(RevGoalPath, Cost, !Result) :-
    !:Result = cord.snoc(!.Result ++ indent(1), String),
    String = format("%s -> Percall cost: %f Calls: %f\n",
        [s(GoalPathString), f(PerCallCost), f(Calls)]),
    GoalPathString = rev_goal_path_to_string(RevGoalPath),
    PerCallCost = cs_cost_get_percall(Cost),
    Calls = cs_cost_get_calls(Cost).

%---------------------------------------------------------------------------%

proc_dynamic_paired_call_site_slots(Deep, PDPtr, PairedSlots) :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    CSDArray = PD ^ pd_sites,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    CSSArray = PS ^ ps_sites,
    array.to_list(CSDArray, CSDSlots),
    array.to_list(CSSArray, CSSSlots),
    assoc_list.from_corresponding_lists(CSSSlots, CSDSlots, PairedSlots).

%---------------------------------------------------------------------------%

cost_and_callees_is_recursive(ParentCliquePtr, CostAndCallees) :-
    Callees = CostAndCallees ^ cac_callees,
    member(Callee, Callees),
    ParentCliquePtr = Callee ^ c_clique.

%---------------------------------------------------------------------------%
