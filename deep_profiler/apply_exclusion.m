%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: apply_exclusion.m.
%
% This module contains the predicates required to implement contour exclusion.
%
%---------------------------------------------------------------------------%

:- module apply_exclusion.
:- interface.

:- import_module exclude.
:- import_module measurements.
:- import_module profile.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.

:- func group_csds_by_call_site(deep, list(pair(call_site_dynamic_ptr))) =
    assoc_list(call_site_static_ptr, list(call_site_dynamic_ptr)).

:- func group_csds_by_procedure(deep, list(pair(call_site_dynamic_ptr))) =
    assoc_list(proc_static_ptr, list(call_site_dynamic_ptr)).

:- func group_csds_by_module(deep, list(pair(call_site_dynamic_ptr))) =
    assoc_list(string, list(call_site_dynamic_ptr)).

:- func group_csds_by_clique(deep, list(pair(call_site_dynamic_ptr))) =
    assoc_list(clique_ptr, list(call_site_dynamic_ptr)).

:- pred compute_parent_csd_prof_info(deep::in, proc_static_ptr::in,
    list(call_site_dynamic_ptr)::in,
    own_prof_info::out, inherit_prof_info::out) is det.

:- func pair_self(call_site_dynamic_ptr) = pair(call_site_dynamic_ptr).

:- func pair_contour(deep, excluded_modules, call_site_dynamic_ptr)
    = pair(call_site_dynamic_ptr).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.

group_csds_by_call_site(Deep, GroupCostCSDPtrs) = Groups :-
    GroupMap = list.foldl(accumulate_csds_by_call_site(Deep),
        GroupCostCSDPtrs, map.init),
    map.to_assoc_list(GroupMap, Groups).

group_csds_by_procedure(Deep, GroupCostCSDPtrs) = Groups :-
    GroupMap = list.foldl(accumulate_csds_by_procedure(Deep),
        GroupCostCSDPtrs, map.init),
    map.to_assoc_list(GroupMap, Groups).

group_csds_by_module(Deep, GroupCostCSDPtrs) = Groups :-
    GroupMap = list.foldl(accumulate_csds_by_module(Deep),
        GroupCostCSDPtrs, map.init),
    map.to_assoc_list(GroupMap, Groups).

group_csds_by_clique(Deep, GroupCostCSDPtrs) = Groups :-
    GroupMap = list.foldl(accumulate_csds_by_clique(Deep),
        GroupCostCSDPtrs, map.init),
    map.to_assoc_list(GroupMap, Groups).

%---------------------------------------------------------------------------%

:- func accumulate_csds_by_call_site(deep, pair(call_site_dynamic_ptr),
    map(call_site_static_ptr, list(call_site_dynamic_ptr))) =
    map(call_site_static_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_call_site(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
    deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
    ( if map.search(Map0, GroupCSSPtr, CostCSDPtrs0) then
        map.det_update(GroupCSSPtr, [CostCSDPtr | CostCSDPtrs0], Map0, Map)
    else
        map.det_insert(GroupCSSPtr, [CostCSDPtr], Map0, Map)
    ).

:- func accumulate_csds_by_procedure(deep, pair(call_site_dynamic_ptr),
    map(proc_static_ptr, list(call_site_dynamic_ptr))) =
    map(proc_static_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_procedure(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
    deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
    deep_lookup_call_site_statics(Deep, GroupCSSPtr, GroupCSS),
    GroupPSPtr = GroupCSS ^ css_container,
    ( if map.search(Map0, GroupPSPtr, CostCSDPtrs0) then
        map.det_update(GroupPSPtr, [CostCSDPtr | CostCSDPtrs0], Map0, Map)
    else
        map.det_insert(GroupPSPtr, [CostCSDPtr], Map0, Map)
    ).

:- func accumulate_csds_by_module(deep, pair(call_site_dynamic_ptr),
    map(string, list(call_site_dynamic_ptr))) =
    map(string, list(call_site_dynamic_ptr)).

accumulate_csds_by_module(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
    deep_lookup_call_site_static_map(Deep, GroupCSDPtr, GroupCSSPtr),
    deep_lookup_call_site_statics(Deep, GroupCSSPtr, GroupCSS),
    GroupPSPtr = GroupCSS ^ css_container,
    deep_lookup_proc_statics(Deep, GroupPSPtr, GroupPS),
    GroupModuleName = GroupPS ^ ps_decl_module,
    ( if map.search(Map0, GroupModuleName, CostCSDPtrs0) then
        map.det_update(GroupModuleName, [CostCSDPtr | CostCSDPtrs0], Map0, Map)
    else
        map.det_insert(GroupModuleName, [CostCSDPtr], Map0, Map)
    ).

:- func accumulate_csds_by_clique(deep, pair(call_site_dynamic_ptr),
    map(clique_ptr, list(call_site_dynamic_ptr))) =
    map(clique_ptr, list(call_site_dynamic_ptr)).

accumulate_csds_by_clique(Deep, GroupCSDPtr - CostCSDPtr, Map0) = Map :-
    deep_lookup_call_site_dynamics(Deep, GroupCSDPtr, GroupCSD),
    CallerPDPtr = GroupCSD ^ csd_caller,
    deep_lookup_clique_index(Deep, CallerPDPtr, CliquePtr),
    ( if map.search(Map0, CliquePtr, CostCSDPtrs0) then
        map.det_update(CliquePtr, [CostCSDPtr | CostCSDPtrs0], Map0, Map)
    else
        map.det_insert(CliquePtr, [CostCSDPtr], Map0, Map)
    ).

%---------------------------------------------------------------------------%

compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc) :-
    list.foldl2(accumulate_parent_csd_prof_info(Deep, CalleePSPtr), CSDPtrs,
        zero_own_prof_info, Own, zero_inherit_prof_info, Desc).

:- pred accumulate_parent_csd_prof_info(deep::in, proc_static_ptr::in,
    call_site_dynamic_ptr::in,
    own_prof_info::in, own_prof_info::out,
    inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_parent_csd_prof_info(Deep, CallerPSPtr, CSDPtr,
        Own0, Own, Desc0, Desc) :-
    deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
    ( if CSD ^ csd_callee = CSD ^ csd_caller then
        % We want to sum only cross-clique callers.
        Own = Own0,
        Desc = Desc0
    else
        deep_lookup_csd_own(Deep, CSDPtr, CSDOwn),
        deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
        add_own_to_own(Own0, CSDOwn) = Own,
        add_inherit_to_inherit(Desc0, CSDDesc) = Desc1,

        deep_lookup_clique_index(Deep, CSD ^ csd_callee, CalleeCliquePtr),
        deep_lookup_clique_members(Deep, CalleeCliquePtr, CalleeCliquePDPtrs),
        list.foldl(compensate_using_comp_table(Deep, CallerPSPtr),
            CalleeCliquePDPtrs, Desc1, Desc)
    ).

:- pred compensate_using_comp_table(deep::in, proc_static_ptr::in,
    proc_dynamic_ptr::in, inherit_prof_info::in, inherit_prof_info::out)
    is det.

compensate_using_comp_table(Deep, CallerPSPtr, PDPtr, Desc0, Desc) :-
    deep_lookup_pd_comp_table(Deep, PDPtr, CompTableArray),
    ( if map.search(CompTableArray, CallerPSPtr, InnerTotal) then
        Desc = subtract_inherit_from_inherit(InnerTotal, Desc0)
    else
        Desc = Desc0
    ).

%---------------------------------------------------------------------------%

pair_self(CSDPtr) = CSDPtr - CSDPtr.

pair_contour(Deep, ExcludeSpec, CSDPtr) =
    apply_contour_exclusion(Deep, ExcludeSpec, CSDPtr) - CSDPtr.

%---------------------------------------------------------------------------%

