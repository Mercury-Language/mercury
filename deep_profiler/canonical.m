%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: canonical.m.
% Authors: conway, zs.
%
% This module contains code for recursively merging sets of ProcDynamic and
% CallSiteDynamic nodes.
%
%---------------------------------------------------------------------------%

:- module canonical.
:- interface.

:- import_module profile.

:- pred canonicalize_cliques(initial_deep::in, initial_deep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module callgraph.
:- import_module measurements.

:- import_module array.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- type merge_info
    --->    merge_info(
                merge_clique_members :: array(list(proc_dynamic_ptr)),
                merge_clique_index   :: array(clique_ptr)
            ).

:- type redirect
    --->    redirect(
                csd_redirect        :: array(call_site_dynamic_ptr),
                                    % index: call_site_dynamic_ptr
                pd_redirect         :: array(proc_dynamic_ptr)
                                    % index: proc_dynamic_ptr
            ).

canonicalize_cliques(!InitDeep) :-
    MaxCSDs = array.max(!.InitDeep ^ init_call_site_dynamics),
    MaxPDs = array.max(!.InitDeep ^ init_proc_dynamics),
    NumCSDs = MaxCSDs + 1,
    NumPDs = MaxPDs + 1,

    find_cliques(!.InitDeep, CliqueList),
    make_clique_indexes(NumPDs, CliqueList, Cliques, CliqueIndex),
    MergeInfo = merge_info(Cliques, CliqueIndex),

    CSDRedirect0 = array.init(NumCSDs, call_site_dynamic_ptr(0)),
    PDRedirect0 = array.init(NumPDs, proc_dynamic_ptr(0)),
    Redirect0 = redirect(CSDRedirect0, PDRedirect0),
    merge_cliques(CliqueList, MergeInfo, !InitDeep, Redirect0, Redirect1),
    compact_dynamics(Redirect1, NumCSDs, NumPDs, !InitDeep).

:- pred merge_cliques(list(list(proc_dynamic_ptr))::in,
    merge_info::in, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_cliques([], _, !InitDeep, !Redirect).
merge_cliques([Clique | Cliques], MergeInfo, !InitDeep, !Redirect) :-
    merge_clique(Clique, MergeInfo, !InitDeep, !Redirect),
    merge_cliques(Cliques, MergeInfo, !InitDeep, !Redirect).

:- pred merge_clique(list(proc_dynamic_ptr)::in,
    merge_info::in, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_clique(CliquePDs0, MergeInfo, !InitDeep, !Redirect) :-
    (
        CliquePDs0 = []
    ;
        CliquePDs0 = [_]
    ;
        CliquePDs0 = [_, _ | _],
        map.init(ProcMap0),
        list.foldl(cluster_pds_by_ps(!.InitDeep), CliquePDs0,
            ProcMap0, ProcMap1),
        map.values(ProcMap1, PDsList1),
        list.filter(two_or_more, PDsList1, ToMergePDsList1),
        (
            ToMergePDsList1 = [_ | _],
            complete_clique(!.InitDeep, !.Redirect, ProcMap1, ProcMap, Clique),
            map.values(ProcMap, PDsList),
            list.filter(two_or_more, PDsList, ToMergePDsList),
            list.foldl2(merge_proc_dynamics_ignore_chosen(MergeInfo, Clique),
                ToMergePDsList, !InitDeep, !Redirect)
        ;
            ToMergePDsList1 = []
        )
    ).

:- pred insert_pds(list(T)::in, set(T)::in, set(T)::out) is det.

insert_pds(List, !Set) :-
    set.insert_list(List, !Set).

    % find set of proc_statics in the CliquePDs
    % for all (first order) calls in CliquePDs, if call is to a procedure
    %   that CliquePDs contains a call to, add its PD to the set
    %
:- pred complete_clique(initial_deep::in, redirect::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out,
    set(proc_dynamic_ptr)::out) is det.

complete_clique(InitDeep, Redirect, !ProcMap, Clique) :-
    map.values(!.ProcMap, PDsList0),
    list.foldl(insert_pds, PDsList0, set.init, Clique0),
    complete_clique_pass(InitDeep, Redirect, Clique0, !ProcMap, no, AddedPD),
    (
        AddedPD = yes,
        disable_warning [suspicious_recursion] (
            complete_clique(InitDeep, Redirect, !ProcMap, Clique)
        )
    ;
        AddedPD = no,
        Clique = Clique0
    ).

:- pred complete_clique_pass(initial_deep::in, redirect::in,
    set(proc_dynamic_ptr)::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out,
    bool::in, bool::out) is det.

complete_clique_pass(InitDeep, _Redirect, Clique, !ProcMap, !AddedPD) :-
    map.to_assoc_list(!.ProcMap, PSPDs0),
    list.foldl2(complete_clique_ps(InitDeep, Clique),
        PSPDs0, !ProcMap, !AddedPD).

:- pred complete_clique_ps(initial_deep::in,
    set(proc_dynamic_ptr)::in,
    pair(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out,
    bool::in, bool::out) is det.

complete_clique_ps(InitDeep, Clique, PSPtr - PDPtrs, !ProcMap, !AddedPD) :-
    ( if PDPtrs = [_, _ | _] then
        lookup_proc_statics(InitDeep ^ init_proc_statics, PSPtr, PS),
        list.map(lookup_pd_site(InitDeep), PDPtrs, PDSites),
        complete_clique_slots(array.max(PS ^ ps_sites), InitDeep,
            Clique, PS ^ ps_sites, PDSites, !ProcMap, !AddedPD)
    else
        true
    ).

:- pred lookup_pd_site(initial_deep::in, proc_dynamic_ptr::in,
    array(call_site_array_slot)::out) is det.

lookup_pd_site(InitDeep, PDPtr, Sites) :-
    lookup_proc_dynamics(InitDeep ^ init_proc_dynamics, PDPtr, PD),
    Sites = PD ^ pd_sites.

:- pred complete_clique_slots(int::in, initial_deep::in,
    set(proc_dynamic_ptr)::in, array(call_site_static_ptr)::in,
    list(array(call_site_array_slot))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out,
    bool::in, bool::out) is det.

complete_clique_slots(SlotNum, InitDeep, Clique, PSSites, PDSites,
        !ProcMap, !AddedPD) :-
    ( if SlotNum >= 0 then
        array.lookup(PSSites, SlotNum, CSSPtr),
        lookup_call_site_statics(InitDeep ^ init_call_site_statics,
            CSSPtr, CSS),
        ( if CSS ^ css_kind = normal_call_and_callee(_, _) then
            lookup_normal_sites(PDSites, SlotNum, CSDPtrs)
        else
            lookup_multi_sites(PDSites, SlotNum, CSDPtrLists),
            list.condense(CSDPtrLists, CSDPtrs)
        ),
        list.filter(valid_call_site_dynamic_ptr_raw(
            InitDeep ^ init_call_site_dynamics), CSDPtrs,
            ValidCSDPtrs),
        list.map(extract_csdptr_callee(InitDeep), ValidCSDPtrs, CalleePDPtrs),
        CalleePDPtrSet = set.list_to_set(CalleePDPtrs),
        set.intersect(CalleePDPtrSet, Clique, Common),
        ( if set.is_empty(Common) then
            true
        else
            set.difference(CalleePDPtrSet, Clique, NewMembers),
            ( if set.is_empty(NewMembers) then
                !:AddedPD = no
            else
                set.to_sorted_list(NewMembers, NewMemberList),
                list.foldl(cluster_pds_by_ps(InitDeep), NewMemberList,
                    !ProcMap),
                !:AddedPD = yes
            )
        ),
        complete_clique_slots(SlotNum - 1, InitDeep, Clique,
            PSSites, PDSites, !ProcMap, !AddedPD)
    else
        true
    ).

:- pred merge_proc_dynamics_ignore_chosen(merge_info::in,
    set(proc_dynamic_ptr)::in, list(proc_dynamic_ptr)::in,
    initial_deep::in, initial_deep::out, redirect::in, redirect::out)
    is det.

merge_proc_dynamics_ignore_chosen(MergeInfo, Clique, CandidatePDPtrs,
        !InitDeep, !Redirect) :-
    merge_proc_dynamics(MergeInfo, Clique, CandidatePDPtrs, _ChosenPDPtr,
        !InitDeep, !Redirect).

:- pred merge_proc_dynamics(merge_info::in, set(proc_dynamic_ptr)::in,
    list(proc_dynamic_ptr)::in, proc_dynamic_ptr::out,
    initial_deep::in, initial_deep::out, redirect::in, redirect::out)
    is det.

merge_proc_dynamics(MergeInfo, Clique, CandidatePDPtrs, ChosenPDPtr,
        !InitDeep, !Redirect) :-
    ProcDynamics0 = !.InitDeep ^ init_proc_dynamics,
    list.filter(valid_proc_dynamic_ptr_raw(ProcDynamics0),
        CandidatePDPtrs, ValidPDPtrs, InvalidPDPtrs),
    require(unify(InvalidPDPtrs, []), "merge_proc_dynamics: invalid pdptrs"),
    (
        ValidPDPtrs = [PrimePDPtr | RestPDPtrs],
        record_pd_redirect(RestPDPtrs, PrimePDPtr, !Redirect),
        lookup_proc_dynamics(ProcDynamics0, PrimePDPtr, PrimePD0),
        list.map(lookup_proc_dynamics(ProcDynamics0), RestPDPtrs, RestPDs),
        list.map(extract_pd_sites, RestPDs, RestSites),
        PrimeSites0 = PrimePD0 ^ pd_sites,
        array.max(PrimeSites0, MaxSiteNum),
        merge_proc_dynamic_slots(MergeInfo, MaxSiteNum, Clique,
            PrimePDPtr, u(PrimeSites0), RestSites, PrimeSites,
            !InitDeep, !Redirect),
        PrimePD = PrimePD0 ^ pd_sites := PrimeSites,
        ProcDynamics1 = !.InitDeep ^ init_proc_dynamics,
        update_proc_dynamics(PrimePDPtr, PrimePD, u(ProcDynamics1),
            ProcDynamics),
        !InitDeep ^ init_proc_dynamics := ProcDynamics,
        ChosenPDPtr = PrimePDPtr
    ;
        ValidPDPtrs = [],
        % This could happen when merging the callees of CSDs representing
        % special calls, but only before we added callcode to the
        % unify/compare routines of builtin types.
        % ChosenPDPtr = proc_dynamic_ptr(0),
        unexpected($pred, "no valid pdptrs")
    ).

:- pred merge_proc_dynamic_slots(merge_info::in, int::in,
    set(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
    array(call_site_array_slot)::array_di,
    list(array(call_site_array_slot))::in,
    array(call_site_array_slot)::array_uo,
    initial_deep::in, initial_deep::out, redirect::in, redirect::out)
    is det.

merge_proc_dynamic_slots(MergeInfo, SlotNum, Clique, PrimePDPtr,
        !.PrimeSiteArray, RestSiteArrays, !:PrimeSiteArray,
        !InitDeep, !Redirect) :-
    ( if SlotNum >= 0 then
        array.lookup(!.PrimeSiteArray, SlotNum, PrimeSite0),
        (
            PrimeSite0 = slot_normal(PrimeCSDPtr0),
            merge_proc_dynamic_normal_slot(MergeInfo, SlotNum,
                Clique, PrimePDPtr, PrimeCSDPtr0,
                RestSiteArrays, PrimeCSDPtr,
                !InitDeep, !Redirect),
            array.set(SlotNum, slot_normal(PrimeCSDPtr), !PrimeSiteArray)
        ;
            PrimeSite0 = slot_multi(IsZeroed, PrimeCSDPtrArray0),
            array.to_list(PrimeCSDPtrArray0, PrimeCSDPtrList0),
            merge_proc_dynamic_multi_slot(MergeInfo, SlotNum,
                Clique, PrimePDPtr, PrimeCSDPtrList0,
                RestSiteArrays, PrimeCSDPtrList,
                !InitDeep, !Redirect),
            PrimeCSDPtrArray = array(PrimeCSDPtrList),
            array.set(SlotNum,
                slot_multi(IsZeroed, PrimeCSDPtrArray),
                !PrimeSiteArray)
        ),
        merge_proc_dynamic_slots(MergeInfo, SlotNum - 1, Clique,
            PrimePDPtr, !.PrimeSiteArray, RestSiteArrays,
            !:PrimeSiteArray, !InitDeep, !Redirect)
    else
        true
    ).

:- pred merge_proc_dynamic_normal_slot(merge_info::in, int::in,
    set(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
    call_site_dynamic_ptr::in, list(array(call_site_array_slot))::in,
    call_site_dynamic_ptr::out, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_proc_dynamic_normal_slot(MergeInfo, SlotNum, Clique,
        PrimePDPtr, PrimeCSDPtr0, RestSiteArrays, PrimeCSDPtr,
        !InitDeep, !Redirect) :-
    lookup_normal_sites(RestSiteArrays, SlotNum, RestCSDPtrs),
    merge_call_site_dynamics(MergeInfo, Clique, PrimePDPtr,
        [PrimeCSDPtr0 | RestCSDPtrs], PrimeCSDPtr,
        !InitDeep, !Redirect).

:- pred accumulate_csd_owns(call_site_dynamic::in,
    own_prof_info::in, own_prof_info::out) is det.

accumulate_csd_owns(CSD, Own0, Own) :-
    Own = add_own_to_own(Own0, CSD ^ csd_own_prof).

:- pred callee_in_clique(initial_deep::in, set(proc_dynamic_ptr)::in,
    call_site_dynamic_ptr::in) is semidet.

callee_in_clique(InitDeep, Clique, CSDPtr) :-
    lookup_call_site_dynamics(InitDeep ^ init_call_site_dynamics,
        CSDPtr, CSD),
    CalleePDPtr = CSD ^ csd_callee,
    set.member(CalleePDPtr, Clique).

:- pred merge_proc_dynamic_multi_slot(merge_info::in, int::in,
    set(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
    list(call_site_dynamic_ptr)::in, list(array(call_site_array_slot))::in,
    list(call_site_dynamic_ptr)::out, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_proc_dynamic_multi_slot(MergeInfo, SlotNum, Clique,
        ParentPDPtr, PrimeCSDPtrs0, RestSiteArrays, PrimeCSDPtrs,
        !InitDeep, !Redirect) :-
    lookup_multi_sites(RestSiteArrays, SlotNum, RestCSDPtrLists),
    list.condense([PrimeCSDPtrs0 | RestCSDPtrLists], AllCSDPtrs),
    map.init(ProcMap0),
    list.foldl(cluster_csds_by_ps(!.InitDeep), AllCSDPtrs,
        ProcMap0, ProcMap),
    map.values(ProcMap, CSDPtrsClusters),
    list.foldl3(merge_multi_slot_cluster(MergeInfo, ParentPDPtr, Clique),
        CSDPtrsClusters, [], PrimeCSDPtrs, !InitDeep, !Redirect).

:- pred merge_multi_slot_cluster(merge_info::in, proc_dynamic_ptr::in,
    set(proc_dynamic_ptr)::in, list(call_site_dynamic_ptr)::in,
    list(call_site_dynamic_ptr)::in, list(call_site_dynamic_ptr)::out,
    initial_deep::in, initial_deep::out, redirect::in, redirect::out)
    is det.

merge_multi_slot_cluster(MergeInfo, ParentPDPtr, Clique, ClusterCSDPtrs,
        PrimeCSDPtrs0, PrimeCSDPtrs, InitDeep0, InitDeep,
        Redirect0, Redirect) :-
    merge_call_site_dynamics(MergeInfo, Clique,
        ParentPDPtr, ClusterCSDPtrs, PrimeCSDPtr,
        InitDeep0, InitDeep, Redirect0, Redirect),
    PrimeCSDPtrs = [PrimeCSDPtr | PrimeCSDPtrs0].

:- pred merge_call_site_dynamics(merge_info::in, set(proc_dynamic_ptr)::in,
    proc_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
    call_site_dynamic_ptr::out, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_call_site_dynamics(MergeInfo, Clique, ParentPDPtr, CandidateCSDPtrs,
        ChosenCSDPtr, !InitDeep, !Redirect) :-
    CallSiteDynamics0 = !.InitDeep ^ init_call_site_dynamics,
    list.filter(valid_call_site_dynamic_ptr_raw(CallSiteDynamics0),
        CandidateCSDPtrs, ValidCSDPtrs),
    (
        ValidCSDPtrs = [],
            % This signifies that there is no call here.
        ChosenCSDPtr = call_site_dynamic_ptr(0)
    ;
        ValidCSDPtrs = [FirstCSDPtr | LaterCSDPtrs],
        lookup_call_site_dynamics(CallSiteDynamics0, FirstCSDPtr, FirstCSD0),
        FirstCSD = FirstCSD0 ^ csd_caller := ParentPDPtr,
        update_call_site_dynamics(FirstCSDPtr, FirstCSD,
            u(CallSiteDynamics0), CallSiteDynamics),
        !InitDeep ^ init_call_site_dynamics := CallSiteDynamics,
        (
            LaterCSDPtrs = []
        ;
            LaterCSDPtrs = [_ | _],
            merge_call_site_dynamics_2(MergeInfo, Clique,
                FirstCSDPtr, LaterCSDPtrs, !InitDeep, !Redirect)
        ),
        ChosenCSDPtr = FirstCSDPtr
    ).

:- pred merge_call_site_dynamics_2(merge_info::in, set(proc_dynamic_ptr)::in,
    call_site_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
    initial_deep::in, initial_deep::out, redirect::in, redirect::out)
    is det.

merge_call_site_dynamics_2(MergeInfo, Clique, PrimeCSDPtr, RestCSDPtrs,
        InitDeep0, InitDeep, Redirect0, Redirect) :-
    % We must check whether PrimeCSDPtr and RestCSDPtrs are in Clique
    % *before* we update the proc_dynamics array in InitDeep0, which is
    % destructive updated to create InitDeep1.
    list.filter(callee_in_clique(InitDeep0, Clique), RestCSDPtrs,
        InClique, NotInClique),
    % XXX design error: should take union of cliques
    % i.e. if call is within clique in *any* caller, it should be within
    % clique in the final configuration
    ( if callee_in_clique(InitDeep0, Clique, PrimeCSDPtr) then
        require(unify(NotInClique, []),
            "merge_proc_dynamic_normal_slot: prime in clique, " ++
                "others not in clique"),
        MergeChildren = no
    else
        require(unify(InClique, []),
            "merge_proc_dynamic_normal_slot: prime not in clique, " ++
                "others in clique"),
        MergeChildren = yes
    ),
    record_csd_redirect(RestCSDPtrs, PrimeCSDPtr, Redirect0, Redirect1),
    CallSiteDynamics0 = InitDeep0 ^ init_call_site_dynamics,
    lookup_call_site_dynamics(CallSiteDynamics0, PrimeCSDPtr, PrimeCSD0),
    list.map(lookup_call_site_dynamics(CallSiteDynamics0),
        RestCSDPtrs, RestCSDs),
    PrimeOwn0 = PrimeCSD0 ^ csd_own_prof,
    list.foldl(accumulate_csd_owns, RestCSDs, PrimeOwn0, PrimeOwn1),
    PrimeCSD1 = PrimeCSD0 ^ csd_own_prof := PrimeOwn1,
    update_call_site_dynamics(PrimeCSDPtr, PrimeCSD1,
        u(CallSiteDynamics0), CallSiteDynamics1),
    InitDeep1 = InitDeep0 ^ init_call_site_dynamics := CallSiteDynamics1,
    (
        MergeChildren = no,
        InitDeep = InitDeep1,
        Redirect = Redirect1
    ;
        MergeChildren = yes,
        merge_call_site_dynamics_descendants(MergeInfo,
            PrimeCSDPtr, RestCSDPtrs, ChosenPDPtr,
            InitDeep1, InitDeep2, Redirect1, Redirect),
        % We must ensure that PrimeCSDPtr ^ csd_callee is updated to reflect
        % the chosen merged ProcDynamic.
        CallSiteDynamics2 = InitDeep2 ^ init_call_site_dynamics,
        lookup_call_site_dynamics(CallSiteDynamics2, PrimeCSDPtr, PrimeCSD2),
        PrimeCSD = PrimeCSD2 ^ csd_callee := ChosenPDPtr,
        update_call_site_dynamics(PrimeCSDPtr, PrimeCSD,
            u(CallSiteDynamics2), CallSiteDynamics),
        InitDeep = InitDeep2 ^ init_call_site_dynamics := CallSiteDynamics
    ).

:- pred merge_call_site_dynamics_descendants(merge_info::in,
    call_site_dynamic_ptr::in, list(call_site_dynamic_ptr)::in,
    proc_dynamic_ptr::out, initial_deep::in, initial_deep::out,
    redirect::in, redirect::out) is det.

merge_call_site_dynamics_descendants(MergeInfo, PrimeCSDPtr, RestCSDPtrs,
        ChosenPDPtr, !InitDeep, !Redirect) :-
    CallSiteDynamics = !.InitDeep ^ init_call_site_dynamics,
    lookup_call_site_dynamics(CallSiteDynamics, PrimeCSDPtr, PrimeCSD),
    extract_csd_callee(PrimeCSD, PrimeCSDCallee),
    list.map(lookup_call_site_dynamics(CallSiteDynamics), RestCSDPtrs,
        RestCSDs),
    list.map(extract_csd_callee, RestCSDs, RestCSDCallees),
    PDPtrs = [PrimeCSDCallee | RestCSDCallees],
    list.foldl(union_cliques(MergeInfo), PDPtrs, set.init, CliqueUnion),
    merge_proc_dynamics(MergeInfo, CliqueUnion, PDPtrs, ChosenPDPtr,
        !InitDeep, !Redirect).

:- pred union_cliques(merge_info::in, proc_dynamic_ptr::in,
    set(proc_dynamic_ptr)::in, set(proc_dynamic_ptr)::out) is det.

union_cliques(MergeInfo, PDPtr, !CliqueUnion) :-
    ( if PDPtr = proc_dynamic_ptr(0) then
        % This can happen with calls to the unify/compare preds of builtin
        % types.
        true
    else
        lookup_clique_index(MergeInfo ^ merge_clique_index, PDPtr, CliquePtr),
        lookup_clique_members(MergeInfo ^ merge_clique_members, CliquePtr,
            Members),
        set.insert_list(Members, !CliqueUnion)
    ).

:- pred lookup_normal_sites(list(array(call_site_array_slot))::in, int::in,
    list(call_site_dynamic_ptr)::out) is det.

lookup_normal_sites([], _, []).
lookup_normal_sites([RestArray | RestArrays], SlotNum, [CSDPtr | CSDPtrs]) :-
    array.lookup(RestArray, SlotNum, Slot),
    (
        Slot = slot_normal(CSDPtr)
    ;
        Slot = slot_multi(_, _),
        unexpected($pred, "found slot_multi")
    ),
    lookup_normal_sites(RestArrays, SlotNum, CSDPtrs).

:- pred lookup_multi_sites(list(array(call_site_array_slot))::in, int::in,
    list(list(call_site_dynamic_ptr))::out) is det.

lookup_multi_sites([], _, []).
lookup_multi_sites([RestArray | RestArrays], SlotNum, [CSDList | CSDLists]) :-
    array.lookup(RestArray, SlotNum, Slot),
    (
        Slot = slot_normal(_),
        unexpected($pred, "found normal")
    ;
        Slot = slot_multi(_, CSDArray),
        array.to_list(CSDArray, CSDList)
    ),
    lookup_multi_sites(RestArrays, SlotNum, CSDLists).

:- pred record_pd_redirect(list(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
    redirect::in, redirect::out) is det.

record_pd_redirect(RestPDPtrs, PrimePDPtr, !Redirect) :-
    trace [compiletime(flag("pd_redirect")), io(!IO)] (
        io.write_string("pd redirect: ", !IO),
        io.print(RestPDPtrs, !IO),
        io.write_string(" -> ", !IO),
        io.print(PrimePDPtr, !IO),
        io.nl(!IO)
    ),

    lookup_pd_redirect(!.Redirect ^ pd_redirect, PrimePDPtr, OldRedirect),
    ( if OldRedirect = proc_dynamic_ptr(0) then
        record_pd_redirect_2(RestPDPtrs, PrimePDPtr, !Redirect)
    else
        unexpected($pred, "prime is redirected")
    ).

:- pred record_pd_redirect_2(list(proc_dynamic_ptr)::in, proc_dynamic_ptr::in,
    redirect::in, redirect::out) is det.

record_pd_redirect_2([], _, !Redirect).
record_pd_redirect_2([RestPDPtr | RestPDPtrs], PrimePDPtr, !Redirect) :-
    ProcRedirect0 = !.Redirect ^ pd_redirect,
    lookup_pd_redirect(ProcRedirect0, RestPDPtr, OldRedirect),
    ( if OldRedirect = proc_dynamic_ptr(0) then
        set_pd_redirect(u(ProcRedirect0), RestPDPtr, PrimePDPtr,
            ProcRedirect)
    else
        unexpected($pred, "already redirected")
    ),
    !Redirect ^ pd_redirect := ProcRedirect,
    record_pd_redirect_2(RestPDPtrs, PrimePDPtr, !Redirect).

:- pred record_csd_redirect(list(call_site_dynamic_ptr)::in,
    call_site_dynamic_ptr::in, redirect::in, redirect::out) is det.

record_csd_redirect(RestCSDPtrs, PrimeCSDPtr, !Redirect) :-
    trace [compiletime(flag("csd_redirect")), io(!IO)] (
        io.write_string("csd redirect: ", !IO),
        io.print(RestCSDPtrs, !IO),
        io.write_string(" -> ", !IO),
        io.print(PrimeCSDPtr, !IO),
        io.nl(!IO)
    ),

    lookup_csd_redirect(!.Redirect ^ csd_redirect, PrimeCSDPtr, OldRedirect),
    ( if OldRedirect = call_site_dynamic_ptr(0) then
        record_csd_redirect_2(RestCSDPtrs, PrimeCSDPtr, !Redirect)
    else
        unexpected($pred, "prime is redirected")
    ).

:- pred record_csd_redirect_2(list(call_site_dynamic_ptr)::in,
    call_site_dynamic_ptr::in, redirect::in, redirect::out) is det.

record_csd_redirect_2([], _, !Redirect).
record_csd_redirect_2([RestCSDPtr | RestCSDPtrs], PrimeCSDPtr, !Redirect) :-
    CallSiteRedirect0 = !.Redirect ^ csd_redirect,
    lookup_csd_redirect(CallSiteRedirect0, RestCSDPtr, OldRedirect),
    ( if OldRedirect = call_site_dynamic_ptr(0) then
        set_csd_redirect(u(CallSiteRedirect0), RestCSDPtr, PrimeCSDPtr,
            CallSiteRedirect)
    else
        unexpected($pred, "already redirected")
    ),
    !Redirect ^ csd_redirect := CallSiteRedirect,
    record_csd_redirect_2(RestCSDPtrs, PrimeCSDPtr, !Redirect).

:- pred two_or_more(list(proc_dynamic_ptr)::in) is semidet.

two_or_more([_, _ | _]).

:- pred cluster_pds_by_ps(initial_deep::in, proc_dynamic_ptr::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out) is det.

cluster_pds_by_ps(InitDeep, PDPtr, !ProcMap) :-
    ProcDynamics = InitDeep ^ init_proc_dynamics,
    ( if valid_proc_dynamic_ptr_raw(ProcDynamics, PDPtr) then
        lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
        PSPtr = PD ^ pd_proc_static,
        ( if map.search(!.ProcMap, PSPtr, PDPtrs0) then
            map.det_update(PSPtr, [PDPtr | PDPtrs0], !ProcMap)
        else
            map.det_insert(PSPtr, [PDPtr], !ProcMap)
        )
    else
        true
    ).

:- pred cluster_csds_by_ps(initial_deep::in, call_site_dynamic_ptr::in,
    map(proc_static_ptr, list(call_site_dynamic_ptr))::in,
    map(proc_static_ptr, list(call_site_dynamic_ptr))::out) is det.

cluster_csds_by_ps(InitDeep, CSDPtr, !ProcMap) :-
    CallSiteDynamics = InitDeep ^ init_call_site_dynamics,
    ( if valid_call_site_dynamic_ptr_raw(CallSiteDynamics, CSDPtr) then
        lookup_call_site_dynamics(CallSiteDynamics, CSDPtr, CSD),
        PDPtr = CSD ^ csd_callee,
        ProcDynamics = InitDeep ^ init_proc_dynamics,
        ( if valid_proc_dynamic_ptr_raw(ProcDynamics, PDPtr) then
            lookup_proc_dynamics(ProcDynamics, PDPtr, PD),
            PSPtr = PD ^ pd_proc_static
        else
            PSPtr = proc_static_ptr(0)
        ),
        ( if map.search(!.ProcMap, PSPtr, CSDPtrs0) then
            map.det_update(PSPtr, [CSDPtr | CSDPtrs0], !ProcMap)
        else
            map.det_insert(PSPtr, [CSDPtr], !ProcMap)
        )
    else
        true
    ).

:- pred lookup_pd_redirect(array(proc_dynamic_ptr)::in,
    proc_dynamic_ptr::in, proc_dynamic_ptr::out) is det.

lookup_pd_redirect(ProcRedirect0, PDPtr, OldRedirect) :-
    PDPtr = proc_dynamic_ptr(PDI),
    array.lookup(ProcRedirect0, PDI, OldRedirect).

:- pred set_pd_redirect(array(proc_dynamic_ptr)::array_di,
    proc_dynamic_ptr::in, proc_dynamic_ptr::in,
    array(proc_dynamic_ptr)::array_uo) is det.

set_pd_redirect(ProcRedirect0, PDPtr, NewRedirect, ProcRedirect) :-
    PDPtr = proc_dynamic_ptr(PDI),
    array.set(PDI, NewRedirect, ProcRedirect0, ProcRedirect).

:- pred lookup_csd_redirect(array(call_site_dynamic_ptr)::in,
    call_site_dynamic_ptr::in, call_site_dynamic_ptr::out) is det.

lookup_csd_redirect(CallSiteRedirect0, CSDPtr, OldRedirect) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    array.lookup(CallSiteRedirect0, CSDI, OldRedirect).

:- pred set_csd_redirect(array(call_site_dynamic_ptr)::array_di,
    call_site_dynamic_ptr::in, call_site_dynamic_ptr::in,
    array(call_site_dynamic_ptr)::array_uo) is det.

set_csd_redirect(CallSiteRedirect0, CSDPtr, NewRedirect, CallSiteRedirect) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    array.set(CSDI, NewRedirect, CallSiteRedirect0, CallSiteRedirect).

%---------------------------------------------------------------------------%

:- pred deref_call_site_dynamic(redirect::in, call_site_dynamic_ptr::in,
    call_site_dynamic_ptr::out) is det.

deref_call_site_dynamic(Redirect, !CSDPtr) :-
    lookup_csd_redirect(Redirect ^ csd_redirect, !.CSDPtr, RedirectCSDPtr),
    RedirectCSDPtr = call_site_dynamic_ptr(RedirectCSDI),
    ( if RedirectCSDI > 0 then
        deref_call_site_dynamic(Redirect, RedirectCSDPtr, !:CSDPtr)
    else
        true
    ).

:- pred deref_proc_dynamic(redirect::in, proc_dynamic_ptr::in,
    proc_dynamic_ptr::out) is det.

deref_proc_dynamic(Redirect, !PDPtr) :-
    lookup_pd_redirect(Redirect ^ pd_redirect, !.PDPtr, RedirectPDPtr),
    RedirectPDPtr = proc_dynamic_ptr(RedirectPDI),
    ( if RedirectPDI > 0 then
        deref_proc_dynamic(Redirect, RedirectPDPtr, !:PDPtr)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred compact_dynamics(redirect::in, int::in, int::in,
    initial_deep::in, initial_deep::out) is det.

compact_dynamics(Redirect0, MaxCSD0, MaxPD0, !InitDeep) :-
    Redirect0 = redirect(CSDredirect0, PDredirect0),
    !.InitDeep = initial_deep(Stats, Root0, CSDs0, PDs0, CSSs, PSs),
    compact_csd_redirect(1, 1, MaxCSD0, NumCSD,
        u(CSDredirect0), CSDredirect),
    compact_pd_redirect(1, 1, MaxPD0, NumPD,
        u(PDredirect0), PDredirect),
    Redirect = redirect(CSDredirect, PDredirect),
    array_map_from_1(subst_in_call_site_dynamic(Redirect),
        u(CSDs0), CSDs1),
    array_map_from_1(subst_in_proc_dynamic(Redirect),
        u(PDs0), PDs1),
    array.shrink(NumCSD, CSDs1, CSDs),
    array.shrink(NumPD, PDs1, PDs),
    lookup_pd_redirect(PDredirect, Root0, Root),
    !:InitDeep = initial_deep(Stats, Root, CSDs, PDs, CSSs, PSs).

:- pred compact_csd_redirect(int::in, int::in, int::in, int::out,
    array(call_site_dynamic_ptr)::array_di,
    array(call_site_dynamic_ptr)::array_uo) is det.

compact_csd_redirect(CurOld, CurNew, MaxOld, NumNew, !CSDredirect) :-
    ( if CurOld > MaxOld then
        NumNew = CurNew
    else
        array.lookup(!.CSDredirect, CurOld, Redirect0),
        ( if Redirect0 = call_site_dynamic_ptr(0) then
            array.set(CurOld, call_site_dynamic_ptr(CurNew), !CSDredirect),
            compact_csd_redirect(CurOld + 1, CurNew + 1, MaxOld, NumNew,
                !CSDredirect)
        else
            % Since this CSD is being redirected, its slot is available for
            % another (non-redirected) CSD.
            compact_csd_redirect(CurOld + 1, CurNew, MaxOld, NumNew,
                !CSDredirect)
        )
    ).

:- pred compact_pd_redirect(int::in, int::in, int::in, int::out,
    array(proc_dynamic_ptr)::array_di,
    array(proc_dynamic_ptr)::array_uo) is det.

compact_pd_redirect(CurOld, CurNew, MaxOld, NumNew, !PDredirect) :-
    ( if CurOld > MaxOld then
        NumNew = CurNew
    else
        array.lookup(!.PDredirect, CurOld, Redirect0),
        ( if Redirect0 = proc_dynamic_ptr(0) then
            array.set(CurOld, proc_dynamic_ptr(CurNew), !PDredirect),
            compact_pd_redirect(CurOld + 1, CurNew + 1, MaxOld, NumNew,
                !PDredirect)
        else
            % Since this PD is being redirected, its slot is
            % available for another (non-redirected) PD.
            compact_pd_redirect(CurOld + 1, CurNew, MaxOld, NumNew,
                !PDredirect)
        )
    ).

:- pred subst_in_call_site_dynamic(redirect::in, call_site_dynamic::in,
    call_site_dynamic::out) is det.

subst_in_call_site_dynamic(Redirect, !CSD) :-
    !.CSD = call_site_dynamic(Caller0, Callee0, Own),
    lookup_pd_redirect(Redirect ^ pd_redirect, Caller0, Caller),
    lookup_pd_redirect(Redirect ^ pd_redirect, Callee0, Callee),
    !:CSD = call_site_dynamic(Caller, Callee, Own).

:- pred subst_in_proc_dynamic(redirect::in, proc_dynamic::in,
    proc_dynamic::out) is det.

subst_in_proc_dynamic(Redirect, !PD) :-
    !.PD = proc_dynamic(PDPtr, Slots0, MaybeCPs),
    array.map(subst_in_slot(Redirect), u(Slots0), Slots),
    !:PD = proc_dynamic(PDPtr, Slots, MaybeCPs).

:- pred subst_in_slot(redirect::in, call_site_array_slot::in,
    call_site_array_slot::out) is det.

subst_in_slot(Redirect, slot_normal(CSDPtr0), slot_normal(CSDPtr)) :-
    lookup_csd_redirect(Redirect ^ csd_redirect, CSDPtr0, CSDPtr).
subst_in_slot(Redirect, slot_multi(IsZeroed, CSDPtrs0),
        slot_multi(IsZeroed, CSDPtrs)) :-
    array.map(lookup_csd_redirect(Redirect ^ csd_redirect),
        u(CSDPtrs0), CSDPtrs).

%---------------------------------------------------------------------------%

:- pred merge_profiles(list(initial_deep)::in, maybe_error(initial_deep)::out)
    is det.

merge_profiles(InitDeeps, MaybeMergedInitDeep) :-
    (
        InitDeeps = [FirstInitDeep | LaterInitDeeps],
        ( if all_compatible(FirstInitDeep, LaterInitDeeps) then
            do_merge_profiles(FirstInitDeep, LaterInitDeeps, MergedInitDeep),
            MaybeMergedInitDeep = ok(MergedInitDeep)
        else
            MaybeMergedInitDeep =
                error("profiles are not from the same executable")
        )
    ;
        InitDeeps = [],
        MaybeMergedInitDeep = error("merge_profiles: empty list of profiles")
    ).

:- pred all_compatible(initial_deep::in, list(initial_deep)::in) is semidet.

all_compatible(BaseInitDeep, OtherInitDeeps) :-
    extract_max_css(BaseInitDeep, BaseMaxCSS),
    extract_max_ps(BaseInitDeep, BaseMaxPS),
    extract_ticks_per_sec(BaseInitDeep, BaseTicksPerSec),
    list.map(extract_max_css, OtherInitDeeps, OtherMaxCSSs),
    list.map(extract_max_ps, OtherInitDeeps, OtherMaxPSs),
    list.map(extract_ticks_per_sec, OtherInitDeeps, OtherTicksPerSec),
    all_true(unify(BaseMaxCSS), OtherMaxCSSs),
    all_true(unify(BaseMaxPS), OtherMaxPSs),
    all_true(unify(BaseTicksPerSec), OtherTicksPerSec),
    extract_init_call_site_statics(BaseInitDeep, BaseCallSiteStatics),
    extract_init_proc_statics(BaseInitDeep, BaseProcStatics),
    list.map(extract_init_call_site_statics, OtherInitDeeps,
        OtherCallSiteStatics),
    list.map(extract_init_proc_statics, OtherInitDeeps, OtherProcStatics),
    array_match_elements(1, BaseMaxCSS, BaseCallSiteStatics,
        OtherCallSiteStatics),
    array_match_elements(1, BaseMaxPS, BaseProcStatics, OtherProcStatics).

:- pred do_merge_profiles(initial_deep::in, list(initial_deep)::in,
    initial_deep::out) is det.

do_merge_profiles(BaseInitDeep, OtherInitDeeps, MergedInitDeep) :-
    extract_max_csd(BaseInitDeep, BaseMaxCSD),
    extract_max_pd(BaseInitDeep, BaseMaxPD),
    list.map(extract_max_csd, OtherInitDeeps, OtherMaxCSDs),
    list.map(extract_max_pd, OtherInitDeeps, OtherMaxPDs),
    list.foldl(int_add, OtherMaxCSDs, BaseMaxCSD, ConcatMaxCSD),
    list.foldl(int_add, OtherMaxPDs, BaseMaxPD, ConcatMaxPD),
    extract_init_call_site_dynamics(BaseInitDeep, BaseCallSiteDynamics),
    extract_init_proc_dynamics(BaseInitDeep, BaseProcDynamics),
    array.lookup(BaseCallSiteDynamics, 0, DummyCSD),
    array.lookup(BaseProcDynamics, 0, DummyPD),
    array.init(ConcatMaxCSD + 1, DummyCSD, ConcatCallSiteDynamics0),
    array.init(ConcatMaxPD + 1, DummyPD, ConcatProcDynamics0),
    AllInitDeeps = [BaseInitDeep | OtherInitDeeps],
    concatenate_profiles(AllInitDeeps, 0, 0,
        ConcatCallSiteDynamics0, ConcatCallSiteDynamics,
        ConcatProcDynamics0, ConcatProcDynamics),

    extract_max_css(BaseInitDeep, BaseMaxCSS),
    extract_max_ps(BaseInitDeep, BaseMaxPS),
    extract_ticks_per_sec(BaseInitDeep, BaseTicksPerSec),
    list.map(extract_instrument_quanta, AllInitDeeps, InstrumentQuantas),
    list.map(extract_user_quanta, AllInitDeeps, UserQuantas),
    list.foldl(int_add, InstrumentQuantas, 0, InstrumentQuanta),
    list.foldl(int_add, UserQuantas, 0, UserQuanta),
    extract_num_callseqs(BaseInitDeep, BaseNumCallSeqs),
    list.map(extract_num_callseqs, OtherInitDeeps, OtherNumCallSeqs),
    list.foldl(int_add, OtherNumCallSeqs, BaseNumCallSeqs, ConcatNumCallSeqs),

    % The program names are not checked. The new profile is named after the
    % base profile.
    BaseProgramName = BaseInitDeep ^ init_profile_stats ^ prs_program_name,

    % With the exception of the canonical flags, we get the flags from
    % the base profile also.
    BaseFlags = BaseInitDeep ^ init_profile_stats ^ prs_deep_flags,
    ConcatFlags = BaseFlags ^ df_canonical_flag := is_canonical,
    ConcatProfileStats = profile_stats(BaseProgramName,
        ConcatMaxCSD, BaseMaxCSS, ConcatMaxPD, BaseMaxPS, ConcatNumCallSeqs,
        BaseTicksPerSec, InstrumentQuanta, UserQuanta, ConcatFlags),
    % The root part is a temporary lie.
    MergedInitDeep = initial_deep(ConcatProfileStats,
        BaseInitDeep ^ init_root,
        ConcatCallSiteDynamics,
        ConcatProcDynamics,
        BaseInitDeep ^ init_call_site_statics,
        BaseInitDeep ^ init_proc_statics).
    % list.map(extract_init_root, AllInitDeeps, Roots),
    % merge clique of roots, replacing root with chosen pd

:- pred concatenate_profiles(list(initial_deep)::in, int::in, int::in,
    call_site_dynamics::array_di, call_site_dynamics::array_uo,
    proc_dynamics::array_di, proc_dynamics::array_uo) is det.

concatenate_profiles([], _PrevMaxCSD, _PrevMaxPD,
        !ConcatCallSiteDynamics, !ConcatProcDynamics).
concatenate_profiles([InitDeep | InitDeeps], PrevMaxCSD, PrevMaxPD,
        !ConcatCallSiteDynamics, !ConcatProcDynamics) :-
    concatenate_profile(InitDeep, PrevMaxCSD, PrevMaxPD, NextMaxCSD, NextMaxPD,
        !ConcatCallSiteDynamics, !ConcatProcDynamics),
    concatenate_profiles(InitDeeps, NextMaxCSD, NextMaxPD,
        !ConcatCallSiteDynamics, !ConcatProcDynamics).

:- pred concatenate_profile(initial_deep::in,
    int::in, int::in, int::out, int::out,
    call_site_dynamics::array_di, call_site_dynamics::array_uo,
    proc_dynamics::array_di, proc_dynamics::array_uo) is det.

concatenate_profile(InitDeep, PrevMaxCSD, PrevMaxPD, NextMaxCSD, NextMaxPD,
        !ConcatCallSiteDynamics, !ConcatProcDynamics) :-
    extract_max_csd(InitDeep, MaxCSD),
    extract_max_pd(InitDeep, MaxPD),
    NextMaxCSD = PrevMaxCSD + MaxCSD,
    NextMaxPD = PrevMaxPD + MaxPD,
    concatenate_profile_csds(1, MaxCSD, PrevMaxCSD, PrevMaxPD,
        InitDeep ^ init_call_site_dynamics, !ConcatCallSiteDynamics),
    concatenate_profile_pds(1, MaxPD, PrevMaxCSD, PrevMaxPD,
        InitDeep ^ init_proc_dynamics, !ConcatProcDynamics).

:- pred concatenate_profile_csds(int::in, int::in, int::in, int::in,
    call_site_dynamics::in,
    call_site_dynamics::array_di, call_site_dynamics::array_uo) is det.

concatenate_profile_csds(Cur, Max, PrevMaxCSD, PrevMaxPD, CallSiteDynamics,
        !ConcatCallSiteDynamics) :-
    ( if Cur =< Max then
        array.lookup(CallSiteDynamics, Cur, CSD0),
        CSD0 = call_site_dynamic(CallerPDPtr0, CalleePDPtr0, Own),
        concat_proc_dynamic_ptr(PrevMaxPD, CallerPDPtr0, CallerPDPtr),
        concat_proc_dynamic_ptr(PrevMaxPD, CalleePDPtr0, CalleePDPtr),
        CSD = call_site_dynamic(CallerPDPtr, CalleePDPtr, Own),
        array.set(PrevMaxCSD + Cur, CSD, !ConcatCallSiteDynamics),
        concatenate_profile_csds(Cur + 1, Max, PrevMaxCSD, PrevMaxPD,
            CallSiteDynamics, !ConcatCallSiteDynamics)
    else
        true
    ).

:- pred concatenate_profile_pds(int::in, int::in, int::in, int::in,
    proc_dynamics::in, proc_dynamics::array_di, proc_dynamics::array_uo)
    is det.

concatenate_profile_pds(Cur, Max, PrevMaxCSD, PrevMaxPD, ProcDynamics,
        !ConcatProcDynamics) :-
    ( if Cur =< Max then
        array.lookup(ProcDynamics, Cur, PD0),
        PD0 = proc_dynamic(PSPtr, Sites0, MaybeCPs),
        array.max(Sites0, MaxSite),
        concatenate_profile_slots(0, MaxSite, PrevMaxCSD, PrevMaxPD,
            u(Sites0), Sites),
        PD = proc_dynamic(PSPtr, Sites, MaybeCPs),
        array.set(PrevMaxPD + Cur, PD, !ConcatProcDynamics),
        concatenate_profile_pds(Cur + 1, Max, PrevMaxCSD, PrevMaxPD,
            ProcDynamics, !ConcatProcDynamics)
    else
        true
    ).

:- pred concatenate_profile_slots(int::in, int::in, int::in, int::in,
    array(call_site_array_slot)::array_di,
    array(call_site_array_slot)::array_uo) is det.

concatenate_profile_slots(Cur, Max, PrevMaxCSD, PrevMaxPD, !Sites) :-
    ( if Cur =< Max then
        array.lookup(!.Sites, Cur, Slot0),
        (
            Slot0 = slot_normal(CSDPtr0),
            concat_call_site_dynamic_ptr(PrevMaxCSD, CSDPtr0, CSDPtr),
            Slot = slot_normal(CSDPtr)
        ;
            Slot0 = slot_multi(IsZeroed, CSDPtrs0),
            array_map_from_0(concat_call_site_dynamic_ptr(PrevMaxCSD),
                u(CSDPtrs0), CSDPtrs),
            Slot = slot_multi(IsZeroed, CSDPtrs)
        ),
        array.set(Cur, Slot, !Sites),
        concatenate_profile_slots(Cur + 1, Max, PrevMaxCSD, PrevMaxPD, !Sites)
    else
        true
    ).

:- pred concat_call_site_dynamic_ptr(int::in, call_site_dynamic_ptr::in,
    call_site_dynamic_ptr::out) is det.

concat_call_site_dynamic_ptr(PrevMaxCSD, !CSDPtr) :-
    !.CSDPtr = call_site_dynamic_ptr(CSDI0),
    ( if CSDI0 = 0 then
        true
    else
        !:CSDPtr = call_site_dynamic_ptr(CSDI0 + PrevMaxCSD)
    ).

:- pred concat_proc_dynamic_ptr(int::in, proc_dynamic_ptr::in,
    proc_dynamic_ptr::out) is det.

concat_proc_dynamic_ptr(PrevMaxPD, !PDPtr) :-
    !.PDPtr = proc_dynamic_ptr(PDI0),
    ( if PDI0 = 0 then
        true
    else
        !:PDPtr = proc_dynamic_ptr(PDI0 + PrevMaxPD)
    ).

%---------------------------------------------------------------------------%

    % array_match_elements(Min, Max, BaseArray, OtherArrays):
    %
    % Succeeds iff all the elements of all the OtherArrays are equal to the
    % corresponding element of BaseArray.
    %
:- pred array_match_elements(int::in, int::in, array(T)::in,
    list(array(T))::in) is semidet.

array_match_elements(N, Max, BaseArray, OtherArrays) :-
    ( if N =< Max then
        array.lookup(BaseArray, N, BaseElement),
        match_element(BaseElement, N, OtherArrays),
        array_match_elements(N + 1, Max, BaseArray, OtherArrays)
    else
        true
    ).

    % match_element(TestElement, Index, Arrays):
    %
    % Succeeds iff the elements of all the Arrays at index Index
    % are equal to TestElement.
    %
:- pred match_element(T::in, int::in, list(array(T))::in) is semidet.

match_element(_, _, []).
match_element(TestElement, Index, [Array | Arrays]) :-
    array.lookup(Array, Index, Element),
    Element = TestElement,
    match_element(Element, Index, Arrays).

:- pred int_add(int::in, int::in, int::out) is det.

int_add(A, B, C) :-
    C = A + B.

%---------------------------------------------------------------------------%
:- end_module canonical.
%---------------------------------------------------------------------------%
