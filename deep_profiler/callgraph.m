%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006, 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains the code for finding the cliques in the call graph
% described by an initial_deep structure. They are returned as a list of
% cliques, in bottom-up order.

:- module callgraph.

:- interface.

:- import_module profile.

:- import_module array.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred find_cliques(initial_deep::in, list(list(proc_dynamic_ptr))::out)
    is det.

:- pred make_clique_indexes(int::in, list(list(proc_dynamic_ptr))::in,
    array(list(proc_dynamic_ptr))::array_uo, array(clique_ptr)::array_uo)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array_util.
:- import_module cliques.

:- import_module int.
:- import_module io.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

find_cliques(InitDeep, BottomUpPDPtrCliqueList) :-
    make_graph(InitDeep, Graph),
    topological_sort(Graph, TopDownPDICliqueList),

    % Turn each of the sets of PDIs into a list of PDPtrs.  We use foldl here
    % because the list may be very long and map runs out of stack space, and
    % we want the final list in reverse order anyway because the propagation
    % algorithm works bottom up.
    callgraph.foldl(accumulate_pdptr_lists, TopDownPDICliqueList,
        [], BottomUpPDPtrCliqueList).

    % This version of foldl is safer when tail recursion isn't available.
    %
:- pred foldl(pred(X, A, A)::in(pred(in, in, out) is det), list(X)::in,
    A::in, A::out) is det.

foldl(P, !.L, !A) :-
    foldl_2(100000, P, !L, !A),
    (
        !.L = []
    ;
        !.L = [_ | _],
        disable_warning [suspicious_recursion] (
            callgraph.foldl(P, !.L, !A)
        )
    ).

:- pred foldl_2(int::in, pred(X, A, A)::in(pred(in, in, out) is det),
    list(X)::in, list(X)::out, A::in, A::out) is det.

foldl_2(Depth, P, !Xs, !A) :-
    ( if Depth > 0 then
        (
            !.Xs = []
        ;
            !.Xs = [X | !:Xs],
            P(X, !A),
            foldl_2(Depth - 1, P, !Xs, !A)
        )
    else
        true
    ).

:- pred accumulate_pdptr_lists(set(int)::in, list(list(proc_dynamic_ptr))::in,
    list(list(proc_dynamic_ptr))::out) is det.

accumulate_pdptr_lists(PDISet, PDPtrLists0, PDPtrLists) :-
    pdi_set_to_pdptr_list(PDISet, PDPtrList),
    PDPtrLists = [PDPtrList | PDPtrLists0].

:- pred pdi_set_to_pdptr_list(set(int)::in, list(proc_dynamic_ptr)::out)
    is det.

pdi_set_to_pdptr_list(PDISet, PDPtrList) :-
    set.to_sorted_list(PDISet, PDIList),
    list.map(pdi_to_pdptr, PDIList, PDPtrList).

:- pred pdi_to_pdptr(int::in, proc_dynamic_ptr::out) is det.

pdi_to_pdptr(PDI, proc_dynamic_ptr(PDI)).

%---------------------------------------------------------------------------%

:- pred make_graph(initial_deep::in, graph::out) is det.

make_graph(InitDeep, Graph) :-
    init(Graph0),
    array_foldl_from_1(add_pd_arcs(InitDeep), InitDeep ^ init_proc_dynamics,
        Graph0, Graph).

:- pred add_pd_arcs(initial_deep::in, int::in, proc_dynamic::in,
    graph::in, graph::out) is det.

add_pd_arcs(InitDeep, PDI, PD, !Graph) :-
    CallSiteRefArray = PD ^ pd_sites,
    array.to_list(CallSiteRefArray, CallSiteRefList),
    list.foldl(add_call_site_arcs(InitDeep, PDI), CallSiteRefList,
        !Graph).

:- pred add_call_site_arcs(initial_deep::in, int::in, call_site_array_slot::in,
    graph::in, graph::out) is det.

add_call_site_arcs(InitDeep, FromPDI, CallSiteSlot, !Graph) :-
    (
        CallSiteSlot = slot_normal(CSDPtr),
        add_csd_arcs(InitDeep, FromPDI, CSDPtr, !Graph)
    ;
        CallSiteSlot = slot_multi(_, CSDPtrArray),
        array.to_list(CSDPtrArray, CSDPtrs),
        list.foldl(add_csd_arcs(InitDeep, FromPDI), CSDPtrs, !Graph)
    ).

:- pred add_csd_arcs(initial_deep::in, int::in, call_site_dynamic_ptr::in,
    graph::in, graph::out) is det.

add_csd_arcs(InitDeep, FromPDI, CSDPtr, !Graph) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
    ( if CSDI > 0 then
        array.lookup(InitDeep ^ init_call_site_dynamics, CSDI, CSD),
        ToPDPtr = CSD ^ csd_callee,
        ToPDPtr = proc_dynamic_ptr(ToPDI),
        trace [compiletime(flag("add_csd_arcs")), io(!IO)] (
            write_arc(FromPDI, ToPDI, CSDI, !IO)
        ),
        add_arc(!.Graph, FromPDI, ToPDI, !:Graph)
    else
        true
    ).

%---------------------------------------------------------------------------%

make_clique_indexes(NPDs, CliqueList, Cliques, CliqueIndex) :-
    Cliques = array(CliqueList),
    array.init(NPDs, clique_ptr(-1), CliqueIndex0),

    % For each clique, add entries to the CliqueIndex array, which maps every
    % proc_dynamic_ptr back to the clique to which it belongs.
    array_foldl_from_1(index_clique, Cliques, CliqueIndex0, CliqueIndex).

:- pred index_clique(int::in, list(proc_dynamic_ptr)::in,
    array(clique_ptr)::array_di, array(clique_ptr)::array_uo) is det.

index_clique(CliqueNum, CliqueMembers, !CliqueIndex) :-
    array_list_foldl(index_clique_member(CliqueNum), CliqueMembers,
        !CliqueIndex).

:- pred index_clique_member(int::in, proc_dynamic_ptr::in,
    array(clique_ptr)::array_di, array(clique_ptr)::array_uo) is det.

index_clique_member(CliqueNum, PDPtr, !CliqueIndex) :-
    PDPtr = proc_dynamic_ptr(PDI),
    trace [compiletime(flag("index_clique_member")), io(!IO)] (
        write_pdi_cn(PDI, CliqueNum, !IO)
    ),
    array.set(PDI, clique_ptr(CliqueNum), !CliqueIndex).

%---------------------------------------------------------------------------%
%
% Predicates for use in debugging.
%

:- pred write_arc(int::in, int::in, int::in, io::di, io::uo) is det.

write_arc(FromPDI, ToPDI, CSDI, !IO) :-
    io.format("arc from pd %d to pd %d through csd %d\n",
        [i(FromPDI), i(ToPDI), i(CSDI)], !IO).

:- pred write_pdi_cn(int::in, int::in, io::di, io::uo) is det.

write_pdi_cn(PDI, CN, !IO) :-
    io.write_string("pdi ", !IO),
    io.write_int(PDI, !IO),
    io.write_string(" -> clique ", !IO),
    io.write_int(CN, !IO),
    io.nl(!IO),
    io.flush_output(!IO).

%---------------------------------------------------------------------------%
:- end_module callgraph.
%---------------------------------------------------------------------------%
