%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% propagate.m
%
% Main author: petdr.
%
% Propagates the counts around the call_graph.
% To do this it first identifies all the cycles in the call graph.
% Each cycle is treated as a new super-predicate.  ie All time propagated into
% a cycle is shared between all it's members.
% Then using a topological sorting of the call graph, the time is propagated
% from the leaves of all the call graph to the head.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module propagate.
:- interface.

:- import_module prof_info.

:- import_module io.
:- import_module relation.

%-----------------------------------------------------------------------------%

:- pred propagate.counts(relation(string)::in, prof::in, prof::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

:- type cycle_info ==
    pair(
        map(string, int),       % predicate - cycle
        multi_map(int, string)  % cycle - list preds
    ).

propagate.counts(CallGraph, !Prof, !IO) :-
    prof_get_addrdeclmap(!.Prof, AddrDeclMap),
    prof_get_profnodemap(!.Prof, ProfNodeMap0),

    propagate.identify_cycles(CallGraph, ATSort, CycleInfo),
    propagate.update_cycles(CycleInfo, AddrDeclMap, ProfNodeMap0,
        ProfNodeMap1),

    propagate.counts_2(ATSort, CycleInfo, AddrDeclMap, ProfNodeMap1,
        ProfNodeMap),

    prof_set_cyclemap(fst(CycleInfo), !Prof),
    prof_set_profnodemap(ProfNodeMap, !Prof).

%-----------------------------------------------------------------------------%

    % propagate.identify_cycles:
    % Identifies the cycles in the callgraph and places the members of each
    % cycle into a map which associates a unique int with each cycle and a
    % multimap which associates with each cycle number a list of preds.
    % Also approximate topologically sorts the call graph.
    %
:- pred propagate.identify_cycles(relation(string)::in, list(string)::out,
    cycle_info::out) is det.

propagate.identify_cycles(Rel, ATSort, CycleInfo) :-
    relation.dfsrev(Rel, DfsRev),
    relation.inverse(Rel, RelInv),
    propagate.identify_cycles_2(DfsRev, 1, RelInv, sparse_bitset.init,
        [], ATSort, cycle_info_init, CycleInfo).

:- pred propagate.identify_cycles_2(list(relation_key)::in, int::in,
    relation(string)::in, relation_key_set::in,
    list(string)::in, list(string)::out, cycle_info::in, cycle_info::out)
    is det.

propagate.identify_cycles_2([], _, _, _, !ATSort, !CycleInfo).
propagate.identify_cycles_2([X | Xs0], CycleNum0, RelInv, Visit0, !ATSort,
        !CycleInfo) :-
    %
    % Do a DFS on R'.  The nodes we can get to and have not
    % already visited before are one cycle in the call graph.
    %
    relation.dfsrev(RelInv, X, Visit0, Visit, DfsRev0),
    list.map(relation.lookup_key(RelInv), DfsRev0, DfsRev),

    (
        (
            DfsRev = [_]
        ;
            DfsRev = [],    % This case should never happen
            error("propagate.identify_cycles_2: empty list\n")
        )
    ->
        CycleNum = CycleNum0
    ;
        CycleNum = CycleNum0 + 1,
        propagate.add_to_cycle_map(DfsRev, CycleNum, !CycleInfo)
    ),

    list.append(DfsRev, !ATSort),
    %
    % Delete all visited elements from Xs0 as they have already
    % been identified as part of a cycle.
    %
    list.delete_elems(Xs0, DfsRev0, Xs),
    propagate.identify_cycles_2(Xs, CycleNum, RelInv, Visit, !ATSort,
        !CycleInfo).

    % cycle_info_init:
    % Initialise the cycle_info structure.
    %
:- func cycle_info_init = cycle_info.

cycle_info_init = map.init - multi_map.init.

    % propagate.add_to_cycle_map:
    % Add all the predicates in a cycle into the cycle map.
    %
:- pred propagate.add_to_cycle_map(list(string)::in, int::in,
    cycle_info::in, cycle_info::out) is det.

propagate.add_to_cycle_map([], _, !CycleInfo).
propagate.add_to_cycle_map([X | Xs], V, M0 - MM0, M - MM) :-
    map.det_insert(M0, X, V, M1),
    multi_map.set(MM0, V, X, MM1),
    propagate.add_to_cycle_map(Xs, V, M1 - MM1, M - MM).

%-----------------------------------------------------------------------------%

:- pred propagate.update_cycles(cycle_info::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

propagate.update_cycles(_M - MM, AddrDecl, !ProfNodeMap) :-
    AssocList = multi_map.to_assoc_list(MM),
    propagate.update_cycles_2(AssocList, AddrDecl, !ProfNodeMap).

:- pred propagate.update_cycles_2(assoc_list(int, list(string))::in,
    addrdecl::in, prof_node_map::in, prof_node_map::out) is det.

propagate.update_cycles_2([], _, !ProfNodeMap).
propagate.update_cycles_2([ Num - Preds | Rest], AddrDecl, !ProfNodeMap) :-
    propagate.update_cycles_3(Preds, Num, AddrDecl, !ProfNodeMap),
    propagate.update_cycles_2(Rest, AddrDecl, !ProfNodeMap).

:- pred propagate.update_cycles_3(list(string)::in, int::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

propagate.update_cycles_3([], _, _, !ProfNodeMap).
propagate.update_cycles_3([P | Ps], CycleNum, AddrDecl, !ProfNodeMap) :-
    get_prof_node(P, AddrDecl, !.ProfNodeMap, ProfNode0),
    prof_node_set_cycle_num(CycleNum, ProfNode0, ProfNode),
    update_prof_node(P, ProfNode, AddrDecl, !ProfNodeMap),
    propagate.update_cycles_3(Ps, CycleNum, AddrDecl, !ProfNodeMap).

%-----------------------------------------------------------------------------%

    % propagate.counts_2
    %   XXX
    %
:- pred propagate.counts_2(list(string)::in, cycle_info::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

propagate.counts_2([], _, _, !ProfNodeMap).
propagate.counts_2([Pred | Preds], M - MM, AddrDeclMap, !ProfNodeMap) :-
    ( map.search(M, Pred, Cycle) ->
        multi_map.lookup(MM, Cycle, CyclePreds),
        list.length(CyclePreds, Length),

        (
            % Throw away the rest of the predicates to
            % be processed by the profiler as we are about
            % to make them into one cycle.
            list.drop((Length - 1), Preds, NewPreds)
        ->
            propagate.process_cycle(CyclePreds, Cycle, AddrDeclMap,
                !ProfNodeMap),
            propagate.counts_2(NewPreds, M-MM, AddrDeclMap, !ProfNodeMap)
        ;
            error("propagate.counts_2: list_drop failed\n")
        )
    ;
        get_prof_node(Pred, AddrDeclMap, !.ProfNodeMap, ProfNode),
        prof_node_get_initial_counts(ProfNode, InitCounts),
        prof_node_get_propagated_counts(ProfNode, PropCounts),
        prof_node_get_parent_list(ProfNode, ParentList),
        prof_node_get_total_calls(ProfNode, TotalCalls),

        TotalCounts = float(InitCounts) + PropCounts,

        propagate.counts_3(ParentList, TotalCounts, float(TotalCalls),
            AddrDeclMap, !ProfNodeMap),
        propagate.counts_2(Preds, M-MM, AddrDeclMap, !ProfNodeMap)
    ).

    % propagate.process_cycle:
    % Takes the list of cycle preds and treats them as one single unit
    % called <cycle X>.
    %
:- pred propagate.process_cycle(list(string)::in, int::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

propagate.process_cycle(Preds, Cycle, AddrMap, !ProfNodeMap) :-
    %
    % Determine the parents of a cycle
    %
    propagate.cycle_parents(Preds, AddrMap, !.ProfNodeMap, Total,
        Recursive, ParentList),
    %
    % Build the cycle name.
    %
    NameStr = string.format("< cycle %d as a whole >", [i(Cycle)]),
    %
    % Work out number of selfcounts.
    %
    SelfCounts = propagate.sum_self_counts(Preds, AddrMap, !.ProfNodeMap),
    %
    % Work out number of propagated counts.
    %
    PropCounts = propagate.sum_propagated_counts(Preds, AddrMap,
        !.ProfNodeMap),

    propagate.build_cycle_list(Preds, AddrMap, !.ProfNodeMap, CycleList),

    ProfNode = prof_node_init_cycle(NameStr, 0, SelfCounts, PropCounts,
        CycleList, Total, Recursive),
    %
    % NB we give the address of a cycle as being the negative of
    % the cycle number as this will be unique.
    %
    Address = -Cycle,
    map.det_insert(!.ProfNodeMap, Address, ProfNode, !:ProfNodeMap),
    %
    % Propagate the counts XXX
    %
    TotalCalls = float.float(Total),
    TotalCounts = float(SelfCounts) + PropCounts,
    propagate.counts_3(ParentList, TotalCounts, TotalCalls, AddrMap,
        !ProfNodeMap).

    % propagate.sum_self_counts:
    % Sums the self counts fields for all the predicates.
    %
:- func propagate.sum_self_counts(list(string), addrdecl, prof_node_map) = int.

propagate.sum_self_counts(Preds, ADMap, PNMap) =
    list.foldl((func(Pred, Sum0) = Sum :-
            get_prof_node(Pred, ADMap, PNMap, ProfNode),
            prof_node_get_initial_counts(ProfNode, InitCount),
            Sum  = Sum0 + InitCount
        ), Preds, 0).

    % propagate.sum_propagated_counts:
    % Sums the propagated counts fields for all the predicates.
    %
:- func propagate.sum_propagated_counts(list(string), addrdecl, prof_node_map)
    = float.

propagate.sum_propagated_counts(Preds, ADMap, PNMap) =
    list.foldl((func(Pred, Sum0) = Sum :-
            get_prof_node(Pred, ADMap, PNMap, ProfNode),
            prof_node_get_propagated_counts(ProfNode, PropCount),
            Sum = Sum0 + PropCount
        ), Preds, 0.0).

    % propagate.build_cycle_list
    % Takes the list of predicates and works out how many times each
    % predicate is called by a fellow predicate
    % XXX Not fully implemented yet.
    %
:- pred propagate.build_cycle_list(list(string)::in, addrdecl::in,
    prof_node_map::in, list(pred_info)::out) is det.

propagate.build_cycle_list([], _, _, []).
propagate.build_cycle_list([P | Ps], ADM, PNM, CycleList) :-
    propagate.build_cycle_list(Ps, ADM, PNM, CycleList0),
    pred_info_init(P, 0, PredInfo),
    CycleList = [ PredInfo | CycleList0].

:- pred propagate.counts_3(list(pred_info)::in, float::in, float::in,
    addrdecl::in, prof_node_map::in, prof_node_map::out) is det.

propagate.counts_3([], _, _, _, !ProfNodeMap).
propagate.counts_3([ P | Ps], TotalCounts, TotalCalls, AddrMap,
        !ProfNodeMap) :-
    pred_info_get_entire(P, Pred, Calls),
    %
    % Work out the number of counts to propagate.
    % XXX Probably need to do a 0.0 check
    %
    ToPropagateCounts = float(Calls) / TotalCalls * TotalCounts,
    %
    % Add new counts to current propagated counts.
    %
    get_prof_node(Pred, AddrMap, !.ProfNodeMap, ProfNode0),
    prof_node_get_propagated_counts(ProfNode0, PropCount0),
    PropCount = PropCount0 + ToPropagateCounts,
    prof_node_set_propagated_counts(PropCount, ProfNode0, ProfNode),
    update_prof_node(Pred, ProfNode, AddrMap, !ProfNodeMap),
    propagate.counts_3(Ps, TotalCounts, TotalCalls, AddrMap, !ProfNodeMap).

    % propagate.cycle_parents
    % Returns a list(pred_info) which is the list of parents of the cycle
    % Also returns how may times the cycle is called and how may times
    % predicates in a cycle call each other.
    %
:- pred propagate.cycle_parents(list(string)::in, addrdecl::in,
    prof_node_map::in, int::out, int::out, list(pred_info)::out) is det.

propagate.cycle_parents(Preds, AddrMap, ProfNodeMap, TotalCalls, SelfCalls,
        ParentList) :-
    propagate.build_parent_map(Preds, AddrMap, ProfNodeMap, TotalCalls,
        SelfCalls, ParentMap),
    map.to_assoc_list(ParentMap, ParentAssocList),
    ParentList = assoc_list_to_pred_info_list(ParentAssocList).

    % propagate.build_parent_map:
    % Builds a map which contains all the parents of a cycle, and the
    % total number of times that parent is called.  Doesn't include the
    % cycle members, and callers which never call any of the members of
    % the cycle.  At the same time also sums the total calls into the
    % cycle and the calls internal to the cycle.
    %
:- pred propagate.build_parent_map(list(string)::in, addrdecl::in,
    prof_node_map::in, int::out, int::out, map(string, int)::out) is det.

propagate.build_parent_map([], _AddrMap, _ProfNodeMap, _, _, _ParentMap) :-
    error("build_parent_map: empty cycle list\n").
propagate.build_parent_map([C | Cs], AddrMap, ProfNodeMap, TotalCalls,
        SelfCalls, ParentMap) :-
    build_parent_map_2([C | Cs], [C | Cs], AddrMap, ProfNodeMap,
        0, TotalCalls, 0, SelfCalls, map.init, ParentMap).

:- pred build_parent_map_2(list(string)::in, list(string)::in, addrdecl::in,
    prof_node_map::in, int::in, int::out, int::in, int::out,
    map(string, int)::in, map(string, int)::out) is det.

build_parent_map_2([], _, _, _, !TotalCalls, !SelfCalls, !ParentMap).
build_parent_map_2([C | Cs], CliqueList, AddrMap, ProfNodeMap,
        !TotalCalls, !SelfCalls, !ParentMap) :-
    get_prof_node(C, AddrMap, ProfNodeMap, ProfNode),
    prof_node_get_parent_list(ProfNode, ParentList),
    add_to_parent_map(ParentList, CliqueList, 0, TotalCalls1,
        0, SelfCalls1, !ParentMap),

    !:TotalCalls = !.TotalCalls + TotalCalls1,
    !:SelfCalls  = !.SelfCalls + SelfCalls1,
    build_parent_map_2(Cs, CliqueList, AddrMap, ProfNodeMap, !TotalCalls,
        !SelfCalls, !ParentMap).

    % add_to_parent_map:
    % Adds list of parents to parent map.  Ignores clique members and
    % repeats and callers which never call current predicate.
    % Also returns the total number of times predicate is called.
    %
:- pred add_to_parent_map(list(pred_info)::in, list(string)::in,
    int::in, int::out, int::in, int::out,
    map(string, int)::in, map(string, int)::out) is det.

add_to_parent_map([], _, !TotalCalls, !SelfCalls, !ParentMap).
add_to_parent_map([P | Ps], CliqueList, !TotalCalls, !SelfCalls, !ParentMap) :-
    pred_info_get_pred_name(P, PredName),
    pred_info_get_counts(P, Counts),
    (
        (
            list.member(PredName, CliqueList)
        ;
            Counts = 0
        )
    ->
        !:SelfCalls = !.SelfCalls + Counts,
        add_to_parent_map(Ps, CliqueList, !TotalCalls, !SelfCalls, !ParentMap)
    ;
        (
            map.search(!.ParentMap, PredName, CurrCount0)
        ->
            CurrCount = CurrCount0 + Counts,
            svmap.det_update(PredName, CurrCount, !ParentMap)
        ;
            svmap.det_insert(PredName, Counts, !ParentMap)
        ),
        !:TotalCalls = !.TotalCalls + Counts,
        add_to_parent_map(Ps, CliqueList, !TotalCalls, !SelfCalls, !ParentMap)
    ).

:- func assoc_list_to_pred_info_list(assoc_list(string, int))
    = list(pred_info).

assoc_list_to_pred_info_list([]) = [].
assoc_list_to_pred_info_list([S - I | Xs]) = List :-
    List0 = assoc_list_to_pred_info_list(Xs),
    pred_info_init(S, I, PredInfo),
    List = [PredInfo | List0].

%------------------------------------------------------------------------------%
:- end_module propagate.
%------------------------------------------------------------------------------%
