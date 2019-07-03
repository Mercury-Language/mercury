%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997, 2003-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: propagate.m
% Main author: petdr.
%
% Propagates the counts around the call_graph.
%
% To do this, it first identifies all the cycles in the call graph.
% Each cycle is treated as a new super-predicate, i.e. all time propagated
% into a cycle is shared between all its members.
% Then using a topological sorting of the call graph, the time is propagated
% from the leaves of all the call graph to the head.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module propagate.
:- interface.

:- import_module prof_info.

:- import_module io.
:- import_module digraph.

%---------------------------------------------------------------------------%

:- pred propagate_counts(digraph(string)::in, prof::in, prof::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- type pred_to_cycle_map == map(string, int).
:- type cycle_to_preds_map == multi_map(int, string).

:- type cycle_info
    --->    cycle_info(
                pred_to_cycle_map   :: pred_to_cycle_map,
                cycle_to_preds_map  :: cycle_to_preds_map
            ).

propagate_counts(CallGraph, !Prof, !IO) :-
    prof_get_addrdeclmap(!.Prof, AddrDeclMap),
    prof_get_profnodemap(!.Prof, ProfNodeMap0),

    identify_cycles(CallGraph, ATSort, CycleInfo),
    update_cycles(CycleInfo, AddrDeclMap, ProfNodeMap0, ProfNodeMap1),

    propagate_counts_2(ATSort, CycleInfo, AddrDeclMap,
        ProfNodeMap1, ProfNodeMap),

    prof_set_cyclemap(CycleInfo ^ pred_to_cycle_map, !Prof),
    prof_set_profnodemap(ProfNodeMap, !Prof).

%---------------------------------------------------------------------------%

    % identify_cycles(Rel, ATSort, CycleInfo):
    %
    % Identifies the cycles in the callgraph and places the members of each
    % cycle into a map which associates a unique int with each cycle and a
    % multimap which associates with each cycle number a list of preds.
    % Also approximate topologically sorts the call graph.
    %
:- pred identify_cycles(digraph(string)::in, list(string)::out,
    cycle_info::out) is det.

identify_cycles(G, ATSort, cycle_info(PredToCycleMap, CycleToPredsMap)) :-
    digraph.dfsrev(G, DfsRev),
    digraph.inverse(G, InvG),
    identify_cycles_2(DfsRev, 1, InvG, sparse_bitset.init,
        [], ATSort, map.init, PredToCycleMap, multi_map.init, CycleToPredsMap).

:- pred identify_cycles_2(list(digraph_key(string))::in, int::in,
    digraph(string)::in, digraph_key_set(string)::in,
    list(string)::in, list(string)::out,
    pred_to_cycle_map::in, pred_to_cycle_map::out,
    cycle_to_preds_map::in, cycle_to_preds_map::out) is det.

identify_cycles_2([], _, _, _, !ATSort, !PredToCycleMap, !CycleToPredsMap).
identify_cycles_2([Key | Keys0], CycleNum0, InvG, Visit0, !ATSort,
        !PredToCycleMap, !CycleToPredsMap) :-
    % Do a depth-first search on G'. The nodes we can get to and have not
    % already visited before are one cycle in the call graph.

    digraph.dfsrev(InvG, Key, Visit0, Visit, DfsRev0),
    list.map(digraph.lookup_vertex(InvG), DfsRev0, DfsRev),

    (
        DfsRev = [],
        unexpected($pred, "empty list\n")
    ;
        DfsRev = [_],
        CycleNum = CycleNum0
    ;
        DfsRev = [_, _ | _],
        CycleNum = CycleNum0 + 1,
        add_to_cycle_map(DfsRev, CycleNum, !PredToCycleMap, !CycleToPredsMap)
    ),

    list.append(DfsRev, !ATSort),

    % Delete all visited elements from Keys0 as they have already been
    % identified as part of a cycle.
    list.delete_elems(Keys0, DfsRev0, Keys),
    identify_cycles_2(Keys, CycleNum, InvG, Visit, !ATSort,
        !PredToCycleMap, !CycleToPredsMap).

    % add_to_cycle_map(Preds, CycleNum, !PredToCycleMap, !CycleToPredsMap):
    %
    % Add all the predicates in a cycle into the cycle map.
    %
:- pred add_to_cycle_map(list(string)::in, int::in,
    pred_to_cycle_map::in, pred_to_cycle_map::out,
    cycle_to_preds_map::in, cycle_to_preds_map::out) is det.

add_to_cycle_map([], _, !PredToCycleMap, !CycleToPredsMap).
add_to_cycle_map([Pred | Preds], Cycle, !PredToCycleMap, !CycleToPredsMap) :-
    map.det_insert(Pred, Cycle, !PredToCycleMap),
    multi_map.set(Cycle, Pred, !CycleToPredsMap),
    add_to_cycle_map(Preds, Cycle, !PredToCycleMap, !CycleToPredsMap).

%---------------------------------------------------------------------------%

:- pred update_cycles(cycle_info::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

update_cycles(CycleInfo, AddrDecl, !ProfNodeMap) :-
    AssocList = multi_map.to_assoc_list(CycleInfo ^ cycle_to_preds_map),
    update_cycles_2(AssocList, AddrDecl, !ProfNodeMap).

:- pred update_cycles_2(assoc_list(int, list(string))::in,
    addrdecl::in, prof_node_map::in, prof_node_map::out) is det.

update_cycles_2([], _, !ProfNodeMap).
update_cycles_2([ Num - Preds | Rest], AddrDecl, !ProfNodeMap) :-
    update_cycles_3(Preds, Num, AddrDecl, !ProfNodeMap),
    update_cycles_2(Rest, AddrDecl, !ProfNodeMap).

:- pred update_cycles_3(list(string)::in, int::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

update_cycles_3([], _, _, !ProfNodeMap).
update_cycles_3([P | Ps], CycleNum, AddrDecl, !ProfNodeMap) :-
    get_prof_node(P, AddrDecl, !.ProfNodeMap, ProfNode0),
    prof_node_set_cycle_num(CycleNum, ProfNode0, ProfNode),
    update_prof_node(P, ProfNode, AddrDecl, !ProfNodeMap),
    update_cycles_3(Ps, CycleNum, AddrDecl, !ProfNodeMap).

%---------------------------------------------------------------------------%

    % propagate_counts_2
    %
    % XXX
    %
:- pred propagate_counts_2(list(string)::in, cycle_info::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

propagate_counts_2([], _, _, !ProfNodeMap).
propagate_counts_2([Pred | Preds], CycleInfo, AddrDeclMap, !ProfNodeMap) :-
    ( if map.search(CycleInfo ^ pred_to_cycle_map, Pred, Cycle) then
        multi_map.lookup(CycleInfo ^ cycle_to_preds_map, Cycle, CyclePreds),
        list.length(CyclePreds, Length),

        % Throw away the rest of the predicates to be processed by the profiler
        % as we are about to make them into one cycle.
        ( if list.drop((Length - 1), Preds, NewPreds)  then
            process_cycle(CyclePreds, Cycle, AddrDeclMap, !ProfNodeMap),
            propagate_counts_2(NewPreds, CycleInfo, AddrDeclMap, !ProfNodeMap)
        else
            unexpected($pred, "list_drop failed\n")
        )
    else
        get_prof_node(Pred, AddrDeclMap, !.ProfNodeMap, ProfNode),
        prof_node_get_initial_counts(ProfNode, InitCounts),
        prof_node_get_propagated_counts(ProfNode, PropCounts),
        prof_node_get_parent_list(ProfNode, ParentList),
        prof_node_get_total_calls(ProfNode, TotalCalls),

        TotalCounts = float(InitCounts) + PropCounts,

        propagate_counts_3(ParentList, TotalCounts, float(TotalCalls),
            AddrDeclMap, !ProfNodeMap),
        propagate_counts_2(Preds, CycleInfo, AddrDeclMap, !ProfNodeMap)
    ).

    % process_cycle(Preds, Cycle, AddrMap, !ProfNodeMap):
    %
    % Takes the list of cycle preds and treats them as one single unit
    % called <cycle X>.
    %
:- pred process_cycle(list(string)::in, int::in, addrdecl::in,
    prof_node_map::in, prof_node_map::out) is det.

process_cycle(Preds, Cycle, AddrMap, !ProfNodeMap) :-
    % Determine the parents of a cycle.
    cycle_parents(Preds, AddrMap, !.ProfNodeMap, Total, Recursive, ParentList),

    % Build the cycle name.
    NameStr = string.format("< cycle %d as a whole >", [i(Cycle)]),

    % Work out number of selfcounts.
    SelfCounts = sum_self_counts(Preds, AddrMap, !.ProfNodeMap),

    % Work out number of propagated counts.
    PropCounts = sum_propagated_counts(Preds, AddrMap, !.ProfNodeMap),

    build_cycle_list(Preds, AddrMap, !.ProfNodeMap, CycleList),

    ProfNode = prof_node_init_cycle(NameStr, 0, SelfCounts, PropCounts,
        CycleList, Total, Recursive),

    % NB We give the address of a cycle as being the negative of the cycle
    % number as this will be unique.
    Address = -Cycle,
    map.det_insert(Address, ProfNode, !ProfNodeMap),

    % Propagate the counts XXX
    TotalCalls = float.float(Total),
    TotalCounts = float(SelfCounts) + PropCounts,
    propagate_counts_3(ParentList, TotalCounts, TotalCalls, AddrMap,
        !ProfNodeMap).

    % sum_self_counts(Preds, ADMap, PNMap):
    %
    % Sums the self counts fields for all the predicates.
    %
:- func sum_self_counts(list(string), addrdecl, prof_node_map) = int.

sum_self_counts(Preds, ADMap, PNMap) = Sum :-
    list.foldl(accumulate_self_counts(ADMap, PNMap), Preds, 0, Sum).

:- pred accumulate_self_counts(addrdecl::in, prof_node_map::in, string::in,
    int::in, int::out) is det.

accumulate_self_counts(ADMap, PNMap, Pred, !Sum) :-
    get_prof_node(Pred, ADMap, PNMap, ProfNode),
    prof_node_get_initial_counts(ProfNode, InitCount),
    !:Sum  = !.Sum + InitCount.

    % sum_propagated_counts:
    %
    % Sums the propagated counts fields for all the predicates.
    %
:- func sum_propagated_counts(list(string), addrdecl, prof_node_map) = float.

sum_propagated_counts(Preds, ADMap, PNMap) = Sum :-
    list.foldl(accumulate_propagated_counts(ADMap, PNMap), Preds, 0.0, Sum).

:- pred accumulate_propagated_counts(addrdecl::in, prof_node_map::in,
    string::in, float::in, float::out) is det.

accumulate_propagated_counts(ADMap, PNMap, Pred, !Sum) :-
    get_prof_node(Pred, ADMap, PNMap, ProfNode),
    prof_node_get_propagated_counts(ProfNode, PropCount),
    !:Sum = !.Sum + PropCount.

    % build_cycle_list
    %
    % Takes the list of predicates and works out how many times each
    % predicate is called by a fellow predicate.
    % XXX Not fully implemented yet.
    %
:- pred build_cycle_list(list(string)::in, addrdecl::in, prof_node_map::in,
    list(pred_info)::out) is det.

build_cycle_list([], _, _, []).
build_cycle_list([P | Ps], ADM, PNM, CycleList) :-
    build_cycle_list(Ps, ADM, PNM, CycleList0),
    pred_info_init(P, 0, PredInfo),
    CycleList = [PredInfo | CycleList0].

:- pred propagate_counts_3(list(pred_info)::in, float::in, float::in,
    addrdecl::in, prof_node_map::in, prof_node_map::out) is det.

propagate_counts_3([], _, _, _, !ProfNodeMap).
propagate_counts_3([ P | Ps], TotalCounts, TotalCalls, AddrMap, !ProfNodeMap) :-
    pred_info_get_entire(P, Pred, Calls),

    % Work out the number of counts to propagate.
    % XXX Probably need to do a 0.0 check.
    ToPropagateCounts = float(Calls) / TotalCalls * TotalCounts,

    % Add new counts to current propagated counts.
    get_prof_node(Pred, AddrMap, !.ProfNodeMap, ProfNode0),
    prof_node_get_propagated_counts(ProfNode0, PropCount0),
    PropCount = PropCount0 + ToPropagateCounts,
    prof_node_set_propagated_counts(PropCount, ProfNode0, ProfNode),
    update_prof_node(Pred, ProfNode, AddrMap, !ProfNodeMap),
    propagate_counts_3(Ps, TotalCounts, TotalCalls, AddrMap, !ProfNodeMap).

    % cycle_parents(Preds, AddrMap, ProfNodeMap, TotalCalls, SelfCalls,
    %   ParentList):
    %
    % Returns a list(pred_info) which is the list of parents of the cycle
    % Also returns how may times the cycle is called and how may times
    % predicates in a cycle call each other.
    %
:- pred cycle_parents(list(string)::in, addrdecl::in, prof_node_map::in,
    int::out, int::out, list(pred_info)::out) is det.

cycle_parents(Preds, AddrMap, ProfNodeMap, TotalCalls, SelfCalls,
        ParentList) :-
    build_parent_map(Preds, AddrMap, ProfNodeMap, TotalCalls,
        SelfCalls, ParentMap),
    map.to_assoc_list(ParentMap, ParentAssocList),
    ParentList = assoc_list_to_pred_info_list(ParentAssocList).

    % build_parent_map(CliqueList, AddrMap, ProfNodeMap, TotalCalls, SelfCalls,
    %   ParentMap):
    %
    % Builds a map which contains all the parents of a cycle, and the
    % total number of times that parent is called.  Doesn't include the
    % cycle members, and callers which never call any of the members of
    % the cycle.  At the same time also sums the total calls into the
    % cycle and the calls internal to the cycle.
    %
:- pred build_parent_map(list(string)::in, addrdecl::in,
    prof_node_map::in, int::out, int::out, map(string, int)::out) is det.

build_parent_map(CliqueList, AddrMap, ProfNodeMap, TotalCalls, SelfCalls,
        ParentMap) :-
    (
        CliqueList = [],
        unexpected($pred, "empty cycle list\n")
    ;
        CliqueList = [_ | _],
        build_parent_map_2(CliqueList, CliqueList, AddrMap, ProfNodeMap,
            0, TotalCalls, 0, SelfCalls, map.init, ParentMap)
    ).

:- pred build_parent_map_2(list(string)::in, list(string)::in, addrdecl::in,
    prof_node_map::in, int::in, int::out, int::in, int::out,
    map(string, int)::in, map(string, int)::out) is det.

build_parent_map_2([], _, _, _, !TotalCalls, !SelfCalls, !ParentMap).
build_parent_map_2([C | Cs], CliqueList, AddrMap, ProfNodeMap,
        !TotalCalls, !SelfCalls, !ParentMap) :-
    get_prof_node(C, AddrMap, ProfNodeMap, ProfNode),
    prof_node_get_parent_list(ProfNode, ParentList),
    add_to_parent_map(ParentList, CliqueList, 0, TotalCalls1, 0, SelfCalls1,
        !ParentMap),

    !:TotalCalls = !.TotalCalls + TotalCalls1,
    !:SelfCalls  = !.SelfCalls + SelfCalls1,
    build_parent_map_2(Cs, CliqueList, AddrMap, ProfNodeMap,
        !TotalCalls, !SelfCalls, !ParentMap).

    % add_to_parent_map(Preds, CliqueList, !TotalCalls, !SelfCalls,
    %   !ParentMap):
    %
    % Adds list of parents to parent map. Ignores clique members and repeats
    % and callers which never call current predicate. Also returns the total
    % number of times predicate is called.
    %
:- pred add_to_parent_map(list(pred_info)::in, list(string)::in,
    int::in, int::out, int::in, int::out,
    map(string, int)::in, map(string, int)::out) is det.

add_to_parent_map([], _, !TotalCalls, !SelfCalls, !ParentMap).
add_to_parent_map([Pred | Preds], CliqueList, !TotalCalls, !SelfCalls,
        !ParentMap) :-
    pred_info_get_pred_name(Pred, PredName),
    pred_info_get_counts(Pred, Counts),
    ( if
        (
            list.member(PredName, CliqueList)
        ;
            Counts = 0
        )
    then
        !:SelfCalls = !.SelfCalls + Counts,
        add_to_parent_map(Preds, CliqueList, !TotalCalls, !SelfCalls,
            !ParentMap)
    else
        ( if map.search(!.ParentMap, PredName, CurrCount0) then
            CurrCount = CurrCount0 + Counts,
            map.det_update(PredName, CurrCount, !ParentMap)
        else
            map.det_insert(PredName, Counts, !ParentMap)
        ),
        !:TotalCalls = !.TotalCalls + Counts,
        add_to_parent_map(Preds, CliqueList, !TotalCalls, !SelfCalls,
            !ParentMap)
    ).

:- func assoc_list_to_pred_info_list(assoc_list(string, int))
    = list(pred_info).

assoc_list_to_pred_info_list([]) = [].
assoc_list_to_pred_info_list([S - I | Xs]) = List :-
    List0 = assoc_list_to_pred_info_list(Xs),
    pred_info_init(S, I, PredInfo),
    List = [PredInfo | List0].

%---------------------------------------------------------------------------%
:- end_module propagate.
%---------------------------------------------------------------------------%
