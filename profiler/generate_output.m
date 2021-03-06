%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: generate_output.m
% Main author: petdr.
%
% Takes the prof structure and generates the output.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module generate_output.
:- interface.

:- import_module output_prof_info.
:- import_module prof_info.

:- import_module io.
:- import_module map.

%---------------------------------------------------------------------------%

:- pred generate_prof_output(io.text_output_stream::in, prof::in,
    map(string, int)::out, profiler_output::out, io::di, io::uo) is det.

:- func checked_float_divide(float, float) = float.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module rbtree.
:- import_module string.

%---------------------------------------------------------------------------%

    % We use rbtrees because they allow duplicate values to be stored.
    % This means that we can then convert to a sorted list of names which
    % can be used to lookup the output_prof map when we actually output.
:- type profiling
    --->    profiling(
                map(string, output_prof),   % associate name with the
                                            % output_prof structure.
                rbtree(float, string),      % associate call graph
                                            % percentage with a name.
                rbtree(flat_key, string)    % as above except for flat
                                            % profile.
            ).

:- type flat_key
    --->    flat_key(
                    float,  % per cent time in this predicate
                    int     % number of calls to this predicate
            ).

%---------------------------------------------------------------------------%

generate_prof_output(ProgressStream, Prof, IndexMap, Output, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),

    % Get intitial values of use.
    prof_get_entire(Prof, _, _, _IntTotalCounts, _, ProfNodeMap, _),
    ProfNodeList = map.values(ProfNodeMap),
    OutputProf0 = profiling_init,
    process_prof_node_list(ProgressStream, ProfNodeList, Prof, VeryVerbose,
        OutputProf0, OutputProf, !IO),

    OutputProf = profiling(InfoMap, CallTree, FlatTree),
    CallList0 = rbtree.values(CallTree),
    FlatList0 = rbtree.values(FlatTree),

    assign_index_numbers(IndexMap, CallList0, CallList),
    FlatList = list.reverse(FlatList0),
    Output = profiler_output(InfoMap, CallList, FlatList).

:- pred process_prof_node_list(io.text_output_stream::in,
    list(prof_node)::in, prof::in, bool::in,
    profiling::in, profiling::out, io::di, io::uo) is det.

process_prof_node_list(_, [], _, _, !OutputProf, !IO).
process_prof_node_list(ProgressStream, [PN | PNs], Prof, VeryVerbose,
        !OutputProf, !IO) :-
    (
        VeryVerbose = yes,
        prof_node_get_pred_name(PN, LabelName),
        io.write_string(ProgressStream, "\n\t% Processing " ++ LabelName, !IO)
    ;
        VeryVerbose = no
    ),
    process_prof_node(PN, Prof, !OutputProf),
    process_prof_node_list(ProgressStream, PNs, Prof, VeryVerbose,
        !OutputProf, !IO).

    % process_prof_node(ProfNode, Prof, !OutputProf):
    %
    % This is the main function.  It converts the prof_node structure
    % to the output_prof structure.
    %
:- pred process_prof_node(prof_node::in, prof::in,
    profiling::in, profiling::out) is det.

process_prof_node(ProfNode, Prof, !OutputProf) :-
    prof_node_type(ProfNode, ProfNodeType),
    ( if ProfNodeType = predicate then
        generate_output_for_single_predicate(ProfNode, Prof, !OutputProf)
    else
        true
        % generate_output.cycle(ProfNode, Prof, OutputProf0, OutputProf)
    ).

    % generate_output_for_cycle(ProfNode, Prof, !OutputProf):
    %
    % XXX
    %
:- pred generate_output_for_cycle(prof_node::in, prof::in,
    profiling::in, profiling::out) is det.

generate_output_for_cycle(ProfNode, Prof, !OutputProf) :-
    prof_get_entire(Prof, Scale, _Units, IntTotalCounts, _, _, _CycleMap),
    TotalCounts = float.float(IntTotalCounts),

    prof_node_get_entire_cycle(ProfNode, Name, CycleNum, Initial, Prop,
        _CycleMembers, TotalCalls, SelfCalls),

    !.OutputProf = profiling(InfoMap0, CallTree0, FreeTree),

    % Calculate proportion of time in current predicate and its descendants
    % as a percentage.
    InitialFloat = float.float(Initial),
    ( if TotalCounts = 0.0 then
        DescPercentage = 0.0
    else
        DescPercentage = (InitialFloat + Prop) / TotalCounts * 100.0
    ),

    % Calculate the self time spent in the current predicate.
    % Calculate the descendant time, which is the time spent in the
    % current predicate and its descendants.
    SelfTime = InitialFloat * Scale,
    DescTime = (InitialFloat + Prop) * Scale,

    OutputProfNode = output_cycle_prof(Name, CycleNum, SelfTime,
        DescPercentage, DescTime, TotalCalls, SelfCalls, [], []),

    map.det_insert(Name, OutputProfNode, InfoMap0, InfoMap),
    rbtree.insert_duplicate(DescPercentage, Name, CallTree0, CallTree),

    !:OutputProf = profiling(InfoMap, CallTree, FreeTree).

    % generate_output_for_single_predicate(ProfNode, Prof, !OutputProf):
    %
    % Fills out the output_prof structure when pred is a single predicate.
    %
:- pred generate_output_for_single_predicate(prof_node::in, prof::in,
    profiling::in, profiling::out) is det.

generate_output_for_single_predicate(ProfNode, Prof, !OutputProf) :-
    prof_get_entire(Prof, Scale, _Units, IntTotalCounts, _, _, CycleMap),
    TotalCounts = float.float(IntTotalCounts),

    prof_node_get_entire_pred(ProfNode, LabelName, CycleNum, Initial, Prop,
        ParentList, ChildList, TotalCalls, SelfCalls, NameList),

    % Node only needs to be processed if it has a parent or a child.
    ( if
        ParentList = [],
        ChildList = []
    then
        true
    else
        !.OutputProf = profiling(InfoMap0, CallTree0, FlatTree0),

        Name = LabelName ++ construct_name(NameList),

        % Calculate proportion of time in current predicate and its
        % descendants as a percentage.
        % Calculate proportion of time in current predicate
        % as a percentage.
        InitialFloat = float.float(Initial),
        ( if TotalCounts = 0.0 then
            DescPercentage = 0.0,
            FlatPercentage = 0.0
        else
            DescPercentage = (InitialFloat + Prop) / TotalCounts * 100.0,
            FlatPercentage = InitialFloat / TotalCounts * 100.0
        ),

        % Calculate the self time spent in the current predicate.
        % Calculate the descendant time, which is the time spent in the
        % current predicate and its descendants
        SelfTime = InitialFloat * Scale,
        DescTime = (InitialFloat+Prop) * Scale,

        process_prof_node_parents(ParentList, SelfTime, DescTime,
            TotalCalls, CycleNum, CycleMap,
            OutputParentList, OutputCycleParentList),
        process_prof_node_children(ChildList, CycleNum, CycleMap,
            Prof, OutputChildList, OutputCycleChildList),

        OutputProfNode = output_prof(Name, CycleNum,
            DescPercentage, FlatPercentage, SelfTime, DescTime,
            TotalCalls, SelfCalls,
            OutputParentList, OutputChildList,
            OutputCycleParentList, OutputCycleChildList
        ),

        map.det_insert(LabelName, OutputProfNode, InfoMap0, InfoMap),
        rbtree.insert_duplicate(DescPercentage, LabelName,
            CallTree0, CallTree),
        rbtree.insert_duplicate(flat_key(FlatPercentage, TotalCalls),
            LabelName, FlatTree0, FlatTree),

        !:OutputProf = profiling(InfoMap, CallTree, FlatTree)
    ).

    % construct_name:
    % When more then one predicate maps to the same address, this predicate
    % will build a string of all the different names separated by 'or's.
    %
:- func construct_name(list(string)) = string.

construct_name([]) = "".
construct_name([Name | Names]) = NameStr :-
    NameStr0 = construct_name(Names),
    string.append(" or ", Name, NameStr1),
    string.append(NameStr1, NameStr0, NameStr).

    % process_prof_node_parents:
    %
    % Generate the parents output structure.
    %
:- pred process_prof_node_parents(list(pred_info)::in, float::in, float::in,
    int::in, int::in, cycle_map::in, list(parent)::out, list(parent)::out)
    is det.

process_prof_node_parents(Parents0, SelfTime, DescTime, TotalCalls0, CycleNum,
        CycleMap, OutputParentList, OutputCycleParentList) :-
    remove_cycle_members(Parents0, CycleNum, CycleMap,
        TotalCalls0, TotalCalls, Parents, OutputCycleParentList),
    FltTotalCalls = float.float(TotalCalls),
    process_prof_node_parents_2(Parents, SelfTime, DescTime, FltTotalCalls,
        CycleMap, OutputParentList).

    % remove_cycle_members(PredInfos, CycleNum, CycleMap, !TotalCalls, List,
    %   OutputCycleParentList):
    %
    % Removes any members of the same cycle from the parent listing
    % of a predicate. Then adjusts the total calls so as not to include
    % that predicate.
    %
:- pred remove_cycle_members(list(pred_info)::in, int::in, cycle_map::in,
    int::in, int::out, list(pred_info)::out, list(parent)::out) is det.

remove_cycle_members([], _, _, !TotalCalls, [], []).
remove_cycle_members([PN | PNs], CycleNum, CycleMap, !TotalCalls, List,
        OutputCycleParentList) :-
    pred_info_get_entire(PN, LabelName, Calls),
    ( if map.search(CycleMap, LabelName, ParentCycleNum) then
        ( if ParentCycleNum = CycleNum then
            !:TotalCalls = !.TotalCalls - Calls,
            remove_cycle_members(PNs, CycleNum, CycleMap, !TotalCalls,
                List, OC0),
            Parent = parent(LabelName, CycleNum, 0.0, 0.0, Calls),
            OutputCycleParentList = [Parent | OC0]
        else
            remove_cycle_members(PNs, CycleNum, CycleMap, !TotalCalls,
                List0, OC0),
            OutputCycleParentList = OC0,
            List = [PN | List0]
        )
    else
        remove_cycle_members(PNs, CycleNum, CycleMap, !TotalCalls,
            List0, OutputCycleParentList),
        List = [PN | List0]
    ).

:- pred process_prof_node_parents_2(list(pred_info)::in, float::in, float::in,
    float::in, cycle_map::in, list(parent)::out) is det.

process_prof_node_parents_2([], _, _, _, _, []).
process_prof_node_parents_2([P | Ps], SelfTime, DescTime, TotalCalls,
        CycleMap, OutputParentList) :-
    rbtree.init(Output0),
    process_prof_node_parents_3([P | Ps], SelfTime, DescTime, TotalCalls,
        CycleMap, Output0, Output),
    rbtree.values(Output, OutputParentList).

:- pred process_prof_node_parents_3(list(pred_info)::in,
    float::in, float::in, float::in, cycle_map::in,
    rbtree(int, parent)::in, rbtree(int, parent)::out) is det.

process_prof_node_parents_3([], _, _, _, _, !Output).
process_prof_node_parents_3([PN | PNs], SelfTime, DescTime, TotalCalls,
        CycleMap, !Output) :-
    pred_info_get_entire(PN, LabelName, Calls),

    % Check if the parent is a member of a cycle.
    ( if map.search(CycleMap, LabelName, ParentCycleNum0) then
        ParentCycleNum = ParentCycleNum0
    else
        ParentCycleNum = 0
    ),

    Proportion = checked_float_divide(float(Calls), TotalCalls),

    % Calculate the amount of the current predicate's self-time spent
    % due to the parent,
    % and the amount of the current predicate's descendant-time spent
    % due to the parent.
    PropSelfTime = SelfTime * Proportion,
    PropDescTime = DescTime * Proportion,

    Parent = parent(LabelName, ParentCycleNum, PropSelfTime, PropDescTime,
        Calls),
    rbtree.insert_duplicate(Calls, Parent, !Output),

    process_prof_node_parents_3(PNs, SelfTime, DescTime, TotalCalls,
        CycleMap, !Output).

:- pred process_prof_node_children(list(pred_info)::in, int::in, cycle_map::in,
    prof::in, list(child)::out, list(child)::out) is det.

process_prof_node_children([], _, _, _, [], []).
process_prof_node_children([C | Cs], CycleNum, CycleMap, Prof, OutputChildList,
        OutputCycleChildList) :-
    remove_child_cycle_members([C|Cs], CycleNum, CycleMap, Children,
        OutputCycleChildList),
    rbtree.init(Output0),
    process_prof_node_children_2(Children, Prof, Output0, Output),
    rbtree.values(Output, OutputChildList).

    % remove_child_cycle_members:
    % Removes any members of the same cycle from the child listing
    % of a predicate and adds them to a new list.
    %
:- pred remove_child_cycle_members(list(pred_info)::in, int::in, cycle_map::in,
    list(pred_info)::out, list(child)::out)is det.

remove_child_cycle_members([], _, _, [], []).
remove_child_cycle_members([PN | PNs], CycleNum, CycleMap, List,
        CycleChildList) :-
    pred_info_get_entire(PN, LabelName, Calls),
    ( if map.search(CycleMap, LabelName, ChildCycleNum) then
        ( if ChildCycleNum = CycleNum then
            remove_child_cycle_members(PNs, CycleNum, CycleMap, List, OC0),
            Child = child(LabelName, CycleNum, 0.0, 0.0, Calls, 0),
            CycleChildList = [Child | OC0]
        else
            remove_child_cycle_members(PNs, CycleNum, CycleMap, List0, OC0),
            CycleChildList = OC0,
            List = [PN | List0]
        )
    else
        remove_child_cycle_members(PNs, CycleNum, CycleMap, List0,
            CycleChildList),
        List = [PN | List0]
    ).

:- pred process_prof_node_children_2(list(pred_info)::in, prof::in,
    rbtree(int, child)::in, rbtree(int, child)::out) is det.

process_prof_node_children_2([], _, !Output).
process_prof_node_children_2([PN | PNs], Prof, !Output) :-
    pred_info_get_entire(PN, LabelName, Calls),
    prof_get_entire(Prof, Scale, _Units, _, AddrMap, ProfNodeMap, CycleMap),

    ( if map.search(CycleMap, LabelName, CycleNum0) then
        CycleNum = CycleNum0
    else
        CycleNum = 0
    ),

    get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode),
    prof_node_get_initial_counts(ProfNode, Initial),
    prof_node_get_propagated_counts(ProfNode, Prop),
    prof_node_get_total_calls(ProfNode, TotalCalls),

    CurrentCount = float(Initial) + Prop,
    Proportion = checked_float_divide(float(Calls), float(TotalCalls)),

    % Calculate the self time spent in the current predicate.
    SelfTime = float(Initial) * Scale,

    % Calculate the descendant time, which is the time spent in the
    % current predicate and its descendants
    DescTime = CurrentCount * Scale,

    % Calculate the amount of the current predicate's self-time spent
    % due to the parent,
    % and the amount of the current predicate's descendant-time spent
    % due to the parent.
    PropSelfTime = SelfTime * Proportion,
    PropDescTime = DescTime * Proportion,

    Child = child(LabelName, CycleNum, PropSelfTime, PropDescTime, Calls,
        TotalCalls),
    rbtree.insert_duplicate(Calls, Child, !Output),
    process_prof_node_children_2(PNs, Prof, !Output).

    % assign_index_numbers(IndexMap, RevList, List):
    %
    % Reverses the output list so that the predicates which account for
    % most of the time come first, and then assigns index numbers.
    %
:- pred assign_index_numbers(map(string, int)::out,
    list(string)::in, list(string)::out) is det.

assign_index_numbers(IndexMap, RevList, List) :-
    list.reverse(RevList, List),
    assign_index_numbers_2(List, 1, map.init, IndexMap).

:- pred assign_index_numbers_2(list(string)::in, int::in,
    map(string, int)::in, map(string, int)::out) is det.

assign_index_numbers_2([], _, !IndexMap).
assign_index_numbers_2([X0 | Xs0], N, !IndexMap) :-
    map.det_insert(X0, N, !IndexMap),
    assign_index_numbers_2(Xs0, N + 1, !IndexMap).

:- func profiling_init = profiling.

profiling_init = Profiling :-
    map.init(InfoMap),
    rbtree.init(CallTree),
    rbtree.init(FlatTree),
    Profiling = profiling(InfoMap, CallTree, FlatTree).

checked_float_divide(A, B) = ( if B = 0.0 then 0.0 else A / B).

%---------------------------------------------------------------------------%
:- end_module generate_output.
%---------------------------------------------------------------------------%
