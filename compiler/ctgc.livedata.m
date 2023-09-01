%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.livedata.m.
% Main author: nancy.
%
% Definition of the live_data type used to represent the set of datastructures
% that are probably (not definitely!) live at a given program point (goal).
% A data structure is said to be "live" if the memory used to represent
% that data structure may possibly be accessed during the further execution
% of the program w.r.t. the program point (goal) looked at.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.livedata.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type livedata.

    % Create an initial set of live data structure, possibly given a list
    % of live variables or data structures.
    %
:- func livedata_init = livedata.   % Which assumes that nothing is live.
:- func livedata_init_from_vars(live_vars) = livedata.
:- func livedata_init_from_datastructs(live_datastructs) = livedata.
:- func livedata_init_as_top = livedata.

    % Verify whether the liveness set is bottom resp. top.
:- pred livedata_is_bottom(livedata::in) is semidet.
:- pred livedata_is_top(livedata::in) is semidet.

    % Return the list of live data structures represented by livedata.
    % Returns a software error when the livedata set is top.
    %
:- func livedata_get_datastructs(livedata) = list(datastruct).

    % Least upper bound of all the livedata.
    %
:- func livedata_least_upper_bound(module_info, var_table,
    livedata, livedata) = livedata.
:- func livedata_least_upper_bound_list(module_info, var_table,
    list(livedata)) = livedata.

    % Subsumption predicates.
    %
:- pred livedata_subsumes_prog_var(livedata::in, prog_var::in) is semidet.
:- pred livedata_subsumes_datastruct(module_info::in, var_table::in,
    livedata::in, datastruct::in) is semidet.

    % Projection operation.
    %
:- func livedata_project(list(prog_var), livedata) = livedata.

%-----------------------------------------------------------------------------%

:- func livedata_init_at_goal(module_info, var_table, hlds_goal_info,
    sharing_as) = livedata.

:- func livedata_add_liveness(module_info, proc_info, live_datastructs,
    sharing_as, livedata) = livedata.

:- pred nodes_are_not_live(module_info::in, var_table::in,
    list(datastruct)::in, livedata::in, nodes_are_not_live_result::out) is det.

:- type nodes_are_not_live_result
    --->    nodes_all_live
    ;       nodes_are_live(list(datastruct)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.util.

:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type livedata
    --->    livedata_bottom      % There are no live data structures.
    ;       livedata_top         % All data structures may be live.
    ;       livedata_live(list(datastruct)).
                                 % Only the listed datastructures can
                                 % possibly be live.

%-----------------------------------------------------------------------------%

livedata_init = livedata_bottom.
livedata_init_from_vars(LiveVars) =
    livedata_live(list.map(datastruct_init, LiveVars)).
livedata_init_from_datastructs(Data) =
    livedata_live(Data).
livedata_init_as_top = livedata_top.

livedata_is_bottom(livedata_bottom).
livedata_is_top(livedata_top).

livedata_get_datastructs(livedata_bottom) = [].
livedata_get_datastructs(livedata_live(Data)) = Data.
livedata_get_datastructs(livedata_top) =
    unexpected($pred, "livedata_get_datastructs: livedata is top.").

livedata_least_upper_bound(ModuleInfo, VarTable, LiveData1,
        LiveData2) = LiveData :-
    (
        LiveData1 = livedata_bottom,
        LiveData = LiveData2
    ;
        LiveData1 = livedata_top,
        LiveData = livedata_top
    ;
        LiveData1 = livedata_live(Data1),
        (
            LiveData2 = livedata_bottom,
            LiveData = LiveData1
        ;
            LiveData2 = livedata_top,
            LiveData = livedata_top
        ;
            LiveData2 = livedata_live(Data2),
            LiveData = livedata_live(
                datastruct_lists_least_upper_bound(ModuleInfo,
                    VarTable, Data1, Data2))
        )
    ).

livedata_least_upper_bound_list(ModuleInfo, VarTable, LiveDataList)
    = list.foldl(livedata_least_upper_bound(ModuleInfo, VarTable),
        LiveDataList, livedata_init).

livedata_subsumes_prog_var(LiveData, ProgVar) :-
    livedata_subsumes_topcell(LiveData, datastruct_init(ProgVar)).

:- pred livedata_subsumes_topcell(livedata::in, datastruct::in) is semidet.

livedata_subsumes_topcell(LiveData, TopCell) :-
    (
        LiveData = livedata_top
    ;
        LiveData = livedata_live(Data),
        list.member(TopCell, Data)
    ).

livedata_subsumes_datastruct(ModuleInfo, VarTable, LiveData, Datastruct):-
    ( if datastruct_refers_to_topcell(Datastruct) then
        livedata_subsumes_topcell(LiveData, Datastruct)
    else
        livedata_subsumes_datastruct_with_selector(ModuleInfo, VarTable,
            LiveData, Datastruct)
    ).

:- pred livedata_subsumes_datastruct_with_selector(module_info::in,
    var_table::in, livedata::in, datastruct::in) is semidet.

livedata_subsumes_datastruct_with_selector(ModuleInfo, VarTable, LiveData,
        Datastruct) :-
    (
        LiveData = livedata_top
    ;
        LiveData = livedata_live(Data),
        datastruct_subsumed_by_list(ModuleInfo, VarTable, Datastruct, Data)
    ).

livedata_project(ProgVars, LiveData) = ProjectedLiveData :-
    (
        LiveData = livedata_bottom,
        ProjectedLiveData = livedata_bottom
    ;
        LiveData = livedata_top,
        ProjectedLiveData = livedata_top
    ;
        LiveData = livedata_live(Data),
        list.filter(list_contains_datastruct_var(ProgVars),
            Data, FilteredData),
        (
            FilteredData = [],
            ProjectedLiveData = livedata_bottom
        ;
            FilteredData = [_ | _],
            ProjectedLiveData = livedata_live(FilteredData)
        )
    ).

:- pred list_contains_datastruct_var(prog_vars::in, datastruct::in) is semidet.

list_contains_datastruct_var(ProgVars, Datastruct) :-
    list.member(Datastruct ^ sc_var, ProgVars).

%-----------------------------------------------------------------------------%

livedata_init_at_goal(ModuleInfo, VarTable, GoalInfo, SharingAs) = LiveData :-
    % Collect the
    % (XXX collect the what?)
    Lfu = goal_info_get_lfu(GoalInfo),
    Lbu = goal_info_get_lbu(GoalInfo),
    Lu = set_of_var.to_sorted_list(set_of_var.union(Lfu, Lbu)),
    ( if
        % When there are no data structure in forward nor backward use,
        % then the livedata set is empty.
        Lu = []
    then
        LiveData = livedata_init
    else if
        % If Lu is not empty, and sharing is top, then all possible
        % datastructures might possibly be live.
        sharing_as_is_top(SharingAs)
    then
        LiveData = livedata_init_as_top
    else if
        % Lu not empty, and sharing is bottom... then only the
        % datastructures in local use are live.
        sharing_as_is_bottom(SharingAs)
    then
        LiveData = livedata_init_from_vars(Lu)
    else
        % Otherwise we have the most general case: Lu not empty, Sharing
        % not top nor bottom.
        SharingDomain = to_structure_sharing_domain(SharingAs),
        (
            SharingDomain = structure_sharing_real(SharingPairs),
            LiveData = livedata_init_at_goal_2(ModuleInfo, VarTable, Lu,
                SharingPairs)
        ;
            ( SharingDomain = structure_sharing_bottom
            ; SharingDomain = structure_sharing_top(_)
            ),
            unexpected($pred, "unexpected SharingDomain")
        )
    ).

    % Preconditions: live_vars is not empty, sharing_as is not top.
    %
    % This function corresponds to pa_alias_as.live_2 in the old reuse branch
    % which is significantly more involved so there may still be more code to
    % port over.  However, its caller always passes the empty set for the
    % LIVE_0 argument which would make a lot of code unnecessary.
    %
:- func livedata_init_at_goal_2(module_info, var_table, live_vars,
    structure_sharing) = livedata.

livedata_init_at_goal_2(ModuleInfo, VarTable, Lu, SharingPairs) = LiveData :-
    % LIVE1 = top-level datastructs from Lu
    LuData = list.map(datastruct_init, Lu),
    LIVE1 = LuData,

    % LIVE2 = datastructs X^s such that X^s is aliased to Y^t,
    % and Y is in Lu.
    live_from_in_use(ModuleInfo, VarTable, LuData, SharingPairs, LIVE2),

    % LIVE3 was some complicated thing.
    % But since LIVE_0 is the empty set LIVE3 would be empty too.

    LiveData = livedata_live(LIVE1 ++ LIVE2).

    % In use information is stored as a set of datastructures. This
    % procedure computes the set of live datastructure using the in-use set
    % and extending it wrt the sharing information.
    %
:- pred live_from_in_use(module_info::in, var_table::in, list(datastruct)::in,
    structure_sharing::in, list(datastruct)::out) is det.

live_from_in_use(ModuleInfo, VarTable, InUseList, SharingPairs, Live):-
    % Filter the list of sharing pairs, keeping only the ones that
    % involve at least one variable from the IN_USE (LU) set.
    list.map(one_of_vars_is_live(ModuleInfo, VarTable, InUseList),
        SharingPairs, DatastructsLists),
    list.condense(DatastructsLists, Live).

	% one_of_vars_is_live(LIST, ALIAS, X^sx1)
	% returns true if
	% 	ALIAS = X^sx - Y^sy
	%   and Y^s1 \in LIST and
	%		sy = s1.s2 => sx1 = sx
	% 	   or
	%		sy.s2 = s1 => sx1 = sx.s2
    %
:- pred one_of_vars_is_live(module_info::in, var_table::in,
    list(datastruct)::in, structure_sharing_pair::in, list(datastruct)::out)
    is det.

one_of_vars_is_live(ModuleInfo, VarTable, Datastructs0, PairXY,
        Datastructs) :-
	one_of_vars_is_live_ordered(ModuleInfo, VarTable, Datastructs0,
        PairXY, L1),
	% Switch order.
    PairXY = X - Y,
    PairYX = Y - X,
	one_of_vars_is_live_ordered(ModuleInfo, VarTable, Datastructs0,
        PairYX, L2),
    Datastructs = L1 ++ L2.

:- pred one_of_vars_is_live_ordered(module_info::in, var_table::in,
	list(datastruct)::in, structure_sharing_pair::in,
    list(datastruct)::out) is det.

one_of_vars_is_live_ordered(ModuleInfo, VarTable, List, Pair, List_Xsx1) :-
	Pair = Xsx - Ysy,
	list.filter(datastruct_same_vars(Ysy), List, Y_List),
	( if
		% First try to find one of the found datastructs which is
		% fully alive: so that Ysy is less or equal to at least one
		% Ys1 in Y_List (sy = s1.s2)
		list.filter(datastruct_subsumed_by(ModuleInfo, VarTable, Ysy),
            Y_List, FY_List),
		FY_List = [_ | _]
	then
		Xsx1 = Xsx,
		List_Xsx1 = [Xsx1]
	else
		% Find all datastructs from Y_List which are less or
		% equal to Ysy. Select the one with the shortest selector
		% (Note that there should be only one solution. If more
		% than one such datastruct is found, the initial live set
		% is not minimal, while this should be somehow guaranteed).
		list.filter_map(
            datastruct_subsumed_by_return_selector(ModuleInfo, VarTable, Ysy),
            Y_List, SelectorList),
		% Each sx1 = sx.s2, where s2 is one of SelectorList.
		list.map(
            ( pred(S2::in, Xsx1::out) is det :-
                datastruct_termshift(ModuleInfo, VarTable, S2, Xsx) = Xsx1
            ), SelectorList, List_Xsx1)
	).

livedata_add_liveness(ModuleInfo, ProcInfo, LuData, LocalSharing, LiveData0)
        = LiveData :-
    ( if sharing_as_is_top(LocalSharing) then
        LiveData = livedata_init_as_top
    else if sharing_as_is_bottom(LocalSharing) then
        proc_info_get_var_table(ProcInfo, VarTable),
        LiveData = livedata_least_upper_bound(ModuleInfo, VarTable,
            LiveData0, livedata_init_from_datastructs(LuData))
    else
        % most general case: normal sharing.
        proc_info_get_var_table(ProcInfo, VarTable),
        ExtendLuData = extend_datastructs(ModuleInfo, ProcInfo,
            LocalSharing, LuData),
        LuLiveData = livedata_init_from_datastructs(ExtendLuData),
        ExtendLiveData = extend_livedata(ModuleInfo, ProcInfo,
            LocalSharing, LiveData0),
        LiveData = livedata_least_upper_bound(ModuleInfo, VarTable,
            LuLiveData, ExtendLiveData)
    ).

:- func extend_livedata(module_info, proc_info, sharing_as, livedata)
    = livedata.

extend_livedata(ModuleInfo, ProcInfo, SharingAs, LiveData0) = LiveData :-
    (
        LiveData0 = livedata_bottom,
        LiveData = livedata_bottom
    ;
        LiveData0 = livedata_top,
        LiveData = livedata_top
    ;
        LiveData0 = livedata_live(Data0),
        LiveData = livedata_live(extend_datastructs(ModuleInfo, ProcInfo,
            SharingAs, Data0))
    ).

nodes_are_not_live(ModuleInfo, VarTable, DeadNodes, LiveData, Result) :-
    (
        LiveData = livedata_top,
        Result = nodes_all_live
    ;
        LiveData = livedata_bottom,
        Result = nodes_are_live([])
    ;
        LiveData = livedata_live(LiveDatastructs),
        datastructs_that_are_subsumed_by_list(ModuleInfo, VarTable,
            DeadNodes, LiveDatastructs, SubsumedNodes),
        Result = nodes_are_live(SubsumedNodes)
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.livedata.
%-----------------------------------------------------------------------------%
