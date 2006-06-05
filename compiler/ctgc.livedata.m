%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.livedata.
:- interface.

:- import_module parse_tree.prog_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.domain.
:- import_module hlds.hlds_goal.

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
:- func livedata_least_upper_bound(module_info, proc_info, livedata, 
    livedata) = livedata.
:- func livedata_least_upper_bound_list(module_info, proc_info, 
    list(livedata)) = livedata.

    % Subsumption predicates.
    %
:- pred livedata_subsumes_prog_var(livedata::in, prog_var::in) is semidet.
:- pred livedata_subsumes_datastruct(module_info::in, proc_info::in, 
    livedata::in, datastruct::in) is semidet.

    % Projection operation.
    % 
:- func livedata_project(list(prog_var), livedata) = livedata.

%-----------------------------------------------------------------------------%

:- func livedata_init_at_goal(module_info, proc_info, hlds_goal_info,
    sharing_as) = livedata.
:- func livedata_add_liveness(module_info, proc_info, live_datastructs,
    sharing_as, livedata) = livedata.

:- pred nodes_are_not_live(module_info::in, proc_info::in, 
    list(datastruct)::in, livedata::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module transform_hlds.ctgc.datastruct.

:- import_module set.

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
livedata_get_datastructs(livedata_top) = unexpected(this_file, 
    "livedata_get_datastructs: livedata is top.").

livedata_least_upper_bound(ModuleInfo, ProcInfo, LiveData1, 
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
                    ProcInfo, Data1, Data2))
        )
    ).

livedata_least_upper_bound_list(ModuleInfo, ProcInfo, LiveDataList) 
    = list.foldl(livedata_least_upper_bound(ModuleInfo, ProcInfo), 
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

livedata_subsumes_datastruct(ModuleInfo, ProcInfo, LiveData, Datastruct):-
    (
        datastruct_refers_to_topcell(Datastruct)
    ->
        livedata_subsumes_topcell(LiveData, Datastruct)
    ;
        livedata_subsumes_datastruct_with_selector(ModuleInfo, ProcInfo,
            LiveData, Datastruct)
    ).

:- pred livedata_subsumes_datastruct_with_selector(module_info::in, 
    proc_info::in, livedata::in, datastruct::in) is semidet.

livedata_subsumes_datastruct_with_selector(ModuleInfo, ProcInfo, LiveData,
        Datastruct) :- 
    (
        LiveData = livedata_top
    ;
        LiveData = livedata_live(Data),
        datastruct_subsumed_by_list(ModuleInfo, ProcInfo, Datastruct, Data)
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

livedata_init_at_goal(ModuleInfo, ProcInfo, GoalInfo, SharingAs) = LiveData :-
    % Collect the 
    % (XXX collect the what?)
    Lfu = goal_info_get_lfu(GoalInfo),
    Lbu = goal_info_get_lbu(GoalInfo), 
    Lu = set.to_sorted_list(set.union(Lfu, Lbu)),
    (
        % When there are no data structure in forward nor backward use, 
        % then the livedata set is empty.
        Lu = []
    -> 
        LiveData = livedata_init
    ;
        % If Lu is not empty, and sharing is top, then all possible
        % datastructures might possibly be live.
        sharing_as_is_top(SharingAs)
    -> 
        LiveData = livedata_init_as_top
    ;
        % Lu not empty, and sharing is bottom... then only the
        % datastructures in local use are live.
        sharing_as_is_bottom(SharingAs)
    -> 
        LiveData = livedata_init_from_vars(Lu)
    ;
        % otherwise we have the most general case: Lu not empty, Sharing
        % not top nor bottom.
        LiveData = livedata_init_at_goal_2(ModuleInfo, ProcInfo, Lu, 
            SharingAs)
    ).

    % Preconditions: live_vars is not empty, sharing_as is not top.
    %
:- func livedata_init_at_goal_2(module_info, proc_info, live_vars, 
    sharing_as) = livedata.

livedata_init_at_goal_2(ModuleInfo, ProcInfo, Lu, SharingAs) = LiveData :-
    % Let Data0 be the set of data structures pointed at by the 
    % set of live vars Lu, then LiveData is the result of "extending" 
    % (Cf. structure_sharing.domain.m) each of the data structures in Data0.
    Data0 = list.map(datastruct_init, Lu),
    DataList = extend_datastructs(ModuleInfo, ProcInfo, SharingAs, 
        Data0), 
    LiveData = livedata_live(DataList).

livedata_add_liveness(ModuleInfo, ProcInfo, LuData, LocalSharing, LiveData0) 
        = LiveData :- 
    ( 
        sharing_as_is_top(LocalSharing)
    ->
        LiveData = livedata_init_as_top
    ; 
        sharing_as_is_bottom(LocalSharing)
    ->
        LiveData = livedata_least_upper_bound(ModuleInfo, ProcInfo, 
            LiveData0, livedata_init_from_datastructs(LuData))
    ;
        % most general case: normal sharing.
        LuLiveData = livedata_init_from_datastructs(
            extend_datastructs(ModuleInfo, ProcInfo, 
                LocalSharing, LuData)),
        ExtendLiveData = extend_livedata(ModuleInfo, ProcInfo, 
            LocalSharing, LiveData0),
        LiveData = livedata_least_upper_bound(ModuleInfo, ProcInfo, 
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

nodes_are_not_live(ModuleInfo, ProcInfo, Nodes, LiveData) :- 
    (
        LiveData = livedata_top,
        fail
    ; 
        LiveData = livedata_bottom,
        true
    ;
        LiveData = livedata_live(Data),
        \+ datastructs_subsumed_by_list(ModuleInfo, ProcInfo, Nodes, Data)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ctgc.livedata.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.livedata.
%-----------------------------------------------------------------------------%
