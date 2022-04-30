%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: rbmm.region_resurrection_renaming.m.
% Main author: Quan Phan.
%
% Region resurrection is the situation where the liveness of a region
% variable along an execution path is like: live, dead, live ..., i.e., the
% variable becomes bound, then unbound, then bound again. This makes region
% variables different from regular Mercury variables.
% This module finds which renaming and reversed renaming of region variables
% are needed so that region resurrection is resolved, and after applying
% the renaming, region variables are regular Mercury variables.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.rbmm.region_resurrection_renaming.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_instruction.
:- import_module transform_hlds.rbmm.region_liveness_info.
:- import_module transform_hlds.smm_common.

:- import_module list.
:- import_module map.
:- import_module multi_map.

%---------------------------------------------------------------------------%

:- type rbmm_renaming_table ==
    map(pred_proc_id, rbmm_renaming_proc).

:- type rbmm_renaming_proc ==
    map(program_point, rbmm_renaming).

    % Most of the time, at a program point there is only one renaming for a
    % region variable. But at some resurrection points, two renamings exists
    % for the resurrecting region. This should happen only in the following
    % case:
    %       remove(R), % R -> R_Resur_1
    %       create(R), % R -> R_Resur_2
    %   (i) goal
    % That's why multi_map is used.
:- type rbmm_renaming == multi_map(string, string).

:- type rbmm_renaming_annotation_table ==
    map(pred_proc_id, rbmm_renaming_annotation_proc).

:- type rbmm_renaming_annotation_proc ==
    map(program_point, list(region_instr)).

:- type proc_resurrection_path_table ==
    map(pred_proc_id, exec_path_region_set_table).

:- type exec_path_region_set_table == map(execution_path, region_set).

:- type join_point_region_name_table ==
    map(pred_proc_id, map(program_point, string)).

    % This predicate traveses execution paths and detects ones in which
    % resurrections of regions happen.
    % For such an execution path it also calculates the regions which
    % resurrect. Only procedures which contain resurrection are kept in the
    % results. And for such procedures only execution paths that contain
    % resurrection are kept.
    %
:- pred compute_resurrection_paths(execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_resurrection_path_table::out) is det.

    % Collect join points in procedures.
    % The purpose of finding join points in a procedure is because if a region
    % is given different names in different execution paths leading to a join
    % point, we need to unify those names at the join point.
    % For this purpose, we will only collect join points in execution paths
    % in which resurrection happens (i.e., the output table of by the above
    % pass).
    %
    % A program point is a join point if it is in at least two execution
    % paths and its previous points in some two execution paths are different.
    %
:- pred collect_join_points(proc_resurrection_path_table::in,
    execution_path_table::in, join_point_region_name_table::out) is det.

    % This predicate find the execution paths in which we need to introduce
    % resurrection renaming. These paths include
    % 1) those in which resurrection happens (computed by
    % compute_resurrection_paths),
    % 2) and those in which resurrection does not happens but contain the join
    % points that belong to those in 1). These can be seen as they share a
    % program point with those in 1). We need to care about those paths
    % because in such an execution path, we also need to rename a resurrected
    % region (in the paths in 1)) so that at the join point it can be renamed
    % again to the unified one. If we keep the original name we will have
    % problem if the region is an output and therefore a renaming to the
    % original name will be introduced after the last program point.
    %
:- pred collect_paths_containing_join_points(execution_path_table::in,
    join_point_region_name_table::in, proc_resurrection_path_table::in,
    proc_resurrection_path_table::out) is det.

    % This predicate only traverses the execution paths in which resurrection
    % renaming is needed, i.e., those in the output table of the previous
    % pass. It computes *renaming* at the points where a resurrected region
    % becomes live.
    % The result here will also only contain procedures in which resurrection
    % happens and for each procedure only execution paths in which
    % resurrection happens.
    %
:- pred collect_region_resurrection_renaming(proc_pp_region_set_table::in,
    proc_region_set_table::in, rpta_info_table::in,
    proc_resurrection_path_table::in, rbmm_renaming_table::out) is det.

    % This predicate collects *renaming* along the execution paths in
    % procedures where region resurrection happens. It also computes
    % the reversed renaming *annotations* to ensure the integrity use of
    % regions.
    %
:- pred collect_renaming_and_annotation(rbmm_renaming_table::in,
    join_point_region_name_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    rpta_info_table::in, proc_resurrection_path_table::in,
    execution_path_table::in, rbmm_renaming_annotation_table::out,
    rbmm_renaming_table::out) is det.

    % Record the annotation for a procedure.
    %
:- pred record_annotation(program_point::in, region_instr::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::out)
    is det.

    % Make a region renaming instruction.
    %
:- pred make_renaming_instruction(string::in, string::in,
    region_instr::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module transform_hlds.rbmm.points_to_graph.

:- import_module assoc_list.
:- import_module counter.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

    % This predicate traveses execution paths and detects ones in which
    % resurrections of regions happen.
    % For such an execution path it also calculates the regions which
    % resurrect. Only procedures which contain resurrection are kept in the
    % results. And for such procedures only execution paths that contain
    % resurrection are kept.
    %
compute_resurrection_paths(ExecPathTable, LRBeforeTable, LRAfterTable,
        BornRTable, DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable,
        PathContainsResurrectionTable) :-
    map.foldl(compute_resurrection_paths_proc(LRBeforeTable, LRAfterTable,
        BornRTable, DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable),
        ExecPathTable, map.init, PathContainsResurrectionTable).

:- pred compute_resurrection_paths_proc(proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, pred_proc_id::in, list(execution_path)::in,
    proc_resurrection_path_table::in, proc_resurrection_path_table::out)
    is det.

compute_resurrection_paths_proc(LRBeforeTable, LRAfterTable,
        BornRTable, DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable,
        PPId, ExecPaths, !PathContainsResurrectionTable) :-
    map.lookup(LRBeforeTable, PPId, LRBeforeProc),
    map.lookup(LRAfterTable, PPId, LRAfterProc),
    map.lookup(BornRTable, PPId, _BornR),
    map.lookup(DeadRTable, PPId, _DeadR),
    map.lookup(LocalRTable, PPId, _LocalR),
    map.lookup(BecomeLiveTable, PPId, BecomeLiveProc),
    map.lookup(BecomeDeadBeforeTable, PPId, BecomeDeadBeforeProc),
    map.lookup(BecomeDeadAfterTable, PPId, BecomeDeadAfterProc),
    list.foldl(compute_resurrection_paths_exec_path(LRBeforeProc, LRAfterProc,
        BecomeLiveProc, BecomeDeadBeforeProc, BecomeDeadAfterProc), ExecPaths,
        map.init, PathContainsResurrectionProc),
    % We only want to include procedures in which resurrection happens in this
    % map.
    ( if map.count(PathContainsResurrectionProc) = 0 then
        true
    else
        map.set(PPId, PathContainsResurrectionProc,
            !PathContainsResurrectionTable)
    ).

:- pred compute_resurrection_paths_exec_path(pp_region_set_table::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, pp_region_set_table::in, execution_path::in,
    exec_path_region_set_table::in, exec_path_region_set_table::out) is det.

compute_resurrection_paths_exec_path(LRBeforeProc, LRAfterProc,
        BecomeLiveProc, BecomeDeadBeforeProc, BecomeDeadAfterProc,
        ExecPath, !ResurrectedRegionProc) :-
    list.foldl3(compute_resurrection_paths_prog_point(LRBeforeProc,
        LRAfterProc, BecomeLiveProc, BecomeDeadBeforeProc,
        BecomeDeadAfterProc), ExecPath,
        set.init, _, set.init, _, set.init, ResurrectedRegionsInExecPath),
    % We want to record only execution paths in which resurrections happen.
    ( if set.is_empty(ResurrectedRegionsInExecPath) then
        true
    else
        map.set(ExecPath, ResurrectedRegionsInExecPath,
            !ResurrectedRegionProc)
    ).

:- pred compute_resurrection_paths_prog_point(pp_region_set_table::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pair(program_point, hlds_goal)::in, region_set::in, region_set::out,
    region_set::in, region_set::out, region_set::in, region_set::out) is det.

compute_resurrection_paths_prog_point(LRBeforeProc, LRAfterProc,
        BecomeLiveProc, BecomeDeadBeforeProc, BecomeDeadAfterProc,
        ProgPoint - _, !CreatedCandidates, !RemovedCandidates,
        !ResurrectedRegionsInExecPath) :-
    map.lookup(LRBeforeProc, ProgPoint, _LRBeforeProgPoint),
    map.lookup(LRAfterProc, ProgPoint, _LRAfterProgPoint),
    map.lookup(BecomeLiveProc, ProgPoint, BecomeLiveProgPoint),
    map.lookup(BecomeDeadBeforeProc, ProgPoint, BecomeDeadBeforeProgPoint),
    map.lookup(BecomeDeadAfterProc, ProgPoint, BecomeDeadAfterProgPoint),
    set.union(BecomeDeadAfterProgPoint, BecomeDeadBeforeProgPoint,
        BecomeDeadAtProgPoint),

    % Resurrected regions:
    % either become live at more than one program point
    % or become dead at more than one program point in an execution path.
    set.intersect(!.CreatedCandidates, BecomeLiveProgPoint,
        CreatedResurrectedRegions),
    set.union(CreatedResurrectedRegions, !ResurrectedRegionsInExecPath),

    set.intersect(!.RemovedCandidates, BecomeDeadAtProgPoint,
        RemovedResurrectedRegions),
    set.union(RemovedResurrectedRegions, !ResurrectedRegionsInExecPath),

    % When a region is known to become live or become dead
    % at one program point, it is considered a candidate for resurrection.
    set.difference(set.union(!.CreatedCandidates, BecomeLiveProgPoint),
        !.ResurrectedRegionsInExecPath, !:CreatedCandidates),

    set.difference(set.union(!.RemovedCandidates, BecomeDeadAtProgPoint),
        !.ResurrectedRegionsInExecPath, !:RemovedCandidates).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % We will only collect join points in procedures where resurrection
    % happens, therefore use the PathContainsResurrectionTable just for the
    % PPIds of such procedures.
    %
    % The new region name at a join point is formed by RegionName_jp_Number.
    % If a region needs new names at several join points then Number will
    % make the new names distinct.
    %
collect_join_points(PathContainsResurrectionTable, ExecPathTable,
        JoinPointTable) :-
    map.foldl(collect_join_points_proc(ExecPathTable),
        PathContainsResurrectionTable, map.init, JoinPointTable).

:- pred collect_join_points_proc(execution_path_table::in,
    pred_proc_id::in, exec_path_region_set_table::in,
    join_point_region_name_table::in,
    join_point_region_name_table::out) is det.

collect_join_points_proc(ExecPathTable, PPId, _, !JoinPointTable) :-
    map.lookup(ExecPathTable, PPId, ExecPaths),
    list.foldr(
        pred(ExecPath::in, Ps0::in, Ps::out) is det :- (
            assoc_list.keys(ExecPath, P),
            Ps = [P | Ps0]),
        ExecPaths, [], Paths),
    list.foldl4(collect_join_points_path(Paths), Paths, map.init, _,
        counter.init(0), _, set.init, _JoinPoints, map.init, JoinPointProc),
    map.set(PPId, JoinPointProc, !JoinPointTable).

:- pred collect_join_points_path(list(list(program_point))::in,
    list(program_point)::in,
    map(program_point, string)::in, map(program_point, string)::out,
    counter::in, counter::out,
    set(program_point)::in, set(program_point)::out,
    map(program_point, string)::in, map(program_point, string)::out) is det.

collect_join_points_path(Paths, Path, !JP2Name, !Counter, !JoinPoints,
        !JoinPointProc) :-
    list.delete_all(Paths, Path, TheOtherPaths),
    % We ignore the first program point in each path because
    % it cannot be a join point.
    ( if Path = [PrevPoint, ProgPoint | ProgPoints] then
        ( if set.member(ProgPoint, !.JoinPoints) then
            true
        else
            ( if is_join_point(ProgPoint, PrevPoint, TheOtherPaths)then
                % Try to lookup the postfix at this jp.
                ( if map.search(!.JP2Name, PrevPoint, JPName0)then
                    JPName = JPName0
                else
                    counter.allocate(N, !Counter),
                    JPName = "_jp_" ++ string.int_to_string(N),
                    map.set(PrevPoint, JPName, !JP2Name)
                ),
                map.set(ProgPoint, JPName, !JoinPointProc),
                set.insert(ProgPoint, !JoinPoints)
            else
                true
            )
        ),
        collect_join_points_path(Paths, [ProgPoint | ProgPoints],
            !JP2Name, !Counter, !JoinPoints, !JoinPointProc)
    else
        true
    ).

    % This predicate succeeds if the first program point is a join point.
    % That means it is at least in another execution path and is preceded
    % by some program point, which is different from the second one.
    %
:- pred is_join_point(program_point::in, program_point::in,
    list(list(program_point))::in) is semidet.

is_join_point(ProgPoint, PrevProgPoint, [Path | Paths]) :-
    ( if is_join_point_2(ProgPoint, PrevProgPoint, Path)then
        true
    else
        is_join_point(ProgPoint, PrevProgPoint, Paths)
    ).

:- pred is_join_point_2(program_point::in, program_point::in,
    list(program_point)::in) is semidet.

is_join_point_2(ProgPoint, PrevProgPoint, [P1, P2 | Ps]) :-
    ( if P2 = ProgPoint then
        P1 \= PrevProgPoint
    else
        is_join_point_2(ProgPoint, PrevProgPoint, [P2 | Ps])
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

collect_paths_containing_join_points(ExecPathTable, JoinPointTable,
        !PathContainsResurrectionTable) :-
    map.foldl(
        collect_paths_containing_join_points_proc(ExecPathTable,
            JoinPointTable),
        !.PathContainsResurrectionTable, !PathContainsResurrectionTable).

:- pred collect_paths_containing_join_points_proc(execution_path_table::in,
    join_point_region_name_table::in, pred_proc_id::in,
    exec_path_region_set_table::in, proc_resurrection_path_table::in,
    proc_resurrection_path_table::out) is det.

collect_paths_containing_join_points_proc(ExecPathTable, JoinPointTable, PPId,
        PathContainsResurrectionProc, !PathContainsResurrectionTable) :-
    map.lookup(ExecPathTable, PPId, ExecPaths),
    map.values(PathContainsResurrectionProc, ResurrectedRegionsInPaths),
    list.foldl(
        ( pred(ResurRegions::in, R0::in, R::out) is det :-
            set.union(R0, ResurRegions, R)
        ), ResurrectedRegionsInPaths, set.init, ResurrectedRegionsProc),
    map.keys(PathContainsResurrectionProc, PathsContainResurrection),
    list.delete_elems(ExecPaths, PathsContainResurrection, NonResurPaths),
    ( if map.search(JoinPointTable, PPId, JoinPointProc) then
        list.foldl(path_containing_join_point(JoinPointProc, PPId,
            ResurrectedRegionsProc),
            NonResurPaths, !PathContainsResurrectionTable)
    else
        true
    ).

:- pred path_containing_join_point(map(program_point, string)::in,
    pred_proc_id::in, set(rptg_node)::in, execution_path::in,
    proc_resurrection_path_table::in, proc_resurrection_path_table::out)
    is det.

path_containing_join_point(JoinPointProc, PPId, ResurrectedRegionsProc,
        NonResurPath, !PathContainsResurrectionTable) :-
    assoc_list.keys(NonResurPath, ProgPointsInPath),
    map.foldl(find_join_points_in_path(ProgPointsInPath), JoinPointProc,
        set.init, JoinPointsInThisPath),
    ( if set.is_empty(JoinPointsInThisPath) then
        true
    else
        map.lookup(!.PathContainsResurrectionTable, PPId,
            PathContainsResurrectionProc0),
        map.det_insert(NonResurPath, ResurrectedRegionsProc,
            PathContainsResurrectionProc0, PathContainsResurrectionProc),
        map.set(PPId, PathContainsResurrectionProc,
            !PathContainsResurrectionTable)
    ).

:- pred find_join_points_in_path(list(program_point)::in,
    program_point::in, string::in, set(program_point)::in,
    set(program_point)::out) is det.

find_join_points_in_path(ProgPointsInPath, JoinPoint, _, !JoinPoints) :-
    ( if list.member(JoinPoint, ProgPointsInPath) then
        set.insert(JoinPoint, !JoinPoints)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

collect_region_resurrection_renaming(BecomeLiveTable, LocalRTable,
        RptaInfoTable, PathContainsResurrectionTable,
        ResurrectionRenameTable) :-
    map.foldl(collect_region_resurrection_renaming_proc(BecomeLiveTable,
        LocalRTable, RptaInfoTable), PathContainsResurrectionTable,
        map.init, ResurrectionRenameTable).

:- pred collect_region_resurrection_renaming_proc(
    proc_pp_region_set_table::in, proc_region_set_table::in,
    rpta_info_table::in, pred_proc_id::in, map(execution_path, region_set)::in,
    rbmm_renaming_table::in, rbmm_renaming_table::out) is det.

collect_region_resurrection_renaming_proc(BecomeLiveTable, _LocalRTable,
        RptaInfoTable, PPId, PathsContainResurrection,
        !ResurrectionRenameTable) :-
    map.lookup(BecomeLiveTable, PPId, BecomeLiveProc),
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    RptaInfo = rpta_info(Graph, _),
    map.foldl(
        collect_region_resurrection_renaming_exec_path(Graph,
            BecomeLiveProc),
        PathsContainResurrection, map.init, ResurrectionRenameProc),
    map.set(PPId, ResurrectionRenameProc, !ResurrectionRenameTable).

:- pred collect_region_resurrection_renaming_exec_path(rpt_graph::in,
    pp_region_set_table::in, execution_path::in, region_set::in,
    rbmm_renaming_proc::in, rbmm_renaming_proc::out) is det.

collect_region_resurrection_renaming_exec_path(Graph, BecomeLiveProc,
        ExecPath, ResurrectedRegions, !ResurrectionRenameProc) :-
    list.foldl2(
        collect_region_resurrection_renaming_prog_point(Graph,
            BecomeLiveProc, ResurrectedRegions),
        ExecPath, counter.init(0), _, !ResurrectionRenameProc).

:- pred collect_region_resurrection_renaming_prog_point(rpt_graph::in,
    pp_region_set_table::in, region_set::in,
    pair(program_point, hlds_goal)::in, counter::in, counter::out,
    rbmm_renaming_proc::in, rbmm_renaming_proc::out) is det.

collect_region_resurrection_renaming_prog_point(Graph, BecomeLiveProc,
        ResurrectedRegions, ProgPoint - _, !RenamingCounter,
        !ResurrectionRenameProc) :-
    map.lookup(BecomeLiveProc, ProgPoint, BecomeLiveProgPoint),
    set.intersect(ResurrectedRegions, BecomeLiveProgPoint,
        ToBeRenamedRegions),
    % We only record the program points where resurrection renaming exists.
    ( if set.is_empty(ToBeRenamedRegions) then
        true
    else
        counter.allocate(N, !RenamingCounter),
        set.fold(
            record_renaming_prog_point(Graph, ProgPoint, N),
            ToBeRenamedRegions, !ResurrectionRenameProc)
    ).

:- pred record_renaming_prog_point(rpt_graph::in, program_point::in, int::in,
    rptg_node::in, rbmm_renaming_proc::in, rbmm_renaming_proc::out) is det.

record_renaming_prog_point(Graph, ProgPoint, RenamingCounter, Region,
        !ResurrectionRenameProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    Renamed = RegionName ++ "_Resur_"
        ++ string.int_to_string(RenamingCounter),
    ( if
        map.search(!.ResurrectionRenameProc, ProgPoint, RenamingProgPoint0)
    then
        map.set(RegionName, [Renamed], RenamingProgPoint0, RenamingProgPoint)
    else
        RenamingProgPoint = map.singleton(RegionName, [Renamed])
    ),
    map.set(ProgPoint, RenamingProgPoint, !ResurrectionRenameProc).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
%
% Collect renaming at each program point.
%
% A renaming at a program point will be applied to annotations attached
% to before and after it. If the associated (atomic) goal is a procedure call
% then the renaming is also applied to its actual region arguments. If the
% goal is a construction the renaming is applied to the regions of the left
% variable.

collect_renaming_and_annotation(ResurrectionRenameTable, JoinPointTable,
        LRBeforeTable, LRAfterTable, BornRTable, RptaInfoTable,
        ResurrectionPathTable, ExecPathTable,
        AnnotationTable, RenamingTable) :-
    map.foldl2(collect_renaming_and_annotation_proc(ExecPathTable,
        JoinPointTable, LRBeforeTable, LRAfterTable, BornRTable, RptaInfoTable,
        ResurrectionPathTable), ResurrectionRenameTable,
        map.init, AnnotationTable, map.init, RenamingTable).

:- pred collect_renaming_and_annotation_proc(execution_path_table::in,
    join_point_region_name_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    rpta_info_table::in, proc_resurrection_path_table::in, pred_proc_id::in,
    rbmm_renaming_proc::in,
    rbmm_renaming_annotation_table::in, rbmm_renaming_annotation_table::out,
    rbmm_renaming_table::in, rbmm_renaming_table::out) is det.

collect_renaming_and_annotation_proc(ExecPathTable, JoinPointTable,
        LRBeforeTable, LRAfterTable, BornRTable, RptaInfoTable,
        ResurrectionPathTable, PPId, ResurrectionRenameProc,
        !AnnotationTable, !RenamingTable) :-
    map.lookup(JoinPointTable, PPId, JoinPointProc),
    map.lookup(LRBeforeTable, PPId, LRBeforeProc),
    map.lookup(LRAfterTable, PPId, LRAfterProc),
    map.lookup(BornRTable, PPId, BornR),
    map.lookup(RptaInfoTable, PPId, RptaInfo),
    RptaInfo = rpta_info(Graph, _),
    % Here we find all regions which resurrects in this procedure.
    % This information is used at a join point to introduce renamings
    % for all resurrecting regions that become live at the join point.
    map.lookup(ResurrectionPathTable, PPId, PathsContainResurrection),
    map.values(PathsContainResurrection, ResurrectedRegionsInPaths),
    list.foldl(
        pred(ResurRegions::in, R0::in, R::out) is det :- (
            set.union(R0, ResurRegions, R)
        ),
        ResurrectedRegionsInPaths, set.init, ResurrectedRegionsProc),
    map.lookup(ExecPathTable, PPId, ExecPaths),
    list.foldl2(collect_renaming_and_annotation_exec_path(
        ResurrectionRenameProc, JoinPointProc, LRBeforeProc, LRAfterProc,
        BornR, Graph, ResurrectedRegionsProc), ExecPaths,
        map.init, AnnotationProc, map.init, RenamingProc),
    map.set(PPId, AnnotationProc, !AnnotationTable),
    map.set(PPId, RenamingProc, !RenamingTable).

    % The renaming along an execution path is built up. Let's see an
    % example of renamings.
    % (1) R1 --> R1_1   // i.e., R1 resurrects and therefore needs renaming.
    % (2) R1 --> R1_1, R2 --> R2_1
    % (3) R1 --> R1_2, R2 --> R2_1 //R1 becomes live again, needs a new name.
    % ...
    %
:- pred collect_renaming_and_annotation_exec_path(rbmm_renaming_proc::in,
    map(program_point, string)::in, pp_region_set_table::in,
    pp_region_set_table::in, region_set::in, rpt_graph::in, region_set::in,
    execution_path::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::out,
    rbmm_renaming_proc::in, rbmm_renaming_proc::out) is det.

collect_renaming_and_annotation_exec_path(_, _, _, _, _, _, _, [],
        !AnnotationProc, !RenamingProc) :-
    unexpected($pred, "empty execution path").
collect_renaming_and_annotation_exec_path(ResurrectionRenameProc,
        JoinPointProc, LRBeforeProc, LRAfterProc, BornR, Graph,
        ResurrectedRegions, [ProgPoint - _ | ProgPoint_Goals],
        !AnnotationProc, !RenamingProc) :-
    % This is the first program point in an execution path.
    % It cannot be a join point. Renaming is needed at this point only
    % when it is a resurrection point.
    %
    ( if map.search(ResurrectionRenameProc, ProgPoint, ResurRename) then
        map.set(ProgPoint, ResurRename, !RenamingProc)
    else
        map.set(ProgPoint, map.init, !RenamingProc)
    ),
    collect_renaming_and_annotation_exec_path_2(ResurrectionRenameProc,
        JoinPointProc, LRBeforeProc, LRAfterProc, BornR, Graph,
        ResurrectedRegions, ProgPoint, ProgPoint_Goals, !AnnotationProc,
        !RenamingProc).

:- pred collect_renaming_and_annotation_exec_path_2(rbmm_renaming_proc::in,
    map(program_point, string)::in, pp_region_set_table::in,
    pp_region_set_table::in,
    region_set::in, rpt_graph::in, region_set::in, program_point::in,
    execution_path::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::out,
    rbmm_renaming_proc::in, rbmm_renaming_proc::out) is det.

collect_renaming_and_annotation_exec_path_2(_, _, _, _, _, _, _, _, [],
        !AnnotationProc, !RenamingProc).
    % This means the first program point is also the last.
    % We do not need to do anything more.
collect_renaming_and_annotation_exec_path_2(ResurrectionRenameProc,
        JoinPointProc, LRBeforeProc, LRAfterProc, BornR, Graph,
        ResurrectedRegions, PrevProgPoint, [ProgPoint - _ | ProgPoint_Goals],
        !AnnotationProc, !RenamingProc) :-
    % This is a program point which is not the first.
    %
    % A program point can belong to different execution paths, therefore
    % it can be processed more than once. If a program point is a
    % *join point*, the process in later execution paths may add new
    % renaming information about some resurrected region(s) which does not
    % resurrect in the already-processed paths covering this program point.
    % To avoid updating information related to the already-processed paths
    % whenever we process a join point we include the information about ALL
    % resurrected regions that are live before the join point. This will
    % ensure that no new renaming information arises when the program
    % point is processed again.
    %
    % At a join point, we need to add suitable annotations to the previous
    % point, i.e., ones related to the renaming at the previous point.
    %
    % At the last program point, we need to add annotations for any region
    % parameters which resurrect.
    %
    map.lookup(!.RenamingProc, PrevProgPoint, PrevRenaming),
    ( if map.search(ResurrectionRenameProc, ProgPoint, ResurRenaming) then
        % This is a resurrection point of some region(s). We need to merge
        % the existing renaming at the previous point with the resurrection
        % renaming here. When two renamings have the same key, i.e.,
        % the related region resurrects, we will keep both renamings.
        multi_map.merge(PrevRenaming, ResurRenaming, Renaming0),
        map.set(ProgPoint, Renaming0, !RenamingProc)
    else
        % This is not a resurrection point (of any regions).
        % Renaming at this point is the same as at its previous point.
        map.set(ProgPoint, PrevRenaming, !RenamingProc)
    ),
    ( if map.search(JoinPointProc, ProgPoint, JoinPointName) then
        % This is a join point.
        % Add annotations to the previous point.
        map.lookup(LRBeforeProc, ProgPoint, LRBeforeProgPoint),
        map.lookup(LRAfterProc, PrevProgPoint, LRAfterPrevProgPoint),
        % Not yet dead in the sense that the region is still needed to be
        % removed.
        set.union(LRBeforeProgPoint, LRAfterPrevProgPoint, NotYetDeadRegions),
        set.intersect(ResurrectedRegions, NotYetDeadRegions,
            ResurrectedAndLiveRegions),
        set.fold2(
            add_annotation_and_renaming_at_join_point(PrevProgPoint, Graph,
                JoinPointName, PrevRenaming),
            ResurrectedAndLiveRegions, !AnnotationProc, map.init, Renaming),
        % We will just overwrite any existing renaming information
        % at this point.
        map.set(ProgPoint, Renaming, !RenamingProc)
    else
        true
    ),
    (
        % This is the last program point in this execution path.
        ProgPoint_Goals = [],
        % Add reversed renaming for regions in bornR.
        set.intersect(ResurrectedRegions, BornR, ResurrectedAndBornRegions),
        map.lookup(!.RenamingProc, ProgPoint, LastRenaming),
        set.fold(
            add_annotation_at_last_prog_point(ProgPoint, Graph,
                LastRenaming),
            ResurrectedAndBornRegions, !AnnotationProc)
    ;
        ProgPoint_Goals = [_ | _],
        collect_renaming_and_annotation_exec_path_2(ResurrectionRenameProc,
            JoinPointProc, LRBeforeProc, LRAfterProc, BornR, Graph,
            ResurrectedRegions, ProgPoint, ProgPoint_Goals, !AnnotationProc,
            !RenamingProc)
    ).

    % This predicate adds renaming annotation after the previous program
    % point and records renaming from existing region name.
    %
:- pred add_annotation_and_renaming_at_join_point(program_point::in,
    rpt_graph::in, string::in, rbmm_renaming::in, rptg_node::in,
    rbmm_renaming_annotation_proc::in, rbmm_renaming_annotation_proc::out,
    rbmm_renaming::in, rbmm_renaming::out) is det.

add_annotation_and_renaming_at_join_point(PrevProgPoint, Graph, JoinPointName,
        PrevRenaming, Region, !AnnotationProc, !Renaming) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    NewName = RegionName ++ JoinPointName,

    % Record renaming at the join point if it doesn't exist yet.
    map.det_insert(RegionName, [NewName], !Renaming),

    % Add annotation to (after) the previous program point.
    % XXX Annotations are only added for resurrected regions that have been
    % renamed in this execution path (i.e., the execution path contains
    % PrevProgPoint and ProgPoint).
    % It seems that we have to add annotations (reverse renaming) for ones that
    % have not been renamed as implemented below too. The only difference is
    % that the reverse renaming is between the new name and the original name.
    ( if map.search(PrevRenaming, RegionName, RenamedNames) then
        list.det_last(RenamedNames, CurrentName),
        make_renaming_instruction(CurrentName, NewName, Annotation),
        record_annotation(PrevProgPoint, Annotation, !AnnotationProc)
    else
        make_renaming_instruction(RegionName, NewName, Annotation),
        record_annotation(PrevProgPoint, Annotation, !AnnotationProc)
    ).

:- pred add_annotation_at_last_prog_point(program_point::in, rpt_graph::in,
    rbmm_renaming::in, rptg_node::in, rbmm_renaming_annotation_proc::in,
    rbmm_renaming_annotation_proc::out) is det.

add_annotation_at_last_prog_point(ProgPoint, Graph, Renaming, Region,
        !AnnotationProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),

    % Add annotation to (after) the program point.
    % Annotations are only added for resurrected regions that have been
    % renamed in this execution path.
    ( if map.search(Renaming, RegionName, CurrentNameList) then
        CurrentName = list.det_last(CurrentNameList),
        make_renaming_instruction(CurrentName, RegionName, Annotation),
        record_annotation(ProgPoint, Annotation, !AnnotationProc)
    else
        true
    ).

record_annotation(ProgPoint, Annotation, !AnnotationProc) :-
    ( if map.search(!.AnnotationProc, ProgPoint, Annotations0) then
        ( if list.member(Annotation, Annotations0) then
            Annotations = Annotations0
        else
            Annotations = [Annotation | Annotations0]
        )
    else
        % No annotation exists at this program point yet.
        Annotations = [Annotation]
    ),
    map.set(ProgPoint, Annotations, !AnnotationProc).

make_renaming_instruction(OldRegionName, NewRegionName, RenameInstruction) :-
    RenameInstruction = rename_region(OldRegionName, NewRegionName).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.region_resurrection_renaming.
%---------------------------------------------------------------------------%
