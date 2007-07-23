%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File rbmm.region_instruction.m.
% Main author: Quan Phan
%
% This module implements the process of introducing region instructions to
% each program point in a procedure based on its region points-to graph and
% live region information.
%

:- module transform_hlds.rbmm.region_instruction.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.rbmm.points_to_info.
:- import_module transform_hlds.rbmm.region_liveness_info.
:- import_module transform_hlds.smm_common.

:- import_module list.
:- import_module map.
:- import_module string.

:- type region_instruction_table
    ==      map(pred_proc_id, region_instruction_proc).

:- type region_instruction_proc
    ==      map(program_point, instructions_before_after).

    % We introduce region instructions before and after a program point.
:- type instructions_before_after
    --->    instructions_before_after(
                instructions_before    :: list(region_instruction),
                                        % Region instructions before a program
                                        % point.

                instructions_after     :: list(region_instruction)
                                        % Region instructions after a program
                                        % point.
            ).

:- type region_instruction
    --->    create_region(
                % Region name.
                string
            )

    ;       remove_region(
                % Region name.
                string
            )

    ;       rename_region(
                % Old region name.
                string,

                % New region name.
                string
            ).

:- type region_instruction_type
    --->    create_region_instruction
    ;       remove_region_instruction
    ;       renaming_region_instruction.

:- pred introduce_region_instructions(module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, region_instruction_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.goal_path.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module transform_hlds.rbmm.points_to_graph.

:- import_module bool.
:- import_module pair.
:- import_module set.
:- import_module svmap.
:- import_module svset.
:- import_module term.
:- import_module varset.

introduce_region_instructions(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable, RegionInstructionTable) :-
    module_info_predids(PredIds, ModuleInfo, _),
    map.init(RegionInstructionTable0),
    list.foldl(introduce_region_instructions_pred(ModuleInfo,
        RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
        VoidVarRegionTable, BornRTable, DeadRTable, LocalRTable), PredIds,
        RegionInstructionTable0, RegionInstructionTable).

:- pred introduce_region_instructions_pred(module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, pred_id::in,
    region_instruction_table::in, region_instruction_table::out) is det.

introduce_region_instructions_pred(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable, PredId, !RegionInstructionTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_non_imported_procids(PredInfo) = ProcIds,
    list.foldl(introduce_region_instructions_proc(ModuleInfo, PredId,
        RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
        VoidVarRegionTable, BornRTable, DeadRTable, LocalRTable),
        ProcIds, !RegionInstructionTable).

:- pred introduce_region_instructions_proc(module_info::in, pred_id::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, proc_id::in,
    region_instruction_table::in, region_instruction_table::out) is det.

introduce_region_instructions_proc(ModuleInfo, PredId, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, VoidVarRegionTable,
        BornRTable, DeadRTable, LocalRTable, ProcId,
        !RegionInstructionTable) :-
    PPId = proc(PredId, ProcId),
    ( some_are_special_preds([PPId], ModuleInfo) ->
        true
    ;
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        map.lookup(RptaInfoTable, PPId, RptaInfo),
        map.lookup(BornRTable, PPId, BornR),
        map.lookup(DeadRTable, PPId, DeadR),
        map.lookup(LocalRTable, PPId, LocalR),
        map.lookup(LRBeforeTable, PPId, ProcLRBefore),
        map.lookup(LRAfterTable, PPId, ProcLRAfter),
        map.lookup(VoidVarRegionTable, PPId, ProcVoidVarRegion),
        map.lookup(ExecPathTable, PPId, ExecPaths),
        introduce_region_instructions_exec_paths(ExecPaths, RptaInfo,
            BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter,
            ProcVoidVarRegion, BornRTable, DeadRTable, ModuleInfo, ProcInfo,
            map.init, RegionInstructionProc),
        svmap.set(PPId, RegionInstructionProc, !RegionInstructionTable)
    ).

    % Follow each execution path of a procedure and introduce
    % region instructions at each program point.
    %
:- pred introduce_region_instructions_exec_paths(list(execution_path)::in,
    rpta_info::in, set(rptg_node)::in, set(rptg_node)::in, set(rptg_node)::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

introduce_region_instructions_exec_paths([], _, _, _, _, _, _, _, _, _, _, _,
        !RegionInstructionProc).
introduce_region_instructions_exec_paths([ExecPath|ExecPaths], RptaInfo,
        BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        !RegionInstructionProc) :-
    introduce_region_instructions_exec_path(ExecPath, RptaInfo, BornR,
        DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !RegionInstructionProc),
    introduce_region_instructions_exec_paths(ExecPaths, RptaInfo, BornR,
        DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, !RegionInstructionProc).

:- pred introduce_region_instructions_exec_path(execution_path::in,
    rpta_info::in, set(rptg_node)::in, set(rptg_node)::in, set(rptg_node)::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

introduce_region_instructions_exec_path([], _, _, _, _, _, _, _, _, _, _, _,
        !RegionInstructionProc).
introduce_region_instructions_exec_path([ProgPoint - Goal | ProgPoint_Goals],
        RptaInfo, BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter,
        ProcVoidVarRegion, BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        !RegionInstructionProc) :-
    map.lookup(ProcLRBefore, ProgPoint, LRBefore),
    map.lookup(ProcLRAfter, ProgPoint, LRAfter),
    map.lookup(ProcVoidVarRegion, ProgPoint, VoidVarRegions),

    % Because void variables at this program point are considered dead
    % right after they get bound, the regions that are reached from them
    % are also candidates to be removed. They will be destroyed after this
    % program point if they are not live after the program point and they
    % are either in DeadR or LocalR or BornR.
    set.difference(VoidVarRegions, LRAfter, DeadVoidVarRegions0),
    set.union(LocalR, BornR, Local_Born),
    set.union(Local_Born, DeadR, Local_Born_Dead),
    set.intersect(Local_Born_Dead, DeadVoidVarRegions0, DeadVoidVarRegions),
    RptaInfo = rpta_info(CallerGraph, _),
    set.fold(
        record_instruction_after_prog_point(remove_region_instruction,
            ProgPoint, CallerGraph),
        DeadVoidVarRegions, !RegionInstructionProc),

    set.difference(LRBefore, LRAfter, ToBeRemoved),
    set.intersect(ToBeRemoved, Local_Born_Dead, ToBeRemovedAndAllowed),
    ( Goal = hlds_goal(switch(_, _, _), _) ->
        % This is a switch, i.e. unification, only rule 4 applied.
        transformation_rule_4_2(ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, !RegionInstructionProc)
    ;
        Goal = hlds_goal(Expr, _),
        set.difference(LRAfter, LRBefore, ToBeCreated),
        set.intersect(ToBeCreated, Local_Born, ToBeCreatedAndAllowed),
        transformation_rule_1(Expr, ProgPoint, ToBeCreatedAndAllowed,
            RptaInfo, BornRTable, !RegionInstructionProc),
        transformation_rule_2(Expr, ProgPoint, ToBeCreatedAndAllowed,
            RptaInfo, ModuleInfo, ProcInfo, !RegionInstructionProc),
        transformation_rule_3(Expr, ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, DeadRTable, !RegionInstructionProc),
        transformation_rule_4(Expr, ProgPoint, ToBeRemovedAndAllowed,
            RptaInfo, !RegionInstructionProc)
    ),

    (
        ProgPoint_Goals = [NextProgPoint - _ | _],
        % Transformation rule T5:
        % When a region is live after a program point but not live before the
        % next program point in the same execution path, it will be removed
        % before the next program point if the current procedure is allowed
        % to remove it.
        map.lookup(ProcLRBefore, NextProgPoint, LRBeforeNext),
        set.difference(LRAfter, LRBeforeNext, ToBeRemovedBeforeNext),
        set.intersect(Local_Born_Dead, ToBeRemovedBeforeNext,
            ToBeRemovedBeforeNextAndAllowed),
        set.fold(record_instruction_before_prog_point(remove_region_instruction,
            NextProgPoint, CallerGraph), ToBeRemovedBeforeNextAndAllowed,
            !RegionInstructionProc),

        introduce_region_instructions_exec_path(ProgPoint_Goals, RptaInfo,
            BornR, DeadR, LocalR,
            ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
            BornRTable, DeadRTable, ModuleInfo, ProcInfo,
            !RegionInstructionProc)
    ;
        % This is the last program point, we finish.
        ProgPoint_Goals = []
    ).

    % Rule 1: if an output region of q is not live, not created by q,
    % and p is allowed to create it, it is created before the call to q.
    %
    % There are two cases: either q is defined in this module or q is
    % imported.
    % The former is dealt with as per the rule.
    % The latter: for now, with the assumption that all source code is in
    % only one module, imported preds will only be ones from
    % Mercury's library. We do not intent to deal with the library's code
    % now therefore we have to assume here that the caller will always
    % *CREATE* the OUTPUT REGION for those procedures.
    %
:- pred transformation_rule_1(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, proc_region_set_table::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

transformation_rule_1(Expr, ProgPoint, ToBeCreatedAndAllowed, CallerRptaInfo,
        BornRTable, !RegionInstructionProc) :-
    (
        Expr = plain_call(CalleePredId, CalleeProcId, _, _, _, _)
    ->
        CalleePPId = proc(CalleePredId, CalleeProcId),
        CallerRptaInfo = rpta_info(CallerGraph, AlphaMapping),
        (
            % Currently we do not collect BornR for non-defined-in-module
            % procedure, so if we cannot find one here then q is an
            % imported.
            map.search(BornRTable, CalleePPId, _)
        ->
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),
            map.lookup(BornRTable, CalleePPId, CalleeBornR),
            map.foldl(
                process_mapping_rule_1(ProgPoint, ToBeCreatedAndAllowed,
                    CalleeBornR, CallerGraph),
                AlphaAtProgPoint, !RegionInstructionProc)
        ;
            % q is from an imported module, therefore we consider BornR of
            % q empty, so just create whatever regions becoming live
            % provided that they are in BornR or LocalR, i.e., p is allowed
            % to create them.
            set.fold(
                record_instruction_before_prog_point(create_region_instruction,
                    ProgPoint, CallerGraph),
                ToBeCreatedAndAllowed, !RegionInstructionProc)
        )
    ;
        ( Expr = conj(_, [])
        ; Expr = disj([])
        ; Expr = unify(_, _, _, _, _)
        )
    ->
        true
    ;
        unexpected(this_file, "transformation_rule_1: found non-atomic goal")
    ).

:- pred process_mapping_rule_1(program_point::in, region_set::in,
    region_set::in, rpt_graph::in, rptg_node::in, rptg_node::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

process_mapping_rule_1(ProgPoint, ToBeCreatedAndAllowed, CalleeBornR,
        CallerGraph, SourceRegion, TargetRegion, !RegionInstructionProc) :-
    (
        set.contains(ToBeCreatedAndAllowed, TargetRegion),
        not set.contains(CalleeBornR, SourceRegion)
    ->
        record_instruction_before_prog_point(create_region_instruction,
            ProgPoint, CallerGraph, TargetRegion, !RegionInstructionProc)
    ;
        true
    ).

    % Transformation rule 2: if a region reachable from the left variable
    % of a construction is not live before the construction but it is live
    % after and is a local region or in a BornR set, then the region is
    % created before the unification.
    %
:- pred transformation_rule_2(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, module_info::in, proc_info::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

transformation_rule_2(Expr, ProgPoint, ToBeCreatedAndAllowed, RptaInfo,
        ModuleInfo, ProcInfo, !RegionInstructionProc) :-
    (
        Expr = unify(X, _, _, construct(_, _, _, _, _, _, _), _)
    ->
        RptaInfo = rpta_info(Graph, _AlphaMapping),
        % Need to be regions reachable from X.
        reach_from_a_variable(Graph, ModuleInfo, ProcInfo, X,
            set.init, Reach_X),

        set.intersect(Reach_X, ToBeCreatedAndAllowed,
            ToBeCreatedAllowedAndReached),
        set.fold(record_instruction_before_prog_point(create_region_instruction,
            ProgPoint, Graph), ToBeCreatedAllowedAndReached,
            !RegionInstructionProc)
    ;
        ( Expr = unify(_, _, _, deconstruct(_, _, _, _, _, _), _)
        ; Expr = unify(_, _, _, assign(_, _), _)
        ; Expr = unify(_, _, _, simple_test(_, _), _)
        ; Expr = unify(_, _, _, complicated_unify(_, _, _), _)
        ; Expr = plain_call(_, _, _, _, _, _)
        ; Expr = conj(_, [])
        ; Expr = disj([])
        )
    ->
        true
    ;
        unexpected(this_file, "transformation_rule_2: non-atomic goal found")
    ).

    % Transformation rule 3: if a region is live before q but it is not
    % live after q and it is not removed by q, the caller will remove it
    % after calling q.
    %
    % There are two cases: either q is defined in this module or q is
    % imported.
    % The former is straightforward because we have the analysis information
    % for q.
    % The latter for now, with the assumption that all source code is in
    % only one module, imported preds will only be ones from
    % Mercury's library. We do not intent to deal with the library's code
    % now therefore we have to assume here that the caller will always
    % REMOVE the REGIONs for those procedures.
    %
:- pred transformation_rule_3(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, proc_region_set_table::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

transformation_rule_3(Expr, ProgPoint, ToBeRemovedAndAllowed, CallerRptaInfo,
        DeadRTable, !RegionInstructionProc) :-
    (
        Expr =  plain_call(CalleePredId, CalleeProcId, _, _, _, _)
    ->
        CalleePPId = proc(CalleePredId, CalleeProcId),
        CallerRptaInfo = rpta_info(CallerGraph, AlphaMapping),
        ( map.search(DeadRTable, CalleePPId, _) ->
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),

            map.lookup(DeadRTable, CalleePPId, CalleeDeadR),
            map.foldl(
                process_mapping_rule_3(ProgPoint, ToBeRemovedAndAllowed,
                    CalleeDeadR, CallerGraph),
                AlphaAtProgPoint, !RegionInstructionProc)
        ;
            % q is from an imported module. So just remove whatever regions
            % become dead provided that p is allowed to remove those
            % regions.
            set.fold(
                record_instruction_after_prog_point(remove_region_instruction,
                    ProgPoint, CallerGraph),
                ToBeRemovedAndAllowed, !RegionInstructionProc)
        )
    ;
        ( Expr = unify(_, _, _, _, _)
        ; Expr = conj(_, [])
        ; Expr = disj([])
        )
    ->
        true
    ;
        unexpected(this_file, "transformation_rule_3: non-atomic goal found")
    ).

:- pred process_mapping_rule_3(program_point::in, region_set::in,
    region_set::in, rpt_graph::in, rptg_node::in, rptg_node::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

process_mapping_rule_3(ProgPoint, ToBeRemovedAndAllowed, CalleeDeadR,
        CallerGraph, SourceRegion, TargetRegion, !RegionInstructionProc) :-
    (
        set.contains(ToBeRemovedAndAllowed, TargetRegion),
        not set.contains(CalleeDeadR, SourceRegion)
    ->
        record_instruction_after_prog_point(remove_region_instruction,
            ProgPoint, CallerGraph, TargetRegion, !RegionInstructionProc)
    ;
        true
    ).

    % Transformation rule 4: if a region is live before a unification but
    % it is not live after and it is in the dead region set or is a local
    % region, then it is removed after the unification if the current
    % procedure is allowed to remove it.
    %
:- pred transformation_rule_4(hlds_goal_expr::in, program_point::in,
    region_set::in, rpta_info::in, region_instruction_proc::in,
    region_instruction_proc::out) is det.

transformation_rule_4(Expr, ProgPoint, ToBeRemovedAndAllowed, RptaInfo,
        !RegionInstructionProc) :-
    (
        Expr = unify(_, _, _, _, _)
    ->
        RptaInfo = rpta_info(Graph, _),
        set.fold(record_instruction_after_prog_point(remove_region_instruction,
            ProgPoint, Graph), ToBeRemovedAndAllowed, !RegionInstructionProc)
    ;
        ( Expr = plain_call(_, _, _, _, _, _)
        ; Expr = conj(_, [])
        ; Expr = disj([])
        )
    ->
        true
    ;
        unexpected(this_file, "transformation_rule_4: non-atomic goal found")
    ).

    % This is for the case rule 4 applied for a unification in a switch,
    % the unification has been removed by MMC.
    % We will record the annotations after the program point.
    %
:- pred transformation_rule_4_2(program_point::in, region_set::in,
    rpta_info::in, region_instruction_proc::in, region_instruction_proc::out)
    is det.

transformation_rule_4_2(ProgPoint, ToBeRemovedAndAllowed, RptaInfo,
        !RegionInstructionProc) :-
    RptaInfo = rpta_info(Graph, _),
    set.fold(
        record_instruction_after_prog_point(remove_region_instruction,
            ProgPoint, Graph),
        ToBeRemovedAndAllowed, !RegionInstructionProc).

:- pred record_instruction_after_prog_point(region_instruction_type::in,
    program_point::in, rpt_graph::in, rptg_node::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

record_instruction_after_prog_point(RegionInstType, ProgPoint, Graph, Region,
        !RegionInstructionProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    make_create_or_remove_instruction(RegionInstType, RegionName,
        RegionInstruction),
    % Attach the instruction to after the program point.
    (
        map.search(!.RegionInstructionProc, ProgPoint,
            instructions_before_after(InstsBefore, InstsAfter))
    ->
        ( list.member(RegionInstruction, InstsAfter) ->
            true
        ;
            svmap.set(ProgPoint,
                instructions_before_after(InstsBefore,
                    [RegionInstruction | InstsAfter]),
                !RegionInstructionProc)
        )
    ;
        svmap.set(ProgPoint,
            instructions_before_after([], [RegionInstruction]),
            !RegionInstructionProc)
    ).

:- pred record_instruction_before_prog_point(region_instruction_type::in,
    program_point::in, rpt_graph::in, rptg_node::in,
    region_instruction_proc::in, region_instruction_proc::out) is det.

record_instruction_before_prog_point(RegionInstType, ProgPoint, Graph, Region,
        !RegionInstructionProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    make_create_or_remove_instruction(RegionInstType, RegionName,
        RegionInstruction),
    % Attach the instruction to before the program point.
    (
        map.search(!.RegionInstructionProc, ProgPoint,
            instructions_before_after(InstsBefore, InstsAfter))
    ->
        ( list.member(RegionInstruction, InstsBefore) ->
            true
        ;
            svmap.set(ProgPoint,
                instructions_before_after([RegionInstruction | InstsBefore],
                    InstsAfter),
                !RegionInstructionProc)
        )
    ;
        svmap.set(ProgPoint,
            instructions_before_after([RegionInstruction], []),
            !RegionInstructionProc)
    ).

:- pred make_create_or_remove_instruction(region_instruction_type::in,
    string::in, region_instruction::out) is det.

make_create_or_remove_instruction(RegionInstType, RegionName,
        RegionInstruction) :-
    (
        RegionInstType = create_region_instruction,
        RegionInstruction = create_region(RegionName)
    ;
        RegionInstType = remove_region_instruction,
        RegionInstruction = remove_region(RegionName)
    ;
        RegionInstType = renaming_region_instruction,
        unexpected(this_file, "make_create_or_remove_instruction: "
            ++ "unexpected region instruction type")
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "rbmm.region_instruction.m".

%-----------------------------------------------------------------------------%
