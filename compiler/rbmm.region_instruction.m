%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.region_instruction.m.
% Main author: Quan Phan
%
% This module implements the process of introducing region instructions to
% each program point in a procedure based on its region points-to graph and
% live region information.
%
%-----------------------------------------------------------------------------%

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

:- type region_instr_table
    ==      map(pred_proc_id, region_instr_proc).

:- type region_instr_proc
    ==      map(program_point, instrs_before_after).

    % We introduce region instructions before and after a program point.
:- type instrs_before_after
    --->    instrs_before_after(
                instrs_before    :: list(region_instr),
                                        % Region instructions before a program
                                        % point.

                instrs_after     :: list(region_instr)
                                        % Region instructions after a program
                                        % point.
            ).

:- type region_instr
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

:- type region_instr_type
    --->    create_region_instr
    ;       remove_region_instr
    ;       renaming_region_instr.

    % This predicate decides on where create and remove instructions need to
    % be inserted. The decision is based on region points-to graphs and
    % the region liveness information (both local and global).
    %
    % It also computes for each program point in a procedure
    % the sets of regions that become live (always before), become dead before,
    % become dead after the point.
    %
    % A region 'becomes live' before a program point means that
    % *locally* it is not live before the point and live after the point and
    % *globally* the procedure is allowed to give life to it.
    %
    % A region 'becomes dead before' a program point means that
    % *locally* it is live after the previous program point and not live before
    % the point and *globally* the procedure is allowed to terminate its life.
    %
    % A region 'becomes dead after' a program point means that
    % *locally* it is live before the program point and not live after the
    % point and *globally* the procedure is allowed to terminate its life.
    %
    % A procedure is 'allowed' to manipulate the regions belongs to either
    % bornR or deadR or localR.
    %
    % These sets are needed for computing resurrection of regions later on.
    %
:- pred introduce_region_instructions(module_info::in, rpta_info_table::in,
    execution_path_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in,
    proc_pp_region_set_table::out, proc_pp_region_set_table::out,
    proc_pp_region_set_table::out, region_instr_table::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module transform_hlds.rbmm.points_to_graph.

:- import_module pair.
:- import_module require.
:- import_module set.

introduce_region_instructions(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable,
        BecomeLiveTable, BecomeDeadBeforeTable, BecomeDeadAfterTable,
        RegionInstructionTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    list.foldl4(introduce_region_instructions_pred(ModuleInfo,
        RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
        VoidVarRegionTable, BornRTable, DeadRTable, LocalRTable), PredIds,
        map.init, BecomeLiveTable,
        map.init, BecomeDeadBeforeTable,
        map.init, BecomeDeadAfterTable,
        map.init, RegionInstructionTable).

:- pred introduce_region_instructions_pred(module_info::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, pred_id::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    region_instr_table::in, region_instr_table::out) is det.

introduce_region_instructions_pred(ModuleInfo, RptaInfoTable, ExecPathTable,
        LRBeforeTable, LRAfterTable, VoidVarRegionTable, BornRTable,
        DeadRTable, LocalRTable, PredId,
        !BecomeLiveTable, !BecomeDeadBeforeTable, !BecomeDeadAfterTable,
        !RegionInstructionTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl4(introduce_region_instructions_proc(ModuleInfo, PredId,
        RptaInfoTable, ExecPathTable, LRBeforeTable, LRAfterTable,
        VoidVarRegionTable, BornRTable, DeadRTable, LocalRTable),
        ProcIds, !BecomeLiveTable, !BecomeDeadBeforeTable,
        !BecomeDeadAfterTable, !RegionInstructionTable).

:- pred introduce_region_instructions_proc(module_info::in, pred_id::in,
    rpta_info_table::in, execution_path_table::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::in,
    proc_pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, proc_region_set_table::in, proc_id::in,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    proc_pp_region_set_table::in, proc_pp_region_set_table::out,
    region_instr_table::in, region_instr_table::out) is det.

introduce_region_instructions_proc(ModuleInfo, PredId, RptaInfoTable,
        ExecPathTable, LRBeforeTable, LRAfterTable, VoidVarRegionTable,
        BornRTable, DeadRTable, LocalRTable, ProcId,
        !BecomeLiveTable, !BecomeDeadBeforeTable, !BecomeDeadAfterTable,
        !RegionInstructionTable) :-
    PPId = proc(PredId, ProcId),
    ( if some_are_special_preds([PPId], ModuleInfo) then
        true
    else
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
            map.init, BecomeLiveProc,
            map.init, BecomeDeadBeforeProc,
            map.init, BecomeDeadAfterProc,
            map.init, RegionInstructionProc),
        map.set(PPId, RegionInstructionProc, !RegionInstructionTable),
        map.set(PPId, BecomeLiveProc, !BecomeLiveTable),
        map.set(PPId, BecomeDeadBeforeProc, !BecomeDeadBeforeTable),
        map.set(PPId, BecomeDeadAfterProc, !BecomeDeadAfterTable)
    ).

    % Follow each execution path of a procedure and introduce
    % region instructions at each program point.
    %
:- pred introduce_region_instructions_exec_paths(list(execution_path)::in,
    rpta_info::in, set(rptg_node)::in, set(rptg_node)::in, set(rptg_node)::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in,
    pp_region_set_table::in, pp_region_set_table::out,
    pp_region_set_table::in, pp_region_set_table::out,
    pp_region_set_table::in, pp_region_set_table::out,
    region_instr_proc::in, region_instr_proc::out) is det.

introduce_region_instructions_exec_paths([], _, _, _, _, _, _, _, _, _, _, _,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc).
introduce_region_instructions_exec_paths([ExecPath|ExecPaths], RptaInfo,
        BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc) :-
    introduce_region_instructions_exec_path(ExecPath, RptaInfo, BornR,
        DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo, set.init,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc),
    introduce_region_instructions_exec_paths(ExecPaths, RptaInfo, BornR,
        DeadR, LocalR, ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
        BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc).

:- pred introduce_region_instructions_exec_path(execution_path::in,
    rpta_info::in, region_set::in, region_set::in, region_set::in,
    pp_region_set_table::in, pp_region_set_table::in,
    pp_region_set_table::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in, region_set::in,
    pp_region_set_table::in, pp_region_set_table::out,
    pp_region_set_table::in, pp_region_set_table::out,
    pp_region_set_table::in, pp_region_set_table::out,
    region_instr_proc::in, region_instr_proc::out) is det.

introduce_region_instructions_exec_path([], _, _, _, _, _, _, _, _, _, _, _, _,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc).
introduce_region_instructions_exec_path([ProgPoint - Goal | ProgPoint_Goals],
        RptaInfo, BornR, DeadR, LocalR, ProcLRBefore, ProcLRAfter,
        ProcVoidVarRegion, BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        BecomeDeadBeforeProgPoint,
        !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
        !RegionInstructionProc) :-
    map.lookup(ProcLRBefore, ProgPoint, LRBefore),
    map.lookup(ProcLRAfter, ProgPoint, LRAfter),
    map.lookup(ProcVoidVarRegion, ProgPoint, VoidVarRegions),

    set.union(set.union(LocalR, BornR), DeadR, Allowed),

    % Because void variables at this program point are considered dead
    % right after they get bound, the regions that are reached from them
    % are also candidates to be removed. They will be destroyed after this
    % program point if they are not live after the program point and they
    % are either in DeadR or LocalR or BornR.
    set.difference(VoidVarRegions, LRAfter, DeadVoidVarRegions0),
    set.intersect(Allowed, DeadVoidVarRegions0, DeadVoidVarRegions),

    RptaInfo = rpta_info(CallerGraph, _),

    set.intersect(Allowed, set.difference(LRBefore, LRAfter), BecomeDead),
    set.intersect(Allowed, set.difference(LRAfter, LRBefore), BecomeLive),

    Goal = hlds_goal(Expr, _),
    map.set(ProgPoint, BecomeLive, !BecomeLiveProc),

    annotate_expr(Expr, ProgPoint, BecomeLive,
        BecomeDead, RptaInfo, BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        CreatedBeforeProgPoint, RemovedAfterProgPoint),

    set.fold(record_instruction_before_prog_point(create_region_instr,
            ProgPoint, CallerGraph),
        CreatedBeforeProgPoint, !RegionInstructionProc),
    set.fold(record_instruction_before_prog_point(remove_region_instr,
            ProgPoint, CallerGraph),
        BecomeDeadBeforeProgPoint, !RegionInstructionProc),
    set.fold(record_instruction_after_prog_point(remove_region_instr,
            ProgPoint, CallerGraph),
        RemovedAfterProgPoint, !RegionInstructionProc),
    set.fold(record_instruction_after_prog_point(remove_region_instr,
            ProgPoint, CallerGraph),
        DeadVoidVarRegions, !RegionInstructionProc),

    map.set(ProgPoint, set.union(BecomeDead, DeadVoidVarRegions),
        !BecomeDeadAfterProc),
    map.set(ProgPoint, BecomeLive, !BecomeLiveProc),
    map.set(ProgPoint, BecomeDeadBeforeProgPoint, !BecomeDeadBeforeProc),

    (
        ProgPoint_Goals = [NextProgPoint - _ | _],
        % Transformation rule T5:
        % When a region is live after a program point but not live before the
        % next program point in the same execution path, it will be removed
        % before the next program point if the current procedure is allowed
        % to remove it.
        map.lookup(ProcLRBefore, NextProgPoint, LRBeforeNext),

        set.intersect(Allowed, set.difference(LRAfter, LRBeforeNext),
            BecomeDeadBeforeNextProgPoint),

        introduce_region_instructions_exec_path(ProgPoint_Goals, RptaInfo,
            BornR, DeadR, LocalR,
            ProcLRBefore, ProcLRAfter, ProcVoidVarRegion,
            BornRTable, DeadRTable, ModuleInfo, ProcInfo,
            BecomeDeadBeforeNextProgPoint,
            !BecomeLiveProc, !BecomeDeadBeforeProc, !BecomeDeadAfterProc,
            !RegionInstructionProc)
    ;
        % This is the last program point, we finish.
        ProgPoint_Goals = []
    ).

:- pred annotate_expr(hlds_goal_expr::in, program_point::in,
    region_set::in, region_set::in, rpta_info::in, proc_region_set_table::in,
    proc_region_set_table::in, module_info::in, proc_info::in,
    region_set::out, region_set::out) is det.

annotate_expr(Expr, ProgPoint, BecomeLive, BecomeDead,
        RptaInfo, BornRTable, DeadRTable, ModuleInfo, ProcInfo,
        CreatedBeforeProgPoint, RemovedAfterProgPoint) :-
    (
        Expr = plain_call(CalleePredId, CalleeProcId, _, _, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        RptaInfo = rpta_info(_CallerGraph, AlphaMapping),
        ( if
            % Currently we do not collect BornR for non-defined-in-module
            % procedure, so if we cannot find one here then q is an
            % imported.
            map.search(BornRTable, CalleePPId, _)
        then
            map.lookup(AlphaMapping, ProgPoint, AlphaAtProgPoint),

            % Rule 1: if an output region of q is not live, not created by q,
            % and p is allowed to create it, it is created before the call to
            % q.
            map.lookup(BornRTable, CalleePPId, CalleeBornR),
            map.foldl(
                process_mapping_rule_1(BecomeLive, CalleeBornR),
                AlphaAtProgPoint, set.init, CreatedBeforeProgPoint),

            % Rule 3: add remove(R) after the call.
            % Transformation rule 3: if a region is live before q but it is not
            % live after q and it is not removed by q, the caller will remove
            % it after calling q.
            map.lookup(DeadRTable, CalleePPId, CalleeDeadR),
            map.foldl(process_mapping_rule_3(BecomeDead, CalleeDeadR),
                AlphaAtProgPoint, set.init, RemovedAfterProgPoint)
        else
            % XXX q is from an imported module. We do not support module
            % system yet.
            % For now we consider BornR and DeadR of q empty,
            % therefore just create and remove whatever regions becoming live
            % and dead.
            CreatedBeforeProgPoint = BecomeLive,
            RemovedAfterProgPoint = BecomeDead
        )
    ;
        Expr = unify(X, _, _, Kind, _),
        RptaInfo = rpta_info(Graph, _AlphaMapping),
        (
            Kind = construct(_, _, _, _, _, _, _),
            % Rule 2: if a region reachable from the left variable of a
            % construction is not live before the construction but it is live
            % after and p is allowed to create it, then the region is
            % created before the unification.

            % Regions reachable from X.
            rptg_reach_from_a_variable(Graph, ModuleInfo, ProcInfo, X,
                set.init, Reach_X),

            set.intersect(Reach_X, BecomeLive, CreatedBeforeProgPoint)
        ;
            ( Kind = deconstruct(_, _, _, _, _, _)
            ; Kind = assign(_, _)
            ; Kind = simple_test(_, _)
            ; Kind = complicated_unify(_, _, _)
            ),
            CreatedBeforeProgPoint = set.init
        ),
        % Rule 4: if a region is live before a unification but
        % it is not live after and p is allowed to remove it,
        % then it is removed after the unification.
        RemovedAfterProgPoint = BecomeDead
    ;
        Expr = switch(_, _, _),
        % Special treatment for an atomic switch, i.e. a deconstruction
        % unification that has been removed by MMC. We record the remove
        % annotations after the program point.
        CreatedBeforeProgPoint = set.init,
        RemovedAfterProgPoint = BecomeDead
    ;
        ( Expr = conj(_, [])
        ; Expr = disj([])
        ),
        CreatedBeforeProgPoint = set.init,
        RemovedAfterProgPoint = set.init
    ;
        Expr = generic_call(_, _, _, _, _),
        unexpected($pred, "generic_call NYI")
    ;
        Expr = call_foreign_proc(_, _, _, _, _, _, _),
        unexpected($pred, "call_foreign_proc NYI")
    ;
        ( Expr = conj(_, [_ | _])
        ; Expr = disj([_ | _])
        ; Expr = if_then_else(_, _, _, _)
        ; Expr = negation(_)
        ; Expr = scope(_, _)
        ),
        unexpected($pred, "non-atomic goal")
    ;
        Expr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

    % This predicate ensures that, out of the regions that become live at
    % the call, we only explicitly create ones that are not created by the
    % call.
    %
:- pred process_mapping_rule_1(region_set::in, region_set::in, rptg_node::in,
    rptg_node::in, region_set::in, region_set::out) is det.

process_mapping_rule_1(BecomeLive, CalleeBornR,
        SourceRegion, TargetRegion, !CreatedBeforeProgPoint) :-
    ( if
        set.contains(BecomeLive, TargetRegion),
        not set.contains(CalleeBornR, SourceRegion)
    then
        set.insert(TargetRegion, !CreatedBeforeProgPoint)
    else
        true
    ).

    % This predicate ensures that, out of the regions that become dead at
    % the call, we only explicitly remove ones that are not removed by the
    % call.
    %
:- pred process_mapping_rule_3(region_set::in, region_set::in,
    rptg_node::in, rptg_node::in, region_set::in, region_set::out) is det.

process_mapping_rule_3(BecomeDead, CalleeDeadR, SourceRegion,
        TargetRegion, !RemovedAfterProgPoint) :-
    ( if
        set.contains(BecomeDead, TargetRegion),
        not set.contains(CalleeDeadR, SourceRegion)
    then
        set.insert(TargetRegion, !RemovedAfterProgPoint)
    else
        true
    ).

:- pred record_instruction_after_prog_point(region_instr_type::in,
    program_point::in, rpt_graph::in, rptg_node::in,
    region_instr_proc::in, region_instr_proc::out) is det.

record_instruction_after_prog_point(RegionInstType, ProgPoint, Graph, Region,
        !RegionInstructionProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    make_create_or_remove_instruction(RegionInstType, RegionName,
        RegionInstruction),
    % Attach the instruction to after the program point.
    ( if
        map.search(!.RegionInstructionProc, ProgPoint,
            instrs_before_after(InstsBefore, InstsAfter))
    then
        ( if list.member(RegionInstruction, InstsAfter) then
            true
        else
            map.set(ProgPoint,
                instrs_before_after(InstsBefore,
                    [RegionInstruction | InstsAfter]),
                !RegionInstructionProc)
        )
    else
        map.set(ProgPoint,
            instrs_before_after([], [RegionInstruction]),
            !RegionInstructionProc)
    ).

    % When adding region annotations to before a program point, we maintain
    % that remove instructions are added to top, create instructions are
    % appended at the end.
    %
:- pred record_instruction_before_prog_point(region_instr_type::in,
    program_point::in, rpt_graph::in, rptg_node::in,
    region_instr_proc::in, region_instr_proc::out) is det.

record_instruction_before_prog_point(RegionInstrType, ProgPoint, Graph, Region,
        !RegionInstructionProc) :-
    RegionName = rptg_lookup_region_name(Graph, Region),
    make_create_or_remove_instruction(RegionInstrType, RegionName,
        RegionInstruction),
    % Attach the instruction to before the program point.
    ( if
        map.search(!.RegionInstructionProc, ProgPoint,
            instrs_before_after(InstrsBefore, InstrsAfter))
    then
        ( if list.member(RegionInstruction, InstrsBefore) then
            true
        else
            % It is only safe to add create intructions after remove
            % instructions before a program point because we allow
            % remove(R1), create(R1), p(..., R1, ...).
            ( if RegionInstrType = create_region_instr then
                NewInstrsBefore = InstrsBefore ++ [RegionInstruction]
            else
                NewInstrsBefore = [RegionInstruction | InstrsBefore]
            ),
            map.set(ProgPoint,
                instrs_before_after(NewInstrsBefore, InstrsAfter),
                !RegionInstructionProc)
        )
    else
        map.set(ProgPoint,
            instrs_before_after([RegionInstruction], []),
            !RegionInstructionProc)
    ).

:- pred make_create_or_remove_instruction(region_instr_type::in,
    string::in, region_instr::out) is det.

make_create_or_remove_instruction(RegionInstrType, RegionName,
        RegionInstruction) :-
    (
        RegionInstrType = create_region_instr,
        RegionInstruction = create_region(RegionName)
    ;
        RegionInstrType = remove_region_instr,
        RegionInstruction = remove_region(RegionName)
    ;
        RegionInstrType = renaming_region_instr,
        unexpected($pred, "unexpected region instruction type")
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.region_instruction.
%-----------------------------------------------------------------------------%
