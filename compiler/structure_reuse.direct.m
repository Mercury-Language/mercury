%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: structure_reuse.direct.m.
% Main authors: nancy.
%
% This module defines procedures and types related to the detection of so
% called "direct reuses" within the CTGC system.  A direct reuse is a
% combination of the location of a deconstruction unification (where a
% datastructure may become garbage under certain conditions) and a set of
% locations of construction unifications where the garbage datastructure can
% be reused locally.
%
% Direct reuse analysis requires two steps:
%   - Detecting where datastructures may become garbage.
%   - Finding where these garbage datastructures can be reused.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.direct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % The first pass, where we process all procedures defined in the module.
    %
:- pred direct_reuse_pass(io.text_output_stream::in, sharing_as_table::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

    % Subsequent passes, where we process only the listed procedures.
    %
:- pred direct_reuse_process_specific_procs(io.text_output_stream::in,
    sharing_as_table::in, list(pred_proc_id)::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.smm_common.

:- import_module bool.
:- import_module map.

:- include_module transform_hlds.ctgc.structure_reuse.direct.detect_garbage.
:- include_module transform_hlds.ctgc.structure_reuse.direct.choose_reuse.

%---------------------------------------------------------------------------%

direct_reuse_pass(ProgressStream, SharingTable, !ModuleInfo, !ReuseTable) :-
    % Gather the pred_ids of the preds that need to be analysed.
    module_info_get_valid_pred_ids(!.ModuleInfo, AllPredIds),
    list.filter(pred_requires_analysis(!.ModuleInfo), AllPredIds,
        ToBeAnalysedPredIds),

    % Analyse and annotate each of the predicates.
    list.foldl2(direct_reuse_process_pred(ProgressStream, SharingTable),
        ToBeAnalysedPredIds, !ModuleInfo, !ReuseTable).

:- pred direct_reuse_process_pred(io.text_output_stream::in,
    sharing_as_table::in, pred_id::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

direct_reuse_process_pred(ProgressStream, SharingTable, PredId,
        !ModuleInfo, !ReuseTable) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ( if
        pred_info_get_origin(PredInfo0, Origin),
        Origin = origin_compiler(made_for_uci(_, _))
    then
        % We can't analyse compiler generated special predicates.
        true
    else if
        pred_info_get_status(PredInfo0, PredStatus),
        PredStatus = pred_status(status_external(_))
    then
        % We can't analyse `:- pragma external_{pred/func}' procedures,
        % but we add an extry to the reuse table so something will be written
        % out to optimisation interface files.
        ProcIds = pred_info_all_procids(PredInfo0),
        list.foldl(set_external_pred_reuse_as(PredId, reuse_as_init, optimal),
            ProcIds, !ReuseTable)
    else
        ProcIds = pred_info_all_non_imported_procids(PredInfo0),
        list.foldl2(
            direct_reuse_process_proc(ProgressStream, SharingTable, PredId),
            ProcIds, !ModuleInfo, !ReuseTable)
    ).

:- pred set_external_pred_reuse_as(pred_id::in, reuse_as::in,
    analysis_status::in, proc_id::in, reuse_as_table::in, reuse_as_table::out)
    is det.

set_external_pred_reuse_as(PredId, ReuseAs, Status, ProcId, !ReuseTable) :-
    reuse_as_table_set(proc(PredId, ProcId),
    reuse_as_and_status(ReuseAs, Status), !ReuseTable).

direct_reuse_process_specific_procs(ProgressStream, SharingTable, PPIds,
        !ModuleInfo, !ReuseTable) :-
    list.foldl2(direct_reuse_process_ppid(ProgressStream, SharingTable), PPIds,
        !ModuleInfo, !ReuseTable).

:- pred direct_reuse_process_ppid(io.text_output_stream::in,
    sharing_as_table::in, pred_proc_id::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

direct_reuse_process_ppid(ProgressStream, SharingTable, proc(PredId, ProcId),
        !ModuleInfo, !ReuseTable) :-
    direct_reuse_process_proc(ProgressStream, SharingTable, PredId, ProcId,
        !ModuleInfo, !ReuseTable).

    % Process one individual procedure.
    %
:- pred direct_reuse_process_proc(io.text_output_stream::in,
    sharing_as_table::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

direct_reuse_process_proc(ProgressStream, SharingTable, PredId, ProcId,
        !ModuleInfo, !ReuseTable) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),

    direct_reuse_process_proc_2(ProgressStream, !.ModuleInfo, SharingTable,
        PredId, PredInfo0, ProcId, ProcInfo0, ProcInfo, ReuseAs),
    % XXX is this right?
    Status = optimal,
    AsAndStatus = reuse_as_and_status(ReuseAs, Status),
    reuse_as_table_set(proc(PredId, ProcId), AsAndStatus, !ReuseTable),

    pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred direct_reuse_process_proc_2(io.text_output_stream::in, module_info::in,
    sharing_as_table::in, pred_id::in, pred_info::in,
    proc_id::in, proc_info::in, proc_info::out, reuse_as::out) is det.

direct_reuse_process_proc_2(ProgressStream, ModuleInfo, SharingTable,
        PredId, PredInfo, ProcId, !ProcInfo, ReuseAs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),

    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, ModuleInfo,
            "Direct reuse analysis of", proc(PredId, ProcId), !IO)
    ),

    proc_info_get_goal(!.ProcInfo, Goal0),

    % Determine the deconstructions in which data may potentially become
    % garbage.

    determine_dead_deconstructions(ModuleInfo, PredInfo, !.ProcInfo,
        SharingTable, Goal0, DeadCellTable),
    trace [io(!IO)] (
        get_debug_output_stream(ModuleInfo, DebugStream, !IO),
        dead_cell_table_maybe_dump(DebugStream, VeryVerbose,
            DeadCellTable, !IO)
    ),

    % Determine how the detected dead datastructures can be reused.
    % This annotates the goal with potential reuses.

    determine_reuse(ModuleInfo, !.ProcInfo, DeadCellTable, Goal0, Goal,
        ReuseAs),

    proc_info_set_goal(Goal, !ProcInfo),

    trace [io(!IO)] (
        maybe_write_string(ProgressStream, VeryVerbose,
            "% reuse analysis done.\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% We use the type dead_cell_table to collect all deconstructions that possibly
% leave garbage behind.
%

    % A dead_cell_table maps program points onto reuse conditions.
    %
:- type dead_cell_table == map(program_point, reuse_condition).

    % Initialise a dead_cell_table.
    %
:- func dead_cell_table_init = dead_cell_table.

dead_cell_table_init = map.init.

    % Check whether the table is empty.
    %
:- pred dead_cell_table_is_empty(dead_cell_table::in) is semidet.

dead_cell_table_is_empty(Table) :-
    map.is_empty(Table).

    % Succeeds if the given program point is listed in the table. Return
    % the associated reuse_condition.
    %
:- func dead_cell_table_search(program_point, dead_cell_table)
    = reuse_condition is semidet.

dead_cell_table_search(PP, Table) = ReuseCond :-
    map.search(Table, PP, ReuseCond).

    % Add a program point and its associated reuse_condition to the table.
    %
:- pred dead_cell_table_set(program_point::in, reuse_condition::in,
    dead_cell_table::in, dead_cell_table::out) is det.

dead_cell_table_set(PP, RC, !Table) :-
    map.set(PP, RC, !Table).

    % Remove a program point from the table.
    %
:- pred dead_cell_table_remove(program_point::in,
    dead_cell_table::in, dead_cell_table::out) is det.

dead_cell_table_remove(PP, !Table) :-
    map.det_remove(PP, _, !Table).

    % Remove all program points from the table for which the reuse_conditions
    % are "conditional".
    %
:- pred dead_cell_table_remove_conditionals(dead_cell_table::in,
    dead_cell_table::out) is det.

dead_cell_table_remove_conditionals(!Table) :-
    map.foldl(dead_cell_table_add_unconditional, !.Table,
        dead_cell_table_init, !:Table).

:- pred dead_cell_table_add_unconditional(program_point::in,
    reuse_condition::in, dead_cell_table::in, dead_cell_table::out) is det.

dead_cell_table_add_unconditional(PP, C, !Table) :-
    ( if reuse_condition_is_conditional(C) then
        true
    else
        dead_cell_table_set(PP, C, !Table)
    ).

%---------------------------------------------------------------------------%

    % Dump the contents of the table.
    %
:- pred dead_cell_table_maybe_dump(io.text_output_stream::in,
    bool::in, dead_cell_table::in, io::di, io::uo) is det.

dead_cell_table_maybe_dump(Stream, MaybeDump, Table, !IO) :-
    (
        MaybeDump = no
    ;
        MaybeDump = yes,
        io.write_string(Stream, "\t\t|--------|\n", !IO),
        map.foldl(dead_cell_entry_dump(Stream), Table, !IO),
        io.write_string(Stream, "\t\t|--------|\n", !IO)
    ).

:- pred dead_cell_entry_dump(io.text_output_stream::in,
    program_point::in, reuse_condition::in, io::di, io::uo) is det.

dead_cell_entry_dump(Stream, PP, Cond, !IO) :-
    ( if reuse_condition_is_conditional(Cond) then
        io.write_string(Stream, "\t\t|  cond  |\t", !IO)
    else
        io.write_string(Stream, "\t\t| always |\t", !IO)
    ),
    dump_program_point(Stream, PP, !IO),
    io.write_string(Stream, "\n", !IO).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.direct.
%---------------------------------------------------------------------------%
