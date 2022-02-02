%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: passes_aux.m.
% Author: zs
%
% This file contains auxiliary routines for the passes of the front and back
% ends of the compiler.
%
%---------------------------------------------------------------------------%

:- module hlds.passes_aux.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module io.
:- import_module list.
:- import_module univ.

%---------------------------------------------------------------------------%

:- type pred_error_task ==
    pred(pred_id, module_info, module_info, pred_info, pred_info,
        list(error_spec), list(error_spec)).
:- inst pred_error_task ==
    (pred(in, in, out, in, out, in, out) is det).

:- type update_pred_task
    --->    update_pred_error(pred_error_task).

:- inst update_pred_task for update_pred_task/0
    --->    update_pred_error(pred_error_task).

:- mode update_pred_task == update_pred_task >> update_pred_task.

:- pred process_valid_nonimported_preds_errors(
    update_pred_task::update_pred_task, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

% Note that update_module_cookie causes some difficulties.
% Ideally, it should be implemented using existential types:
%
%   :- type update_proc_task
%   --->
%           ...
%   ;       some [T] update_module_cookie(pred(pred_proc_id,
%               proc_info, proc_info, module_info, module_info, T, T), T)
%
% That would avoid the need for messing about with type_to_univ and
% univ_to_type.
%
% Originally, it was implemented by changing `update_proc_task' to
% `update_proc_task(T)':
%
%   :- type update_proc_task(T)
%   --->
%           ...
%   ;       update_module_cookie(pred(pred_proc_id, proc_info, proc_info,
%               module_info, module_info, T, T), T)
%
% but that is not a good solution, because it causes a lot of warnings
% about unbound type variables.

:- type proc_task ==
    pred(module_info, proc_info, proc_info).
:- inst proc_task ==
    (pred(in, in, out) is det).

:- type proc_ids_task ==
    pred(module_info, pred_proc_id, proc_info, proc_info).
:- inst proc_ids_task ==
    (pred(in, in, in, out) is det).

:- type proc_ids_pred_task ==
    pred(module_info, pred_proc_id, pred_info, proc_info, proc_info).
:- inst proc_ids_pred_task ==
    (pred(in, in, in, in, out) is det).

:- type module_task ==
    pred(pred_proc_id, proc_info, proc_info,
        module_info, module_info).
:- inst module_task ==
    (pred(in, in, out, in, out) is det).

:- type module_pred_task ==
    pred(pred_proc_id, pred_info, proc_info, proc_info,
        module_info, module_info).
:- inst module_pred_task ==
    (pred(in, in, in, out, in, out) is det).

:- type module_cookie_task ==
    pred(pred_proc_id, proc_info, proc_info,
        module_info, module_info, univ, univ).
:- inst module_cookie_task ==
    (pred(in, in, out, in, out, in, out) is det).

:- type module_pred_cookie_task ==
    pred(pred_proc_id, pred_info, proc_info, proc_info,
        module_info, module_info, univ, univ).
:- inst module_pred_cookie_task ==
    (pred(in, in, in, out, in, out, in, out) is det).

%---------------------------------------------------------------------------%

:- type update_proc_task
    --->    update_proc(proc_task)
    ;       update_proc_ids(proc_ids_task)
    ;       update_proc_ids_pred(proc_ids_pred_task)
    ;       update_module(module_task)
    ;       update_module_pred(module_pred_task)
    ;       update_module_cookie(module_cookie_task, univ)
    ;       update_module_pred_cookie(module_pred_cookie_task, univ).

:- inst update_proc_task for update_proc_task/0
    --->    update_proc(proc_task)
    ;       update_proc_ids(proc_ids_task)
    ;       update_proc_ids_pred(proc_ids_pred_task)
    ;       update_module(module_task)
    ;       update_module_pred(module_pred_task)
    ;       update_module_cookie(module_cookie_task, ground)
    ;       update_module_pred_cookie(module_pred_cookie_task, ground).

:- mode update_proc_task == update_proc_task >> update_proc_task.

%---------------------------------------------------------------------------%

:- pred process_valid_nonimported_procs(update_proc_task::update_proc_task,
    module_info::in, module_info::out) is det.

:- pred process_valid_nonimported_procs_update(
    update_proc_task::update_proc_task,
    update_proc_task::out(update_proc_task),
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

:- pred write_pred_progress_message(module_info::in, string::in,
    pred_id::in, io::di, io::uo) is det.

:- pred write_proc_progress_message(module_info::in, string::in,
    pred_proc_id::in, io::di, io::uo) is det.
:- pred write_proc_progress_message(module_info::in, string::in,
    pred_id::in, proc_id::in, io::di, io::uo) is det.

:- pred maybe_report_sizes(module_info::in, io::di, io::uo) is det.

:- pred get_error_output_stream(module_info::in,
    io.text_output_stream::out, io::di, io::uo) is det.
:- pred get_progress_output_stream(module_info::in,
    io.text_output_stream::out, io::di, io::uo) is det.
:- pred get_debug_output_stream(module_info::in,
    io.text_output_stream::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % should_dump_stage(StageNum, StageNumStr, StageName, DumpStages):
    %
    % If StageName or the string form of StateNum appears in DumpStages,
    % either directly, or as part of a range, then succeed; otherwise, fail.
    %
:- pred should_dump_stage(int::in, string::in, string::in, list(string)::in)
    is semidet.

:- func stage_num_str(int) = string.

:- type dump_info
    --->    no_prev_dump
    ;       prev_dumped_hlds(string, module_info).

    % maybe_dump_hlds(HLDS, StageNum, StageName, !DumpInfo, !IO):
    %
    % If the options in HLDS call for it, dump the (selected parts of)
    % the HLDS, unless they would be the same as the previous dump.
    %
:- pred maybe_dump_hlds(module_info::in, int::in, string::in,
    dump_info::in, dump_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.

:- import_module assoc_list.
:- import_module benchmarking.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

process_valid_nonimported_preds_errors(Task, !ModuleInfo, !Specs, !IO) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl2(process_valid_nonimported_pred(Task), PredIds,
        !ModuleInfo, !Specs).

:- pred process_valid_nonimported_pred(update_pred_task::in(update_pred_task),
    pred_id::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

process_valid_nonimported_pred(Task, PredId, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ( if pred_info_is_imported(PredInfo0) then
        true
    else
        Task = update_pred_error(Closure),
        Closure(PredId, !ModuleInfo, PredInfo0, PredInfo, !Specs),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- inst par_proc_task for update_proc_task/0
    --->    update_proc(proc_task)
    ;       update_proc_ids(proc_ids_task)
    ;       update_proc_ids_pred(proc_ids_pred_task).

:- mode par_proc_task == par_proc_task >> par_proc_task.

:- inst seq_proc_task for update_proc_task/0
    --->    update_module(module_task)
    ;       update_module_pred(module_pred_task)
    ;       update_module_cookie(module_cookie_task, ground)
    ;       update_module_pred_cookie(module_pred_cookie_task, ground).

:- mode seq_proc_task == seq_proc_task >> seq_proc_task.

process_valid_nonimported_procs(Task, !ModuleInfo) :-
    process_valid_nonimported_procs_update(Task, _, !ModuleInfo).

process_valid_nonimported_procs_update(!Task, !ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, ValidPredIds),
    (
        ( !.Task = update_proc(_)
        ; !.Task = update_proc_ids(_)
        ; !.Task = update_proc_ids_pred(_)
        ),
        ValidPredIdSet = set_tree234.list_to_set(ValidPredIds),
        module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
        map.to_assoc_list(PredIdTable0, PredIdsInfos0),
        par_process_valid_nonimported_procs_in_preds(!.ModuleInfo, !.Task,
            ValidPredIdSet, PredIdsInfos0, PredIdsInfos),
        map.from_sorted_assoc_list(PredIdsInfos, PredIdTable),
        module_info_set_pred_id_table(PredIdTable, !ModuleInfo)
    ;
        ( !.Task = update_module(_)
        ; !.Task = update_module_pred(_)
        ; !.Task = update_module_cookie(_, _)
        ; !.Task = update_module_pred_cookie(_, _)
        ),
        seq_process_valid_nonimported_procs_in_preds(ValidPredIds, !Task,
            !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

:- pred par_process_valid_nonimported_procs_in_preds(module_info::in,
    update_proc_task::par_proc_task, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out)
    is det.

par_process_valid_nonimported_procs_in_preds(_, _, _, [], []).
par_process_valid_nonimported_procs_in_preds(ModuleInfo, Task, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos]) :-
    PredIdInfo0 = PredId - PredInfo0,
    ( if
        set_tree234.contains(ValidPredIdSet, PredId),
        ProcIds = pred_info_valid_non_imported_procids(PredInfo0),
        ProcIds = [_ | _]
    then
        % Potential parallelization site.
        par_process_valid_nonimported_procs(ModuleInfo, Task, PredId, ProcIds,
            PredInfo0, PredInfo),
        PredIdInfo = PredId - PredInfo,
        par_process_valid_nonimported_procs_in_preds(ModuleInfo, Task,
            ValidPredIdSet, PredIdsInfos0, PredIdsInfos)
    else
        PredIdInfo = PredIdInfo0,
        par_process_valid_nonimported_procs_in_preds(ModuleInfo, Task,
            ValidPredIdSet, PredIdsInfos0, PredIdsInfos)
    ).

:- pred par_process_valid_nonimported_procs(module_info::in,
    update_proc_task::par_proc_task, pred_id::in, list(proc_id)::in,
    pred_info::in, pred_info::out) is det.

par_process_valid_nonimported_procs(_, _, _, [], !PredInfo).
par_process_valid_nonimported_procs(ModuleInfo, Task, PredId,
        [ProcId | ProcIds], !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, ProcMap0),
    map.lookup(ProcMap0, ProcId, Proc0),

    PredProcId = proc(PredId, ProcId),
    (
        Task = update_proc(Closure),
        Closure(ModuleInfo, Proc0, Proc)
    ;
        Task = update_proc_ids(Closure),
        Closure(ModuleInfo, PredProcId, Proc0, Proc)
    ;
        Task = update_proc_ids_pred(Closure),
        Closure(ModuleInfo, PredProcId, !.PredInfo, Proc0, Proc)
    ),

    map.det_update(ProcId, Proc, ProcMap0, ProcMap),
    pred_info_set_proc_table(ProcMap, !PredInfo),

    par_process_valid_nonimported_procs(ModuleInfo, Task, PredId, ProcIds,
        !PredInfo).

%---------------------------------------------------------------------------%

:- pred seq_process_valid_nonimported_procs_in_preds(list(pred_id)::in,
    update_proc_task::seq_proc_task, update_proc_task::out(seq_proc_task),
    module_info::in, module_info::out) is det.

seq_process_valid_nonimported_procs_in_preds([], !Task, !ModuleInfo).
seq_process_valid_nonimported_procs_in_preds([PredId | PredIds], !Task,
        !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    seq_process_valid_nonimported_procs(PredId, ProcIds, !Task, !ModuleInfo),
    seq_process_valid_nonimported_procs_in_preds(PredIds, !Task, !ModuleInfo).

:- pred seq_process_valid_nonimported_procs(pred_id::in, list(proc_id)::in,
    update_proc_task::seq_proc_task, update_proc_task::out(seq_proc_task),
    module_info::in, module_info::out) is det.

seq_process_valid_nonimported_procs(_PredId, [], !Task, !ModuleInfo).
seq_process_valid_nonimported_procs(PredId, [ProcId | ProcIds], !Task,
        !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),

    PredProcId = proc(PredId, ProcId),
    (
        !.Task = update_module(Closure),
        Closure(PredProcId, ProcInfo0, ProcInfo, !ModuleInfo)
    ;
        !.Task = update_module_pred(Closure),
        Closure(PredProcId, PredInfo0, ProcInfo0, ProcInfo, !ModuleInfo)
    ;
        !.Task = update_module_cookie(Closure, Cookie0),
        Closure(PredProcId, ProcInfo0, ProcInfo, !ModuleInfo, Cookie0, Cookie),
        !:Task = update_module_cookie(Closure, Cookie)
    ;
        !.Task = update_module_pred_cookie(Closure, Cookie0),
        Closure(PredProcId, PredInfo0, ProcInfo0, ProcInfo, !ModuleInfo,
            Cookie0, Cookie),
        !:Task = update_module_pred_cookie(Closure, Cookie)
    ),

    % If the pass changed the module_info, it may have changed the pred table
    % or the proc table for this pred_id. Do not take any chances.

    module_info_pred_info(!.ModuleInfo, PredId, PredInfo8),
    pred_info_set_proc_info(ProcId, ProcInfo, PredInfo8, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),

    seq_process_valid_nonimported_procs(PredId, ProcIds, !Task, !ModuleInfo).

%---------------------------------------------------------------------------%

write_pred_progress_message(ModuleInfo, Message, PredId, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        globals.get_progress_output_stream(Globals, ModuleName, Stream, !IO),
        PredStr = pred_id_to_string(ModuleInfo, PredId),
        io.format(Stream, "%% %s %s\n", [s(Message), s(PredStr)], !IO),
        io.flush_output(Stream, !IO)
    ;
        VeryVerbose = no
    ).

write_proc_progress_message(ModuleInfo, Message, proc(PredId, ProcId), !IO) :-
    write_proc_progress_message(ModuleInfo, Message, PredId, ProcId, !IO).

write_proc_progress_message(ModuleInfo, Message, PredId, ProcId, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        globals.get_progress_output_stream(Globals, ModuleName, Stream, !IO),
        ProcStr = pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId),
        io.format(Stream, "%% %s %s\n", [s(Message), s(ProcStr)], !IO),
        io.flush_output(Stream, !IO)
    ;
        VeryVerbose = no
    ).

%---------------------------------------------------------------------------%

maybe_report_sizes(HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    (
        Statistics = yes,
        report_sizes(HLDS, !IO)
    ;
        Statistics = no
    ).

:- pred report_sizes(module_info::in, io::di, io::uo) is det.

report_sizes(ModuleInfo, !IO) :-
    get_debug_output_stream(ModuleInfo, Stream, !IO),

    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    io.format(Stream, "Pred table size = %d\n",
        [i(map.count(PredIdTable))], !IO),

    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorDefns),
    io.format(Stream, "Type table size = %d\n",
        [i(list.length(TypeCtorDefns))], !IO),

    module_info_get_cons_table(ModuleInfo, CtorTable),
    get_all_cons_defns(CtorTable, CtorDefns),
    io.format(Stream, "Constructor table size = %d\n",
        [i(list.length(CtorDefns))], !IO).

%---------------------------------------------------------------------------%

get_error_output_stream(ModuleInfo, Stream, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    globals.get_error_output_stream(Globals, ModuleName, Stream, !IO).

get_progress_output_stream(ModuleInfo, Stream, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    globals.get_progress_output_stream(Globals, ModuleName, Stream, !IO).

get_debug_output_stream(ModuleInfo, Stream, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    globals.get_debug_output_stream(Globals, ModuleName, Stream, !IO).

%---------------------------------------------------------------------------%

should_dump_stage(StageNum, StageNumStr, StageName, DumpStages) :-
    some [DumpStage] (
        list.member(DumpStage, DumpStages),
        (
            StageName = DumpStage
        ;
            "all" = DumpStage
        ;
            (
                DumpStage = StageNumStr
            ;
                string.append("0", DumpStage, StageNumStr)
            ;
                string.append("00", DumpStage, StageNumStr)
            )
        ;
            string.append(From, "+", DumpStage),
            string.to_int(From, FromInt),
            StageNum >= FromInt
        )
    ).

stage_num_str(StageNum) = string.format("%03d", [i(StageNum)]).

maybe_dump_hlds(HLDS, StageNum, StageName, !DumpInfo, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_accumulating_option(Globals, dump_hlds, DumpHLDSStages),
    globals.lookup_accumulating_option(Globals, dump_trace_counts,
        DumpTraceStages),
    globals.lookup_string_option(Globals, dump_hlds_file_suffix,
        UserFileSuffix),
    StageNumStr = stage_num_str(StageNum),
    ( if
        should_dump_stage(StageNum, StageNumStr, StageName, DumpHLDSStages)
    then
        module_info_get_dump_hlds_base_file_name(HLDS, BaseFileName),
        DumpFileName = BaseFileName ++ "." ++ StageNumStr ++ "-" ++ StageName
            ++ UserFileSuffix,
        ( if
            !.DumpInfo = prev_dumped_hlds(PrevDumpFileName, PrevHLDS),
            HLDS = PrevHLDS
        then
            globals.lookup_bool_option(Globals, dump_same_hlds, DumpSameHLDS),
            get_progress_output_stream(HLDS, ProgressStream, !IO),
            (
                DumpSameHLDS = no,
                % Don't create a dump file for this stage, and keep the records
                % about previously dumped stages as they are. We do print a
                % message (if asked to) about *why* we don't create this file.
                (
                    Verbose = no
                ;
                    Verbose = yes,
                    io.format(ProgressStream, "%% HLDS dump `%s' would be " ++
                        "identical to previous dump.\n",
                        [s(DumpFileName)], !IO)
                ),

                % If a previous dump exists with this name, leaving it around
                % would be quite misleading. However, there is nothing useful
                % we can do if the removal fails.
                io.remove_file(DumpFileName, _Result, !IO)
            ;
                DumpSameHLDS = yes,
                CurDumpFileName = PrevDumpFileName,
                io.open_output(DumpFileName, Res, !IO),
                (
                    Res = ok(FileStream),
                    io.format(FileStream,
                        "This stage is identical to the stage in %s.\n",
                        [s(PrevDumpFileName)], !IO),
                    io.close_output(FileStream, !IO)
                ;
                    Res = error(IOError),
                    maybe_write_string(ProgressStream, Verbose, "\n", !IO),
                    IOErrorMsg = io.error_message(IOError),
                    string.format("can't open file `%s' for output: %s\n",
                        [s(DumpFileName), s(IOErrorMsg)], Msg),
                    get_error_output_stream(HLDS, ErrorStream, !IO),
                    report_error(ErrorStream, Msg, !IO)
                ),
                !:DumpInfo = prev_dumped_hlds(CurDumpFileName, HLDS)
            )
        else
            dump_hlds(DumpFileName, HLDS, !IO),
            CurDumpFileName = DumpFileName,
            !:DumpInfo = prev_dumped_hlds(CurDumpFileName, HLDS)
        )
    else if
        should_dump_stage(StageNum, StageNumStr, StageName, DumpTraceStages)
    then
        module_info_get_dump_hlds_base_file_name(HLDS, BaseFileName),
        DumpFileName = string.det_remove_suffix(BaseFileName, ".hlds_dump") ++
            ".trace_counts." ++ StageNumStr ++ "-" ++ StageName ++
            UserFileSuffix,
        write_out_trace_counts(DumpFileName, MaybeTraceCountsError, !IO),
        get_progress_output_stream(HLDS, ProgressStream, !IO),
        (
            MaybeTraceCountsError = no,
            (
                Verbose = no
            ;
                Verbose = yes,
                io.format(ProgressStream, "%% Dumped trace counts to `%s'\n",
                    [s(DumpFileName)], !IO),
                io.flush_output(ProgressStream, !IO)
            )
        ;
            MaybeTraceCountsError = yes(TraceCountsError),
            io.format(ProgressStream, "%% %s\n", [s(TraceCountsError)], !IO),
            io.flush_output(ProgressStream, !IO)
        )
    else
        true
    ).

:- pred dump_hlds(string::in, module_info::in, io::di, io::uo) is det.

dump_hlds(DumpFile, HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    globals.lookup_bool_option(Globals, statistics, Stats),
    get_progress_output_stream(HLDS, ProgressStream, !IO),
    (
        Verbose = no
    ;
        Verbose = yes,
        io.format(ProgressStream, "%% Dumping out HLDS to `%s'...",
            [s(DumpFile)], !IO),
        io.flush_output(ProgressStream, !IO)
    ),
    io.open_output(DumpFile, DumpFileResult, !IO),
    (
        DumpFileResult = ok(DumpFileStream),
        write_hlds(DumpFileStream, HLDS, !IO),
        io.close_output(DumpFileStream, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        DumpFileResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, "\n", !IO),
        get_error_output_stream(HLDS, ErrorStream, !IO),
        string.format("can't open file `%s' for output: %s",
            [s(DumpFile), s(io.error_message(IOError))], Msg),
        report_error(ErrorStream, Msg, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.passes_aux.
%---------------------------------------------------------------------------%
