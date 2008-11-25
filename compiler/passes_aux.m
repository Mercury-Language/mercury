%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: passes_aux.m.
% Author: zs
%
% This file contains auxiliary routines for the passes of the front and back
% ends of the compiler.
%
%-----------------------------------------------------------------------------%

:- module hlds.passes_aux.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type task
    --->    update_proc(pred(module_info, proc_info, proc_info))
    ;       update_proc_predid(pred(pred_id, module_info, proc_info,
                proc_info))
    ;       update_proc_predprocid(pred(pred_id, proc_id, module_info,
                pred_info, proc_info, proc_info))
    ;       update_proc_io(pred(pred_id, proc_id, module_info,
                proc_info, proc_info, io, io))
    ;       update_proc_error(pred(pred_id, proc_id, module_info, module_info,
                proc_info, proc_info, int, int, io, io))
    ;       update_pred_error(pred_error_task)
    ;       update_module(pred(pred_id, proc_id, pred_info,
                proc_info, proc_info, module_info, module_info))
    ;       update_module_io(pred(pred_id, proc_id, proc_info, proc_info,
                module_info, module_info, io, io))
            % It would be better to use an existentially-quantified type
            % rather than `univ' here, but the current version of Mercury
            % doesn't support existentially-quantified types.
    ;       update_module_cookie(pred(pred_id, proc_id, proc_info, proc_info,
                univ, univ, module_info, module_info), univ).

% Note that update_module_cookie causes some difficulties.
% Ideally, it should be implemented using existential types:
%
%   :- type task
%   --->
%           ...
%   ;       some [T] update_module_cookie(pred(pred_id, proc_id,
%               proc_info, proc_info, T, T, module_info, module_info), T)
%
% That would avoid the need for messing about with type_to_univ and
% univ_to_type.
%
% Originally, it was implemented by changing `task' to `task(T)':
%
%   :- type task(T)
%   --->
%           ...
%   ;       update_module_cookie(pred(pred_id, proc_id, proc_info, proc_info,
%               T, T, module_info, module_info), T)
%
% but that is not a good solution, because it causes a lot of warnings
% about unbound type variables.

:- inst task ==
    bound(( update_proc(pred(in, in, out) is det)
        ;   update_proc_predid(pred(in, in, in, out) is det)
        ;   update_proc_predprocid(pred(in, in, in, in, in, out) is det)
        ;   update_proc_io(pred(in, in, in, in, out, di, uo) is det)
        ;   update_proc_error(pred(in, in, in, out, in, out, out, out, di, uo)
                is det)
        ;   update_pred_error(pred(in, in, out, in, out, in, out, di, uo)
                is det)
        ;   update_module(pred(in, in, in, in, out, in, out) is det)
        ;   update_module_io(pred(in, in, in, out, in, out, di, uo) is det)
        ;   update_module_cookie(pred(in, in, in, out, in, out, in, out)
                is det, ground)
        )).

:- mode task == task >> task.

:- type pred_error_task ==
        pred(pred_id, module_info, module_info, pred_info, pred_info,
            list(error_spec), list(error_spec), io, io).

:- inst pred_error_task ==
    (pred(in, in, out, in, out, in, out, di, uo) is det).

:- pred process_all_nonimported_procs_errors(task::task,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.
:- pred process_all_nonimported_procs(task::task,
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred process_all_nonimported_procs_update_errors(
    task::task, task::out(task), module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.
:- pred process_all_nonimported_procs_update(task::task, task::out(task),
    module_info::in, module_info::out, io::di, io::uo) is det.

    % Process procedures for which a given test succeeds.
    %
:- pred process_matching_nonimported_procs_errors(task::task,
    pred(pred_info)::in(pred(in) is semidet),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.
:- pred process_matching_nonimported_procs(task::task,
    pred(pred_info)::in(pred(in) is semidet),
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred process_matching_nonimported_procs_update_errors(
    task::task, task::out(task), pred(pred_info)::in(pred(in) is semidet),
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.
:- pred process_matching_nonimported_procs_update(
    task::task, task::out(task), pred(pred_info)::in(pred(in) is semidet),
    module_info::in, module_info::out, io::di, io::uo) is det.

:- pred write_pred_progress_message(string::in, pred_id::in, module_info::in,
    io::di, io::uo) is det.

:- pred write_proc_progress_message(string::in, pred_proc_id::in,
    module_info::in, io::di, io::uo) is det.

:- pred write_proc_progress_message(string::in, pred_id::in, proc_id::in,
    module_info::in, io::di, io::uo) is det.

:- pred maybe_report_sizes(module_info::in, io::di, io::uo) is det.

    % Prints the id of the given procedure via report_pred_name_mode,
    % preceded by "In: " and the context.
    % In new code, use describe_one_pred_name_mode in hlds_error_util instead.
    %
:- pred report_pred_proc_id(module_info::in, pred_id::in, proc_id::in,
    maybe(prog_context)::in, prog_context::out, io::di, io::uo) is det.

    % report_pred_name_mode(PredOrFunc, Name, ArgModes):
    % Depending on PredOrFunc, prints either
    %   Name(ArgMode1, ..., ArgModeN)
    % or
    %   Name(ArgMode1, ..., ArgModeN-1) = ArgModeN
    % In new code, use describe_one_pred_name_mode in hlds_error_util instead.
    %
:- pred report_pred_name_mode(pred_or_func::in, string::in, list(mer_mode)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.process_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

process_all_nonimported_procs_errors(Task, !ModuleInfo, !Specs, !IO) :-
    True = (pred(_PredInfo::in) is semidet :- true),
    process_matching_nonimported_procs_errors(Task, True, !ModuleInfo,
        !Specs, !IO).

process_all_nonimported_procs(Task, !ModuleInfo, !IO) :-
    process_all_nonimported_procs_errors(Task, !ModuleInfo, [], Specs, !IO),
    expect(unify(Specs, []), this_file,
        "process_all_nonimported_procs: Specs").

process_all_nonimported_procs_update_errors(!Task, !ModuleInfo, !Specs, !IO) :-
    True = (pred(_PredInfo::in) is semidet :- true),
    process_matching_nonimported_procs_update_errors(!Task, True, !ModuleInfo,
        !Specs, !IO).

process_all_nonimported_procs_update(!Task, !ModuleInfo, !IO) :-
    process_all_nonimported_procs_update_errors(!Task, !ModuleInfo,
        [], Specs, !IO),
    expect(unify(Specs, []), this_file,
        "process_all_nonimported_procs_update: Specs").

process_matching_nonimported_procs_errors(Task, Filter, !ModuleInfo,
        !Specs, !IO) :-
    module_info_predids(PredIds, !ModuleInfo),
    ( Task = update_pred_error(Pred) ->
        list.foldl3(process_nonimported_pred(Pred, Filter), PredIds,
            !ModuleInfo, !Specs, !IO)
    ;
        process_nonimported_procs_in_preds(PredIds, Task, _, Filter,
            !ModuleInfo, !IO)
    ).

process_matching_nonimported_procs(Task, Filter, !ModuleInfo, !IO) :-
    process_matching_nonimported_procs_errors(Task, Filter, !ModuleInfo,
        [], Specs, !IO),
    expect(unify(Specs, []), this_file,
        "process_matching_nonimported_procs: Specs").

process_matching_nonimported_procs_update_errors(Task0, Task, Filter,
        !ModuleInfo, !Specs, !IO) :-
    module_info_predids(PredIds, !ModuleInfo),
    process_nonimported_procs_in_preds(PredIds, Task0, Task, Filter,
        !ModuleInfo, !IO).

process_matching_nonimported_procs_update(Task0, Task, Filter,
        !ModuleInfo, !IO) :-
    process_matching_nonimported_procs_update_errors(Task0, Task, Filter,
        !ModuleInfo, [], Specs, !IO),
    expect(unify(Specs, []), this_file,
        "process_matching_nonimported_procs_update_errors: Specs").

:- pred process_nonimported_pred(pred_error_task::in(pred_error_task),
    pred(pred_info)::in(pred(in) is semidet), pred_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

process_nonimported_pred(Task, Filter, PredId, !ModuleInfo, !Specs, !IO) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    (
        ( pred_info_is_imported(PredInfo0)
        ; \+ call(Filter, PredInfo0)
        )
    ->
        true
    ;
        Task(PredId, !ModuleInfo, PredInfo0, PredInfo, !Specs, !IO),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ).

:- pred process_nonimported_procs_in_preds(list(pred_id)::in,
    task::task, task::out(task), pred(pred_info)::in(pred(in) is semidet),
    module_info::in, module_info::out, io::di, io::uo) is det.

process_nonimported_procs_in_preds([], !Task, _, !ModuleInfo, !IO).
process_nonimported_procs_in_preds([PredId | PredIds], !Task, Filter,
        !ModuleInfo, !IO) :-
    module_info_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    ( call(Filter, PredInfo) ->
        ProcIds = pred_info_non_imported_procids(PredInfo),
        process_nonimported_procs(ProcIds, PredId, !Task, !ModuleInfo, !IO)
    ;
        true
    ),
    process_nonimported_procs_in_preds(PredIds, !Task, Filter,
        !ModuleInfo, !IO).

:- pred process_nonimported_procs(list(proc_id)::in, pred_id::in,
    task::task, task::out(task),
    module_info::in, module_info::out, io::di, io::uo) is det.

process_nonimported_procs([], _PredId, !Task, !ModuleInfo, !IO).
process_nonimported_procs([ProcId | ProcIds], PredId, !Task, !ModuleInfo,
        !IO) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, Pred0),
    pred_info_get_procedures(Pred0, Procs0),
    map.lookup(Procs0, ProcId, Proc0),

    (
        !.Task = update_module(Closure),
        Closure(PredId, ProcId, Pred0, Proc0, Proc, !ModuleInfo)
    ;
        !.Task = update_module_io(Closure),
        Closure(PredId, ProcId, Proc0, Proc, !ModuleInfo, !IO)
    ;
        !.Task = update_proc(Closure),
        Closure(!.ModuleInfo, Proc0, Proc)
    ;
        !.Task = update_proc_predid(Closure),
        Closure(PredId, !.ModuleInfo, Proc0, Proc)
    ;
        !.Task = update_proc_predprocid(Closure),
        Closure(PredId, ProcId, !.ModuleInfo, Pred0, Proc0, Proc)
    ;
        !.Task = update_proc_io(Closure),
        Closure(PredId, ProcId, !.ModuleInfo, Proc0, Proc, !IO)
    ;
        !.Task = update_proc_error(Closure),
        Closure(PredId, ProcId, !ModuleInfo, Proc0, Proc, WarnCnt, ErrCnt,
            !IO),
        passes_aux.handle_errors(WarnCnt, ErrCnt, !ModuleInfo, !IO)
    ;
        !.Task = update_pred_error(_),
        unexpected(this_file, "process_non_imported_procs")
    ;
        !.Task = update_module_cookie(Closure, Cookie0),
        Closure(PredId, ProcId, Proc0, Proc, Cookie0, Cookie1, !ModuleInfo),
        !:Task = update_module_cookie(Closure, Cookie1)
    ),

    % If the pass changed the module_info, it may have changed the pred table
    % or the proc table for this pred_id.  Don't take any chances.

    module_info_preds(!.ModuleInfo, Preds8),
    map.lookup(Preds8, PredId, Pred8),
    pred_info_get_procedures(Pred8, Procs8),

    map.det_update(Procs8, ProcId, Proc, Procs),
    pred_info_set_procedures(Procs, Pred8, Pred),
    map.det_update(Preds8, PredId, Pred, Preds),
    module_info_set_preds(Preds, !ModuleInfo),

    process_nonimported_procs(ProcIds, PredId, !Task, !ModuleInfo, !IO).

write_pred_progress_message(Message, PredId, ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io.write_string(Message, !IO),
        hlds_out.write_pred_id(ModuleInfo, PredId, !IO),
        io.write_string("\n", !IO)
    ;
        VeryVerbose = no
    ).

write_proc_progress_message(Message, proc(PredId, ProcId), ModuleInfo, !IO) :-
    write_proc_progress_message(Message, PredId, ProcId, ModuleInfo, !IO).

write_proc_progress_message(Message, PredId, ProcId, ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io.write_string(Message, !IO),
        hlds_out.write_pred_proc_id_pair(ModuleInfo, PredId, ProcId, !IO),
        io.write_string("\n", !IO)
    ;
        VeryVerbose = no
    ).

:- pred handle_errors(int::in, int::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

handle_errors(WarnCnt, ErrCnt, !ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
    (
        (
            ErrCnt > 0
        ;
            WarnCnt > 0,
            HaltAtWarn = yes
        )
    ->
        io.set_exit_status(1, !IO),
        module_info_incr_errors(!ModuleInfo)
    ;
        true
    ).

maybe_report_sizes(HLDS, !IO) :-
    globals.io_lookup_bool_option(statistics, Statistics, !IO),
    (
        Statistics = yes,
        report_sizes(HLDS, !IO)
    ;
        Statistics = no
    ).

:- pred report_sizes(module_info::in, io::di, io::uo) is det.

report_sizes(ModuleInfo, !IO) :-
    module_info_preds(ModuleInfo, Preds),
    tree_stats("Pred table", Preds, !IO),
    module_info_get_type_table(ModuleInfo, Types),
    tree_stats("Type table", Types, !IO),
    module_info_get_cons_table(ModuleInfo, Ctors),
    tree_stats("Constructor table", Ctors, !IO).

:- pred tree_stats(string::in, map(_K, _V)::in, io::di, io::uo) is det.

tree_stats(Description, Tree, !IO) :-
    map.count(Tree, Count),
    io.write_string(Description, !IO),
    io.write_string(": count = ", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

report_pred_proc_id(ModuleInfo, PredId, ProcId, MaybeContext, Context, !IO) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    proc_info_get_context(ProcInfo, Context),
    proc_info_get_argmodes(ProcInfo, ArgModes0),

    % We need to strip off the extra type_info arguments inserted at the
    % front by polymorphism.m - we only want the last `PredArity' of them.
    list.length(ArgModes0, NumArgModes),
    NumToDrop = NumArgModes - Arity,
    ( list.drop(NumToDrop, ArgModes0, ArgModes1) ->
        ArgModes = ArgModes1
    ;
        unexpected(this_file, "report_pred_proc_id: list.drop failed")
    ),
    (
        MaybeContext = yes(OutContext)
    ;
        MaybeContext = no,
        OutContext = Context
    ),
    prog_out.write_context(OutContext, !IO),
    io.write_string("In `", !IO),
    report_pred_name_mode(PredOrFunc, PredName, ArgModes, !IO),
    io.write_string("':\n", !IO).

report_pred_name_mode(pf_predicate, PredName, ArgModes, !IO) :-
    io.write_string(PredName, !IO),
    (
        ArgModes = [_ | _],
        varset.init(InstVarSet),   % XXX inst var names
        io.write_string("(", !IO),
        strip_builtin_qualifiers_from_mode_list(ArgModes, StrippedArgModes),
        mercury_output_mode_list(StrippedArgModes, InstVarSet, !IO),
        io.write_string(")", !IO)
    ;
        ArgModes = []
    ).

report_pred_name_mode(pf_function, FuncName, ArgModes, !IO) :-
    varset.init(InstVarSet),   % XXX inst var names
    strip_builtin_qualifiers_from_mode_list(ArgModes, StrippedArgModes),
    pred_args_to_func_args(StrippedArgModes, FuncArgModes, FuncRetMode),
    io.write_string(FuncName, !IO),
    (
        FuncArgModes = [_ | _],
        io.write_string("(", !IO),
        mercury_output_mode_list(FuncArgModes, InstVarSet, !IO),
        io.write_string(")", !IO)
    ;
        FuncArgModes = []
    ),
    io.write_string(" = ", !IO),
    mercury_output_mode(FuncRetMode, InstVarSet, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "passes_aux.m".

%-----------------------------------------------------------------------------%
:- end_module passes_aux.
%-----------------------------------------------------------------------------%
