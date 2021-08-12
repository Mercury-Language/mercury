%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.build.m.
%
% This module provides mechanisms to build targets.
%
%---------------------------------------------------------------------------%

:- module make.build.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_succeeded.

%---------------------------------------------------------------------------%

:- type build(T, Info1, Info2) ==
    pred(globals, T, maybe_succeeded, Info1, Info2, io, io).
:- type build(T, Info) == build(T, Info, Info).
:- type build(T) == build(T, make_info).
:- inst build == (pred(in, in, out, in, out, di, uo) is det).

    % build_with_module_options(Globals, ModuleName, ExtraArgs, Builder,
    %   Succeeded, !Info, !IO).
    %
    % Perform the given closure after updating the option_table in the globals
    % to contain the module-specific options for the specified module and
    % the extra options given in the ExtraArgs.
    % Adds `--invoked-by-mmc-make' and `--use-subdirs' to the option list.
    %
:- pred build_with_module_options(globals::in, module_name::in,
    list(string)::in, build(list(string))::in(build), maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % build_with_module_options_args(Globals, ModuleName, OptionsVariables,
    %   OptionArgs, ExtraArgs, Builder, Succeeded, !Info, !IO).
    %
    % Perform the given closure after updating the option_table in the globals
    % to contain the module-specific options for the specified module and
    % the extra options given in ExtraArgs and OptionArgs.
    % Does not add `--invoked-by-mmc-make' and `--use-subdirs' to the
    % option list.
    %
:- pred build_with_module_options_args(globals::in, module_name::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build), maybe_succeeded::out,
    Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

%---------------------%

    % Perform the given closure with an output stream created to append to
    % the error file for the given module.
    %
:- pred build_with_output_redirect(globals::in, module_name::in,
    build(io.text_output_stream)::in(build), maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- type build2(T, U) ==
    pred(globals, T, U, maybe_succeeded, make_info, make_info, io, io).
:- inst build2 == (pred(in, in, in, out, in, out, di, uo) is det).

:- pred build_with_module_options_and_output_redirect(globals::in,
    module_name::in, list(string)::in,
    build2(list(string), io.text_output_stream)::in(build2),
    maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------%

    % Produce an output stream which writes to the error file
    % for the given module.
    %
    % XXX We should do away with this predicate altogether,
    % and just have every part of the compiler write to explicitly specified
    % output streams.
    %
:- pred prepare_to_redirect_output(module_name::in,
    maybe(io.text_output_stream)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Close the module error output stream.
    %
    % XXX We should do away with this predicate altogether,
    % and just have every part of the compiler write to explicitly specified
    % output streams.
    %
:- pred unredirect_output(globals::in, module_name::in,
    io.text_output_stream::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Versions of foldl which stop if the supplied predicate returns
% Succeeded = `no' for any element of the list.
%

    % foldl2_pred_with_status(Globals, T, Succeeded, !Info).
    %
:- type foldl2_pred_with_status(T, Info, IO) ==
    pred(globals, T, maybe_succeeded, Info, Info, IO, IO).
:- inst foldl2_pred_with_status == (pred(in, in, out, in, out, di, uo) is det).

    % foldl2_maybe_stop_at_error(KeepGoing, P, Globals, List, Succeeded,
    %   !Info, !IO).
    %
:- pred foldl2_maybe_stop_at_error(maybe_keep_going::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

    % foldl3_pred_with_status(Globals, T, Succeeded, !Acc, !Info).
    %
:- type foldl3_pred_with_status(T, Acc, Info, IO) ==
    pred(globals, T, maybe_succeeded, Acc, Acc, Info, Info, IO, IO).
:- inst foldl3_pred_with_status ==
    (pred(in, in, out, in, out, in, out, di, uo) is det).

    % foldl3_maybe_stop_at_error(KeepGoing, P, Globals, List, Succeeded,
    %   !Acc, !Info).
    %
:- pred foldl3_maybe_stop_at_error(maybe_keep_going::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::out, Acc::in, Acc::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

%---------------------------------------------------------------------------%

    % foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing, P, Globals,
    %   List, Succeeded, !Info, !IO).
    %
    % Like foldl2_maybe_stop_at_error, but if parallel make is enabled,
    % it tries to perform a first pass that overlaps execution of P(elem)
    % in separate threads or processes. Updates to !Info in the first pass are
    % ignored. If the first pass succeeds, a second sequential pass is made in
    % which updates !Info are kept. Hence it must be safe to execute P(elem)
    % concurrently, in any order, and multiple times.
    %
:- pred foldl2_maybe_stop_at_error_maybe_parallel(maybe_keep_going::in,
    foldl2_pred_with_status(T, make_info, io)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % A lock to prevent interleaved output to standard output from parallel
    % processes.
    %
:- type stdout_lock.

:- pred with_locked_stdout(make_info::in,
    pred(io, io)::in(pred(di, uo) is det), io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.handle_options.
:- import_module libs.process_util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module getopt.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

build_with_module_options(Globals, ModuleName, ExtraOptions, Build, Succeeded,
        !Info, !IO) :-
    build_with_module_options_args_invoked(Globals, invoked_by_mmc_make,
        ModuleName, !.Info ^ mki_detected_grade_flags,
        !.Info ^ mki_options_variables, !.Info ^ mki_option_args,
        ExtraOptions, Build, Succeeded, !.Info, MaybeInfo, !IO),
    (
        MaybeInfo = yes(!:Info)
    ;
        MaybeInfo = no
    ).

build_with_module_options_args(Globals, ModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions,
        Build, Succeeded, !Info, !IO) :-
    build_with_module_options_args_invoked(Globals, not_invoked_by_mmc_make,
        ModuleName, DetectedGradeFlags, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO).

:- pred build_with_module_options_args_invoked(globals::in,
    maybe_invoked_by_mmc_make::in, module_name::in,
    list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    maybe_succeeded::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

build_with_module_options_args_invoked(Globals, InvokedByMmcMake, ModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions, Build,
        Succeeded, Info0, MaybeInfo, !IO) :-
    lookup_mmc_module_options(OptionVariables, ModuleName, ModuleOptionArgs,
        LookupSpecs, !IO),
    write_error_specs_ignore(Globals, LookupSpecs, !IO),
    LookupErrors = contains_errors(Globals, LookupSpecs),
    (
        LookupErrors = yes,
        MaybeInfo = no,
        Succeeded = did_not_succeed
    ;
        LookupErrors = no,
        % --invoked-by-mmc-make disables reading DEFAULT_MCFLAGS from the
        % environment (DEFAULT_MCFLAGS is included in OptionArgs) and
        % generation of `.d' files. --use-subdirs is needed because
        % the code to install libraries uses `--use-grade-subdirs' and
        % assumes the interface files were built with `--use-subdirs'.
        (
            InvokedByMmcMake = invoked_by_mmc_make,
            UseSubdirs = ["--use-subdirs"],
            InvokedByMake = ["--invoked-by-mmc-make"]
        ;
            InvokedByMmcMake = not_invoked_by_mmc_make,
            UseSubdirs = [],
            InvokedByMake = []
        ),

        AllOptionArgs = InvokedByMake ++ DetectedGradeFlags ++
            ModuleOptionArgs ++ OptionArgs ++ ExtraOptions ++ UseSubdirs,
        handle_given_options(AllOptionArgs, _, _,
            OptionSpecs, BuildGlobals, !IO),
        (
            OptionSpecs = [_ | _],
            Succeeded = did_not_succeed,
            MaybeInfo = no,
            get_error_output_stream(BuildGlobals, ModuleName,
                ErrorStream, !IO),
            usage_errors(ErrorStream, BuildGlobals, OptionSpecs, !IO)
        ;
            OptionSpecs = [],
            Build(BuildGlobals, AllOptionArgs, Succeeded, Info0, Info, !IO),
            MaybeInfo = yes(Info)
        )
    ).

%---------------------%

build_with_output_redirect(Globals, ModuleName, Build, Succeeded,
        !Info, !IO) :-
    prepare_to_redirect_output(ModuleName, RedirectResult, !Info, !IO),
    (
        RedirectResult = no,
        Succeeded = did_not_succeed
    ;
        RedirectResult = yes(ErrorStream),
        Build(Globals, ErrorStream, Succeeded, !Info, !IO),
        unredirect_output(Globals, ModuleName, ErrorStream, !Info, !IO)
    ).

build_with_module_options_and_output_redirect(Globals, ModuleName,
        ExtraOptions, Build, Succeeded, !Info, !IO) :-
    build_with_module_options(Globals, ModuleName, ExtraOptions,
        build_with_module_options_and_output_redirect_2(ModuleName, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_2(module_name::in,
    build2(list(string), io.text_output_stream)::in(build2), globals::in,
    list(string)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_with_module_options_and_output_redirect_2(ModuleName, Build, Globals,
        AllOptions, Succeeded, !Info, !IO) :-
    build_with_output_redirect(Globals, ModuleName,
        build_with_module_options_and_output_redirect_3(AllOptions, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_3(list(string)::in,
    build2(list(string), io.text_output_stream)::in(build2), globals::in,
    io.text_output_stream::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

build_with_module_options_and_output_redirect_3(AllOptions, Build, Globals,
        ErrorStream, Succeeded, !Info, !IO) :-
    Build(Globals, AllOptions, ErrorStream, Succeeded, !Info, !IO).

%---------------------------------------------------------------------------%

prepare_to_redirect_output(_ModuleName, MaybeErrorStream, !Info, !IO) :-
    % Write the output to a temporary file first, to make it easy
    % to just print the part of the error file that relates to the
    % current command. It will be appended to the error file later.
    open_temp_output(ErrorFileResult, !IO),
    (
        ErrorFileResult = ok({_ErrorFileName, ErrorOutputStream}),
        MaybeErrorStream = yes(ErrorOutputStream)
    ;
        ErrorFileResult = error(ErrorMessage),
        MaybeErrorStream = no,
        with_locked_stdout(!.Info,
            write_error_creating_temp_file(ErrorMessage), !IO)
    ).

unredirect_output(Globals, ModuleName, ErrorOutputStream, !Info, !IO) :-
    io.output_stream_name(ErrorOutputStream, TmpErrorFileName, !IO),
    io.close_output(ErrorOutputStream, !IO),

    io.open_input(TmpErrorFileName, TmpErrorInputRes, !IO),
    (
        TmpErrorInputRes = ok(TmpErrorInputStream),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".err")), ModuleName, ErrorFileName, !IO),
        ( if set.member(ModuleName, !.Info ^ mki_error_file_modules) then
            io.open_append(ErrorFileName, ErrorFileRes, !IO)
        else
            io.open_output(ErrorFileName, ErrorFileRes, !IO)
        ),
        (
            ErrorFileRes = ok(ErrorFileOutputStream),
            globals.lookup_int_option(Globals, output_compile_error_lines,
                LinesToWrite),
            io.output_stream(CurrentOutputStream, !IO),
            with_locked_stdout(!.Info,
                make_write_error_streams(TmpErrorFileName, TmpErrorInputStream,
                    ErrorFileOutputStream, CurrentOutputStream, LinesToWrite),
                !IO),
            io.close_output(ErrorFileOutputStream, !IO),

            !Info ^ mki_error_file_modules :=
                set.insert(!.Info ^ mki_error_file_modules, ModuleName)
        ;
            ErrorFileRes = error(Error),
            with_locked_stdout(!.Info,
                write_error_opening_file(TmpErrorFileName, Error), !IO)
        ),
        io.close_input(TmpErrorInputStream, !IO)
    ;
        TmpErrorInputRes = error(Error),
        with_locked_stdout(!.Info,
            write_error_opening_file(TmpErrorFileName, Error), !IO)
    ),
    io.remove_file(TmpErrorFileName, _, !IO).

:- pred make_write_error_streams(string::in, io.input_stream::in,
    io.text_output_stream::in, io.text_output_stream::in, int::in,
    io::di, io::uo) is det.

make_write_error_streams(FileName, InputStream,
        FullOutputStream, PartialOutputStream, LinesToWrite, !IO) :-
    io.input_stream_foldl2_io(InputStream,
        make_write_error_char(FullOutputStream, PartialOutputStream),
        LinesToWrite, Res, !IO),
    (
        Res = ok(_)
    ;
        Res = error(_, Error),
        io.format("Error reading `%s': %s\n",
            [s(FileName), s(io.error_message(Error))], !IO)
    ).

:- pred make_write_error_char(io.text_output_stream::in,
    io.text_output_stream::in, char::in, int::in, int::out,
    io::di, io::uo) is det.

make_write_error_char(FullOutputStream, PartialOutputStream, Char,
        !LinesRemaining, !IO) :-
    io.write_char(FullOutputStream, Char, !IO),
    ( if !.LinesRemaining > 0 then
        io.write_char(PartialOutputStream, Char, !IO),
        ( if Char = '\n' then
            !:LinesRemaining = !.LinesRemaining - 1
        else
            true
        )
    else if !.LinesRemaining = 0 then
        io.output_stream_name(FullOutputStream, FullOutputFileName, !IO),
        io.format(PartialOutputStream,
            "... error log truncated, see `%s' for the complete log.\n",
            [s(FullOutputFileName)], !IO),
        !:LinesRemaining = -1
    else
        true
    ).

:- pred write_error_opening_file(string::in, io.error::in, io::di, io::uo)
    is det.

write_error_opening_file(FileName, Error, !IO) :-
    io.format("Error opening `%s': %s\n",
        [s(FileName), s(io.error_message(Error))], !IO).

:- pred write_error_creating_temp_file(string::in, io::di, io::uo) is det.

write_error_creating_temp_file(ErrorMessage, !IO) :-
    io.write_string(ErrorMessage ++ "\n", !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Globals, Targets, Succeeded,
        !Info, !IO) :-
    foldl2_maybe_stop_at_error_loop(KeepGoing, MakeTarget, Globals, Targets,
        succeeded, Succeeded, !Info, !IO).

:- pred foldl2_maybe_stop_at_error_loop(maybe_keep_going::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::in, maybe_succeeded::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

foldl2_maybe_stop_at_error_loop(_KeepGoing, _P, _Globals, [], !Succeeded,
        !Info, !IO).
foldl2_maybe_stop_at_error_loop(KeepGoing, P, Globals, [T | Ts], !Succeeded,
        !Info, !IO) :-
    P(Globals, T, NewSucceeded, !Info, !IO),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        foldl2_maybe_stop_at_error_loop(KeepGoing, P, Globals, Ts,
            !Succeeded, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

foldl3_maybe_stop_at_error(KeepGoing, P, Globals, Ts, Succeeded,
        !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_loop(KeepGoing, P, Globals, Ts,
        succeeded, Succeeded, !Acc, !Info, !IO).

:- pred foldl3_maybe_stop_at_error_loop(maybe_keep_going::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::in, maybe_succeeded::out,
    Acc::in, Acc::out, Info::in, Info::out, IO::di, IO::uo) is det.

foldl3_maybe_stop_at_error_loop(_KeepGoing, _P, _Globals, [],
        !Succeeded, !Acc, !Info, !IO).
foldl3_maybe_stop_at_error_loop(KeepGoing, P, Globals, [T | Ts],
        !Succeeded, !Acc, !Info, !IO) :-
    P(Globals, T, NewSucceeded, !Acc, !Info, !IO),
    ( if
        ( NewSucceeded = succeeded
        ; KeepGoing = do_keep_going
        )
    then
        !:Succeeded = !.Succeeded `and` NewSucceeded,
        foldl3_maybe_stop_at_error_loop(KeepGoing, P, Globals, Ts,
            !Succeeded, !Acc, !Info, !IO)
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Parallel (concurrent) fold.
%

foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing, MakeTarget, Globals,
        Targets, Succeeded, !Info, !IO) :-
    globals.lookup_int_option(Globals, jobs, Jobs),
    ( if
        Jobs > 1,
        process_util.can_fork,
        have_job_ctl_ipc
    then
        % First pass.
        foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs,
            MakeTarget, Globals, Targets, Succeeded0, !Info, !IO),
        % Second pass (sequential).
        (
            Succeeded0 = succeeded,
            % Disable the `--rebuild' option during the sequential pass
            % otherwise all the targets will be built a second time.
            globals.set_option(rebuild, bool(no), Globals, NoRebuildGlobals),
            foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, NoRebuildGlobals,
                Targets, Succeeded, !Info, !IO)
        ;
            Succeeded0 = did_not_succeed,
            Succeeded = did_not_succeed
        )
    else
        foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Globals,
            Targets, Succeeded, !Info, !IO)
    ).

:- pred foldl2_maybe_stop_at_error_parallel_processes(maybe_keep_going::in,
    int::in,
    foldl2_pred_with_status(T, make_info, io)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, maybe_succeeded::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs, MakeTarget,
        Globals, Targets, Succeeded, !Info, !IO) :-
    TotalTasks = list.length(Targets),
    create_job_ctl(TotalTasks, MaybeJobCtl, !IO),
    (
        MaybeJobCtl = yes(JobCtl),
        !Info ^ mki_maybe_stdout_lock := yes(JobCtl),
        list.foldl2(
            start_worker_process(Globals, KeepGoing, MakeTarget, Targets,
                JobCtl, !.Info),
            2 .. Jobs, [], Pids, !IO),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        build_with_check_for_interrupt(VeryVerbose,
            worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl,
                succeeded),
            worker_loop_signal_cleanup(JobCtl, Pids), Succeeded0, !Info, !IO),
        list.foldl2(reap_worker_process, Pids, Succeeded0, Succeeded, !IO),
        !Info ^ mki_maybe_stdout_lock := no,
        destroy_job_ctl(JobCtl, !IO)
    ;
        MaybeJobCtl = no,
        Succeeded = did_not_succeed
    ).

:- pred start_worker_process(globals::in, maybe_keep_going::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, job_ctl::in, Info::in, int::in, list(pid)::in, list(pid)::out,
    io::di, io::uo) is det.

start_worker_process(Globals, KeepGoing, MakeTarget, Targets, JobCtl, Info,
        _ChildN, !Pids, !IO) :-
    start_in_forked_process(
        child_worker(Globals, KeepGoing, MakeTarget, Targets, JobCtl, Info),
        MaybePid, !IO),
    (
        MaybePid = yes(Pid),
        !:Pids = [Pid | !.Pids]
    ;
        MaybePid = no
    ).

:- pred child_worker(globals::in, maybe_keep_going::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, job_ctl::in, Info::in, maybe_succeeded::out,
    io::di, io::uo) is det.

child_worker(Globals, KeepGoing, MakeTarget, Targets, JobCtl, Info0,
        Succeeded, !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    build_with_check_for_interrupt(VeryVerbose,
        worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl,
            succeeded),
        worker_loop_signal_cleanup(JobCtl, []), Succeeded, Info0, _Info, !IO).

:- pred worker_loop(globals::in, maybe_keep_going::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, job_ctl::in, maybe_succeeded::in, maybe_succeeded::out,
    Info::in, Info::out, io::di, io::uo) is det.

worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl,
        !Succeeded, !Info, !IO) :-
    accept_task(JobCtl, TaskNumber, !IO),
    ( if TaskNumber >= 0 then
        Target = list.det_index0(Targets, TaskNumber),
        MakeTarget(Globals, Target, TargetSucceeded, !Info, !IO),
        (
            TargetSucceeded = succeeded,
            mark_task_done(JobCtl, TaskNumber, !IO)
        ;
            TargetSucceeded = did_not_succeed,
            KeepGoingBool = ( if KeepGoing = do_keep_going then yes else no ),
            mark_task_error(JobCtl, TaskNumber, KeepGoingBool, !IO),
            !:Succeeded = did_not_succeed
        ),
        worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl,
            !Succeeded, !Info, !IO)
    else
        % No more tasks.
        true
    ).

:- pred worker_loop_signal_cleanup(job_ctl::in, list(pid)::in,
    Info::in, Info::out, io::di, io::uo) is det.

worker_loop_signal_cleanup(JobCtl, Pids, !Info, !IO) :-
    mark_abort(JobCtl, !IO),
    list.foldl(send_signal(sigint), Pids, !IO).

:- pred reap_worker_process(pid::in, maybe_succeeded::in, maybe_succeeded::out,
    io::di, io::uo) is det.

reap_worker_process(Pid, !Succeeded, !IO) :-
    wait_pid(Pid, Status, !IO),
    ( if
        !.Succeeded = succeeded,
        Status = ok(exited(0))
    then
        true
    else
        !:Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Shared memory IPC for parallel workers.
%

:- pragma foreign_decl("C", "
typedef struct MC_JobCtl MC_JobCtl;
").

:- pragma foreign_decl("C", local,
"
#ifdef MR_HAVE_SYS_MMAN_H
  #include <sys/mman.h>

  // Just in case.
  #if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
    #define MAP_ANONYMOUS MAP_ANON
  #endif
#endif

#ifdef MAP_ANONYMOUS
  // Darwin 5.x and FreeBSD do not implement process-shared POSIX mutexes.
  // Use System V semaphores instead. As System V semaphores seem to be more
  // widely supported we may consider using them exclusively or in preference
  // to POSIX mutexes in the future.
  #if !defined(__APPLE__) && !defined(__FreeBSD__) && \
        defined(MR_HAVE_PTHREAD_H) && \
        defined(MR_HAVE_PTHREAD_MUTEXATTR_SETPSHARED)
    #include <pthread.h>

    #define MC_HAVE_JOBCTL_IPC 1
  #elif defined(MR_HAVE_SYS_SEM_H)
    #include <sys/sem.h>

    #define MC_USE_SYSV_SEMAPHORE 1
    #define MC_HAVE_JOBCTL_IPC 1
  #endif
#endif

#ifdef MC_HAVE_JOBCTL_IPC

typedef enum MC_TaskStatus MC_TaskStatus;

enum MC_TaskStatus {
    TASK_NEW,       // task not yet attempted
    TASK_ACCEPTED,  // someone is working on this task
    TASK_DONE,      // task successfully completed
    TASK_ERROR      // error occurred when working on the task
};

// This structure is placed in shared memory.
struct MC_JobCtl {
    // Static data.
    MR_Integer      jc_total_tasks;

    // Dynamic data.  The mutex protects the rest.
  #ifdef MC_USE_SYSV_SEMAPHORE
    int             jc_semid;
  #else
    pthread_mutex_t jc_mutex;
  #endif
    MR_bool         jc_abort;
    MC_TaskStatus   jc_tasks[MR_VARIABLE_SIZED];
};

#define MC_JOB_CTL_SIZE(N)  (sizeof(MC_JobCtl) + (N) * sizeof(MC_TaskStatus))

static MC_JobCtl *  MC_create_job_ctl(MR_Integer total_tasks);
static void         MC_lock_job_ctl(MC_JobCtl *job_ctl);
static void         MC_unlock_job_ctl(MC_JobCtl *job_ctl);

#endif // MC_HAVE_JOBCTL_IPC
").

:- pragma foreign_code("C", "

#ifdef MC_HAVE_JOBCTL_IPC

static MC_JobCtl *
MC_create_job_ctl(MR_Integer total_tasks)
{
    size_t              size;
    MC_JobCtl           *job_ctl;
    MR_Integer          i;

    size = MC_JOB_CTL_SIZE(total_tasks);

    // Create the shared memory segment.
    job_ctl = mmap(NULL, size, PROT_READ | PROT_WRITE,
        MAP_ANONYMOUS | MAP_SHARED, -1, 0);
    if (job_ctl == (void *) -1) {
        perror(""MC_create_job_ctl: mmap"");
        return NULL;
    }

#ifdef MC_USE_SYSV_SEMAPHORE
    {
        struct sembuf sb;

        job_ctl->jc_semid = semget(IPC_PRIVATE, 1, 0600);
        if (job_ctl->jc_semid == -1) {
            perror(""MC_create_sem: semget"");
            munmap(job_ctl, size);
            return NULL;
        }

        sb.sem_num = 0;
        sb.sem_op = 1;
        sb.sem_flg = 0;
        if (semop(job_ctl->jc_semid, &sb, 1) == -1) {
            perror(""MC_create_sem: semop"");
            semctl(job_ctl->jc_semid, 0, IPC_RMID);
            munmap(job_ctl, size);
            return NULL;
        }
    }
#else
    {
        pthread_mutexattr_t mutex_attr;

        pthread_mutexattr_init(&mutex_attr);
        if (pthread_mutexattr_setpshared(&mutex_attr, PTHREAD_PROCESS_SHARED)
            != 0)
        {
            perror(""MC_create_job_ctl: pthread_mutexattr_setpshared"");
            pthread_mutexattr_destroy(&mutex_attr);
            munmap(job_ctl, size);
            return NULL;
        }

        if (pthread_mutex_init(&job_ctl->jc_mutex, &mutex_attr) != 0) {
            perror(""MC_create_job_ctl: sem_init"");
            pthread_mutexattr_destroy(&mutex_attr);
            munmap(job_ctl, size);
            return NULL;
        }

        pthread_mutexattr_destroy(&mutex_attr);
    }
#endif

    job_ctl->jc_total_tasks = total_tasks;
    job_ctl->jc_abort = MR_FALSE;
    for (i = 0; i < total_tasks; i++) {
        job_ctl->jc_tasks[i] = TASK_NEW;
    }

    return job_ctl;
}

static void
MC_lock_job_ctl(MC_JobCtl *job_ctl)
{
#ifdef MC_USE_SYSV_SEMAPHORE
    struct sembuf sb;

    sb.sem_num = 0;
    sb.sem_op = -1;
    sb.sem_flg = 0;
    if (semop(job_ctl->jc_semid, &sb, 1) == -1) {
        perror(""MC_lock_job_ctl: semop"");
    }
#else
    pthread_mutex_lock(&job_ctl->jc_mutex);
#endif
}

static void
MC_unlock_job_ctl(MC_JobCtl *job_ctl)
{
#ifdef MC_USE_SYSV_SEMAPHORE
    struct sembuf sb;

    sb.sem_num = 0;
    sb.sem_op = 1;
    sb.sem_flg = 0;
    if (semop(job_ctl->jc_semid, &sb, 1) == -1) {
        perror(""MC_unlock_job_ctl: semop"");
    }
#else
    pthread_mutex_unlock(&job_ctl->jc_mutex);
#endif
}

#endif // MC_HAVE_JOBCTL_IPC
").

:- type job_ctl.
:- pragma foreign_type("C", job_ctl, "MC_JobCtl *").
:- pragma foreign_type("C#", job_ctl, "object").                % stub
:- pragma foreign_type("Java", job_ctl, "java.lang.Object").    % stub

:- pred have_job_ctl_ipc is semidet.

have_job_ctl_ipc :-
    semidet_fail.

:- pragma foreign_proc("C",
    have_job_ctl_ipc,
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pred create_job_ctl(int::in, maybe(job_ctl)::out, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(create_job_ctl/4)).

:- pragma foreign_proc("C",
    create_job_ctl(TotalJobs::in, MaybeJobCtl::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_JobCtl *job_ctl;

    job_ctl = MC_create_job_ctl(TotalJobs);
    if (job_ctl != NULL) {
        MaybeJobCtl = MC_make_yes_job_ctl(job_ctl);
    } else {
        MaybeJobCtl = MC_make_no_job_ctl();
    }
#else
    MaybeJobCtl = MC_make_no_job_ctl();
#endif
").

create_job_ctl(_, _, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- pred destroy_job_ctl(job_ctl::in, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(destroy_job_ctl/3)).

:- pragma foreign_proc("C",
    destroy_job_ctl(JobCtl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
  #ifdef MC_USE_SYSV_SEMAPHORE
    semctl(JobCtl->jc_semid, 0, IPC_RMID);
  #else
    pthread_mutex_destroy(&JobCtl->jc_mutex);
  #endif

    munmap(JobCtl, MC_JOB_CTL_SIZE(JobCtl->jc_total_tasks));
#endif
").

destroy_job_ctl(_, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- pred accept_task(job_ctl::in, int::out, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(accept_task/4)).

:- pragma foreign_proc("C",
    accept_task(JobCtl::in, TaskNumber::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    TaskNumber = -1;

#ifdef MC_HAVE_JOBCTL_IPC
    MC_lock_job_ctl(JobCtl);

    if (!JobCtl->jc_abort) {
        MR_Integer  i;

        for (i = 0; i < JobCtl->jc_total_tasks; i++) {
            if (JobCtl->jc_tasks[i] == TASK_NEW) {
                JobCtl->jc_tasks[i] = TASK_ACCEPTED;
                TaskNumber = i;
                break;
            }
        }
    }

    MC_unlock_job_ctl(JobCtl);
#endif
").

accept_task(_, _, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- pred mark_task_done(job_ctl::in, int::in, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(mark_task_done/4)).

:- pragma foreign_proc("C",
    mark_task_done(JobCtl::in, TaskNumber::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_lock_job_ctl(JobCtl);
    JobCtl->jc_tasks[TaskNumber] = TASK_DONE;
    MC_unlock_job_ctl(JobCtl);
#endif
").

mark_task_done(_, _, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- pred mark_task_error(job_ctl::in, int::in, bool::in, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(mark_task_error/5)).

:- pragma foreign_proc("C",
    mark_task_error(JobCtl::in, TaskNumber::in, KeepGoing::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_lock_job_ctl(JobCtl);

    JobCtl->jc_tasks[TaskNumber] = TASK_ERROR;
    if (!KeepGoing) {
        JobCtl->jc_abort = MR_TRUE;
    }

    MC_unlock_job_ctl(JobCtl);
#endif
").

mark_task_error(_, _, _, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- pred mark_abort(job_ctl::in, io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(mark_abort/3)).

:- pragma foreign_proc("C",
    mark_abort(JobCtl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_lock_job_ctl(JobCtl);
    JobCtl->jc_abort = MR_TRUE;
    MC_unlock_job_ctl(JobCtl);
#endif
").

mark_abort(_, _, _) :-
    unexpected($file, $pred, "non-C backend").

:- func make_yes_job_ctl(job_ctl) = maybe(job_ctl).
:- pragma foreign_export("C", make_yes_job_ctl(in) = out,
    "MC_make_yes_job_ctl").

make_yes_job_ctl(JobCtl) = yes(JobCtl).

:- func make_no_job_ctl = maybe(job_ctl).
:- pragma foreign_export("C", make_no_job_ctl = out,
    "MC_make_no_job_ctl").

make_no_job_ctl = no.

%---------------------------------------------------------------------------%
%
% Prevent interleaved error output.
%

    % We reuse the job_ctl type.
    %
:- type stdout_lock == job_ctl.

:- pred lock_stdout(stdout_lock::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    lock_stdout(JobCtl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_lock_job_ctl(JobCtl);
#endif
").

lock_stdout(_, !IO).

:- pred unlock_stdout(stdout_lock::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unlock_stdout(JobCtl::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MC_HAVE_JOBCTL_IPC
    MC_unlock_job_ctl(JobCtl);
#endif
").

unlock_stdout(_, !IO).

with_locked_stdout(Info, Pred, !IO) :-
    MaybeLock = Info ^ mki_maybe_stdout_lock,
    (
        MaybeLock = yes(Lock),
        lock_stdout(Lock, !IO),
        Pred(!IO),
        unlock_stdout(Lock, !IO)
    ;
        MaybeLock = no,
        Pred(!IO)
    ).

%---------------------------------------------------------------------------%
:- end_module make.build.
%---------------------------------------------------------------------------%
