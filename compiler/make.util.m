%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2008 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.util.m.
% Main author: stayl.
%
% Assorted predicates used to implement `mmc --make'.
%
%-----------------------------------------------------------------------------%

:- module make.util.
:- interface.

%-----------------------------------------------------------------------------%
%
% Versions of foldl which stop if the supplied predicate returns `no'
% for any element of the list.
%

    % foldl2_pred_with_status(T, Succeeded, !Info).
    %
:- type foldl2_pred_with_status(T, Info, IO) ==
    pred(T, bool, Info, Info, IO, IO).
:- inst foldl2_pred_with_status == (pred(in, out, in, out, di, uo) is det).

    % foldl2_maybe_stop_at_error(KeepGoing, P, List, Succeeded, !Info).
    %
:- pred foldl2_maybe_stop_at_error(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    list(T)::in, bool::out, Info::in, Info::out, IO::di, IO::uo) is det.

    % foldl3_pred_with_status(T, Succeeded, !Acc, !Info).
    %
:- type foldl3_pred_with_status(T, Acc, Info, IO) ==
    pred(T, bool, Acc, Acc, Info, Info, IO, IO).
:- inst foldl3_pred_with_status ==
    (pred(in, out, in, out, in, out, di, uo) is det).

    % foldl3_maybe_stop_at_error(KeepGoing, P, List, Succeeded, !Acc,
    %   !Info).
    %
:- pred foldl3_maybe_stop_at_error(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    list(T)::in, bool::out, Acc::in, Acc::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

%-----------------------------------------------------------------------------%

    % foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing, P, List, Succeeded,
    %   !Info, !IO).
    %
    % Like foldl2_maybe_stop_at_error, but if parallel make is enabled, it
    % tries to perform a first pass that overlaps execution of P(elem) in
    % separate threads or processes.  Updates to !Info in the first pass are
    % ignored.  If the first pass succeeds, a second sequential pass is made in
    % which updates !Info are kept.  Hence it must be safe to execute P(elem)
    % concurrently, in any order, and multiple times.
    %
:- pred foldl2_maybe_stop_at_error_maybe_parallel(bool::in,
    foldl2_pred_with_status(T, make_info, io)::in(foldl2_pred_with_status),
    list(T)::in, bool::out, make_info::in, make_info::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

:- type build(T, Info1, Info2) == pred(T, bool, Info1, Info2, io, io).
:- type build(T, Info) == build(T, Info, Info).
:- type build(T) == build(T, make_info).
:- inst build == (pred(in, out, in, out, di, uo) is det).

    % build_with_module_options(ModuleName, ExtraArgs, Builder, Succeeded,
    %   !Info).
    %
    % Perform the given closure after updating the option_table in the globals
    % in the io.state to contain the module-specific options for the specified
    % module and the extra options given in the ExtraArgs.
    % Adds `--invoked-by-mmc-make' and `--use-subdirs' to the option list.
    % The old option table will be restored afterwards.
    %
:- pred build_with_module_options(module_name::in,
    list(string)::in, build(list(string))::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % build_with_module_options_args(ModuleName, OptionsVariables,
    %   OptionArgs, ExtraArgs, Builder, Succeeded, !Info).
    %
    % Perform the given closure after updating the option_table in the globals
    % in the io.state to contain the module-specific options for the specified
    % module and the extra options given in ExtraArgs and OptionArgs.
    % Does not add `--invoked-by-mmc-make' and `--use-subdirs' to the
    % option list. The old option table will be restored afterwards.
    %
:- pred build_with_module_options_args(module_name::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

    % Perform the given closure with an output stream created
    % to append to the error file for the given module.
    %
:- pred build_with_output_redirect(module_name::in,
    build(io.output_stream)::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Produce an output stream which writes to the error file
    % for the given module.
    %
:- pred redirect_output(module_name::in, maybe(io.output_stream)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Close the module error output stream.
    %
:- pred unredirect_output(module_name::in, io.output_stream::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type build2(T, U) == pred(T, U, bool, make_info, make_info, io, io).
:- inst build2 == (pred(in, in, out, in, out, di, uo) is det).

:- pred build_with_module_options_and_output_redirect(module_name::in,
    list(string)::in, build2(list(string), io.output_stream)::in(build2),
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timestamp handling
%

    % Find the timestamp updated when a target is produced.
    %
:- pred get_timestamp_file_timestamp(target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(dependency_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_target_timestamp(Search, TargetFile, Timestamp)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(maybe_search::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_file_name(Search, TargetFile, FileName).
    %
    % Compute a file name for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_file_name(maybe_search::in, target_file::in, file_name::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Find the timestamp of the first file matching the given
    % file name in one of the given directories.
    %
:- pred get_file_timestamp(list(dir_name)::in, file_name::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Return the oldest of the timestamps if both are of the form
    % `ok(Timestamp)', returning `error(Error)' otherwise.
    %
:- func find_oldest_timestamp(maybe_error(timestamp),
    maybe_error(timestamp)) = maybe_error(timestamp).

%-----------------------------------------------------------------------------%
%
% Remove file a file, deleting the cached timestamp
% The removal is reported to the user if the given boolean option is set.
% In general the option given should be `--very-verbose' when making a
% `.clean' or `.realclean target', and `--verbose-make' when cleaning
% after an interrupted build.
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(option::in, target_file::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(option::in, module_name::in,
    module_target_type::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % make_remove_file(VerboseOption, ModuleName, Extension, !Info).
    %
:- pred make_remove_file(option::in, module_name::in, string::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_remove_file(option::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func make_target_file_list(list(module_name), module_target_type) =
    list(target_file).

:- func make_dependency_list(list(module_name), module_target_type)
    = list(dependency_file).

:- func target_extension(globals, module_target_type) = maybe(string).
:- mode target_extension(in, in) = out is det.
:- mode target_extension(in, out) = in(bound(yes(ground))) is nondet.

:- pred linked_target_file_name(module_name::in, linked_target_type::in,
    file_name::out, io::di, io::uo) is det.

    % Find the extension for the timestamp file for the
    % given target type, if one exists.
    %
:- func timestamp_extension(globals, module_target_type) = string is semidet.

:- pred target_is_grade_or_arch_dependent(module_target_type::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Debugging, verbose and error messages
%

    % Apply the given predicate if `--debug-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred debug_msg(pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is det.

    % Apply the given predicate if `--verbose-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_msg(pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Apply the given predicate if the given boolean option is set to `yes'.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_msg(option::in, pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Write a debugging message relating to a given target file.
    %
:- pred debug_file_msg(target_file::in, string::in, io::di, io::uo) is det.

:- pred make_write_dependency_file(dependency_file::in, io::di, io::uo) is det.

:- pred make_write_dependency_file_list(list(dependency_file)::in,
    io::di, io::uo) is det.

:- pred make_write_target_file(target_file::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_linked_target_message(file_name::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_target_message(target_file::in, io::di, io::uo) is det.

:- pred maybe_make_target_message_to_stream(io.output_stream::in,
    target_file::in, io::di, io::uo) is det.

    % Write a message "Reanalysing invalid/suboptimal modules" if
    % `--verbose-make' is set.
    %
:- pred maybe_reanalyse_modules_message(io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred target_file_error(target_file::in, io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred file_error(file_name::in, io::di, io::uo) is det.

    % If the given target was specified on the command line, warn that it
    % was already up to date.
    %
:- pred maybe_warn_up_to_date_target(pair(module_name, target_type)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Write a message "Made symlink/copy of <filename>" if
    % `--verbose-make' is set.
    %
:- pred maybe_symlink_or_copy_linked_target_message(
    pair(module_name, target_type)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timing
%

:- pred get_real_milliseconds(int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Hash functions
%

:- pred module_name_hash(module_name::in, int::out) is det.

:- pred dependency_file_hash(dependency_file::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.compiler_util.
:- import_module libs.handle_options.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module exception.
:- import_module getopt_io.
:- import_module set.
:- import_module thread.
:- import_module thread.channel.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Targets, Success,
        !Info, !IO) :-
    foldl2_maybe_stop_at_error_2(KeepGoing, MakeTarget, Targets, yes, Success,
        !Info, !IO).

:- pred foldl2_maybe_stop_at_error_2(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    list(T)::in, bool::in, bool::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

foldl2_maybe_stop_at_error_2(_KeepGoing, _P, [], !Success, !Info, !IO).
foldl2_maybe_stop_at_error_2(KeepGoing, P, [T | Ts], !Success, !Info, !IO) :-
    P(T, NewSuccess, !Info, !IO),
    (
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    ->
        !:Success = !.Success `and` NewSuccess,
        foldl2_maybe_stop_at_error_2(KeepGoing, P, Ts, !Success, !Info, !IO)
    ;
        !:Success = no
    ).

foldl3_maybe_stop_at_error(KeepGoing, P, Ts, Success, !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_2(KeepGoing, P, Ts, yes, Success,
        !Acc, !Info, !IO).

:- pred foldl3_maybe_stop_at_error_2(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    list(T)::in, bool::in, bool::out, Acc::in, Acc::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

foldl3_maybe_stop_at_error_2(_KeepGoing, _P, [], !Success, !Acc, !Info, !IO).
foldl3_maybe_stop_at_error_2(KeepGoing, P, [T | Ts],
        !Success, !Acc, !Info, !IO) :-
    P(T, NewSuccess, !Acc, !Info, !IO),
    (
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    ->
        !:Success = !.Success `and` NewSuccess,
        foldl3_maybe_stop_at_error_2(KeepGoing, P, Ts, !Success, !Acc,
            !Info, !IO)
    ;
        !:Success = no
    ).

%-----------------------------------------------------------------------------%
%
% Parallel (concurrent) fold
%

:- type child_exit
    --->    child_succeeded
    ;       child_failed
    ;       child_exception(univ).

:- inst child_succeeded_or_failed
    --->    child_succeeded
    ;       child_failed.

    % A generic interface for the two parallel fold implementations:
    % one using processes and one using threads.
    %
:- typeclass par_fold(PF) where [

    % run_in_child(Pred, Info, T, Succeeded, !PF, !IO)
    %
    % Start executing Pred in a child thread/process.  Succeeded is `yes' iff
    % the child was successfully spawned.
    %
    pred run_in_child(
        foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
        Info::in, T::in, bool::out, PF::in, PF::out, io::di, io::uo) is det,

    % Block until a child exit code is received.
    %
    pred wait_for_child_exit(child_exit::out(child_succeeded_or_failed),
        PF::in, PF::out, io::di, io::uo) is det
].

foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing, MakeTarget, Targets,
        Success, !Info, !IO) :-
    globals.io_lookup_int_option(jobs, Jobs, !IO),
    ( Jobs > 1 ->
        % First pass.
        % fork() is disabled on threaded grades.
        ( process_util.can_fork ->
            foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs,
                MakeTarget, Targets, Success0, !.Info, !IO)
        ; thread.can_spawn ->
            foldl2_maybe_stop_at_error_parallel_threads(KeepGoing, Jobs,
                MakeTarget, Targets, Success0, !.Info, !IO)
        ;
            Success0 = yes
        ),
        % Second pass (sequential).
        (
            Success0 = yes,
            % Disable the `--rebuild' option during the sequential pass
            % otherwise all the targets will be built a second time.
            globals.io_lookup_bool_option(rebuild, Rebuild, !IO),
            globals.io_set_option(rebuild, bool(no), !IO),
            foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Targets, Success,
                !Info, !IO),
            globals.io_set_option(rebuild, bool(Rebuild), !IO)
        ;
            Success0 = no,
            Success = no
        )
    ;
        foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Targets, Success,
            !Info, !IO)
    ).

:- pred do_parallel_foldl2(bool::in, int::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    Info::in, list(T)::in, bool::out, PF::in, PF::out, io::di, io::uo)
    is det <= par_fold(PF).

do_parallel_foldl2(KeepGoing, Jobs, MakeTarget, Info, Targets, Success,
        !PF, !IO) :-
    list.split_upto(Jobs, Targets, InitialTargets, LaterTargets),
    start_initial_child_jobs(KeepGoing, MakeTarget, Info, InitialTargets,
        0, NumChildJobs, !PF, !IO),
    ( NumChildJobs < length(InitialTargets) ->
        Success0 = no
    ;
        Success0 = yes
    ),
    do_parallel_foldl2_parent_loop(KeepGoing, MakeTarget, Info,
        NumChildJobs, LaterTargets, Success0, Success, !PF, !IO).

:- pred start_initial_child_jobs(bool::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    Info::in, list(T)::in, int::in, int::out,
    PF::in, PF::out, io::di, io::uo) is det <= par_fold(PF).

start_initial_child_jobs(_KeepGoing, _MakeTarget, _Info,
        [], !NumChildJobs, !PF, !IO).
start_initial_child_jobs(KeepGoing, MakeTarget, Info,
        [Target | Targets], !NumChildJobs, !PF, !IO) :-
    run_in_child(MakeTarget, Info, Target, Success, !PF, !IO),
    (
        Success = yes,
        start_initial_child_jobs(KeepGoing, MakeTarget, Info, Targets,
            !.NumChildJobs + 1, !:NumChildJobs, !PF, !IO)
    ;
        Success = no,
        KeepGoing = yes,
        start_initial_child_jobs(KeepGoing, MakeTarget, Info, Targets,
            !NumChildJobs, !PF, !IO)
    ;
        Success = no,
        KeepGoing = no
    ).

:- pred do_parallel_foldl2_parent_loop(bool::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    Info::in, int::in, list(T)::in, bool::in, bool::out, PF::in, PF::out,
    io::di, io::uo) is det <= par_fold(PF).

do_parallel_foldl2_parent_loop(KeepGoing, MakeTarget, Info, NumChildJobs0,
        Targets, !Success, !PF, !IO) :-
    (
        % We are done once all running children have terminated and there are
        % no more targets to make.
        NumChildJobs0 = 0,
        Targets = []
    ->
        true
    ;
        % Wait for a running child to indicate that it is finished.
        wait_for_child_exit(Exit, !PF, !IO),
        (
            Exit = child_succeeded,
            NewSuccess = yes
        ;
            Exit = child_failed,
            NewSuccess = no
        ),
        (
            ( NewSuccess = yes
            ; KeepGoing = yes
            )
        ->
            !:Success = !.Success `and` NewSuccess,
            (
                Targets = [],
                MoreTargets = [],
                NumChildJobs = NumChildJobs0 - 1
            ;
                Targets = [NextTarget | MoreTargets],
                run_in_child(MakeTarget, Info, NextTarget, ChildStarted,
                    !PF, !IO),
                (
                    ChildStarted = yes,
                    NumChildJobs = NumChildJobs0
                ;
                    ChildStarted = no,
                    NumChildJobs = NumChildJobs0 - 1,
                    !:Success = no
                )
            ),
            do_parallel_foldl2_parent_loop(KeepGoing, MakeTarget, Info,
                NumChildJobs, MoreTargets, !Success, !PF, !IO)
        ;
            % Wait for the other running children to terminate before
            % returning.
            !:Success = no,
            wait_for_child_exits(NumChildJobs0 - 1, !PF, !IO)
        )
    ).

:- pred wait_for_child_exits(int::in,
    PF::in, PF::out, io::di, io::uo) is det <= par_fold(PF).

wait_for_child_exits(Num, !PF, !IO) :-
    ( Num > 0 ->
        wait_for_child_exit(_, !PF, !IO),
        wait_for_child_exits(Num - 1, !PF, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
%
% Parallel fold using processes
%

:- type fork_par_fold
    --->    fork_par_fold(
                fpf_children :: set(pid)
            ).

:- instance par_fold(fork_par_fold) where [
    pred(run_in_child/8) is run_in_child_process,
    pred(wait_for_child_exit/5) is wait_for_child_process_exit
].

:- pred foldl2_maybe_stop_at_error_parallel_processes(bool::in, int::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, bool::out, Info::in, io::di, io::uo) is det.

foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs, MakeTarget,
        Targets, Success, Info, !IO) :-
    PF0 = fork_par_fold(set.init),
    do_parallel_foldl2(KeepGoing, Jobs, MakeTarget, Info, Targets,
        Success, PF0, _PF, !IO).

:- pred run_in_child_process(
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    Info::in, T::in, bool::out, fork_par_fold::in, fork_par_fold::out,
    io::di, io::uo) is det.

run_in_child_process(P, Info, T, ChildStarted, PF0, PF, !IO) :-
    start_in_forked_process(
        (pred(Success::out, !.IO::di, !:IO::uo) is det :-
            P(T, Success, Info, _Info, !IO)
        ), MaybePid, !IO),
    (
        MaybePid = yes(Pid),
        ChildStarted = yes,
        PF0 = fork_par_fold(Set0),
        set.insert(Set0, Pid, Set),
        PF = fork_par_fold(Set)
    ;
        MaybePid = no,
        ChildStarted = no,
        PF = PF0
    ).

:- pred wait_for_child_process_exit(child_exit::out(child_succeeded_or_failed),
    fork_par_fold::in, fork_par_fold::out, io::di, io::uo) is det.

wait_for_child_process_exit(ChildExit, PF0, PF, !IO) :-
    wait_any(DeadPid, ChildStatus, !IO),
    fork_par_fold(Pids0) = PF0,
    ( set.remove(Pids0, DeadPid, Pids) ->
        ( ChildStatus = ok(exited(0)) ->
            ChildExit = child_succeeded
        ;
            ChildExit = child_failed
        ),
        PF = fork_par_fold(Pids)
    ;
        % Not a child of ours, maybe a grand child.  Ignore it.
        wait_for_child_process_exit(ChildExit, PF0, PF, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Parallel fold using threads
%

:- type thread_par_fold
    --->    thread_par_fold(
                tpf_channel     :: channel(child_exit),
                                % A channel to communicate between the children
                                % and the parent.

                tpf_maybe_excp  :: maybe(univ)
                                % Remember the first of any exceptions thrown
                                % by child threads.
            ).

:- instance par_fold(thread_par_fold) where [
    pred(run_in_child/8) is run_in_child_thread,
    pred(wait_for_child_exit/5) is wait_for_child_thread_exit
].

:- pred foldl2_maybe_stop_at_error_parallel_threads(bool::in, int::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, bool::out, Info::in, io::di, io::uo) is det.

foldl2_maybe_stop_at_error_parallel_threads(KeepGoing, Jobs, MakeTarget,
        Targets, Success, Info, !IO) :-
    channel.init(Channel, !IO),
    PF0 = thread_par_fold(Channel, no),
    do_parallel_foldl2(KeepGoing, Jobs, MakeTarget, Info, Targets, Success,
        PF0, PF, !IO),
    %
    % Rethrow the first of any exceptions which terminated a child thread.
    %
    MaybeExcp = PF ^ tpf_maybe_excp,
    (
        MaybeExcp = yes(Excp),
        rethrow(exception(Excp) : exception_result(unit))
    ;
        MaybeExcp = no
    ).

:- pred run_in_child_thread(
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    Info::in, T::in, bool::out, thread_par_fold::in, thread_par_fold::out,
    io::di, io::uo) is det.

run_in_child_thread(P, Info, T, ChildStarted, PF, PF, !IO) :-
    promise_equivalent_solutions [!:IO] (
        spawn((pred(!.IO::di, !:IO::uo) is cc_multi :-
            try_io((pred(Succ::out, !.IO::di, !:IO::uo) is det :-
                P(T, Succ, Info, _Info, !IO)
            ), Result, !IO),
            (
                Result = succeeded(yes),
                Exit = child_succeeded
            ;
                Result = succeeded(no),
                Exit = child_failed
            ;
                Result = exception(Excp),
                Exit = child_exception(Excp)
            ),
            channel.put(PF ^ tpf_channel, Exit, !IO)
        ), !IO)
    ),
    ChildStarted = yes.

:- pred wait_for_child_thread_exit(child_exit::out(child_succeeded_or_failed),
    thread_par_fold::in, thread_par_fold::out, io::di, io::uo) is det.

wait_for_child_thread_exit(ChildExit, !PF, !IO) :-
    channel.take(!.PF ^ tpf_channel, ChildExit0, !IO),
    (
        ( ChildExit0 = child_succeeded
        ; ChildExit0 = child_failed
        ),
        ChildExit = ChildExit0
    ;
        ChildExit0 = child_exception(Excp),
        ChildExit = child_failed,
        MaybeExcp0 = !.PF ^ tpf_maybe_excp,
        (
            MaybeExcp0 = no,
            !PF ^ tpf_maybe_excp := yes(Excp)
        ;
            MaybeExcp0 = yes(_)
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

build_with_module_options_and_output_redirect(ModuleName, ExtraOptions,
        Build, Succeeded, !Info, !IO) :-
    build_with_module_options(ModuleName, ExtraOptions,
        build_with_module_options_and_output_redirect_2(ModuleName, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_2(module_name::in,
    build2(list(string), io.output_stream)::in(build2), list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

build_with_module_options_and_output_redirect_2(ModuleName, Build, AllOptions,
        Succeeded, !Info, !IO) :-
    build_with_output_redirect(ModuleName,
        build_with_module_options_and_output_redirect_3(AllOptions, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_3(list(string)::in,
    build2(list(string), io.output_stream)::in(build2),
    io.output_stream::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_with_module_options_and_output_redirect_3(AllOptions, Build,
        ErrorStream, Succeeded, !Info, !IO) :-
    Build(AllOptions, ErrorStream, Succeeded, !Info, !IO).

build_with_output_redirect(ModuleName, Build, Succeeded, !Info, !IO) :-
    redirect_output(ModuleName, RedirectResult, !Info, !IO),
    (
        RedirectResult = no,
        Succeeded = no
    ;
        RedirectResult = yes(ErrorStream),
        Build(ErrorStream, Succeeded, !Info, !IO),
        unredirect_output(ModuleName, ErrorStream, !Info, !IO)
    ).

build_with_module_options(ModuleName, ExtraOptions, Build, Succeeded,
        !Info, !IO) :-
    build_with_module_options_args_invoked(yes, ModuleName,
        !.Info ^ options_variables, !.Info ^ option_args, ExtraOptions, Build,
        Succeeded, !.Info, MaybeInfo, !IO),
    (
        MaybeInfo = yes(!:Info)
    ;
        MaybeInfo = no
    ).

build_with_module_options_args(ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO) :-
    build_with_module_options_args_invoked(no, ModuleName, OptionVariables,
        OptionArgs, ExtraOptions, Build, Succeeded, !Info, !IO).

:- pred build_with_module_options_args_invoked(bool::in, module_name::in,
    options_variables::in, list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

build_with_module_options_args_invoked(InvokedByMmcMake, ModuleName,
        OptionVariables, OptionArgs, ExtraOptions, Build, Succeeded,
        Info0, MaybeInfo, !IO) :-
    lookup_mmc_module_options(OptionVariables, ModuleName, OptionsResult, !IO),
    (
        OptionsResult = no,
        MaybeInfo = no,
        Succeeded = no
    ;
        OptionsResult = yes(ModuleOptionArgs),
        globals.io_get_globals(Globals, !IO),

        % --invoked-by-mmc-make disables reading DEFAULT_MCFLAGS
        % from the environment (DEFAULT_MCFLAGS is included in
        % OptionArgs) and generation of `.d' files.
        % --use-subdirs is needed because the code to install
        % libraries uses `--use-grade-subdirs' and assumes the
        % interface files were built with `--use-subdirs'.
        (
            InvokedByMmcMake = yes,
            UseSubdirs = ["--use-subdirs"],
            InvokedByMake = ["--invoked-by-mmc-make"]
        ;
            InvokedByMmcMake = no,
            UseSubdirs = [],
            InvokedByMake = []
        ),

        AllOptionArgs = list.condense([InvokedByMake, ModuleOptionArgs,
            OptionArgs, ExtraOptions, UseSubdirs]),
        handle_options(AllOptionArgs, OptionsErrors, _, _, _, !IO),
        (
            OptionsErrors = [_ | _],
            Succeeded = no,
            MaybeInfo = no,
            usage_errors(OptionsErrors, !IO)
        ;
            OptionsErrors = [],
            Build(AllOptionArgs, Succeeded, Info0, Info, !IO),
            MaybeInfo = yes(Info),
            globals.io_set_globals(Globals, !IO)
        )
    ).

redirect_output(_ModuleName, MaybeErrorStream, !Info, !IO) :-
    % Write the output to a temporary file first, so it's easy to just print
    % the part of the error file that relates to the current command. It will
    % be appended to the error file later.

    io.make_temp(ErrorFileName, !IO),
    io.open_output(ErrorFileName, ErrorFileRes, !IO),
    (
        ErrorFileRes = ok(ErrorOutputStream),
        MaybeErrorStream = yes(ErrorOutputStream)
    ;
        ErrorFileRes = error(IOError),
        MaybeErrorStream = no,
        io.write_string("** Error opening `", !IO),
        io.write_string(ErrorFileName, !IO),
        io.write_string("' for output: ", !IO),
        io.error_message(IOError, Msg),
        io.write_string(Msg, !IO),
        io.nl(!IO)
    ).

unredirect_output(ModuleName, ErrorOutputStream, !Info, !IO) :-
    io.output_stream_name(ErrorOutputStream, TmpErrorFileName, !IO),
    io.close_output(ErrorOutputStream, !IO),

    io.open_input(TmpErrorFileName, TmpErrorInputRes, !IO),
    (
        TmpErrorInputRes = ok(TmpErrorInputStream),
        module_name_to_file_name(ModuleName, ".err", do_create_dirs,
            ErrorFileName, !IO),
        ( set.member(ModuleName, !.Info ^ error_file_modules) ->
            io.open_append(ErrorFileName, ErrorFileRes, !IO)
        ;
            io.open_output(ErrorFileName, ErrorFileRes, !IO)
        ),
        (
            ErrorFileRes = ok(ErrorFileOutputStream),
            globals.io_lookup_int_option(output_compile_error_lines,
                LinesToWrite, !IO),
            io.output_stream(CurrentOutputStream, !IO),
            io.input_stream_foldl2_io(TmpErrorInputStream,
                make_write_error_char(ErrorFileOutputStream,
                    CurrentOutputStream), LinesToWrite, TmpFileInputRes, !IO),
            (
                TmpFileInputRes = ok(_)
            ;
                TmpFileInputRes = error(_, TmpFileInputError),
                io.write_string("Error reading `", !IO),
                io.write_string(TmpErrorFileName, !IO),
                io.write_string("': ", !IO),
                io.write_string(io.error_message(TmpFileInputError), !IO),
                io.nl(!IO)
            ),

            io.close_output(ErrorFileOutputStream, !IO),

            !:Info = !.Info ^ error_file_modules :=
                set.insert(!.Info ^ error_file_modules, ModuleName)
        ;
            ErrorFileRes = error(Error),
            io.write_string("Error opening `", !IO),
            io.write_string(TmpErrorFileName, !IO),
            io.write_string("': ", !IO),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO)
        ),
        io.close_input(TmpErrorInputStream, !IO)
    ;
        TmpErrorInputRes = error(Error),
        io.write_string("Error opening `", !IO),
        io.write_string(TmpErrorFileName, !IO),
        io.write_string("': ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ),
    io.remove_file(TmpErrorFileName, _, !IO).

:- pred make_write_error_char(io.output_stream::in, io.output_stream::in,
    char::in, int::in, int::out, io::di, io::uo) is det.

make_write_error_char(FullOutputStream, PartialOutputStream, Char,
        !LinesRemaining, !IO) :-
    io.write_char(FullOutputStream, Char, !IO),
    ( !.LinesRemaining > 0 ->
        io.write_char(PartialOutputStream, Char, !IO),
        ( Char = '\n' ->
            !:LinesRemaining = !.LinesRemaining - 1
        ;
            true
        )
    ; !.LinesRemaining = 0 ->
        io.output_stream_name(FullOutputStream, FullOutputFileName, !IO),
        io.write_string(PartialOutputStream, "... error log truncated, see `",
            !IO),
        io.write_string(PartialOutputStream, FullOutputFileName, !IO),
        io.write_string(PartialOutputStream, "' for the complete log.\n", !IO),
        % Only write the above message once.
        !:LinesRemaining = -1
    ;
        true
    ).

%-----------------------------------------------------------------------------%

get_timestamp_file_timestamp(target_file(ModuleName, FileType),
        MaybeTimestamp, !Info, !IO) :-
    globals.io_get_globals(Globals, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        module_name_to_file_name(ModuleName, TimestampExt, do_not_create_dirs,
            FileName, !IO)
    ;
        module_target_to_file_name(ModuleName, FileType, do_not_create_dirs,
            FileName, !IO)
    ),

    % We should only ever look for timestamp files in the current directory.
    % Timestamp files are only used when processing a module, and only modules
    % in the current directory are processed.
    SearchDirs = [dir.this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

get_dependency_timestamp(DependencyFile, MaybeTimestamp, !Info, !IO) :-
    (
        DependencyFile = dep_file(FileName, MaybeOption),
        (
            MaybeOption = yes(Option),
            globals.io_lookup_accumulating_option(Option, SearchDirs, !IO)
        ;
            MaybeOption = no,
            SearchDirs = [dir.this_directory]
        ),
        get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO)
    ;
        DependencyFile = dep_target(Target),
        get_target_timestamp(do_search, Target, MaybeTimestamp0, !Info, !IO),
        (
            Target = target_file(_, module_target_c_header(header_mih)),
            MaybeTimestamp0 = ok(_)
        ->
            % Don't rebuild the `.o' file if an irrelevant part of a
            % `.mih' file has changed. If a relevant part of a `.mih'
            % file changed, the interface files of the imported module
            % must have changed in a way that would force the `.c' and
            % `.o' files of the current module to be rebuilt.
            MaybeTimestamp = ok(oldest_timestamp)
        ;
            MaybeTimestamp = MaybeTimestamp0
        )
    ).

get_target_timestamp(Search, TargetFile, MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(_ModuleName, FileType),
    get_file_name(Search, TargetFile, FileName, !Info, !IO),
    ( FileType = module_target_analysis_registry ->
        get_target_timestamp_analysis_registry(Search, TargetFile, FileName,
            MaybeTimestamp, !Info, !IO)
    ;
        get_target_timestamp_2(Search, TargetFile, FileName, MaybeTimestamp,
            !Info, !IO)
    ).

    % Special treatment for `.analysis' files.  If the corresponding
    % `.analysis_status' file says the `.analysis' file is invalid then we
    % treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(maybe_search::in,
    target_file::in, file_name::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry(Search, TargetFile, FileName,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, _FileType),
    ( MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) ->
        MaybeTimestamp = MaybeTimestamp0
    ;
        analysis.read_module_overall_status(mmc, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_2(Search, TargetFile, FileName,
                MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            !Info ^ file_timestamps ^ elem(FileName) := MaybeTimestamp
        )
    ).

:- pred get_target_timestamp_2(maybe_search::in, target_file::in,
    file_name::in, maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_2(Search, TargetFile, FileName, MaybeTimestamp,
        !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    (
        Search = do_search,
        get_search_directories(FileType, SearchDirs, !IO)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    (
        MaybeTimestamp0 = error(_),
        ( FileType = module_target_intermodule_interface
        ; FileType = module_target_analysis_registry
        )
    ->
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.

        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            Imports ^ module_dir \= dir.this_directory
        ->
            MaybeTimestamp = ok(oldest_timestamp),
            !:Info = !.Info ^ file_timestamps ^ elem(FileName)
                := MaybeTimestamp
        ;
            MaybeTimestamp = MaybeTimestamp0
        )
    ;
        MaybeTimestamp = MaybeTimestamp0
    ).

get_file_name(Search, TargetFile, FileName, !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    ( FileType = module_target_source ->
        % In some cases the module name won't match the file name
        % (module mdb.parse might be in parse.m or mdb.m), so we need to
        % look up the file name here.
        get_module_dependencies(ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            FileName = Imports ^ source_file_name
        ;
            MaybeImports = no,

            % Something has gone wrong generating the dependencies,
            % so just take a punt (which probably won't work).
            module_name_to_file_name(ModuleName, ".m", do_not_create_dirs,
                FileName, !IO)
        )
    ;
        globals.io_get_globals(Globals, !IO),
        MaybeExt = target_extension(Globals, FileType),
        (
            MaybeExt = yes(Ext),
            (
                Search = do_search,
                module_name_to_search_file_name_cache(ModuleName, Ext,
                    FileName, !Info, !IO)
            ;
                Search = do_not_search,
                % Not common enough to cache.
                module_name_to_file_name(ModuleName, Ext, do_not_create_dirs,
                    FileName, !IO)
            )
        ;
            MaybeExt = no,
            module_target_to_file_name_maybe_search(ModuleName, FileType,
                do_not_create_dirs, Search, FileName, !IO)
        )
    ).

:- pred module_name_to_search_file_name_cache(module_name::in,
    string::in, string::out, make_info::in, make_info::out, io::di, io::uo)
    is det.

module_name_to_search_file_name_cache(ModuleName, Ext, FileName, !Info, !IO) :-
    Key = ModuleName - Ext,
    ( map.search(!.Info ^ search_file_name_cache, Key, FileName0) ->
        FileName = FileName0
    ;
        module_name_to_search_file_name(ModuleName, Ext, FileName, !IO),
        !Info ^ search_file_name_cache ^ elem(Key) := FileName
    ).

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    ( MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) ->
        MaybeTimestamp = MaybeTimestamp0
    ;
        io.input_stream(OldInputStream, !IO),
        search_for_file(SearchDirs, FileName, SearchResult, !IO),
        (
            SearchResult = ok(_),
            io.input_stream_name(FullFileName, !IO),
            io.set_input_stream(OldInputStream, FileStream, !IO),
            io.close_input(FileStream, !IO),
            io.file_modification_time(FullFileName, TimeTResult, !IO),
            (
                TimeTResult = ok(TimeT),
                Timestamp = time_t_to_timestamp(TimeT),
                MaybeTimestamp = ok(Timestamp)
            ;
                TimeTResult = error(Error),
                MaybeTimestamp = error(io.error_message(Error))
            ),
            !:Info = !.Info ^ file_timestamps ^ elem(FileName)
                := MaybeTimestamp
        ;
            SearchResult = error(_),
            MaybeTimestamp = error("file `" ++ FileName ++ "' not found")
        )
    ).

:- pred get_search_directories(module_target_type::in, list(dir_name)::out,
    io::di, io::uo) is det.

get_search_directories(FileType, SearchDirs, !IO) :-
    MaybeOpt = search_for_file_type(FileType),
    (
        MaybeOpt = yes(SearchDirOpt),
        globals.io_lookup_accumulating_option(SearchDirOpt, SearchDirs0, !IO),
        % Make sure the current directory is searched
        % for C headers and libraries.
        SearchDirs =
            ( list.member(dir.this_directory, SearchDirs0) ->
                SearchDirs0
            ;
                [dir.this_directory | SearchDirs0]
            )
    ;
        MaybeOpt = no,
        SearchDirs = [dir.this_directory]
    ).

find_oldest_timestamp(error(_) @ MaybeTimestamp, _) = MaybeTimestamp.
find_oldest_timestamp(ok(_), error(_) @ MaybeTimestamp) = MaybeTimestamp.
find_oldest_timestamp(ok(Timestamp1), ok(Timestamp2)) = ok(Timestamp) :-
    ( compare((<), Timestamp1, Timestamp2) ->
        Timestamp = Timestamp1
    ;
        Timestamp = Timestamp2
    ).

%-----------------------------------------------------------------------------%

make_remove_target_file(VerboseOption, target_file(ModuleName, FileType),
        !Info, !IO) :-
    make_remove_target_file(VerboseOption, ModuleName, FileType, !Info, !IO).

make_remove_target_file(VerboseOption, ModuleName, FileType, !Info, !IO) :-
    globals.io_get_globals(Globals, !IO),
    module_target_to_file_name(ModuleName, FileType, do_not_create_dirs,
        FileName, !IO),
    make_remove_file(VerboseOption, FileName, !Info, !IO),
    ( TimestampExt = timestamp_extension(Globals, FileType) ->
        make_remove_file(VerboseOption, ModuleName, TimestampExt, !Info, !IO)
    ;
        true
    ).

make_remove_file(VerboseOption, ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(ModuleName, Ext, do_not_create_dirs, FileName,
        !IO),
    make_remove_file(VerboseOption, FileName, !Info, !IO).

make_remove_file(VerboseOption, FileName, !Info, !IO) :-
    verbose_msg(VerboseOption, report_remove_file(FileName), !IO),
    io.remove_file_recursively(FileName, _, !IO),
    !:Info = !.Info ^ file_timestamps :=
        map.delete(!.Info ^ file_timestamps, FileName).

:- pred report_remove_file(string::in, io::di, io::uo) is det.

report_remove_file(FileName, !IO) :-
    io.write_string("Removing ", !IO),
    io.write_string(FileName, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

make_target_file_list(ModuleNames, FileType) =
    list.map((func(ModuleName) = target_file(ModuleName, FileType)),
        ModuleNames).

make_dependency_list(ModuleNames, FileType) =
    list.map((func(Module) = dep_target(target_file(Module, FileType))),
        ModuleNames).

target_extension(_, module_target_source) = yes(".m").
target_extension(_, module_target_errors) = yes(".err").
target_extension(_, module_target_private_interface) = yes(".int0").
target_extension(_, module_target_long_interface) = yes(".int").
target_extension(_, module_target_short_interface) = yes(".int2").
target_extension(_, module_target_unqualified_short_interface) = yes(".int3").
target_extension(_, module_target_intermodule_interface) = yes(".opt").
target_extension(_, module_target_analysis_registry) = yes(".analysis").
target_extension(_, module_target_track_flags) = yes(".track_flags").
target_extension(_, module_target_c_header(header_mih)) = yes(".mih").
target_extension(_, module_target_c_header(header_mh)) = yes(".mh").
target_extension(_, module_target_c_code) = yes(".c").
target_extension(_, module_target_il_code) = yes(".il").

    % XXX ".exe" if the module contains main.
target_extension(_, module_target_il_asm) = yes(".dll").
target_extension(_, module_target_java_code) = yes(".java").
target_extension(_, module_target_java_class_code) = yes(".class").
target_extension(_, module_target_erlang_header) = yes(".hrl").
target_extension(_, module_target_erlang_code) = yes(".erl").
target_extension(_, module_target_erlang_beam_code) = yes(".beam").
target_extension(_, module_target_asm_code(non_pic)) = yes(".s").
target_extension(_, module_target_asm_code(link_with_pic)) = yes(".s").
target_extension(_, module_target_asm_code(pic)) = yes(".pic_s").
target_extension(Globals, module_target_object_code(PIC)) = yes(Ext) :-
    maybe_pic_object_file_extension(Globals, PIC, Ext).
target_extension(_, module_target_xml_doc) = yes(".xml").

    % These all need to be handled as special cases.
target_extension(_, module_target_foreign_object(_, _)) = no.
target_extension(_, module_target_foreign_il_asm(_)) = no.
target_extension(_, module_target_fact_table_object(_, _)) = no.

linked_target_file_name(ModuleName, executable, FileName, !IO) :-
    globals.io_lookup_string_option(executable_file_extension, Ext, !IO),
    module_name_to_file_name(ModuleName, Ext,
        do_not_create_dirs, FileName, !IO).
linked_target_file_name(ModuleName, static_library, FileName, !IO) :-
    globals.io_lookup_string_option(library_extension, Ext, !IO),
    module_name_to_lib_file_name("lib", ModuleName, Ext,
        do_not_create_dirs, FileName, !IO).
linked_target_file_name(ModuleName, shared_library, FileName, !IO) :-
    globals.io_lookup_string_option(shared_library_extension, Ext, !IO),
    module_name_to_lib_file_name("lib", ModuleName, Ext,
        do_not_create_dirs, FileName, !IO).
linked_target_file_name(ModuleName, java_archive, FileName, !IO) :-
    module_name_to_file_name(ModuleName, ".jar",
        do_not_create_dirs, FileName, !IO).
linked_target_file_name(ModuleName, erlang_archive, FileName, !IO) :-
    module_name_to_lib_file_name("lib", ModuleName, ".beams",
        do_not_create_dirs, FileName, !IO).

:- pred module_target_to_file_name(module_name::in, module_target_type::in,
    maybe_create_dirs::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name(ModuleName, TargetType, MkDir, FileName, !IO) :-
    module_target_to_file_name_maybe_search(ModuleName, TargetType, MkDir,
        do_not_search, FileName, !IO).

:- pred module_target_to_search_file_name(module_name::in,
    module_target_type::in, file_name::out, io::di, io::uo) is det.

module_target_to_search_file_name(ModuleName, TargetType, FileName, !IO) :-
    module_target_to_file_name_maybe_search(ModuleName, TargetType,
        do_not_create_dirs, do_search, FileName, !IO).

:- pred module_target_to_file_name_maybe_search(module_name::in,
    module_target_type::in, maybe_create_dirs::in, maybe_search::in,
    file_name::out, io::di, io::uo) is det.

module_target_to_file_name_maybe_search(ModuleName, TargetType, MkDir, Search,
        FileName, !IO) :-
    globals.io_get_globals(Globals, !IO),
    target_extension(Globals, TargetType) = MaybeExt,
    (
        MaybeExt = yes(Ext),
        (
            Search = do_search,
            module_name_to_search_file_name(ModuleName, Ext, FileName, !IO)
        ;
            Search = do_not_search,
            module_name_to_file_name(ModuleName, Ext, MkDir, FileName, !IO)
        )
    ;
        MaybeExt = no,
        (
            TargetType = module_target_foreign_object(PIC, Lang),
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name_maybe_search(ForeignModuleName,
                    module_target_object_code(PIC), MkDir, Search, FileName,
                    !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ;
            TargetType = module_target_foreign_il_asm(Lang),
            (
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            ->
                module_target_to_file_name_maybe_search(ForeignModuleName,
                    module_target_il_asm, MkDir, Search, FileName, !IO)
            ;
                unexpected(this_file, "module_target_to_file_name_2")
            )
        ;
            TargetType = module_target_fact_table_object(PIC, FactFile),
            maybe_pic_object_file_extension(PIC, Ext, !IO),
            fact_table_file_name(ModuleName, FactFile, Ext, MkDir, FileName,
                !IO)
        ;
            ( TargetType = module_target_analysis_registry
            ; TargetType = module_target_asm_code(_)
            ; TargetType = make.module_target_c_code
            ; TargetType = module_target_c_header(_)
            ; TargetType = module_target_erlang_beam_code
            ; TargetType = module_target_erlang_code
            ; TargetType = module_target_erlang_header
            ; TargetType = module_target_errors
            ; TargetType = make.module_target_il_asm
            ; TargetType = module_target_il_code
            ; TargetType = module_target_intermodule_interface
            ; TargetType = module_target_java_code
            ; TargetType = module_target_java_class_code
            ; TargetType = module_target_long_interface
            ; TargetType = module_target_object_code(_)
            ; TargetType = module_target_private_interface
            ; TargetType = module_target_short_interface
            ; TargetType = module_target_source
            ; TargetType = module_target_unqualified_short_interface
            ; TargetType = module_target_xml_doc
            ; TargetType = module_target_track_flags
            ),
            unexpected(this_file, "module_target_to_file_name_2")
        )
    ).

    % Note that we need a timestamp file for `.err' files because
    % errors are written to the `.err' file even when writing interfaces.
    % The timestamp is only updated when compiling to target code.
    %
    % We need a timestamp file for `.analysis' files because they
    % can be modified in the process of analysing _another_ module.
    % The timestamp is only updated after actually analysing the module that
    % the `.analysis' file corresponds to.
    %
    % Header files share a timestamp file with their corresponding target code
    % files.
    %
timestamp_extension(_, module_target_errors) = ".err_date".
timestamp_extension(_, module_target_private_interface) = ".date0".
timestamp_extension(_, module_target_long_interface) = ".date".
timestamp_extension(_, module_target_short_interface) = ".date".
timestamp_extension(_, module_target_unqualified_short_interface) = ".date3".
timestamp_extension(_, module_target_intermodule_interface) = ".optdate".
timestamp_extension(_, module_target_analysis_registry) = ".analysis_date".
timestamp_extension(_, module_target_c_code) = ".c_date".
timestamp_extension(Globals, module_target_c_header(_)) = Ext :-
    globals.get_target(Globals, Target),
    (
        Target = target_asm,
        ModuleTargetType = module_target_asm_code(non_pic)
    ;
        % XXX Some of these alternatives don't look right.
        ( Target = target_c
        ; Target = target_x86_64
        ; Target = target_il
        ; Target = target_java
        ; Target = target_erlang
        ),
        ModuleTargetType = module_target_c_code
    ),
    Ext = timestamp_extension(Globals, ModuleTargetType).
timestamp_extension(_, module_target_il_code) = ".il_date".
timestamp_extension(_, module_target_java_code) = ".java_date".
timestamp_extension(_, module_target_erlang_code) = ".erl_date".
timestamp_extension(Globals, module_target_erlang_header) =
    timestamp_extension(Globals, module_target_erlang_code).
timestamp_extension(_, module_target_asm_code(non_pic)) = ".s_date".
timestamp_extension(_, module_target_asm_code(pic)) = ".pic_s_date".

:- func search_for_file_type(module_target_type) = maybe(option).

search_for_file_type(module_target_source) = no.
search_for_file_type(module_target_errors) = no.
    % XXX only for inter-module optimization.
search_for_file_type(module_target_private_interface) =
        yes(search_directories).
search_for_file_type(module_target_long_interface) = yes(search_directories).
search_for_file_type(module_target_short_interface) = yes(search_directories).
search_for_file_type(module_target_unqualified_short_interface) =
        yes(search_directories).
search_for_file_type(module_target_intermodule_interface) =
        yes(intermod_directories).
search_for_file_type(module_target_analysis_registry) =
        yes(intermod_directories).
search_for_file_type(module_target_track_flags) = no.
search_for_file_type(module_target_c_header(_)) = yes(c_include_directory).
search_for_file_type(module_target_c_code) = no.
search_for_file_type(module_target_il_code) = no.
search_for_file_type(module_target_il_asm) = no.
search_for_file_type(module_target_java_code) = no.
search_for_file_type(module_target_java_class_code) = no.
search_for_file_type(module_target_erlang_header) =
        yes(erlang_include_directory).
search_for_file_type(module_target_erlang_code) = no.
search_for_file_type(module_target_erlang_beam_code) = no.
search_for_file_type(module_target_asm_code(_)) = no.
search_for_file_type(module_target_object_code(_)) = no.
search_for_file_type(module_target_foreign_object(_, _)) = no.
search_for_file_type(module_target_foreign_il_asm(_)) = no.
search_for_file_type(module_target_fact_table_object(_, _)) = no.
search_for_file_type(module_target_xml_doc) = no.

target_is_grade_or_arch_dependent(Target) :-
    is_target_grade_or_arch_dependent(Target) = yes.

:- func is_target_grade_or_arch_dependent(module_target_type) = bool.

is_target_grade_or_arch_dependent(Target) = IsDependent :-
    (
        ( Target = module_target_source
        ; Target = module_target_errors
        ; Target = module_target_private_interface
        ; Target = module_target_long_interface
        ; Target = module_target_short_interface
        ; Target = module_target_unqualified_short_interface
        ; Target = module_target_c_header(header_mh)
        ; Target = module_target_xml_doc
        ),
        IsDependent = no
    ;
        ( Target = module_target_intermodule_interface
        ; Target = module_target_analysis_registry
        ; Target = module_target_track_flags
        ; Target = module_target_c_header(header_mih)
        ; Target = module_target_c_code
        ; Target = module_target_il_code
        ; Target = module_target_il_asm
        ; Target = module_target_java_code
        ; Target = module_target_java_class_code
        ; Target = module_target_erlang_code
        ; Target = module_target_erlang_beam_code
        ; Target = module_target_erlang_header
        ; Target = module_target_asm_code(_)
        ; Target = module_target_object_code(_)
        ; Target = module_target_foreign_object(_, _)
        ; Target = module_target_foreign_il_asm(_)
        ; Target = module_target_fact_table_object(_, _)
        ),
        IsDependent = yes
    ).

%-----------------------------------------------------------------------------%

debug_msg(P, !IO) :-
    verbose_msg(debug_make, P, !IO).

verbose_msg(P, !IO) :-
    verbose_msg(verbose_make, P, !IO).

verbose_msg(Option, P, !IO) :-
    globals.io_lookup_bool_option(Option, OptionValue, !IO),
    (
        OptionValue = yes,
        P(!IO),
        io.flush_output(!IO)
    ;
        OptionValue = no
    ).

debug_file_msg(TargetFile, Msg, !IO) :-
    debug_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            make_write_target_file(TargetFile, !IO),
            io.write_string(": ", !IO),
            io.write_string(Msg, !IO),
            io.nl(!IO)
        ), !IO).

make_write_dependency_file(dep_target(TargetFile), !IO) :-
    make_write_target_file(TargetFile, !IO).
make_write_dependency_file(dep_file(FileName, _), !IO) :-
    io.write_string(FileName, !IO).

make_write_dependency_file_list([], !IO).
make_write_dependency_file_list([DepFile | DepFiles], !IO) :-
    io.write_string("\t", !IO),
    make_write_dependency_file(DepFile, !IO),
    io.nl(!IO),
    make_write_dependency_file_list(DepFiles, !IO).

make_write_target_file(TargetFile, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    module_target_to_file_name(ModuleName, FileType, do_not_create_dirs,
        FileName, !IO),
    io.write_string(FileName, !IO).

maybe_make_linked_target_message(TargetFile, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Making ", !IO),
            io.write_string(TargetFile, !IO),
            io.nl(!IO)
        ), !IO).

maybe_make_target_message(TargetFile, !IO) :-
    io.output_stream(OutputStream, !IO),
    maybe_make_target_message_to_stream(OutputStream, TargetFile, !IO).

maybe_make_target_message_to_stream(OutputStream, TargetFile, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.set_output_stream(OutputStream, OldOutputStream, !IO),
            io.write_string("Making ", !IO),
            make_write_target_file(TargetFile, !IO),
            io.nl(!IO),
            io.set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

maybe_reanalyse_modules_message(!IO) :-
    io.output_stream(OutputStream, !IO),
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.set_output_stream(OutputStream, OldOutputStream, !IO),
            io.write_string("Reanalysing invalid/suboptimal modules\n", !IO),
            io.set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

target_file_error(TargetFile, !IO) :-
    io.write_string("** Error making `", !IO),
    make_write_target_file(TargetFile, !IO),
    io.write_string("'.\n", !IO).

file_error(TargetFile, !IO) :-
    io.write_string("** Error making `", !IO),
    io.write_string(TargetFile, !IO),
    io.write_string("'.\n", !IO).

maybe_warn_up_to_date_target(Target, !Info, !IO) :-
    globals.io_lookup_bool_option(warn_up_to_date, Warn, !IO),
    (
        Warn = yes,
        ( set.member(Target, !.Info ^ command_line_targets) ->
            io.write_string("** Nothing to be done for `", !IO),
            make_write_module_or_linked_target(Target, !IO),
            io.write_string("'.\n", !IO)
        ;
            true
        )
    ;
        Warn = no
    ),
    !:Info = !.Info ^ command_line_targets :=
        set.delete(!.Info ^ command_line_targets, Target).

maybe_symlink_or_copy_linked_target_message(Target, !IO) :-
    verbose_msg(
        (pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Made symlink/copy of ", !IO),
            make_write_module_or_linked_target(Target, !IO),
            io.write_string("\n", !IO)
        ), !IO).

:- pred make_write_module_or_linked_target(pair(module_name, target_type)::in,
    io::di, io::uo) is det.

make_write_module_or_linked_target(ModuleName - FileType, !IO) :-
    (
        FileType = module_target(ModuleTargetType),
        TargetFile = target_file(ModuleName, ModuleTargetType),
        make_write_target_file(TargetFile, !IO)
    ;
        FileType = linked_target(LinkedTargetType),
        linked_target_file_name(ModuleName, LinkedTargetType, FileName, !IO),
        io.write_string(FileName, !IO)
    ;
        FileType = misc_target(_),
        unexpected(this_file,
            "maybe_warn_up_to_date_target: misc_target")
    ).

%-----------------------------------------------------------------------------%
%
% Timing
%

:- pragma foreign_proc("C",
    get_real_milliseconds(Time::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = MR_get_real_milliseconds();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Hash functions
%

module_name_hash(SymName, Hash) :-
    (
        SymName = unqualified(String),
        Hash = string.hash(String)
    ;
        SymName = qualified(_Qual, String),
        % Hashing the the module qualifier seems to be not worthwhile.
        Hash = string.hash(String)
    ).

dependency_file_hash(DepFile, Hash) :-
    (
        DepFile = dep_target(TargetFile),
        Hash = target_file_hash(TargetFile)
    ;
        DepFile = dep_file(FileName, _MaybeOption),
        Hash = string.hash(FileName)
    ).

:- func target_file_hash(target_file) = int.

target_file_hash(TargetFile) = Hash :-
    TargetFile = target_file(ModuleName, Type),
    module_name_hash(ModuleName, Hash0),
    Hash1 = module_target_type_to_nonce(Type),
    Hash = mix(Hash0, Hash1).

:- func module_target_type_to_nonce(module_target_type) = int.

module_target_type_to_nonce(Type) = X :-
    (
        Type = module_target_source,
        X = 1
    ;
        Type = module_target_errors,
        X = 2
    ;
        Type = module_target_private_interface,
        X = 3
    ;
        Type = module_target_long_interface,
        X = 4
    ;
        Type = module_target_short_interface,
        X = 5
    ;
        Type = module_target_unqualified_short_interface,
        X = 6
    ;
        Type = module_target_intermodule_interface,
        X = 7
    ;
        Type = module_target_analysis_registry,
        X = 8
    ;
        Type = module_target_c_header(header_mh),
        X = 9
    ;
        Type = module_target_c_header(header_mih),
        X = 10
    ;
        Type = module_target_c_code,
        X = 11
    ;
        Type = module_target_il_code,
        X = 12
    ;
        Type = module_target_il_asm,
        X = 13
    ;
        Type = module_target_java_code,
        X = 14
    ;
        Type = module_target_erlang_header,
        X = 15
    ;
        Type = module_target_erlang_code,
        X = 16
    ;
        Type = module_target_erlang_beam_code,
        X = 17
    ;
        Type = module_target_asm_code(_PIC),
        X = 18
    ;
        Type = module_target_object_code(PIC),
        X = 19 `mix` pic_to_nonce(PIC)
    ;
        Type = module_target_foreign_il_asm(_ForeignLang),
        X = 20
    ;
        Type = module_target_foreign_object(_PIC, _ForeignLang),
        X = 21
    ;
        Type = module_target_fact_table_object(_PIC, _FileName),
        X = 22
    ;
        Type = module_target_xml_doc,
        X = 23
    ;
        Type = module_target_track_flags,
        X = 24
    ;
        Type = module_target_java_class_code,
        X = 25
    ).

:- func pic_to_nonce(pic) = int.

pic_to_nonce(pic) = 1.
pic_to_nonce(link_with_pic) = 2.
pic_to_nonce(non_pic) = 3.

:- func mix(int, int) = int.

mix(H0, X) = H :-
    H1 = H0 `xor` (H0 `unchecked_left_shift` 5),
    H = H1 `xor` X.

:- func concoct_second_hash(int) = int.

concoct_second_hash(H) = mix(H, 0xfe3dbe7f).    % whatever

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "make.util.m".

%-----------------------------------------------------------------------------%
:- end_module make.util.
%-----------------------------------------------------------------------------%
