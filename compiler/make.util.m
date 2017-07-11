%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.util.m.
% Authors: stayl, wangp.
%
% Assorted predicates used to implement `mmc --make'.
%
%-----------------------------------------------------------------------------%

:- module make.util.
:- interface.

:- import_module libs.globals.

%-----------------------------------------------------------------------------%
%
% Versions of foldl which stop if the supplied predicate returns `no'
% for any element of the list.
%

    % foldl2_pred_with_status(Globals, T, Succeeded, !Info).
    %
:- type foldl2_pred_with_status(T, Info, IO) ==
    pred(globals, T, bool, Info, Info, IO, IO).
:- inst foldl2_pred_with_status == (pred(in, in, out, in, out, di, uo) is det).

    % foldl2_maybe_stop_at_error(KeepGoing, P, Globals, List, Succeeded,
    %   !Info, !IO).
    %
:- pred foldl2_maybe_stop_at_error(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, bool::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

    % foldl3_pred_with_status(Globals, T, Succeeded, !Acc, !Info).
    %
:- type foldl3_pred_with_status(T, Acc, Info, IO) ==
    pred(globals, T, bool, Acc, Acc, Info, Info, IO, IO).
:- inst foldl3_pred_with_status ==
    (pred(in, in, out, in, out, in, out, di, uo) is det).

    % foldl3_maybe_stop_at_error(KeepGoing, P, Globals, List, Succeeded,
    %   !Acc, !Info).
    %
:- pred foldl3_maybe_stop_at_error(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    globals::in, list(T)::in, bool::out, Acc::in, Acc::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

%-----------------------------------------------------------------------------%

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
:- pred foldl2_maybe_stop_at_error_maybe_parallel(bool::in,
    foldl2_pred_with_status(T, make_info, io)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type build(T, Info1, Info2) == pred(globals, T, bool, Info1, Info2, io, io).
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
    list(string)::in, build(list(string))::in(build), bool::out,
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
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

    % Perform the given closure with an output stream created to append to
    % the error file for the given module.
    %
:- pred build_with_output_redirect(globals::in, module_name::in,
    build(io.output_stream)::in(build), bool::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Produce an output stream which writes to the error file
    % for the given module.
    %
:- pred redirect_output(module_name::in, maybe(io.output_stream)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Close the module error output stream.
    %
:- pred unredirect_output(globals::in, module_name::in, io.output_stream::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type build2(T, U) ==
    pred(globals, T, U, bool, make_info, make_info, io, io).
:- inst build2 == (pred(in, in, in, out, in, out, di, uo) is det).

:- pred build_with_module_options_and_output_redirect(globals::in,
    module_name::in, list(string)::in,
    build2(list(string), io.output_stream)::in(build2),
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timestamp handling.
%

    % Find the timestamp updated when a target is produced.
    %
:- pred get_timestamp_file_timestamp(globals::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % Find the timestamp for the given dependency file.
    %
:- pred get_dependency_timestamp(globals::in, dependency_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_target_timestamp(Globals, Search, TargetFile, Timestamp)
    %
    % Find the timestamp for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_target_timestamp(globals::in, maybe_search::in, target_file::in,
    maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % get_file_name(Globals, Search, TargetFile, FileName, !IO):
    %
    % Compute a file name for the given target file.
    % `Search' should be `do_search' if the file could be part of an
    % installed library.
    %
:- pred get_file_name(globals::in, maybe_search::in, target_file::in,
    file_name::out, make_info::in, make_info::out, io::di, io::uo) is det.

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
% Remove file a file, deleting the cached timestamp.
% The removal is reported to the user if the given boolean option is set.
% In general the option given should be `--very-verbose' when making a
% `.clean' or `.realclean target', and `--verbose-make' when cleaning
% after an interrupted build.
%

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file(globals::in, option::in, target_file::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Remove the target file and the corresponding timestamp file.
    %
:- pred make_remove_target_file_by_name(globals::in, option::in,
    module_name::in, module_target_type::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

    % make_remove_module_file(Globals, VerboseOption, ModuleName, Extension,
    %   !Info, !IO).
    %
:- pred make_remove_module_file(globals::in, option::in, module_name::in,
    string::in, make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_remove_file(globals::in, option::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- func make_target_file_list(list(module_name), module_target_type) =
    list(target_file).

:- func make_dependency_list(list(module_name), module_target_type)
    = list(dependency_file).

:- func target_extension(globals, module_target_type) = maybe(string).
:- mode target_extension(in, in) = out is det.
:- mode target_extension(in, out) = in(bound(yes(ground))) is nondet.

:- pred target_extension_synonym(string::in, module_target_type::out)
    is semidet.

:- pred linked_target_file_name(globals::in, module_name::in,
    linked_target_type::in, file_name::out, io::di, io::uo) is det.

    % Find the extension for the timestamp file for the given target type,
    % if one exists.
    %
:- pred timestamp_extension(module_target_type::in, string::out) is semidet.

:- pred target_is_grade_or_arch_dependent(module_target_type::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Debugging, verbose messages, and error messages.
%

    % A lock to prevent interleaved output to standard output from parallel
    % processes.
    %
:- type stdout_lock.

    % Apply the given predicate if `--debug-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred debug_make_msg(globals::in, pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Apply the given predicate if `--verbose-make' is set.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_make_msg(globals::in, pred(io, io)::(pred(di, uo) is det),
    io::di, io::uo) is det.

    % Apply the given predicate if the given boolean option is set to `yes'.
    % XXX Do we need this, now that we have trace goals?
    %
:- pred verbose_make_msg_option(globals::in, option::in,
    pred(io, io)::(pred(di, uo) is det), io::di, io::uo) is det.

    % Write a debugging message relating to a given target file.
    %
:- pred debug_file_msg(globals::in, target_file::in, string::in,
    io::di, io::uo) is det.

:- pred make_write_dependency_file(globals::in, dependency_file::in,
    io::di, io::uo) is det.

:- pred make_write_dependency_file_list(globals::in, list(dependency_file)::in,
    io::di, io::uo) is det.

:- pred make_write_target_file(globals::in, target_file::in,
    io::di, io::uo) is det.

:- pred make_write_target_file_wrapped(globals::in, string::in,
    target_file::in, string::in, io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_linked_target_message(globals::in, file_name::in,
    io::di, io::uo) is det.

    % Write a message "Making <filename>" if `--verbose-make' is set.
    %
:- pred maybe_make_target_message(globals::in, target_file::in,
    io::di, io::uo) is det.

:- pred maybe_make_target_message_to_stream(globals::in, io.output_stream::in,
    target_file::in, io::di, io::uo) is det.

    % Write a message "Reanalysing invalid/suboptimal modules" if
    % `--verbose-make' is set.
    %
:- pred maybe_reanalyse_modules_message(globals::in, io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred target_file_error(make_info::in, globals::in, target_file::in,
    io::di, io::uo) is det.

    % Write a message "** Error making <filename>".
    %
:- pred file_error(make_info::in, file_name::in, io::di, io::uo) is det.

    % If the given target was specified on the command line, warn that it
    % was already up to date.
    %
:- pred maybe_warn_up_to_date_target(globals::in,
    pair(module_name, target_type)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

    % Write a message "Made symlink/copy of <filename>"
    % if `--verbose-make' is set.
    %
:- pred maybe_symlink_or_copy_linked_target_message(globals::in,
    pair(module_name, target_type)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Timing.
%

:- pred get_real_milliseconds(int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Hash functions.
%

:- pred module_name_hash(module_name::in, int::out) is det.

:- pred dependency_file_hash(dependency_file::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module libs.handle_options.
:- import_module libs.process_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.
:- import_module transform_hlds.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module char.
:- import_module dir.
:- import_module getopt_io.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Globals, Targets, Success,
        !Info, !IO) :-
    foldl2_maybe_stop_at_error_2(KeepGoing, MakeTarget, Globals, Targets, yes,
        Success, !Info, !IO).

:- pred foldl2_maybe_stop_at_error_2(bool::in,
    foldl2_pred_with_status(T, Info, IO)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, bool::in, bool::out, Info::in, Info::out,
    IO::di, IO::uo) is det.

foldl2_maybe_stop_at_error_2(_KeepGoing, _P, _Globals, [], !Success,
        !Info, !IO).
foldl2_maybe_stop_at_error_2(KeepGoing, P, Globals, [T | Ts], !Success,
        !Info, !IO) :-
    P(Globals, T, NewSuccess, !Info, !IO),
    ( if
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    then
        !:Success = !.Success `and` NewSuccess,
        foldl2_maybe_stop_at_error_2(KeepGoing, P, Globals, Ts, !Success,
            !Info, !IO)
    else
        !:Success = no
    ).

foldl3_maybe_stop_at_error(KeepGoing, P, Globals, Ts, Success,
        !Acc, !Info, !IO) :-
    foldl3_maybe_stop_at_error_2(KeepGoing, P, Globals, Ts, yes, Success,
        !Acc, !Info, !IO).

:- pred foldl3_maybe_stop_at_error_2(bool::in,
    foldl3_pred_with_status(T, Acc, Info, IO)::in(foldl3_pred_with_status),
    globals::in, list(T)::in, bool::in, bool::out, Acc::in, Acc::out,
    Info::in, Info::out, IO::di, IO::uo) is det.

foldl3_maybe_stop_at_error_2(_KeepGoing, _P, _Globals, [],
        !Success, !Acc, !Info, !IO).
foldl3_maybe_stop_at_error_2(KeepGoing, P, Globals, [T | Ts],
        !Success, !Acc, !Info, !IO) :-
    P(Globals, T, NewSuccess, !Acc, !Info, !IO),
    ( if
        ( NewSuccess = yes
        ; KeepGoing = yes
        )
    then
        !:Success = !.Success `and` NewSuccess,
        foldl3_maybe_stop_at_error_2(KeepGoing, P, Globals, Ts,
            !Success, !Acc, !Info, !IO)
    else
        !:Success = no
    ).

%-----------------------------------------------------------------------------%
%
% Parallel (concurrent) fold.
%

foldl2_maybe_stop_at_error_maybe_parallel(KeepGoing, MakeTarget, Globals,
        Targets, Success, !Info, !IO) :-
    globals.lookup_int_option(Globals, jobs, Jobs),
    ( if
        Jobs > 1,
        process_util.can_fork,
        have_job_ctl_ipc
    then
        % First pass.
        foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs,
            MakeTarget, Globals, Targets, Success0, !Info, !IO),
        % Second pass (sequential).
        (
            Success0 = yes,
            % Disable the `--rebuild' option during the sequential pass
            % otherwise all the targets will be built a second time.
            globals.set_option(rebuild, bool(no), Globals, NoRebuildGlobals),
            foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, NoRebuildGlobals,
                Targets, Success, !Info, !IO)
        ;
            Success0 = no,
            Success = no
        )
    else
        foldl2_maybe_stop_at_error(KeepGoing, MakeTarget, Globals,
            Targets, Success, !Info, !IO)
    ).

:- pred foldl2_maybe_stop_at_error_parallel_processes(bool::in, int::in,
    foldl2_pred_with_status(T, make_info, io)::in(foldl2_pred_with_status),
    globals::in, list(T)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

foldl2_maybe_stop_at_error_parallel_processes(KeepGoing, Jobs, MakeTarget,
        Globals, Targets, Success, !Info, !IO) :-
    TotalTasks = list.length(Targets),
    create_job_ctl(TotalTasks, MaybeJobCtl, !IO),
    (
        MaybeJobCtl = yes(JobCtl),
        !Info ^ maybe_stdout_lock := yes(JobCtl),
        list.foldl2(
            start_worker_process(Globals, KeepGoing, MakeTarget, Targets,
                JobCtl, !.Info),
            2 .. Jobs, [], Pids, !IO),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        build_with_check_for_interrupt(VeryVerbose,
            worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl, yes),
            worker_loop_signal_cleanup(JobCtl, Pids), Success0, !Info, !IO),
        list.foldl2(reap_worker_process, Pids, Success0, Success, !IO),
        !Info ^ maybe_stdout_lock := no,
        destroy_job_ctl(JobCtl, !IO)
    ;
        MaybeJobCtl = no,
        Success = no
    ).

:- pred start_worker_process(globals::in, bool::in,
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

:- pred child_worker(globals::in, bool::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, job_ctl::in, Info::in, bool::out, io::di, io::uo) is det.

child_worker(Globals, KeepGoing, MakeTarget, Targets, JobCtl, Info0, Success,
        !IO) :-
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    build_with_check_for_interrupt(VeryVerbose,
        worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl, yes),
        worker_loop_signal_cleanup(JobCtl, []), Success, Info0, _Info, !IO).

:- pred worker_loop(globals::in, bool::in,
    foldl2_pred_with_status(T, Info, io)::in(foldl2_pred_with_status),
    list(T)::in, job_ctl::in, bool::in, bool::out, Info::in, Info::out,
    io::di, io::uo) is det.

worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl, !Success,
        !Info, !IO) :-
    accept_task(JobCtl, TaskNumber, !IO),
    ( if TaskNumber >= 0 then
        Target = list.det_index0(Targets, TaskNumber),
        MakeTarget(Globals, Target, TargetSuccess, !Info, !IO),
        (
            TargetSuccess = yes,
            mark_task_done(JobCtl, TaskNumber, !IO)
        ;
            TargetSuccess = no,
            mark_task_error(JobCtl, TaskNumber, KeepGoing, !IO),
            !:Success = no
        ),
        worker_loop(Globals, KeepGoing, MakeTarget, Targets, JobCtl, !Success,
            !Info, !IO)
    else
        % No more tasks.
        true
    ).

:- pred worker_loop_signal_cleanup(job_ctl::in, list(pid)::in,
    Info::in, Info::out, io::di, io::uo) is det.

worker_loop_signal_cleanup(JobCtl, Pids, !Info, !IO) :-
    mark_abort(JobCtl, !IO),
    list.foldl(send_signal(sigint), Pids, !IO).

:- pred reap_worker_process(pid::in, bool::in, bool::out,
    io::di, io::uo) is det.

reap_worker_process(Pid, !Success, !IO) :-
    wait_pid(Pid, Status, !IO),
    ( if
        !.Success = yes,
        Status = ok(exited(0))
    then
        true
    else
        !:Success = no
    ).

%-----------------------------------------------------------------------------%
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

  /* Just in case. */
  #if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
    #define MAP_ANONYMOUS MAP_ANON
  #endif
#endif

#ifdef MAP_ANONYMOUS
  /*
  ** Darwin 5.x and FreeBSD do not implement process-shared POSIX mutexes.
  ** Use System V semaphores instead. As System V semaphores seem to be more
  ** widely supported we may consider using them exclusively or in preference
  ** to POSIX mutexes in the future.
  */
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
    TASK_NEW,       /* task not yet attempted */
    TASK_ACCEPTED,  /* someone is working on this task */
    TASK_DONE,      /* task successfully completed */
    TASK_ERROR      /* error occurred when working on the task */
};

/* This structure is placed in shared memory. */
struct MC_JobCtl {
    /* Static data. */
    MR_Integer      jc_total_tasks;

    /* Dynamic data.  The mutex protects the rest. */
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

#endif /* MC_HAVE_JOBCTL_IPC */
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

    /* Create the shared memory segment. */
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

#endif /* MC_HAVE_JOBCTL_IPC */
").

:- type job_ctl.
:- pragma foreign_type("C", job_ctl, "MC_JobCtl *").
:- pragma foreign_type("C#", job_ctl, "object"). % stub
:- pragma foreign_type("Java", job_ctl, "java.lang.Object"). % stub
:- pragma foreign_type("Erlang", job_ctl, ""). % stub

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

%-----------------------------------------------------------------------------%
%
% Prevent interleaved error output
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

:- pred with_locked_stdout(make_info::in,
    pred(io, io)::in(pred(di, uo) is det), io::di, io::uo) is det.

with_locked_stdout(Info, Pred, !IO) :-
    MaybeLock = Info ^ maybe_stdout_lock,
    (
        MaybeLock = yes(Lock),
        lock_stdout(Lock, !IO),
        Pred(!IO),
        unlock_stdout(Lock, !IO)
    ;
        MaybeLock = no,
        Pred(!IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

build_with_module_options_and_output_redirect(Globals, ModuleName,
        ExtraOptions, Build, Succeeded, !Info, !IO) :-
    build_with_module_options(Globals, ModuleName, ExtraOptions,
        build_with_module_options_and_output_redirect_2(ModuleName, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_2(module_name::in,
    build2(list(string), io.output_stream)::in(build2), globals::in,
    list(string)::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_with_module_options_and_output_redirect_2(ModuleName, Build, Globals,
        AllOptions, Succeeded, !Info, !IO) :-
    build_with_output_redirect(Globals, ModuleName,
        build_with_module_options_and_output_redirect_3(AllOptions, Build),
        Succeeded, !Info, !IO).

:- pred build_with_module_options_and_output_redirect_3(list(string)::in,
    build2(list(string), io.output_stream)::in(build2), globals::in,
    io.output_stream::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

build_with_module_options_and_output_redirect_3(AllOptions, Build, Globals,
        ErrorStream, Succeeded, !Info, !IO) :-
    Build(Globals, AllOptions, ErrorStream, Succeeded, !Info, !IO).

build_with_output_redirect(Globals, ModuleName, Build, Succeeded, !Info,
        !IO) :-
    redirect_output(ModuleName, RedirectResult, !Info, !IO),
    (
        RedirectResult = no,
        Succeeded = no
    ;
        RedirectResult = yes(ErrorStream),
        Build(Globals, ErrorStream, Succeeded, !Info, !IO),
        unredirect_output(Globals, ModuleName, ErrorStream, !Info, !IO)
    ).

build_with_module_options(Globals, ModuleName, ExtraOptions, Build, Succeeded,
        !Info, !IO) :-
    build_with_module_options_args_invoked(Globals, yes, ModuleName,
        !.Info ^ detected_grade_flags, !.Info ^ options_variables,
        !.Info ^ option_args, ExtraOptions, Build, Succeeded,
        !.Info, MaybeInfo, !IO),
    (
        MaybeInfo = yes(!:Info)
    ;
        MaybeInfo = no
    ).

build_with_module_options_args(Globals, ModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions,
        Build, Succeeded, !Info, !IO) :-
    build_with_module_options_args_invoked(Globals, no, ModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions,
        Build, Succeeded, !Info, !IO).

:- pred build_with_module_options_args_invoked(globals::in, bool::in,
    module_name::in, list(string)::in, options_variables::in,
    list(string)::in, list(string)::in,
    build(list(string), Info1, Info2)::in(build),
    bool::out, Info1::in, maybe(Info2)::out, io::di, io::uo) is det.

build_with_module_options_args_invoked(Globals, InvokedByMmcMake, ModuleName,
        DetectedGradeFlags, OptionVariables, OptionArgs, ExtraOptions, Build,
        Succeeded, Info0, MaybeInfo, !IO) :-
    lookup_mmc_module_options(Globals, OptionVariables, ModuleName,
        OptionsResult, !IO),
    (
        OptionsResult = no,
        MaybeInfo = no,
        Succeeded = no
    ;
        OptionsResult = yes(ModuleOptionArgs),

        % --invoked-by-mmc-make disables reading DEFAULT_MCFLAGS from the
        % environment (DEFAULT_MCFLAGS is included in OptionArgs) and
        % generation of `.d' files. --use-subdirs is needed because
        % the code to install libraries uses `--use-grade-subdirs' and
        % assumes the interface files were built with `--use-subdirs'.
        (
            InvokedByMmcMake = yes,
            UseSubdirs = ["--use-subdirs"],
            InvokedByMake = ["--invoked-by-mmc-make"]
        ;
            InvokedByMmcMake = no,
            UseSubdirs = [],
            InvokedByMake = []
        ),

        AllOptionArgs = InvokedByMake ++ DetectedGradeFlags ++
            ModuleOptionArgs ++ OptionArgs ++ ExtraOptions ++ UseSubdirs,
        handle_given_options(AllOptionArgs, _, _,
            OptionSpecs, BuildGlobals, !IO),
        (
            OptionSpecs = [_ | _],
            Succeeded = no,
            MaybeInfo = no,
            usage_errors(BuildGlobals, OptionSpecs, !IO)
        ;
            OptionSpecs = [],
            Build(BuildGlobals, AllOptionArgs, Succeeded, Info0, Info, !IO),
            MaybeInfo = yes(Info)
        )
    ).

redirect_output(_ModuleName, MaybeErrorStream, !Info, !IO) :-
    % Write the output to a temporary file first, so it's easy to just print
    % the part of the error file that relates to the current command. It will
    % be appended to the error file later.
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
        module_name_to_file_name(Globals, do_create_dirs, ".err",
            ModuleName, ErrorFileName, !IO),
        ( if set.member(ModuleName, !.Info ^ error_file_modules) then
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

            !Info ^ error_file_modules :=
                set.insert(!.Info ^ error_file_modules, ModuleName)
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
    io.output_stream::in, io.output_stream::in, int::in, io::di, io::uo)
    is det.

make_write_error_streams(FileName, InputStream, FullOutputStream,
        PartialOutputStream, LinesToWrite, !IO) :-
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

:- pred make_write_error_char(io.output_stream::in, io.output_stream::in,
    char::in, int::in, int::out, io::di, io::uo) is det.

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
        io.write_string(PartialOutputStream, "... error log truncated, see `",
            !IO),
        io.write_string(PartialOutputStream, FullOutputFileName, !IO),
        io.write_string(PartialOutputStream, "' for the complete log.\n", !IO),
        % Only write the above message once.
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
    io.write_string(ErrorMessage, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%

get_timestamp_file_timestamp(Globals, target_file(ModuleName, FileType),
        MaybeTimestamp, !Info, !IO) :-
    ( if timestamp_extension(FileType, TimestampExt) then
        module_name_to_file_name(Globals, do_not_create_dirs, TimestampExt,
            ModuleName, FileName, !IO)
    else
        module_target_to_file_name(Globals, do_not_create_dirs, FileType,
            ModuleName, FileName, !IO)
    ),

    % We should only ever look for timestamp files in the current directory.
    % Timestamp files are only used when processing a module, and only modules
    % in the current directory are processed.
    SearchDirs = [dir.this_directory],
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO).

get_dependency_timestamp(Globals, DependencyFile, MaybeTimestamp, !Info,
        !IO) :-
    (
        DependencyFile = dep_file(FileName, MaybeOption),
        (
            MaybeOption = yes(Option),
            globals.lookup_accumulating_option(Globals, Option, SearchDirs)
        ;
            MaybeOption = no,
            SearchDirs = [dir.this_directory]
        ),
        get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO)
    ;
        DependencyFile = dep_target(Target),
        get_target_timestamp(Globals, do_search, Target, MaybeTimestamp0,
            !Info, !IO),
        ( if
            Target = target_file(_, module_target_c_header(header_mih)),
            MaybeTimestamp0 = ok(_)
        then
            % Don't rebuild the `.o' file if an irrelevant part of a
            % `.mih' file has changed. If a relevant part of a `.mih'
            % file changed, the interface files of the imported module
            % must have changed in a way that would force the `.c' and
            % `.o' files of the current module to be rebuilt.
            MaybeTimestamp = ok(oldest_timestamp)
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    ).

get_target_timestamp(Globals, Search, TargetFile, MaybeTimestamp, !Info,
        !IO) :-
    TargetFile = target_file(_ModuleName, FileType),
    get_file_name(Globals, Search, TargetFile, FileName, !Info, !IO),
    ( if FileType = module_target_analysis_registry then
        get_target_timestamp_analysis_registry(Globals, Search, TargetFile,
            FileName, MaybeTimestamp, !Info, !IO)
    else
        get_target_timestamp_2(Globals, Search, TargetFile,
            FileName, MaybeTimestamp, !Info, !IO)
    ).

    % Special treatment for `.analysis' files. If the corresponding
    % `.analysis_status' file says the `.analysis' file is invalid then we
    % treat it as out of date.
    %
:- pred get_target_timestamp_analysis_registry(globals::in, maybe_search::in,
    target_file::in, file_name::in, maybe_error(timestamp)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

get_target_timestamp_analysis_registry(Globals, Search, TargetFile, FileName,
        MaybeTimestamp, !Info, !IO) :-
    TargetFile = target_file(ModuleName, _FileType),
    ( if MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) then
        MaybeTimestamp = MaybeTimestamp0
    else
        do_read_module_overall_status(mmc, Globals, ModuleName, Status, !IO),
        (
            ( Status = optimal
            ; Status = suboptimal
            ),
            get_target_timestamp_2(Globals, Search, TargetFile, FileName,
                MaybeTimestamp, !Info, !IO)
        ;
            Status = invalid,
            MaybeTimestamp = error("invalid module"),
            !Info ^ file_timestamps ^ elem(FileName) := MaybeTimestamp
        )
    ).

:- pred get_target_timestamp_2(globals::in, maybe_search::in, target_file::in,
    file_name::in, maybe_error(timestamp)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

get_target_timestamp_2(Globals, Search, TargetFile, FileName, MaybeTimestamp,
        !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    (
        Search = do_search,
        get_search_directories(Globals, FileType, SearchDirs)
    ;
        Search = do_not_search,
        SearchDirs = [dir.this_directory]
    ),
    get_file_timestamp(SearchDirs, FileName, MaybeTimestamp0, !Info, !IO),
    ( if
        MaybeTimestamp0 = error(_),
        ( FileType = module_target_intermodule_interface
        ; FileType = module_target_analysis_registry
        )
    then
        % If a `.opt' file in another directory doesn't exist,
        % it just means that a library wasn't compiled with
        % `--intermodule-optimization'.
        % Similarly for `.analysis' files.

        get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
        ( if
            MaybeImports = yes(Imports),
            Imports ^ mai_module_dir \= dir.this_directory
        then
            MaybeTimestamp = ok(oldest_timestamp),
            !:Info = !.Info ^ file_timestamps ^ elem(FileName)
                := MaybeTimestamp
        else
            MaybeTimestamp = MaybeTimestamp0
        )
    else
        MaybeTimestamp = MaybeTimestamp0
    ).

get_file_name(Globals, Search, TargetFile, FileName, !Info, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    ( if FileType = module_target_source then
        % In some cases the module name won't match the file name
        % (module mdb.parse might be in parse.m or mdb.m), so we need to
        % look up the file name here.
        get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO),
        (
            MaybeImports = yes(Imports),
            FileName = Imports ^ mai_source_file_name
        ;
            MaybeImports = no,

            % Something has gone wrong generating the dependencies,
            % so just take a punt (which probably won't work).
            module_name_to_file_name(Globals, do_not_create_dirs, ".m",
                ModuleName, FileName, !IO)
        )
    else
        MaybeExt = target_extension(Globals, FileType),
        (
            MaybeExt = yes(Ext),
            (
                Search = do_search,
                module_name_to_search_file_name_cache(Globals, Ext,
                    ModuleName, FileName, !Info, !IO)
            ;
                Search = do_not_search,
                % Not common enough to cache.
                module_name_to_file_name(Globals, do_not_create_dirs, Ext,
                    ModuleName, FileName, !IO)
            )
        ;
            MaybeExt = no,
            module_target_to_file_name_maybe_search(Globals, Search,
                do_not_create_dirs, FileType, ModuleName, FileName, !IO)
        )
    ).

:- pred module_name_to_search_file_name_cache(globals::in, string::in,
    module_name::in, string::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

module_name_to_search_file_name_cache(Globals, Ext, ModuleName, FileName,
        !Info, !IO) :-
    Key = ModuleName - Ext,
    ( if map.search(!.Info ^ search_file_name_cache, Key, FileName0) then
        FileName = FileName0
    else
        module_name_to_search_file_name(Globals, Ext, ModuleName, FileName,
            !IO),
        !Info ^ search_file_name_cache ^ elem(Key) := FileName
    ).

get_file_timestamp(SearchDirs, FileName, MaybeTimestamp, !Info, !IO) :-
    ( if MaybeTimestamp0 = !.Info ^ file_timestamps ^ elem(FileName) then
        MaybeTimestamp = MaybeTimestamp0
    else
        search_for_file_mod_time(SearchDirs, FileName, SearchResult, !IO),
        (
            SearchResult = ok(TimeT),
            Timestamp = time_t_to_timestamp(TimeT),
            MaybeTimestamp = ok(Timestamp),
            !Info ^ file_timestamps ^ elem(FileName) := MaybeTimestamp
        ;
            SearchResult = error(_),
            MaybeTimestamp = error("file `" ++ FileName ++ "' not found")
        )
    ).

:- pred get_search_directories(globals::in, module_target_type::in,
    list(dir_name)::out) is det.

get_search_directories(Globals, FileType, SearchDirs) :-
    MaybeOpt = search_for_file_type(FileType),
    (
        MaybeOpt = yes(SearchDirOpt),
        globals.lookup_accumulating_option(Globals, SearchDirOpt, SearchDirs0),
        % Make sure the current directory is searched for C headers
        % and libraries.
        ( if list.member(dir.this_directory, SearchDirs0) then
            SearchDirs = SearchDirs0
        else
            SearchDirs = [dir.this_directory | SearchDirs0]
        )
    ;
        MaybeOpt = no,
        SearchDirs = [dir.this_directory]
    ).

find_oldest_timestamp(error(_) @ MaybeTimestamp, _) = MaybeTimestamp.
find_oldest_timestamp(ok(_), error(_) @ MaybeTimestamp) = MaybeTimestamp.
find_oldest_timestamp(ok(Timestamp1), ok(Timestamp2)) = ok(Timestamp) :-
    ( if compare((<), Timestamp1, Timestamp2) then
        Timestamp = Timestamp1
    else
        Timestamp = Timestamp2
    ).

%-----------------------------------------------------------------------------%

make_remove_target_file(Globals, VerboseOption, Target, !Info, !IO) :-
    Target = target_file(ModuleName, FileType),
    make_remove_target_file_by_name(Globals, VerboseOption,
        ModuleName, FileType, !Info, !IO).

make_remove_target_file_by_name(Globals, VerboseOption, ModuleName, FileType,
        !Info, !IO) :-
    module_target_to_file_name(Globals, do_not_create_dirs, FileType,
        ModuleName, FileName, !IO),
    make_remove_file(Globals, VerboseOption, FileName, !Info, !IO),
    ( if timestamp_extension(FileType, TimestampExt) then
        make_remove_module_file(Globals, VerboseOption, ModuleName,
            TimestampExt, !Info, !IO)
    else
        true
    ).

make_remove_module_file(Globals, VerboseOption, ModuleName, Ext, !Info, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, Ext,
        ModuleName, FileName, !IO),
    make_remove_file(Globals, VerboseOption, FileName, !Info, !IO).

make_remove_file(Globals, VerboseOption, FileName, !Info, !IO) :-
    verbose_make_msg_option(Globals, VerboseOption,
        report_remove_file(FileName), !IO),
    io.remove_file_recursively(FileName, _, !IO),
    FileTimestamps0 = !.Info ^ file_timestamps,
    map.delete(FileName, FileTimestamps0, FileTimestamps),
    !Info ^ file_timestamps := FileTimestamps.

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

    % XXX ".exe" if the module contains main.
target_extension(_, module_target_csharp_code) = yes(".cs").
target_extension(_, module_target_java_code) = yes(".java").
target_extension(_, module_target_java_class_code) = yes(".class").
target_extension(_, module_target_erlang_header) = yes(".hrl").
target_extension(_, module_target_erlang_code) = yes(".erl").
target_extension(_, module_target_erlang_beam_code) = yes(".beam").
target_extension(Globals, module_target_object_code(PIC)) = yes(Ext) :-
    maybe_pic_object_file_extension(Globals, PIC, Ext).
target_extension(_, module_target_xml_doc) = yes(".xml").

    % These all need to be handled as special cases.
target_extension(_, module_target_foreign_object(_, _)) = no.
target_extension(_, module_target_fact_table_object(_, _)) = no.

    % Currently the .cs extension is still treated as the build-all target for
    % C files, so we accept .csharp for C# files.
target_extension_synonym(".csharp", module_target_csharp_code).

linked_target_file_name(Globals, ModuleName, TargetType, FileName, !IO) :-
    (
        TargetType = executable,
        globals.lookup_string_option(Globals, executable_file_extension, Ext),
        module_name_to_file_name(Globals, do_not_create_dirs, Ext,
            ModuleName, FileName, !IO)
    ;
        TargetType = static_library,
        globals.lookup_string_option(Globals, library_extension, Ext),
        module_name_to_lib_file_name(Globals, "lib", ModuleName, Ext,
            do_not_create_dirs, FileName, !IO)
    ;
        TargetType = shared_library,
        globals.lookup_string_option(Globals, shared_library_extension, Ext),
        module_name_to_lib_file_name(Globals, "lib", ModuleName, Ext,
            do_not_create_dirs, FileName, !IO)
    ;
        TargetType = csharp_executable,
        module_name_to_file_name(Globals, do_not_create_dirs, ".exe",
            ModuleName, FileName, !IO)
    ;
        TargetType = csharp_library,
        module_name_to_file_name(Globals, do_not_create_dirs, ".dll",
            ModuleName, FileName, !IO)
    ;
        TargetType = erlang_launcher,
        % These are shell scripts.
        % XXX Shouldn't the extension be ".bat" when --target-env-type
        % is windows?
        module_name_to_file_name(Globals, do_not_create_dirs, "",
            ModuleName, FileName, !IO)
    ;
        ( TargetType = java_archive
        ; TargetType = java_executable
        ),
        module_name_to_file_name(Globals, do_not_create_dirs, ".jar",
            ModuleName, FileName, !IO)
    ;
        TargetType = erlang_archive,
        module_name_to_lib_file_name(Globals, "lib", ModuleName, ".beams",
            do_not_create_dirs, FileName, !IO)
    ).

:- pred module_target_to_file_name(globals::in, maybe_create_dirs::in,
    module_target_type::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

module_target_to_file_name(Globals, MkDir, TargetType, ModuleName, FileName,
        !IO) :-
    module_target_to_file_name_maybe_search(Globals, do_not_search, MkDir,
        TargetType, ModuleName, FileName, !IO).

:- pred module_target_to_file_name_maybe_search(globals::in,
    maybe_search::in, maybe_create_dirs::in, module_target_type::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_target_to_file_name_maybe_search(Globals, Search, MkDir, TargetType,
        ModuleName, FileName, !IO) :-
    target_extension(Globals, TargetType) = MaybeExt,
    (
        MaybeExt = yes(Ext),
        (
            Search = do_search,
            module_name_to_search_file_name(Globals, Ext,
                ModuleName, FileName, !IO)
        ;
            Search = do_not_search,
            module_name_to_file_name(Globals, MkDir, Ext,
                ModuleName, FileName, !IO)
        )
    ;
        MaybeExt = no,
        (
            TargetType = module_target_foreign_object(PIC, Lang),
            ( if
                ForeignModuleName =
                    foreign_language_module_name(ModuleName, Lang)
            then
                module_target_to_file_name_maybe_search(Globals,
                    Search, MkDir, module_target_object_code(PIC),
                    ForeignModuleName, FileName, !IO)
            else
                unexpected($module, $pred, "object test failed")
            )
        ;
            TargetType = module_target_fact_table_object(PIC, FactFile),
            maybe_pic_object_file_extension(Globals, PIC, Ext),
            fact_table_file_name(Globals, ModuleName, FactFile, Ext, MkDir,
                FileName, !IO)
        ;
            ( TargetType = module_target_analysis_registry
            ; TargetType = make.module_target_c_code
            ; TargetType = module_target_c_header(_)
            ; TargetType = module_target_erlang_beam_code
            ; TargetType = module_target_erlang_code
            ; TargetType = module_target_erlang_header
            ; TargetType = module_target_errors
            ; TargetType = module_target_intermodule_interface
            ; TargetType = module_target_csharp_code
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
            unexpected($module, $pred, "unexpected TargetType")
        )
    ).

timestamp_extension(ModuleTargetType, Extension) :-
    (
        ModuleTargetType = module_target_errors,
        % We need a timestamp file for `.err' files because errors are written
        % to the `.err' file even when writing interfaces. The timestamp
        % is only updated when compiling to target code.
        Extension = ".err_date"
    ;
        ModuleTargetType = module_target_private_interface,
        Extension = ".date0"
    ;
        ModuleTargetType = module_target_long_interface,
        Extension = ".date"
    ;
        ModuleTargetType = module_target_short_interface,
        Extension = ".date"
    ;
        ModuleTargetType = module_target_unqualified_short_interface,
        Extension = ".date3"
    ;
        ModuleTargetType = module_target_intermodule_interface,
        Extension = ".optdate"
    ;
        ModuleTargetType = module_target_analysis_registry,
        % We need a timestamp file for `.analysis' files because they
        % can be modified in the process of analysing _another_ module.
        % The timestamp is only updated after actually analysing the module
        % that the `.analysis' file corresponds to.
        Extension = ".analysis_date"
    ;
        % Header files share a timestamp file with their corresponding
        % target code files.
        ( ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_c_header(_)
        ),
        Extension = ".c_date"
    ;
        ModuleTargetType = module_target_csharp_code,
        Extension = ".cs_date"
    ;
        ModuleTargetType = module_target_java_code,
        Extension = ".java_date"
    ;
        % Header files share a timestamp file with their corresponding
        % target code files.
        ( ModuleTargetType = module_target_erlang_code
        ; ModuleTargetType = module_target_erlang_header
        ),
        Extension = ".erl_date"
    ).

:- func search_for_file_type(module_target_type) = maybe(option).

search_for_file_type(ModuleTargetType) = MaybeSearchOption :-
    (
        ( ModuleTargetType = module_target_source
        ; ModuleTargetType = module_target_errors
        ; ModuleTargetType = module_target_track_flags
        ; ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_csharp_code
        ; ModuleTargetType = module_target_java_code
        ; ModuleTargetType = module_target_java_class_code
        ; ModuleTargetType = module_target_erlang_code
        ; ModuleTargetType = module_target_erlang_beam_code
        ; ModuleTargetType = module_target_object_code(_)
        ; ModuleTargetType = module_target_foreign_object(_, _)
        ; ModuleTargetType = module_target_fact_table_object(_, _)
        ; ModuleTargetType = module_target_xml_doc
        ),
        MaybeSearchOption = no
    ;
        ( ModuleTargetType = module_target_private_interface
        ; ModuleTargetType = module_target_short_interface
        ; ModuleTargetType = module_target_long_interface
        ; ModuleTargetType = module_target_unqualified_short_interface
        ),
        MaybeSearchOption = yes(search_directories)
    ;
        ( ModuleTargetType = module_target_intermodule_interface
        ; ModuleTargetType = module_target_analysis_registry
        ),
        MaybeSearchOption = yes(intermod_directories)
    ;
        ModuleTargetType = module_target_c_header(_),
        MaybeSearchOption = yes(c_include_directory)
    ;
        ModuleTargetType = module_target_erlang_header,
        MaybeSearchOption = yes(erlang_include_directory)
    ).

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
        ; Target = module_target_csharp_code
        ; Target = module_target_java_code
        ; Target = module_target_java_class_code
        ; Target = module_target_erlang_code
        ; Target = module_target_erlang_beam_code
        ; Target = module_target_erlang_header
        ; Target = module_target_object_code(_)
        ; Target = module_target_foreign_object(_, _)
        ; Target = module_target_fact_table_object(_, _)
        ),
        IsDependent = yes
    ).

%-----------------------------------------------------------------------------%

debug_make_msg(Globals, P, !IO) :-
    verbose_make_msg_option(Globals, debug_make, P, !IO).

verbose_make_msg(Globals, P, !IO) :-
    verbose_make_msg_option(Globals, verbose_make, P, !IO).

verbose_make_msg_option(Globals, Option, P, !IO) :-
    globals.lookup_bool_option(Globals, Option, OptionValue),
    (
        OptionValue = yes,
        P(!IO),
        io.flush_output(!IO)
    ;
        OptionValue = no
    ).

debug_file_msg(Globals, TargetFile, Msg, !IO) :-
    debug_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            make_write_target_file(Globals, TargetFile, !IO),
            io.write_string(": ", !IO),
            io.write_string(Msg, !IO),
            io.nl(!IO)
        ), !IO).

make_write_dependency_file(Globals, dep_target(TargetFile), !IO) :-
    make_write_target_file(Globals, TargetFile, !IO).
make_write_dependency_file(_Globals, dep_file(FileName, _), !IO) :-
    io.write_string(FileName, !IO).

make_write_dependency_file_list(_, [], !IO).
make_write_dependency_file_list(Globals, [DepFile | DepFiles], !IO) :-
    io.write_string("\t", !IO),
    make_write_dependency_file(Globals, DepFile, !IO),
    io.nl(!IO),
    make_write_dependency_file_list(Globals, DepFiles, !IO).

make_write_target_file(Globals, TargetFile, !IO) :-
    make_write_target_file_wrapped(Globals, "", TargetFile, "", !IO).

make_write_target_file_wrapped(Globals, Prefix, TargetFile, Suffix, !IO) :-
    TargetFile = target_file(ModuleName, FileType),
    module_target_to_file_name(Globals, do_not_create_dirs, FileType,
        ModuleName, FileName, !IO),
    ( if
        Prefix = "",
        Suffix = ""
    then
        io.write_string(FileName, !IO)
    else
        % Try to write this with one call to avoid interleaved output when
        % doing parallel builds.
        io.write_string(Prefix ++ FileName ++ Suffix, !IO)
    ).

maybe_make_linked_target_message(Globals, TargetFile, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            % Write this with one call to avoid interleaved output
            % when doing parallel builds.
            io.write_string("Making " ++ TargetFile ++ "\n", !IO)
        ), !IO).

maybe_make_target_message(Globals, TargetFile, !IO) :-
    io.output_stream(OutputStream, !IO),
    maybe_make_target_message_to_stream(Globals, OutputStream, TargetFile,
        !IO).

maybe_make_target_message_to_stream(Globals, OutputStream, TargetFile, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.set_output_stream(OutputStream, OldOutputStream, !IO),
            make_write_target_file_wrapped(Globals, "Making ", TargetFile,
                "\n", !IO),
            io.set_output_stream(OldOutputStream, _, !IO)
        ), !IO).

maybe_reanalyse_modules_message(Globals, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.output_stream(OutputStream, !IO),
            io.write_string(OutputStream,
                "Reanalysing invalid/suboptimal modules\n", !IO)
        ), !IO).

target_file_error(Info, Globals, TargetFile, !IO) :-
    with_locked_stdout(Info,
        make_write_target_file_wrapped(Globals,
            "** Error making `", TargetFile, "'.\n"), !IO).

file_error(Info, TargetFile, !IO) :-
    with_locked_stdout(Info,
        io.write_string("** Error making `" ++ TargetFile ++ "'.\n"), !IO).

maybe_warn_up_to_date_target(Globals, Target, !Info, !IO) :-
    globals.lookup_bool_option(Globals, warn_up_to_date, Warn),
    (
        Warn = yes,
        ( if set.member(Target, !.Info ^ command_line_targets) then
            io.write_string("** Nothing to be done for `", !IO),
            make_write_module_or_linked_target(Globals, Target, !IO),
            io.write_string("'.\n", !IO)
        else
            true
        )
    ;
        Warn = no
    ),
    CmdLineTargets0 = !.Info ^ command_line_targets,
    set.delete(Target, CmdLineTargets0, CmdLineTargets),
    !Info ^ command_line_targets := CmdLineTargets.

maybe_symlink_or_copy_linked_target_message(Globals, Target, !IO) :-
    verbose_make_msg(Globals,
        ( pred(!.IO::di, !:IO::uo) is det :-
            io.write_string("Made symlink/copy of ", !IO),
            make_write_module_or_linked_target(Globals, Target, !IO),
            io.write_string("\n", !IO)
        ), !IO).

:- pred make_write_module_or_linked_target(globals::in,
    pair(module_name, target_type)::in, io::di, io::uo) is det.

make_write_module_or_linked_target(Globals, ModuleName - FileType, !IO) :-
    (
        FileType = module_target(ModuleTargetType),
        TargetFile = target_file(ModuleName, ModuleTargetType),
        make_write_target_file(Globals, TargetFile, !IO)
    ;
        FileType = linked_target(LinkedTargetType),
        linked_target_file_name(Globals, ModuleName, LinkedTargetType,
            FileName, !IO),
        io.write_string(FileName, !IO)
    ;
        FileType = misc_target(_),
        unexpected($module, $pred, "misc_target")
    ).

%-----------------------------------------------------------------------------%
%
% Timing.
%

:- pragma foreign_proc("C",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = MR_get_real_milliseconds();
").

:- pragma foreign_proc("C#",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Time = System.Environment.TickCount;
").

:- pragma foreign_proc("Java",
    get_real_milliseconds(Time::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    // The loss of precision is acceptable for mmc --make.
    Time = (int) System.currentTimeMillis();
").

get_real_milliseconds(_, _, _) :-
    sorry($file, $pred).

%-----------------------------------------------------------------------------%
%
% Hash functions.
%

module_name_hash(SymName, Hash) :-
    (
        SymName = unqualified(String),
        Hash = string.hash(String)
    ;
        SymName = qualified(_Qual, String),
        % Hashing the module qualifier seems to be not worthwhile.
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
        Type = module_target_java_code,
        X = 12
    ;
        Type = module_target_erlang_header,
        X = 13
    ;
        Type = module_target_erlang_code,
        X = 14
    ;
        Type = module_target_erlang_beam_code,
        X = 15
    ;
        Type = module_target_object_code(PIC),
        X = 16 `mix` pic_to_nonce(PIC)
    ;
        Type = module_target_foreign_object(_PIC, _ForeignLang),
        X = 17
    ;
        Type = module_target_fact_table_object(_PIC, _FileName),
        X = 18
    ;
        Type = module_target_xml_doc,
        X = 19
    ;
        Type = module_target_track_flags,
        X = 20
    ;
        Type = module_target_java_class_code,
        X = 21
    ;
        Type = module_target_csharp_code,
        X = 22
    ).

:- func pic_to_nonce(pic) = int.

pic_to_nonce(pic) = 1.
pic_to_nonce(non_pic) = 3.
% For compatibility; we used to have pic_to_nonce(link_with_pic) = 2.

:- func mix(int, int) = int.

mix(H0, X) = H :-
    H1 = H0 `xor` (H0 `unchecked_left_shift` 5),
    H = H1 `xor` X.

%-----------------------------------------------------------------------------%
:- end_module make.util.
%-----------------------------------------------------------------------------%
