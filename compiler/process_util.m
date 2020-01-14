%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: process_util.m.
% Main author: stayl.
%
% Process and signal handling, mainly for use by make.m and its submodules.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module libs.process_util.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type build0(Info) == pred(bool, Info, Info, io, io).
:- inst build0 == (pred(out, in, out, di, uo) is det).

:- type post_signal_cleanup(Info) == pred(Info, Info, io, io).
:- inst post_signal_cleanup == (pred(in, out, di, uo) is det).

    % build_with_check_for_interrupt(VeryVerbose, Build, Cleanup, Succeeded,
    %   !Info, !IO):
    %
    % Apply `Build' with signal handlers installed to check for signals
    % which would normally kill the process. If a signal occurs call `Cleanup',
    % then restore signal handlers to their defaults and reraise the signal
    % to kill the current process. An action being performed in a child process
    % by call_in_forked_process will be killed if a fatal signal (SIGINT,
    % SIGTERM, SIGHUP or SIGQUIT) is received by the current process.
    % An action being performed within the current process or by system()
    % will run to completion, with the interrupt being taken immediately
    % afterwards.
    %
:- pred build_with_check_for_interrupt(bool::in, build0(Info)::in(build0),
    post_signal_cleanup(Info)::in(post_signal_cleanup), bool::out,
    Info::in, Info::out, io::di, io::uo) is det.

    % raise_signal(Signal).
    % Send `Signal' to the current process.
    %
:- pred raise_signal(int::in, io::di, io::uo) is det.

    % send_signal(Signal, Pid).
    % Send `Signal' to `Pid'.
    %
:- pred send_signal(int::in, pid::in, io::di, io::uo) is det.

:- func sigint = int.

%-----------------------------------------------------------------------------%

:- type io_pred == pred(bool, io, io).
:- inst io_pred == (pred(out, di, uo) is det).

:- type pid == int.

    % Does fork() work on the current platform.
    %
:- pred can_fork is semidet.

    % call_in_forked_process(P, AltP, Succeeded):
    %
    % Execute `P' in a separate process.
    %
    % We prefer to use fork() rather than system() because
    % that will avoid shell and Mercury runtime startup overhead.
    % Interrupt handling will also work better (system() on Linux
    % ignores SIGINT).
    %
    % If fork() is not supported on the current architecture,
    % `AltP' will be called instead in the current process.
    %
:- pred call_in_forked_process_with_backup(io_pred::in(io_pred),
    io_pred::in(io_pred), bool::out, io::di, io::uo) is det.

    % As above, but if fork() is not available, just call the
    % predicate in the current process.
    %
:- pred call_in_forked_process(io_pred::in(io_pred), bool::out,
    io::di, io::uo) is det.

    % start_in_forked_process(P, Succeeded, !IO)
    %
    % Start executing `P' in a child process.  Returns immediately, i.e. does
    % not wait for `P' to finish.  This predicate should only be called if
    % fork() is available.
    %
    % The child process's exit code will be 0 if `P' returns a success value of
    % `yes', or 1 if the success value is `no'.
    %
:- pred start_in_forked_process(io_pred::in(io_pred), maybe(pid)::out,
    io::di, io::uo) is det.

    % wait_pid(Pid, ExitCode, !IO)
    %
    % Block until the child process with process id Pid exited.
    % Return the exit code of the child.
    %
:- pred wait_pid(pid::in, io.res(io.system_result)::out, io::di, io::uo)
    is det.

    % wait_any(Pid, ExitCode, !IO)
    %
    % Block until a child process has exited. Return the process ID
    % of the child and its exit code.
    %
:- pred wait_any(pid::out, io.res(io.system_result)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.   % Required by non-C grades.

%-----------------------------------------------------------------------------%

build_with_check_for_interrupt(VeryVerbose, Build, Cleanup, Succeeded,
        !Info, !IO) :-
    setup_signal_handlers(MaybeSigIntHandler, !IO),
    Build(Succeeded0, !Info, !IO),
    restore_signal_handlers(MaybeSigIntHandler, !IO),
    check_for_signal(Signalled, Signal, !IO),
    ( if Signalled = 1 then
        Succeeded = no,
        (
            VeryVerbose = yes,
            io.write_string("** Received signal ", !IO),
            io.write_int(Signal, !IO),
            io.write_string(", cleaning up.\n", !IO)
        ;
            VeryVerbose = no
        ),
        Cleanup(!Info, !IO),

        % The signal handler has been restored to the default,
        % so this should kill us.
        raise_signal(Signal, !IO)
    else
        Succeeded = Succeeded0
    ).

:- type signal_action ---> signal_action.
:- pragma foreign_type("C", signal_action, "MR_signal_action").

:- pragma foreign_decl("C",
"
#ifdef MR_HAVE_UNISTD_H
  #include <unistd.h>
#endif

#ifdef MR_HAVE_SYS_TYPES_H
  #include <sys/types.h>
#endif

#ifdef MR_HAVE_SYS_WAIT_H
  #include <sys/wait.h>
#endif

#include <errno.h>

#include ""mercury_signal.h""
#include ""mercury_types.h""
#include ""mercury_heap.h""
#include ""mercury_misc.h""

#if defined(MR_HAVE_FORK) && defined(MR_HAVE_WAIT) && defined(MR_HAVE_KILL)
  #define MC_CAN_FORK 1
#endif

#define MC_SETUP_SIGNAL_HANDLER(sig, handler) \
        MR_setup_signal(sig, (MR_Code *) handler, MR_FALSE, \
            ""mercury_compile: cannot install signal handler"");

    /* Have we received a signal. */
extern volatile sig_atomic_t MC_signalled;

    /*
    ** Which signal did we receive.
    ** XXX This assumes a signal number will fit into a sig_atomic_t.
    */
extern volatile sig_atomic_t MC_signal_received;

void MC_mercury_compile_signal_handler(int sig);
").

:- pragma foreign_code("C",
"
volatile sig_atomic_t MC_signalled = MR_FALSE;
volatile sig_atomic_t MC_signal_received = 0;

void
MC_mercury_compile_signal_handler(int sig)
{
    MC_signalled = MR_TRUE;
    MC_signal_received = sig;
}
").

:- pred setup_signal_handlers(signal_action::out, io::di, io::uo) is det.

setup_signal_handlers(signal_action::out, IO::di, IO::uo).

:- pragma foreign_proc("C",
    setup_signal_handlers(SigintHandler::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MC_signalled = MR_FALSE;

    /*
    ** mdb sets up a SIGINT handler, so we should restore
    ** it after we're done.
    */
    MR_get_signal_action(SIGINT, &SigintHandler,
        ""error getting SIGINT handler"");
    MC_SETUP_SIGNAL_HANDLER(SIGINT, MC_mercury_compile_signal_handler);
    MC_SETUP_SIGNAL_HANDLER(SIGTERM, MC_mercury_compile_signal_handler);
#ifdef SIGHUP
    MC_SETUP_SIGNAL_HANDLER(SIGHUP, MC_mercury_compile_signal_handler);
#endif
#ifdef SIGQUIT
    MC_SETUP_SIGNAL_HANDLER(SIGQUIT, MC_mercury_compile_signal_handler);
#endif
").

:- pred restore_signal_handlers(signal_action::in, io::di, io::uo) is det.

restore_signal_handlers(_::in, IO::di, IO::uo).

:- pragma foreign_proc("C",
    restore_signal_handlers(SigintHandler::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    MR_set_signal_action(SIGINT, &SigintHandler,
        ""error resetting SIGINT handler"");
    MC_SETUP_SIGNAL_HANDLER(SIGTERM, SIG_DFL);
#ifdef SIGHUP
    MC_SETUP_SIGNAL_HANDLER(SIGHUP, SIG_DFL);
#endif
#ifdef SIGQUIT
    MC_SETUP_SIGNAL_HANDLER(SIGQUIT, SIG_DFL);
#endif
").

    % Restore all signal handlers to default values in the child so that
    % the child will be killed by the signals the parent is catching.
    %
:- pred setup_child_signal_handlers(io::di, io::uo) is det.

setup_child_signal_handlers(!IO) :-
    restore_signal_handlers(sig_dfl, !IO).

:- func sig_dfl = signal_action.

sig_dfl = (signal_action::out).

:- pragma foreign_proc("C",
    sig_dfl = (Result::out),
    [will_not_call_mercury, promise_pure],
"
    MR_init_signal_action(&Result, SIG_DFL, MR_FALSE, MR_TRUE);
").

:- pred check_for_signal(int::out, int::out, io::di, io::uo) is det.

check_for_signal(0::out, 0::out, IO::di, IO::uo).

:- pragma foreign_proc("C",
    check_for_signal(Signalled::out, Signal::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Signalled = (MC_signalled ? 1 : 0);
    Signal = MC_signal_received;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include <signal.h>").

    % If this aborted it would cause partially built files
    % to be left lying around with `--make'.
raise_signal(_::in, IO::di, IO::uo).

:- pragma foreign_proc("C",
    raise_signal(Signal::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    raise((int)Signal);
").

:- pragma no_determinism_warning(send_signal/4).

:- pragma foreign_proc("C",
    send_signal(Pid::in, Signal::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef MR_HAVE_KILL
    kill((pid_t)Pid, (int)Signal);
#endif
").

send_signal(_, _, _, _) :-
    sorry($file, $pred).

:- pragma no_determinism_warning(sigint/0).

:- pragma foreign_proc("C",
    sigint = (Sigint::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Sigint = SIGINT;
").

sigint = _ :-
    sorry($file, $pred).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    can_fork,
    [will_not_call_mercury, thread_safe, promise_pure],
"
    /*
    ** call_in_forked_process_2 is not `thread_safe' so will hold a mutex
    ** that the child process will want.  At the same time the parent process
    ** waits for the child to exit, so we have a deadlock.
    **
    ** Also, in pthreads, a forked process does not inherit the threads of
    ** the original process so it is not at all clear whether we could use
    ** fork() when running in a parallel grade.
    */
#if (defined MC_CAN_FORK) && (!defined MR_THREAD_SAFE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

can_fork :- semidet_fail.

call_in_forked_process_with_backup(P, AltP, Success, !IO) :-
    ( if can_fork then
        start_in_forked_process(P, MaybePid, !IO),
        (
            MaybePid = yes(Pid),
            do_wait(Pid, _, CallStatus, !IO),
            Status = decode_system_command_exit_code(CallStatus),
            Success = ( if Status = ok(exited(0)) then yes else no )
        ;
            MaybePid = no,
            Success = no
        )
    else
        AltP(Success, !IO)
    ).

call_in_forked_process(P, Success, !IO) :-
    call_in_forked_process_with_backup(P, P, Success, !IO).

start_in_forked_process(P, MaybePid, !IO) :-
    start_in_forked_process_2(P, Pid, !IO),
    ( if Pid = 0 then
        MaybePid = no
    else
        MaybePid = yes(Pid)
    ).

:- pred start_in_forked_process_2(io_pred::in(io_pred), pid::out,
    io::di, io::uo) is det.
:- pragma no_determinism_warning(start_in_forked_process_2/4).

:- pragma foreign_proc("C",
    start_in_forked_process_2(Pred::in(io_pred), Pid::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io],
"
#ifdef MC_CAN_FORK

    Pid = fork();
    if (Pid == -1) {                        /* error */
        MR_perror(""error in fork()"");
    } else if (Pid == 0) {                  /* child */
        MR_Integer exit_status;

        MC_call_child_process_io_pred(Pred, &exit_status);
        exit((int)exit_status);
    } else {                                /* parent */
    }

#else /* ! MC_CAN_FORK */
    Pid = 0;
#endif /* ! MC_CAN_FORK */
").

start_in_forked_process_2(_, _, !IO) :-
    sorry($pred, "NYI in languages other than C").

    % call_child_process_io_pred(P, ExitStatus).
    %
:- pred call_child_process_io_pred(io_pred::in(io_pred), int::out,
    io::di, io::uo) is det.

:- pragma foreign_export("C",
    call_child_process_io_pred(in(io_pred), out, di, uo),
    "MC_call_child_process_io_pred").

call_child_process_io_pred(P, Status, !IO) :-
    setup_child_signal_handlers(!IO),
    P(Success, !IO),
    (
        Success = yes,
        Status = 0
    ;
        Success = no,
        Status = 1
    ).

    % do_wait(Pid, WaitedPid, Status, !IO)
    %
    % Wait until Pid exits and return its status.
    % If Pid is -1 then wait for any child process to exit.
    %
:- pred do_wait(pid::in, pid::out, int::out, io::di, io::uo) is det.
:- pragma no_determinism_warning(do_wait/5).

:- pragma foreign_proc("C",
    do_wait(Pid::in, WaitedPid::out, Status::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
#ifdef MC_CAN_FORK
    {
        int     child_status;
        pid_t   wait_status;

        /*
        ** Make sure the wait() is interrupted by the signals
        ** which cause us to exit.
        */
        MR_signal_should_restart(SIGINT, MR_FALSE);
        MR_signal_should_restart(SIGTERM, MR_FALSE);
#ifdef SIGHUP
        MR_signal_should_restart(SIGHUP, MR_FALSE);
#endif
#ifdef SIGQUIT
        MR_signal_should_restart(SIGQUIT, MR_FALSE);
#endif

        while (1) {
            wait_status = waitpid((pid_t)Pid, &child_status, 0);
            if (wait_status != -1) {
                WaitedPid = wait_status;
                Status = child_status;
                break;
            } else if (MR_is_eintr(errno)) {
                if (MC_signalled) {
                    /*
                    ** A normally fatal signal has been received, so kill the
                    ** child immediately.  Use SIGTERM, not MC_signal_received,
                    ** because the child may be inside a call to system() which
                    ** would cause SIGINT to be ignored on some systems (e.g.
                    ** Linux).
                    */
                    if (Pid != -1) {
                        kill((pid_t)Pid, SIGTERM);
                    }
                    break;
                }
            } else {
                /*
                ** This should never happen.
                */
                MR_perror(""error in wait(): "");
                Status = 1;
                break;
            }
        }

        /*
        ** Restore the system call signal behaviour.
        */
        MR_signal_should_restart(SIGINT, MR_TRUE);
        MR_signal_should_restart(SIGTERM, MR_TRUE);
#ifdef SIGHUP
        MR_signal_should_restart(SIGHUP, MR_TRUE);
#endif
#ifdef SIGQUIT
        MR_signal_should_restart(SIGQUIT, MR_TRUE);
#endif
    }

#else /* ! MC_CAN_FORK */
    MR_perror(""cannot wait() when fork() is unavailable: "");
    Status = 1;
#endif /* ! MC_CAN_FORK */
").

do_wait(_, _, _, _, _) :-
    sorry($file, $pred).

wait_pid(Pid, Status, !IO) :-
    do_wait(Pid, _Pid, Status0, !IO),
    Status = decode_system_command_exit_code(Status0).

wait_any(Pid, Status, !IO) :-
    do_wait(-1, Pid, Status0, !IO),
    Status = decode_system_command_exit_code(Status0).

%-----------------------------------------------------------------------------%
:- end_module libs.process_util.
%-----------------------------------------------------------------------------%
