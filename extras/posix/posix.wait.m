%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: posix.wait.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.wait.
:- interface.

%-----------------------------------------------------------------------------%

:- type wait_for
    --->    any_child
    ;       child(pid_t)
    ;       child_in_group(pid_t)
    ;       child_in_same_group.

:- type status
    --->    exit(int)
    ;       signal(int).

:- pred wait(posix.result({pid_t, status})::out, io::di, io::uo) is det.

:- pred waitpid(wait_for::in, posix.result({pid_t, status})::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <sys/wait.h>
").

%-----------------------------------------------------------------------------%

wait(Result, !IO) :-
    wait0(Pid, Status, !IO),
    ( if Pid < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        ( if if_exited(Status) then
            Result = ok({pid(Pid), exit(exit_status(Status))})
        else
            Result = ok({pid(Pid), signal(term_sig(Status))})
        )
    ).

:- pred wait0(int::out, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    wait0(Pid::out, Status::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    int status;
    Pid = wait(&status);
    Status = status;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

waitpid(WaitFor, Result, !IO) :-
    (
        WaitFor = any_child,
        Pid0 = pid(-1)
    ;
        WaitFor = child(Pid0)
    ;
        WaitFor = child_in_group(pid(Group)),
        Pid0 = pid(-Group)
    ;
        WaitFor = child_in_same_group,
        Pid0 = pid(0)
    ),
    waitpid0(Pid0, Pid, Status, !IO),
    ( if Pid < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        ( if if_exited(Status) then
            Result = ok({pid(Pid), exit(exit_status(Status))})
        else
            Result = ok({pid(Pid), signal(term_sig(Status))})
        )
    ).

:- pred waitpid0(pid_t::in, int::out, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    waitpid0(Pid0::in, Pid::out, Status::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    int status;
    Pid = waitpid(Pid0, &status, 0);
    Status = status;
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pred if_exited(int::in) is semidet.
:- pragma foreign_proc("C",
    if_exited(Status::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = WIFEXITED(Status);
").

:- pred if_signaled(int::in) is semidet.
:- pragma foreign_proc("C",
    if_signaled(Status::in),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    SUCCESS_INDICATOR = WIFSIGNALED(Status);
").

:- func exit_status(int) = int.
:- pragma foreign_proc("C",
    exit_status(Status::in) = (ExitCode::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ExitCode = WEXITSTATUS(Status);
").

:- func term_sig(int) = int.
:- pragma foreign_proc("C",
    term_sig(Status::in) = (ExitCode::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ExitCode = WTERMSIG(Status);
").

%-----------------------------------------------------------------------------%
:- end_module posix.wait.
%-----------------------------------------------------------------------------%
