%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__wait.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__wait.

:- interface.

:- type wait_for
	--->	any_child
	;	child(pid_t)
	;	child_in_group(pid_t)
	;	child_in_same_group
	.

:- type status
	--->	exit(int)
	;	signal(int)
	.

:- pred wait(posix__result({pid_t, status}), io__state, io__state).
:- mode wait(out, di, uo) is det.

:- pred waitpid(wait_for, posix__result({pid_t, status}), io__state, io__state).
:- mode waitpid(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <sys/wait.h>
").

%------------------------------------------------------------------------------%

wait(Result) -->
	wait0(Pid, Status),
	( if { Pid < 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ if if_exited(Status) then
			Result = ok({pid(Pid), exit(exit_status(Status))})
		else
			Result = ok({pid(Pid), signal(term_sig(Status))})
		}
	).

:- pred wait0(int, int, io__state, io__state).
:- mode wait0(out, out, di, uo) is det.

:- pragma c_code(wait0(Pid::out, Status::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	int status;
	Pid = wait(&status);
	Status = status;
	IO = IO0;
}").

%------------------------------------------------------------------------------%

waitpid(WaitFor, Result) -->
	(
		{ WaitFor = any_child, Pid0 = pid(-1) }
	;
		{ WaitFor = child(Pid0) }
	;
		{ WaitFor = child_in_group(pid(Group)), Pid0 = pid(-Group) }
	;
		{ WaitFor = child_in_same_group, Pid0 = pid(0) }
	),
	waitpid0(Pid0, Pid, Status),
	( if { Pid < 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ if if_exited(Status) then
			Result = ok({pid(Pid), exit(exit_status(Status))})
		else
			Result = ok({pid(Pid), signal(term_sig(Status))})
		}
	).

:- pred waitpid0(pid_t, int, int, io__state, io__state).
:- mode waitpid0(in, out, out, di, uo) is det.

:- pragma c_code(waitpid0(Pid0::in, Pid::out, Status::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	int status;
	Pid = waitpid(Pid0, &status, 0);
	Status = status;
	IO = IO0;
}").

%------------------------------------------------------------------------------%

:- pred if_exited(int).
:- mode if_exited(in) is semidet.

:- pragma c_code(if_exited(Status::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = WIFEXITED(Status);
").

:- pred if_signaled(int).
:- mode if_signaled(in) is semidet.

:- pragma c_code(if_signaled(Status::in), [will_not_call_mercury, thread_safe], "
	SUCCESS_INDICATOR = WIFSIGNALED(Status);
").

:- func exit_status(int) = int.

:- pragma c_code(exit_status(Status::in) = (ExitCode::out),
		[will_not_call_mercury, thread_safe],
"
	ExitCode = WEXITSTATUS(Status);
").

:- func term_sig(int) = int.

:- pragma c_code(term_sig(Status::in) = (ExitCode::out),
		[will_not_call_mercury, thread_safe],
"
	ExitCode = WTERMSIG(Status);
").

%------------------------------------------------------------------------------%

