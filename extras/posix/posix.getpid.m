%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__getpid.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__getpid.

:- interface.

:- pred getpid(pid_t, io__state, io__state).
:- mode getpid(out, di, uo) is det.

:- pred getppid(pid_t, io__state, io__state).
:- mode getppid(out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

:- pragma c_code(getpid(Pid::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Pid = getpid();
	IO = IO0;
").

:- pragma c_code(getppid(Pid::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Pid = getppid();
	IO = IO0;
").

%------------------------------------------------------------------------------%

