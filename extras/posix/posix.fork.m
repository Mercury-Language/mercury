%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__fork.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__fork.

:- interface.

:- type whoami
	--->	child
	;	parent(pid_t)
	.

:- pred fork(posix__result(whoami), io__state, io__state).
:- mode fork(out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

fork(Result) -->
	fork0(Pid),
	( if { Pid < 0 } then
		errno(Err),
		{ Result = error(Err) }
	else if { Pid = 0 } then
		{ Result = ok(child) }
	else
		{ Result = ok(parent(pid(Pid))) }
	).

:- pred fork0(int, io__state, io__state).
:- mode fork0(out, di, uo) is det.

:- pragma c_code(fork0(Pid::out, IO0::di, IO::uo), [will_not_call_mercury], "{
	Pid = fork();
	IO = IO0;
}").

%------------------------------------------------------------------------------%

