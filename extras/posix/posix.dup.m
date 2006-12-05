%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__dup.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__dup.

:- interface.

:- pred dup(fd, posix__result(fd), io__state, io__state).
:- mode dup(in, out, di, uo) is det.

:- pred dup2(fd, fd, posix__result(fd), io__state, io__state).
:- mode dup2(in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

dup(Fd, Result) -->
	dup0(Fd, fd(NewFd)),
	( if { NewFd < 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ Result = ok(fd(NewFd)) }
	).

:- pred dup0(fd, fd, io__state, io__state).
:- mode dup0(in, out, di, uo) is det.

:- pragma c_code(dup0(OldFd::in, NewFd::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	NewFd = dup(OldFd);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

dup2(OldFd, NewFd, Result) -->
	dup2_2(OldFd, NewFd, fd(Ret)),
	( if { Ret < 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ Result = ok(fd(Ret)) }
	).

:- pred dup2_2(fd, fd, fd, io__state, io__state).
:- mode dup2_2(in, in, out, di, uo) is det.

:- pragma c_code(dup2_2(OldFd::in, NewFd::in, Ret::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "{
	Ret = dup2(OldFd, NewFd);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

