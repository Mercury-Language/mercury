%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__kill.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__kill.

:- interface.

:- import_module int.

:- pred kill(pid_t, int, posix__result, io__state, io__state).
:- mode kill(in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <signal.h>
").

%------------------------------------------------------------------------------%

kill(Pid, Sig, Result) -->
	kill0(Pid, Sig, Res),
	( if { Res \= 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ Result = ok }
	).

:- pred kill0(pid_t, int, int, io__state, io__state).
:- mode kill0(in, in, out, di, uo) is det.

:- pragma c_code(kill0(Pid::in, Sig::in, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury], "{
	Res = kill(Pid, Sig);
	IO = IO0;
}").

%------------------------------------------------------------------------------%

