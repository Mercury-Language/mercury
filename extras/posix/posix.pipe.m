%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__pipe.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__pipe.

:- interface.

:- pred pipe(posix__result({fd, fd}), io__state, io__state).
:- mode pipe(out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

pipe(Result) -->
	pipe0(Reading, Writing, Res),
	( if { Res \= 0 } then
		errno(Err),
		{ Result = error(Err) }
	else
		{ Result = ok({Reading, Writing}) }
	).

:- pred pipe0(fd, fd, int, io__state, io__state).
:- mode pipe0(out, out, out, di, uo) is det.

:- pragma c_code(pipe0(R::out, W::out, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury], "{
	int filedes[2];
	Res = pipe(filedes);
	R = filedes[0];
	W = filedes[1];
	IO = IO0;
}").

%------------------------------------------------------------------------------%

