%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__mkdir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__mkdir.

:- interface.

:- import_module string.

:- pred mkdir(string, mode, posix__result, io__state, io__state).
:- mode mkdir(in, in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <sys/types.h>
	#include <sys/stat.h>
").

%------------------------------------------------------------------------------%

mkdir(Path, Mode, Result) -->
	mkdir0(Path, Mode, Res),
	( if { Res = 0 } then
	    { Result = ok }
	else
	    errno(Err),
	    { Result = error(Err) }
	).				    

:- pred mkdir0(string, mode, int, io__state, io__state).
:- mode mkdir0(in, in, out, di, uo) is det.

:- pragma c_code(mkdir0(Path::in, Mode::in, Res::out, IO0::di, IO::uo),
	    [will_not_call_mercury, thread_safe], "
	Res = mkdir(Path, Mode);
	IO = IO0;
").
		
%------------------------------------------------------------------------------%

