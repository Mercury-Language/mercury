%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__rmdir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__rmdir.

:- interface.

:- import_module string.

:- pred rmdir(string, posix__result, io__state, io__state).
:- mode rmdir(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma c_header_code("
	#include <unistd.h>
").

%------------------------------------------------------------------------------%

rmdir(Path, Result) -->
	rmdir0(Path, Res),
	( if { Res = 0 } then
	    { Result = ok }
	else
	    errno(Err),
	    { Result = error(Err) }
	).				    

:- pred rmdir0(string, int, io__state, io__state).
:- mode rmdir0(in, out, di, uo) is det.

:- pragma c_code(rmdir0(Path::in, Res::out, IO0::di, IO::uo),
	    [will_not_call_mercury, thread_safe], "
	Res = rmdir(Path);
	IO = IO0;
").
		
%------------------------------------------------------------------------------%

