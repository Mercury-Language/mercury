%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__opendir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__opendir.

:- interface.

:- import_module io, string.

:- pred opendir(string, posix__result(dir), io__state, io__state).
:- mode opendir(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
#include <sys/types.h>
#include <dirent.h>
").

opendir(Path, Result) -->
	opendir0(Path, Dir, Res),
	( if { Res = 0 } then
		{ Result = ok(Dir) }
	else
		errno(Err),
		{ Result = error(Err) }
	).				    

:- pred opendir0(string, dir, int, io__state, io__state).
:- mode opendir0(in, out, out, di, uo) is det.

:- pragma c_code(opendir0(Path::in, Dir::out, Res::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	Dir = (MR_Word) opendir(Path);
	Res = (Dir == 0);
	IO = IO0;
").

%------------------------------------------------------------------------------%

