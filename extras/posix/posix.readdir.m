%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__readdir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__readdir.

:- interface.

:- import_module io, string.

:- pred readdir(dir, posix__result(string), io__state, io__state).
:- mode readdir(in, out, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
#include <sys/types.h>
#include <dirent.h>
").

readdir(Dir, Result) -->
	readdir0(Dir, Entry, Res),
	( if { Res = 0 } then
		{ Result = ok(Entry) }
	else
		errno(Err),
		{ Result = error(Err) }
	).				    

:- pred readdir0(dir, string, int, io__state, io__state).
:- mode readdir0(in, out, out, di, uo) is det.

:- pragma c_code(readdir0(Dir::in, Entry::out, Result::out, IO0::di, IO::uo),
		[will_not_call_mercury, thread_safe], "
	struct dirent *ent = readdir((DIR *)Dir);
	if (ent != NULL) {
		MR_make_aligned_string_copy(Entry, ent->d_name);
		Result = 0;
	} else {
		Entry = NULL;
		Result = 1;
	}
	IO = IO0;
").

%------------------------------------------------------------------------------%

