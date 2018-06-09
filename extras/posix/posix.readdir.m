%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2007, 2010 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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

:- pragma foreign_decl("C", "
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

:- pred readdir0(dir::in, string::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
	readdir0(Dir::in, Entry::out, Result::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
	struct dirent *ent = readdir(Dir);
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

