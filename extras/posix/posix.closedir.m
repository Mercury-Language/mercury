%------------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% module: posix__closedir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%
:- module posix__closedir.

:- interface.

:- import_module io.

:- pred closedir(dir, io__state, io__state).
:- mode closedir(in, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- pragma c_header_code("
#include <sys/types.h>
#include <dirent.h>
").

:- pragma c_code(closedir(Dir::in, IO0::di, IO::uo),
	[will_not_call_mercury, thread_safe], "
	closedir((DIR *)Dir);
	IO = IO0;
").

%------------------------------------------------------------------------------%

