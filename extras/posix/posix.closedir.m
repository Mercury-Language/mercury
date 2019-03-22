%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.closedir.m.
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.closedir.
:- interface.

:- import_module io.

:- pred closedir(dir::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include <sys/types.h>
#include <dirent.h>
").

:- pragma foreign_proc("C",
    closedir(Dir::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    closedir(Dir);
").

%-----------------------------------------------------------------------------%
:- end_module posix.closedir.
%-----------------------------------------------------------------------------%
