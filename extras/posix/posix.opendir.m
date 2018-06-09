%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.opendir.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.opendir.
:- interface.

:- import_module io.
:- import_module string.

:- pred opendir(string::in, posix.result(dir)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include <sys/types.h>
#include <dirent.h>
").

opendir(Path, Result, !IO) :-
    opendir0(Path, Dir, Res, !IO),
    ( if Res = 0 then
        Result = ok(Dir)
    else
        errno(Err, !IO),
        Result = error(Err)
    ).                  

:- pred opendir0(string::in, dir::out, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    opendir0(Path::in, Dir::out, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Dir = opendir(Path);
    Res = (Dir == NULL);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.opendir.
%-----------------------------------------------------------------------------%
