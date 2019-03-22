%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 2001, 2007, 2010 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%------------------------------------------------------------------------------%
%
% module: posix.readdir.m
% main author: Michael Day <miked@lendtech.com.au>
%
%------------------------------------------------------------------------------%

:- module posix.readdir.
:- interface.

:- import_module io.
:- import_module string.

:- pred readdir(dir::in, posix.result(string)::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <dirent.h>
").

readdir(Dir, Result, !IO) :-
    readdir0(Dir, Entry, Res, !IO),
    ( if Res = 0 then
        Result = ok(Entry)
    else
        errno(Err, !IO),
        Result = error(Err)
    ).

:- pred readdir0(dir::in, string::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    readdir0(Dir::in, Entry::out, Result::out, _IO0::di, _IO::uo),
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
").

%------------------------------------------------------------------------------%
:- end_module posix.readdir.
%------------------------------------------------------------------------------%
