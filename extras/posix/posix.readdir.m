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

:- type readdir_result
    --->    ok(string)
    ;       eof
    ;       error(posix.error).

:- pred readdir(dir::in, readdir_result::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include ""mercury_string.h""
    #include <sys/types.h>
    #include <dirent.h>
").

readdir(Dir, Result, !IO) :-
    readdir0(Dir, Entry, RawResult, !IO),
    (
        RawResult = readdir_ok,
        Result = ok(Entry)
    ;
        RawResult = readdir_eof,
        Result = eof
    ;
        RawResult = readdir_error,
        errno(Error, !IO),
        Result = error(Error)
    ).

:- type raw_readdir_result
    --->    readdir_ok
    ;       readdir_eof
    ;       readdir_error.

:- pragma foreign_export_enum("C", raw_readdir_result/0,
    [prefix("MPOSIX_"), uppercase]).

:- pred readdir0(dir::in, string::out, raw_readdir_result::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    readdir0(Dir::in, Entry::out, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    struct dirent *ent;
    errno = 0;
    ent = readdir(Dir);
    if (ent != NULL) {
        Result = MPOSIX_READDIR_OK;
        MR_make_aligned_string_copy(Entry, ent->d_name);
    } else {
        if (errno == 0) {
            Result = MPOSIX_READDIR_EOF;
        } else {
            Result = MPOSIX_READDIR_ERROR;
        }
        Entry = MR_make_string_const("""");
    }
").

%------------------------------------------------------------------------------%
:- end_module posix.readdir.
%------------------------------------------------------------------------------%
