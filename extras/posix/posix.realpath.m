%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%------------------------------------------------------------------------------%
%
% Module: posix.readdir.m.
% Main author: Volker Wysk
%
%------------------------------------------------------------------------------%

:- module posix.realpath.
:- interface.

:- import_module io.
:- import_module string.

    % Return the canonicalized absolute pathname. See realpath(3).
    %
:- pred realpath(string::in, posix.result(string)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

realpath(Path, Result, !IO) :-
    realpath0(Path, IsOk, RealPath, !IO),
    (
        IsOk = yes,
        Result = ok(RealPath)
    ;
        IsOk = no,
        errno(Errno, !IO),
        Result = error(Errno)
    ).

:- pragma foreign_decl("C", "
    #include <stdlib.h>
").

:- pred realpath0(string::in, bool::out, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    realpath0(Path::in, IsOk::out, Result::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    char *actualpath;
    actualpath = realpath(Path, NULL);
    if (actualpath != NULL) {
        IsOk = MR_YES;
        MR_make_aligned_string_copy(Result, actualpath);
        free(actualpath);
    } else {
        IsOk = MR_NO;
        Result = MR_make_string_const("""");
    }
").

%----------------------------------------------------------------------------%
:- end_module posix.realpath.
%----------------------------------------------------------------------------%
