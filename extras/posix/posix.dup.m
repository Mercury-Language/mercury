%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2006-2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.dup.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.dup.
:- interface.

:- pred dup(fd::in, posix.result(fd)::out, io::di, io::uo) is det.

:- pred dup2(fd::in, fd::in, posix.result(fd)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
").

%------------------------------------------------------------------------------%

dup(Fd, Result, !IO) :-
    dup0(Fd, fd(NewFd), !IO),
    ( if NewFd < 0  then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok(fd(NewFd))
    ).

:- pred dup0(fd::in, fd::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    dup0(OldFd::in, NewFd::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    do {
        NewFd = dup(OldFd);
    } while (NewFd == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%

dup2(OldFd, NewFd, Result, !IO) :-
    dup2_2(OldFd, NewFd, fd(Ret), !IO),
    ( if Ret < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok(fd(Ret))
    ).

:- pred dup2_2(fd::in, fd::in, fd::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dup2_2(OldFd::in, NewFd::in, Ret::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    do {
        Ret = dup2(OldFd, NewFd);
    } while (Ret == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%
:- end_module posix.dup.
%-----------------------------------------------------------------------------%
