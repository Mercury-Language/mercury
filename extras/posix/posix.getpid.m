%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.getpid.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.getpid.
:- interface.

:- pred getpid(pid_t::out, io::di, io::uo) is det.

:- pred getppid(pid_t::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    getpid(Pid::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Pid = getpid();
    IO = IO0;
").

:- pragma foreign_proc("C",
    getppid(Pid::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Pid = getppid();
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.getpid.
%-----------------------------------------------------------------------------%
