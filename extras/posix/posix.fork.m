%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: posix.fork.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.fork.
:- interface.

:- type whoami
    --->    child
    ;       parent(pid_t).

:- pred fork(posix.result(whoami)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

fork(Result, !IO) :-
    fork0(Pid, !IO),
    ( if Pid < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else if Pid = 0 then
        Result = ok(child)
    else
        Result = ok(parent(pid(Pid)))
    ).

:- pred fork0(int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    fork0(Pid::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Pid = fork();
    IO = IO0;
").

%------------------------------------------------------------------------------%
:- end_module posix.fork.
%------------------------------------------------------------------------------%
