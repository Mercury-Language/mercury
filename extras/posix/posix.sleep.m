%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.sleep.m
% Main author: Volker Wysk <post@volker-wysk.de>
%
%-----------------------------------------------------------------------------%

:- module posix.sleep.
:- interface.

:- pred sleep(uint::in, uint::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <unistd.h>").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    sleep(Seconds::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res = sleep(Seconds);
").

%-----------------------------------------------------------------------------%
:- end_module posix.sleep.
%-----------------------------------------------------------------------------%
