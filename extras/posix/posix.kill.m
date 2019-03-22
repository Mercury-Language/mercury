%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.kill.
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.kill.
:- interface.

:- import_module int.

:- pred kill(pid_t::in, int::in, posix.result::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <signal.h>
").

%-----------------------------------------------------------------------------%

kill(Pid, Sig, Result, !IO) :-
    kill0(Pid, Sig, Res, !IO),
    ( if Res \= 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok
    ).

:- pred kill0(pid_t::in, int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    kill0(Pid::in, Sig::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    Res = kill(Pid, Sig);
").

%-----------------------------------------------------------------------------%
:- end_module posix.kill.
%-----------------------------------------------------------------------------%
