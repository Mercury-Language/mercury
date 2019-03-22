%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.mkdir.m
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.mkdir.
:- interface.

:- pred mkdir(string::in, mode_t::in, posix.result::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <sys/stat.h>
").

%-----------------------------------------------------------------------------%

mkdir(Path, Mode, Result, !IO) :-
    mkdir0(Path, Mode, Res, !IO),
    ( if Res = 0 then
        Result = ok
    else
        errno(Err, !IO),
        Result = error(Err)
    ).

:- pred mkdir0(string::in, mode_t::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    mkdir0(Path::in, Mode::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res = mkdir(Path, Mode);
").

%-----------------------------------------------------------------------------%
:- end_module posix.mkdir.
%-----------------------------------------------------------------------------%
