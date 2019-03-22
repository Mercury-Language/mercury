%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.pipe.
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.pipe.
:- interface.

%-----------------------------------------------------------------------------%

:- pred pipe(posix.result({fd, fd})::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

pipe(Result, !IO) :-
    pipe0(Reading, Writing, Res, !IO),
    ( if Res \= 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok({Reading, Writing})
    ).

:- pred pipe0(fd::out, fd::out, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    pipe0(R::out, W::out, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int filedes[2];
    Res = pipe(filedes);
    R = filedes[0];
    W = filedes[1];
").

%-----------------------------------------------------------------------------%
:- end_module posix.pipe.
%-----------------------------------------------------------------------------%
