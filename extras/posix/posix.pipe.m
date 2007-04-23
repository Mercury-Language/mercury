%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
    pipe0(R::out, W::out, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int filedes[2];
    Res = pipe(filedes);
    R = filedes[0];
    W = filedes[1];
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.pipe.
%-----------------------------------------------------------------------------%
