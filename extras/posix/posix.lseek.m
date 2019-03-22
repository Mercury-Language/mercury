%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 1999, 2005, 2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%------------------------------------------------------------------------------%
%
% Module: posix.lseek.m
% Main author: conway@cs.mu.oz.au
%
%------------------------------------------------------------------------------%

:- module posix.lseek.
:- interface.

:- type whence
    --->    set
    ;       cur
    ;       end.

:- pred lseek(fd::in, int::in, lseek.whence::in, posix.result(int)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <sys/types.h>
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

lseek(Fd, Offset, Whence, Result, !IO) :-
    lseek0(Fd, Offset, Whence, Res, !IO),
    ( if Res < 0 then
        errno(Err, !IO),
        Result = error(Err)
    else
        Result = ok(Res)
    ).

:- pred lseek0(fd::in, int::in, lseek.whence::in, int::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    lseek0(Fd::in, Offset::in, Whence::in, Res::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res = lseek(Fd, Offset, Whence);
").

:- pragma foreign_enum("C", lseek.whence/0, [
    set - "SEEK_SET",
    cur - "SEEK_CUR",
    end - "SEEK_END"
]).

%-----------------------------------------------------------------------------%
:- end_module posix.lseek.
%-----------------------------------------------------------------------------%
