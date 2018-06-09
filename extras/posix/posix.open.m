%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2001, 2007 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.open.
% Main author: conway@cs.mu.oz.au
%
% This module provides and interface to the open function and its relatives.
%
%-----------------------------------------------------------------------------%

:- module posix.open.
:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type oflag
    --->    rdonly
    ;       wronly
    ;       rdwr
    ;       creat
    ;       excl
    ;       noctty
    ;       trunc
    ;       append
    ;       ndelay
    ;       sync.

:- pred open(string::in, list(oflag)::in, posix.result(fd)::out,
    io::di, io::uo) is det.

:- pred open(string::in, list(oflag)::in, mode_t::in, posix.result(fd)::out,
    io::di, io::uo) is det.

:- pred creat(string::in, mode_t::in, posix.result(fd)::out,
    io::di, io::uo) is det.

:- pred close(fd::in, posix.result::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
    #include <fcntl.h>
").

%-----------------------------------------------------------------------------%

open(PathName, FlagList, Result, !IO) :-
    open0(PathName, oflags(FlagList), FdNo, !IO),
    ( FdNo < 0 ->
        errno(Error, !IO),
        Result = error(Error)
    ;
        Result = ok(fd(FdNo))
    ).

:- pred open0(string::in, int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    open0(PathName::in, Flags::in, FileDes::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
 "
    FileDes = open(PathName, Flags);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

open(PathName, FlagList, Mode, Result, !IO) :-
    open0(PathName, oflags(FlagList), Mode, FdNo, !IO),
    ( FdNo < 0 ->
        errno(Error, !IO),
        Result = error(Error)
    ;
        Result = ok(fd(FdNo))
    ).

:- pred open0(string::in, int::in, mode_t::in, int::out, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    open0(PathName::in, Flags::in, Mode::in, FileDes::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    FileDes = open(PathName, Flags, Mode);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

creat(PathName, Mode, Result, !IO) :-
    creat0(PathName, Mode, FdNo, !IO),
    ( FdNo < 0 ->
        errno(Error, !IO),
        Result = error(Error)
    ;
        Result = ok(fd(FdNo))
    ).

:- pred creat0(string::in, mode_t::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    creat0(PathName::in, Mode::in, FileDes::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    FileDes = creat(PathName, Mode);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

close(fd(FdNo), Result, !IO) :-
    close0(FdNo, Res0, !IO),
    ( Res0 < 0 ->
        errno(Error, !IO),
        Result = error(Error)
    ;
        Result = ok
    ).

:- pred close0(int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    close0(Fd::in, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    do {
        Res = close(Fd);
    } while (Res == -1 && MR_is_eintr(errno));
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func oflags(list(oflag)) = int.

oflags(FlagList) = Or :-
    orflags(FlagList, 0, Or).

:- pred orflags(list(oflag)::in, int::in, int::out) is det.

orflags([], !Or).
orflags([F | Fs], !Or) :-
    !:Or = !.Or \/ oflagval(F),
    orflags(Fs, !Or).

:- func oflagval(oflag) = int.

:- pragma foreign_proc("C",
    oflagval(F::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = F;
").

:- pragma foreign_enum("C", oflag/0, [
    rdonly  - "O_RDONLY",
    wronly  - "O_WRONLY",
    rdwr    - "O_RDWR",
    creat   - "O_CREAT",
    excl    - "O_EXCL",
    noctty  - "O_NOCTTY",
    trunc   - "O_TRUNC",
    append  - "O_APPEND",
    ndelay  - "O_NDELAY",
    sync    - "O_SYNC"
]).

%-----------------------------------------------------------------------------%
:- end_module posix.open.
%-----------------------------------------------------------------------------%
