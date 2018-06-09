%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2007 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.read.
% Main author: conway@cs.mu.oz.au
%
%-----------------------------------------------------------------------------%

:- module posix.read.
:- interface.

:- import_module bitmap.

:- pred read(fd::in, int::in, posix.result(int)::out,
    bitmap::bitmap_di, bitmap::bitmap_uo, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

read(Fd, ToRead, Result, !Bitmap, !IO) :-
    read0(Fd, ToRead, Read, !Bitmap, !IO),
    ( Read < 0 ->
        errno(Err, !IO),
        Result = error(Err)
    ;
        Result = ok(Read)
    ).

:- pred read0(fd::in, int::in, int::out,
    bitmap::bitmap_di, bitmap::bitmap_uo, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    read0(Fd::in, ToRead::in, Read::out, Bitmap0::bitmap_di, Bitmap::bitmap_uo,
        IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
 "
    do {
        Read = read(Fd, Bitmap0->elements, ToRead);
    } while (Read == -1 && MR_is_eintr(errno));

    Bitmap = Bitmap0;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.read.
%-----------------------------------------------------------------------------%
