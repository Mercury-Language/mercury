%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2007 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.write.
% Main author: conway@cs.mu.oz.au
%
%-----------------------------------------------------------------------------%

:- module posix.write.
:- interface.

:- import_module bitmap.

%-----------------------------------------------------------------------------%

:- pred write(fd::in, int::in, bitmap::in, posix.result(int)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
").

%-----------------------------------------------------------------------------%

write(Fd, ToWrite, Text, Result, !IO) :-
    write0(Fd, ToWrite, Text, Res, !IO),
    ( Res < 0 ->
        errno(Err, !IO),
        Result = error(Err)
    ;
        Result = ok(Res)
    ).

:- pred write0(fd::in, int::in, bitmap::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    write0(Fd::in, ToWrite::in, Bitmap::in, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    do {
        Res = write(Fd, Bitmap->elements, ToWrite);
    } while (Res == -1 && MR_is_eintr(errno));
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.write.
%-----------------------------------------------------------------------------%
