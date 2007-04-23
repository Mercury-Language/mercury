%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: posix.write.
% Main author: conway@cs.mu.oz.au
%
%-----------------------------------------------------------------------------%

:- module posix.write.
:- interface.

:- import_module text.

%-----------------------------------------------------------------------------%

:- pred write(fd::in, int::in, text::in, posix.result(int)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
    #include ""text_header.h""
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

:- pred write0(fd::in, int::in, text::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    write0(Fd::in, ToWrite::in, Text::in, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    ME_Text *txtptr;

    txtptr = (ME_Text *) Text;
    Res = write(Fd, txtptr->data, ToWrite);
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.write.
%-----------------------------------------------------------------------------%
