%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999, 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: posix.read.
% Main author: conway@cs.mu.oz.au
%
%-----------------------------------------------------------------------------%

:- module posix.read.
:- interface.

:- import_module text.

:- pred read(fd::in, int::in, posix.result(int)::out,
    text::di, text::uo, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <unistd.h>
    #include ""text_header.h""
").

%-----------------------------------------------------------------------------%

read(Fd, ToRead, Result, !Text, !IO) :-
    read0(Fd, ToRead, Read, !Text, !IO),
    ( Read < 0 ->
        errno(Err, !IO),
        Result = error(Err)
    ;
        Result = ok(Read)
    ).

:- pred read0(fd::in, int::in, int::out, text::di, text::uo,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    read0(Fd::in, ToRead::in, Read::out, Text0::di, Text::uo, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
 "
    ME_Text *txtptr;

    txtptr = (ME_Text *) Text0;

    Read = read(Fd, txtptr->data, ToRead);

    Text = Text0;
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.read.
%-----------------------------------------------------------------------------%
