%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%----------------------------------------------------------------------------%
%
% Module: posix.chdir.m
% Main author: Volker Wysk <post@volker-wysk.de>
%
%----------------------------------------------------------------------------%

:- module posix.chdir.
:- interface.

:- import_module io.
:- import_module string.

    % Change the working directory. See chdir(2).
    %
:- pred chdir(string::in, posix.result::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

chdir(Path, Res, !IO) :-
    chdir0(Path, Res0, !IO),
    ( if Res0 \= 0 then
        errno(Errno, !IO),
        Res = error(Errno)
    else
        Res = ok
    ).

:- pragma foreign_decl("C", "#include <unistd.h>").

:- pred chdir0(string::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    chdir0(Path::in, Res0::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    Res0 = chdir(Path);
").

%----------------------------------------------------------------------------%
:- end_module posix.chdir.
%----------------------------------------------------------------------------%
