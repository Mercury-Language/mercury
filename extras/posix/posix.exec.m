%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2006-2007 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% Module: posix.exec.m.
% Main author: Michael Day <miked@lendtech.com.au>
%
%-----------------------------------------------------------------------------%

:- module posix.exec.
:- interface.

:- import_module list.
:- import_module map.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type argv == list(string).

:- type env == map(string, string).

:- pred exec(string::in, argv::in, env::in, posix.result::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module pair.

:- pragma foreign_decl("C", "#include <unistd.h>").

%-----------------------------------------------------------------------------%

exec(Command, Args, Env, Result, !IO) :-
    exec0(Command,
        array(Args ++ [null]),
        array(list.map(variable, map.to_assoc_list(Env)) ++ [null]),
        !IO
    ),
    errno(Err, !IO),
    Result = error(Err).

:- func variable(pair(string)) = string.

variable(Name - Value) = Name ++ "=" ++ Value.

:- func null = string.
:- pragma foreign_proc("C",
    null = (Null::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Null = NULL;
").

:- pred exec0(string::in,
    array(string)::array_ui, array(string)::array_ui,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    exec0(Command::in, Args::array_ui, Env::array_ui, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int ret;

    do {
        ret = execve(Command,
            ((MR_ArrayType *)Args)->elements, 
            ((MR_ArrayType *)Env)->elements);
    } while (ret == -1 && MR_is_eintr(errno));
    IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module posix.exec.
%-----------------------------------------------------------------------------%
