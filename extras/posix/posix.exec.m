%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2006-2007 The University of Melbourne.
% Copyright (C) 2018-2019 The Mercury team.
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

:- pred execvp(string::in, argv::in, posix.result::out,
    io::di, io::uo) is det.

:- pred execv(string::in, argv::in, posix.result::out,
    io::di, io::uo) is det.

:- pred execve(string::in, argv::in, env::in, posix.result::out,
    io::di, io::uo) is det.

:- pred exec(string::in, argv::in, env::in, posix.result::out,
    io::di, io::uo) is det.

% This is a GNU extension, and not included:
% :- pred execvpe(string::in, argv::in, env::in, posix.result::out,
%   io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.

:- pragma foreign_decl("C", "#include <unistd.h>").

%-----------------------------------------------------------------------------%

:- type string_array.

:- pragma foreign_type("C", string_array, "char **").

:- pred make_string_array(list(string)::in, string_array::out) is det.

:- pragma foreign_proc("C",
    make_string_array(List::in, Array::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_Word ptr;
    size_t  len;
    size_t  i;

    len = 0;
    ptr = List;
    while (! MR_list_is_empty(ptr)) {
        len++;
        ptr = MR_list_tail(ptr);
    }

    Array = MR_GC_NEW_ARRAY_ATTRIB(char *, len + 1, MR_ALLOC_ID);

    i = 0;
    ptr = List;
    while (! MR_list_is_empty(ptr)) {
        Array[i] = (char *) MR_list_head(ptr);
        ptr = MR_list_tail(ptr);
        i++;
    }

    Array[i] = NULL;
").

:- func variable(pair(string)) = string.

variable(Name - Value) = Name ++ "=" ++ Value.

%-----------------------------------------------------------------------------%
%
% int execve(const char *filename, char *const argv[], char *const envp[]);
%

execve(Command, Args, Env, Result, !IO) :-
    make_string_array(Args, ArgsArray),
    make_string_array(map(variable, map.to_assoc_list(Env)), EnvArray),
    execve0(Command, ArgsArray, EnvArray, !IO),
    errno(Err, !IO),
    Result = error(Err).

:- pred execve0(string::in, string_array::in, string_array::in, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    execve0(Command::in, ArgsArray::in, EnvArray::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int ret;

    do {
        ret = execve(Command, ArgsArray, EnvArray);
    } while (ret == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%
%
% int execvp(const char *file, char *const argv[]);
%

execvp(Command, Args, Result, !IO) :-
    make_string_array(Args, ArgsArray),
    execvp0(Command, ArgsArray, !IO),
    errno(Err, !IO),
    Result = error(Err).

:- pred execvp0(string::in, string_array::in, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    execvp0(Command::in, ArgsArray::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int ret;

    do {
        ret = execvp(Command, ArgsArray);
    } while (ret == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%
%
% int execv(const char *path, char *const argv[]);
%

execv(Command, Args, Result, !IO) :-
    make_string_array(Args, ArgsArray),
    execv0(Command, ArgsArray, !IO),
    errno(Err, !IO),
    Result = error(Err).

:- pred execv0(string::in, string_array::in, io::di, io::uo)
    is det.
:- pragma foreign_proc("C",
    execv0(Command::in, ArgsArray::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    int ret;

    do {
        ret = execv(Command, ArgsArray);
    } while (ret == -1 && MR_is_eintr(errno));
").

%-----------------------------------------------------------------------------%
%
% Synonym for execve, for backwards compatibility

exec(Command, Args, Env, Result, !IO) :-
    execve(Command, Args, Env, Result, !IO).

%-----------------------------------------------------------------------------%
:- end_module posix.exec.
%-----------------------------------------------------------------------------%
