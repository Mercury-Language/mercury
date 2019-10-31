%-------------------------------------_--------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%----------------------------------------------------------------------------%
%
% Module: posix.strerror
% Main author: Volker Wysk <post@volker-wysk.de>
%
%----------------------------------------------------------------------------%

:- module posix.strerror.
:- interface.

:- import_module string.

:- pred strerror(posix.error::in, string::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

strerror(Err, Msg, !IO) :-
    error_to_cerrno(Err, CErrno),
    strerror0(CErrno, Msg, !IO).

:- pragma foreign_decl("C", "#include \"mercury_runtime_util.h\"").

:- pred strerror0(int::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    strerror0(Errno::in, Txt::out, _IO1::di, _IO2::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io],
"
    char buf[200];
    MR_strerror(Errno, buf, 200);
    MR_make_aligned_string_copy(Txt, buf);
").

%----------------------------------------------------------------------------%
:- end_module posix.strerror.
%----------------------------------------------------------------------------%
