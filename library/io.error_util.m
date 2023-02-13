% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.error_util.m
%
% This module provides some predicates for dealing with I/O errors.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.error_util.
:- interface.

:- func no_error = system_error.

    % is_error(Error, MessagePrefix, MaybeIOError, !IO):
    % Returns `yes(IOError)' if Error indicates an error (not success).
    %
:- pred is_error(system_error::in, string::in, maybe(io.error)::out,
    io::di, io::uo) is det.

    % is_error_maybe_win32(Error, IsWin32Error, MessagePrefix, MaybeIOError,
    %   !IO):
    % Same as is_error except that IsWin32Error is `yes' if Error originates
    % from a Win32 system error code, `no' otherwise.
    %
:- pred is_error_maybe_win32(system_error::in, bool::in, string::in,
    maybe(io.error)::out, io::di, io::uo) is det.

:- pred make_io_error_from_system_error_impl(io.system_error::in, string::in,
    io.error::out, io::di, io::uo) is det.

:- pred make_io_error_from_windows_error_impl(io.system_error::in, string::in,
    io.error::out, io::di, io::uo) is det.

    % make_io_error_from_maybe_win32_error(Error, IsWin32Error, Prefix,
    %   IOError, !IO):
    % Helper to call either make_io_error_from_system_error_impl or
    % make_io_error_from_windows_error_impl.
    %
:- pred make_io_error_from_maybe_win32_error(system_error::in, bool::in,
    string::in, io.error::out, io::di, io::uo) is det.

    % For use by bitmap.m, and other standard library modules
    % that want to do I/O.
    %
:- pred throw_on_output_error(system_error::in, io::di, io::uo) is det.

:- pred throw_on_close_error(system_error::in, io::di, io::uo) is det.

:- pred throw_on_error(system_error::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type system_error_style
    --->    syserr_errno
    ;       syserr_errno_or_win32
    ;       syserr_exception_object.

:- pragma foreign_export_enum("C", system_error_style/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("C#", system_error_style/0,
    [prefix("ML_"), uppercase]).
:- pragma foreign_export_enum("Java", system_error_style/0,
    [prefix("ML_"), uppercase]).

:- func native_system_error_style = system_error_style.

:- pragma foreign_proc("C",
    native_system_error_style = (SysErrStyle::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_WIN32
    SysErrStyle = ML_SYSERR_ERRNO_OR_WIN32;
#else
    SysErrStyle = ML_SYSERR_ERRNO;
#endif
").
:- pragma foreign_proc("C#",
    native_system_error_style = (SysErrStyle::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SysErrStyle = io__error_util.ML_SYSERR_EXCEPTION_OBJECT;
").
:- pragma foreign_proc("Java",
    native_system_error_style = (SysErrStyle::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SysErrStyle = jmercury.io__error_util.ML_SYSERR_EXCEPTION_OBJECT;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = 0;
").

:- pragma foreign_proc("C#",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = null;
").

:- pragma foreign_proc("Java",
    no_error = (Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Error = null;
").

:- pragma inline(pred(is_error/5)).

is_error(Error, Prefix, MaybeError, !IO) :-
    ( if system_error_is_success(Error) then
        MaybeError = no
    else
        make_io_error_from_system_error_impl(Error, Prefix, IOError, !IO),
        MaybeError = yes(IOError)
    ).

is_error_maybe_win32(Error, IsWin32Error, Prefix, MaybeError, !IO) :-
    ( if system_error_is_success(Error) then
        MaybeError = no
    else
        make_io_error_from_maybe_win32_error(Error, IsWin32Error, Prefix,
            IOError, !IO),
        MaybeError = yes(IOError)
    ).

%---------------------------------------------------------------------------%

make_io_error_from_system_error_impl(Error, Prefix, IOError, !IO) :-
    SysErrStyle = native_system_error_style,
    (
        ( SysErrStyle = syserr_errno
        ; SysErrStyle = syserr_errno_or_win32
        ),
        make_errno_message(Error, Prefix, Msg, !IO),
        IOError = io_error_errno(Msg, Error)
    ;
        SysErrStyle = syserr_exception_object,
        get_exception_object_message(Error, Msg0, !IO),
        ( if Prefix = "" then
            Msg = Msg0
        else
            Msg = Prefix ++ Msg0
        ),
        IOError = io_error_exception_object(Msg, Error)
    ).

make_io_error_from_windows_error_impl(Error, Prefix, IOError, !IO) :-
    SysErrStyle = native_system_error_style,
    (
        SysErrStyle = syserr_errno_or_win32,
        make_win32_error_message(Error, Prefix, Msg, !IO),
        IOError = io_error_win32(Msg, Error)
    ;
        ( SysErrStyle = syserr_errno
        ; SysErrStyle = syserr_exception_object
        ),
        error("io.error_util.make_io_error_from_windows_error: " ++
            "inapplicable platform")
    ).

make_io_error_from_maybe_win32_error(Error, IsWin32Error, Prefix, IOError,
        !IO) :-
    (
        IsWin32Error = yes,
        make_io_error_from_windows_error_impl(Error, Prefix, IOError, !IO)
    ;
        IsWin32Error = no,
        make_io_error_from_system_error_impl(Error, Prefix, IOError, !IO)
    ).

%---------------------------------------------------------------------------%

:- pragma inline(pred(throw_on_output_error/3)).

throw_on_output_error(Error, !IO) :-
    throw_on_error(Error, "error writing to output file: ", !IO).

throw_on_close_error(Error, !IO) :-
    throw_on_error(Error, "error closing file: ", !IO).

:- pragma inline(pred(throw_on_error/4)).

throw_on_error(Error, Prefix, !IO) :-
    % This follows the logic of is_error, but does not construct
    % a MaybeError as an intermediate data structure.
    ( if system_error_is_success(Error) then
        true
    else
        make_io_error_from_system_error_impl(Error, Prefix, IOError, !IO),
        throw(IOError)
    ).

%---------------------------------------------------------------------------%

    % This requires the I/O state because the strerror/strerror_r functions
    % depend on the current locale.
    %
:- pred make_errno_message(io.system_error::in, string::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_errno_message(Errno::in, Prefix::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    char        errbuf[MR_STRERROR_BUF_SIZE];
    const char  *errmsg;
    size_t      errmsg_len;
    size_t      prefix_len;

    prefix_len = strlen(Prefix);
    errmsg = MR_strerror(Errno, errbuf, sizeof(errbuf));
    errmsg_len = strlen(errmsg);
    MR_allocate_aligned_string_msg(Msg, prefix_len + errmsg_len, MR_ALLOC_ID);
    MR_memcpy(Msg, Prefix, prefix_len);
    MR_memcpy(Msg + prefix_len, errmsg, errmsg_len + 1); // include NUL
").

make_errno_message(_, _, _, !IO) :-
    error("io.error_util.make_errno_message: inapplicable back-end").

%---------------------------------------------------------------------------%

    % This requires the I/O state because the FormatMessage call depends
    % on the current locale.
    %
    % XXX is FormatMessage thread-safe? Nothing suggests that it is not.
    %
:- pred make_win32_error_message(io.system_error::in, string::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_win32_error_message(ErrorCode::in, Prefix::in, Msg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
#ifdef MR_WIN32
    char    *errmsg;
    size_t  errmsg_len;
    size_t  prefix_len;

    if (FormatMessage(
            FORMAT_MESSAGE_ALLOCATE_BUFFER
            | FORMAT_MESSAGE_FROM_SYSTEM
            | FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL,
            ErrorCode,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPTSTR) &errmsg,
            0,
            NULL) > 0)
    {
        // Remove trailing CR LF sequence.
        char *cr = strchr(errmsg, '\\r');
        if (cr != NULL) {
            *cr = '\\0';
            errmsg_len = (size_t) (cr - errmsg);
        } else {
            errmsg_len = strlen(errmsg);
        }
        prefix_len = strlen(Prefix);
        MR_allocate_aligned_string_msg(Msg, prefix_len + errmsg_len,
            MR_ALLOC_ID);
        MR_memcpy(Msg, Prefix, prefix_len);
        MR_memcpy(Msg + prefix_len, errmsg, errmsg_len + 1); // include NUL
        LocalFree(errmsg);
    } else {
        Msg = MR_make_string(MR_ALLOC_ID, ""%sSystem error 0x%X"",
            Prefix, ErrorCode);
    }
#else
    MR_fatal_error(""io.error_util.make_win32_error_message: not on Windows"");
#endif
").

make_win32_error_message(_, _, _, !IO) :-
    error("io.error_util.make_win32_error_message: inapplicable back-end").

%---------------------------------------------------------------------------%

    % This requires the I/O state because the exception message may be
    % localised (at least for C#).
    %
:- pred get_exception_object_message(io.system_error::in, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C#",
    get_exception_object_message(Exception::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    if (Exception == null) {
        Msg = ""null"";
    } else {
        Msg = Exception.Message;
    }
").
:- pragma foreign_proc("Java",
    get_exception_object_message(Exception::in, Msg::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_export_body],
"
    if (Exception == null) {
        Msg = ""null"";
    } else {
        Msg = Exception.getMessage();
        if (Msg == null) {
            Msg = ""null"";
        }
    }
").

get_exception_object_message(_, _, !IO) :-
    error("io.error_util.get_exception_object_message: inapplicable back-end").

%---------------------------------------------------------------------------%
:- end_module io.error_util.
%---------------------------------------------------------------------------%
