%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.system_rng.m.
% Main author: Julien Fischer
%
% This module provides access to the system random number generator. This
% generator is a platform specific cryptographically secure random number
% generator that is seeded from the OS entropy pool.
%
% The intended use of this generator is to provide small amounts of
% high-quality random material suitable, for example, for seeding other
% random number generators. It is not intended for generating large amounts
% of random material.
%
% On the C# backend, the system RNG is an instance of the
% System.Security.Cryptography.RandomNumberGenerator class that uses the
% default cryptographic random number generator implementation.
%
% On the Java backend, the system RNG is an instance of the
% java.security.SecureRandom class using the default algorithm random number
% algorithm.
%
% NYI for the C backends.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module random.system_rng.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

    % A handle through which the system RNG can be accessed.
    %
    % In general, it is *not* safe to share system RNG handles across
    % multiple threads. In circumstances where multiple threads require access
    % to the system RNG, each thread should open its own handle.
    %
:- type system_rng.

:- instance urandom(system_rng, io).

%---------------------------------------------------------------------------%

    % True if this platform provides a system RNG.
    %
:- pred have_system_rng is semidet.

    % open_system_rng(MaybeHandle, !IO):
    %
    % Returns a handle through which the system RNG can be accessed.
    %
:- pred open_system_rng(maybe_error(system_rng)::out, io::di, io::uo)
    is det.

    % close_system_rng(Handle, !IO):
    %
    % Release the system RNG handle along with any OS resources associated
    % with it. Throws an exception if an error occurs.
    %
:- pred close_system_rng(system_rng::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Generate a uniformly distributed unsigned integer of 8, 16, 32 or
    % 64 bits respectively using the system RNG.
    %
:- pred generate_uint8(system_rng::in, uint8::out, io::di, io::uo) is det.
:- pred generate_uint16(system_rng::in, uint16::out, io::di, io::uo) is det.
:- pred generate_uint32(system_rng::in, uint32::out, io::di, io::uo) is det.
:- pred generate_uint64(system_rng::in, uint64::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%


:- pragma foreign_decl("C", "

#include \"mercury_conf_param.h\"
#include \"mercury_memory.h\"
#include \"mercury_misc.h\"
#include \"mercury_runtime_util.h\"
#include \"mercury_std.h\"
#include \"mercury_string.h\"
#include \"mercury_types.h\"
#include \"mercury_windows.h\"

#if defined(MR_HAVE_UNISTD_H)
    #include <unistd.h>
#endif
#if defined(MR_HAVE_FCNTL_H)
    #include <fcntl.h>
#endif
#if defined(MR_HAVE_SYS_STAT_H)
    #include <sys/stat.h>
#endif

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>

// The following macros define if the system random number exists on this
// system and, if so, how it is accessed.
//
// Only one of the following must be defined.
//
// ML_SYSRAND_IMPL_ARC4RANDOM (NYI)
//    the system RNG is implemented by calling the arc4random() family of
//    functions. Note: this for when arc4random() is provided by libc (as on
//    macOS and the BSDs), not for when it is provided as a separate library
//    (e.g. libbsd on Linux).
//
// ML_SYSRAND_IMPL_RAND_S (NYI)
//    the system RNG is implemented by calling the rand_s() function
//    (Windows only).
//
// ML_SYSRAND_IMPL_GETRANDOM (NYI)
//    the system RNG is implemented by calling getrandom() (sufficiently
//    recent Linux kernels only).
//
// ML_SYSRAND_IMPL_URANDOM
//     the system RNG is implemented by reading from /dev/urandom.
//
// ML_SYSRAND_IMPL_NONE
//     there is no system RNG is not available on this platform.

#if defined(__linux__) || defined(MR_SOLARIS) || defined(_AIX)
    #define ML_SYSRAND_IMPL_URANDOM
#else
    #define ML_SYSRAND_IMPL_NONE
#endif

#if defined(ML_SYSRAND_IMPL_URANDOM)
    struct ML_SystemRandomHandle_Struct {
        int ML_srh_fd;
    };
    typedef struct ML_SystemRandomHandle_Struct *ML_SystemRandomHandle;
#else
    typedef MR_Unsigned ML_SystemRandomHandle;
#endif

// When succeeded is MR_TRUE, returns a handle through which the system
// RNG can be accessed; err_msg will point to the empty string in this case.
// When succeeded is MR_FALSE, the value return is not a valid handle and
// err_msg will point ot a string (on the Mercury heap) describing why a handle
// for the system RNG could not be acquired.
//
extern ML_SystemRandomHandle ML_random_open(MR_Bool *succeeded,
    MR_String *err_msg);

// Attempt to close the handle to the system RNG.
// Returns MR_TRUE on success with err_msg pointing to the empty string.
// Returns MR_FALSE on failure with err_msg pointing to a string (on the
// Mercury heap) that describes whey the handle could not be closed.
//
extern MR_Bool ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg);


// Fill buffer with len random bytes generated by the system RNG.
// Returns MR_TRUE if len bytes were generated; err_msg will point to the empty
// string.
// Returns MR_FALSE if the system RNG has been unable to generate len bytes.
// In this case the contents of buffer are indeterminate and err_msg will point
// to a string (on the Mercury heap) that describes the problem.
//
extern MR_Bool ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg);
").

:- pragma foreign_type("C", system_rng,
    "ML_SystemRandomHandle", [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", system_rng,
    "System.Security.Cryptography.RandomNumberGenerator").
:- pragma foreign_type("Java", system_rng,
    "java.security.SecureRandom").

%---------------------------------------------------------------------------%

:- instance urandom(system_rng, io) where [
    ( generate_uint8(Handle, U, !IO) :-
        system_rng.generate_uint8(Handle, U, !IO)
    ),
    ( generate_uint16(Handle, U, !IO) :-
        system_rng.generate_uint16(Handle, U, !IO)
    ),
    ( generate_uint32(Handle, U, !IO) :-
        system_rng.generate_uint32(Handle, U, !IO)
    ),
    ( generate_uint64(Handle, U, !IO) :-
        system_rng.generate_uint64(Handle, U, !IO)
    )
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    have_system_rng,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(ML_SYSRAND_IMPL_NONE)
    SUCCESS_INDICATOR = MR_FALSE;
#else
    SUCCESS_INDICATOR = MR_TRUE;
#endif
").

:- pragma foreign_proc("C#",
    have_system_rng,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    have_system_rng,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

%---------------------------------------------------------------------------%

open_system_rng(Result, !IO) :-
    do_open_system_rng(Handle, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes,
        Result = ok(Handle)
    ;
        IsOk = no,
        Result = error(ErrorMsg)
    ).

:- pred do_open_system_rng(system_rng::out, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_open_system_rng(Handle::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Handle = ML_random_open(&IsOk, &ErrorMsg);
").

:- pragma foreign_proc("C#",
    do_open_system_rng(Handle::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Handle =
        System.Security.Cryptography.RandomNumberGenerator.Create();
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_open_system_rng(Handle::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Handle = new java.security.SecureRandom();
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

close_system_rng(Handle, !IO) :-
    do_close_system_rng(Handle, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw_system_rng_error($pred, ErrorMsg)
    ).

:- pred do_close_system_rng(system_rng::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_close_system_rng(Handle::in, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IsOk = ML_random_close(Handle, &ErrorMsg);
").

:- pragma foreign_proc("C#",
    do_close_system_rng(Handle::in, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Handle
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_close_system_rng(Handle::in, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Handle
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

generate_uint8(Handle, U8, !IO) :-
    do_generate_uint8(Handle, U8, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw_system_rng_error($pred, ErrorMsg)
    ).

:- pred do_generate_uint8(system_rng::in, uint8::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_generate_uint8(Handle::in, U8::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    union {
        uint8_t n;
        unsigned char buffer[1];
    } u;
    IsOk = ML_random_generate_bytes(Handle, u.buffer, sizeof(u.buffer),
        &ErrorMsg);
    U8 = u.n;
").

:- pragma foreign_proc("C#",
    do_generate_uint8(Handle::in, U8::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[1];
    Handle.GetBytes(bytes);
    U8 = bytes[0];
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_generate_uint8(Handle::in, U8::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[1];
    Handle.nextBytes(bytes);
    U8 = bytes[0];
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

generate_uint16(Handle, U16, !IO) :-
    do_generate_uint16(Handle, U16, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw_system_rng_error($pred, ErrorMsg)
    ).

:- pred do_generate_uint16(system_rng::in, uint16::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_generate_uint16(Handle::in, U16::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    union {
        uint16_t n;
        unsigned char buffer[2];
    } u;
    IsOk = ML_random_generate_bytes(Handle, u.buffer, sizeof(u.buffer),
        &ErrorMsg);
    U16 = u.n;
").

:- pragma foreign_proc("C#",
    do_generate_uint16(Handle::in, U16::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[2];
    Handle.GetBytes(bytes);
    U16 = (ushort) (bytes[1] << 8 | (bytes[0] & 0x00ff));
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_generate_uint16(Handle::in, U16::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[2];
    Handle.nextBytes(bytes);
    U16 = (short) (bytes[0] << java.lang.Byte.SIZE | (bytes[1] & 0x00ff));
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

generate_uint32(Handle, U32, !IO) :-
    do_generate_uint32(Handle, U32, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw_system_rng_error($pred, ErrorMsg)
    ).

:- pred do_generate_uint32(system_rng::in, uint32::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_generate_uint32(Handle::in, U32::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    union {
        uint32_t n;
        unsigned char buffer[4];
    } u;
    IsOk = ML_random_generate_bytes(Handle, u.buffer, sizeof(u.buffer),
        &ErrorMsg);
    U32 = u.n;
").

:- pragma foreign_proc("C#",
    do_generate_uint32(Handle::in, U32::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[4];
    Handle.GetBytes(bytes);
    U32 = (uint) (bytes[3] << 24 | bytes[2] << 16 | bytes[1] << 8 | bytes[0]);
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_generate_uint32(Handle::in, U32::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = Handle.nextInt();
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

generate_uint64(Handle, U64, !IO) :-
    do_generate_uint64(Handle, U64, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw_system_rng_error($pred, ErrorMsg)
    ).

:- pred do_generate_uint64(system_rng::in, uint64::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_generate_uint64(Handle::in, U64::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    union {
        uint64_t n;
        unsigned char buffer[8];
    } u;
    IsOk = ML_random_generate_bytes(Handle, u.buffer, sizeof(u.buffer),
        &ErrorMsg);
    U64 = u.n;
").

:- pragma foreign_proc("C#",
    do_generate_uint64(Handle::in, U64::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[8];
    Handle.GetBytes(bytes);
    U64 = (ulong) (
        (ulong) bytes[7] << 56 |
        (ulong) bytes[6] << 48 |
        (ulong) bytes[5] << 40 |
        (ulong) bytes[4] << 32 |
        (ulong) bytes[3] << 24 |
        (ulong) bytes[2] << 16 |
        (ulong) bytes[1] << 8  |
        (ulong) bytes[0]);
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_generate_uint64(Handle::in, U64::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = Handle.nextLong();
    IsOk = bool.YES;
    ErrorMsg = \"\";
").

%---------------------------------------------------------------------------%

:- pred throw_system_rng_error(string::in, string::in) is erroneous.

throw_system_rng_error(Pred, Msg) :-
    string.format("%s: %s", [s(Pred), s(Msg)], Error),
    throw(software_error(Error)).

%---------------------------------------------------------------------------%

:- pragma foreign_code("C", "

#if defined(ML_SYSRAND_IMPL_URANDOM)

ML_SystemRandomHandle
ML_random_open(MR_Bool *succeeded, MR_String *err_msg)
{
    int fd;
    char errbuf[MR_STRERROR_BUF_SIZE];
    const char *errno_msg;
    ML_SystemRandomHandle handle;

    do {
        fd = open(\"/dev/urandom\", O_RDONLY);
    } while (fd == -1 && MR_is_eintr(errno));

    if (fd == -1) {
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_save_transient_hp();
        MR_make_aligned_string_copy(*err_msg, errno_msg);
        MR_restore_transient_hp();
        *succeeded = MR_NO;
        handle = NULL;
    } else {
        handle = MR_GC_NEW(struct ML_SystemRandomHandle_Struct);
        handle->ML_srh_fd = fd;
        *err_msg = MR_make_string_const(\"\");
        *succeeded = MR_YES;
    }

    return handle;
}

MR_Bool
ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg)
{
    char errbuf[MR_STRERROR_BUF_SIZE];
    const char *errno_msg;

    if (close(handle->ML_srh_fd) == -1) {
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_save_transient_hp();
        MR_make_aligned_string_copy(*err_msg, errno_msg);
        MR_restore_transient_hp();
        return MR_NO;
    } else {
        handle->ML_srh_fd = -1;
        *err_msg = MR_make_string_const(\"\");
        return MR_YES;
    }
}

MR_Bool
ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg)
{
    int i;
    char errbuf[MR_STRERROR_BUF_SIZE];
    const char *errno_msg;

    for (i = 0; i < len; ) {
        size_t to_read = len - i;
        ssize_t bytes_read = read(handle->ML_srh_fd, buffer, to_read);
        if (bytes_read == -1) {
            if (MR_is_eintr(errno)) {
                continue;
            } else {
                errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
                MR_save_transient_hp();
                MR_make_aligned_string_copy(*err_msg, errno_msg);
                MR_restore_transient_hp();
                return MR_FALSE;
            }
        }
        i += bytes_read;
    }

    *err_msg = MR_make_string_const(\"\");
    return MR_TRUE;
}

#else // ML_SYSRAND_IMPL_NONE

ML_SystemRandomHandle
ML_random_open(MR_Bool *succeeded, MR_String *err_msg)
{
    succeeded = MR_FALSE;
    *err_msg = MR_make_string_const(\"No system RNG available\");
    return 0; // Dummy value.
}

MR_Bool
ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg)
{
    MR_fatal_error(\"ML_random_close - no system RNG available.\");
}

MR_Bool
ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg)
{
    MR_fatal_error(\"ML_random_generate_bytes - no system RNG available.\");
}

#endif

").

%---------------------------------------------------------------------------%
:- end_module random.system_rng.
%---------------------------------------------------------------------------%
