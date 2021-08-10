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
% On the C backends, the system RNG depends on the operating system.
% For macOS, Cygwin, OpenBSD, NetBSD and versions of FreeBSD from 12 onwards,
% we use the arc4random() family of functions.
% For Windows, we use the rand_s() function.
% For Linux, AIX, Solaris and versions of FreeBSD before 12, we read randomness
% from /dev/urandom; on these system each open system RNG handle will require
% an open file descriptor.
% On other operating systems the system RNG is not available; on these systems
% attempting to open a system RNG handle will throw an exception.
%
% On the C# backend, the system RNG is an instance of the
% System.Security.Cryptography.RandomNumberGenerator class that uses the
% default cryptographic random number generator implementation.
%
% On the Java backend, the system RNG is an instance of the
% java.security.SecureRandom class using the default algorithm random number
% algorithm.
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

#if defined(MR_HAVE_SYS_PARAM_H)
    #include <sys/param.h>
#endif

#if defined(MR_MAC_OSX)
    #include <AvailabilityMacros.h>
#endif

// The following macros define if the system random number exists on this
// system and, if so, how it is accessed.
//
// Only one of the following must be defined.
//
// ML_SYSRAND_IMPL_ARC4RANDOM
//    the system RNG is implemented by calling the arc4random() family of
//    functions. Note: this for when arc4random() is provided by libc (as on
//    macOS and the BSDs), not for when it is provided as a separate library
//    (e.g. libbsd on Linux).
//
//    This should only be enabled on systems where arc4random() uses a secure
//    PRNG, such as ChaCha20; it should _not_ be enabled on systems where
//    arc4random() still uses RC4.
//
// ML_SYSRAND_IMPL_RAND_S
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
#elif defined(__OpenBSD__) || defined(__NetBSD__) || defined(MR_CYGWIN)
    #define ML_SYSRAND_IMPL_ARC4RANDOM
#elif __FreeBSD__ >= 12
    // arc4random() on FreeBSD used RC4 until version 12.
    #define ML_SYSRAND_IMPL_ARC4RANDOM
#elif defined(__FreeBSD__)
    #define ML_SYSRAND_IMPL_URANDOM
#elif defined(MR_MAC_OSX)
   // arc4random() on macOS used RC4 until version 10.12.
   // XXX this will be unnecessary when we stop supporting versions
   // of macOS before 10.12.
   #if defined(MAC_OS_X_VERSION_10_12) && \\
        MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_12
      #define ML_SYSRAND_IMPL_ARC4RANDOM
   #else
      #define ML_SYSRAND_IMPL_URANDOM
   #endif
#elif defined(MR_WIN32)
    #define ML_SYSRAND_IMPL_RAND_S
#else
    #define ML_SYSRAND_IMPL_NONE
#endif

struct ML_SystemRandomHandle_Struct {
    #if defined(ML_SYSRAND_IMPL_URANDOM)
        int ML_srh_fd;
    #else
        MR_Bool ML_srh_is_open;
    #endif
};
typedef struct ML_SystemRandomHandle_Struct *ML_SystemRandomHandle;

").

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", system_rng,
    "ML_SystemRandomHandle", [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", system_rng,
    "random__system_rng.ML_SystemRandomHandle").
:- pragma foreign_type("Java", system_rng,
    "random__system_rng.ML_SystemRandomHandle").

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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Handle = ML_random_open(&IsOk, &ErrorMsg);
").

:- pragma foreign_proc("C#",
    do_open_system_rng(Handle::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Handle = new random__system_rng.ML_SystemRandomHandle();
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").

:- pragma foreign_proc("Java",
    do_open_system_rng(Handle::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Handle = new random__system_rng.ML_SystemRandomHandle();
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
    [will_not_call_mercury, promise_pure, thread_safe,
        may_not_duplicate],
"
    IsOk = ML_random_close(Handle, &ErrorMsg);
").

:- pragma foreign_proc("C#",
    do_close_system_rng(Handle::in, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        IsOk = mr_bool.NO;
        ErrorMsg = \"system RNG handle already closed\";
    } else {
        Handle.close();
        IsOk = mr_bool.YES;
        ErrorMsg = \"\";
    }
").

:- pragma foreign_proc("Java",
    do_close_system_rng(Handle::in, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        IsOk = bool.NO;
        ErrorMsg = \"system RNG handle already closed\";
    } else {
        Handle.close();
        IsOk = bool.YES;
        ErrorMsg = \"\";
    }
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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
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
    if (Handle.isClosed()) {
        U8 = 0;
        IsOk = mr_bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U8 = Handle.getByte();
        IsOk = mr_bool.NO;
        ErrorMsg = \"\";
    }
").

:- pragma foreign_proc("Java",
    do_generate_uint8(Handle::in, U8::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        U8 = 0;
        IsOk = bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U8 = Handle.getByte();
        IsOk = bool.YES;
        ErrorMsg = \"\";
    }
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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
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
    if (Handle.isClosed()) {
        U16 = 0;
        IsOk = mr_bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U16 = Handle.getUShort();
        IsOk = mr_bool.YES;
        ErrorMsg = \"\";
    }
").

:- pragma foreign_proc("Java",
    do_generate_uint16(Handle::in, U16::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        U16 = 0;
        IsOk = bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U16 = Handle.getShort();
        IsOk = bool.YES;
        ErrorMsg = \"\";
    }
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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
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
    if (Handle.isClosed()) {
        U32 = 0;
        IsOk = mr_bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U32 = Handle.getUInt();
        IsOk = mr_bool.YES;
        ErrorMsg = \"\";
    }
").

:- pragma foreign_proc("Java",
    do_generate_uint32(Handle::in, U32::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        U32 = 0;
        IsOk = bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U32 = Handle.getInt();
        IsOk = bool.YES;
        ErrorMsg = \"\";
    }
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
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
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
    if (Handle.isClosed()) {
        U64 = 0;
        IsOk = mr_bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U64 = Handle.getULong();
        IsOk = mr_bool.YES;
        ErrorMsg = \"\";
    }
").

:- pragma foreign_proc("Java",
    do_generate_uint64(Handle::in, U64::out, IsOk::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Handle.isClosed()) {
        U64 = 0;
        IsOk = bool.NO;
        ErrorMsg = \"system RNG handle is closed\";
    } else {
        U64 = Handle.getLong();
        IsOk = bool.YES;
        ErrorMsg = \"\";
    }
").

%---------------------------------------------------------------------------%

:- pred throw_system_rng_error(string::in, string::in) is erroneous.

throw_system_rng_error(Pred, Msg) :-
    string.format("%s: %s", [s(Pred), s(Msg)], Error),
    throw(software_error(Error)).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "

#include \"mercury_memory.h\"          // For MR_GC_NEW.
#include \"mercury_misc.h\"            // For MR_fatal_error.
#include \"mercury_runtime_util.h\"    // For MR_strerror.
#include \"mercury_regs.h\"            // For MR_{save,restore}_transient_hp
#include \"mercury_reg_workarounds.h\" // For MR_memcpy.
#include \"mercury_std.h\"             // For MR_TRUE, MR_FALSE etc.
#include \"mercury_string.h\"          // For MR_make_aligned_string_copy etc.
#include \"mercury_types.h\"           // For MR_Bool.
#include \"mercury_windows.h\"

#if defined(ML_SYSRAND_IMPL_URANDOM)
  #if defined(MR_HAVE_UNISTD_H)
    #include <unistd.h>
  #endif
  #if defined(MR_HAVE_FCNTL_H)
    #include <fcntl.h>
  #endif
  #if defined(MR_HAVE_SYS_STAT_H)
    #include <sys/stat.h>
  #endif
#endif

#include <errno.h>
#include <stdint.h>

// On Windows we need to ensure that _CRT_RAND_S is defined before stdlib.h
// is included but this must be done in runtime/mercury_std.h.
#include <stdlib.h>

// When succeeded is MR_TRUE, returns a handle through which the system
// RNG can be accessed; err_msg will point to the empty string in this case.
// When succeeded is MR_FALSE, the value return is not a valid handle and
// err_msg will point to a string (on the Mercury heap) describing why a handle
// for the system RNG could not be acquired.
//
static ML_SystemRandomHandle ML_random_open(MR_Bool *succeeded,
    MR_String *err_msg);

// Attempt to close the handle to the system RNG.
// Returns MR_TRUE on success with err_msg pointing to the empty string.
// Returns MR_FALSE on failure with err_msg pointing to a string (on the
// Mercury heap) that describes why the handle could not be closed.
//
static MR_Bool ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg);

// Fill buffer with len random bytes generated by the system RNG.
// Returns MR_TRUE if len bytes were generated; err_msg will point to the empty
// string.
// Returns MR_FALSE if the system RNG has been unable to generate len bytes.
// In this case the contents of buffer are indeterminate and err_msg will point
// to a string (on the Mercury heap) that describes the problem.
//
static MR_Bool ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg);
").

:- pragma foreign_code("C", "

#if defined(ML_SYSRAND_IMPL_URANDOM)

static ML_SystemRandomHandle
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

static MR_Bool
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

static MR_Bool
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

#elif defined(ML_SYSRAND_IMPL_ARC4RANDOM)

static ML_SystemRandomHandle
ML_random_open(MR_Bool *succeeded, MR_String *err_msg)
{
    ML_SystemRandomHandle handle =
        MR_GC_NEW(struct ML_SystemRandomHandle_Struct);
    handle->ML_srh_is_open = MR_TRUE;
    *succeeded = MR_TRUE;
    *err_msg = MR_make_string_const(\"\");
    return handle;
}

static MR_Bool
ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg)
{
    if (handle->ML_srh_is_open) {
        handle->ML_srh_is_open = MR_FALSE;
        *err_msg = MR_make_string_const(\"\");
        return MR_TRUE;
    } else {
        *err_msg =
            MR_make_string_const(\"system RNG handle is already closed\");
        return MR_FALSE;
    }
}

static MR_Bool
ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg)
{
    if (handle->ML_srh_is_open) {
        arc4random_buf(buffer, len);
        *err_msg = MR_make_string_const(\"\");
        return MR_TRUE;
    } else {
        *err_msg = MR_make_string_const(\"system RNG handle is closed\");
        return MR_FALSE;
    }
}

#elif defined(ML_SYSRAND_IMPL_RAND_S)

static ML_SystemRandomHandle
ML_random_open(MR_Bool *succeeded, MR_String *err_msg)
{
    ML_SystemRandomHandle handle =
        MR_GC_NEW(struct ML_SystemRandomHandle_Struct);
    handle->ML_srh_is_open = MR_TRUE;
    *succeeded = MR_TRUE;
    *err_msg = MR_make_string_const(\"\");
    return handle;
}

static MR_Bool
ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg)
{
    if (handle->ML_srh_is_open) {
        handle->ML_srh_is_open = MR_FALSE;
        *err_msg = MR_make_string_const(\"\");
        return MR_TRUE;
    } else {
        *err_msg =
            MR_make_string_const(\"system RNG handle is already closed\");
        return MR_FALSE;
    }
}

static MR_Bool
ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg)
{
    int err;
    unsigned int n;
    size_t num_to_read = len;

    if (!handle->ML_srh_is_open) {
        *err_msg = MR_make_string_const(\"system RNG handle is closed\");
        return MR_FALSE;
    }

    while (num_to_read > 0) {
        if (num_to_read < 4) {
            err = rand_s(&n);
            if (err != 0) {
                goto rand_s_failure_handler;
            }
            MR_memcpy(buffer, (unsigned char *) &n, num_to_read);
            break;
        } else {
            err = rand_s((unsigned int *) buffer);
            if (err != 0) {
                goto rand_s_failure_handler;
            }
            num_to_read -= 4;
            buffer += 4;
        }
    }

    *err_msg = MR_make_string_const(\"\");
    return MR_TRUE;

rand_s_failure_handler:

    *err_msg = MR_make_string_const(\"rand_s failed\");
    return MR_FALSE;
}

#else // ML_SYSRAND_IMPL_NONE

static ML_SystemRandomHandle
ML_random_open(MR_Bool *succeeded, MR_String *err_msg)
{
    succeeded = MR_FALSE;
    *err_msg = MR_make_string_const(\"No system RNG available\");
    return 0; // Dummy value.
}

static MR_Bool
ML_random_close(ML_SystemRandomHandle handle, MR_String *err_msg)
{
    MR_fatal_error(\"ML_random_close - no system RNG available.\");
}

static MR_Bool
ML_random_generate_bytes(ML_SystemRandomHandle handle,
    unsigned char *buffer, size_t len, MR_String *err_msg)
{
    MR_fatal_error(\"ML_random_generate_bytes - no system RNG available.\");
}

#endif

").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "

    public class ML_SystemRandomHandle {

        private System.Security.Cryptography.RandomNumberGenerator handle;

        public ML_SystemRandomHandle() {
            handle =
                System.Security.Cryptography.RandomNumberGenerator.Create();
        }

        public void close() {
            handle = null;
        }

        public bool isClosed() {
            return (handle == null);
        }

        public byte getByte() {
            byte[] bytes = new byte[1];
            handle.GetBytes(bytes);
            return bytes[0];
        }

        public ushort getUShort() {
            byte[] bytes = new byte[2];
            handle.GetBytes(bytes);
            return (ushort) (bytes[1] << 8 | (bytes[0] & 0x00ff));
        }

        public uint getUInt() {
            byte[] bytes = new byte[4];
            handle.GetBytes(bytes);
            return (uint) (
                bytes[3] << 24 |
                bytes[2] << 16 |
                bytes[1] << 8  |
                bytes[0]);
        }

        public ulong getULong() {
            byte[] bytes = new byte[8];
            handle.GetBytes(bytes);
            return (ulong) (
                (ulong) bytes[7] << 56 |
                (ulong) bytes[6] << 48 |
                (ulong) bytes[5] << 40 |
                (ulong) bytes[4] << 32 |
                (ulong) bytes[3] << 24 |
                (ulong) bytes[2] << 16 |
                (ulong) bytes[1] << 8  |
                (ulong) bytes[0]);
        }
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Java", "

    public static class ML_SystemRandomHandle {

        private java.security.SecureRandom handle;

        public ML_SystemRandomHandle() {
            handle = new java.security.SecureRandom();
        }

        public void close() {
            handle = null;
        }

        public boolean isClosed() {
            return (handle == null);
        }

        public byte getByte() {
            byte[] bytes = new byte[1];
            handle.nextBytes(bytes);
            return bytes[0];
        }

        public short getShort() {
            byte[] bytes = new byte[2];
            handle.nextBytes(bytes);
            return (short)
                (bytes[0] << java.lang.Byte.SIZE | (bytes[1] & 0x00ff));
        }

        public int getInt() {
            return handle.nextInt();
        }

        public long getLong() {
            return handle.nextLong();
        }
    }
").


%---------------------------------------------------------------------------%
:- end_module random.system_rng.
%---------------------------------------------------------------------------%
