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

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", system_rng,
    "java.security.SecureRandom").
:- pragma foreign_type("C#", system_rng,
    "System.Security.Cryptography.RandomNumberGenerator").

:- type system_rng
    --->    system_rng.

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

have_system_rng :-
    semidet_false.

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

do_open_system_rng(Handle, IsOk, ErrorMsg, !IO) :-
    Handle = system_rng,
    IsOk = no,
    ErrorMsg = "No system RNG available".

%---------------------------------------------------------------------------%

close_system_rng(Handle, !IO) :-
    do_close_system_rng(Handle, IsOk, ErrorMsg, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(software_error(ErrorMsg))
    ).

:- pred do_close_system_rng(system_rng::in, bool::out, string::out,
    io::di, io::uo) is det.

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

do_close_system_rng(_, _, _, _, _) :-
    private_builtin.sorry("No system RNG available").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    generate_uint8(Handle::in, U8::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[1];
    Handle.GetBytes(bytes);
    U8 = bytes[0];
").

:- pragma foreign_proc("Java",
    generate_uint8(Handle::in, U8::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[1];
    Handle.nextBytes(bytes);
    U8 = bytes[0];
").

generate_uint8(_, _, !IO) :-
    private_builtin.sorry("No system RNG available").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    generate_uint16(Handle::in, U16::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[2];
    Handle.GetBytes(bytes);
    U16 = (ushort) System.BitConverter.ToInt16(bytes);
").

:- pragma foreign_proc("Java",
    generate_uint16(Handle::in, U16::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    byte[] bytes = new byte[2];
    Handle.nextBytes(bytes);
    U16 = (short) (bytes[0] << java.lang.Byte.SIZE | (bytes[1] & 0x00ff));
").

generate_uint16(_, _, !IO) :-
    private_builtin.sorry("No system RNG available").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    generate_uint32(Handle::in, U32::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[4];
    Handle.GetBytes(bytes);
    U32 = (uint) System.BitConverter.ToInt32(bytes);
").

:- pragma foreign_proc("Java",
    generate_uint32(Handle::in, U32::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U32 = Handle.nextInt();
").
generate_uint32(_, _, !IO) :-
    private_builtin.sorry("No system RNG available").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    generate_uint64(Handle::in, U64::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    byte[] bytes = new byte[8];
    Handle.GetBytes(bytes);
    U64 = (ulong) System.BitConverter.ToInt64(bytes);
").

:- pragma foreign_proc("Java",
    generate_uint64(Handle::in, U64::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U64 = Handle.nextLong();
").

generate_uint64(_, _, !IO) :-
    private_builtin.sorry("No system RNG available").

%---------------------------------------------------------------------------%
:- end_module random.system_rng.
%---------------------------------------------------------------------------%
