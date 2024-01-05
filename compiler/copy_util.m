%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: copy_util.m.
%
% This module provides a predicate for copying files.
%
%-----------------------------------------------------------------------------%

:- module libs.copy_util.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.

:- import_module io.

%-----------------------------------------------------------------------------%

    % copy_file(Globals, ProgressStream, Source, Destination, Succeeded, !IO).
    %
    % XXX A version of this predicate belongs in the standard library.
    %
:- pred copy_file(globals::in, io.text_output_stream::in,
    file_name::in, file_name::in, io.res::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.maybe_util.
:- import_module libs.system_cmds.

:- import_module bool.
:- import_module int.

%-----------------------------------------------------------------------------%

copy_file(Globals, ProgressStream, Source, Destination, Res, !IO) :-
    % Try to use the system's cp command in order to preserve metadata.
    Command = make_install_file_command(Globals, Source, Destination),
    invoke_system_command(Globals, ProgressStream, ProgressStream,
        cmd_verbose, Command, Succeeded, !IO),
    (
        Succeeded = succeeded,
        Res = ok
    ;
        Succeeded = did_not_succeed,
        do_copy_file(Source, Destination, Res, !IO)
    ).

    % XXX TODO: copying the file byte-by-byte is inefficient.
    % If the OS or platform we are on provides a system call for copying files,
    % we should use that in preference to the code below.
    % When the standard library has a byte_array type, the code below should be
    % change the code below to read the file being copied into a byte_array and
    % then write out that array using a single system call.
    %
:- pred do_copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

do_copy_file(Source, Destination, Res, !IO) :-
    io.open_binary_input(Source, SourceRes, !IO),
    (
        SourceRes = ok(SourceStream),
        io.open_binary_output(Destination, DestRes, !IO),
        (
            DestRes = ok(DestStream),
            copy_bytes(SourceStream, DestStream, Res, !IO),
            io.close_binary_input(SourceStream, !IO),
            io.close_binary_output(DestStream, !IO)
        ;
            DestRes = error(Error),
            Res = error(Error)
        )
    ;
        SourceRes = error(Error),
        Res = error(Error)
    ).

:- pred copy_bytes(io.binary_input_stream::in, io.binary_output_stream::in,
    io.res::out, io::di, io::uo) is det.

copy_bytes(Source, Destination, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        copy_bytes_plain(Source, Destination, Res, !IO)
    ;
        ShouldReduce = yes,
        copy_bytes_chunk(Source, Destination, Res, !IO)
    ).

:- pred copy_bytes_plain(io.binary_input_stream::in,
    io.binary_output_stream::in, io.res::out, io::di, io::uo) is det.

copy_bytes_plain(Source, Destination, Res, !IO) :-
    io.read_binary_uint8_unboxed(Source, ByteResult, Byte, !IO),
    (
        ByteResult = ok,
        io.write_binary_uint8(Destination, Byte, !IO),
        copy_bytes_plain(Source, Destination, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok
    ;
        ByteResult = error(Error),
        Res = error(Error)
    ).

:- type copy_chunk_inner_res0
    --->    ccir0_ok
    ;       ccir0_error(io.error)
    ;       ccir0_more.

:- pred copy_bytes_chunk(io.binary_input_stream::in,
    io.binary_output_stream::in, io.res::out, io::di, io::uo) is det.

copy_bytes_chunk(Source, Destination, Res, !IO) :-
    % ChunkSize gives the maximum number of recursive calls we want to allow in
    % the copy_bytes_inner predicate. Without such a limit, the depth of
    % recursion, which depends on the size of the file they read, will cause
    % exhaustion of the det stack in debug grades, since there is no tail
    % recursion in such grades.
    %
    % With this arrangement, the maximum number of stack frames needed to
    % process a file of size N is N/1000 + 1000, the former being the number of
    % frames of copy_bytes_chunk predicate, the latter being the max number of
    % frames of the copy_bytes_inner predicate.
    %
    ChunkSize = 1000,
    copy_bytes_inner(ChunkSize, Source, Destination, InnerRes, !IO),
    (
        InnerRes = ccir0_ok,
        Res = ok
    ;
        InnerRes = ccir0_error(Error),
        Res = error(Error)
    ;
        InnerRes = ccir0_more,
        copy_bytes_chunk(Source, Destination, Res, !IO)
    ).

:- pred copy_bytes_inner(int::in, io.binary_input_stream::in,
    io.binary_output_stream::in, copy_chunk_inner_res0::out,
    io::di, io::uo) is det.

copy_bytes_inner(Left, Source, Destination, Res, !IO) :-
    ( if Left > 0 then
        io.read_binary_uint8_unboxed(Source, ByteResult, Byte, !IO),
        (
            ByteResult = ok,
            io.write_binary_uint8(Destination, Byte, !IO),
            copy_bytes_inner(Left - 1, Source, Destination, Res, !IO)
        ;
            ByteResult = eof,
            Res = ccir0_ok
        ;
            ByteResult = error(Error),
            Res = ccir0_error(Error)
        )
    else
        Res = ccir0_more
    ).

:- pred should_reduce_stack_usage(bool::out) is det.

% For non-C backends.
should_reduce_stack_usage(yes).

:- pragma foreign_proc("C",
    should_reduce_stack_usage(ShouldReduce::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
#ifdef  MR_EXEC_TRACE
    ShouldReduce = MR_YES;
#else
    ShouldReduce = MR_NO;
#endif
").

%-----------------------------------------------------------------------------%
:- end_module copy_util.
%-----------------------------------------------------------------------------%
