%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: binfile.m
% Main author: Mark Brown
%
% "Random" number generator that reads numbers from a binary file.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module binfile.
:- interface.

:- import_module io.
:- import_module random.

%---------------------------------------------------------------------------%

:- type binfile.
:- instance urandom(binfile, io).

    % Open a binfile generator from a filename. This should be closed
    % when no longer needed.
    %
:- pred open(string, io.res(binfile), io, io).
:- mode open(in, out, di, uo) is det.

    % Close a binfile generator.
    %
:- pred close(binfile, io, io).
:- mode close(in, di, uo) is det.

%---------------------------------------------------------------------------%

    % Generate an unsigned integer of 8, 16, 32 or 64 bits, reespectively.
    % This reads the required number of bytes from the file and interprets
    % them as an unsigned, big-endian integer.
    %
    % Throws an exception if the end-of-file is reached.
    %
:- pred generate_uint8(binfile::in, uint8::out, io::di, io::uo) is det.
:- pred generate_uint16(binfile::in, uint16::out, io::di, io::uo) is det.
:- pred generate_uint32(binfile::in, uint32::out, io::di, io::uo) is det.
:- pred generate_uint64(binfile::in, uint64::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type binfile
    --->    binfile(binary_input_stream).

:- instance urandom(binfile, io) where [
    pred(generate_uint8/4) is binfile.generate_uint8,
    pred(generate_uint16/4) is binfile.generate_uint16,
    pred(generate_uint32/4) is binfile.generate_uint32,
    pred(generate_uint64/4) is binfile.generate_uint64
].

%---------------------------------------------------------------------------%

open(Filename, Res, !IO) :-
    io.open_binary_input(Filename, Res0, !IO),
    (
        Res0 = ok(Stream),
        Res = ok(binfile(Stream))
    ;
        Res0 = error(E),
        Res = error(E)
    ).

close(binfile(Stream), !IO) :-
    io.close_binary_input(Stream, !IO).

%---------------------------------------------------------------------------%

generate_uint8(binfile(Stream), N, !IO) :-
    io.read_binary_uint8(Stream, Res, !IO),
    (
        Res = ok(N)
    ;
        Res = eof,
        unexpected($pred, "end of file")
    ;
        Res = error(E),
        unexpected($pred, io.error_message(E))
    ).

generate_uint16(binfile(Stream), N, !IO) :-
    io.read_binary_uint16_be(Stream, Res, !IO),
    handle_res(Res, N).

generate_uint32(binfile(Stream), N, !IO) :-
    io.read_binary_uint32_be(Stream, Res, !IO),
    handle_res(Res, N).

generate_uint64(binfile(Stream), N, !IO) :-
    io.read_binary_uint64_be(Stream, Res, !IO),
    handle_res(Res, N).

:- pred handle_res(maybe_incomplete_result(T)::in, T::out) is det.

handle_res(Res, N) :-
    (
        Res = ok(N)
    ;
        ( Res = eof
        ; Res = incomplete(_)
        ),
        unexpected($pred, "end of file")
    ;
        Res = error(E),
        unexpected($pred, io.error_message(E))
    ).

%---------------------------------------------------------------------------%
