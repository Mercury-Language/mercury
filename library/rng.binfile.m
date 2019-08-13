%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rng.binfile.m
% Main author: Mark Brown
%
% "Random" number generator that reads numbers from a binary file.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rng.binfile.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- type binfile.
:- instance urng(binfile, io).

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

    % Generate a number between 0 and max_uint64. This reads 8 bytes
    % at a time from the binfile and interprets them as an unsigned,
    % big-endian integer.
    %
    % Throws an exception if the end-of-file is reached.
    %
:- pred rand(binfile, uint64, io, io).
:- mode rand(in, out, di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type binfile
    --->    binfile(binary_input_stream).

:- instance urng(binfile, io) where [
    pred(urandom/4) is rand,
    ( urandom_max(_) = uint64.max_uint64 )
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

rand(binfile(Stream), N, !IO) :-
    io.read_binary_uint64_be(Stream, Res, !IO),
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
