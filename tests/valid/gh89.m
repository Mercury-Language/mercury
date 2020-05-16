%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for github issue #89.
%
% Any copyright is dedicated to the Public Domain.
% https://creativecommons.org/publicdomain/zero/1.0/
%
% Released by Transnat Games for testing purposes.
%
% Crashes with the following compiler flags:
% mmc --make crash -O 6 --intermodule-optimization --use-grade-subdirs \
%  --grade=hlc.gc.spf.ssdebug
%
% Crashes as:
% Making Mercury/hlc.gc.spf.ssdebug/x86_64-unknown-openbsd6.5/Mercury/cs/crash.c
% Uncaught Mercury exception:
% Software Error: list.m: predicate `list.det_drop'/3: Unexpected: index out of range
% ** Error making `Mercury/hlc.gc.spf.ssdebug/x86_64-unknown-openbsd6.5/Mercury/cs/crash.c'.
%
% The crash requires an optimization level (I'm not sure which, 1 is fine but 6
% is not), intermodule optimizations, grade subdirs, and an ssdebug grade.
%
%---------------------------------------------------------------------------%
%
% The cause of the abort is the the source-to-source debugging transformation
% does not understand how to process the specializations produced by the
% higher-order optimization.  Until it does, we have disabled the use of that
% optimization (and a bunch of others) by default with source-to-source
% debugging.
%
%---------------------------------------------------------------------------%

:- module gh89.
:- interface.

:- use_module array.

:- pred measure(array.array(int)::in, int::out, int::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

:- pred measure(_, int, int, int, int).
:- mode measure(in, in, out, in, out) is det.

measure(_, W, W, H, H).

%------------------------------------------------------------------------------%

measure(Glyphs, W, H) :-
    % There has to be something that actually uses the first argument to this
    % predicate, otherwise the crash doesn't happen.
    array.foldl2(measure, Glyphs, 0, W, 0, H).
