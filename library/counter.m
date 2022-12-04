%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2005-2006, 2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: counter.m.
% Author: zs.
% Stability: high.
%
% Predicates for dealing with counters, which are mechanisms for allocating
% consecutively numbered integers. The abstraction barrier eliminates the
% possibility of confusion along the lines of "does this counter record
% the next number to be handed out, or the last number that was handed out?".
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module counter.
:- interface.

%---------------------------------------------------------------------------%

:- type counter.

    % init(N) = Counter:
    % init(N, Counter):
    %
    % Return in Counter a counter whose first allocation will be
    % the integer N.
    %
:- func init(int) = counter.
:- pred init(int::in, counter::out) is det.

    % allocate(N, Counter0, Counter) takes a counter, and returns
    %
    % - the next integer to be allocated from that counter, and
    % - the updated state of the counter.
    %
:- pred allocate(int::out, counter::in, counter::out) is det.

%---------------------------------------------------------------------------%

:- type ucounter.

    % uinit(N) = Counter:
    % uinit(N, Counter):
    %
    % Return in Counter a counter whose first allocation will be
    % the unsigned integer N.
    %
:- func uinit(uint) = ucounter.
:- pred uinit(uint::in, ucounter::out) is det.

    % uallocate(N, Counter0, Counter) takes a counter, and returns
    %
    % - the next unsigned integer to be allocated from that counter, and
    % - the updated state of the counter.
    %
:- pred uallocate(uint::out, ucounter::in, ucounter::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.

%---------------------------------------------------------------------------%

:- type counter
    --->    counter(int).

init(N) = counter(N).

init(N, counter(N)).

allocate(N, counter(N), counter(N + 1)).

%---------------------------------------------------------------------------%

:- type ucounter
    --->    ucounter(uint).

uinit(N) = ucounter(N).

uinit(N, ucounter(N)).

uallocate(N, ucounter(N), ucounter(N + 1u)).

%---------------------------------------------------------------------------%
:- end_module counter.
%---------------------------------------------------------------------------%
