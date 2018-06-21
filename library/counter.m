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

    % init(N, Counter) returns a counter whose first allocation will be the
    % integer N.
    %
:- pred init(int::in, counter::out) is det.

    % A function version of init/2.
    %
:- func init(int) = counter.

    % allocate(N, Counter0, Counter) takes a counter, and returns (a) the next
    % integer to be allocated from that counter, and (b) the updated state of
    % the counter.
    %
:- pred allocate(int::out, counter::in, counter::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- type counter
    --->    counter(int).

init(N, counter.init(N)).

init(N) = counter(N).

allocate(N, counter(N), counter(N + 1)).

%---------------------------------------------------------------------------%
:- end_module counter.
%---------------------------------------------------------------------------%
