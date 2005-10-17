%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
%-----------------------------------------------------------------------------%

:- module counter.

:- interface.

:- type counter.

    % counter_init(N, Counter) returns a counter whose first allocation
    % will be the integer N.
    %
:- pred counter__init(int::in, counter::out) is det.

    % A function version of counter__init/2.
    %
:- func counter__init(int) = counter.

    % counter__allocate(N, Counter0, Counter) takes a counter, and
    % returns (a) the next integer to be allocated from that counter,
    % and (b) the updated state of the counter.
    %
:- pred counter__allocate(int::out, counter::in, counter::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- type counter
    --->    counter(int).

counter__init(N) = counter(N).

counter__init(N, counter__init(N)).

counter__allocate(N, counter(N), counter(N + 1)).
