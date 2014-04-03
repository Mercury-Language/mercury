%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005, 2014 Mission Critical IT.
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: thread.barrier.m
% Original author: Peter Ross
% Stability: low
%
% This module provides a barrier implementation.
%
% A barrier is a position in a program that any thread (of N threads) must
% be suspended at until all the other threads (of N) reach the same
% position.
%
% Barriers are represented by calls to barrier/3 (defined below).  Different
% code locations can belong to the same conceptual barrier using values of
% type barrier.  The same code location can also be used by multiple
% barriers by supplying different values.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module thread.barrier.
:- interface.

:- import_module io.

:- type barrier.

    % init(N, Barrier, !IO)
    %
    % Create a barrier for N threads.
    %
:- pred init(int::in, barrier::out, io::di, io::uo) is det.

    % wait(Barrier, !IO)
    %
    % Indicate that the current thread has reached the barrier.  Throws a
    % software_error/1 exception if this barrier has been used by more than
    % N threads.
    %
:- pred wait(barrier::in, io::di, io::uo) is det.

    % release_barrier(Barrier, !IO)
    %
    % Release all the threads waiting at the barrier regardless
    % of whether or not N threads have arrived at the barrier.
    %
:- pred release(barrier::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

% TODO:
%
%   In some grades it may be possible to improve performance by writing
%   this natively rather than using mvar.
%
%   A semaphore may be better for the "go" signal than an mvar.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module thread.mvar.
:- import_module unit.

%------------------------------------------------------------------------------%

:- type barrier
    --->    barrier(
                % How many threads we are still waiting on?
                b_waiting_for   :: mvar(int),

                % Can we go yet?
                b_go            :: mvar(unit)
            ).

%------------------------------------------------------------------------------%

init(N, barrier(WaitingOn, Go), !IO) :-
    init(WaitingOn, !IO),
    init(Go, !IO),
    put(WaitingOn, N, !IO).

%------------------------------------------------------------------------------%

wait(barrier(WaitingOn, Go), !IO) :-
    take(WaitingOn, N, !IO),
    StillWaitingFor = N - 1,

    ( StillWaitingFor > 0 ->
        % There are still outstanding threads.
        
        % Unlock the counter
        put(WaitingOn, StillWaitingFor, !IO),

        % Wait on the barrier then unlock another thread.
        take(Go, _, !IO),
        put(Go, unit, !IO)
    ; StillWaitingFor = 0 ->
        % The last thread at the barrier, so signal that we can go.
        put(Go, unit, !IO),
        put(WaitingOn, StillWaitingFor, !IO)
    ;
        unexpected($file, $pred,
            "Too many threads called barrier/3 on this barrier.")
    ).

release(barrier(WaitingOn, Go), !IO) :-
    % Allow all the threads at the barrier to go.
    put(Go, unit, !IO),
    take(WaitingOn, N, !IO),
    put(WaitingOn, N - 1, !IO).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
