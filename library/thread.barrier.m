%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005, 2014 Mission Critical IT.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
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
% Barriers are represented by calls to barrier/3 (defined below). Different
% code locations can belong to the same conceptual barrier using values of
% type barrier. The same code location can also be used by multiple
% barriers by supplying different values.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
    % Indicate that the current thread has reached the barrier. Throws a
    % software_error/1 exception if this barrier has been used by more than
    % N threads.
    %
:- pred wait(barrier::in, io::di, io::uo) is det.

    % release_barrier(Barrier, !IO)
    %
    % Release all the threads waiting at the barrier regardless of whether
    % or not N threads have arrived at the barrier. This can be called by
    % any thread, it does not have to be a thread that would normally call
    % wait/3.
    %
:- pred release(barrier::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% TODO:
%
%   In some grades it may be possible to improve performance by writing
%   this natively rather than using mvar.
%
%   A semaphore may be better for the "go" signal than an mvar.

:- import_module int.
:- import_module require.
:- import_module thread.mvar.

%---------------------------------------------------------------------------%

:- type barrier
    --->    barrier(
                % How many threads we are still waiting on?
                b_waiting_for   :: mvar(int),

                % Can we go yet?
                b_go            :: mvar(why_can_we_go)
            ).

    % We use this type to say why execution may proceed after reaching the
    % barrier. If it is because the counter reached zero or because
    % release/3 was called.
    %
:- type why_can_we_go
    --->    can_go_normal
    ;       can_go_release_called.

%---------------------------------------------------------------------------%

init(N, barrier(WaitingOn, Go), !IO) :-
    init(WaitingOn, !IO),
    init(Go, !IO),
    put(WaitingOn, N, !IO).

%---------------------------------------------------------------------------%

wait(barrier(WaitingOn, Go), !IO) :-
    take(WaitingOn, N, !IO),
    StillWaitingFor = N - 1,

    ( if StillWaitingFor > 0 then
        % There are still outstanding threads.

        % Unlock the counter.
        put(WaitingOn, StillWaitingFor, !IO),

        % Wait on the barrier then unlock another thread.
        take(Go, WhyGo, !IO),
        put(Go, WhyGo, !IO)
    else if StillWaitingFor = 0 then
        % The last thread at the barrier, so signal that we can go.
        put(Go, can_go_normal, !IO),
        put(WaitingOn, StillWaitingFor, !IO)
    else
        put(WaitingOn, 0, !IO),

        % Go is always updated before WaitingOn, so if this branch is being
        % executed (either because release was called or because the barrier
        % was called excessively) then we know that this call to take will
        % not block, in either of those cases there will always be a value
        % in Go.
        take(Go, WhyGo, !IO),
        put(Go, WhyGo, !IO),
        (
            WhyGo = can_go_normal,
            unexpected($file, $pred,
                "Too many threads called barrier/3 on this barrier.")
        ;
            WhyGo = can_go_release_called
        )
    ).

release(barrier(WaitingOn, Go), !IO) :-
    % Allow all the threads at the barrier to go.
    put(Go, can_go_release_called, !IO),

    % We must set WaitingOn to zero so that the StillWaitingFor = 0 branch
    % above is not executed more than once, if it is it will block when it
    % tries to write a value to Go as Go already has a value. Instead we
    % set it to zero, which means that StillWaitingOn will be -1, we use a
    % special value of can_go_release_called for Go so that this branch does
    % not raise an error.
    %
    % This algorithm has the nice benefit that if release/3 is not
    % considered an alternative to calling barrier, so that a barrier can be
    % canceled by a thread that would not normally call wait/3 itself.
    take(WaitingOn, _N, !IO),
    put(WaitingOn, 0, !IO).

%---------------------------------------------------------------------------%
:- end_module thread.barrier.
%---------------------------------------------------------------------------%
