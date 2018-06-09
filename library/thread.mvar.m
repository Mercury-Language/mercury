%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2003, 2006-2007, 2011 The University of Melbourne.
% Copyright (C) 2014, 2016, 2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.mvar.m.
% Main author: petdr, fjh.
% Stability: low.
%
% This module provides a Mercury version of Haskell mutable variables.  A
% mutable variable (mvar) is a reference to a mutable location which can
% either contain a value of type T or be empty.
%
% Access to a mvar is thread-safe and can be used to synchronize between
% different threads.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module thread.mvar.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type mvar(T).

    % Create an empty mvar.
    %
:- pred init(mvar(T)::out, io::di, io::uo) is det.

    % Create an mvar with the given initial value.
    %
:- pred init(T::in, mvar(T)::out, io::di, io::uo) is det.

    % Create an empty mvar.
    %
:- impure func impure_init = (mvar(T)::uo) is det.

    % Create an mvar with the given initial value.
    %
:- impure func impure_init(T) = mvar(T).

    % Create an empty mvar.
    %
    % This has been renamed to impure_init.
    %
:- impure func init = (mvar(T)::uo) is det.
:- pragma obsolete(init/0).

    % Take the contents of the mvar out, leaving the mvar empty.
    % If the mvar is empty, block until some thread fills the mvar.
    %
:- pred take(mvar(T)::in, T::out, io::di, io::uo) is det.

    % Take the contents of the mvar out, leaving the mvar empty.
    % Returns immediately with no if the mvar was empty, or yes(X) if
    % the mvar contained X.
    %
:- pred try_take(mvar(T)::in, maybe(T)::out, io::di, io::uo) is det.

    % Place the value of type T into an empty mvar.
    % If the mvar is full then block until it becomes empty.
    %
:- pred put(mvar(T)::in, T::in, io::di, io::uo) is det.

    % Place the value of type T into an empty mvar, returning yes on success.
    % If the mvar is full then return no immediately without blocking.
    %
:- pred try_put(mvar(T)::in, T::in, bool::out, io::di, io::uo) is det.

    % Read the contents of mvar without taking it out.
    % If the mvar is empty then block until it is full.
    % This is similar to mvar.take followed by mvar.put, but atomic.
    %
:- pred read(mvar(T)::in, T::out, io::di, io::uo) is det.

    % Try to read the contents of mvar without taking it out.
    % Returns immediately with no if the mvar was empty, or yes(X) if
    % the mvar contained X.
    %
:- pred try_read(mvar(T)::in, maybe(T)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.
:- import_module thread.semaphore.

%---------------------------------------------------------------------------%

:- type mvar(T)
    --->    mvar(
                semaphore,  % full
                semaphore,  % empty
                mutvar(T)   % data
            ).

%---------------------------------------------------------------------------%

init(Mvar, !IO) :-
    promise_pure (
        impure Mvar = impure_init
    ).

init(Value, Mvar, !IO) :-
    promise_pure (
        impure Mvar = impure_init(Value)
    ).

impure_init = mvar(Full, Empty, Ref) :-
    impure semaphore.impure_init(0, Full),
    impure semaphore.impure_init(1, Empty),     % initially empty
    impure new_mutvar0(Ref).

impure_init(Value) = mvar(Full, Empty, Ref) :-
    impure semaphore.impure_init(1, Full),      % initially full
    impure semaphore.impure_init(0, Empty),
    impure new_mutvar(Value, Ref).

init = Mvar :-
    impure Mvar = impure_init.

%---------------------------------------------------------------------------%

take(mvar(Full, Empty, Ref), Data, !IO) :-
    promise_pure (
        semaphore.wait(Full, !IO),
        impure get_mutvar(Ref, Data),
        % Avoid unwanted memory retention.
        impure clear_mutvar(Ref),
        semaphore.signal(Empty, !IO)
    ).

%---------------------------------------------------------------------------%

try_take(mvar(Full, Empty, Ref), MaybeData, !IO) :-
    promise_pure (
        semaphore.try_wait(Full, Success, !IO),
        (
            Success = yes,
            impure get_mutvar(Ref, Data),
            % Avoid unwanted memory retention.
            impure clear_mutvar(Ref),
            semaphore.signal(Empty, !IO),
            MaybeData = yes(Data)
        ;
            Success = no,
            MaybeData = no
        )
    ).

%---------------------------------------------------------------------------%

put(mvar(Full, Empty, Ref), Data, !IO) :-
    promise_pure (
        semaphore.wait(Empty, !IO),
        impure set_mutvar(Ref, Data),
        semaphore.signal(Full, !IO)
    ).

%---------------------------------------------------------------------------%

try_put(mvar(Full, Empty, Ref), Data, Success, !IO) :-
    promise_pure (
        semaphore.try_wait(Empty, Success, !IO),
        (
            Success = yes,
            impure set_mutvar(Ref, Data),
            semaphore.signal(Full, !IO)
        ;
            Success = no
        )
    ).

%---------------------------------------------------------------------------%

read(mvar(Full, _Empty, Ref), Data, !IO) :-
    promise_pure (
        semaphore.wait(Full, !IO),
        impure get_mutvar(Ref, Data),
        semaphore.signal(Full, !IO)
    ).

%---------------------------------------------------------------------------%

try_read(mvar(Full, _Empty, Ref), MaybeData, !IO) :-
    promise_pure (
        semaphore.try_wait(Full, Success, !IO),
        (
            Success = yes,
            impure get_mutvar(Ref, Data),
            semaphore.signal(Full, !IO),
            MaybeData = yes(Data)
        ;
            Success = no,
            MaybeData = no
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
