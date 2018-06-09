%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006, 2010 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: concurrent_stream.m.
% Main author: conway.
% Stability: medium.
%
% This module implements a simple concurrent data-stream.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module concurrent_stream.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type concurrent_stream(T).

:- type concurrent_stream.result(T)
    --->    end
    ;       error(string)
    ;       ok(T).

    % new(Stream, !IO) creates a new data concurrent_stream `Stream'.
    %
:- pred new(concurrent_stream(T)::out, io::di, io::uo) is det.

    % get(Stream, Result, !IO) blocks until a message appears
    % on the data stream `Stream'. When a message arrives, `Result' is
    % bound to the value of the message.
    %
:- pred get(concurrent_stream(T)::in, concurrent_stream.result(T)::out,
    io::di, io::uo) is det.

    % put(Stream, Thing, !IO) adds `Thing' to the end of the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred put(concurrent_stream(T)::in, T::in, io::di, io::uo) is det.

    % end(Stream, !IO) puts an end-of-stream marker on the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred end(concurrent_stream(T)::in, io::di, io::uo) is det.

    % error(Stream, !IO) puts an error message on the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred error(concurrent_stream(T)::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma require_feature_set([concurrency]).

:- import_module thread.
:- import_module thread.semaphore.

:- import_module queue.
:- import_module require.
:- import_module store.

%-----------------------------------------------------------------------------%

:- type concurrent_stream(T)
    --->    concurrent_stream(
                semaphore,
                io_mutvar(concurrent_stream0(T)),
                semaphore
            ).

:- type concurrent_stream0(T) == queue(concurrent_stream.result(T)).

new(Stream, !IO) :-
    queue.init(Queue),
    store.new_mutvar(Queue, QueueRef, !IO),
    semaphore.init(Lock, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.init(Semaphore, !IO),
    Stream = concurrent_stream(Lock, QueueRef, Semaphore).

put(Stream, Thing, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(ok(Thing), Queue0, Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    signal(Lock, !IO),
    signal(Semaphore, !IO).

end(Stream, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(end, Queue0, Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.signal(Semaphore, !IO).

error(Stream, Msg, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(error(Msg), Queue0, Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.signal(Semaphore, !IO).

get(Stream, Thing, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Semaphore, !IO),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    ( if queue.get(Thing0, Queue0, Queue) then
        Thing = Thing0,
        store.set_mutvar(QueueRef, Queue, !IO)
    else
        error("concurrent_stream.get/4: queue and semaphore out of sync")
    ),
    semaphore.signal(Lock, !IO).

%-----------------------------------------------------------------------------%
:- end_module concurrent_stream.
%-----------------------------------------------------------------------------%
