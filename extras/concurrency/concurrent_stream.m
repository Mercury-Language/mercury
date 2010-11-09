%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
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
    semaphore.new(Lock, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.new(Semaphore, !IO),
    Stream = concurrent_stream(Lock, QueueRef, Semaphore).

put(Stream, Thing, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(Queue0, ok(Thing), Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    signal(Lock, !IO),
    signal(Semaphore, !IO).

end(Stream, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(Queue0, end, Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.signal(Semaphore, !IO).

error(Stream, Msg, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    queue.put(Queue0, error(Msg), Queue),
    store.set_mutvar(QueueRef, Queue, !IO),
    semaphore.signal(Lock, !IO),
    semaphore.signal(Semaphore, !IO).

get(Stream, Thing, !IO) :-
    Stream = concurrent_stream(Lock, QueueRef, Semaphore),
    semaphore.wait(Semaphore, !IO),
    semaphore.wait(Lock, !IO),
    store.get_mutvar(QueueRef, Queue0, !IO),
    ( queue.get(Queue0, Thing0, Queue) ->
        Thing = Thing0,
        store.set_mutvar(QueueRef, Queue, !IO)
    ;
        error("concurrent_stream.get/4: queue and semaphore out of sync")
    ),
    semaphore.signal(Lock, !IO).

%-----------------------------------------------------------------------------%
:- end_module concurrent_stream.
%-----------------------------------------------------------------------------%
