%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2006-2007 The University of Melbourne.
% Copyright (C) 2014-2015, 2018, 2020-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.channel.m.
% Main author: petdr.
% Stability: low.
%
% A mvar can only contain a single value, a channel on the other hand provides
% unbounded buffering.
%
% For example a program could consist of 2 worker threads and one logging
% thread. The worker threads can place messages into the channel, and they
% will be buffered for processing by the logging thread.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module thread.channel.
:- interface.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type channel(T).

    % Initialise a channel.
    %
:- pred init(channel(T)::out, io::di, io::uo) is det.

    % Put an item at the end of the channel.
    %
:- pred put(channel(T)::in, T::in, io::di, io::uo) is det.

    % Take an item from the start of the channel, block if there is
    % nothing in the channel.
    %
:- pred take(channel(T)::in, T::out, io::di, io::uo) is det.

    % Take an item from the start of the channel.
    % Returns immediately with no if the channel was empty.
    %
:- pred try_take(channel(T)::in, maybe(T)::out, io::di, io::uo) is det.

    % Duplicate a channel. The new channel sees all (and only) the
    % data written to the channel after the `duplicate'/4 call.
    %
:- pred duplicate(channel(T)::in, channel(T)::out, io::di, io::uo)
    is det.

    % Place an item back at the start of the channel.
    %
    % WARNING: a call to channel.untake will deadlock if a call to
    % channel.take is blocked on the same channel.
    %
:- pred untake(channel(T)::in, T::in, io::di, io::uo) is det.
:- pragma obsolete(pred(untake/4)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module thread.mvar.

%---------------------------------------------------------------------------%

:- type channel(T)
    --->    channel(
                mvar(stream(T)),    % Read end.
                mvar(stream(T))     % Write end.
            ).

:- type stream(T) == mvar(item(T)).

:- type item(T)
    --->    item(
                T,          % The current item.
                stream(T)   % The rest of the stream.
            ).

channel.init(channel(Read, Write), !IO) :-
    mvar.init(Read, !IO),
    mvar.init(Write, !IO),
    mvar.init(Hole, !IO),
    mvar.put(Read, Hole, !IO),
    mvar.put(Write, Hole, !IO).

channel.put(channel(_Read, Write), Val, !IO) :-
    mvar.init(NewHole, !IO),
    mvar.take(Write, OldHole, !IO),
    mvar.put(Write, NewHole, !IO),
    mvar.put(OldHole, item(Val, NewHole), !IO).

channel.take(channel(Read, _Write), Val, !IO) :-
    mvar.take(Read, Head, !IO),
    mvar.take(Head, item(Val, NewHead), !IO),
    mvar.put(Read, NewHead, !IO).

channel.try_take(channel(Read, _Write), MaybeVal, !IO) :-
    mvar.take(Read, Head, !IO),
    mvar.try_take(Head, MaybeItem, !IO),
    (
        MaybeItem = yes(item(Val, NewHead)),
        MaybeVal = yes(Val)
    ;
        MaybeItem = no,
        MaybeVal = no,
        NewHead = Head
    ),
    mvar.put(Read, NewHead, !IO).

channel.duplicate(channel(_Read, Write), channel(NewRead, Write), !IO) :-
    mvar.init(NewRead, !IO),
    mvar.take(Write, Hole, !IO),
    mvar.put(Write, Hole, !IO),
    mvar.put(NewRead, Hole, !IO).

channel.untake(channel(Read, _Write), Val, !IO) :-
    mvar.init(NewHead, !IO),
    mvar.take(Read, Head, !IO),
    mvar.put(NewHead, item(Val, Head), !IO),
    mvar.put(Read, NewHead, !IO).

%---------------------------------------------------------------------------%
:- end_module thread.channel.
%---------------------------------------------------------------------------%
