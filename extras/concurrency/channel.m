%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% File: channel.m.
% Main author: petdr.
% Stability: low.
%
% A mvar can only contain a single value, a channel on the other hand provides
% unbounded buffering.
%
% For example a program could consist of 2 worker threads and one logging
% thread.  The worker threads can place messages into the channel, and they
% will be buffered for processing by the logging thread.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module channel.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type channel(T).

    % Initialise a channel.
    %
:- pred channel.init(channel(T)::out, io::di, io::uo) is det.

    % Put an item at the end of the channel.
    %
:- pred channel.put(channel(T)::in, T::in, io::di, io::uo) is det.

    % Take an item from the start of the channel, block if there is
    % nothing in the channel.
    %
:- pred channel.take(channel(T)::in, T::out, io::di, io::uo) is det.

    % Duplicate a channel.  The new channel sees all (and only) the
    % data written to the channel after the channel.duplicate call.
    % 
:- pred channel.duplicate(channel(T)::in, channel(T)::out, io::di, io::uo)
    is det.

    % Place an item back at the start of the channel.
    %
:- pred channel.untake(channel(T)::in, T::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mvar.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
