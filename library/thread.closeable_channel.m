%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.closeable_channel.m.
% Main author: wangp.
% Stability: low.
%
% Unbounded closeable channels.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module thread.closeable_channel.
:- interface.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- type closeable_channel(T).

    % Initialise a channel.
    %
:- pred init(closeable_channel(T)::out, io::di, io::uo) is det.

    % Put an item at the end of the channel.
    % Returns `yes' if successful, or `no' if the channel is closed.
    %
:- pred put(closeable_channel(T)::in, T::in, bool::out, io::di, io::uo)
    is det.

    % Close a channel. Once a channel is closed, no more items can be added
    % to it. Closing a channel that is already closed has no effect.
    %
:- pred close(closeable_channel(T)::in, io::di, io::uo) is det.

:- type take_result(T)
    --->    ok(T)
    ;       closed.

    % Take an item from the start of the channel, blocking until an item is
    % available or until the channel is closed. Returns `ok(Item)' if `Item'
    % was taken, or `closed' if the channel is closed.
    %
:- pred take(closeable_channel(T)::in, take_result(T)::out, io::di, io::uo)
    is det.

:- type try_take_result(T)
    --->    ok(T)
    ;       closed
    ;       would_block.

    % Take an item from the start of the channel, but do not block.
    % Returns `ok(Item)' if `Item' was taken from the channel,
    % `closed' if no item was taken because the channel is closed, or
    % `would_block' if no item could be taken from the channel without
    % blocking. `would_block' may be returned even if the channel is not
    % empty.
    %
:- pred try_take(closeable_channel(T)::in, try_take_result(T)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module maybe.
:- import_module require.
:- import_module thread.mvar.

%---------------------------------------------------------------------------%

    % Invariants:
    % - If the channel is open, the hole at the write end is empty.
    % - If the channel is closed, the hole at the write end is full and
    %   contains `closed'.
    %
:- type closeable_channel(T)
    --->    channel(
                mvar(stream(T)),    % Read end.
                mvar(stream(T))     % Write end.
            ).

:- type stream(T) == mvar(item(T)).

:- type item(T)
    --->    item(
                T,          % The current item.
                stream(T)   % The rest of the stream.
            )
    ;       closed.         % End of the stream.

init(channel(Read, Write), !IO) :-
    mvar.init(Read, !IO),
    mvar.init(Write, !IO),
    mvar.init(Hole, !IO),
    mvar.put(Read, Hole, !IO),
    mvar.put(Write, Hole, !IO).

put(channel(_Read, Write), Val, Success, !IO) :-
    mvar.init(NewHole, !IO),
    mvar.take(Write, OldHole, !IO),
    mvar.try_put(OldHole, item(Val, NewHole), Success, !IO),
    (
        Success = yes,
        % The channel was open, and remains open.
        mvar.put(Write, NewHole, !IO)
    ;
        Success = no,
        % The channel was closed, and remains closed.
        mvar.put(Write, OldHole, !IO)
    ).

close(channel(_Read, Write), !IO) :-
    mvar.take(Write, Hole, !IO),
    % We have exclusive WRITE access to the hole.
    % If the channel is open, the hole is empty so this will succeed.
    % If the channel is closed, the hole is already full with `closed'.
    mvar.try_put(Hole, closed, _Success, !IO),
    mvar.put(Write, Hole, !IO).

take(channel(Read, _Write), Res, !IO) :-
    mvar.take(Read, Head, !IO),
    mvar.read(Head, ItemOrClosed, !IO),
    (
        ItemOrClosed = item(Val, NewHead),
        Res = ok(Val)
    ;
        ItemOrClosed = closed,
        Res = closed,
        NewHead = Head
    ),
    mvar.put(Read, NewHead, !IO).

try_take(channel(Read, _Write), Res, !IO) :-
    mvar.take(Read, Head, !IO),
    mvar.try_read(Head, TryRead, !IO),
    (
        TryRead = yes(ItemOrClosed),
        (
            ItemOrClosed = item(Val, NewHead),
            Res = ok(Val)
        ;
            ItemOrClosed = closed,
            Res = closed,
            NewHead = Head
        )
    ;
        TryRead = no,
        Res = would_block,
        NewHead = Head
    ),
    mvar.put(Read, NewHead, !IO).

%---------------------------------------------------------------------------%
:- end_module thread.closeable_channel.
%---------------------------------------------------------------------------%
