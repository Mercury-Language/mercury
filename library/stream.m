%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2010 The University of Melbourne.
% Copyright (C) 2014-2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: stream.m.
% Authors: juliensf, maclarty.
% Stability: low
%
% This module provides a family of type classes for defining streams
% in Mercury. It also provides some generic predicates that operate
% on instances of these type classes.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module stream.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module list.

:- include_module string_writer.

%---------------------------------------------------------------------------%
%
% Types used by streams.
%

:- type name == string.

:- type result(Error)
    --->    ok
    ;       eof
    ;       error(Error).

:- type result(T, Error)
    --->    ok(T)
    ;       eof
    ;       error(Error).

:- type res(Error)
    --->    ok
    ;       error(Error).

:- type res(T, Error)
    --->    ok(T)
    ;       error(Error).

    % maybe_partial_res is used when it is possible to return a partial result
    % when an error occurs.
    %
:- type maybe_partial_res(T, Error)
    --->    ok(T)
    ;       error(T, Error).

%---------------------------------------------------------------------------%
%
% Stream errors.
%

:- typeclass error(Error) where
[

    % Convert a stream error into a human-readable format.
    % e.g. for use in error messages.
    %
    func error_message(Error) = string
].

%---------------------------------------------------------------------------%
%
% Streams.
%

    % A stream consists of a handle type and a state type.
    % The state type is threaded through, and destructively updated by,
    % the stream operations.
    %
:- typeclass stream(Stream, State) <= (Stream -> State) where
[
    % Returns a descriptive name for the stream.
    % Intended for use in error messages.
    %
    pred name(Stream::in, name::out, State::di, State::uo) is det
].

%---------------------------------------------------------------------------%
%
% Input streams.
%

    % An input stream is a source of data.
    %
:- typeclass input(Stream, State) <= stream(Stream, State) where [].

    % A reader stream is a subclass of a specific input stream that can be
    % used to read data of a specific type from that input stream.
    % A single input stream can support multiple reader subclasses.
    %
:- typeclass reader(Stream, Unit, State, Error)
    <= (input(Stream, State), error(Error), (Stream, Unit -> Error)) where
[
    % Get the next unit from the given stream.
    %
    % The get operation should block until the next unit is available,
    % or the end of the stream or an error is detected.
    %
    % If a call to get/4 returns `eof', all further calls to get/4,
    % unboxed_get/5 or bulk_get/9 for that stream return `eof'.
    %
    % If a call to get/4 returns `error(...)', all further calls
    % to get/4, unboxed_get/5 or bulk_get/4 for that stream return an error,
    % although not necessarily the same one.
    %
    % XXX We should provide an interface to allow the user to reset the
    % error status to try again if an error is transient.
    %
    pred get(Stream::in, result(Unit, Error)::out,
        State::di, State::uo) is det
].

    % An unboxed_reader stream is like a reader stream except that it provides
    % an interface that avoids a memory allocation when there is no error.
    %
:- typeclass unboxed_reader(Stream, Unit, State, Error)
    <= (input(Stream, State), error(Error), (Stream, Unit -> Error)) where
[
    % Get the next unit from the given stream. On error or eof, return
    % an *arbitrary* value of type Unit.
    %
    % The unboxed_get operation should block until the next unit is available,
    % or the end of the stream or an error is detected.
    %
    % If a call to unboxed_get/5 returns `eof', all further calls to get/4,
    % unboxed_get/5 or bulk_get/9 for that stream return `eof'.
    %
    % If a call to unboxed_get/5 returns `error(...)', all further calls
    % to get/4, unboxed_get/5 or bulk_get/4 for that stream return an error,
    % although not necessarily the same one.
    %
    % XXX We should provide an interface to allow the user to reset the
    % error status to try again if an error is transient.
    %
    pred unboxed_get(Stream::in, result(Error)::out, Unit::out,
        State::di, State::uo) is det
].

    % A bulk_reader stream is a subclass of specific input stream that can
    % be used to read multiple items of data of a specific type from that
    % input stream into a specified container. For example, binary input
    % streams may be able to efficiently read bytes into a bitmap.
    % A single input stream can support multiple bulk_reader subclasses.
    %
:- typeclass bulk_reader(Stream, Index, Store, State, Error)
    <= (input(Stream, State), error(Error),
        (Stream, Index, Store -> Error)) where
[
    % bulk_get(Stream, Index, NumItems, !Store, NumItemsRead, Result, !State).
    %
    % Read at most NumItems items into the given Store starting at the
    % given index, returning the number of items read.
    %
    % If the read succeeds, Result will be `ok' and NumItemsRead will equal
    % NumItems.
    %
    % On end-of-stream, bulk_get/9 puts as many items as it can into !Store.
    % NumItemsRead is less than NumItems, and Result is `ok'.
    %
    % If an error is detected, bulk_get/9 puts as many items as it can into
    % !Store. In such cases, NumItemsRead will be less than NumItems, and
    % Result will be `error(Err)'.
    %
    % Blocks until NumItems items are available or the end of the stream
    % is reached or an error is detected.
    %
    % Throws an exception if Index given is out of range, or if NumItems units
    % starting at Index will not fit in !Store.
    %
    % If a call to bulk_get/4 returns fewer than NumItems items, all further
    % calls to get/4, unboxed_get/5 or bulk_get/4 for that stream return no
    % items.
    %
    % If a call to bulk_get/9 returns `error(...)', all further calls to
    % get/4, unboxed_get/5 or bulk_get/9 for that stream return an error,
    % although not necessarily the same one.
    %
    pred bulk_get(Stream::in, Index::in, int::in,
        Store::bulk_get_di, Store::bulk_get_uo,
        int::out, res(Error)::out, State::di, State::uo) is det
].

    % XXX These should be di and uo, but with the current state of the mode
    % system, an unsafe_promise_unique call would be required at each call
    % to bulk_get.
:- mode bulk_get_di == in.
:- mode bulk_get_uo == out.

%---------------------------------------------------------------------------%
%
% Output streams.
%

    % An output stream is a destination for data.
    % Note that unlike input streams, output streams do not include
    % an explicit error type. They should handle errors by throwing exceptions.
    %
:- typeclass output(Stream, State)
    <= stream(Stream, State) where
[
    % For buffered output streams, completely write out any data in the buffer.
    % For unbuffered streams, this operation is a no-op.
    %
    pred flush(Stream::in, State::di, State::uo) is det
].

    % A writer stream is a subclass of specific output stream that can be
    % used to write data of a specific type to that output stream.
    % A single output stream can support multiple writer subclasses.
    %
:- typeclass writer(Stream, Unit, State)
    <= output(Stream, State) where
[
    % Write the next unit to the given stream.
    % Blocks if the whole unit cannot be written to the stream at the time
    % of the call (for example because a buffer is full).
    %
    pred put(Stream::in, Unit::in, State::di, State::uo) is det
].

%---------------------------------------------------------------------------%
%
% Duplex streams.
%

    % A duplex stream is a stream that can act as both a source and
    % destination of data, i.e. it is a both an input and an output stream.
    %
:- typeclass duplex(Stream, State)
    <= (input(Stream, State), output(Stream, State)) where
[
].

%---------------------------------------------------------------------------%
%
% Putback streams.
%

    % A putback stream is an input stream that allows data to be pushed back
    % onto the stream. As with reader subclasses it is possible to define
    % multiple putback subclasses for a single input stream.
    %
:- typeclass putback(Stream, Unit, State, Error)
    <= reader(Stream, Unit, State, Error) where
[
    % Un-gets a unit from the specified input stream.
    % Only one unit of putback is guaranteed to be successful.
    %
    pred unget(Stream::in, Unit::in, State::di, State::uo) is det
].

    % As above, but guarantees that an unlimited number of units may be pushed
    % back onto the stream.
    %
:- typeclass unbounded_putback(Stream, Unit, State, Error)
    <= putback(Stream, Unit, State, Error) where
[
].

%---------------------------------------------------------------------------%
%
% Seekable streams.
%

    % whence denotes the base for a seek operation.
    %   set - seek relative to the start of the file
    %   cur - seek relative to the current position in the file
    %   end - seek relative to the end of the file.
    %
:- type whence
    --->    set
    ;       cur
    ;       end.

:- typeclass seekable(Stream, State) <= stream(Stream, State)
    where
[
    % Seek to an offset relative to whence on the specified stream.
    % The offset is measured in bytes.
    %
    pred seek(Stream::in, whence::in, int::in, State::di, State::uo) is det,

    % As above, but the offset is always a 64-bit value.
    %
    pred seek64(Stream::in, whence::in, int64::in, State::di, State::uo) is det
].

%---------------------------------------------------------------------------%
%
% Line oriented streams.
%

    % A line oriented stream is a stream that keeps track of line numbers.
    %
:- typeclass line_oriented(Stream, State) <= stream(Stream, State)
    where
[
    % Get the current line number for the specified stream.
    %
    pred get_line(Stream::in, int::out, State::di, State::uo) is det,

    % Set the current line number of the specified stream.
    %
    pred set_line(Stream::in, int::in,  State::di, State::uo) is det
].

%---------------------------------------------------------------------------%
%
% Generic folds over input streams.
%

    % Applies the given closure to each Unit read from the input stream in
    % turn, until eof or error.
    %
:- pred input_stream_fold(Stream, pred(Unit, T, T), T,
    maybe_partial_res(T, Error), State, State)
    <= reader(Stream, Unit, State, Error).
:- mode input_stream_fold(in, in(pred(in, in, out) is det),
    in, out, di, uo) is det.
:- mode input_stream_fold(in, in(pred(in, in, out) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each Unit read from the input stream in
    % turn, until eof or error.
    %
:- pred input_stream_fold_state(Stream, pred(Unit, State, State),
    res(Error), State, State)
    <= reader(Stream, Unit, State, Error).
:- mode input_stream_fold_state(in, in(pred(in, di, uo) is det),
    out, di, uo) is det.
:- mode input_stream_fold_state(in, in(pred(in, di, uo) is cc_multi),
    out, di, uo) is cc_multi.

    % Applies the given closure to each Unit read from the input stream
    % in turn, until eof or error.
    %
:- pred input_stream_fold2_state(Stream,
    pred(Unit, T, T, State, State), T, maybe_partial_res(T, Error),
    State, State) <= reader(Stream, Unit, State, Error).
:- mode input_stream_fold2_state(in,
    in(pred(in, in, out, di, uo) is det),
    in, out, di, uo) is det.
:- mode input_stream_fold2_state(in,
    in(pred(in, in, out, di, uo) is cc_multi),
    in, out, di, uo) is cc_multi.

    % Applies the given closure to each Unit read from the input stream
    % in turn, until eof or error, or the closure returns `no' as its
    % second argument.
    %
:- pred input_stream_fold2_state_maybe_stop(Stream,
    pred(Unit, bool, T, T, State, State),
    T, maybe_partial_res(T, Error), State, State)
    <= reader(Stream, Unit, State, Error).
:- mode input_stream_fold2_state_maybe_stop(in,
    in(pred(in, out, in, out, di, uo) is det), in, out, di, uo) is det.
:- mode input_stream_fold2_state_maybe_stop(in,
    in(pred(in, out, in, out, di, uo) is cc_multi), in, out, di, uo)
    is cc_multi.

%---------------------------------------------------------------------------%
%
% Misc. operations on input streams.
%

    % Discard all the whitespace characters satisfying `char.is_whitespace'
    % from the specified stream.
    %
:- pred ignore_whitespace(Stream::in, result(Error)::out,
    State::di, State::uo)
    is det <= putback(Stream, char, State, Error).

%---------------------------------------------------------------------------%
%
% Misc. operations on output streams.
%

    % put_list(Stream, Write, Sep, List, !State).
    %
    % Write all the elements List to Stream separated by Sep.
    %
:- pred put_list(Stream, pred(Stream, T, State, State),
    pred(Stream, State, State), list(T), State, State)
    <= output(Stream, State).
:- mode put_list(in, pred(in, in, di, uo) is det, pred(in, di, uo) is det,
    in, di, uo) is det.
:- mode put_list(in, pred(in, in, di, uo) is cc_multi,
    pred(in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode put_list(in, pred(in, in, di, uo) is cc_multi,
    pred(in, di, uo) is det, in, di, uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% TODO:
% * Add non-blocking versions of the stream operations.
% * Thread-safety aspects(?)
% * Add a typeclass to hold the offset method for binary input streams.

%---------------------------------------------------------------------------%
%
% Folds over input streams.
%

input_stream_fold(Stream, Pred, T0, Res, !S) :-
    get(Stream, Result, !S),
    (
        Result = ok(Unit),
        Pred(Unit, T0, T1),
        input_stream_fold(Stream, Pred, T1, Res, !S)
    ;
        Result = eof,
        Res = ok(T0)
    ;
        Result = error(Error),
        Res = error(T0, Error)
    ).

input_stream_fold_state(Stream, Pred, Res, !S) :-
    get(Stream, Result0, !S),
    (
        Result0 = ok(Result),
        Pred(Result, !S),
        input_stream_fold_state(Stream, Pred, Res, !S)
    ;
        Result0 = eof,
        Res = ok
    ;
        Result0 = error(Error),
        Res = error(Error)
    ).

input_stream_fold2_state(Stream, Pred, T0, Res, !S) :-
    get(Stream, Result0, !S),
    (
        Result0 = ok(Result),
        Pred(Result, T0, T1, !S),
        input_stream_fold2_state(Stream, Pred, T1, Res, !S)
    ;
        Result0 = eof,
        Res = ok(T0)
    ;
        Result0 = error(Error),
        Res = error(T0, Error)
    ).

input_stream_fold2_state_maybe_stop(Stream, Pred, T0, Res, !S) :-
    get(Stream, Result0, !S),
    (
        Result0 = ok(Result),
        Pred(Result, Continue, T0, T1, !S),
        (
            Continue = no,
            Res = ok(T1)
        ;
            Continue = yes,
            input_stream_fold2_state_maybe_stop(Stream, Pred, T1, Res,
                !S)
        )
    ;
        Result0 = eof,
        Res = ok(T0)
    ;
        Result0 = error(Error),
        Res = error(T0, Error)
    ).

%---------------------------------------------------------------------------%

ignore_whitespace(Stream, Result, !State) :-
    get(Stream, CharResult, !State),
    (
        CharResult = error(Error),
        Result = error(Error)
    ;
        CharResult = eof,
        Result = eof
    ;
        CharResult = ok(Char),
        ( if char.is_whitespace(Char) then
            ignore_whitespace(Stream, Result, !State)
        else
            unget(Stream, Char, !State),
            Result = ok
        )
    ).

%---------------------------------------------------------------------------%

put_list(_Stream, _Pred, _Sep, [], !State).
put_list(Stream, Pred, Sep, [X | Xs], !State) :-
    Pred(Stream, X, !State),
    (
        Xs = []
    ;
        Xs = [_ | _],
        Sep(Stream, !State),
        put_list(Stream, Pred, Sep, Xs, !State)
    ).

%---------------------------------------------------------------------------%
:- end_module stream.
%---------------------------------------------------------------------------%
