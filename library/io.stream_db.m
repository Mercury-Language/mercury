%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.stream_db.m.
%
% This module maintains the database of I/O streams.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.stream_db.
:- interface.

:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

% Types and predicates for managing the stream info database.

:- type stream_db == map(stream_id, stream_info).

:- type stream_info
    --->    stream(
                stream_id               :: int,
                stream_mode             :: stream_mode,
                stream_content          :: stream_content,
                stream_source           :: stream_source
            ).

:- type maybe_stream_info
    --->    stream(
                maybe_stream_id         :: int,
                maybe_stream_mode       :: stream_mode,
                maybe_stream_content    :: stream_content,
                maybe_stream_source     :: stream_source
            )
    ;       unknown_stream.

:- type stream_mode
    --->    input
    ;       output
    ;       append.

:- type stream_content
    --->    text
    ;       binary
    ;       preopen.

:- type stream_source
    --->    file(string)    % the file name
    ;       stdin
    ;       stdout
    ;       stderr.

%---------------------%

    % Retrieves the database mapping streams to the information we have
    % about those streams.
    %
    % For get_stream_db, caller must hold the stream_db lock.
    % For get_stream_db_with_locking, caller must NOT hold the stream_db lock.
    %
:- pred get_stream_db(stream_db::out, io::di, io::uo) is det.
:- impure pred get_stream_db_with_locking(stream_db::out) is det.

    % Caller must hold the stream_db lock.
    %
:- pred set_stream_db(stream_db::in, io::di, io::uo) is det.

%---------------------%

:- pred insert_stream_info(stream::in, stream_info::in, io::di, io::uo) is det.

:- pred maybe_delete_stream_info(io.stream::in, io::di, io::uo) is det.

%---------------------%

:- pred stream_name(stream::in, string::out, io::di, io::uo) is det.

:- pred stream_info(io.stream::in, maybe(stream_info)::out,
    io::di, io::uo) is det.

%---------------------%

    % Returns the information associated with the specified input
    % stream in the given stream database.
    %
:- func input_stream_info(stream_db, io.text_input_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified binary input
    % stream in the given stream database.
    %
:- func binary_input_stream_info(stream_db, io.binary_input_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified output
    % stream in the given stream database.
    %
:- func output_stream_info(stream_db, io.text_output_stream)
    = maybe_stream_info.

    % Returns the information associated with the specified binary output
    % stream in the given stream database.
    %
:- func binary_output_stream_info(stream_db, io.binary_output_stream)
    = maybe_stream_info.

%---------------------%

    % If the univ contains an I/O stream, return information about that
    % stream, otherwise fail.
    %
:- func get_io_stream_info(stream_db, T) = maybe_stream_info is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness],
"
    StreamDb = ML_io_stream_db;
").
:- pragma foreign_proc("C#",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = mercury.io__stream_db.ML_io_stream_db;
").
:- pragma foreign_proc("Java",
    get_stream_db(StreamDb::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    StreamDb = jmercury.io__stream_db.ML_io_stream_db;
").

%---------------------%

:- pragma foreign_proc("C",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
    StreamDb = ML_io_stream_db;
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.get_stream_db/1"");
").
:- pragma foreign_proc("C#",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = mercury.io__stream_db.ML_io_stream_db;
").
:- pragma foreign_proc("Java",
    get_stream_db_with_locking(StreamDb::out),
    [will_not_call_mercury, tabled_for_io],
"
    StreamDb = jmercury.io__stream_db.ML_io_stream_db;
").

%---------------------%

:- pragma foreign_proc("C",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness, no_sharing],
"
    ML_io_stream_db = StreamDb;
").
:- pragma foreign_proc("C#",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    mercury.io__stream_db.ML_io_stream_db = StreamDb;
").
:- pragma foreign_proc("Java",
    set_stream_db(StreamDb::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    jmercury.io__stream_db.ML_io_stream_db = StreamDb;
").

%---------------------%

:- pred lock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    lock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_LOCK(&ML_io_stream_db_lock, ""io.lock_stream_db/2"");
").

lock_stream_db(!IO).

:- pred unlock_stream_db(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unlock_stream_db(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        no_sharing],
"
    MR_UNLOCK(&ML_io_stream_db_lock, ""io.stream_db.unlock_stream_db/2"");
").

unlock_stream_db(!IO).

%---------------------------------------------------------------------------%

insert_stream_info(Stream, Name, !IO) :-
    lock_stream_db(!IO),
    get_stream_db(StreamDb0, !IO),
    map.set(get_stream_id(Stream), Name, StreamDb0, StreamDb),
    set_stream_db(StreamDb, !IO),
    unlock_stream_db(!IO).

maybe_delete_stream_info(Stream, !IO) :-
    may_delete_stream_info(MayDeleteStreamInfo, !IO),
    ( if MayDeleteStreamInfo = 0 then
        true
    else
        lock_stream_db(!IO),
        get_stream_db(StreamDb0, !IO),
        map.delete(get_stream_id(Stream), StreamDb0, StreamDb),
        set_stream_db(StreamDb, !IO),
        unlock_stream_db(!IO)
    ).

    % Return an integer that is nonzero if and only if we should delete
    % the information we have about stream when that stream is closed.
    % The debugger may need this information in order to display the stream id
    % in a user-friendly manner even after the stream is closed (e.g. after
    % performing a retry after the close), so if debugging is enabled, we
    % hang on to the stream info until the end of the execution. This is a
    % space leak, but one that is acceptable in a program being debugged.
    %
:- pred may_delete_stream_info(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    may_delete_stream_info(MayDelete::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    MayDelete = !MR_debug_ever_enabled;
").

may_delete_stream_info(1, !IO).

%---------------------------------------------------------------------------%

stream_name(Stream, Name, !IO) :-
    stream_info(Stream, MaybeInfo, !IO),
    (
        MaybeInfo = yes(Info),
        Info = stream(_, _, _, Source),
        Name = source_name(Source)
    ;
        MaybeInfo = no,
        Name = "<stream name unavailable>"
    ).

stream_info(Stream, MaybeInfo, !IO) :-
    lock_stream_db(!IO),
    get_stream_db(StreamDb, !IO),
    unlock_stream_db(!IO),
    ( if map.search(StreamDb, get_stream_id(Stream), Info) then
        MaybeInfo = yes(Info)
    else
        MaybeInfo = no
    ).

%---------------------------------------------------------------------------%

input_stream_info(StreamDb, text_input_stream(Stream)) =
    get_maybe_stream_info(StreamDb, Stream).

binary_input_stream_info(StreamDb, binary_input_stream(Stream)) =
    get_maybe_stream_info(StreamDb, Stream).

output_stream_info(StreamDb, text_output_stream(Stream)) =
    get_maybe_stream_info(StreamDb, Stream).

binary_output_stream_info(StreamDb, binary_output_stream(Stream)) =
    get_maybe_stream_info(StreamDb, Stream).

:- func get_maybe_stream_info(stream_db, io.stream) = maybe_stream_info.

get_maybe_stream_info(StreamDb, Stream) = Info :-
    ( if map.search(StreamDb, get_stream_id(Stream), Info0) then
        % Info0 and Info have different types.
        Info0 = stream(Id, Mode, Content, Source),
        Info  = stream(Id, Mode, Content, Source)
    else
        Info  = unknown_stream
    ).

%---------------------%

get_io_stream_info(StreamDB, Stream) = StreamInfo :-
    ( if dynamic_cast(Stream, text_input_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, text_output_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, binary_input_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, binary_output_stream(IOStream0)) then
        IOStream = IOStream0
    else if dynamic_cast(Stream, IOStream0) then
        IOStream = IOStream0
    else
        fail
    ),
    StreamInfo = get_maybe_stream_info(StreamDB, IOStream).

:- func source_name(stream_source) = string.

source_name(file(Name)) = Name.
source_name(stdin) = "<standard input>".
source_name(stdout) = "<standard output>".
source_name(stderr) = "<standard error>".

%---------------------%

:- func get_stream_id(stream) = stream_id.

:- pragma foreign_proc("C",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifndef MR_NATIVE_GC
    // Most of the time, we can just use the pointer to the stream
    // as a unique identifier.
    Id = (MR_Word) Stream;
#else
    // For accurate GC we embed an ID in the MercuryFile and retrieve it here.
    Id = (Stream)->id;
#endif
").
:- pragma foreign_proc("C#",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure],
"
    Id = Stream.id;
").
:- pragma foreign_proc("Java",
    get_stream_id(Stream::in) = (Id::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    Id = Stream.id;
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
extern MR_Word          ML_io_stream_db;

#ifdef MR_THREAD_SAFE
    extern MercuryLock  ML_io_stream_db_lock;
#endif
").

:- pragma foreign_code("C", "
MR_Word         ML_io_stream_db;

#ifdef MR_THREAD_SAFE
    MercuryLock ML_io_stream_db_lock;
#endif
").

:- pragma foreign_code("Java", "
    public static tree234.Tree234_2<Integer, Stream_info_0> ML_io_stream_db
        = new tree234.Tree234_2.Empty_0<Integer, Stream_info_0>();
").

:- pragma foreign_code("C#", "
    // The ML_ prefixes here are not really needed,
    // since the C# code all gets generated inside a class,
    // but we keep them for consistency with the C code.

    public static tree234.Tree234_2 ML_io_stream_db =
        new tree234.Tree234_2.Empty_0();
").

%---------------------------------------------------------------------------%
:- end_module io.stream_db.
%---------------------------------------------------------------------------%
