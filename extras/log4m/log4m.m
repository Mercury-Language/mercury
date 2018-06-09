%---------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%------------------------------------------------------------------------------%
/****M* libs/log4m/log4m.m/log4m
 * NAME
 *   log4m
 * PURPOSE
 *   A module for providing logging services.
 * AUTHOR
 *   Peter Ross <pro@missioncriticalit.com>
 * BUGS
 *   This module isn't thread-safe, as one updates to the logger state can
 *   be lost.
 * TODO
 *   Make the update of the internal log state thread-safe.
 ****
 */
:- module log4m.

:- interface.

:- import_module bool, io, list.

/****T* libs/log4m/log4m.m/id
 * NAME
 *    Type: id
 * DESCRIPTION
 *    An id for identifying a logger.  The id's are hierarchical.
 *    The root logger is [].
 *    A child of the root logger is ["Performance"]
 *    A child of the previous logger is ["Child", "Performance"]
 *    Note that the lists are in reverse order to the naming of loggers
 *    in log4j.
 * SOURCE
 */
:- type id == list(string).
/*****/

/****T* libs/log4m/log4m.m/level
 * NAME
 *    Type: level
 * DESCRIPTION
 *    A log message is given a level.  debug is the lowest level, fatal
 *    the highest.
 * SOURCE
 */
:- type level
    --->    debug
    ;       info
    ;       warn
    ;       error
    ;       fatal.
/*****/

/****T* libs/log4m/log4m.m/addivity
 * NAME
 *    Type: addivity
 * DESCRIPTION
 *    The addivity type is used to determine the whether to continue
 *    or stop searching for appenders.
 * SOURCE
 */
:- type addivity
    --->    stop
    ;       continue.
/*****/


/****T* libs/log4m/log4m.m/appender
 * NAME
 *    Typeclass: appender
 * DESCRIPTION
 *    The typeclass appender describes types which can be used to write
 *    a string to somewhere.
 * SOURCE
 */
:- typeclass appender(T) where [
    pred write_string(T::in, id::in, level::in, string::in, io::di, io::uo) is det
].  
/*****/


/****P* libs/log4m/log4m.m/debug 
 * NAME
 *    debug 
 * SYNOPSIS
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be debug.
 *
 *    Note that all the appenders are lost.
 * SOURCE
 */
:- pred debug(io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/info 
 * NAME
 *    info 
 * SYNOPSIS
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be info.
 *
 *    Note that all the appenders are lost.
 * SOURCE
 */
:- pred info(io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/warn 
 * NAME
 *    warn 
 * SYNOPSIS
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be warn.
 *
 *    Note that all the appenders are lost.
 * SOURCE
 */
:- pred warn(io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/error 
 * NAME
 *    error 
 * SYNOPSIS
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be error.
 *
 *    Note that all the appenders are lost.
 * SOURCE
 */
:- pred error(io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/fatal 
 * NAME
 *    fatal 
 * SYNOPSIS
 *    Resets the logger state to be empty, then sets the level of the
 *    root logger to be fatal.
 *
 *    Note that all the appenders are lost.
 * SOURCE
 */
:- pred fatal(io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/update_level 
 * NAME
 *    update_level 
 * SYNOPSIS
 *    Update the logging level of an id.
 * PARAMETERS
 *    id - The id of the logger whose level we are updating.
 *
 *    level - The level to set the logger to.
 * TODO
 *    Make this function thread safe.
 * SOURCE
 */
:- pred update_level(id::in, level::in, io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/add_appender 
 * NAME
 *    add_appender 
 * SYNOPSIS
 *    Add an appender to be called when logging at a specified id,
 *    and specify via the addivity whether to continue searching for
 *    appenders to call in the parent loggers.
 * FUNCTION
 *    The sequence of calls:
 *      add_appender(["L1"], stop, A1, !IO),
 *      add_appender(["L1"], continue, A2, !IO),
 *      add_appender(["L2", "L1"], stop, B1, !IO),
 *      add_appender(["L3, "L2", "L1"], continue, C1, !IO)
 *    will have the following behaviour.
 *
 *    When logging for ["L1"] appenders A1 and A2 will be called.
 *    The appenders for [] will not be called because we specified that
 *    the addivity stop, when setting the appender A1.  Setting the
 *    addivity to continue for A2 does not override the original stop.
 *
 *    When logging for ["L2", "L1"] only appender B1 will be called.
 *    The addivity stop prevents calling the appenders for ["L1"] and [].
 *
 *    When logging for ["L3, "L2", "L1"] the appenders C1 and B1.
 *    B1 is called because the addivity is set to continue and B1
 *    is implied by the level ["L2", "L1"].
 * TODO
 *    Determine if the setting of addivity continue after the setting
 *    of the addivity stop should override the stop.
 * SOURCE
 */
:- pred add_appender(id::in, addivity::in, T::in,
                io::di, io::uo) is det <= appender(T).
/*****/

/****P* libs/log4m/log4m.m/will_log 
 * NAME
 *    will_log 
 * SYNOPSIS
 *    For a given logger id at the specified level return an indicator
 *    of whether or not logging would occur.
 * SOURCE
 */
:- pred will_log(id::in, level::in, bool::out, io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/log 
 * NAME
 *    log 
 * SYNOPSIS
 *    If will_log < log4m/log4m.m/will_log > indicates that logging
 *    should occur then call the appenders implied by the logger id
 *    with the specified string.
 * SOURCE
 */
:- pred log(id::in, level::in, string::in, io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/log_f
 * NAME
 *    log_f
 * SYNOPSIS
 *    If will_log < log4m/log4m.m/will_log > indicates that logging
 *    should occur then call the appenders implied by the logger id
 *    with the result of evaluating the function which generates a string.
 *
 *    The idea behind this predicate is to avoid the possibly expensive
 *    creation of log messages, only to not have the message logged.
 *    This is done by passing a closure which creates the log message
 *    only when it is needed.  Note one still has to be careful that
 *    all the expensive calculations are done inside the closure.
 * SOURCE
 */
:- pred log_f(id::in, level::in, ((func) = string)::in, io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/log_p
 * NAME
 *    log_p
 * SYNOPSIS
 *    If will_log indicates that loggind should occur _and_ predicate
 *    check_condition succeeds then call the appenders implied by the logger id
 *    with the result of evaluating the predicate which generates a string.
 * SOURCE
 */
:- type check_condition == pred(string).
:- inst check == (pred(out) is semidet).

:- pred log_p(id::in, level::in, check_condition::in(check), io::di, io::uo) is det.
/*****/

/****P* libs/log4m/log4m.m/unsafe_log_f
 * NAME
 *    unsafe_log_f
 * SYNOPSIS
 *    Gets a fake io__state and calls log_f.
 *    Unsafe because the compiler might decide to eliminate this call on you.
 * SOURCE
 */
:- pred unsafe_log_f(id::in, level::in, ((func) = string)::in) is det.
/*****/

/****P* libs/log4m/log4m.m/impure_log_f
 * NAME
 *    impure_log_f
 * SYNOPSIS
 *    Gets a fake io__state and calls log_f.
 * SOURCE
 */
:- impure pred impure_log_f(id::in, level::in, ((func) = string)::in) is det.
/*****/


/****P* libs/log4m/log4m.m/update_log 
 * NAME
 *    update_log 
 * SYNOPSIS
 *    Update the logger state with levels read from a file.
 *    Returns an error if the file couldn't be opened.
 * FUNCTION
 *    Expects a file where each line is in the following format.
 *
 *      set_level(["L1"], debug).
 *
 *    This predicate will throw an exception if the lines are of the
 *    incorrect format.
 * SOURCE
 */
:- pred update_log(string::in, io__res::out, io::di, io::uo) is det.
/*****/

%------------------------------------------------------------------------------%

/****T* libs/log4m/log4m.m/spec
 * NAME
 *    Type: spec
 * DESCRIPTION
 *    A formatted appender takes a description of how a message is to be
 *    formatted plus the underlying appender that is to be used to write
 *    the actual log message.
 *
 *    The message is formatted by simply applying in order each component
 *    in the list(spec) to the underlying formatter (T).
 * SOURCE
 */
:- type formatted_appender(T)
    --->    formatted_appender(list(spec), T).
/*****/

/****T* libs/log4m/log4m.m/spec
 * NAME
 *    Type: spec
 * DESCRIPTION
 *    A component to be output
 *      - date
 *          The current date in the format "YYYY-MM-DD HH:MM:SS"
 *      - id
 *          The id of the current message to be logged in the format
 *          ["A", "B"]
 *      - level
 *          The level of the log message as a string. eg "debug"
 *      - message
 *          The actual message to be logged
 *      - str(S)
 *          The string, S, is written to the underlying appender.
 *      - thread_id
 *          The id of the thread which output the log message.
 * SOURCE
 */
:- type spec
    --->    date
    ;       id
    ;       level
    ;       message
    ;       str(string)
    ;       thread_id
    .
/*****/

/****T* libs/log4m/log4m.m/appender(formatted_appender(T))
 * NAME
 *    Instance: appender(formatted_appender(T))
 * DESCRIPTION
 *    Apply the formatting specified before writing to
 *    the appender T.
 * DERIVED FROM
 *    log4m/log4m.m/appender
 * SOURCE
 */
:- instance appender(formatted_appender(T)) <= appender(T).
/*****/

/****T* libs/log4m/log4m.m/appender(io__output_stream)
 * NAME
 *    Instance: appender(io__output_stream)
 * DESCRIPTION
 *    Write the specified string to the io__output_stream.
 * DERIVED FROM
 *    log4m/log4m.m/appender
 * SOURCE
 */
:- instance appender(io__output_stream).
/*****/

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module map.

:- type log
    --->    log(level_map, appenders_map).

:- type level_map == map(id, level).

:- type appenders_map == map(id, appenders).

:- type appenders
    --->    stop(list(appender))
    ;       continue(list(appender))
    .

:- type appender
    --->    some [T] appender(T) => appender(T).


%------------------------------------------------------------------------------%

debug(!IO) :- set_log(debug, !IO).
info(!IO) :- set_log(info, !IO).
warn(!IO) :- set_log(warn, !IO).
error(!IO) :- set_log(error, !IO).
fatal(!IO) :- set_log(fatal, !IO).

:- func debug = log.
:- func info = log.
:- func warn = log.
:- func error = log.
:- func fatal = log.

debug = log(map__set(map__init, [], debug), map__init).
info = log(map__set(map__init, [], info), map__init).
warn = log(map__set(map__init, [], warn), map__init).
error = log(map__set(map__init, [], error), map__init).
fatal = log(map__set(map__init, [], fatal), map__init).

will_log(Id, Level, WillLog, !IO) :-
    get_log(Log, !IO),
    Log = log(LevelsMap, _),
    LoggerLevel = find_logger_level(Id, LevelsMap),
    compare(Res, Level, LoggerLevel),
    ( (Res = (=) ; Res = (>)) ->
        WillLog = yes
    ;
        WillLog = no
    ).

log(Id, Level, Msg, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    ( WillLog = yes ->
        get_log(Log, !IO),
        Log = log(_, Appenders),
        write_levels(Id, Level, Msg, Appenders, !IO)
    ;
        true
    ).

log_f(Id, Level, Func, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    ( WillLog = yes ->
        get_log(Log, !IO),
        Log = log(_, Appenders),
        write_levels(Id, Level, apply(Func), Appenders, !IO)
    ;
        true
    ).

:- func find_logger_level(id, level_map) = level.

find_logger_level([], LevelMap) = map__lookup(LevelMap, []).
find_logger_level([H|T], LevelMap) =
    ( map__search(LevelMap, [H|T], Level) ->
        Level
    ;
        find_logger_level(T, LevelMap)
    ).

%------------------------------------------------------------------------------%
log_p(Id, Level, P, !IO) :-
    will_log(Id, Level, WillLog, !IO),
    ( WillLog = yes, P(ToLog) ->
        get_log(Log, !IO),
        Log = log(_, Appenders),
        write_levels(Id, Level, ToLog, Appenders, !IO)
    ;
        true
    ).
%------------------------------------------------------------------------------%

:- pragma promise_pure(unsafe_log_f/3).
unsafe_log_f(Id, Level, Func) :-
    impure impure_log_f(Id, Level, Func).

impure_log_f(Id, Level, Func) :-
    impure IO = io,
    log_f(Id, Level, Func, IO, _).

:- impure func io = io.
:- mode io = uo is det.

:- pragma foreign_proc(c, io = (IO::uo), [will_not_call_mercury], "
    IO = 0;
").

%------------------------------------------------------------------------------%

:- pred write_levels(id::in, level::in, string::in,
                appenders_map::in, io::di, io::uo) is det.

write_levels(Id, Level, String, Map, !IO) :-
    write_levels(Id, Id, Level, String, Map, !IO).

:- pred write_levels(id::in, id::in, level::in, string::in,
                appenders_map::in, io::di, io::uo) is det.

write_levels([], Id, Level, String, Map, !IO) :-
    ( map__search(Map, [], Data) ->
        write_appenders(Id, Level, String, Data, !IO)
    ;
        true
    ).
write_levels([H|T], Id, Level, String, Map, !IO) :-
    ( map__search(Map, [H|T], Data) ->
        write_appenders(Id, Level, String, Data, !IO),
        ( Data = stop(_)
        ; Data = continue(_),
            write_levels(T, Id, Level, String, Map, !IO)
        )
    ;
        write_levels(T, Id, Level, String, Map, !IO)
    ).

:- pred write_appenders(id::in, level::in, string::in, appenders::in, io::di, io::uo) is det.

write_appenders(Id, Level, String, stop(Appenders), !IO) :-
        list__foldl(write_appender(Id, Level, String), Appenders, !IO).
write_appenders(Id, Level, String, continue(Appenders), !IO) :-
        list__foldl(write_appender(Id, Level, String), Appenders, !IO).

:- pred write_appender(id::in, level::in, string::in, appender::in, io::di, io::uo) is det.

write_appender(Id, Level, S, appender(A), !IO) :-
    write_string(A, Id, Level, S, !IO).

%------------------------------------------------------------------------------%

:- import_module require, string.

:- type logfile
    --->    set_level(id, level).

update_log(FileName, Result, !IO) :-
    io__open_input(FileName, OpenRes, !IO),
    ( OpenRes = ok(Stream),
        get_log(Log0, !IO),
        read_file(Stream, Log0, Log, !IO),
        set_log(Log, !IO),
        io__close_input(Stream, !IO),
        Result = ok
    ; OpenRes = error(E),
        Result = error(E)
    ).

:- pred read_file(io__input_stream::in,
                log::in, log::out, io::di, io::uo) is det.

read_file(Input, !Log, !IO) :-
    io__read(Input, Result, !IO),
    ( Result = ok(set_level(Id, Level)),
        !:Log = update_level(Id, Level, !.Log),
        read_file(Input, !Log, !IO)
    ; Result = eof,
        true
    ; Result = error(Msg, Line),
        input_stream_name(Input, Name, !IO),
        error(format("log4m.read_file: %s line %d has error %s.",
                [s(Name), i(Line), s(Msg)]))
    ).
    

%------------------------------------------------------------------------------%

update_level(Id, Level, !IO) :-
    get_log(Log, !IO),
    set_log(update_level(Id, Level, Log), !IO).

:- func update_level(id, level, log) = log.

update_level(Id, Level, log(M, A)) = log(map__set(M, Id, Level), A).

%------------------------------------------------------------------------------%

add_appender(Id, Addivity, Appender, !IO) :-
    get_log(Log0, !IO),
    Log0 = log(Levels, Appenders0),
    add_appender_2(Id, Addivity, Appender, Appenders0, Appenders),
    Log = log(Levels, Appenders),
    set_log(Log, !IO).

:- pred add_appender_2(id::in, addivity::in, T::in,
                appenders_map::in, appenders_map::out) is det <= appender(T).

add_appender_2(Id, Addivity, T, !Map) :-
    App = 'new appender'(T),
    ( map__search(!.Map, Id, Data0) ->
        ( Data0 = stop(Appenders),
            Data = stop([App | Appenders])
        ; Data0 = continue(Appenders),
            ( Addivity = stop,
                Data = stop([App | Appenders])
            ; Addivity = continue,
                Data = continue([App | Appenders])
            )
        )
    ;
        ( Addivity = stop,
            Data = stop([App])
        ; Addivity = continue,
            Data = continue([App])
        )
    ),
    !:Map = map__set(!.Map, Id, Data).

%------------------------------------------------------------------------------%

:- instance appender(io__output_stream) where [
    (write_string(S, _Id, _Level, Str, !.IO, !:IO) :-
        io__write_string(S, Str, !IO),
        io__flush_output(S, !IO)
    )
].

%------------------------------------------------------------------------------%

    % XXX none of this thread safe.
:- pragma foreign_decl(c, local, "
static MR_Word LOG4M_log = (MR_Word) NULL;
").

:- pred get_log(log::out, io::di, io::uo) is det.
:- pragma promise_pure(get_log/3).

get_log(Log, !IO) :-
    ( impure get_log(Log0) ->
        Log = Log0
    ;
        Log = fatal
    ).

:- pred set_log(log::in, io::di, io::uo) is det.
:- pragma promise_pure(set_log/3).

set_log(Log, !IO) :-
    impure set_log(Log).

:- impure pred get_log(log::out) is semidet.
:- pragma foreign_proc(c, get_log(Log::out),
        [will_not_call_mercury], "
    if (LOG4M_log) {
        Log = LOG4M_log;
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- impure pred set_log(log::in) is det.
:- pragma foreign_proc(c, set_log(Log::in),
        [will_not_call_mercury], "
    LOG4M_log = Log;
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- import_module cord, int, time.

:- instance appender(formatted_appender(T)) <= appender(T) where [
    write_string(formatted_appender(Specs, A), Id, Level, Message, !IO) :-
        list__foldl(write_format(A, Id, Level, Message), Specs, !IO)
].

:- pred write_format(A::in, id::in, level::in, string::in,
                spec::in, io::di, io::uo) is det <= appender(A).

write_format(A, Id, Level, _Message, date, !IO) :-
    time(Time, !IO),
    localtime(Time, TM, !IO),
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, _YD, _WD, _DST),
    Date = string__format("%4d-%02d-%02d %02d:%02d:%02d",
            [i(Yr+1900), i(Mnt+1), i(MD), i(Hrs), i(Min), i(Sec)]),
    write_string(A, Id, Level, Date, !IO).
write_format(A, Id, Level, _Message, id, !IO) :-
    write_string(A, Id, Level, id(Id), !IO).
write_format(A, Id, Level, _Message, level, !IO) :-
    write_string(A, Id, Level, level(Level), !IO).
write_format(A, Id, Level, Message, message, !IO) :-
    write_string(A, Id, Level, Message, !IO).
write_format(A, Id, Level, _Message, str(S), !IO) :-
    write_string(A, Id, Level, S, !IO).
write_format(A, Id, Level, _Message, thread_id, !IO) :-
    thread_id(TId, !IO),
    write_string(A, Id, Level, string__format("%06d", [i(TId)]), !IO).

:- func id(id) = string.

id([]) = "[]".
id([X|Xs]) = append_list(list(s("[") ++ s(string(X)) ++ id2(Xs) ++ s("]"))).

:- func id2(id) = cord(string).

id2([]) = empty.
id2([X|Xs]) = s(", ") ++ s(string(X)) ++ id2(Xs).

:- func s(T) = cord(T).
s(X) = singleton(X).
:- func level(level) = string.

level(debug) = "debug".
level(info) = "info".
level(warn) = "warn".
level(error) = "error".
level(fatal) = "fatal".

:- pred thread_id(int::out, io::di, io::uo) is det.  

:- pragma foreign_proc(c, thread_id(Id::out, IO0::di, IO::uo),
                [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io], "
#ifdef MR_THREAD_SAFE
	pthread_t thread;
	thread = pthread_self();
    Id = (int) thread;
#else
    Id = getpid();
#endif
    IO = IO0;
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0
