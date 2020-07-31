%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: timestamp.m.
% Main author: stayl.
%
% Timestamp representation for smart recompilation.
%
%-----------------------------------------------------------------------------%

:- module libs.timestamp.
:- interface.

:- import_module time.

    % For use by predicates that do or do not return timestamps, as requested.
:- type maybe_return_timestamp
    --->    do_return_timestamp
    ;       dont_return_timestamp.

    % For use by predicates that already have a record of what a file's
    % contents were at a particular point in time.
:- type read_module_and_timestamps
    --->    always_read_module(maybe_return_timestamp)
            % Read the file regardless of its timestamp.
    ;       dont_read_module_if_match(timestamp).
            % If the file's timestamp matches the one given here, don't read
            % the file; just return its timestamp (which is this timestamp).

    % A `timestamp' is similar to a `time_t' except that timestamps are system
    % independent. A timestamp string (obtained using timestamp_to_string)
    % written on one system can be read on any other system. Comparison of
    % values of type `timestamp' (via compare/3) is equivalent to comparison
    % of the times represented.
:- type timestamp.

    % Return a timestamp which is older than any other timestamp.
    %
:- func oldest_timestamp = timestamp.

    % Return a timestamp which is newer than any other timestamp.
    %
:- func newest_timestamp = timestamp.

    % Converts the calendar time value `Time' into a timestamp.
    % Equivalent to `gm_time_to_timestamp(gmtime(Time))'.
    %
:- func time_t_to_timestamp(time_t) = timestamp.

    % Converts a timestamp into a string with format "yyyy-mm-dd hh:mm:ss",
    % expressed as UTC.
    %
:- func timestamp_to_string(timestamp) = string.

    % Converts a string formatted as "yyyy-mm-dd hh:mm:ss", into a timestamp.
    % Fails if the string does not have the correct format.
    %
:- func string_to_timestamp(string) = timestamp is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module require.   % Required for erlang grade.
:- import_module string.

%-----------------------------------------------------------------------------%

    % A timestamp is a string formatted as "yyyy-mm-dd hh:mm:ss"
    % representing a time expressed as UTC (Universal Coordinated Time).
    %
    % We use a no-tag type rather than an abstract equivalence type to avoid
    % type errors with abstract equivalence types in the hlc backend.
:- type timestamp
    --->    timestamp(string).

oldest_timestamp = timestamp("0000-00-00 00:00:00").
newest_timestamp = timestamp("9999-99-99 99:99:99").

time_t_to_timestamp(Time) = gmtime_to_timestamp(time.gmtime(Time)).

:- func gmtime_to_timestamp(tm) = timestamp.

gmtime_to_timestamp(tm(Year, Month, MD, Hrs, Min, Sec, YD, WD, DST)) =
    timestamp(gmtime_to_timestamp_string(Year, Month, MD, Hrs, Min, Sec,
        YD, WD, maybe_dst_to_int(DST))).

:- func gmtime_to_timestamp_string(int, int, int, int, int, int, int, int, int)
    = string.

:- pragma foreign_decl("C", "
    #include ""mercury_string.h""
    #include <time.h>
").
:- pragma foreign_proc("C",
    gmtime_to_timestamp_string(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in,
        Sec::in, YD::in, WD::in, N::in) = (Result::out),
    [will_not_call_mercury, promise_pure],
"{
    int size;
    struct tm t;

    t.tm_sec = (int) Sec;
    t.tm_min = (int) Min;
    t.tm_hour = (int) Hrs;
    t.tm_mon = (int) Mnt;
    t.tm_year = (int) Yr;
    t.tm_wday = (int) WD;
    t.tm_mday = (int) MD;
    t.tm_yday = (int) YD;
    t.tm_isdst = (int) N;

    size = sizeof ""yyyy-mm-dd hh:mm:ss"";
    MR_allocate_aligned_string_msg(Result, size - 1, MR_ALLOC_ID);

    strftime(Result, size, ""%Y-%m-%d %H:%M:%S"", &t);
}").

:- pragma foreign_proc("C#",
    gmtime_to_timestamp_string(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in,
        Sec::in, _YD::in, _WD::in, _N::in) = (Result::out),
    [will_not_call_mercury, promise_pure],
"{
    System.DateTime t;
    t = new System.DateTime(Yr + 1900, Mnt + 1, MD, Hrs, Min, Sec);

    string format_str = ""yyyy-MM-dd hh:mm:ss"";
    Result = t.ToString(format_str);
}").

:- pragma foreign_proc("Java",
    gmtime_to_timestamp_string(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in,
        Sec::in, _YD::in, _WD::in, _N::in) = (Result::out),
    [will_not_call_mercury, promise_pure],
"
    java.util.GregorianCalendar cal =
        new java.util.GregorianCalendar(Yr + 1900, Mnt - 1, MD, Hrs, Min, Sec);
    java.text.SimpleDateFormat sdf =
        new java.text.SimpleDateFormat(""yyyy-MM-dd HH:mm:ss"");
    java.util.Date date = new java.util.Date(cal.getTimeInMillis());
    Result = sdf.format(date);
").

gmtime_to_timestamp_string(_, _, _, _, _, _, _, _, _) = _ :-
    sorry($file, $pred).

:- func maybe_dst_to_int(maybe(dst)) = int.

maybe_dst_to_int(M) = N :-
    (
        M = yes(DST),
        (
            DST = daylight_time,
            N = 1
        ;
            DST = standard_time,
            N = 0
        )
    ;
        M = no,
        N = -1
    ).

timestamp_to_string(timestamp(Timestamp)) = Timestamp.

string_to_timestamp(Timestamp) = timestamp(Timestamp) :-
    % The if-then-else here is to force order of evaluation --
    % we need to ensure that the sanity checks occur before the
    % calls to unsafe_index. The offsets are only valid if the string
    % contains only ASCII characters, as expected.
    ( if
        string.all_match(plausible_timestamp_char, Timestamp),
        string.length(Timestamp, TimestampLength),
        string.length("yyyy-mm-dd hh:mm:ss", ShouldBeTimestampLength),
        TimestampLength = ShouldBeTimestampLength
    then
        string.to_int(string.unsafe_between(Timestamp, 0, 4), _),

        string.unsafe_index(Timestamp, 4, '-'),

        string.to_int(string.unsafe_between(Timestamp, 5, 7), Month),
        Month >= 1,
        Month =< 12,

        string.unsafe_index(Timestamp, 7, '-'),

        string.to_int(string.unsafe_between(Timestamp, 8, 10), Day),
        Day >= 1,
        Day =< 31,

        string.unsafe_index(Timestamp, 10, ' '),

        string.to_int(string.unsafe_between(Timestamp, 11, 13), Hour),
        Hour >= 0,
        Hour =< 23,

        string.unsafe_index(Timestamp, 13, ':'),

        string.to_int(string.unsafe_between(Timestamp, 14, 16), Minute),
        Minute >= 0,
        Minute =< 59,

        string.unsafe_index(Timestamp, 16, ':'),

        string.to_int(string.unsafe_between(Timestamp, 17, 19), Second),
        Second >= 0,
        Second =< 61    % Seconds 60 and 61 are for leap seconds.
    else
        fail
    ).

:- pred plausible_timestamp_char(char::in) is semidet.

plausible_timestamp_char(Char) :-
    char.to_int(Char, CharInt),
    char.to_int(':', HighestInt),
    CharInt =< HighestInt.

%-----------------------------------------------------------------------------%
:- end_module libs.timestamp.
%-----------------------------------------------------------------------------%
