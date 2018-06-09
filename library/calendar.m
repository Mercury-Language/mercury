%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2010 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: calendar.m.
% Main authors: maclarty
% Stability: low.
%
% Proleptic Gregorian calendar utilities.
%
% The Gregorian calendar is the calendar that is currently used by most of
% the world. In this calendar a year is a leap year if it is divisible by
% 4, but not divisible by 100. The only exception is if the year is divisible
% by 400, in which case it is a leap year. For example 1900 is not leap year,
% while 2000 is. The proleptic Gregorian calendar is an extension of the
% Gregorian calendar backward in time to before it was first introduced in
% 1582.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module calendar.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % A point on the Proleptic Gregorian calendar, to the nearest microsecond.
    %
:- type date.

    % A more meaningful name for the above.
    %
:- type date_time == date.

    % Date components.
    %
:- type year == int.         % Year 0 is 1 BC, -1 is 2 BC, etc.
:- type day_of_month == int. % 1..31 depending on the month and year
:- type hour == int.         % 0..23
:- type minute == int.       % 0..59
:- type second == int.       % 0..61 (60 and 61 are for leap seconds)
:- type microsecond == int.  % 0..999999

:- type month
    --->    january
    ;       february
    ;       march
    ;       april
    ;       may
    ;       june
    ;       july
    ;       august
    ;       september
    ;       october
    ;       november
    ;       december.

:- type day_of_week
    --->    monday
    ;       tuesday
    ;       wednesday
    ;       thursday
    ;       friday
    ;       saturday
    ;       sunday.

%---------------------%

    % Functions to retrieve the components of a date.
    %
:- func year(date) = year.
:- func month(date) = month.
:- func day_of_month(date) = day_of_month.
:- func day_of_week(date) = day_of_week.
:- func hour(date) = hour.
:- func minute(date) = minute.
:- func second(date) = second.
:- func microsecond(date) = microsecond.

    % int_to_month(Int, Month):
    % Int is the number of Month where months are numbered from 1-12.
    %
:- pred int_to_month(int, month).
:- mode int_to_month(in, out) is semidet.
:- mode int_to_month(out, in) is det.

    % det_int_to_month(Int) returns the month corresponding to Int.
    % Throws an exception if Int is not in 1-12.
    %
:- func det_int_to_month(int) = month.

    % int_to_month(Int, Month):
    % Int is the number of Month where months are numbered from 0-11.
    %
:- pred int0_to_month(int, month).
:- mode int0_to_month(in, out) is semidet.
:- mode int0_to_month(out, in) is det.

    % det_int0_to_month(Int) returns the month corresponding to Int.
    % Throws an exception if Int is not in 0-11.
    %
:- func det_int0_to_month(int) = month.

    % month_to_int(Month) returns the number of Month where months are
    % numbered from 1-12.
    %
:- func month_to_int(month) = int.

    % month_to_int0(Month) returns the number of Month where months are
    % numbered from 0-11.
    %
:- func month_to_int0(month) = int.

%---------------------%

    % init_date(Year, Month, Day, Hour, Minute, Second, MicroSecond, Date):
    % Initialize a new date. Fails if the given date is invalid.
    %
:- pred init_date(year::in, month::in, day_of_month::in, hour::in,
    minute::in, second::in, microsecond::in, date::out) is semidet.

    % Same as above, but throws an exception if the date is invalid.
    %
:- func det_init_date(year, month, day_of_month, hour, minute, second,
    microsecond) = date.

    % Retrieve all the components of a date.
    %
:- pred unpack_date(date::in,
    year::out, month::out, day_of_month::out, hour::out, minute::out,
    second::out, microsecond::out) is det.

%---------------------%

    % Convert a string of the form "YYYY-MM-DD HH:MM:SS.mmmmmm" to a date.
    % The microseconds component (.mmmmmm) is optional.
    %
:- pred date_from_string(string::in, date::out) is semidet.

    % Same as above, but throws an exception if the string is not a valid date.
    %
:- func det_date_from_string(string) = date.

    % Convert a date to a string of the form "YYYY-MM-DD HH:MM:SS.mmmmmm".
    % If the microseconds component of the date is zero, then the
    % ".mmmmmm" part is omitted.
    %
:- func date_to_string(date) = string.

%---------------------%

    % Get the current local time.
    %
:- pred current_local_time(date::out, io::di, io::uo) is det.

    % Get the current UTC time.
    %
:- pred current_utc_time(date::out, io::di, io::uo) is det.

    % Calculate the Julian day number for a date on the Gregorian calendar.
    %
:- func julian_day_number(date) = int.

    % Returns 1970/01/01 00:00:00.
    %
:- func unix_epoch = date.

    % same_date(A, B):
    % True iff A and B are equal with respect to only their date components.
    % The time components are ignored.
    %
:- pred same_date(date::in, date::in) is semidet.

%---------------------------------------------------------------------------%
%
% Durations.
%

    % A period of time measured in years, months, days, hours, minutes,
    % seconds and microseconds. Internally a duration is represented
    % using only months, days, seconds and microseconds components.
    %
:- type duration.

    % Duration components.
    %
:- type years == int.
:- type months == int.
:- type days == int.
:- type hours == int.
:- type minutes == int.
:- type seconds == int.
:- type microseconds == int.

    % Functions to retrieve duration components.
    %
:- func years(duration) = years.
:- func months(duration) = months.
:- func days(duration) = days.
:- func hours(duration) = hours.
:- func minutes(duration) = minutes.
:- func seconds(duration) = seconds.
:- func microseconds(duration) = microseconds.

    % init_duration(Years, Months, Days, Hours, Minutes,
    %   Seconds, MicroSeconds) = Duration.
    % Create a new duration. All of the components should either be
    % non-negative or non-positive (they can all be zero).
    %
:- func init_duration(years, months, days, hours, minutes, seconds,
    microseconds) = duration.

    % Retrieve all the components of a duration.
    %
:- pred unpack_duration(duration::in, years::out, months::out,
    days::out, hours::out, minutes::out, seconds::out, microseconds::out)
    is det.

    % Return the zero length duration.
    %
:- func zero_duration = duration.

    % Negate a duration.
    %
:- func negate(duration) = duration.

%---------------------%

    % Parse a duration string.
    %
    % The string should be of the form "PnYnMnDTnHnMnS" where each "n" is a
    % non-negative integer representing the number of years (Y), months (M),
    % days (D), hours (H), minutes (M) or seconds (S). The duration string
    % always starts with 'P' and the 'T' separates the date and time components
    % of the duration. A component may be omitted if it is zero, and the 'T'
    % separator is not required if all the time components are zero.
    % The second component may include a fraction component using a period.
    % This fraction component should not have a resolution higher than a
    % microsecond.
    %
    % For example the duration 1 year, 18 months, 100 days, 10 hours, 15
    % minutes 90 seconds and 300 microseconds can be written as:
    %   P1Y18M100DT10H15M90.0003S
    % while the duration 1 month and 2 days can be written as:
    %   P1M2D
    %
    % Note that internally the duration is represented using only months,
    % days, seconds and microseconds, so that
    % duration_to_string(det_duration_from_string("P1Y18M100DT10H15M90.0003S"))
    % will result in the string "P2Y6M100DT10H16M30.0003S".
    %
:- pred duration_from_string(string::in, duration::out) is semidet.

    % Same as above, but throws an exception if the duration string is invalid.
    %
:- func det_duration_from_string(string) = duration.

    % Convert a duration to a string using the same representation
    % parsed by duration_from_string.
    %
:- func duration_to_string(duration) = string.

%---------------------%

    % Add a duration to a date.
    %
    % First the years and months are added to the date.
    % If this causes the day to be out of range (e.g. April 31), then it is
    % decreased until it is in range (e.g. April 30). Next the remaining
    % days, hours, minutes and seconds components are added. These could
    % in turn cause the month and year components of the date to change again.
    %
:- pred add_duration(duration::in, date::in, date::out) is det.

    % This predicate implements a partial order relation on durations.
    % DurationA is less than or equal to DurationB iff for all of the
    % dates list below, adding DurationA to the date results in a date
    % less than or equal to the date obtained by adding DurationB.
    %
    %    1696-09-01 00:00:00
    %    1697-02-01 00:00:00
    %    1903-03-01 00:00:00
    %    1903-07-01 00:00:00
    %
    % There is only a partial order on durations, because some durations
    % cannot be said to be less than, equal to or greater than another duration
    % (e.g. 1 month vs. 30 days).
    %
:- pred duration_leq(duration::in, duration::in) is semidet.

    % Get the difference between local and UTC time as a duration.
    %
    % local_time_offset(TZ, !IO) is equivalent to:
    %   current_local_time(Local, !IO),
    %   current_utc_time(UTC, !IO),
    %   TZ = duration(UTC, Local)
    % except that it is as if the calls to current_utc_time and
    % current_local_time occurred at the same instant.
    %
    % To convert UTC time to local time, add the result of local_time_offset/3
    % to UTC (using add_duration/3). To compute UTC given the local time,
    % first negate the result of local_time_offset/3 (using negate/1) and then
    % add it to the local time.
    %
:- pred local_time_offset(duration::out, io::di, io::uo) is det.

    % duration(DateA, DateB) = Duration.
    % Find the duration between two dates using a "greedy" algorithm.
    % The algorithm is greedy in the sense that it will try to maximise each
    % component in the returned duration in the following order: years, months,
    % days, hours, minutes, seconds, microseconds.
    % The returned duration is positive if DateB is after DateA and negative
    % if DateB is before DateA.
    % Any leap seconds that occurred between the two dates are ignored.
    % The dates should be in the same timezone and in the same daylight
    % savings phase. To work out the duration between dates in different
    % timezones or daylight savings phases, first convert the dates to UTC.
    %
    % If the seconds components of DateA and DateB are < 60 then
    % add_duration(DateA, duration(DateA, DateB), DateB) will hold, but
    % add_duration(DateB, negate(duration(DateA, DateB)), DateA) may not hold.
    % For example if:
    %   DateA = 2001-01-31
    %   DateB = 2001-02-28
    %   Duration = 1 month
    % then the following holds:
    %   add_duration(duration(DateA, DateB), DateA, DateB)
    % but the following does not:
    %   add_duration(negate(duration(DateA, DateB), DateB, DateA)
    % (Adding -1 month to 2001-02-28 will yield 2001-01-28).
    %
:- func duration(date, date) = duration.

    % Same as above, except that the year and month components of the
    % returned duration will always be zero. The duration will be in terms
    % of days, hours, minutes, seconds and microseconds only.
    %
:- func day_duration(date, date) = duration.

%---------------------------------------------------------------------------%
%
% Folds over ranges of dates.
%

    % foldl_days(Pred, Start, End, !Acc):
    % Calls Pred for each day in the range of dates from Start to End
    % with an accumulator.
    % Each date in the range is generated by adding a duration of one day
    % to the previous date using the add_duration/3 predicate.
    % Consequently, the time components of the dates in the range may
    % differ if the time components of the given start and end times
    % include leap seconds.
    %
:- pred foldl_days(pred(date, A, A), date, date, A, A).
:- mode foldl_days(pred(in, in, out) is det, in, in, in, out) is det.
:- mode foldl_days(pred(in, mdi, muo) is det, in, in, mdi, muo) is det.
:- mode foldl_days(pred(in, di, uo) is det, in, in, di, uo) is det.
:- mode foldl_days(pred(in, in, out) is semidet, in, in, in, out) is semidet.
:- mode foldl_days(pred(in, mdi, muo) is semidet, in, in, mdi, muo) is semidet.
:- mode foldl_days(pred(in, di, uo) is semidet, in, in, di, uo) is semidet.

    % foldl2_days(Pred, Start, End, !Acc1, !Acc2):
    % As above, but with two accumulators.
    %
:- pred foldl2_days(pred(date, A, A, B, B), date, date, A, A, B, B).
:- mode foldl2_days(pred(in, in, out, in, out) is det, in, in, in, out,
    in, out) is det.
:- mode foldl2_days(pred(in, in, out, mdi, muo) is det, in, in, in, out,
    mdi, muo) is det.
:- mode foldl2_days(pred(in, in, out, di, uo) is det, in, in, in, out,
    di, uo) is det.
:- mode foldl2_days(pred(in, in, out, in, out) is semidet, in, in, in, out,
    in, out) is semidet.
:- mode foldl2_days(pred(in, in, out, mdi, muo) is semidet, in, in, in, out,
    mdi, muo) is semidet.
:- mode foldl2_days(pred(in, in, out, di, uo) is semidet, in, in, in, out,
    di, uo) is semidet.

    % foldl3_days(Pred, Start, End, !Acc1, !Acc2, !Acc3):
    % As above, but with three accumulators.
    %
:- pred foldl3_days(pred(date, A, A, B, B, C, C), date, date,
    A, A, B, B, C, C).
:- mode foldl3_days(pred(in, in, out, in, out, in, out) is det, in, in,
    in, out, in, out, in, out) is det.
:- mode foldl3_days(pred(in, in, out, in, out, mdi, muo) is det, in, in,
    in, out, in, out, mdi, muo) is det.
:- mode foldl3_days(pred(in, in, out, in, out, di, uo) is det, in, in,
    in, out, in, out, di, uo) is det.
:- mode foldl3_days(pred(in, in, out, in, out, in, out) is semidet, in, in,
    in, out, in, out, in, out) is semidet.
:- mode foldl3_days(pred(in, in, out, in, out, mdi, muo) is semidet, in, in,
    in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_days(pred(in, in, out, in, out, di, uo) is semidet, in, in,
    in, out, in, out, di, uo) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module time.

%---------------------------------------------------------------------------%

:- type date
    --->    date(
                dt_year             :: int,
                dt_month            :: int,
                dt_day              :: int,
                dt_hour             :: int,
                dt_minute           :: int,
                dt_second           :: int,
                dt_microsecond      :: int
            ).

:- type duration
    --->    duration(
                dur_months          :: int,
                dur_days            :: int,
                dur_seconds         :: int,
                dur_microseconds    :: int
            ).

%---------------------------------------------------------------------------%

year(Date) = Date ^ dt_year.
month(Date) = det_int_to_month(Date ^ dt_month).
day_of_month(Date) = Date ^ dt_day.
day_of_week(Date) = compute_day_of_week(Date).
hour(Date) = Date ^ dt_hour.
minute(Date) = Date ^ dt_minute.
second(Date) = Date ^ dt_second.
microsecond(Date) = Date ^ dt_microsecond.

:- func compute_day_of_week(date) = day_of_week.

compute_day_of_week(Date) = DayOfWeek :-
    % We compute the day of the week by working out the Julian day modulo 7.
    JDN = julian_day_number(Date),
    Mod = JDN mod 7,
    DayOfWeek = det_day_of_week_from_mod(Mod).

:- func det_day_of_week_from_mod(int) = day_of_week.

det_day_of_week_from_mod(Mod) = DayOfWeek :-
    ( if day_of_week_num(DayOfWeek0, Mod) then
        DayOfWeek = DayOfWeek0
    else
        unexpected($pred, "invalid mod: " ++ int_to_string(Mod))
    ).

:- pred day_of_week_num(day_of_week, int).
:- mode day_of_week_num(in, out) is det.
:- mode day_of_week_num(out, in) is semidet.

day_of_week_num(monday, 0).
day_of_week_num(tuesday, 1).
day_of_week_num(wednesday, 2).
day_of_week_num(thursday, 3).
day_of_week_num(friday, 4).
day_of_week_num(saturday, 5).
day_of_week_num(sunday, 6).

int_to_month(1, january).
int_to_month(2, february).
int_to_month(3, march).
int_to_month(4, april).
int_to_month(5, may).
int_to_month(6, june).
int_to_month(7, july).
int_to_month(8, august).
int_to_month(9, september).
int_to_month(10, october).
int_to_month(11, november).
int_to_month(12, december).

det_int_to_month(Int) =
    ( if int_to_month(Int, Month) then
        Month
    else
        unexpected($pred, "invalid month: " ++ int_to_string(Int))
    ).

int0_to_month(Int, Month) :-
    int_to_month(Int + 1, Month).

det_int0_to_month(Int) =
    ( if int0_to_month(Int, Month) then
        Month
    else
        unexpected($pred, "invalid month: " ++ int_to_string(Int))
    ).

month_to_int(Month) = Int :-
    int_to_month(Int, Month).

month_to_int0(Month) = Int :-
    int0_to_month(Int, Month).

%---------------------------------------------------------------------------%

init_date(Year, Month, Day, Hour, Minute, Second, MicroSecond, Date) :-
    Day >= 1,
    Day =< max_day_in_month_for(Year, month_to_int(Month)),
    Hour < 24,
    Minute < 60,
    Second < 62,
    MicroSecond < 1000000,
    Date = date(Year, month_to_int(Month), Day, Hour, Minute, Second,
        MicroSecond).

det_init_date(Year, Month, Day, Hour, Minute, Second, MicroSecond)
        = Date :-
    ( if
        init_date(Year, Month, Day, Hour, Minute, Second, MicroSecond, Date0)
    then
        Date = Date0
    else
        Msg = string.format("invalid date: %i-%i-%i %i:%i:%i",
            [i(Year), i(month_to_int(Month)), i(Day), i(Hour),
            i(Minute), i(Second)]),
        unexpected($pred, Msg)
    ).

unpack_date(date(Year, Month, Day, Hour, Minute, Second, MicroSecond),
    Year, det_int_to_month(Month), Day, Hour, Minute, Second, MicroSecond).

%---------------------------------------------------------------------------%

date_from_string(Str, Date) :-
    some [!Chars] (
        !:Chars = string.to_char_list(Str),
        ( if read_char((-), !.Chars, Rest1) then
            !:Chars = Rest1,
            read_int_and_num_chars(Year0, YearChars, !Chars),
            Year = -Year0
        else
            read_int_and_num_chars(Year, YearChars, !Chars)
        ),
        YearChars >= 4,
        read_char((-), !Chars),
        read_int_and_num_chars(Month, 2, !Chars),
        Month >= 1,
        Month =< 12,
        read_char((-), !Chars),
        read_int_and_num_chars(Day, 2, !Chars),
        Day >= 1,
        Day =< max_day_in_month_for(Year, Month),
        read_char(' ', !Chars),
        read_int_and_num_chars(Hour, 2, !Chars),
        Hour >= 0,
        Hour =< 23,
        read_char((:), !Chars),
        read_int_and_num_chars(Minute, 2, !Chars),
        Minute >= 0,
        Minute =< 59,
        read_char((:), !Chars),
        read_int_and_num_chars(Second, 2, !Chars),
        Second < 62,
        read_microseconds(MicroSecond, !Chars),
        !.Chars = [],
        Date = date(Year, Month, Day, Hour, Minute, Second, MicroSecond)
    ).

det_date_from_string(Str) = Date :-
    ( if date_from_string(Str, Date0) then
        Date = Date0
    else
        unexpected($pred, "invalid date: " ++ Str)
    ).

date_to_string(Date) = Str :-
    unpack_date(Date, Year0, Month, Day, Hour, Minute, Second, MicroSecond),
    ( if Year0 < 0 then
        SignStr = "-",
        Year = -Year0
    else
        SignStr = "",
        Year = Year0
    ),
    MicroSecondStr = microsecond_string(MicroSecond),
    Str = string.format("%s%04d-%02d-%02d %02d:%02d:%02d%s",
        [s(SignStr), i(Year), i(month_to_int(Month)), i(Day),
        i(Hour), i(Minute), i(Second), s(MicroSecondStr)]).

%---------------------------------------------------------------------------%

current_local_time(Now, !IO) :-
    time.time(TimeT, !IO),
    time.localtime(TimeT, TM, !IO),
    Now = tm_to_date(TM).

current_utc_time(Now, !IO) :-
    time.time(TimeT, !IO),
    TM = time.gmtime(TimeT),
    Now = tm_to_date(TM).

:- func tm_to_date(time.tm) = date.

tm_to_date(TM) = Date :-
    TM = tm(TMYear, TMMonth, TMDay, TMHour, TMMinute, TMSecond, _, _, _),
    Year = 1900 + TMYear,
    Month = TMMonth + 1,
    Day = TMDay,
    Hour = TMHour,
    Minute = TMMinute,
    Second = TMSecond,
    Date = date(Year, Month, Day, Hour, Minute, Second, 0).

julian_day_number(date(Year, Month, Day, _, _, _, _)) = JDN :-
    % The algorithm is described at
    % http://en.wikipedia.org/wiki/Julian_day.
    A = (14 - Month) div 12,
    Y = Year + 4800 - A,
    M = Month + 12 * A - 3,
    JDN = Day + ( 153 * M + 2 ) div 5 + 365 * Y + Y div 4 - Y div 100 +
        Y div 400 - 32045.

unix_epoch = date(1970, 1, 1, 0, 0, 0, 0).

same_date(A, B) :-
    A = date(Year, Month, Day, _, _, _, _),
    B = date(Year, Month, Day, _, _, _, _).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

years(Dur) = Dur ^ dur_months // 12.
months(Dur) = Dur ^ dur_months rem 12.
days(Dur) = Dur ^ dur_days.
hours(Dur) = Dur ^ dur_seconds // 3600.
minutes(Dur) = (Dur ^ dur_seconds rem 3600) // 60.
seconds(Dur) = Dur ^ dur_seconds rem 60.
microseconds(Dur) = Dur ^ dur_microseconds.

init_duration(Years0, Months0, Days0, Hours0, Minutes0, Seconds0,
        MicroSeconds0) = Dur :-
    ( if
        (
            Years0 >= 0,
            Months0 >= 0,
            Days0 >= 0,
            Hours0 >= 0,
            Minutes0 >= 0,
            Seconds0 >= 0,
            MicroSeconds0 >= 0
        ;
            Years0 =< 0,
            Months0 =< 0,
            Days0 =< 0,
            Hours0 =< 0,
            Minutes0 =< 0,
            Seconds0 =< 0,
            MicroSeconds0 =< 0
        )
    then
        Months = Years0 * 12 + Months0,
        Seconds1 = Seconds0 + MicroSeconds0 // microseconds_per_second,
        MicroSeconds = MicroSeconds0 rem microseconds_per_second,
        Seconds2 = Seconds1 + Minutes0 * 60 + Hours0 * 3600,
        Days = Days0 + Seconds2 // seconds_per_day,
        Seconds = Seconds2 rem seconds_per_day,
        Dur = duration(Months, Days, Seconds, MicroSeconds)
    else
        unexpected($pred, "some components negative and some positive")
    ).

:- func seconds_per_day = int.

seconds_per_day = 86400.

:- func microseconds_per_second = int.

microseconds_per_second = 1000000.

unpack_duration(Duration,
    years(Duration), months(Duration), days(Duration), hours(Duration),
    minutes(Duration), seconds(Duration), microseconds(Duration)).

zero_duration = duration(0, 0, 0, 0).

negate(duration(Months, Days, Seconds, MicroSeconds)) =
    duration(-Months, -Days, -Seconds, -MicroSeconds).

%---------------------------------------------------------------------------%

duration_from_string(Str, Duration) :-
    some [!Chars] (
        !:Chars = string.to_char_list(Str),
        read_sign(Sign, !Chars),
        read_char('P', !Chars),
        read_years(Years, !Chars),
        read_months(Months, !Chars),
        read_days(Days, !Chars),
        ( if read_char('T', !.Chars, TimePart) then
            TimePart = [_ | _],
            read_hours(Hours, TimePart, !:Chars),
            read_minutes(Minutes, !Chars),
            read_seconds_and_microseconds(Seconds, MicroSeconds, !Chars),
            !.Chars = [],
            Duration = init_duration(Sign * Years, Sign * Months,
                Sign * Days, Sign * Hours, Sign * Minutes, Sign * Seconds,
                Sign * MicroSeconds)
        else
            !.Chars = [],
            Duration = init_duration(Sign * Years, Sign * Months, Sign * Days,
                0, 0, 0, 0)
        )
    ).

det_duration_from_string(Str) = Duration :-
    ( if duration_from_string(Str, Duration0) then
        Duration = Duration0
    else
        unexpected($pred, "invalid duration: " ++ Str)
    ).

%---------------------------------------------------------------------------%

duration_to_string(duration(Months, Days, Seconds, MicroSeconds) @ Duration)
        = Str :-
    ( if
        Months = 0,
        Days = 0,
        Seconds = 0,
        MicroSeconds = 0
    then
        % At least one component must appear in the string.
        % The choice of days is arbitrary.
        Str = "P0D"
    else
        ( if
            Months >= 0,
            Days >= 0,
            Seconds >= 0,
            MicroSeconds >= 0
        then
            Sign = 1,
            SignStr = ""
        else if
            Months =< 0,
            Days =< 0,
            Seconds =< 0,
            MicroSeconds =< 0
        then
            Sign = -1,
            SignStr = "-"
        else
            unexpected($pred, "duration components have mixed signs")
        ),
        ( if
            Seconds = 0,
            MicroSeconds = 0
        then
            TimePart = []
        else
            TimePart = ["T",
                string_if_nonzero(Sign * hours(Duration), "H"),
                string_if_nonzero(Sign * minutes(Duration), "M"),
                seconds_duration_string(Sign * seconds(Duration),
                    Sign * microseconds(Duration))
            ]
        ),
        Str = string.append_list([
            SignStr, "P",
            string_if_nonzero(Sign * years(Duration), "Y"),
            string_if_nonzero(Sign * months(Duration), "M"),
            string_if_nonzero(Sign * days(Duration), "D")] ++ TimePart)
    ).

:- func string_if_nonzero(int, string) = string.

string_if_nonzero(X, Suffix) =
    ( if X = 0 then
        ""
    else
        int_to_string(X) ++ Suffix
    ).

:- func seconds_duration_string(seconds, microseconds) = string.

seconds_duration_string(Seconds, MicroSeconds) = Str :-
    ( if Seconds = 0, MicroSeconds = 0 then
        Str = ""
    else
        Str = string.from_int(Seconds) ++
            microsecond_string(MicroSeconds) ++ "S"
    ).

:- func microsecond_string(microseconds) = string.

microsecond_string(MicroSeconds) = Str :-
    ( if MicroSeconds > 0 then
        Str = rstrip_pred(unify('0'),
            string.format(".%06d", [i(MicroSeconds)]))
    else
        Str = ""
    ).

%---------------------------------------------------------------------------%
%
% Adding durations to date times.
%
% The following is a fairly direct translation of the algorithm at
% http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes.
%

add_duration(D, S, E) :-
    some [!Temp, !Carry, !E] (
        !:E = date(0, 0, 0, 0, 0, 0, 0),
        % Months
        !:Temp = S ^ dt_month + D ^ dur_months,
        !E ^ dt_month := modulo(!.Temp, 1, 13),
        !:Carry = fquotient(!.Temp, 1, 13),
        % Years
        !E ^ dt_year := S ^ dt_year + !.Carry,
        % Microseconds
        !:Temp = S ^ dt_microsecond + D ^ dur_microseconds,
        !E ^ dt_microsecond := modulo(!.Temp, microseconds_per_second),
        !:Carry = div(!.Temp, microseconds_per_second),
        % Seconds
        !:Temp = S ^ dt_second + D ^ dur_seconds + !.Carry,
        !E ^ dt_second := modulo(!.Temp, 60),
        !:Carry = div(!.Temp, 60),
        % Minutes
        !:Temp = S ^ dt_minute + !.Carry,
        !E ^ dt_minute := int.mod(!.Temp, 60),
        !:Carry = int.div(!.Temp, 60),
        % Hours
        !:Temp = S ^ dt_hour + !.Carry,
        !E ^ dt_hour := int.mod(!.Temp, 24),
        !:Carry = int.div(!.Temp, 24),
        % Days
        MaxDaysInMonth = max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month),
        ( if S ^ dt_day > MaxDaysInMonth then
            TempDays = MaxDaysInMonth
        else if S ^ dt_day < 1 then
            TempDays = 1
        else
            TempDays = S ^ dt_day
        ),
        !E ^ dt_day := TempDays + D ^ dur_days + !.Carry,
        add_duration_loop(D, S, !E),
        E = !.E
    ).

:- pred add_duration_loop(duration::in, date::in, date::in, date::out) is det.

add_duration_loop(D, S, !E) :-
    ( if !.E ^ dt_day < 1 then
        !E ^ dt_day := !.E ^ dt_day +
            max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month - 1),
        Carry = -1,
        Temp = !.E ^ dt_month + Carry,
        !E ^ dt_month := modulo(Temp, 1, 13),
        !E ^ dt_year := !.E ^ dt_year + fquotient(Temp, 1, 13),
        add_duration_loop(D, S, !E)
    else if
        MaxDaysInMonth = max_day_in_month_for(!.E ^ dt_year, !.E ^ dt_month),
        !.E ^ dt_day > MaxDaysInMonth
    then
        !E ^ dt_day := !.E ^ dt_day - MaxDaysInMonth,
        Carry = 1,
        Temp = !.E ^ dt_month + Carry,
        !E ^ dt_month := modulo(Temp, 1, 13),
        !E ^ dt_year := !.E ^ dt_year + fquotient(Temp, 1, 13),
        add_duration_loop(D, S, !E)
    else
        true
    ).

:- func fquotient(int, int, int) = int.

fquotient(A, Low, High) = int.div(A - Low, High - Low).

:- func modulo(int, int) = int.

modulo(A, B) = A - div(A, B) * B.

:- func modulo(int, int, int) = int.

modulo(A, Low, High) = modulo(A - Low, High - Low) + Low.

:- func max_day_in_month_for(int, int) = int.

max_day_in_month_for(YearValue, MonthValue) = Max :-
    M = int.mod(MonthValue - 1, 12) + 1,
    Y = YearValue + int.div(MonthValue - 1, 12),
    ( if
        (
            ( M = 1 ; M = 3 ; M = 5 ; M = 7 ; M = 8 ; M = 10 ; M = 12 ),
            Max0 = 31
        ;
            ( M = 4 ; M = 6 ; M = 9 ; M = 11 ),
            Max0 = 30
        ;
            M = 2,
            ( if ( Y mod 400 = 0 ; ( Y mod 100 \= 0, Y mod 4 = 0 ) ) then
                Max0 = 29
            else
                Max0 = 28
            )
        )
    then
        Max = Max0
    else
        % This should never happen.
        unexpected($pred, "unexpected value for M: " ++ string(M))
    ).

%---------------------------------------------------------------------------%

duration_leq(DurA, DurB) :-
    % Partial relation on durations. This algorithm is described at
    % http://www.w3.org/TR/xmlschema-2/#duration.
    list.all_true(
        ( pred(TestDate::in) is semidet :-
            add_duration(DurA, TestDate, DateA),
            add_duration(DurB, TestDate, DateB),
            compare(CompRes, DateA, DateB),
            ( CompRes = (<) ; CompRes = (=) )
        ), test_dates).

    % Returns dates used to compare durations.
    %
:- func test_dates = list(date).

test_dates = [
    date(1696, 9, 1, 0, 0, 0, 0),
    date(1697, 2, 1, 0, 0, 0, 0),
    date(1903, 3, 1, 0, 0, 0, 0),
    date(1903, 7, 1, 0, 0, 0, 0)
].

local_time_offset(TZ, !IO) :-
    time.time(TimeT, !IO),
    time.localtime(TimeT, LocalTM, !IO),
    GMTM = time.gmtime(TimeT),
    LocalTime = tm_to_date(LocalTM),
    GMTime = tm_to_date(GMTM),
    TZ = duration(GMTime, LocalTime).

%---------------------------------------------------------------------------%
%
% Computing the duration between two dates.
%

duration(DateA, DateB) = Duration :-
    compare(CompResult, DateB, DateA),
    (
        CompResult = (<),
        greedy_subtract_descending(ascending, DateA, DateB, Duration0),
        Duration = negate(Duration0)
    ;
        CompResult = (=),
        Duration = zero_duration
    ;
        CompResult = (>),
        greedy_subtract_descending(descending, DateB, DateA, Duration)
    ).

:- type order
    --->    ascending
    ;       descending.

    % This predicate has the precondition that DateA < DateB. OriginalOrder is
    % the original order of the date arguments (descending means that in the
    % original call DateA < DateB, while ascending means that in the original
    % call DateA > DateB). This is needed to correctly compute the days
    % component of the resulting duration. The calculation is different
    % depending on the original order, because we want the invariant:
    %   add_duration(duration(DateA, DateB), DateA, DateB)
    % to hold, and in the case where DateA > DateB, Duration will be negative.
    %
:- pred greedy_subtract_descending(order::in, date::in, date::in,
    duration::out) is det.

greedy_subtract_descending(OriginalOrder, DateA, DateB, Duration) :-
    some [!Borrow] (
        MicroSecondA = DateA ^ dt_microsecond,
        MicroSecondB = DateB ^ dt_microsecond,
        subtract_ints_with_borrow(microseconds_per_second, MicroSecondA,
            MicroSecondB, MicroSeconds, !:Borrow),
        SecondA = DateA ^ dt_second - !.Borrow,
        SecondB = DateB ^ dt_second,
        subtract_ints_with_borrow(60, SecondA, SecondB, Seconds, !:Borrow),
        MinuteA = DateA ^ dt_minute - !.Borrow,
        MinuteB = DateB ^ dt_minute,
        subtract_ints_with_borrow(60, MinuteA, MinuteB, Minutes, !:Borrow),
        HourA = DateA ^ dt_hour - !.Borrow,
        HourB = DateB ^ dt_hour,
        subtract_ints_with_borrow(24, HourA, HourB, Hours, !:Borrow),
        ( OriginalOrder = descending,
            add_duration(duration(0, -1, 0, 0), DateA, DateAMinus1Month),
            DaysToBorrow = max_day_in_month_for(DateAMinus1Month ^ dt_year,
                DateAMinus1Month ^ dt_month),
            DateAEndOfMonth = max_day_in_month_for(DateA ^ dt_year,
                DateA ^ dt_month),
            DayA = DateA ^ dt_day - !.Borrow,
            DayB = int.min(DateB ^ dt_day, DateAEndOfMonth)
        ; OriginalOrder = ascending,
            DaysToBorrow = max_day_in_month_for(DateB ^ dt_year,
                DateB ^ dt_month),
            DateBEndOfMonth = max_day_in_month_for(DateB ^ dt_year,
                DateB ^ dt_month),
            DayA = int.min(DateA ^ dt_day - !.Borrow, DateBEndOfMonth),
            DayB = DateB ^ dt_day
        ),
        subtract_ints_with_borrow(DaysToBorrow, DayA, DayB, Days, !:Borrow),
        MonthA = DateA ^ dt_month - !.Borrow,
        MonthB = DateB ^ dt_month,
        subtract_ints_with_borrow(12, MonthA, MonthB, Months, !:Borrow),
        YearA = DateA ^ dt_year - !.Borrow,
        YearB = DateB ^ dt_year,
        ( if YearA >= YearB then
            Years = YearA - YearB
        else
            % If this happens, then DateA < DateB, which violates
            % a precondition of this predicate.
            unexpected($pred, "left over years")
        ),
        Duration = init_duration(Years, Months, Days, Hours, Minutes, Seconds,
            MicroSeconds)
    ).

    % subtract_ints_with_borrow(BorrowAmount, Val1, Val2, Val, Borrow):
    % Subtract Val2 from Val1, possibly borrowing BorrowAmount if Val1 < Val2.
    % If an amount is borrowed, then Borrow is set to 1, otherwise it is set
    % to 0.
    %
:- pred subtract_ints_with_borrow(int::in, int::in, int::in, int::out,
    int::out) is det.

subtract_ints_with_borrow(BorrowVal, Val1, Val2, Diff, Borrow) :-
    ( if Val1 >= Val2 then
        Borrow = 0,
        Diff = Val1 - Val2
    else
        Borrow = 1,
        Diff = BorrowVal + Val1 - Val2
    ).

day_duration(DateA, DateB) = Duration :-
    builtin.compare(CompResult, DateB, DateA),
    (
        CompResult = (<),
        Duration0 = day_duration(DateB, DateA),
        Duration = negate(Duration0)
    ;
        CompResult = (=),
        Duration = zero_duration
    ;
        CompResult = (>),
        some [!Borrow] (
            MicroSecond1 = DateB ^ dt_microsecond,
            MicroSecond2 = DateA ^ dt_microsecond,
            subtract_ints_with_borrow(microseconds_per_second, MicroSecond1,
                MicroSecond2, MicroSeconds, !:Borrow),
            Second1 = DateB ^ dt_second - !.Borrow,
            Second2 = DateA ^ dt_second,
            subtract_ints_with_borrow(60, Second1, Second2, Seconds,
                !:Borrow),
            Minute1 = DateB ^ dt_minute - !.Borrow,
            Minute2 = DateA ^ dt_minute,
            subtract_ints_with_borrow(60, Minute1, Minute2, Minutes,
                !:Borrow),
            Hour1 = DateB ^ dt_hour - !.Borrow,
            Hour2 = DateA ^ dt_hour,
            subtract_ints_with_borrow(24, Hour1, Hour2, Hours, !:Borrow),
            JDN1 = julian_day_number(DateB),
            JDN2 = julian_day_number(DateA),
            Days = JDN1 - !.Borrow - JDN2,
            Duration = init_duration(0, 0, Days, Hours, Minutes, Seconds,
                MicroSeconds)
        )
    ).

%---------------------------------------------------------------------------%

foldl_days(Pred, !.Curr, End, !Acc) :-
    compare(Res, !.Curr, End),
    (
        ( Res = (<)
        ; Res = (=)
        ),
        Pred(!.Curr, !Acc),
        add_duration(init_duration(0, 0, 1, 0, 0, 0, 0), !Curr),
        foldl_days(Pred, !.Curr, End, !Acc)
    ;
        Res = (>)
    ).

foldl2_days(Pred, !.Curr, End, !Acc1, !Acc2) :-
    compare(Res, !.Curr, End),
    (
        ( Res = (<)
        ; Res = (=)
        ),
        Pred(!.Curr, !Acc1, !Acc2),
        add_duration(init_duration(0, 0, 1, 0, 0, 0, 0), !Curr),
        foldl2_days(Pred, !.Curr, End, !Acc1, !Acc2)
    ;
        Res = (>)
    ).

foldl3_days(Pred, !.Curr, End, !Acc1, !Acc2, !Acc3) :-
    compare(Res, !.Curr, End),
    (
        ( Res = (<)
        ; Res = (=)
        ),
        Pred(!.Curr, !Acc1, !Acc2, !Acc3),
        add_duration(init_duration(0, 0, 1, 0, 0, 0, 0), !Curr),
        foldl3_days(Pred, !.Curr, End, !Acc1, !Acc2, !Acc3)
    ;
        Res = (>)
    ).

%---------------------------------------------------------------------------%
%
% Parsing predicates.
%

:- pred read_microseconds(microseconds::out, list(char)::in, list(char)::out)
    is det.

read_microseconds(MicroSeconds, !Chars) :-
    ( if
        read_char((.), !.Chars, Chars1),
        read_int_and_num_chars(Fraction, FractionDigits, Chars1, !:Chars),
        FractionDigits > 0,
        FractionDigits < 7
    then
        MicroSeconds = int.pow(10, 6 - FractionDigits) * Fraction
    else
        MicroSeconds = 0
    ).

:- pred read_int_and_num_chars(int::out, int::out,
    list(char)::in, list(char)::out) is det.

read_int_and_num_chars(Val, N, !Chars) :-
    read_int_and_num_chars_2(0, Val, 0, N, !Chars).

:- pred read_int_and_num_chars_2(int::in, int::out, int::in, int::out,
    list(char)::in, list(char)::out) is det.

read_int_and_num_chars_2(!Val, !N, !Chars) :-
    ( if
        !.Chars = [Char | Rest],
        decimal_digit_to_int(Char, Digit)
    then
        !:Val = !.Val * 10 + Digit,
        read_int_and_num_chars_2(!Val, !.N + 1, !:N, Rest, !:Chars)
    else
        true
    ).

:- pred read_sign(int::out, list(char)::in, list(char)::out) is det.

read_sign(Sign, !Chars) :-
    ( if !.Chars = [(-) | Rest] then
        !:Chars = Rest,
        Sign = -1
    else
        Sign = 1
    ).

:- pred read_char(char::out, list(char)::in, list(char)::out) is semidet.

read_char(Char, [Char | Rest], Rest).

:- pred read_years(int::out, list(char)::in, list(char)::out) is det.

read_years(Years, !Chars) :-
    read_int_and_char_or_zero(Years, 'Y', !Chars).

:- pred read_months(int::out, list(char)::in, list(char)::out) is det.

read_months(Months, !Chars) :-
    read_int_and_char_or_zero(Months, 'M', !Chars).

:- pred read_days(int::out, list(char)::in, list(char)::out) is det.

read_days(Days, !Chars) :-
    read_int_and_char_or_zero(Days, 'D', !Chars).

:- pred read_hours(int::out, list(char)::in, list(char)::out) is det.

read_hours(Hours, !Chars) :-
    read_int_and_char_or_zero(Hours, 'H', !Chars).

:- pred read_minutes(int::out, list(char)::in, list(char)::out) is det.

read_minutes(Minutes, !Chars) :-
    read_int_and_char_or_zero(Minutes, 'M', !Chars).

:- pred read_seconds_and_microseconds(seconds::out, microseconds::out,
    list(char)::in, list(char)::out) is det.

read_seconds_and_microseconds(Seconds, MicroSeconds, !Chars) :-
    ( if
        read_int(Seconds0, !.Chars, Chars1),
        read_microseconds(MicroSeconds0, Chars1, Chars2),
        read_char('S', Chars2, Chars3)
    then
        !:Chars = Chars3,
        Seconds = Seconds0,
        MicroSeconds = MicroSeconds0
    else
        Seconds = 0,
        MicroSeconds = 0
    ).

:- pred read_int_and_char_or_zero(int::out, char::in,
    list(char)::in, list(char)::out) is det.

read_int_and_char_or_zero(Int, Char, !Chars) :-
    ( if
        read_int(Int0, !.Chars, Chars1),
        Chars1 = [Char | Rest]
    then
        !:Chars = Rest,
        Int = Int0
    else
        Int = 0
    ).

:- pred read_int(int::out, list(char)::in, list(char)::out) is det.

read_int(Val, !Chars) :-
    read_int_2(0, Val, !Chars).

:- pred read_int_2(int::in, int::out, list(char)::in, list(char)::out) is det.

read_int_2(!Val, !Chars) :-
    ( if
        !.Chars = [Char | Rest],
        decimal_digit_to_int(Char, Digit)
    then
        !:Val = !.Val * 10 + Digit,
        read_int_2(!Val, Rest, !:Chars)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module calendar.
%---------------------------------------------------------------------------%
