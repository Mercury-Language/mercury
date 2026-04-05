%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module calendar_basics.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module calendar.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_det_int_to_month("det_int_to_month", det_int_to_month, !IO),
    test_det_int_to_month("det_int0_to_month", det_int0_to_month, !IO),
    test_int_to_month("int_to_month",
        (pred(I::in, M::out) is semidet :- int_to_month(I, M)), !IO),
    test_int_to_month("int0_to_month",
        (pred(I::in, M::out) is semidet :- int0_to_month(I, M)), !IO),
    test_month_to_int("month_to_int", month_to_int, !IO),
    test_month_to_int("month_to_int0", month_to_int0, !IO),
    test_days_in_month(!IO),
    test_is_leap_year(!IO),
    test_unix_epoch(!IO),
    test_julian_day_number(!IO).

%---------------------------------------------------------------------------%

:- pred test_det_int_to_month(string::in,
    (func(int) = month)::in, io::di, io::uo) is cc_multi.

test_det_int_to_month(Desc, Func, !IO) :-
    io.format("=== Test %s/2 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_det_int_to_month(Desc, Func), ints, !IO),
    io.nl(!IO).

:- pred do_test_det_int_to_month(string::in,
    (func(int) = month)::in, int::in, io::di, io::uo) is cc_multi.

do_test_det_int_to_month(Desc, Func, Int, !IO) :-
    io.format("%s(%d) ==> ", [s(Desc), i(Int)], !IO),
    ( try []
        Month = Func(Int)
    then
        io.format("%s\n", [s(string(Month))], !IO)
    catch_any _ ->
        io.write_string("EXCEPTION\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int_to_month(string::in,
    pred(int, month)::in(pred(in, out) is semidet), io::di, io::uo) is det.

test_int_to_month(Desc, Pred, !IO) :-
    io.format("=== Test %s/2 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_int_to_month(Desc, Pred), ints, !IO),
    io.nl(!IO).

:- pred do_test_int_to_month(string::in,
    pred(int, month)::in(pred(in, out) is semidet), int::in,
    io::di, io::uo) is det.

do_test_int_to_month(Desc, Pred, Int, !IO) :-
    io.format("%s(%d) ==> ", [s(Desc), i(Int)], !IO),
    ( if Pred(Int, Month) then
        io.format("%s\n", [s(string(Month))], !IO)
    else
        io.write_string("FAILED\n", !IO)
    ).

:- func ints = list(int).

ints = [
    -1,
    0,
    1,
    2,
    11,
    12,
    13
].

%---------------------------------------------------------------------------%

:- pred test_month_to_int(string::in, (func(month) = int)::in,
    io::di, io::uo) is det.

test_month_to_int(Desc, Func, !IO) :-
    io.format("=== Test %s/1 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_month_to_int(Desc, Func), months, !IO),
    io.nl(!IO).

:- pred do_test_month_to_int(string::in, (func(month) = int)::in, month::in,
    io::di, io::uo) is det.

do_test_month_to_int(Desc, Func, Month, !IO) :-
    Int = Func(Month),
    io.format("%s(%s) = %d\n", [s(Desc), s(string(Month)), i(Int)], !IO).

%---------------------------------------------------------------------------%

:- pred test_days_in_month(io::di, io::uo) is det.

test_days_in_month(!IO) :-
    io.write_string("=== Test days_in_month/2 ===\n\n", !IO),
    list.foldl(do_test_days_in_month, [1977, 2000], !IO).

:- pred do_test_days_in_month(year::in, io::di, io::uo) is det.

do_test_days_in_month(Year, !IO) :-
    list.foldl(do_test_days_in_month_2(Year), months, !IO),
    io.nl(!IO).

:- pred do_test_days_in_month_2(year::in, month::in, io::di, io::uo) is det.

do_test_days_in_month_2(Year, Month, !IO) :-
    DaysInMonth = days_in_month(Year, Month),
    io.format("days_in_month(%d, %s) = %d\n",
        [i(Year), s(string(Month)), i(DaysInMonth)], !IO).

%---------------------------------------------------------------------------%

:- pred test_is_leap_year(io::di, io::uo) is det.

test_is_leap_year(!IO) :-
    io.write_string("=== Test is_leap_year/1 ===\n\n", !IO),
    list.foldl(do_test_is_leap_year, test_years, !IO),
    io.nl(!IO).

:- pred do_test_is_leap_year(year::in, io::di, io::uo) is det.

do_test_is_leap_year(Year, !IO) :-
    Desc = ( if is_leap_year(Year) then "leap" else "common" ),
    io.format("Year %d is a %s year.\n", [i(Year), s(Desc)], !IO).

:- func test_years = list(year).

test_years = [
    2000,   % Divisible by 400: leap year.
    1900,   % Divisible by 100, but not by 100: common year.
    2024,   % Divisible by 4, but not by 1000: leap year.
    2023,   % Not divisible by 4: common year.
       0,   % Divisible by 400: leap year.
      -1,   % Not divisible by 4: common year.
      -4,   % Divisible by 4: leap year.
    -100    % Divisible by 100, but not 400: common year.
].

%---------------------------------------------------------------------------%

:- pred test_unix_epoch(io::di, io::uo) is det.

test_unix_epoch(!IO) :-
    io.write_string("=== Test unix_epoch/0 ===\n\n", !IO),
    unpack_date_time(unix_epoch, Year, Month, DayOfMonth, Hour,
        Minute, Second, Microsecond),
    io.format("Unix epoch year   = %d\n", [i(Year)], !IO),
    io.format("Unix epoch month  = %s\n", [s(string(Month))], !IO),
    io.format("Unix epoch day    = %d\n", [i(DayOfMonth)], !IO),
    io.format("Unix epoch hour   = %d\n", [i(Hour)], !IO),
    io.format("Unix epoch minute = %d\n", [i(Minute)], !IO),
    io.format("Unix epoch second = %d\n", [i(Second)], !IO),
    io.format("Unix epoch microsecond = %d\n", [i(Microsecond)], !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred test_julian_day_number(io::di, io::uo) is det.

test_julian_day_number(!IO) :-
    io.write_string("=== Test julian_day_number/1 ===\n\n", !IO),
    list.foldl(do_test_julian_day_number, julian_day_tests, !IO),
    io.nl(!IO).

:- pred do_test_julian_day_number(julian_day_test::in, io::di, io::uo) is det.

do_test_julian_day_number(Test, !IO) :-
    Test = julian_day_test(Desc, DateTime, ExpectedJDN),
    ComputedJDN = julian_day_number(DateTime),
    io.format("julian_day_number(%s) = %d",
        [s(date_time_to_string(DateTime)), i(ComputedJDN)], !IO),
    ( if ComputedJDN = ExpectedJDN then
        io.format(" OK (%s)\n", [s(Desc)], !IO)
    else
        io.format(" WRONG (expected: %d)\n",
            [i(ExpectedJDN)], !IO)
    ).

:- type julian_day_test
    --->    julian_day_test(
                description :: string,
                date_time   :: date_time,
                julian_day  :: int
            ).

:- func julian_day_tests = list(julian_day_test).

julian_day_tests = [
    julian_day_test(
        "Start of Julian period",
        det_init_date_time(-4713, november, 24, 0, 0, 0, 0),
        0
    ),
    julian_day_test(
        "Unix epoch",
        unix_epoch,
        2440588
    ),
    julian_day_test(
        "J2000",
        det_init_date_time(2000, january, 1, 0, 0, 0, 0),
        2451545
    ),
    julian_day_test(
        "Day before Gregorian calender adoption",
        det_init_date_time(1582, october, 14, 0, 0, 0, 0),
        2299160
    ),
    julian_day_test(
        "First day of the Gregorian calendar",
        det_init_date_time(1582, october, 15, 0, 0, 0, 0),
        2299161
    ),
    julian_day_test(
        "Leap day",
        det_init_date_time(2000, february, 29, 0, 0, 0, 0),
        2451604
    ),
    julian_day_test(
        "Day after leap day",
        det_init_date_time(2000, march, 1, 0, 0, 0, 0),
        2451605
    ),
    julian_day_test(
        "Non-leap century",
        det_init_date_time(1900, february, 28, 0, 0, 0, 0),
        2415079
    ),
    julian_day_test(
        "Day after non-leap Feb",
        det_init_date_time(1900, march, 1, 0, 0, 0, 0),
        2415080
    ),
    julian_day_test(
        "Ordinary leap year",
        det_init_date_time(2024, february, 29, 0, 0, 0, 0),
        2460370
    ),
    julian_day_test(
        "New Year's Eve",
        det_init_date_time(2007, december, 31, 0, 0, 0, 0),
        2454466
    ),
    julian_day_test(
        "New year's Day",
        det_init_date_time(2008, january, 1, 0, 0, 0, 0),
        2454467
    ),
    julian_day_test(
        "Year zero",
        det_init_date_time(0, january, 1, 0, 0, 0, 0),
        1721060
    ),
    julian_day_test(
        "Year -1",
        det_init_date_time(-1, january, 1, 0, 0, 0, 0),
        1720695
    ),
    julian_day_test(
        "Midnight",
        det_init_date_time(2000, january, 1, 0, 0, 0, 0),
        2451545
    ),
    julian_day_test(
        "End of day",
        det_init_date_time(2000, january, 1, 23, 59, 59, 0),
        2451545
    ),
    julian_day_test(
        "Large year",
        det_init_date_time(10000, january, 1, 0, 0, 0, 0),
        5373485
    )
].

%---------------------------------------------------------------------------%

:- func months = list(month).

months = [
    january,
    february,
    march,
    april,
    may,
    june,
    july,
    august,
    september,
    october,
    november,
    december
].

%---------------------------------------------------------------------------%
:- end_module calendar_basics.
%---------------------------------------------------------------------------%
