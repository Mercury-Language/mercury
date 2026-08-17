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
:- import_module int.
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
    test_unix_epoch(!IO),
    test_julian_day_number(!IO),
    test_clocks(!IO),
    test_local_time_offset(!IO).

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

:- pred test_unix_epoch(io::di, io::uo) is det.

test_unix_epoch(!IO) :-
    io.write_string("=== Test unix_epoch/0 ===\n\n", !IO),
    unpack_date(unix_epoch, Year, Month, DayOfMonth, Hour,
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
        [s(date_to_string(DateTime)), i(ComputedJDN)], !IO),
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
        "Day before start of Julian period",
        det_init_date(-4713, november, 23, 0, 0, 0, 0),
        -1
    ),
    julian_day_test(
        "Start of Julian period",
        det_init_date(-4713, november, 24, 0, 0, 0, 0),
        0
    ),
    julian_day_test(
        "Unix epoch",
        unix_epoch,
        2440588
    ),
    julian_day_test(
        "J2000",
        det_init_date(2000, january, 1, 0, 0, 0, 0),
        2451545
    ),
    julian_day_test(
        "Day before Gregorian calendar adoption",
        det_init_date(1582, october, 14, 0, 0, 0, 0),
        2299160
    ),
    julian_day_test(
        "First day of the Gregorian calendar",
        det_init_date(1582, october, 15, 0, 0, 0, 0),
        2299161
    ),
    julian_day_test(
        "Leap day",
        det_init_date(2000, february, 29, 0, 0, 0, 0),
        2451604
    ),
    julian_day_test(
        "Day after leap day",
        det_init_date(2000, march, 1, 0, 0, 0, 0),
        2451605
    ),
    julian_day_test(
        "Non-leap century",
        det_init_date(1900, february, 28, 0, 0, 0, 0),
        2415079
    ),
    julian_day_test(
        "Day after non-leap Feb",
        det_init_date(1900, march, 1, 0, 0, 0, 0),
        2415080
    ),
    julian_day_test(
        "Ordinary leap year",
        det_init_date(2024, february, 29, 0, 0, 0, 0),
        2460370
    ),
    julian_day_test(
        "New Year's Eve",
        det_init_date(2007, december, 31, 0, 0, 0, 0),
        2454466
    ),
    julian_day_test(
        "New Year's Day",
        det_init_date(2008, january, 1, 0, 0, 0, 0),
        2454467
    ),
    julian_day_test(
        "Year zero",
        det_init_date(0, january, 1, 0, 0, 0, 0),
        1721060
    ),
    julian_day_test(
        "Year -1",
        det_init_date(-1, january, 1, 0, 0, 0, 0),
        1720695
    ),
    julian_day_test(
        "Midnight",
        det_init_date(2000, january, 1, 0, 0, 0, 0),
        2451545
    ),
    julian_day_test(
        "End of day",
        det_init_date(2000, january, 1, 23, 59, 59, 0),
        2451545
    ),
    julian_day_test(
        "Large year",
        det_init_date(10000, january, 1, 0, 0, 0, 0),
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

:- pred test_clocks(io::di, io::uo) is cc_multi.

test_clocks(!IO) :-
    io.write_string("=== Test current_{local,utc}_time/3 ===\n\n", !IO),

    % Any test that examines the entire date_time returned by
    % current_{local,utc}_time/3 will necessarily be nondeterministic.
    % We can however check that:
    %
    % 1. The predicate returns a valid date_time and does not throw an
    %    exception.
    % 2. The microsecond component is zero (as documented).

    do_clock_test("current_local_time/3", current_local_time, !IO),
    do_clock_test("current_utc_time/3", current_utc_time, !IO),
    io.nl(!IO).

:- pred do_clock_test(string::in,
    pred(date_time, io, io)::in(pred(out, di, uo) is det),
    io::di, io::uo) is cc_multi.

do_clock_test(ClockDesc, ClockPred, !IO) :-
    io.format("TEST: %s ", [s(ClockDesc)], !IO),
    ( try [io(!IO)]
        ClockPred(DateTime, !IO)
    then
        Microseconds = DateTime ^ microsecond,
        ( if Microseconds = 0 then
            io.write_string("PASSED\n", !IO)
        else
            io.format("FAILED (microseconds = %d)\n",
                [i(Microseconds)], !IO)
        )
    catch_any _ ->
        io.write_string("FAILED (exception)\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_local_time_offset(io::di, io::uo) is cc_multi.

test_local_time_offset(!IO) :-
    io.write_string("=== Test local_time_offset/3 ===\n\n", !IO),
    io.write_string("TEST: local_time_offset/3 ", !IO),
    ( try [io(!IO)]
        local_time_offset(Offset, !IO)
    then
        % UTC offsets range from -12:00 to +14:00, check that we are within
        % these. UTC offsets are given in whole numbers of minutes.
        % Components other than hours and minutes should be zero.
        ( if
            Offset ^ years = 0,
            Offset ^ months = 0,
            Offset ^ days = 0,
            Offset ^ hours > -13,
            Offset ^ hours < 15,
            Offset ^ minutes >= -59,
            Offset ^ minutes =< 59,
            Offset ^ seconds = 0,
            Offset ^ microseconds = 0
        then
            io.write_string("PASSED\n", !IO)
        else
            io.format("FAILED (offset = %s)\n", [s(string(Offset))], !IO)
        )
    catch_any _ ->
        io.write_string("FAILED (exception)\n", !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module calendar_basics.
%---------------------------------------------------------------------------%
