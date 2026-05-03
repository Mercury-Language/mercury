%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test predicates and functions from the calendar module that convert
% date_times to and from strings.
%
%---------------------------------------------------------------------------%

:- module calendar_date_time_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module calendar.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_valid_date_times(!IO),
    test_invalid_date_times(!IO),
    test_exception_valid_date_times(!IO),
    test_exception_invalid_date_times(!IO).

%---------------------------------------------------------------------------%

:- pred test_valid_date_times(io::di, io::uo) is det.

test_valid_date_times(!IO) :-
    io.write_string(
        "=== Testing date_time_from_string/2 with valid inputs ===\n\n",
        !IO),
    list.foldl(do_test_valid_date_time(yes), valid_date_times, !IO),
    list.foldl(do_test_valid_date_time(no), valid_no_roundtrip_date_times,
        !IO),
    io.nl(!IO).

:- pred do_test_valid_date_time(bool::in, dt_conv_test::in,
    io::di, io::uo) is det.

do_test_valid_date_time(CheckRoundTrip, Test, !IO) :-
    Test = dt_conv_test(Desc, TestString),
    io.format("date_time_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( if date_time_from_string(TestString, DateTime) then
        RoundTripString = date_time_to_string(DateTime),
        (
            CheckRoundTrip = yes,
            ( if TestString = RoundTripString then
                io.format("TEST PASSED (accepted: %s)\n",
                    [s(string(DateTime))], !IO)
            else
                io.write_string("TEST FAILED (roundtrip failed)\n", !IO)
            )
        ;
            CheckRoundTrip = no,
            io.format("TEST PASSED (accepted: %s: to-string: \"%s\"))\n",
                [s(string(DateTime)), s(RoundTripString)], !IO)
        )
    else
        io.format("TEST FAILED (rejected: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_invalid_date_times(io::di, io::uo) is det.

test_invalid_date_times(!IO) :-
    io.write_string(
        "=== Testing date_time_from_string/2 with invalid inputs ===\n\n",
        !IO),
    list.foldl(do_test_invalid_date_time, invalid_date_times, !IO),
    io.nl(!IO).

:- pred do_test_invalid_date_time(dt_conv_test::in, io::di, io::uo) is det.

do_test_invalid_date_time(Test, !IO) :-
    Test = dt_conv_test(Desc, TestString),
    io.format("date_time_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( if date_time_from_string(TestString, DateTime) then
        DateTimeString = date_time_to_string(DateTime),
        io.format("TEST FAILED (accepted: %s)\n",
            [s(DateTimeString)], !IO)
    else
        io.format("TEST PASSED (rejected: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_exception_valid_date_times(io::di, io::uo) is cc_multi.

test_exception_valid_date_times(!IO) :-
    io.write_string(
        "=== Testing det_date_from_string/1 with valid inputs ===\n\n", !IO),
    list.foldl(do_test_exception_valid_date_time, valid_date_times, !IO),
    io.nl(!IO).

:- pred do_test_exception_valid_date_time(dt_conv_test::in, io::di, io::uo)
    is cc_multi.

do_test_exception_valid_date_time(Test, !IO) :-
    Test = dt_conv_test(Desc, TestString),
    io.format("det_date_time_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( try []
        DateTime = det_date_time_from_string(TestString)
    then
        io.format("TEST PASSED (accepted: %s)\n",
            [s(string(DateTime))], !IO)
    catch_any _ ->
        io.format("TEST FAILED (exception: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_exception_invalid_date_times(io::di, io::uo) is cc_multi.

test_exception_invalid_date_times(!IO) :-
    io.write_string(
        "=== Testing det_date_from_string/1 with invalid inputs ===\n\n", !IO),
    list.foldl(do_test_exception_invalid_date_time, invalid_date_times, !IO),
    io.nl(!IO).

:- pred do_test_exception_invalid_date_time(dt_conv_test::in, io::di, io::uo)
    is cc_multi.

do_test_exception_invalid_date_time(Test, !IO) :-
    Test = dt_conv_test(Desc, TestString),
    io.format("det_date_time_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( try []
        DateTime = det_date_time_from_string(TestString)
    then
        io.format("TEST FAILED (accepted: %s)\n",
            [s(string(DateTime))], !IO)
    catch_any _ ->
        io.format("TEST PASSED (exception: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- type dt_conv_test
    --->    dt_conv_test(
                description :: string,
                test_string :: string
            ).

%---------------------------------------------------------------------------%

% Valid date_times crated from strings are mostly round-trippable through
% date_time_to_string/1. The only exceptions are where the year components
% have leading zeros or the fractional components have trailing zeros.

    % These can be round-tripped through date_time_to_string/1.
    %
:- func valid_date_times = list(dt_conv_test).

valid_date_times = [
    dt_conv_test("unremarkable mid-month date", "2024-01-15 12:30:00"),
    dt_conv_test("arbitrary date in the past", "1977-10-09 12:00:00"),

    dt_conv_test("first day of year zero", "0000-01-01 00:00:00"),
    dt_conv_test("last day of year zero", "0000-12-31 23:59:59"),
    dt_conv_test("first day of year one", "0001-01-01 00:00:00"),
    dt_conv_test("last four-digit year", "9999-12-31 23:59:59"),

    dt_conv_test("year -1", "-0001-01-01 00:00:00"),
    dt_conv_test("end of year -1", "-0001-12-31 00:00:00"),
    dt_conv_test("Julian period origin in proleptic Gregorian calendar",
        "-4713-11-24 00:00:00"),

    dt_conv_test("five-digit year", "10000-01-01 00:00:00"),
    dt_conv_test("six-digit year", "100000-01-01 00:00:00"),

    dt_conv_test("400-year leap year", "2000-02-29 00:00:00"),
    dt_conv_test("ordinary leap year", "2024-02-29 00:00:00"),
    dt_conv_test("year zero is a leap year", "0000-02-29 00:00:00"),
    dt_conv_test("negative leap year", "-0004-02-29 00:00:00"),

    dt_conv_test("last day of Feb in a non-leap year",
        "2023-02-28 00:00:00"),
    dt_conv_test("last day of Feb in a century non-leap year",
        "1900-02-28 00:00:00"),

    dt_conv_test("end of Jan",   "2024-01-31 00:00:00"),
    dt_conv_test("end of March", "2024-03-31 00:00:00"),
    dt_conv_test("end of April", "2024-04-30 00:00:00"),
    dt_conv_test("end of May",   "2024-05-31 00:00:00"),
    dt_conv_test("end of June",  "2024-06-30 00:00:00"),
    dt_conv_test("end of July",  "2024-07-31 00:00:00"),
    dt_conv_test("end of Aug",   "2024-08-31 00:00:00"),
    dt_conv_test("end of Sep",   "2024-09-30 00:00:00"),
    dt_conv_test("end of Oct",   "2024-10-31 00:00:00"),
    dt_conv_test("end of Nov",   "2024-11-30 00:00:00"),
    dt_conv_test("end of Dec",   "2024-12-31 00:00:00"),

    dt_conv_test("midnight", "2024-01-01 00:00:00"),
    dt_conv_test("last second of the day", "2024-01-01 23:59:59"),
    dt_conv_test("leap second", "2024-01-01 00:00:60"),
    dt_conv_test("leap second on leap day", "2024-02-29 23:59:60"),

    dt_conv_test("one fractional digit", "2024-01-01 00:00:00.1"),
    dt_conv_test("two fractional digits", "2024-01-01 00:00:00.12"),
    dt_conv_test("three fractional digits", "2024-01-01 00:00:00.123"),
    dt_conv_test("four fractional digits", "2024-01-01 00:00:00.1234"),
    dt_conv_test("five fractional digits", "2024-01-01 00:00:00.12345"),
    dt_conv_test("six fractional digits", "2024-01-01 00:00:00.123456"),
    dt_conv_test("smallest nonzero microsecond", "2024-01-01 00:00:00.000001"),
    dt_conv_test("largest microsecond value", "2024-01-01 00:00:00.999999"),

    dt_conv_test("all maximum", "2024-12-31 23:59:60.999999"),
    dt_conv_test("Unix epoch", "1970-01-01 00:00:00"),
    dt_conv_test("first day of the Gregorian calendar",
        "1582-10-15 00:00:00")
].

    % These cannot be round-tripped through date_time_to_string/1.
    %
:- func valid_no_roundtrip_date_times = list(dt_conv_test).

valid_no_roundtrip_date_times =[
    dt_conv_test("one fractional trailing zero", "2024-01-01 00:00:00.10"),
    dt_conv_test("two fractional trailing zeros", "2024-01-01 00:00:00.100"),
    dt_conv_test("three fractional trailing zeros",
        "2024-01-01 00:00:00.1000"),
    dt_conv_test("four fractional trailing zeros",
        "2024-01-01 00:00:00.10000"),
    dt_conv_test("five fractional trailing zeros",
        "2024-01-01 00:00:00.100000"),

    dt_conv_test("trailing zeros after two fractional digits",
        "2024-01-01 00:00:00.120000"),
    dt_conv_test("trailing zeros after three fractional digits",
        "2024-01-01 00:00:00.123000"),
    dt_conv_test("trailing zeros after four fractional digits",
        "2024-01-01 00:00:00.123400"),
    dt_conv_test("trailing zeros after five fractional digits",
        "2024-01-01 00:00:00.123450"),

    dt_conv_test("single zero fractional digit",
        "2024-01-01 00:00:00.0"),
    dt_conv_test("all zero fractional digits",
        "2024-01-01 00:00:00.000000"),

    dt_conv_test("five digit year zero", "00000-01-01 00:00:00"),
    dt_conv_test("five digit year one", "00001-01-01 00:00:00")
].

%---------------------------------------------------------------------------%

:- func invalid_date_times = list(dt_conv_test).

invalid_date_times = [
    dt_conv_test("empty string", ""),
    dt_conv_test("blank string", "    "),
    dt_conv_test("not a date", "not a date"),
    dt_conv_test("missing time component", "2024-01-01"),
    dt_conv_test("missing date component", "12:00:00"),
    dt_conv_test("incorrect date separator", "2024/01/01 00:00:00"),
    dt_conv_test("ISO 8601 T separator", "2024-01-01T00:00:00"),
    dt_conv_test("missing seconds", "2024-01-01 00:00"),
    dt_conv_test("trailing space", "2024-01-01 00:00:00 "),
    dt_conv_test("leading space", " 2024-01-01 00:00:00"),
    dt_conv_test("trailing timezone indicator", "2024-01-01 00:00:00Z"),
    dt_conv_test("trailing UTC offset", "2024-01-01 00:00:00+10:00"),

    dt_conv_test("two-digit year", "99-01-01 00:00:00"),
    dt_conv_test("three-digit year", "999-01-01 00:00:00"),

    dt_conv_test("one-digit month", "2024-1-01 00:00:00"),
    dt_conv_test("one-digit day", "2024-01-1 00:00:00"),
    dt_conv_test("one-digit hour", "2024-01-01 0:00:00"),
    dt_conv_test("one-digit minute", "2024-01-01 00:0:00"),
    dt_conv_test("one-digit second", "2024-01-01 00:00:0"),

    dt_conv_test("month zero", "2024-00-01 00:00:00"),
    dt_conv_test("month 13", "2024-13-01 00:00:00"),
    dt_conv_test("day zero", "2024-01-00 00:00:00"),
    dt_conv_test("day 32 in a 31-day month", "2024-01-32 00:00:00"),
    dt_conv_test("Feb 29 in a non-leap year",  "2023-02-29 00:00:00"),
    dt_conv_test("Feb 29 in a century non-leap year", "1900-02-29 00:00:00"),
    dt_conv_test("day 31 in a 30-day month", "2024-04-31 00:00:00"),
    dt_conv_test("hour 24", "2024-01-01 24:00:00"),
    dt_conv_test("minute 60", "2024-01-01 00:60:00"),
    dt_conv_test("second 61", "2024-01-01 00:00:61"),

    dt_conv_test("trailing dot with no digits", "2024-01-01 00:00:00."),
    dt_conv_test("seven fractional digits", "2024-01-01 00:00:00.1234567"),

    dt_conv_test("negative sign but only two-digit year",
        "-01-01 00:00:00"),
    dt_conv_test("negative year with out-of-range month",
        "-0001-13-01 00:00:00"),

    dt_conv_test("letter in month field", "2024-ab-01 00:00:00"),
    dt_conv_test("letters in hour field", "2024-01-01 xx:00:00"),

    dt_conv_test("double negative prefix", "--0001-01-01 00:00:00"),
    dt_conv_test("negative sign on month", "2024--01-01 00:00:00"),
    dt_conv_test("negative sign on hour", "2024-01-01 -01:00:00"),
    dt_conv_test("negative sign on second", "2024-01-01 00:00:-01"),

    dt_conv_test("double space between date and time",
        "2024-01-01  00:00:00"),
    dt_conv_test("two decimal points", "2024-01-01 00:00:00.123.456"),
    dt_conv_test("decimal point in microseconds follow by more",
        "2024-01-01 00:00:00.123456.7"),
    dt_conv_test("three-digit month", "2024-001-01 00:00:00"),
    dt_conv_test("three-digit day", "2024-01-001 00:00:00"),
    dt_conv_test("three-digit hour", "2024-01-01 000:00:00"),
    dt_conv_test("three-digit minute", "2024-01-01 00:001:00"),
    dt_conv_test("three-digit second", "2024-01-01 00:00:001"),

    dt_conv_test("seven zeros in microseconds",
        "2024-01-01 00:00:00.0000000"),

    dt_conv_test("space within time component", "2024-01-01 00 :00:00"),
    dt_conv_test("space within date component", "2024 -01-01 00:00:00"),

    dt_conv_test("colons in date part", "2024:01:01 00:00:00"),
    dt_conv_test("dashes in time part", "2024-01-01 00-00-00"),

    dt_conv_test("missing month", "2024--01 00:00:00"),
    dt_conv_test("missing day", "2024-01- 00:00:00"),
    dt_conv_test("missing hour", "2024-01-01 :00:00"),
    dt_conv_test("missing minute", "2024-01-01 00::00"),
    dt_conv_test("trailing colon with no second digits",
        "2024-01-01 00:00:")
].

%---------------------------------------------------------------------------%
:- end_module calendar_date_time_conv.
%---------------------------------------------------------------------------%
