%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test predicates and functions from the calendar module that convert
% durations to and from strings.
%
%---------------------------------------------------------------------------%

:- module calendar_duration_conv.
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
    test_valid_durations(!IO),
    test_invalid_durations(!IO),
    test_exception_valid_durations(!IO),
    test_exception_invalid_durations(!IO).

%---------------------------------------------------------------------------%

:- pred test_valid_durations(io::di, io::uo) is det.

test_valid_durations(!IO) :-
    io.write_string(
        "=== Testing duration_from_string/2 with valid inputs ===\n\n",
        !IO),
    list.foldl(do_test_valid_duration, valid_durations, !IO),
    io.nl(!IO).

:- pred do_test_valid_duration(duration_test::in, io::di, io::uo) is det.

do_test_valid_duration(Test, !IO) :-
    Test = duration_test(Desc, TestString),
    io.format("duration_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( if duration_from_string(TestString, Duration) then
        io.format("TEST PASSED (accepted: %s)\n",
            [s(string(Duration))], !IO)
    else
        io.format("TEST FAILED (rejected: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_invalid_durations(io::di, io::uo) is det.

test_invalid_durations(!IO) :-
    io.write_string(
        "=== Testing duration_from_string/2 with invalid inputs ===\n\n",
        !IO),
    list.foldl(do_test_invalid_duration, invalid_durations, !IO),
    io.nl(!IO).

:- pred do_test_invalid_duration(duration_test::in, io::di, io::uo) is det.

do_test_invalid_duration(Test, !IO) :-
    Test = duration_test(Desc, TestString),
    io.format("duration_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( if duration_from_string(TestString, Duration) then
        DurationString = duration_to_string(Duration),
        io.format("TEST FAILED (accepted: %s)\n",
            [s(DurationString)], !IO)
    else
        io.format("TEST PASSED (rejected: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_exception_valid_durations(io::di, io::uo) is cc_multi.

test_exception_valid_durations(!IO) :-
    io.write_string(
        "=== Testing det_duration_from_string/1 with valid inputs ===\n\n", !IO),
    list.foldl(do_test_exception_valid_duration, valid_durations, !IO),
    io.nl(!IO).

:- pred do_test_exception_valid_duration(duration_test::in, io::di, io::uo)
    is cc_multi.

do_test_exception_valid_duration(Test, !IO) :-
    Test = duration_test(Desc, TestString),
    io.format("det_duration_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( try []
        Duration = det_duration_from_string(TestString)
    then
        io.format("TEST PASSED (accepted: %s)\n",
            [s(string(Duration))], !IO)
    catch_any _ ->
        io.format("TEST FAILED (exception: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_exception_invalid_durations(io::di, io::uo) is cc_multi.

test_exception_invalid_durations(!IO) :-
    io.write_string(
        "=== Testing det_duration_from_string/1 with invalid inputs ===\n\n", !IO),
    list.foldl(do_test_exception_invalid_duration, invalid_durations, !IO),
    io.nl(!IO).

:- pred do_test_exception_invalid_duration(duration_test::in, io::di, io::uo)
    is cc_multi.

do_test_exception_invalid_duration(Test, !IO) :-
    Test = duration_test(Desc, TestString),
    io.format("det_duration_from_string(\"%s\") ===> ", [s(TestString)], !IO),
    ( try []
        Duration = det_duration_from_string(TestString)
    then
        io.format("TEST FAILED (accepted: %s)\n",
            [s(string(Duration))], !IO)
    catch_any _ ->
        io.format("TEST PASSED (exception: %s)\n", [s(Desc)], !IO)
    ).

%---------------------------------------------------------------------------%

:- type duration_test
    --->    duration_test(
                description :: string,
                test_string :: string
            ).

%---------------------------------------------------------------------------%

:- func valid_durations = list(duration_test).

valid_durations = [

    % Date components only.
    duration_test("years only", "P1Y"),
    duration_test("months only", "P1M"),
    duration_test("days only", "P1D"),
    duration_test("years and months", "P1Y2M"),
    duration_test("years and days", "P1Y2D"),
    duration_test("months and days", "P1M2D"),
    duration_test("all date components", "P1Y2M3D"),

    % Time components only.
    duration_test("hours only", "PT1H"),
    duration_test("minutes only", "PT1M"),
    duration_test("seconds only", "PT1S"),
    duration_test("hours and minutes", "PT1H2M"),
    duration_test("hours and seconds", "PT1H2S"),
    duration_test("minutes and seconds", "PT1M2S"),
    duration_test("all time components", "PT1H2M3S"),

    % Mixed date and time components.
    duration_test("days and hours", "P1DT1H"),
    duration_test("all components", "P1Y2M3DT4H5M6S"),

    % Negative durations.
    duration_test("negative days", "-P1D"),
    duration_test("negative all components", "-P1Y2M3DT4H5M6S"),

    % Large values that normalise.
    duration_test("large months", "P18M"),
    duration_test("large hours", "PT25H"),
    duration_test("large seconds", "PT90S"),
    duration_test("large all components",
        "P1Y18M100DT10H15M90.0003S"),

    % Fractional seconds.
    duration_test("one fractional digit", "PT1.1S"),
    duration_test("two fractional digits", "PT1.12S"),
    duration_test("three fractional digits", "PT1.123S"),
    duration_test("four fractional digits", "PT1.1234S"),
    duration_test("five fractional digits", "PT1.12345S"),
    duration_test("six fractional digits", "PT1.123456S"),
    duration_test("smallest nonzero microsecond", "PT0.000001S"),
    duration_test("largest microsecond value", "PT0.999999S"),
    duration_test("fractional seconds only", "PT0.5S"),
    duration_test("zero seconds with fraction", "PT0.0003S"),

    % Zero durations.
    duration_test("zero seconds", "PT0S"),
    duration_test("zero seconds with explicit fraction", "PT0.0S"),
    duration_test("zero minutes", "PT0M"),
    duration_test("zero hours", "PT0H"),
    duration_test("zero months", "P0M"),
    duration_test("zero days", "P0D"),
    duration_test("all explicit zeros", "P0Y0M0DT0H0M0S")
].

%---------------------------------------------------------------------------%

:- func invalid_durations = list(duration_test).

invalid_durations = [
    % Structural/format errors.
    duration_test("empty string", ""),
    duration_test("blank string", "    "),
    duration_test("not a duration", "not a duration"),
    duration_test("missing P prefix", "1Y2M3D"),
    duration_test("P with no components", "P"),
    duration_test("P with no components and leading whitespace", "   P"),
    duration_test("P with no components and trailing whitespace", "P   "),
    duration_test("missing P prefix, starts with T", "T1H"),
    duration_test("P and T but no time components", "PT"),
    duration_test("negative P with no components", "-P"),
    duration_test("lowercase p", "p1Y"),
    duration_test("lowercase unit designator", "P1y"),
    duration_test("bare S with no digits", "P1DT1H30MS"),

    % Missing T separator.
    duration_test("hours without T separator", "P1H"),
    duration_test("seconds without T separator", "P1S"),

    % Wrong component order.
    duration_test("years after time components", "P1DT1H2M3Y"),
    duration_test("years after months", "P1M1Y"),
    duration_test("hours after seconds", "PT1S1H"),
    duration_test("minutes after seconds", "PT1S1M"),
    duration_test("hours after minutes", "PT1M1H"),

    % Duplicate components.
    duration_test("years specified twice", "P1Y1Y"),
    duration_test("days specified twice", "P1D1D"),
    duration_test("hours specified twice", "PT1H1H"),

    % Negative numbers in components.
    duration_test("negative number after P", "P-1Y"),
    duration_test("negative number after T", "PT-1H"),

    % Double sign.
    duration_test("double negative prefix", "--P1D"),
    duration_test("negative prefix and negative component", "-P-1D"),

    % Fractional parts on non-seconds components.
    duration_test("fractional years", "P1.5Y"),
    duration_test("fractional months", "P1.5M"),
    duration_test("fractional days", "P1.5D"),
    duration_test("fractional hours", "PT1.5H"),
    duration_test("fractional minutes", "PT1.5M"),

    % Fractional seconds errors.
    duration_test("seven fractional digits on seconds",
        "PT1.1234567S"),
    duration_test("decimal point with no fraction digits", "PT1.S"),
    duration_test("no integer part before decimal point", "PT.5S"),

    % Leading, trailing, and embedded whitespace.
    duration_test("trailing space", "P1D "),
    duration_test("leading space", " P1D"),
    duration_test("whitespace between P and components", "P 1Y"),
    duration_test("trailing text", "P1DT1H30M extra"),
    duration_test("invalid character after component", "P1DX"),
    duration_test("T followed by whitespace", "P1DT   "),

    % Non-digit where digit expected.
    duration_test("letter where number expected", "PxY"),
    duration_test("letter in time component", "PT1HxM"),

    % T present but no time components follow.
    duration_test("T with no time components", "P1DT")
].

%---------------------------------------------------------------------------%
:- end_module calendar_duration_conv.
%---------------------------------------------------------------------------%
