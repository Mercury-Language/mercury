%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Test calendar.init_data/8 and calendar.det_init_date/7.
%---------------------------------------------------------------------------%

:- module calendar_init_date.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module calendar.
:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_valid_dates(!IO),
    test_invalid_dates(!IO),
    test_det_init_date(!IO).

%---------------------------------------------------------------------------%

:- type test_date
    --->    test_date(
                description  :: string,
                year         :: year,
                month        :: month,
                day_of_month :: day_of_month,
                hour         :: hour,
                minute       :: minute,
                second       :: second,
                microsecond  :: microsecond
            ).

%---------------------------------------------------------------------------%
%
% Valid dates.
%

:- pred test_valid_dates(io::di, io::uo) is det.

test_valid_dates(!IO) :-
    list.foldl(test_valid_case, valid_dates, !IO).

:- pred test_valid_case(test_date::in, io::di, io::uo) is det.

test_valid_case(TestDate, !IO) :-
    TestDate  = test_date(Desc, Y, M, D, H, Min, S, Us),
    ( if init_date(Y, M, D, H, Min, S, Us, Date) then
        % Round-trip: verify that components are preserved by unpack_date/8.
        unpack_date(Date, Y2, M2, D2, H2, Min2, S2, Us2),
        ( if
            Y = Y2, M = M2, D = D2, H = H2, Min = Min2, S = S2, Us = Us2
        then
            io.format("PASS: valid date (%s)\n", [s(Desc)], !IO)
        else
            io.format("FAIL: round-trip mismatch for valid date (%s)\n",
                [s(Desc)], !IO)
        )
    else
        io.format("FAIL: init_date/8 failed for valid date (%s)\n",
            [s(Desc)], !IO)
    ).

:- func valid_dates = list(test_date).

valid_dates = [
    % Ordinary dates.
    test_date("ordinary date",      2024, january,  15, 12, 30,  0,  0),

    % Minimum component values.
    test_date("minimum components", 2024, january,   1,  0,  0,  0,  0),

    % Maximum time-of-day values.
    test_date("max hour",           2024, january,   1, 23,  0,  0,  0),
    test_date("max minute",         2024, january,   1,  0, 59,  0,  0),
    test_date("max second",         2024, january,   1,  0,  0, 59,  0),
    test_date("leap second",        2024, january,   1,  0,  0, 60,  0),
    test_date("max microsecond",    2024, january,   1,  0,  0,  0,  999999),

    % Month boundary days.
    test_date("jan 31",             2024, january,  31,  0,  0,  0,  0),
    test_date("feb 28 non-leap",    2023, february, 28,  0,  0,  0,  0),
    test_date("feb 29 leap",        2024, february, 29,  0,  0,  0,  0),
    test_date("mar 31",             2024, march,    31,  0,  0,  0,  0),
    test_date("apr 30",             2024, april,    30,  0,  0,  0,  0),
    test_date("dec 31",             2024, december, 31,  0,  0,  0,  0),

    % Century year (non-leap).
    test_date("feb 28 century",     1900, february, 28,  0,  0,  0,  0),

    % 400-year (leap).
    test_date("feb 29 400-year",    2000, february, 29,  0,  0,  0,  0),

    % Negative years.
    test_date("negative year",        -1, january,   1,  0,  0,  0,  0),

    % Year zero (1 BC).
    test_date("year zero",             0, march,     1,  0,  0,  0,  0),

    % Year with more than 4 digits.
    test_date("far future",      1000000, march,    13,  0,  0,  0,  0)
].

%---------------------------------------------------------------------------%
%
% Invalid dates.
%

:- pred test_invalid_dates(io::di, io::uo) is det.

test_invalid_dates(!IO) :-
    list.foldl(test_invalid_case, invalid_dates, !IO).

:- pred test_invalid_case(test_date::in, io::di, io::uo) is det.

test_invalid_case(TestDate, !IO) :-
    TestDate = test_date(Desc, Y, M, D, H, Min, S, Us),
    ( if init_date(Y, M, D, H, Min, S, Us, _Date) then
        io.format("FAIL: init_date/8 succeeded for invalid date (%s)\n",
            [s(Desc)], !IO)
    else
        io.format("PASS: init_date/8 failed for invalid date (%s)\n",
            [s(Desc)], !IO)
    ).

:- func invalid_dates = list(test_date).

invalid_dates = [
    % Day-of-month out of range.
    test_date("day zero",             2024, january,   0,  0,  0,   0,  0),
    test_date("day 32 in january",    2024, january,  32,  0,  0,   0,  0),
    test_date("feb 29 non-leap",      2023, february, 29,  0,  0,   0,  0),
    test_date("feb 29 century",       1900, february, 29,  0,  0,   0,  0),
    test_date("apr 31",               2024, april,    31,  0,  0,   0,  0),

    % Hour out of range.
    test_date("negative hour",        2024, january,   1, -1,  0,   0,  0),
    test_date("hour 24",              2024, january,   1, 24,  0,   0,  0),

    % Minute out of range.
    test_date("negative minute",      2024, january,   1,  0, -1,   0,  0),
    test_date("minute 60",            2024, january,   1,  0, 60,   0,  0),

    % Second out of range.
    test_date("negative second",      2024, january,   1,  0,  0,  -1,  0),
    test_date("second 62",            2024, january,   1,  0,  0,  62,  0),

    % Microsecond out of range.
    test_date("negative microsecond", 2024, january,   1,  0,   0,  0, -1),
    test_date("microsecond 1000000",  2024, january,   1,  0,   0,  0, 1000000)
].

%---------------------------------------------------------------------------%
%
% Test det_init_date/7.
%

:- pred test_det_init_date(io::di, io::uo) is cc_multi.

test_det_init_date(!IO) :-
    list.foldl(test_det_exception_case, exception_dates, !IO).

:- pred test_det_exception_case(test_date::in, io::di, io::uo) is cc_multi.

test_det_exception_case(TestDate, !IO) :-
    TestDate = test_date(Desc, Y, M, D, H, Min, S, Us),
    ( try []
        Date = det_init_date(Y, M, D, H, Min, S, Us)
    then
        use_date(Date, !IO),
        io.format("FAIL: det_init_date did not throw for (%s)\n",
            [s(Desc)], !IO)
    catch_any _ ->
        io.format("PASS: det_init_date threw exception for (%s)\n",
            [s(Desc)], !IO)
    ).

:- func exception_dates = list(test_date).

exception_dates = [
    test_date("det: day 32",    2024, january, 32, 0, 0, 0, 0),
    test_date("det: hour 24",   2024, january,  1, 24, 0, 0, 0),
    test_date("det: minute 60", 2024, january,  1, 0, 60, 0, 0)
].

:- pragma no_inline(pred(use_date/3)).
:- pred use_date(date::in, io::di, io::uo) is det.

use_date(_, !IO).

%---------------------------------------------------------------------------%
:- end_module calendar_init_date.
%---------------------------------------------------------------------------%
