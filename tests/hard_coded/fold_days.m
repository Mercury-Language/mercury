%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fold_days.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module calendar.
:- import_module int.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    % Test 1: 1900 was not a leap year.
    Date1A = det_init_date_time(1900, february, 25, 0, 0, 0, 0),
    Date1B = det_init_date_time(1900, march, 1, 0, 0, 0, 0),
    io.write_string("Test 1:\n", !IO),
    foldl_days(write_date_time, Date1A, Date1B, !IO),
    io.nl(!IO),

    % Test 2: 2000 was a leap year.
    Date2A = det_init_date_time(2000, february, 25, 0, 0, 0, 0),
    Date2B = det_init_date_time(2000, march, 1, 0, 0, 0, 0),
    io.write_string("Test 2:\n", !IO),
    foldl_days(write_date_time, Date2A, Date2B, !IO),
    io.nl(!IO),

    % Test 3: 1977 was not a leap year.
    Date3A = det_init_date_time(1977, february, 25, 0, 0, 0, 0),
    Date3B = det_init_date_time(1977, march, 1, 0, 0, 0, 0),
    io.write_string("Test 3:\n", !IO),
    foldl_days(write_date_time, Date3A, Date3B, !IO),
    io.nl(!IO),

    % Test 4: calendar dates of start and end are the same,
    % time of start is less than that of the end.
    Date4A = det_init_date_time(1977, february, 17, 0, 0, 0, 0),
    Date4B = det_init_date_time(1977, february, 17, 1, 0, 0, 0),
    io.write_string("Test 4:\n", !IO),
    foldl_days(write_date_time, Date4A, Date4B, !IO),
    io.nl(!IO),

    % Test 5: calendar dates of start and end are the same,
    % time of start is equal to that of the end.
    Date5A = det_init_date_time(1977, february, 17, 0, 0, 0, 0),
    Date5B = det_init_date_time(1977, february, 17, 0, 0, 0, 0),
    io.write_string("Test 5:\n", !IO),
    foldl_days(write_date_time, Date5A, Date5B, !IO),
    io.nl(!IO),

    % Test 6: calendar dates of start and end are the same,
    % time of the start is greater than that of the end.
    Date6A = det_init_date_time(1977, february, 17, 1, 0, 0, 0),
    Date6B = det_init_date_time(1977, february, 17, 0, 0, 0, 0),
    io.write_string("Test 6:\n", !IO),
    foldl_days(write_date_time, Date6A, Date6B, !IO),
    io.nl(!IO),

    % Test 7: calendar date of start is after that of end.
    Date7A = det_init_date_time(2026, april, 7, 0, 0, 0, 0),
    Date7B = det_init_date_time(2025, april, 7, 0, 0, 0, 0),
    io.write_string("Test 7:\n", !IO),
    foldl_days(write_date_time, Date7A, Date7B, !IO),
    io.nl(!IO),

    % Test 8: calendar dates of start and end cross a year boundary.
    Date8A = det_init_date_time(2007, december, 30, 0, 0, 0, 0),
    Date8B = det_init_date_time(2008, january, 2, 0, 0, 0, 0),
    io.write_string("Test 8:\n", !IO),
    foldl_days(write_date_time, Date8A, Date8B, !IO),
    io.nl(!IO),

    % Test 9: foldl2_days test.
    Date9A = det_init_date_time(2024, january, 1, 0, 0, 0, 0),
    Date9B = det_init_date_time(2024, january, 31, 0, 0, 0, 0),
    io.write_string("Test 9:\n", !IO),
    foldl2_days(write_and_count_date_time, Date9A, Date9B, 0, NumDays9, !IO),
    io.format("NumDays = %d\n", [i(NumDays9)], !IO),
    io.nl(!IO),

    % Test 10: foldl3_days test.
    Date10A = det_init_date_time(2024, february, 1, 0, 0, 0, 0),
    Date10B = det_init_date_time(2024, february, 29, 0, 0, 0, 0),
    io.write_string("Test 10:\n", !IO),
    foldl3_days(do_foldl3_test, Date10A, Date10B, 0, NumDays10,
        0, NumMondays10, !IO),
    io.format("NumDays = %d\n", [i(NumDays10)], !IO),
    io.format("NumMondays = %d\n", [i(NumMondays10)], !IO).

:- pred write_date_time(date_time::in, io::di, io::uo) is det.

write_date_time(DateTime, !IO) :-
    Str = date_time_to_string(DateTime),
    io.write_string(Str ++ "\n", !IO).

:- pred write_and_count_date_time(date_time::in,
    int::in, int::out, io::di, io::uo) is det.

write_and_count_date_time(DateTime, !Count, !IO) :-
    write_date_time(DateTime, !IO),
    !:Count = !.Count + 1.

:- pred do_foldl3_test(date_time::in,
    int::in, int::out, int::in, int::out, io::di, io::uo) is det.

do_foldl3_test(DateTime, !NumDays, !NumMondays, !IO) :-
    write_date_time(DateTime, !IO),
    !:NumDays = !.NumDays + 1,
    DayOfWeek = day_of_week(DateTime),
    ( if DayOfWeek = monday then
        !:NumMondays = !.NumMondays + 1
    else
        true
    ).
